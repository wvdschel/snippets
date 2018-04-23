import gdb
from traceback import print_exc
from datetime import datetime

class TraceFunctionsI(gdb.Command):
    def __init__(self):
        self.threads = {}
        self.context_switch_interval = 0
        self.step_size = 10
        self.output_filename = None

        self.reset()
        if hasattr(gdb.events, "new_thread"):
            gdb.events.new_thread.connect(self.new_thread)
        else:
            printf("This GDB does not know the new_thread event, multi-threading will not work.")
            
        super(TraceFunctionsI, self).__init__("tracefunctionsi", gdb.COMMAND_OBSCURE)

    @staticmethod
    def printf(msg):
        print("tracefunctionsi: %s" % (msg,))

    @staticmethod
    def describe_current_position():
        frame = gdb.newest_frame()
        func = frame.function()
        sal = frame.find_sal()
        line = "%s:%s" % (sal.filename, sal.line)
        return "%s %s" % (func, line)

    @staticmethod
    def callstack_depth():
        depth = 1
        frame = gdb.newest_frame()
        while frame is not None:
            frame = frame.older()
            depth += 1
        return depth

    def new_thread(self, event):
        thread = event.inferior_thread
        self.threads[thread.global_num] = thread
        TraceFunctionsI.printf("aware of new thread %d: %s" % (thread.global_num, thread.ptid,))

    def reset(self):
        if hasattr(self, "output"):
            self.output.flush()
            if self.output != gdb:
                self.output.close()
        if self.output_filename:
            self.output = open(self.output_filename, mode='a')
        else:
            self.output = gdb

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)

        command = None
        instruction_count = None
        if len(argv) > 0:
            command = argv[0]
            try:
                instruction_count = int(argv[0])
            except ValueError:
                pass
        
        if instruction_count:
            self.tracefunctionsi(instruction_count)
        elif command == "step-size":
            try:
                self.step_size = int(argv[1])
            except:
                print("usage: tracefunctionsi step-size <instruction count>")
        elif command == "context-switch-interval":
            try:
                self.context_switch_interval = int(argv[1])
            except:
                print("usage: tracefunctionsi context_switch_interval <step count>")
        elif command == "output":
            if len(argv) > 1:
                self.output_filename = argv[1]
            else:
                self.output_filename = None
            self.reset()
        elif command == "info" or command is None:
            print("tracefunctionsi configuration:")
            print("  Output: %s" % (self.output_filename if self.output_filename else "gdb console"))
            print("  Step size: %d instructions" % self.step_size)
            print("  Context switch interval: %d steps" % self.context_switch_interval)
        else:
            print("Usage: tracefunctionsi (<instruction count> | output [filename] | step-size <instruction count> | context-switch-interval <step count> | info)")

    def log(self, prefix, pc, function, filename, linenumber):
        timestamp = (datetime.now() - self.start_time).total_seconds()
        thread = gdb.selected_thread().ptid

        if function:
            function_str = function.print_name
        else:
            function_str = "unknown function around %x" % pc

        if filename:
            line = "%s:%d" % (filename, linenumber)
        else:
            line = "unknown source"
        self.output.write("[%04.8f] %s %s %s %s\n" % (timestamp, thread, prefix, function_str, line))
        #TraceFunctionsI.printf("[%s] %s %s %s %s" % (timestamp, thread, prefix, function_str, line))

    def switch_thread(self, current_thread_id=None, first_thread_id=None):
        if len(self.threads) == 0:
            printf("warning: no known threads, no thread switch performed.")
            return
        all_threads = sorted(self.threads.keys())
        if current_thread_id is None:
            current_thread_id = gdb.selected_thread().global_num
        if first_thread_id is None:
            first_thread_id = current_thread_id

        try:
            next_thread_idx = all_threads.index(current_thread_id) + 1
            if next_thread_idx >= len(all_threads):
                next_thread_idx = 0
        except ValueError:
            next_thread_idx = 0

        #printf("Attempting to switch to thread at idx %d of %s" % (next_thread_idx, all_threads))
        next_thread_id = all_threads[next_thread_idx]
        if next_thread_id == first_thread_id:
            if len(all_threads) > 0:
                TraceFunctionsI.printf("error: failed to find any next thread to execute. Not performing context switch.")
        
        next_thread = self.threads[next_thread_id]
        if next_thread.is_exited() or not next_thread.is_valid():
            TraceFunctionsI.printf("thread %s has become invalid or exited, removing from watch list." % (next_thread.ptid,))
            del self.threads[next_thread_id]
            self.switch_thread(current_thread_id, first_thread_id)
        else:
            # TODO this doesn't work at all
            gdb.execute("set scheduler-locking off", to_string=True)
            TraceFunctionsI.printf("switching to thread %s" % (next_thread.ptid,))
            next_thread.switch()
            #brk = gdb.FinishBreakpoint(gdb.newest_frame(), internal=True)
            #brk.thread = int(next_thread.global_num)

            gtid = gdb.selected_thread().global_num
            frame = gdb.newest_frame()
            sal = frame.find_sal()
            if sal.symtab:
                filename = sal.symtab.filename
            else:
                filename = None
            linenumber = sal.line
            function = frame.function()
            gdb.execute("finish", to_string=True)
            self.log("returned from", frame.pc(), function, filename, linenumber)
            gdb.execute("set scheduler-locking on", to_string=True)
            TraceFunctionsI.printf("switch to thread %s completed" % (next_thread.ptid,))

    def tracefunctionsi(self, instruction_count):
        try:
            gdb.execute("set pagination off", to_string=True)
            #gdb.execute("set scheduler-locking on", to_string=True)

            self.start_time = datetime.now()
            TraceFunctionsI.printf("Starting trace at %s" % self.start_time.isoformat())
            self.output.write("Starting trace at %s\n" % self.start_time.isoformat())

            stack_depths = {}
            frame = gdb.newest_frame()
            active_functions = {}
            active_source_files = {}
            active_source_lines = {}
            first_addrs = {}
            step_in_thread = 0
            TraceFunctionsI.printf("tracing function calls for %d instructions" % instruction_count)

            while instruction_count > 0:
                if step_in_thread == self.context_switch_interval and self.context_switch_interval != 0:
                    step_in_thread = 0
                    TraceFunctionsI.printf("Initiating thread switch")
                    self.switch_thread()

                gtid = gdb.selected_thread().global_num
                frame = gdb.newest_frame()
                sal = frame.find_sal()
                if sal.symtab:
                    filename = sal.symtab.filename
                else:
                    filename = None
                linenumber = sal.line
                function = frame.function()
                stack_depth = TraceFunctionsI.callstack_depth()

                if gtid not in stack_depths:
                    TraceFunctionsI.printf("initializing thread %s" % gtid)
                    stack_depths[gtid] = stack_depth
                    active_functions[gtid] = function
                    active_source_files[gtid] = filename
                    active_source_lines[gtid] = linenumber
                    first_addrs[gtid] = frame.pc()
                else:
                    if filename != active_source_files[gtid] or \
                       function != active_functions[gtid] or \
                       stack_depth != stack_depths[gtid]:
                        if stack_depth <= stack_depths[gtid]:
                            self.log("returned from", first_addrs[gtid], active_functions[gtid], active_source_files[gtid], active_source_lines[gtid])
                        if stack_depth >= stack_depths[gtid]:
                            self.log("entering", frame.pc(), function, filename, linenumber)

                        stack_depths[gtid] = stack_depth
                        active_functions[gtid] = function
                        active_source_files[gtid] = filename
                        active_source_lines[gtid] = linenumber
                        first_addrs[gtid] = frame.pc()

                curr_step_size = min(self.step_size, instruction_count)
                gdb.execute("stepi %d" % curr_step_size, to_string=True)
                step_in_thread += 1
                instruction_count -= curr_step_size
                #TraceFunctionsI.printf("Stepped %d instructions, step %d/%d in thread %d, %d instructions left" % (
                #    curr_step_size, step_in_thread, self.context_switch_interval, gtid, instruction_count))
        except Exception as e:
            print_exc(e)
            raise e
        TraceFunctionsI.printf("done.")
        self.output.flush()


#print(dir(gdb))
TraceFunctionsI()
