import gdb
from threading import Thread
from traceback import print_exc
from datetime import datetime
from os.path import isfile
import subprocess

class FollowEditor(gdb.Command):
    @staticmethod
    def printf(msg):
        print("follow-editor: %s" % (msg,))

    def __init__(self):
        self.editor_command = "emacsclient -n +%L %F"
        self.next_prompt_hook = gdb.prompt_hook
        self.directories = {}
        self.current_loc = (None, None)
        self.enabled = False

        gdb.prompt_hook = self.prompt_hook
        super(FollowEditor, self).__init__("follow-editor", gdb.COMMAND_OBSCURE)

    def run_editor_command(self, filename, line):
        new_loc = (filename, line)
        if self.current_loc == new_loc:
            return
        self.current_loc = new_loc
        
        command = self.editor_command.replace("%L", str(line)).replace("%F", filename).replace("%%", "%")
        #FollowEditor.printf("Trying to run %s" % command)
        proc = subprocess.Popen(command, shell=True,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        def wait_for_editor():
            proc.communicate()
            proc.wait()

        Thread(target=wait_for_editor).start()

    def prompt_hook(self, gdb_instance):
        try:
            if self.enabled:
                frame = gdb.selected_frame()
                if frame:
                    sal = frame.find_sal()
                    if sal.symtab and sal.line:
                        filename = sal.symtab.fullname()
                        if filename and isfile(filename):
                            self.run_editor_command(filename, sal.line)
                        else:
                            FollowEditor.printf("No such file: %s" % filename)
        except Exception as e:
            string_err = str(e)
            if string_err != "No frame is currently selected.":
                print_exc(e)
        finally:
            if self.next_prompt_hook:
                return self.next_prompt_hook()

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)

        command = None
        if len(argv) > 0:
            command = argv[0]
        
        if command == "command":
            try:
                self.editor_command = argv[1]
            except:
                print("usage: follow-editor command <command>")
        elif command == "on":
            self.enabled = True
        elif command == "off":
            self.enabled = False
        # elif command == "add-path-translation":
        #     if len(argv) == 3:
        #         dir_from = argv[1]
        #         dir_to = argv[2]
        #         self.directories[dir_from] = dir_to
        #     else:
        #         print("usage: follow-editor add-path-translation <from-path> <to-path>")
        # elif command == "del-path-translation":
        #     if len(argv) == 2:
        #         dir_from = argv[1]
        #         if dir_from in self.directories:
        #             del self.directories[dir_from]
        #         else:
        #             FollowEditor.printf("Unknown directory: %s" % dir_from)
        #     else:
        #         print("usage: follow-editor del-path-translation <from-directory>")
        elif command == "info" or command is None:
            print("follow-editor configuration:")
            print("  Editor command: %s" % (self.editor_command))
            print("  Run editor command: %s" % ("on" if self.enabled else "off"))
            # if len(self.directories):
            #     print("  Directory translations:")
            #     for k,v in self.directories:
            #         print("    %s -> %s" % (k,v))
        else:
            # print("Usage: follow-editor (command <command> | add-path-translation <from> <to> | del-path-translation <from> | info)")
            print("Usage: follow-editor (command <command> | on | off | info)")


#print(dir(gdb))
FollowEditor()
