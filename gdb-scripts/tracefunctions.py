import gdb
from datetime import datetime

def describe_current_position():
	frame = gdb.newest_frame()
	func = frame.function()
	sal = frame.find_sal()
	line = "%s:%s" % (sal.filename, sal.line)
	return "%s %s" % (func, line)


class TraceFunctions(gdb.Command):
	def __init__(self):
		self.connected = False
		super(TraceFunctions, self).__init__("tracefunctions", gdb.COMMAND_OBSCURE)

	def reset(self):
		if hasattr(self, "output"):
			output.flush()
			if output != gdb:
				output.close()
		gdb.flush()
		self.connected = False
		self.threads = {}
		self.output_desc = "gdb console"
		self.output = gdb
		gdb.events.inferior_call_pre.disconnect(self.call_pre)
		gdb.events.inferior_call_post.disconnect(self.call_post)

	def invoke(self, arg, from_tty):
		argv = gdb.string_to_argv(arg)
		if argv[0] == "on":
			self.reset()
			if len(argv) > 1:
				self.output = open(argv[2], mode='a')
				self.desc = "file " + argv[2]
			gdb.events.inferior_call_pre.connect(self.call_pre)
			gdb.events.inferior_call_post.connect(self.call_post)
		elif argv[0] == "off":
			self.reset()
		elif argv[0] == "show":
			if self.connected:
				print("Tracing function calls to %s" % self.output_desc)
			else:
				print("Not tracing function calls")
		else:
			print("Usage: tracefunctions (on [filename] | off)")
		#now = datetime.now()
		#print("%s %s" % (now.isoformat(), describe_current_position()))
		#gdb.execute("step", to_string=False)
		#pass

	def log(prefix, event):
		timestamp = datetime.now().isoformat()
		thread = event.ptid

		sal = gdb.find_pc_line(event.address)
		block = gdb.block_for_pc(event.address)
		while block and not block.function:
			block = block.superblock

		if block:
			function = block.function.print_name
		else:
			function = "unknown function at %x" % event.address

		if sal.symtab:
			line = "%s:%d" % (sal.symtab.fullname(), sal.line)
		else:
			line = "unknown source"
		self.output.write("[%s] %s %s %s %s" % (timestamp, thread, prefix, function, line))

	def call_pre(self, event):
		self.log("entering", event)

	def call_post(self, event):
		self.log("leaving", event)


def debug_event(event):
	print("%s / %s" % (repr(event), str(event)))


if hasattr(gdb.events, "inferior_call_pre"):
        TraceFunctions()
else:
        print("This GDB does not have inferior_call_pre or inferior_call_post events, tracefunctions will not work.")
