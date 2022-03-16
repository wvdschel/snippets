import gdb
from traceback import print_exc
from datetime import datetime

class DumpEnv(gdb.Command):
    def __init__(self):
        super(DumpEnv, self).__init__("dumpenv", gdb.COMMAND_OBSCURE)

    def invoke(self, arg, from_tty):
        pos = 0
        while True:
            val = gdb.parse_and_eval('((char**)environ)[%d]' % pos)
            if 0 == val.cast(gdb.lookup_type('long')):
                break
            print(val.string())
            pos += 1

DumpEnv()
