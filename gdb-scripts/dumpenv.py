import gdb
from traceback import print_exc
from datetime import datetime

class DumpEnv(gdb.Command):
    def __init__(self):
        super(DumpEnv, self).__init__("dumpenv", gdb.COMMAND_OBSCURE)

    def invoke(self, arg, from_tty):
        pass

DumpEnv()
