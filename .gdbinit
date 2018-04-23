add-auto-load-safe-path .gdbinit
source ~/Unison/Code/snippets/gdb-scripts/tracefunctions.py
source ~/Unison/Code/snippets/gdb-scripts/tracefunctionsi.py
source ~/Unison/Code/snippets/gdb-scripts/follow-editor.py
follow-editor command "e -n %F:%L"
