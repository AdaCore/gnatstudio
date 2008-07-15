"""This script shows how to reindent automatically all source files from
   all projects"""


from GPS import *

def indent_all (menu):
   for p in Project.root().dependencies(recursive=True):
      for s in p.sources():
         ed = EditorBuffer.get (s)
         ed.indent ()
         ed.save()
         ed.close()
   Console().write ("Done indenting")

Menu.create ("/Edit/Indent all files", indent_all)
