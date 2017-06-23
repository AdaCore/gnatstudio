"""This plug-in adds a menu Edit->Indent all files which will indent
   automatically all source files from all projects"""


from GPS import Project, Menu, Console, EditorBuffer


def indent_all(menu):
    for f in Project.root().sources(recursive=True):
        ed = EditorBuffer.get(f)
        ed.indent()
        ed.save()
        ed.close()
    Console().write("Done indenting")


Menu.create("/Edit/Indent all files", indent_all)
