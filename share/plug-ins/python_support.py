"""This file provides enhanced support for editing Python files

Nothing is done if the projects do not support python, otherwise, the
following are provided:
  - Create highlight rules for python files
  - Defines a convenient filter "Python File" to use when defining your
    own actions
  - New menu to reload the current script. This menu is only visible when
    editing a python file.
  - Easy access to standard python library through shift-F3
  - Easy access to user-defined GPS configuration scripts through shift-F3
    This is always active, even if the current project doesn't support
    python
  - Add links to python documentation on the internet
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

pydoc_port = 9432
## Port that should be used for the pydoc daemon.
## This is a small program provided in the python distribution, which
## external web browsers can connect to to get the documentation for the
## standard python library


############################################################################
## No user customization below this line
############################################################################

# To be added (from idle environment)
#   - "indent region", "dedent region", "check module", "run module"
#   - "class browser" -> project view in GPS

import GPS, sys, os.path

try:
  import gtk
  has_pygtk=1
except:
  has_pygtk=0

pydoc_proc = None
python_menu = None

def register_doc():
  GPS.parse_xml ("""
  <documentation_file>
     <name>http://www.python.org/doc/2.4.2/tut/tut.html</name>
     <descr>Python tutorial</descr>
     <menu>/Python/Python Tutorial</menu>
     <category>Scripts</category>
  </documentation_file>
  <documentation_file>
     <shell lang="python">python_support.show_python_library()</shell>
     <descr>Python Library</descr>
     <menu>/Python/Python Library</menu>
     <category>Scripts</category>
  </documentation_file>""")

  if has_pygtk:
        GPS.parse_xml ("""
  <documentation_file>
     <name>http://www.pygtk.org/pygtk2tutorial/index.html</name>
     <descr>PyGTK tutorial</descr>
     <menu>/Python/PyGTK Tutorial</menu>
     <category>Scripts</category>
  </documentation_file>
  <documentation_file>
     <name>http://www.pygtk.org/pygtk2reference/index.html</name>
     <descr>PyGTK Reference Manual</descr>
     <menu>/Python/PyGTK Reference Manual</menu>
     <category>Scripts</category>
  </documentation_file>""")

def create_python_menu():
  global python_menu
  if not python_menu:
     python_menu = GPS.Menu.create ("/Python", ref="Help", add_before=1)
     GPS.Menu.create ("/Python/Import & Reload", on_activate=reload_file)
     register_doc ()

def destroy_python_menu():
  global python_menu
  if python_menu:
     python_menu.destroy()
     python_menu = None

def reload_file (menu):
  """Reload the currently edited file in python.
If the file has not been imported yet, import it initially.
Otherwise, reload the current version of the file."""
  try:
     file = GPS.current_context().file()
     module=os.path.splitext (os.path.basename (file.name()))[0]

     ## The actual import and reload must be done in the context of the
     ## GPS console so that they are visible there. The current function
     ## executes in a different context, and would not impact the GPS
     ## console as a result otherwise.

     ## We cannot use  execfile(...), since that would be the equivalent
     ## of "from ... import *", not of "import ..."

     if sys.modules.has_key (module):
        GPS.exec_in_console ("reload (sys.modules[\"" + module + "\"])")

     else:
        try:
           sys.path.index (os.path.dirname (file.name()))
        except:
           sys.path = [os.path.dirname (file.name())] + sys.path
        mod = __import__ (module)

        # This would import in the current context, not what we want
        # exec (compile ("import " + module, "<cmdline>", "exec"))

        ## The proper solution is to execute in the context of the GPS console
        GPS.exec_in_console ("import " + module)
        
  except:
     pass   ## Current context is not a file

def context_changed (hook_name, context):
  """Called when a new context is activated in GPS"""
  try:
    if context.file().language() == "python":
       create_python_menu()
    else:
       destroy_python_menu()
  except:
    destroy_python_menu()

def project_recomputed (hook_name):
  """if python is one of the supported language for the project, add various
     predefined directories that may contain python files, so that shift-F3
     works to open these files as it does for the Ada runtime"""
  GPS.Project.add_predefined_paths (sources=GPS.get_home_dir() + "plug-ins")
  try:
    GPS.Project.root().languages (recursive=True).index ("python")
    # The rest is done only if we support python
    GPS.Project.add_predefined_paths (sources=os.pathsep.join (sys.path))
  except:
    pass

def show_python_library ():
  """Open a navigator to show the help on the python library"""
  global pydoc_proc, pydoc_port
  if not pydoc_proc:
     while 1:
        pydoc_proc = GPS.Process ("pydoc -p " + `pydoc_port`)
        out = pydoc_proc.expect ("pydoc server ready|Address already in use", 10000)
        try:
           out.rindex ("Address already in use")
           pydoc_port += 1
        except:
           break
  GPS.HTML.browse ("http://localhost:" + `pydoc_port` + "/")

def before_exit (hook_name):
  """Called before GPS exits"""
  global pydoc_proc
  if pydoc_proc:
    pydoc_proc.kill()
    pydoc_proc = None
  return 1

## Always register python immediately as a language, so that projects that
## have
##    for Languages use ("Python");
## do not have to define a naming scheme and can depend on the default.

GPS.parse_xml ("""
  <Language>
    <Name>Python</Name>
    <Spec_Suffix>.py</Spec_Suffix>
    <Keywords>(a(nd|ssert|s)|break|c(lass|ontinue)|de[fl]|e(l(if|se)|x(cept|ec))|f(inally|or|rom)|global|i(mport|[fns])|lambda|not|or|p(ass|rint)|r(aise|eturn)|try:|while|yield)\\b</Keywords>
    <Context>
      <New_Line_Comment_Start>#</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>class</Name>
        <Pattern>^\s*class\s+([\w_][\w\d_]+)\s*:</Pattern>
        <Index>1</Index>
        <Icon>package_xpm</Icon>
      </Category>
      <Category>
        <Name>procedure</Name>
        <Pattern>^\s*def\s+([\w_][\w\d_]+)\s*\([^\)]*\)\s*:</Pattern>
        <Index>1</Index>
        <Icon>subprogram_xpm</Icon>
      </Category>
    </Categories>
  </Language>

  <filter_and name="Python file">
    <filter id="Source editor" />
     <filter language="Python" />
  </filter_and>
""")

GPS.Hook ("project_view_changed").add (project_recomputed)
GPS.Hook ("before_exit_action_hook").add (before_exit)
GPS.Hook ("context_changed").add (context_changed)
