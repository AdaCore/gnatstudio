
## Nothing is done if the projects do not support python, otherwise, the
## following are provided:
##   - Create highlight rules for python files
##   - Defines a convenient filter "Python File" to use when defining your
##     own actions
##   - New menu to reload the current script. This menu is only visible when
##     editing a python file.
##   - Easy access to standard python library through shift-F3
##   - Easy access to user-defined GPS configuration scripts through shift-F3
##     This is always active, even if the current project doesn't support
##     python
##   - Add links to python documentation on the internet

## To be added (from idle environment)
##   - "indent region", "dedent region", "check module", "run module"
##   - "class browser" -> project view in GPS

import GPS, sys, os.path


pydoc_proc = None
pydoc_port = 9432

def create_python_menu():
  try:
     menu = GPS.Menu.get ("/Python/")
  except:
     GPS.Menu.create ("/Python/reload file", on_activate=reload_file)
     GPS.parse_xml ("""
  <documentation_file>
     <name>http://www.python.org/doc/2.3.4/tut/tut.html</name>
     <descr>Python tutorial</descr>
     <menu>/Python/Python Tutorial</menu>
     <category>Scripts</category>
  </documentation_file>

  <documentation_file>
     <shell lang="python">python_support.show_python_library()</shell>
     <descr>Python Library</descr>
     <menu>/Python/Python Library</menu>
     <category>Scripts</category>
  </documentation_file>
""")

def destroy_python_menu():
  try:
     GPS.Menu.get ("/Python/").destroy()
  except:
     pass

def reload_file (menu):
  """Reload the currently edited file in python"""
  try:
     file = GPS.current_context().file()
     module=os.path.splitext (os.path.basename (file.name()))[0]

     try:
        reload (sys.modules[module])
     except KeyError:
        import module
  except:
     pass   ## Current context is not a file

def project_recomputed (hook_name):
  """Setup GPS for python support"""

  GPS.Project.add_predefined_paths \
    (sources=GPS.get_home_dir() + "plug-ins")

  try:
    GPS.Project.root().languages (recursive=True).index ("python")

    # The rest is done only if we support python
    create_python_menu()
    list=[]
    for p in sys.path:
      for root, dirs, files in os.walk ("/usr/local/lib/python2.3"):
         list += [os.path.join (root, d) for d in dirs]
      
    GPS.Project.add_predefined_paths (sources=os.pathsep.join (list))

  except:
    destroy_python_menu()

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


### Do not call create_python_menu yet, since keeping a hidden toplevel menu
### triggers bugs in gtk+, for instance leaving an empty space in the menubar

# create_python_menu()

GPS.Hook ("project_view_changed").add (project_recomputed)
GPS.Hook ("before_exit_action_hook").add (before_exit)

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

