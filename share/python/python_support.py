## This file provides extra support for editing python scripts through GPS
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

import GPS, sys, os.path

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


def context_changed (hook_name, context):
  """Called when a new context is activated in GPS"""
  try:
    if context.file().language() == "python":
       GPS.Menu.get ("/Python").show()
    else:
       GPS.Menu.get ("/Python").hide()
  except:
    GPS.Menu.get ("/Python").hide()


def project_recomputed (hook_name):
  """Setup GPS for python support"""

  GPS.Project.add_predefined_paths \
    (sources=GPS.get_home_dir() + "python_startup")

  try:
    GPS.Project.root().languages (recursive=True).index ("python")

    # The rest is done only if we support python
    GPS.Project.add_predefined_paths (sources=os.pathsep.join (sys.path))

  except:
    pass

GPS.Menu.create ("/Python/reload file", on_activate=reload_file)
GPS.Menu.get ("/Python").hide()

GPS.Hook ("project_view_changed").add (project_recomputed)
GPS.Hook ("context_changed").add (context_changed);

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
  </Language>

  <filter_and name="Python file">
     <filter id="Source editor" />
     <filter language="Python" />
  </filter_and>""")

