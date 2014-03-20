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
## No user customization below this line
############################################################################

# To be added (from idle environment)
#   - "indent region", "dedent region", "check module", "run module"
#   - "class browser" -> project view in GPS

import GPS
import sys
import os.path
import gps_utils

try:
    from gi.repository import Gtk
    has_pygtk = 1
except:
    has_pygtk = 0


class Python_Support(object):
    def __init__(self):
        self.port_pref = GPS.Preference("Plugins/python_support/port")
        self.port_pref.create(
            "Pydoc port", "integer",
            """Port that should be used when spawning the pydoc daemon.
This is a small local server to which your web browser connects to display the
documentation for the standard python library. It is accessed through the
/Python menu when editing a python file""",
            9432)

        regex = ("(a(nd|ssert|s)|break|c(lass|ontinue)|de[fl]|e(l(if|se)"
                 + "|x(cept|ec))|f(inally|or|rom)|global|i(mport|[fns])"
                 + "|lambda|not|or|p(ass|rint)|r(aise|eturn)|try|while|yield)"
                 + "\\b")

        XML = """
  <Language>
    <Name>Python</Name>
    <Body_Suffix>.py</Body_Suffix>
    <Obj_Suffix>.pyc</Obj_Suffix>
    <Keywords>%s</Keywords>
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
        <Pattern>^\s*class\s+([\w_][\w\d_]+)\s*(\([^\)]*\))?:</Pattern>
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

  <documentation_file>
     <name>http://docs.python.org/2/tutorial/</name>
     <descr>Python tutorial</descr>
     <menu>/Help/Python/Python Tutorial</menu>
     <category>Scripts</category>
  </documentation_file>
  <documentation_file>
     <shell lang="python">
        GPS.execute_action('display python library help')
     </shell>
     <descr>Python Library</descr>
     <menu>/Help/Python/Python Library</menu>
     <category>Scripts</category>
  </documentation_file>"""

        if has_pygtk:
            XML += """
  <documentation_file>
     <name>http://www.pygtk.org/pygtk2tutorial/index.html</name>
     <descr>PyGTK tutorial</descr>
     <menu>/Help/Python/PyGTK Tutorial</menu>
     <category>Scripts</category>
  </documentation_file>
  <documentation_file>
     <name>http://www.pygtk.org/pygtk2reference/index.html</name>
     <descr>PyGTK Reference Manual</descr>
     <menu>/Help/Python/PyGTK Reference Manual</menu>
     <category>Scripts</category>
  </documentation_file>""" % (regex)

        GPS.parse_xml(XML)

        gps_utils.make_interactive(
            callback=self.show_python_library,
            name='display python library help')

        gps_utils.make_interactive(
            callback=self.reload_file,
            name='reload python file',
            filter='Python file',
            contextual='Python/Import & Reload')

        self.pydoc_proc = None
        GPS.Hook("project_view_changed").add(self._project_recomputed)
        GPS.Hook("before_exit_action_hook").add(self._before_exit)

    def reload_file(self):
        """
Reload the currently edited file in python.
If the file has not been imported yet, import it initially.
Otherwise, reload the current version of the file.
        """

        try:
            file = GPS.current_context().file()
            module = os.path.splitext(os.path.basename(file.name()))[0]

            ## The actual import and reload must be done in the context of the
            ## GPS console so that they are visible there. The current function
            ## executes in a different context, and would not impact the GPS
            ## console as a result otherwise.

            ## We cannot use  execfile(...), since that would be the equivalent
            ## of "from ... import *", not of "import ..."

            if module in sys.modules:
                GPS.exec_in_console("reload(sys.modules[\"" + module + "\"])")

            else:
                try:
                    sys.path.index(os.path.dirname(file.name()))
                except:
                    sys.path = [os.path.dirname(file.name())] + sys.path
                mod = __import__(module)

                # This would import in the current context, not what we want
                # exec (compile ("import " + module, "<cmdline>", "exec"))

                ## The proper solution is to execute in the context of the GPS
                ## console
                GPS.exec_in_console("import " + module)
        except:
            pass   # Current context is not a file

    def _project_recomputed(self, hook_name):
        """
if python is one of the supported language for the project, add various
predefined directories that may contain python files, so that shift-F3
works to open these files as it does for the Ada runtime
        """

        GPS.Project.add_predefined_paths(
            sources="%splug-ins" % GPS.get_home_dir())
        try:
            GPS.Project.root().languages(recursive=True).index("python")
            # The rest is done only if we support python
            GPS.Project.add_predefined_paths(sources=os.pathsep.join(sys.path))
        except:
            pass

    def show_python_library(self):
        """Open a navigator to show the help on the python library"""
        base = port = self.port_pref.get()
        if not self.pydoc_proc:
            while port - base < 10:
                self.pydoc_proc = GPS.Process("pydoc -p %s" % port)
                out = self.pydoc_proc.expect(
                    "pydoc server ready|Address already in use", 10000)
                try:
                    out.rindex(   # raise exception if not found
                        "Address already in use")
                    port += 1
                except:
                    break

        GPS.HTML.browse("http://localhost:%s/" % port)

    def _before_exit(self, hook_name):
        """Called before GPS exits"""
        if self.pydoc_proc:
            self.pydoc_proc.kill()
            self.pydoc_proc = None
        return 1


class PythonTracer(object):
    """ Basic python tracer, useful for GPS plugin development and debug.
    """

    def __init__(self):
        self.logger = GPS.Logger("Python_Tracer")
        self.prev_trace = sys.gettrace()
        sys.settrace(self.trace)

    def __del__(self):
        sys.settrace(self.prev_trace)

    def trace(self, frame, event, arg):
        filename, lineno = frame.f_code.co_filename, frame.f_lineno
        self.logger.log("%s:%s:%s:%s" % (filename, lineno, event, arg))

# Create the class once GPS is started, so that the filter is created
# immediately when parsing XML, and we can create our actions.
GPS.Hook("gps_started").add(lambda h: Python_Support())
