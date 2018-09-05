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
# No user customization below this line
############################################################################

# To be added (from idle environment)
#   - "indent region", "dedent region", "check module", "run module"
#   - "class browser" -> project view in GPS

import GPS
import sys
import ast
import os.path
import gps_utils
import os_utils
from constructs import CAT_FUNCTION, VISIBILITY_PUBLIC, CAT_PARAMETER, \
    VISIBILITY_PRIVATE, CAT_TYPE, CAT_LOOP_STATEMENT, CAT_IF_STATEMENT
import text_utils

try:
    from gi.repository import Gtk
    has_pygtk = 1
except ImportError:
    Gtk = None
    has_pygtk = 0


def get_last_body_statement(node):
    if hasattr(node, "body"):
        return get_last_body_statement(node.body[-1])
    else:
        return node


# noinspection PyPep8Naming
class ASTVisitor(ast.NodeVisitor):

    def __init__(self, bufstr, clist):
        self.buflines = bufstr.splitlines()
        self.lines_offsets = [0 for _ in self.buflines]

        for i in range(1, len(self.buflines)):
            self.lines_offsets[i] = (self.lines_offsets[i - 1] +
                                     len(self.buflines[i - 1]) + 1)

        self.clist = clist

    def get_offset(self, lineno, col):
        return self.lines_offsets[lineno - 1] + col + 1

    def make_tuple(self, line, col):
        return line, col, self.get_offset(line, col)

    def get_locations(self, n, kw=None):
        end_line = n.end_line
        start_pos = self.make_tuple(n.lineno, n.col_offset)
        end_col = len(self.buflines[end_line - 1])
        end_pos = self.make_tuple(end_line, end_col)
        if kw:
            # n.lineno value corresponds to the first line of the block.
            # I.E. if the function/class has decorators it will return the line
            # of the first decorator and not the line where the function/class
            # is declared.
            line = n.lineno
            for d in n.decorator_list:
                line = max(line, d.lineno)
            if n.decorator_list:
                line = line + 1
            entity_pos = self.make_tuple(line, n.col_offset + len(kw) + 1)
            return start_pos, end_pos, entity_pos
        else:
            return start_pos, end_pos

    @staticmethod
    def make_fn_profile(fn_node):
        args = fn_node.args
        argsd = (a.id for a in args.args)

        profile = "({0})".format(
            ", ".join(argsd) +
            (", %s" % args.vararg if args.vararg else "") +
            (", %s" % args.kwarg if args.kwarg else "")
        )

        return profile

    def visit_FunctionDef(self, n):
        self.generic_visit(n)
        start_pos, end_pos, entity_pos = self.get_locations(n, "def ")
        self.clist.add_construct(
            CAT_FUNCTION, False, VISIBILITY_PUBLIC, n.name, "",
            start_pos, end_pos, entity_pos
        )

    def visit_Name(self, node):
        start_pos = self.make_tuple(node.lineno, node.col_offset)
        if isinstance(node.ctx, ast.Param):
            self.clist.add_construct(
                CAT_PARAMETER, False, VISIBILITY_PRIVATE, node.id, "",
                start_pos, start_pos, start_pos
            )

    def generic_visit(self, n):
        ast.NodeVisitor.generic_visit(self, n)
        if getattr(n, "lineno", None):
            end_line = n.lineno
            for node_name in n._fields:
                _node = getattr(n, node_name)
                node = _node
                if isinstance(_node, list):
                    if not _node:
                        continue
                    node = _node[-1]
                end_line = max(end_line, getattr(node, "end_line", 0))
            n.end_line = end_line
        else:
            n.end_line = None

    def visit_ClassDef(self, n):
        self.generic_visit(n)
        start_pos, end_pos, entity_pos = self.get_locations(n, "class ")
        self.clist.add_construct(
            CAT_TYPE, False, VISIBILITY_PUBLIC, n.name, "",
            start_pos, end_pos, entity_pos
        )

    def add_private_construct(self, n, constructs_cat):
        self.generic_visit(n)
        start_pos, end_pos = self.get_locations(n)
        self.clist.add_construct(
            constructs_cat, False, VISIBILITY_PRIVATE, "", "",
            start_pos, end_pos, start_pos
        )

    def visit_While(self, n):
        self.add_private_construct(n, CAT_LOOP_STATEMENT)

    def visit_If(self, n):
        self.add_private_construct(n, CAT_IF_STATEMENT)

    def visit_For(self, n):
        self.add_private_construct(n, CAT_LOOP_STATEMENT)


# noinspection PyMethodMayBeStatic
class PythonLanguage(GPS.Language):

    def __init__(self):
        pass

    def parse_constructs(self, constructs_list, gps_file, string):
        del gps_file
        try:
            tree = ast.parse(string)
            tree.lineno = 0
            ASTVisitor(string, constructs_list).visit(tree)
        except SyntaxError:
            pass


class PythonSupport(object):

    def __init__(self):
        """
        Various initializations done before the gps_started hook
        """

        self.port_pref = GPS.Preference("Documentation:Python/port")
        self.port_pref.create(
            "Pydoc port", "integer",
            "Port that should be used when spawning the pydoc daemon. "
            "This is a small local server to which your web browser "
            "connects to display the documentation for the standard "
            "python library. It is accessed through the /Python menu when "
            "editing a python file",
            9432)

        # Add the language definition before the gps_started hook, so that
        # python files are correctly found

        GPS.Language.register(PythonLanguage(), "Python", ".py", "", ".pyc")
        XML = """
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
        """

        if Gtk:
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
            </documentation_file>
            """

        GPS.parse_xml(XML)

    def gps_started(self):
        """
        Initializations done after the gps_started hook
        """

        # This action requires pydoc
        if os_utils.locate_exec_on_path('pydoc'):
            gps_utils.make_interactive(
                callback=self.show_python_library,
                name='display python library help')

        gps_utils.make_interactive(
            callback=self.reload_file,
            name='reload python file',
            filter='Python file',
            contextual='Python/Import & Reload')

        gps_utils.make_interactive(
            callback=self.indent_on_new_line,
            name="Python Auto Indentation",
            filter='Python file')

        self.pydoc_proc = None
        GPS.Hook("project_view_changed").add(self._project_recomputed)
        GPS.Hook("before_exit_action_hook").add(self._before_exit)

    def indent_on_new_line(self):
        """
        This action parse the code (if it's python) and move cursor to
        the desired indentation level.
        """
        editor = GPS.EditorBuffer.get()
        start = editor.selection_start()
        end = editor.selection_end()

        # if a block is selected, delete the block
        if start.line() != end.line() or start.column() != end.column():
            editor.delete(start, end)

        # place the cursor at the head of a new line
        editor.insert(start, "\n")

        # do indentation
        self.python_parse_indent(editor, start)

    def python_parse_indent(self, e, start):
        """
           parse the text and predict python indentation when hitting return
           * return the indentation (int)
           * text is edited with cursor at indentation level after returned
           * end is position of cursor
        """

        def find_level(on_this_string):
            """
            Find relatively level change for a given string
            Return relative level change and prefix to search
            """

            # by default, no level change, no prefix to search
            level, group = 0, []

            # case : enter subprogram, innermost level decides
            if on_this_string.endswith(":"):
                level = 1
                group = ["if", "else", "for", "while",
                         "def", "class", "try", "except"]
            else:
                # case: return to a function, previous def decides
                if on_this_string.startswith("return"):
                    level = -2
                    group = ["def"]

                # case: break out loops, innermost loop decides
                if on_this_string.startswith("break") or \
                   on_this_string.startswith("continue"):
                    level = -1
                    group = ["for", "while"]

            return (level, group)

        source = e.get_chars(to=start).splitlines()

        # initialize
        last = source[0]
        end = start
        previous_indent = 0

        for i in range(len(source) - 1, -1, -1):
            # ident is same as last non-comment not-empty line, no level change
            tmpstring = source[i].lstrip(" ")
            if tmpstring != "" and not tmpstring.startswith("#"):
                last = source[i]
                previous_indent = len(last) - len(tmpstring)
                end = e.at(i + 1, 1).end_of_line()
                break

        # STEP 1 parse parenthesis
        level, group = 0, []

        result = text_utils.parse_parentheses(e, end=end)
        stack = result[0]

        # go to responsible parentheses' line, if exists
        if len(stack) > 0:
            last_loc = stack.pop()
            previous_indent = last_loc[1] + 1

            # closing a (), same line responsible for level change
            if result[1]:
                level, group = find_level(source[last_loc[0]].lstrip(" "))

        # else the last line is responsible
        else:
            tmphead = last.lstrip(" ")
            level, group = find_level(tmphead)

        # STEP 2 find prev indent quantity (number of whitespaces)
        prefix = ""
        begin = 0
        # only when level changes
        if level != 0:
            for i in range(end.line() - 1, -1, -1):
                for pref in group:
                    if source[i].lstrip(" ").startswith(pref):
                        begin = i
                        prefix = pref
                        break

                # if hit the prefix during loop, modify previous_indent
                if prefix is not "":
                    previous_indent = len(source[begin].split(prefix)[0])
                    break

        # STEP 3 find the correct indent number
        level = 0 if level < 0 else level
        indent = previous_indent + level * 4

        # STEP 4 do indentation and move the cursor
        e.insert(e.at(start.line() + 1, 1), " " * indent)
        e.main_cursor().move(e.at(start.line() + 1, indent + 1))
        return indent

    def reload_file(self):
        """
        Reload the currently edited file in python.
        If the file has not been imported yet, import it initially.
        Otherwise, reload the current version of the file.
        """

        try:
            f = GPS.current_context().file()
            module = os.path.splitext(os.path.basename(f.path))[0]

            # The actual import and reload must be done in the context of the
            # GPS console so that they are visible there. The current function
            # executes in a different context, and would not impact the GPS
            # console as a result otherwise.

            # We cannot use  execfile(...), since that would be the equivalent
            # of "from ... import *", not of "import ..."

            if module in sys.modules:
                GPS.exec_in_console("reload(sys.modules[\"" + module + "\"])")

            else:
                try:
                    sys.path.index(os.path.dirname(f.path))
                except Exception:
                    sys.path = [os.path.dirname(f.path)] + sys.path
                __import__(module)

                # This would import in the current context, not what we want
                # exec (compile ("import " + module, "<cmdline>", "exec"))

                # The proper solution is to execute in the context of the GPS
                # console
                GPS.exec_in_console("import " + module)
        except Exception:
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
        except Exception:
            pass

    def show_python_library(self):
        """Open a navigator to show the help on the python library"""
        base = port = self.port_pref.get()
        if not self.pydoc_proc:
            while port - base < 10:
                self.pydoc_proc = GPS.Process(["pydoc", "-p", "%s" % port])
                out = self.pydoc_proc.expect(
                    "pydoc server ready|Address already in use", 10000)
                try:
                    out.rindex(   # raise exception if not found
                        "Address already in use")
                    port += 1
                except Exception:
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
module = PythonSupport()
GPS.Hook("gps_started").add(lambda h: module.gps_started())
