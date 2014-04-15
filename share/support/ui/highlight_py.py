from highlight_common import *
from keyword import kwlist

hl_format_escapes = simple(r"\{\d+?\}", tag=tag_string_escapes)
string_hl = region_template(tag="string", highlighter=(hl_inside_strings,
                                                       hl_format_escapes))

register_highlighter(
    language="python",
    spec=(
        # Match multiline strings
        string_hl(r'"""', r'"""', name="multiline_string_sq"),
        string_hl(r"'''", r"'''", name="multiline_string_dq"),

        # Match string literals
        string_hl(r"'", r"'"),
        string_hl(r'"', r'"'),

        # Match comments lines
        region(r"#", "\n", tag="comment", highlighter=(hl_comment_notes,)),

        # Match number literals
        simple(r"\b[0-9]*\.?[0-9]+\b", tag="number"),

        # Match keywords
        words(kwlist, tag="keyword"),

        # Match self
        simple("self", tag=tag_comment_notes),

        # Match important constants and builtins
        words(["True", "False", "None", "int", "str", "buffer", "unicode",
               "list", "tuple", "bytearray", "xrange", "dict", "Ellipsis",
               "NotImplemented", "__debug__", "__doc__", "__file__",
               "__name__", "__package__"], tag="type"),

        words(["__import__", "abs", "all", "any", "apply", "bin", "callable",
               "classmethod", "cmp", "coerce", "compile", "delattr", "dir",
               "divmod", "enumerate", "eval", "execfile", "filter", "format",
               "getattr", "globals", "locals", "hasattr", "hash", "help",
               "hex",
               "id", "input", "intern", "isinstance", "issubclass", "iter",
               "len", "map", "max", "min", "next", "oct", "open", "ord", "pow",
               "property", "range", "xrange", "raw_input", "reduce", "reload",
               "repr", "reversed", "round", "setattr", "slice", "sorted",
               "staticmethod", "sum", "vars", "zip"], tag="block"),

        words(
            ["BaseException", "Exception", "StandardError", "ArithmeticError",
             "LookupError", "EnvironmentError", "AssertionError",
             "AttributeError", "BufferError", "EOFError", "FloatingPointError",
             "GeneratorExit", "IOError", "ImportError", "IndexError",
             "KeyError", "KeyboardInterrupt", "MemoryError", "NameError",
             "NotImplementedError", "OSError", "OverflowError",
             "ReferenceError", "RuntimeError", "StopIteration", "SyntaxError",
             "IndentationError", "TabError", "SystemError", "SystemExit",
             "TypeError", "UnboundLocalError", "UnicodeError",
             "UnicodeEncodeError", "UnicodeDecodeError",
             "UnicodeTranslateError", "ValueError", "VMSError", "WindowsError",
             "ZeroDivisionError", "Warning", "UserWarning", "BytesWarning",
             "DeprecationWarning", "PendingDepricationWarning",
             "SyntaxWarning", "RuntimeWarning", "FutureWarning",
             "ImportWarning", "UnicodeWarning"], tag="type")
    )
)
