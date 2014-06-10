"""
This is the python completion plugin for GPS.
Its aim is to integrate the jedi (python auto-completion API)
into GPS.
"""

try:
    import jedi
except ImportError:
    # If import fails, add the manual install path of jedi to:
    # sys.path list.
    import os
    import sys
    sys.path.append(os.path.join(os.path.dirname(__file__), "jedi"))
    import jedi

import GPS
from GPS import EditorBuffer
from text_utils import forward_until
from completion import CompletionResolver, CompletionProposal
import completion


TYPE_LABELS = {
    # Global hash tabel for pop-out label icons
    # Values are constants defined in completion.py
    "function": completion.CAT_FUNCTION,
    "keyword": completion.CAT_TYPE,
    "instance": completion.CAT_STRUCTURE,
    "class": completion.CAT_CLASS,
    "module": completion.CAT_NAMESPACE,
    "statement": completion.CAT_DECLARE_BLOCK
}


class PythonResolver(CompletionResolver):
    """
       The Python Resolver class that inherits completion.CompletionResolver.
    """
    def __init__(self):
        self.__prefix = None

    def get_completions(self, loc):
        """
           Overriden method.
           Returns a list of completion objects for GPS.
        """

        if loc.buffer().file().language() == "python":
            # Only parse the source text when:
            # The curretly editing buffer is of language "python"
            # This ensures that the resolver returns completion object
            # only for python file.

            # Feed Jedi API
            script = jedi.Script(
                source=loc.buffer().get_chars(),
                line=loc.line(),
                column=loc.column()-1)

            # Sort, filter and return results
            return sorted((CompletionProposal(
                name=i.name,
                label=i.name,
                documentation=i.docstring(),
                language_category=TYPE_LABELS.get(
                    i.type, completion.CAT_UNKNOWN))
                for i in script.complete()
                if i.name.startswith(self.__prefix)),
                key=lambda d: d.name)

        return []

    def get_completion_prefix(self, loc):
        """
           Overriden method.
           Prefix is the head of the word to be completed.

              Prefix+complete = word

           Prefix is stored in a field.
        """
        beginning = completion.to_completion_point(loc)
        self.__prefix = loc.buffer().get_chars(beginning, loc)
        self.__prefix = self.__prefix.strip("\n")
        return self.__prefix


# Create an instance of the python resolver class and register it with GPS
tr = PythonResolver()
GPS.Completion.register(tr)
