"""
This is the python completion plugin for GPS.
Its aim is to integrate the jedi (python auto-completion API)
into GPS.
"""

import os
import sys
from itertools import chain

try:
    import jedi
except ImportError:
    # If import fails, add the manual install path of jedi to:
    # sys.path list.
    sys.path.append(os.path.join(os.path.dirname(__file__), "jedi"))
    import jedi

import GPS
from text_utils import forward_until
from completion import CompletionResolver, CompletionProposal
import completion
from modules import Module

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
        # additional directories that module search will perform
        self.source_dirs = set([])

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

            # check in the current directory
            sys_path_backup = list(sys.path)
            self.source_dirs.update([loc.buffer().file().directory()])
            sys.path = sys.path + list(self.source_dirs)

            # Feed Jedi API
            script = jedi.Script(
                source=loc.buffer().get_chars(),
                line=loc.line(),
                column=loc.column() - 1,
                )

            # Sort, filter results
            result = sorted((CompletionProposal(
                name=i.name,
                label=i.name,
                documentation=i.docstring(),
                language_category=TYPE_LABELS.get(
                    i.type, completion.CAT_UNKNOWN))
                for i in script.complete()
                if i.name.startswith(self.__prefix)),
                key=lambda d: d.name)

            # restore sys.path before exit
            sys.path = sys_path_backup
            return result

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


class Jedi_Module(Module):

    __resolver = PythonResolver()

    def __refresh_source_dirs(self):
        """
           Update resolver's source_dirs with user's working directory
        """
        self.__resolver.source_dirs = set(
            chain.from_iterable(i.source_dirs()
                                for i in [GPS.Project.root()] +
                                GPS.Project.root().dependencies()
                                if "python" in i.languages())
        )

    # The followings are hooks:

    def gps_started(self):
        """
           When GPS start, create and register a resolver
           and update its source dirs
        """
        GPS.Completion.register(self.__resolver)
        self.__refresh_source_dirs()

    def project_changed(self):
        """
           When project changes, prepare new source dirs for resolver
        """
        self.__refresh_source_dirs()

    def project_view_changed(self):
        """
           When project view changes, update source dirs for resolver
        """
        self.__refresh_source_dirs()
