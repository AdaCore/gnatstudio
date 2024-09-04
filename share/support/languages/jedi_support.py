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
    "statement": completion.CAT_DECLARE_BLOCK,
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
        Overridden method.
        Returns a list of completion objects for GPS.
        """

        # this works only on Python files
        if loc.buffer().file().language() != "python":
            return []

        # check if the current char can belong to an identifier
        current_char = loc.forward_char(-1).get_char()
        if not (
            current_char and (current_char in ["_", "."] or current_char.isalnum())
        ):
            return []

        # We can't rely on sys.path and must create a jedi.Project see
        # extract from the online doc below:
        #    If project is provided with a sys_path, that is going to be used.
        #    If environment is provided, its sys.path will be used
        #    (see Environment.get_sys_path);
        #    Otherwise sys.path will match that of the default environment of
        #    Jedi, which typically matches the sys path that was used at the
        #    time when Jedi was imported.

        self.source_dirs.update([loc.buffer().file().directory()])
        project = jedi.Project(None, sys_path=sys.path + list(self.source_dirs))

        try:
            # filter out ^L in source text
            text = loc.buffer().get_chars()
            # text = text.replace('\x0c', ' ')
            # Feed Jedi API
            script = jedi.Script(code=text, project=project)
            completions = script.complete(line=loc.line(), column=loc.column() - 1)

            # Sort, filter results
            result = sorted(
                (
                    CompletionProposal(
                        name=i.name,
                        label=i.name,
                        documentation=i.docstring(),
                        language_category=TYPE_LABELS.get(
                            i.type, completion.CAT_UNKNOWN
                        ),
                    )
                    for i in completions
                    if i.name.startswith(self.__prefix)
                ),
                key=lambda d: d.name,
            )
        except:
            jedi_log = GPS.Logger("JEDI_PARSING")
            jedi_log.log("jedi fails to parse:" + loc.buffer().file().path)
            result = []

        return result

    def get_completion_prefix(self, loc):
        """
        Overridden method.
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
        try:
            self.__resolver.source_dirs = set(
                chain.from_iterable(
                    i.source_dirs()
                    for i in [GPS.Project.root()] + GPS.Project.root().dependencies()
                    if "python" in i.languages()
                )
            )
        except:
            GPS.Logger("JEDI_SUPPORT").log("Dependencies are not processed")

    # The followings are hooks:

    def setup(self):
        """
        When GPS start, create and register a resolver
        and update its source dirs
        """
        GPS.Completion.register(self.__resolver, "python")

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
