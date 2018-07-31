"""
Implements completion resolvers.

A completion resolver is an object that provides completion proposals in
the completion window in source editors.

To implement a new completion resolver:

  - derive from the class CompletionResolver
    and override the method get_completions

  - register your class using GPS.Completion.Register
1
Example
-------

This shows how to create two completion resolvers: a simple one which just
returns a list of completion proposals, and one which is a function
(supposedly more costly) which returns completions proposals one by one.

    import GPS
    from modules import Module
    from completion import CompletionResolver, CompletionProposal
    import time

    # A simple resolver

    class Simple_Resolver(CompletionResolver):
        def __init__(self):
            pass

        def get_completions(self, loc):
            return [CompletionProposal("cat", "cat", "description of cat"),
                    CompletionProposal("dog", "dog", "description of dog")]


    # An advanced resolver

    class Advanced_Resolver(CompletionResolver):

        def __init__(self):
            pass

        def get_completions(self, loc):
            current = 1
            while current < 1000:
                yield CompletionProposal(
                   "completion %s" % current,
                   "item %s" % current,
                   "documentation for item %s" % current)
                current += 1


    # Register both resolvers

    GPS.Completion.register(Simple_Resolver(), 'Ada')
    GPS.Completion.register(Advanced_Resolver(), 'Ada')

"""

import text_utils

CAT_UNKNOWN = 1
CAT_PACKAGE = 2
CAT_NAMESPACE = 3
CAT_TASK = 4
Cat_PROCEDURE = 5
CAT_FUNCTION = 6
CAT_METHOD = 7
CAT_CONSTRUCTOR = 8
CAT_DESTRUCTOR = 9
CAT_PROTECTED = 10
CAT_ENTRY = 11
CAT_CLASS = 12
CAT_STRUCTURE = 13
CAT_CASE_INSIDE_RECORD = 14
CAT_UNION = 15
CAT_TYPE = 16
CAT_SUBTYPE = 17
CAT_VARIABLE = 18
CAT_LOCAL_VARIABLE = 19
CAT_PARAMETER = 20
CAT_DISCRIMINANT = 21
CAT_FIELD = 22
CAT_LITERAL = 23
CAT_REPRESENTATION_CLAUSE = 24
CAT_WITH = 25
CAT_USE = 26
CAT_INCLUDE = 27
CAT_LOOP_STATEMENT = 28
CAT_IF_STATEMENT = 29
CAT_CASE_STATEMENT = 30
CAT_SELECT_STATEMENT = 31
CAT_ACCEPT_STATEMENT = 32
CAT_DECLARE_BLOCK = 33
CAT_RETURN_BLOCK = 34
Cat_Simple_Block = 35
CAT_EXCEPTION_HANDLER = 36
CAT_PRAGMA = 37
CAT_ASPECT = 38
CAT_CUSTOM = 39


def to_completion_point(ed_loc):
    """
       Find the beginning of the word currently being completed.
       Word = [a-zA-Z0-9_]
    """
    loc = text_utils.forward_until(
            ed_loc.forward_char(-1),
            lambda c: not (c.isalpha() or c.isdigit() or c == "_"),
            backwards=True,
        ).forward_char()
    # Special case for a word beginning at (1, 1)
    if ed_loc.line() == 1 and loc.column() == ed_loc.column():
        loc = loc.beginning_of_line()
    return loc


class CompletionProposal(object):

    """ A completion proposal.
    """

    def __init__(self, name, label, documentation,
                 icon_name="", action_name="", language_category=CAT_CUSTOM):
        """
            Creates a Completion Proposal.
            The following fields can be defined:
                - name: the actual string which will be inserted in the editor
                        if this proposal is chosen
                - label: the label to be used in the list in the completion
                        window. For a short text completion, this should be
                        the same as name.
                - documentation: text appearing in the documentation window
                        when this proposal is selected in the list.
                        In pango markup language.
                - icon_name: (optional) the name of a named icon in the icon
                        theme.
                - action_name: (optional) the name of an action to execute
                        when this proposal is selected.
        """

        self.name = name
        self.label = label
        self.documentation = documentation
        self.icon_name = icon_name
        self.action_name = action_name
        self.language_category = language_category

    def get_data_as_list(self):
        return [self.name, self.label, self.documentation,
                self.icon_name, self.action_name, self.language_category]


class SimpleCompletionResolver(object):

    def __init__(self):
        pass

    def get_completions(self, loc):
        """
            Return a list of CompletionProposals.
            loc is an EditorLocation, pointing to the point at which the
            completion is occurring.
        """
        return []


class CompletionResolver(object):

    def __init__(self):
        pass

    def get_completion_prefix(self, loc):
        """
           Return the current replacement prefix, as a string.
           For instance when completing
             foo (bl
           then this should return "bl", indicating that the completions
           being proposed will complete a word starting with "bl"

           The engine will call get_completion_prefix before get_completions,
           so you may cache the result of get_completion_prefix if you need.
        """
        return ""

    def get_completions(self, loc):
        """
            Return an iterable object returning iteration proposals.
            loc is an EditorLocation, pointing to the point at which the
            completion is occurring.
        """
        return []

    def _ada_get_completions(self, loc):
        """ Binding function, do not override """
        return AdaIterableWrapper(self.get_completions(loc))


class AdaIterableWrapper(object):

    """
        This object wraps around a python iterable object, and provides
        binding to the Ada completion iterator. For internal use only.
    """

    def __init__(self, iterable):
        self.iterable = iterable
        self.iterator = None

    def _ada_first(self):
        if self.iterator:
            self.iterator.rewind()
        if not self.iterator:
            self.iterator = AdaIteratorWrapper(self.iterable.__iter__())

        return self.iterator


class AdaIteratorWrapper(object):

    """ Binding to a Python iterator, for internal use only. """

    def __init__(self, iterator):
        self.iterator = iterator
        self.current = None
        self.at_end = False
        self.cached_results = []
        # this contains the results that we have already visited

        self.index_in_cache = -1
        # Index of the current item in the cache, positive only
        # when we are re-visiting.

        # The python iterator mechanism only knows we're at end when a call to
        # next() raises an exception. To map to the Ada behavior, we do one
        # call to next() ahead of time and cache the result.
        self._ada_next()

    def rewind(self):
        if self.cached_results:
            self.index_in_cache = 0

    def _ada_at_end(self):
        return self.at_end

    def _ada_get(self):
        return self.current

    def _ada_next(self):
        if self.index_in_cache < 0:
            # This means that we are not currently visiting the cache

            try:
                self.current = self.iterator.next()
            except StopIteration:
                self.at_end = True
                self.current = None
        else:
            # We are currently visiting the cache

            if self.index_in_cache < len(self.cached_results):
                # We are currently navigating within the cache

                self.current = self.cached_results[self.index_in_cache]
                self.index_in_cache += 1
            else:
                # we had been navigating in the cache but reached the end:
                # inform that we are no longer navigating the cache, and
                # call again to visit using the iterator.

                self.index_in_cache = -1
                self._ada_next()
