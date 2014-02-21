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
            """ complete with start_number """
            current = 1
            while current < 1000:
                yield CompletionProposal(
                   "completion %s" % current,
                   "item %s" % current,
                   "documentation for item %s" % current)
                current += 1


    # Register both resolvers

    GPS.Completion.register(Simple_Resolver())
    GPS.Completion.register(Advanced_Resolver())

"""


class CompletionProposal(object):
    """ A completion proposal.
    """

    def __init__(self, name, label, documentation,
                 icon_name="", action_name=""):
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
                - icon_name: (optional) the name of a stock icon
                - action_name: (optional) the name of an action to execute
                        when this proposal is selected.
        """

        self.name = name
        self.label = label
        self.documentation = documentation
        self.icon_name = icon_name
        self.action_name = action_name

    def get_data_as_list(self):
        return [self.name, self.label, self.documentation,
                self.icon_name, self.action_name]


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
        self.iterator = iterable.__iter__()

    def _ada_first(self):
        return AdaIteratorWrapper(self.iterator.__iter__())


class AdaIteratorWrapper(object):
    """ Binding to a Python iterator, for internal use only. """

    def __init__(self, iterator):
        self.iterator = iterator
        self.current = None
        self.at_end = False

        # the python iterator mechanism only knows we're at end when a call to
        # next() raises an exception. To map to the Ada behavior, we do one
        # call to next() ahead of time and cache the result.
        self._ada_next()

    def _ada_at_end(self):
        return self.at_end

    def _ada_get(self):
        return self.current

    def _ada_next(self):
        try:
            self.current = self.iterator.next()
        except StopIteration:
            self.at_end = True
            self.current = None
