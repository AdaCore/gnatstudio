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

    GPS.Completion.register(Simple_Resolver())
    GPS.Completion.register(Advanced_Resolver())

"""


import GPS

Cat_Unknown = 1
Cat_Package = 2
Cat_Namespace = 3
Cat_Task = 4
Cat_Procedure = 5
Cat_Function = 6
Cat_Method = 7
Cat_Constructor = 8
Cat_Destructor = 9
Cat_Protected = 10
Cat_Entry = 11
Cat_Class = 12
Cat_Structure = 13
Cat_Case_Inside_Record = 14
Cat_Union = 15
Cat_Type = 16
Cat_Subtype = 17
Cat_Variable = 18
Cat_Local_Variable = 19
Cat_Parameter = 20
Cat_Discriminant = 21
Cat_Field = 22
Cat_Literal = 23
Cat_Representation_Clause = 24
Cat_With = 25
Cat_Use = 26
Cat_Include = 27
Cat_Loop_Statement = 28
Cat_If_Statement = 29
Cat_Case_Statement = 30
Cat_Select_Statement = 31
Cat_Accept_Statement = 32
Cat_Declare_Block = 33
Cat_Return_Block = 34
Cat_Simple_Block = 35
Cat_Exception_Handler = 36
Cat_Pragma = 37
Cat_Aspect = 38
Cat_Custom = 39


class CompletionProposal(object):
    """ A completion proposal.
    """

    def __init__(self, name, label, documentation,
                 icon_name="", action_name="", language_category=Cat_Custom):
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
