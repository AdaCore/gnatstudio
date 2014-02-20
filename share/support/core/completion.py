"""
Implements completion resolvers.

A completion resolver is an object that provides completion proposals in
the completion window in source editors.

To implement a new completion resolver:

  - derive from the class CompletionResolver
    and override the method get_completions

  - register your class using GPS.Completion.Register

"""


class CompletionProposal(object):
    """ A completion proposal.
    """

    def __init__(self, name, label, documentation,
                 icon_name="", action_name=""):
        """
            Creates a Completion Proposal
           name: the
        """

        self.name = name
        self.label = label
        self.documentation = documentation
        self.icon_name = icon_name
        self.action_name = action_name

    def get_data_as_list(self):
        return [self.name, self.label, self.documentation,
                self.icon_name, self.action_name]


class CompletionResolver(object):

    def __init__(self):
        pass

    def get_completions(self, loc):
        """
            Return a list of CompletionProposals.
            loc is an EditorLocation, pointing to the point at which the
            completion is occurring.
        """
        return []
