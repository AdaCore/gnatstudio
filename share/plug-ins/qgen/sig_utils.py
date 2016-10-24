class Signal(object):
    """
    A class describing a signal element and its styling attributes
    """

    def __init__(self, id):
        self.id = id
        self.watched = False
        self.logged = False
        self.style = ''

    def compute_style(self):
        """
        Creates the name of the style to apply to the signal. The corresponding
        style will be described in mdl2json.tmplt
        """
        self.style = ''

        if self.watched:
            self.style = '_watchpoint'
        if self.logged:
            self.style += '_logpoint'

    def reset(self):
        self.logged = False
        self.watched = False
