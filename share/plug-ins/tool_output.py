"""This plug-in provides base class for custom output parser.

An example:

import GPS, tool_output

class PopupParser(tool_output.OutputParser):
    def on_stdout(self,text):
        GPS.MDI.dialog (text)
        if self.child != None:
            self.child.on_stdout (text)

To add custom parser to a target insert next line to target desription:

<output-parsers>[default] popupparser</output-parsers>
</target>
"""


#############################################################################
## No user customization below this line
#############################################################################

class OutputParserMetaClass(type):
    registered = dict()   # list of registered parsers

    def __new__(cls, name, bases, attrs):
        new_class = type.__new__(cls, name, bases, attrs)
        OutputParserMetaClass.registered[new_class.get_name()] = new_class
        return new_class

    def get_name(self):
        """Return the name of the parser, either from a "name" class
            attribute, or from the class name
        """
        return getattr(self, 'name', self.__name__).lower()

class OutputParser(object):
    __metaclass__ = OutputParserMetaClass

    def __init__(self,child):
        self.child = child

    def on_stdout(self,text):
        if self.child != None:
            self.child.on_stdout (text)

    def on_stderr(self,text):
        if self.child != None:
           self.child.on_stderr (text)

    def on_exit(self):
        if self.child != None:
            self.child.on_exit ()

def create_parser(name, child=None):
    if OutputParserMetaClass.registered.has_key (name):
        return OutputParserMetaClass.registered[name](child)
    else:
        return None
