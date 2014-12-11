from __future__ import absolute_import

from copy import copy
from xml.sax.saxutils import escape


class X(object):

    """
    Class to construct and print XML nodes in python syntax. Here is an
    example of use::

        X("foo", bar="baz").children(
            X("qux", pouet="kanasson"),
            "bar<>",
        )

    will render::

        <foo bar="baz">
            <qux pouet="kanasson" />
            bar&lt;&gt;
        </foo>
    """

    def __init__(self, tag_name, *args, **kwargs):
        """
        Construct a new node

        :param string tag_name: the name of the XML tag
        :param kwargs: attributes of the tag
        :type kwargs: dict[string, string]

        """
        self.tag_name = tag_name
        self.kws = {k: w for k, w in kwargs.items() if w is not None}
        self._children = list(args)

    def children(self, *args):
        """
        Returns a new version of self with args as the list of children to
        the node.

        If passed only one argument that is an iterable, will add the
        content of it to X. Else, will add every passed arg to X.

        :rtype: X
        """
        c = copy(self)
        if len(args) == 1 and not isinstance(args[0], str):
            try:
                c._children = c._children + list(args[0])
            except TypeError:
                c._children = c._children + list(args)
        else:
            c._children = c._children + list(args)
        return c

    def params(self, **kwargs):
        """
        Returns a new version of self with kwargs as new parameters to the
        node

        :param kwargs: The parameters to add
        :type kwargs: dict[string, string]
        :rtype: X
        """
        c = copy(self)
        c.kws = c.kws.copy()
        c.kws.update({k: w for k, w in kwargs.items() if w is not None})
        return c

    @staticmethod
    def child_to_str(child):
        """
        Returns the seriazilation of `child`. Handle both XML nodes and string
        ones.
        """
        return escape(child) if isinstance(child, str) else str(child)

    def __str__(self):
        """
        String representation of self. Used to get the XML repr

        :rtype: string
        """
        kws = " ".join('{0}="{1}"'.format(k, v) for k, v in self.kws.items())
        if self._children:
            return "<{0} {1}>{2}</{0}>".format(
                self.tag_name, kws,
                "\n".join(self.child_to_str(c) for c in self._children)
            )
        else:
            return "<{0} {1} />".format(self.tag_name, kws)

    def with_header(self):
        """
        String representation of self with xml header added

        :rtype: string
        """
        return '<?xml version="1.0"?>\n' + self.__str__()
