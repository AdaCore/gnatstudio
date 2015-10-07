def register_tag_handler(handler):
    """
    Registers instance of custom tag handler.
    """
    pass  # implemented in Ada


class InlineTagHandler:

    """
    This class is intended to be used as base class for custom tag handlers of
    inline tags.
    """

    def __init__(self, name):
        """
        Initialize base class

        :param string name: Name of custom tag
        """

        pass  # implemented in Ada

    def has_parameter(self):
        """
        Should returns True when custom tag has parameter. Default
        implementation returns False.

        :return: Should or should not additional parameter be parsed and passes
          to custom tag handler.
        """
        pass  # implemented in Ada

    def to_markup(self, writer, parameter):
        """
        Do custom tag processing and use writer to generate documentation.

        :param MarkupGenerator writer: Writer to be used to generate
          documentation.
        :type writer: :class:`GPS.MarkupGenerator`
        :param string parameter: Additional parameter of custom tag provided in
          source code.
        """
        pass  # implemented in Ada


class MarkupGenerator:
    """
    Used by custom tag handler to generate structured output.
    """

    def start_paragraph(attributes):
        """
        Opens paragraph in generated documentation.
        :func:`GPS.MarkupGenerator.end_paragraph` must be called to close
        paragraph.

        Paragraphs can be nested and can contain lists and text too.

        :param dictionary attributes: Additional attributes to be passed to
          backend.
        """
        pass  # implemented in Ada

    def end_paragraph():
        """
        Closes paragraph in generated documentation.
        """
        pass  # implemented in Ada

    def start_list(attributes):
        """
        Opens list in generated documentation.
        func:`GPS.MarkupGenerator.end_list` must be called to close list.

        Lists can contains list items only.

        :param dictionary attributes: Additional attributes to be passed to
          backend.
        """
        pass  # implemented in Ada

    def end_list():
        """
        Closes list in generated documentation.
        """
        pass  # implemented in Ada

    def start_list_item(attributes):
        """
        Opens item of the list in generated documentation.
        func:`GPS.MarkupGenerator.end_list_item` must be called to close list
        item.

        List item can contain paragraph, lists and text.

        :param dictionary attributes: Additional attributes to be passed to
          backend.
        """
        pass  # implemented in Ada

    def end_list_item():
        """
        Closes list item in generated documentation.
        """
        pass  # implemented in Ada

    def text(text, attributes):
        """
        Outputs test to generated documentation.

        :param string text: Text to be output in generated documentation.
        :param dict attributes: Additional attributes to be passed to
          backend.
        """
        pass  # implemented in Ada

    def html(html):
        """
        Outputs HTML markup to generated documentation. This markup is
        processed by browser to display its content.

        Note, this function is intended to be used for HTML backend only.

        :param string html: HTML markup to be output in generated
          documentation.
        """
        pass  # implemented in Ada

    def generate_after_paragraph():
        """
        Switch MarkupGenerator to generate output after processing of paragraph
        where custom tag is used.
        """
        pass  # implemented in Ada

    def generate_inline():
        """
        Switch MarkupGenerator to generate output in place where custom tag is
        used.
        """
        pass  # implemented in Ada
