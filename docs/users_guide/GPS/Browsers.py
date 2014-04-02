"""
Interface to the graph drawing API in GPS.
"""

import GPS


def enum(**enums):
    return type('Enum', (), enums)


class Style(object):
    """
    This class provides multiple drawing attributes that are used when
    drawing on a browser. This includes colors, dash patterns, arrows
    and symbols,...
    Very often, such a style will be reused for multiple objects.
    """

    Arrow = enum(NONE=0, OPEN=1, SOLID=2, DIAMOND=3)
    Symbol = enum(NONE=0, CROSS=1, STRIKE=2, DOUBLE_STRIKE=3)
    Underline = enum(NONE=0, SINGLE=1, DOUBLE=2, LOW=3)
    Align = enum(LEFT=0, MIDDLE=1, RIGHT=2)
    # Keep in sync with gps_utils/__init__.py

    def __init__(
        self,
        stroke="black",
        fill="",
        lineWidth=1.0,
        dashes=[],
        sloppy=False,
        fontName="sans 9",
        fontUnderline=Underline.NONE,
        fontStrike=False,
        fontColor="black",
        fontLineSpacing=0,
        fontHalign=Align.LEFT,
        fontValign=0.0,
        arrowFromHead=Arrow.NONE,
        arrowFromLength=8.0,
        arrowFromAngle=0.4,
        arrowFromStroke="black",
        arrowFromFill="",
        arrowToHead=Arrow.NONE,
        arrowToLength=8.0,
        arrowToAngle=0.4,
        arrowToStroke="black",
        arrowToFill="",
        symbolFromName=Symbol.NONE,
        symbolFromStroke="black",
        symbolFromDist=16.0,
        symbolToName=Symbol.NONE,
        symbolToStroke="black",
        symbolToDist=16.0
    ):
        """
        Constructs a new style.
        This function takes a very large number of parameters, but they are
        all optional. Not all of them apply to all objects either.

        :param stroke: A string describing the color to use to draw lines.
           The format is one of "rgb(255,255,255)", "rgba(255,255,255,1.0)"
           or "#123456". Transparency is supported when using rgba.

        :param fill: A string describing how closed objects should be filled.
           The format is either a color, as for stroke, or a string describing
           a gradient as in the following:

               "linear x0 y0 x1 y1 offset1 color1 offset2 color2 ..."

           where (x0,y0) and (x1,y1) define the orientation of the gradient.
           It is recommended that they are defined in the range 0..1, since
           the gradient will be automatically transformed to extend to the
           whole size of the object. The rest of the parameters are used to
           define each color the gradient is going through, and the offset
           (in the range 0..1) where the color must occur. For instance:

               "linear 0 0 1 1 0 rgba(255,0,0,0.2) 1 rgba(0,255,0,0.2)"

        :param lineWidth: A float, the width of a line

        :param dashes: A list of floats, which define a dash pattern. The
           first number is the number of pixels which should get ink, then
           the number of transparent pixels, then again the number of inked
           pixels,... The pattern will automatically repeat itself.

        :param sloppy: A boolean; when this is true, no straight line is
           displayed. They are instead approximated with curves. Combined with
           a hand-drawing font, this makes the display look as if it has been
           hand-drawn.

        :param fontName: A string describing the font to use and its size.

        :param fontUnderline: The underline to use for the text. Values should
           be from GPS.Browsers.Style.Underline.

        :param fontStrike: A boolean, whether to strikethrough the text.

        :param fontColor: A string describing the color to use for text.

        :param fontLineSpacing: An integer, an extra number of pixels to
           insert between each lines of text.

        :param fontHalign: How text should be aligned horizontally within
           its bounding box. Values should be from GPS.Browsers.Style.Align.

        :param fontValign: How text should be aligned vertically within
           its bounding box. This is a float in the range 0.0 .. 1.0, where
           0 is the top and 1 is the bottom.

        :param arrowFromHead: How should arrows be displayed on the origin
           of a line. Values should be from GPS.Browsers.Style.Arrow.

        :param arrowFromLength: A float, the size of an arrow.

        :param arrowFromAngle: A float, the angle for an arrow.

        :param arrowFromStroke: A string describing the stroke color for the
           arrow.

        :param arrowFromFill: A string describing the fill pattern for the
           arrow.

        :param arrowToHead: same as arrowFromHead, but for the end of a line
        :param arrowToLength: same as arrowFromLength for the end of the line
        :param arrowToAngle: same as arrowFromAngle for the end of the line
        :param arrowToStroke: same as arrowFromStroke for the end of the line
        :param arrowToFill: same as arrowFromFill for the end of the line

        :param symbolFromName: the extra symbol to display near the origin
           of a line. Values should be from GPS.Browsers.Style.Symbol

        :param symbolFromStroke: the stroke color to use for the symbol.
        :param symbolFromDist: a float, the distance from the end of the line
           at which the symbol should be displayed.
        """


class Item(object):
    """
    This abstract class represents any of the items that can be displayed in
    a browser.
    Such items have an outline, whose form depends on the type of the item
    (rectangular, polygone,...)
    They can almost all contain children. Unless you specified an explicit
    size for the item, its size will be computed to include all the children.
    The children are stacked either vertically or horizontally within their
    container, so that one child appears by default immediately below or to
    the right of the previous one.
    Extra margins can be specified to force extra space.
    """

    Float = enum(NONE=0, START=1, END=2)
    Align = enum(START=0, MIDDLE=1, END=2)
    Overflow = enum(PREVENT=0, HIDE=1)

    def __init__(self):
        """
        Will raise an exception, this is an abstract class.
        """

    def set_position(self, x, y):
        """
        Indicates the position of the item. This is the position within its
        parent item, or if there is no parent this is the absolute position
        of the item within the diagram.
        Calling this function should always be done for toplevel items, but
        is optional for children, since their position is computed
        automatically by their container (which is especially useful with
        text items, whose size might be hard to compute).

        :param x: a float
        :param y: a float
        """

    def add(self,
            item,
            align=Align.START,
            margin=(0, 0, 0, 0),
            float=Float.NONE,
            overflow=Overflow.PREVENT):
        """
        Add a child item.
        This child will be displayed as part of the item, and will move with
        it. The size of the child will impact the size of its parent, unless
        you have forced a specific size for the latter.

        :param item: A GPS.Browsers.Item instance

        :param align: How the item should be aligned within its parent. When
           the size of the child is computed automatically, it will have the
           same width as its parent (for vertical layout) or the same height
           as its parent (for horizontal layout), minus the margins. In this
           case, the align parameter will play no role. But when the child is
           smaller than its parent, the align parameter indicates on which
           side an extra margin is added.
           Use GPS.Browsers.Item.Align for the values.

        :param margin: Extra margin to each side of the item (resp. top,
           right, bottom and left margin). These a float values.

        :param float: Whether the child should be floating within its parent.
           This impacts the layout: by default, in a vertical layout, all
           children are put below one another, so that they do not overlap.
           However, when a child is floating, it will be put at the current
           y coordinate, but the next item will be put at the same coordinate
           as if the child was not there.

        :param overflow: Whether the child's size should impact the parent
           size. For instance, you might want to display as much text as
           possible in a box. When overflow is set to PREVENT, the box
           will be made as large as needed to have the whole text visible
           (unless you have specified an explicit size for the box, as
           usual). But when the overflow is set to HIDE, the box will
           get its size from the other children, and the text will simply
           be ellipsized if it does not fit in the box.
        """


class RectItem(Item):
    """
    A special kind of item, which displays a rectangular box, optionally
    with rounded corner. It can contain children items.
    """

    def __init__(self, style, width=-1.0, height=-1.0, radius=0.0):
        """
        Creates a new rectangular item.

        :param style: An instance of GPS.Browsers.Style

        :param width: used to force a specific width for the item.
           If this is null or negative, the width will be computed from
           the children of the item.

        :param height: used to force a specific height, which will be
           computed automatically if height is negative or null

        :param radius: a float, the radius for the angles.
        """


class EllipseItem(Item):
    """
    An item which displays an ellipse or a circle. It can contain
    children.
    """

    def __init__(self, style, width=-1.0, height=-1.0):
        """
        Create a new ellipse/circle item, inscribed in the box
        given by the width and height.

        :param style: An instance of GPS.Browsers.Style

        :param width: used to force a specific width for the item.
           If negative or null, the width is computed from the children
           of the item.

        :param height: similar to width.
        """


class PolylineItem(Item):
    """
    An item which displays a set of connected lines.
    It can be used to draw polygons for instance, or simple lines.
    """

    def __init__(self, style, points, close=False):
        """
        Create a new polyilne item.

        :param style: An instance of GPS.Browsers.Style
        :param points: A list of float, each points at which line
           ends. The coordinates are relative to the top-left corner
           of the item (so that (0,0) is the top-left corner itself).
           The floats are grouped into pair, each of which describes
           the coordinates of one points. For instance:

               (0,0,  100,100)

           displays a single line which goes from the top-left corner
           to a point at coordinates (100,100).

        :param close: A boolean, whether the last point should
           automatically be linked to the first. This ensures the
           polygone is properly closed and can be filled.
        """


class TextItem(Item):
    """
    An item that displays text (optionaly within a rectangular box).
    """

    Text_Arrow = enum(NONE=0, UP=1, DOWN=2, LEFT=3, RIGHT=4)

    def __init__(self, style, text, directed=Text_Arrow.NONE):
        """
        Creates a new text item

        :param style: An instance of GPS.Browsers.Style
        :param text: a string, the text to display.
        :param directed: whether to draw an additional arrow next
           to the text. This can be used for instance when the
           text is next to a link, and indicates in which direction
           the text applies.
        """


class HrItem(Item):
    """
    A horizontal-line item, with optional text in the middle.
    This is basically represented as:

        ----- text -----
    """

    def __init__(self, style, text=""):
        """
        Creates a new horizontal line.

        :param style: An instance of GPS.Browsers.Style
        :param text: a string, the text to display.
        """


class Link(object):
    """
    A line between two items.
    When the items are moved, the line is automatically adjusted to
    stay connected to those items.
    """

    Routing = enum(ORTHOGONAL=0, STRAIGHT=1, CURVE=2, ORTHOCURVE=3)
    Side = enum(AUTO=0, TOP=1, RIGHT=2, BOTTOM=3, LEFT=4, NO_CLIP=5)

    def __init__(
        self,
        origin,
        to,
        style,
        routing=Routing.STRAIGHT,
        fromX=0.5,
        fromY=0.5,
        fromSide=Side.AUTO,
        toX=0.5,
        toY=0.5,
        toSide=Side.AUTO
    ):
        """
        Creates a new link attached to the two items FROM and TO.

        :param from: an instance of GPS.Browsers.Item, the origin of the
           link.
        :param to: an instance of GPS.Browsers.Item, the target of the
           link.
        :param style: an instance of GPS.Browers.Style
        :param routing: the routing algorithm to use to compute how the
           link is displayed. A STRAIGHT link takes the shortest route
           between the two items, whereas an ORTHOGONAL link only uses
           horizontal and vertical lines to do so. A CURVE is almost
           the same as a straight link, but is slightly curves, which
           is useful when several links between the same two items exist.
           An ORTHOCURVE link uses bezier curves to link the two items.
        :param fromX: the position within the origin where the link is
           attached. This is a float in the range 0.0 .. 1.0. The link
           itself is not displayed on top of the origin box, but changing
           the attachment point will change the point at which the link
           exits from the origin box.
        :param fromY: similar to fromY, for the vertical axis
        :param fromSide: This can be used to force the link to exist from
           a specific side of its toplevel container. For instance, if you
           set fromX to 0.0, the link will always exit from the left side
           of self. But if self itself is contained within another item,
           it is possible that the line from the left side of self to the
           target of the link will in fact exit from some other place
           in the container item. Setting fromSide to
           GPS.Browsers.Link.Side.LEFT will make sure the link exits from
           the left side of the parent item too.
        """


class Diagram(object):
    """
    A diagram contains a set of items.
    """

    def __init__(self):
        """
        Creates a new empty diagram.
        """

    def add(self, item):
        """
        Add a new item to the diagram. The coordinates of the item
        are set through item.set_position().

        :param item: An instance of GPS.Browsers.Item
        """


class View(GPS.GUI):
    """
    A view shows a part of a diagram and its objects.
    Multiple views can be associated with the same diagram.
    """

    def __init__(self):
        """
        Always raise an exception, use create() to create a new
        diagram instead.
        """

    @staticmethod
    def create(diagram, title):
        """
        Creates a new view that shows the given diagram.
        This view is automatically made visible in GPS, and title
        is used for the corresponding notebook tab.
        """
