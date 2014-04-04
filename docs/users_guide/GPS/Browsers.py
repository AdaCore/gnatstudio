"""
Interface to the graph drawing API in GPS.
"""

import GPS


def enum(name, **enums):
    """
    A dummy implementation of enumerations only suitable for documentation.
    Each of the individual enum value is accessible as an attribute, but
    printing the enum itself results in a printable string, not <type Enum>.
    The enum values are transformed into strings, since otherwise sphinx
    will print their integer value, not the enumeration name.
    """
    d = dict()
    for key, val in enums.items():
        d[key] = "%s.%s" % (name, key)
    e = type('Enum', (str, ), d)
    sorteditems = sorted(enums.iteritems(), key=lambda x: x[1])
    return e('Enumeration %s' % ','.join("%s=%s" % s for s in sorteditems))


class Style(object):
    """
    This class provides multiple drawing attributes that are used when
    drawing on a browser. This includes colors, dash patterns, arrows
    and symbols,...
    Very often, such a style will be reused for multiple objects.
    """

    Arrow = enum('Style.Arrow', NONE=0, OPEN=1, SOLID=2, DIAMOND=3)
    Symbol = enum('Style.Symbol', NONE=0, CROSS=1, STRIKE=2, DOUBLE_STRIKE=3)
    Underline = enum('Style.Underline', NONE=0, SINGLE=1, DOUBLE=2, LOW=3)
    Align = enum('Style.Align', LEFT=0, MIDDLE=1, RIGHT=2)
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
        arrowFrom=Arrow.NONE,
        arrowFromLength=8.0,
        arrowFromAngle=0.4,
        arrowFromStroke="black",
        arrowFromFill="",
        arrowFromWidth=1.0,
        arrowTo=Arrow.NONE,
        arrowToLength=8.0,
        arrowToAngle=0.4,
        arrowToStroke="black",
        arrowToFill="",
        arrowToWidth=1.0,
        symbolFrom=Symbol.NONE,
        symbolFromStroke="black",
        symbolFromDist=16.0,
        symbolFromWidth=1.0,
        symbolTo=Symbol.NONE,
        symbolToStroke="black",
        symbolToDist=16.0,
        symbolToWidth=1.0
    ):
        """
        Constructs a new style.
        This function takes a very large number of parameters, but they are
        all optional. Not all of them apply to all objects either.

        :param str stroke: the color to use to draw lines.
           The format is one of "rgb(255,255,255)", "rgba(255,255,255,1.0)"
           or "#123456". Transparency is supported when using rgba.

        :param str fill: how closed objects should be filled.
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

        :param float lineWidth: the width of a line

        :param list_of_floats dashes: a dash pattern. The
           first number is the number of pixels which should get ink, then
           the number of transparent pixels, then again the number of inked
           pixels,... The pattern will automatically repeat itself.

        :param boolean sloppy: when true, no straight line is
           displayed. They are instead approximated with curves. Combined with
           a hand-drawing font, this makes the display look as if it has been
           hand-drawn.

        :param str fontName: the font to use and its size.

        :param GPS.Browsers.Style.Underline fontUnderline: The underline to
           use for the text

        :param boolean fontStrike: whether to strikethrough the text.

        :param str fontColor: the color to use for text.

        :param int fontLineSpacing: extra number of pixels to
           insert between each lines of text.

        :param GPS.Browsers.Style.Align fontHalign: How text should be aligned
           horizontally within its bounding box.

        :param float fontValign: How text should be aligned vertically within
           its bounding box. This is a float in the range 0.0 .. 1.0, where
           0 is the top and 1 is the bottom.

        :param GPS.Browsers.Style.Arrow arrowFrom: How should arrows be
           displayed on the origin of a line.

        :param float arrowFromLength: the size of an arrow.

        :param float arrowFromAngle: the angle for an arrow.

        :param str arrowFromStroke: the stroke color for the arrow.

        :param str arrowFromFill: the fill pattern for the arrow.

        :param GPS.Browsers.Style.Arrow arrowTo: same as arrowFrom,
           but for the end of a line
        :param float arrowToLength: similar to arrowFromLength
        :param float arrowToAngle: similar to arrowFromAngle
        :param str arrowToStroke: similar to arrowFromStroke
        :param str arrowToFill: similar to arrowFromFill

        :param GPS.Browsers.Style.Symbol symbolFrom: the extra symbol
           to display near the origin of a line.

        :param str symbolFromStroke: the stroke color to use for the symbol.
        :param float symbolFromDist: the distance from the end of the line
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

    Align = enum('Item.Align', START=0, MIDDLE=1, END=2)
    Overflow = enum('Item.Overflow', PREVENT=0, HIDE=1)
    Layout = enum('Item.Layout', HORIZONTAL=0, VERTICAL=1)

    def __init__(self):
        """
        Will raise an exception, this is an abstract class.
        """

    def set_position(self, x=None, y=None):
        """
        Indicates the position of the item. This is the position within its
        parent item, or if there is no parent this is the absolute position
        of the item within the diagram.
        Calling this function should always be done for toplevel items, but
        is optional for children, since their position is computed
        automatically by their container (which is especially useful with
        text items, whose size might be hard to compute).

        :param float x: coordinates relative to parent or browser.
        :param float y: coordinates relative to parent or browser.
        """

    def set_child_layout(self, layout=Layout.VERTICAL):
        """
        Choose how children are organized within this item.

        :param GPS.Browsers.Item.Layout layout: if set to VERTICAL, then
           the child items are put below one another, otherwise they are
           put next to one another.
        """

    def set_min_size(self, width=1.0, height=1.0):
        """
        Forces a minimal size for self.
        It could be make larger if its children request a larger size, but
        will not smaller than the given size.

        :param float width: minimal width
        :param float height: minimal height
        """

    def add(self,
            item,
            align=Align.START,
            margin=(0, 0, 0, 0),
            float=False,
            overflow=Overflow.PREVENT):
        """
        Add a child item.
        This child will be displayed as part of the item, and will move with
        it. The size of the child will impact the size of its parent, unless
        you have forced a specific size for the latter.

        :param GPS.Browsers.Item item: the item to add

        :param GPS.Browsers.Item.Align align: How the item should be aligned
           within its parent. When
           the size of the child is computed automatically, it will have the
           same width as its parent (for vertical layout) or the same height
           as its parent (for horizontal layout), minus the margins. In this
           case, the align parameter will play no role. But when the child is
           smaller than its parent, the align parameter indicates on which
           side an extra margin is added.

        :param list_of_float margin: Extra margin to each side of the item
           (resp. top, right, bottom and left margin). These a float values.

        :param bool float: Whether the child should be floating within its
           parent.
           This impacts the layout: by default, in a vertical layout, all
           children are put below one another, so that they do not overlap.
           However, when a child is floating, it will be put at the current
           y coordinate, but the next item will be put at the same coordinate
           as if the child was not there.

        :param GPS.Browsers.Item.Overflow overflow: Whether the child's size
           should impact the parent
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

        :param GPS.Browsers.Style style: how to draw the item

        :param float width: used to force a specific width for the item.
           If this is null or negative, the width will be computed from
           the children of the item.

        :param float height: used to force a specific height, which will be
           computed automatically if height is negative or null

        :param float radius: the radius for the angles.
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

        :param GPS.Browsers.Style style: how to draw the item

        :param float width: used to force a specific width for the item.
           If negative or null, the width is computed from the children
           of the item.

        :param float height: similar to width.
        """


class PolylineItem(Item):
    """
    An item which displays a set of connected lines.
    It can be used to draw polygons for instance, or simple lines.
    """

    def __init__(self, style, points, close=False):
        """
        Create a new polyilne item.

        :param GPS.Browsers.Style style: how to draw the item
        :param list_of_float points: each points at which line
           ends. The coordinates are relative to the top-left corner
           of the item (so that (0,0) is the top-left corner itself).
           The floats are grouped into pair, each of which describes
           the coordinates of one points. For instance:

               (0,0,  100,100)

           displays a single line which goes from the top-left corner
           to a point at coordinates (100,100).

        :param bool close: whether the last point should
           automatically be linked to the first. This ensures the
           polygone is properly closed and can be filled.
        """


class TextItem(Item):
    """
    An item that displays text (optionaly within a rectangular box).
    """

    TextArrow = enum(
        'TextItem.Text_Arrow', NONE=0, UP=1, DOWN=2, LEFT=3, RIGHT=4)

    def __init__(self, style, text, directed=TextArrow.NONE):
        """
        Creates a new text item

        :param GPS.Browsers.Style style: how to draw the item
        :param str text: the text to display.
        :param bool directed: whether to draw an additional arrow next
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

        :param GPS.Browsers.Style style: how to draw the item
        :param str text: the text to display.
        """


class Link(object):
    """
    A line between two items.
    When the items are moved, the line is automatically adjusted to
    stay connected to those items.
    """

    Routing = enum('Link.Routing',
                   ORTHOGONAL=0, STRAIGHT=1, CURVE=2, ORTHOCURVE=3)
    Side = enum('Link.Side',
                AUTO=0, TOP=1, RIGHT=2, BOTTOM=3, LEFT=4, NO_CLIP=5)

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

        :param GPS.Browsers.Item from: the origin of the link.
        :param GPS.Browsers.Item to: the target of the link.
        :param GPS.Browsers.Style style: how to draw the item
        :param GPS.Browsers.Link.Routing routing: the routing algorithm to
           use to compute how the
           link is displayed. A STRAIGHT link takes the shortest route
           between the two items, whereas an ORTHOGONAL link only uses
           horizontal and vertical lines to do so. A CURVE is almost
           the same as a straight link, but is slightly curves, which
           is useful when several links between the same two items exist.
           An ORTHOCURVE link uses bezier curves to link the two items.
        :param float fromX: the position within the origin where the link is
           attached. This is a float in the range 0.0 .. 1.0. The link
           itself is not displayed on top of the origin box, but changing
           the attachment point will change the point at which the link
           exits from the origin box.
        :param float fromY: similar to fromY, for the vertical axis
        :param GPS.Browsers.Link.Side fromSide: This can be used to force the
           link to exist from
           a specific side of its toplevel container. For instance, if you
           set fromX to 0.0, the link will always exit from the left side
           of self. But if self itself is contained within another item,
           it is possible that the line from the left side of self to the
           target of the link will in fact exit from some other place
           in the container item. Setting fromSide to
           GPS.Browsers.Link.Side.LEFT will make sure the link exits from
           the left side of the parent item too.
        """

    def set_waypoints(self, points, relative=False):
        """
        Force specific waypoints for link.
        The lines will pass through each of these points when it is routed
        as straight or orthogonal lines.
        By default, the coordinates are absolute (in the same coordinate
        system as the items). If however you specify relative coordinates,
        then each point is relative to the previous one (and the first one
        is relative to the link's attachment point on its origin item).

        :param list_of_float points:
           The floats are grouped into pair, each of which describes
           the coordinates of one points.

        :param bool relative: whether the coordinates are relative to
           the previous point, or absolte.

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

        :param GPS.Browsers.Item item: the item to add
        """

    @staticmethod
    def load_json(file):
        """
        Load a JSON file into a series of diagrams.

        The format of the file is described below, and is basically a
        serialization of all the style and object attributes mentioned
        elsewhere in this documentation.

        The JSON file should contain an object ("{...}") with the following
        attributes:

            - `styles`: this is a list of style objects. Each style is itself
              described as an object whose attributes are any of the valid
              parameters for :func:`GPS.Browsers.Style.__init__`. In addition
              the style should have an `id` attribute that can be referenced
              elsewhere in the file, to avoid duplicating style information.

              There are a few special ids that can be used to set the default
              properties or styles for the items defined in this json
              file. For instance, if the id starts with "__default_props_"
              followed by one of the valid item type, the object can define
              the default attribute for any of the valid attributes of items.

              If the id starts with "__default_style_", the object defines the
              style properties for any of the item type.

              For instance::

                  {"styles": [
                     {"id": "__default_props_text", "margin": [0, 5, 0, 5]},
                     {"id": "__default_props_rect", "minWidth": 200},
                     {"id": "__default_style_text", "fontName": "arial 10"},
                     {"id": "customStyle1", "stroke": "blue"}
                  ]}

            - `diagrams`: this is a list of diagram object. A single file can
              contain multiple diagrams, but a browser window always only
              displays a single diagram.

              Each diagram object has two possible attributes:

                  * `items`: this is a list of item objects (see below)
                  * `links`: this is a list of link objects (see below)

        An item object is itself described with a JSON object, with the
        following possible attributes:

            - `id`: an optional string, which defines an id for an object.
              This id can be used when creating links. It is also stored
              as an id attribute in the instance of :class:`GPS.Browsers.Item`
              that is created.

            - `x`, `y`: optional float attributes. See the description of
              :func:`GPS.Browsers.Item.set_position()`.

            - `style`: an optional string (which then references the id of one
              of the styles defined in the "styles" attribute described above,
              or an inline JSON object that describes a style as above.

            - `type`: the type of the item. Valid values are "rect", "hr",
              "ellipse", "polyline" or "text" (see the corresponding classes in
              this documentation). This attribute is optional, and will be
              guessed automatically in some cases. For instance, when the
              object also has a `text` attribute, it is considered as a text
              item. If it has a "points" attribute it is considered as a
              polyline item. The default `type` is "rect".

            - `minWidth` and `minHeight`: the minimal size for an item. See
              :func:`GPS.Browsers.Item.set_min_size()`.

            - `vbox` and `hbox`: list of items, which are the children of the
              current item. These children are organized either vertically or
              horizontally (only one of the two attributes can be specified,
              vbox takes precedence).

        When an item is created as a child of another one (in its parent's
        `vbox` or `hbox`), it may have the following additional attributes.
        See :func:`GPS.Browsers.Item.add` for more information.

            - `margin`: the margins around the item. This is either a single
              float (in which case all margins are equal), or a list of four
              floats which give the top, right, bottom and left margins
              respectively.

            - `align`: one of the values from :class:`GPS.Browsers.Item.Align`.
              In a JSON file, though, you can only use the corresponding
              integer values.

            - `float`: whether the item is set as floating

            - `overflow`: one of the integer values for
              :class:`GPS.Browers.Item.Overflow`

        Text objects (corresponding to :class:`GPS.Browsers.TextItem`) have the
        following additional attributes:

            - `text`: the text to display.

        Hr objects (corresponding to :class:`GPS.Browsers.HrItem`) have the
        following additional attributes:

            - `text`: the text to display.

        Polyline objects (corresponding to :class:`GPS.Browsers.PolylineItem`)
        have the following additional attributes:

            - `points`: the points that describe the contour of the object, as
              a list of floats. They are grouped into pairs, each of which
              describes the coordinates of a point.

            - `close`: whether the last point should automatically be linked
              to the first.

        Ellipse items (corresponding to :class:`GPS.Browsers.EllipseItem`)
        have the following additional attributes:

            - `width`, `height`: optional floats, which describe the rectangle
              into which the ellipse is inscribed. When unspecified, the size
              is computed from the list of children.

        Rect items (corresponding to :class:`GPS.Browsers.RectItem`) have the
        following additional attributes:

            - `width`, `height`: optional floats.  When unspecified, the size
              is computed from the list of children.

            - `radius`: optional float indicating the radius for the corners
              of the rectangle.

        A link object contains the following attributes (see
        :class:`GPS.Browser.Link` for more information on the parameters):

            - `id`: an optional string id for the link, if you need to create
              links to it.

            - `from` and `to`: these are record with one mandatory field,
              `ref`, which is the id of one of the objects or links created
              previously. In addition, these records can also specify the
              following attributes:

                   - `anchorx`, `anchory`: floats in the range 0.0 .. 1.0
                     which specify where the item is attached in its target
                     item

                   - `side` takes its value from GPS.Browsers.Link.Side, and
                     is used to force the link to emerge from one specific
                     side of the item.

            - `route`: one of :class:`GPS.Browsers.Link.Routing` (as integer)
              to indicate how the link is displayed.

            - `waypoints`: a list of floats, which will be grouped into pairs
              to define waypoints. See :func:`GPS.Browsers.Link.set_waypoints`.
              An alternative definition is to use an object with two fields,
              `points` which is the list of floats, and `relative` which is
              a bool indicating whether the points are in absolute coordinates
              or relative to the previous point.

        Here is an example which draws two items linked together::

            {"styles": {},
             "diagrams": [
               {"items": [
                  {"x": 0, "y": 0, "style": "customStyle1",
                   "id": "first item",
                   "width": 100, "height": 100,
                   "vbox": [
                      {"text": "Name",
                       "style": {"fontName": "arial 20", stroke:null}},
                      {"type": "hr", "text":"attributes"},
                      {"text": "+attr:integer"}
                   ]
                  },
                  {"x": 100, "y":200",
                   "id": "second item",
                   "text": "Annotation"},
                ],
                "links": [
                  {"from": {"ref": "first item"},
                   "to":   {"ref": "second item"},
                   "style": {"stroke": "blue", "dashes": [4, 4]}}
                ]
               }
            ]
            }


        :param str file: an object that has a read() function, or the name
           of a file as a string.
        :return: the list of GPS.Browsers.Diagram objects created.
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
        This view is automatically made visible in GPS.

        :param str title: the title used for the notebook tab.
        """
