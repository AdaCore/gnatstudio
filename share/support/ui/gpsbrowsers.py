"""
Add various high-level features for the support of
customer browsers in python.
"""

import GPS
import GPS.Browsers as B
import json
import traceback


class Styles(object):

    """
    This class is used to manage object styles, and provide a cache
    were appropriate for efficiency.
    """

    def __init__(self):
        self.styles = dict()   # user styles, indexed by id
        self.default_styles = dict()  # default styles, indexed by item type
        self.default_props = None   # default style props, by item type

        self.default_item_props = None  # default item properties

        self.create_default_styles()
        self.create_default_item_props()

    def create_default_item_props(self):
        """
        Create the default properties to use for items, like the
        minWidth, minHeight, alignment, margins,...
        """

        self.default_item_props = dict(
            text=dict(
                #margin=[0, 5, 0, 5],
                # align=2,
            )
        )

    def create_default_styles(self):
        """
        Creates the default styles for the various types of items.
        The goal is that these styles provide the default values for
        otherwise unspecified properties of the items, depending on
        the type of the item.
        This reduces the size of the JSON files.
        """

        self.default_props = dict(
            text=dict(
                stroke='',
                fontName='arial 10'),
            rect=dict(
                fill='white',
                stroke='black'),
            polyline=dict(
                stroke='black'),
            hr=dict(
                stroke='black',
                fontName='arial 7'),
            link=dict(
                stroke='black'))

    def parse(self, json, itemType):
        """
        Return the style instance to use given a json description.
        This description is merged with the default properties for this
        itemType.

        :param json: a dict coming from JSON
        :param itemType: a string, the type of the item to which this style
           applies. Should be None when defining a style.
        :return: an instance of GPS.Browsers.Style
        """
        if json is None:
            # Apply the default style, and use shared instances when possible
            st = self.default_styles.get(itemType)
            if st is None:
                st = B.Style(**self.default_props.get(itemType, {}))
                self.default_styles[itemType] = st
            return st

        if isinstance(json, basestring):
            return self.styles[json]

        id = json.get('id')
        if id and id.startswith('__default_props_'):
            id = id[16:]
            self.default_item_props[id] = json
            return

        default = self.default_props.get(itemType)

        props = {}
        for key in ("stroke", "fill", "lineWidth", "dashes", "sloppy",
                    "fontName", "fontUnderline", "fontStrike", "fontColor",
                    "fontLineSpacing", "fontHalign", "arrowFrom",
                    "arrowFromLength", "arrowFromAngle", "arrowFromStroke",
                    "arrowFromFill", "arrowFromWidth", "arrowTo",
                    "arrowToLength", "arrowToAngle", "arrowToStroke",
                    "arrowToFill", "arrowToWidth", "symbolFrom",
                    "symbolFromStroke", "symbolFromDist", "symbolFromWidth",
                    "symbolTo", "symbolToStroke", "symbolToDist",
                    "symbolToWidth", "shadowColor"):
            if key in json:
                # JSON 'null' gives a Python None, but we still want to use
                # the non-default value for the parameter in this case
                val = json[key]
                if val is None:
                    val = ""
                props[key] = val
            elif default and key in default:
                props[key] = default[key]

        if itemType is None and id and id.startswith('__default_style_'):
            # were we overriding the default props
            id = id[16:]
            self.default_props[id] = props
            self.default_style[id] = None
            return

        s = B.Style(**props)

        if id:
            self.styles[id] = s

        return s


def parse_item(items, json, styles):
    """
    Create an item from JSON.

    :param items: a dict of all items, indexed by id
    :param json: a dict describing the style
    :param styles: an instance of Styles
    :return: the GPS.Browsers.Item
    """

    t = json.get('type', None)
    if t is None:
        if json.get('text') is not None:
            t = 'text'
        elif json.get('points') is not None:
            t = 'polyline'
        else:
            t = 'rect'
        json['type'] = t

    st = styles.parse(json.get('style'), t)
    it_default = styles.default_item_props.get(t, {})

    if t == 'text':
        it = B.TextItem(
            style=st,
            text=json.get('text', it_default.get('text', '')),
            directed=json.get('directed'))
    elif t == 'hr':
        it = B.HrItem(
            style=st,
            text=json.get('text', it_default.get('text', '')))
    elif t == 'polyline':
        it = B.PolylineItem(
            style=st,
            points=json.get('points', []),
            relative=json.get('relative', False),
            close=json.get('close', it_default.get('close', False)))
    elif t == 'ellipse':
        it = B.EllipseItem(
            style=st,
            width=json.get('width', it_default.get('width', -1)),
            height=json.get('height', it_default.get('height', -1)))
    else:
        it = B.RectItem(
            style=st,
            width=json.get('width', it_default.get('width', -1)),
            height=json.get('height', it_default.get('height', -1)),
            radius=json.get('radius', it_default.get('radius', 0)))

    if json.get('hbox') is not None:
        it.set_child_layout(B.Item.Layout.HORIZONTAL)

    for o in json.get('vbox') or json.get('hbox') or []:
        it2 = parse_item(items, o, styles)
        it2_default = styles.default_item_props.get(o['type'], {})

        margin = o.get('margin', it2_default.get('margin'))
        if (margin is not None
            and (isinstance(margin, float)
                 or isinstance(margin, int))):

            margin = [margin, margin, margin, margin]

        it.add(it2,
               margin=margin,
               align=o.get('align', it2_default.get('align')),
               float=o.get('float'),
               overflow=o.get('overflow', it2_default.get('overflow')))

    it.set_min_size(
        width=json.get('minWidth', it_default.get('minWidth')),
        height=json.get('minHeight', it_default.get('minHeight')))
    it.set_position(
        x=json.get('x'),
        y=json.get('y'),
        anchorx=json.get('anchorx', 0),
        anchory=json.get('anchory', 0))

    data = json.get('data')
    if data is not None:
        it.data = data

    id = json.get('id')
    if id is not None:
        it.id = id
        items[id] = it

    return it


def load_json_file(file, diagramFactory=None):
    """
    Load the contents of the JSON file into the specified diagram. See the
    GPS.Browsers documentation for more information on the format.

    :param file: A file-like object, or a string, the name of the file
    :return: the list of GPS.Browsers.Diagram that were created
    """

    try:
        if isinstance(file, str):
            file = open(file)

        return load_json_data(json.load(file), diagramFactory)
    except Exception as e:
        GPS.Console().write("Unexpected exception %s\n%s\n" % (
            e, traceback.format_exc()))


def load_json_data(data, diagramFactory=None):
    """
    Load a JSON string.

    :param data: An object (dict, list) loaded from a JSON file
    :return: the list of GPS.Browsers.Diagram that were created
    """
    styles = Styles()
    items = dict()
    diagrams = []

    for s in data.get('styles', []):
        styles.parse(s, None)

    for d in data.get('diagrams', []):
        if diagramFactory is None:
            diag = B.Diagram()   # A new diagram
        else:
            diag = diagramFactory()
        diagrams.append(diag)

        diag.ids = items
        diag.id = d.get('id', None)

        for o in d.get('items', []):
            it = parse_item(items, o, styles)
            diag.add(it)

        for l in d.get('links', []):
            f = l.get('from')
            fitem = items.get(f.get('ref'))
            if not fitem:
                GPS.Console().write(
                    "Object not found (id '%s')\n" % f.get('ref'))
                continue

            t = l.get('to')
            titem = items.get(t.get('ref'))
            if not titem:
                GPS.Console().write(
                    "Object not found (id '%s')\n" % t.get('ref'))
                continue

            label = None
            if l.get('label'):
                label = parse_item(items, l.get('label'), styles)

            fromLabel = None
            if f.get('label'):
                fromLabel = parse_item(items, f.get('label'), styles)

            toLabel = None
            if t.get('label'):
                toLabel = parse_item(items, t.get('label'), styles)

            link = B.Link(
                origin=fitem,
                to=titem,
                style=styles.parse(l.get('style'), 'link'),
                routing=l.get('route'),
                label=label,
                fromX=f.get('anchorx'),
                fromY=f.get('anchory'),
                fromSide=f.get("side", B.Link.Side.AUTO),
                fromLabel=fromLabel,
                toX=t.get('anchorx'),
                toY=t.get('anchory'),
                toLabel=toLabel,
                toSide=f.get("side", B.Link.Side.AUTO))
            link.id = l.get('id')

            w = l.get('waypoints')
            if w:
                if isinstance(w, list):
                    link.set_waypoints(l['waypoints'], relative=False)
                else:
                    link.set_waypoints(
                        w['points'], relative=w.get('relative', False))

            diag.add(link)

            if link.id is not None:
                items[link.id] = link

    return diagrams

B.Diagram.load_json = staticmethod(load_json_file)
