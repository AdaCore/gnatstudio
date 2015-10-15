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

        self.default_item_props = dict()

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

    def parse(self, json, itemType, id=None):
        """
        Return the style instance to use given a json description.
        This description is merged with the default properties for this
        itemType.

        :param json: a dict coming from JSON
        :param itemType: a string, the type of the item to which this style
           applies. Should be None when defining a style.
        :param str id: the id the style we are defining. If this is
           specified, the style is stored in self for later reuse.
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
                    "symbolToWidth", "shadowColor", "shadowOffsetY",
                    "shadowOffsetY"):
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


class JSON_Diagram_File():
    """
    A JSON file that contains the definition of multiple diagrams.
    """

    def __init__(self, data, factory=None):
        """
        :param str data: the JSON data
        :param factory: a function that creates a new empty diagram
        """
        self.styles = Styles()
        self.templates = {}  # id -> template (JSON data)
        self.diagrams = []
        self.factory = factory
        self.__load(data)

    def get(self, id=None):
        """
        Retrieve a diagram by its id, and ensure it has been created
        :param str id: if None, returns the first diagram
        :return: an instance of JSON_Diagram
        """
        for d in self.diagrams:
            if d.id == id:
                d.ensure()
                return d

        d = self.diagrams[0]
        d.ensure()
        return d

    def get_diagram_for_item(self, id):
        """
        Return the diagram to use for a given item
        :return:  (GPS.Diagram, Item)
        """
        for d in self.diagrams:
            d.ensure()
            it = d.get_item(id)
            if it:
                return (d, it)
        return None

    def clear_selection(self):
        """
        Clear the selection in all diagrams.
        This does not force the creation of diagrams (since if they have not
        been created yet, they cannot have a selection either.
        """

        for d in self.diagrams:
            d.clear_selection()

    def __load(self, data):
        """
        Load a JSON string.
        :param data: An object (dict, list) loaded from a JSON file,
            or a string that is parsed as json.
        :return: the list of GPS.Browsers.Diagram that were created
        """
        if isinstance(data, str):
            try:
                data = json.loads(data)
            except ValueError as e:
                GPS.Console().write("Error when parsing JSON: %s\n" % (
                    e, ))
                return

        for id, s in data.get('styles', {}).iteritems():
            self.styles.parse(s, None, id=id)

        for id, t in data.get('templates', {}).iteritems():
            self.templates[id] = t

        for d in data.get('diagrams', []):
            # ??? should build diagrams only when they are displayed

            if self.factory is None:
                diag = JSON_Diagram(file=self, json=d)   # A new diagram
            else:
                diag = self.factory(file=self, json=d)

            self.diagrams.append(diag)


class JSON_Diagram(B.Diagram):
    """
    A diagram loaded from a JSON file.
    Such a diagram is created lazily, via a call to ensure().
    This allows loading big JSON files, and only create the
    diagrams when they are actually needed.
    """

    def __init__(self, file, json):
        """
        :param JSON_Diagram_File file: a handle on the file
        :param json: the JSON data for just this diagram
        """

        self.__file = file
        self.__json = json
        self.__items = {}  # id -> item
        self.id = json.get('id', None)
        super(JSON_Diagram, self).__init__()

    def get_item(self, id):
        """
        Return the item with the given id
        """
        return self.__items.get(id)

    def ensure(self):
        """
        Ensure that the diagram has actually been created from the
        JSON information.
        """
        if not self.__json:
            return

        d = self.__json
        self.__json = None

        for o in d.get('items', []):
            it = self.__parse_item(o)
            self.add(it)

        for l in d.get('links', []):
            f = l.get('from')
            fitem = self.__items.get(f.get('ref'))
            if not fitem:
                GPS.Console().write(
                    "Object not found ('%s')\n" % f.get('ref'))
                continue

            t = l.get('to')
            titem = self.__items.get(t.get('ref'))
            if not titem:
                GPS.Console().write(
                    "Object not found ('%s')\n" % t.get('ref'))
                continue

            label = None
            if l.get('label'):
                label = self.__parse_item(l.get('label'))

            fromLabel = None
            if f.get('label'):
                fromLabel = self.__parse_item(f.get('label'))

            toLabel = None
            if t.get('label'):
                toLabel = self.__parse_item(t.get('label'))

            link = B.Link(
                origin=fitem,
                to=titem,
                style=self.__file.styles.parse(l.get('style'), 'link'),
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

            self.add(link)

            if link.id is not None:
                self.__items[link.id] = link

    @staticmethod
    def merge_template(orig, template):
        """
        Merge all data from the template into orig
        """
        for field, value in template.iteritems():
            o = orig.get(field, None)
            if o is None:
                orig[field] = value
            elif isinstance(o, dict):
                merge_template(o, value)  # recursive merge
            elif isinstance(o, list):
                o.extend(value)  # add the values

    def __parse_item(self, json):
        """
        Create an item from JSON.
        :param json: the JSON data for the item
        :return: the GPS.Browsers.Item
        """

        # Expand templates first, in case the type is defined there
        if 'template' in json:
            JSON_Diagram.merge_template(
                json, self.__file.templates[json['template']])
            del json['template']

        t = json.get('type', None)
        if t is None:
            if json.get('text') is not None:
                t = 'text'
            elif json.get('points') is not None:
                t = 'polyline'
            else:
                t = 'rect'
            json['type'] = t

        st = self.__file.styles.parse(json.get('style'), t)
        it_default = self.__file.styles.default_item_props.get(t, {})

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
            it = B.EllipseItem(style=st)
        else:
            it = B.RectItem(
                style=st,
                radius=json.get('radius', it_default.get('radius', 0)))

        if json.get('hbox') is not None:
            it.set_child_layout(B.Item.Layout.HORIZONTAL)

        id = json.get('id')
        if id is not None:
            it.id = id
            self.__items[id] = it

        data = json.get('data')
        if data is not None:
            it.data = data

        for o in json.get('vbox') or json.get('hbox') or []:
            it2 = self.__parse_item(o)
            it2_default = self.__file.styles.default_item_props.get(
                o['type'], {})

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

        w = json.get('width', it_default.get('width', B.Item.Size.FIT))
        h = json.get('height', it_default.get('height', B.Item.Size.FIT))
        it.set_size(w, h)

        w = json.get('minWidth', it_default.get('minWidth', None))
        if w is not None:
            it.set_width_range(min=w)

        h = json.get('minHeight', it_default.get('minHeight', None))
        if h is not None:
            it.set_height_range(min=h)

        it.set_position(
            x=json.get('x'),
            y=json.get('y'),
            anchorx=json.get('anchorx', 0),
            anchory=json.get('anchory', 0))

        return it


def load_json_file(file, diagramFactory=None):
    """
    Load the contents of the JSON file into the specified diagram. See the
    GPS.Browsers documentation for more information on the format.

    :param file: A file-like object, or a string, the name of the file
    :return: an instance of JSON_Diagram_File
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
    :return: an instance of JSON_Diagram_File
    """
    return JSON_Diagram_File(data, diagramFactory)


B.Diagram.load_json = staticmethod(load_json_file)
B.Diagram.load_json_data = staticmethod(load_json_data)
