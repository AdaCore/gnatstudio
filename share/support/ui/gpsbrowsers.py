"""
Add various high-level features for the support of
customer browsers in python.
"""

import GPS
import GPS.Browsers as B
import json
import traceback
import extensions


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

    def __init__(self, data, factory=None, load_styles=None):
        """
        :param str data: the JSON data
        :param factory: a function that creates a new empty diagram
        """
        self.load_styles = load_styles is None
        self.styles = Styles() if self.load_styles else load_styles
        self.templates = {}  # id -> template (JSON data)
        self.diagrams = []
        self.index = []  # (id, children (JSON Array))
        self.factory = factory
        self.__load(data)

    def contains(self, id):
        """
        Tells whether a diagram is contained within this file
        without loading it.
        :return boolean: Existence of diagram with name id within self
        """
        for d in self.diagrams:
            if d.id == id:
                return True
        return False

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

        if self.diagrams:
            d = self.diagrams[0]
            d.ensure()
            return d
        return None

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

    def set_item_style(self, item, style):
        """
        Change the style of an item. This function can be used to temporarily
        alter the style of items to show special statuses for instance.

        :param item: the item or link to alter
        :param style: Either a string (the name of a style to lookup in the
           templates, or an object (an expanded style), or None (to restore
           the original style).
        """

        # Save original style if needed
        if not hasattr(item, "_standard_style"):
            item._standard_style = item.style

        # Apply the new style
        if style is None:
            item.style = item._standard_style
        else:
            item.style = self.styles.parse(style, 'link')

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
        if self.load_styles:
            for id, s in data.get('styles', {}).iteritems():
                self.styles.parse(s, None, id=id)

        for id, t in data.get('templates', {}).iteritems():
            self.templates[id] = t

        for d in data.get('diagrams', []):
            # ??? should build diagrams only when they are displayed
            self.index.append((d.get('id'), d.get('children', [])))

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

        for link in d.get('links', []):
            browserlink = self.__parse_link(link)
            self.add(browserlink)

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

    def __parse_link(self, json):
        """
        Create a link from JSON.
        :param json: the JSON data for the link
        :return: the GPS.Browsers.Link
        """

        # Expand templates first, in case the type is defined there
        if 'template' in json:
            JSON_Diagram.merge_template(
                json, self.__file.templates[json['template']])
            del json['template']

        f = json.get('from')
        fitem = self.__items.get(f.get('ref'))
        if not fitem:
            GPS.Console().write(
                "Object not found ('%s')\n" % f.get('ref'))
            return None

        t = json.get('to')
        titem = self.__items.get(t.get('ref'))
        if not titem:
            GPS.Console().write(
                "Object not found ('%s')\n" % t.get('ref'))
            return None

        label = None
        if json.get('label'):
            label = self.__parse_item(json.get('label'))

        fromLabel = None
        if f.get('label'):
            fromLabel = self.__parse_item(f.get('label'))

        toLabel = None
        if t.get('label'):
            toLabel = self.__parse_item(t.get('label'))

        browserlink = B.Link(
            origin=fitem,
            to=titem,
            style=self.__file.styles.parse(json.get('style'), 'link'),
            routing=json.get('route'),
            label=label,
            fromX=f.get('anchorx'),
            fromY=f.get('anchory'),
            fromSide=f.get("side", B.Link.Side.AUTO),
            fromLabel=fromLabel,
            toX=t.get('anchorx'),
            toY=t.get('anchory'),
            toLabel=toLabel,
            toSide=f.get("side", B.Link.Side.AUTO))

        self.__parse_id(browserlink, json)
        browserlink.data = json.get('data', {})

        w = json.get('waypoints')
        if w:
            if isinstance(w, list):
                browserlink.set_waypoints(
                    json['waypoints'], relative=False)
            else:
                browserlink.set_waypoints(
                    w['points'], relative=w.get('relative', False))

        return browserlink

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

        self.__parse_id(it, json)
        it.data = json.get('data', {})

        for o in json.get('vbox') or json.get('hbox') or []:
            it2 = self.__parse_item(o)
            it2_default = self.__file.styles.default_item_props.get(
                o['type'], {})

            margin = o.get('margin', it2_default.get('margin'))
            if (margin is not None and
                (isinstance(margin, float) or
                 isinstance(margin, int))):

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

    def __parse_id(self, item, json):
        """
        Check whether an id is defined in json.
        :param GPS.Browser.Item item: the link or item created from json
        :param json: the JSON data
        """
        id = json.get('id')
        if id is not None:
            item.id = id
            self.__items[id] = item


@extensions.extend_module(GPS.Browsers)
class Item:
    def recurse(self):
        """
        Return self and all its child items.
        """
        yield self
        if self.children:
            for child in self.children:
                for it in child.recurse():
                    yield it

    def toplevel(self):
        """
        Return the toplevel parent
        """
        while self:
            parent = self.parent
            if parent is None:
                return self
            self = parent

    def get_parent_with_id(self):
        """
        Return self if it has an id, or its first parent with an id
        :return: a `GPS.Browser.Item`
        """
        item = self
        while item and not hasattr(item, "id"):
            item = item.parent
        return item


@extensions.extend_module(GPS.Browsers)
class Link:
    def recurse(self):
        """
        Return self and all its child items.
        """
        yield self
        if self.label:
            for it in self.label.recurse():
                yield it
        if self.fromLabel:
            for it in self.fromLabel.recurse():
                yield it
        if self.toLabel:
            for it in self.toLabel.recurse():
                yield it


@extensions.extend_module(GPS.Browsers)
class Diagram:

    @staticmethod
    def load_json(file, diagramFactory=None, load_styles=None):
        """
        Load the contents of the JSON file into the specified diagram. See the
        GPS.Browsers documentation for more information on the format.

        :param file: A file-like object, or a string, the name of the file
        :return: an instance of JSON_Diagram_File
        """

        try:
            if isinstance(file, str):
                file = open(file)

            return GPS.Browsers.Diagram.load_json_data(
                json.load(file), diagramFactory, load_styles)
        except Exception as e:
            GPS.Console().write("Unexpected exception %s\n%s\n" % (
                e, traceback.format_exc()))

    @staticmethod
    def load_json_data(data, diagramFactory=None, load_styles=None):
        """
        :return: an instance of JSON_Diagram_File
        """
        return JSON_Diagram_File(data, diagramFactory, load_styles)
