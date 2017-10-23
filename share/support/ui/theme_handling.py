"""Package for handling colors and color themes in the abstract"""

import GPS
import colorsys
import gps_utils
from gi.repository import Gtk, Gdk

css_template = """
@define-color gutter_color {gutter_fg};
@define-color gutter_background {gutter_bg};
@define-color browser_decoration_background  {browser_decoration_bg};
@define-color browser_decoration_color {browser_decoration_fg};

GtkTextView [ -GtkWidget-cursor-color: {caret}; ]

*:selected, *:selected:focus [
   color: {theme_selected_fg};
   background-color: {theme_selected_bg};
]
"""
# The CSS template ready for processing with .format

css_colors = ['theme_selected_bg', 'theme_selected_fg',
              'gutter_bg', 'gutter_fg',
              'browser_decoration_bg', 'browser_decoration_fg',
              'caret']
# These are the colors that are interpreted by the CSS template

variant_prefs = {
    'aspects': "Src-Editor-Aspects-Variant",
    'annotations': "Src-Editor-Code-Annotations-Variant-18",
    'ephemeral_smart': "Src-Editor-Ephemeral-Smart",
    'ephemeral_simple': "Src-Editor-Ephemeral-Simple",
    'hyperlinks': "Src-Editor-Hyper-Links-Variant",
    'strings':  "Src-Editor-Strings-Variant",
    'numbers': "Src-Editor-Numbers-Variant",
    'annotated_comments': "Src-Editor-Annotated-Comments-Variant",
    'comments': "Src-Editor-Comments-Variant",
    'keywords': "Src-Editor-Keywords-Variant",
    'types': "Src-Editor-Type-Variant",
    'blocks': "Src-Editor-Block-Variant"

}
# These keys are interpreted as variant preferences
# Keys: our internal easy-to-remember key; values: current names of GPS prefs

rgb_prefs = {
    'command_window': 'Command-Windows-Background-Color',
    'search': "Search-Src-Highlight-Color",
    'messages': "Messages-Highlight-Color",
    'errors': "Errors-Src-Highlight-Color",
    'warnings': "Warnings-Src-Highlight-Color",
    'style': "Style-Src-Highlight-Color",
    'info': "Info-Src-Highlight-Color",
    'isearch': "Plugins/isearch/nextmatchcolor",
    'msg_high': "High-Importance-Messages-Highlight",
    'msg_medium': "Medium-Importance-Messages-Highlight",
    'msg_low': "Low-Importance-Messages-Highlight",
    'horiz_diff': "Horizontal-Diff-Change-Color",
    'diff_change': "Diff-Change-Color",
    'diff_remove': "Diff-Remove-Color",
    'diff_append': "Diff-Append-Color",
    'breakpoint': "Debugger-Line-With-Breakpoint",
    'dispatching': "Plugins/dispatching/color",
    'ispell': "Plugins/ispell/bgcolor",
    'multicursor_entity': "Plugins/multi_cursors/multicursor_on_entity_color",
    'multicursor_selection':
        "Editor/Fonts & Colors:General/multicursor_selection_color",
    'readonly': "Editor/Fonts & Colors:General/read_only_color",
    'debugger_current': "Debugger-Editor-Current-Line",
    'current_line': "Src-Editor-Current-Line-Color",
    'current_block': "Src-Editor-Current-Block-Color",
    'browsers_bg': "Browsers-Bg-Color",
}
# These keys are interpreted as rgb preferences
# Keys: our internal easy-to-remember key; values: current names of GPS prefs

# colors that map to css attributes


def c(x):
    """Shortcut"""
    return int(x * 255.0)


class Color:
    """Represents a color"""

    def __init__(self, from_hex=None, from_rgba=None):
        """Initialize self from
            - a hex representation "#03f4b2" (rgb) or "#09889904" (rgba), or
            - r,g,b,a values provided between 0 and 255
               (a is optional, defaulting to 255)

        """
        # self.r, self.g, self.b, self.a are floats between 0
        self.a = 1.0
        if from_hex:
            self.r, self.g, self.b = (
                int(from_hex[1 + 2*x:1 + 2*x+2], 16) / 255.0
                for x in xrange(3))
            if len(from_hex) == 9:
                self.a = float(int(from_hex[7:9], 16)) / 255.0
        else:
            self.r, self.g, self.b = (x / 255.0 for x in from_rgba[0:3])
            if len(from_rgba) == 4:
                self.a = from_rgba[3] / 255.0

    def get_luminosity(self):
        """Return the luminosity as a float between 0.0 and 1.0 """

        h, l, s = colorsys.rgb_to_hls(self.r, self.g, self.b)
        return l

    def to_rgba_string(self):
        """Return self as a string of the form "rgba(r,g,b,a)" where
           the values are integers between 0 and 255 for r,g,b
           and 0 and 1 for a
        """

        return "rgba({},{},{},{})".format(c(self.r),
                                          c(self.g),
                                          c(self.b),
                                          self.a)

    def to_hex6_string(self):
        """Return self as an hex #xxyyzz color, dropping alpha"""
        def c(x):
            return int(x * 255.0)
        return '#{:02x}{:02x}{:02x}'.format(c(self.r), c(self.g), c(self.b))

    def lighten(self, amount):
        """Return a new color which is a variant of this color, modified by
           a given amount between -1.0 and 1.0. If amount is negative, darken
           the color, otherwise lighten it.
        """
        h, l, s = colorsys.rgb_to_hls(self.r, self.g, self.b)

        l = l + amount

        if l < 0.0:
            l = 0.0
        if l > 1.0:
            l = 1.0

        r, g, b = colorsys.hls_to_rgb(h, l, s)
        return Color(from_rgba=(c(r), c(g), c(b), c(self.a)))

    def saturate(self, amount):
        """Return a new color which is a variant of this color, modified by
           a given amount between -1.0 and 1.0. If amount is negative, darken
           the color, otherwise lighten it.
        """
        h, l, s = colorsys.rgb_to_hls(self.r, self.g, self.b)

        s = s + amount

        if s < 0.0:
            s = 0.0
        if s > 1.0:
            s = 1.0

        r, g, b = colorsys.hls_to_rgb(h, l, s)
        return Color(from_rgba=(c(r), c(g), c(b), c(self.a)))

    def mix(self, other, coef=0.5):
        """ Return a mix of colors self and other.
            Coef is where to place the cursor: 0.0 for color self,
            1.0 for color other.
        """

        ha, la, sa = colorsys.rgb_to_hls(self.r, self.g, self.b)
        hb, lb, sb = colorsys.rgb_to_hls(other.r, other.g, other.b)

        h = (1 - coef) * ha + coef * hb
        s = (1 - coef) * sa + coef * sb
        l = (1 - coef) * la + coef * lb

        r, g, b = colorsys.hls_to_rgb(h, l, s)
        return Color(from_rgba=(c(r), c(g), c(b),
                                 (1 - coef) * self.a + coef * other.a))


def Rgba(r, g, b, a=255):
    """shortcut"""
    return Color(from_rgba=(r, g, b, a))

transparent = Rgba(0, 0, 0, 0)
black = Rgba(0, 0, 0, 255)
white = Rgba(255, 255, 255, 255)

common_light = {
    "base_theme": "Adwaita",
    "editor_fg": black,
    "editor_bg": white,
    "gutter_fg": Color("#0e0e0e"),
    "gutter_bg": Color("#f8f8f8"),
    "browser_decoration_fg": Color("#0e0e0e"),
    "browser_decoration_bg": Color("#f8f8f8"),
    "theme_selected_bg": Color("#B5D6FE"),
    "theme_selected_fg": black,
    "caret": black,
    "aspects": ("DEFAULT", Rgba(96, 97, 95), transparent),
    "annotations": ("DEFAULT", transparent, Rgba(224, 224, 224)),
    'hyperlinks': ("DEFAULT", Rgba(0, 0, 255), transparent),
    'strings': ("DEFAULT", Rgba(206, 123, 0), transparent),
    'numbers': ("DEFAULT", Rgba(255, 51, 51), transparent),
    'annotated_comments': ("DEFAULT", Rgba(96, 97, 95), transparent),
    'comments': ("DEFAULT", Rgba(150, 150, 150), transparent),
    'keywords': ("DEFAULT", Rgba(0, 0, 230), transparent),
    'types': ("DEFAULT", Rgba(0, 153, 0), transparent),
    'blocks': ("DEFAULT", Rgba(96, 97, 95), transparent),
    "ephemeral_smart": ("DEFAULT", transparent, Rgba(252, 172, 79, 102)),
    "ephemeral_simple": ("DEFAULT", transparent, Rgba(134, 134, 134, 0.35)),

    "isearch": Color("#5555ff"),
    'command_window': white,

    "search": Rgba(189, 215, 255),
    "messages": Rgba(255, 0, 0),
    "errors": Rgba(255, 183, 183),
    "warnings": Rgba(255, 204, 156),
    "style": Rgba(255, 255, 173),
    "info": Rgba(173, 255, 194),
    "msg_high": Rgba(236, 197, 197),
    "msg_medium": Rgba(255, 218, 185),
    "msg_low": Rgba(255, 255, 240),
    "horiz_diff": Rgba(253, 230, 106),
    "diff_change": Rgba(236, 236, 170),
    "diff_remove": Rgba(255, 160, 160),
    "diff_append": Rgba(136, 238, 170),
    "breakpoint": Rgba(0, 0, 255, 77),

    "dispatching": Rgba(255, 243, 194),
    "ispell": Rgba(255, 255, 0),
    "multicursor_entity": Color("#94C3D7"),
    "multicursor_selection": Color("#96C5D9"),
    "readonly": Color("#e0e0e0"),

    'debugger_current': Rgba(125, 236, 57, 153),
    'current_line': Rgba(226, 226, 226, 102),
    'current_block': Rgba(226, 226, 226),
    'browsers_bg': Rgba(255, 255, 255)
}

common_dark = {
    "base_theme": "Adwaita (Dark)",
    "editor_fg": Rgba(186, 186, 186),
    "editor_bg": Rgba(34, 35, 36),
    "gutter_fg": Color("#e0e0e0"),
    "gutter_bg": Color("#343434"),
    "browser_decoration_fg": Color("#e0e0e0"),
    "browser_decoration_bg": Color("#343434"),
    "theme_bg": Color("#2280d2"),
    "theme_fg": Color("#2280d2"),
    "theme_selected_fg": black,
    "theme_selected_bg": Rgba(0, 67, 152),

    "caret": white,
    "aspects": ("DEFAULT", Rgba(114, 159, 207), transparent),
    "annotations": ("DEFAULT", transparent, Rgba(96, 96, 96)),

    'hyperlinks': ("DEFAULT", Rgba(114, 159, 207), transparent),
    'strings': ("DEFAULT", Rgba(242, 212, 44), transparent),
    'numbers': ("DEFAULT", Rgba(255, 51, 51), transparent),
    'annotated_comments': ("DEFAULT", Rgba(96, 97, 95), transparent),
    'comments': ("DEFAULT", Rgba(114, 159, 207), transparent),
    'keywords': ("DEFAULT", Rgba(240, 141, 36), transparent),
    'types': ("DEFAULT", Rgba(142, 105, 201), transparent),
    'blocks': ("DEFAULT", Rgba(104, 194, 68), transparent),

    "ephemeral_smart": ("DEFAULT", transparent, Rgba(128, 236, 255, 90)),
    "ephemeral_simple": ("DEFAULT", transparent, Rgba(180, 180, 180, 128)),

    'isearch': Rgba(9, 60, 60),
    'command_window': Rgba(38, 38, 38),

    "search": Rgba(0, 113, 128),
    "messages": Rgba(200, 42, 42),
    "errors": Rgba(75, 34, 34),
    "warnings": Rgba(85, 52, 18),
    "style": Rgba(68, 12, 42),
    "info": Rgba(53, 77, 59),
    "msg_high": Rgba(255, 99, 71),
    "msg_medium": Rgba(255, 140, 0),
    "msg_low": Rgba(189, 183, 107),
    "horiz_diff": Rgba(143, 89, 2),
    "diff_change": Rgba(107, 73, 19),
    "diff_remove": Rgba(88, 43, 43),
    "diff_append": Rgba(38, 68, 36),
    "breakpoint": Rgba(190, 191, 196),

    "dispatching": Rgba(46, 52, 56),
    "ispell": Rgba(206, 92, 0),

    "multicursor_entity": Color("#3070A0"),
    "multicursor_selection": Color("#3272A2"),
    "readonly": Color("#2E3436"),

    'debugger_current': Rgba(58, 71, 54, 153),
    'current_line':  Rgba(48, 51, 51),
    'current_block':  Rgba(48, 51, 51),
    'browsers_bg': Rgba(255, 255, 255)
}


class Theme(object):
    """Abstract class that represents a theme"""

    def __init__(self, name, light=True, extra=None):
        """Initialise. light indicates whether to initialize with the
           light theme color values.
           Extra is a dict containing:
             some keys in 'css_colors': value => a Color
             some keys in 'rgb_prefs': value => a Color
             some keys in 'variant_prefs': value => a tuple
               (weight, fg, bg)
                where weight is "DEFAULT", "NORMAL",
                                "BOLD", "ITALIC", or "BOLD_ITALIC"
                fg and bg are Colors.
        """

        self.name = name
        self.light = light

        if self.light:
            self.d = common_light.copy()
        else:
            self.d = common_dark.copy()

        self.d.update(extra)

    def apply_preferences(self, provider):
        """Apply the current theme as preferences"""

        # it is possible that values have been added to common_dark,
        # common_light!
        # TODO: to be revised once color prefs are no longer floating
        # around disparate plugins
        if self.light:
            self.d.update({k: common_light[k]
                           for k in common_light if k not in self.d})
        else:
            self.d.update({k: common_dark[k]
                           for k in common_dark if k not in self.d})

        font = GPS.Preference(
                "Src-Editor-Reference-Style").get().split("@")[0]

        def to_pref_str(t):
            """Return a variant pref string from a pref tuple"""
            return "{}@{}@{}".format(t[0],
                                     t[1].to_rgba_string(),
                                     t[2].to_rgba_string())

        def pref_set(gps_pref, val):
            """Set a preference; log any errors"""
            try:
                GPS.Preference(gps_pref).set(val)
            except GPS.Exception:
                # TODO: there is a potential issue here: some preferences
                # do not exist until their plugin is loaded (ispell, isearch);
                # we might need to move the definitions out of the plugins,
                # or reuse existing colors
                pass

        # Create internal preference
        pref_gtk_theme = "GPS6-Gtk-Theme-Name"
        gtkpref_name = "/ColorTheme gtk+"

        # Freeze the preferences

        with gps_utils.freeze_prefs():
            # The theme preference
            pref_set(pref_gtk_theme, self.d['base_theme'])

            # The editor fg and bg
            font = GPS.Preference(
                    "Src-Editor-Reference-Style").get().split("@")[0]
            GPS.Preference(
                "Src-Editor-Reference-Style").set("{}@{}@{}".format(
                    font,
                    self.d['editor_fg'].to_rgba_string(),
                    self.d['editor_bg'].to_rgba_string()))

            # Apply the theme preferences
            for key in variant_prefs:
                pref_set(variant_prefs[key], to_pref_str(self.d[key]))

            for key in rgb_prefs:
                pref_set(rgb_prefs[key], self.d[key].to_rgba_string())

        # Apply the CSS preference

        css = css_template.format(
            **{k: self.d[k].to_hex6_string() for k in css_colors}
        ).replace('[', '{').replace(']', '}')
        provider.load_from_data(css)
        GPS.Preference(gtkpref_name).set(css)

    def generate_example_label(self):
        """Generate an example Gtk.Label demoing this theme"""
        label_markup = """{keywords}procedure{i} {blocks}Foo{i}
  (An_Integer : {keywords}in out{i} {types}Integer{i} := {numbers}0{i};
   A_String   : {types}String{i}  := {strings}"some text"{i})
  {keywords}with{i} {aspects}Pre  => An_Integer >= -1{i};
{comments}--  Documentation for Foo{i}

---------
-- Foo --
---------

{keywords}procedure{i} {blocks}Foo{i}
  (An_Integer : {keywords}in out{i} {types}Integer{i} := {numbers}0{i};
   A_String   : {types}String{i}  := {strings}"some text"{i}) {keywords}is{i}
{keywords}begin{i}
   {comments}--  Do the actual loop{i}

   {keywords}for{i} J {keywords}in{i} A_String'Range {keywords}loop{i}
      Put_Line ({strings}"bla"{i} &amp; (A + {numbers}10{i})'Img);
   {keywords}end loop{i};
{keywords}end{i} {blocks}Foo{i};
"""
        # Add line numbers
        num = 1
        prefixed = []
        for l in label_markup.splitlines():
            prefixed.append(
                '<span color="{}" background="{}">{:4d} </span> {}'.format(
                    self.d['gutter_fg'].to_hex6_string(),
                    self.d['gutter_bg'].to_hex6_string(),
                    num,
                    l))
            num = num + 1

        font = GPS.Preference("Src-Editor-Reference-Style").get().split("@")[0]
        label_markup = '<span font="{}">'.format(font) + '\n'.join(
            prefixed) + '</span>'

        b = Gtk.HBox()
        l = Gtk.Label()
        b.pack_start(l, False, False, 0)
        _, bg = Gdk.Color.parse(self.d['editor_bg'].to_hex6_string())
        _, fg = Gdk.Color.parse(self.d['editor_fg'].to_hex6_string())
        b.modify_bg(Gtk.StateType.NORMAL, bg)
        l.modify_fg(Gtk.StateType.NORMAL, fg)
        process_dict = {'i': "</span>"}
        for key in ['keywords', 'blocks', 'comments',
                    'strings', 'numbers', 'aspects', 'types']:
            val = self.d[key]
            process_dict[key] = '<span {} {} {}>'.format(
                'color="{}"'.format(val[1].to_hex6_string())
                if val[1].a != 0.0 else '',
                'background="{}"'.format(val[2].to_hex6_string())
                if val[2].a != 0.0 else '',

                'font-weight="BOLD"' if 'BOLD' in val[0] else '' +
                ' font-style="ITALIC"' if "ITALIC" in val[0] else '')

        l.set_markup(label_markup.format(**process_dict))
        return b
