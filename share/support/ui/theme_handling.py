"""Package for handling colors and color themes in the abstract"""

import GPS
import colorsys
import gs_utils
from gi.repository import Gtk, Gdk
import re


gtk_theme_pref_name = "GPS6-Gtk-Theme-Name"
# The preference that controls the Gtk+ base theme (e.g: Adwaita).


css_template = """
@define-color editor_bg_color {editor_bg};
@define-color editor_fg_color {editor_fg};
@define-color readonly_color {readonly};
@define-color theme_selected_bg_color {theme_selected_bg};
@define-color theme_selected_fg_color {theme_selected_fg};

GtkTextView [caret-color: {caret};]
"""
# The CSS template ready for processing with .format

css_colors = [
    "editor_bg",
    "editor_fg",
    "readonly",
    "theme_selected_bg",
    "theme_selected_fg",
    "browser_decoration_bg",
    "browser_decoration_fg",
    "caret",
]
# These are the colors that are interpreted by the CSS template

prefs_to_color_keys = {
    "Src-Editor-Reference-Style": ["editor_fg", "editor_bg"],
    "Editor/Fonts & Colors:General/read_only_color": ["readonly"],
}

# The map that associates preferences with their corresponding
# CSS colors.

variant_prefs = {
    "aspects": "Src-Editor-Aspects-Variant",
    "annotations": "Src-Editor-Code-Annotations-Variant-18",
    "ephemeral_smart": "Src-Editor-Ephemeral-Smart",
    "ephemeral_simple": "Src-Editor-Ephemeral-Simple",
    "hyperlinks": "Src-Editor-Hyper-Links-Variant",
    "strings": "Src-Editor-Strings-Variant",
    "numbers": "Src-Editor-Numbers-Variant",
    "annotated_comments": "Src-Editor-Annotated-Comments-Variant",
    "comments": "Src-Editor-Comments-Variant",
    "keywords": "Src-Editor-Keywords-Variant",
    "types": "Src-Editor-Type-Variant",
    "blocks": "Src-Editor-Block-Variant",
    "keywords_in_aspects": "Src-Editor-Aspects-Keywords-Variant",
    "comments_in_aspects": "Src-Editor-Aspects-Comments-Variant",
    "numbers_in_aspects": "Src-Editor-Aspects-Numbers-Variant",
    "strings_in_aspects": "Src-Editor-Aspects-Strings-Variant",
    "preprocessor": "Editor/Fonts & Colors:General/preprocessor",
    "diff_patch_remove": "Diff-Patch-Remove-Variant",
    "diff_patch_append": "Diff-Patch-Append-Variant",
    "blocks_in_aspects": "Src-Editor-Aspects-Block-Variant",
    "types_in_aspects": "Src-Editor-Aspects-Type-Variant",
    "namespaces": "Src-Editor-LSP-Semantic-Namespace",
    "classes": "Src-Editor-LSP-Semantic-Class",
    "enums": "Src-Editor-LSP-Semantic-Enum",
    "interfaces": "Src-Editor-LSP-Semantic-Interface",
    "structs": "Src-Editor-LSP-Semantic-Struct",
    "typeparameters": "Src-Editor-LSP-Semantic-TypeParameter",
    "parameters": "Src-Editor-LSP-Semantic-Parameter",
    "variables": "Src-Editor-LSP-Semantic-Variable",
    "propertys": "Src-Editor-LSP-Semantic-Property",
    "enummembers": "Src-Editor-LSP-Semantic-EnumMember",
    "functions": "Src-Editor-LSP-Semantic-Function",
    "modifiers": "Src-Editor-LSP-Semantic-Modifier",
    "operators": "Src-Editor-LSP-Semantic-Operator",
    "deprecateds": "Src-Editor-LSP-Semantic-Deprecated",
}
# These keys are interpreted as variant preferences
# Keys: our internal easy-to-remember key; values: current names of GPS prefs

rgb_prefs = {
    "command_window": "Command-Windows-Background-Color",
    "search": "Search-Src-Highlight-Color",
    "messages": "Messages-Highlight-Color",
    "isearch": "Plugins/isearch/nextmatchcolor",
    "msg_annotations": "Annottations-Messages-Highlight",
    "msg_info": "Info-Messages-Highlight",
    "msg_high": "High-Importance-Messages-Highlight",
    "msg_medium": "Medium-Importance-Messages-Highlight",
    "msg_low": "Low-Importance-Messages-Highlight",
    "msg_removed": "Messages-Removed-Foreground",
    "breakpoint": "Debugger-Line-With-Breakpoint",
    "dispatching": "Plugins/dispatching/color",
    "ispell": "Plugins/ispell/bgcolor",
    "multicursor_entity": "Plugins/multi_cursors/multicursor_on_entity_color",
    "multicursor_selection": "Editor/Fonts & Colors:General/multicursor_selection_color",
    "readonly": "Editor/Fonts & Colors:General/read_only_color",
    "debugger_current": "Debugger-Editor-Current-Line",
    "current_line": "Src-Editor-Current-Line-Color",
    "current_block": "Src-Editor-Current-Block-Color",
    "browsers_bg": "Browsers-Bg-Color",
    "diff_side_default": "Diff-Side-Default-Color",
    "diff_side_change": "Diff-Side-Change-Color",
    "diff_side_remove": "Diff-Side-Remove-Color",
    "diff_side_append": "Diff-Side-Append-Color",
    "bookmarks": "Src-Editor-Bookmarks",
    "semantic_readonly": "LSP-Semantic-Readonly",
}
# These keys are interpreted as rgb preferences
# Keys: our internal easy-to-remember key; values: current names of GPS prefs

# colors that map to css attributes


def c(x):
    """Shortcut"""
    return int(x * 255.0)


class Color:
    """Represents a color"""

    def __init__(self, from_hex=None, from_rgba=None, from_pref=None):
        """Initialize self from
        - a hex representation "#03f4b2" (rgb) or "#09889904" (rgba), or
        - r,g,b,a values provided between 0 and 255
           (a is optional, defaulting to 255)
        - a color preference's string value (e.g: rgba(20, 40, 30, 0)
          or rgb(20, 40, 30))
        """
        # self.r, self.g, self.b, self.a are floats between 0
        self.a = 1.0
        if from_hex:
            self.r, self.g, self.b = (
                int(from_hex[1 + 2 * x : 1 + 2 * x + 2], 16) / 255.0 for x in range(3)
            )
            if len(from_hex) == 9:
                self.a = float(int(from_hex[7:9], 16)) / 255.0
        elif from_rgba:
            self.r, self.g, self.b = (x / 255.0 for x in from_rgba[0:3])
            if len(from_rgba) == 4:
                self.a = from_rgba[3] / 255.0
        else:
            rgba_values = re.findall("\d+", from_pref)
            self.r, self.g, self.b = (int(x) / 255.0 for x in rgba_values[0:3])
            if len(rgba_values) == 4:
                self.a = int(rgba_values[3]) / 255.0

    def __eq__(self, other):
        """Return True if the Colors RGBA components match"""
        if isinstance(self, other.__class__):
            return (
                self.r == other.r
                and self.g == other.g
                and self.b == other.b
                and self.a == other.a
            )

        return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_luminosity(self):
        """Return the luminosity as a float between 0.0 and 1.0"""

        h, l, s = colorsys.rgb_to_hls(self.r, self.g, self.b)
        return l

    def to_rgba_string(self):
        """Return self as a string of the form "rgba(r,g,b,a)" where
        the values are integers between 0 and 255 for r,g,b
        and 0 and 1 for a
        """

        return "rgba({},{},{},{})".format(c(self.r), c(self.g), c(self.b), self.a)

    def to_hex6_string(self):
        """Return self as an hex #xxyyzz color, dropping alpha"""

        def c(x):
            return int(x * 255.0)

        return "#{:02x}{:02x}{:02x}".format(c(self.r), c(self.g), c(self.b))

    def lighten(self, amount):
        """Return a new color which is a variant of this color, modified by
        a given amount between -1.0 and 1.0. If amount is negative, darken
        the color, otherwise lighten it.
        """
        h, light, s = colorsys.rgb_to_hls(self.r, self.g, self.b)

        light = light + amount

        if light < 0.0:
            light = 0.0
        if light > 1.0:
            light = 1.0

        r, g, b = colorsys.hls_to_rgb(h, light, s)
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
        """Return a mix of colors self and other.
        Coef is where to place the cursor: 0.0 for color self,
        1.0 for color other.
        """

        def m(a, b):
            return a * (1 - coef) + b * coef

        return Color(
            from_rgba=(
                c(m(self.r, other.r)),
                c(m(self.g, other.g)),
                c(m(self.b, other.b)),
                c(m(self.a, other.a)),
            )
        )


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
    "browser_decoration_fg": Color("#0e0e0e"),
    "browser_decoration_bg": Color("#f8f8f8"),
    "theme_selected_bg": Color("#B5D6FE"),
    "theme_selected_fg": black,
    "caret": black,
    "aspects": ("DEFAULT", Rgba(96, 97, 95), transparent),
    "annotations": ("DEFAULT", transparent, Rgba(224, 224, 224)),
    "hyperlinks": ("DEFAULT", Rgba(0, 0, 255), transparent),
    "strings": ("DEFAULT", Rgba(206, 123, 0), transparent),
    "numbers": ("DEFAULT", Rgba(128, 0, 212), transparent),
    "annotated_comments": ("DEFAULT", Rgba(96, 97, 95), transparent),
    "comments": ("DEFAULT", Rgba(150, 150, 150), transparent),
    "keywords": ("DEFAULT", Rgba(0, 0, 230), transparent),
    "types": ("DEFAULT", Rgba(0, 153, 0), transparent),
    "blocks": ("DEFAULT", Rgba(96, 97, 95), transparent),
    "ephemeral_smart": ("DEFAULT", transparent, Rgba(252, 172, 79, 102)),
    "ephemeral_simple": ("DEFAULT", transparent, Rgba(134, 134, 134, 90)),
    "isearch": Color("#5555ff"),
    "command_window": white,
    "search": Rgba(189, 215, 255),
    "messages": Rgba(255, 0, 0),
    "msg_annotations": Rgba(224, 224, 224),
    "msg_info": Rgba(189, 229, 248),
    "msg_high": Rgba(255, 183, 183),
    "msg_medium": Rgba(255, 204, 156),
    "msg_low": Rgba(255, 255, 240),
    "msg_removed": Color("#5A5A5A"),
    "diff_side_default": Rgba(186, 189, 182),
    "diff_side_change": Rgba(236, 236, 170),
    "diff_side_remove": Rgba(255, 160, 160),
    "diff_side_append": Rgba(136, 238, 170),
    "diff_patch_remove": ("Default", Rgba(120, 10, 10), transparent),
    "diff_patch_append": ("Default", Rgba(10, 100, 10), transparent),
    "breakpoint": Rgba(0, 0, 255, 77),
    "dispatching": Rgba(255, 243, 194),
    "ispell": Rgba(255, 255, 0),
    "multicursor_entity": Color("#94C3D7"),
    "multicursor_selection": Color("#96C5D9"),
    "readonly": Color("#e0e0e0"),
    "debugger_current": Rgba(125, 236, 57, 153),
    "current_line": Rgba(226, 226, 226, 102),
    "current_block": Rgba(226, 226, 226),
    "browsers_bg": Rgba(255, 255, 255),
    "keywords_in_aspects": ("DEFAULT", Rgba(0, 109, 151), transparent),
    "comments_in_aspects": ("DEFAULT", Color("#969696"), transparent),
    "numbers_in_aspects": ("DEFAULT", Color("#FF3333"), transparent),
    "strings_in_aspects": ("DEFAULT", Color("#CE7B00"), transparent),
    "preprocessor": ("DEFAULT", Color("#606090"), transparent),
    "bookmarks": Rgba(205, 0, 255, 77),
    "blocks_in_aspects": ("DEFAULT", Color("#60615F"), transparent),
    "types_in_aspects": ("DEFAULT", Color("#009900"), transparent),
    "namespaces": ("DEFAULT", Color("#310142"), transparent),
    "classes": ("DEFAULT", Color("#550073"), transparent),
    "enums": ("DEFAULT", Color("#a38a27"), transparent),
    "interfaces": ("DEFAULT", Color("#8900ba"), transparent),
    "structs": ("DEFAULT", Color("#015878"), transparent),
    "typeparameters": ("DEFAULT", Color("#590034"), transparent),
    "parameters": ("DEFAULT", Color("#008cbf"), transparent),
    "variables": ("DEFAULT", Color("#c93c00"), transparent),
    "propertys": ("DEFAULT", Color("#bf0270"), transparent),
    "enummembers": ("DEFAULT", Color("#c4a321"), transparent),
    "functions": ("DEFAULT", Color("#b500f5"), transparent),
    "modifiers": ("DEFAULT", Color("#00873f"), transparent),
    "operators": ("DEFAULT", Color("#005427"), transparent),
    "deprecateds": ("DEFAULT", Color("#ff0000"), transparent),
    "semantic_readonly": Rgba(242, 255, 237),
}

common_dark = {
    "base_theme": "Adwaita (Dark)",
    "editor_fg": Rgba(186, 186, 186),
    "editor_bg": Rgba(34, 35, 36),
    "browser_decoration_fg": Color("#e0e0e0"),
    "browser_decoration_bg": Color("#343434"),
    "theme_bg": Color("#2280d2"),
    "theme_fg": Color("#2280d2"),
    "theme_selected_fg": white,
    "theme_selected_bg": Rgba(52, 101, 164),
    "caret": white,
    "aspects": ("DEFAULT", Rgba(114, 159, 207), transparent),
    "annotations": ("DEFAULT", transparent, Rgba(96, 96, 96)),
    "hyperlinks": ("DEFAULT", Rgba(114, 159, 207), transparent),
    "strings": ("DEFAULT", Rgba(242, 212, 44), transparent),
    "numbers": ("DEFAULT", Rgba(66, 180, 0), transparent),
    "annotated_comments": ("DEFAULT", Rgba(96, 97, 95), transparent),
    "comments": ("DEFAULT", Rgba(114, 159, 207), transparent),
    "keywords": ("DEFAULT", Rgba(240, 141, 36), transparent),
    "types": ("DEFAULT", Rgba(142, 105, 201), transparent),
    "blocks": ("DEFAULT", Rgba(104, 194, 68), transparent),
    "ephemeral_smart": ("DEFAULT", transparent, Rgba(128, 236, 255, 90)),
    "ephemeral_simple": ("DEFAULT", transparent, Rgba(180, 180, 180, 128)),
    "isearch": Rgba(9, 60, 60),
    "command_window": Rgba(38, 38, 38),
    "search": Rgba(0, 113, 128),
    "messages": Rgba(200, 42, 42),
    "msg_annotations": Rgba(85, 87, 83),
    "msg_info": Rgba(15, 53, 188, 120),
    "msg_high": Rgba(75, 34, 34),
    "msg_medium": Rgba(85, 52, 18),
    "msg_low": Rgba(191, 188, 64, 50),
    "msg_removed": Rgba(160, 160, 160),
    "diff_side_default": Rgba(85, 87, 83),
    "diff_side_change": Rgba(107, 73, 19),
    "diff_side_remove": Rgba(88, 43, 43),
    "diff_side_append": Rgba(38, 68, 36),
    "diff_patch_remove": ("Default", Rgba(239, 41, 41), transparent),
    "diff_patch_append": ("Default", Rgba(115, 210, 22), transparent),
    "breakpoint": Rgba(190, 191, 196),
    "dispatching": Rgba(46, 52, 56),
    "ispell": Rgba(206, 92, 0),
    "multicursor_entity": Color("#3070A0"),
    "multicursor_selection": Color("#3272A2"),
    "readonly": Color("#2E3436"),
    "debugger_current": Rgba(58, 71, 54, 153),
    "current_line": Rgba(48, 51, 51),
    "current_block": Rgba(48, 51, 51),
    "browsers_bg": Rgba(34, 35, 36),
    "keywords_in_aspects": ("DEFAULT", Rgba(173, 127, 168), transparent),
    "comments_in_aspects": ("DEFAULT", Color("#7398BF"), transparent),
    "numbers_in_aspects": ("DEFAULT", Color("#FF3333"), transparent),
    "strings_in_aspects": ("DEFAULT", Color("#CE7B00"), transparent),
    "preprocessor": ("DEFAULT", Color("#606090"), transparent),
    "bookmarks": Rgba(205, 0, 255, 77),
    "blocks_in_aspects": ("DEFAULT", Color("#E6E6E6"), transparent),
    "types_in_aspects": ("DEFAULT", Color("#CCFFCC"), transparent),
    "namespaces": ("DEFAULT", Color("#C8C8C8"), transparent),
    "classes": ("DEFAULT", Color("#4CD5E0"), transparent),
    "enums": ("DEFAULT", Color("#4CE099"), transparent),
    "interfaces": ("ITALIC", Rgba(186, 186, 186), Color("#4CD5E0")),
    "structs": ("DEFAULT", Color("#49D66A"), transparent),
    "typeparameters": ("ITALIC", Rgba(186, 186, 186), Color("#3E52EB")),
    "parameters": ("ITALIC", Rgba(186, 186, 186), Color("#241f31")),
    "variables": ("DEFAULT", Color("#9CDCFE"), transparent),
    "propertys": ("DEFAULT", Color("#8F9DFF"), transparent),
    "enummembers": ("DEFAULT", Color("#93E6BE"), transparent),
    "functions": ("DEFAULT", Color("#DCDCAA"), transparent),
    "modifiers": ("DEFAULT", Color("#569cd6"), transparent),
    "operators": ("DEFAULT", Color("#C586C0"), transparent),
    "deprecateds": ("DEFAULT", Color("#ff0000"), transparent),
    "semantic_readonly": Rgba(62, 62, 62),
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

    def generate_css(self, refresh=False):
        """
        Generate the css from the theme and the user preferences.
        """
        custom_colors = {}
        if refresh:
            for pref_name in prefs_to_color_keys:
                pref = GPS.Preference(pref_name)
                color_keys = prefs_to_color_keys[pref_name]

                if len(color_keys) == 2:
                    pref_val = pref.get().split("@")
                    fg_rgb_str = pref_val[1]
                    bg_rgb_str = pref_val[2]

                    custom_colors[color_keys[0]] = fg_rgb_str
                    custom_colors[color_keys[1]] = bg_rgb_str
                else:
                    pref_val_rgb = pref.get()
                    custom_colors[color_keys[0]] = pref_val_rgb

        def find_color(name):
            if custom_colors and name in custom_colors:
                return custom_colors[name]
            else:
                return self.d[name].to_hex6_string()

        css = css_template.format(**{k: find_color(k) for k in css_colors})
        return css.replace("[", "{").replace("]", "}")

    def apply_preferences(self, provider):
        """Apply the current theme as preferences"""

        # it is possible that values have been added to common_dark,
        # common_light!
        # TODO: to be revised once color prefs are no longer floating
        # around disparate plugins
        if self.light:
            self.d.update({k: common_light[k] for k in common_light if k not in self.d})
        else:
            self.d.update({k: common_dark[k] for k in common_dark if k not in self.d})

        font = GPS.Preference("Src-Editor-Reference-Style").get().split("@")[0]

        def to_pref_str(t):
            """Return a variant pref string from a pref tuple"""
            return "{}@{}@{}".format(t[0], t[1].to_rgba_string(), t[2].to_rgba_string())

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

        # Freeze the preferences

        with gs_utils.freeze_prefs():
            # The theme preference
            pref_set(gtk_theme_pref_name, self.d["base_theme"])

            # The editor fg and bg
            font = GPS.Preference("Src-Editor-Reference-Style").get().split("@")[0]
            GPS.Preference("Src-Editor-Reference-Style").set(
                "{}@{}@{}".format(
                    font,
                    self.d["editor_fg"].to_rgba_string(),
                    self.d["editor_bg"].to_rgba_string(),
                )
            )

            # Apply the theme preferences
            for key in variant_prefs:
                pref_set(variant_prefs[key], to_pref_str(self.d[key]))

            for key in rgb_prefs:
                pref_set(rgb_prefs[key], self.d[key].to_rgba_string())

        # Apply the CSS preference
        css = self.generate_css(refresh=False)
        # load_from_data expect a bytestring
        provider.load_from_data(css.encode())

    def generate_example_label(self):
        """Generate an example Gtk.Label demoing this theme"""
        label_markup = """{keywords}procedure{i} {blocks}Foo{i}
  (An_Integer : {keywords}in out{i} {types}Integer{i} := {numbers}0{i};
   A_String   : {types}String{i}  := {strings}"some text"{i})
  {keywords}with{i} {aspects}Pre  => An_Integer >= -1{i};
{comments}--  Documentation for Foo{i}

{comments}---------
-- Foo --
---------{i}

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

        # Compute the preview's gutter foreground color by mixing the editor's
        # foreground and background color.
        # This formula needs to be synchronized with the formula that computes
        # the 'gutter_color' in gps.css.
        gutter_fg_color = self.d["editor_fg"].mix(self.d["editor_bg"], 0.6)

        for line in label_markup.splitlines():
            prefixed.append(
                '<span color="{}">{:4d} </span> {}'.format(
                    gutter_fg_color.to_hex6_string(), num, line
                )
            )
            num = num + 1

        font = GPS.Preference("Src-Editor-Reference-Style").get().split("@")[0]
        label_markup = '<span font="{}">'.format(font) + "\n".join(prefixed) + "</span>"

        b = Gtk.HBox()
        label = Gtk.Label()
        b.pack_start(label, False, False, 0)
        _, bg = Gdk.Color.parse(self.d["editor_bg"].to_hex6_string())
        _, fg = Gdk.Color.parse(self.d["editor_fg"].to_hex6_string())
        b.modify_bg(Gtk.StateType.NORMAL, bg)
        label.modify_fg(Gtk.StateType.NORMAL, fg)
        process_dict = {"i": "</span>"}
        for key in [
            "keywords",
            "blocks",
            "comments",
            "strings",
            "numbers",
            "aspects",
            "types",
        ]:
            val = self.d[key]
            process_dict[key] = "<span {} {} {}>".format(
                'color="{}"'.format(val[1].to_hex6_string()) if val[1].a != 0.0 else "",
                'background="{}"'.format(val[2].to_hex6_string())
                if val[2].a != 0.0
                else "",
                'font-weight="BOLD"'
                if "BOLD" in val[0]
                else "" + ' font-style="ITALIC"'
                if "ITALIC" in val[0]
                else "",
            )

        label.set_markup(label_markup.format(**process_dict))
        return b
