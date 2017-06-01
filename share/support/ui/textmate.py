""" This is a utility package for importing TextMate .tmTheme definition
    files.
"""

import colorsys
import glob
import os
import plistlib
import sys
import traceback

import GPS
import gps_utils
import colorschemes

light_common = {
    "@theme_selected_bg_color": "#B5D6FE",
    "@editor_bg_selection":     "#B5D6FE",
    "@editor_fg_selection":     "black",

    "Src-Editor-Aspects-Variant": ("DEFAULT", "rgb(96,97,95)", "transparent"),

    "Src-Editor-Code-Annotations-Variant-18": (
        "DEFAULT", "transparent", "rgb(224, 224, 224)"),

    "Src-Editor-Ephemeral-Smart": (
        "DEFAULT", "transparent", "rgba(252,172,79,0.4)"),

    "Src-Editor-Ephemeral-Simple": (
        "DEFAULT", "transparent", "rgba(134,134,134,0.35)"),

    'Plugins/isearch/nextmatchcolor': 'cyan',
    'Command-Windows-Background-Color': 'white',

    "Search-Src-Highlight-Color": "rgb(189,215,255)",
    "Messages-Highlight-Color": "rgb(255,0,0)",
    "Errors-Src-Highlight-Color": "rgb(255,183,183)",
    "Warnings-Src-Highlight-Color": "rgb(255,204,156)",
    "Style-Src-Highlight-Color": "rgb(255,255,173)",
    "Info-Src-Highlight-Color": "rgb(173,255,194)",
    "High-Importance-Messages-Highlight": "rgb(236, 197, 197)",
    "Medium-Importance-Messages-Highlight": "rgb(255,218,185)",
    "Low-Importance-Messages-Highlight": "rgb(255,255,240)",
    "Horizontal-Diff-Change-Color": "rgb(253,230,106)",
    "Diff-Change-Color": "rgb(236,236,170)",
    "Diff-Remove-Color": "rgb(255,160,160)",
    "Diff-Append-Color": "rgb(136,238,170)",
    "Debugger-Line-With-Breakpoint": "rgba(0,0,255,0.3)",

    "Plugins/dispatching/color": "rgb(255,243,194)",
    "Plugins/ispell/bgcolor": "rgb(255,255,0)",
    "Plugins/multi_cursors/multicursor_on_entity_color": "#94C3D7",
    "Plugins/multi_cursors/multicursor_selection_color": "#96C5D9",
}

dark_common = {
    "@theme_selected_bg_color": "#2280d2",
    "@editor_bg_selection":     "#2280d2",
    "@editor_fg_selection":     "white",

    "Src-Editor-Aspects-Variant": ("DEFAULT",
                                   "rgb(117,113,94)",
                                   "transparent"),

    "Src-Editor-Code-Annotations-Variant-18": (
        "DEFAULT", "transparent", "rgb(96, 96, 96)"),

    "Src-Editor-Ephemeral-Smart": (
        "DEFAULT", "transparent", "rgba(128,236,255,0.35)"),

    "Src-Editor-Ephemeral-Simple": (
        "DEFAULT", "transparent", "rgba(180,180,180,0.5)"),

    'Plugins/isearch/nextmatchcolor': 'rgb(9,60,60)',
    'Command-Windows-Background-Color': 'rgb(38, 38, 38)',

    "Search-Src-Highlight-Color": "rgb(0,113,128)",
    "Messages-Highlight-Color": "rgb(200,42,42)",
    "Errors-Src-Highlight-Color": "rgb(75,34,34)",
    "Warnings-Src-Highlight-Color": "rgb(85,52,18)",
    "Style-Src-Highlight-Color": "rgb(68,12,42)",
    "Info-Src-Highlight-Color": "rgb(53,77,59)",
    "High-Importance-Messages-Highlight": "rgb(255,99,71)",
    "Medium-Importance-Messages-Highlight": "rgb(255,140,0)",
    "Low-Importance-Messages-Highlight": "rgb(189,183,107)",
    "Horizontal-Diff-Change-Color": "rgb(143,89,2)",
    "Diff-Change-Color": "rgb(107,73,19)",
    "Diff-Remove-Color": "rgb(88,43,43)",
    "Diff-Append-Color": "rgb(38,68,36)",
    "Debugger-Line-With-Breakpoint": "rgb(190,191,196)",

    "Plugins/dispatching/color": "rgb(46,52,56)",
    "Plugins/ispell/bgcolor": "rgb(206,92,0)",

    "Plugins/multi_cursors/multicursor_on_entity_color": "#3070A0",
    "Plugins/multi_cursors/multicursor_selection_color": "#3272A2",
}

text_variant_prefs = {
    "comment":                "Src-Editor-Comments-Variant",
    "constant.numeric":       "Src-Editor-Numbers-Variant",
    "constant.character":     "Src-Editor-Strings-Variant",
    "constant.language":      "Src-Editor-Aspects-Variant",  # because why not
    "keyword":                "Src-Editor-Keywords-Variant",
    "entity.name.type":       "Src-Editor-Type-Variant",
    "entity.name.type.class": "Src-Editor-Type-Variant",
    "storage.type":           "Src-Editor-Type-Variant",
    "string":                 "Src-Editor-Strings-Variant",
    "entity.name.function":   "Src-Editor-Block-Variant",
    "string.other.link":      "Src-Editor-Hyper-Links-Variant",
    "constant.character.escape":      "Editor/General/string_escapes",
    "constant.preprocessor.c":        "Editor/General/preprocessor",
    "constant.preprocessor.c.entity": "Editor/General/preprocessor",
    "meta.preprocessor":              "Editor/General/preprocessor",
}

color_prefs = {
    "entity.name.function":   "Src-Editor-Current-Block-Color",
}


def to_GPS_prefs(d):
    """ Considering a dictionary d representing prefs in the theme plist,
        attempt to find a matching GPS preference. When found, return
        a list of (key, value) where key is the name of a GPS preference, and
        value the value which is represented in d. Otherwise return [].
    """

    if 'scope' not in d:
        return []

    # Find all scopes in scope

    scopes = [s.strip() for s in d['scope'].split(',')]

    prefs = []

    for scope in scopes:
        if scope in text_variant_prefs:
            s = d['settings']
            fg, bg, style = "transparent", "transparent", "DEFAULT"

            if 'foreground' in s:
                fg = s['foreground']
            if 'background' in s:
                bg = s['background']
            if 'fontStyle' in s:
                theme_style = s['fontStyle']
                if 'bold' in theme_style and 'italic' in theme_style:
                    style = 'BOLD_ITALIC'
                elif 'bold' in theme_style:
                    style = 'BOLD'
                elif 'italic' in theme_style:
                    style = 'DEFAULT'

            prefs.append((text_variant_prefs[scope], (style, fg, bg)))

        if scope in color_prefs:
            s = d['settings']
            fg = "transparent"

            if 'foreground' in s:
                fg = s['foreground']

            prefs.append((color_prefs[scope], fg))

    return prefs


def to_rgb(color, out_of=255.0):
    """ Return the r, g, b (between 0 and out_of) of a color given in #rrggbb
        format.
    """
    return (int(color[1 + 2*x:1 + 2*x+2], 16) * out_of / 255.0
            for x in xrange(3))


def to_rgba(color, opacity=0.4, force_alpha=False):
    """ Take a color of the form "#RRGGBB" or "RRGGBBAA" and return it in the
        form "rgba(r,g,b,a)". Use given opacity as default opacity.
    """
    the_opacity = opacity

    if not force_alpha:
        if len(color) == 9:
            the_opacity = float(int(color[7:9], 16)) / 256.0

    r, g, b = to_rgb(color)
    return "rgba(%s,%s,%s,%s)" % (r, g, b, the_opacity)


def get_luminosity(color):
    """ Return as a float between 0 and 255.0 the luminosity of color.
        Color should be given in hex string format, for instance "#5c66b2".
    """
    r, g, b = to_rgb(color, 1.0)
    h, l, s = colorsys.rgb_to_hls(r, g, b)
    return l * 255.0


def lighten(color, amount):
    """ If amount is positive, lighten the color. Otherwise, shade.
    """
    r, g, b = to_rgb(color, 1.0)
    h, l, s = colorsys.rgb_to_hls(r, g, b)

    l = l + amount

    if l < 0.0:
        l = 0.0
    if l > 1.0:
        l = 1.0

    r, g, b = colorsys.hls_to_rgb(h, l, s)

    def c(x):
        return int(x * 255.0)
    return '#{:02x}{:02x}{:02x}'.format(c(r), c(g), c(b))


def mix(a, b, coef=0.5):
    """ Return a mix of colors a and b, specified in #rrggbb format.
        Coef is where to place the cursor: 0.0 for color a, 1.0 for color b.
    """
    ra, ga, ba = to_rgb(a, 1.0)
    rb, gb, bb = to_rgb(b, 1.0)

    ha, la, sa = colorsys.rgb_to_hls(ra, ga, ba)
    hb, lb, sb = colorsys.rgb_to_hls(rb, gb, bb)

    h = (1 - coef) * ha + coef * hb
    s = (1 - coef) * sa + coef * sb
    l = (1 - coef) * la + coef * lb

    r, g, b = colorsys.hls_to_rgb(h, l, s)

    def c(x):
        return int(x * 255.0)
    return '#{:02x}{:02x}{:02x}'.format(c(r), c(g), c(b))


class Theme(object):

    def __init__(self, filename):
        """ Load filename as a color scheme file in GPS.
            The file should be in TextMate (.tmTheme) format.
        """

        self.o = plistlib.readPlist(filename)
        self.name = self.o['name']

        # for convenience
        self.general = self.o['settings'][0]['settings']

    def theme_dict(self):
        """ Return a dictionary of preferences
        """

        d = {}  # The result dict

        d['name'] = self.name

        if 'foreground' in self.general and 'background' in self.general:
            fg = self.general['foreground']
            bg = self.general['background']
        else:
            # Warn if theme does not provide these essential elements
            GPS.Console("Messages").write(
                "Background or foreground not found in textmate theme '%s'.\n"
                % self.name)

            fg = "#000000"
            bg = "#ffffff"

        # The right way to get the luminosity of a theme is to compare the
        # background and the foreground.

        is_light = get_luminosity(bg) > get_luminosity(fg)
        light_val = is_light * 1 + (not is_light) * -1  # to use in equations

        # Go through all the scopes being specified, and attempt to find
        # a match for a GPS pref
        for theme_pref_dict in self.o['settings'][1:]:
            prefs = to_GPS_prefs(theme_pref_dict)
            for p in prefs:
                d[p[0]] = p[1]

        # Get the general settings

        d['@text_color'] = fg
        d['@base_color'] = bg
        d["Src-Editor-Reference-Style"] = ("${editorfont}", fg, bg)
        d["General-Default-Font"] = "${font}"

        # Compute the selection

        # ??? The selection is generally too close to the line highlight
        # in textmate themes: do not read the selection from these themes.

        # if 'selection' in self.general:
        #     d['@theme_selected_bg_color'] = self.general['selection']
        #     d['@editor_bg_selection'] = self.general['selection']

        # ... instead, invert the default fg and bg.

        d['@editor_bg_selection'] = self.general['foreground']
        d['@editor_fg_selection'] = self.general['background']

        # Compute nice gutter settings

        d['@gutter_color'] = lighten(fg, 0.07 * light_val)
        d['@gutter_background'] = lighten(bg, -0.05 * light_val)

        # Compute nice browser settings

        bb = bg
        if "Browsers-Bg-Color" in d:
            bb = d["Browsers-Bg-Color"]

        d["Browsers-Bg-Color"] = bb

        d['@browser_decoration_color'] = lighten(fg, 0.07 * light_val)
        d['@browser_decoration_background'] = lighten(bb, -0.05 * light_val)

        # Caret

        if 'caret' in self.general:
            # The following preferences are derived from the caret color:
            # caret, current line, and current block.

            d["@caret"] = self.general['caret']

        # Ignore the line highlight in textmate themes. instead,
        # compute them from the background.

        d["Src-Editor-Current-Line-Color"] = to_rgba(
            lighten(bg, -0.15 * light_val), 0.5, force_alpha=True)
        d["Src-Editor-Current-Block-Color"] = to_rgba(
            lighten(fg, 0.25 * light_val), 0.5, force_alpha=True)
        d["Src-Editor-Code-Annotations-Variant-18"] = (
            "DEFAULT", "transparent",
            to_rgba(lighten(bg, -0.15 * light_val), 0.5, force_alpha=True))

        # Compute values for the auto-highlight-occurrences

        d["Src-Editor-Ephemeral-Simple"] = (
            "DEFAULT", "transparent",
            # For the simple case, mix the normal fg and bg, then lighten
            # and make it transparent
            to_rgba(lighten(mix(fg, bg, 0.3), 0.05 * light_val),
                    0.6,
                    force_alpha=True))

        kw = fg
        # Attempt to find the color for the keywords
        if "Src-Editor-Keywords-Variant" in d:
            kw = d["Src-Editor-Keywords-Variant"][1]

        d["Src-Editor-Ephemeral-Smart"] = (
            "DEFAULT", "transparent",
            # For the smart case, use the keyword foreground as background
            to_rgba(kw, 0.6, force_alpha=True))
        # Compute the general luminosity of the theme, for sorting purposes

        luminosity = get_luminosity(self.general['background'])
        d['@luminosity'] = luminosity

        if is_light:
            d["GPS6-Gtk-Theme-Name"] = "Adwaita"
            return gps_utils.Chainmap(d, light_common)
        else:
            d["GPS6-Gtk-Theme-Name"] = "Adwaita (Dark)"
            return gps_utils.Chainmap(d, dark_common)


def textmate_themes():
    """ Find all themes installed in the color_themes directory
        and return them as a list of dictionary objects.
    """

    results = []

    for file in glob.glob(os.path.join(
        GPS.get_system_dir(),
            'share', 'gps', 'color_themes', 'themes', '*', '*.tmTheme')):
        try:
            results.append(Theme(file).theme_dict())
        except:
            msg, _, tb = sys.exc_info()
            tb = "\n".join(traceback.format_list(traceback.extract_tb(tb)))

            GPS.Console("Messages").write(
                "Exception when parsing theme file '%s':\n%s\n%s\n"
                % (file, msg, str(tb)))

    return results
