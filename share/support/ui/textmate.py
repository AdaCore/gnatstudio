""" This is a utility package for importing TextMate .tmTheme definition
    files.
"""

import glob
import os
import plistlib
import sys
import traceback

import GPS
from theme_handling import Theme, Color, transparent

text_variant_prefs = {
    "comment":                "comments",
    "constant.numeric":       "numbers",
    "constant.character":     "strings",
    "constant.language":      "aspects",  # because why not
    "keyword":                "keywords",
    "entity.name.type.class": "types",
    "entity.name.type":       "types",
    "string":                 "strings",
    "entity.name.function":   "blocks",
    "string.other.link":      "hyperlinks",

    # TODO: convert these
    #    "constant.character.escape":      "Editor/General/string_escapes",
    #    "constant.preprocessor.c":        "Editor/General/preprocessor",
    #    "constant.preprocessor.c.entity": "Editor/General/preprocessor",
    #    "meta.preprocessor":              "Editor/General/preprocessor",
}

color_prefs = {
    "entity.name.function":   "current_block",
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
            fg, bg, style = transparent, transparent, "DEFAULT"

            if 'foreground' in s:
                fg = Color(s['foreground'])
            if 'background' in s:
                bg = Color(s['background'])
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
            fg = transparent

            if 'foreground' in s:
                fg = Color(s['foreground'])

            prefs.append((color_prefs[scope], fg))

    return prefs


class TextmateTheme(object):

    def __init__(self, filename):
        """ Load filename as a color scheme file in GPS.
            The file should be in TextMate (.tmTheme) format.
        """

        self.o = plistlib.readPlist(filename)
        self.name = self.o['name']

        # for convenience
        self.general = self.o['settings'][0]['settings']

    def theme(self):
        """ Return a dictionary of preferences
        """
        d = {}  # The result dict

        d['name'] = self.name

        if 'foreground' in self.general and 'background' in self.general:
            fg_color = Color(self.general['foreground'])
            # Do not use transparency for the foreground
            fg_color.a = 1.0
            bg_color = Color(self.general['background'])
        else:
            # Warn if theme does not provide these essential elements
            GPS.Console("Messages").write(
                "Background or foreground not found in textmate theme '%s'.\n"
                % self.name)

            fg_color = Color("#000000")
            bg_color = Color("#ffffff")

        fg_lum = fg_color.get_luminosity()
        bg_lum = bg_color.get_luminosity()

        # The right way to get the luminosity of a theme is to compare the
        # background and the foreground.

        is_light = bg_lum > fg_lum
        light_val = is_light * 1 + (not is_light) * -1  # to use in equations

        # Go through all the scopes being specified, and attempt to find
        # a match for a GPS pref
        for theme_pref_dict in self.o['settings'][1:]:
            prefs = to_GPS_prefs(theme_pref_dict)
            for p in prefs:
                d[p[0]] = p[1]

        # Get the general settings

        d["editor_fg"] = fg_color
        d["editor_bg"] = bg_color

        # Compute the selection

        # ??? The selection is generally too close to the line highlight
        # in textmate themes: do not read the selection from these themes.

        # if 'selection' in self.general:
        #     d['@theme_selected_bg_color'] = self.general['selection']
        #     d['@editor_bg_selection'] = self.general['selection']

        # ... instead, invert the default fg and bg.

        d['theme_selected_bg'] = fg_color
        d['theme_selected_fg'] = bg_color

        # Compute nice browser settings

        d["browsers_bg"] = bg_color

        d['browser_decoration_fg'] = fg_color.lighten(0.07 * light_val)
        d['browser_decoration_bg'] = bg_color.lighten(-0.05 * light_val)

        # Caret

        if 'caret' in self.general:
            # The following preferences are derived from the caret color:
            # caret, current line, and current block.

            d["caret"] = Color(self.general['caret'])

        # Ignore the line highlight in textmate themes. instead,
        # compute them from the background.

        cl_color = bg_color.lighten(-0.15 * light_val)
        cl_color.a = 0.5
        d["current_line"] = cl_color

        cb_color = fg_color.lighten(0.25 * light_val)
        cb_color.a = 0.5
        d["current_block"] = cb_color
        d["annotations"] = ("DEFAULT", transparent, cl_color)

        # Compute values for the auto-highlight-occurrences

        # For the simple case, compute the color by lightening accordingly
        # the editor's bakckground color.
        e_simple_color = bg_color.lighten(-0.25 * light_val)
        e_simple_color.a = 0.5
        d["ephemeral_simple"] = ("DEFAULT", transparent, e_simple_color)

        # For the smart cases, use preferably the keywords fg color but
        # applying some alpha to it. Otherwise fallback to the default
        # foreground color with some alpha too.
        e_smart_color = Color(from_hex=fg_color.to_hex6_string())
        if "keywords" in d:
            e_smart_color = Color(from_hex=d["keywords"][1].to_hex6_string())

        e_smart_color.a = 0.2

        d["ephemeral_smart"] = (
            "DEFAULT", transparent,
            e_smart_color)

        return Theme(self.name, is_light, d)


def textmate_themes():
    """ Find all themes installed in the color_themes directory
        and return them as a list of dictionary objects.
    """

    results = []

    default_themes = glob.glob(os.path.join(
        GPS.get_system_dir(),
        'share', 'gps', 'color_themes', 'themes', '*', '*.tmTheme'))

    user_themes = glob.glob(os.path.join(
        GPS.get_home_dir(), 'themes', '*.tmTheme'))

    for file in default_themes + user_themes:
        try:
            results.append(TextmateTheme(file).theme())
        except Exception:
            msg, _, tb = sys.exc_info()
            tb = "\n".join(traceback.format_list(traceback.extract_tb(tb)))

            GPS.Console("Messages").write(
                "Exception when parsing theme file '%s':\n%s\n%s\n"
                % (file, msg, str(tb)))

    return results
