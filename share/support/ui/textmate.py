"""
This is a utility package for importing TextMate .tmTheme definition
files.

Dictionary mapping TextMate scope prefixes to semantic token style name
prefixes.  A scope is matched by prefix: any suffix appended after the
base scope name becomes a modifier suffix on the style name.  For example,
the scope "entity.name.variable.readonly" matches the prefix
"entity.name.variable" and produces the style "variable.readonly".

To add custom styles for specific token/modifier combinations in a
.tmTheme file, use the following scope naming convention:

  Base token scopes (one style per token type):
    entity.other.namespace        -> namespace
    entity.name.type              -> type
    entity.name.type.class.semantic -> class
    entity.name.enum              -> enum
    entity.name.interface         -> interface
    entity.name.struct            -> struct
    entity.name.typeparameter     -> typeparameter
    entity.name.parameter         -> parameter
    entity.name.variable          -> variable
    entity.name.property          -> property
    entity.name.enum.member       -> enummember
    entity.other.function         -> function
    keyword                       -> keyword
    entity.name.modifier          -> modifier
    comment                       -> comment
    string / constant.character   -> string
    constant.numeric              -> number
    entity.name.operator          -> operator

  Modifier variants (append modifier to base scope):
    entity.name.variable.readonly           -> variable.readonly
    entity.name.variable.globalvariable     -> variable.globalvariable
    entity.name.variable.localvariable      -> variable.localvariable
    entity.name.variable.declaration        -> variable.declaration
    entity.name.variable.deprecated         -> variable.deprecated
    entity.name.parameter.readonly          -> parameter.readonly
    entity.name.parameter.deprecated        -> parameter.deprecated
    entity.other.function.deprecated        -> function.deprecated
    entity.name.type.deprecated             -> type.deprecated
    entity.name.enum.deprecated             -> enum.deprecated
    entity.name.type.class.semantic.deprecated -> class.deprecated
    (and so on for any other token + modifier combination)

  The "deprecated" style (fallback when no type-specific variant exists):
    entity.name.deprecated                  -> deprecated

Example .tmTheme entry to highlight global variables in bold red:
  <dict>
    <key>scope</key>
    <string>entity.name.variable.globalvariable</string>
    <key>settings</key>
    <dict>
      <key>foreground</key><string>#CC0000</string>
      <key>fontStyle</key><string>bold</string>
    </dict>
  </dict>
"""

import glob
import os
import plistlib
import sys
import traceback

import GPS
from theme_handling import Theme, Color, transparent

text_variant_prefs = {
    "comment": "comments",  # Comments
    "comment.todo": "comment_notes",  # TODO and NOTE in comments
    "comment.aspect": "comments_in_aspects",  # Comments in aspect
    "comment.annotated": "annotated_comments",  # SPARK Annotations
    "keyword": "keywords",  # Keywords
    "keyword.aspect": "keywords_in_aspects",  # Keywords in aspect
    "constant.numeric": "numbers",  # Numbers
    "constant.numeric.aspect": "numbers_in_aspects",  # Numbers in aspect
    "constant.character": "strings",  # Strings
    "constant.character.escape": "string escapes",  # String escapes
    "constant.language": "aspects",  # Ada/SPARK aspects
    "constant.preprocessor.c": "preprocessor",  # Preprocessor
    "constant.preprocessor.c.entity": "preprocessor",  # Preprocessor
    "constant.ephemeral": "ephemeral_simple",  # Ephemeral highlighting (simple)
    "entity.name.type": "types",  # Types
    "entity.name.type.class": "types",  # Types
    "entity.name.type.aspect": "types_in_aspects",  # Types in ghost
    "entity.name.aspect": "blocks_in_aspects",  # Ghost names
    "entity.name.function": "blocks",  # Block highlighting
    "entity.other.ephemeral": "ephemeral_smart",  # Ephemeral highlighting (smart)
    "string": "strings",  # Strings
    "string.aspect": "strings_in_aspects",  # Strings in aspect
    "string.other.link": "hyperlinks",  # Hyper links
    "meta.preprocessor": "preprocessor",  # Preprocessor
    "meta.annotations": "annotations",  # Code annotations
    "meta.block": "blocks",  # Block highlighting
}
# Maps TextMate scopes to variant preference keys (traditional syntax
# highlighting).  Scopes for semantic token types (entity.name.variable,
# entity.name.parameter, etc.) are NOT listed here: they are handled
# exclusively by text_style_prefs below and consumed by apply_styles().

color_prefs = {
    "meta.block.current": "current_block",  # Current block color
    "meta.readonly": "readonly",  # Read-only code
    "meta.bookmark": "bookmarks",  # Lines with a bookmark
    "meta.multicursor_selection": "multicursor_selection",  # Multi cursor selection
}

text_style_prefs = {
    "entity.other.namespace": "namespace",
    "entity.name.type": "type",
    "entity.name.type.class": "type",
    "entity.name.type.class.semantic": "class",
    "entity.name.enum": "enum",
    "entity.name.interface": "interface",
    "entity.name.struct": "struct",
    "entity.name.typeparameter": "typeparameter",
    "entity.name.parameter": "parameter",
    "entity.name.variable": "variable",
    "entity.name.property": "property",
    "entity.name.enum.member": "enummember",
    "entity.other.function": "function",
    "entity.name.function": "function",
    "keyword": "keyword",
    "entity.name.modifier": "modifier",
    "comment": "comment",
    "constant.character": "string",
    "string": "string",
    "constant.numeric": "number",
    "entity.name.operator": "operator",
    "entity.name.deprecated": "deprecated",
}


def to_GPS_prefs(d):
    """Considering a dictionary d representing prefs in the theme plist,
    attempt to find a matching GPS preference. When found, return
    a list of (key, value) where key is the name of a GPS preference, and
    value the value which is represented in d. Otherwise return [].
    """

    if "scope" not in d:
        return []

    # Find all scopes in scope

    scopes = [s.strip() for s in d["scope"].split(",")]

    prefs = []

    for scope in scopes:
        s = d["settings"]

        # Variant preferences (traditional syntax highlighting).
        # No 'continue': the scope may also match a semantic style below.
        if scope in text_variant_prefs:
            fg, bg, style = transparent, transparent, "DEFAULT"

            if "foreground" in s:
                fg = Color(s["foreground"])
            if "background" in s:
                bg = Color(s["background"])
            if "fontStyle" in s:
                theme_style = s["fontStyle"]
                if "bold" in theme_style and "italic" in theme_style:
                    style = "BOLD_ITALIC"
                elif "bold" in theme_style:
                    style = "BOLD"
                elif "italic" in theme_style:
                    style = "ITALIC"

            prefs.append((text_variant_prefs[scope], (style, fg, bg)))

        # Color preferences
        if scope in color_prefs:
            fg = transparent

            if "foreground" in s:
                fg = Color(s["foreground"])

            prefs.append((color_prefs[scope], fg))
            continue

        # Semantic token styles — use longest matching prefix so that
        # e.g. "entity.name.type.class.semantic" matches the full key
        # rather than the shorter "entity.name.type" prefix.
        best_prefix = ""
        for typ in text_style_prefs:
            if scope.startswith(typ) and len(typ) > len(best_prefix):
                best_prefix = typ

        if best_prefix:
            fg, bg, style = transparent, transparent, "NONE"
            underline, underline_color = "_NONE", transparent
            # NONE is the value of the underline, so use _NONE as an
            # uninitialized value
            strikethrough, strikethrough_color = "NONE", transparent

            if "foreground" in s:
                fg = Color(s["foreground"])
            if "background" in s:
                bg = Color(s["background"])
            if "fontStyle" in s:
                theme_style = s["fontStyle"]
                if "bold" in theme_style and "italic" in theme_style:
                    style = "BOLD_ITALIC"
                elif "bold" in theme_style:
                    style = "BOLD"
                elif "italic" in theme_style:
                    style = "ITALIC"
                elif "normal" in theme_style:
                    style = "DEFAULT"

            if "underline" in s:
                theme_underline = s["underline"]
                if "none" in theme_underline:
                    underline = "NONE"
                elif "single" in theme_underline:
                    underline = "SINGLE"
                elif "double" in theme_underline:
                    underline = "DOUBLE"
                elif "error" in theme_underline:
                    underline = "ERROR"
            if "underline_color" in s:
                underline_color = Color(s["underline_color"])

            if "strikethrough" in s:
                theme_strikethrough = s["strikethrough"]
                if "true" in theme_strikethrough:
                    strikethrough = "TRUE"
                elif "false" in theme_strikethrough:
                    strikethrough = "FALSE"
            if "strikethrough_color" in s:
                strikethrough_color = Color(s["strikethrough_color"])

            style_name = scope.replace(best_prefix, text_style_prefs[best_prefix])
            prefs.append(
                (
                    style_name,
                    (
                        style,
                        fg,
                        bg,
                        [underline, underline_color],
                        [strikethrough, strikethrough_color],
                    ),
                )
            )

    return prefs


class TextmateTheme(object):
    def __init__(self, filename):
        """Load filename as a color scheme file in GS.
        The file should be in TextMate (.tmTheme) format.
        """

        with open(filename, "rb") as f:
            self.o = plistlib.load(f)
        self.name = self.o["name"]

        # for convenience
        self.general = self.o["settings"][0]["settings"]

    def theme(self):
        """Return a dictionary of preferences"""
        d = {}  # The result dict

        d["name"] = self.name

        if "foreground" in self.general and "background" in self.general:
            fg_color = Color(self.general["foreground"])
            # Do not use transparency for the foreground
            fg_color.a = 1.0
            bg_color = Color(self.general["background"])
        else:
            # Warn if theme does not provide these essential elements
            GPS.Console("Messages").write(
                "Background or foreground not found in textmate theme '%s'.\n"
                % self.name
            )

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
        for theme_pref_dict in self.o["settings"][1:]:
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

        d["theme_selected_bg"] = fg_color
        d["theme_selected_fg"] = bg_color

        # Compute nice browser settings

        d["browsers_bg"] = bg_color

        d["browser_decoration_fg"] = fg_color.lighten(0.07 * light_val)
        d["browser_decoration_bg"] = bg_color.lighten(-0.05 * light_val)

        # Caret

        if "caret" in self.general:
            # The following preferences are derived from the caret color:
            # caret, current line, and current block.

            d["caret"] = Color(self.general["caret"])

        # Current line
        cl_color = bg_color.lighten(-0.15 * light_val)
        cl_color.a = 0.5

        if "lineHighlight" in self.general:
            d["current_line"] = Color(self.general["lineHighlight"])

        else:
            # Compute them from the background if not specified
            d["current_line"] = cl_color

        if "current_block" not in d:
            # Compute them from the background if not specified
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

        d["ephemeral_smart"] = ("DEFAULT", transparent, e_smart_color)

        return Theme(self.name, is_light, d)


def textmate_themes():
    """Find all themes installed in the color_themes directory
    and return them as a list of dictionary objects.
    """

    results = []

    default_themes = glob.glob(
        os.path.join(
            GPS.get_system_dir(),
            "share",
            "gnatstudio",
            "color_themes",
            "themes",
            "*",
            "*.tmTheme",
        )
    )

    user_themes = glob.glob(os.path.join(GPS.get_home_dir(), "themes", "*.tmTheme"))

    for file in default_themes + user_themes:
        try:
            results.append(TextmateTheme(file).theme())
        except Exception:
            msg, _, tb = sys.exc_info()
            tb = "\n".join(traceback.format_list(traceback.extract_tb(tb)))

            GPS.Console("Messages").write(
                "Exception when parsing theme file '%s':\n%s\n%s\n"
                % (file, msg, str(tb))
            )

    return results


def parse_style_overrides(filename):
    """Parse a .tmTheme file and return a dict of style overrides.

    Unlike TextmateTheme.theme(), this does NOT build a full Theme object:
    it only extracts the scope-to-preference mappings.  The result can be
    merged directly into an existing Theme.d dictionary, allowing users to
    customise semantic highlighting on top of any selected theme.
    """
    overrides = {}
    with open(filename, "rb") as f:
        o = plistlib.load(f)

    for entry in o.get("settings", [])[1:]:
        for key, val in to_GPS_prefs(entry):
            overrides[key] = val

    return overrides
