"""
This plugin provides a new preference that allows users to
select a color scheme.

These themes are inspired from:
    http://colorsublime.com/?page=5
"""



import GPS
from gi.repository import Gtk, Gdk
import gps_utils


# Colors should use "rgb()" or "rgba()" format, not "#...". This is so that
# __on_preferences_changed can detect changes done to the preferences and set
# the color scheme to "Custom" appropriately.

themes = [
   {"name": "Default",
    "GPS6-Gtk-Theme-Name": "Adwaita",
    "@theme_bg_color": None,           # the background color for windows
    "@theme_fg_color": None,           # default color for the text
    "@theme_selected_bg_color": None,  # selection in trees or menus
    "@editor_bg_selection": "#4A90D9",
    "General-Default-Style":                 ("${font}", "rgb(0,0,0)", "rgb(255,255,255)"),
    "Debugger-Editor-Current-Line":          "rgba(125,236,57,0.6)",
    "Src-Editor-Current-Line-Color":         "rgb(209,220,252)",
    "Src-Editor-Reference-Style":            ("${editorfont}", "rgb(0,0,0)", "rgb(255,255,255)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "rgb(0,0,255)",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "rgb(206,123,0)", "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT", "rgb(255,51,51)", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "rgb(96,97,95)", "transparent"),
    "Src-Editor-Aspects-Variant":            ("DEFAULT", "rgb(96,97,95)", "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT", "rgb(150,150,150)", "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT", "rgb(0,0,230)", "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT", "rgb(0,153,0)", "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT", "rgb(96,97,95)", "transparent")
   },
   {"name": "Darkside",
    "GPS6-Gtk-Theme-Name": "Adwaita (Dark)",
    "@theme_bg_color": "#222324",
    "@editor_bg_selection": "#49483E",
    "General-Default-Style":                 ("${font}", "rgb(186,186,186)", "rgb(34,35,36)"),
    "Debugger-Editor-Current-Line":          "rgba(58,71,54,0.6)",
    "Src-Editor-Current-Line-Color":         "rgb(48,51,51)",
    "Src-Editor-Reference-Style":            ("${editorfont}", "rgb(186,186,186)", "rgb(34,35,36)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "rgb(0,0,255)",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "rgb(242,212,44)", "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT", "rgb(255,51,51)", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "rgb(114,159,207)", "transparent"),
    "Src-Editor-Aspects-Variant":            ("DEFAULT", "rgb(114,159,207)", "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT", "rgb(114,159,207)", "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT", "rgb(240,141,36)", "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT", "rgb(142,105,201)", "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT", "rgb(104,194,68)", "transparent")
   },
   {"name": "Monokai",
    "GPS6-Gtk-Theme-Name": "Adwaita (Dark)",
    "@theme_bg_color": "#272822",
    "@theme_fg_color": "#F8F8F2",
    "@theme_selected_bg_color": "#49483E",
    "@editor_bg_selection": "#49483E",
    "General-Default-Style":                 ("${font}", "rgb(248,248,242)", "rgb(39,40,34)"),
    "Debugger-Editor-Current-Line":          "rgba(58,71,54,0.6)",
    "Src-Editor-Current-Line-Color":         "rgb(73,72,62)",
    "Src-Editor-Reference-Style":            ("${editorfont}", "rgb(248,248,242)", "rgb(39,40,34)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "rgb(0,0,255)",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "rgb(230,219,116)", "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT", "rgb(255,51,51)", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "rgb(117,113,94)", "transparent"),
    "Src-Editor-Aspects-Variant":            ("DEFAULT", "rgb(117,113,94)", "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT", "rgb(117,113,94)", "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT", "rgb(249,38,114)", "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT", "rgb(102,217,239)", "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT", "rgb(230,219,116)", "transparent")
   },
   {"name": "iPlastic",
    "GPS6-Gtk-Theme-Name": "Adwaita",
    "@editor_bg_selection": "#4A90D9",
    "General-Default-Style":                 ("${font}", "rgb(0,0,0)", "rgb(238,238,238)"),
    "Debugger-Editor-Current-Line":          "rgba(125,236,57,0.6)",
    "Src-Editor-Current-Line-Color":         "rgb(140,140,140)",
    "Src-Editor-Reference-Style":            ("${editorfont}", "rgb(0,0,0)", "rgb(238,238,238)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "rgb(0,0,255)",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "rgb(0,153,51)", "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT", "rgb(255,51,51)", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "rgb(0,102,255)", "transparent"),
    "Src-Editor-Aspects-Variant":            ("DEFAULT", "rgb(0,102,255)", "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT", "rgb(0,102,255)", "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT", "rgb(0,0,255)", "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT", "rgb(102,217,239)", "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT", "rgb(255,128,0)", "transparent")
   },
]


class Color_Theme_Switcher(object):

    pref_name = "General/ColorTheme"
    pref_gtk_theme = "GPS6-Gtk-Theme-Name"
    gtkpref_name = "/ColorTheme gtk+"

    def __init__(self):
        args = ["Custom"] + [t["name"] for t in themes]
        p = GPS.Preference(self.pref_name)
        p.create(
            "Color Theme",  # label
            "enum",         # type
            "Selecting a color scheme provides default values for a number of"
               + " other preferences, which can still be changed independently",
            0,   # Custom
            *args)
        self.current = p.get()

        GPS.Preference(self.gtkpref_name).create("", "string", "", "")

        self.provider = Gtk.CssProvider()
        screen = Gdk.Display.get_default().get_default_screen()
        Gtk.StyleContext.add_provider_for_screen(screen, self.provider, 901)

        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)

    def __del__(self):
        GPS.Hook("preferences_changed").remove(self.__on_preferences_changed)

    def __set_gtk_properties(self):
        c = GPS.Preference(self.gtkpref_name).get()
        if c == "":
            self.provider.load_from_data("*{}")  # Clear contents
        else:
            self.provider.load_from_data(c)

    def __for_each_pref(self, theme, cb):
        """For each preference defined in the theme, calls cb with:
               cb(name, value)
           where value is the string value.
        """

        default = GPS.Preference("General-Default-Style").get().split("@")[0]
        font = GPS.Preference("Src-Editor-Reference-Style").get().split("@")[0]

        def subst(s):
            return s.replace("${font}", default) \
                    .replace("${editorfont}", font) \
                    .replace("transparent", "rgba(0,0,0,0)")

        for key, v in theme.iteritems():
            if key in ("name", ) or key[0] == '@':
                pass
            elif isinstance(v, str):
                cb(key, subst(v))
            elif isinstance(v, tuple):
                cb(key, "@".join((subst(v[0]), subst(v[1]), subst(v[2]))))

    def __on_preferences_changed(self, hook):
        v = GPS.Preference(self.pref_name).get()
        if v == "Custom":
            self.current = v

        else:
            values = {}
            for t in themes:
                if t["name"] == v:
                    values = t
                    break

            if self.current != v:
                GPS.Logger("COLORSCHEME").log(
                    "Applying new color scheme %s" % (v, ))
                self.current = v
                self.apply_theme(values)
            else:
                self.__modified = False

                # if any of the preferences was changed, set to "Custom"
                def test_changed(key, value):
                    if GPS.Preference(key).get() != value:
                        GPS.Logger("COLORSCHEME").log(
                            "pref %s is different: %s != %s" %
                            (key, value, GPS.Preference(key).get()))
                        self.__modified = True

                self.__for_each_pref(values, lambda k, v: test_changed(k, v))

                if self.__modified:
                    GPS.Preference(self.pref_name).set("Custom")


    def apply_theme(self, theme):
        colors = ""

        v = theme.get("@theme_bg_color")
        if v:
            colors += "@define-color theme_bg_color %s;" %  v

        v = theme.get("@theme_selected_bg_color")
        if v:
            colors += "@define-color theme_selected_bg_color %s;" % v

        v = theme.get("@theme_fg_color")
        if v:
            colors += "@define-color theme_fg_color %s;" % v

        v = theme.get("@editor_bg_selection")
        if v:
            colors += "*:selected, *:selected:focus {background-color: %s}" % v

        v = theme.get("@editor_fg_selection")
        if v:
            colors += "*:selected, *:selected:focus {color: %s}" % v

        with gps_utils.freeze_prefs():
           GPS.Preference(self.gtkpref_name).set(colors)

           if theme.get("@theme"):
               GPS.Preference(self.pref_gtk_theme).set(theme.get("@theme"))
           self.__for_each_pref(theme, lambda k, v: GPS.Preference(k).set(v))

        self.__set_gtk_properties()

Color_Theme_Switcher()
