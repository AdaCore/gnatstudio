"""
This plugin provides a new preference that allows users to
select a color scheme.

These themes are inspired from:
    http://colorsublime.com/?page=5
"""



import GPS
from gi.repository import Gtk, Gdk


themes = [
   {"name": "Default",
    "@dark": False,
    #"@theme_bg_color": None,           # the background color for windows
    #"@theme_fg_color": None,           # default color for the text
    #"@theme_selected_bg_color": None,  # selection in trees or menus
    #"@editor_bg_selection": None,      # background for selection in editors
    #"@editor_fg_selection": None,      # foreground for selection in editors
    "General-Default-Style":                 ("${font}", "black", "white"),
    "Src-Editor-Current-Line-Color":         "#D1DCFC",
    "Src-Editor-Reference-Style":            ("${editorfont}", "black", "white"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "blue",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "brown",   "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "#21A9DE", "transparent"),
    "Src-Editor-Comments-Variant":           ("ITALIC",  "blue",    "transparent"),
    "Src-Editor-Keywords-Variant":           ("BOLD",    "black",   "transparent"),
    "Src-Editor-Type-Variant":               ("DEFAULT", "#009CD9", "transparent"),
    "Src-Editor-Block-Variant":              ("DEFAULT", "#A600D9", "transparent")
   },
   {"name": "Darkside",
    "@dark": True,
    "@theme_bg_color": "#222324",
    "General-Default-Style":                 ("${font}", "#BABABA", "transparent"),
    "Src-Editor-Current-Line-Color":         "#303333",
    "Src-Editor-Reference-Style":            ("${editorfont}", "#BABABA", "#222324"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "blue",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "#F2D42C", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "#729FCF", "transparent"),
    "Src-Editor-Comments-Variant":           ("ITALIC",  "#729FCF", "transparent"),
    "Src-Editor-Keywords-Variant":           ("BOLD",    "#F08D24", "transparent"),
    "Src-Editor-Type-Variant":               ("DEFAULT", "#8E69C9", "transparent"),
    "Src-Editor-Block-Variant":              ("DEFAULT", "#68C244", "transparent")
   },
   {"name": "Monokai",
    "@dark": True,
    "@theme_bg_color": "#272822",
    "@theme_fg_color": "#F8F8F2",
    "@theme_selected_bg_color": "#49483E",
    "@editor_bg_selection": "#49483E",
    "General-Default-Style":                 ("${font}", "#F8F8F2", "#272822"),
    "Src-Editor-Current-Line-Color":         "#49483E",
    "Src-Editor-Reference-Style":            ("${editorfont}", "#F8F8F2", "#272822"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "blue",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "#E6DB74", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT", "#75715E", "transparent"),
    "Src-Editor-Comments-Variant":           ("ITALIC",  "#75715E", "transparent"),
    "Src-Editor-Keywords-Variant":           ("BOLD",    "#F92672", "transparent"),
    "Src-Editor-Type-Variant":               ("DEFAULT", "#66D9EF", "transparent"),
    "Src-Editor-Block-Variant":              ("DEFAULT", "#A6E22E", "transparent")
   },
   {"name": "iPlastic",
    "@dark": False,
    "General-Default-Style":                 ("${font}", "#000000", "#EEEEEE"),
    "Src-Editor-Current-Line-Color":         "rgba(0,0,0,0.2)",
    "Src-Editor-Reference-Style":            ("${editorfont}", "#000000", "#EEEEEE"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT", "blue",    "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT", "#009933", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("ITALIC",  "#0066FF", "transparent"),
    "Src-Editor-Comments-Variant":           ("ITALIC",  "#0066FF", "transparent"),
    "Src-Editor-Keywords-Variant":           ("BOLD",    "#0000FF", "transparent"),
    "Src-Editor-Type-Variant":               ("BOLD",    "#66D9EF", "transparent"),
    "Src-Editor-Block-Variant":              ("DEFAULT", "#FF8000", "transparent")
   },
]


class Color_Theme_Switcher(object):

    pref_name = "General/ColorTheme"
    
    def __init__(self):
        args = ["Custom"] + [t["name"] for t in themes]
        p = GPS.Preference(self.pref_name)
        p.create(
            "Color Theme",  # label
            "enum",         # type
            "Selecting a color scheme provides default values for a number of"
               + " other preferences, which can still be changed independently",
            0,   # default
            *args)
        self.current = p.get()

        self.provider = Gtk.CssProvider()
        screen = Gdk.Display.get_default().get_default_screen()
        Gtk.StyleContext.add_provider_for_screen(screen, self.provider, 901)

        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)

    def __del__(self):
        GPS.Hook("preferences_changed").remove(self.__on_preferences_changed)

    def __on_preferences_changed(self, hook):
        v = GPS.Preference(self.pref_name).get()
        if v == "Custom":
            self.current = v
        elif self.current != v:
            self.current = v
            for t in themes:
                if t["name"] == self.current:
                    self.apply_theme(t)
                    break

            GPS.Preference(self.pref_name).set("Custom")

    def apply_theme(self, theme):
        s = Gtk.Settings.get_default()

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

        # ??? Disable overriding aspects from the theme, since the user would
        # not be able to set his own easily, and in addition they would not be
        # reset correctly on startup
        #
        # self.provider.load_from_data(colors)

        s.set_property(
            'gtk-application-prefer-dark-theme', theme.get("@dark", False))
   
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
                GPS.Preference(key).set(subst(v))
            elif isinstance(v, tuple):
                GPS.Preference(key).set(
                    "@".join((subst(v[0]), subst(v[1]), subst(v[2]))))
    

Color_Theme_Switcher()
