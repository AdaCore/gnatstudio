"""
This plugin provides a new preference that allows users to
select a color theme.

These themes are inspired from:
    http://colorsublime.com/?page=5
"""

import os

has_cairo = True
try:
    import cairo
except:
    has_cairo = False

import GPS
import pygps
import textmate
from textmate import light_common, dark_common

from modules import Module
from gps_utils import make_interactive

try:
    from gi.repository import Gtk, Gdk
except ImportError:
    pass

import gps_utils

STYLE_WARNING = GPS.Style("editor-warnings")
STYLE_WARNING.set_background(
    GPS.Preference("Warnings-Src-Highlight-Color").get())

STYLE_ERROR = GPS.Style("editor-errors")
STYLE_ERROR.set_background(
    GPS.Preference("Errors-Src-Highlight-Color").get())

_VIEW_TITLE = "Color Theme"

default = gps_utils.Chainmap(light_common, {
    "name": "Default",
    "GPS6-Gtk-Theme-Name": "Adwaita",
    "@theme_bg_color": None,           # the background color for windows
    "@theme_fg_color": None,           # default color for the text
    "@theme_selected_bg_color": None,  # selection in trees or menus
    "@editor_bg_selection": "rgb(74,144,217)",
    "General-Default-Font": "${font}",
    "Debugger-Editor-Current-Line":   "rgba(125,236,57,0.6)",
    "Src-Editor-Current-Line-Color":  "rgba(226,226,226,0.4)",
    "Src-Editor-Reference-Style": (
        "${editorfont}", "rgb(0,0,0)", "rgb(255,255,255)"),
    "Src-Editor-Hyper-Links-Variant": (
        "DEFAULT", "rgb(0,0,255)", "transparent"),
    "Src-Editor-Strings-Variant": ("DEFAULT", "rgb(206,123,0)", "transparent"),
    "Src-Editor-Numbers-Variant": ("DEFAULT", "rgb(255,51,51)", "transparent"),
    "Src-Editor-Annotated-Comments-Variant": (
        "DEFAULT", "rgb(96,97,95)", "transparent"),
    "Src-Editor-Comments-Variant": (
        "DEFAULT", "rgb(150,150,150)", "transparent"),
    "Src-Editor-Keywords-Variant": ("DEFAULT", "rgb(0,0,230)", "transparent"),
    "Src-Editor-Type-Variant": ("DEFAULT", "rgb(0,153,0)", "transparent"),
    "Src-Editor-Block-Variant": ("DEFAULT", "rgb(96,97,95)", "transparent"),
    "Browsers-Bg-Color": "rgb(255,255,255)"
})


monokai = gps_utils.Chainmap(dark_common, {
    "name": "Monokai",
    "GPS6-Gtk-Theme-Name": "Adwaita (Dark)",
    "@theme_bg_color": "rgb(39,40,34)",
    "@theme_fg_color": "rgb(248,248,242)",
    "@theme_selected_bg_color": "rgb(0,67,152)",
    "@editor_bg_selection": "rgb(0,67,152)",
    "General-Default-Font":                  ("${font}"),
    "Debugger-Editor-Current-Line":          "rgba(58,71,54,0.6)",
    "Src-Editor-Current-Line-Color":         "rgb(73,72,62)",
    "Src-Editor-Reference-Style":            ("${editorfont}",
                                              "rgb(248,248,242)",
                                              "rgb(39,40,34)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT",
                                              "rgb(0,0,255)",
                                              "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT",
                                              "rgb(230,219,116)",
                                              "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT",
                                              "rgb(255,51,51)",
                                              "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT",
                                              "rgb(117,113,94)",
                                              "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT",
                                              "rgb(117,113,94)",
                                              "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT",
                                              "rgb(249,38,114)",
                                              "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT",
                                             "rgb(102,217,239)",
                                             "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT",
                                             "rgb(230,219,116)",
                                             "transparent"),
    "Browsers-Bg-Color": "rgb(39,40,34)"
})

darkside = gps_utils.Chainmap(dark_common, {
    "name": "Darkside",
    "GPS6-Gtk-Theme-Name": "Adwaita (Dark)",
    "@theme_bg_color": "rgb(34,35,36)",
    "@editor_bg_selection": "rgb(0,67,152)",
    "General-Default-Font":                  ("${font}"),
    "Debugger-Editor-Current-Line":          "rgba(58,71,54,0.6)",
    "Src-Editor-Current-Line-Color":         "rgb(48,51,51)",
    "Src-Editor-Reference-Style":            ("${editorfont}",
                                              "rgb(186,186,186)",
                                              "rgb(34,35,36)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT",
                                              "rgb(0,0,255)",
                                              "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT",
                                              "rgb(242,212,44)",
                                              "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT",
                                              "rgb(255,51,51)",
                                              "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT",
                                              "rgb(114,159,207)",
                                              "transparent"),
    "Src-Editor-Aspects-Variant":            ("DEFAULT",
                                              "rgb(114,159,207)",
                                              "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT",
                                              "rgb(114,159,207)",
                                              "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT",
                                              "rgb(240,141,36)",
                                              "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT",
                                             "rgb(142,105,201)",
                                             "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT",
                                             "rgb(104,194,68)",
                                             "transparent"),
    "Browsers-Bg-Color": "rgb(34,35,36)"
})

iplastic = gps_utils.Chainmap(light_common, {
    "name": "iPlastic",
    "GPS6-Gtk-Theme-Name": "Adwaita",
    "@editor_bg_selection": "rgb(74,144,217)",
    "General-Default-Font":                  ("${font}"),
    "Debugger-Editor-Current-Line":          "rgba(125,236,57,0.6)",
    "Src-Editor-Current-Line-Color":         "rgba(226,226,226,0.4)",
    "Src-Editor-Reference-Style":            ("${editorfont}",
                                              "rgb(0,0,0)",
                                              "rgb(238,238,238)"),
    "Src-Editor-Hyper-Links-Variant":        ("DEFAULT",
                                              "rgb(0,0,255)",
                                              "transparent"),
    "Src-Editor-Strings-Variant":            ("DEFAULT",
                                              "rgb(0,153,51)",
                                              "transparent"),
    "Src-Editor-Numbers-Variant":            ("DEFAULT",
                                              "rgb(255,51,51)",
                                              "transparent"),
    "Src-Editor-Annotated-Comments-Variant": ("DEFAULT",
                                              "rgb(0,102,255)",
                                              "transparent"),
    "Src-Editor-Aspects-Variant":            ("DEFAULT",
                                              "rgb(0,102,255)",
                                              "transparent"),
    "Src-Editor-Comments-Variant":           ("DEFAULT",
                                              "rgb(0,102,255)",
                                              "transparent"),
    "Src-Editor-Keywords-Variant":           ("DEFAULT",
                                              "rgb(0,0,255)",
                                              "transparent"),
    "Src-Editor-Type-Variant":              ("DEFAULT",
                                             "rgb(102,217,239)",
                                             "transparent"),
    "Src-Editor-Block-Variant":             ("DEFAULT",
                                             "rgb(255,128,0)",
                                             "transparent"),
    "Browsers-Bg-Color": "rgb(238,238,238)"
})

themes = []


def get_themes():
    """ Load and return the list of themes.
        Each theme is a dictionary of values.
    """
    global themes

    if not themes:
        themes = [default, darkside, monokai, iplastic
                  ] + textmate.textmate_themes()

    return themes


def pref_set(gps_pref, val):
    try:
        GPS.Preference(gps_pref).set(val)
    except GPS.Exception:
        pass


class ColorThemeSwitcher(object):

    pref_gtk_theme = "GPS6-Gtk-Theme-Name"
    gtkpref_name = "/ColorTheme gtk+"

    def __init__(self):
        self.__modified = False

        GPS.Preference(self.gtkpref_name).create("", "string", "", "")

        self.provider = Gtk.CssProvider()
        screen = Gdk.Display.get_default().get_default_screen()
        Gtk.StyleContext.add_provider_for_screen(screen, self.provider, 901)

        self.__set_gtk_properties()

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

        default = GPS.Preference("General-Default-Font").get()
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

    def apply_theme(self, theme):
        """ Apply the theme to GPS.
            :param dict theme: dict to map preferences to their values
            (with special-case support for CSS-defined values, see above.)
        """
        colors = ""

        recognized_colors = ["theme_bg_color", "theme_selected_bg_color",
                             "theme_fg_color", "gutter_color",
                             "gutter_background",
                             "browser_decoration_background",
                             "browser_decoration_color"]

        for c in recognized_colors:
            v = theme.get("@" + c)
            if v:
                colors += "@define-color %s %s;" % (c, v)

        v = theme.get("@editor_bg_selection")
        if v:
            colors += "*:selected, *:selected:focus {background-color: %s}" % v

        v = theme.get("@editor_fg_selection")
        if v:
            colors += "*:selected, *:selected:focus {color: %s}" % v

        v = theme.get("@caret")
        if v:
            colors += "GtkTextView {-GtkWidget-cursor-color: %s}" % v

        with gps_utils.freeze_prefs():
            pref_set(self.gtkpref_name, colors)

            if theme.get("@theme"):
                pref_set(self.pref_gtk_theme, theme.get("@theme"))

            self.__for_each_pref(theme, lambda k, v: pref_set(k, v))

        self.__set_gtk_properties()

the_theme_switcher = ColorThemeSwitcher()

themes_to_process = []


def generate_snapshots(directory):
    """ Utility function to generate snapshots for all themes.
        This generates all snapshots in the specified directory, then
        exits GPS.
        This is meant to be launched by GPS developers, to regenerate
        the screenshots.
    """

    global themes_to_process

    if not has_cairo:
        GPS.MDI.dialog("pycairo must be enabled in order to take snapshots")
        return

    if not os.path.exists(directory):
        os.mkdir(directory)

    # Prepare an interesting editor buffer
    if os.path.exists("sample.adb"):
        os.remove("sample.adb")

    buf = GPS.EditorBuffer.get(GPS.File("sample.adb"))
    buf.insert("""
   procedure Foo
     (An_Integer : in out Integer := 0;
      A_String   : String  := "some text")
     with Pre  => An_Integer >= -1;
   --  Documentation for Foo

   ---------
   -- Foo --
   ---------

   procedure Foo
     (An_Integer : in out Integer := 0;
      A_String   : String  := "some text") is
   begin
      --  Do the actual loop

      for J in A_String'Range loop
         Put_Line ("bla" & (A + 10)'Img);
      end loop;
   end Foo;

""")
    buf.save()

    #  Get the widget to capture

    view = buf.current_view()
    view.goto(buf.at(19, 28))
    widget = view.pywidget()

    themes_to_process = list(get_themes())

    def process_one(timeout):
        global themes_to_process
        if len(themes_to_process) == 0:
            timeout.remove()
            GPS.exit(True)

        t = themes_to_process[0]
        themes_to_process.remove(t)

        name = t['name']
        file_base_name = t['name'] + ".png"
        GPS.Console("Messages").write("generating for %s\n" % name)
        the_theme_switcher.apply_theme(t)
        pygps.process_all_events()
        surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, 400, 300)
        context = cairo.Context(surface)
        widget.draw(context)
        surface.write_to_png(os.path.join(directory, file_base_name))

    GPS.Timeout(1000, process_one)


def get_luminosity(x):
    """ Utility function to sort themes based on luminosity.
    """
    if '@luminosity' in x:
        return x['@luminosity']
    else:
        return 0.0


class ColorSchemePicker(object):

    def get_widget(self):
        self.vbox = Gtk.VBox()
        self.vbox.set_name(_VIEW_TITLE)
        self.snapshots_dir = os.path.join(
            GPS.get_system_dir(), "share", "gps", "color_themes", "snapshots")

        flow = Gtk.FlowBox()
        flow.set_selection_mode(Gtk.SelectionMode.NONE)
        self.vbox.pack_start(flow, True, True, 0)

        index = 0

        the_themes = list(get_themes())

        the_themes.sort(key=get_luminosity)

        for t in the_themes:
            flow.add(self.__one_box(t))
            c = flow.get_child_at_index(index)
            c.connect("activate", self.__on_chosen, t)
            index += 1

        # a default size that allows showing two columns of previews
        self.vbox.set_size_request(930, 600)

        return self.vbox

    def __on_chosen(self, widget, theme):
        the_theme_switcher.apply_theme(theme)

    def __one_box(self, theme):
        """ Return one widget representing one theme in the flowbox.
        """
        vbox = Gtk.VBox()

        image = Gtk.Image.new_from_file(
            os.path.join(self.snapshots_dir, theme['name'] + '.png'))
        vbox.pack_start(image, False, False, 0)
        hbox = Gtk.HBox()
        vbox.pack_start(hbox, False, False, 0)

        b = Gtk.Button(theme['name'])
        b.connect('clicked', self.__on_chosen, theme)
        hbox.pack_start(b, True, False, 0)

        return vbox


# Register the color theme picker as a preferences page

GPS.PreferencesPage.create(name="Color Theme",
                           get_widget=ColorSchemePicker().get_widget)
