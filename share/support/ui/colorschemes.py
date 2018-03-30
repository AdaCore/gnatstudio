"""
This plugin provides a new preference that allows users to
select a color theme.

These themes are inspired from:
    http://colorsublime.com/?page=5
"""

import os
import GPS
from gps_utils import hook
from theme_handling import (
    Theme, Rgba, transparent, Color,
    gtk_css_pref_name)
import textmate

try:
    from gi.repository import Gtk, Gdk
except ImportError:
    pass

logger = GPS.Logger("COLORSCHEMES")

STYLE_WARNING = GPS.Style("editor-warnings")
STYLE_WARNING.set_background(
    GPS.Preference("Warnings-Src-Highlight-Color").get())

STYLE_ERROR = GPS.Style("editor-errors")
STYLE_ERROR.set_background(
    GPS.Preference("Errors-Src-Highlight-Color").get())

_VIEW_TITLE = "Color Theme"

default = Theme("Default", True, {
    "theme_selected_bg": Rgba(74, 144, 217),
})

darkside = Theme("Darkside", False, {})

monokai = Theme("Monokai", False, {
    "debugger_current": Rgba(58, 71, 54, 153),
    "current_line": Rgba(73, 72, 62),
    "editor_fg": Rgba(248, 248, 242),
    "editor_bg": Rgba(39, 40, 34),
    "hyperlinks": ("DEFAULT", Rgba(114, 159, 207), transparent),
    "strings": ("DEFAULT", Rgba(230, 219, 116), transparent),
    "numbers": ("DEFAULT", Rgba(255, 51, 51), transparent),
    "annotated_comments": ("DEFAULT", Rgba(117, 113, 94), transparent),
    "comments": ("DEFAULT", Rgba(117, 113, 94), transparent),
    "keywords": ("DEFAULT", Rgba(249, 38, 114), transparent),
    "types": ("DEFAULT", Rgba(102, 217, 239), transparent),
    "blocks": ("DEFAULT", Rgba(230, 219, 116), transparent),
    "browsers_bg": Rgba(39, 40, 34)
})

iplastic = Theme("iPlastic", True, {
    "theme_selected_bg": Rgba(74, 144, 217),
    "debugger_current": Rgba(125, 236, 57, 153),
    "current_line": Rgba(226, 226, 226, 102),
    "editor_fg": Rgba(0, 0, 0),
    "editor_bg": Rgba(238, 238, 238),
    "hyperlinks": ("DEFAULT", Rgba(0, 0, 255), transparent),
    "strings": ("DEFAULT", Rgba(0, 153, 51), transparent),
    "numbers": ("DEFAULT", Rgba(255, 51, 51), transparent),
    "annotated_comments": ("DEFAULT", Rgba(0, 102, 255), transparent),
    "aspects": ("DEFAULT", Rgba(0, 102, 255), transparent),
    "comments": ("DEFAULT", Rgba(0, 102, 255), transparent),
    "keywords": ("DEFAULT", Rgba(0, 0, 255), transparent),
    "types": ("DEFAULT", Rgba(102, 217, 239), transparent),
    "blocks": ("DEFAULT", Rgba(255, 128, 0), transparent),
    "browsers_bg": Rgba(238, 238, 238)
})

themes = []


def get_themes():
    """ Load and return the list of themes.
        Each theme is a dictionary of values.
    """
    global themes

    if not themes:

        # Recompute the gutter's foreground color directly from
        # the editors colors for the basic themes.
        # It makes sure that that the line numbers can't be mixed with
        # the actual code.

        basic_themes = [default, darkside, monokai, iplastic]
        for theme in basic_themes:
            fg_color = theme.d['editor_fg']
            bg_color = theme.d['editor_bg']
            theme.d['gutter_fg'] = fg_color.mix(bg_color, 0.6)

        themes = basic_themes + textmate.textmate_themes()

    return themes


def validate_color(s):
    """return a valid color from s"""
    if s.startswith('#'):
        if len(s) > 7:
            return s[0:7]
    return s


def pref_set(gps_pref, val):
    try:
        GPS.Preference(gps_pref).set(val)
    except GPS.Exception:
        pass


class ColorThemeSwitcher(object):

    def __init__(self):
        self.__modified = False

        GPS.Preference(gtk_css_pref_name).create(
            "Custom theme's CSS", "string", "", "")

        self.provider = Gtk.CssProvider()
        screen = Gdk.Display.get_default().get_default_screen()
        Gtk.StyleContext.add_provider_for_screen(
            screen,
            self.provider,
            Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

        self.__set_gtk_properties()

    def __set_gtk_properties(self):
        c = GPS.Preference(gtk_css_pref_name).get()
        try:
            if c == "":
                self.provider.load_from_data("*{}")  # Clear contents
            else:
                self.provider.load_from_data(c)
        except Exception:
            GPS.Console().write(
                "resetting theme preference %s\n" % gtk_css_pref_name)
            GPS.Preference(gtk_css_pref_name).set('')

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
        theme.apply_preferences(self.provider)


the_theme_switcher = ColorThemeSwitcher()

themes_to_process = []


def get_luminosity(x):
    """ Utility function to sort themes based on luminosity.
    """
    if '@luminosity' in x:
        return x['@luminosity']
    else:
        return 0.0


class ColorSchemePicker(object):

    themes = []

    snapshots_dir = None

    vbox = None

    flow = None

    light_theme_radio = None

    dark_theme_radio = None

    # The indexes of the light and dark themes displayed in the color theme
    # preferences page assistant.
    LIGHT_THEME_INDEX = 0
    DARK_THEME_INDEX = 1

    # The indexes of the light and dark themes radio buttons in view's flow box
    LIGHT_RADIO_CHILD_INDEX = 0
    DARK_RADIO_CHILD_INDEX = 1

    def get_preferences_page(self):
        """
        Used to construct the default color theme preferences page (i.e: the
        one displayed in the Preferences editor dialog).
        """
        self.__initialize_view()

        self.flow.set_selection_mode(Gtk.SelectionMode.NONE)

        index = 0

        for t in self.themes:
            self.flow.add(self.__one_box(t))
            c = self.flow.get_child_at_index(index)
            c.connect("activate", self.__on_chosen, t)
            index += 1

        return self.vbox

    def get_preferences_assistant_page(self):
        """
        Used to construct the color theme preferences assistant page. This page
        is much simpler than the one displayed in the preferences editor: it
        displays only two possibilities, a light theme (default) and a dark
        one.
        """
        self.__initialize_view()

        self.flow.set_selection_mode(Gtk.SelectionMode.SINGLE)
        self.flow.set_activate_on_single_click(True)

        # Create the light theme radio box

        vbox, self.light_theme_radio = self.__create_radio_box(
            default, radio_group=None)
        self.flow.add(vbox)
        self.flow.select_child(
            self.flow.get_children()[self.LIGHT_RADIO_CHILD_INDEX])

        # Create the dark theme radio box

        vbox, self.dark_theme_radio = self.__create_radio_box(
            darkside,
            radio_group=self.light_theme_radio)
        self.dark_theme_radio.set_active(False)
        self.flow.add(vbox)

        self.flow.connect("child-activated", self.__on_child_activated)

        # Select the 'Default' theme by default

        self.__on_chosen(self.light_theme_radio, default)

        return self.vbox

    def __initialize_view(self):
        """
        Initialize the general attributes of the color theme preferences page
        view.
        """
        self.vbox = Gtk.VBox()
        self.vbox.set_name(_VIEW_TITLE)

        self.snapshots_dir = os.path.join(
            GPS.get_system_dir(), "share", "gps", "color_themes", "snapshots")

        self.flow = Gtk.FlowBox()
        self.flow.set_min_children_per_line(2)
        self.vbox.pack_start(self.flow, True, True, 0)

        self.themes = list(get_themes())

        self.themes.sort(key=lambda x: x.name)

    def __create_radio_box(self, theme, radio_group=None):
        """
        Create a box containing a radio button for the given ``theme``,
        appending it to the given ``radio_group``, and the theme's image.
        """
        vbox = Gtk.VBox(homogeneous=False)

        hbox = Gtk.HBox()
        vbox.pack_start(hbox, False, False, 10)

        radio_button = Gtk.RadioButton(group=radio_group, label=theme.name)

        radio_button.connect("toggled", self.__on_chosen, theme)
        hbox.pack_start(radio_button, False, False, 15)

        label = theme.generate_example_label()
        vbox.pack_start(label, False, False, 10)

        return vbox, radio_button

    def __on_child_activated(self, flow, child):
        """
        Called when a child has been activated in the preferences assistant
        page view. Toggles the associated theme radio button.
        """
        if child.get_index() == self.LIGHT_RADIO_CHILD_INDEX:
            self.light_theme_radio.set_active(True)
        else:
            self.dark_theme_radio.set_active(True)

    def __on_chosen(self, widget, theme):
        """
        Called when the theme's button or radio button has been clicked.
        """
        if widget == self.light_theme_radio:
            self.flow.select_child(
                self.flow.get_children()[self.LIGHT_RADIO_CHILD_INDEX])
        elif widget == self.dark_theme_radio:
            self.flow.select_child(
                self.flow.get_children()[self.DARK_RADIO_CHILD_INDEX])

        the_theme_switcher.apply_theme(theme)

    def __one_box(self, theme):
        """
        Return one widget representing one theme in the flowbox.
        """
        vbox = Gtk.VBox()

        label = theme.generate_example_label()
        vbox.pack_start(label, False, False, 0)
        hbox = Gtk.HBox()
        vbox.pack_start(hbox, False, False, 0)

        b = Gtk.Button(theme.name)
        b.connect('clicked', self.__on_chosen, theme)
        hbox.pack_start(b, True, False, 0)

        return vbox

    def no_theme_fallback(self):
        """
        If needed, Try to deduce a theme from the user's preferences and, if
        found, applies it.
        Otherwise, expose the editor's CSS colors directly from the associated
        preferences.
        Needed for compatibility.
        """

        if not GPS.Preference(gtk_css_pref_name).get():
            logger.log("Searching for a fallback theme...")

            # Get the color values for the editor's style preferences and
            # compare them with the editor colors of the known themes.
            # If there is a match, apply the given theme.
            # Otherwise, define directly the needed editor CSS colors from
            # the editor's style preference value.

            editor_style = GPS.Preference(
                "Src-Editor-Reference-Style").get().split("@")
            editor_bg_color = Color(from_pref=editor_style[2])
            editor_fg_color = Color(from_pref=editor_style[1])

            themes = get_themes()
            fallback_theme = None
            for t in themes:
                if (t.d['editor_bg'] == editor_bg_color
                        and t.d['editor_fg'] == editor_fg_color):
                    fallback_theme = t
                    break

            if fallback_theme:
                logger.log("%s used as fallback theme" % (fallback_theme.name))
                the_theme_switcher.apply_theme(fallback_theme)
            else:
                logger.log("No fallback theme found: applying default CSS")
                css = ("""
@define-color editor_bg_color %s;
@define-color editor_fg_color %s;""" % (editor_bg_color.to_rgba_string(),
                                        editor_fg_color.to_rgba_string()))
                the_theme_switcher.provider.load_from_data(css)
                GPS.Preference(gtk_css_pref_name).set(css)


picker = ColorSchemePicker()

# Register the color theme picker as a preferences page

GPS.PreferencesPage.create(name="General/Color Theme",
                           get_widget=picker.get_preferences_page,
                           priority=0)

# Register a simpler page for the color theme. This page will be displayed
# in the preferences assistant but not in the preferences editor dialog.

GPS.PreferencesPage.create(
    name="Color Theme Assistant",
    get_widget=picker.get_preferences_assistant_page,
    is_integrated=True)


# Check if we need to apply a fallback theme for compatibility reasons

@hook("gps_started")
def on_started():
    picker.no_theme_fallback()
