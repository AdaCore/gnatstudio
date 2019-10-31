"""
This plugin provides a new preference that allows users to
select a color theme.

These themes are inspired from:
    http://colorsublime.com/?page=5
"""

import GPS
from gs_utils import hook
from theme_handling import (
    Theme, Rgba, transparent, Color,
    gtk_css_pref_name, prefs_to_css_colors)
import textmate


try:
    from gi.repository import Gtk, Gdk
except ImportError:
    pass

logger = GPS.Logger("COLORSCHEMES")

STYLE_WARNING = GPS.Style.create_from_preferences(
    "editor-warnings", "", "Medium-Importance-Messages-Highlight")
STYLE_ERROR = GPS.Style.create_from_preferences(
    "editor-errors", "", "High-Importance-Messages-Highlight")

_VIEW_TITLE = "Color Theme"

default = Theme("Default", True, {
    "theme_selected_bg": Rgba(74, 144, 217),
    "theme_selected_fg": Rgba(255, 255, 255, 255),
})

darkside = Theme("Darkside", False, {})

color_theme_pref = GPS.Preference("/Color-Theme").create(
    "Color theme", "string", "Darkside")

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
    "browsers_bg": Rgba(39, 40, 34),
    "ephemeral_simple": ("DEFAULT", transparent, Rgba(106, 108, 92, 128)),
    "ephemeral_smart": ("DEFAULT", transparent, Rgba(128, 236, 255, 90))
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

    def apply_theme(self, theme):
        """ Apply the theme to GPS.
            :param dict theme: dict to map preferences to their values
            (with special-case support for CSS-defined values, see above.)
        """
        theme.apply_preferences(self.provider)
        color_theme_pref.set(theme.name)


the_theme_switcher = ColorThemeSwitcher()


def get_luminosity(x):
    """ Utility function to sort themes based on luminosity.
    """
    if '@luminosity' in x:
        return x['@luminosity']
    else:
        return 0.0


class ColorSchemePicker(object):

    vbox = None

    flow = None

    def get_preferences_page(self, themes, active_theme=None):
        """
        Used to construct the Color Themes preferences pages (i.e: the
        one displayed in the Preferences editor dialog and the one displayed
        in the Preferences Assistant).

        :param list themes: The list of themes displayed in the page
        :param string active_theme: The theme to select. If None, the
          preference is used instead.
        """
        self.__initialize_view()

        self.flow.set_selection_mode(Gtk.SelectionMode.SINGLE)
        self.flow.set_activate_on_single_click(True)

        index = 0
        radio = None
        active_theme = (active_theme if active_theme is not None else
                        color_theme_pref.get())

        for theme in themes:
            is_theme_active = theme.name == active_theme

            child, radio = self.__create_radio_box(
                theme,
                radio_group=radio,
                active=is_theme_active)

            # Used to synchronize the radio button and the enclosing flowbox
            # child: when one gets selected/toggled, the other widget needs
            # to be selected/toggled.

            child.radio = radio
            radio.child = child

            self.flow.add(child)

            if is_theme_active:
                self.flow.select_child(self.flow.get_children()[index])
                radio.set_active(True)

            index += 1

        self.flow.connect("child-activated", self.__on_child_activated)

        return self.vbox

    def __initialize_view(self):
        """
        Initialize the general attributes of the color theme preferences page
        view.
        """
        PADDING = 5

        self.vbox = Gtk.VBox()
        self.vbox.set_name(_VIEW_TITLE)

        self.doc_label_hbox = Gtk.HBox()
        self.vbox.pack_start(self.doc_label_hbox, False, False, PADDING)

        self.doc_label = Gtk.Label()
        self.doc_label.set_alignment(0.0, 0.5)
        self.doc_label.set_markup(
            "You can add your own themes in the TextMate "
            "format (.tmTheme) by adding them in the "
            "<b>GNATSTUDIO_HOME/.gnatstudio/themes/</b> directory.")
        self.doc_label.get_style_context().add_class("dialog-views-doc-labels")
        self.doc_label_hbox.pack_start(self.doc_label, False, False, PADDING)

        self.flow = Gtk.FlowBox()
        self.flow.set_min_children_per_line(2)
        self.vbox.pack_start(self.flow, True, True, PADDING)

    def __create_radio_box(self, theme, radio_group=None, active=False):
        """
        Create a box containing a radio button for the given ``theme``,
        appending it to the given ``radio_group``, and the theme's image.
        """
        vbox = Gtk.VBox(homogeneous=False)

        hbox = Gtk.HBox()
        vbox.pack_start(hbox, False, False, 10)

        radio_button = Gtk.RadioButton(group=radio_group, label=theme.name)
        radio_button.set_name("radio-" + theme.name)
        hbox.pack_start(radio_button, False, False, 15)

        label = theme.generate_example_label()
        vbox.pack_start(label, False, False, 10)

        child = Gtk.FlowBoxChild()
        child.add(vbox)

        radio_button.connect("toggled", self.__on_chosen, theme)

        return child, radio_button

    def __on_child_activated(self, flow, child):
        """
        Called when a child has been activated in the preferences assistant
        page view. Toggles the associated theme radio button.
        """
        child.radio.set_active(True)

    def __on_chosen(self, radio, theme):
        """
        Called when the theme's radio button has been clicked.
        """
        self.flow.select_child(radio.child)
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

        b = Gtk.ToggleButton(theme.name)

        # If the color theme preference is set to the given theme, toggle its
        # associated button.
        if theme.name == color_theme_pref.get():
            b.set_active(True)

        b.connect('toggled', self.__on_chosen, theme)
        hbox.pack_start(b, True, False, 0)

        b.set_name("theme-button-" + theme.name)

        return vbox

    def no_theme_fallback(self):
        """
        If needed, Try to deduce a theme from the user's preferences and, if
        found, applies it.
        Otherwise, expose the editor's CSS colors directly from the associated
        preferences.
        Needed for compatibility.
        """

        if not color_theme_pref.get():
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
                the_theme_switcher.apply_theme(darkside)

    def on_pref_changed(self, hook):
        """
        Update the CSS colors using the new values set for their corresponding
        preferences.
        """

        # Create a new provider instead of using the previous one since
        # the Gtk.CssProvider.load_from_data method clears the
        # previouly loaded CSS: we don't want to loose the other colors
        # that may have been set.

        the_theme_switcher.provider = Gtk.CssProvider()
        screen = Gdk.Display.get_default().get_default_screen()
        Gtk.StyleContext.add_provider_for_screen(
            screen,
            the_theme_switcher.provider,
            Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

        # Redefine the CSS color variables linked with color/variant
        # preferences.

        css_template = "@define-color {} {};"
        css = ""

        for pref_name in prefs_to_css_colors:
            pref = GPS.Preference(pref_name)
            color_names = prefs_to_css_colors[pref_name]

            if len(color_names) == 2:
                pref_val = pref.get().split('@')
                fg = pref_val[1]
                bg = pref_val[2]

                css += '\n' + css_template.format(color_names[0], fg)
                css += '\n' + css_template.format(color_names[1], bg)
            else:
                pref_val = pref.get()
                css += '\n' + css_template.format(color_names[0], pref_val)

        if css:
            the_theme_switcher.provider.load_from_data(css)


picker = ColorSchemePicker()

# Register the color theme picker as a preferences page, with all the available
# themes

GPS.PreferencesPage.create(
    name="General/Color Theme",
    get_widget=(lambda: picker.get_preferences_page(get_themes())),
    priority=0)

# Register a simpler page for the Preferences assistant, displaying only
# a light and a dark theme.

GPS.PreferencesPage.create(
    name="Color Theme Assistant",
    get_widget=(
        lambda: picker.get_preferences_page([default, darkside], "Darkside")),
    is_integrated=True)


# Check if we need to apply a fallback theme for compatibility reasons.
# Add a hook function on the 'preferences_changed' hook to reapply the
# CSS colors if an associated preference has changed (e.g: the
# editor's style preference).

@hook("gps_started")
def on_started():
    picker.no_theme_fallback()
    GPS.Hook("preferences_changed").add_debounce(picker.on_pref_changed)
