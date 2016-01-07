""" Provide the Tip of the Day dialog.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS

from gi.repository import Gtk, Gdk
import os.path
import re

tips = """

Tip: Insert spaces with TAB key
Img: tip_insert_spaces_with_tab.png

You'd like the TAB key to insert spaces? Go to Edit->Key shortcuts,
Editor page, select the <i>Insert Tab With Spaces</i> action, then click on
<i>Grab</i> and hit the <i>TAB</i> key.
The number of spaces inserted will depend on the indentation level set
in the preferences dialog (Edit->Preferences).

Tip: Multiple views of the same file
Img: tip_multiple_views.png

Need to view multiple parts of the same file? Use the menu File->New View,
or hold the shift key, click on the Editor's title bar and drag it to a
place where you'd like the new view to be created

Tip: ESC to close a dialog

Did you know that you can close dialogs in GPS using the <i>ESC</i> key?
This also applies to this <i>Tip of the Day</i> dialog!

Tip: Multi language builder
Img: tip_multi_language_builder.png

GPS will automatically try to detect whether your project should be built
using <i>gnatmake</i> (Ada language used only) or <i>gprbuild</i> (other
languages used).
If you want to force a specific builder, go to Edit->Preferences, then
in the General section, set the <i>Multi language builder</i> preference.

Tip: Move the position of notebook tabs
Img: tip_notebook_tabs_position.png

Did you know that you can move the position of the notebook tabs?
To do so, you can right click on any tab and select the <i>Tabs location</i>
menu. Alternatively, you can also go to Edit->Preferences and select the
Windows page.

Tip: Jump to first location
Img: tip_jump_to_first_location.png

By default GPS will jump to the first location loaded in the <i>Locations</i>
window (e.g. first build message, or first search result). If you do not
like this behavior, click the rightmost button in the local toolbar of the
<i>Locations</i> window and uncheck the <i>Jump to first location</i> menus.

Tip: Highlight all occurrences of a word
Img: tip_all_occurrences.png

You'd like to highlight all occurrences of the current word in the editor?
Enable the <i>occurrences.py</i> plug-in in Tools->Plug-ins and then set a key
short cut for the 'Mark Occurrences' action via the Edit->Key Shortcuts editor.

Tip: Disable indentation temporarily

To disable casing and indentation on the next key (e.g. Enter key), you
can use the control-q key and then press e.g. Enter.

Tip: Task Manager
Img: tip_task_manager.png

When a task is on going in background (e.g. build, search), GPS displays
a progress bar summarizing the current state of the running task(s).
To get more details, you can double-click on the progress bar, which will
open a <i>Task Manager</i> where you can see each separate task, suspend
them, interrupt them, etc.
You can also display the task manager at any time using the Tools->Views->Task
menu.

Tip: OS shell
Img: tip_os_shell.png

Need to have a quick access to an OS shell?
Go to Tools->Consoles->OS Shell

Tip: Build menu configuration
Img: tip_build_menu.png

You would like to configure the Build menu items (e.g. modify the commands
launched, or add/remove menu items)? Go to Build->Settings->Targets.

Tip: Makefile support
Img: tip_makefile_support.png

If you have a Makefile in the same directory where you project file is located,
GPS will automatically parse it and create a Build->Makefile menu with
all the targets defined in your Makefile.

Tip: Keyboard macro
Img: tip_keyboard_macro.png

You always perform the same sequence of keys? You can record it as a macro
and replay it automatically using the Tools->Macro menu.

Tip: Navigating with hyperlinks

If you hold the <i>Ctrl</i> key in a source editor, each source entity under
the mouse becomes a hyperlink like in a web browser. Clicking on an underlined
entity will go immediately to its declaration (or body if you use the middle
button, or are already on a declaration). Give it a try!

Tip: Sorting in Outline View

The Outline View (Tools->Views->Outline) by default displays entities in
alphabetical order. You can instead get entities sorted by order of appearance
in the file by right clicking in the Outline View and unselect
<i>Outline->Sort alphabetically</i>.

Tip: Build modes

To rebuild your project with different switches, you can use the Build mode
combo selector in the main toolbar: by default it provides predefined
modes (debug, optimize, gprof, ...). You can also create your own build
modes via simple xml files.

Tip: Temporarily add a build switch

Need to temporarily and quickly add a switch for your build? Use the
Build->Project menu and you'll get access to the command line launched by GPS
to perform the build action, with the ability to modify it.

Tip: Smart completion

If you need GPS to automatically complete the name of an entity, or fill
a subprogram profile, simply pressing ctrl-space in a source editor will
display a completion list with possible completion for the current word.
Automatic completion is also displayed automatically when you type special
characters such as '<b>.</b>' or '<b>(</b>'.

Tip: Dynamic smart completion

You'd like GPS to make more suggestions automatically to complete
identifiers while you're typing without having to press control-space? Go to
Edit->Preferences, Editor and then set the <i>Smart completion</i> preference
to <i>Dynamic</i>. You might also want to tune the default timeout.

Tip: Highlight dispatching calls

Did you know that GPS can highlight all dispatching calls in your source?
To do so, go to Tools->Plug-ins and enable the <i>dispatching.py</i> plug-in.

Tip: Source navigation through dispatching calls

Did you know that when opening a contextual menu on a dispatching call,
GPS is able to list all the possible targets for this call? By default, GPS
will take into account the cross reference information it has already
computed in memory, and you can get more accurate and complete results
by going to Edit->Preferences, select the Editor page and set the
<i>Submenu for dispatching calls</i> preference to <i>Accurate</i>.

Tip: On the fly auto casing

To enable on the fly automatic casing of your identifiers, go to
Edit->Preferences, then select Editor->Ada and set <i>Casing policy</i> to
<i>On_The_Fly</i> or if you find that too intrusive, <i>End_Of_Line</i>.

Tip: Disable temporarily auto casing

To temporarily disable auto casing, you can use the <i>alt-q</i> key.
This can be particularly useful when you've set the casing policy to
<i>On The Fly</i> (see previous tip). You can re-enable it using the same key.

Tip: How to gain space on your workspace

In order to gain room in your workspace, you can go to Edit->Preferences,
then Windows and disable the <i>Show title bars</i> option: this will remove
the title bars on each window.

Tip: Automatic code fixing

In many cases, the warning and error messages generated by GNAT are so
precise, that it is possible for GPS to automatically fix the code for you.

When this is possible, GPS will add a small <i>wrench</i> icon on the left
of the message in the Locations view which you can click on.

If multiple fixes are possible, GPS will display a menu with the possible
choices.

Tip: Splash screen

Tired of getting the splash screen at start up?
Go to Edit->Preferences, select the General page and disable the
<i>Display splash screen</i> preference.

Tip: Welcome dialog

When you start GPS in a directory which contains a single project file,
GPS will automatically use this project. Otherwise, GPS will open the
<i>Welcome Dialog</i> where you can select which project to load, or work
with the default project.

If you prefer to always start with the default project instead of getting
the <i>Welcome Dialog</i>, go to Edit->Preferences, select the General
page and disable the <i>Display welcome window</i> preference.

Tip: Multiple key shortcut

Did you know that you can associate a multiple key shortcut to any action in
GPS? To do so, go to Edit->Key shortcuts, select the action or menu you'd
like to associate or change a key binding, click <i>Grab</i> and then
type the two keys (e.g. <i>control-x a</i>) for your key binding.

Tip: Add subprogram box

Ever wanted to ask GPS to automatically add a box containing the current
subprogram name just before this subprogram?
Go to Edit->Key shortcuts, select Editor->Subprogram Box then glick <i>Grab</i>
and type a key short cut (e.g. <i>control-b control-b</i>). You can then use
this key shortcut in the middle of a subprogram.

Tip: How to <i>float</i> a window

To make a window floating (i.e. outside the main GPS window), simply click
on the title bar of the window and, while holding the mouse button, drag
the mouse outside the main window, and release the mouse button: this will
<i>float</i> the selected window.

Tip: How to <i>unfloat</i> a window

To unfloat a window, you can simply click on the close button and it will
automatically go back to its original location in the GPS main window.
Alternatively, you can also select the window, and then use the Window->Float
menu in the main window.

Tip: Direct access to predefined packages

If you go to Help->GNAT Runtime, you'll get access to all the predefined
Ada and GNAT packages and access their spec/documentation directly.

Tip: Display <i>Standard</i> package

You'd like to display the contents of the Ada package <i>Standard</i>
corresponding to your compiler? Go to Help->GNAT Runtime->Standard.

Tip: Open from project

Did you know that you can quickly open any file defined in your project or part
of the GNAT run-time by using the File->Open From Project... menu, or using the
<i>shift-F3</i> key: this will open a dialog where you can type the beginning
of any file and get automatic completion using the <i>TAB</i> key.

Tip: Support for multiple toolchains

Using the Build->Settings->Toolchains menu, you can enable support for multiple
toolchains, which means that GPS will use one toolchain for building, and
another toolchain (typically more recent) for other tools (source navigation,
coding standard checker, pretty printer, etc).

Tip: Support for old compilers

Did you know that GPS is independent of the underlying compiler toolchain
and can be upgraded without installing e.g. a new GNAT version?
GPS supports GNAT versions as far back as 3.16 up to today's version.

Tip: Remote Programming

Tired of launching GPS on a slow remote machine and display it on your local,
fast desktop computer via an X connection? The <i>Remote Programming</i>
capability of GPS is made for you. This mode allows you to run GPS locally
and drive your remote toolchain automatically via <i>ssh</i>, <i>rsh</i>
or <i>telnet</i>. See Tools->Views->Remote menu to create your remote
configuration.

Tip: Refactoring: name parameters

You'd like to add parameter names to a subprogram call? In your source editor,
move the mouse on the call you'd like to modify, right click, and select
Refactoring->Name parameters. This will e.g. replace
   <b>Procedure_Call (X, True);</b>
into
   <b>Procedure_Call (Object => X, Confirm => True);</b>

Tip: Refactoring: rename entity

Did you know that GPS can rename an entity in your whole project? This
capability works at the semantic level, so is not a simple text search and
replace. Instead, it works by using the semantic information generated by GNAT
in the <i>.ali</i> files.
To perform a global renaming, select any reference to the entity you'd like
to rename in a source editor, then right click and select
Refactoring->Rename <b>entity</b>.

Tip: Break on exception in debugger

You need to break on any exception raised in the debugger?
You can have GPS set this special breakpoint each time you start the debugger
automatically by going to Edit->Preferences, select Debugger page and enable
the <i>Break on exceptions</i> preference.
Alternatively you can also type the <i>break exception</i> command directly
in the debugger console.

Tip: Copy/Paste contextual menus

You can add Cut/Copy/Paste menus to the Editor's contextual menu by
going to Tools->Plug-ins and enable the <i>copy_paste.py</i> plug-in.

Tip: Automatic highlighting of occurrences

Leave your cursor on an entity: GPS will automatically highlight all references
to this entity in the current editor. Also, the color of the highlight gives a
hint on the category of the entity.

Tip: Compute dependency path between two units

If you want to understand why two units or files depend on each other (to
understand an elaboration circularity for example, or explain why changing
a given file triggers recompilation of another file), you can go to
Tools->Plug-ins and enable the <i>filedeps.py</i> plug-in.

Tip: Your tip here

Have your own idea for a nice tip and would like to share it with other GPS
users? Please send us your suggestion at report@adacore.com.

"""


def parse_tips():
    """ Parse the tips string and return a list of the form
        [ (tip_title_1, tip_text_1, tip_img_1),
          (tip_title_2, tip_text_2, tip_img_2) ... ]
    """

    result = []

    current_tip_title = ""
    current_tip_text = ""
    current_tip_img = ""

    title_re = re.compile("^Tip: (.*)")
    img_re = re.compile("^Img: (.*)")

    for l in tips.split('\n'):
        title = title_re.match(l)
        img = img_re.match(l)

        if title:
            if len(current_tip_text) > 0:
                result += [(current_tip_title,
                            current_tip_text,
                            current_tip_img)]
                current_tip_img = ""
                current_tip_text = ""
            current_tip_title = title.group(1) or ""

        elif img:
            # commented for now, see I903-007
            # current_tip_img = os.path.join (GPS.get_system_dir(),
            #                                 "share", "gps", "plug-ins",
            #                                 "images",
            #                                 img.group(1)) or ""
            pass

        else:
            if len(l) > 1:
                current_tip_text += l + '\n'

    if len(current_tip_text) > 0:
        result += [(current_tip_title, current_tip_text, current_tip_img)]

    return result


def display_tip(title, doc, img):
    """ Display the tip. Return a widget containing the tip. """

    vbox = Gtk.VBox()

    # display the title
    hbox = Gtk.HBox()

    title_label = Gtk.Label()
    title_label.set_use_markup(True)
    title_label.set_markup("""<big>%s</big>""" % title)

    hbox.pack_start(title_label, False, False, 10)

    vbox.pack_start(hbox, False, False, 10)

    # display the image if any

    if img != "":
        hbox = Gtk.HBox()

        image = Gtk.Image()
        image.set_from_file(img)
        image.show()

        hbox.pack_start(image, False, False, 10)

        vbox.pack_start(hbox, False, False, 10)

    # display the documentation
    hbox = Gtk.HBox()
    doc_label = Gtk.Label()
    doc_label.set_use_markup(True)
    doc_label.set_markup("""%s""" % doc)
    doc_label.set_selectable(True)

    hbox.pack_start(doc_label, False, False, 10)

    vbox.pack_start(hbox, True, True, 10)

    return vbox


class Tip:

    def on_key_press(self, widget, event):
        """ Callback on a key press event"""

        if event.keyval == Gdk.KEY_Escape:
            self.on_close_button(widget)
            return True

        return False

    def on_close_button(self, widget):
        """ Callback on a click on the close button """

        # save the current tip number

        GPS.Preference("tip-of-the-day-number").set(self.tip_number)

        # take into account the checkbox

        GPS.Preference(
            "General/Display-Tip-Of-The-Day").set(self.check.get_active())

        self.window.destroy()

    def on_next_button(self, widget):
        """ Callback on a click on the next button """
        self.tip_number += 1

        if self.tip_number >= len(self.results):
            self.tip_number = 0

        self.display_tip(self.tip_number)

    def on_prev_button(self, widget):
        """ Callback on a click on the previous button """
        self.tip_number -= 1

        if self.tip_number < 0:
            self.tip_number = len(self.results) - 1

        self.display_tip(self.tip_number)

    def display_tip(self, number):
        """ Display tip of the day of the given number """
        if self.tip_container.get_child():
            self.tip_container.remove(self.tip_container.get_child())

        self.tip_container.add(display_tip
                               (self.results[self.tip_number][0],
                                self.results[self.tip_number][1],
                                self.results[self.tip_number][2]))

        self.window.show_all()

    def __init__(self, results, parent=None, initial_tip=0):
        """ Display the Tip of the Day window """

        self.results = results

        border = Gdk.color_parse("#6897CB")  # GPS blue
        background = Gdk.color_parse("#F0F0F0")  # light grey

        window = Gtk.Window()
        self.window = window

        window.set_default_size(550, 350)
        window.set_decorated(True)

        if isinstance(parent, Gtk.Window):
            window.set_transient_for(parent)

        window.set_position(Gtk.WindowPosition.CENTER_ON_PARENT)
        window.modify_bg(Gtk.StateType.NORMAL, border)

        # EventBox for the blue frame

        ebox = Gtk.EventBox()
        ebox.modify_bg(Gtk.StateType.NORMAL, background)
        ebox.modify_fg(Gtk.StateType.NORMAL, Gdk.color_parse("black"))
        ebox.set_border_width(5)
        window.add(ebox)

        vbox = Gtk.VBox()
        ebox.add(vbox)

        # window title

        hbox = Gtk.HBox()
        title_label = Gtk.Label()
        title_label.set_use_markup(True)
        title_label.set_markup(
            """<big>Tip of the Day</big>""")

        img = Gtk.Image()
        img.set_from_stock(Gtk.STOCK_INFO, Gtk.IconSize.LARGE_TOOLBAR)
        hbox.pack_start(img, False, False, 10)
        hbox.pack_start(title_label, False, False, 3)
        vbox.pack_start(hbox, False, False, 10)

        # display the tip

        self.tip_number = initial_tip

        self.tip_container = Gtk.Frame()
        self.tip_container.set_shadow_type(Gtk.ShadowType.NONE)

        vbox.pack_start(self.tip_container, True, True, 0)

        # display the previous/next buttons

        hbox = Gtk.HBox()

        label = Gtk.Label()
        label.set_use_markup(True)
        label.set_markup('<span foreground="#0000FF">Next &gt;</span>')

        next_button = Gtk.Button()
        next_button.add(label)

        label = Gtk.Label()
        label.set_use_markup(True)
        label.set_markup('<span foreground="#0000FF">&lt; Previous</span>')

        prev_button = Gtk.Button()
        prev_button.add(label)

        for button in [next_button, prev_button]:
            button.modify_bg(Gtk.StateType.NORMAL, background)
            button.modify_bg(Gtk.StateType.PRELIGHT, background)
            button.modify_bg(Gtk.StateType.ACTIVE, background)
            button.set_relief(Gtk.ReliefStyle.NONE)

        hbox.pack_end(next_button, False, False, 10)
        hbox.pack_end(prev_button, False, False, 3)

        vbox.pack_start(hbox, False, False, 10)

        # display the footer

        hbox = Gtk.HBox()

        close_button = Gtk.Button(Gtk.STOCK_CLOSE)
        close_button.set_use_stock(True)
        close_button.grab_focus()

        hbox.pack_end(close_button, False, False, 10)

        self.check = Gtk.CheckButton()
        self.check.set_active(True)
        self.check.set_label("Display Tip of the Day on startup")
        self.check.modify_bg(Gtk.StateType.PRELIGHT, background)

        hbox.pack_start(self.check, False, False, 10)

        vbox.pack_start(hbox, False, False, 10)

        close_button.set_can_default(True)
        window.set_default(close_button)
        window.set_focus(close_button)
        window.show_all()

        # Display tip number 0

        self.display_tip(self.tip_number)

        # callbacks

        window.connect("key_press_event", self.on_key_press)
        close_button.connect("clicked", self.on_close_button)
        next_button.connect("clicked", self.on_next_button)
        prev_button.connect("clicked", self.on_prev_button)

# Register preferences

GPS.Preference("tip-of-the-day-number").create(
    "Tip of the day #",
    "integer",
    "The last tip of the day displayed",
    0)


def on_gps_started(hook):
    if not GPS.Preference("General/Display-Tip-Of-The-Day").get():
        return
    # If we reach this point, display the tip of the day

    # Parse the tips file
    results = parse_tips()

    # Get the main window
    messages = GPS.MDI.get("Messages").pywidget()
    top = messages.get_toplevel()

    # If one toplevel is a dialog for the main window, do not display the
    # tip-of-the-day

    for t in Gtk.Window.list_toplevels():
        if t.get_transient_for() == top:
            return

    t = Tip(results, top, GPS.Preference("tip-of-the-day-number").get())
    t.on_next_button(None)

GPS.Hook("gps_started").add(on_gps_started)
