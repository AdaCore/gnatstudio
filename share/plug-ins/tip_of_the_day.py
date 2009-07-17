""" Provide the Tip of the Day dialog.
"""


############################################################################
## No user customization below this line
############################################################################

import GPS

import gtk
import gtk.gdk
import os.path
import re

tips = """

Tip 1

You'd like the TAB key to insert spaces? Go to Edit->Key shortcuts,
then select the Editor->Insert Tab With Spaces action, then click on
'Grab' and hit the 'TAB' key.

Tip 2

Need to view multiple parts of the same file? Use the menu File->New View,
or hold the shift key, click on the Editor's title bar and drag it to a
place where you'd like the new view to be created

Tip 3

In order to gain room in your workspace, you can go to Edit->Preferences,
then Windows and disable the 'Show title bars' option: this will remove
the title bars on each window.

Tip 4

Did you know that you can move the position of the notebook tabs?
To do so, you can right click on any tab and select the Tabs location menu.
Alternatively, you can also go to Edit->Preferences and then Windows.

Tip 5

Edit->Key shortcuts Editor->Subprogram Box then glick 'Grab' and
type a key short cut (e.g. "control-b control-b")

Tip 6

To temporarily disable auto casing, you can use the alt-q key

Tip 7

To disable casing and indentation on the next key (e.g. Enter key), you
can use the control-q key and then press e.g. Enter.

Tip 8

To unfloat a window, you can simply click on the close button and it will
automatically go back to its original location in the GPS main window.
Alternatively, you can also select the window, and then use the Window->Float
menu in the main window.

Tip 9

Need to have a quick access to an OS shell? Go to Tools->Consoles->OS Shell

Tip 10

You would like to configure the Build menu items (either modify the commands
launched, or add/remove menu items)? Go to Build->Settings->Targets

Tip 11

If you have a Makefile in the same directory where you project file is located,
GPS will automatically parse it and create a Build->Makefile menu with
all the targets defined in your Makefile

Tip 12

You always perform the same sequence of keys? You can record it as a macro
and replay it automatically using the Tools->Macro menu

Tip 13

Need to find an entity in your project and its associated
profile/documentation? Use Tools->Views->Entities

Tip 14

Did you know that you can drag-n-drop the Entities View
(Tools->Views->Entities) to e.g. the left part of the main window, and it will
automatically switch to a vertical display to fit the space available?

Tip 15

The Outline View (Tools->Views->Outline) by default displays entities in
alphabetical order. You can instead get entities sorted by order of appearance
in the file by right clicking in the Outline View and unselect
Outline->Sort alphabetically

Tip 16

To rebuild your project with different switches, you can use the Build mode
combo selector in the main toolbar: by default it provides predefined
modes (debug, optimize, gprof, ...). You can also create your own build
modes via simple xml files.

Tip 17

Need to temporarily and quickly add a switch for your build? Use the
Build->Project menu and you'll get access to the command line launched by GPS
to perform the build action, with the ability to modify it.

Tip 18

If you need GPS to automatically complete the name of an entity, or fill
a subprogram profile, simply pressing ctrl-space in a source editor will
display a completion list with possible completion for the current word.
Automatic completion is also displayed automatically when you type special
characters such as '.' or '('

Tip 19

You'd like GPS to make more suggestions automatically to complete identifiers
while you're typing without having to press control-space? Go to
Edit->Preferences, Editor and then set the 'Smart competion' preference to
'Dynamic'. You might also want to tune the default timeout.

Tip 20

Did you know that GPS can highlight all dispatching calls in your source?
To do so, go to Tools->Plug-ins and enable the 'dispatching.py' plug-in.

Tip 21

Did you know that when opening a contextual menu on a dispatching call,
GPS is able to list all the possible targets for this call? By default, GPS
will take into account the cross reference information it has already
computed in memory, and you can get more accurate and complete results
by going to Edit->Preferences and change the
Editor->Submenu for dispatching calls preference to 'Accurate'.

Tip 22

Need to have GPS insert automatically text of Ada constructs? Go to
Tools->Plug-ins and enable the predef_ada_entity_insertions.py plug-in which
will add contextual menus in the source editor.

Tip 23

You'd like to highlight all occurrences of the current word in the editor?
Enable the 'occurrences.py' plug-in in Tools->Plug-ins and then set a key
short cut for the 'Mark Occurrences' action via the Edit->Shortcuts editor.


"""

def parse_tips ():
    """ Parse the tips string and return a list of the form
        [ (tip_title_1, tip_text_1), (tip_title_2, tip_text_2) ... ]
    """

    result = []

    current_tip_title = ""
    current_tip_text  = ""

    title_re = re.compile ("Tip [0-9]+:?( .*)?")

    for l in tips.split('\n'):
       m = title_re.match(l)

       if m:
           if len (current_tip_text) > 0:
               result += [(current_tip_title, current_tip_text)]
               current_tip_text = ""
           current_tip_title = m.group(1) or ""

       else:
           if len (l) > 1:
              current_tip_text += l + '\n'

    if len (current_tip_text) > 0:
        result += [(current_tip_title, current_tip_text)]

    return result

def display_tip (title, doc):
    """ Display the tip. Return a widget containing the tip. """

    vbox = gtk.VBox()

    # display the title
    hbox = gtk.HBox()

    title_label = gtk.Label()
    title_label.set_use_markup(True)
    title_label.set_markup ("""<big>%s</big>""" % title)

    hbox.pack_start (title_label, False, False, 10)

    vbox.pack_start (hbox, False, False, 10);

    # display the documentation
    hbox = gtk.HBox()
    doc_label = gtk.Label()
    doc_label.set_use_markup(True)
    doc_label.set_markup ("""%s""" % doc)

    hbox.pack_start (doc_label, False, False, 10)

    vbox.pack_start (hbox, True, True, 10);

    return vbox

class Tip:

    def on_close_button (self, widget):
        """ Callback on a click on the close button """

        # save the current tip number

        GPS.Preference ("tip-of-the-day-number").set (self.tip_number)

        # take into account the checkbox

        GPS.Preference (
          "General/Display-Tip-Of-The-Day").set (self.check.get_active())

        self.window.destroy()

    def on_next_button (self, widget):
        """ Callback on a click on the next button """
        self.tip_number += 1

        if self.tip_number >= len (self.results):
            self.tip_number = 0

        self.display_tip (self.tip_number)

    def on_prev_button (self, widget):
        """ Callback on a click on the previous button """
        self.tip_number -= 1

        if self.tip_number < 0:
            self.tip_number = len (self.results) - 1

        self.display_tip (self.tip_number)

    def display_tip (self, number):
        """ Display tip of the day of the given number """
        if self.tip_container.get_child():
            self.tip_container.remove (self.tip_container.get_child())

        self.tip_container.add (display_tip
                         (self.results[self.tip_number][0],
                          self.results[self.tip_number][1]))

        self.window.show_all()

    def __init__(self, results, parent=None, initial_tip=0):
        """ Display the Tip of the Day window """

        self.results = results

        background = gtk.gdk.color_parse ("#FFFFE0")

        window = gtk.Window()
        self.window = window

        window.set_default_size (550, 350)
        window.set_decorated (False)

        if parent:
            window.set_transient_for (parent)

        window.set_position (gtk.WIN_POS_CENTER_ON_PARENT)
        window.modify_bg (gtk.STATE_NORMAL, background)

        frame = gtk.Frame()
        frame.set_shadow_type (gtk.SHADOW_OUT)
        window.add (frame)

        vbox = gtk.VBox()
        frame.add (vbox)

        # window title

        hbox = gtk.HBox()
        title_label = gtk.Label()
        title_label.set_use_markup(True)
        title_label.set_markup (
          """<big>Tip of the Day</big>""")

        img=gtk.Image()
        img.set_from_stock (gtk.STOCK_INFO, gtk.ICON_SIZE_LARGE_TOOLBAR)
        hbox.pack_start (img, False, False, 10)
        hbox.pack_start (title_label, False, False, 3)
        vbox.pack_start (hbox, False, False, 10)

        # display the tip

        self.tip_number = initial_tip

        self.tip_container = gtk.Frame()
        self.tip_container.set_shadow_type (gtk.SHADOW_NONE)

        vbox.pack_start (self.tip_container, True, True, 0)

        # display the previous/next buttons

        hbox = gtk.HBox()

        label = gtk.Label()
        label.set_use_markup (True)
        label.set_markup ('<span foreground="#0000FF">Next &gt;</span>')

        next_button = gtk.Button ()
        next_button.add(label)

        label = gtk.Label()
        label.set_use_markup (True)
        label.set_markup ('<span foreground="#0000FF">&lt; Previous</span>')

        prev_button = gtk.Button ()
        prev_button.add(label)

        for button in [next_button, prev_button]:
            button.modify_bg (gtk.STATE_NORMAL, background)
            button.modify_bg (gtk.STATE_PRELIGHT, background)
            button.modify_bg (gtk.STATE_ACTIVE, background)
            button.set_relief (gtk.RELIEF_NONE)

        hbox.pack_end (next_button, False, False, 10)
        hbox.pack_end (prev_button, False, False, 3)

        vbox.pack_start (hbox, False, False, 10)

        # display the footer

        hbox = gtk.HBox()

        close_button = gtk.Button (gtk.STOCK_CLOSE)
        close_button.set_use_stock (True)
        close_button.grab_focus()

        hbox.pack_end (close_button, False, False, 10)

        self.check = gtk.CheckButton()
        self.check.set_active (True)
        self.check.set_label ("Display Tip of the Day on startup")
        self.check.modify_bg (gtk.STATE_PRELIGHT, background)

        hbox.pack_start (self.check, False, False, 10)

        vbox.pack_start (hbox, False, False, 10)

        close_button.set_flags(close_button.flags() and gtk.CAN_DEFAULT)
        window.set_default (close_button)
        window.set_focus (close_button)
        window.show_all()

        # Display tip number 0

        self.display_tip (self.tip_number)

        # callbacks

        close_button.connect ("clicked", self.on_close_button)
        next_button.connect ("clicked", self.on_next_button)
        prev_button.connect ("clicked", self.on_prev_button)


# Register preferences

GPS.Preference ("tip-of-the-day-number").create (
  "last-tip-of-the-day",
  "integer",
  "The number of the last tip of the day",
  0)

GPS.Preference ("General/Display-Tip-Of-The-Day").create (
 "Tip of the Day", "boolean",
 "Whether GPS should display the Tip of the Day dialog", True)

def on_gps_started (hook):
     if not GPS.Preference ("General/Display-Tip-Of-The-Day").get():
          return
     # If we reach this point, display the tip of the day

     # Parse the tips file
     results = parse_tips ()

     # Get the main window
     messages = GPS.MDI.get ("Messages").pywidget()
     top = messages.get_toplevel()
     t = Tip (results, top, GPS.Preference ("tip-of-the-day-number").get ())
     t.on_next_button (None)

GPS.Hook ("gps_started").add (on_gps_started)
