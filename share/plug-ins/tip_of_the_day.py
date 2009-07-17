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

def parse_tips_file (filename):
    """ Parse filename and return a list of the form
        [ (tip_title_1, tip_text_1), (tip_title_2, tip_text_2) ... ]
    """

    result = []

    current_tip_title = ""
    current_tip_text  = ""

    f=open(filename)

    title_re = re.compile ("Tip [0-9]+:?( .*)?")

    for l in f:
       m = title_re.match(l)

       if m:
           if len (current_tip_text) > 0:
               result += [(current_tip_title, current_tip_text)]
               current_tip_text = ""
           current_tip_title = m.group(1) or ""

       else:
           if len (l) > 1:
              current_tip_text += l

    f.close()

    return result

def display_tip (title, doc):
    """ Display the tip. Return a widget containing the tip. """

    vbox = gtk.VBox()

    # display the title
    hbox = gtk.HBox()

    title_label = gtk.Label()
    title_label.set_use_markup(True)
    title_label.set_markup ("""<span font_size="x-large">%s</span>""" % title)

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

        window.set_default_size (600, 400)
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
          """<span font_size="xx-large">Tip of the Day</span>""")

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
     results = parse_tips_file (os.path.join
         (os.path.dirname(__file__), "tips.txt"))

     # Get the main window
     messages = GPS.MDI.get ("Messages").pywidget()
     top = messages.get_toplevel()
     print (str (top))
     t = Tip (results, top, GPS.Preference ("tip-of-the-day-number").get ())
     t.on_next_button (None)

GPS.Hook ("gps_started").add (on_gps_started)
