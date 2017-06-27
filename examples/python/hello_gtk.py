# This file shows how to create a simple graphical window using the
# gtk python module

from gi.repository import Gtk


def hello_cb(button):
    window.destroy()


window = Gtk.Window(Gtk.WindowType.TOPLEVEL)  # create a top level window
window.set_border_width(10)              # set padding round child widget

button = Gtk.Button("Hello World")
button.connect("clicked", hello_cb)      # call hello_cb when clicked
window.add(button)                       # add button to window
window.show_all()                        # show window and its children
