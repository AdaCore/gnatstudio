import GPS
from gi.repository import Gtk, Gdk, GObject
from theme_handling import Color


class signalSetter(Gtk.Dialog):
    """
    Dialog displaying the signals that can have their value
    set and guidelines to set them
    """

    # Gdk.color_parse expecs an #rrggbb string format but the gps
    # preference is in the rgb(R,G,B) format so we convert it
    # using the Color class.
    error_color = Gdk.color_parse(Color(from_pref=GPS.Preference(
        'High-Importance-Messages-Highlight').get()).to_hex6_string())

    def __destroy_popover(self, popover):
        popover.destroy()
        GObject.source_remove(self.timeout_id)

    def __set_button_clicked(self, button, entry_field, check, debugger,
                             function, var, modeling_map):
        current_function = debugger.frames()[debugger.current_frame()][2]
        popover = Gtk.Popover()
        popover.set_relative_to(button)
        is_persistent = check.get_active()

        # Persistent value has been disabled
        if self.is_watched and not is_persistent:
            self.is_watched = False
            debugger.send("qgen_delete_watchpoint %s/%s" % (function, var),
                          output=False)
            info = Gtk.Label("Persistent value successfully disabled")
        elif current_function == function:
            desired_value = entry_field.get_text()
            previous_val = debugger.value_of(var)
            debugger.set_variable(var, desired_value)
            new_value = debugger.value_of(var)
            changed_persistent_value = new_value != self.watched_value

            # Pass the end of the function to qgen_watchpoint when the
            # language is C. This argument used to clean watchpoint is
            # not necessary in Ada as this is done properly already.
            func_info = modeling_map.get_func_bounds(function)
            function_file = func_info[0]
            if GPS.File(function_file).language().lower() == "c":
                extra_arg = '"' + function_file + ":" + func_info[-1][-1] + '"'
            else:
                extra_arg = ""

            # Only create a watchpoint if it did not exist already
            # or if its value changed.
            if is_persistent and (
                    not self.is_watched or changed_persistent_value):
                debugger.send('qgen_watchpoint %s/%s "%s" %s' % (
                    function, var, desired_value, extra_arg),
                              output=False)
                if not self.is_watched:
                    set_label = "Persistent variable value set"
                else:
                    set_label = "Persistent variable value updated"

                self.is_watched = True
                self.watched_value = new_value
                info = Gtk.Label(set_label + " successfully.")
            # Check that the syntax was correct by comparing to the new
            # value.
            elif previous_val != new_value:
                info = Gtk.Label("Variable value set successfully.")
            else:
                info = Gtk.Label("Incorrect or same value.")
                popover.modify_bg(popover.get_state(),
                                  signalSetter.error_color)

        else:
            info = Gtk.Label("The current frame does not "
                             + "contain this variable.")
            popover.modify_bg(popover.get_state(), signalSetter.error_color)

        popover.add(info)
        popover.set_position(Gtk.PositionType.LEFT)
        popover.show_all()
        self.timeout_id = GObject.timeout_add(1500, self.__destroy_popover,
                                              popover)

    def __init__(self, debugger, signal, modeling_map, is_watched):
        Gtk.Dialog.__init__(
            self,
            title="Setting values for variables associated to the signal %s" %
            signal,
            parent=GPS.MDI.current().pywidget().get_toplevel(),
            flags=Gtk.DialogFlags.MODAL
        )

        self.is_watched = is_watched
        self.watched_value = None
        self.cancelButton = self.add_button('Close', Gtk.ResponseType.CANCEL)

        self.vbox.padding = 25
        # Signal setter buttons
        signal_label = Gtk.Label("Stay conform with the debugger "
                                 + "expected format for this type")
        self.vbox.pack_start(signal_label, False, False, 5)

        current_function = debugger.frames()[debugger.current_frame()][2]

        for s in modeling_map.get_symbols(signal):
            ss = s.split('/')  # Remove the "context/" part
            var = ss[-1].strip()
            if ss[0] == current_function:
                current = debugger.value_of(var)
            else:
                current = "<not available in this frame>"
            signal_box = Gtk.HBox(homogeneous=False)
            signal_name = Gtk.Label(s)
            entry_field = Gtk.Entry()
            entry_field.set_text(current)
            entry_field.set_size_request(250, -1)
            validate_button = Gtk.Button(label="Apply")
            persistent_label = Gtk.Label("Persistent")
            check = Gtk.CheckButton()
            check.set_active(is_watched)
            check.set_tooltip_text(
                "Check to always keep these value across iterations")

            validate_button.connect("clicked", self.__set_button_clicked,
                                    entry_field, check, debugger, ss[0],
                                    var, modeling_map)

            signal_box.pack_start(persistent_label, False, False, 5)
            signal_box.pack_start(check, False, False, 5)
            signal_box.pack_start(signal_name, False, False, 5)
            signal_box.pack_end(validate_button, False, False, 5)
            signal_box.pack_end(entry_field, False, False, 5)
            self.vbox.pack_start(signal_box, False, False, 5)

        self.show_all()
