""" This plugin provides various menus, contextual or not, to cut, copy and
paste text.

In particular, it also provide the capability to copy some text with the
corresponding line numbers. For example, if you select a whole buffer that
contains

procedure Void is
begin
   null;
end Void;

and activate the /Edit/Copy with line numbers menu, the following entry will
be added to the clipboard:

1. procedure Void is
2. begin
3.    null;
4. end Void;

Note that line numbers will be aligned to the biggest one, e.g.

 8. procedure Two is
 9. begin
10.    null;
11. end Two;

"""

###########################################################################
# No user customization below this line
############################################################################

import GPS
import gps_utils

GPS.Preference("Plugins/copy_paste/stdmenu").create(
    "Contextual menu", "boolean",
    """If enabled, contextual menus will be created for copying, cutting and
pasting text. They will correspond to the /Edit/Copy, /Edit/Cut and
/Edit/Paste menus. You must restart GPS to take changes into account.""",
    True)

GPS.Preference("Plugins/copy_paste/greyedout").create(
    "Grey out contextual menu", "boolean",
    """If disabled, contextual menu entries are hidden when not applicable.
If enabled, the entries are still visible by greyed out.
You must restart GPS to take changes into account.""",
    True)

GPS.Preference("Plugins/copy_paste/copy_with_line_nums").create(
    "Copy with line numbers", "boolean",
    """If enabled a contextual menu to copy some text with the line numbers
will be created.
Otherwise, the capability will only be accessible from the /Edit/Copy with
line numbers menu and possibly the associated key shortcut.""",
    False)


def on_area(context):
    buf = GPS.EditorBuffer.get(open=False)
    if not buf:
        return False

    start = buf.selection_start()
    end = buf.selection_end()
    return start != end


@gps_utils.interactive(
    name='Copy with line numbers',
    menu='/Edit/Copy with line numbers',
    before='Paste',
    filter=on_area)
def copy_with_line_numbers():
    buffer = GPS.EditorBuffer.get()
    loc_start = buffer.selection_start()
    loc_end = buffer.selection_end().forward_char(-1)
    selection_start = loc_start.line()
    selection_end = loc_end.line()
    result = ""

    max_len = len(str(selection_end))

    for line in range(selection_start, selection_end + 1):
        if line == selection_end:
            end = loc_end
        else:
            end = loc_start.end_of_line()

        prefix = ""
        for j in range(len('%s' % line), max_len):
            prefix = prefix + " "

        result = result + '%s%s. %s' % (
            prefix, line, buffer.get_chars(loc_start, end))
        loc_start = loc_start.forward_line(1)

    GPS.Clipboard.copy(result)


@gps_utils.hook('gps_started')
def __gps_started():
    global grey_out_contextual

    if GPS.Preference("Plugins/copy_paste/stdmenu").get():
        grey_out_contextual = GPS.Preference(
            "Plugins/copy_paste/greyedout").get()

        # ??? Should still show them when inapplicable if grey_out_contextual
        GPS.Action('cut to clipboard').contextual('Cut', group=-1)
        GPS.Action('copy to clipboard').contextual('Copy', group=-1)
        if GPS.Preference("Plugins/copy_paste/copy_with_line_nums").get():
            GPS.Action('Copy with line numbers').contextual(
                'Copy with line numbers', group=-1)
        GPS.Action('paste from clipboard').contextual('Paste', group=-1)
