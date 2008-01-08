""" This plug-in provides various menus, contextual or not, to cut, copy and
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

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Plug-ins)

standard_contextual=True
# If True, contextual menus will be created for copying, cutting and pasting
# text. They will correspond to the /Edit/Copy, /Edit/Cut and /Edit/Paste
# menus.

copy_with_line_numbers_contextual=False
# If True and standard_contextual is also True, a contextual menu to copy some
# text with the line numbers will be created.
#  Otherwise, the capability will only be accessible from the
# /Edit/Copy with line numbers menu and eventualllly the associated key
# shortcut.

grey_out_contextual=True
# If left to False, each time the contextual menu filter is not matched, the 
# contextual menu is hidden. Otherwise, it will be created by greyed out.


###########################################################################
## No user customization below this line
############################################################################

import GPS

def copy_with_line_numbers (menu):
   buffer = GPS.EditorBuffer.get()
   loc_start = buffer.selection_start()
   loc_end   = buffer.selection_end().forward_char(-1)
   selection_start = loc_start.line()
   selection_end   = loc_end.line()
   result = ""

   max_len = len (`selection_end`)

   for line in range (selection_start, selection_end + 1):
      if line == selection_end:
         end = loc_end
      else:
         end = loc_start.end_of_line()

      prefix = ""
      for j in range (len (`line`), max_len):
         prefix = prefix + " "

      result = result + (prefix + `line` + ". " +
                         buffer.get_chars (loc_start, end))
      loc_start = loc_start.forward_line (1)

   GPS.Clipboard.copy (result)

GPS.Menu.create ("/Edit/Copy with line numbers",
                 on_activate=copy_with_line_numbers,
                 ref="Copy",
                 add_before=False)

def on_area (context):
   buf = GPS.EditorBuffer.get(open=False)
   if not buf:
      return False

   start = buf.selection_start()
   end = buf.selection_end()
   return start != end

def on_copy (context):
   GPS.Editor.copy()
def on_cut (context):
   GPS.Editor.cut()
def on_paste (context):
   GPS.Editor.paste()

def on_gps_started (hook):
   if standard_contextual:
      GPS.Contextual ("sep_group_1").create \
        (on_activate=None, group=-1, visibility_filter=not grey_out_contextual)
      GPS.Contextual ("Paste").create \
        (on_activate=on_paste, group=-1, \
         visibility_filter=not grey_out_contextual)
      if copy_with_line_numbers_contextual:
         GPS.Contextual ("Copy with line numbers").create \
           (on_activate=copy_with_line_numbers, filter=on_area,  group=-1, \
            visibility_filter=not grey_out_contextual)
      GPS.Contextual ("Copy").create \
        (on_activate=on_copy, filter=on_area, group=-1, \
         visibility_filter=not grey_out_contextual)
      GPS.Contextual ("Cut").create \
        (on_activate=on_cut, filter=on_area, group=-1, \
         visibility_filter=not grey_out_contextual)
 
GPS.Hook ("gps_started").add (on_gps_started)
