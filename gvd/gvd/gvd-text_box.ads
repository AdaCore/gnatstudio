-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Gdk.Color;
with Gdk.Font;
with Gtk.Box;
with Gtk.Layout;
with Gtk.Menu;
with Gtk.Pixmap;
with Gtk.Text;
with Gtkada.Types;
with Odd.Types;

--  This box is a general box, that can contain any scrollable widget.
--  It is associated with a vertical scrollbar, and a layout on the left
--  side that can contain any widget. This layout is scrolled at the same
--  time as the widget itself.

package Odd.Text_Boxes is

   type Odd_Text_Box_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Odd_Text_Box is access all Odd_Text_Box_Record'Class;

   procedure Gtk_New (Box   : out Odd_Text_Box);
   --  Create a new box.

   procedure Initialize (Box   : access Odd_Text_Box_Record'Class);
   --  Initialize a new box structure

   procedure Configure
     (Box               : access Odd_Text_Box_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array);
   --  Set the various settings of an editor.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.
   --  Current_Line_Icon is displayed on the left of the line currently
   --  "active" (using the procedure Set_Line below).
   --
   --  The editor will automatically free its allocated memory when it is
   --  destroyed.

   procedure Insert
     (Box    : access Odd_Text_Box_Record;
      Fore   : in Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Back   : in Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Chars  : in String := "");
   --  Insert some text in the child text.

   procedure Set_Line
     (Box         : access Odd_Text_Box_Record;
      Line        : Natural;
      Set_Current : Boolean := True);
   --  Set the current line (and draw the button on the side).
   --  If Set_Current is True, then the line becomes the current line (ie the
   --  one on which the debugger is stopped). Otherwise, Line is simply the
   --  line that we want to display in the editor.

   function Get_Line (Box : access Odd_Text_Box_Record) return Natural;
   --  Return the current line.

   function Get_Child (Box : access Odd_Text_Box_Record)
                      return Gtk.Text.Gtk_Text;
   --  Return the child

   function Get_Buttons (Box : access Odd_Text_Box_Record)
                        return Gtk.Layout.Gtk_Layout;
   --  The layout where the buttons are displayed.

   function Pixels_From_Line
     (Box  : access Odd_Text_Box_Record;
      Line : Natural)
     return Gint;
   --  Return the location (in pixels) for a given line.

   function Line_From_Pixels
     (Box  : access Odd_Text_Box_Record;
      Y    : Gint)
     return Natural;
   --  Return the line for a given location in pixels.

   procedure Hide_Current_Line_Button (Box : access Odd_Text_Box_Record);
   --  Hide the arrow that shows the current line.

   function On_Pixmap_Clicked
     (Box    : access Odd_Text_Box_Record;
      Button : Natural;
      Line   : Natural)
     return Boolean;
   --  Called whenever the left or right mouse buttons are pressed in the
   --  buttons area.

   function Invisible_Column_Width (Box : access Odd_Text_Box_Record)
                                   return Glib.Gint;
   --  Return the width, in pixels, of the column on the left side of the
   --  text widget, whose contents is not included in the buffer.
   --  This column can be used for instance to display line numbers.

   function Child_Contextual_Menu
     (Box    : access Odd_Text_Box_Record;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu;
   --  Return the contextual menu to use when the user right-clicks in the
   --  text window of the child.
   --  The user has clicked on the Line-th line of the buffer, and the text
   --  below the cursor was Entity.

   procedure Set_Buffer
     (Box            : access Odd_Text_Box_Record;
      Buffer         : Odd.Types.String_Access := null;
      Clear_Previous : Boolean := True);
   --  Changes the buffer associated with the child. The child widget is simply
   --  cleared, but not redrawn, and you should call Update_Child to draw it
   --  again.
   --  The previous buffer is freed if Clear_Previous, but no copy of Buffer is
   --  made (we simply store the pointer).
   --  Set Buffer to null if you simply want to free the memory.

   function Get_Buffer (Box : access Odd_Text_Box_Record)
                       return Odd.Types.String_Access;
   --  Return the buffer associated with the box

   function Lines_Count (Box : access Odd_Text_Box_Record) return Natural;
   --  Return the number of lines found in the Buffer associated with the
   --  child.
   --  This function can be expensive to compute.

   function Is_Empty (Box : access Odd_Text_Box_Record) return Boolean;
   --  Return True if no data is displayed in the child.

   procedure Update_Child (Box : access Odd_Text_Box_Record'Class);
   --  Update the contents of the text in Child, from the contents of the
   --  buffer.
   --  If no buffer is defined (or the last call to Set_Buffer simply passed
   --  null), then the child is simply cleared. Otherwise, the function
   --  Insert_Buffer is called after clearing the buffer

   procedure Insert_Buffer
     (Box    : access Odd_Text_Box_Record;
      Buffer : String);
   --  This function is responsible for copying the contents of the buffer into
   --  the text child (which is the default behavior).
   --  It can be overriden if one wants to provide syntax highlighting.

private
   type Odd_Text_Box_Record is new Gtk.Box.Gtk_Box_Record with record
      Child               : Gtk.Text.Gtk_Text;
      Current_Line_Button : Gtk.Pixmap.Gtk_Pixmap;
      Current_Line        : Natural := 0;

      Buttons             : Gtk.Layout.Gtk_Layout;

      Buffer              : Odd.Types.String_Access;
      --  The contents of the text window
      --  ??? Is this really needed, this duplicates the memory used for the
      --  child.

      Font                : Gdk.Font.Gdk_Font;
      Line_Height         : Glib.Gint;
      --  Height in pixel of a single line in the editor
   end record;
end Odd.Text_Boxes;
