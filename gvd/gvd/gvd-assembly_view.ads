-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements a text area target to the display of assembly
--  code.

with Glib;
with Glib.Object;
with Gdk.Bitmap;
with Gdk.Font;
with Gdk.Rectangle;
with Gdk.Pixmap;
with Gtk.Box;
with Gtk.Layout;
with Gtk.Menu;
with Gtk.Pixmap;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Gtkada.Types;
with Basic_Types;
with Pango.Font;

with Basic_Types;
with GVD.Types;

package GVD.Assembly_View is

   type GVD_Assembly_View_Record is new Gtk.Box.Gtk_Box_Record with private;
   type GVD_Assembly_View is access all GVD_Assembly_View_Record'Class;

   procedure Gtk_New
     (Assembly_View : out GVD_Assembly_View;
      Process       : access Glib.Object.GObject_Record'Class);

   procedure Initialize
     (Assembly_View : GVD_Assembly_View;
      Process       : access Glib.Object.GObject_Record'Class);

   procedure Configure
     (Assembly_View     : GVD_Assembly_View;
      Font              : Pango.Font.Pango_Font_Description;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array);
   --  Set the various settings of the assembly view.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.
   --  Current_Line_Icon is displayed on the left of the line currently
   --  "active" (using the procedure Set_Line below).

   procedure Set_Address
     (Assembly_View : GVD_Assembly_View;
      Pc            : GVD.Types.Address_Type);
   --  See GVD.Code_Editors for more information

   procedure Set_Source_Line
     (Assembly_View : GVD_Assembly_View;
      Source_Line   : Natural);

   procedure Update_Display (Assembly_View : GVD_Assembly_View);

   procedure Update_Breakpoints
     (Assembly_View : GVD_Assembly_View;
      Br            : GVD.Types.Breakpoint_Array);
   --  See GVD.Code_Editors for more information

   procedure On_Executable_Changed (Assembly_View : GVD_Assembly_View);
   --  Called when the executable associated with the explorer has changed.

   procedure Preferences_Changed (Assembly_View : GVD_Assembly_View);
   --  Called when the preferences have changed, and the editor should be
   --  redisplayed with the new setup.

private

   procedure Highlight_Address_Range (Assembly_View : GVD_Assembly_View);
   --  Highlight address lines in the buffer that correspond to a given source
   --  line.

   procedure Set_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural;
      Set_Current   : Boolean := True);
   --  Set the current line (and draw the button on the side).
   --  If Set_Current is True, then the line becomes the current line (ie the
   --  one on which the debugger is stopped). Otherwise, Line is simply the
   --  line that we want to display in the assembly view.

   procedure Set_Font
     (Assembly_View : GVD_Assembly_View;
      Font          : Pango.Font.Pango_Font_Description);
   --  Set the font used for the box.
   --  This is called by Configure internally.

   procedure Insert_At_Cursor
     (Assembly_View : GVD_Assembly_View;
      Chars         : String := "");
   --  Insert some text in the child text at the cursor position.

   procedure Insert_At_Cursor
     (Assembly_View : GVD_Assembly_View;
      Tag           : Gtk.Text_Tag.Gtk_Text_Tag;
      Chars         : String := "");
   --  Insert some text in the child text at the cursor position.
   --  Tag is applied to the inserted text.

   function Pixels_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Glib.Gint;
   --  Return the location (in pixels) for a given line.

   function Line_From_Pixels
     (Assembly_View : GVD_Assembly_View;
      Y             : Glib.Gint) return Natural;
   --  Return the line for a given location in pixels.

   function Index_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Natural;
   --  Return the index in the buffer at which Line starts.

   procedure Move_N_Columns
     (Assembly_View : GVD_Assembly_View;
      Index         : in out Natural;
      Columns       : Integer);
   --  Move the current Index by Columns columns to the left, while
   --  staying on the current line. Index will never be moved to the next
   --  line.
   --  Note that Columns is the number of visible columns in the widget, ie
   --  after Tabs have been expanded.

   procedure Hide_Current_Line_Button (Assembly_View : GVD_Assembly_View);
   --  Hide the arrow that shows the current line.

   function On_Pixmap_Clicked
     (Assembly_View : GVD_Assembly_View;
      Button        : Natural;
      Line          : Natural) return Boolean;
   --  Called whenever the left or right mouse buttons are pressed in the
   --  buttons area.

   function Invisible_Column_Width
     (Assembly_View : GVD_Assembly_View) return Glib.Gint;
   --  Return the width, in pixels, of the column on the left side of the
   --  text widget, whose contents is not included in the buffer.
   --  This column can be used for instance to display line numbers.

   function Child_Contextual_Menu
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural;
      Entity        : String) return Gtk.Menu.Gtk_Menu;
   --  Return the contextual menu to use when the user right-clicks in the
   --  text window of the child.
   --  The user has clicked on the Line-th line of the buffer, and the text
   --  below the cursor was Entity.
   --  Note that Line might be 0 in case the user clicked in an empty buffer.

   procedure Set_Text
     (Assembly_View : GVD_Assembly_View;
      Text          : String);
   --  Set the text associated with the box. The Hightlighting is reset.

   procedure Get_Entity_Area
     (Assembly_View : GVD_Assembly_View;
      X, Y          : in Glib.Gint;
      Area          : out Gdk.Rectangle.Gdk_Rectangle;
      Entity        : in out Basic_Types.String_Access);
   --  Return the entity pointed to by the mouse, as well as the smallest
   --  rectangle containing the entity. The X,Y coordinates of the rectangle
   --  should be relative to the X,Y arguments passed to the procedure.
   --  Area is not relevant if the Entity returned is null.

   type Cache_Data;
   type Cache_Data_Access is access Cache_Data;
   type Cache_Data is record
      Low, High : GVD.Types.Address_Type;
      --  The low and high ranges for this item

      Data      : Basic_Types.String_Access;
      --  The assembly code for that range

      Next      : Cache_Data_Access;
   end record;
   --  This implements a cache for the assembly code, for specific ranges.
   --  Some debuggers (gdb) might take a long time to output the assembly code
   --  for a specific region, so it is better to keep it once we have it.

   type GVD_Assembly_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Process             : Glib.Object.GObject;
      View                : Gtk.Text_View.Gtk_Text_View;
      Buttons             : Gtk.Layout.Gtk_Layout;

      Cache               : Cache_Data_Access;
      Current_Range       : Cache_Data_Access;
      --  The range of assembly code being displayed.

      Stop_Pixmap         : Gdk.Pixmap.Gdk_Pixmap := Gdk.Pixmap.Null_Pixmap;
      Stop_Mask           : Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;

      Current_Line_Button : Gtk.Pixmap.Gtk_Pixmap;
      Current_Line        : Natural := 0;
      --  ??? Not sure we need to keep that.

      Font                : Gdk.Font.Gdk_Font;

      Line_Height         : Glib.Gint;
      --  Height in pixel of a single line in the editor
      --  ??? Not sure we need to keep that.

      Highlight_Tag       : Gtk.Text_Tag.Gtk_Text_Tag;
      --  The way the assembly range corresponding to the current source line
      --  should be displayed.

      Pc_Tag              : Gtk.Text_Tag.Gtk_Text_Tag;

      Pc                  : GVD.Types.Address_Type :=
        GVD.Types.Invalid_Address;
      Source_Line_Start   : GVD.Types.Address_Type :=
        GVD.Types.Invalid_Address;
      Source_Line_End     : GVD.Types.Address_Type :=
        GVD.Types.Invalid_Address;

   end record;

end GVD.Assembly_View;
