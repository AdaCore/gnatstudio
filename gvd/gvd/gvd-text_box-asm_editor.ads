-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package implements a text area target to the display of assembly
--  code.

with Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gtk.Menu;
with Gtk.Widget;
with Gtkada.Types;
with GVD.Text_Boxes;
with GVD.Types;

package GVD.Asm_Editors is

   type Asm_Editor_Record is new GVD.Text_Boxes.GVD_Text_Box_Record with
     private;
   type Asm_Editor is access all Asm_Editor_Record'Class;

   procedure Gtk_New
     (Editor  : out Asm_Editor;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new asm editor.

   procedure Initialize
     (Editor  : access Asm_Editor_Record'Class;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal procedure.

   procedure Configure
     (Editor            : access Asm_Editor_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Strings_Color     : Gdk.Color.Gdk_Color;
      Keyword_Color     : Gdk.Color.Gdk_Color);
   --  Set the various settings of an editor.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.
   --  Current_Line_Icon is displayed on the left of the line currently
   --  "active" (using the procedure Set_Line below).

   procedure Set_Address
     (Editor : access Asm_Editor_Record;
      Pc     : String);
   --  See GVD.Code_Editors for more information

   function On_Pixmap_Clicked
     (Editor : access Asm_Editor_Record;
      Button : Natural;
      Line   : Natural) return Boolean;
   --  See GVD.Text_Boxes for documentation

   function Child_Contextual_Menu
     (Editor : access Asm_Editor_Record;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu;
   --  See GVD.Text_Boxes for documentation

   procedure Update_Breakpoints
     (Editor    : access Asm_Editor_Record;
      Br        : GVD.Types.Breakpoint_Array);
   --  See GVD.Code_Editors for more information

   procedure Highlight_Address_Range
     (Editor   : access Asm_Editor_Record;
      Source_Line : Natural);
   --  Highlight the assembly lines matching a given source line.

   procedure On_Executable_Changed
     (Editor : access Asm_Editor_Record);
   --  Called when the executable associated with the explorer has changed.

private

   type Cache_Data;
   type Cache_Data_Access is access Cache_Data;
   type Cache_Data is record
      Low, High : GVD.Types.String_Access;
      --  The low and high ranges for this item

      Data      : GVD.Types.String_Access;
      --  The assembly code for that range

      Next      : Cache_Data_Access;
   end record;
   --  This implements a cache for the assembly code, for specific ranges.
   --  Some debuggers (gdb) might take a long time to output the assembly code
   --  for a specific region, so it is better to keep it once we have it.

   type Asm_Editor_Record is new GVD.Text_Boxes.GVD_Text_Box_Record with record
      Process         : Gtk.Widget.Gtk_Widget;
      Keywords_Color  : Gdk.Color.Gdk_Color;
      Strings_Color   : Gdk.Color.Gdk_Color;
      Highlight_Color : Gdk.Color.Gdk_Color;

      Stop_Pixmap    : Gdk.Pixmap.Gdk_Pixmap := Gdk.Pixmap.Null_Pixmap;
      Stop_Mask      : Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;

      Cache          : Cache_Data_Access;
      Current_Range  : Cache_Data_Access;
   end record;

end GVD.Asm_Editors;
