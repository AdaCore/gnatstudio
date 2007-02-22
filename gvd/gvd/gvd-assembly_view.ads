-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2007                      --
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

with GNAT.Strings;

with Glib;
with Glib.Object;
with Gtk.Box;
with Gtk.Menu;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Pango.Font;

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
      Font              : Pango.Font.Pango_Font_Description);
   --  Set the various settings of the assembly view.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.

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

   procedure Preferences_Changed (Assembly_View : GVD_Assembly_View);
   --  Called when the preferences have changed, and the editor should be
   --  redisplayed with the new setup.

private

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

   function Index_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Natural;
   --  Return the index in the buffer at which Line starts.

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

   type Cache_Data;
   type Cache_Data_Access is access Cache_Data;
   type Cache_Data is record
      Low, High : GVD.Types.Address_Type;
      --  The low and high ranges for this item

      Data      : GNAT.Strings.String_Access;
      --  The assembly code for that range

      Next      : Cache_Data_Access;
   end record;
   --  This implements a cache for the assembly code, for specific ranges.
   --  Some debuggers (gdb) might take a long time to output the assembly code
   --  for a specific region, so it is better to keep it once we have it.

   type GVD_Assembly_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Process             : Glib.Object.GObject;
      View                : Gtk.Text_View.Gtk_Text_View;

      Cache               : Cache_Data_Access;
      Current_Range       : Cache_Data_Access;
      --  The range of assembly code being displayed.

      Current_Line        : Natural := 0;
      --  ??? Not sure we need to keep that.

      Highlight_Tag       : Gtk.Text_Tag.Gtk_Text_Tag;
      --  The way the assembly range corresponding to the current source line
      --  should be displayed.

      Pc_Tag              : Gtk.Text_Tag.Gtk_Text_Tag;
      --  Tag used to materialized the PC.

      Breakpoint_Tag      : Gtk.Text_Tag.Gtk_Text_Tag;
      --  Tag used to materialized breakpoints.

      Pc                  : GVD.Types.Address_Type :=
                              GVD.Types.Invalid_Address;
      Source_Line_Start   : GVD.Types.Address_Type :=
                              GVD.Types.Invalid_Address;
      Source_Line_End     : GVD.Types.Address_Type :=
                              GVD.Types.Invalid_Address;
      Breakpoints         : GVD.Types.Breakpoint_Array_Ptr;

   end record;

end GVD.Assembly_View;
