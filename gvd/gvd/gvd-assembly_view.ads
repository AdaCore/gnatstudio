-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                Copyright (C) 2000-2008, AdaCore                   --
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

with Gtk.Menu;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Pango.Font;

with GPS.Kernel;
with GVD.Process;
with GVD.Types;
with GVD.Views;

package GVD.Assembly_View is

   type Assembly_View_Record is
     new GVD.Views.Scrolled_Views.Process_View_Record with private;
   type Assembly_View is access all Assembly_View_Record'Class;

   procedure Attach_To_Assembly_View
     (Debugger            : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach Debugger to an assembly view.
   --  If an unattached assembly view exists in the desktop, it is reused.
   --  Otherwise one, is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to an assembly view.
   --
   --  The debugger console should be created already. When it is closed (ie
   --  the debugger exits), the assembly view will be destroyed.

   procedure Update_Assembly_View
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class);
   --  Update the assembly view

   procedure Update_Breakpoints
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class);
   --  Refresh the breakpoint information associated with the assembly view

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the assembly view

   procedure Configure
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the various settings of the assembly view.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.

   procedure Set_Source_Line
     (View        : Assembly_View;
      Source_Line : Natural);

   procedure Update (View : access Assembly_View_Record);

private
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

   type Assembly_View_Record is
     new GVD.Views.Scrolled_Views.Process_View_Record with
      record
         View                : Gtk.Text_View.Gtk_Text_View;

         Cache               : Cache_Data_Access;
         Current_Range       : Cache_Data_Access;
         --  The range of assembly code being displayed.

         Current_Line        : Natural := 0;
         --  ??? Not sure we need to keep that.

         Highlight_Tag       : Gtk.Text_Tag.Gtk_Text_Tag;
         --  The way the assembly range corresponding to the current source
         --  line should be displayed.

         Pc_Tag              : Gtk.Text_Tag.Gtk_Text_Tag;
         --  Tag used to materialized the PC.

         Breakpoint_Tag      : Gtk.Text_Tag.Gtk_Text_Tag;
         --  Tag used to materialized breakpoints.

         Source_Line_Start   : GVD.Types.Address_Type :=
                                 GVD.Types.Invalid_Address;
         Source_Line_End     : GVD.Types.Address_Type :=
                                 GVD.Types.Invalid_Address;
      end record;

   procedure Set_Font
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the font used for the box.
   --  This is called by Configure internally.

   function Index_From_Line
     (View : Assembly_View;
      Line : Natural) return Natural;
   --  Return the index in the buffer at which Line starts.

   function Child_Contextual_Menu
     (View   : Assembly_View;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu;
   --  Return the contextual menu to use when the user right-clicks in the
   --  text window of the child.
   --  The user has clicked on the Line-th line of the buffer, and the text
   --  below the cursor was Entity.
   --  Note that Line might be 0 in case the user clicked in an empty buffer.

   procedure Set_Text
     (View : Assembly_View;
      Text : String);
   --  Set the text associated with the box. The Hightlighting is reset.

end GVD.Assembly_View;
