-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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

with Gdk.Color;                use Gdk.Color;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Main;                 use Gtk.Main;

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Standard_Hooks;  use Glide_Kernel.Standard_Hooks;
with VFS;

with Gtkada.Tree_View;         use Gtkada.Tree_View;

package Glide_Result_View is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register this module in GPS.

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the shell commands for this module. This must be a separate
   --  subprogram, since the console is loaded before all other modules,
   --  including the scripting languages

   type Result_View_Record is new Gtk_Hbox_Record with private;
   type Result_View is access all Result_View_Record'Class;

   procedure Gtk_New
     (View        : out Result_View;
      Kernel      : Kernel_Handle;
      Module      : Module_ID);
   --  Create a new Location_View.

   procedure Initialize
     (View   : access Result_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Module_ID);
   --  Internal initialization procedure.

   function Get_Or_Create_Result_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Result_View;
   --  Return the results view widget. Create it if it doesn't exist and
   --  Allow_Creation is true.

   procedure Insert_Result
     (Kernel             : access Kernel_Handle_Record'Class;
      Category           : String;
      File               : VFS.Virtual_File;
      Text               : String;
      Line               : Positive;
      Column             : Positive;
      Length             : Natural := 0;
      Highlight          : Boolean := False;
      Highlight_Category : String := "";
      Quiet              : Boolean := False;
      Remove_Duplicates  : Boolean := True;
      Enable_Counter     : Boolean := True);
   --  Insert a new location in the result view.
   --  This is similar to Insert, except it creates the result view if
   --  necessary.
   --  If Quiet is True, the locations window will not be raised, and the
   --  cursor will not jump to the first location.
   --  If Remove_Duplicates is True, remove the duplicates while inserting
   --  the items.
   --  If Enable_Counter is True, enable the counting of the items on-the-fly.
   --  See Recount_Category below.

   procedure Recount_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String);
   --  Update the counters for Category.

   procedure Remove_Category
     (View          : access Result_View_Record'Class;
      Identifier    : String);
   --  Remove category Identifier from the view. All corresponding marks
   --  are deleted.

   procedure Remove_Result_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String);
   --  Remove Category from the results view, if it exists.
   --  Same as Remove_Category, except for the parameters type.


   procedure Next_Item
     (View      : access Result_View_Record'Class;
      Backwards : Boolean := False);
   --  If an item if selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Add_Action_Item
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Category      : String;
      H_Category    : String;
      File          : VFS.Virtual_File;
      Line          : Natural;
      Column        : Natural;
      Message       : String;
      Action        : Action_Item);
   --  Add an action item to be associated to a specified location.
   --  If Action is null, the action item will be removed from that location.
   --  H_Category is the category to highlight the item with.

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : String;
      Highlight               : Boolean := False;
      Style_Category          : String := "";
      Warning_Category        : String := "";
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1;
      Quiet                   : Boolean := False);
   --  Perform a basic parsing on Text, and add any found file locations
   --  to the results view in Category.
   --  If Highlighting is True, attempt to highlight the corresponding
   --  locations using Category as highlighting identifier.
   --  File_Location_Regexp indicates how file locations should be recognized.
   --  The default blank value will matches locations reported by gcc or GNAT,
   --  ie "file:line:column message". The various index parameters indicate the
   --  relevant parenthesis pair in the regexp.

private

   type Result_View_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;
      Tree   : Tree_View;

      Non_Leaf_Color : Gdk.Color.Gdk_Color;
      --  The color to use in the first column, depending on the status of the
      --  line.

      Category_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Pixbuf     : Gdk.Pixbuf.Gdk_Pixbuf;

      Action_Column   : Gtk_Tree_View_Column;

      Idle_Handler    : Timeout_Handler_Id;
      Idle_Registered : Boolean := False;

      Sort_By_Category : Boolean := False;
      --  Whether the view should be sorted by category.

      Sorting_Column   : Gtk_Tree_View_Column;
   end record;

end Glide_Result_View;
