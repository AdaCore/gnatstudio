-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2009, AdaCore                  --
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

--  This package handles source file locations and displays them
--  in a graphical tree, per category.

with GNAT.Expect;
with GNAT.Strings;

with GNATCOLL.VFS;

with Gdk.Color;                      use Gdk.Color;
with Gdk.Pixbuf;                     use Gdk.Pixbuf;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Cell_Renderer_Text;         use Gtk.Cell_Renderer_Text;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Glib;
with Glib.Main;

with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Standard_Hooks;      use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;              use GPS.Kernel.Styles;
with GPS.Location_View_Filter_Panel; use GPS.Location_View_Filter_Panel;
with Gtkada.Tree_View;               use Gtkada.Tree_View;
with Basic_Types;                    use Basic_Types;
with Generic_List;

package GPS.Location_View is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module in GPS

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the shell commands for this module. This must be a separate
   --  subprogram, since the console is loaded before all other modules,
   --  including the scripting languages.

   type Location_View_Record is new Gtk_Vbox_Record with private;
   type Location_View is access all Location_View_Record'Class;

   procedure Gtk_New
     (View   : out Location_View;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID);
   --  Create a new Location_View

   procedure Initialize
     (View   : access Location_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID);
   --  Internal initialization procedure

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Location_View;
   --  Return the results view widget. Create it if it doesn't exist and
   --  Allow_Creation is true.

   function Category_Count
     (View     : access Location_View_Record'Class;
      Category : String) return Natural;
   --  Return the number of entries for a given category

   procedure Recount_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String);
   --  Update the counters for Category

   procedure Next_Item
     (View      : access Location_View_Record'Class;
      Backwards : Boolean := False);
   --  If an item is selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Add_Action_Item
     (View       : access Location_View_Record'Class;
      Identifier : String;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural;
      Column     : Natural;
      Message    : String;
      Action     : Action_Item);
   --  Add an action item to be associated to a specified location.
   --  If Action is null, the action item will be removed from that location.

   procedure Add_Location
     (View               : access Location_View_Record'Class;
      Category           : Glib.UTF8_String;
      File               : GNATCOLL.VFS.Virtual_File;
      Line               : Positive;
      Column             : Visible_Column_Type;
      Length             : Natural;
      Highlight          : Boolean;
      Message            : Glib.UTF8_String;
      Highlight_Category : Style_Access;
      Quiet              : Boolean;
      Remove_Duplicates  : Boolean;
      Enable_Counter     : Boolean;
      Sort_In_File       : Boolean;
      Look_For_Secondary : Boolean;
      Parent_Iter        : in out Gtk_Tree_Iter);
   --  Add a file locaton in Category (the name of the node in the location
   --  window).
   --  File is an absolute file name. If File is not currently open, do not
   --  create marks for File, but add it to the list of unresolved files
   --  instead.
   --  Message is the text to display, in pango markup language.
   --  If Quiet is True, do not raise the locations window and do not jump
   --  on the first item.
   --  If Remove_Duplicates is True, do not insert the entry if it is a
   --  duplicate.
   --  If Model is set, append the items to Model, otherwise append them
   --  to View.Tree.Model.
   --  If Highlight is true, then the matching line in the source editor will
   --  be highlighted in the color specified by Highlight_Category.
   --  If Sort_In_File is True, then all entries for each file will be sorted
   --  by (line, column). This is slightly slower, and should be set to False
   --  if you know that you are inserting them sorted already.
   --  If Look_For_Secondary is true, then we'll look and add secondary
   --  location references, if any.

   procedure Remove_Category
     (View       : access Location_View_Record'Class;
      Identifier : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural := 0);
   --  Remove category Identifier from the view. All corresponding marks
   --  are deleted.
   --  Identifier is the escaped string.

   function Model
     (Self : not null access Location_View_Record'Class)
      return not null Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns internal model.

private
   type Location_Record;
   type Location_Record_Access is access Location_Record;

   procedure Free (X : in out Location_Record_Access);
   --  Free memory associated to X

   package Location_List is new Generic_List (Location_Record_Access, Free);
   use Location_List;

   type Location_Record is record
      Category           : GNAT.Strings.String_Access;
      File               : GNATCOLL.VFS.Virtual_File;
      Line               : Integer;
      Column             : Visible_Column_Type;
      Length             : Integer;
      Highlight          : Boolean;
      Message            : GNAT.Strings.String_Access;
      --  ??? This should be a UTF8_String_Access
      Highlight_Category : GNAT.Strings.String_Access;
      --  ??? This should be a UTF8_String_Access

      Children           : List;
   end record;

   type Location_View_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;

      Tree          : Tree_View;
      Text_Renderer : Gtk_Cell_Renderer_Text;
      Filter_Panel  : Locations_Filter_Panel;

      RegExp       : GNAT.Expect.Pattern_Matcher_Access;
      Text         : GNAT.Strings.String_Access;
      Is_Hide      : Boolean := False;

      Non_Leaf_Color : Gdk.Color.Gdk_Color;
      --  The color to use in the first column, depending on the status of the
      --  line.

      Category_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Pixbuf     : Gdk.Pixbuf.Gdk_Pixbuf;

      Action_Column   : Gtk_Tree_View_Column;

      --  Idle handlers

      Idle_Redraw_Handler : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Idle_Row_Handler    : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;

      Sort_By_Category : Boolean := False;
      --  Whether the view should be sorted by category

      Sorting_Column   : Gtk_Tree_View_Column;

      Stored_Locations : List;

      --  The following are used for detection of secondary file locations
      Secondary_File_Pattern : GNAT.Expect.Pattern_Matcher_Access;
      --  Regexp corresponding to a detection of the secondary file
      SFF : Natural;
      --  Index of the secondary file
      SFC : Natural;
      --  Index of the secondary column
      SFL : Natural;
      --  Index of the secondary line

      Row : Gtk_Tree_Path;
      --  Used to record the row to make visible, see Idle_Show_Row
   end record;

end GPS.Location_View;
