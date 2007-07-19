-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2007, AdaCore                  --
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

with Gdk.Color;                 use Gdk.Color;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Main;                  use Gtk.Main;
with Glib;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;         use GPS.Kernel.Styles;
with VFS;

with GNAT.Regpat;               use GNAT.Regpat;
with Gtkada.Tree_View;          use Gtkada.Tree_View;
with Basic_Types;               use Basic_Types;

with Generic_List;
with GNAT.Strings;

package GPS.Location_View is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module in GPS.

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the shell commands for this module. This must be a separate
   --  subprogram, since the console is loaded before all other modules,
   --  including the scripting languages

   type Location_View_Record is new Gtk_Hbox_Record with private;
   type Location_View is access all Location_View_Record'Class;

   procedure Gtk_New
     (View        : out Location_View;
      Kernel      : Kernel_Handle;
      Module      : Abstract_Module_ID);
   --  Create a new Location_View.

   procedure Initialize
     (View   : access Location_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID);
   --  Internal initialization procedure.

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Location_View;
   --  Return the results view widget. Create it if it doesn't exist and
   --  Allow_Creation is true.

   procedure Insert_Location
     (Kernel             : access Kernel_Handle_Record'Class;
      Category           : Glib.UTF8_String;
      File               : VFS.Virtual_File;
      Text               : String;
      Line               : Positive;
      Column             : Visible_Column_Type;
      Length             : Natural := 0;
      Highlight          : Boolean := False;
      Highlight_Category : Style_Access := null;
      Quiet              : Boolean := False;
      Remove_Duplicates  : Boolean := True;
      Enable_Counter     : Boolean := True;
      Has_Markups        : Boolean := False;
      Sort_In_File       : Boolean := False);
   --  Insert a new location in the result view.
   --  This is similar to Insert, except it creates the result view if
   --  necessary.
   --  If Quiet is True, the locations window will not be raised, and the
   --  cursor will not jump to the first location.
   --  If Remove_Duplicates is True, remove the duplicates while inserting
   --  the items.
   --  If Enable_Counter is True, enable the counting of the items on-the-fly.
   --  See Recount_Category below.
   --  If Has_Markups is True, then Text should be in Pango Markup language;
   --  in this case, the markups will be interpreted and displayed in the
   --  Locations view.
   --  If Sort_In_File is true, then the new entry will be inserted before the
   --  first entry with a higher line number. This should be avoided if you
   --  know that you are already inserting entries sorted, since it is slower.

   function Category_Count
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String) return Natural;
   --  Return the number of entries for a given category

   procedure Recount_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String);
   --  Update the counters for Category

   procedure Remove_Location_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : VFS.Virtual_File := VFS.No_File);
   --  Remove Category from the results view, if it exists.
   --  If File /= No_File, remove only the node corresponding to File in
   --  the category.

   procedure Next_Item
     (View      : access Location_View_Record'Class;
      Backwards : Boolean := False);
   --  If an item is selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Add_Action_Item
     (View          : access Location_View_Record'Class;
      Identifier    : String;
      Category      : String;
      H_Category    : Style_Access;
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
      Category                : Glib.UTF8_String;
      Highlight               : Boolean := False;
      Highlight_Category      : Style_Access := null;
      Style_Category          : Style_Access := null;
      Warning_Category        : Style_Access := null;
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
   --  locations using Highlight_Category, Style_Category or Warning_Category
   --  as highlighting identifier.
   --  File_Location_Regexp indicates how file locations should be recognized.
   --  The default blank value will matches locations reported by gcc or GNAT,
   --  ie "file:line:column message". The various index parameters indicate the
   --  relevant parenthesis pair in the regexp.

private
   type Location_Record;
   type Location_Record_Access is access Location_Record;

   procedure Free (X : in out Location_Record_Access);
   --  Free memory associated to X

   package Location_List is new Generic_List (Location_Record_Access, Free);
   use Location_List;

   type Location_Record is record
      Category           : GNAT.Strings.String_Access;
      File               : VFS.Virtual_File;
      Line               : Integer;
      Column             : Visible_Column_Type;
      Length             : Integer;
      Highlight          : Boolean;
      Message            : GNAT.Strings.String_Access;
      Highlight_Category : GNAT.Strings.String_Access;

      Children           : List;
   end record;

   type Pattern_Matcher_Access is access Pattern_Matcher;

   type Location_View_Record is new Gtk_Hbox_Record with record
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

      Stored_Locations : List;

      --  The following are used for detection of secondary file locations
      Secondary_File_Pattern : Pattern_Matcher_Access;
      --  Regexp corresponding to a detection of the secondary file.
      SFF : Natural;
      --  Index of the secondary file
      SFC : Natural;
      --  Index of the secondary column
      SFL : Natural;
      --  Index of the secondary line
   end record;

end GPS.Location_View;
