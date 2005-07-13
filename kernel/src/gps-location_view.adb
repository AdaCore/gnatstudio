-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                             AdaCore                               --
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

with Glib;                     use Glib;
with Glib.Convert;
with Glib.Values;              use Glib.Values;
with Glib.Object;              use Glib.Object;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.Event;                use Gdk.Event;

with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Check_Menu_Item;      use Gtk.Check_Menu_Item;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Enums;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;               use Gtk.Widget;

with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Box;                  use Gtk.Box;

with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;

with GNAT.OS_Lib;
with GNAT.Regpat;              use GNAT.Regpat;

with String_Utils;             use String_Utils;
with String_List_Utils;        use String_List_Utils;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with GPS.Kernel.Scripts;     use GPS.Kernel.Scripts;
with Pixmaps_IDE;              use Pixmaps_IDE;
with GPS.Intl;               use GPS.Intl;
with VFS;                      use VFS;
with Commands.Interactive;     use Commands.Interactive;

with Traces;                   use Traces;
with Commands;                 use Commands;
with Basic_Types;              use Basic_Types;
with System;

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Conversion;

package body GPS.Location_View is

   Me : constant Debug_Handle := Create ("GPS_Location_View");

   Non_Leaf_Color_Name : constant String := "blue";
   --  <preference> color to use for category and file names

   type Location_View_Module is new Module_ID_Record with null record;
   Location_View_Module_Id : Module_ID;

   package View_Idle is new Gtk.Main.Timeout (Location_View);

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   Messages_Padding : constant Integer := 10;

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column               : constant := 0;
   Base_Name_Column          : constant := 1;
   Absolute_Name_Column      : constant := 2;
   Mark_Column               : constant := 3;
   Node_Type_Column          : constant := 4;
   Line_Column               : constant := 5;
   Column_Column             : constant := 6;
   Length_Column             : constant := 7;
   Color_Column              : constant := 8;
   Button_Column             : constant := 9;
   Action_Column             : constant := 10;
   Highlight_Column          : constant := 11;
   Highlight_Category_Column : constant := 12;
   Number_Of_Items_Column    : constant := 13;
   Total_Column              : constant := 14;
   Category_Line_Column      : constant := 15;

   Output_Cst        : aliased constant String := "output";
   Category_Cst      : aliased constant String := "category";
   Regexp_Cst        : aliased constant String := "regexp";
   File_Index_Cst    : aliased constant String := "file_index";
   Line_Index_Cst    : aliased constant String := "line_index";
   Col_Index_Cst     : aliased constant String := "column_index";
   Msg_Index_Cst     : aliased constant String := "msg_index";
   Style_Index_Cst   : aliased constant String := "style_index";
   Warning_Index_Cst : aliased constant String := "warning_index";
   File_Cst          : aliased constant String := "file";
   Line_Cst          : aliased constant String := "line";
   Column_Cst        : aliased constant String := "column";
   Message_Cst       : aliased constant String := "message";
   Highlight_Cst     : aliased constant String := "highlight";
   Length_Cst        : aliased constant String := "length";
   Highlight_Cat_Cst : aliased constant String := "highlight_category";
   Style_Cat_Cst     : aliased constant String := "style_category";
   Warning_Cat_Cst   : aliased constant String := "warning_category";

   Parse_Location_Parameters : constant Cst_Argument_List :=
     (1  => Output_Cst'Access,
      2  => Category_Cst'Access,
      3  => Regexp_Cst'Access,
      4  => File_Index_Cst'Access,
      5  => Line_Index_Cst'Access,
      6  => Col_Index_Cst'Access,
      7  => Msg_Index_Cst'Access,
      8  => Style_Index_Cst'Access,
      9  => Warning_Index_Cst'Access,
      10 => Highlight_Cat_Cst'Access,
      11 => Style_Cat_Cst'Access,
      12 => Warning_Cat_Cst'Access);
   Remove_Category_Parameters : constant Cst_Argument_List :=
     (1 => Category_Cst'Access);
   Locations_Add_Parameters : constant Cst_Argument_List :=
     (1 => Category_Cst'Access,
      2 => File_Cst'Access,
      3 => Line_Cst'Access,
      4 => Column_Cst'Access,
      5 => Message_Cst'Access,
      6 => Highlight_Cst'Access,
      7 => Length_Cst'Access);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Message
     (View : access Location_View_Record'Class;
      Iter : Gtk_Tree_Iter) return String;
   --  Return the message stored at Iter.

   function Get_File
     (View : access Location_View_Record'Class;
      Iter : Gtk_Tree_Iter) return VFS.Virtual_File;
   --  Return the file stored at Iter.

   procedure Remove_Category
     (View       : access Location_View_Record'Class;
      Identifier : String;
      File       : VFS.Virtual_File);
   --  Remove category Identifier from the view. All corresponding marks
   --  are deleted.

   procedure Set_Column_Types (View : access Location_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Get_Category_File
     (View          : access Location_View_Record'Class;
      Model         : Gtk_Tree_Store;
      Category      : String;
      H_Category    : String;
      File          : VFS.Virtual_File;
      Category_Iter : out Gtk_Tree_Iter;
      File_Iter     : out Gtk_Tree_Iter;
      New_Category  : out Boolean;
      Create        : Boolean := True);
   --  Return the iter corresponding to Category, create it if
   --  necessary and if Create is True.
   --  If File is "", then the category iter will be returned.
   --  If the category was created, New_Category is set to True.

   procedure Fill_Iter
     (View               : access Location_View_Record'Class;
      Model              : Gtk_Tree_Store;
      Iter               : Gtk_Tree_Iter;
      Base_Name          : String;
      Absolute_Name      : VFS.Virtual_File;
      Message            : String;
      Mark               : Integer := -1;
      Line               : Integer;
      Column             : Integer;
      Length             : Integer;
      Highlighting       : Boolean;
      Highlight_Category : String;
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf);
   --  Fill information in Iter.
   --  Base_Name can be left to the empty string, it will then be computed
   --  automatically from Absolute_Name.
   --  If Line is 0, consider the item as a non-leaf item.

   procedure Add_Location
     (View               : access Location_View_Record'Class;
      Model              : Gtk_Tree_Store;
      Category           : String;
      File               : VFS.Virtual_File;
      Line               : Positive;
      Column             : Positive;
      Length             : Natural;
      Highlight          : Boolean;
      Message            : String;
      Highlight_Category : String;
      Quiet              : Boolean;
      Remove_Duplicates  : Boolean;
      Enable_Counter     : Boolean);
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
   --  be highlighted in the color specified by Highlight_Category

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event)
      return Boolean;
   --  Callback for the "button_press" event.

   procedure Goto_Location (Object   : access Gtk_Widget_Record'Class);
   --  Goto the selected location in the Location_View.

   procedure Remove_Category_Or_File_Iter
     (View : Location_View;
      Iter : in out Gtk_Tree_Iter);
   --  Clear all the marks and highlightings in file or category.

   procedure Remove_Category (Object   : access Gtk_Widget_Record'Class);
   --  Remove the selected category in the Location_View.

   type Clear_Locations_View_Command is new Interactive_Command
      with null record;
   function Execute
     (Command : access Clear_Locations_View_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove everything in the Location_View.

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Default context factory.

   function Create_Mark
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File;
      Line     : Natural := 1;
      Column   : Natural := 1;
      Length   : Natural := 0) return String;
   --  Create a mark for Filename, at position given by Line, Column, with
   --  length Length.
   --  Return the identifier corresponding to the mark that has been created.

   procedure Highlight_Line
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename           : VFS.Virtual_File;
      Line               : Natural;
      Column             : Natural;
      Length             : Natural;
      Highlight_Category : String;
      Highlight          : Boolean := True);
   --  Highlight the line with the corresponding category.
   --  If Highlight is set to False, remove the highlighting.
   --  If Line = 0, highlight / unhighlight all lines in file.
   --  If Length = 0, highlight the whole line, otherwise use highlight_range.

   procedure On_Row_Expanded
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_expanded" signal.

   function Get_Or_Create_Location_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return MDI_Child;
   --  Internal version of Get_Or_Create_Location_View

   function Location_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : access Hooks_Data'Class) return Boolean;
   --  Called when the user executes Location_Action_Hook

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive shell command handler.

   procedure Redraw_Totals
     (View : access Location_View_Record'Class);
   --  Reset the columns corresponding to the "total" items.

   function Idle_Redraw (View : Location_View) return Boolean;
   --  Redraw the "total" items.

   procedure Toggle_Sort
     (Widget : access Gtk_Widget_Record'Class);
   --  Callback for the activation of the sort contextual menu item.

   function Get_Category_Name
     (Model    : access Gtk_Tree_Store_Record'Class;
      Category : Gtk_Tree_Iter) return String;
   --  Return the name of the category associated with that iterator

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   -----------
   -- Hooks --
   -----------

   type File_Edited_Hook_Record is new Hook_Args_Record with record
      View : Location_View;
   end record;
   type File_Edited_Hook is access File_Edited_Hook_Record'Class;
   procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook.

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      View : Location_View renames Hook.View;
      File : constant VFS.Virtual_File := File_Hooks_Args (Data.all).File;

      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Line_Iter     : Gtk_Tree_Iter;
   begin
      --  Loop on the files in the result view and highlight lines as
      --  necessary.

      Category_Iter := Get_Iter_First (View.Tree.Model);

      while Category_Iter /= Null_Iter loop
         File_Iter := Children (View.Tree.Model, Category_Iter);

         while File_Iter /= Null_Iter loop
            if File = Get_File (View, File_Iter) then
               --  The file which has just been opened was in the locations
               --  view, highlight lines as necessary.
               Line_Iter := Children (View.Tree.Model, File_Iter);

               while Line_Iter /= Null_Iter loop
                  Highlight_Line
                    (Kernel,
                     File,
                     Integer
                       (Get_Int (View.Tree.Model, Line_Iter, Line_Column)),
                     Integer
                       (Get_Int (View.Tree.Model, Line_Iter, Column_Column)),
                     Integer
                       (Get_Int (View.Tree.Model, Line_Iter, Length_Column)),
                     Get_String
                       (View.Tree.Model,
                        File_Iter,
                        Highlight_Category_Column));

                  Next (View.Tree.Model, Line_Iter);
               end loop;
            end if;

            Next (View.Tree.Model, File_Iter);
         end loop;

         Next (View.Tree.Model, Category_Iter);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   -----------------
   -- Idle_Redraw --
   -----------------

   function Idle_Redraw (View : Location_View) return Boolean is
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;

      procedure Set_Total (Iter : Gtk_Tree_Iter; Nb_Items : Integer);
      --  Set in View.Tree.Model and Item the Total_Column string

      ---------------
      -- Set_Total --
      ---------------

      procedure Set_Total (Iter : Gtk_Tree_Iter; Nb_Items : Integer) is
         Img   : constant String := Image (Nb_Items);
      begin
         if Nb_Items = 1 then
            Set (View.Tree.Model, Iter, Total_Column,
                 " (" & Img & (-" item") & ")");
         else
            Set (View.Tree.Model, Iter, Total_Column,
                 " (" & Img & (-" items") & ")");
         end if;
      end Set_Total;

   begin
      Category_Iter := Get_Iter_First (View.Tree.Model);

      while Category_Iter /= Null_Iter loop
         File_Iter := Children (View.Tree.Model, Category_Iter);

         while File_Iter /= Null_Iter loop
            Set_Total
              (File_Iter,
               Integer
                 (Get_Int
                    (View.Tree.Model, File_Iter, Number_Of_Items_Column)));
            Next (View.Tree.Model, File_Iter);
         end loop;

         Set_Total
           (Category_Iter,
            Integer
              (Get_Int
                 (View.Tree.Model, Category_Iter, Number_Of_Items_Column)));
         Next (View.Tree.Model, Category_Iter);
      end loop;

      View.Idle_Registered := False;

      return False;
   end Idle_Redraw;

   -------------------
   -- Redraw_Totals --
   -------------------

   procedure Redraw_Totals
     (View : access Location_View_Record'Class) is
   begin
      if View.Idle_Registered then
         return;
      end if;

      View.Idle_Handler := View_Idle.Add
        (500, Idle_Redraw'Access, Location_View (View));
      View.Idle_Registered := True;
   end Redraw_Totals;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File;
      Line     : Natural := 1;
      Column   : Natural := 1;
      Length   : Natural := 0) return String
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'(Full_Name (Filename).all),
         2 => new String'(Image (Line)),
         3 => new String'(Image (Column)),
         4 => new String'(Image (Length)));
      Location : constant String :=
        Execute_GPS_Shell_Command (Kernel, "Editor.create_mark", Args);
   begin
      Basic_Types.Free (Args);
      return Location;
   end Create_Mark;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename           : VFS.Virtual_File;
      Line               : Natural;
      Column             : Natural;
      Length             : Natural;
      Highlight_Category : String;
      Highlight          : Boolean := True)
   is
      Args    : GNAT.OS_Lib.Argument_List (1 .. 5) :=
        (1 => new String'(Full_Name (Filename).all),
         2 => new String'(Highlight_Category),
         3 => new String'(Image (Line)),
         4 => new String'(Image (Column)),
         5 => new String'(Image (Column + Length)));
      Command : GNAT.OS_Lib.String_Access;
   begin
      if Highlight_Category = "" then
         return;
      end if;

      if Highlight then
         if Length = 0 then
            Command := new String'("Editor.highlight");
         else
            Command := new String'("Editor.highlight_range");
         end if;
      else
         Command := new String'("Editor.unhighlight");
      end if;

      if Line = 0 then
         Execute_GPS_Shell_Command (Kernel, Command.all, Args (1 .. 2));
      else
         if Length = 0 then
            Execute_GPS_Shell_Command (Kernel, Command.all, Args (1 .. 3));
         else
            if Highlight then
               Execute_GPS_Shell_Command (Kernel, Command.all, Args);
            else
               Execute_GPS_Shell_Command (Kernel, Command.all, Args (1 .. 3));
            end if;
         end if;
      end if;

      Basic_Types.Free (Args);
      GNAT.OS_Lib.Free (Command);
   end Highlight_Line;

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location (Object   : access Gtk_Widget_Record'Class) is
      View  : constant Location_View := Location_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Success : Boolean := True;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (View.Tree.Model, Iter);

      while Success and then Get_Depth (Path) /= 3 loop
         Success := Expand_Row (View.Tree, Path, False);
         Down (Path);
         Select_Path (Get_Selection (View.Tree), Path);
      end loop;

      Iter := Get_Iter (View.Tree.Model, Path);
      Path_Free (Path);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         Mark : constant Gint := Get_Int (Model, Iter, Mark_Column);
         Args : GNAT.OS_Lib.Argument_List :=
           (1 => new String'(Image (Integer (Mark))));
      begin
         if Mark /= -1 then
            Execute_GPS_Shell_Command (View.Kernel, "Editor.goto_mark", Args);
         end if;
         Free (Args);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Goto_Location;

   ----------------------------------
   -- Remove_Category_Or_File_Iter --
   ----------------------------------

   procedure Remove_Category_Or_File_Iter
     (View : Location_View;
      Iter : in out Gtk_Tree_Iter)
   is
      File_Iter : Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      File_Path : Gtk_Tree_Path;
      Loc_Iter  : Gtk_Tree_Iter;

      Removing_Category : Boolean := False;
      --  Indicates whether we are removing a whole category or just a file.

      use String_List;
      Categories : String_List.List;
   begin
      --  Unhighight all the lines and remove all marks in children of the
      --  category / file.

      if Iter = Null_Iter then
         return;
      end if;

      Iter_Copy (Iter, File_Iter);

      File_Path := Get_Path (View.Tree.Model, File_Iter);

      if Get_Depth (File_Path) = 1 then
         File_Iter := Children (View.Tree.Model, File_Iter);
         Removing_Category := True;

      elsif Get_Depth (File_Path) /= 2 then
         Path_Free (File_Path);
         return;
      end if;

      Path_Free (File_Path);

      while File_Iter /= Null_Iter loop
         --  Delete the marks corresponding to all locations in this file.
         Loc_Iter := Children (View.Tree.Model, File_Iter);

         while Loc_Iter /= Null_Iter loop
            declare
               Mark : constant Gint :=
                 Get_Int (View.Tree.Model, Loc_Iter, Mark_Column);
               Args : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'(Image (Integer (Mark))));

            begin
               if Mark /= -1 then
                  Execute_GPS_Shell_Command
                    (View.Kernel, "Editor.delete_mark", Args);
               end if;
               Free (Args);
            end;

            Add_Unique_Sorted
              (Categories,
               Get_String
                 (View.Tree.Model, Loc_Iter, Highlight_Category_Column));

            Next (View.Tree.Model, Loc_Iter);
         end loop;

         --  ??? Shouldn't we only remove for the specific lines, in case
         --  we have multiple categories associated with the file (for
         --  instance a loca search and all refs search)
         while not Is_Empty (Categories) loop
            Highlight_Line
              (View.Kernel,
               Get_File (View, File_Iter), 0, 0, 0, Head (Categories), False);
            Next (Categories, Free_Data => True);
         end loop;

         exit when not Removing_Category;

         Next (View.Tree.Model, File_Iter);
      end loop;

      if not Removing_Category then
         Parent := Gtk.Tree_Store.Parent (View.Tree.Model, Iter);

         Set (View.Tree.Model, Parent, Number_Of_Items_Column,
              Get_Int (View.Tree.Model, Parent, Number_Of_Items_Column)
              - Get_Int (View.Tree.Model, Iter, Number_Of_Items_Column));

         Redraw_Totals (View);
      end if;

      Remove (View.Tree.Model, Iter);
   end Remove_Category_Or_File_Iter;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category (Object   : access Gtk_Widget_Record'Class) is
      View  : constant Location_View := Location_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      Remove_Category_Or_File_Iter (View, Iter);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Remove_Category;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (View               : access Location_View_Record'Class;
      Model              : Gtk_Tree_Store;
      Iter               : Gtk_Tree_Iter;
      Base_Name          : String;
      Absolute_Name      : VFS.Virtual_File;
      Message            : String;
      Mark               : Integer := -1;
      Line               : Integer;
      Column             : Integer;
      Length             : Integer;
      Highlighting       : Boolean;
      Highlight_Category : String;
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf)
   is
      function To_Proxy is new
        Ada.Unchecked_Conversion (System.Address, C_Proxy);

      Value : GValue;
   begin
      if Base_Name = "" then
         Set (Model, Iter, Base_Name_Column, VFS.Base_Name (Absolute_Name));
      else
         if Message = "" then
            Set (Model, Iter, Base_Name_Column, Base_Name);
         else
            declare
               Padding : constant String (1 .. Messages_Padding) :=
                 (others => ' ');
            begin
               Set (Model, Iter, Base_Name_Column,
                    "<b>" & Base_Name & "</b>"
                    & Padding (1 .. Messages_Padding - Base_Name'Length)
                    & Glib.Convert.Locale_To_UTF8 (Message));
            end;
         end if;
      end if;

      Set_File (Value, Absolute_Name);

      Set_Value (Model, Iter, Absolute_Name_Column, Value);

      Set (Model, Iter, Mark_Column, Gint (Mark));
      Set (Model, Iter, Line_Column, Gint (Line));
      Set (Model, Iter, Column_Column, Gint (Column));
      Set (Model, Iter, Length_Column, Gint (Length));
      Set (Model, Iter, Icon_Column, C_Proxy (Pixbuf));
      Set (Model, Iter, Highlight_Column, Highlighting);
      Set (Model, Iter, Highlight_Category_Column, Highlight_Category);
      Set (Model, Iter, Number_Of_Items_Column, 0);

      --  ??? Lexicographic order will be used for line numbers > 1_000_000

      declare
         Img : constant String := Integer'Image (Line + 1_000_000);
      begin
         Set
           (Model,
            Iter,
            Category_Line_Column,
            Highlight_Category & Img (Img'Last - 5 .. Img'Last));
      end;

      if Line = 0 then
         Set (Model, Iter, Color_Column,
              To_Proxy (View.Non_Leaf_Color'Address));
      else
         Set (Model, Iter, Color_Column, C_Proxy'(null));
      end if;
   end Fill_Iter;

   ---------------
   -- Next_Item --
   ---------------

   procedure Next_Item
     (View      : access Location_View_Record'Class;
      Backwards : Boolean := False)
   is
      Iter          : Gtk_Tree_Iter;
      Path          : Gtk_Tree_Path;
      File_Path     : Gtk_Tree_Path;
      Category_Path : Gtk_Tree_Path;
      Model         : Gtk_Tree_Model;
      Success       : Boolean := True;

   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (View.Tree.Model, Iter);

      --  Expand to the next path corresponding to a location node.

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (View.Tree, Path, False);
         Down (Path);
         Select_Path (Get_Selection (View.Tree), Path);
      end loop;

      if Get_Depth (Path) /= 3 then
         Path_Free (Path);

         return;
      end if;

      File_Path := Copy (Path);
      Success := Up (File_Path);

      Category_Path := Copy (File_Path);
      Success := Up (Category_Path);

      if Backwards then
         Success := Prev (Path);
      else
         Next (Path);
      end if;

      if not Success or else Get_Iter (View.Tree.Model, Path) = Null_Iter then
         if Backwards then
            Success := Prev (File_Path);
         else
            Next (File_Path);
         end if;

         if not Success
           or else Get_Iter (View.Tree.Model, File_Path) = Null_Iter
         then
            File_Path := Copy (Category_Path);
            Down (File_Path);

            if Backwards then
               while Get_Iter (View.Tree.Model, File_Path) /= Null_Iter loop
                  Next (File_Path);
               end loop;

               Success := Prev (File_Path);
            end if;
         end if;

         Success := Expand_Row (View.Tree, File_Path, False);
         Path := Copy (File_Path);
         Down (Path);

         if Backwards then
            while Get_Iter (View.Tree.Model, Path) /= Null_Iter loop
               Next (Path);
            end loop;

            Success := Prev (Path);
         end if;
      end if;

      Select_Path (Get_Selection (View.Tree), Path);
      Scroll_To_Cell (View.Tree, Path, null, True, 0.1, 0.1);
      Goto_Location (View);

      Path_Free (File_Path);
      Path_Free (Path);
      Path_Free (Category_Path);
   end Next_Item;

   -----------------------
   -- Get_Category_Name --
   -----------------------

   function Get_Category_Name
     (Model    : access Gtk_Tree_Store_Record'Class;
      Category : Gtk_Tree_Iter) return String is
   begin
      return Get_String (Model, Category, Base_Name_Column);
   end Get_Category_Name;

   -----------------------
   -- Get_Category_File --
   -----------------------

   procedure Get_Category_File
     (View          : access Location_View_Record'Class;
      Model         : Gtk_Tree_Store;
      Category      : String;
      H_Category    : String;
      File          : VFS.Virtual_File;
      Category_Iter : out Gtk_Tree_Iter;
      File_Iter     : out Gtk_Tree_Iter;
      New_Category  : out Boolean;
      Create        : Boolean := True)
   is
      Category_UTF8 : constant String :=
        Glib.Convert.Locale_To_UTF8 (Category);
   begin
      File_Iter := Null_Iter;
      Category_Iter := Get_Iter_First (Model);
      New_Category := False;

      while Category_Iter /= Null_Iter
        and then Get_Category_Name (Model, Category_Iter) /= Category_UTF8
      loop
         Next (Model, Category_Iter);
      end loop;

      if Category_Iter = Null_Iter then
         if Create then
            Append (Model, Category_Iter, Null_Iter);
            Fill_Iter
              (View, Model, Category_Iter, Category_UTF8, VFS.No_File,
               "", -1, 0, 0, 0, False,
               H_Category, View.Category_Pixbuf);
            New_Category := True;
         else
            return;
         end if;
      end if;

      if File = VFS.No_File then
         return;
      end if;

      File_Iter := Children (Model, Category_Iter);

      while File_Iter /= Null_Iter loop
         if Get_File (View, File_Iter) = File then
            return;
         end if;

         Next (Model, File_Iter);
      end loop;

      --  When we reach this point, we need to create a new sub-category.

      if Create then
         Append (Model, File_Iter, Category_Iter);
         Fill_Iter
           (View, Model, File_Iter, "", File, "", -1, 0, 0, 0,
            False, H_Category, View.File_Pixbuf);
      end if;

      return;
   end Get_Category_File;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (View               : access Location_View_Record'Class;
      Model              : Gtk_Tree_Store;
      Category           : String;
      File               : VFS.Virtual_File;
      Line               : Positive;
      Column             : Positive;
      Length             : Natural;
      Highlight          : Boolean;
      Message            : String;
      Highlight_Category : String;
      Quiet              : Boolean;
      Remove_Duplicates  : Boolean;
      Enable_Counter     : Boolean)
   is
      Category_Iter    : Gtk_Tree_Iter;
      File_Iter        : Gtk_Tree_Iter;
      Iter             : Gtk_Tree_Iter;
      Category_Created : Boolean;
      Dummy            : Boolean;
      pragma Unreferenced (Dummy);

      Path               : Gtk_Tree_Path;
   begin
      if not Is_Absolute_Path (File) then
         return;
      end if;

      Get_Category_File
        (View, Model, Category, Highlight_Category,
         File, Category_Iter, File_Iter, Category_Created);

      --  Check whether the same item already exists.

      if Remove_Duplicates then
         if Category_Iter /= Null_Iter
           and then File_Iter /= Null_Iter
         then
            Iter := Children (Model, File_Iter);

            while Iter /= Null_Iter loop
               if Get_Int (Model, Iter, Line_Column) = Gint (Line)
                 and then Get_Int
                   (Model, Iter, Column_Column) = Gint (Column)
                 and then Get_Message (View, Iter) = Message
               then
                  return;
               end if;

               Next (Model, Iter);
            end loop;
         end if;
      end if;

      Append (Model, Iter, File_Iter);

      if Enable_Counter then
         Set (Model, File_Iter, Number_Of_Items_Column,
              Get_Int (Model, File_Iter, Number_Of_Items_Column) + 1);
         Set
           (Model, Category_Iter, Number_Of_Items_Column,
            Get_Int (Model, Category_Iter, Number_Of_Items_Column) + 1);

         Redraw_Totals (View);
      end if;

      if Highlight then
         Highlight_Line
           (View.Kernel, File, Line, Column, Length, Highlight_Category);
      end if;

      declare
         Output : constant String := Create_Mark
           (View.Kernel, File, Line, Column, Length);
      begin
         Fill_Iter
           (View,
            Model,
            Iter,
            Image (Line) & ":" & Image (Column), File,
            Message,
            Safe_Value (Output, -1),
            Line, Column, Length, Highlight,
            Highlight_Category);
      end;

      if Category_Created then
         if Gtk_Tree_Model (Model) = Get_Model (View.Tree) then
            Path := Get_Path (Model, Category_Iter);
            Dummy := Expand_Row (View.Tree, Path, False);
            Path_Free (Path);

            if not Quiet then
               declare
                  MDI   : constant MDI_Window := Get_MDI (View.Kernel);
                  Child : constant MDI_Child :=
                    Find_MDI_Child_By_Tag (MDI, Location_View_Record'Tag);
               begin
                  if Child /= null then
                     Raise_Child (Child, Give_Focus => False);
                  end if;
               end;
            end if;

            Path := Get_Path (Model, File_Iter);
            Dummy := Expand_Row (View.Tree, Path, False);
            Path_Free (Path);

            Path := Get_Path (Model, Iter);
            Select_Path (Get_Selection (View.Tree), Path);
            Scroll_To_Cell (View.Tree, Path, null, False, 0.1, 0.1);
            Path_Free (Path);

            if not Quiet
              and then Get_Pref (View.Kernel, Auto_Jump_To_First)
            then
               Goto_Location (View);
            end if;
         end if;
      end if;
   end Add_Location;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (View : access Location_View_Record'Class) is
      Tree          : constant Tree_View := View.Tree;
      Col           : Gtk_Tree_View_Column renames View.Sorting_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;

      Dummy         : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (View.Action_Column);
      Gtk_New (Pixbuf_Rend);
      Pack_Start (View.Action_Column, Pixbuf_Rend, False);
      Add_Attribute (View.Action_Column, Pixbuf_Rend, "pixbuf", Button_Column);
      Dummy := Append_Column (Tree, View.Action_Column);

      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "markup", Base_Name_Column);
      Add_Attribute (Col, Text_Rend, "foreground_gdk", Color_Column);

      Gtk_New (Text_Rend);
      Pack_Start (Col, Text_Rend, False);
      Add_Attribute (Col, Text_Rend, "markup", Total_Column);

      Dummy := Append_Column (Tree, Col);
      Set_Expander_Column (Tree, Col);

      Clicked (View.Sorting_Column);
   end Set_Column_Types;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column               => Gdk.Pixbuf.Get_Type,
         Absolute_Name_Column      => Get_Virtual_File_Type,

         Base_Name_Column          => GType_String,

         Mark_Column               => GType_Int,
         --  Number of the mark, -1 if no mark is set.

         Line_Column               => GType_Int,
         Column_Column             => GType_Int,
         Length_Column             => GType_Int,
         Node_Type_Column          => GType_Int,
         Color_Column              => Gdk_Color_Type,
         Button_Column             => Gdk.Pixbuf.Get_Type,
         Action_Column             => GType_Pointer,
         Highlight_Column          => GType_Boolean,
         Highlight_Category_Column => GType_String,
         Number_Of_Items_Column    => GType_Int,
         Total_Column              => GType_String,
         Category_Line_Column      => GType_String);
   end Columns_Types;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V    : constant Location_View := Location_View (View);
      Iter : Gtk_Tree_Iter;
   begin
      --  Remove all categories.

      Iter := Get_Iter_First (V.Tree.Model);

      while Iter /= Null_Iter loop
         Remove_Category_Or_File_Iter (V, Iter);
         Iter := Get_Iter_First (V.Tree.Model);
      end loop;

      Unref (V.Category_Pixbuf);
      Unref (V.File_Pixbuf);

      if V.Idle_Registered then
         Timeout_Remove (V.Idle_Handler);
         V.Idle_Registered := False;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Location_View;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID)
   is
   begin
      View := new Location_View_Record;
      Initialize (View, Kernel, Module);
   end Gtk_New;

   ------------------
   -- Context_Func --
   ------------------

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Event);
      Mitem    : Gtk_Menu_Item;

      Explorer : constant Location_View := Location_View (Object);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Model;
      Location   : Message_Context_Access := null;
      Check    : Gtk_Check_Menu_Item;

   begin
      Get_Selected (Get_Selection (Explorer.Tree), Model, Iter);

      if Model = null then
         return null;
      end if;

      Path := Get_Path (Model, Iter);

      if Path = null then
         return null;
      end if;

      Gtk_New (Check, -"Sort by subcategory");
      Set_Active (Check, Explorer.Sort_By_Category);
      Append (Menu, Check);
      Widget_Callback.Object_Connect
         (Check, "activate", Toggle_Sort'Access, Explorer);

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
         Unselect_All (Get_Selection (Explorer.Tree));
         Select_Path (Get_Selection (Explorer.Tree), Path);
      end if;

      Iter := Get_Iter (Explorer.Tree.Model, Path);

      if Get_Depth (Path) = 1 then
         Gtk_New (Mitem, -"Remove category");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Remove_Category'Access, Explorer,
            After => False);
         Append (Menu, Mitem);

      elsif Get_Depth (Path) = 2 then
         Gtk_New (Mitem, -"Remove File");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Remove_Category'Access, Explorer,
            After => False);
         Append (Menu, Mitem);

      elsif Get_Depth (Path) = 3 then
         Gtk_New (Mitem, -"Jump to location");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Goto_Location'Access, Explorer,
            After => False);

         Append (Menu, Mitem);

         declare
            Line   : constant Positive := Positive
              (Get_Int (Model, Iter, Line_Column));
            Column : constant Positive := Positive
              (Get_Int (Model, Iter, Column_Column));
            Par    : constant Gtk_Tree_Iter := Parent (Model, Iter);
            Granpa : constant Gtk_Tree_Iter := Parent (Model, Par);
         begin
            Location := new Message_Context;
            Set_File_Information
              (Location,
               Get_File (Explorer, Par),
               Line   => Line,
               Column => Column);
            Set_Message_Information
              (Location,
               Category => Get_String (Model, Granpa, Base_Name_Column),
               Message  => Get_Message (Explorer, Iter));
         end;
      end if;

      if Location /= null then
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;

      Path_Free (Path);
      return Selection_Context_Access (Location);
   end Context_Func;

   -----------------
   -- Toggle_Sort --
   -----------------

   procedure Toggle_Sort
     (Widget : access Gtk_Widget_Record'Class)
   is
      Explorer : constant Location_View := Location_View (Widget);
   begin
      Explorer.Sort_By_Category := not Explorer.Sort_By_Category;

      if Explorer.Sort_By_Category then
         Set_Sort_Column_Id (Explorer.Sorting_Column, Category_Line_Column);
      else
         Set_Sort_Column_Id (Explorer.Sorting_Column, Line_Column);
      end if;

      Clicked (Explorer.Sorting_Column);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Toggle_Sort;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Location_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID)
   is
      Scrolled : Gtk_Scrolled_Window;
      Success  : Boolean;

      File_Hook : File_Edited_Hook;
   begin
      Initialize_Hbox (View);

      View.Kernel := Kernel;

      View.Non_Leaf_Color := Parse (Non_Leaf_Color_Name);
      Alloc_Color
        (Get_Default_Colormap, View.Non_Leaf_Color, False, True, Success);

      View.Category_Pixbuf := Gdk_New_From_Xpm_Data (var_xpm);
      View.File_Pixbuf     := Gdk_New_From_Xpm_Data (mini_page_xpm);

      --  Initialize the tree.

      Gtk_New (View.Tree, Columns_Types);
      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, False);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      Widget_Callback.Connect (View, "destroy", On_Destroy'Access);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press'Access),
         View,
         After => False);

      Widget_Callback.Connect
        (View.Tree, "row_expanded", On_Row_Expanded'Access);

      Register_Contextual_Menu
        (View.Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Module_ID (Module),
         Context_Func    => Context_Func'Access);

      File_Hook := new File_Edited_Hook_Record;
      File_Hook.View := Location_View (View);
      Add_Hook
        (View.Kernel,
         GPS.Kernel.File_Edited_Hook,
         File_Hook,
         Watch => GObject (View));

      Modify_Font (View.Tree, Get_Pref_Font (Kernel, Default_Style));
      --  ??? Should we free the font description ?
   end Initialize;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree : constant Gtk_Tree_View := Gtk_Tree_View (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 1), Iter);
      Scroll_To_Cell
        (Tree, Get_Path (Get_Model (Tree), Iter), null, True, 0.1, 0.1);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Row_Expanded;

   ---------------------
   -- Insert_Location --
   ---------------------

   procedure Insert_Location
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
      Enable_Counter     : Boolean := True;
      Has_Markups        : Boolean := False)
   is
      View : constant Location_View := Get_Or_Create_Location_View (Kernel);
   begin
      if View /= null then
         if Has_Markups then
            Add_Location
              (View, View.Tree.Model, Category, File, Line, Column, Length,
               Highlight, Text, Highlight_Category,
               Quiet             => Quiet,
               Remove_Duplicates => Remove_Duplicates,
               Enable_Counter    => Enable_Counter);
         else
            Add_Location
              (View, View.Tree.Model, Category, File, Line, Column, Length,
               Highlight, Escape_Text (Text), Highlight_Category,
               Quiet             => Quiet,
               Remove_Duplicates => Remove_Duplicates,
               Enable_Counter    => Enable_Counter);
         end if;

         Gtkada.MDI.Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), View));
      end if;
   end Insert_Location;

   ----------------------
   -- Recount_Category --
   ----------------------

   procedure Recount_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String)
   is
      View : constant Location_View :=
               Get_Or_Create_Location_View (Kernel, Allow_Creation => False);
      Cat   : Gtk_Tree_Iter;
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;
      Total : Gint := 0;
      Sub   : Gint := 0;

   begin
      if View = null then
         return;
      end if;

      Get_Category_File
        (View,
         View.Tree.Model,
         Category, "", VFS.No_File, Cat, Iter, Dummy, False);

      if Cat = Null_Iter then
         return;
      end if;

      Iter := Children (View.Tree.Model, Cat);

      while Iter /= Null_Iter loop
         Sub := N_Children (View.Tree.Model, Iter);
         Set (View.Tree.Model, Iter, Number_Of_Items_Column, Sub);
         Total := Total + Sub;
         Next (View.Tree.Model, Iter);
      end loop;

      Set (View.Tree.Model, Cat, Number_Of_Items_Column, Total);

      Redraw_Totals (View);
   end Recount_Category;

   ------------------------------
   -- Remove_Location_Category --
   ------------------------------

   procedure Remove_Location_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : VFS.Virtual_File := VFS.No_File)
   is
      View : constant Location_View :=
        Get_Or_Create_Location_View (Kernel, Allow_Creation => False);
   begin
      if View /= null then
         Remove_Category (View, Category, File);
      end if;
   end Remove_Location_Category;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (View       : access Location_View_Record'Class;
      Identifier : String;
      File       : VFS.Virtual_File)
   is
      Iter       : Gtk_Tree_Iter;
      File_Iter  : Gtk_Tree_Iter;
      Dummy      : Boolean;
   begin
      Get_Category_File
        (View,
         View.Tree.Model,
         Identifier, "", File, Iter, File_Iter, Dummy);

      if File_Iter = Null_Iter then
         Remove_Category_Or_File_Iter (Location_View (View), Iter);
      else
         Remove_Category_Or_File_Iter (Location_View (View), File_Iter);

         if Children (View.Tree.Model, Iter) = Null_Iter then
            Remove_Category_Or_File_Iter (Location_View (View), Iter);
         end if;
      end if;
   end Remove_Category;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      Explorer : constant Location_View := Location_View (View);
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Success   : Command_Return_Type;
      pragma Unreferenced (Success);

   begin
      if Get_Button (Event) = 1 then
         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         if Path /= null then
            if Get_Depth (Path) /= 3 then
               Path_Free (Path);
               return False;
            else
               if Column = Explorer.Action_Column then
                  declare
                     Value   : GValue;
                     Iter    : Gtk_Tree_Iter;
                     Action  : Action_Item;

                  begin
                     Iter := Get_Iter (Explorer.Tree.Model, Path);
                     Get_Value
                       (Explorer.Tree.Model, Iter, Action_Column, Value);
                     Action := To_Action_Item (Get_Address (Value));

                     if Action /= null
                       and then Action.Associated_Command /= null
                     then
                        Success := Execute (Action.Associated_Command);
                     end if;

                     Unset (Value);
                  end;
               end if;

               Select_Path (Get_Selection (Explorer.Tree), Path);
               Goto_Location (View);
            end if;

            Path_Free (Path);
         end if;

         return True;

      else
         Grab_Focus (Explorer.Tree);

         --  If there is no selection, select the item under the cursor.
         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         if Path /= null then
            if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
               Unselect_All (Get_Selection (Explorer.Tree));
               Select_Path (Get_Selection (Explorer.Tree), Path);
            end if;

            Path_Free (Path);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   ---------------------
   -- Add_Action_Item --
   ---------------------

   procedure Add_Action_Item
     (View          : access Location_View_Record'Class;
      Identifier    : String;
      Category      : String;
      H_Category    : String;
      File          : VFS.Virtual_File;
      Line          : Natural;
      Column        : Natural;
      Message       : String;
      Action        : Action_Item)
   is
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Created       : Boolean;
      Line_Iter     : Gtk_Tree_Iter;

      Value         : GValue;
      Old_Action    : Action_Item;

      pragma Unreferenced (Identifier);
   begin
      Trace (Me, "Add_Action_Item: "
             & Full_Name (File).all
             & ' ' & Category & Line'Img & Column'Img
             & ' ' & Message);

      Get_Category_File
        (View, View.Tree.Model, Category, H_Category,
         File, Category_Iter, File_Iter, Created, False);

      if Category_Iter = Null_Iter then
         Trace (Me, "Add_Action_Item: Category " & H_Category & " not found");
      end if;

      if File_Iter = Null_Iter then
         Trace (Me, "Add_Action_Item: File " & Full_Name (File).all
                & " not found");
      end if;

      if Category_Iter /= Null_Iter
        and then File_Iter /= Null_Iter
      then
         Line_Iter := Children (View.Tree.Model, File_Iter);

         while Line_Iter /= Null_Iter loop
            if Get_Int
              (View.Tree.Model, Line_Iter, Line_Column) = Gint (Line)
              and then Get_Int
                (View.Tree.Model, Line_Iter, Column_Column) = Gint (Column)
              and then Get_Message (View, Line_Iter) =  Message
            then
               if Action = null then
                  Set (View.Tree.Model, Line_Iter,
                       Button_Column, C_Proxy (Null_Pixbuf));

                  Get_Value
                    (View.Tree.Model, Line_Iter, Action_Column, Value);
                  Old_Action := To_Action_Item (Get_Address (Value));

                  if Old_Action /= null then
                     Free (Old_Action);
                  end if;

                  Set_Address (Value, System.Null_Address);
                  Set_Value
                    (View.Tree.Model, Line_Iter, Action_Column, Value);
                  Unset (Value);

               else
                  Set (View.Tree.Model, Line_Iter,
                       Button_Column, C_Proxy (Action.Image));
                  Init (Value, GType_Pointer);
                  Set_Address (Value, To_Address (Action));

                  Set_Value
                    (View.Tree.Model, Line_Iter, Action_Column, Value);
                  Unset (Value);
               end if;

               return;
            end if;

            Next (View.Tree.Model, Line_Iter);
         end loop;
      end if;

      Trace (Me, "Add_Action_Item: entry not found");
   end Add_Action_Item;

   ---------------------------------
   -- Get_Or_Create_Location_View --
   ---------------------------------

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return Location_View
   is
      Child : MDI_Child;
   begin
      Child := Get_Or_Create_Location_View_MDI (Kernel, Allow_Creation);

      if Child = null then
         return null;
      else
         return Location_View (Get_Widget (Child));
      end if;
   end Get_Or_Create_Location_View;

   -------------------------------------
   -- Get_Or_Create_Location_View_MDI --
   -------------------------------------

   function Get_Or_Create_Location_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return MDI_Child
   is
      Child   : MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Location_View_Record'Tag);
      Locations : Location_View;
   begin
      if Child = null then
         if not Allow_Creation then
            return null;
         end if;

         Gtk_New (Locations, Kernel_Handle (Kernel),
                  Abstract_Module_ID (Location_View_Module_Id));
         Child := Put
           (Kernel, Locations,
            Module              => Location_View_Module_Id,
            Default_Width       => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height      => Get_Pref (Kernel, Default_Widget_Height),
            Position            => Position_Bottom,
            Desktop_Independent => True);
         Set_Focus_Child (Child);
         Set_Title (Child, -"Locations");
      end if;

      return Child;
   end Get_Or_Create_Location_View_MDI;

   -------------------
   -- Location_Hook --
   -------------------

   function Location_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : access Hooks_Data'Class) return Boolean
   is
      View : constant Location_View := Get_Or_Create_Location_View
        (Kernel, False);
      D : Location_Hooks_Args := Location_Hooks_Args (Data.all);
   begin
      Add_Action_Item
        (View, D.Identifier, D.Category, "", D.File,
         Integer (D.Line), Integer (D.Column), D.Message, D.Action);
      return True;
   end Location_Hook;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View  : Location_View;
      Child : MDI_Child;
      Node  : List_Node;
      Loc   : Location_Record;
   begin
      Child := Get_Or_Create_Location_View_MDI
        (Kernel, Allow_Creation => False);

      if Child = null then
         return;
      end if;

      View := Location_View (Get_Widget (Child));

      Node := First (View.Stored_Locations);

      while Node /= Null_Node loop
         Loc := Data (Node);
         Add_Location
           (View     => View,
            Model    => View.Tree.Model,
            Category => Loc.Category.all,
            File     => Loc.File,
            Line     => Loc.Line,
            Column   => Loc.Column,
            Length   => Loc.Length,
            Highlight => Loc.Highlight,
            Message  => Loc.Message.all,
            Highlight_Category => Loc.Highlight_Category.all,
            Quiet              => True,
            Remove_Duplicates  => False,
            Enable_Counter     => True);
         Node := Next (Node);
      end loop;

      Free (View.Stored_Locations);
   end Preferences_Changed;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Child : MDI_Child;
      View : Location_View;
      Category, File, Location : Node_Ptr;
   begin
      if Node.Tag.all = "Location_View_Record" then
         Child := Get_Or_Create_Location_View_MDI
           (User, Allow_Creation => True);
         View := Location_View (Get_Widget (Child));
         Category := Node.Child;
         while Category /= null loop
            declare
               Category_Name : constant String :=
                 Get_Attribute (Category, "name");
               File_Name     : Virtual_File;
               Loc           : Location_Record;
            begin
               --  Do not load errors in the project file back, since in fact
               --  they have already been overwritten if required when the
               --  project was loaded before the desktop
               if Category_Name /= "Project" then
                  File := Category.Child;
                  while File /= null loop
                     File_Name := Create
                       (Full_Filename => Get_Attribute (File, "name"));

                     Location := File.Child;
                     while Location /= null loop
                        Loc :=
                          (Category => new String'(Category_Name),
                           File     => File_Name,
                           Line     => Integer'Value
                             (Get_Attribute (Location, "line", "0")),
                           Column   => Integer'Value
                             (Get_Attribute (Location, "column", "0")),
                           Length   => Integer'Value
                             (Get_Attribute (Location, "length", "0")),
                           Highlight => True or else Boolean'Value
                             (Get_Attribute (Location, "highlight", "False")),
                           Message  => new String'
                             (Get_Attribute (Location, "message", "")),
                           Highlight_Category => new String'
                             (Get_Attribute (Location, "category", "")));
                        Location := Location.Next;

                        Append (View.Stored_Locations, Loc);
                     end loop;

                     File := File.Next;
                  end loop;
               end if;
            end;

            Category := Category.Next;
         end loop;

         return Child;
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
      View : Location_View;
      Category, File, Location : Node_Ptr;
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Location_Iter : Gtk_Tree_Iter;

   begin
      if Widget.all in Location_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Location_View_Record");

         View := Location_View (Widget);
         Category_Iter := Get_Iter_First (View.Tree.Model);
         while Category_Iter /= Null_Iter loop
            Category := new Node;
            Category.Tag := new String'("Category");
            Set_Attribute (Category, "name",
                           Get_Category_Name
                             (View.Tree.Model, Category_Iter));
            Add_Child (N, Category, True);

            File_Iter := Children (View.Tree.Model, Category_Iter);

            while File_Iter /= Null_Iter loop
               File := new Node;
               File.Tag := new String'("File");
               Add_Child (Category, File, True);

               Set_Attribute
                 (File, "name", Full_Name (Get_File (View, File_Iter)).all);

               Location_Iter := Children (View.Tree.Model, File_Iter);

               while Location_Iter /= Null_Iter loop
                  Location := new Node;
                  Location.Tag := new String'("Location");
                  Add_Child (File, Location, True);
                  Set_Attribute (Location, "line",
                                 Gint'Image
                                   (Get_Int (View.Tree.Model, Location_Iter,
                                             Line_Column)));
                  Set_Attribute (Location, "column",
                                 Gint'Image
                                   (Get_Int (View.Tree.Model, Location_Iter,
                                             Column_Column)));
                  Set_Attribute (Location, "length",
                                 Gint'Image
                                   (Get_Int (View.Tree.Model, Location_Iter,
                                             Length_Column)));
                  Set_Attribute (Location, "message",
                                 Get_Message (View, Location_Iter));
                  Set_Attribute (Location, "category",
                                 Get_String
                                   (View.Tree.Model, Location_Iter,
                                    Highlight_Category_Column));
                  Set_Attribute (Location, "highlight",
                                 Boolean'Image
                                   (Get_Boolean
                                      (View.Tree.Model, Location_Iter,
                                       Highlight_Column)));

                  Next (View.Tree.Model, Location_Iter);
               end loop;

               Next (View.Tree.Model, File_Iter);
            end loop;

            Next (View.Tree.Model, Category_Iter);
         end loop;

         return N;
      end if;

      return null;
   end Save_Desktop;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Clear_Locations_View_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Location_View :=
        Get_Or_Create_Location_View (Get_Kernel (Context.Context), False);
      Iter : Gtk_Tree_Iter;
   begin
      loop
         Iter := Get_Iter_First (View.Tree.Model);
         exit when Iter = Null_Iter;
         Remove_Category_Or_File_Iter (View, Iter);
      end loop;
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Module_Name : constant String := "Location View";
      Command : Interactive_Command_Access;
   begin
      Location_View_Module_Id := new Location_View_Module;
      Register_Module
        (Module      => Location_View_Module_Id,
         Kernel      => Kernel,
         Module_Name => Module_Name);

      Add_Hook (Kernel, Location_Action_Hook, Location_Hook'Access);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Clear_Locations_View_Command;
      Register_Contextual_Menu
        (Kernel,
         Name        => "Clear Locations View",
         Label       => -"Clear Locations View",
         Filter      => Create (Module => Module_Name),
         Action      => Command);

      Add_Hook (Kernel, Preferences_Changed_Hook, Preferences_Changed'Access);
   end Register_Module;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Locations_Class : constant Class_Type := New_Class
        (Kernel, "Locations");
   begin
      Register_Command
        (Kernel, "parse",
         Minimum_Args => 2,
         Maximum_Args => 12,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args => Locations_Add_Parameters'Length - 2,
         Maximum_Args => Locations_Add_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_category",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => Locations_Class,
         Static_Method => True,
         Handler      => Default_Command_Handler'Access);
   end Register_Commands;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "parse" then
         Name_Parameters (Data, Parse_Location_Parameters);
         declare
            Highlight_Category : constant String := Nth_Arg (Data, 10, "");
            Style_Category     : constant String := Nth_Arg (Data, 11, "");
            Warning_Category   : constant String := Nth_Arg (Data, 12, "");
         begin
            Parse_File_Locations
              (Get_Kernel (Data),
               Highlight               => Highlight_Category /= ""
                  or else Style_Category /= ""
                  or else Warning_Category /= "",
               Text                    => Nth_Arg (Data, 1),
               Category                => Nth_Arg (Data, 2),
               Highlight_Category      => Highlight_Category,
               Style_Category          => Style_Category,
               Warning_Category        => Warning_Category,
               File_Location_Regexp    => Nth_Arg (Data, 3, ""),
               File_Index_In_Regexp    => Nth_Arg (Data, 4, -1),
               Line_Index_In_Regexp    => Nth_Arg (Data, 5, -1),
               Col_Index_In_Regexp     => Nth_Arg (Data, 6, -1),
               Msg_Index_In_Regexp     => Nth_Arg (Data, 7, -1),
               Style_Index_In_Regexp   => Nth_Arg (Data, 8, -1),
               Warning_Index_In_Regexp => Nth_Arg (Data, 9, -1));
         end;

      elsif Command = "remove_category" then
         Name_Parameters (Data, Remove_Category_Parameters);
         Remove_Location_Category
           (Get_Kernel (Data),
            Category => Nth_Arg (Data, 1));

      elsif Command = "add" then
         Name_Parameters (Data, Locations_Add_Parameters);
         declare
            Highlight : constant String  := Nth_Arg (Data, 6, "");
         begin
            Insert_Location
              (Get_Kernel (Data),
               Category           => Nth_Arg (Data, 1),
               File               => Get_File (Get_Data
                 (Nth_Arg (Data, 2, (Get_File_Class (Get_Kernel (Data)))))),
               Line               => Nth_Arg (Data, 3),
               Column             => Nth_Arg (Data, 4),
               Text               => Nth_Arg (Data, 5),
               Length             => Nth_Arg (Data, 7, 0),
               Highlight          => Highlight /= "",
               Highlight_Category => Highlight,
               Quiet              => True);
         end;
      end if;
   end Default_Command_Handler;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : String;
      Highlight               : Boolean := False;
      Highlight_Category      : String := "";
      Style_Category          : String := "";
      Warning_Category        : String := "";
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1;
      Quiet                   : Boolean := False)
   is
      View      : constant Location_View :=
        Get_Or_Create_Location_View (Kernel);
      Model     : Gtk_Tree_Store;
      Expand    : Boolean := Quiet;

      function Get_File_Location return Pattern_Matcher;
      --  Return the pattern matcher for the file location

      function Get_Index
        (Pref : Param_Spec_Int; Value : Integer) return Integer;
      --  If Value is -1, return Pref, otherwise return Value

      function Get_Message (Last : Natural) return String;
      --  Return the error message. For backward compatibility with existing
      --  preferences file, we check that the message Index is still good.
      --  Otherwise, we return the last part of the regexp

      -----------------------
      -- Get_File_Location --
      -----------------------

      function Get_File_Location return Pattern_Matcher is
      begin
         if File_Location_Regexp = "" then
            return Compile (Get_Pref (Kernel, File_Pattern));
         else
            return Compile (File_Location_Regexp);
         end if;
      end Get_File_Location;

      Max : Integer := 0;
      --  Maximal value for the indexes

      ---------------
      -- Get_Index --
      ---------------

      function Get_Index
        (Pref : Param_Spec_Int; Value : Integer) return Integer
      is
         Location : Integer;
      begin
         if Value = -1 then
            Location := Integer (Get_Pref (Kernel, Pref));
         else
            Location := Value;
         end if;

         Max := Integer'Max (Max, Location);
         return Location;
      end Get_Index;

      File_Location : constant Pattern_Matcher := Get_File_Location;
      File_Index    : constant Integer :=
        Get_Index (File_Pattern_Index, File_Index_In_Regexp);
      Line_Index    : constant Integer :=
        Get_Index (Line_Pattern_Index, Line_Index_In_Regexp);
      Col_Index     : constant Integer :=
        Get_Index (Column_Pattern_Index, Col_Index_In_Regexp);
      Msg_Index     : constant Integer :=
        Get_Index (Message_Pattern_Index, Msg_Index_In_Regexp);
      Style_Index  : constant Integer :=
        Get_Index (Style_Pattern_Index, Style_Index_In_Regexp);
      Warning_Index : constant Integer :=
        Get_Index (Warning_Pattern_Index, Warning_Index_In_Regexp);
      Matched    : Match_Array (0 .. Max);
      Start      : Natural := Text'First;
      Last       : Natural;
      Real_Last  : Natural;
      Line       : Natural := 1;
      Column     : Natural := 1;
      Length     : Natural := 0;
      C          : String_Access;

      -----------------
      -- Get_Message --
      -----------------

      function Get_Message (Last : Natural) return String is
      begin
         if Matched (Msg_Index) /= No_Match then
            return Text
              (Matched (Msg_Index).First .. Matched (Msg_Index).Last);
         else
            return Text (Last + 1 .. Real_Last);
         end if;
      end Get_Message;

   begin
      if Quiet then
         Length := 1;
      end if;

      Model := View.Tree.Model;

      while Start <= Text'Last loop
         --  Parse Text line by line and look for file locations

         while Start < Text'Last
           and then (Text (Start) = ASCII.CR
                     or else Text (Start) = ASCII.LF)
         loop
            Start := Start + 1;
         end loop;

         Real_Last := Start;

         while Real_Last < Text'Last
           and then Text (Real_Last + 1) /= ASCII.CR
           and then Text (Real_Last + 1) /= ASCII.LF
         loop
            Real_Last := Real_Last + 1;
         end loop;

         Match (File_Location, Text (Start .. Real_Last), Matched);

         if Matched (0) /= No_Match then
            if Matched (Line_Index) /= No_Match then
               Line := Integer'Value
                 (Text
                    (Matched (Line_Index).First .. Matched (Line_Index).Last));

               if Line <= 0 then
                  Line := 1;
               end if;
            end if;

            if Matched (Col_Index) = No_Match then
               Last := Matched (Line_Index).Last;
            else
               Last := Matched (Col_Index).Last;
               Column := Integer'Value
                 (Text (Matched (Col_Index).First ..
                            Matched (Col_Index).Last));

               if Column <= 0 then
                  Column := 1;
               end if;
            end if;

            if Matched (Warning_Index) /= No_Match then
               C := Warning_Category'Unrestricted_Access;
            elsif  Matched (Style_Index) /= No_Match then
               C := Style_Category'Unrestricted_Access;
            else
               C := Highlight_Category'Unrestricted_Access;
            end if;

            Add_Location
              (View               => View,
               Model              => Model,
               Category           => Category,
               File               => Create
                 (Text (Matched
                          (File_Index).First .. Matched (File_Index).Last),
                  Kernel),
               Line               => Positive (Line),
               Column             => Positive (Column),
               Length             => Length,
               Highlight          => Highlight,
               Message            => Escape_Text (Get_Message (Last)),
               Highlight_Category => C.all,
               Quiet              => Expand,
               Remove_Duplicates  => False,
               Enable_Counter     => False);

            Expand := False;
         end if;

         Start := Real_Last + 1;
      end loop;

      Recount_Category (Kernel, Category);

      if View.Sort_By_Category then
         if Get_Sort_Column_Id (View.Sorting_Column)
           /= Category_Line_Column
         then
            Set_Sort_Column_Id (View.Sorting_Column, Category_Line_Column);
         end if;
      else
         if Get_Sort_Column_Id (View.Sorting_Column) /= Line_Column then
            Set_Sort_Column_Id (View.Sorting_Column, Line_Column);
         end if;
      end if;
   end Parse_File_Locations;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Location_Record) is
   begin
      Free (X.Category);
      Free (X.Message);
      Free (X.Highlight_Category);
   end Free;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (View : access Location_View_Record'Class;
      Iter : Gtk_Tree_Iter) return String
   is
      M : constant String := Get_String
        (View.Tree.Model, Iter, Base_Name_Column);
   begin
      if M'Length > Messages_Padding then
         return M (M'First + Messages_Padding + 7 .. M'Last);
      end if;

      return "";
   end Get_Message;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (View : access Location_View_Record'Class;
      Iter : Gtk_Tree_Iter) return VFS.Virtual_File
   is
      Result : Virtual_File;
      Value  : GValue;
   begin
      Get_Value (View.Tree.Model, Iter, Absolute_Name_Column, Value);
      Result := Get_File (Value);
      Unset (Value);
      return Result;
   end Get_File;

end GPS.Location_View;
