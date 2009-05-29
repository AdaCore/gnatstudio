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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Regpat;               use GNAT.Regpat;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;

with Glib.Main;                 use Glib.Main;
with Glib.Object;               use Glib.Object;
with Glib.Properties;
with Glib.Values;               use Glib.Values;
with Glib;                      use Glib;

with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tooltips;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with Default_Preferences;       use Default_Preferences;
with GPS.Editors.GtkAda;        use GPS.Editors, GPS.Editors.GtkAda;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Locations;      use GPS.Kernel.Locations;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with String_Utils;              use String_Utils;
with UTF8_Utils;                use UTF8_Utils;
with XML_Utils;                 use XML_Utils;
with Traces;                    use Traces;

package body GPS.Location_View is

   type Location_View_Module is new Module_ID_Record with null record;
   Location_View_Module_Id : Module_ID;

   package View_Idle is new Glib.Main.Generic_Sources (Location_View);

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called whenever the location in the current editor has changed, so that
   --  we can highlight the corresponding line in the locations window

   function Idle_Show_Row (View : Location_View) return Boolean;
   --  Idle callback used to ensure that the proper path on the location view
   --  is visible.

   function Idle_Expand_Category (Self : Location_View) return Boolean;
   --  Idle callback used to expand category, its first file and select
   --  first message and the open first location if requested.

   package Query_Tooltip_Callbacks is
     new Gtk.Handlers.User_Return_Callback
           (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Location_View);

   function On_Query_Tooltip
     (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Params : Glib.Values.GValues;
      Self   : Location_View)
      return Boolean;

   ---------------------
   -- Local constants --
   ---------------------

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
   Look_Sec_Cst      : aliased constant String := "look_for_secondary";

   Parse_Location_Parameters  : constant Cst_Argument_List :=
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
   Locations_Add_Parameters   : constant Cst_Argument_List :=
                                  (1 => Category_Cst'Access,
                                   2 => File_Cst'Access,
                                   3 => Line_Cst'Access,
                                   4 => Column_Cst'Access,
                                   5 => Message_Cst'Access,
                                   6 => Highlight_Cst'Access,
                                   7 => Length_Cst'Access,
                                   8 => Look_Sec_Cst'Access);

   -----------------
   -- Local types --
   -----------------

   type Location is record
      File    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line    : Positive := 1;
      Column  : Visible_Column_Type := 1;
      Message : GNAT.Strings.String_Access;
   end record;

   procedure Free (X : in out Location);
   --  Free memory associated to X

   package Locations_List is new Generic_List (Location, Free);
   use Locations_List;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Dump_To_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Dump the contents of the Locations View in File, in XML format

   procedure Read_Secondary_Pattern_Preferences
     (View : access Location_View_Record'Class);
   --  Read the preferences corresponding to the secondary file location
   --  detection.

   function Extract_Locations
     (View    : access Location_View_Record'Class;
      Message : String) return Locations_List.List;
   --  Return a list of file locations contained in Message

   procedure Set_Column_Types (View : access Location_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event

   procedure Remove_Category (Object : access Gtk_Widget_Record'Class);
   --  Remove the selected category in the Location_View

   procedure Expand_Category (Object : access Gtk_Widget_Record'Class);
   --  Expand all files in the selected Category

   procedure Collapse (Object : access Gtk_Widget_Record'Class);
   --  Collapse all categories in the Location View

   type Clear_Locations_View_Command is new Interactive_Command
      with null record;
   overriding function Execute
     (Command : access Clear_Locations_View_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove everything in the Location_View

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Default context factory

   function Create_Mark
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Line     : Natural := 1;
      Column   : Visible_Column_Type := 1) return Editor_Mark'Class;
   --  Create a mark for Filename, at position given by Line, Column, with
   --  length Length.

   procedure On_Row_Expanded
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_expanded" signal

   procedure On_Row_Collapsed
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_collapsed" signal

   procedure On_Model_Row_Inserted
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the model's "row_inserted" signal

   procedure On_Filter_Row_Inserted
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the filter's "row_inserted" signal

   function Get_Or_Create_Location_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return MDI_Child;
   --  Internal version of Get_Or_Create_Location_View

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive shell command handler

   procedure Toggle_Sort
     (Widget : access Gtk_Widget_Record'Class);
   --  Callback for the activation of the sort contextual menu item

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function Is_Visible
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self  : Location_View) return Boolean;
   --  Used by model filter for query item visibility.

   procedure On_Apply_Filter (Self : access Location_View_Record'Class);
   --  Called on "apply-filter" signal from filter panel

   procedure On_Cancel_Filter (Self : access Location_View_Record'Class);
   --  Called on "cancel-filter" signal from filter panel

   procedure On_Visibility_Toggled (Self : access Location_View_Record'Class);
   --  Called on "visibility-toggled" signal from filter panel

   procedure On_Filter_Panel_Activated
     (Widget : access Gtk_Widget_Record'Class);
   --  Called when filter panel item in the context menu is activated

   package Location_View_Callbacks is
     new Gtk.Handlers.Callback (Location_View_Record);

   package Visible_Funcs is
     new Gtk.Tree_Model_Filter.Visible_Funcs (Location_View);

   --------------------------
   -- Idle_Expand_Category --
   --------------------------

   function Idle_Expand_Category (Self : Location_View) return Boolean is
      Dummy : Boolean;
      pragma Warnings (Off, Dummy);

   begin
      --  Raise Locations window

      declare
         Child : constant MDI_Child :=
                   Find_MDI_Child_By_Tag
                     (Get_MDI (Self.Kernel), Location_View_Record'Tag);

      begin
         if Child /= null then
            Raise_Child (Child, Give_Focus => False);
         end if;
      end;

      --  Expand category and first file items

      Dummy := Expand_Row (Self.Tree, Self.Expand_Path, False);

      Down (Self.Expand_Path);
      Dummy := Expand_Row (Self.Tree, Self.Expand_Path, False);

      --  Select first message

      Down (Self.Expand_Path);
      Select_Path (Get_Selection (Self.Tree), Self.Expand_Path);
      Scroll_To_Cell (Self.Tree, Self.Expand_Path, null, False, 0.1, 0.1);

      --  Cleanup

      Path_Free (Self.Expand_Path);
      Self.Expand_Path := null;
      Self.Idle_Expand_Handler := No_Source_Id;

      --  If go to location operation is requested, when go to the location of
      --  the selected message (it is first message)

      if Self.Goto_Expanded then
         Self.Goto_Expanded := False;
         Self.Goto_Location;
      end if;

      return False;
   end Idle_Expand_Category;

   -------------------
   -- Idle_Show_Row --
   -------------------

   function Idle_Show_Row (View : Location_View) return Boolean is
      Path                 : constant Gtk_Tree_Path := View.Row;
      Iter                 : Gtk_Tree_Iter;
      Start_Path, End_Path : Gtk_Tree_Path;
      Success              : Boolean := False;
      Res                  : Gint;
   begin
      Get_Visible_Range (View.Tree, Start_Path, End_Path, Success);

      if not Success then
         return False;
      end if;

      --  Ensure that when a row is expanded some children are visible

      Down (Path);
      Iter := Get_Iter (Get_Model (View.Tree), Path);

      if N_Children (Get_Model (View.Tree), Iter) > 1 then
         --  More than one child, try to display the first child and the
         --  following one. This is cleaner as a row returned as visible even
         --  if partially on the screen. So if the second child is at least
         --  partly visible we know for sure that the first child is fully
         --  visible.
         Next (Path);
      end if;

      Res := Compare (Path, End_Path);

      if Res >= 0
        and then Get_Iter (Get_Model (View.Tree), Path) /= Null_Iter
      then
         --  Path is the last path visible, scoll to see some entries under
         --  this node.
         Scroll_To_Cell (View.Tree, Path, null, True, 0.9, 0.1);
      end if;

      Path_Free (Path);
      View.Row := null;
      Path_Free (Start_Path);
      Path_Free (End_Path);

      View.Idle_Row_Handler := Glib.Main.No_Source_Id;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         View.Idle_Row_Handler := Glib.Main.No_Source_Id;
         return False;
   end Idle_Show_Row;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Line     : Natural := 1;
      Column   : Visible_Column_Type := 1) return Editor_Mark'Class
   is
   begin
      return New_Mark
        (Get_Buffer_Factory (Kernel).all,
         File   => Filename,
         Line   => Line,
         Column => Integer (Column));
   end Create_Mark;

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location (Self : access Location_View_Record'Class) is
      Iter    : Gtk_Tree_Iter;
      Model   : Gtk_Tree_Model;
      Path    : Gtk_Tree_Path;
      Success : Boolean := True;
   begin
      if Self.Idle_Expand_Handler /= No_Source_Id then
         Self.Goto_Expanded := True;

         return;
      end if;

      Get_Selected (Get_Selection (Self.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (Model, Iter);

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (Self.Tree, Path, False);
         Down (Path);
         Select_Path (Get_Selection (Self.Tree), Path);
      end loop;

      Iter := Get_Iter (Model, Path);
      Path_Free (Path);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         Mark : constant Editor_Mark'Class :=
           Get_Mark (Model, Iter, Mark_Column);
         Loc : constant Editor_Location'Class := Mark.Location (True);
      begin
         if Mark /= Nil_Editor_Mark then
            Loc.Buffer.Current_View.Cursor_Goto (Loc, Raise_View => True);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Goto_Location;

   ---------------------
   -- Expand_Category --
   ---------------------

   procedure Expand_Category (Object : access Gtk_Widget_Record'Class) is
      View  : constant Location_View := Location_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      Path := Get_Path (Model, Iter);

      while Get_Depth (Path) > 1 loop
         Dummy := Up (Path);
      end loop;

      Dummy := Expand_Row (View.Tree, Path, True);

      Path_Free (Path);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Expand_Category;

   --------------
   -- Collapse --
   --------------

   procedure Collapse (Object : access Gtk_Widget_Record'Class) is
      View : constant Location_View := Location_View (Object);
   begin
      Collapse_All (View.Tree);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Collapse;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category (Object : access Gtk_Widget_Record'Class) is
      View        : constant Location_View := Location_View (Object);
      Filter_Iter : Gtk_Tree_Iter;
      Store_Iter  : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;

   begin
      Get_Selected (Get_Selection (View.Tree), Model, Filter_Iter);
      View.Filter.Convert_Iter_To_Child_Iter (Store_Iter, Filter_Iter);
      Remove_Category_Or_File_Iter (View.Kernel, View.Model, Store_Iter);
      View.Model.Redraw_Totals;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_Category;

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
      pragma Warnings (Off, Success);

   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (Model, Iter);

      --  Expand to the next path corresponding to a location node

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (View.Tree, Path, False);
         Down (Path);
         Select_Path (Get_Selection (View.Tree), Path);
      end loop;

      if Get_Depth (Path) < 3 then
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

      if not Success or else Get_Iter (Model, Path) = Null_Iter then
         if Backwards then
            Success := Prev (File_Path);
         else
            Next (File_Path);
         end if;

         if not Success
           or else Get_Iter (Model, File_Path) = Null_Iter
         then
            if Locations_Wrap.Get_Pref then
               File_Path := Copy (Category_Path);
               Down (File_Path);

               if Backwards then
                  while Get_Iter (Model, File_Path) /= Null_Iter loop
                     Next (File_Path);
                  end loop;

                  Success := Prev (File_Path);
               end if;
            else
               Path_Free (File_Path);
               Path_Free (Path);
               Path_Free (Category_Path);
               return;
            end if;
         end if;

         Success := Expand_Row (View.Tree, File_Path, False);
         Path := Copy (File_Path);
         Down (Path);

         if Backwards then
            while Get_Iter (Model, Path) /= Null_Iter loop
               Next (Path);
            end loop;

            Success := Prev (Path);
         end if;
      end if;

      Select_Path (Get_Selection (View.Tree), Path);
      Scroll_To_Cell (View.Tree, Path, null, False, 0.1, 0.1);
      Goto_Location (View);

      Path_Free (File_Path);
      Path_Free (Path);
      Path_Free (Category_Path);
   end Next_Item;

   ------------------
   -- Add_Location --
   ------------------

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
      Remove_Duplicates  : Boolean;
      Sort_In_File       : Boolean;
      Look_For_Secondary : Boolean;
      Parent_Iter        : in out Gtk_Tree_Iter)
   is
      Category_Iter    : Gtk_Tree_Iter;
      File_Iter        : Gtk_Tree_Iter;
      Iter, Iter2      : Gtk_Tree_Iter := Null_Iter;
      Aux_Iter         : Gtk_Tree_Iter;
      Category_Created : Boolean;
      Dummy            : Boolean;
      pragma Unreferenced (Dummy);

      Path                   : Gtk_Tree_Path;
      Added                  : Boolean := False;
      Locs                   : Locations_List.List;
      Loc                    : Location;
      Node                   : Locations_List.List_Node;
      Has_Secondary_Location : Boolean := False;

      function Matches_Location (Iter : Gtk_Tree_Iter) return Boolean;
      --  Return True if Iter matches the file, line and column of the location
      --  being considered for addition.

      ----------------------
      -- Matches_Location --
      ----------------------

      function Matches_Location (Iter : Gtk_Tree_Iter) return Boolean is
      begin
         return  View.Model.Get_Int (Iter, Line_Column) = Gint (Line)
           and then View.Model.Get_Int (Iter, Column_Column) = Gint (Column)
           and then Get_File (View.Model, Iter) = File;
      end Matches_Location;

   begin
      if not Is_Absolute_Path (File) then
         return;
      end if;

      Get_Category_File
        (View.Model, Category,
         File, Category_Iter, File_Iter, Category_Created, True);

      --  Check whether the same item already exists

      if Remove_Duplicates then
         if Category_Iter /= Null_Iter
           and then File_Iter /= Null_Iter
         then
            Iter := View.Model.Children (File_Iter);

            while Iter /= Null_Iter loop
               if View.Model.Get_Int (Iter, Line_Column) = Gint (Line)
                 and then View.Model.Get_Int
                   (Iter, Column_Column) = Gint (Column)
                 and then Get_Message (View.Model, Iter) = Message
               then
                  return;
               end if;

               View.Model.Next (Iter);
            end loop;
         end if;
      end if;

      if Sort_In_File then
         Iter2 := View.Model.Children (File_Iter);
         while Iter2 /= Null_Iter loop
            if View.Model.Get_Int (Iter2, Line_Column) > Gint (Line)
              or else (View.Model.Get_Int (Iter2, Line_Column) = Gint (Line)
                       and then View.Model.Get_Int (Iter2, Column_Column) >
                         Gint (Column))
            then
               View.Model.Insert_Before (Iter, File_Iter, Iter2);
               Added := True;
               exit;
            end if;
            View.Model.Next (Iter2);
         end loop;
      end if;

      --  Recompute number of items and activate idle redraw of counters

      View.Model.Set
        (File_Iter,
         Number_Of_Items_Column,
         View.Model.Get_Int (File_Iter, Number_Of_Items_Column) + 1);
      View.Model.Set
        (Category_Iter,
         Number_Of_Items_Column,
         View.Model.Get_Int (Category_Iter, Number_Of_Items_Column) + 1);
      View.Model.Redraw_Totals;

      if Highlight then
         Highlight_Line
           (View.Kernel, File, Line, Column, Length, Highlight_Category);
      end if;

      --  Look for secondary file information and loop on information found
      if Look_For_Secondary then
         Locs := Extract_Locations (View, Message);
         Has_Secondary_Location := not Is_Empty (Locs);
      else
         Has_Secondary_Location := False;
      end if;

      declare
         Potential_Parent : Gtk_Tree_Iter := Null_Iter;
      begin
         --  If we have a secondary file, look for a potential parent iter:
         --  an iter with same line and same column.
         --  Most likely Parent_Iter will fill that role.

         if Has_Secondary_Location then
            if Parent_Iter = Null_Iter then
               --  No parent iter, browse the list for an iter with acceptable
               --  line and column.

               Iter := View.Model.Children (File_Iter);

               while Iter /= Null_Iter loop
                  exit when Matches_Location (Iter);

                  View.Model.Next (Iter);
               end loop;

               Potential_Parent := Iter;

            else
               --  We have a parent, check that the line and column are the
               --  same.
               if Matches_Location (Parent_Iter) then
                  --  We have an acceptable potential parent
                  Potential_Parent := Parent_Iter;
               end if;
            end if;

            if Potential_Parent = Null_Iter then
               --  If we have not found a potential parent, add an iter and
               --  fill it with primary information, and then create an iter
               --  with just the secondary information.

               if not Added then
                  View.Model.Append (Iter, File_Iter);
               end if;

               Fill_Iter
                 (View.Model, Iter,
                  Image (Line) & ":" & Image (Integer (Column)),
                  File, Message,
                  Create_Mark (View.Kernel, File, Line, Column),
                  Line, Column, Length, Highlight,
                  Highlight_Category);

               Potential_Parent := Iter;
            else
               --  We have found a potential parent, look after it if there
               --  are iters with the same line and column.

               Iter := Potential_Parent;

               while Iter /= Null_Iter loop
                  if Matches_Location (Iter) then
                     Potential_Parent := Iter;
                  else
                     exit;
                  end if;

                  View.Model.Next (Iter);
               end loop;
            end if;

            --  We now have a filled potential parent, add iters for the
            --  secondary information.

            Parent_Iter := Potential_Parent;

            Node := First (Locs);

            while Node /= Locations_List.Null_Node loop
               View.Model.Append (Iter, Potential_Parent);

               Loc := Data (Node);

               Fill_Iter
                 (View.Model, Iter, " ", Loc.File, Loc.Message.all,
                  Create_Mark (View.Kernel, Loc.File, Loc.Line, Loc.Column),
                  Loc.Line, Loc.Column, Length,
                  Highlight, Highlight_Category);

               View.Filter.Convert_Child_Iter_To_Iter
                 (Aux_Iter, Potential_Parent);
               Path := View.Filter.Get_Path (Aux_Iter);
               Dummy := Expand_Row (View.Tree, Path, False);
               Path_Free (Path);

               Node := Next (Node);
            end loop;

            Free (Locs);

            Iter := Potential_Parent;

         else
            --  Fill Iter with main information

            if not Added then
               View.Model.Append (Iter, File_Iter);
            end if;

            Fill_Iter
              (View.Model, Iter,
               Image (Line) & ":" & Image (Integer (Column)),
               File, Message,
               Create_Mark (View.Kernel, File, Line, Column),
               Line, Column, Length, Highlight,
               Highlight_Category);

            Parent_Iter := Iter;
         end if;
      end;
   end Add_Location;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (View : access Location_View_Record'Class) is
      Col         : Gtk_Tree_View_Column renames View.Sorting_Column;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;

      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Set_Rules_Hint (View.Tree, False);

      Gtk_New (View.Action_Column);
      Gtk_New (Pixbuf_Rend);
      Pack_Start (View.Action_Column, Pixbuf_Rend, False);
      Add_Attribute (View.Action_Column, Pixbuf_Rend, "pixbuf", Button_Column);
      Dummy := Append_Column (View.Tree, View.Action_Column);

      Gtk_New (View.Text_Renderer);
      Gtk_New (Pixbuf_Rend);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, View.Text_Renderer, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, View.Text_Renderer, "markup", Base_Name_Column);
      Add_Attribute (Col, View.Text_Renderer, "foreground_gdk", Color_Column);

      Dummy := Append_Column (View.Tree, Col);
      View.Tree.Set_Expander_Column (Col);

      Clicked (View.Sorting_Column);
   end Set_Column_Types;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V    : constant Location_View := Location_View (View);

   begin
      Basic_Types.Unchecked_Free (V.Secondary_File_Pattern);

      if V.Row /= null then
         Path_Free (V.Row);
      end if;

      if V.Idle_Row_Handler /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (V.Idle_Row_Handler);
      end if;

      if V.Idle_Expand_Handler /= No_Source_Id then
         Glib.Main.Remove (V.Idle_Expand_Handler);
      end if;

      if V.Expand_Path /= null then
         Path_Free (V.Expand_Path);
      end if;

      --  Free regular expression

      Basic_Types.Unchecked_Free (V.RegExp);
      GNAT.Strings.Free (V.Text);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Location_View;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID) is
   begin
      View := new Location_View_Record;
      Initialize (View, Kernel, Module);
   end Gtk_New;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Kernel, Event_Widget, Event);
      Mitem    : Gtk_Menu_Item;

      Explorer : constant Location_View := Location_View (Object);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Model;
      Check    : Gtk_Check_Menu_Item;
      Created  : Boolean := False;

   begin
      Get_Selected (Get_Selection (Explorer.Tree), Model, Iter);

      if Model = null then
         return;
      end if;

      Path := Get_Path (Model, Iter);

      if Path = null then
         return;
      end if;

      Gtk_New (Check, -"Filter panel");
      Set_Active (Check, Explorer.Filter_Panel.Mapped_Is_Set);
      Append (Menu, Check);
      Widget_Callback.Object_Connect
        (Check,
         Gtk.Menu_Item.Signal_Activate,
         On_Filter_Panel_Activated'Access,
         Explorer);

      Gtk_New (Check, -"Sort by subcategory");
      Set_Active (Check, Explorer.Sort_By_Category);
      Append (Menu, Check);
      Widget_Callback.Object_Connect
        (Check, Gtk.Menu_Item.Signal_Activate, Toggle_Sort'Access, Explorer);

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      Gtk_New (Mitem, -"Expand category");
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Mitem,
         Gtk.Menu_Item.Signal_Activate,
         Expand_Category'Access,
         Explorer,
         After => False);
      Append (Menu, Mitem);

      Gtk_New (Mitem, -"Collapse all");
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, Collapse'Access, Explorer,
         After => False);
      Append (Menu, Mitem);

      if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
         Unselect_All (Get_Selection (Explorer.Tree));
         Select_Path (Get_Selection (Explorer.Tree), Path);
      end if;

      Iter := Get_Iter (Model, Path);

      if Get_Depth (Path) = 1 then
         Gtk_New (Mitem, -"Remove category");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem,
            Gtk.Menu_Item.Signal_Activate,
            Remove_Category'Access,
            Explorer,
            After => False);
         Append (Menu, Mitem);

      elsif Get_Depth (Path) = 2 then
         Gtk_New (Mitem, -"Remove File");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem,
            Gtk.Menu_Item.Signal_Activate,
            Remove_Category'Access,
            Explorer,
            After => False);
         Append (Menu, Mitem);

      elsif Get_Depth (Path) >= 3 then
         Gtk_New (Mitem, -"Jump to location");
         Location_View_Callbacks.Object_Connect
           (Mitem,
            Gtk.Menu_Item.Signal_Activate,
            Goto_Location'Access,
            Explorer,
            False);

         Append (Menu, Mitem);

         declare
            Line   : constant Positive := Positive
              (Get_Int (Model, Iter, Line_Column));
            Column : constant Visible_Column_Type := Visible_Column_Type
              (Get_Int (Model, Iter, Column_Column));
            Par    : constant Gtk_Tree_Iter := Parent (Model, Iter);
            Granpa : constant Gtk_Tree_Iter := Parent (Model, Par);
         begin
            Created := True;
            Set_File_Information
              (Context,
               Files  => (1 => Get_File (Model, Par)),
               Line   => Line,
               Column => Column);
            Set_Message_Information
              (Context,
               Category => Get_String (Model, Granpa, Base_Name_Column),
               Message  => Get_Message (Model, Iter));
         end;
      end if;

      if Created then
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;

      Path_Free (Path);
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
      when E : others => Trace (Exception_Handle, E);
   end Toggle_Sort;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D           : constant File_Location_Hooks_Args_Access :=
                      File_Location_Hooks_Args_Access (Data);
      Child       : MDI_Child;
      Locations   : Location_View;
      Category_Iter, File_Iter, Loc_Iter, Current : Gtk_Tree_Iter;
      Filter_Loc_Iter  : Gtk_Tree_Iter;
      Category_Created : Boolean;
      Model            : Gtk_Tree_Model;
      Path             : Gtk_Tree_Path;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Location_View_Record'Tag);
      Locations := Location_View (Get_Widget (Child));

      --  Check current selection: if it is on the same line as the new
      --  location, do not change the selection. Otherwise, there is no easy
      --  way for a user to click on a secondary location found in the same
      --  error message.

      Get_Selected (Get_Selection (Locations.Tree), Model, Current);

      if Current /= Null_Iter
        and then Integer (Get_Int (Model, Current, Line_Column))
        = D.Line
      then
         return;
      end if;

      --  Highlight the location. Use the same category as the current
      --  selection, since otherwise the user that has both "Builder results"
      --  and "search" would automatically be moved to the builder when
      --  traversing all search results.

      if Current = Null_Iter then
         Get_Category_File
           (Locations.Model,
            Category      => "Builder results",
            File          => D.File,
            Category_Iter => Category_Iter,
            File_Iter       => File_Iter,
            New_Category    => Category_Created,
            Create          => False);

      else
         Get_Category_File
           (Locations.Model,
            Category      => Get_Category_Name (Model, Current),
            File          => D.File,
            Category_Iter => Category_Iter,
            File_Iter     => File_Iter,
            New_Category  => Category_Created,
            Create        => False);
      end if;

      if File_Iter /= Null_Iter then
         Get_Line_Column_Iter
           (Model     => Locations.Model,
            --  File_Iter belongs to model store
            File_Iter => File_Iter,
            Line      => D.Line,
            Column    => 0,
            Loc_Iter  => Loc_Iter);

         if Loc_Iter /= Null_Iter then
            Locations.Filter.Convert_Child_Iter_To_Iter
              (Filter_Loc_Iter, Loc_Iter);

            if Loc_Iter /= Null_Iter then
               Path := Get_Path (Model, Filter_Loc_Iter);
               Expand_To_Path (Locations.Tree, Path);
               Select_Iter (Get_Selection (Locations.Tree), Filter_Loc_Iter);
               Scroll_To_Cell (Locations.Tree, Path, null, False, 0.1, 0.1);
               Path_Free (Path);
            end if;
         end if;
      end if;
   end Location_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Location_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID)
   is
      Scrolled  : Gtk_Scrolled_Window;

   begin
      Initialize_Vbox (View);

      View.Kernel := Kernel;

      --  Initialize the tree

      Gtk_New (View.Model, Kernel);
      Gtk_New (View.Filter, View.Model);
      Visible_Funcs.Set_Visible_Func
        (View.Filter, Is_Visible'Access, Location_View (View));

      Gtk_New (View.Tree, View.Filter);
      View.Tree.Set_Headers_Visible (False);
      View.Tree.Set_Name ("Locations Tree");

      Set_Column_Types (View);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, View.Tree);

      View.Pack_Start (Scrolled);

      --  Initialize the filter panel

      Gtk_New (View.Filter_Panel, Kernel);
      Location_View_Callbacks.Object_Connect
        (View.Filter_Panel,
         Signal_Apply_Filter,
         Location_View_Callbacks.To_Marshaller (On_Apply_Filter'Access),
         Location_View (View));
      Location_View_Callbacks.Object_Connect
        (View.Filter_Panel,
         Signal_Cancel_Filter,
         Location_View_Callbacks.To_Marshaller (On_Cancel_Filter'Access),
         Location_View (View));
      Location_View_Callbacks.Object_Connect
        (View.Filter_Panel,
         Signal_Visibility_Toggled,
         Location_View_Callbacks.To_Marshaller (On_Visibility_Toggled'Access),
         Location_View (View));
      View.Pack_Start (View.Filter_Panel, False, False);

      Glib.Properties.Set_Property
        (View.Tree, Gtk.Widget.Has_Tooltip_Property, True);
      Query_Tooltip_Callbacks.Connect
        (View.Tree,
         Gtk.Widget.Signal_Query_Tooltip,
         On_Query_Tooltip'Access,
         Location_View (View));

      Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press'Access),
         View,
         After => False);

      Location_View_Callbacks.Object_Connect
        (View.Tree,
         Signal_Row_Expanded, On_Row_Expanded'Access,
         Slot_Object => View,
         After       => True);
      Location_View_Callbacks.Object_Connect
        (View.Tree,
         Signal_Row_Collapsed,
         On_Row_Collapsed'Access,
         View);
      Location_View_Callbacks.Object_Connect
        (View.Filter,
         Signal_Row_Inserted,
         On_Filter_Row_Inserted'Access,
         View);
      Location_View_Callbacks.Object_Connect
        (View.Model,
         Signal_Row_Inserted,
         On_Model_Row_Inserted'Access,
         View);

      Register_Contextual_Menu
        (View.Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Module_ID (Module),
         Context_Func    => Context_Func'Access);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "location_view.preferences_changed",
                Watch => GObject (View));
      Modify_Font (View.Tree, View_Fixed_Font.Get_Pref);

      Add_Hook (Kernel, Location_Changed_Hook,
                Wrapper (Location_Changed'Access),
                Name  => "locations.location_changed",
                Watch => GObject (View));

      Read_Secondary_Pattern_Preferences (View);
   end Initialize;

   ----------------------------
   -- On_Filter_Row_Inserted --
   ----------------------------

   procedure On_Filter_Row_Inserted
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues)
   is
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Warnings (Off, Dummy);

   begin
      Get_Tree_Iter (Nth (Params, 2), Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Self.Filter.Get_Path (Iter);

      if Get_Depth (Path) > 1 then
         Dummy := Up (Path);
         Iter := Self.Filter.Get_Iter (Path);

         if Self.Filter.Get_Boolean (Iter, Expanded_State_Column) then
            Dummy := Expand_Row (Self.Tree, Path, False);

         else
            Dummy := Collapse_Row (Self.Tree, Path);
         end if;
      end if;

      Path_Free (Path);
   end On_Filter_Row_Inserted;

   ---------------------------
   -- On_Model_Row_Inserted --
   ---------------------------

   procedure On_Model_Row_Inserted
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues)
   is
      Iter                   : Gtk_Tree_Iter;
      Category_Iter          : Gtk_Tree_Iter;
      File_Iter              : Gtk_Tree_Iter;
      Message_Iter           : Gtk_Tree_Iter;
      Category_View_Iter     : Gtk_Tree_Iter;
      File_View_Iter         : Gtk_Tree_Iter;
      File_Next_View_Iter    : Gtk_Tree_Iter;
      Message_View_Iter      : Gtk_Tree_Iter;
      Message_Next_View_Iter : Gtk_Tree_Iter;
      Path                   : Gtk_Tree_Path;
      Depth                  : Gint;
      Dummy                  : Boolean;
      pragma Warnings (Off, Dummy);

   begin
      Get_Tree_Iter (Nth (Params, 2), Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Self.Model.Get_Path (Iter);
      Depth := Get_Depth (Path);
      Path_Free (Path);

      if Depth = 3 then
         --  Message row

         Message_Iter := Iter;
         File_Iter := Self.Model.Parent (Message_Iter);
         Category_Iter := Self.Model.Parent (File_Iter);

         Self.Filter.Convert_Child_Iter_To_Iter
           (Message_View_Iter, Message_Iter);

         if Message_View_Iter = Null_Iter then
            --  Message is filtered out

            return;
         end if;

         Message_Next_View_Iter := Message_View_Iter;
         Self.Filter.Next (Message_Next_View_Iter);

         Self.Filter.Convert_Child_Iter_To_Iter (File_View_Iter, File_Iter);
         File_Next_View_Iter := File_View_Iter;
         Self.Filter.Next (File_Next_View_Iter);

         Self.Filter.Convert_Child_Iter_To_Iter
           (Category_View_Iter, Category_Iter);

         if Self.Filter.Children (Category_View_Iter) = File_View_Iter
           and then File_Next_View_Iter = Null_Iter
           and then Self.Filter.Children (File_View_Iter) = Message_View_Iter
           and then Message_Next_View_Iter = Null_Iter
         then
            --  It is a first visible message for the new category. Thus,
            --  we need to expand category and file, select first message,
            --  and go to location of the first message if requested. Because
            --  user expects the first visible message (not a first inserted
            --  message) be selected (and its location be opened in source
            --  editor) the operation is delayed.

            if Self.Expand_Path /= null then
               Path_Free (Self.Expand_Path);
            end if;

            Self.Expand_Path := Self.Filter.Get_Path (Category_View_Iter);

            if Self.Idle_Expand_Handler = No_Source_Id then
               Self.Idle_Expand_Handler :=
                 View_Idle.Idle_Add
                   (Idle_Expand_Category'Access, Location_View (Self));
            end if;
         end if;
      end if;
   end On_Model_Row_Inserted;

   ----------------------
   -- On_Row_Collapsed --
   ----------------------

   procedure On_Row_Collapsed
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues)
   is
      Iter       : Gtk_Tree_Iter;
      Model_Iter : Gtk_Tree_Iter;

   begin
      Get_Tree_Iter (Nth (Params, 1), Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Self.Filter.Convert_Iter_To_Child_Iter (Model_Iter, Iter);
      Self.Model.Set (Model_Iter, Expanded_State_Column, False);
   end On_Row_Collapsed;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (Self   : access Location_View_Record'Class;
      Params : Glib.Values.GValues)
   is
      Iter       : Gtk_Tree_Iter;
      Model_Iter : Gtk_Tree_Iter;

   begin
      Get_Tree_Iter (Nth (Params, 1), Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Self.Filter.Convert_Iter_To_Child_Iter (Model_Iter, Iter);
      Self.Model.Set (Model_Iter, Expanded_State_Column, True);

      if Self.Idle_Row_Handler = Glib.Main.No_Source_Id then
         if Self.Row /= null then
            Path_Free (Self.Row);
         end if;

         Self.Row := Get_Path (Get_Model (Self.Tree), Iter);

         Self.Idle_Row_Handler := View_Idle.Idle_Add
           (Idle_Show_Row'Access, Location_View (Self));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Row_Expanded;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Explorer  : constant Location_View := Location_View (View);
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Success   : Command_Return_Type;
      pragma Unreferenced (Success);

      Cell_Rect : Gdk_Rectangle;
      Back_Rect : Gdk_Rectangle;

   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X), Gint (Y), Path, Column, Buffer_X, Buffer_Y, Row_Found);

         if Column /= Explorer.Action_Column then
            Get_Cell_Area
              (Explorer.Tree, Path,
               Explorer.Sorting_Column, Cell_Rect);
            Get_Background_Area
              (Explorer.Tree, Path,
               Explorer.Sorting_Column, Back_Rect);

            --  If we are clicking before the beginning of the cell, allow the
            --  event to pass. This allows clicking on expanders.

            if Buffer_X > Back_Rect.X
              and then Buffer_X < Cell_Rect.X
            then
               Path_Free (Path);
               return False;
            end if;
         end if;

         if Path /= null then
            if Get_Depth (Path) < 3 then
               Path_Free (Path);
               return False;
            else
               if Column = Explorer.Action_Column then
                  declare
                     Value    : GValue;
                     Iter     : Gtk_Tree_Iter;
                     Aux_Iter : Gtk_Tree_Iter;
                     Action   : Action_Item;

                  begin
                     Aux_Iter := Explorer.Filter.Get_Iter (Path);
                     Explorer.Filter.Convert_Iter_To_Child_Iter
                       (Iter, Aux_Iter);

                     Explorer.Model.Get_Value (Iter, Action_Column, Value);
                     Action := To_Action_Item (Get_Address (Value));

                     if Action /= null
                       and then Action.Associated_Command /= null
                     then
                        Success := Execute (Action.Associated_Command);
                     end if;

                     Unset (Value);
                  end;

                  Select_Path (Get_Selection (Explorer.Tree), Path);

               else
                  Select_Path (Get_Selection (Explorer.Tree), Path);
                  Goto_Location (Location_View (View));
               end if;
            end if;

            Path_Free (Path);
         end if;

         return True;

      else
         Grab_Focus (Explorer.Tree);

         --  If there is no selection, select the item under the cursor

         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X), Gint (Y), Path, Column, Buffer_X, Buffer_Y, Row_Found);

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
         Trace (Exception_Handle, E);
         return False;
   end Button_Press;

   ---------------------------------
   -- Get_Or_Create_Location_View --
   ---------------------------------

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Location_View
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
      Allow_Creation : Boolean := True) return MDI_Child
   is
      Child     : GPS_MDI_Child;
      Locations : Location_View;
   begin
      if Get_MDI (Kernel) = null then
         --  We are destroying everything. This function gets called likely
         --  because a module (code_analysis for instance) tries to cleanup
         --  the locations view after the latter has already been destroyed)
         return null;
      end if;

      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
         (Get_MDI (Kernel), Location_View_Record'Tag));
      if Child = null then
         if not Allow_Creation then
            return null;
         end if;

         Gtk_New (Locations, Kernel_Handle (Kernel),
                  Abstract_Module_ID (Location_View_Module_Id));
         Gtk_New (Child, Locations,
                  Module              => Location_View_Module_Id,
                  Default_Width       => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height      => Gint (Default_Widget_Height.Get_Pref),
                  Group               => Group_Consoles,
                  Desktop_Independent => True);
         Set_Title (Child, -"Locations");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);
         Set_Focus_Child (Child);
      end if;

      return MDI_Child (Child);
   end Get_Or_Create_Location_View_MDI;

   ----------------------------------------
   -- Read_Secondary_Pattern_Preferences --
   ----------------------------------------

   procedure Read_Secondary_Pattern_Preferences
     (View : access Location_View_Record'Class)
   is
   begin
      if View.Secondary_File_Pattern /= null then
         Unchecked_Free (View.Secondary_File_Pattern);
      end if;

      View.Secondary_File_Pattern := new Pattern_Matcher'
        (Compile (Secondary_File_Pattern.Get_Pref));
      View.SFF := Secondary_File_Pattern_Index.Get_Pref;
      View.SFL := Secondary_Line_Pattern_Index.Get_Pref;
      View.SFC := Secondary_Column_Pattern_Index.Get_Pref;
   end Read_Secondary_Pattern_Preferences;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View              : Location_View;
      Child             : MDI_Child;
      Node, Sub         : Location_List.List_Node;
      Loc               : Location_Record;
      Iter, Parent_Iter : Gtk_Tree_Iter := Null_Iter;
      Aux_Iter          : Gtk_Tree_Iter;
      Appended          : Boolean;
      Path              : Gtk_Tree_Path;

   begin
      Child := Get_Or_Create_Location_View_MDI
        (Kernel, Allow_Creation => False);

      if Child = null then
         return;
      end if;

      View := Location_View (Get_Widget (Child));
      Read_Secondary_Pattern_Preferences (View);

      Modify_Font (View.Tree, View_Fixed_Font.Get_Pref);
      Node := First (View.Stored_Locations);

      while Node /= Location_List.Null_Node loop
         Loc := Data (Node).all;
         Add_Location
           (View               => View,
            Category           => Loc.Category.all,
            File               => Loc.File,
            Line               => Loc.Line,
            Column             => Loc.Column,
            Length             => Loc.Length,
            Highlight          => Loc.Highlight,
            Message            => Loc.Message.all,
            Highlight_Category => Get_Or_Create_Style
              (Kernel_Handle (Kernel), Loc.Highlight_Category.all, False),
            Remove_Duplicates  => False,
            Sort_In_File       => False,
            Parent_Iter        => Parent_Iter,
            Look_For_Secondary => False);

         Sub := First (Loc.Children);

         Appended := Sub /= Location_List.Null_Node;

         while Sub /= Location_List.Null_Node loop
            View.Model.Append (Iter, Parent_Iter);

            Loc := Data (Sub).all;

            Fill_Iter
              (View.Model, Iter, " ",
               Loc.File, Loc.Message.all,
               Create_Mark (View.Kernel, Loc.File, Loc.Line, Loc.Column),
               Loc.Line, Loc.Column, Loc.Length, False,
               Get_Or_Create_Style
                 (Kernel_Handle (Kernel), Loc.Highlight_Category.all));

            Sub := Next (Sub);
         end loop;

         if Appended then
            View.Filter.Convert_Child_Iter_To_Iter (Aux_Iter, Parent_Iter);
            Path := View.Filter.Get_Path (Aux_Iter);
            Appended := Expand_Row (View.Tree, Path, False);
            Path_Free (Path);
         end if;

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
      Child                    : MDI_Child;
      View                     : Location_View;
      Category, File, Location : Node_Ptr;

      procedure Read_Location
        (Iter     : Node_Ptr;
         L        : in out Location_List.List;
         Category : String);
      --  Read a file location recursively

      -------------------
      -- Read_Location --
      -------------------

      procedure Read_Location
        (Iter     : Node_Ptr;
         L        : in out Location_List.List;
         Category : String)
      is
         N   : Node_Ptr;
         Loc : Location_Record_Access;
      begin
         if Iter.Tag.all /= "Location" then
            return;
         end if;

         Loc := new Location_Record'
           (Category => new String'(Category),
            File     => Get_File_Child (Iter, "file"),
            Line     => Integer'Value (Get_Attribute (Iter, "line", "0")),
            Column   => Visible_Column_Type'Value
              (Get_Attribute (Iter, "column", "0")),
            Length   => Integer'Value
              (Get_Attribute (Iter, "length", "0")),
            Highlight => True or else Boolean'Value
              (Get_Attribute (Iter, "highlight", "False")),
            Message  => new String'
              (Get_Attribute (Iter, "message", "")),
            Highlight_Category => new String'
              (Get_Attribute (Iter, "category", "")),
            Children => Location_List.Null_List);
         Append (L, Loc);

         N := Iter.Child;
         while N /= null loop
            Read_Location (N, Loc.Children, Category);
            N := N.Next;
         end loop;
      end Read_Location;

   begin
      if Node.Tag.all = "Location_View_Record" then
         Child := Get_Or_Create_Location_View_MDI
           (User, Allow_Creation => True);
         View := Location_View (Get_Widget (Child));
         Category := Node.Child;

         if Boolean'Value (Get_Attribute (Node, "filter_panel", "FALSE")) then
            View.Filter_Panel.Show;

         else
            View.Filter_Panel.Hide;
         end if;

         while Category /= null loop
            declare
               Category_Name : constant String :=
                                 Get_Attribute (Category, "name");
            begin
               --  Do not load errors in the project file back, since in fact
               --  they have already been overwritten if required when the
               --  project was loaded before the desktop
               if Category_Name /= "Project" then
                  File := Category.Child;
                  while File /= null loop
                     Location := File.Child;

                     while Location /= null loop
                        Read_Location
                          (Location, View.Stored_Locations, Category_Name);
                        Location := Location.Next;
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
      N              : Node_Ptr;
      View           : Location_View;
      Category, File : Node_Ptr;
      Category_Iter  : Gtk_Tree_Iter;
      File_Iter      : Gtk_Tree_Iter;
      Location_Iter  : Gtk_Tree_Iter;

      procedure Add_Location_Iter
        (Iter   : Gtk_Tree_Iter;
         Parent : Node_Ptr);
      --  Add the location and children recursively

      -----------------------
      -- Add_Location_Iter --
      -----------------------

      procedure Add_Location_Iter
        (Iter   : Gtk_Tree_Iter;
         Parent : Node_Ptr)
      is
         Child  : Gtk_Tree_Iter;
         Loc    : Node_Ptr;
         Value  : GValue;
         Action : Action_Item;
      begin
         Loc := new Node;
         Loc.Tag := new String'("Location");
         Add_Child (Parent, Loc, True);
         Add_File_Child (Loc, "file", Get_File (View.Model, Iter));
         Set_Attribute (Loc, "line",
           Gint'Image (View.Model.Get_Int (Iter, Line_Column)));
         Set_Attribute
           (Loc, "column",
            Gint'Image (View.Model.Get_Int (Iter, Column_Column)));
         Set_Attribute
           (Loc, "length",
            Gint'Image (View.Model.Get_Int (Iter, Length_Column)));
         Set_Attribute (Loc, "message", Get_Message (View.Model, Iter));
         Set_Attribute
           (Loc, "category",
            Get_Name (Get_Highlighting_Style (View.Model, Iter)));
         Set_Attribute
           (Loc, "highlight",
            Boolean'Image
              (View.Model.Get_Boolean
                 (Iter, GPS.Location_Model.Highlight_Column)));

         View.Model.Get_Value (Iter, Action_Column, Value);
         Action := To_Action_Item (Get_Address (Value));

         if Action /= null then
            Set_Attribute (Loc, "has_action", "true");
         end if;

         Unset (Value);

         Child := View.Model.Children (Iter);

         while Child /= Null_Iter loop
            Add_Location_Iter (Child, Loc);
            View.Model.Next (Child);
         end loop;
      end Add_Location_Iter;

   begin
      if Widget.all in Location_View_Record'Class then
         View := Location_View (Widget);

         N := new Node;
         N.Tag := new String'("Location_View_Record");

         if View.Filter_Panel.Mapped_Is_Set then
            Set_Attribute (N, "filter_panel", "TRUE");
         end if;

         Category_Iter := View.Model.Get_Iter_First;

         while Category_Iter /= Null_Iter loop
            Category := new Node;
            Category.Tag := new String'("Category");
            Set_Attribute
              (Category, "name",
               Get_Category_Name (View.Model, Category_Iter));
            Add_Child (N, Category, True);

            File_Iter := View.Model.Children (Category_Iter);

            while File_Iter /= Null_Iter loop
               File := new Node;
               File.Tag := new String'("File");
               Add_Child (Category, File, True);

               Add_File_Child
                 (File, "name", Get_File (View.Model, File_Iter));

               Location_Iter := View.Model.Children (File_Iter);

               while Location_Iter /= Null_Iter loop
                  Add_Location_Iter (Location_Iter, File);
                  View.Model.Next (Location_Iter);
               end loop;

               View.Model.Next (File_Iter);
            end loop;

            View.Model.Next (Category_Iter);
         end loop;

         return N;
      end if;

      return null;
   end Save_Desktop;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Clear_Locations_View_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Location_View :=
               Get_Or_Create_Location_View
                 (Get_Kernel (Context.Context), False);
      Iter : Gtk_Tree_Iter;
   begin
      loop
         Iter := View.Model.Get_Iter_First;
         exit when Iter = Null_Iter;
         Remove_Category_Or_File_Iter (View.Kernel, View.Model, Iter);
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
      Command     : Interactive_Command_Access;
   begin
      Location_View_Module_Id := new Location_View_Module;
      Register_Module
        (Module      => Location_View_Module_Id,
         Kernel      => Kernel,
         Module_Name => Module_Name);

      GPS.Kernel.Locations.Register (Kernel);
      GPS.Kernel.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Clear_Locations_View_Command;
      Register_Contextual_Menu
        (Kernel,
         Name   => "Clear Locations View",
         Label  => -"Clear Locations View",
         Filter => Create (Module => Module_Name),
         Action => Command);
   end Register_Module;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Locations_Class : constant Class_Type := New_Class (Kernel, "Locations");
   begin
      Register_Command
        (Kernel, "parse",
         Minimum_Args  => 2,
         Maximum_Args  => Parse_Location_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args  => Locations_Add_Parameters'Length - 3,
         Maximum_Args  => Locations_Add_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_category",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_categories",
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_locations",
         Class         => Locations_Class,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "dump",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
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
            Kernel             : constant Kernel_Handle := Get_Kernel (Data);
            Highlight_Category : constant String :=
                                   Nth_Arg (Data, 10, "Builder results");
            Style_Category     : constant String :=
                                   Nth_Arg (Data, 11, "Style errors");
            Warning_Category   : constant String :=
                                   Nth_Arg (Data, 12, "Builder warnings");
            S                  : GNAT.Strings.String_Access;
            Output             : Unchecked_String_Access;
            Len                : Natural;
            Valid              : Boolean;

         begin
            S := new String'(Nth_Arg (Data, 1));
            Unknown_To_UTF8 (S.all, Output, Len, Valid);

            if not Valid then
               Console.Insert
                 (Kernel,
                  -"Locations.parse: could not convert input to UTF8",
                  Mode => Console.Error);

            else
               if Output = null then
                  Parse_File_Locations
                    (Get_Kernel (Data),
                     Highlight               => Highlight_Category /= ""
                        or else Style_Category /= ""
                        or else Warning_Category /= "",
                     Text                    => S.all,
                     Category                => Nth_Arg (Data, 2),
                     Highlight_Category      =>
                       Get_Or_Create_Style (Kernel, Highlight_Category, False),
                     Style_Category          =>
                       Get_Or_Create_Style (Kernel, Style_Category, False),
                     Warning_Category        =>
                       Get_Or_Create_Style (Kernel, Warning_Category, False),
                     File_Location_Regexp    => Nth_Arg (Data, 3, ""),
                     File_Index_In_Regexp    => Nth_Arg (Data, 4, -1),
                     Line_Index_In_Regexp    => Nth_Arg (Data, 5, -1),
                     Col_Index_In_Regexp     => Nth_Arg (Data, 6, -1),
                     Msg_Index_In_Regexp     => Nth_Arg (Data, 7, -1),
                     Style_Index_In_Regexp   => Nth_Arg (Data, 8, -1),
                     Warning_Index_In_Regexp => Nth_Arg (Data, 9, -1),
                     Remove_Duplicates       => True);

               else
                  Parse_File_Locations
                    (Get_Kernel (Data),
                     Highlight               => Highlight_Category /= ""
                        or else Style_Category /= ""
                        or else Warning_Category /= "",
                     Text                    => Output (1 .. Len),
                     Category                => Nth_Arg (Data, 2),
                     Highlight_Category      =>
                       Get_Or_Create_Style (Kernel, Highlight_Category, False),
                     Style_Category          =>
                       Get_Or_Create_Style (Kernel, Style_Category, False),
                     Warning_Category        =>
                       Get_Or_Create_Style (Kernel, Warning_Category, False),
                     File_Location_Regexp    => Nth_Arg (Data, 3, ""),
                     File_Index_In_Regexp    => Nth_Arg (Data, 4, -1),
                     Line_Index_In_Regexp    => Nth_Arg (Data, 5, -1),
                     Col_Index_In_Regexp     => Nth_Arg (Data, 6, -1),
                     Msg_Index_In_Regexp     => Nth_Arg (Data, 7, -1),
                     Style_Index_In_Regexp   => Nth_Arg (Data, 8, -1),
                     Warning_Index_In_Regexp => Nth_Arg (Data, 9, -1),
                     Remove_Duplicates       => True);
                  Free (Output);
               end if;
            end if;

            GNAT.Strings.Free (S);
         end;

      elsif Command = "remove_category" then
         Name_Parameters (Data, Remove_Category_Parameters);
         Remove_Location_Category
           (Get_Kernel (Data),
            Category => Nth_Arg (Data, 1));

      elsif Command = "list_categories" then
         declare
            View : constant Location_View := Get_Or_Create_Location_View
              (Get_Kernel (Data), Allow_Creation => False);
            Iter : Gtk_Tree_Iter;

         begin
            Set_Return_Value_As_List (Data);
            if View /= null then
               Iter := View.Model.Get_Iter_First;

               while Iter /= Null_Iter loop
                  Set_Return_Value
                    (Data, Get_Category_Name (View.Model, Iter));
                  View.Model.Next (Iter);
               end loop;
            end if;
         end;

      elsif Command = "list_locations" then
         declare
            View      : constant Location_View := Get_Or_Create_Location_View
              (Get_Kernel (Data), Allow_Creation => False);
            Iter, Dummy : Gtk_Tree_Iter;
            Dummy_B   : Boolean;
            Script    : constant Scripting_Language := Get_Script (Data);
            Category  : constant String := Nth_Arg (Data, 1);
            File      : constant GNATCOLL.VFS.Virtual_File := Create
              (Nth_Arg (Data, 2), Get_Kernel (Data), Use_Source_Path => True);
            Line, Col : Gint;
         begin
            Set_Return_Value_As_List (Data);

            if View /= null then
               Get_Category_File
                 (Model         => View.Model,
                  Category      => Category,
                  File          => File,
                  Category_Iter => Dummy,
                  File_Iter     => Iter,
                  New_Category  => Dummy_B,
                  Create        => False);

               if Iter /= Null_Iter then
                  Iter := View.Model.Children (Iter);
               end if;

               while Iter /= Null_Iter loop
                  Line := View.Model.Get_Int (Iter, Line_Column);
                  Col  := View.Model.Get_Int (Iter, Column_Column);
                  Set_Return_Value
                    (Data,
                     Create_File_Location
                       (Script => Script,
                        File   => Create_File (Script, File),
                        Line   => Integer (Line),
                        Column => Visible_Column_Type (Col)));
                  Set_Return_Value
                    (Data, View.Model.Get_String (Iter, Base_Name_Column));
                  View.Model.Next (Iter);
               end loop;
            end if;
         end;

      elsif Command = "add" then
         Name_Parameters (Data, Locations_Add_Parameters);
         declare
            Highlight : constant String  := Nth_Arg (Data, 6, "");
         begin
            Insert_Location
              (Get_Kernel (Data),
               Category           => Nth_Arg (Data, 1),
               File               => Get_Data
                 (Nth_Arg (Data, 2, (Get_File_Class (Get_Kernel (Data))))),
               Line               => Nth_Arg (Data, 3),
               Column             => Visible_Column_Type
                 (Nth_Arg (Data, 4, Default => 1)),
               Text               => Nth_Arg (Data, 5),
               Length             => Nth_Arg (Data, 7, 0),
               Highlight          => Highlight /= "",
               Highlight_Category => Get_Or_Create_Style
                 (Get_Kernel (Data), Highlight, False),
               Quiet              => True,
               Sort_In_File       => True,
               Look_For_Secondary => Nth_Arg (Data, 8, False));
         end;

      elsif Command = "dump" then
         Name_Parameters (Data, Locations_Add_Parameters);
         Dump_To_File (Get_Kernel (Data), Create (Nth_Arg (Data, 1)));
      end if;
   end Default_Command_Handler;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      View    : constant Location_View :=
                  Get_Or_Create_Location_View (Kernel);
      N       : Node_Ptr;
      Success : Boolean;

   begin
      N := Save_Desktop (View, Kernel_Handle (Kernel));
      Print (N, File, Success);
      Free (N);

      if not Success then
         Report_Preference_File_Error (Kernel, File);
      end if;
   end Dump_To_File;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Location_Record_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Location_Record, Location_Record_Access);

      use GNAT.Strings;

   begin
      Free (X.Category);
      Free (X.Message);
      Free (X.Highlight_Category);
      Free (X.Children);
      Unchecked_Free (X);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Location) is
      use GNAT.Strings;
   begin
      Free (X.Message);
   end Free;

   -----------------------
   -- Extract_Locations --
   -----------------------

   function Extract_Locations
     (View    : access Location_View_Record'Class;
      Message : String) return Locations_List.List
   is
      Result  : Locations_List.List;
      Matched : Match_Array (0 .. 9);
      Loc     : Location;
      Start   : Natural := Message'First;
   begin
      while Start <= Message'Last loop
         Match
           (View.Secondary_File_Pattern.all,
            Message (Start .. Message'Last), Matched);

         exit when Matched (0) = No_Match;

         Loc.File := Create
           (+Message (Matched (View.SFF).First .. Matched (View.SFF).Last),
            View.Kernel);

         Loc.Message := new String'
           (Message (Message'First .. Matched (0).First - 1)
            & "<span color=""blue""><u>"
            & Message (Matched (0).First .. Matched (0).Last)
            & "</u></span>"
            & Message (Matched (0).Last + 1 .. Message'Last));

         if Matched (View.SFL) /= No_Match then
            declare
               Val : constant Integer := Safe_Value
                (Message (Matched (View.SFL).First ..
                    Matched (View.SFL).Last), 1);
            begin
               if Val >= 1 then
                  Loc.Line := Val;
               else
                  Loc.Line := 1;
               end if;
            end;
         end if;

         if Matched (View.SFC) /= No_Match then
            declare
               Val : constant Integer := Safe_Value
                 (Message (Matched (View.SFF).First ..
                    Matched (View.SFF).Last), 1);
            begin
               if Val >= 1 then
                  Loc.Column :=  Visible_Column_Type (Val);
               else
                  Loc.Column := 1;
               end if;
            end;
         end if;

         Append (Result, Loc);
         Loc := (No_File, 1, 1, null);
         Start := Matched (1).Last + 1;
      end loop;

      return Result;
   end Extract_Locations;

   ---------------------
   -- On_Apply_Filter --
   ---------------------

   procedure On_Apply_Filter (Self : access Location_View_Record'Class) is
      Pattern     : constant String := Self.Filter_Panel.Get_Pattern;
      New_Reg_Exp : GNAT.Expect.Pattern_Matcher_Access;
      New_Text    : GNAT.Strings.String_Access;

   begin
      if Pattern /= "" then
         if Self.Filter_Panel.Get_Is_Reg_Exp then
            New_Reg_Exp :=
              new GNAT.Regpat.Pattern_Matcher'(GNAT.Regpat.Compile (Pattern));

         else
            New_Text := new String'(Pattern);
         end if;
      end if;

      Basic_Types.Unchecked_Free (Self.RegExp);
      GNAT.Strings.Free (Self.Text);

      Self.RegExp := New_Reg_Exp;
      Self.Text := New_Text;
      Self.Is_Hide := Self.Filter_Panel.Get_Hide_Matched;

      Self.Filter.Refilter;

      Get_Or_Create_Location_View_MDI (Self.Kernel).Set_Title
        (+"Locations (filtered)");
   end On_Apply_Filter;

   ----------------------
   -- On_Cancel_Filter --
   ----------------------

   procedure On_Cancel_Filter (Self : access Location_View_Record'Class) is
   begin
      Basic_Types.Unchecked_Free (Self.RegExp);
      GNAT.Strings.Free (Self.Text);

      Self.Filter.Refilter;

      Get_Or_Create_Location_View_MDI (Self.Kernel).Set_Title (+"Locations");
   end On_Cancel_Filter;

   -------------------------------
   -- On_Filter_Panel_Activated --
   -------------------------------

   procedure On_Filter_Panel_Activated
     (Widget : access Gtk_Widget_Record'Class)
   is
      Self : constant Location_View := Location_View (Widget);

   begin
      if Self.Filter_Panel.Mapped_Is_Set then
         Self.Filter_Panel.Hide;

      else
         Self.Filter_Panel.Show;
      end if;
   end On_Filter_Panel_Activated;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   function On_Query_Tooltip
     (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Params : Glib.Values.GValues;
      Self   : Location_View)
      return Boolean
   is
      pragma Unreferenced (Object);

      X             : Glib.Gint :=
                        Glib.Values.Get_Int (Glib.Values.Nth (Params, 1));
      Y             : Glib.Gint :=
                        Glib.Values.Get_Int (Glib.Values.Nth (Params, 2));
      Keyboard_Mode : constant Boolean :=
                        Glib.Values.Get_Boolean (Glib.Values.Nth (Params, 3));
      Stub          : Gtk.Tooltips.Gtk_Tooltips_Record;
      Tooltip       : constant Gtk.Tooltips.Gtk_Tooltips :=
                        Gtk.Tooltips.Gtk_Tooltips
                          (Glib.Object.Get_User_Data
                             (Glib.Values.Get_Address
                                (Glib.Values.Nth (Params, 4)), Stub));
      Success       : Boolean;
      Model         : Gtk.Tree_Model.Gtk_Tree_Model;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Rect          : Gdk.Rectangle.Gdk_Rectangle;
      X_Offset      : Glib.Gint;
      Y_Offset      : Glib.Gint;
      Start         : Glib.Gint;
      Width         : Glib.Gint;
      Height        : Glib.Gint;
      X1            : Glib.Gint;
      X2            : Glib.Gint;

   begin
      Self.Tree.Get_Tooltip_Context
        (X, Y, Keyboard_Mode, Model, Path, Iter, Success);

      if not Success then
         Gtk.Tree_Model.Path_Free (Path);

         return False;
      end if;

      Self.Sorting_Column.Cell_Set_Cell_Data (Model, Iter, False, False);

      Self.Tree.Get_Cell_Area (Path, Self.Sorting_Column, Rect);
      X1 := Rect.X;
      X2 := Rect.X;

      Self.Sorting_Column.Cell_Get_Position
        (Self.Text_Renderer, Start, Width, Success);

      if not Success then
         Gtk.Tree_Model.Path_Free (Path);

         return False;
      end if;

      X2 := X2 + Start;

      Self.Text_Renderer.Get_Size
        (Self.Tree, Rect, X_Offset, Y_Offset, Width, Height);
      X2 := X2 + Width;

      Self.Tree.Get_Visible_Rect (Rect);

      if X1 > Rect.X and X2 < (Rect.X + Rect.Width) then
         Gtk.Tree_Model.Path_Free (Path);

         return False;
      end if;

      Tooltip.Set_Markup (Model.Get_String (Iter, Base_Name_Column));
      Self.Tree.Set_Tooltip_Row (Tooltip, Path);

      Gtk.Tree_Model.Path_Free (Path);

      return True;
   end On_Query_Tooltip;

   ---------------------------
   -- On_Visibility_Toggled --
   ---------------------------

   procedure On_Visibility_Toggled
     (Self : access Location_View_Record'Class) is
   begin
      Self.Is_Hide := Self.Filter_Panel.Get_Hide_Matched;

      Self.Filter.Refilter;
   end On_Visibility_Toggled;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self  : Location_View) return Boolean
   is
      use type GNAT.Strings.String_Access;

      Message : constant String := Get_Message (Model, Iter);
      Depth   : Natural := 0;
      Path    : Gtk_Tree_Path;
      Found   : Boolean;

   begin
      Path := Model.Get_Path (Iter);
      Depth := Natural (Get_Depth (Path));
      Path_Free (Path);

      if Depth < 3 then
         return True;

      else
         if Self.RegExp /= null then
            Found := GNAT.Regpat.Match (Self.RegExp.all, Message);

         elsif Self.Text /= null then
            Found := Ada.Strings.Fixed.Index (Message, Self.Text.all) /= 0;

         else
            return True;
         end if;

         if Self.Is_Hide then
            Found := not Found;
         end if;

         return Found;
      end if;
   end Is_Visible;

   -----------
   -- Model --
   -----------

   function Model
     (Self : not null access Location_View_Record'Class)
      return not null GPS.Location_Model.Location_Model is
   begin
      return Self.Model;
   end Model;

end GPS.Location_View;
