-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--               Copyright (C) 2005-2007, AdaCore                    --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                    use System;

with GNAT.OS_Lib;
with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Strings;              use GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Values;               use Glib.Values;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Generic_Views;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Generic_List;
with Traces;                    use Traces;
with Tooltips;
with XML_Parsers;               use XML_Parsers;

package body Bookmark_Views is

   Me : constant Debug_Handle := Create ("Bookmarks");

   Icon_Column     : constant := 0;
   Name_Column     : constant := 1;
   Data_Column     : constant := 2;
   Editable_Column : constant := 3;

   type Bookmark_Data is record
      Marker    : Location_Marker;
      Name      : GNAT.Strings.String_Access;
      Instances : Instance_List;
   end record;
   type Bookmark_Data_Access is access Bookmark_Data;

   procedure Free (Data : in out Bookmark_Data_Access);
   package Bookmark_List is new Generic_List (Bookmark_Data_Access, Free);
   use Bookmark_List;

   type Bookmark_Views_Module_Record is new Module_ID_Record with record
      List : Bookmark_List.List;
   end record;
   type Bookmark_Views_Module_Access
     is access all Bookmark_Views_Module_Record'Class;

   Bookmark_Views_Module : Bookmark_Views_Module_Access;

   type Bookmark_View_Record is new Generic_Views.View_Record with record
      Tree      : Gtk_Tree_View;
      Kernel    : Kernel_Handle;
      Goto_Icon : Gdk_Pixbuf;
      Deleting  : Boolean := False;
      --  Whether we are deleting multiple bookmarks.
   end record;
   type Bookmark_View is access all Bookmark_View_Record'Class;

   package Bookmarks_Selection_Foreach is new
     Selection_Foreach (Bookmark_View_Record);
   use Bookmarks_Selection_Foreach;

   procedure Initialize
     (View   : access Bookmark_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new Bookmark view

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Bookmark_View",
      View_Name          => "Bookmarks",
      Formal_View_Record => Bookmark_View_Record);
   subtype Bookmark_View_Access is Generic_View.View_Access;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Bookmark_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Bookmark_Data_Access, System.Address);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Marker_Record'Class, Location_Marker);

   procedure Refresh (View : access Bookmark_View_Record'Class);
   --  Refresh the contents of the Bookmark view

   procedure Delete_Bookmark
     (Kernel   : access Kernel_Handle_Record'Class;
      Bookmark : Bookmark_Data);
   --  Delete an existing bookmark

   function Bookmark_From_Name
     (Name : String) return Bookmark_List.List_Node;
   --  Return the location marker for the first bookmark named Name.
   --  null is returned if not found

   function Instance_From_Bookmark
     (List   : Bookmark_List.List_Node;
      Class  : Class_Type;
      Script : access Scripting_Language_Record'Class) return Class_Instance;
   --  Get or create the class_instance associated with the bookmark described
   --  in List.

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Context factory when creating contextual menus

   procedure Load_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   --  Load or save the bookmarks from the XML file

   procedure Edited_Callback
     (V      : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a line is edited in the view

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for this module

   package Bookmark_Idle is new Gtk.Main.Idle (Bookmark_View_Access);
   use Bookmark_Idle;
   function Start_Editing_Idle (View : Bookmark_View_Access) return Boolean;
   --  Function called to start editing the selected line. This is necessary
   --  since any editing is stopped as soon as the tree gains the focus back.

   type Refresh_Hook is new Function_With_Args with record
      View : Bookmark_View;
   end record;
   type Refresh_Hook_Access is access all Refresh_Hook'Class;
   procedure Execute
     (Func   : Refresh_Hook;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Function called when a hook has been added or removed, so that we can
   --  properly refresh the view.

   type Delete_Bookmark_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Delete_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Delete the selected bookmark

   type Create_Bookmark_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Create_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Create a new bookmark

   type Rename_Bookmark_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Rename_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Rename the selected bookmark

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Called when the bookmark view is destroyed

   procedure On_Model_Row_Changed (View : access Gtk_Widget_Record'Class);
   --  Called when the treeview model is changed

   --------------
   -- Tooltips --
   --------------

   type Bookmark_View_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Bookmark_View : Bookmark_View_Access;
   end record;
   type Bookmark_View_Tooltips_Access is
     access all Bookmark_View_Tooltips'Class;
   procedure Draw
     (Tooltip : access Bookmark_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access Bookmark_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Model      : constant Gtk_Tree_Model :=
                     Get_Model (Tooltip.Bookmark_View.Tree);

      Window     : Gdk.Window.Gdk_Window;
      New_Window : Gdk.Window.Gdk_Window;
      Mask       : Gdk_Modifier_Type;
      X, Y       : Gint;
      Path       : Gtk_Tree_Path;
      Column     : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y     : Gint;
      Row_Found  : Boolean := False;
      Iter       : Gtk_Tree_Iter;
      Selected   : Integer;
      pragma Unreferenced (Selected);

      Text       : GNAT.Strings.String_Access;
      Data       : Bookmark_Data_Access;
   begin
      Pixmap := null;
      Area   := (0, 0, 0, 0);

      Window := Get_Bin_Window (Tooltip.Bookmark_View.Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);

      Get_Path_At_Pos
        (Tooltip.Bookmark_View.Tree, X, Y, Path,
         Column, Cell_X, Cell_Y, Row_Found);

      if not Row_Found then
         return;
      end if;

      Get_Cell_Area (Tooltip.Bookmark_View.Tree, Path, Column, Area);
      Iter := Get_Iter (Model, Path);
      Path_Free (Path);
      Selected := Integer (Get_Int (Model, Iter, 1));

      Data := Convert (Get_Address (Model, Iter, Data_Column));

      declare
         Location : constant String := To_String (Data.Marker);
      begin
         if Location = Data.Name.all then
            Text := new String'("Location: " & Location);
         else
            Text := new String'
              ("Name: " & Data.Name.all & ASCII.LF & "Location: " & Location);
         end if;
      end;

      Create_Pixmap_From_Text
        (Text.all,
         Get_Pref (Default_Font),
         Get_Pref (Tooltip_Color),
         Tooltip.Bookmark_View.Tree,
         Pixmap);
      Free (Text);
   end Draw;

   ---------------------
   -- Delete_Bookmark --
   ---------------------

   procedure Delete_Bookmark
     (Kernel   : access Kernel_Handle_Record'Class;
      Bookmark : Bookmark_Data)
   is
      Node, Prev  : List_Node;
   begin
      Node := First (Bookmark_Views_Module.List);
      while Node /= Null_Node loop
         --  Compare pointers to string directly. If they are the same, the
         --  pointers are the same anyway
         if Bookmark_List.Data (Node).Name = Bookmark.Name then
            declare
               Name : constant String := Bookmark_List.Data (Node).Name.all;
            begin
               Remove_Nodes (Bookmark_Views_Module.List, Prev, Node);
               Run_String_Hook (Kernel, Bookmark_Removed_Hook, Name);
               exit;
            end;
         end if;

         Prev := Node;
         Node := Next (Node);
      end loop;

      Save_Bookmarks (Kernel);
   end Delete_Bookmark;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Delete_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Bookmark_View_Access :=
                Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Model : constant Gtk_Tree_Model := Get_Model (View.Tree);
      Data  : Bookmark_Data_Access;
      Iter  : Gtk_Tree_Iter;
   begin
      Iter := Get_Iter_First (Model);
      View.Deleting := True;

      while Iter /= Null_Iter loop
         if Iter_Is_Selected (Get_Selection (View.Tree), Iter) then
            Data := Convert (Get_Address (Model, Iter, Data_Column));

            if Data /= null then
               Delete_Bookmark (Get_Kernel (Context.Context), Data.all);
               Remove (Gtk_Tree_Store (Model), Iter);
               Iter := Get_Iter_First (Model);
            else
               Next (Model, Iter);
            end if;
         else
            Next (Model, Iter);
         end if;
      end loop;

      View.Deleting := False;
      Refresh (View);
      return Success;
   end Execute;

   ------------------------
   -- Start_Editing_Idle --
   ------------------------

   function Start_Editing_Idle (View : Bookmark_View_Access) return Boolean is
      procedure Edit_Selected
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Data_Type_Access);
      --  Foreach callback on a selection

      -------------------
      -- Edit_Selected --
      -------------------

      procedure Edit_Selected
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Data_Type_Access) is
         pragma Unreferenced (Model, Data);
      begin
         if Iter /= Null_Iter then
            Set_Cursor
              (View.Tree,
               Path          => Path,
               Focus_Column  => Get_Column (View.Tree, 2),
               Start_Editing => True);
         end if;
      exception
         when E : others => Trace (Exception_Handle, E);
      end Edit_Selected;

   begin
      Selected_Foreach
        (Get_Selection (View.Tree), Edit_Selected'Unrestricted_Access,
         Data_Type_Access (View));
      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Start_Editing_Idle;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Rename_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      View  : constant Bookmark_View_Access :=
                Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      Iter  : Gtk_Tree_Iter;
      Id    : Idle_Handler_Id;
      pragma Unreferenced (Command, Id);
   begin
      if Context.Event /= null then
         Iter := Find_Iter_For_Event (View.Tree, Model, Context.Event);
         if Iter /= Null_Iter then
            Unselect_All (Get_Selection (View.Tree));
            Select_Iter (Get_Selection (View.Tree), Iter);

            --  Start the edition in idle mode, since otherwise the tree gains
            --  the focus when the menu is hidden, and stops the edition
            --  immediately.

            Id := Add (Start_Editing_Idle'Access, View,
                       Priority => Priority_High_Idle);
            return Success;
         end if;
      end if;
      return Failure;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Bookmark_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Bookmark_Data, Bookmark_Data_Access);
   begin
      Destroy (Data.Marker.all);
      Free (Data.Name);
      Unchecked_Free (Data.Marker);
      Unchecked_Free (Data);
   end Free;

   --------------------------
   -- View_Context_Factory --
   --------------------------

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Kernel, Event_Widget, Menu, Context);
      --  Nothing special in the context, just the module itself so that people
      --  can still add information if needed
      V     : constant Bookmark_View_Access := Bookmark_View_Access (Object);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Iter  : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);

      if Iter /= Null_Iter then
         Select_Iter (Get_Selection (V.Tree), Iter);
      end if;
   end View_Context_Factory;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      View   : constant Bookmark_View_Access := Bookmark_View_Access (Clip);
      Path   : Gtk_Tree_Path;
      Model  : constant Gtk_Tree_Store :=
                 Gtk_Tree_Store (Get_Model (View.Tree));
      Iter   : Gtk_Tree_Iter;
      Marker : Bookmark_Data_Access;
      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      if (Get_State (Event) and (Control_Mask or Shift_Mask)) /= 0 then
         --  If there is a ctrl or shift key modifier present, grab the focus
         --  on the tree so that ctrl-clicking and shift-clicking extend the
         --  multiple selection as expected.
         Grab_Focus (View.Tree);

      elsif Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (View.Tree, Model, Event);

         if Iter /= Null_Iter then
            --  Select the row that was clicked.
            Path := Get_Path (Model, Iter);
            Set_Cursor (View.Tree, Path, null, False);
            Path_Free (Path);

            Marker := Convert (Get_Address (Model, Iter, Data_Column));

            if Marker /= null then
               Result := Go_To (Marker.Marker, View.Kernel);
               --  Return True here to prevent focus from flickering between
               --  editor and bookmark view.
               return True;
            end if;
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Create_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Mark   : constant Location_Marker := Create_Marker (Kernel);
      Child  : constant MDI_Child :=
                 Find_MDI_Child_By_Tag
                   (Get_MDI (Kernel), Bookmark_View_Record'Tag);
      View   : Bookmark_View_Access;
      Model  : Gtk_Tree_Store;
      Iter   : Gtk_Tree_Iter;
      Id     : Idle_Handler_Id;
      pragma Unreferenced (Id);
   begin
      if Mark /= null then
         Append (Bookmark_Views_Module.List,
                 new Bookmark_Data'
                   (Marker => Mark,
                    Name   => new String'(To_String (Mark)),
                    Instances => Null_Instance_List));

         if Child = null then
            View := Generic_View.Get_Or_Create_View (Kernel);
         else
            View := Bookmark_View_Access (Get_Widget (Child));
         end if;

         Model := Gtk_Tree_Store (Get_Model (View.Tree));
         Refresh (View);
         Run_String_Hook (Kernel, Bookmark_Added_Hook, To_String (Mark));

         --  Start editing the name of the bookmark immediately

         Iter := Get_Iter_First (Model);
         while Iter /= Null_Iter loop
            if Convert (Get_Address (Model, Iter, Data_Column)).Marker =
              Mark
            then
               Unselect_All (Get_Selection (View.Tree));
               Select_Iter (Get_Selection (View.Tree), Iter);

               exit;
            end if;
            Next (Get_Model (View.Tree), Iter);
         end loop;

         --  Register a callback for editing the selected node

         Id := Add
           (Start_Editing_Idle'Access, View, Priority => Priority_Low_Idle);
         return Success;
      end if;
      return Failure;
   end Execute;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Bookmark_View_Record'Class) is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      List  : Bookmark_List.List_Node := First (Bookmark_Views_Module.List);
      Iter  : Gtk_Tree_Iter;
   begin
      if View.Deleting then
         return;
      end if;

      Clear (Model);

      while List /= Null_Node loop
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, Icon_Column, C_Proxy (View.Goto_Icon));
         Set (Model, Iter, Name_Column, Data (List).Name.all);
         Set (Model, Iter, Data_Column, Address => Convert (Data (List)));
         Set (Model, Iter, Editable_Column, True);
         List := Next (List);
      end loop;
   end Refresh;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (V      : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      View        : constant Gtk_Tree_View := Gtk_Tree_View (V);
      M           : constant Gtk_Tree_Store :=
                      Gtk_Tree_Store (Get_Model (View));
      Path_String : constant String := Get_String (Nth (Params, 1));
      Text_Value  : constant GValue := Nth (Params, 2);
      Iter        : Gtk_Tree_Iter;
      Mark        : Bookmark_Data_Access;
   begin
      Iter := Get_Iter_From_String (M, Path_String);
      Mark := Convert (Get_Address (M, Iter, Data_Column));
      Free (Mark.Name);
      Mark.Name := new String'(Get_String (Text_Value));
      Save_Bookmarks (Get_Kernel (Bookmark_Views_Module.all));
   end Edited_Callback;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View : constant Bookmark_View_Access :=
               Generic_View.Get_Or_Create_View (Kernel, Focus => False);
   begin
      Modify_Font (View.Tree, Get_Pref (View_Fixed_Font));
   end On_Preferences_Changed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
   begin
      Unref (Bookmark_View_Access (View).Goto_Icon);
   end On_Destroy;

   --------------------------
   -- On_Model_Row_Changed --
   --------------------------

   procedure On_Model_Row_Changed (View : access Gtk_Widget_Record'Class) is
      V : constant Bookmark_View_Access := Bookmark_View_Access (View);
      pragma Unreferenced (V);
   begin
      null;
--      Save_Bookmarks (V.Kernel);
   end On_Model_Row_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Bookmark_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Tooltip   : Bookmark_View_Tooltips_Access;
      Refresh_H : Refresh_Hook_Access;
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Gtk.Scrolled_Window.Initialize (View);
      Set_Policy (View, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (Icon_Column     => Gdk.Pixbuf.Get_Type,
                                Name_Column     => GType_String,
                                Data_Column     => GType_Pointer,
                                Editable_Column => GType_Boolean),
         Column_Names       => (1 => null, 2 => null),
         Editable_Columns   => (Name_Column => Editable_Column),
         Editable_Callback  => (Name_Column => Edited_Callback'Access),
         Show_Column_Titles => False,
         Selection_Mode     => Selection_Multiple,
         Sortable_Columns   => True,
         Initial_Sort_On    => 2,
         Merge_Icon_Columns => False,
         Hide_Expander      => True);
      Set_Name (View.Tree, "Bookmark TreeView"); --  For the testsuite
      Add (View, View.Tree);

      View.Goto_Icon := Render_Icon (View, Stock_Jump_To, Icon_Size_Menu);

      Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);
      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Module_ID (Bookmark_Views_Module),
         Context_Func    => View_Context_Factory'Access);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (On_Preferences_Changed'Access),
                Name  => "bookmark_views.preferences_changed",
                Watch => GObject (View));
      Refresh (View);

      Refresh_H := new Refresh_Hook'
        (Function_With_Args with View => Bookmark_View (View));
      Add_Hook (Kernel, Bookmark_Added_Hook,
                Refresh_H,
                Name  => "bookmark_views.refresh",
                Watch => GObject (View));
      Add_Hook (Kernel, Bookmark_Removed_Hook,
                Refresh_H,
                Name  => "bookmark_views.refresh",
                Watch => GObject (View));

      --  Each change to the model (in particular renaming of bookmarks) should
      --  be saved immediately into the bookmarks.xml file. Unfortunately,
      --  there doesn't appear to be a signal when a cell is finished editing,
      --  so we just monitor these events at the model level directly.

      Widget_Callback.Object_Connect
        (Get_Model (View.Tree), Signal_Row_Changed,
         On_Model_Row_Changed'Access, View);

      --  Initialize tooltips

      Tooltip := new Bookmark_View_Tooltips;
      Tooltip.Bookmark_View := Bookmark_View_Access (View);
      Set_Tooltip (Tooltip, View.Tree, 250);
   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Func   : Refresh_Hook;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
   begin
      Refresh (Func.View);
   end Execute;

   --------------------
   -- Load_Bookmarks --
   --------------------

   procedure Load_Bookmarks (Kernel : access Kernel_Handle_Record'Class) is
      Filename    : constant String := Get_Home_Dir (Kernel) & "bookmarks.xml";
      File, Child : Node_Ptr;
      Err         : String_Access;
      Marker      : Location_Marker;
   begin
      if GNAT.OS_Lib.Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);

         else
            Child := File.Child;

            while Child /= null loop
               Marker := Create_Marker (Kernel, Child);

               if Marker /= null then
                  declare
                     Name : constant String :=
                              Get_Attribute (Child, "bookmark_name", "");
                  begin
                     if Name = "" then
                        Append (Bookmark_Views_Module.List,
                                new Bookmark_Data'
                                  (Marker    => Marker,
                                   Instances => Null_Instance_List,
                                   Name   => new String'(To_String (Marker))));
                     else
                        Append (Bookmark_Views_Module.List,
                                new Bookmark_Data'
                                  (Marker    => Marker,
                                   Instances => Null_Instance_List,
                                   Name      => new String'(Name)));
                     end if;
                  end;
               end if;

               Child := Child.Next;
            end loop;

            Free (File);
         end if;

         Run_Hook (Kernel, Bookmark_Added_Hook);
      end if;
   end Load_Bookmarks;

   --------------------
   -- Save_Bookmarks --
   --------------------

   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class) is
      Filename    : constant String := Get_Home_Dir (Kernel) & "bookmarks.xml";
      File, Child : Node_Ptr;
      List        : Bookmark_List.List_Node :=
                      First (Bookmark_Views_Module.List);
      Success     : Boolean;
   begin
      Trace (Me, "Saving " & Filename);
      File := new Node;
      File.Tag := new String'("Bookmarks");

      while List /= Null_Node loop
         Child := Save (Data (List).Marker);

         if Child /= null then
            Set_Attribute (Child, "bookmark_name",
                           Data (List).Name.all);
            Add_Child (File, Child, Append => True);
         end if;

         List := Next (List);
      end loop;

      Print (File, Filename, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Bookmarks;

   ------------------------
   -- Bookmark_From_Name --
   ------------------------

   function Bookmark_From_Name
     (Name : String) return Bookmark_List.List_Node
   is
      List : Bookmark_List.List_Node := First (Bookmark_Views_Module.List);
   begin
      while List /= Null_Node loop
         exit when Data (List).Name.all = Name;
         List := Next (List);
      end loop;
      return List;
   end Bookmark_From_Name;

   ----------------------------
   -- Instance_From_Bookmark --
   ----------------------------

   function Instance_From_Bookmark
     (List   : Bookmark_List.List_Node;
      Class  : Class_Type;
      Script : access Scripting_Language_Record'Class) return Class_Instance
   is
      Inst : Class_Instance :=
               Get (Bookmark_List.Data (List).Instances, Script);
   begin
      if Inst = No_Class_Instance then
         Inst := New_Instance (Script, Class);
         Set_Data (Inst, Class, Bookmark_List.Data (List).Name.all);
         Set (Bookmark_List.Data (List).Instances, Script, Inst);
      end if;

      return Inst;
   end Instance_From_Bookmark;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Bookmark_Class : constant Class_Type :=
                         New_Class
                           (Get_Scripts (Get_Kernel (Data)), "Bookmark");
      Name_Cst       : aliased constant String := "name";
      Inst           : Class_Instance;
      Bookmark       : Bookmark_List.List_Node;
      Marker         : Location_Marker;
      Tmp            : Boolean;
      List           : Bookmark_List.List_Node;
      pragma Unreferenced (Tmp);
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, "Cannot create instances of GPS.Bookmark."
            & " Use GPS.Bookmark.get() instead");

      elsif Command = "get" then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Bookmark := Bookmark_From_Name (Nth_Arg (Data, 1));
         if Bookmark = Null_Node then
            Set_Error_Msg (Data, "No such bookmark");
         else
            Inst := Instance_From_Bookmark
              (Bookmark, Bookmark_Class, Get_Script (Data));
            Set_Return_Value (Data, Inst);
         end if;

      elsif Command = "create" then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Marker := Create_Marker (Get_Kernel (Data));
         if Marker = null then
            Set_Error_Msg (Data, "Can't create bookmark for this context");
         else
            Prepend
              (Bookmark_Views_Module.List,
               new Bookmark_Data'
                 (Marker    => Marker,
                  Instances => Null_Instance_List,
                  Name      => new String'(Nth_Arg (Data, 1))));
            Save_Bookmarks (Get_Kernel (Data));
            Run_String_Hook
              (Get_Kernel (Data), Bookmark_Added_Hook, Nth_Arg (Data, 1));
            Bookmark := First (Bookmark_Views_Module.List);
            Set_Return_Value
              (Data, Instance_From_Bookmark
                 (Bookmark, Bookmark_Class, Get_Script (Data)));
         end if;

      elsif Command = "name" then
         Inst := Nth_Arg (Data, 1, Bookmark_Class);
         Set_Return_Value (Data, String'(Get_Data (Inst, Bookmark_Class)));

      elsif Command = "delete" then
         Inst := Nth_Arg (Data, 1, Bookmark_Class);
         Bookmark := Bookmark_From_Name (Get_Data (Inst, Bookmark_Class));
         if Bookmark = Null_Node then
            Set_Error_Msg (Data, "Invalid bookmark");
         else
            Delete_Bookmark
              (Get_Kernel (Data), Bookmark_List.Data (Bookmark).all);
         end if;

      elsif Command = "rename" then
         Name_Parameters (Data, (2 => Name_Cst'Unchecked_Access));
         Inst := Nth_Arg (Data, 1, Bookmark_Class);
         Bookmark := Bookmark_From_Name (Get_Data (Inst, Bookmark_Class));
         if Bookmark = Null_Node then
            Set_Error_Msg (Data, "Invalid bookmark");
         else
            Run_String_Hook
              (Get_Kernel (Data), Bookmark_Added_Hook,
               Bookmark_List.Data (Bookmark).Name.all);
            Free (Bookmark_List.Data (Bookmark).Name);
            Bookmark_List.Data (Bookmark).Name :=
              new String'(Nth_Arg (Data, 2));
            Set_Data (Inst, Bookmark_Class, String'(Nth_Arg (Data, 2)));
            Run_String_Hook
              (Get_Kernel (Data), Bookmark_Added_Hook, Nth_Arg (Data, 2));
            Save_Bookmarks (Get_Kernel (Data));
         end if;

      elsif Command = "goto" then
         Inst := Nth_Arg (Data, 1, Bookmark_Class);
         Bookmark := Bookmark_From_Name (Get_Data (Inst, Bookmark_Class));
         if Bookmark = Null_Node
           or else not Go_To
             (Bookmark_List.Data (Bookmark).Marker, Get_Kernel (Data))
         then
            Set_Error_Msg (Data, "Invalid bookmark");
         end if;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         List := First (Bookmark_Views_Module.List);
         while List /= Null_Node loop
            Inst := Instance_From_Bookmark
              (List, Bookmark_Class, Get_Script (Data));
            Set_Return_Value (Data, Inst);
            List := Next (List);
         end loop;
      end if;
   end Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Bookmark_Class     : constant Class_Type :=
                             New_Class (Get_Scripts (Kernel), "Bookmark");
      Src_Action_Context : constant Action_Filter :=
                             Lookup_Filter (Kernel, "Source editor");
      Command            : Interactive_Command_Access;

   begin
      Bookmark_Views_Module := new Bookmark_Views_Module_Record;
      Generic_View.Register_Module
        (Kernel, Module_ID (Bookmark_Views_Module),
         "_Bookmarks", -"Call Tree");

      Register_Hook_No_Return (Kernel, Bookmark_Added_Hook, String_Hook_Type);
      Register_Hook_No_Return
        (Kernel, Bookmark_Removed_Hook, String_Hook_Type);

      Load_Bookmarks (Kernel);

      Command := new Rename_Bookmark_Command;
      Register_Contextual_Menu
        (Kernel, "Bookmark View Rename Bookmark",
         Action => Command,
         Filter => Create (Module => "Bookmark_View"),
         Label  => -"Rename bookmark");

      Command := new Delete_Bookmark_Command;
      Register_Contextual_Menu
        (Kernel, "Bookmark View Delete Bookmark",
         Action => Command,
         Filter => Create (Module => "Bookmark_View"),
         Label  => -"Delete bookmark");

      Command := new Create_Bookmark_Command;
      Register_Menu
        (Kernel,
         "/" & (-"Edit"), -"Create Bookmark", "",
         Ref_Item => -"Comment Lines",
         Callback => null,
         Command  => Command,
         Filter   => Src_Action_Context);
      Register_Action
        (Kernel, "Bookmark Create", Command,
         -("Create a bookmark at the current location"));

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Command_Handler'Access,
         Bookmark_Class);
      Register_Command
        (Kernel, "get", 1, 1, Command_Handler'Access, Bookmark_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "create", 1, 1, Command_Handler'Access, Bookmark_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "list", 0, 0, Command_Handler'Access, Bookmark_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "name", 0, 0, Command_Handler'Access, Bookmark_Class);
      Register_Command
        (Kernel, "rename", 1, 1, Command_Handler'Access, Bookmark_Class);
      Register_Command
        (Kernel, "delete", 0, 0, Command_Handler'Access, Bookmark_Class);
      Register_Command
        (Kernel, "goto", 0, 0, Command_Handler'Access, Bookmark_Class);
   end Register_Module;

end Bookmark_Views;
