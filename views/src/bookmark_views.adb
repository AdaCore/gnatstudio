-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005                         --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                    use System;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

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
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Generic_List;
with XML_Parsers;               use XML_Parsers;
with Traces;                    use Traces;
with Tooltips;

package body Bookmark_Views is
   Me : constant Debug_Handle := Create ("Bookmarks");

   Icon_Column     : constant := 0;
   Name_Column     : constant := 1;
   Data_Column     : constant := 2;
   Editable_Column : constant := 3;

   type Bookmark_Data is record
      Marker : Location_Marker;
      Name   : GNAT.OS_Lib.String_Access;
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

   procedure Destroy (Module : in out Bookmark_Views_Module_Record);
   --  Called when the module is destroyed

   Bookmark_Views_Module : Bookmark_Views_Module_Access;

   type Bookmark_View_Record is new Generic_Views.View_Record with record
      Tree      : Gtk_Tree_View;
      Kernel    : Kernel_Handle;
      Goto_Icon : Gdk_Pixbuf;
   end record;

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

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Context factory when creating contextual menus

   function Get_Selected_From_Event
     (View  : access Bookmark_View_Record'Class;
      Event : Gdk_Event) return Bookmark_Data_Access;
   --  Return the entry selected by event

   procedure Set
     (Tree   : access Gtk_Tree_Store_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Gint;
      Value  : System.Address);
   --  Set a pointer in the tree

   procedure Load_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   --  Load or save the bookmarks from the XML file

   procedure Edited_Callback
     (V      : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a line is edited in the view

   package Bookmark_Idle is new Gtk.Main.Idle (Bookmark_View_Access);
   use Bookmark_Idle;
   function Start_Editing_Idle (View : Bookmark_View_Access) return Boolean;
   --  Function called to start editing the selected line. This is necessary
   --  since any editing is stopped as soon as the tree gains the focus back.

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
      Window     : Gdk.Window.Gdk_Window;
      New_Window : Gdk.Window.Gdk_Window;
      Mask       : Gdk_Modifier_Type;

      Model      : constant Gtk_Tree_Model :=
                     Get_Model (Tooltip.Bookmark_View.Tree);

      X, Y       : Gint;
      Path       : Gtk_Tree_Path;
      Column     : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y     : Gint;
      Row_Found  : Boolean := False;
      Iter       : Gtk_Tree_Iter;
      Selected   : Integer;
      pragma Unreferenced (Selected);

      Text       : GNAT.OS_Lib.String_Access;
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
            Text := new String'("Name: " & Data.Name.all
                                & ASCII.LF
                                & "Location: " & Location);
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

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Delete_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View        : constant Bookmark_View_Access :=
        Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Data        : Bookmark_Data_Access;
      Node, Prev  : List_Node;
   begin
      if Context.Event /= null then
         Data := Get_Selected_From_Event (View, Context.Event);
         if Data /= null then
            Node := First (Bookmark_Views_Module.List);
            while Node /= Null_Node loop
               if Bookmark_List.Data (Node) = Data then
                  Remove_Nodes (Bookmark_Views_Module.List, Prev, Node);
                  exit;
               end if;

               Prev := Node;
               Node := Next (Node);
            end loop;
            Refresh (View);
            return Success;
         end if;
      end if;
      return Failure;
   end Execute;

   ------------------------
   -- Start_Editing_Idle --
   ------------------------

   function Start_Editing_Idle (View : Bookmark_View_Access) return Boolean is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         Set_Cursor
           (View.Tree,
            Path          => Path,
            Focus_Column  => Get_Column (View.Tree, 2),
            Start_Editing => True);
         Path_Free (Path);
      end if;
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

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree   : access Gtk_Tree_Store_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Gint;
      Value  : System.Address)
   is
      procedure Internal
        (Tree   : System.Address;
         Iter   : Gtk_Tree_Iter;
         Column : Gint;
         Value  : System.Address;
         Final  : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");
   begin
      Internal (Get_Object (Tree), Iter, Column, Value);
   end Set;

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

   -----------------------------
   -- Get_Selected_From_Event --
   -----------------------------

   function Get_Selected_From_Event
     (View  : access Bookmark_View_Record'Class;
      Event : Gdk_Event) return Bookmark_Data_Access
   is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      Iter  : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (View.Tree, Model, Event);
      if Iter /= Null_Iter then
         return Convert (Get_Address (Model, Iter, Data_Column));
      else
         return null;
      end if;
   end Get_Selected_From_Event;

   --------------------------
   -- View_Context_Factory --
   --------------------------

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Menu);
      --  Nothing special in the context, just the module itself so that people
      --  can still add information if needed
      V       : constant Bookmark_View_Access := Bookmark_View_Access (Object);
      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Context : constant Selection_Context_Access := new Selection_Context;
      Iter    : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);
      if Iter /= Null_Iter then
         Select_Iter (Get_Selection (V.Tree), Iter);
      end if;
      return Context;
   end View_Context_Factory;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      View   : constant Bookmark_View_Access := Bookmark_View_Access (Clip);
      Model  : constant Gtk_Tree_Store :=
                 Gtk_Tree_Store (Get_Model (View.Tree));
      Marker : Bookmark_Data_Access;
      Iter   : Gtk_Tree_Iter;
      Col    : Gtk_Tree_View_Column;
      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      if Get_Button (Event) = 1 then
         --  ??? why the above call is needed
         Coordinates_For_Event (View.Tree, Model, Event, Iter, Col);
         Marker := Get_Selected_From_Event (View, Event);

         --  Always return True to prevent the selection
         if Marker /= null then
            Result := Go_To (Marker.Marker, View.Kernel);
            return True;
         end if;
      end if;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Information (E));
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
      Child  : constant MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Bookmark_View_Record'Tag);
      View   : Bookmark_View_Access;
      Model  : Gtk_Tree_Store;
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
   begin
      if Mark /= null then
         Append (Bookmark_Views_Module.List,
                 new Bookmark_Data'
                   (Marker => Mark,
                    Name   => new String'(To_String (Mark))));
         if Child /= null then
            View  := Bookmark_View_Access (Get_Widget (Child));
            Model := Gtk_Tree_Store (Get_Model (View.Tree));
            Refresh (View);

            --  Start editing the name of the bookmark immediately
            Iter := Get_Iter_First (Model);
            while Iter /= Null_Iter loop
               if Convert (Get_Address (Model, Iter, Data_Column)).Marker =
                 Mark
               then
                  Path := Get_Path (Model, Iter);
                  Set_Cursor
                    (View.Tree,
                     Path          => Path,
                     Focus_Column  => Get_Column (View.Tree, 2),
                     Start_Editing => True);
                  Path_Free (Path);
                  exit;
               end if;
               Next (Get_Model (View.Tree), Iter);
            end loop;
         end if;
         Run_Hook (Kernel, Bookmark_Added_Hook);
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
      List  :  Bookmark_List.List_Node := First (Bookmark_Views_Module.List);
      Iter  : Gtk_Tree_Iter;
   begin
      Clear (Model);
      while List /= Null_Node loop
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, Icon_Column, C_Proxy (View.Goto_Icon));
         Set (Model, Iter, Name_Column, Data (List).Name.all);
         Set (Model, Iter, Data_Column, Convert (Data (List)));
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
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));
      Text_Value  : constant GValue := Nth (Params, 2);
      Mark        : Bookmark_Data_Access;
   begin
      Iter := Get_Iter_From_String (M, Path_String);
      Mark := Convert (Get_Address (M, Iter, Data_Column));
      Free (Mark.Name);
      Mark.Name := new String'(Get_String (Text_Value));
   end Edited_Callback;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View : constant Bookmark_View_Access :=
        Generic_View.Get_Or_Create_View (Kernel);
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Bookmark_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Tooltip  : Bookmark_View_Tooltips_Access;
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
         Selection_Mode     => Selection_Single,
         Sortable_Columns   => True,
         Initial_Sort_On    => 2,
         Merge_Icon_Columns => False,
         Hide_Expander      => True);
      Add (View, View.Tree);

      View.Goto_Icon := Render_Icon (View, Stock_Jump_To, Icon_Size_Menu);

      Widget_Callback.Connect (View, "destroy", On_Destroy'Access);
      Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
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

      --  Initialize tooltips

      Tooltip := new Bookmark_View_Tooltips;
      Tooltip.Bookmark_View := Bookmark_View_Access (View);
      Set_Tooltip (Tooltip, View.Tree, 250);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Bookmark_Views_Module_Record) is
   begin
      Save_Bookmarks (Get_Kernel (Module));
   end Destroy;

   --------------------
   -- Load_Bookmarks --
   --------------------

   procedure Load_Bookmarks (Kernel : access Kernel_Handle_Record'Class) is
      Filename    : constant String := Get_Home_Dir (Kernel) & "bookmarks.xml";
      File, Child : Node_Ptr;
      Err         : String_Access;
      Marker      : Location_Marker;
   begin
      if Is_Regular_File (Filename) then
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
                                  (Marker => Marker,
                                   Name   => new String'(To_String (Marker))));
                     else
                        Append (Bookmark_Views_Module.List,
                                new Bookmark_Data'
                                  (Marker => Marker,
                                   Name   => new String'(Name)));
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

      Print (File, Filename);
      Free (File);
   end Save_Bookmarks;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      Bookmark_Views_Module := new Bookmark_Views_Module_Record;
      Generic_View.Register_Module
        (Kernel, Module_ID (Bookmark_Views_Module));

      Register_Hook_No_Args (Kernel, Bookmark_Added_Hook);

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
         Ref_Item => -"Comment Lines", Callback => null, Command => Command);
      Register_Action
        (Kernel, "Bookmark Create", Command,
         -("Create a bookmark at the current location"));
   end Register_Module;

end Bookmark_Views;
