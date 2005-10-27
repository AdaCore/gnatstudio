-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Types;                 use Gdk.Types;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with Histories;                 use Histories;
with GUI_Utils;                 use GUI_Utils;
with Src_Editor_Module;         use Src_Editor_Module;
with VFS;                       use VFS;
with Traces;                    use Traces;
with String_List_Utils;         use String_List_Utils;
with Commands.Interactive;      use Commands, Commands.Interactive;

package body Buffer_Views is
   Icon_Column : constant := 0;
   Name_Column : constant := 1;
   Data_Column : constant := 2;

   Untitled    : constant String := "Untitled";
   --  Label used for new window that is not yet saved

   History_Editors_Only : constant History_Key :=
     "Windows_View_Editors_Only";
   --  Used to store the current filter settings in histories

   Buffer_View_Module : Module_ID;

   type Buffer_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Tree   : Gtk_Tree_View;
      Kernel : Kernel_Handle;
      File   : Virtual_File; -- current selected file (cache)
   end record;
   type Buffer_View_Access is access all Buffer_View_Record'Class;

   procedure On_Open_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Create the Buffer view (or raise the existing one)

   function Open_View
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Create the Buffer view if needed

   procedure Gtk_New
     (View   : out Buffer_View_Access;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new Buffer view

   procedure Child_Selected (View : access Gtk_Widget_Record'Class);
   --  Called when a new child is selected

   procedure Refresh (View : access Gtk_Widget_Record'Class);
   --  Refresh the contents of the Buffer view

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path;
   --  Return the path at which Event has occured.
   --  User must free memory associated to the returned path.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Handling of desktops

   type Close_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close the selected editors

   function Get_Selected_Files
     (Explorer : Buffer_View_Access) return String_List_Utils.String_List.List;
   --  Return the list of files that are selected

   package Explorer_Selection_Foreach is
     new Selection_Foreach (Buffer_View_Record);

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Context factory when creating contextual menus

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : Buffer_View_Access) return String_List_Utils.String_List.List
   is
      Result : String_List_Utils.String_List.List;

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access);
      --  Add an item to Result.

      -----------------------
      -- Add_Selected_Item --
      -----------------------

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access)
      is
         pragma Unreferenced (Path, Data);
      begin
         String_List_Utils.String_List.Append
           (Result, Get_String (Model, Iter, Data_Column));
      end Add_Selected_Item;

   begin
      if Explorer = null then
         return Result;
      end if;

      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Explorer.Tree),
         Add_Selected_Item'Unrestricted_Access,
         Explorer_Selection_Foreach.Data_Type_Access (Explorer));
      return Result;
   end Get_Selected_Files;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      use type String_List_Utils.String_List.List_Node;

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      View     : constant Buffer_View_Access := Buffer_View_Access
        (Get_Widget
           (Find_MDI_Child_By_Tag (Get_MDI (Kernel), Buffer_View_Record'Tag)));
      Selected : String_List_Utils.String_List.List;
      Node     : String_List_Utils.String_List.List_Node;
   begin
      Selected := Get_Selected_Files (View);

      Node := String_List_Utils.String_List.First (Selected);

      while Node /= String_List_Utils.String_List.Null_Node loop
         declare
            Filename : constant String :=
                         String_List_Utils.String_List.Data (Node);
            File     : constant Virtual_File := Create (Filename);
         begin
            Close_File_Editors (Kernel, File);
         end;

         Node := String_List_Utils.String_List.Next (Node);
      end loop;

      String_List_Utils.String_List.Free (Selected);

      return Success;
   end Execute;

   -----------------------
   -- Get_Path_At_Event --
   -----------------------

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path
   is
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;

   begin
      Path := Gtk_New;
      Get_Path_At_Pos
        (Tree, Gint (X), Gint (Y),
         Path, Column, Buffer_X, Buffer_Y, Row_Found);

      return Path;
   end Get_Path_At_Event;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Explorer : constant Buffer_View_Access := Buffer_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Model    : constant Gtk_Tree_Store :=
                   Gtk_Tree_Store (Get_Model (Explorer.Tree));
      Path     : constant Gtk_Tree_Path :=
        Get_Path_At_Event (Explorer.Tree, Event);
      Iter     : Gtk_Tree_Iter;
      Child    : MDI_Child;
   begin
      if Path /= null then
         Iter := Get_Iter (Model, Path);
         Path_Free (Path);

         Child := Find_MDI_Child_By_Name
           (Get_MDI (Kernel), Get_String (Model, Iter, Data_Column));

         --  If there is any modified, don't do anything. The user is trying to
         --  select multiple lines

         if Get_State (Event) = 0 then
            if Get_Event_Type (Event) = Gdk_2button_Press then
               Raise_Child (Child, Give_Focus => True);
               return True;

            elsif Get_Event_Type (Event) = Button_Press
              and then Get_Button (Event) = 1
            then
               Child_Drag_Begin (Child => Child, Event => Event);
               Raise_Child (Child, Give_Focus => True);
               return True;
            end if;
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected (View : access Gtk_Widget_Record'Class) is
      V     : constant Buffer_View_Access := Buffer_View_Access (View);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Model);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (V.Kernel));
   begin
      --  If we are in the buffers view, do not show it, since otherwise that
      --  breaks the selection of multiple lines
      Unselect_All (Get_Selection (V.Tree));
      if Child /= null and then Get_Widget (Child) /= Gtk_Widget (V) then
         declare
            Selected : constant String := Get_Title (Child);
         begin
            while Iter /= Null_Iter loop
               if Get_String (Model, Iter, Data_Column) = Selected then
                  Select_Iter (Get_Selection (V.Tree), Iter);
                  exit;
               end if;

               Next (Model, Iter);
            end loop;
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Message (E));
   end Child_Selected;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Gtk_Widget_Record'Class) is
      V       : constant Buffer_View_Access := Buffer_View_Access (View);
      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      I_Child : Child_Iterator;
      Child   : MDI_Child;
      Iter    : Gtk_Tree_Iter;
      Editors_Only : Boolean;
   begin
      Create_New_Boolean_Key_If_Necessary
        (Get_History (V.Kernel).all, History_Editors_Only, True);
      Editors_Only := Get_History
        (Get_History (V.Kernel).all, History_Editors_Only);

      Clear (Model);
      I_Child := First_Child (Get_MDI (V.Kernel));

      loop
         Child := Get (I_Child);
         exit when Child = null;

         if not Editors_Only
           or else Is_Source_Box (Child)
         then
            declare
               Name : constant String := Get_Short_Title (Child);
            begin
               Append (Model, Iter, Null_Iter);
               if Name = "" then
                  Set (Model, Iter, Name_Column, Untitled);
                  Set (Model, Iter, Data_Column, Untitled);
               else
                  Set (Model, Iter, Icon_Column, C_Proxy (Get_Icon (Child)));
                  Set (Model, Iter, Name_Column, Name);
                  Set (Model, Iter, Data_Column, Get_Title (Child));
               end if;

               if Child = Get_Focus_Child (Get_MDI (V.Kernel)) then
                  Select_Iter (Get_Selection (V.Tree), Iter);
               end if;
            end;
         end if;

         Next (I_Child);
      end loop;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Message (E));
   end Refresh;

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
      pragma Unreferenced (Event_Widget);
      --  Nothing special in the context, just the module itself so that people
      --  can still add information if needed
      V       : constant Buffer_View_Access := Buffer_View_Access (Object);
      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Context : constant Selection_Context_Access := new Selection_Context;
      Iter    : Gtk_Tree_Iter;
      Check   : Gtk_Check_Menu_Item;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);
      Select_Iter (Get_Selection (V.Tree), Iter);

      if Menu /= null then
         Gtk_New (Check, Label => -"Show editors only");
         Associate (Get_History (Kernel).all, History_Editors_Only, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled", Refresh'Access, Slot_Object => V);

      end if;

      return Context;
   end View_Context_Factory;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Buffer_View_Access;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      View := new Buffer_View_Record;
      View.Kernel := Kernel_Handle (Kernel);
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      Pack_Start (View, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (Icon_Column => Gdk.Pixbuf.Get_Type,
                                Name_Column => GType_String,
                                Data_Column => GType_String),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Selection_Mode     => Selection_Multiple,
         Sortable_Columns   => True,
         Initial_Sort_On    => 2,
         Hide_Expander      => True);
      Add (Scrolled, View.Tree);

      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_added", Refresh'Access, Slot_Object => View);
      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_removed", Refresh'Access,
         Slot_Object => View);
      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_title_changed", Refresh'Access, View);
      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_selected", Child_Selected'Access, View);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Buffer_View_Module,
         Context_Func    => View_Context_Factory'Access);

      Refresh (View);
   end Gtk_New;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Buffer_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Windows_View");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Windows_View" then
         return Open_View (User);
      end if;
      return null;
   end Load_Desktop;

   ---------------
   -- Open_View --
   ---------------

   function Open_View
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child
   is
      Child : MDI_Child;
      View  : Buffer_View_Access;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Buffer_View_Record'Tag);

      if Child = null then
         Gtk_New (View, Kernel);
         Child := Put
           (Kernel, View,
            Default_Width  => 215,
            Default_Height => 600,
            Position       => Position_Left,
            Module         => Buffer_View_Module);
         Set_Title (Child, -"Windows View", -"Windows View");
      end if;

      return Child;
   end Open_View;

   ------------------
   -- On_Open_View --
   ------------------

   procedure On_Open_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      View : MDI_Child;
      pragma Unreferenced (Widget);
   begin
      View := Open_View (Kernel);
      Raise_Child (View);
      Set_Focus_Child (Get_MDI (Kernel), View);
   end On_Open_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      Buffer_View_Module := new Module_ID_Record;
      Register_Module
        (Module      => Buffer_View_Module,
         Module_Name => "Buffer_View",
         Kernel      => Kernel);
      Register_Menu
        (Kernel,
         "/" & (-"Tools"), -"Windows View", "", On_Open_View'Access,
         Ref_Item => -"File View", Add_Before => False);

      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Close_Command;
      Register_Contextual_Menu
        (Kernel, "Windows View Close Windows",
         Action => Command,
         Filter => Create (Module => "Buffer_View"),
         Label  => -"Close selected windows");
   end Register_Module;

end Buffer_Views;
