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
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Intl;                  use GPS.Intl;
with Histories;                 use Histories;
with GUI_Utils;                 use GUI_Utils;
with Src_Editor_Module;         use Src_Editor_Module;
with VFS;                       use VFS;
with Traces;                    use Traces;
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

   History_Show_Notebooks : constant History_Key :=
     "Windows_View_Show_Notebooks";
   --  Used to store the current view settings in histories

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

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Context factory when creating contextual menus

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      View     : constant Buffer_View_Access := Buffer_View_Access
        (Get_Widget
           (Find_MDI_Child_By_Tag (Get_MDI (Kernel), Buffer_View_Record'Tag)));
      Model    : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));
      Child    : MDI_Child;
      Iter, Iter2 : Gtk_Tree_Iter;
      Count       : Natural := 0;
      CIter       : Child_Iterator := First_Child (Get_MDI (Kernel));
   begin
      while Get (CIter) /= null loop
         Count := Count + 1;
         Next (CIter);
      end loop;

      declare
         Children : MDI_Child_Array (1 .. Count);
      begin
         Count := Children'First;

         Iter := Get_Iter_First (Get_Model (View.Tree));
         while Iter /= Null_Iter loop
            Iter2 := Gtk.Tree_Store.Children (Model, Iter);
            while Iter2 /= Null_Iter loop
               if Iter_Is_Selected (Get_Selection (View.Tree), Iter2) then
                  Child := Find_MDI_Child_By_Name
                    (Get_MDI (Kernel), Get_String (Model, Iter2, Data_Column));
                  if Child /= null then
                     Children (Count) := Child;
                     Count := Count + 1;
                  end if;
               end if;

               Next (Model, Iter2);
            end loop;

            if Iter_Is_Selected (Get_Selection (View.Tree), Iter) then
               Child := Find_MDI_Child_By_Name
                 (Get_MDI (Kernel), Get_String (Model, Iter, Data_Column));
               if Child /= null then
                  Children (Count) := Child;
                  Count := Count + 1;
               end if;
            end if;

            Next (Model, Iter);
         end loop;

         for C in Children'Range loop
            Close_Child (Children (C));
         end loop;
      end;

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

         --  Only for actual windows
         if Children (Model, Iter) = Null_Iter then

            Child := Find_MDI_Child_By_Name
              (Get_MDI (Kernel), Get_String (Model, Iter, Data_Column));

            --  If there is any modified, don't do anything. The user is trying
            --  to select multiple lines

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
      Iter2 : Gtk_Tree_Iter;
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
               Iter2 := Children (Model, Iter);
               if Iter2 = Null_Iter then
                  if Get_String (Model, Iter, Data_Column) = Selected then
                     Select_Iter (Get_Selection (V.Tree), Iter);
                     exit;
                  end if;
               else
                  while Iter2 /= Null_Iter loop
                     if Get_String (Model, Iter2, Data_Column) = Selected then
                        Select_Iter (Get_Selection (V.Tree), Iter2);
                        return;
                     end if;
                     Next (Model, Iter2);
                  end loop;
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
      Editors_Only   : constant Boolean := Get_History
        (Get_History (V.Kernel).all, History_Editors_Only);
      Show_Notebooks : constant Boolean := Get_History
        (Get_History (V.Kernel).all, History_Show_Notebooks);

      Notebook_Index : Integer := -1;
      Iter           : Gtk_Tree_Iter := Null_Iter;

      procedure Show_Child (Parent : Gtk_Tree_Iter; Child : MDI_Child);
      --  Insert the line for Child in the view

      procedure Purify;
      --  Clean up the tree so that we do not show empty notebook or
      --  notebooks with a single child

      ------------
      -- Purify --
      ------------

      procedure Purify is
         Iter2 : Gtk_Tree_Iter;
      begin
         if Iter /= Null_Iter then
            Iter2 := Children (Model, Iter);
            if Iter2 = Null_Iter then
               --  If we had an empty notebook, remove it
               Remove (Model, Iter);
            elsif N_Children (Model, Iter) = 1 then
               --  Single child ?
               Set (Model, Iter, Icon_Column,
                    Get_C_Proxy (Model, Iter2, Icon_Column));
               Set (Model, Iter, Name_Column,
                    Get_String (Model, Iter2, Name_Column));
               Set (Model, Iter, Data_Column,
                    Get_String (Model, Iter2, Data_Column));
               Remove (Model, Iter2);
            else
               Notebook_Index := Notebook_Index + 1;
            end if;
         else
            Notebook_Index := Notebook_Index + 1;
         end if;
      end Purify;

      ----------------
      -- Show_Child --
      ----------------

      procedure Show_Child (Parent : Gtk_Tree_Iter; Child : MDI_Child) is
         Name : constant String := Get_Short_Title (Child);
         Iter : Gtk_Tree_Iter;
      begin
         if not Editors_Only
           or else Is_Source_Box (Child)
         then
            Append (Model, Iter, Parent);
            if Name = "" then
               Set (Model, Iter, Name_Column, Untitled);
               Set (Model, Iter, Data_Column, Untitled);
            else
               Set (Model, Iter, Icon_Column,
                    C_Proxy (Get_Icon (Child)));
               Set (Model, Iter, Name_Column, Name);
               Set (Model, Iter, Data_Column, Get_Title (Child));
            end if;

            if Child = Get_Focus_Child (Get_MDI (V.Kernel)) then
               Select_Iter (Get_Selection (V.Tree), Iter);
            end if;
         end if;
      end Show_Child;

      I_Child : Gtkada.MDI.Child_Iterator;
      Child   : MDI_Child;
      Column  : Gint;
      Current_Notebook : Gtk_Notebook;
      pragma Unreferenced (Column);

   begin
      Clear (Model);

      if Show_Notebooks then
         Column := Freeze_Sort (Gtk_Tree_Store (Get_Model (V.Tree)));
      else
         Thaw_Sort (Gtk_Tree_Store (Get_Model (V.Tree)), 1);
      end if;

      I_Child := First_Child
        (Get_MDI (V.Kernel), Group_By_Notebook => Show_Notebooks);
      loop
         Child := Get (I_Child);
         exit when Child = null;

         if Show_Notebooks then
            if Notebook_Index = -1
              or else Get_Notebook (I_Child) /= Current_Notebook
            then
               Purify;
               Current_Notebook := Get_Notebook (I_Child);
               Append (Model, Iter, Null_Iter);
               Set (Model, Iter, Name_Column,
                    -"Notebook" & Integer'Image (Notebook_Index));
            end if;

            Show_Child (Iter, Child);
         else
            Show_Child (Null_Iter, Child);
         end if;

         Next (I_Child);
      end loop;

      if Show_Notebooks then
         Purify;
      end if;

      Expand_All (V.Tree);

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
      V       : constant Buffer_View_Access := Buffer_View_Access (Object);
      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Context : Selection_Context_Access;
      Iter    : Gtk_Tree_Iter;
      Check   : Gtk_Check_Menu_Item;
   begin
      --  Focus on the window, so that the selection is correctly taken into
      --  account

      Raise_Child
        (Find_MDI_Child (Get_MDI (V.Kernel), V), Give_Focus => True);

      Iter := Find_Iter_For_Event (V.Tree, Model, Event);

      if Iter /= Null_Iter then
         --  Nothing special in the context, just the module itself so that
         --  people can still add information if needed
         Context := new Selection_Context;

         if not Iter_Is_Selected (Get_Selection (V.Tree), Iter) then
            Unselect_All (Get_Selection (V.Tree));
            Select_Iter (Get_Selection (V.Tree), Iter);
         end if;
      end if;

      if Menu /= null then
         Gtk_New (Check, Label => -"Show editors only");
         Associate (Get_History (Kernel).all, History_Editors_Only, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled", Refresh'Access, Slot_Object => V);

         Gtk_New (Check, Label => -"Show notebooks");
         Associate
           (Get_History (Kernel).all, History_Show_Notebooks, Check);
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
         Hide_Expander      => False);
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
      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_icon_changed", Refresh'Access, View);
      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "float_child", Refresh'Access, View);
      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "children_reorganized", Refresh'Access, View);

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

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, History_Editors_Only, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, History_Show_Notebooks, False);

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
