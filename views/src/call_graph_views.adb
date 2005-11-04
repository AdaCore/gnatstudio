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

with Ada.Exceptions;              use Ada.Exceptions;
with Basic_Types;                 use Basic_Types;
with Commands.Interactive;        use Commands, Commands.Interactive;
with Entities.Commands;           use Entities.Commands;
with Entities.Queries;            use Entities, Entities.Queries;
with Gdk.Event;                   use Gdk.Event;
with Generic_Views;
with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with Gtk.Check_Menu_Item;         use Gtk.Check_Menu_Item;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_Selection;          use Gtk.Tree_Selection;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;          use GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks;   use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                    use GPS.Intl;
with GUI_Utils;                   use GUI_Utils;
with Histories;                   use Histories;
with String_Utils;                use String_Utils;
with Traces;                      use Traces;
with VFS;                         use VFS;

package body Call_Graph_Views is
   Me : constant Debug_Handle := Create ("Call_Graph");

   Name_Column    : constant := 0;
   Decl_Column    : constant := 1;
   Entity_Column  : constant := 2;
   File_Column    : constant := 3;
   Line_Column    : constant := 4;
   Column_Column  : constant := 5;

   History_Show_Locations : constant History_Key :=
     "Call_Graph_Show_Locations";

   Ellipsis_UTF8 : constant String :=
     Character'Val (16#E2#) & Character'Val (16#80#)
     & Character'Val (16#A6#);
   --  UTF8 encoding for the ellipsis character

   Called_At_Label : constant String := Ellipsis_UTF8 & " called at";
   --  Label used for the locations node

   Computing_Label : constant String := "computing...";
   --  Label used while computing the ancestors call graph

   type View_Type is (View_Any, View_Calls, View_Called_By);

   type Callgraph_View_Record is new Gtk_Scrolled_Window_Record with record
      Tree   : Gtk_Tree_View;
      Kernel : Kernel_Handle;
      Typ    : View_Type := View_Any;
   end record;

   procedure Initialize
     (View   : access Callgraph_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new view

   package Generic_View is new Generic_Views
     (Module_Name => "Callgraph_View",
      View_Name   => "Call Graph View",
      View_Record => Callgraph_View_Record);
   subtype Callgraph_View_Access is Generic_View.View_Access;

   type Entity_Calls_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Search for the entities called by the current entity in Context

   type Entity_Called_By_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Search for th entities calling the current entity in Context

   type Ancestors_User_Data is new Commands_User_Data_Record with record
      View        : Callgraph_View_Access;
      Entity_Iter : Gtk_Tree_Iter;
   end record;
   type Ancestors_User_Data_Access is access all Ancestors_User_Data'Class;
   procedure Destroy
     (Data : in out Ancestors_User_Data; Cancelled : Boolean);
   function On_Entity_Found
     (Data        : access Ancestors_User_Data;
      Entity      : Entities.Entity_Information;
      Parent      : Entities.Entity_Information;
      Ref         : Entities.Entity_Reference;
      Is_Renaming : Boolean) return Boolean;
   --  See inherited documentation

   function Insert_Entity
     (View        : access Callgraph_View_Record'Class;
      Parent      : Entity_Information;
      Entity      : Entity_Information;
      Suffix      : String := "";
      Parent_Iter : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter;
   --  Insert an entry for Entity in the tree.
   --  Parent_Iter is the parent node for the node representing the entity.
   --  Parent is the caller of the entity.

   procedure Insert_Ref
     (View   : access Callgraph_View_Record'Class;
      Ref    : Entity_Reference;
      Parent : Gtk_Tree_Iter);
   --  Insert a reference in the tree

   procedure Add_Locations_Iter
     (View   : access Callgraph_View_Record'Class;
      Parent : Entity_Information;
      Entity : Entity_Information;
      Iter   : Gtk_Tree_Iter);
   --  Add the locations node to Iter. This node is associated with the
   --  entity, called by Parent.

   procedure Compute_Called_Entities
     (View        : Callgraph_View_Access;
      Entity      : Entity_Information;
      Entity_Iter : Gtk_Tree_Iter);
   --  Display the list of entities called by Entity. The View must have the
   --  right type

   procedure Compute_Calling_Entities
     (View        : Callgraph_View_Access;
      Entity      : Entity_Information;
      Entity_Iter : Gtk_Tree_Iter);
   --  Display the list of entities calling by Entity. The View must have the
   --  right type

   procedure On_Row_Expanded
     (View : access Gtk_Widget_Record'Class; Args : GValues);
   --  Called when a row is expanded by the user

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Context factory when creating contextual menus

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event

   procedure Hide_Show_Locations (View  : access Gtk_Widget_Record'Class);
   --  Hide or show the locations

   -------------------------
   -- Hide_Show_Locations --
   -------------------------

   procedure Hide_Show_Locations (View  : access Gtk_Widget_Record'Class) is
      V : constant Callgraph_View_Access := Callgraph_View_Access (View);
      M : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Show : constant Boolean :=
        Get_History (Get_History (V.Kernel).all, History_Show_Locations);

      procedure Remove_Locations (Iter : Gtk_Tree_Iter);
      --  Remove the locations in Iter and its children

      procedure Add_Locations
        (Parent : Entity_Information; Parent_Iter : Gtk_Tree_Iter);
      --  Add the locations nodes

      ----------------------
      -- Remove_Locations --
      ----------------------

      procedure Remove_Locations (Iter : Gtk_Tree_Iter) is
         It, It2 : Gtk_Tree_Iter;
         Path    : Gtk_Tree_Path;
      begin
         Iter_Copy (Source => Iter, Dest => It);
         while It /= Null_Iter loop
            if Get_String (M, It, Name_Column) = Called_At_Label then
               Iter_Copy (Source => It, Dest => It2);
               Next (M, It);
               Remove (M, It2);
            else
               Path := Get_Path (M, It);

               if Children (M, It) /= Null_Iter
                 and then Row_Expanded (V.Tree, Path)
               then
                  Remove_Locations (Children (M, It));
               end if;

               Path_Free (Path);
               Next (M, It);
            end if;
         end loop;
      end Remove_Locations;

      -------------------
      -- Add_Locations --
      -------------------

      procedure Add_Locations
        (Parent : Entity_Information; Parent_Iter : Gtk_Tree_Iter)
      is
         Iter   : Gtk_Tree_Iter := Children (M, Parent_Iter);
         Value  : GValue;
         Entity : Entity_Information;
      begin
         while Iter /= Null_Iter loop
            Get_Value (M, Iter, Entity_Column, Value);
            Entity := From_GValue (Value);
            Unset (Value);

            if Entity /= null then
               Add_Locations_Iter (V, Parent, Entity, Iter);
               Add_Locations (Entity, Iter);
            end if;

            Next (M, Iter);
         end loop;
      end Add_Locations;

      Parent_Iter : Gtk_Tree_Iter;
      Value       : GValue;
   begin
      if Show then
         Parent_Iter := Get_Iter_First (M);
         while Parent_Iter /= Null_Iter loop
            Get_Value (M, Parent_Iter, Entity_Column, Value);
            Add_Locations (From_GValue (Value), Parent_Iter);
            Unset (Value);
            Next (M, Parent_Iter);
         end loop;

      else
         Remove_Locations (Get_Iter_First (M));
      end if;
   end Hide_Show_Locations;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      V        : constant Callgraph_View_Access :=
        Callgraph_View_Access (View);
      Kernel   : constant Kernel_Handle := V.Kernel;
      Model    : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (V.Tree));
      Iter     : Gtk_Tree_Iter;
      Value    : GValue;
      File     : Virtual_File;
      Entity   : Entity_Information;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);
      if Iter /= Null_Iter then
         Get_Value (Model, Iter, File_Column, Value);
         File := Get_File (Value);
         Unset (Value);

         if File /= VFS.No_File then
            Get_Value (Model, Parent (Model, Parent (Model, Iter)),
                       Entity_Column, Value);
            Entity := From_GValue (Value);
            Unset (Value);

            Open_File_Editor
              (Kernel,
               Filename   => File,
               Line       => Natural (Get_Int (Model, Iter, Line_Column)),
               Column     => Natural (Get_Int (Model, Iter, Column_Column)),
               Column_End => Natural (Get_Int (Model, Iter, Column_Column))
                  + Get_Name (Entity)'Length);
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
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (View : access Gtk_Widget_Record'Class;
      Args : GValues)
   is
      V      : constant Callgraph_View_Access := Callgraph_View_Access (View);
      M      : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Iter, Child : Gtk_Tree_Iter := Null_Iter;
      Value       : GValue;
      Entity      : Entity_Information;
      Column      : constant Gint := Freeze_Sort (M);
   begin
      Get_Tree_Iter (Nth (Args, 1), Iter);

      Get_Value (M, Iter, Entity_Column, Value);
      Entity := From_GValue (Value);
      Unset (Value);

      --  If we have the locations node, do nothing
      if Entity = null then
         return;
      end if;

      --  We always recompute the call graph. gtk+ would lose the expanded
      --  status of children anyway, so we might as well recompute everything.
      --  It is also more logical from the user's point of view that this would
      --  act as a refresh.
      --  Keep one child for the moment, or the expanded status is lost by gtk+

      while N_Children (M, Iter) > 1 loop
         Child := Children (M, Iter);
         if Get_String (M, Child, Name_Column) = Called_At_Label then
            Next (M, Child);
         end if;
         Remove (M, Child);
      end loop;

      case V.Typ is
         when View_Any       => null;
         when View_Calls     => Compute_Called_Entities  (V, Entity, Iter);
         when View_Called_By =>
            Compute_Calling_Entities (V, Entity, Iter);
            Append (M, Child, Iter);
            Set (M, Child, Name_Column, Computing_Label);
      end case;

      --  Remove the locations node if we have a toplevel entity

      if Parent (M, Iter) = Null_Iter
        or else not Get_History
          (Get_History (V.Kernel).all, History_Show_Locations)
      then
         Child := Children (M, Iter);
         Remove (M, Child);
      end if;

      Thaw_Sort (M, Column);

   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception: "
                & Exception_Information (E));
         Thaw_Sort (M, Column);
   end On_Row_Expanded;

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
      V     : constant Callgraph_View_Access := Callgraph_View_Access (Object);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Iter  : Gtk_Tree_Iter;
      Context : Entity_Selection_Context_Access;
      Entity  : Entity_Information;
      Value   : GValue;
      Check   : Gtk_Check_Menu_Item;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);
      if Iter /= Null_Iter then
         Select_Iter (Get_Selection (V.Tree), Iter);

         Get_Value (Get_Model (V.Tree), Iter, Entity_Column, Value);
         Entity := From_GValue (Value);
         Unset (Value);

         Context := new Entity_Selection_Context;
         Set_File_Information   (Context, File => VFS.No_File);
         Set_Entity_Information (Context, Entity => Entity);
      else
         Unselect_All (Get_Selection (V.Tree));
      end if;

      if Menu /= null then
         Gtk_New (Check, Label => -"Show locations");
         Associate
           (Get_History (Kernel).all, History_Show_Locations, Check);
         Prepend (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled", Hide_Show_Locations'Access, V);
      end if;
      return Selection_Context_Access (Context);
   end View_Context_Factory;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Callgraph_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Names : GNAT.OS_Lib.String_List :=
        (1 => new String'(-"Name"),
         2 => new String'(-"Declaration"));
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Gtk.Scrolled_Window.Initialize (View);
      Set_Policy (View, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (Name_Column   => GType_String,
                                Decl_Column   => GType_String,
                                Entity_Column => Get_Entity_Information_Type,
                                File_Column   => Get_Virtual_File_Type,
                                Line_Column   => GType_Int,
                                Column_Column => GType_Int),
         Column_Names       => Names,
         Show_Column_Titles => True,
         Sortable_Columns   => True,
         Initial_Sort_On    => Names'First);
      Add (View, View.Tree);

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
         ID              => Generic_View.Get_Module,
         Context_Func    => View_Context_Factory'Access);

      Widget_Callback.Object_Connect
        (View.Tree, "row_expanded",
         On_Row_Expanded'Access, Slot_Object => View);

      Free (Names);
   end Initialize;

   ------------------------
   -- Add_Locations_Iter --
   ------------------------

   procedure Add_Locations_Iter
     (View   : access Callgraph_View_Record'Class;
      Parent : Entity_Information;
      Entity : Entity_Information;
      Iter   : Gtk_Tree_Iter)
   is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));
      Locations : Gtk_Tree_Iter;
      Refs  : Entity_Reference_Iterator;
      Ref   : Entity_Reference;
   begin
      Append (Model, Locations, Iter);
      Set (Model, Locations, Name_Column, Called_At_Label);

      if Parent /= null
        and then Get_History
          (Get_History (View.Kernel).all, History_Show_Locations)
      then
         Find_All_References
           (Iter     => Refs,
            Entity   => Entity,
            In_Scope => Parent);
         while not At_End (Refs) loop
            Ref := Get (Refs);
            if Ref /= No_Entity_Reference
              and then Show_In_Call_Graph (Get_Kind (Ref))
              and then Get_Caller (Ref) = Parent
              and then Is_Subprogram (Get_Entity (Refs))
              and then Get_Declaration_Of (Entity) /= Get_Location (Ref)
            then
               Insert_Ref (View, Ref, Locations);
            end if;

            Next (Refs);
         end loop;
         Destroy (Refs);
      end if;
   end Add_Locations_Iter;

   -------------------
   -- Insert_Entity --
   -------------------

   function Insert_Entity
     (View        : access Callgraph_View_Record'Class;
      Parent      : Entity_Information;
      Entity      : Entity_Information;
      Suffix      : String := "";
      Parent_Iter : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter
   is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));
      Iter  : Gtk_Tree_Iter;
      Decl  : constant File_Location := Get_Declaration_Of (Entity);
   begin
      Append (Model, Iter, Parent_Iter);
      Set (Model, Iter, Name_Column, Get_Name (Entity).all & Suffix);
      Set (Model, Iter, Decl_Column,
           Base_Name (Get_Filename (Get_File (Decl)))
           & ':' & Image (Get_Line (Decl))
           & ':' & Image (Get_Column (Decl)));
      Set_Value (Model, Iter, Entity_Column, To_GValue (Entity));

      --  Append a dummy child, so that the parent can be expanded to show its
      --  called entities. This child will be used to collect the locations
      --  where the entity occurs

      Add_Locations_Iter (View, Parent, Entity, Iter);
      return Iter;
   end Insert_Entity;

   ----------------
   -- Insert_Ref --
   ----------------

   procedure Insert_Ref
     (View   : access Callgraph_View_Record'Class;
      Ref    : Entity_Reference;
      Parent : Gtk_Tree_Iter)
   is
      Model   : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));
      Iter   : Gtk_Tree_Iter;
      File   : constant Virtual_File :=
        Get_Filename (Get_File (Get_Location (Ref)));
      Line   : constant Integer := Get_Line (Get_Location (Ref));
      Column : constant Integer := Get_Column (Get_Location (Ref));
      Value  : GValue;
      L      : constant String := "0000" & Image (Line);
      C      : constant String := "0000" & Image (Column);
   begin
      Set_File (Value, File);

      Append (Model, Iter, Parent);
      Set (Model, Iter, Name_Column,
           L (L'Last - 3 .. L'Last) & ':' & C (C'Last - 3 .. C'Last));
      Set_Value (Model, Iter, File_Column,   Value);
      Set (Model, Iter, Line_Column,   Gint (Line));
      Set (Model, Iter, Column_Column, Gint (Column));

      Unset (Value);
   end Insert_Ref;

   -----------------------------
   -- Compute_Called_Entities --
   -----------------------------

   procedure Compute_Called_Entities
     (View        : Callgraph_View_Access;
      Entity      : Entity_Information;
      Entity_Iter : Gtk_Tree_Iter)
   is
      Calls       : Calls_Iterator;
      Called_E    : Entity_Information;
      Iter        : Gtk_Tree_Iter;
      pragma Unreferenced (Iter);
   begin
      if View.Typ = View_Called_By then
         Trace (Me, "Trying to display calls by in a ""called by"" view");
         return;
      end if;

      if Entity /= null then
         Push_State (View.Kernel, Busy);
         View.Typ := View_Calls;

         Calls := Get_All_Called_Entities (Entity);
         while not At_End (Calls) loop
            Called_E := Get (Calls);

            if Called_E /= null
              and then Is_Subprogram (Called_E)
            then
               Iter := Insert_Entity (View, Entity, Called_E, "", Entity_Iter);
            end if;

            Next (Calls);
         end loop;

         Destroy (Calls);
         Pop_State (View.Kernel);
      end if;
   end Compute_Called_Entities;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Data : in out Ancestors_User_Data; Cancelled : Boolean)
   is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Data.View.Tree));
      Child : Gtk_Tree_Iter;
   begin
      if not Cancelled then
         Child := Children (Model, Data.Entity_Iter);
         while Child /= Null_Iter loop
            if Get_String (Model, Child, Name_Column) = Computing_Label then
               Remove (Model, Child);
               exit;
            end if;
            Next (Model, Child);
         end loop;
      end if;
   end Destroy;

   ---------------------
   -- On_Entity_Found --
   ---------------------

   function On_Entity_Found
     (Data        : access Ancestors_User_Data;
      Entity      : Entities.Entity_Information;
      Parent      : Entities.Entity_Information;
      Ref         : Entities.Entity_Reference;
      Is_Renaming : Boolean) return Boolean
   is
      Iter : Gtk_Tree_Iter;
      pragma Unreferenced (Ref, Is_Renaming, Iter);
   begin
      Trace (Me, "MANU On_Entity_Found " & Get_Name (Parent).all);
      Iter := Insert_Entity
        (Data.View, Entity, Parent, "", Data.Entity_Iter);
      return True;
   end On_Entity_Found;

   ------------------------------
   -- Compute_Calling_Entities --
   ------------------------------

   procedure Compute_Calling_Entities
     (View        : Callgraph_View_Access;
      Entity      : Entity_Information;
      Entity_Iter : Gtk_Tree_Iter)
   is
      Data : Ancestors_User_Data_Access;
   begin
      if View.Typ = View_Calls then
         Trace (Me, "Trying to display called by in a ""calls"" view");
         return;
      end if;

      if Entity /= null then
         Push_State (View.Kernel, Busy);
         Data := new Ancestors_User_Data'
           (Commands_User_Data_Record with
            View        => View,
            Entity_Iter => Entity_Iter);

         Examine_Ancestors_Call_Graph
           (Kernel          => View.Kernel,
            Entity          => Entity,
            User_Data       => Data,
            Watch           => Gtk_Widget (View),
            Background_Mode => True);

         Pop_State (View.Kernel);
      end if;
   end Compute_Calling_Entities;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant Callgraph_View_Access :=
        Generic_View.Get_Or_Create_View (Kernel, False);
      Entity : constant Entity_Information := Get_Entity
        (Entity_Selection_Context_Access (Context.Context),
         Ask_If_Overloaded => True);
      pragma Unreferenced (Command);
   begin
      View.Typ := View_Calls;
      Expand_Row (View.Tree, Insert_Entity (View, null, Entity, -" calls "));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant Callgraph_View_Access :=
        Generic_View.Get_Or_Create_View (Kernel, False);
      Entity : constant Entity_Information := Get_Entity
        (Entity_Selection_Context_Access (Context.Context),
         Ask_If_Overloaded => True);
      pragma Unreferenced (Command);
   begin
      View.Typ := View_Called_By;
      Expand_Row
        (View.Tree, Insert_Entity (View, null, Entity, -" called by "));
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter  : Action_Filter;
      Command : Interactive_Command_Access;
   begin
      Generic_View.Register_Module (Kernel);

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, History_Show_Locations, True);

      Filter := Lookup_Filter (Kernel, "Entity is container");

      Command := new Entity_Calls_Command;
      Register_Contextual_Menu
        (Kernel, "Entity calls",
         Label    => "References/%e calls (in tree)",
         Ref_Item => "Entity calls in browser",
         Filter   => Filter,
         Action   => Command);

      Command := new Entity_Called_By_Command;
      Register_Contextual_Menu
        (Kernel, "Entity called by",
         Label    => "References/%e is called by (in tree)",
         Ref_Item => "Entity called by in browser",
         Filter   => Filter,
         Action   => Command);
   end Register_Module;

end Call_Graph_Views;
