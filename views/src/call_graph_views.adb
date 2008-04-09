-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

with GNAT.Strings;                use GNAT.Strings;

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Gdk.Event;                   use Gdk.Event;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Types.Keysyms;           use Gdk.Types.Keysyms;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Paned;                   use Gtk.Paned;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_Selection;          use Gtk.Tree_Selection;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.List_Store;              use Gtk.List_Store;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;

with Basic_Types;                 use Basic_Types;
with Commands.Interactive;        use Commands, Commands.Interactive;
with Entities.Commands;           use Entities.Commands;
with Entities;                    use Entities;
with Entities.Values;             use Entities.Values;
with Generic_Views;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;          use GPS.Kernel.Modules;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks;   use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                    use GPS.Intl;
with GUI_Utils;                   use GUI_Utils;
with Histories;                   use Histories;
with String_Utils;                use String_Utils;
with Traces;                      use Traces;
with VFS;                         use VFS;
with VFS.Values;                  use VFS.Values;
with Glib.Xml_Int;                use Glib.Xml_Int;

with Generic_List;
with System;
with Ada.Unchecked_Conversion;

package body Call_Graph_Views is

   ---------------
   -- Constants --
   ---------------

   Name_Column    : constant := 0;
   Decl_Column    : constant := 1;
   Entity_Column  : constant := 2;
   File_Column    : constant := 3;
   Line_Column    : constant := 4;
   Column_Column  : constant := 5;
   List_Column    : constant := 6;

   Location_Line_Column      : constant := 0;
   Location_Column_Column    : constant := 1;
   Location_Character_Column : constant := 2;
   Location_String_Column    : constant := 3;
   Location_File_Column      : constant := 4;

   History_Show_Locations : constant History_Key :=
                              "Call_Graph_Show_Locations";

   Computing_Label : constant String := "computing...";
   --  Label used while computing the ancestors call graph

   -----------------
   -- Local types --
   -----------------

   type View_Type is (View_Calls, View_Called_By);

   type Callgraph_View_Record is new Generic_Views.View_Record with record
      Tree   : Gtk_Tree_View;
      Kernel : Kernel_Handle;
      Typ    : View_Type := View_Calls;

      Show_Locations : Boolean := True;
      --  Whether we should show the locations in the call graph

      Block_On_Expanded : Boolean := False;
      --  If true, we do not recompute the contents of children nodes when a
      --  node is expanded

      Locations_Tree  : Gtk_Tree_View;
      Locations_Model : Gtk_List_Store;

      Pane            : Gtk_Hpaned;
   end record;

   type Reference_Record is record
      Line   : Integer;
      Column : Visible_Column_Type;
      File   : VFS.Virtual_File;
   end record;

   procedure Free (X : in out Reference_Record);
   --  Free memory associated to X

   package Reference_List is new Generic_List (Reference_Record, Free);
   use Reference_List;

   type List_Access is access List;

   function To_Reference_List is new Ada.Unchecked_Conversion
     (System.Address, List_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (List_Access, System.Address);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Locations_List
     (View   : access Callgraph_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Create : Boolean := False) return List_Access;
   --  Get the locations list associated with Iter. Create it if necessary

   function To_Record (Ref : Entity_Reference) return Reference_Record;
   --  Extract the relevant information from Ref

   function To_XML (R : Reference_Record) return Node_Ptr;
   function From_XML (N : Node_Ptr) return Reference_Record;
   --  Conversion functions

   function Save_To_XML
     (View : access Callgraph_View_Record) return Glib.Xml_Int.Node_Ptr;
   procedure Load_From_XML
     (View : access Callgraph_View_Record; XML : Glib.Xml_Int.Node_Ptr);
   procedure Initialize
     (View   : access Callgraph_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new view

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Callgraph_View",
      View_Name          => "Call Tree",
      Formal_View_Record => Callgraph_View_Record);
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
      View           : Callgraph_View_Access;
      Entity_Iter    : Gtk_Tree_Iter;
      Computing_Iter : Gtk_Tree_Iter := Null_Iter;  --  "computing..." node
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
      Ref         : Entity_Reference;
      Suffix      : String := "";
      Parent_Iter : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter;
   --  Insert an entry for Entity in the tree.
   --  Parent_Iter is the parent node for the node representing the entity.
   --  Parent is the caller of the entity.

   procedure On_Row_Expanded
     (View : access Gtk_Widget_Record'Class; Args : GValues);
   --  Called when a row is expanded by the user

   procedure On_Selection_Changed (View : access Gtk_Widget_Record'Class);
   --  Called when the selection changes in the view

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Context factory when creating contextual menus

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the main tree

   function Button_Press_On_List
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the locations list

   function On_Key_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "key_press" event

   procedure Select_Current_Location
     (View : access Callgraph_View_Record'Class);
   --  Open an editor to the current location

   procedure Open_Selected_Value
     (View : access Callgraph_View_Record'Class);
   --  Open the value currently selected in the main tree

   ---------------
   -- To_Record --
   ---------------

   function To_Record (Ref : Entity_Reference) return Reference_Record is
   begin
      return (Get_Line (Get_Location (Ref)),
              Get_Column (Get_Location (Ref)),
              Get_Filename (Get_File (Get_Location (Ref))));
   end To_Record;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_Record) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   -----------------------------
   -- Select_Current_Location --
   -----------------------------

   procedure Select_Current_Location
     (View : access Callgraph_View_Record'Class)
   is
      Iter   : Gtk_Tree_Iter;
      Model  : Gtk_Tree_Model;
      File   : VFS.Virtual_File;
      Value  : GValue;
      Entity : Entity_Information;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter /= Null_Iter then
         if Parent (Model, Iter) = Null_Iter then
            Open_Selected_Value (View);
         else
            --  The entity to highlight depends on the type of the view
            case View.Typ is
            when View_Calls =>
               Get_Value (Model, Iter, Entity_Column, Value);
            when View_Called_By =>
               Get_Value (Model, Parent (Model, Iter), Entity_Column, Value);
            end case;

            Entity := From_GValue (Value);
            Unset (Value);

            Get_Selected (Get_Selection (View.Locations_Tree), Model, Iter);

            if Iter /= Null_Iter then
               File := Create (Get_String (Model, Iter, Location_File_Column));

               Open_File_Editor
                 (View.Kernel,
                  Filename   => File,
                  Line       => Natural
                    (Get_Int (Model, Iter, Location_Line_Column)),
                  Column     => Visible_Column_Type
                    (Get_Int (Model, Iter, Location_Column_Column)),
                  Column_End => Visible_Column_Type
                    (Get_Int (Model, Iter, Location_Column_Column))
                  + Get_Name (Entity)'Length,
                  New_File   => False,
                  Focus => False);
            end if;
         end if;
      end if;
   end Select_Current_Location;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      V     : Callgraph_View_Access;
      Key   : Gdk_Key_Type;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

      procedure Strong_Select (T : Gtk_Tree_View; I : Gtk_Tree_Iter);
      --  Select and place the keyboard focus on I

      procedure Select_In_Base_Tree (Going_Down : Boolean := True);
      --  Select the next iter in the base tree, with the given direction

      -------------------
      -- Strong_Select --
      -------------------

      procedure Strong_Select (T : Gtk_Tree_View; I : Gtk_Tree_Iter) is
         Path : Gtk_Tree_Path;
      begin
         Select_Iter (Get_Selection (T), I);
         Path := Get_Path (Get_Model (T), I);
         Set_Cursor (T, Path, null, False);
         Path_Free (Path);
      end Strong_Select;

      -------------------------
      -- Select_In_Base_Tree --
      -------------------------

      procedure Select_In_Base_Tree (Going_Down : Boolean := True) is
         Selected : Gtk_Tree_Iter;
         Model    : Gtk_Tree_Model;
         New_Iter : Gtk_Tree_Iter;
         Path     : Gtk_Tree_Path;
         Result   : Boolean;
      begin
         Get_Selected (Get_Selection (V.Tree), Model, Selected);

         if Selected /= Null_Iter then
            Iter_Copy (Selected, New_Iter);

            Path := Get_Path (Model, New_Iter);

            if Going_Down then
               if Row_Expanded (V.Tree, Path) then
                  New_Iter := Children (Model, New_Iter);
               else
                  Next (Model, New_Iter);

                  while New_Iter = Null_Iter and then Up (Path)
                    and then Get_Depth (Path) > 0
                  loop
                     New_Iter := Get_Iter (Model, Path);
                     Next (Model, New_Iter);
                  end loop;
               end if;

            else
               Result := Prev (Path);

               if Result then
                  if Row_Expanded (V.Tree, Path) then
                     while Row_Expanded (V.Tree, Path) loop
                        Down (Path);
                     end loop;

                     while Get_Iter (Model, Path) /= Null_Iter loop
                        New_Iter := Get_Iter (Model, Path);
                        Next (Path);
                     end loop;
                  else
                     New_Iter := Get_Iter (Model, Path);
                  end if;
               else
                  New_Iter := Parent (Model, Selected);
               end if;

            end if;

            Path_Free (Path);

            if New_Iter /= Null_Iter then
               Strong_Select (V.Tree, New_Iter);

               if Going_Down then
                  New_Iter := Get_Iter_First (V.Locations_Model);

               else
                  if Children (V.Locations_Model, Null_Iter) /= Null_Iter then
                     New_Iter := Nth_Child
                       (V.Locations_Model, Null_Iter,
                        N_Children (V.Locations_Model) - 1);
                  else
                     New_Iter := Null_Iter;
                  end if;
               end if;

               if New_Iter /= Null_Iter then
                  Strong_Select (V.Locations_Tree, New_Iter);
                  Grab_Focus (V.Locations_Tree);
               end if;
            end if;
         end if;
      end Select_In_Base_Tree;

      Path   : Gtk_Tree_Path;
      Result : Boolean;
   begin
      V := Callgraph_View_Access (View);

      Key := Get_Key_Val (Event);

      case Key is
         when GDK_Down | GDK_KP_Down =>
            Get_Selected (Get_Selection (V.Locations_Tree), Model, Iter);

            if Iter = Null_Iter then
               Select_In_Base_Tree (Going_Down => True);
            else
               Next (Model, Iter);

               if Iter = Null_Iter then
                  Select_In_Base_Tree (Going_Down => True);
               else
                  Strong_Select (V.Locations_Tree, Iter);
                  Grab_Focus (V.Locations_Tree);
               end if;
            end if;

            return True;

         when GDK_Up | GDK_KP_Up =>
            Get_Selected (Get_Selection (V.Locations_Tree), Model, Iter);

            if Iter = Null_Iter then
               Select_In_Base_Tree (Going_Down => False);
            else
               Path := Get_Path (Model, Iter);

               Result := Prev (Path);

               if Result then
                  Strong_Select (V.Locations_Tree, Get_Iter (Model, Path));
                  Grab_Focus (V.Locations_Tree);
               else
                  Select_In_Base_Tree (Going_Down => False);
               end if;

               Path_Free (Path);
            end if;

            return True;

         when GDK_Right | GDK_KP_Right =>
            Get_Selected (Get_Selection (V.Tree), Model, Iter);

            if Iter /= Null_Iter then
               Path := Get_Path (Model, Iter);
               if Row_Expanded (V.Tree, Path) then
                  Iter := Children (Model, Iter);

                  if Iter /= Null_Iter then
                     Strong_Select (V.Tree, Iter);
                  end if;
               else
                  Result := Expand_Row (V.Tree, Path, False);
               end if;
               Path_Free (Path);
            end if;

            return True;

         when GDK_Left | GDK_KP_Left =>
            Get_Selected (Get_Selection (V.Tree), Model, Iter);

            if Iter /= Null_Iter then
               Path := Get_Path (Model, Iter);

               if Row_Expanded (V.Tree, Path) then
                  Result := Collapse_Row (V.Tree, Path);
               else
                  if Up (Path) then
                     Result := Collapse_Row (V.Tree, Path);
                     if Result then
                        Strong_Select (V.Tree, Get_Iter (Model, Path));
                     end if;
                  end if;
               end if;

               Path_Free (Path);
            end if;

            return True;

         when GDK_Return =>
            Select_Current_Location (V);

            return True;
         when others =>
            return False;
      end case;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Key_Press;

   -------------------------
   -- Open_Selected_Value --
   -------------------------

   procedure Open_Selected_Value
     (View : access Callgraph_View_Record'Class)
   is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

      Value  : GValue;
      Entity : Entity_Information;
      Loc    : File_Location;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter /= Null_Iter then
         Get_Value (Model, Iter, Entity_Column, Value);
         Entity := From_GValue (Value);
         Unset (Value);

         Loc := Get_Declaration_Of (Entity);

         Open_File_Editor
           (View.Kernel,
            Filename   => Get_Filename (Get_File (Loc)),
            Line       => Get_Line (Loc),
            Column     => Get_Column (Loc),
            Column_End => Get_Column (Loc) + Get_Name (Entity)'Length,
            Focus => False);
      end if;
   end Open_Selected_Value;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View  : Callgraph_View_Access;
   begin
      View := Callgraph_View_Access (Widget);

      if Get_Event_Type (Event) = Gdk_2button_Press then
         Open_Selected_Value (View);
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press;

   --------------------------
   -- Button_Press_On_List --
   --------------------------

   function Button_Press_On_List
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View  : Callgraph_View_Access;
      Iter  : Gtk_Tree_Iter;
   begin
      View := Callgraph_View_Access (Widget);

      if Get_Event_Type (Event) = Button_Press then
         Iter := Find_Iter_For_Event
           (View.Locations_Tree, View.Locations_Model, Event);

         if Iter /= Null_Iter then
            Select_Iter (Get_Selection (View.Locations_Tree), Iter);
            Select_Current_Location (View);
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press_On_List;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (View : access Gtk_Widget_Record'Class)
   is
      V : constant Callgraph_View_Access := Callgraph_View_Access (View);
      L : List_Access;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

      Value : GValue;
      N     : List_Node;
      R     : Reference_Record;
      T     : Gtk_Tree_Iter;
      Address : System.Address;
      Appended : Boolean := False;
      use type System.Address;
   begin
      Get_Selected (Get_Selection (V.Tree), Model, Iter);

      if Iter /= Null_Iter then
         Clear (V.Locations_Model);

         Get_Value (Model, Iter, List_Column, Value);
         Address := Get_Address (Value);

         if Address /= System.Null_Address then
            L := To_Reference_List (Address);

            N := First (L.all);

            while N /= Null_Node loop
               R := Data (N);

               Append (V.Locations_Model, T);
               Appended := True;

               Set (V.Locations_Model, T, Location_Line_Column,
                    Gint (R.Line));
               Set (V.Locations_Model, T, Location_Column_Column,
                    Gint (R.Column));
               Set (V.Locations_Model, T, Location_Character_Column, ":");
               Set (V.Locations_Model, T, Location_String_Column,
                    "    " & Base_Name (R.File));
               Set (V.Locations_Model, T, Location_File_Column,
                    Full_Name (R.File).all);

               N := Next (N);
            end loop;
         end if;

         if Appended then
            Select_Iter
              (Get_Selection (V.Locations_Tree),
               Get_Iter_First (V.Locations_Model));
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Selection_Changed;

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
      Dummy  : Gtk_Tree_Iter;
      Value  : GValue;
      Entity : Entity_Information;
      Column : Gint;
      Data   : Ancestors_User_Data_Access;
   begin
      if V.Block_On_Expanded then
         return;
      end if;

      Get_Tree_Iter (Nth (Args, 1), Iter);

      Get_Value (M, Iter, Entity_Column, Value);
      Entity := From_GValue (Value);
      Unset (Value);

      --  If we have the locations node, do nothing
      if Entity = null then
         return;
      end if;

      Column := Freeze_Sort (M);

      --  We always recompute the call graph. gtk+ would lose the expanded
      --  status of children anyway, so we might as well recompute everything.
      --  It is also more logical from the user's point of view that this would
      --  act as a refresh.
      --  Keep one child for the moment, or the expanded status is lost by gtk+

      Prepend (M, Child, Iter);
      Iter_Copy (Child, Dummy);
      Next (M, Dummy);

      while Dummy /= Null_Iter loop
         Remove (M, Dummy);
         Iter_Copy (Child, Dummy);
         Next (M, Dummy);
      end loop;

      Data := new Ancestors_User_Data'
        (Commands_User_Data_Record with
         View           => V,
         Computing_Iter => Null_Iter,
         Entity_Iter    => Iter);
      Append (M, Data.Computing_Iter, Iter);
      Set (M, Data.Computing_Iter, Name_Column, Computing_Label);

      case V.Typ is
         when View_Calls     =>
            Examine_Entity_Call_Graph
              (Kernel          => V.Kernel,
               Entity          => Entity,
               User_Data       => Data,
               Get_All_Refs    => True);

         when View_Called_By =>
            Examine_Ancestors_Call_Graph
              (Kernel          => V.Kernel,
               Entity          => Entity,
               User_Data       => Data,
               Watch           => Gtk_Widget (V),
               Background_Mode => True);
      end case;

      --  Remove the dummy node

      Child := Children (M, Iter);
      Remove (M, Child);

      Thaw_Sort (M, Column);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Thaw_Sort (M, Column);
   end On_Row_Expanded;

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
      pragma Unreferenced (Event_Widget, Kernel, Menu);
      V       : constant Callgraph_View_Access :=
                  Callgraph_View_Access (Object);
      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (V.Tree));
      Iter    : Gtk_Tree_Iter;
      Entity  : Entity_Information;
      Value   : GValue;
   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);
      if Iter /= Null_Iter then
         Select_Iter (Get_Selection (V.Tree), Iter);

         Get_Value (Get_Model (V.Tree), Iter, Entity_Column, Value);
         Entity := From_GValue (Value);
         Unset (Value);

         if Entity /= null then
            Set_File_Information   (Context, File => VFS.No_File);
            Set_Entity_Information (Context, Entity => Entity);
         end if;
      else
         Unselect_All (Get_Selection (V.Tree));
      end if;
   end View_Context_Factory;

   ------------
   -- To_XML --
   ------------

   function To_XML (R : Reference_Record) return Node_Ptr is
      Result : Node_Ptr;
   begin
      Result := new Node;
      Result.Tag := new String'("loc");
      Set_Attribute (Result, "line", R.Line'Img);
      Set_Attribute (Result, "column", R.Column'Img);
      Set_Attribute (Result, "file", Full_Name (R.File).all);
      return Result;
   end To_XML;

   --------------
   -- From_XML --
   --------------

   function From_XML (N : Node_Ptr) return Reference_Record is
      Result : Reference_Record;
   begin
      Result.Line := Integer'Value (Get_Attribute (N, "line"));
      Result.Column := Visible_Column_Type'Value (Get_Attribute (N, "column"));
      Result.File := Create (Get_Attribute (N, "file"));
      return Result;
   end From_XML;

   ------------------------
   -- Get_Locations_List --
   ------------------------

   function Get_Locations_List
     (View   : access Callgraph_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Create : Boolean := False) return List_Access
   is
      L_Value : GValue;
      L       : List_Access;
      Addr    : System.Address;

      use type System.Address;
   begin
      Get_Value (Get_Model (View.Tree), Iter, List_Column, L_Value);
      Addr := Get_Address (L_Value);

      if Addr /= System.Null_Address then
         return To_Reference_List (Addr);

      elsif Create then
         L := new List;
         Addr := To_Address (L);
         Init (L_Value, GType_Pointer);
         Set_Address (L_Value, Addr);
         Set_Value
           (Gtk_Tree_Store (Get_Model (View.Tree)),
            Iter, List_Column, L_Value);

         return L;
      end if;

      return null;
   end Get_Locations_List;

   -----------------
   -- Save_To_XML --
   -----------------

   function Save_To_XML
     (View : access Callgraph_View_Record) return Glib.Xml_Int.Node_Ptr
   is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      Root  : Node_Ptr;

      procedure Recursive_Save
        (Parent_Iter : Gtk_Tree_Iter; Parent : Node_Ptr);
      --  Save Iter recursively

      --------------------
      -- Recursive_Save --
      --------------------

      procedure Recursive_Save
        (Parent_Iter : Gtk_Tree_Iter; Parent : Node_Ptr)
      is
         Iter   : Gtk_Tree_Iter;
         N      : Node_Ptr;
         Entity : Entity_Information;
         Value  : GValue;
         L      : List_Access;
         Node   : List_Node;
         Decl   : File_Location;
         Path   : Gtk_Tree_Path;

      begin
         if Parent_Iter = Null_Iter then
            Iter := Get_Iter_First (Model);
         else
            Iter := Children (Model, Parent_Iter);
         end if;

         while Iter /= Null_Iter loop
            Get_Value (Model, Iter, Entity_Column, Value);
            Entity := From_GValue (Value);
            Unset (Value);

            if Entity /= null then
               N := new Glib.Xml_Int.Node;
               N.Tag := new String'("entity");
               Add_Child (Parent, N, Append => True);

               Path := Get_Path (Model, Iter);
               if Row_Expanded (View.Tree, Path) then
                  Set_Attribute (N, "expanded", "true");
               end if;
               Path_Free (Path);

               Set_Attribute
                 (N, "name", Get_String (Model, Iter, Name_Column));
               Set_Attribute
                 (N, "decl", Get_String (Model, Iter, Decl_Column));

               Decl := Get_Declaration_Of (Entity);
               N.Tag := new String'("entity");
               Set_Attribute
                 (N, "entity_name", Get_Name (Entity).all);
               Set_Attribute
                 (N, "entity_decl",
                  Full_Name (Get_Filename (Get_File (Decl))).all);
               Set_Attribute (N, "entity_line", Image (Get_Line (Decl)));
               Set_Attribute
                 (N, "entity_column", Image (Integer (Get_Column (Decl))));

               L := Get_Locations_List (View, Iter, False);

               if L /= null then
                  Node := First (L.all);

                  while Node /= Null_Node loop
                     Glib.Xml_Int.Add_Child
                       (N, To_XML (Data (Node)), True);
                     Node := Next (Node);
                  end loop;
               end if;
            end if;

            Recursive_Save (Iter, N);

            Next (Model, Iter);
         end loop;
      end Recursive_Save;

   begin
      Root := new Node;
      Root.Tag := new String'("callgraph");
      case View.Typ is
         when View_Called_By =>
            Set_Attribute (Root, "type", "called_by");
         when View_Calls =>
            Set_Attribute (Root, "type", "calls");
      end case;

      Set_Attribute (Root, "position", Get_Position (View.Pane)'Img);

      Recursive_Save (Null_Iter, Root);
      return Root;
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   procedure Load_From_XML
     (View : access Callgraph_View_Record; XML : Glib.Xml_Int.Node_Ptr)
   is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));

      procedure Recursive_Load
        (Parent_Iter   : Gtk_Tree_Iter;
         Node          : Node_Ptr;
         Expand_Parent : Boolean);
      --  Load Node into Iter, and the children of Node.
      --  Expand_Parent should be set to True if the parent node should be
      --  expanded after a first child has been added to it. We unfortunately
      --  need this parameter, since otherwise a child node cannot be expanded
      --  before its parent has been expanded, and a parent cannot be expanded
      --  before it has at least one child

      --------------------
      -- Recursive_Load --
      --------------------

      procedure Recursive_Load
        (Parent_Iter   : Gtk_Tree_Iter;
         Node          : Node_Ptr;
         Expand_Parent : Boolean)
      is
         Entity : Entity_Information;
         File   : Virtual_File;
         Iter   : Gtk_Tree_Iter := Null_Iter;
         Dummy  : Gtk_Tree_Iter;
         Source : Source_File;
         N      : Node_Ptr := Node;
         L      : List_Access;
         Tmp    : Boolean;
         pragma Unreferenced (Tmp);
      begin
         while N /= null loop
            if N.Tag.all = "loc" then
               L := Get_Locations_List (View, Parent_Iter, True);
               Append (L.all, From_XML (N));

            else
               Append (Model, Iter, Parent_Iter);

               if Expand_Parent and then N = Node then
                  View.Block_On_Expanded := True;
                  Expand_Row (View.Tree, Parent_Iter);
                  View.Block_On_Expanded := False;
               end if;

               Set (Model, Iter, Name_Column, Get_Attribute (N, "name"));
               Set (Model, Iter, Decl_Column, Get_Attribute (N, "decl"));

               File := Create
                 (Full_Filename => Get_Attribute (N, "entity_decl"));
               Source := Get_Or_Create
                 (Db            => Get_Database (View.Kernel),
                  File          => File);
               if Source /= null then
                  Entity := Get_Or_Create
                    (Name => Get_Attribute (N, "entity_name"),
                     File => Source,
                     Line => Safe_Value (Get_Attribute (N, "entity_line")),
                     Column => Basic_Types.Visible_Column_Type
                       (Safe_Value (Get_Attribute (N, "entity_column"))));
               else
                  Entity := null;
               end if;

               Set_Value (Model, Iter, Entity_Column, To_GValue (Entity));
            end if;

            if N.Child /= null then
               Recursive_Load
                 (Iter, N.Child,
                  Expand_Parent => Get_Attribute (N, "expanded") = "true");
            end if;

            if Iter /= Null_Iter
              and then Children (Model, Iter) = Null_Iter
            then
               Append (Model, Dummy, Iter);
               Set (Model, Dummy, Name_Column, Computing_Label);
            end if;

            N := N.Next;
         end loop;
      end Recursive_Load;

   begin
      if Get_Attribute (XML, "type") = "calls" then
         View.Typ := View_Calls;
      else
         View.Typ := View_Called_By;
      end if;

      declare
         Pos_Str : constant String := Get_Attribute (XML, "position");
      begin
         if Pos_Str /= "" then
            Set_Position (View.Pane, Gint'Value (Pos_Str));
         end if;
      end;

      Recursive_Load (Null_Iter, XML.Child, False);
   end Load_From_XML;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Callgraph_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Names  : GNAT.Strings.String_List := (1 => new String'(-"Name"));
      Scroll : Gtk_Scrolled_Window;

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
                                Column_Column => GType_Int,
                                List_Column   => GType_Pointer),
         Column_Names       => Names,
         Show_Column_Titles => False,
         Sortable_Columns   => True,
         Initial_Sort_On    => Names'First);
      Set_Name (View.Tree, "Call Graph Tree");  --  For test suite
      Gtk_New_Hpaned (View.Pane);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add (Scroll, View.Tree);
      Add1 (View.Pane, Scroll);

      Add_With_Viewport (View, View.Pane);

      --  Create the lines list

      Gtk_New (View.Locations_Model,
               (Location_Line_Column      => GType_Int,
                Location_Column_Column    => GType_Int,
                Location_Character_Column => GType_String,
                Location_String_Column    => GType_String,
                Location_File_Column      => GType_String));
      Gtk_New (View.Locations_Tree, View.Locations_Model);
      Set_Headers_Visible (View.Locations_Tree, False);

      Set_Name (View.Locations_Tree, "Call Graph Location Tree");
      --  For test suite

      --  Create the locations model

      declare
         C     : Gtk_Cell_Renderer_Text;
         Dummy : Gint;
         pragma Unreferenced (Dummy);
         Col   : Gtk_Tree_View_Column;
      begin
         Gtk_New (Col);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_Line_Column);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_Character_Column);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_Column_Column);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_String_Column);

         Dummy := Append_Column (View.Locations_Tree, Col);
      end;

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add (Scroll, View.Locations_Tree);
      Add2 (View.Pane, Scroll);

      View.Show_Locations :=
        Get_History (Get_History (Kernel).all, History_Show_Locations);

      Modify_Font (View.Tree, Get_Pref (View_Fixed_Font));

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Return_Callback.Object_Connect
        (View.Locations_Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press_On_List'Access),
         Slot_Object => View,
         After       => False);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press'Access),
         Slot_Object => View,
         After       => False);

      Return_Callback.Object_Connect
        (View.Locations_Tree,
         Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press'Access),
         Slot_Object => View,
         After       => False);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Generic_View.Get_Module,
         Context_Func    => View_Context_Factory'Access);

      Widget_Callback.Object_Connect
        (View.Tree, Signal_Row_Expanded,
         On_Row_Expanded'Access, Slot_Object => View);

      Widget_Callback.Object_Connect
        (Get_Selection (View.Tree), Signal_Changed,
         Widget_Callback.To_Marshaller (On_Selection_Changed'Access),
         Slot_Object => View);

      Free (Names);
   end Initialize;

   -------------------
   -- Insert_Entity --
   -------------------

   function Insert_Entity
     (View        : access Callgraph_View_Record'Class;
      Parent      : Entity_Information;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Suffix      : String := "";
      Parent_Iter : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter
   is
      pragma Unreferenced (Parent);
      Model     : constant Gtk_Tree_Store :=
                    Gtk_Tree_Store (Get_Model (View.Tree));
      Decl      : constant File_Location := Get_Declaration_Of (Entity);
      Iter      : Gtk_Tree_Iter;
      Locations : Gtk_Tree_Iter := Null_Iter;
      Value     : GValue;
      Ent       : Entity_Information;
   begin
      if Parent_Iter = Null_Iter then
         Iter := Get_Iter_First (Model);
      else
         Iter := Children (Model, Parent_Iter);
      end if;

      --  Check whether the entity already exists in the call graph

      while Iter /= Null_Iter loop
         Get_Value (Model, Iter, Entity_Column, Value);
         Ent := From_GValue (Value);
         Unset (Value);

         exit when Ent = Entity;

         Next (Model, Iter);
      end loop;

      if Iter = Null_Iter then
         Append (Model, Iter, Parent_Iter);
         Set (Model, Iter, Name_Column, Get_Name (Entity).all & Suffix);
         Set (Model, Iter, Decl_Column,
              Base_Name (Get_Filename (Get_File (Decl)))
              & ':' & Image (Get_Line (Decl))
              & ':' & Image (Integer (Get_Column (Decl))));
         Set_Value (Model, Iter, Entity_Column, To_GValue (Entity));

         --  Append a dummy child, so that the parent can be expanded to
         --  show its called entities.
         Append (Model, Locations, Iter);
         Set (Model, Locations, Name_Column, Computing_Label);

      else
         Locations := Children (Model, Iter);
         while Locations /= Null_Iter loop
            Next (Model, Locations);
         end loop;
      end if;

      if Ref /= No_Entity_Reference then
         declare
            L : List_Access;
            Value   : GValue;
            Address : System.Address;
            use type System.Address;

         begin
            Get_Value (Model, Iter, List_Column, Value);
            Address := Get_Address (Value);

            if Address = System.Null_Address then
               L := new List;
            else
               L := To_Reference_List (Address);
            end if;

            Append (L.all, To_Record (Ref));

            Set_Address (Value, To_Address (L));

            Set_Value
              (Gtk_Tree_Store (Get_Model (View.Tree)),
               Iter, List_Column, Value);
         end;
      end if;

      return Iter;
   end Insert_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Data : in out Ancestors_User_Data; Cancelled : Boolean)
   is
      Model : Gtk_Tree_Store;
   begin
      if not Cancelled
        and then Data.Computing_Iter /= Null_Iter
      then
         Model := Gtk_Tree_Store (Get_Model (Data.View.Tree));
         Remove (Model, Data.Computing_Iter);
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
      pragma Unreferenced (Is_Renaming, Iter);
   begin
      case Data.View.Typ is
         when View_Called_By =>
            Iter := Insert_Entity
              (View        => Data.View,
               Parent      => Entity,
               Entity      => Parent,
               Ref         => Ref,
               Suffix      => "",
               Parent_Iter => Data.Entity_Iter);

         when View_Calls =>
            Iter := Insert_Entity
              (View        => Data.View,
               Parent      => Parent,
               Entity      => Entity,
               Ref         => Ref,
               Suffix      => "",
               Parent_Iter => Data.Entity_Iter);
      end case;
      return True;
   end On_Entity_Found;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : Callgraph_View_Access;
      Entity : constant Entity_Information := Get_Entity
        (Context.Context, Ask_If_Overloaded => True);
      pragma Unreferenced (Command);

      R      : Gtk_Requisition;
   begin
      if Entity /= null then
         View := Generic_View.Get_Or_Create_View
           (Kernel, False, Group => GPS.Kernel.MDI.Group_Consoles);
         View.Typ := View_Calls;
         Expand_Row (View.Tree,
           Insert_Entity (View, null, Entity, No_Entity_Reference,
             -" calls "));

         Size_Request (View.Tree, R);
         R := Get_Child_Requisition (View.Tree);
         Set_Position (View.Pane, (R.Width * 3) / 2);
      end if;

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
      View   : Callgraph_View_Access;
      Entity : constant Entity_Information := Get_Entity
        (Context.Context, Ask_If_Overloaded => True);
      pragma Unreferenced (Command);

      R      : Gtk_Requisition;
   begin
      if Entity /= null then
         View := Generic_View.Get_Or_Create_View
           (Kernel, False, Group => GPS.Kernel.MDI.Group_Consoles);
         View.Typ := View_Called_By;
         Expand_Row
           (View.Tree, Insert_Entity (View, null, Entity,
            No_Entity_Reference, -" is called by "));

         Size_Request (View.Tree, R);
         R := Get_Child_Requisition (View.Tree);
         Set_Position (View.Pane, (R.Width * 3) / 2);
      end if;

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
      Generic_View.Register_Module
        (Kernel, Menu_Name => "Call _Tree", Before_Menu => -"Remote");

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, History_Show_Locations, True);

      Filter := Lookup_Filter (Kernel, "Entity is container");

      Command := new Entity_Calls_Command;
      Register_Contextual_Menu
        (Kernel, "Entity calls",
         Label      => "%e calls",
         Filter     => Filter,
         Action     => Command,
         Ref_Item   => "Goto file spec<->body",
         Add_Before => False);

      Command := new Entity_Called_By_Command;
      Register_Contextual_Menu
        (Kernel, "Entity called by",
         Label      => "%e is called by",
         Filter     => Filter,
         Action     => Command,
         Ref_Item   => "Entity calls",
         Add_Before => False);

   end Register_Module;

end Call_Graph_Views;
