-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
--                            ACT-Europe                             --
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

with Ada.Exceptions;       use Ada.Exceptions;
with System;               use System;

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Actions; use Glide_Kernel.Actions;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glib.Object;          use Glib.Object;
with Glide_Intl;           use Glide_Intl;
with Traces;               use Traces;
with GUI_Utils;            use GUI_Utils;
with GNAT.Strings;         use GNAT.Strings;
with Commands.Interactive; use Commands.Interactive;

with Glib;                 use Glib;
with Gdk.Event;            use Gdk.Event;
with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Combo;            use Gtk.Combo;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.Event_Box;        use Gtk.Event_Box;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Label;            use Gtk.Label;
with Gtk.Stock;            use Gtk.Stock;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Tooltips;         use Gtk.Tooltips;
with Gtk.Paned;            use Gtk.Paned;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtkada.Handlers;           use Gtkada.Handlers;

package body Action_Editor is

   type Action_Editor_Module_Record is new Module_ID_Record with null record;
   type Action_Editor_Module_Access is access all
       Action_Editor_Module_Record'Class;
   Action_Editor_Module : Action_Editor_Module_Access;

   No_Filter      : constant String := "<No filter>";   --  -"No filter"
   Unnamed_Filter : constant String := "<Unnamed filter>";
   --  -"Unnamed filter"

   type Actions_Editor_Record is new Gtk_Dialog_Record with record
      Kernel      : Kernel_Handle;
      View        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
   end record;
   type Actions_Editor is access all Actions_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Actions_Editor; Kernel : access Kernel_Handle_Record'Class);
   --  Create a new actions editor

   type Action_Editor_Dialog_Record is new Gtk_Dialog_Record with record
      Action      : Action_Record;
      Kernel      : Kernel_Handle;
      Action_Name : Gtk_Label;
      Filter      : Gtk_Combo;
      Description : Gtk_Text_Buffer;
      Properties  : Gtk_Widget;
      Model       : Gtk_Tree_Store;
      View        : Gtk_Tree_View;
      Components  : Gtk_Box;
   end record;
   type Action_Editor_Dialog is access all Action_Editor_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog      : out Action_Editor_Dialog;
      Kernel      : access Kernel_Handle_Record'Class;
      Action_Name : String;
      Action      : Action_Record);
   --  Create a new editor for Action

   procedure On_Edit_Actions
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the actions editor

   Action_Column    : constant := 0;
   Component_Column : constant := 0;
   Component_Index  : constant := 1;

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String) return Gtk_Tree_Iter;
   --  Insert a new item in the tree model for the list of actions

   function Set
     (Model     : Gtk_Tree_Store;
      Parent    : Gtk_Tree_Iter;
      Component : String;
      Index     : Integer) return Gtk_Tree_Iter;
   --  Insert a new item in the tree model for the list of components

   function Find_Parent
     (Model : Gtk_Tree_Store; Filter : Action_Filter) return Gtk_Tree_Iter;
   --  Find the node in the tree that represents Filter. This node is created
   --  if it doesn't exist yet.

   function On_Button_Press
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when the user has clicked in the list of actions

   procedure On_Component_Changed (Dialog : access Gtk_Widget_Record'Class);
   --  Called when a new component is selected.

   --------------------------
   -- On_Component_Changed --
   --------------------------

   procedure On_Component_Changed (Dialog : access Gtk_Widget_Record'Class) is
      Ed    : constant Action_Editor_Dialog := Action_Editor_Dialog (Dialog);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Index : Gint;
      Comp_Iter, Result : Component_Iterator;
      Comp      : Command_Component;
      Current   : Gint := 1;
      Label     : Gtk_Label;

      procedure Get_Component_From_Index
        (Comp_Iter : in out Component_Iterator;
         Result    : out Component_Iterator);
      --  Return the the Index-th component in the action, including the
      --  on-failure components. Result must be freed by the caller

      procedure Get_Component_From_Index
        (Comp_Iter : in out Component_Iterator;
         Result    : out Component_Iterator)
      is
         Child : Component_Iterator;
      begin
         while Get (Comp_Iter) /= null and then Current /= Index loop
            Current := Current + 1;

            Child := On_Failure (Comp_Iter);
            if Child /= null then
               Get_Component_From_Index (Child, Result);
               if Result /= null then
                  Free (Comp_Iter);
                  return;
               end if;
            end if;

            Next (Comp_Iter);
         end loop;

         if Get (Comp_Iter) /= null then
            Result := Comp_Iter;
         else
            Free (Comp_Iter);
            Result := null;
         end if;
      end Get_Component_From_Index;

   begin
      Get_Selected (Get_Selection (Ed.View), Model, Iter);
      if Iter /= Null_Iter then
         Remove_All_Children (Ed.Components);

         Index := Get_Int (Ed.Model, Iter, Component_Index);
         if Index /= -1 then

            Comp_Iter := Start (Ed.Action.Command);
            Get_Component_From_Index (Comp_Iter, Result);

            if Result /= null then
               Comp := Get (Result);
               if Comp /= null then
                  Pack_Start
                    (Ed.Components, Component_Editor (Ed.Kernel, Comp),
                     Expand => False);
               end if;
               Free (Result);
            end if;
         else
            Gtk_New (Label, -("The following commands are executed if the"
                              & " previous one has failed"));
            Pack_Start (Ed.Components, Label, Expand => True);
         end if;

         Show_All (Ed.Components);
      end if;
   end On_Component_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog : out Action_Editor_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Action_Name : String;
      Action : Action_Record)
   is
      Event_Box : Gtk_Event_Box;
      Frame     : Gtk_Frame;
      Scrolled  : Gtk_Scrolled_Window;
      Text      : Gtk_Text_View;
      Hbox      : Gtk_Box;
      Label     : Gtk_Label;
      Button    : Gtk_Button;
      W         : Gtk_Widget;
      Render   : Gtk_Cell_Renderer_Text;
      Num      : Gint;
      Col      : Gtk_Tree_View_Column;
      Comp_Iter : Component_Iterator;
      Pane     : Gtk_Paned;
      pragma Unreferenced (W, Num);

      Filter_Iter : Action_Filter_Iterator := Start (Kernel);
      Filter      : Action_Filter;
      List        : Gtk.Enums.String_List.Glist;
      Index       : Integer := 1;

      procedure Add_Components
        (Comp_Iter : in out Component_Iterator; Parent : Gtk_Tree_Iter);
      --  Add all the components from Comp_Iter to the tree

      procedure Add_Components
        (Comp_Iter : in out Component_Iterator; Parent : Gtk_Tree_Iter)
      is
         Comp : Command_Component;
         Child : Component_Iterator;
         Node : Gtk_Tree_Iter;
      begin
         loop
            Comp := Get (Comp_Iter);
            exit when Comp = null;

            Node := Set
              (Dialog.Model, Parent,
               Component => Get_Name (Comp),
               Index     => Index);
            Index := Index + 1;

            Child := On_Failure (Comp_Iter);
            if Child /= null then
               Node := Set
                 (Dialog.Model, Node,
                  Component => "on-failure",
                  Index     => -1);
               Add_Components (Child, Node);
            end if;

            Next (Comp_Iter);
         end loop;

         Free (Comp_Iter);
      end Add_Components;

   begin
      Dialog := new Action_Editor_Dialog_Record;
      Dialog.Action := Action;
      Dialog.Kernel := Kernel_Handle (Kernel);
      Initialize
        (Dialog,
         Title  => -"Editing action """ & Action_Name & """",
         Parent => Get_Main_Window (Kernel),
         Flags  => Destroy_With_Parent);
      Set_Default_Size (Dialog, 800, 600);

      --  Action name

      Create_Blue_Label (Dialog.Action_Name, Event_Box);
      Set_Text (Dialog.Action_Name, Action_Name);
      Pack_Start (Get_Vbox (Dialog), Event_Box, Expand => False);

      --  Description

      Gtk_New (Frame, -"Description");
      Pack_Start (Get_Vbox (Dialog), Frame, Expand => False);
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scrolled);
      Gtk_New (Dialog.Description);
      Gtk_New (Text, Dialog.Description);
      Set_Wrap_Mode (Text, Wrap_Word);
      Set_Editable (Text, False);
      Add (Scrolled, Text);
      if Action.Description /= null then
         Set_Text (Dialog.Description, Action.Description.all);
      end if;

      --  Filter

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Hbox, Expand => False, Padding => 5);
      Gtk_New (Label, -"Filter: ");
      Pack_Start (Hbox, Label, Expand => False);
      Gtk_New (Dialog.Filter);
      Set_Editable (Get_Entry (Dialog.Filter), False);
      Pack_Start (Hbox, Dialog.Filter, Expand => True, Fill => True);
      Gtk_New (Button, -"Edit filters");
      Set_Sensitive (Button, False);
      Pack_Start (Hbox, Button, Expand => False);
      Set_Tip (Get_Tooltips (Kernel), Dialog.Filter,
               -("The filter indicates when the action applies. If the filter"
                 & " doesn't match the current context, then the action"
                 & " will not be executed"));

      loop
         Filter := Get (Filter_Iter);
         exit when Filter = null;

         Gtk.Enums.String_List.Prepend (List, Get_Name (Filter));
         Next (Kernel, Filter_Iter);
      end loop;
      Gtk.Enums.String_List.Prepend (List, -Unnamed_Filter);
      Gtk.Enums.String_List.Prepend (List, -No_Filter);
      Set_Popdown_Strings (Dialog.Filter, List);
      Gtk.Enums.String_List.Free (List);

      if Action.Filter /= null then
         if Get_Name (Action.Filter) = "" then
            Set_Text (Get_Entry (Dialog.Filter), -Unnamed_Filter);
         else
            Set_Text (Get_Entry (Dialog.Filter), Get_Name (Action.Filter));
         end if;
      else
         Set_Text (Get_Entry (Dialog.Filter), -No_Filter);
      end if;

      --  General action properties

      Dialog.Properties := Command_Editor (Action.Command);
      if Dialog.Properties /= null then
         Gtk_New (Frame, -"Properties");
         Pack_Start (Get_Vbox (Dialog), Frame, Expand => False, Padding => 5);
         Add (Frame, Dialog.Properties);
      end if;

      --  List of components

      Gtk_New (Frame, -"Commands");
      Pack_Start (Get_Vbox (Dialog), Frame, Expand => True, Padding => 5);
      Gtk_New_Hpaned (Pane);
      Add (Frame, Pane);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Pane, Scrolled, True, True);

      Gtk_New (Dialog.Model,
               (Component_Column => GType_String,
                Component_Index  => GType_Int));
      Gtk_New (Dialog.View, Dialog.Model);
      Set_Headers_Visible (Dialog.View, False);
      Add (Scrolled, Dialog.View);

      Gtk_New (Render);

      Gtk_New (Col);
      Num := Append_Column (Dialog.View, Col);
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Component_Column);
      Set_Resizable (Col, True);

      Widget_Callback.Object_Connect
        (Get_Selection (Dialog.View), "changed",
         Widget_Callback.To_Marshaller (On_Component_Changed'Access),
         Slot_Object => Dialog);

      Gtk_New_Vbox (Dialog.Components, Homogeneous => False);
      Pack2 (Pane, Dialog.Components, True, True);

      Comp_Iter := Start (Action.Command);
      Add_Components (Comp_Iter, Null_Iter);
      Select_Iter (Get_Selection (Dialog.View), Get_Iter_First (Dialog.Model));
      Expand_All (Dialog.View);

      Set_Position (Pane, 200);

      W := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      W := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Ed        : constant Actions_Editor := Actions_Editor (Editor);
      Iter      : Gtk_Tree_Iter;
      Action    : Action_Record;
      Dialog    : Action_Editor_Dialog;
   begin
      Iter := Find_Iter_For_Event (Ed.View, Ed.Model, Event);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter = Null_Iter
        or else Children (Ed.Model, Iter) /= Null_Iter
        or else Get_Event_Type (Event) /= Gdk_2button_Press
      then
         return False;
      end if;

      Action := Lookup_Action (Ed.Kernel, Get_String (Ed.Model, Iter, 0));
      Gtk_New (Dialog, Ed.Kernel, Get_String (Ed.Model, Iter, 0), Action);
      Show_All (Dialog);

      case Run (Dialog) is
         when Gtk_Response_OK =>
            null;
         when Gtk_Response_Cancel =>
            null;
         when others =>
            null;
      end case;

      Destroy (Dialog);

      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return True;
   end On_Button_Press;

   ---------
   -- Set --
   ---------

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String) return Gtk_Tree_Iter
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1       : Gint;
         Value1     : String;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

      Iter : aliased Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Internal
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column, Value1 => Descr & ASCII.NUL);
      return Iter;
   end Set;

   ---------
   -- Set --
   ---------

   function Set
     (Model     : Gtk_Tree_Store;
      Parent    : Gtk_Tree_Iter;
      Component : String;
      Index     : Integer) return Gtk_Tree_Iter
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1       : Gint; Value1 : String;
         Col2       : Gint; Value2 : Gint;
         Final      : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

      Iter : aliased Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Internal
        (Get_Object (Model), Iter'Address,
         Col1 => Component_Column, Value1 => Component & ASCII.NUL,
         Col2 => Component_Index,  Value2 => Gint (Index));
      return Iter;
   end Set;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Model : Gtk_Tree_Store; Filter : Action_Filter) return Gtk_Tree_Iter
   is
      Result : Gtk_Tree_Iter;
   begin
      if Filter = null or else Get_Name (Filter) = "" then
         Result := Find_Node (Model, -"General", Action_Column);
         if Result = Null_Iter then
            Result := Set (Model, Null_Iter, -"General");
         end if;
      else
         Result := Find_Node (Model, Get_Name (Filter), Action_Column);
         if Result = Null_Iter then
            Result := Set (Model, Null_Iter, Get_Name (Filter));
         end if;
      end if;
      return Result;
   end Find_Parent;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Actions_Editor;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      Render   : Gtk_Cell_Renderer_Text;
      Num      : Gint;
      Col      : Gtk_Tree_View_Column;
      W        : Gtk_Widget;
      Action      : Action_Record;
      Action_Iter : Action_Iterator := Start (Kernel);
      Node        : Gtk_Tree_Iter;
      pragma Unreferenced (W, Num, Node);
   begin
      Editor := new Actions_Editor_Record;
      Editor.Kernel := Kernel_Handle (Kernel);
      Initialize (Editor,
                  Title  => -"Actions editor",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Destroy_With_Parent);
      Set_Default_Size (Editor, 640, 400);

      --  List of actions

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Get_Vbox (Editor), Scrolled, Expand => True);

      Gtk_New (Editor.Model,
               (Action_Column => GType_String));
      Gtk_New (Editor.View, Editor.Model);
      Add (Scrolled, Editor.View);

      Gtk_New (Render);

      Gtk_New (Col);
      Num := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Action");
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Action_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Action_Column);

      Clicked (Col);

      Return_Callback.Object_Connect
        (Editor.View,
         "button_press_event",
         Return_Callback.To_Marshaller (On_Button_Press'Access),
         Slot_Object => Editor);

      loop
         Action := Get (Action_Iter);
         exit when Action = No_Action;

         Node := Set
           (Model  => Editor.Model,
            Parent => Find_Parent (Editor.Model, Action.Filter),
            Descr  => Get (Action_Iter));
         Next (Editor.Kernel, Action_Iter);
      end loop;

      W := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      W := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   ---------------------
   -- On_Edit_Actions --
   ---------------------

   procedure On_Edit_Actions
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Editor : Actions_Editor;
   begin
      Gtk_New (Editor, Kernel);
      Show_All (Editor);

      case Run (Editor) is
         when Gtk_Response_OK =>
            null;
         when Gtk_Response_Cancel =>
            null;
         when others =>
            null;
      end case;

      Destroy (Editor);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Edit_Actions;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Action_Editor_Module := new Action_Editor_Module_Record;
      Register_Module
         (Module      => Module_ID (Action_Editor_Module),
          Kernel      => Kernel,
          Module_Name => "Action_Editor");

      Register_Menu
        (Kernel, '/' & (-"Edit"), -"Actions",
         Ref_Item   => -"Preferences",
         Add_Before => False,
         Callback   => On_Edit_Actions'Access);
   end Register_Module;

end Action_Editor;

