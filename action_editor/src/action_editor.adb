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
      Action_Name : Gtk_Label;
      Filter      : Gtk_Combo;
      Command     : Gtk_Frame;
      Description : Gtk_Text_Buffer;
      Properties_Box : Gtk_Box;
      Command_Box    : Gtk_Box;
   end record;
   type Actions_Editor is access all Actions_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Actions_Editor; Kernel : access Kernel_Handle_Record'Class);
   --  Create a new actions editor

   procedure Update_Contents
     (Editor : access Actions_Editor_Record'Class);
   --  Update the contents of the editor

   procedure On_Edit_Actions
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the actions editor

   Action_Column : constant := 0;

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String) return Gtk_Tree_Iter;
   --  Insert a new item in the tree model.

   function Find_Parent
     (Model : Gtk_Tree_Store; Filter : Action_Filter) return Gtk_Tree_Iter;
   --  Find the node in the tree that represents Filter. This node is created
   --  if it doesn't exist yet.

   procedure Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Actions_Editor := Actions_Editor (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Action    : Action_Record;
      Comp_Iter : Component_Iterator;
      Comp      : Command_Component;
      W         : Gtk_Widget;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Action := Lookup_Action (Ed.Kernel, Get_String (Model, Iter, 0));

         Set_Text (Ed.Action_Name, Get_String (Model, Iter, 0));

         if Action.Description /= null then
            Set_Text (Ed.Description, Action.Description.all);
         else
            Set_Text (Ed.Description, "");
         end if;

         if Action.Filter /= null then
            if Get_Name (Action.Filter) = "" then
               Set_Text (Get_Entry (Ed.Filter), -Unnamed_Filter);
            else
               Set_Text (Get_Entry (Ed.Filter), Get_Name (Action.Filter));
            end if;
         else
            Set_Text (Get_Entry (Ed.Filter), -No_Filter);
         end if;

         Remove_All_Children (Ed.Properties_Box);
         W := Command_Editor (Action.Command);
         if W /= null then
            Pack_Start (Ed.Properties_Box, W, Expand => True);
         end if;
         Show_All (Ed.Properties_Box);

         Remove_All_Children (Ed.Command_Box);
         Comp_Iter := Start (Action.Command);
         loop
            Comp := Get (Comp_Iter);
            exit when Comp = null;
            Pack_Start
              (Ed.Command_Box, Component_Editor (Comp), Expand => False);
            Next (Comp_Iter);
         end loop;
         Show_All (Ed.Command_Box);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Selection_Changed;

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
         Col1 : Gint; Value1 : String;
         Final : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");
      Iter : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Parent);
      Internal
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column, Value1 => Descr & ASCII.NUL);
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

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
     (Editor : access Actions_Editor_Record'Class)
   is
      Action      : Action_Record;
      Action_Iter : Action_Iterator := Start (Editor.Kernel);
      Filter_Iter : Action_Filter_Iterator := Start (Editor.Kernel);
      Node        : Gtk_Tree_Iter;
      Filter      : Action_Filter;
      List        : Gtk.Enums.String_List.Glist;
      pragma Unreferenced (Node);
   begin
      loop
         Action := Get (Action_Iter);
         exit when Action = No_Action;

         Node := Set
           (Model  => Editor.Model,
            Parent => Find_Parent (Editor.Model, Action.Filter),
            Descr  => Get (Action_Iter));
         Next (Editor.Kernel, Action_Iter);
      end loop;

      loop
         Filter := Get (Filter_Iter);
         exit when Filter = null;

         Gtk.Enums.String_List.Prepend (List, Get_Name (Filter));
         Next (Editor.Kernel, Filter_Iter);
      end loop;

      Gtk.Enums.String_List.Prepend (List, -Unnamed_Filter);
      Gtk.Enums.String_List.Prepend (List, -No_Filter);

      Set_Popdown_Strings (Editor.Filter, List);
      Gtk.Enums.String_List.Free (List);
   end Update_Contents;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Actions_Editor;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      Pane     : Gtk_Paned;
      Render   : Gtk_Cell_Renderer_Text;
      Num      : Gint;
      Col      : Gtk_Tree_View_Column;
      Vbox     : Gtk_Box;
      Hbox     : Gtk_Box;
      Frame    : Gtk_Frame;
      Event    : Gtk_Event_Box;
      W        : Gtk_Widget;
      Text     : Gtk_Text_View;
      Label    : Gtk_Label;
      Button   : Gtk_Button;
      pragma Unreferenced (W, Num);
   begin
      Editor := new Actions_Editor_Record;
      Editor.Kernel := Kernel_Handle (Kernel);
      Initialize (Editor,
                  Title  => -"Actions editor",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Destroy_With_Parent);
      Set_Default_Size (Editor, 640, 400);

      Gtk_New_Hpaned (Pane);
      Pack_Start (Get_Vbox (Editor), Pane, Expand => True, Fill => True);

      --  List of actions

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Pane, Scrolled, True, True);

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

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.View), "changed",
         Widget_Callback.To_Marshaller (Selection_Changed'Access),
         Editor);

      --  Right frame

      Gtk_New (Frame);
      Pack2 (Pane, Frame, False, False);

      Gtk_New_Vbox (Vbox, Homogeneous => False);
      Add (Frame, Vbox);

      --  Name of current action

      Create_Blue_Label (Editor.Action_Name, Event);
      Pack_Start (Vbox, Event, Expand => False);

      --  Filter

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Vbox, Hbox, Expand => False);

      Gtk_New (Label, -"Filter: ");
      Pack_Start (Hbox, Label, Expand => False);

      Gtk_New (Editor.Filter);
      Set_Editable (Get_Entry (Editor.Filter), False);
      Pack_Start (Hbox, Editor.Filter, Expand => True, Fill => True);

      Gtk_New (Button, -"Edit filters");
      Set_Sensitive (Button, False);
      Pack_Start (Hbox, Button, Expand => False);

      --  Properties

      Gtk_New_Vbox (Editor.Properties_Box, Homogeneous => False);
      Pack_Start (Vbox, Editor.Properties_Box, Expand => False);

      --  Command

      Gtk_New (Editor.Command, -"Command");
      Pack_Start (Vbox, Editor.Command, Expand => True);

      Gtk_New_Vbox (Editor.Command_Box, Homogeneous => False);
      Add (Editor.Command, Editor.Command_Box);

      --  Description area

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Vbox, Scrolled, Expand => False, Fill => True);

      Gtk_New (Editor.Description);
      Gtk_New (Text, Editor.Description);
      Set_Wrap_Mode (Text, Wrap_Word);
      Set_Editable (Text, False);
      Add (Scrolled, Text);

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
      Update_Contents (Editor);
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

