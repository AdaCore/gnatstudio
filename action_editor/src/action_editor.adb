-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2007, AdaCore             --
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

with System;                   use System;
with GNAT.OS_Lib;
with GNAT.Strings;             use GNAT.Strings;

with Gdk.Event;                use Gdk.Event;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;

with Gtkada.Handlers;          use Gtkada.Handlers;

with Commands.Interactive;     use Commands.Interactive;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Console;       use GPS.Kernel.Console;
with GPS.Kernel.Custom;        use GPS.Kernel.Custom;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel;               use GPS.Kernel;
with GUI_Utils;                use GUI_Utils;
with Traces;                   use Traces;
with VFS;                      use VFS;
with XML_Parsers;              use XML_Parsers;

package body Action_Editor is

   Me : constant Debug_Handle := Create ("Action_Editor");

   type Action_Editor_Module_Record is new Module_ID_Record with null record;
   type Action_Editor_Module_Access is
     access all Action_Editor_Module_Record'Class;

   Action_Editor_Module : Action_Editor_Module_Access;

   No_Filter      : constant String := "<No filter>";   --  -"No filter"
   Unnamed_Filter : constant String := "<Unnamed filter>";
   --  -"Unnamed filter"

   type Actions_Editor_Record is new Gtk_Dialog_Record with record
      Kernel      : Kernel_Handle;
      View        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Filter_Built_In : Gtk_Check_Button;
   end record;
   type Actions_Editor is access all Actions_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Actions_Editor; Kernel : access Kernel_Handle_Record'Class);
   --  Create a new actions editor

   type Action_Editor_Dialog_Record is new Gtk_Dialog_Record with record
      Action      : Action_Record_Access;
      Kernel      : Kernel_Handle;
      Action_Name : Gtk_Label;
      Filter      : Gtk_Combo;
      Description : Gtk_Text_Buffer;
--      Properties  : Gtk_Widget;
--      Model       : Gtk_Tree_Store;
--      View        : Gtk_Tree_View;
      Editor      : Command_Editor;
   end record;
   type Action_Editor_Dialog is access all Action_Editor_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog      : out Action_Editor_Dialog;
      Kernel      : access Kernel_Handle_Record'Class;
      Action_Name : String;
      Action      : Action_Record_Access);
   --  Create a new editor for Action

   procedure On_Edit_Actions
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the actions editor

   Action_Column    : constant := 0;
   Modified_Column  : constant := 1;

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Modified : Boolean) return Gtk_Tree_Iter;
   --  Insert a new item in the tree model for the list of actions

   function Find_Parent
     (Model : Gtk_Tree_Store; Filter : Action_Filter) return Gtk_Tree_Iter;
   --  Find the node in the tree that represents Filter. This node is created
   --  if it doesn't exist yet.

   function On_Button_Press
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when the user has clicked in the list of actions

   procedure Update_Contents
     (Editor : access Actions_Editor_Record'Class);
   --  Update the list of actions in the editor

   procedure Update_Action_From_Dialog
     (Action : Action_Record_Access;
      Dialog : Action_Editor_Dialog);
   --  Update the action from the dialog

   procedure Save_Custom_Actions
     (Kernel : access Kernel_Handle_Record'Class);
   --  Save the actions modified interactively through this module

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog : out Action_Editor_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Action_Name : String;
      Action : Action_Record_Access)
   is
      Event_Box : Gtk_Event_Box;
      Frame     : Gtk_Frame;
      Scrolled  : Gtk_Scrolled_Window;
      Text      : Gtk_Text_View;
      Hbox      : Gtk_Box;
      Label     : Gtk_Label;
      Button    : Gtk_Button;
      W         : Gtk_Widget;
      pragma Unreferenced (W);

      Filter_Iter : Action_Filter_Iterator := Start (Kernel);
      Filter      : Action_Filter;
      List        : Gtk.Enums.String_List.Glist;

   begin
      Dialog := new Action_Editor_Dialog_Record;
      Dialog.Action := Action;
      Dialog.Kernel := Kernel_Handle (Kernel);
      Initialize
        (Dialog,
         Title  => -"Editing action """ & Action_Name & """",
         Parent => Get_Current_Window (Kernel_Handle (Kernel)),
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

      Dialog.Editor := Create_Command_Editor (Action.Command, Kernel);
      Pack_Start
        (Get_Vbox (Dialog), Dialog.Editor, Expand => True, Fill => True);

      W := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      W := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   -------------------------------
   -- Update_Action_From_Dialog --
   -------------------------------

   procedure Update_Action_From_Dialog
     (Action : Action_Record_Access;
      Dialog : Action_Editor_Dialog)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      XML : Node_Ptr;
   begin
      Action.Modified := True;

      Free (Action.Description);

      Get_Start_Iter (Dialog.Description, Start_Iter);
      Get_End_Iter (Dialog.Description, End_Iter);
      Action.Description := new String'
        (Get_Text (Dialog.Description, Start_Iter, End_Iter));

      --  ??? Not implemented yet

--        Free (Action.Command);
--        Action.Command := new Custom_Command;
      XML := To_XML (Dialog.Editor);

--        Create
--          (Item           => Custom_Command_Access (Action.Command),
--           Name           => Get_Text (Dialog.Action_Name),
--           Kernel         => Dialog.Kernel,
--           Command        => XML,
--           Default_Output => );

      Free (XML);
   end Update_Action_From_Dialog;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Ed        : constant Actions_Editor := Actions_Editor (Editor);
      Iter      : Gtk_Tree_Iter;
      Action    : Action_Record_Access;
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
            Update_Action_From_Dialog (Action, Dialog);
            Set (Ed.Model, Iter, Modified_Column, True);
         when Gtk_Response_Cancel =>
            null;
         when others =>
            null;
      end case;

      Destroy (Dialog);

      return True;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return True;
   end On_Button_Press;

   ---------
   -- Set --
   ---------

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Modified : Boolean) return Gtk_Tree_Iter
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1       : Gint; Value1 : String;
         Col2       : Gint; Value2 : Gint);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr_int");

      Iter : aliased Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Internal
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column, Value1 => Descr & ASCII.NUL,
         Col2 => Modified_Column, Value2 => Boolean'Pos (Modified));
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
            Result := Set (Model, Null_Iter, -"General", False);
         end if;
      else
         Result := Find_Node (Model, Get_Name (Filter), Action_Column);
         if Result = Null_Iter then
            Result := Set (Model, Null_Iter, Get_Name (Filter), False);
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
      Action      : Action_Record_Access;
      Action_Iter : Action_Iterator := Start (Editor.Kernel);
      Node        : Gtk_Tree_Iter;
      pragma Unreferenced (Node);
   begin
      loop
         Action := Get (Action_Iter);
         exit when Action = null;

         Node := Set
           (Model  => Editor.Model,
            Parent => Find_Parent (Editor.Model, Action.Filter),
            Descr  => Get (Action_Iter),
            Modified => Action.Modified);
         Next (Editor.Kernel, Action_Iter);
      end loop;
   end Update_Contents;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Actions_Editor;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      Render   : Gtk_Cell_Renderer_Text;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Num      : Gint;
      Col      : Gtk_Tree_View_Column;
      W        : Gtk_Widget;
      pragma Unreferenced (W, Num);
   begin
      Editor := new Actions_Editor_Record;
      Editor.Kernel := Kernel_Handle (Kernel);
      Initialize (Editor,
                  Title  => -"Actions editor",
                  Parent => Get_Current_Window (Kernel_Handle (Kernel)),
                  Flags  => Destroy_With_Parent);
      Set_Default_Size (Editor, 640, 400);

      --  Filters

      Gtk_New (Editor.Filter_Built_In, -"Hide built-in actions");
      Pack_Start (Get_Vbox (Editor), Editor.Filter_Built_In, Expand => False);
      Set_Active (Editor.Filter_Built_In, False);
      Set_Sensitive (Editor.Filter_Built_In, False);

      --  List of actions

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Get_Vbox (Editor), Scrolled, Expand => True);

      Gtk_New (Editor.Model,
               (Action_Column   => GType_String,
                Modified_Column => GType_Boolean));
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

      Gtk_New (Toggle_Render);
      Gtk_New (Col);
      Num := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Modified");
      Pack_Start (Col, Toggle_Render, False);
      Add_Attribute (Col, Toggle_Render, "active", Modified_Column);

      Return_Callback.Object_Connect
        (Editor.View,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Button_Press'Access),
         Slot_Object => Editor);

      Update_Contents (Editor);

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
            Save_Custom_Actions (Kernel);
         when Gtk_Response_Cancel =>
            null;
         when others =>
            null;
      end case;

      Destroy (Editor);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Edit_Actions;

   -------------------------
   -- Save_Custom_Actions --
   -------------------------

   procedure Save_Custom_Actions
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "actions.xml";
      Tree        : Node_Ptr;
      Error       : String_Access;
      Action      : Action_Record_Access;
      Action_Iter : Action_Iterator := Start (Kernel);
      Child       : Node_Ptr;
      Descr       : Node_Ptr;
      Success     : Boolean;

   begin
      --  We need to preserve the actions that were already defined in this
      --  file

      if GNAT.OS_Lib.Is_Regular_File (Filename) then
         Parse (Filename, Tree, Error);
         Free (Error);
      end if;

      if Tree = null then
         Tree := new Node;
         Tree.Tag := new String'("actions");
      end if;

      loop
         Action := Get (Action_Iter);
         exit when Action = null;

         if Action.Modified then
            Child := Find_Tag_With_Attribute
              (Tree.Child, Tag => "action",
               Key => "name", Value => Get (Action_Iter));
            if Child /= null then
               Free (Child);
            end if;

            Child := new Node;
            Child.Tag := new String'("action");
            Set_Attribute (Child, "name", Get (Action_Iter));
            Add_Child (Tree, Child);

            Descr := new Node;
            Descr.Tag := new String'("description");
            Descr.Value := new String'(Action.Description.all);
            Add_Child (Child, Descr);

--              if Action.Filter /= null then
--                 To_XML (Action.Filter, Child);
--              end if;

            --  ??? Not implemented yet.
            --  To_XML (Action.Command, Child);

            Action.Modified := False;
         end if;

         Next (Kernel, Action_Iter);
      end loop;

      Trace (Me, "Saving " & Filename);
      Print (Tree, Filename, Success);
      Free (Tree);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Custom_Actions;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename : constant String :=
        Get_Home_Dir (Kernel) & "actions.xml";
      Tree : Node_Ptr;
      Err  : String_Access;
   begin
      Action_Editor_Module := new Action_Editor_Module_Record;
      Register_Module
         (Module      => Module_ID (Action_Editor_Module),
          Kernel      => Kernel,
          Module_Name => "Action_Editor");

      Register_Menu
        (Kernel, '/' & (-"Edit"), -"Ac_tions",
         Ref_Item   => -"Preferences",
         Callback   => On_Edit_Actions'Access);

      if GNAT.OS_Lib.Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         XML_Parsers.Parse (Filename, Tree, Err);
         if Tree /= null then
            GPS.Kernel.Custom.Execute_Customization_String
              (Kernel, Create (Full_Filename => Filename), Tree.Child,
               User_Specific);
            Free (Tree);
         else
            Insert (Kernel, Err.all, Mode => GPS.Kernel.Console.Error);
            Free (Err);
         end if;
      end if;
   end Register_Module;

end Action_Editor;
