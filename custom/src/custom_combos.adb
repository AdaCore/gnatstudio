------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.List_Store;     use Gtk.List_Store;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Toolbar;        use Gtk.Toolbar;
with Gtk.Tool_Item;      use Gtk.Tool_Item;
with Gtk.Tool_Button;    use Gtk.Tool_Button;
with Gtk.Tree_Model;     use Gtk.Tree_Model;
with Gtk.Label;          use Gtk.Label;
with Gtk.Widget;         use Gtk.Widget;

with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts, GNATCOLL.Scripts.Gtkada;
with GPS.Kernel.MDI;     use GPS.Kernel.MDI;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GPS.Intl;           use GPS.Intl;

with Custom_Module;      use Custom_Module;
with GUI_Utils;          use GUI_Utils;

package body Custom_Combos is

   Id_Cst            : aliased constant String := "id";
   Position_Cst      : aliased constant String := "position";
   Label_Cst         : aliased constant String := "label";
   Choice_Cst        : aliased constant String := "choice";
   On_Select         : aliased constant String := "on_selected";
   On_Click          : aliased constant String := "on_click";
   On_Changed        : aliased constant String := "on_changed";
   Widget_Cst        : aliased constant String := "widget";
   Tooltip_Cst       : aliased constant String := "tooltip";
   Stock_Id_Cst      : aliased constant String := "stock_id";
   Pos_Cst           : aliased constant String := "pos";

   Add_Args             : constant Cst_Argument_List :=
                            (Choice_Cst'Access, On_Select'Access);
   Remove_Args          : constant Cst_Argument_List :=
                            (1 => Choice_Cst'Access);
   Set_Text_Args        : constant Cst_Argument_List :=
                            (1 => Choice_Cst'Access);
   Simple_Args          : constant Cst_Argument_List := (1 => Id_Cst'Access);
   Get_By_Pos_Args      : constant Cst_Argument_List :=
                            (1 => Position_Cst'Access);
   Create_Combo_Args    : constant Cst_Argument_List :=
                            (Id_Cst'Access, Label_Cst'Access,
                             On_Changed'Access);
   Append_Args          : constant Cst_Argument_List :=
     (1 => Widget_Cst'Access,
      2 => Tooltip_Cst'Access,
      3 => Label_Cst'Access);
   Insert_Args          : constant Cst_Argument_List :=
     (1 => Widget_Cst'Access,
      2 => Tooltip_Cst'Access,
      3 => Label_Cst'Access,
      4 => Pos_Cst'Access);
   Create_Button_Args   : constant Cst_Argument_List :=
     (Id_Cst'Access, Label_Cst'Access, On_Click'Access);
   Create_Tool_Button_Args : constant Cst_Argument_List :=
     (Stock_Id_Cst'Access, Label_Cst'Access, On_Click'Access);
   Set_Button_Text_Args : constant Cst_Argument_List :=
                            (1 => Label_Cst'Access);

   type Custom_Combo_Record is new Gtk_Box_Record with record
      Combo : Gtk_Combo_Box_Text;
   end record;
   type Custom_Combo is access all Custom_Combo_Record'Class;

   type Changed_Callback_Data_Type is record
      Subprogram : Subprogram_Type;
      Item       : Unbounded_String;
   end record;

   package Combo_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Changed_Callback_Data_Type);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Create_Combo
     (Kernel     : access Kernel_Handle_Record'Class;
      Id         : String;
      Title      : String;
      On_Changed : Subprogram_Type;
      Instance   : Class_Instance) return Custom_Combo;
   --  Create a new combo

   procedure Add_Combo_Entry
     (Combo       : access Custom_Combo_Record'Class;
      Label       : String;
      On_Selected : Subprogram_Type);
   --  Add a combo entry and a command that should be
   --  executed whenever this entry is selected.
   --  On_Selected is also executed when the entry is selected

   procedure Custom_Entry_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with Combo class

   procedure Button_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with Button class

   procedure Tool_Button_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with Tool Button class

   procedure Custom_Toolbar_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with Toolbar class

   procedure On_Button_Clicked
     (Button    : access Gtk_Widget_Record'Class;
      User_Data : Subprogram_Type);
   --  Called when a button is pressed

   function Lookup_Component
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String) return Gtk_Widget;
   --  Return a registered component with label S. If no component was found,
   --  return null.

   function Lookup_Component_By_Pos
     (Kernel : access Kernel_Handle_Record'Class;
      Pos    : Integer) return Gtk_Widget;
   --  Return the Pos-th component

   procedure Combo_Changed
     (Combo_Box : access Gtk_Widget_Record'Class;
      Data      : Changed_Callback_Data_Type);
   --  Called whenever the text in GPS_Combo is changed.

   procedure Remove_Combo_Item
     (Combo : access Custom_Combo_Record'Class;
      Label : String);
   --  Remove choice identified by Label from combo identified by Id.

   -----------------------
   -- Remove_Combo_Item --
   -----------------------

   procedure Remove_Combo_Item
     (Combo : access Custom_Combo_Record'Class;
      Label : String)
   is
      Iter : Gtk_Tree_Iter;
      List : constant Gtk_List_Store := -Combo.Combo.Get_Model;

   begin
      Iter := List.Get_Iter_First;
      while Iter /= Null_Iter loop
         if List.Get_String (Iter, 0) = Label then
            List.Remove (Iter);
            exit;
         end if;

         List.Next (Iter);
      end loop;
   end Remove_Combo_Item;

   -----------------------------
   -- Lookup_Component_By_Pos --
   -----------------------------

   function Lookup_Component_By_Pos
     (Kernel : access Kernel_Handle_Record'Class;
      Pos    : Integer) return Gtk_Widget
   is
      use Gtk.Widget.Widget_List;
      Toolbar  : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Children : Glist := Get_Children (Toolbar);
      Tmp      : Glist := Children;
      Index    : Natural := 0;
      Child    : Gtk_Widget;
   begin
      while Tmp /= Null_List loop
         if Index = Pos then
            Child := Get_Data (Tmp);
            Free (Children);
            return Child;
         end if;

         Index := Index + 1;
         Tmp := Next (Tmp);
      end loop;

      Free (Children);
      return null;
   end Lookup_Component_By_Pos;

   ----------------------
   -- Lookup_Component --
   ----------------------

   function Lookup_Component
     (Kernel : access Kernel_Handle_Record'Class;
      Id     : String) return Gtk_Widget
   is
      use Gtk.Widget.Widget_List;
      Toolbar  : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Children : Glist := Get_Children (Toolbar);
      Tmp      : Glist := Children;
      Child    : Gtk_Widget;
   begin
      while Tmp /= Null_List loop
         Child := Get_Data (Tmp);
         if Get_Name (Child) = Id  then
            Free (Children);
            return Child;

         elsif Child.all in Gtk_Tool_Item_Record'Class
           and then Get_Child (Gtk_Tool_Item (Child)) /= null
           and then Get_Name (Get_Child (Gtk_Tool_Item (Child))) = Id
         then
            Free (Children);
            return Get_Child (Gtk_Tool_Item (Child));
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (Children);
      return null;
   end Lookup_Component;

   -------------------
   -- Combo_Changed --
   -------------------

   procedure Combo_Changed
     (Combo_Box : access Gtk_Widget_Record'Class;
      Data      : Changed_Callback_Data_Type)
   is
      Combo      : constant Custom_Combo := Custom_Combo (Combo_Box);
      Combo_Text : constant String := Get_Active_Text (Combo.Combo);

   begin
      if Combo_Text /= "" and then Data.Subprogram /= null
        and then
          (Data.Item = Null_Unbounded_String --  Execute every time
           or else To_String (Data.Item) = Combo_Text) --  item specific cb
      then
         declare
            S   : constant Scripting_Language :=
                    Get_Script (Data.Subprogram.all);
            D   : Callback_Data'Class := Create (S, Arguments_Count => 2);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (D, 1, Get_Instance (S, Combo));
            Set_Nth_Arg (D, 2, Combo_Text);
            Tmp := Execute (Data.Subprogram, D);
            Free (D);
         end;
      end if;
   end Combo_Changed;

   ------------------
   -- Create_Combo --
   ------------------

   function Create_Combo
     (Kernel     : access Kernel_Handle_Record'Class;
      Id         : String;
      Title      : String;
      On_Changed : Subprogram_Type;
      Instance   : Class_Instance) return Custom_Combo
   is
      Combo : Custom_Combo := Custom_Combo (Lookup_Component (Kernel, Id));
      Label : Gtk_Label;

   begin
      if Combo /= null then
         Insert
           (Kernel, -"Entry already registered: " & Id,
            Mode => Error);

      else
         Combo := new Custom_Combo_Record;
         Initialize_Hbox (Combo, Homogeneous => False, Spacing => 0);

         if Title /= "" then
            Gtk_New (Label, Title);
            Pack_Start (Combo, Label, Expand => False, Padding => 4);
         end if;

         Gtk_New (Combo.Combo);
         Set_Name (Combo, Id);
         Pack_Start
           (Combo, Combo.Combo, Expand => True, Fill => True, Padding => 4);

         Combo_Callback.Object_Connect
           (Combo.Combo, Gtk.Combo_Box.Signal_Changed, Combo_Changed'Access,
            Slot_Object => Combo,
            User_Data   => (Subprogram => On_Changed,
                            Item       => Null_Unbounded_String),
            After       => True);

         Set_Data (Instance, Widget => GObject (Combo));
      end if;
      return Combo;
   end Create_Combo;

   ---------------------
   -- Add_Combo_Entry --
   ---------------------

   procedure Add_Combo_Entry
     (Combo       : access Custom_Combo_Record'Class;
      Label       : String;
      On_Selected : Subprogram_Type)
   is
   begin
      Add_Unique_Combo_Entry (Combo.Combo, Label);
      if On_Selected /= null then
         Combo_Callback.Object_Connect
           (Combo.Combo, Gtk.Combo_Box.Signal_Changed, Combo_Changed'Access,
            Slot_Object => Combo,
            User_Data   => (Subprogram => On_Selected,
                            Item       => To_Unbounded_String (Label)),
            After       => True);
      end if;
   end Add_Combo_Entry;

   -----------------------
   -- On_Button_Clicked --
   -----------------------

   procedure On_Button_Clicked
     (Button    : access Gtk_Widget_Record'Class;
      User_Data : Subprogram_Type)
   is
      S   : constant Scripting_Language := Get_Script (User_Data.all);
      D   : Callback_Data'Class := Create (S, Arguments_Count => 1);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg (D, 1, Get_Instance (S, Button));
      Tmp := Execute (User_Data, D);
      Free (D);
   end On_Button_Clicked;

   ----------------------------
   -- Custom_Toolbar_Handler --
   ----------------------------

   procedure Custom_Toolbar_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Custom_Module_ID.all);
      Widget : Gtk_Widget;
      Inst   : Class_Instance;

      procedure Insert
        (Widget  : Gtk_Widget;
         Tooltip : String;
         Pos     : Gint);
      --  Factorize code between "append" and "insert" commands

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Widget  : Gtk_Widget;
         Tooltip : String;
         Pos     : Gint)
      is
         Item : Gtk_Tool_Item;
      begin
         if Widget.all in Gtk_Tool_Item_Record'Class then
            Item := Gtk_Tool_Item (Widget);
         else
            Gtk_New (Item);
            Add (Item, Widget);
         end if;

         Insert (Get_Toolbar (Kernel), Item, Pos);
         Show_All (Item);

         if Tooltip /= "" then
            Set_Tooltip_Text (Item, Tooltip);
         end if;
      end Insert;

   begin
      if Command = Constructor_Method then
         null;

      elsif Command = "get" then
         Name_Parameters (Data, Simple_Args);
         Widget := Lookup_Component (Kernel, Nth_Arg (Data, 2));

         if Widget = null then
            Set_Error_Msg (Data, -"Component not found: " & Nth_Arg (Data, 2));
         else
            Set_Return_Value (Data, Get_Instance (Get_Script (Data), Widget));
         end if;

      elsif Command = "get_by_pos" then
         Name_Parameters (Data, Get_By_Pos_Args);
         Widget := Lookup_Component_By_Pos (Kernel, Nth_Arg (Data, 2));

         if Widget = null then
            Set_Error_Msg (Data, -"Component not found: " & Nth_Arg (Data, 2));

         else
            Inst := Get_Instance (Get_Script (Data), Widget);
            if Inst /= No_Class_Instance then
               Set_Return_Value (Data, Inst);
            else
               Inst := New_Instance
                 (Get_Script (Data), Get_GUI_Class (Kernel));
               Set_Data (Inst, Widget => GObject (Widget));
               Set_Return_Value (Data, Inst);
            end if;
         end if;

      elsif Command = "append" then
         Name_Parameters (Data, Append_Args);

         declare
            EntInst : constant Class_Instance :=
              Nth_Arg (Data, 2, Get_GUI_Class (Kernel));
            Widget : constant Gtk_Widget :=
              Gtk_Widget (GObject'(Get_Data (EntInst)));
            Tip    : constant String := Nth_Arg (Data, 3, "");
            Pos : constant Gint := Get_Toolbar_Section (Kernel, null, "user");
         begin
            Insert (Widget, Tip, Pos);
         end;

      elsif Command = "insert" then
         Name_Parameters (Data, Insert_Args);

         declare
            EntInst : constant Class_Instance :=
              Nth_Arg (Data, 2, Get_GUI_Class (Kernel));
            Widget : constant Gtk_Widget :=
              Gtk_Widget (GObject'(Get_Data (EntInst)));
            Pos    : constant Integer := Nth_Arg (Data, 3, -1);
            Tip    : constant String := Nth_Arg (Data, 4, "");
         begin
            Insert (Widget, Tip, Gint (Pos));
         end;
      end if;
   end Custom_Toolbar_Handler;

   --------------------------
   -- Custom_Entry_Handler --
   --------------------------

   procedure Custom_Entry_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle  := Get_Kernel (Custom_Module_ID.all);
      Class  : constant Class_Type     := New_Class (Kernel, "Combo");
      Inst   : constant Class_Instance := Nth_Arg (Data, 1, Class);
      Combo  : Custom_Combo;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Create_Combo_Args);
         Combo := Create_Combo
           (Kernel, Id  => Nth_Arg (Data, 2),
            Title       => Nth_Arg (Data, 3, ""),
            On_Changed  => Nth_Arg (Data, 4, null),
            Instance    => Inst);
         Show_All (Combo);

      elsif Command = "add" then
         Name_Parameters (Data, Add_Args);
         Add_Combo_Entry
           (Combo       => Custom_Combo (GObject'(Get_Data (Inst))),
            Label       => Nth_Arg (Data, 2),
            On_Selected => Nth_Arg (Data, 3, null));

      elsif Command = "remove" then
         Name_Parameters (Data, Remove_Args);
         Remove_Combo_Item
           (Combo => Custom_Combo (GObject'(Get_Data (Inst))),
            Label => Nth_Arg (Data, 2));

      elsif Command = "clear" then
         Name_Parameters (Data, Simple_Args);
         Clear
           (-Get_Model (Custom_Combo (GObject'(Get_Data (Inst))).Combo));

      elsif Command = "get_text" then
         Name_Parameters (Data, Simple_Args);
         Set_Return_Value
           (Data,
            Get_Active_Text (Custom_Combo (GObject'(Get_Data (Inst))).Combo));

      elsif Command = "set_text" then
         Name_Parameters (Data, Set_Text_Args);
         Set_Active_Text
           (Custom_Combo (GObject'(Get_Data (Inst))).Combo,
            Nth_Arg (Data, 2));
      end if;
   end Custom_Entry_Handler;

   --------------------
   -- Button_Handler --
   --------------------

   procedure Button_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle  := Get_Kernel (Custom_Module_ID.all);
      Class  : constant Class_Type     := New_Class (Kernel, "Button");
      Inst   : constant Class_Instance := Nth_Arg (Data, 1, Class);
      Button : Gtk_Button;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Create_Button_Args);
         Gtk_New (Button, Nth_Arg (Data, 3));
         Set_Name (Button, Nth_Arg (Data, 2));
         Set_Data (Inst, Widget => GObject (Button));
         Show_All (Button);
         Subprogram_Callback.Connect
           (Button, Gtk.Button.Signal_Clicked,
            On_Button_Clicked'Access,
            User_Data => Nth_Arg (Data, 4));

         if Nth_Arg (Data, 5, False) then
            Set_Property (Gtk_Button (GObject'(Get_Data (Inst))),
                          Gtk.Button.Use_Stock_Property,
                          True);
         end if;

      elsif Command = "set_text" then
         Name_Parameters (Data, Set_Button_Text_Args);
         Set_Property (Gtk_Button (GObject'(Get_Data (Inst))),
                       Gtk.Button.Label_Property,
                       Nth_Arg (Data, 2));
      end if;
   end Button_Handler;

   -------------------------
   -- Tool_Button_Handler --
   -------------------------

   procedure Tool_Button_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle  := Get_Kernel (Custom_Module_ID.all);
      Class  : constant Class_Type     := New_Class (Kernel, "ToolButton");
      Inst   : constant Class_Instance := Nth_Arg (Data, 1, Class);
      Button : Gtk_Tool_Button;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Create_Tool_Button_Args);
         Gtk_New_From_Stock (Button, Nth_Arg (Data, 2));
         Set_Label (Button, Nth_Arg (Data, 3));
         Set_Data (Inst, Widget => GObject (Button));
         Show_All (Button);
         Subprogram_Callback.Connect
           (Button,
            Gtk.Tool_Button.Signal_Clicked,
            On_Button_Clicked'Access,
            User_Data => Nth_Arg (Data, 4));

      elsif Command = "set_label" then
         Name_Parameters (Data, Set_Button_Text_Args);
         Gtk_Tool_Button (GObject'(Get_Data (Inst))).Set_Label
           (Nth_Arg (Data, 2));
      end if;
   end Tool_Button_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Toolbar_Class : constant Class_Type := New_Class
        (Kernel, "Toolbar", Base => Get_GUI_Class (Kernel));
      Combo_Class   : constant Class_Type := New_Class
        (Kernel, "Combo", Base => Get_GUI_Class (Kernel));
      Button_Class  : constant Class_Type := New_Class
        (Kernel, "Button", Base => Get_GUI_Class (Kernel));
      Tool_Button_Class  : constant Class_Type := New_Class
        (Kernel, "ToolButton", Base => Get_GUI_Class (Kernel));
   begin
      Register_Command
        (Kernel, "get",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Toolbar_Class,
         Handler      => Custom_Toolbar_Handler'Access);
      Register_Command
        (Kernel, "get_by_pos",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Toolbar_Class,
         Handler      => Custom_Toolbar_Handler'Access);
      Register_Command
        (Kernel, Constructor_Method,
         Class   => Toolbar_Class,
         Handler => Custom_Toolbar_Handler'Access);
      Register_Command
        (Kernel, "append",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Toolbar_Class,
         Handler      => Custom_Toolbar_Handler'Access);
      Register_Command
        (Kernel, "insert",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Class        => Toolbar_Class,
         Handler      => Custom_Toolbar_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Combo_Class,
         Handler      => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "remove",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Combo_Class,
         Handler      => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Combo_Class,
         Handler      => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "clear",
         Class   => Combo_Class,
         Handler => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "get_text",
         Class   => Combo_Class,
         Handler => Custom_Entry_Handler'Access);
      Register_Command
        (Kernel, "set_text",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Combo_Class,
         Handler      => Custom_Entry_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 3,
         Maximum_Args => 4,
         Class        => Button_Class,
         Handler      => Button_Handler'Access);
      Register_Command
        (Kernel, "set_text",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Button_Class,
         Handler      => Button_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 3,
         Maximum_Args => 3,
         Class        => Tool_Button_Class,
         Handler      => Tool_Button_Handler'Access);
   end Register_Commands;

end Custom_Combos;
