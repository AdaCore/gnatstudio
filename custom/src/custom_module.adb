-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2008, AdaCore             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GComLin;                   use GComLin;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Scripts.Gtkada;   use GNATCOLL.Scripts, GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with System.Assertions;         use System.Assertions;

with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib;                      use Glib;

with Gtk.Accel_Label;           use Gtk.Accel_Label;
with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Icon_Factory;          use Gtk.Icon_Factory;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;

with Commands.Custom;           use Commands.Custom;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with Custom_Combos;             use Custom_Combos;
with Custom_Timeout;            use Custom_Timeout;
with Expect_Interface;          use Expect_Interface;
with File_Utils;                use File_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GUI_Utils;                 use GUI_Utils;
with Language.Custom;           use Language.Custom;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with Switches_Chooser;          use Switches_Chooser;
with Traces;                    use Traces;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;
with XML_Viewer;

package body Custom_Module is

   Me : constant Debug_Handle := Create ("custom_module");

   type Custom_Module_ID_Record is new Module_ID_Record with null record;

   Path_Cst          : aliased constant String := "path";
   On_Activate_Cst   : aliased constant String := "on_activate";
   Add_Before_Cst    : aliased constant String := "add_before";
   Ref_Cst           : aliased constant String := "ref";
   Name_Cst          : aliased constant String := "name";
   Label_Cst         : aliased constant String := "label";
   Filter_Cst        : aliased constant String := "filter";
   Factory_Cst       : aliased constant String := "factory";
   Group_Cst         : aliased constant String := "group";
   Visibility_Filter_Cst : aliased constant String := "visibility_filter";

   Menu_Get_Params : constant Cst_Argument_List :=
     (1 => Path_Cst'Access);
   Menu_Rename_Params : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Menu_Create_Params : constant Cst_Argument_List :=
     (1 => Path_Cst'Access,
      2 => On_Activate_Cst'Access,
      3 => Ref_Cst'Access,
      4 => Add_Before_Cst'Access,
      5 => Filter_Cst'Access);
   Contextual_Constructor_Params : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Contextual_Create_Params : constant Cst_Argument_List :=
     (1 => On_Activate_Cst'Access,
      2 => Label_Cst'Access,
      3 => Filter_Cst'Access,
      4 => Ref_Cst'Access,
      5 => Add_Before_Cst'Access,
      6 => Group_Cst'Access,
      7 => Visibility_Filter_Cst'Access);
   Contextual_Create_Dynamic_Params : constant Cst_Argument_List :=
     (1 => Factory_Cst'Access,
      2 => On_Activate_Cst'Access,
      3 => Label_Cst'Access,
      4 => Filter_Cst'Access,
      5 => Ref_Cst'Access,
      6 => Add_Before_Cst'Access,
      7 => Group_Cst'Access,
      8 => Visibility_Filter_Cst'Access);

   type Subprogram_Type_Menu_Record is new Gtk_Menu_Item_Record with record
      On_Activate : Subprogram_Type;
   end record;
   type Subprogram_Type_Menu is access all Subprogram_Type_Menu_Record'Class;

   type Action_Filter_Wrapper is new Action_Filter_Record with record
      Filter : Subprogram_Type;
   end record;
   function Filter_Matches_Primitive
     (Filter  : access Action_Filter_Wrapper;
      Context : Selection_Context) return Boolean;
   --  A filter that executes a shell subprogram

   procedure On_Activate (Menu : access Gtk_Widget_Record'Class);
   --  Called when a Subprogram_Type_Menu is activated

   procedure Customize
     (Module : access Custom_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   procedure Menu_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands for GPS.Menu

   procedure Contextual_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands for GPS.Contextual

   type Contextual_Shell_Cmd is new Interactive_Command with record
      Contextual  : Class_Instance;
      On_Activate : Subprogram_Type;
   end record;
   type Contextual_Shell_Command is access all Contextual_Shell_Cmd'Class;
   function Execute
     (Command : access Contextual_Shell_Cmd;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Type used to define contextual menus from a scripting language

   type Contextual_Shell_Filters is new Action_Filter_Record with record
      Contextual : Class_Instance;
      Filter     : Subprogram_Type;
   end record;
   type Contextual_Shell_Filter is access all Contextual_Shell_Filters'Class;
   function Filter_Matches_Primitive
     (Filter  : access Contextual_Shell_Filters;
      Context : Selection_Context) return Boolean;
   --  Type used to define contextual menus from a scripting language

   type Contextual_Shell_Labels is new Contextual_Menu_Label_Creator_Record
   with record
      Contextual : Class_Instance;
      Label      : Subprogram_Type;
   end record;
   type Contextual_Shell_Label is access all Contextual_Shell_Labels'Class;
   function Get_Label
     (Creator : access Contextual_Shell_Labels;
      Context : Selection_Context) return String;
   --  Type used to define contextual menus from a scripting language

   type Create_Dynamic_Contextual is new Submenu_Factory_Record with record
      Contextual  : Class_Instance;
      On_Activate : Subprogram_Type;
      Factory     : Subprogram_Type;
   end record;
   type Create_Dynamic_Contextual_Access is access all
     Create_Dynamic_Contextual'Class;
   procedure Append_To_Menu
     (Factory : access Create_Dynamic_Contextual;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Create a dynamic contextual menu from a script

   type Python_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Index : Natural;
   end record;
   type Python_Menu_Item is access all Python_Menu_Item_Record'Class;
   --  A special kind of menu item associated with an index

   type Dynamic_Context is record
      Contextual : Create_Dynamic_Contextual_Access;
      Context    : Class_Instance;
   end record;

   package Factory_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Menu_Item.Gtk_Menu_Item_Record,
      User_Type   => Dynamic_Context);

   procedure On_Dynamic_Menu_Activate
     (Item    : access Gtk_Menu_Item_Record'Class;
      Factory : Dynamic_Context);
   --  Called when an entry is a dynamic contextual menu created through a
   --  scripting language was activated.

   procedure Parse_Switches_Node
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : GNATCOLL.VFS.Virtual_File;
      Current_Tool : in out Tool_Properties_Record;
      Node         : Node_Ptr);
   --  Parse a <switches> node, and returns the corresponding configuration

   -------------------------
   -- Parse_Switches_Node --
   -------------------------

   procedure Parse_Switches_Node
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : GNATCOLL.VFS.Virtual_File;
      Current_Tool : in out Tool_Properties_Record;
      Node         : Node_Ptr)
   is
      Comlin_Config : Command_Line_Configuration;
      Char       : constant String := Get_Attribute (Node, "switch_char", "-");
      Default_Sep : constant String := Get_Attribute (Node, "separator", "");

      procedure Coordinates_From_Node (N : Node_Ptr; Line, Col : out Natural);
      --  Get the line and column from N

      function Check_Space_In_Switch (Switch : String) return Boolean;
      --  Return True if Switch contains a space

      procedure Process_Title_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Check_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Spin_Node       (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Field_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Radio_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Combo_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Popup_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Dependency_Node (N : Node_Ptr);
      procedure Process_Expansion_Node  (N : Node_Ptr);
      --  Process a child node (resp. <title>, <check>, <spin>, <radio>,
      --  <combo>, <popup>, <dependency>, <expansion>)

      procedure Process_Radio_Entry_Nodes
        (Parent : Node_Ptr; Radio : Radio_Switch);
      function Process_Combo_Entry_Nodes
        (Parent : Node_Ptr) return Combo_Switch_Array;
      --  Return the contents of all the <radio-entry> and
      --  <combo-entry> nodes of Parent

      procedure Parse_Popup_Or_Main
        (N     : Node_Ptr;
         Popup : Popup_Index);
      --  Parse the subnodes of <switches>

      ---------------------------
      -- Check_Space_In_Switch --
      ---------------------------

      function Check_Space_In_Switch (Switch : String) return Boolean is
      begin
         for S in Switch'Range loop
            if Is_Blank (Switch (S)) then
               Insert
                 (Kernel,
                  -("Attribute switch cannot contain spaces. Use the separator"
                    & " attribute if you need to separate the switch and its"
                    & " argument"),
                  Mode => GPS.Kernel.Console.Error);
               return True;
            end if;
         end loop;
         return False;
      end Check_Space_In_Switch;

      ---------------------------
      -- Coordinates_From_Node --
      ---------------------------

      procedure Coordinates_From_Node
        (N : Node_Ptr; Line, Col : out Natural)
      is
      begin
         Line := Safe_Value (Get_Attribute (N, "line", "1"));
         Col  := Safe_Value (Get_Attribute (N, "column", "1"));
      end Coordinates_From_Node;

      ------------------------
      -- Process_Title_Node --
      ------------------------

      procedure Process_Title_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Line_Span : constant Integer :=
                       Safe_Value (Get_Attribute (N, "line-span", "1"));
         Col_Span  : constant Integer :=
                       Safe_Value (Get_Attribute (N, "column-span", "1"));
      begin
         Coordinates_From_Node (N, Line, Col);
         Set_Frame_Title
           (Current_Tool.Config,
            Title     => N.Value.all,
            Line      => Line,
            Column    => Col,
            Line_Span => Line_Span,
            Col_Span  => Col_Span,
            Popup     => Popup);
      end Process_Title_Node;

      -----------------------------
      -- Process_Dependency_Node --
      -----------------------------

      procedure Process_Dependency_Node (N : Node_Ptr) is
         Master_Page   : constant String := Get_Attribute (N, "master-page");
         Master_Switch : constant String := Get_Attribute (N, "master-switch");
         Slave_Page    : constant String := Get_Attribute (N, "slave-page");
         Slave_Switch  : constant String := Get_Attribute (N, "slave-switch");
         Master_Status : constant String :=
                           Get_Attribute (N, "master-status", "true");
         Slave_Status  : constant String :=
           Get_Attribute (N, "slave-status", "true");
         Tool          : Tool_Properties_Record;
         Config        : Switches_Editor_Config;
      begin
         if Master_Page = ""
           or else Master_Switch = ""
           or else Slave_Page = ""
           or else Slave_Switch = ""
         then
            Insert
              (Kernel,
                 -("Invalid <dependency> node in custom file,"
                 & " all attributes must be specified, in file "
                 & Full_Name (File).all),
               Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Master_Page = Current_Tool.Tool_Name.all then
            Config := Current_Tool.Config;
         else
            Tool := Get_Tool_Properties (Kernel, Master_Page);
            if Tool /= No_Tool then
               Config := Tool.Config;
            else
               Insert
                 (Kernel,
                  -("<dependency> node in custom file references"
                    & " unknown tool: ")
                  & Master_Page & (-" in file ") & Full_Name (File).all,
                  Mode => GPS.Kernel.Console.Error);
               return;
            end if;
         end if;

         Add_Dependency
           (Config,
            Master_Switch,
            Master_Status = "true" or else Master_Status = "on",
            Slave_Page,
            Slave_Switch,
            Slave_Status = "true" or else Slave_Status = "on");
      end Process_Dependency_Node;

      -------------------------------
      -- Process_Radio_Entry_Nodes --
      -------------------------------

      procedure Process_Radio_Entry_Nodes
        (Parent : Node_Ptr;
         Radio  : Radio_Switch)
      is
         N            : Node_Ptr := Parent.Child;
      begin
         while N /= null loop
            if N.Tag.all = "radio-entry" then
               declare
                  Label  : constant String := Get_Attribute (N, "label");
                  Switch : constant String := Get_Attribute (N, "switch");
               begin
                  if Label = "" then
                     Insert
                       (Kernel,
                          -("Invalid <radio-entry> node in custom file,"
                            & " requires a label and a switch attributes"),
                        Mode => GPS.Kernel.Console.Error);
                     return;
                  end if;

                  if Check_Space_In_Switch (Switch) then
                     return;
                  end if;

                  Add_Radio_Entry
                    (Config => Current_Tool.Config,
                     Radio  => Radio,
                     Label  => Label,
                     Switch => Switch,
                     Tip    => Get_Attribute (N, "tip"));
               end;
            end if;
            N := N.Next;
         end loop;
      end Process_Radio_Entry_Nodes;

      -------------------------------
      -- Process_Combo_Entry_Nodes --
      -------------------------------

      function Process_Combo_Entry_Nodes
        (Parent : Node_Ptr) return Combo_Switch_Array
      is
         N            : Node_Ptr := Parent.Child;
         Num_Children : Natural := 0;
      begin
         while N /= null loop
            if N.Tag.all = "combo-entry" then
               Num_Children := Num_Children + 1;
            end if;
            N := N.Next;
         end loop;

         declare
            Buttons : Combo_Switch_Array (1 .. Num_Children);
         begin
            N := Parent.Child;
            for B in Buttons'Range loop
               while N.Tag.all /= "combo-entry" loop
                  N := N.Next;
               end loop;

               declare
                  Label : constant String := Get_Attribute (N, "label");
                  Value : constant String := Get_Attribute (N, "value");
               begin
                  if Label = "" or else Value = "" then
                     Insert
                       (Kernel,
                          -("Invalid <combo-entry> node in custom file,"
                            & " requires a label and a switch attributes"),
                        Mode => GPS.Kernel.Console.Error);
                     return Buttons (1 .. 0);
                  end if;

                  Buttons (B) :=
                    (Label => To_Unbounded_String (Label),
                     Value => To_Unbounded_String (Value));
               end;

               N := N.Next;
            end loop;

            return Buttons;
         end;
      end Process_Combo_Entry_Nodes;

      ------------------------
      -- Process_Radio_Node --
      ------------------------

      procedure Process_Radio_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         R         : Radio_Switch;
      begin
         Coordinates_From_Node (N, Line, Col);

         R := Add_Radio
           (Config => Current_Tool.Config,
            Line   => Line,
            Column => Col,
            Popup  => Popup);
         Process_Radio_Entry_Nodes (N, R);
      end Process_Radio_Node;

      ------------------------
      -- Process_Popup_Node --
      ------------------------

      procedure Process_Popup_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Pop       : Popup_Index;
      begin
         Coordinates_From_Node (N, Line, Col);
         if Label = "" then
            Insert
              (Kernel,
                 -("Invalid <popup> node in custom file,"
                   & " requires a label attributes"),
               Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         Pop := Add_Popup
           (Config  => Current_Tool.Config,
            Label   => Label,
            Lines   => Safe_Value (Get_Attribute (N, "lines", "1")),
            Columns => Safe_Value (Get_Attribute (N, "columns", "1")),
            Line    => Line,
            Column  => Col,
            Popup   => Popup);

         Parse_Popup_Or_Main
           (N     => N,
            Popup => Pop);
      end Process_Popup_Node;

      ------------------------
      -- Process_Combo_Node --
      ------------------------

      procedure Process_Combo_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Switch = "" then
            Insert (Kernel,
                      -("Invalid <combo> node in custom file, requires"
                        & " a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Combo
           (Config    => Current_Tool.Config,
            Label     => Label,
            Switch    => Switch,
            Separator => Get_Attribute (N, "separator", " "),
            No_Switch => Get_Attribute (N, "noswitch"),
            No_Digit  => Get_Attribute (N, "nodigit"),
            Entries   => Process_Combo_Entry_Nodes (N),
            Tip       => Get_Attribute (N, "tip"),
            Line      => Line,
            Column    => Col,
            Popup     => Popup);
      end Process_Combo_Node;

      ------------------------
      -- Process_Field_Node --
      ------------------------

      procedure Process_Field_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label   : constant String := Get_Attribute (N, "label");
         Switch  : constant String := Get_Attribute (N, "switch");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <field> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Field
           (Current_Tool.Config,
            Label        => Label,
            Switch       => Switch,
            Separator    => Get_Attribute (N, "separator", Default_Sep),
            Tip          => Get_Attribute (N, "tip"),
            As_Directory =>
              Get_Attribute (N, "as-directory", "false") = "true",
            As_File      => Get_Attribute (N, "as-file", "false") = "true",
            Line         => Line,
            Column       => Col,
            Popup        => Popup);
      end Process_Field_Node;

      -----------------------
      -- Process_Spin_Node --
      -----------------------

      procedure Process_Spin_Node  (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <spin> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Spin
           (Config    => Current_Tool.Config,
            Label     => Label,
            Switch    => Switch,
            Tip       => Get_Attribute (N, "tip"),
            Separator => Get_Attribute (N, "separator", Default_Sep),
            Min       => Safe_Value (Get_Attribute (N, "min", "1")),
            Max       => Safe_Value (Get_Attribute (N, "max", "1")),
            Default   => Safe_Value (Get_Attribute (N, "default", "1")),
            Line      => Line,
            Column    => Col,
            Popup     => Popup);
      end Process_Spin_Node;

      ------------------------
      -- Process_Check_Node --
      ------------------------

      procedure Process_Check_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <check> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Check
           (Config => Current_Tool.Config,
            Label  => Label,
            Switch => Switch,
            Tip    => Get_Attribute (N, "tip"),
            Line   => Line,
            Column => Col,
            Popup  => Popup);
      end Process_Check_Node;

      ----------------------------
      -- Process_Expansion_Node --
      ----------------------------

      procedure Process_Expansion_Node (N : Node_Ptr) is
         Switch       : constant String := Get_Attribute (N, "switch");
         Alias        : constant String := Get_Attribute (N, "alias");
      begin
         if Switch = "" then
            Insert (Kernel,
                      -("Invalid <expansion> node in custom file, requires"
                        & " a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         if Alias = "" then
            Define_Prefix (Comlin_Config, Prefix => Switch);
         else
            Define_Alias  (Comlin_Config, Switch, Alias);
         end if;
      end Process_Expansion_Node;

      -------------------------
      -- Parse_Popup_Or_Main --
      -------------------------

      procedure Parse_Popup_Or_Main
        (N     : Node_Ptr;
         Popup : Popup_Index)
      is
         N2 : Node_Ptr := N.Child;
      begin
         while N2 /= null loop
            if N2.Tag.all = "title" then
               Process_Title_Node (N2, Popup);
            elsif N2.Tag.all = "check" then
               Process_Check_Node (N2, Popup);
            elsif N2.Tag.all = "spin" then
               Process_Spin_Node (N2, Popup);
            elsif N2.Tag.all = "radio" then
               Process_Radio_Node (N2, Popup);
            elsif N2.Tag.all = "field" then
               Process_Field_Node (N2, Popup);
            elsif N2.Tag.all = "combo" then
               Process_Combo_Node (N2, Popup);
            elsif N2.Tag.all = "popup" then
               Process_Popup_Node (N2, Popup);
            elsif N2.Tag.all = "dependency" then
               Process_Dependency_Node (N2);
            elsif N2.Tag.all = "expansion" then
               Process_Expansion_Node (N2);
            else
               Insert (Kernel,
                       -"Invalid xml tag child for <switches>: "
                       & N2.Tag.all);
            end if;

            N2 := N2.Next;
         end loop;
      exception
         when E : others => Trace (Exception_Handle, E);
      end Parse_Popup_Or_Main;

   begin
      Current_Tool.Config := Create
        (Default_Separator => Default_Sep,
         Switch_Char     => Char (Char'First),
         Scrolled_Window => Boolean'Value
           (Get_Attribute (Node, "use_scrolled_window", "false")),
         Show_Command_Line => Boolean'Value
           (Get_Attribute (Node, "show_command_line", "true")),
         Lines           => Safe_Value (Get_Attribute (Node, "lines", "1")),
         Columns         => Safe_Value (Get_Attribute (Node, "columns", "1")));

      Parse_Popup_Or_Main (Node, Main_Window);

      --  Set the configuration only after it has potentially
      --  been created by Parsing_Switches_XML
      Set_Configuration (Current_Tool.Config, Comlin_Config);
   end Parse_Switches_Node;

   ------------------------------
   -- On_Dynamic_Menu_Activate --
   ------------------------------

   procedure On_Dynamic_Menu_Activate
     (Item    : access Gtk_Menu_Item_Record'Class;
      Factory : Dynamic_Context)
   is
      Script : constant Scripting_Language :=
        Get_Script (Factory.Contextual.Contextual);
      C : Callback_Data'Class := Create (Script, Arguments_Count => 3);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg (C, 1, Factory.Context);
      Set_Nth_Arg (C, 2, Get_Text (Gtk_Label (Get_Child (Item))));
      Set_Nth_Arg (C, 3, Python_Menu_Item (Item).Index);
      Tmp := Execute (Factory.Contextual.On_Activate, C);
      Free (C);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Dynamic_Menu_Activate;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Factory : access Create_Dynamic_Contextual;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Script : constant Scripting_Language := Get_Script (Factory.Contextual);
      Contextual_Class : constant Class_Type := New_Class
        (Get_Kernel (Script), "Contextual");
      C : Callback_Data'Class := Create (Script, Arguments_Count => 1);
      Item : Python_Menu_Item;
   begin
      Trace (Me, "Append_To_Menu "
             & String'(Get_Data (Factory.Contextual, Contextual_Class)));

      Set_Nth_Arg
        (C, 1, Create_Context (Get_Script (Factory.Contextual), Context));

      declare
         List : String_List := Execute (Factory.Factory, C);
      begin
         for L in List'Range loop
            if List (L) /= null then
               Item := new Python_Menu_Item_Record;
               Item := Python_Menu_Item
                 (Find_Or_Create_Menu_Tree
                    (Menu_Bar      => null,
                     Menu          => Gtk_Menu (Menu),
                     Path          => List (L).all,
                     Use_Mnemonics => False,
                     Accelerators  =>
                       Get_Default_Accelerators (Get_Kernel (Script)),
                     New_Item      => Gtk_Menu_Item (Item)));

               Item.Index := L - List'First;
               Factory_Callback.Connect
                 (Item, Signal_Activate, On_Dynamic_Menu_Activate'Access,
                  User_Data =>
                    (Contextual => Create_Dynamic_Contextual_Access (Factory),
                     Context    => Create_Context (Script, Context)));
            end if;
         end loop;

         Free (List);
      end;

      Show_All (Menu);
      Free (C);
   end Append_To_Menu;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Contextual_Shell_Cmd;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      C : Callback_Data'Class := Create
        (Get_Script (Command.Contextual), Arguments_Count => 1);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg
        (C, 1, Create_Context
           (Get_Script (Command.Contextual),
            Selection_Context (Context.Context)));
      Tmp := Execute (Command.On_Activate, C);
      Free (C);
      return Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Contextual_Shell_Filters;
      Context : Selection_Context) return Boolean
   is
      C : Callback_Data'Class := Create
        (Get_Script (Filter.Filter.all), Arguments_Count => 1);
      Tmp : Boolean;
   begin
      Set_Nth_Arg
        (C, 1, Create_Context (Get_Script (Filter.Filter.all), Context));
      Tmp := Execute (Filter.Filter, C);
      Free (C);
      return Tmp;
   end Filter_Matches_Primitive;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Creator : access Contextual_Shell_Labels;
      Context : Selection_Context) return String
   is
      C : Callback_Data'Class := Create
        (Get_Script (Creator.Label.all), Arguments_Count => 1);
   begin
      Set_Nth_Arg
        (C, 1, Create_Context (Get_Script (Creator.Label.all), Context));
      declare
         Str : constant String := Execute (Creator.Label, C);
      begin
         Free (C);
         return Str;
      end;
   end Get_Label;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Custom_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);

      Kernel  : constant Kernel_Handle := Get_Kernel (Module.all);
      Handler : constant Language_Handler := Language_Handler
        (Get_Language_Handler (Kernel));

      procedure Add_Child
        (Parent_Path  : String;
         Current_Node : Node_Ptr);
      --  Add a menuitem or submenu to the Parent_Path, according to
      --  what Current_Node contains.

      procedure Parse_Action_Node (Node : Node_Ptr);
      procedure Parse_Contextual_Node (Node : Node_Ptr);
      procedure Parse_Button_Node (Node : Node_Ptr);
      procedure Parse_Entry_Node (Node : Node_Ptr);
      procedure Parse_Tool_Node (Node : Node_Ptr);
      procedure Parse_Stock_Node (Node : Node_Ptr);
      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : UTF8_String);
      procedure Parse_Submenu_Node
        (Node : Node_Ptr; Parent_Path : UTF8_String);
      function Parse_Filter_Node (Node : Node_Ptr;
                                  Name : String) return Action_Filter;
      --  Parse the various nodes: <action>, <shell>, ...

      ---------------------------
      -- Parse_Contextual_Node --
      ---------------------------

      procedure Parse_Contextual_Node (Node : Node_Ptr) is
         Action  : constant String := Get_Attribute (Node, "action");
         Before  : constant String := Get_Attribute (Node, "before", "");
         After   : constant String := Get_Attribute (Node, "after", "");
         Child   : Node_Ptr;
         Title   : GNAT.OS_Lib.String_Access;
         Command : Action_Record_Access;
      begin
         Title := new String'(Action);

         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);

            else
               Insert
                 (Kernel,
                  -"Invalid child node for <contextual>: " & Child.Tag.all,
                  Mode => Error);
               Free (Title);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         if Action /= "" then
            Command := Lookup_Action (Kernel, Action);
            if Command.Command = null then
               Insert (Kernel,
                       -"Command not found when creating contextual menu: "
                       & Action,
                       Mode => Error);
               Free (Title);
               raise Assert_Failure;
            end if;
         else
            Command := null;
         end if;

         if Before /= "" then
            Register_Contextual_Menu
              (Kernel,
               Name       => Title.all,
               Label      => Title.all,
               Action     => Command,
               Ref_Item   => Before,
               Add_Before => True);
         elsif After /= "" then
            Register_Contextual_Menu
              (Kernel,
               Name       => Title.all,
               Label      => Title.all,
               Action     => Command,
               Ref_Item   => After,
               Add_Before => False);
         else
            Register_Contextual_Menu
              (Kernel,
               Name       => Title.all,
               Label      => Title.all,
               Action     => Command);
         end if;

         Free (Title);
      end Parse_Contextual_Node;

      ---------------------
      -- Parse_Tool_Node --
      ---------------------

      procedure Parse_Tool_Node (Node : Node_Ptr) is
         Name      : constant String := Get_Attribute (Node, "name");
         Pack      : constant String :=
                       Get_Attribute (Node, "package", Projects.Ide_Package);
         Index     : constant String :=
                       To_Lower (Get_Attribute (Node, "index", Name));
         Attribute : constant String :=
                       Get_Attribute (Node, "attribute", "default_switches");
         Override  : constant Boolean :=
                       (To_Lower (Get_Attribute (Node, "override", "false"))
                        = "true");
         N         : Node_Ptr := Node.Child;
         Tool      : Tool_Properties_Record;

      begin
         if Name = "" then
            Insert (Kernel,
                    -"Invalid <tool> node, it must have a name attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         Tool.Tool_Name         := new String'(Name);
         Tool.Project_Package   := new String'(Pack);
         Tool.Project_Attribute := new String'(Attribute);
         Tool.Project_Index     := new String'(Index);
         Tool.Override          := Override;

         while N /= null loop
            if N.Tag.all = "initial-cmd-line" then
               Free (Tool.Initial_Cmd_Line);
               Tool.Initial_Cmd_Line := new String'(N.Value.all);

            elsif N.Tag.all = "language" then
               Append
                 (Tool.Languages, (1 => new String'(To_Lower (N.Value.all))));

            elsif N.Tag.all = "switches" then
               Parse_Switches_Node
                 (Kernel       => Kernel,
                  File         => File,
                  Current_Tool => Tool,
                  Node         => N);

            else
               Insert (Kernel,
                       -"Unsupport child tag for <tool>: " & N.Tag.all,
                       Mode => Error);
            end if;

            N := N.Next;
         end loop;

         Register_Tool (Kernel, Tool);
      end Parse_Tool_Node;

      -----------------------
      -- Parse_Filter_Node --
      -----------------------

      function Parse_Filter_Node (Node : Node_Ptr;
                                  Name : String) return Action_Filter is
         Filter, Filter_Tmp : Action_Filter;
         Child  : Node_Ptr;
      begin
         if Node.Tag.all = "filter" then
            declare
               Lang    : constant String  := Get_Attribute (Node, "language");
               Shell   : constant String  := Get_Attribute (Node, "shell_cmd");
               Shell_Lang : constant String :=
                              Get_Attribute
                                (Node, "shell_lang", GPS_Shell_Name);
               Module  : constant String := Get_Attribute (Node, "module");
               Id      : constant String := Get_Attribute (Node, "id");
            begin
               if Id /= "" then
                  Filter := Lookup_Filter (Kernel, Id);

                  if Filter = null then
                     Insert
                       (Kernel,
                          -"Unknown action filter " & Id,
                        Mode => Error);
                     raise Assert_Failure;
                  end if;

                  if Lang /= "" or
                    Shell /= "" or
                    Shell_Lang /= GPS_Shell_Name or
                    Module /= ""
                  then
                     Insert
                       (Kernel,
                        -"Filter " & Name & ": Id shall be the only " &
                        "attribute when defined.",
                        Mode => Error);
                     if Lang /= "" then
                        Insert (Kernel,
                                -" Attribute language=" & Lang & " ignored.",
                                Mode => Error);
                     end if;
                     if Shell /= "" then
                        Insert (Kernel,
                                -" Attribute shell_cmd=" & Shell & " ignored.",
                                Mode => Error);
                     end if;
                     if Shell_Lang /= GPS_Shell_Name then
                        Insert (Kernel,
                                -" Attribute shell_lang=" & Shell_Lang &
                                " ignored.",
                                Mode => Error);
                     end if;
                     if Module /= "" then
                        Insert (Kernel,
                                -" Attribute module=" & Module & " ignored.",
                                Mode => Error);
                     end if;
                  end if;
               else
                  Filter := Action_Filter
                    (Create
                       (Language   => Lang,
                        Shell      => Shell,
                        Shell_Lang => Shell_Lang,
                        Module     => Module));
                  Set_Error_Message (Filter, Get_Attribute (Node, "error"));
               end if;
            end;

         else
            Child := Node.Child;
            while Child /= null loop
               if Child.Tag.all = "filter"
                 or else Child.Tag.all = "filter_and"
                 or else Child.Tag.all = "filter_or"
               then
                  Filter_Tmp := Parse_Filter_Node (Child, Name);

                  if Filter = null then
                     Filter := Filter_Tmp;
                  elsif Node.Tag.all = "filter_and" then
                     Filter := Action_Filter (Filter and Filter_Tmp);
                  elsif Node.Tag.all = "filter_or" then
                     Filter := Action_Filter (Filter or Filter_Tmp);
                  end if;
               end if;

               Child := Child.Next;
            end loop;

            Set_Error_Message (Filter, Get_Attribute (Node, "error"));
         end if;

         return Filter;
      end Parse_Filter_Node;

      -----------------------
      -- Parse_Action_Node --
      -----------------------

      procedure Parse_Action_Node (Node : Node_Ptr) is
         Name            : constant String := Get_Attribute (Node, "name");
         Category        : constant String :=
                             Get_Attribute (Node, "category", "General");
         Child           : Node_Ptr;
         Command         : Custom_Command_Access;
         Description     : GNAT.OS_Lib.String_Access := new String'("");
         Filter_A        : Action_Filter;
         Implicit_Filter : Action_Filter;
      begin
         if Name = "" then
            Insert
              (Kernel,
               -("<action> tags in customization files must have"
                 & " a name attribute"),
               Mode => Error);
            raise Assert_Failure;  --  So that the name of the file is shown
         end if;

         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "shell"
              or else To_Lower (Child.Tag.all) = "external"
              or else To_Lower (Child.Tag.all) = "on-failure"
            then
               --  Handled directly by Commands.Custom
               null;

            elsif Child.Tag.all = "filter"
              or else Child.Tag.all = "filter_and"
              or else Child.Tag.all = "filter_or"
            then
               if Filter_A /= null then
                  Filter_A := Action_Filter
                    (Filter_A or Parse_Filter_Node (Child, Name));
               else
                  Filter_A := Parse_Filter_Node (Child, Name);
               end if;

            elsif To_Lower (Child.Tag.all) = "description" then
               Free (Description);
               Description := new String'(Child.Value.all);

            else
               Insert
                 (Kernel,
                  -"Invalid child node for <action> tag",
                  Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         Create (Command,
                 Name                 => Name,
                 Kernel               => Kernel,
                 Command              => Node.Child,
                 Default_Output       => Get_Attribute
                   (Node, "output", Console_Output),
                 Show_In_Task_Manager => To_Lower
                   (Get_Attribute
                     (Node, "show-task-manager", "false")) = "true",
                 Show_Command         => To_Lower
                   (Get_Attribute (Node, "show-command", "true")) = "true");

         Implicit_Filter := Create_Filter (Node.Child);

         if Implicit_Filter /= null then
            if Filter_A /= null then
               declare
                  Error : constant String := Get_Error_Message (Filter_A);
               begin
                  Filter_A := Action_Filter (Filter_A and Implicit_Filter);
                  Set_Error_Message (Filter_A, Error);
               end;
            else
               Filter_A := Implicit_Filter;
            end if;
         end if;

         Register_Action
           (Kernel,
            Name        => Name,
            Command     => Command,
            Description => Description.all,
            Defined_In  => File,
            Category    => Category,
            Filter      => Filter_A);
         Free (Description);
      end Parse_Action_Node;

      -----------------------
      -- Parse_Button_Node --
      -----------------------

      procedure Parse_Button_Node (Node : Node_Ptr) is
         Action  : constant String := Get_Attribute (Node, "action");
         Child   : Node_Ptr;
         Title   : GNAT.OS_Lib.String_Access := new String'("");
         Pixmap  : GNAT.OS_Lib.String_Access := new String'("");
         Image   : Gtk_Image;
         Command : Action_Record_Access;
         Space   : Gtk_Separator_Tool_Item;

      begin
         Child := Node.Child;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);
            elsif To_Lower (Child.Tag.all) = "pixmap" then
               Free (Pixmap);
               Pixmap := new String'(Child.Value.all);
            else
               Insert
                 (Kernel, -"Invalid child node for <button> tag",
                  Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         if Title.all /= "" then
            if Action = "" then
               Insert (Kernel, -"<button> nodes must have an action attribute",
                       Mode => Error);
               raise Assert_Failure;
            end if;

            if Pixmap.all /= ""
              and then Is_Regular_File (Pixmap.all)
            then
               Gtk_New (Image, Pixmap.all);
            end if;

            Command := Lookup_Action (Kernel, Action);

            if Command /= null and then Command.Command /= null then
               Register_Button
                 (Kernel,
                  Title.all,
                  Command.Command,
                  Image,
                  Tooltip => Title.all);
            end if;

         else
            Gtk_New (Space);
            Set_Draw (Space, True);
            Show_All (Space);
            Insert (Get_Toolbar (Kernel), Space, -1);
         end if;

         Free (Title);
         Free (Pixmap);
      end Parse_Button_Node;

      ----------------------
      -- Parse_Entry_Node --
      ----------------------

      procedure Parse_Entry_Node (Node : Node_Ptr) is
         Child : Node_Ptr;
         Id    : constant String := Get_Attribute (Node, "id", "");
         Label : constant String := Get_Attribute (Node, "label", "");
      begin
         if Id = "" then
            Insert
              (Kernel,
               -"<entry> nodes must have a non-empty ""id"" attribute",
               Mode => Error);
            return;
         end if;

         --  The creation of the new combo and its entries is done through
         --  shell commands, so that the action is automatically converted to
         --  a Subprogram_Type. This also limits the direct dependencies
         --  between the packages.

         Execute_GPS_Shell_Command
           (Kernel => Kernel,
            Command =>
              "Toolbar; Toolbar.append %1 """
              & Id & """ """ & Label & """ """
              & Get_Attribute (Node, "on-changed") & """");

         --  Parse the child nodes

         Child := Node.Child;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "choice" then
               --  ??? Need to implement "default" attribute.
               declare
                  On_Selected : constant String :=
                    Get_Attribute (Child, "on-selected");
               begin
                  if On_Selected /= "" then
                     Execute_GPS_Shell_Command
                       (Kernel,
                        "Toolbar; Toolbar.entry %1 """ & Id
                        & """; ToolbarEntry.add %1 """ & Child.Value.all
                        & """ """ & On_Selected & """");
                  else
                     Execute_GPS_Shell_Command
                       (Kernel,
                        "Toolbar; Toolbar.entry %1 """ & Id
                        & """; ToolbarEntry.add %1 """ & Child.Value.all
                        & """");
                  end if;
               end;
            else
               Insert
                 (Kernel,
                  -"Invalid child node for <entry> tag: " & Child.Tag.all,
                  Mode => Error);
            end if;

            Child := Child.Next;
         end loop;
      end Parse_Entry_Node;

      ------------------------
      -- Parse_Submenu_Node --
      ------------------------

      procedure Parse_Submenu_Node
        (Node : Node_Ptr; Parent_Path : UTF8_String)
      is
         Before : constant String := Get_Attribute (Node, "before");
         After  : constant String := Get_Attribute (Node, "after");
         Child  : Node_Ptr := Node.Child;
         Title  : GNAT.OS_Lib.String_Access := new String'("");
      begin
         --  First look for the title of the submenu
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               if Title.all /= "" then
                  Insert (Kernel,
                          -"Only one <title> node allowed in <submenu>",
                          Mode => Error);
                  raise Assert_Failure;
               end if;

               Free (Title);
               Title := new String'(Child.Value.all);
            end if;
            Child := Child.Next;
         end loop;

         Child := Node.Child;

         Trace (Me, "Creating submenu with title=" & Title.all);

         --  If specific locations are specified, create the menu now

         if Before /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path =>
                 Name_As_Directory (Parent_Path, UNIX) & Title.all,
               Item        => null,
               Ref_Item    => Before,
               Add_Before  => True);
         elsif After /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path =>
                 Name_As_Directory (Parent_Path, UNIX) & Title.all,
               Item        => null,
               Ref_Item    => After,
               Add_Before  => False);
         end if;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               null; --  Already handled
            elsif To_Lower (Child.Tag.all) = "submenu" then
               Parse_Submenu_Node
                 (Child, Name_As_Directory (Parent_Path, UNIX) & Title.all);
            elsif To_Lower (Child.Tag.all) = "menu" then
               Parse_Menu_Node
                 (Child, Name_As_Directory (Parent_Path, UNIX) & Title.all);
            elsif To_Lower (Child.Tag.all) = "menu_item"
              or else To_Lower (Child.Tag.all) = "toolbar_item"
            then
               Insert
                 (Kernel,
                  -("<menu_item> and <toolbar_item> are no longer"
                    & " supported. Please use the program"
                    & " gps2custom-1.3 to convert to the new format."),
                  Mode => Error);
               raise Assert_Failure;
            else
               Insert (Kernel,
                       -"Invalid child node for <submenu>: "
                       & Child.Tag.all,
                       Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         Free (Title);
      end Parse_Submenu_Node;

      ---------------------
      -- Parse_Menu_Node --
      ---------------------

      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : UTF8_String) is
         Action  : constant String := Get_Attribute (Node, "action");
         Before  : constant String := Get_Attribute (Node, "before");
         After   : constant String := Get_Attribute (Node, "after");
         Child   : Node_Ptr;
         Title   : GNAT.OS_Lib.String_Access := new String'("");
         Item    : Gtk_Menu_Item;
         Command : Action_Record_Access;
      begin
         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);
            else
               Insert
                 (Kernel, -"Invalid child node for <menu> tag", Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         --  Special case to allow separators
         if Action = ""
           and then Title.all /= ""
         then
            Insert (Kernel, -"<menu> nodes must have an action attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         if Title.all = "" then
            Gtk_New (Item);
            Register_Menu (Kernel, Parent_Path, Item);
         else
            Command := Lookup_Action (Kernel, Action);
            if Command /= null and then Command.Command /= null then

               if Before /= "" then
                  Register_Menu
                    (Kernel,
                     Dir_Name
                       (Name_As_Directory (Parent_Path, UNIX) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command,
                     Ref_Item    => Before);

               elsif After /= "" then
                  Register_Menu
                    (Kernel,
                     Dir_Name
                       (Name_As_Directory (Parent_Path, UNIX) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command,
                     Ref_Item    => After,
                     Add_Before  => False);

               else
                  Register_Menu
                    (Kernel,
                     Dir_Name
                       (Name_As_Directory (Parent_Path, UNIX) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command);
               end if;
            else
               Insert
                 (Kernel, -"Command not found for creating menu: "
                  & Action, Mode => Error);
            end if;
         end if;
         Free (Title);
      end Parse_Menu_Node;

      ----------------------
      -- Parse_Stock_Node --
      ----------------------

      procedure Parse_Stock_Node (Node : Node_Ptr) is
         Child   : Node_Ptr := Node.Child;
         Factory : constant Gtk_Icon_Factory := Get_Icon_Factory (Kernel);
         Source  : Gtk_Icon_Source;
         Set     : Gtk_Icon_Set;

         procedure Add_Alternate_Sources;
         --  Add the alternate sources to the icon set.

         ---------------------------
         -- Add_Alternate_Sources --
         ---------------------------

         procedure Add_Alternate_Sources is
            Files    : Node_Ptr := Child.Child;
            Pic_File : GNATCOLL.VFS.Virtual_File;
         begin
            while Files /= null loop
               if To_Lower (Files.Tag.all) = "alternate" then
                  declare
                     Filename : constant String :=
                       Get_Attribute (Files, "file");
                  begin
                     if Filename = "" then
                        Insert
                          (Kernel,
                           -"No alternate file specified for icon "
                           & Get_Attribute (Child, "id"), Mode => Error);
                     else
                        if Is_Absolute_Path (Filename) then
                           Pic_File := Create (Filename);
                        else
                           if File = GNATCOLL.VFS.No_File then
                              Pic_File := Create (Get_Current_Dir & Filename);
                           else
                              Pic_File := Create
                                (Dir_Name (File).all & Filename);
                           end if;
                        end if;

                        if Is_Regular_File (Pic_File) then
                           Source := Gtk_New;
                           Set_Filename (Source, Full_Name (Pic_File).all);

                           declare
                              Size : Gtk.Enums.Gtk_Icon_Size;
                           begin
                              Size := Gtk.Enums.Gtk_Icon_Size'Value
                                (Get_Attribute (Files, "size"));
                              Set_Size (Source, Size);
                              Set_Size_Wildcarded (Source, False);
                           exception
                              when Constraint_Error =>
                                 Insert
                                   (Kernel,
                                    -("No valid size specified for alternate"
                                      & " image to use for icon ")
                                    & Get_Attribute (Child, "id"),
                                    Mode => Error);
                           end;

                           Add_Source (Set, Source);
                           Free (Source);
                        else
                           Insert
                             (Kernel,
                              -"Error when creating stock icon "
                              & Get_Attribute (Child, "id")
                              & (-". File not found: ")
                              & Full_Name (Pic_File).all);
                        end if;

                     end if;
                  end;
               else
                  Insert
                    (Kernel,
                     -"child for <icon> node not recognized: " &
                     Files.Tag.all);
               end if;

               Files := Files.Next;
            end loop;
         end Add_Alternate_Sources;

      begin
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "icon" then
               declare
                  Id       : constant String := Get_Attribute (Child, "id");
                  Filename : constant String := Get_Attribute (Child, "file");
                  Pic_File : GNATCOLL.VFS.Virtual_File;
               begin
                  if Id = "" then
                     Insert
                       (Kernel,
                        -"No id specified for stock icon.",
                        Mode => Error);

                  elsif Filename = "" then
                     Insert
                       (Kernel,
                        -"No file specified for stock icon " & Id,
                        Mode => Error);

                  else
                     if Is_Absolute_Path (Filename) then
                        Pic_File := Create (Filename);
                     else
                        if File = GNATCOLL.VFS.No_File then
                           Pic_File := Create (Get_Current_Dir & Filename);
                        else
                           Pic_File := Create (Dir_Name (File).all & Filename);
                        end if;
                     end if;

                     if Is_Regular_File (Pic_File) then
                        Set    := Gtk_New;

                        Source := Gtk_New;
                        Set_Filename (Source, Full_Name (Pic_File).all);
                        Add_Source (Set, Source);
                        Free (Source);

                        Add_Alternate_Sources;

                        Add (Factory, Id, Set);

                        declare
                           Stock : Gtk_Stock_Item;
                        begin
                           Gtk_New
                             (Stock, Id,
                              -Get_Attribute (Child, "label"),
                             0, 0, "");
                           Add (Stock);
                        end;

                     else
                        Insert
                          (Kernel,
                           -"Error when creating stock icon " & Id
                           & (-". File not found: ")
                           & Full_Name (Pic_File).all);
                     end if;
                  end if;
               end;
            end if;

            Child := Child.Next;
         end loop;
      end Parse_Stock_Node;

      ---------------
      -- Add_Child --
      ---------------

      procedure Add_Child (Parent_Path : String; Current_Node : Node_Ptr) is
         Lang : Custom_Language_Access;
      begin
         if Current_Node = null
           or else Current_Node.Tag = null
         then
            return;
         end if;

         if To_Lower (Current_Node.Tag.all) = "language" then
            --  ??? Lang is never freed
            Lang := new Language.Custom.Custom_Language;
            Initialize (Lang, Handler, Kernel, Current_Node);

         elsif Current_Node.Tag.all = "menu" then
            Parse_Menu_Node (Current_Node, Parent_Path);

         elsif To_Lower (Current_Node.Tag.all) = "submenu" then
            Parse_Submenu_Node (Current_Node, Parent_Path);

         elsif To_Lower (Current_Node.Tag.all) = "action" then
            Parse_Action_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "contextual" then
            Parse_Contextual_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "filter"
           or else To_Lower (Current_Node.Tag.all) = "filter_and"
           or else To_Lower (Current_Node.Tag.all) = "filter_or"
         then
            declare
               Name   : constant String :=
                          Get_Attribute (Current_Node, "name");
               Filter : Action_Filter;
            begin
               if Name = "" then
                  Insert
                    (Kernel,
                     -("<filter>, <filter_and> and <filter_or> tags must have"
                       & " a name attribute when defined outside of <action>"),
                     Mode => Error);
               else
                  Filter := Parse_Filter_Node (Current_Node, Name);
                  Register_Filter (Kernel, Filter, Name);
               end if;
            end;

         elsif To_Lower (Current_Node.Tag.all) = "button" then
            Parse_Button_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "tool" then
            Parse_Tool_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "stock" then
            Parse_Stock_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "entry" then
            Parse_Entry_Node (Current_Node);

         end if;
      end Add_Child;

   begin
      Add_Child ("/", Node);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Customize;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate (Menu : access Gtk_Widget_Record'Class) is
      M : constant Subprogram_Type_Menu := Subprogram_Type_Menu (Menu);
   begin
      if M.On_Activate /= null then
         declare
            Inst : constant Class_Instance :=
              Get_Instance (Get_Script (M.On_Activate.all), M);
            C : Callback_Data'Class := Create
              (Get_Script (Inst), Arguments_Count => 1);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Trace (Me, "Callback for menu");
            Set_Nth_Arg (C, 1, Inst);
            Tmp := Execute (M.On_Activate, C);
            Free (C);
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Activate;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Action_Filter_Wrapper;
      Context : Selection_Context) return Boolean
   is
      C      : Callback_Data'Class :=
                 Create (Get_Script (Filter.Filter.all), 1);
      Result : Boolean;
   begin
      Set_Nth_Arg
        (C, 1, Create_Context (Get_Script (Filter.Filter.all), Context));
      Result := Execute (Filter.Filter, C);
      Free (C);
      return Result;
   end Filter_Matches_Primitive;

   ------------------
   -- Menu_Handler --
   ------------------

   procedure Menu_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Menu_Class : constant Class_Type := New_Class (Kernel, "Menu");
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, -("Cannot build instances of GPS.Menu directly."
                    & " Use GPS.Menu.get() or GPS.Menu.create() instead"));

      elsif Command = "get" then
         Name_Parameters (Data, Menu_Get_Params);
         declare
            Path : constant String := Nth_Arg (Data, 1);
            Menu : constant Gtk_Menu_Item := Find_Menu_Item (Kernel, Path);
            Inst : Class_Instance;
         begin
            if Menu = null then
               Set_Error_Msg (Data, -"No such menu: " & Path);
            else
               Inst := Get_Instance (Get_Script (Data), Widget => Menu);
               if Inst = No_Class_Instance then
                  Inst := New_Instance (Get_Script (Data), Menu_Class);
                  Set_Data (Inst, Widget => GObject (Menu));
               end if;

               Set_Return_Value (Data, Inst);
            end if;
         end;

      elsif Command = "create" then
         Name_Parameters (Data, Menu_Create_Params);
         declare
            Inst : Class_Instance;
            Path : constant String := Nth_Arg (Data, 1);
            Filter : constant Subprogram_Type := Nth_Arg (Data, 5, null);
            Filter_A : Action_Filter;

            Item : Gtk_Menu_Item;
            Menu : Subprogram_Type_Menu;
            Last : Integer := Path'First - 1;
         begin
            --  Take into account backslashes when extracting components of the
            --  menu path.
            for J in reverse Path'Range loop
               if Path (J) = '/'
                 and then (J = Path'First
                           or else Path (J - 1) /= '\')
               then
                  Last := J;
                  exit;
               end if;
            end loop;

            if Path'Length > 0 and then Path (Last + 1) = '-' then
               Gtk_New (Item);
            else
               Menu := new Subprogram_Type_Menu_Record;
               Gtk.Menu_Item.Initialize_With_Mnemonic
                 (Menu, Label => Unprotect (Path (Last + 1 .. Path'Last)));
               Menu.On_Activate := Nth_Arg (Data, 2, null);
               Widget_Callback.Connect
                 (Menu, Signal_Activate, On_Activate'Access);
               Set_Accel_Path
                 (Menu, "<gps>" & Path, Get_Default_Accelerators (Kernel));

               Item := Gtk_Menu_Item (Menu);
            end if;

            if Filter /= null then
               Filter_A := new Action_Filter_Wrapper'
                 (Action_Filter_Record with Filter);
            end if;

            Register_Menu
              (Kernel      => Kernel,
               Parent_Path => Path (Path'First .. Last - 1),
               Item        => Item,
               Ref_Item    => Nth_Arg (Data, 3, ""),
               Add_Before  => Nth_Arg (Data, 4, True),
               Filter      => Filter_A);

            Inst := New_Instance (Get_Script (Data), Menu_Class);
            Set_Data (Inst, Widget => GObject (Item));
            Set_Return_Value (Data, Inst);
         end;

      elsif Command = "rename" then
         Name_Parameters (Data, Menu_Rename_Params);
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Menu_Class);
            W    : constant Gtk_Widget     :=
                     Gtk_Widget (GObject'(Get_Data (Inst)));
            Menu : constant Gtk_Menu_Item  := Gtk_Menu_Item (W);
            Label : Gtk_Accel_Label;
         begin
            if W /= null then
               Gtk_New (Label, "");
               Set_Text_With_Mnemonic (Label, Nth_Arg (Data, 2));
               Set_Alignment (Label, 0.0, 0.5);
               Set_Accel_Widget (Label, Menu);

               Remove (Menu, Get_Child (Menu));
               Add (Menu, Label);
               Show_All (Label);
            end if;
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Menu_Handler;

   ------------------------
   -- Contextual_Handler --
   ------------------------

   procedure Contextual_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel           : constant Kernel_Handle := Get_Kernel (Data);
      Contextual_Class : constant Class_Type :=
                           New_Class (Kernel, "Contextual");
      Inst             : Class_Instance;
      Cmd              : Contextual_Shell_Command;
      Filter           : Contextual_Shell_Filter;
      Label            : Contextual_Shell_Label;
      Subp             : Subprogram_Type;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Contextual_Constructor_Params);
         Inst := Nth_Arg (Data, 1, Contextual_Class);
         Set_Data (Inst, Contextual_Class,
                   Value => String'(Nth_Arg (Data, 2)));

      elsif Command = "show" then
         Inst := Nth_Arg (Data, 1, Contextual_Class);
         Set_Contextual_Menu_Visible
           (Kernel, String'(Get_Data (Inst, Contextual_Class)), True);

      elsif Command = "hide" then
         Inst := Nth_Arg (Data, 1, Contextual_Class);
         Set_Contextual_Menu_Visible
           (Kernel, String'(Get_Data (Inst, Contextual_Class)), False);

      elsif Command = "set_sensitive" then
         Inst := Nth_Arg (Data, 1, Contextual_Class);
         Set_Contextual_Menu_Sensitivity
           (Kernel,
            String'(Get_Data (Inst, Contextual_Class)),
            Nth_Arg (Data, 2));

      elsif Command = "create" then
         Name_Parameters (Data, Contextual_Create_Params);
         Inst := Nth_Arg (Data, 1, Contextual_Class);

         declare
            Tmp : Subprogram_Type;
         begin
            Tmp := Nth_Arg (Data, 2); --  May raise No_Such_Parameter
            Cmd := new Contextual_Shell_Cmd;
            Cmd.Contextual := Inst;
            Cmd.On_Activate := Tmp;
         exception
            when No_Such_Parameter =>
               Cmd := null;
         end;

         Subp := Nth_Arg (Data, 4, null);
         if Subp /= null then
            Filter := new Contextual_Shell_Filters'
              (Action_Filter_Record with Contextual => Inst, Filter => Subp);
         end if;

         Subp := Nth_Arg (Data, 3, null);
         if Subp /= null then
            Label := new Contextual_Shell_Labels'
              (Contextual => Inst, Label => Subp);
            Register_Contextual_Menu
              (Kernel,
               Name              => Get_Data (Inst, Contextual_Class),
               Action            => Interactive_Command_Access (Cmd),
               Filter            => Action_Filter (Filter),
               Visibility_Filter => Nth_Arg (Data, 8, True),
               Label             => Label,
               Ref_Item          => Nth_Arg (Data, 5, ""),
               Add_Before        => Nth_Arg (Data, 6, True),
               Group             => Nth_Arg
                 (Data, 7, Default_Contextual_Group));
         else
            Register_Contextual_Menu
              (Kernel,
               Name              => Get_Data (Inst, Contextual_Class),
               Action            => Interactive_Command_Access (Cmd),
               Filter            => Action_Filter (Filter),
               Visibility_Filter => Nth_Arg (Data, 8, True),
               Ref_Item          => Nth_Arg (Data, 5, ""),
               Add_Before        => Nth_Arg (Data, 6, True),
               Group             => Nth_Arg
                 (Data, 7, Default_Contextual_Group));
         end if;

      elsif Command = "create_dynamic" then
         Name_Parameters (Data, Contextual_Create_Dynamic_Params);
         Inst := Nth_Arg (Data, 1, Contextual_Class);

         Subp := Nth_Arg (Data, 5, null);
         if Subp /= null then
            Filter  := new Contextual_Shell_Filters'
              (Action_Filter_Record with Contextual => Inst, Filter => Subp);
         end if;

         Register_Contextual_Submenu
           (Kernel,
            Name              => Get_Data (Inst, Contextual_Class),
            Filter            => Action_Filter (Filter),
            Visibility_Filter => Nth_Arg (Data, 9, True),
            Label             => Nth_Arg (Data, 4, ""),
            Submenu           => new Create_Dynamic_Contextual'
              (Contextual  => Inst,
               Factory     => Nth_Arg (Data, 2),
               On_Activate => Nth_Arg (Data, 3)),
            Ref_Item          => Nth_Arg (Data, 6, ""),
            Add_Before        => Nth_Arg (Data, 7, True),
            Group             => Nth_Arg (Data, 8, Default_Contextual_Group));

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         declare
            Menus : String_List_Access :=
              Get_Registered_Contextual_Menus (Kernel);
         begin
            if Menus /= null then
               for M in Menus'Range loop
                  Set_Return_Value (Data, Menus (M).all);
               end loop;
            end if;

            Free (Menus);
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Contextual_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Class : constant Class_Type := New_Class
        (Kernel, "Menu", Base => Get_GUI_Class (Kernel));

      Contextual_Class : constant Class_Type := New_Class
        (Kernel, "Contextual");
   begin
      Custom_Module_ID := new Custom_Module_ID_Record;
      Register_Module
        (Module      => Custom_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Custom",
         Priority    => Low_Priority);

      Expect_Interface.Register_Commands (Kernel);
      Custom_Combos.Register_Commands (Kernel);
      Custom_Timeout.Register_Commands (Kernel);
      XML_Viewer.Register_Commands (Kernel);

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Menu_Class,
         Handler      => Menu_Handler'Access);
      Register_Command
        (Kernel, "get",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Menu_Class,
         Static_Method => True,
         Handler       => Menu_Handler'Access);
      Register_Command
        (Kernel, "create",
         Minimum_Args  => 1,
         Maximum_Args  => 5,
         Static_Method => True,
         Class         => Menu_Class,
         Handler       => Menu_Handler'Access);
      Register_Command
        (Kernel, "rename",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Menu_Class,
         Handler       => Menu_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Class   => Contextual_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler => Contextual_Handler'Access);
      Register_Command
        (Kernel, "show",
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Register_Command
        (Kernel, "hide",
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Register_Command
        (Kernel, "set_sensitive",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Register_Command
        (Kernel, "create",
         Minimum_Args => 1,
         Maximum_Args => 7,
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Register_Command
        (Kernel, "create_dynamic",
         Minimum_Args => 2,
         Maximum_Args => 8,
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Register_Command
        (Kernel, "list",
         Class         => Contextual_Class,
         Static_Method => True,
         Handler       => Contextual_Handler'Access);
   end Register_Module;

end Custom_Module;
