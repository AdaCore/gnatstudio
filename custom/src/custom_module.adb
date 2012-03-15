------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts.Gtkada;   use GNATCOLL.Scripts, GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Glib.Object;               use Glib.Object;

with Gtk.Accel_Label;           use Gtk.Accel_Label;
with Gtk.Check_Menu_Item;
with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Icon_Factory;          use Gtk.Icon_Factory;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item;       use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;

with Commands.Custom;           use Commands.Custom;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with Custom_Combos;             use Custom_Combos;
with Custom_Timeout;            use Custom_Timeout;
with Expect_Interface;          use Expect_Interface;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GUI_Utils;                 use GUI_Utils;
with Language.Custom;           use Language.Custom;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with String_Utils;              use String_Utils;
with Switches_Chooser;          use Switches_Chooser;
with Traces;                    use Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with XML_Viewer;

with Switches_Parser;           use Switches_Parser;
with XML_Utils;                 use XML_Utils;
with XML_Utils.GtkAda;          use XML_Utils.GtkAda;

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
   Description_Cst   : aliased constant String := "description";
   Category_Cst      : aliased constant String := "category";
   Key_Cst           : aliased constant String := "key";
   Action_Cst        : aliased constant String := "action";
   Is_Active_Cst     : aliased constant String := "is_active";

   Menu_Get_Params : constant Cst_Argument_List :=
     (1 => Path_Cst'Access);
   Menu_Rename_Params : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Menu_Set_Active_Params : constant Cst_Argument_List :=
     (1 => Is_Active_Cst'Access);
   Menu_Create_Params : constant Cst_Argument_List :=
     (1 => Path_Cst'Access,
      2 => On_Activate_Cst'Access,
      3 => Ref_Cst'Access,
      4 => Add_Before_Cst'Access,
      5 => Filter_Cst'Access,
      6 => Group_Cst'Access);
   Contextual_Constructor_Params : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Contextual_Create_Params : constant Cst_Argument_List :=
     (1 => On_Activate_Cst'Access,
      2 => Label_Cst'Access,
      3 => Filter_Cst'Access,
      4 => Ref_Cst'Access,
      5 => Add_Before_Cst'Access,
      6 => Group_Cst'Access,
      7 => Visibility_Filter_Cst'Access,
      8 => Action_Cst'Access);
   Contextual_Create_Dynamic_Params : constant Cst_Argument_List :=
     (1 => Factory_Cst'Access,
      2 => On_Activate_Cst'Access,
      3 => Label_Cst'Access,
      4 => Filter_Cst'Access,
      5 => Ref_Cst'Access,
      6 => Add_Before_Cst'Access,
      7 => Group_Cst'Access,
      8 => Visibility_Filter_Cst'Access);

   package Radio_Group_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Widget_SList.GSlist,
      "="          => Widget_SList."=");

   Radio_Groups : Radio_Group_Maps.Map;

   package Subprogram_Callback is new
     Gtk.Handlers.User_Callback (Gtk_Menu_Item_Record, Subprogram_Type);

   type Action_Filter_Wrapper is new Action_Filter_Record with record
      Filter : Subprogram_Type;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Action_Filter_Wrapper;
      Context : Selection_Context) return Boolean;
   --  A filter that executes a shell subprogram

   procedure On_Activate
     (Menu        : access Gtk_Menu_Item_Record'Class;
      On_Activate : Subprogram_Type);
   --  Called when a Subprogram_Type_Menu is activated

   procedure On_Destroy_Subprogram_Menu
     (Menu        : access Gtk_Menu_Item_Record'Class;
      On_Activate : Subprogram_Type);
   --  Called when a subprogram_type_menu is destroyed

   overriding procedure Customize
     (Module : access Custom_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   procedure Menu_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands for GPS.Menu

   procedure Contextual_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands for GPS.Contextual

   procedure Action_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands for GPS.Action

   type Subprogram_Command_Record is new Interactive_Command with record
      Pass_Context : Boolean := True;
      --  Whether the context should be passed as Arg to On_Activate

      On_Activate  : Subprogram_Type;
   end record;
   type Subprogram_Command is access all Subprogram_Command_Record'Class;
   overriding procedure Free (Cmd : in out Subprogram_Command_Record);
   overriding function Execute
     (Command : access Subprogram_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Type used to define contextual menus from a scripting language

   type Subprogram_Filter_Record is new Action_Filter_Record with record
      Filter     : Subprogram_Type;
   end record;
   type Subprogram_Filter is access all Subprogram_Filter_Record'Class;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Filter_Record;
      Context : Selection_Context) return Boolean;
   overriding procedure Free (Filter : in out Subprogram_Filter_Record);
   --  Type used to define contextual menus from a scripting language

   type Subprogram_Label_Record is new Contextual_Menu_Label_Creator_Record
   with record
      Label      : Subprogram_Type;
   end record;
   type Subprogram_Label is access all Subprogram_Label_Record'Class;
   overriding function Get_Label
     (Creator : access Subprogram_Label_Record;
      Context : Selection_Context) return String;
   --  Type used to define contextual menus from a scripting language

   type Create_Dynamic_Contextual is new Submenu_Factory_Record with record
      On_Activate : Subprogram_Type;
      Factory     : Subprogram_Type;
   end record;
   type Create_Dynamic_Contextual_Access is access all
     Create_Dynamic_Contextual'Class;
   overriding procedure Append_To_Menu
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

   function Filter_From_Argument
     (Data   : Callback_Data'Class;
      Nth    : Integer) return Action_Filter;
   --  Convert one of the arguments of Data into a filter. This argument can be
   --  specified either as a string referencing a predefined filter, or as a
   --  subprogram callback

   ----------
   -- Free --
   ----------

   overriding procedure Free (Filter : in out Subprogram_Filter_Record) is
   begin
      Free (Filter.Filter);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Cmd : in out Subprogram_Command_Record) is
   begin
      Free (Cmd.On_Activate);
   end Free;

   -------------------------
   -- Parse_Switches_Node --
   -------------------------

   procedure Parse_Switches_Node
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : GNATCOLL.VFS.Virtual_File;
      Current_Tool : in out Tool_Properties_Record;
      Node         : Node_Ptr)
   is
      M : Unbounded_String;

      function Finder (Name : String) return Switches_Editor_Config;
      --  Looks up registered tool corresponding to Name.

      function Finder (Name : String) return Switches_Editor_Config is
         Tool : Tool_Properties_Record;
      begin
         Tool := Get_Tool_Properties (Kernel, Name);
         if Tool /= No_Tool then
            return Tool.Config;
         else
            return null;
         end if;
      end Finder;
   begin
      Switches_Parser.Parse_Switches_Node
        (Current_Tool_Name   => Current_Tool.Tool_Name.all,
         Current_Tool_Config => Current_Tool.Config,
         Error_Message       => M,
         Finder              => Finder'Unrestricted_Access,
         Node                => Node);

      if M /= Null_Unbounded_String then
         Insert
           (Kernel,
            (-("Error when parsing file "))
            & Display_Full_Name (File) & ":" & ASCII.LF
            & To_String (M),
            Mode => Error);
      end if;
   end Parse_Switches_Node;

   ------------------------------
   -- On_Dynamic_Menu_Activate --
   ------------------------------

   procedure On_Dynamic_Menu_Activate
     (Item    : access Gtk_Menu_Item_Record'Class;
      Factory : Dynamic_Context)
   is
      Script : constant Scripting_Language :=
        Get_Script (Factory.Contextual.On_Activate.all);
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

   overriding procedure Append_To_Menu
     (Factory : access Create_Dynamic_Contextual;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Script : constant Scripting_Language :=
        Get_Script (Factory.On_Activate.all);
      C : Callback_Data'Class := Create (Script, Arguments_Count => 1);
      Item : Python_Menu_Item;
   begin
      Set_Nth_Arg (C, 1, Create_Context (Script, Context));

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

   overriding function Execute
     (Command : access Subprogram_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Count : Natural := 0;
   begin
      if Command.Pass_Context then
         Count := Count + 1;
      end if;

      declare
         C : Callback_Data'Class := Create
           (Get_Script (Command.On_Activate.all), Arguments_Count => Count);
         Tmp : Boolean;
         pragma Unreferenced (Tmp);
      begin
         if Command.Pass_Context then
            Set_Nth_Arg
              (C, 1, Create_Context
                 (Get_Script (Command.On_Activate.all), Context.Context));
         end if;

         Tmp := Execute (Command.On_Activate, C);
         Free (C);
      end;

      return Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Filter_Record;
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

   overriding function Get_Label
     (Creator : access Subprogram_Label_Record;
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

   overriding procedure Customize
     (Module : access Custom_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);

      Kernel  : constant Kernel_Handle := Get_Kernel (Module.all);
      Handler : constant Language_Handler := Get_Language_Handler (Kernel);

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
               pragma Assert (False);
               return;
            end if;

            Child := Child.Next;
         end loop;

         if Action /= "" then
            Command := Lookup_Action (Kernel, Action);
            if Command = null or else Command.Command = null then
               Insert (Kernel,
                       -"Command not found when creating contextual menu: "
                       & Action,
                       Mode => Error);
               Free (Title);
               pragma Assert (False);
               return;
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
                       Get_Attribute (Node, "package", Ide_Package);
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
            pragma Assert (False);
            return;
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
                       (Kernel, -"Unknown action filter " & Id, Mode => Error);
                     pragma Assert (False);
                     return Filter;
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
                  Filter :=
                    Create
                      (Language   => Lang,
                       Shell      => Shell,
                       Shell_Lang => Shell_Lang,
                       Module     => Module);
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
                     Filter := Filter and Filter_Tmp;
                  elsif Node.Tag.all = "filter_or" then
                     Filter := Filter or Filter_Tmp;
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
            pragma Assert (False);
            return;
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
                  Filter_A := Filter_A or Parse_Filter_Node (Child, Name);
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
               pragma Assert (False);
               return;
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
                  Filter_A := Filter_A and Implicit_Filter;
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
         use type Glib.Gint;

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
               pragma Assert (False);
               return;
            end if;

            Child := Child.Next;
         end loop;

         if Title.all /= "" then
            if Action = "" then
               Insert (Kernel, -"<button> nodes must have an action attribute",
                       Mode => Error);
               pragma Assert (False);
               return;
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
         CL    : Arg_List;
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

         CL := Create ("Toolbar");
         Execute_GPS_Shell_Command (Kernel, CL);

         CL := Parse_String ("Toolbar.append %1 """
              & Id & """ """ & Label & """ """
              & Get_Attribute (Node, "on-changed") & """", Separate_Args);
         Execute_GPS_Shell_Command (Kernel, CL);

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
                     CL := Create ("Toolbar");
                     Execute_GPS_Shell_Command (Kernel, CL);
                     CL := Parse_String
                       ("Toolbar.entry %1 """ & Id
                        & """; ToolbarEntry.add %1 """ & Child.Value.all
                        & """ """ & On_Selected & """", Separate_Args);
                     Execute_GPS_Shell_Command (Kernel, CL);
                  else
                     CL := Create ("Toolbar");
                     Execute_GPS_Shell_Command (Kernel, CL);
                     CL := Parse_String
                       ("Toolbar.entry %1 """ & Id
                        & """; ToolbarEntry.add %1 """ & Child.Value.all
                        & """", Separate_Args);
                     Execute_GPS_Shell_Command (Kernel, CL);
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
                  pragma Assert (False);
                  return;
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
               Parent_Path => Format (Parent_Path) & Title.all,
               Item        => null,
               Ref_Item    => Before,
               Add_Before  => True);
         elsif After /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path => Format (Parent_Path) & Title.all,
               Item        => null,
               Ref_Item    => After,
               Add_Before  => False);
         end if;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               null; --  Already handled
            elsif To_Lower (Child.Tag.all) = "submenu" then
               Parse_Submenu_Node
                 (Child, Format (Parent_Path) & Title.all);
            elsif To_Lower (Child.Tag.all) = "menu" then
               Parse_Menu_Node
                 (Child, Format (Parent_Path) & Title.all);
            elsif To_Lower (Child.Tag.all) = "menu_item"
              or else To_Lower (Child.Tag.all) = "toolbar_item"
            then
               Insert
                 (Kernel,
                  -("<menu_item> and <toolbar_item> are no longer"
                    & " supported. Please use the program"
                    & " gps2custom-1.3 to convert to the new format."),
                  Mode => Error);
               pragma Assert (False);
               return;
            else
               Insert (Kernel,
                       -"Invalid child node for <submenu>: "
                       & Child.Tag.all,
                       Mode => Error);
               pragma Assert (False);
               return;
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
         Sep     : Gtk_Separator_Menu_Item;
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
               pragma Assert (False);
               return;
            end if;

            Child := Child.Next;
         end loop;

         --  Special case to allow separators
         if Action = ""
           and then Title.all /= ""
         then
            Insert (Kernel, -"<menu> nodes must have an action attribute",
                    Mode => Error);
            pragma Assert (False);
            return;
         end if;

         if Title.all = "" then
            Gtk_New (Sep);
            Register_Menu (Kernel, Parent_Path, Sep);
         else
            Command := Lookup_Action (Kernel, Action);
            if Command /= null and then Command.Command /= null then

               if Before /= "" then
                  Register_Menu
                    (Kernel,
                     Dir_Name (Format (Parent_Path) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command,
                     Ref_Item    => Before);

               elsif After /= "" then
                  Register_Menu
                    (Kernel,
                     Dir_Name (Format (Parent_Path) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command,
                     Ref_Item    => After,
                     Add_Before  => False);

               else
                  Register_Menu
                    (Kernel,
                     Dir_Name (Format (Parent_Path) & Title.all),
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
                     Filename : constant Virtual_File :=
                                  Get_File_Child (Files, "file");
                  begin
                     if Filename = No_File then
                        Insert
                          (Kernel,
                           -"No alternate file specified for icon "
                           & Get_Attribute (Child, "id"), Mode => Error);
                     else
                        if Is_Absolute_Path (Filename) then
                           Pic_File := Filename;
                        else
                           if File = GNATCOLL.VFS.No_File then
                              Pic_File := Create_From_Dir
                                (Get_Current_Dir, Filename.Full_Name);
                           else
                              Pic_File := Create_From_Dir
                                (Get_Parent (File), Filename.Full_Name);
                           end if;
                        end if;

                        if Is_Regular_File (Pic_File) then
                           Source := Gtk_New;
                           Set_Filename (Source, +Full_Name (Pic_File, True));

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
                              & Display_Full_Name (Pic_File));
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
                  Filename : constant Virtual_File :=
                               Get_File_Child (Child, "file");

                  Pic_File : GNATCOLL.VFS.Virtual_File;
               begin
                  if Id = "" then
                     Insert
                       (Kernel,
                        -"No id specified for stock icon.",
                        Mode => Error);

                  elsif Filename = No_File then
                     Insert
                       (Kernel,
                        -"No file specified for stock icon " & Id,
                        Mode => Error);

                  else
                     if Is_Absolute_Path (Filename) then
                        Pic_File := Filename;
                     else
                        if File = GNATCOLL.VFS.No_File then
                           Pic_File := Create_From_Dir
                             (Get_Current_Dir, Filename.Full_Name);
                        else
                           Pic_File := Create_From_Dir
                             (Get_Parent (File), Filename.Full_Name);
                        end if;
                     end if;

                     if Is_Regular_File (Pic_File) then
                        Set    := Gtk_New;

                        Source := Gtk_New;
                        Set_Filename (Source, +Full_Name (Pic_File, True));
                        Add_Source (Set, Source);
                        Free (Source);

                        Add_Alternate_Sources;

                        Add (Factory, Id, Set);
                        Unref (Set);

                        declare
                           Stock : Gtk_Stock_Item;
                        begin
                           Gtk_New
                             (Stock, Id,
                              -Get_Attribute (Child, "label"),
                             0, 0, "");
                           Add (Stock);
                           Free (Stock);  --  gtk+ took its own copy
                        end;

                     else
                        Insert
                          (Kernel,
                           -"Error when creating stock icon " & Id
                           & (-". File not found: ")
                           & Display_Full_Name (Pic_File));
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
      begin
         if Current_Node = null
           or else Current_Node.Tag = null
         then
            return;
         end if;

         if To_Lower (Current_Node.Tag.all) = "language" then
            Initialize (Handler, Kernel, Current_Node);

         elsif Current_Node.Tag.all = "menu" then
            Parse_Menu_Node (Current_Node, Parent_Path);

         elsif To_Lower (Current_Node.Tag.all) = "submenu" then
            Parse_Submenu_Node (Current_Node, Parent_Path);

         elsif To_Lower (Current_Node.Tag.all) = "action" then
            Parse_Action_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "contextual" then
            Parse_Contextual_Node (Current_Node);

         --  XML is case-sensitive, and we do not need to be backward
         --  compatible for this node, so expect lower-case only
         elsif Current_Node.Tag.all = "perspective" then
            Kernel_Desktop.Define_Perspective
              (Get_MDI (Kernel), Convert (Current_Node), Kernel);

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

   procedure On_Activate
     (Menu        : access Gtk_Menu_Item_Record'Class;
      On_Activate : Subprogram_Type) is
   begin
      if On_Activate /= null then
         declare
            Inst : constant Class_Instance :=
              Get_Instance (Get_Script (On_Activate.all), Menu);
            C : Callback_Data'Class := Create
              (Get_Script (Inst), Arguments_Count => 1);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Trace (Me, "Callback for menu");
            Set_Nth_Arg (C, 1, Inst);
            Tmp := Execute (On_Activate, C);
            Free (C);
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Activate;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
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
            Group : constant String := Nth_Arg (Data, 6, "");
            Filter : constant Subprogram_Type := Nth_Arg (Data, 5, null);
            Filter_A : Action_Filter;
            Subprogram : constant Subprogram_Type := Nth_Arg (Data, 2, null);

            Item : Gtk_Menu_Item;
            Sep  : Gtk_Separator_Menu_Item;
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
               Gtk_New (Sep);
               Item := Gtk_Menu_Item (Sep);
            else
               if Group = "" then
                  Gtk.Menu_Item.Gtk_New_With_Mnemonic
                    (Item,
                     Label => Unprotect (Path (Last + 1 .. Path'Last)));
               else
                  declare
                     Inserted : Boolean;
                     Cursor   : Radio_Group_Maps.Cursor;
                     List     : Widget_SList.GSlist;
                     Menu     : Gtk_Radio_Menu_Item;
                  begin
                     Radio_Groups.Insert (Group, List, Cursor, Inserted);

                     if not Inserted then
                        List := Radio_Group_Maps.Element (Cursor);
                     end if;

                     Gtk.Radio_Menu_Item.Gtk_New_With_Mnemonic
                       (Menu,
                        Group => List,
                        Label => Unprotect (Path (Last + 1 .. Path'Last)));

                     List := Get_Group (Menu);

                     Radio_Groups.Replace_Element (Cursor, List);

                     Item := Gtk_Menu_Item (Menu);
                  end;
               end if;

               Subprogram_Callback.Connect
                 (Item,
                  Signal_Activate,
                  On_Activate'Access,
                  Subprogram);

               Subprogram_Callback.Connect
                 (Item,
                  "destroy",
                  On_Destroy_Subprogram_Menu'Access,
                  Subprogram);

               Set_Accel_Path
                 (Item, "<gps>" & Path, Get_Default_Accelerators (Kernel));
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
      elsif Command = "get_active" then
         declare
            use Gtk.Check_Menu_Item;
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Menu_Class);
            W    : constant GObject := Get_Data (Inst);
         begin
            if W /= null and then
              W.all in Gtk_Check_Menu_Item_Record'Class
            then
               Set_Return_Value (Data, Get_Active (Gtk_Check_Menu_Item (W)));
            else
               Set_Return_Value (Data, False);
            end if;
         end;
      elsif Command = "set_active" then
         Name_Parameters (Data, Menu_Set_Active_Params);
         declare
            use Gtk.Check_Menu_Item;
            Inst  : constant Class_Instance := Nth_Arg (Data, 1, Menu_Class);
            Value : constant Boolean := Nth_Arg (Data, 2, True);
            W     : constant GObject := Get_Data (Inst);
         begin
            if W /= null and then
              W.all in Gtk_Check_Menu_Item_Record'Class
            then
               Set_Active (Gtk_Check_Menu_Item (W), Value);
            end if;
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

   --------------------------------
   -- On_Destroy_Subprogram_Menu --
   --------------------------------

   procedure On_Destroy_Subprogram_Menu
     (Menu        : access Gtk_Menu_Item_Record'Class;
      On_Activate : Subprogram_Type)
   is
      pragma Unreferenced (Menu);
      Object : Subprogram_Type := On_Activate;
   begin
      Free (Object);
   end On_Destroy_Subprogram_Menu;

   ------------------------
   -- Contextual_Handler --
   ------------------------

   procedure Contextual_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel           : constant Kernel_Handle := Get_Kernel (Data);
      Contextual_Class : constant Class_Type :=
                           New_Class (Kernel, "Contextual");
      Action_Class : constant Class_Type := New_Class
        (Kernel, "Action", Base => Get_GUI_Class (Kernel));

      Action           : Action_Record_Access;
      Action_Inst      : Class_Instance;
      Inst             : Class_Instance;
      Cmd              : Subprogram_Command;
      Filter           : Subprogram_Filter;
      The_Filter       : Action_Filter;
      Label            : Subprogram_Label;
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

         Action_Inst := Nth_Arg (Data, 9, Action_Class, True);
         if Action_Inst /= No_Class_Instance then
            Action  := Lookup_Action
              (Kernel, String'(Get_Data (Action_Inst, Action_Class)));

            if Action /= null then
               The_Filter := Action.Filter;
            end if;
         end if;

         declare
            Tmp : Subprogram_Type;
         begin
            Tmp := Nth_Arg (Data, 2); --  May raise No_Such_Parameter
            Cmd := new Subprogram_Command_Record;
            Cmd.On_Activate := Tmp;
         exception
            when No_Such_Parameter =>
               Cmd := null;
         end;

         Subp := Nth_Arg (Data, 4, null);
         if Subp /= null then
            Filter := new Subprogram_Filter_Record'
              (Action_Filter_Record with Filter => Subp);

            if The_Filter = null then
               The_Filter := Action_Filter (Filter);
            else
               The_Filter := The_Filter and Action_Filter (Filter);
            end if;
         end if;

         Subp := Nth_Arg (Data, 3, null);
         if Subp /= null then
            Label := new Subprogram_Label_Record'(Label => Subp);
         end if;

         if Label /= null then
            Register_Contextual_Menu
              (Kernel,
               Name              => Get_Data (Inst, Contextual_Class),
               Action            => Interactive_Command_Access (Cmd),
               Filter            => The_Filter,
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
               Filter            => The_Filter,
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
            Filter  := new Subprogram_Filter_Record'
              (Action_Filter_Record with Filter => Subp);
         end if;

         Register_Contextual_Submenu
           (Kernel,
            Name              => Get_Data (Inst, Contextual_Class),
            Filter            => Action_Filter (Filter),
            Visibility_Filter => Nth_Arg (Data, 9, True),
            Label             => Nth_Arg (Data, 4, ""),
            Submenu           => new Create_Dynamic_Contextual'
              (Factory     => Nth_Arg (Data, 2),
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

   --------------------------
   -- Filter_From_Argument --
   --------------------------

   function Filter_From_Argument
     (Data   : Callback_Data'Class;
      Nth    : Integer) return Action_Filter
   is
      Filter    : Action_Filter;
      Filter_Cb : Subprogram_Type;
   begin
      --  First case: the filter is a string referencing a predefined
      --  filter (it could be a string naming a subprogram in some of the
      --  scripts, though, so we need to test whether such a predefined
      --  filter exists)

      begin
         declare
            Name : constant String := Nth_Arg (Data, Nth, "");
         begin
            if Name = "" then
               --  Parameter was not specified, no filter to use
               return null;
            end if;

            Filter := Lookup_Filter (Get_Kernel (Data), Name);
            if Filter /= null then
               return Filter;
            end if;
         end;

      exception
         when Invalid_Parameter =>
            --  The parameter was not a string
            null;
      end;

      --  Either the parameter was not a string, or did not match an existing
      --  filter. It could be a subprogram_type in some scripting languages so
      --  we try that as well. We let the Invalid_Parameter exception through
      --  in case some other type of argument was specified.

      Filter_Cb := Nth_Arg (Data, Nth);
      return new Subprogram_Filter_Record'
        (Action_Filter_Record with
         Filter     => Filter_Cb);
   end Filter_From_Argument;

   --------------------
   -- Action_Handler --
   --------------------

   procedure Action_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Action_Class : constant Class_Type := New_Class
        (Kernel, "Action", Base => Get_GUI_Class (Kernel));
      Menu_Class : constant Class_Type := New_Class
        (Kernel, "Menu", Base => Get_GUI_Class (Kernel));

      Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);
         Set_Data (Inst, Action_Class, Value => String'(Nth_Arg (Data, 2)));

      elsif Command = "create" then
         Name_Parameters (Data, (1 => On_Activate_Cst'Access,
                                 2 => Filter_Cst'Access,
                                 3 => Category_Cst'Access,
                                 4 => Description_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);

         declare
            Name : constant String := String'(Get_Data (Inst, Action_Class));
            Filter  : constant Action_Filter := Filter_From_Argument (Data, 3);
            Category    : constant String := Nth_Arg (Data, 4, "General");
            Descr       : constant String := Nth_Arg (Data, 5, "");
            Cmd         : Subprogram_Command;
         begin
            Cmd := new Subprogram_Command_Record;
            Cmd.Pass_Context := False;
            Cmd.On_Activate  := Nth_Arg (Data, 2);

            Register_Action
              (Kernel,
               Name        => Name,
               Command     => Cmd,
               Description => Descr,
               Category    => Category,
               Filter      => Filter);
         end;

      elsif Command = "key" then
         Name_Parameters (Data, (1 => Key_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);
         Bind_Default_Key
           (Kernel      => Kernel,
            Action      => String'(Get_Data (Inst, Action_Class)),
            Default_Key => Nth_Arg (Data, 2));

      elsif Command = "menu" then
         Name_Parameters (Data, (1 => Path_Cst'Access,
                                 2 => Ref_Cst'Access,
                                 3 => Add_Before_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);

         declare
            Item   : Gtk_Menu_Item;
            Path   : constant String  := Nth_Arg (Data, 2);
            Ref    : constant String  := Nth_Arg (Data, 3, "");
            Before : constant Boolean := Nth_Arg (Data, 4, True);
            Action : constant Action_Record_Access :=
              Lookup_Action (Kernel, String'(Get_Data (Inst, Action_Class)));

         begin
            if Action /= null then
               Item := Register_Menu
                 (Kernel,
                  Parent_Path => Dir_Name (Path),
                  Text        => Base_Name (Path),
                  Ref_Item    => Ref,
                  Add_Before  => Before,
                  Callback    => null,
                  Action      => Action);

               Inst := New_Instance (Get_Script (Data), Menu_Class);
               Set_Data (Inst, Widget => GObject (Item));
               Set_Return_Value (Data, Inst);
            end if;
         end;

      elsif Command = "contextual" then
         Name_Parameters (Data, (1 => Path_Cst'Access,
                                 2 => Ref_Cst'Access,
                                 3 => Add_Before_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);

         declare
            Path   : constant String  := Nth_Arg (Data, 2);
            Ref    : constant String  := Nth_Arg (Data, 3, "");
            Before : constant Boolean := Nth_Arg (Data, 4, True);
            Action : constant Action_Record_Access :=
              Lookup_Action (Kernel, String'(Get_Data (Inst, Action_Class)));
         begin
            if Action /= null then
               Register_Contextual_Menu
                 (Kernel,
                  Name        => Path,
                  Ref_Item    => Ref,
                  Add_Before  => Before,
                  Action      => Action);
            end if;
         end;
      end if;
   end Action_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Class : constant Class_Type := New_Class
        (Kernel, "Menu", Base => Get_GUI_Class (Kernel));
      Action_Class : constant Class_Type := New_Class
        (Kernel, "Action", Base => Get_GUI_Class (Kernel));
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
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Action_Handler'Access);
      Register_Command
        (Kernel, "create",
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 4,
         Handler       => Action_Handler'Access);
      Register_Command
        (Kernel, "key",
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Action_Handler'Access);
      Register_Command
        (Kernel, "menu",
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 3,
         Handler       => Action_Handler'Access);
      Register_Command
        (Kernel, "contextual",
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 3,
         Handler       => Action_Handler'Access);

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
         Maximum_Args  => 6,
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
        (Kernel, "get_active",
         Maximum_Args  => 0,
         Class         => Menu_Class,
         Handler       => Menu_Handler'Access);
      Register_Command
        (Kernel, "set_active",
         Minimum_Args  => 0,
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
         Maximum_Args => 8,
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
