------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib.Object;

with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

with Commands.Custom;           use Commands.Custom;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with Custom_Timeout;
with Expect_Interface;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
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
with XML_Viewer;

with Switches_Parser;
with XML_Utils;                 use XML_Utils;
with XML_Utils.GtkAda;          use XML_Utils.GtkAda;
with Language.Shell;

package body Custom_Module is

   Me : constant Trace_Handle := Create ("GPS.CUSTOM.MODULE");

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
   Key_Cst           : aliased constant String := "key";

   Menu_Get_Params : constant Cst_Argument_List :=
     (1 => Path_Cst'Access);
   Contextual_Constructor_Params : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
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

   type Action_Filter_Wrapper is new Action_Filter_Record with record
      Filter : Subprogram_Type;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Action_Filter_Wrapper;
      Context : Selection_Context) return Boolean;
   overriding procedure Customize
     (Module : access Custom_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation
   overriding procedure Free (Filter : in out Action_Filter_Wrapper);
   overriding function Get_Debug_Name
     (Self : access Action_Filter_Wrapper) return String
     is ("python filter: " & Self.Filter.Get_Name);
   --  A filter that executes a shell subprogram

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
   overriding procedure Primitive_Free
     (Cmd : in out Subprogram_Command_Record);
   overriding function Execute
     (Command : access Subprogram_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Type used to define contextual menus from a scripting language

   type Subprogram_Filter_Record is new Action_Filter_Record with record
      Filter     : Subprogram_Type;
   end record;
   type Subprogram_Filter is access all Subprogram_Filter_Record'Class;
   overriding function Get_Debug_Name
     (Self : access Subprogram_Filter_Record) return String
     is ("python subprogram: " & Self.Filter.Get_Name);
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Filter_Record;
      Context : Selection_Context) return Boolean;
   overriding procedure Free (Filter : in out Subprogram_Filter_Record);
   --  Type used to define contextual menus from a scripting language

   type Subprogram_Label_Record is new Contextual_Menu_Label_Creator_Record
   with record
      Label : Subprogram_Type;
      Path  : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Subprogram_Label is access all Subprogram_Label_Record'Class;
   overriding function Get_Label
     (Creator : access Subprogram_Label_Record;
      Context : Selection_Context) return String;
   --  Type used to define contextual menus from a scripting language
   overriding function Get_Path
     (Creator : access Subprogram_Label_Record)
      return String;

   type Create_Dynamic_Contextual is new Submenu_Factory_Record with record
      On_Activate : Subprogram_Type;
      Factory     : Subprogram_Type;
   end record;
   type Create_Dynamic_Contextual_Access is access all
     Create_Dynamic_Contextual'Class;
   overriding procedure Append_To_Menu
     (Factory : access Create_Dynamic_Contextual;
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
      Current_Tool : Tool_Properties;
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

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
     (Cmd : in out Subprogram_Command_Record) is
   begin
      Free (Cmd.On_Activate);
   end Primitive_Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Filter : in out Action_Filter_Wrapper) is
   begin
      Free (Filter.Filter);
      Action_Filter_Record (Filter).Free;
   end Free;

   -------------------------
   -- Parse_Switches_Node --
   -------------------------

   procedure Parse_Switches_Node
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : GNATCOLL.VFS.Virtual_File;
      Current_Tool : Tool_Properties;
      Node         : Node_Ptr)
   is
      M : Unbounded_String;

      function Finder (Name : String) return Switches_Editor_Config;
      --  Looks up registered tool corresponding to Name.

      function Finder (Name : String) return Switches_Editor_Config is
         Tool : Tool_Properties;
      begin
         Tool := Get_Tool_Properties (Kernel, Name);
         if Tool /= null then
            return Tool.Config;
         else
            return null;
         end if;
      end Finder;
   begin
      Switches_Parser.Parse_Switches_Node
        (Current_Tool_Name   => Current_Tool.Tool_Name,
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
      when E : others => Trace (Me, E);
   end On_Dynamic_Menu_Activate;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Create_Dynamic_Contextual;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
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
                     Path          => Escape_Underscore (List (L).all),
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

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Creator : access Subprogram_Label_Record)
      return String is
   begin
      return Ada.Strings.Unbounded.To_String (Creator.Path);
   end Get_Path;

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

      Current_Menu_Section : Natural := 0;
      function Section_Name return String is
          (if Current_Menu_Section = 0
           then ""
           else "--section" & Current_Menu_Section'Img);
      --  Return the label for a separator menu item (which is translated to
      --  a section in the menu model)

      procedure Parse_Action_Node (Node : Node_Ptr);
      procedure Parse_Contextual_Node (Node : Node_Ptr);
      procedure Parse_Button_Node (Node : Node_Ptr);
      procedure Parse_Entry_Node (Node : Node_Ptr);
      procedure Parse_Tool_Node (Node : Node_Ptr);
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
         Group   : Integer := Default_Contextual_Group;
         Child   : Node_Ptr;
         Title   : GNAT.OS_Lib.String_Access;
         Filter  : Action_Filter := null;
      begin
         Title := new String'(Action);

         declare
            Group_Image : constant String := Get_Attribute (Node, "group", "");
         begin
            if Group_Image /= "" then
               Group := Integer'Value (Group_Image);
            end if;
         exception
            when Constraint_Error =>
               null;
         end;

         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);

            elsif To_Lower (Child.Tag.all) = "filter" then
               declare
                  Id : constant String := Get_Attribute (Child, "id", "");
               begin
                  if Id /= "" then
                     Filter := Lookup_Filter (Kernel, Id);
                     if Filter = null then
                        Insert (Kernel,
                                -"Invalid filter name: " & Id,
                                Mode => Error);
                     end if;
                  end if;
               end;

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

         if Before /= "" then
            Register_Contextual_Menu
              (Kernel,
               Name       => Title.all,
               Label      => Title.all,
               Action     => Action,
               Ref_Item   => Before,
               Add_Before => True,
               Group      => Group,
               Filter     => Filter);

         elsif After /= "" then
            Register_Contextual_Menu
              (Kernel,
               Name       => Title.all,
               Label      => Title.all,
               Action     => Action,
               Ref_Item   => After,
               Add_Before => False,
               Group      => Group,
               Filter     => Filter);

         else
            Register_Contextual_Menu
              (Kernel,
               Name    => Title.all,
               Label   => Title.all,
               Action  => Action,
               Group   => Group,
               Filter  => Filter);
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
         Tool      : Tool_Properties;

      begin
         if Name = "" then
            Insert (Kernel,
                    -"Invalid <tool> node, it must have a name attribute",
                    Mode => Error);
            pragma Assert (False);
            return;
         end if;

         Tool := new Tool_Properties_Record;
         Tool.Tool_Name         := To_Unbounded_String (Name);
         Tool.Project_Package   := To_Unbounded_String (Pack);
         Tool.Project_Attribute := To_Unbounded_String (Attribute);
         Tool.Project_Index     := To_Unbounded_String (Index);
         Tool.Override          := Override;

         while N /= null loop
            if N.Tag.all = "initial-cmd-line" then
               Tool.Initial_Cmd_Line := To_Unbounded_String (N.Value.all);

            elsif N.Tag.all = "language" then
               Append (Tool.Languages, To_Lower (N.Value.all));

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
                  Set_Error_Message
                    (Filter,
                     To_Unbounded_String (Get_Attribute (Node, "error")));
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

            Set_Error_Message
              (Filter, To_Unbounded_String (Get_Attribute (Node, "error")));
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

         Implicit_Filter := Create_Filter (Kernel, Node.Child);

         if Implicit_Filter /= null then
            if Filter_A /= null then
               declare
                  Error : constant Unbounded_String :=
                    Get_Error_Message (Filter_A);

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
            Category    => Category,
            Filter      => Filter_A);
         Free (Description);
      end Parse_Action_Node;

      -----------------------
      -- Parse_Button_Node --
      -----------------------

      procedure Parse_Button_Node (Node : Node_Ptr) is
         Action  : constant String := Get_Attribute (Node, "action");
         Icon    : constant String := Get_Attribute (Node, "iconname");
         --  For back compability support stock='icon' attribute
         Stock   : constant String := Get_Attribute (Node, "stock");
         Child   : Node_Ptr;

      begin
         Child := Node.Child;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Insert
                 (Kernel,
                  -("The <button> node now ignores its 'title' child (for"
                    & " action '") & Action & "'");
            elsif To_Lower (Child.Tag.all) = "pixmap" then
               Insert
                 (Kernel,
                  -("The <button> node now ignores its 'pixmap' child (for"
                    & " action '") & Action & "'");
            else
               Insert
                 (Kernel, -"Invalid child node for <button> tag",
                  Mode => Error);
               return;
            end if;

            Child := Child.Next;
         end loop;

         if Action = "" then
            Insert (Kernel, -"<button> nodes must have an action attribute",
                    Mode => Error);
            return;
         end if;

         if Icon = "" then
            --  Rollback to compability with 'stock' attribute
            Register_Button (Kernel, Action, Icon_Name => Stock);
         else
            Register_Button (Kernel, Action, Icon_Name => Icon);
         end if;
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

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               null; --  Already handled
            elsif To_Lower (Child.Tag.all) = "submenu" then
               Parse_Submenu_Node
                 (Child, Create_Menu_Path (Parent_Path, Title.all));
            elsif To_Lower (Child.Tag.all) = "menu" then
               Parse_Menu_Node
                 (Child, Create_Menu_Path (Parent_Path, Title.all));
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
         After_Attribute   : constant String := Get_Attribute (Node, "after");
         After   : constant String :=
            (if After_Attribute /= ""
             then After_Attribute
             else Section_Name);
         Child   : Node_Ptr;
         Title   : GNAT.OS_Lib.String_Access := new String'("");

      begin
         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);
            else
               Insert
                 (Kernel, -"Invalid child node for <menu> tag", Mode => Error);
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

         elsif Title.all = "" then
            Current_Menu_Section := Current_Menu_Section + 1;
            Register_Menu
               (Kernel,
                Create_Menu_Path (Parent_Path, Section_Name),
                Action => "");
         else
            if Before /= "" then
               Register_Menu
                 (Kernel,
                  Create_Menu_Path (Parent_Path, Title.all),
                  Action => Action,
                  Ref_Item    => Before);
            elsif After /= "" then
               Register_Menu
                 (Kernel,
                  Create_Menu_Path (Parent_Path, Title.all),
                  Action          => Action,
                  Ref_Item        => After,
                  Before_Ref_Item => False);
            else
               Register_Menu
                 (Kernel,
                  Create_Menu_Path (Parent_Path, Title.all),
                  Action => Action);
            end if;
         end if;
         Free (Title);
      end Parse_Menu_Node;

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
            Insert
              (Kernel,
               -"<stock> no longer supported in customization");

         elsif To_Lower (Current_Node.Tag.all) = "entry" then
            Parse_Entry_Node (Current_Node);

         end if;
      end Add_Child;

   begin
      Add_Child ("/", Node);

   exception
      when E : others => Trace (Me, E);
   end Customize;

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
            Inst : Class_Instance;
         begin
            Inst := New_Instance (Get_Script (Data), Menu_Class);
            Set_Data (Inst, Menu_Class, Path);
            Set_Return_Value (Data, Inst);
         end;

      elsif Command = "action" then
         declare
            Inst  : constant Class_Instance := Nth_Arg (Data, 1, Menu_Class);
            Path  : constant String := Get_Data (Inst, Menu_Class);
            Action : constant String := Action_From_Menu (Kernel, Path);
            Result : Class_Instance;
            Action_Class : constant Class_Type := New_Class (Kernel, "Action");
         begin
            Result := New_Instance (Data.Get_Script, Action_Class);
            Set_Data (Result, Action_Class, Value => Action);
            Set_Return_Value (Data, Result);
         end;
      end if;
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
      Filter           : Subprogram_Filter;
      The_Filter       : access Action_Filter_Record'Class;
      Enable_Filter    : Action_Filter;
      Label            : Subprogram_Label;
      pragma Unreferenced (Label);
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

      elsif Command = "name" then
         Inst := Nth_Arg (Data, 1, Contextual_Class);
         Set_Return_Value (Data, String'(Get_Data (Inst, Contextual_Class)));

      elsif Command = "set_sensitive" then
         Inst := Nth_Arg (Data, 1, Contextual_Class);
         Set_Contextual_Menu_Sensitivity
           (Kernel,
            String'(Get_Data (Inst, Contextual_Class)),
            Nth_Arg (Data, 2));

      elsif Command = "create_dynamic" then
         Name_Parameters (Data, Contextual_Create_Dynamic_Params);
         Inst := Nth_Arg (Data, 1, Contextual_Class);

         Subp := Nth_Arg (Data, 5, null);
         if Subp /= null then
            Filter  := new Subprogram_Filter_Record'
              (Action_Filter_Record with Filter => Subp);
            The_Filter := Action_Filter (Filter);
         end if;

         Subp := Nth_Arg (Data, 9, null);
         if Subp /= null then
            Filter  := new Subprogram_Filter_Record'
              (Action_Filter_Record with Filter => Subp);
            Enable_Filter := Action_Filter (Filter);
         end if;

         Register_Contextual_Submenu
           (Kernel,
            Name              => Get_Data (Inst, Contextual_Class),
            Filter            => The_Filter,
            Enable_Filter     => Enable_Filter,
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
      when E : others => Trace (Me, E);
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
            else
               Insert (Get_Kernel (Data),
                       -"Invalid filter name: " & Name,
                       Mode => Error);
               return null;
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
      --
      --  Since we want to reduce the number of filters that are run by GPS,
      --  we try to reuse existing filters when possible. This is done by
      --  registering the python filters as the name and address of the python
      --  subprogram to run.

      Filter_Cb := Nth_Arg (Data, Nth);

      declare
         N : constant String := "script: " & Filter_Cb.Get_Name;
      begin
         Filter := Get_Kernel (Data).Lookup_Filter (N);
         if Filter = null then
            Filter := new Subprogram_Filter_Record'
              (Action_Filter_Record with
               Filter     => Filter_Cb);
            Get_Kernel (Data).Register_Filter (Filter, N);
         end if;
         return Filter;
      end;
   end Filter_From_Argument;

   --------------------
   -- Action_Handler --
   --------------------

   procedure Action_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Action_Class : constant Class_Type := New_Class (Kernel, "Action");
      Menu_Class : constant Class_Type := New_Class
        (Kernel, "Menu", Base => Get_GUI_Class (Kernel));

      Inst : Class_Instance;
      Cmd  : Subprogram_Command;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);
         Set_Data (Inst, Action_Class, Value => String'(Nth_Arg (Data, 2)));

      elsif Command = "exists" then
         Inst := Nth_Arg (Data, 1, Action_Class);
         Data.Set_Return_Value
           (Lookup_Action
              (Kernel,
               Name => String'(Get_Data (Inst, Action_Class))) /= null);

      elsif Command = "create" then
         Inst := Nth_Arg (Data, 1, Action_Class);
         Cmd  := new Subprogram_Command_Record;
         Cmd.Pass_Context := False;
         Cmd.On_Activate  := Data.Nth_Arg (2);
         Register_Action
           (Kernel,
            Name         => String'(Get_Data (Inst, Action_Class)),
            Command      => Cmd,
            Filter       => Filter_From_Argument (Data, 3),
            Category     => Data.Nth_Arg (4, "General"),
            Description  => Data.Nth_Arg (5, ""),
            Icon_Name    => Data.Nth_Arg (6, ""),
            For_Learning => Data.Nth_Arg (7, False));

      elsif Command = "disable" then
         Inst := Nth_Arg (Data, 1, Action_Class);
         declare
            Disabled : constant Boolean := Nth_Arg (Data, 2, True);
            Action   : constant String := Get_Data (Inst, Action_Class);
         begin
            Set_Action_Disabled
              (Kernel,
               Name     => Action,
               Disabled => Disabled);
         end;

      elsif Command = "destroy_ui" then
         Inst := Nth_Arg (Data, 1, Action_Class);
         Remove_UI_For_Action (Kernel, Get_Data (Inst, Action_Class));

      elsif Command = "key" then
         Name_Parameters (Data, (1 => Key_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);
         Set_Default_Key
           (Kernel      => Kernel,
            Action      => String'(Get_Data (Inst, Action_Class)),
            Default_Key => Nth_Arg (Data, 2),
            Exclusive   => Data.Nth_Arg (3, True));

      elsif Command = "get_keys" then
         Inst := Nth_Arg (Data, 1, Action_Class);
         Data.Set_Return_Value
           (Kernel.Get_Shortcut
              (Action          => String'(Get_Data (Inst, Action_Class)),
               Use_Markup      => False,
               Return_Multiple => True));

      elsif Command = "can_execute" then
         Inst := Data.Nth_Arg (1, Action_Class);
         declare
            A : constant Action_Record_Access := Lookup_Action
              (Kernel, Get_Data (Inst, Action_Class));
         begin
            Data.Set_Return_Value
              (A /= null
               and then Filter_Matches (A, Get_Current_Context (Kernel)));
         end;

      elsif Command = "execute_if_possible" then
         Inst := Data.Nth_Arg (1, Action_Class);
         Data.Set_Return_Value
            (Execute_Action
               (Kernel,
                String'(Get_Data (Inst, Action_Class)),
                Error_Msg_In_Console => False,
                Synchronous          => True));

      elsif Command = "menu" then
         Name_Parameters (Data, (1 => Path_Cst'Access,
                                 2 => Ref_Cst'Access,
                                 3 => Add_Before_Cst'Access));
         Inst := Nth_Arg (Data, 1, Action_Class);

         declare
            Path   : constant String  := Nth_Arg (Data, 2);
            Ref    : constant String  := Nth_Arg (Data, 3, "");
            Before : constant Boolean := Nth_Arg (Data, 4, True);
            Action : constant String := Get_Data (Inst, Action_Class);
         begin
            Register_Menu
              (Kernel, Path, Action,
               Ref_Item        => Ref,
               Before_Ref_Item => Before);
            Inst := New_Instance (Get_Script (Data), Menu_Class);
            Set_Data (Inst, Menu_Class, Path);
            Set_Return_Value (Data, Inst);
         end;

      elsif Command = "button" then
         Inst := Data.Nth_Arg (1, Action_Class);
         declare
            Action_Name      : constant String :=
                                 Get_Data (Inst, Action_Class);
            Toolbar_Name     : constant String := Data.Nth_Arg (2, "main");
            Section_Name     : constant String := Data.Nth_Arg (3, "");
            Group_Name       : constant String := Data.Nth_Arg (4, "");
            Label_Name       : constant String := Data.Nth_Arg (5, "");
            Icon_Name        : constant String := Data.Nth_Arg (6, "");
            Hide             : constant Boolean := Data.Nth_Arg (7, False);
            Actual_Icon_Name : constant String :=
                                 (if Icon_Name /= "" then Icon_Name
                                  else
                                     Get_Icon_Name
                                    (Lookup_Action (Kernel, Action_Name)));
         begin
            Inst := Data.Nth_Arg (1);
            Register_Button
              (Kernel,
               Action    => Action_Name,
               Toolbar   => Toolbar_Name,
               Section   => Section_Name,
               Group     => Group_Name,
               Label     => Label_Name,
               Hide      => Hide,
               Icon_Name => Actual_Icon_Name);
         end;
      elsif Command = "contextual" then
         Inst := Nth_Arg (Data, 1, Action_Class);

         declare
            Ref    : constant String  := Nth_Arg (Data, 3, "");
            Before : constant Boolean := Nth_Arg (Data, 4, True);
            Action : constant String := Get_Data (Inst, Action_Class);
            Group  : constant Integer := Nth_Arg (Data, 5, 0);
            Label  : Subprogram_Label;
            Subp   : Subprogram_Type;
         begin
            declare
               Str : constant String := Nth_Arg (Data, 2);
            begin
               if Str /= "" and then Str (Str'Last) = '-' then
                  Register_Contextual_Separator
                    (Kernel,
                     In_Submenu  => Str (Str'First .. Str'Last - 2),
                     Ref_Item    => Ref,
                     Add_Before  => Before,
                     Group       => Group);
               else
                  Register_Contextual_Menu
                    (Kernel,
                     Label       => Str,
                     Ref_Item    => Ref,
                     Add_Before  => Before,
                     Group       => Group,
                     Action      => Action);
               end if;
            end;
         exception
            when others =>
               --  Assume path is a function
               Subp := Nth_Arg (Data, 2, null);
               Label := new Subprogram_Label_Record'
                 (Label => Subp,
                  Path  => Ada.Strings.Unbounded.To_Unbounded_String
                    (Nth_Arg (Data, 6, "")));
               Register_Contextual_Menu
                 (Kernel,
                  Name        => Action,
                  Label       => Label,
                  Ref_Item    => Ref,
                  Add_Before  => Before,
                  Action      => Action);
         end;

      elsif Command = "__doc__" then
         Inst := Data.Nth_Arg (1, Action_Class);
         declare
            Action_Name : constant String := Get_Data (Inst, Action_Class);
            Action      : constant Action_Record_Access :=
              Lookup_Action (Kernel, Action_Name);

         begin
            if Action = null then
               Set_Return_Value (Data, String'(""));
            else
               Set_Return_Value (Data,
                                 Get_Full_Description
                                   (Action           => Action,
                                    Kernel           => Kernel,
                                    Use_Markup       => False,
                                    Include_Name     => False,
                                    Include_Category => False,
                                    Include_Menus    => False));
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
      Menu_Class : constant Class_Type := New_Class (Kernel, "Menu");
      Action_Class : constant Class_Type := New_Class (Kernel, "Action");
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
      Custom_Timeout.Register_Commands (Kernel);
      XML_Viewer.Register_Commands (Kernel);

      Register_Command
        (Kernel, Constructor_Method,
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("exists",
         Class         => Action_Class,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("create",
         Class         => Action_Class,
         Params        => (1 => Param ("on_activate"),
                           2 => Param ("filter", Optional => True),
                           3 => Param ("category", Optional => True),
                           4 => Param ("description", Optional => True),
                           5 => Param ("icon",        Optional => True),
                           6 => Param ("for_learning", Optional => True)),
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("disable",
         Params        => (1 => Param ("disabled", Optional => True)),
         Class         => Action_Class,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("destroy_ui",
         Class         => Action_Class,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("can_execute",
         Class         => Action_Class,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Property
        ("__doc__",
         Class         => Action_Class,
         Getter        => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("execute_if_possible",
         Class         => Action_Class,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("key",
         Class         => Action_Class,
         Params        => (2 => Param ("key"),
                           3 => Param ("exclusive", Optional => True)),
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get_keys",
         Class   => Action_Class,
         Handler => Action_Handler'Access);
      Register_Command
        (Kernel, "menu",
         Class         => Action_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 3,
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("contextual",
         Class         => Action_Class,
         Params        => (2 => Param ("path"),
                           3 => Param ("ref",         Optional => True),
                           4 => Param ("add_before",  Optional => True),
                           5 => Param ("group",       Optional => True),
                           6 => Param ("static_path", Optional => True)),
         Handler       => Action_Handler'Access);
      Kernel.Scripts.Register_Command
        ("button",
         Params  => (1 => Param ("toolbar", Optional => True),
                     2 => Param ("section", Optional => True),
                     3 => Param ("group",   Optional => True),
                     4 => Param ("label",   Optional => True),
                     5 => Param ("icon",    Optional => True),
                     6 => Param ("hide",    Optional => True)),
         Class   => Action_Class,
         Handler => Action_Handler'Access);

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
      Kernel.Scripts.Register_Property
        ("action",
         Class         => Menu_Class,
         Getter        => Menu_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class   => Contextual_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler => Contextual_Handler'Access);
      Kernel.Scripts.Register_Command
        ("show",
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Kernel.Scripts.Register_Command
        ("hide",
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_sensitive",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Kernel.Scripts.Register_Command
        ("create_dynamic",
         Minimum_Args => 2,
         Maximum_Args => 8,
         Class => Contextual_Class,
         Handler => Contextual_Handler'Access);
      Kernel.Scripts.Register_Command
        ("list",
         Class         => Contextual_Class,
         Static_Method => True,
         Handler       => Contextual_Handler'Access);
      Kernel.Scripts.Register_Property
        ("name",
         Class         => Contextual_Class,
         Getter        => Contextual_Handler'Access);

      Language.Shell.Setup (Kernel_Handle (Kernel));
   end Register_Module;

end Custom_Module;
