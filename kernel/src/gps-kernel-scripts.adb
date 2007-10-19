-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2007, AdaCore                  --
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

with Ada.Unchecked_Conversion;

with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Regpat;             use GNAT.Regpat;
with GNAT.Scripts.Gtkada;     use GNAT.Scripts.Gtkada;
with GNAT.Scripts.Utils;      use GNAT.Scripts.Utils;
with GNAT.Traces;             use GNAT.Traces;

with Glib.Object;             use Glib.Object;
with Gtk.Label;               use Gtk.Label;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Object;              use Gtk.Object;
with Gtk.Widget;              use Gtk.Widget;

with Basic_Types;             use Basic_Types;
with Commands.Interactive;    use Commands, Commands.Interactive;
with Entities.Queries;        use Entities, Entities.Queries;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.Custom;       use GPS.Kernel.Custom;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Properties;   use GPS.Kernel.Properties;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel.Command_API;  use GPS.Kernel.Command_API;
with Histories;               use Histories;
with Interactive_Consoles;    use Interactive_Consoles;
with Language_Handlers;       use Language_Handlers;
with Prj.Ext;                 use Prj.Ext;
with Projects.Editor;         use Projects.Editor;
with Projects.Registry;       use Projects.Registry;
with Projects;                use Projects;
with String_List_Utils;
with System;                  use System;
with System.Address_Image;
with System.Assertions;
with Traces;
with VFS;                     use VFS;
with OS_Utils;                use OS_Utils;

package body GPS.Kernel.Scripts is

   Me     : constant Trace_Handle := Create ("GPS.Kernel.Scripts");
   Ref_Me : constant Trace_Handle := Create ("Scripts.Ref", GNAT.Traces.Off);

   Entity_Class_Name        : constant String := "Entity";
   File_Class_Name          : constant String := "File";
   Project_Class_Name       : constant String := "Project";
   Context_Class_Name       : constant String := "Context";
   File_Location_Class_Name : constant String := "FileLocation";
   Logger_Class_Name        : constant String := "Logger";
   Hook_Class_Name          : constant String := "Hook";

   function To_Address is new Ada.Unchecked_Conversion
     (Selection_Context_Data, System.Address);

   type GPS_Properties_Type is
     (Files, Contexts, Entities, Projects, File_Locations, Debug_Handles);

   type GPS_Properties_Record (Typ : GPS_Properties_Type)
     is new Instance_Property_Record
   with record
      case Typ is
         when Files =>
            File : Virtual_File;
         when Contexts =>
            Context : Selection_Context := No_Context;
         when Entities =>
            Entity  : Entity_Information;
         when Projects =>
            Project : Project_Type;
         when File_Locations =>
            Location : File_Location_Info;
         when Debug_Handles =>
            Handle   : Trace_Handle;
      end case;
   end record;

   type GPS_Properties is access all GPS_Properties_Record'Class;
   overriding procedure Destroy (Prop : in out GPS_Properties_Record);
   overriding function Get_Instances
     (Prop : GPS_Properties_Record) return Instance_List_Access;
   --  See inherited documentation

   procedure On_Console_Destroy
     (Console : access Gtk_Widget_Record'Class;
      Subprogram : Subprogram_Type);
   --  Called when an interactive console is destroyed

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the default commands

   procedure Create_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Entity" command

   procedure Create_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "File" command

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Project" command

   procedure Create_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Location" command

   procedure Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all context-related commands

   procedure Entity_Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all entity_context-related commands

   procedure GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all GUI class commands

   procedure Logger_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all Logger class commands

   procedure Set_Data (Instance : Class_Instance; File : Virtual_File);
   procedure Set_Data
     (Instance : Class_Instance; Project  : Project_Type);
   procedure Set_Data
     (Instance : Class_Instance; Location : File_Location_Info);
   procedure Set_Data
     (Instance : Class_Instance; Context  : Selection_Context);
   --  Set the data for an instance

   function On_Console_Input
     (Console : access Interactive_Console_Record'Class;
      Input   : String; User_Data : System.Address) return String;
   --  Called when input is available on a console

   procedure Console_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles command related to GPS.Console

   function Get_Or_Create_Context
     (Script : access Scripting_Language_Record'Class;
      Class  : Class_Type;
      Context : GPS.Kernel.Selection_Context)
      return Class_Instance;
   --  Create a new instance representing the context. If such an instance
   --  already exists for that context, return the same one so that the user
   --  can store his own data in it.

   function Get_Area_Context_Class
     (Repo : Scripts_Repository) return Class_Type;
   function Get_File_Context_Class
     (Repo : Scripts_Repository) return Class_Type;
   function Get_Context_Class
     (Repo : Scripts_Repository) return Class_Type;
   function Get_Entity_Context_Class
     (Repo : Scripts_Repository) return Class_Type;
   --  Create or return existing classes

   type Hyper_Link_Subprogram is new Hyper_Link_Callback_Record with record
      Subprogram : Subprogram_Type;
   end record;
   overriding procedure On_Click
     (Link : access Hyper_Link_Subprogram; Text : String);
   --  Called when a user clicks on a hyper link in a console

   Name_Cst       : aliased constant String := "name";
   Filename_Cst   : aliased constant String := "filename";
   File_Cst       : aliased constant String := "file";
   Line_Cst       : aliased constant String := "line";
   Col_Cst        : aliased constant String := "column";
   Shared_Lib_Cst : aliased constant String := "shared_lib";
   Module_Cst     : aliased constant String := "module";
   Xml_Cst        : aliased constant String := "xml";
   Action_Cst     : aliased constant String := "action";
   Prefix_Cst     : aliased constant String := "prefix";
   Sensitive_Cst  : aliased constant String := "sensitive";
   Force_Cst      : aliased constant String := "force";
   Value_Cst      : aliased constant String := "value";
   Recursive_Cst  : aliased constant String := "recursive";
   Default_Cst    : aliased constant String := "default_to_root";
   Nth_Cst        : aliased constant String := "nth";
   Local_Cst      : aliased constant String := "local";
   Regexp_Cst     : aliased constant String := "regexp";
   On_Click_Cst   : aliased constant String := "on_click";
   Text_Cst       : aliased constant String := "text";

   Create_Link_Args         : constant Cst_Argument_List :=
     (1 => Regexp_Cst'Access, 2 => On_Click_Cst'Access);
   Write_With_Link_Args         : constant Cst_Argument_List :=
     (1 => Text_Cst'Access);
   Project_Cmd_Parameters   : constant Cst_Argument_List :=
                                (1 => Name_Cst'Access);
   Insmod_Cmd_Parameters    : constant Cst_Argument_List :=
                                (1 => Shared_Lib_Cst'Access,
                                 2 => Module_Cst'Access);
   Body_Cmd_Parameters      : constant Cst_Argument_List :=
                                (1 => Nth_Cst'Access);
   Entity_Cmd_Parameters    : constant Cst_Argument_List :=
                                (Name_Cst'Access, File_Cst'Access,
                                 Line_Cst'Access, Col_Cst'Access);
   File_Cmd_Parameters      : constant Cst_Argument_List :=
                                (1 => Name_Cst'Access,
                                 2 => Local_Cst'Access);
   File_Project_Parameters  : constant Cst_Argument_List :=
                                (1 => Default_Cst'Access);
   File_Entities_Parameters  : constant Cst_Argument_List :=
                                (1 => Local_Cst'Access);
   Open_Cmd_Parameters      : constant Cst_Argument_List :=
                                (1 => Filename_Cst'Access,
                                 2 => Force_Cst'Access);
   Location_Cmd_Parameters  : constant Cst_Argument_List :=
                                (1 => Filename_Cst'Access,
                                 2 => Line_Cst'Access,
                                 3 => Col_Cst'Access);
   Xml_Custom_Parameters    : constant Cst_Argument_List :=
                                (1 => Xml_Cst'Access);
   Exec_Action_Parameters   : constant Cst_Argument_List :=
                                (1 => Action_Cst'Access);
   Scenar_Var_Parameters    : constant Cst_Argument_List :=
                                (1 => Prefix_Cst'Access);
   Set_Sensitive_Parameters : constant Cst_Argument_List :=
                                (1 => Sensitive_Cst'Access);
   Set_Scenario_Parameters  : constant Cst_Argument_List :=
                                (1 => Name_Cst'Access,
                                 2 => Value_Cst'Access);

   Accept_Input_Cst : aliased constant String := "accept_input";
   On_Input_Cst     : aliased constant String := "on_input";
   On_Destroy_Cst   : aliased constant String := "on_destroy";

   Console_Constructor_Args : constant Cst_Argument_List :=
     (Name_Cst'Access, Force_Cst'Access,
      On_Input_Cst'Access, On_Destroy_Cst'Access, Accept_Input_Cst'Access);

   Enable_Cst         : aliased constant String := "enable";

   Enable_Input_Args  : constant Cst_Argument_List := (1 => Enable_Cst'Access);

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Entity : Entity_Information) is
   begin
      if not Is_Subclass (Instance, Entity_Class_Name) then
         raise Invalid_Data;
      end if;

      Ref (Entity);
      Set_Data
        (Instance, Entity_Class_Name,
         GPS_Properties_Record'(Typ => Entities, Entity => Entity));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Location : File_Location_Info) is
   begin
      if not Is_Subclass (Instance, File_Location_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, File_Location_Class_Name,
         GPS_Properties_Record'
           (Typ => File_Locations, Location => Location));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Entity_Information
   is
      Class : constant Class_Type := Get_Entity_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance := Nth_Arg
        (Data, N, Class, Allow_Null => True);
      Props : Instance_Property;
   begin
      if Inst = No_Class_Instance then
         return null;
      end if;

      Props := Get_Data (Inst, Entity_Class_Name);
      if Props = null then
         return null;
      else
         return GPS_Properties (Props).Entity;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Data : Callback_Data'Class; N : Positive)
      return File_Location_Info
   is
      Class : constant Class_Type :=
                Get_File_Location_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance :=
                Nth_Arg (Data, N, Class);
      D     : Instance_Property;
   begin
      if Inst /= No_Class_Instance then
         D := Get_Data (Inst, File_Location_Class_Name);
      end if;

      if D = null then
         return No_File_Location;
      else
         return GPS_Properties (D).Location;
      end if;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Instance : Class_Instance; File : Virtual_File) is
   begin
      if not Is_Subclass (Instance, File_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, File_Class_Name,
         GPS_Properties_Record'(Typ => Files, File => File));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Virtual_File
   is
      Class : constant Class_Type := Get_File_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance := Nth_Arg (Data, N, Class);
   begin
      return Get_Data (Inst);
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : Class_Instance) return Virtual_File is
      Data : Instance_Property;
   begin
      if Instance /= No_Class_Instance then
         Data := Get_Data (Instance, File_Class_Name);
      end if;

      if Data = null then
         return VFS.No_File;
      else
         return GPS_Properties (Data).File;
      end if;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Instance : Class_Instance; Project  : Project_Type) is
   begin
      if not Is_Subclass (Instance, Project_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, Project_Class_Name,
         GPS_Properties_Record'(Typ => Projects, Project => Project));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Project_Type
   is
      Class : constant Class_Type := Get_Project_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance := Nth_Arg (Data, N, Class);
      Value : constant Instance_Property :=
        Get_Data (Inst, Project_Class_Name);
   begin
      if Value = null then
         return No_Project;
      else
         return GPS_Properties (Value).Project;
      end if;
   end Get_Data;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "get_system_dir" then
         Set_Return_Value (Data, Get_System_Dir (Kernel));

      elsif Command = "get_tmp_dir" then
         Set_Return_Value (Data, OS_Utils.Get_Tmp_Dir);

      elsif Command = "get_home_dir" then
         Set_Return_Value (Data, Get_Home_Dir (Kernel));

      elsif Command = "insmod" then
         Name_Parameters (Data, Insmod_Cmd_Parameters);

         declare
            Shared  : constant String := Nth_Arg (Data, 1);
            Module  : constant String := Nth_Arg (Data, 2);
            Success : Boolean;
         begin
            Dynamic_Register_Module (Kernel, Shared, Module, Success);

            if Success then
               Set_Return_Value (Data, -"Module successfully loaded.");
            else
               Set_Return_Value (Data, -"Couldn't load module.");
            end if;
         end;

      elsif Command = "lsmod" then
         declare
            use type Module_List.List_Node;
            Current : Module_List.List_Node;
            List    : constant Module_List.List := List_Of_Modules (Kernel);

         begin
            Current := Module_List.First (List);

            Set_Return_Value_As_List (Data);

            while Current /= Module_List.Null_Node loop
               Set_Return_Value
                 (Data,
                  Module_Name (Module_List.Data (Current)));
               Current := Module_List.Next (Current);
            end loop;
         end;

      elsif Command = "supported_languages" then
         declare
            Langs : Argument_List := Known_Languages
              (Get_Language_Handler (Kernel), Sorted => True);
         begin
            Set_Return_Value_As_List (Data);

            for L in Langs'Range loop
               Set_Return_Value (Data, Langs (L).all);
            end loop;

            Free (Langs);
         end;

      elsif Command = "set_busy" then
         Push_State (Kernel, Processing);

      elsif Command = "get_busy" then
         Set_Return_Value (Data, Get_Busy (Kernel));

      elsif Command = "unset_busy" then
         Pop_State (Kernel);

      elsif Command = "parse_xml" then
         Name_Parameters (Data, Xml_Custom_Parameters);
         declare
            Err : constant String :=
                    GPS.Kernel.Custom.Add_Customization_String
                      (Kernel, Nth_Arg (Data, 1),
                       From_File  => Current_Script (Get_Script (Data)),
                       Start_Line => 1);
         begin
            if Err /= "" then
               Set_Error_Msg (Data, Err);
            end if;
         end;

      elsif Command = "execute_action"
        or else Command = "execute_asynchronous_action"
      then
         Name_Parameters (Data, Exec_Action_Parameters);

         declare
            Synchronous : constant Boolean := Command = "execute_action";
            Action      : constant Action_Record_Access := Lookup_Action
              (Kernel, Nth_Arg (Data, 1));
            Context     : constant Selection_Context :=
                            Get_Current_Context (Kernel);
            Custom      : Command_Access;
            Args        : String_List_Access;
         begin
            if Action = null then
               Set_Error_Msg (Data, -"No such registered action");

            elsif Context = No_Context then
               Set_Error_Msg
                 (Data, -"No current context, can't execute action");

            elsif not Filter_Matches (Action.Filter, Context) then
               Set_Error_Msg (Data, -"Invalid context for the action");

            else
               Args := new String_List (1 .. Number_Of_Arguments (Data) - 1);
               for Index in 2 .. Number_Of_Arguments (Data) loop
                  Args (Index - 1) := new String'(Nth_Arg (Data, Index));
               end loop;

               Custom := Create_Proxy
                 (Command => Action.Command,
                  Context => (Event       => null,
                              Context     => Context,
                              Synchronous => Synchronous,
                              Dir         => null,
                              Args        => Args,
                              Label       => new String'(Nth_Arg (Data, 1)),
                              Repeat_Count     => 1,
                              Remaining_Repeat => 0));

               if Synchronous then
                  Launch_Foreground_Command
                    (Kernel, Custom, Destroy_On_Exit => True);
               else
                  Launch_Background_Command
                    (Kernel, Custom,
                     Destroy_On_Exit => True,
                     Active          => Synchronous,
                     Show_Bar        => True,
                     Queue_Id        => "");
               end if;
            end if;
         end;

      elsif Command = "scenario_variables" then
         declare
            Vars : constant Scenario_Variable_Array :=
                     Scenario_Variables (Kernel);
         begin
            for V in Vars'Range loop
               Set_Return_Value (Data, Value_Of (Vars (V)));
               Set_Return_Value_Key
                 (Data, External_Reference_Of (Vars (V)));
            end loop;
         end;

      elsif Command = "set_scenario_variable" then
         Name_Parameters (Data, Set_Scenario_Parameters);
         declare
            Name  : constant String := Nth_Arg (Data, 1);
            Value : constant String := Nth_Arg (Data, 2);
         begin
            Prj.Ext.Add (Name, Value);
            Run_Hook (Kernel, Variable_Changed_Hook);
         end;

      elsif Command = "scenario_variables_cmd_line" then
         Name_Parameters (Data, Scenar_Var_Parameters);
         declare
            Prefix : constant String := Nth_Arg (Data, 1, "");
         begin
            Set_Return_Value
              (Data, Scenario_Variables_Cmd_Line (Kernel, Prefix));
         end;

      elsif Command = "scenario_variables_values" then
         declare
            Vars : constant Scenario_Variable_Array :=
                     Scenario_Variables (Kernel);
         begin
            for V in Vars'Range loop
               declare
                  use String_List_Utils.String_List;

                  Name   : constant String := External_Reference_Of (Vars (V));
                  Values : String_List_Utils.String_List.List :=
                             Enum_Values_Of
                               (Vars (V), Get_Registry (Kernel).all);
                  Iter   : String_List_Utils.String_List.List_Node :=
                             First (Values);
               begin
                  while Iter /= String_List_Utils.String_List.Null_Node loop
                     Set_Return_Value
                       (Data, String_List_Utils.String_List.Data (Iter));
                     Set_Return_Value_Key (Data, Name, True);
                     Iter := Next (Iter);
                  end loop;

                  Free (Values);
               end;
            end loop;
         end;
      end if;
   end Default_Command_Handler;

   -------------------------------------
   -- Create_Location_Command_Handler --
   -------------------------------------

   procedure Create_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Location : File_Location_Info;

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Location_Cmd_Parameters);

         declare
            File     : constant Class_Instance  :=
                         Nth_Arg (Data, 2, Get_File_Class (Kernel));
            L        : constant Integer := Nth_Arg (Data, 3);
            C        : constant Visible_Column_Type :=
                         Visible_Column_Type (Nth_Arg (Data, 4, Default => 1));
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_File_Location_Class (Kernel));
         begin
            Set_Data (Instance, File_Location_Info'(File, L, C));
         end;

      elsif Command = "line" then
         Location := Get_Data (Data, 1);
         Set_Return_Value (Data, Get_Line (Location));

      elsif Command = "column" then
         Location := Get_Data (Data, 1);
         Set_Return_Value (Data, Natural (Get_Column (Location)));

      elsif Command = "file" then
         Location := Get_Data (Data, 1);

         declare
            File : constant Class_Instance := Get_File (Location);
         begin
            Set_Return_Value (Data, File);
         end;
      end if;
   end Create_Location_Command_Handler;

   -----------------------------------
   -- Create_Entity_Command_Handler --
   -----------------------------------

   procedure Create_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Entity : Entity_Information;
      Ref    : Entity_Reference;

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Entity_Cmd_Parameters);

         declare
            Name   : constant String  := Nth_Arg (Data, 2);
            File   : constant Class_Instance  :=
                       Nth_Arg (Data, 3, Get_File_Class (Kernel),
                                Default    => No_Class_Instance,
                                Allow_Null => True);
            L      : Integer := Nth_Arg (Data, 4, Default => 1);
            C      : Visible_Column_Type :=
                       Visible_Column_Type (Nth_Arg (Data, 5, Default => 1));
            Status : Find_Decl_Or_Body_Query_Status;
            F      : Virtual_File;
            Source : Source_File;

         begin
            if File = No_Class_Instance then
               --  ??? MANU Don't know what Handler to pass here, since we do
               --  not have enough information to recognize the context.
               Source := Get_Predefined_File
                 (Get_Database (Kernel),
                  Get_LI_Handler_By_Name
                    (Get_Language_Handler (Kernel), "Ada"));
               L      := Predefined_Line;
               C      := Predefined_Column;
            else
               F := Get_Data (File);
               Source := Get_Or_Create
                 (Get_Database (Kernel), F,
                  Get_LI_Handler (Get_Database (Kernel), F));
            end if;

            Find_Declaration_Or_Overloaded
              (Kernel            => Kernel,
               File              => Source,
               Entity_Name       => Name,
               Line              => L,
               Column            => C,
               Ask_If_Overloaded => False,
               Closest_Ref       => Ref,
               Entity            => Entity,
               Status            => Status);

            if Status /= Success and then Status /= Fuzzy_Match then
               Set_Error_Msg (Data, -"Entity not found");
            else
               declare
                  Instance : constant Class_Instance :=
                    Nth_Arg (Data, 1, Get_Entity_Class (Kernel));
               begin
                  Set_Data (Instance, Entity);
               end;
            end if;
         end;

      elsif Command = "name" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Get_Name (Entity).all);

      elsif Command = "declaration" then
         declare
            Location : File_Location;
         begin
            Entity := Get_Data (Data, 1);
            Location := Get_Declaration_Of (Entity);

            Set_Return_Value
              (Data, Create_File_Location
                 (Get_Script (Data),
                  File   => Create_File
                    (Get_Script (Data), Get_Filename (Get_File (Location))),
                  Line   => Get_Line (Location),
                  Column => Get_Column (Location)));
         end;

      elsif Command = "body" then
         Name_Parameters (Data, Body_Cmd_Parameters);
         declare
            Location : File_Location := Standard.Entities.No_File_Location;
            Count    : Integer := Nth_Arg (Data, 2, 1);
         begin
            Entity := Get_Data (Data, 1);
            while Count > 0 loop
               Find_Next_Body
                 (Entity, Current_Location => Location, Location => Location,
                  No_Location_If_First => True);
               Count := Count - 1;
            end loop;

            if Location /= Standard.Entities.No_File_Location then
               Set_Return_Value
                 (Data, Create_File_Location
                    (Get_Script (Data),
                     File   => Create_File
                       (Get_Script (Data), Get_Filename (Get_File (Location))),
                     Line   => Get_Line (Location),
                     Column => Get_Column (Location)));

            else
               Set_Error_Msg (Data, -"Body not found for the entity");
            end if;
         end;

      elsif Command = "attributes" then
         Entity := Get_Data (Data, 1);
         declare
            Attr : constant Entity_Attributes := Get_Attributes (Entity);
         begin
            for A in Attr'Range loop
               Set_Return_Value (Data, Attr (A));
               Set_Return_Value_Key (Data, Image (A));
            end loop;
         end;
      end if;
   end Create_Entity_Command_Handler;

   ---------------------------------
   -- Create_File_Command_Handler --
   ---------------------------------

   procedure Create_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Info   : Virtual_File;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, File_Cmd_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_File_Class (Kernel));
            Name     : constant String := Nth_Arg (Data, 2);
            File     : Virtual_File;
         begin
            if Is_Absolute_Path (Name) then
               if Is_Cygwin_Path (Name) then
                  --  This is a cygwing PATH style, convert to standard DOS
                  Set_Data
                    (Instance,
                     Create (Format_Pathname (Name, DOS)));
               else
                  Set_Data (Instance, Create (Name));
               end if;
               return;
            end if;

            --  Base name case. Find full name using the following rules:
            --  1) If third argument is set to true, create from current dir
            --  else
            --  2) If Base Name can be found in project, use it
            --  else
            --  3) Create from current dir

            --  If we really want to create from current directory
            if Number_Of_Arguments (Data) > 2 then
               declare
                  From_Current : constant Boolean := Nth_Arg (Data, 3);
               begin
                  if From_Current then
                     Set_Data
                       (Instance,
                        Create_From_Dir (Get_Current_Dir, Nth_Arg (Data, 2)));
                     return;
                  end if;
               end;
            end if;

            --  Kernel's Create_Form_Base will override File if needed
            File := Create_From_Dir (Get_Current_Dir, Nth_Arg (Data, 2));
            Set_Data (Instance,
                      Create_From_Base (Full_Name (File).all, Kernel));
         end;

      elsif Command = "name" then
         Info := Get_Data (Data, 1);
         Set_Return_Value (Data, Full_Name (Info).all);

      elsif Command = "project" then
         Name_Parameters (Data, File_Project_Parameters);
         Info := Get_Data (Data, 1);
         Set_Return_Value
           (Data, Create_Project
            (Get_Script (Data),
             Get_Project_From_File
             (Registry         => Project_Registry (Get_Registry (Kernel).all),
              Source_Filename   => Info,
              Root_If_Not_Found => Nth_Arg (Data, 2, True))));

      elsif Command = "directory" then
         Info := Get_Data (Data, 1);
         Set_Return_Value (Data, Dir_Name (Info).all);

      elsif Command = "language" then
         Info := Get_Data (Data, 1);
         Set_Return_Value
           (Data, Get_Language_From_File
              (Get_Language_Handler (Kernel), Info));

      elsif Command = "other_file" then
         declare
            Other   : Virtual_File;
            Project : Project_Type;
         begin
            Info := Get_Data (Data, 1);

            Project := Get_Project_From_File
              (Project_Registry (Get_Registry (Kernel).all),
               Info,
               Root_If_Not_Found => True);
            Other := Create
              (Other_File_Base_Name (Project, Info), Project,
               Use_Object_Path => False);

            Set_Return_Value (Data, Create_File (Get_Script (Data), Other));
         end;

      elsif Command = "entities" then
         Name_Parameters (Data, File_Entities_Parameters);
         Info := Get_Data (Data, 1);
         declare
            Iter   : Entity_Iterator;
            Defined_In_File : constant Boolean := Nth_Arg (Data, 2, True);
            Ent    : Entity_Information;
            Source : Source_File;
         begin
            Set_Return_Value_As_List (Data);
            Source := Get_Or_Create
              (Db            => Get_Database (Kernel),
               File          => Info,
               Allow_Create  => True);

            if Source /= null then
               Find_All_Entities_In_File
                 (Iter  => Iter,
                  File  => Source,
                  Name  => "");

               while not At_End (Iter) loop
                  Ent := Get (Iter);
                  if not Defined_In_File
                    or else Get_Filename (Get_File (Get_Declaration_Of (Ent)))
                    = Info
                  then
                     Set_Return_Value
                       (Data, Create_Entity (Get_Script (Data), Ent));
                  end if;
                  Next (Iter);
               end loop;
               Destroy (Iter);
            end if;
         end;

      end if;
   end Create_File_Command_Handler;

   ------------------------------------
   -- Create_Project_Command_Handler --
   ------------------------------------

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Instance : Class_Instance;
      Project  : Project_Type;

   begin
      if Command = "load" then
         Name_Parameters (Data, Open_Cmd_Parameters);
         Load_Project (Kernel,
                       Create (Normalize_Pathname (Nth_Arg (Data, 1))),
                       No_Save => Nth_Arg (Data, 2, False));
         Set_Return_Value
           (Data, Create_Project (Get_Script (Data), Get_Project (Kernel)));

      elsif Command = "recompute" then
         Recompute_View (Get_Kernel (Data));

      elsif Command = "root" then
         Set_Return_Value
           (Data, Create_Project (Get_Script (Data), Get_Project (Kernel)));

      else
         if Command = Constructor_Method then
            Name_Parameters (Data, Project_Cmd_Parameters);
            Project  := Get_Project_From_Name
              (Project_Registry (Get_Registry (Kernel).all),
               Get_String (Nth_Arg (Data, 2)));

            if Project = No_Project then
               Set_Error_Msg (Data, -"No such project: " & Nth_Arg (Data, 2));
            else
               Instance := Nth_Arg (Data, 1, Get_Project_Class (Kernel));
               Set_Data (Instance, Project);
            end if;

         elsif Command = "name" then
            Project := Get_Data (Data, 1);
            Set_Return_Value (Data, Project_Name (Project));

         elsif Command = "file" then
            Project := Get_Data (Data, 1);
            Set_Return_Value
              (Data,
               Create_File (Get_Script (Data), Project_Path (Project)));

         elsif Command = "ancestor_deps" then
            declare
               Iter : Imported_Project_Iterator;
               P    : Project_Type;
            begin
               Project := Get_Data (Data, 1);
               Set_Return_Value_As_List (Data);
               Iter := Find_All_Projects_Importing
                 (Project, Include_Self => True);

               loop
                  P := Current (Iter);
                  exit when P = No_Project;
                  Set_Return_Value
                    (Data, Create_Project (Get_Script (Data), P));
                  Next (Iter);
               end loop;
            end;

         elsif Command = "dependencies" then
            Name_Parameters (Data, (1 => Recursive_Cst'Access));
            declare
               Recursive : constant Boolean := Nth_Arg (Data, 2, False);
               Iter : Imported_Project_Iterator;
               P    : Project_Type;
            begin
               Project := Get_Data (Data, 1);
               Set_Return_Value_As_List (Data);
               Iter := Start
                 (Project, Recursive => True, Direct_Only => not Recursive);

               loop
                  P := Current (Iter);
                  exit when P = No_Project;
                  Set_Return_Value
                    (Data, Create_Project (Get_Script (Data), P));
                  Next (Iter);
               end loop;
            end;

         end if;
      end if;
   end Create_Project_Command_Handler;

   ------------------------------------
   -- Entity_Context_Command_Handler --
   ------------------------------------

   procedure Entity_Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Entity : constant Selection_Context := Get_Data (Data, 1);
   begin
      if Command = "entity" then
         Set_Return_Value
           (Data, Create_Entity (Get_Script (Data), Get_Entity (Entity)));
      end if;
   end Entity_Context_Command_Handler;

   -----------------------------
   -- Context_Command_Handler --
   -----------------------------

   procedure Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Context : Selection_Context;
      Object  : Glib.Object.GObject;
      Menu    : Gtk.Menu.Gtk_Menu;
      L, C    : Integer := -1;
      Inst    : Class_Instance;

      procedure Recursive_Analyze_Menu
        (Depth : Natural;
         Menu  : Gtk_Menu);
      --  Recursively set the menu content as command result.
      --  Depth is the current depth of the analyzed menu
      --  Menu is the menu that will be analyzed

      ----------------------------
      -- Recursive_Analyze_Menu --
      ----------------------------

      procedure Recursive_Analyze_Menu
        (Depth : Natural;
         Menu  : Gtk_Menu)
      is
         List      : Gtk.Widget.Widget_List.Glist;
         Menu_Item : Gtk_Menu_Item;
         Label     : Gtk.Label.Gtk_Label;
         Submenu   : Gtk_Menu;
         use type Widget_List.Glist;
      begin
         List := Get_Children (Menu);

         while List /= Widget_List.Null_List loop
            Menu_Item := Gtk_Menu_Item (Widget_List.Get_Data (List));

            if Menu_Item /= null then
               Label := Gtk_Label (Gtk.Menu_Item.Get_Child (Menu_Item));

               if Label /= null then
                  Set_Return_Value
                    (Data, Natural'Image (Depth) & " - " & Get_Text (Label));
               else
                  Set_Return_Value
                    (Data, Natural'Image (Depth) & " - " & "<separator>");
               end if;

               Submenu := Gtk_Menu (Get_Submenu (Menu_Item));
               if Submenu /= null then
                  Recursive_Analyze_Menu (Depth + 1, Submenu);
               end if;
            end if;

            List := Widget_List.Next (List);
         end loop;
      end Recursive_Analyze_Menu;

   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, -"Cannot create an instance of this class");

      elsif Command = "start_line" then
         Context := Get_Data (Data, 1);
         Get_Area (Context, L, C);
         Set_Return_Value (Data, L);

      elsif Command = "end_line" then
         Context := Get_Data (Data, 1);
         Get_Area (Context, L, C);
         Set_Return_Value (Data, C);

      elsif Command = "file" then
         Context := Get_Data (Data, 1);
         if Has_File_Information (Context) then
            Set_Return_Value
              (Data, Create_File (Get_Script (Data),
               File_Information (Context)));
         else
            Set_Error_Msg (Data, -"No file information stored in the context");
         end if;

      elsif Command = "location" then
         Context := Get_Data (Data, 1);
         if Has_Line_Information (Context) then
            L := Line_Information (Context);
         end if;

         if Has_Column_Information (Context) then
            C := Integer (Column_Information (Context));
         end if;

         if Has_File_Information (Context) then
            Set_Return_Value
              (Data,
               Create_File_Location
                 (Get_Script (Data),
                  (Create_File (Get_Script (Data),
                   File_Information (Context))),
                  L,
                  Visible_Column_Type (C)));
         else
            Set_Error_Msg
              (Data, -"No file information stored in the context");
         end if;

      elsif Command = "project" then
         Context := Get_Data (Data, 1);
         if Has_Project_Information (Context) then
            Set_Return_Value
              (Data,
               Create_Project (Get_Script (Data),
                 Project_Information (Context)));
         elsif Has_File_Information (Context) then
            --  Since the editor doesn't provide the project, we emulate it
            --  here
            Set_Return_Value
              (Data,
               Create_Project
                 (Get_Script (Data),
                  Get_Project_From_File
                    (Project_Registry (Get_Registry (Kernel).all),
                     File_Information (Context),
                     Root_If_Not_Found => False)));
         else
            Set_Error_Msg (Data, -"No project stored in the context");
         end if;

      elsif Command = "directory" then
         Context := Get_Data (Data, 1);
         if Has_Directory_Information (Context) then
            Set_Return_Value (Data, Directory_Information (Context));
         else
            Set_Error_Msg (Data, -"No directory stored in the context");
         end if;

      elsif Command = "location" then
         Context := Get_Data (Data, 1);

         if Has_Line_Information (Context) then
            L := Line_Information (Context);
         end if;

         if Has_Column_Information (Context) then
            C := Integer (Column_Information (Context));
         end if;

         if Has_File_Information (Context) then
            Set_Return_Value
              (Data,
               Create_File_Location
                 (Get_Script (Data),
                  (Create_File (Get_Script (Data),
                   File_Information (Context))),
                  L,
                  Visible_Column_Type (C)));
         else
            Set_Error_Msg
              (Data, -"No file information stored in the context");
         end if;

      elsif Command = "current_context"
        or else Command = "contextual_context"
      then
         if Command = "current_context" then
            Context := Get_Current_Context (Kernel);
         else
            Context := Kernel.Last_Context_For_Contextual;
         end if;

         Inst := Create_Context (Get_Script (Data), Context);
         if Inst = No_Class_Instance then
            Set_Error_Msg (Data, -"No context available");
         else
            Set_Return_Value (Data, Inst);
         end if;

      elsif Command = "contextual_menu" then
         Context := Get_Data (Data, 1);
         Object := GObject (Get_Current_Focus_Widget (Kernel));

         if Object /= null then
            Gtk.Menu.Gtk_New (Menu);
            GPS.Kernel.Modules.Create_Contextual_Menu
              (Kernel, Object, Context, Menu);
            if Menu /= null then
               Set_Return_Value_As_List (Data);
               Recursive_Analyze_Menu (1, Menu);
            else
               Set_Return_Value (Data, "<empty menu>");
            end if;
            Destroy (Menu);
         else
            Set_Error_Msg (Data, -"Seems like no window has focus...");
         end if;
      end if;
   end Context_Command_Handler;

   ----------------------
   -- On_Console_Input --
   ----------------------

   function On_Console_Input
     (Console : access Interactive_Console_Record'Class;
      Input   : String; User_Data : System.Address) return String
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Subprogram_Type);
      On_Input : constant Subprogram_Type := Convert (User_Data);
      Instance : constant Class_Instance :=
                   Get_Instance (Get_Script (On_Input.all), Console);
      C        : Callback_Data'Class := Create (Get_Script (On_Input.all), 2);
      Tmp      : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg (C, 1, Instance);
      Set_Nth_Arg (C, 2, Input);
      Tmp := Execute (On_Input, C);
      Free (C);
      return "";   --  ??? Should this be the output of the command
   end On_Console_Input;

   ------------------------
   -- On_Console_Destroy --
   ------------------------

   procedure On_Console_Destroy
     (Console    : access Gtk_Widget_Record'Class;
      Subprogram : Subprogram_Type)
   is
      Inst   : constant Class_Instance :=
                 Get_Instance (Get_Script (Subprogram.all), Console);
      Script : constant Scripting_Language := Get_Script (Subprogram.all);
   begin
      if Script /= null then
         declare
            C : Callback_Data'Class := Create (Script, 1);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (C, 1, Inst);
            Tmp := Execute (Subprogram, C);
            Free (C);
         end;
      end if;
   end On_Console_Destroy;

   -----------------------------
   -- Console_Command_Handler --
   -----------------------------

   procedure Console_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst          : constant Class_Instance := Nth_Arg (Data, 1);
      Console       : Interactive_Console;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Console_Constructor_Args);
         declare
            Title        : constant String := Nth_Arg (Data, 2, "");
            Force        : constant Boolean := Nth_Arg (Data, 3, False);
            On_Input     : constant Subprogram_Type := Nth_Arg (Data, 4, null);
            On_Destroy   : constant Subprogram_Type := Nth_Arg (Data, 5, null);
            Accept_Input : constant Boolean := Nth_Arg (Data, 6, True);
         begin
            Console := Create_Interactive_Console
              (Kernel              => Get_Kernel (Data),
               Title               => Title,
               History             => History_Key ("console_" & Title),
               Create_If_Not_Exist => Title /= "Python"
                 and then Title /= "Shell",
               Force_Create        => Force,
               Accept_Input        => Accept_Input);
            --   ??? If the console was already associated with an instance,
            --  we would lose that original instance and all data the user
            --  might have stored in it.

            if Console = null then
               if Title = "Python" or else Title = "Shell" then
                  Set_Error_Msg
                    (Data, "To create the python or shell console, please use"
                     & " the menus /Tools/Consoles/... through "
                     & " GPS.execute_action");
               else
                  Set_Error_Msg
                    (Data, "Could not create the console " & Title);
               end if;
               return;
            end if;

            GNAT.Scripts.Set_Data
              (Inst, Get_Or_Create_Virtual_Console (Console));

            if Title /= ""
              and then Console /= null
              and then On_Input /= null
            then
               Set_Command_Handler
                 (Console, On_Console_Input'Access, On_Input.all'Address);
            end if;

            if Console /= null and then On_Destroy /= null then
               Subprogram_Callback.Connect
                 (Console, Signal_Destroy, On_Console_Destroy'Access,
                  User_Data => On_Destroy);
            end if;
         end;

      elsif Command = "accept_input" then
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Set_Return_Value (Data, Is_Editable (Console));
         else
            Set_Error_Msg (Data, -"Console was closed by user");
         end if;

      elsif Command = "enable_input" then
         Name_Parameters (Data, Enable_Input_Args);
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Enable_Prompt_Display (Console, Nth_Arg (Data, 2));
         else
            Set_Error_Msg (Data, -"Console was closed by user");
         end if;

      elsif Command = "get_text" then
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Set_Return_Value (Data, Get_Chars (Console));
         else
            Set_Error_Msg (Data, -"Console was closed by user");
         end if;

      elsif Command = "create_link" then
         Name_Parameters (Data, Create_Link_Args);
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         declare
            Cb : constant Hyper_Link_Callback := new Hyper_Link_Subprogram'
              (Hyper_Link_Callback_Record with
               Subprogram => Nth_Arg (Data, 3));
         begin
            Create_Hyper_Link
              (Console,
               Regexp => Compile (Nth_Arg (Data, 2)),
               Callback => Cb);
         exception
            when GNAT.Regpat.Expression_Error =>
               Set_Error_Msg (Data, "Invalid regular expression");
         end;

      elsif Command = "write_with_links" then
         Name_Parameters (Data, Write_With_Link_Args);
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         Insert_With_Links
           (Console,
            Text      => Nth_Arg (Data, 2),
            Add_LF    => False);
      end if;
   end Console_Command_Handler;

   --------------------
   -- Logger_Handler --
   --------------------

   procedure Logger_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name_Cst            : aliased constant String := "name";
      Message_Cst         : aliased constant String := "message";
      Active_Cst          : aliased constant String := "active";
      Condition_Cst       : aliased constant String := "condition";
      Error_Message_Cst   : aliased constant String := "error_message";
      Success_Message_Cst : aliased constant String := "success_message";
      Inst                : constant Class_Instance := Nth_Arg (Data, 1);
      Prop                : Instance_Property;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Set_Data
           (Inst, Logger_Class_Name, GPS_Properties_Record'
              (Typ => Debug_Handles, Handle => Create (Nth_Arg (Data, 2))));

      elsif Command = "log" then
         Name_Parameters (Data, (1 => Message_Cst'Unchecked_Access));
         Prop := Get_Data (Inst, Logger_Class_Name);
         Trace (GPS_Properties (Prop).Handle, Nth_Arg (Data, 2));

      elsif Command = "set_active" then
         Name_Parameters (Data, (1 => Active_Cst'Unchecked_Access));
         Prop := Get_Data (Inst, Logger_Class_Name);
         Set_Active (GPS_Properties (Prop).Handle, Nth_Arg (Data, 2));

      elsif Command = "check" then
         Name_Parameters (Data, (1 => Condition_Cst'Unchecked_Access,
                                 2 => Error_Message_Cst'Unchecked_Access,
                                 3 => Success_Message_Cst'Unchecked_Access));
         begin
            Prop := Get_Data (Inst, Logger_Class_Name);
            Assert (GPS_Properties (Prop).Handle,
                    Condition          => Nth_Arg (Data, 2),
                    Error_Message      => Nth_Arg (Data, 3),
                    Message_If_Success => Nth_Arg (Data, 4, ""));
         exception
            when System.Assertions.Assert_Failure =>
               Set_Error_Msg (Data, "Assertion error: " & Nth_Arg (Data, 3));
         end;
      end if;
   end Logger_Handler;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command       : String;
      Minimum_Args  : Natural    := 0;
      Maximum_Args  : Natural    := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False) is
   begin
      Register_Command
        (Repo          => Kernel.Scripts,
         Command       => Command,
         Minimum_Args  => Minimum_Args,
         Maximum_Args  => Maximum_Args,
         Handler       => Handler,
         Class         => Class,
         Static_Method => Static_Method);
   end Register_Command;

   ---------
   -- Get --
   ---------

   function Get
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      List   : Callback_Data_List;
      Script : access Scripting_Language_Record'Class)
      return Callback_Data_Access is
   begin
      return Get (Get_Scripts (Kernel), List, Script);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      List   : in out Callback_Data_List;
      Script : access Scripting_Language_Record'Class;
      Data   : Callback_Data_Access) is
   begin
      Set (Get_Scripts (Kernel), List, Script, Data);
   end Set;

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Base   : Class_Type := No_Class) return Class_Type is
   begin
      return New_Class (Get_Scripts (Kernel), Name, Base);
   end New_Class;

   --------------------------------------
   -- Register_Default_Script_Commands --
   --------------------------------------

   procedure Register_Default_Script_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Console_Class_Name : constant String := "Console";
      Console_Class      : constant Class_Type := New_Class
        (Kernel.Scripts, Console_Class_Name, Base => Get_GUI_Class (Kernel));
      Logger             : constant Class_Type :=
                             New_Class (Kernel.Scripts, Logger_Class_Name);
   begin
      GNAT.Scripts.Register_Standard_Classes
        (Get_Scripts (Kernel),
         Console_Class_Name => Console_Class_Name);

      Register_Command
        (Kernel, Constructor_Method, 1, 1, Logger_Handler'Access, Logger);
      Register_Command
        (Kernel, "log", 1, 1, Logger_Handler'Access, Logger);
      Register_Command
        (Kernel, "set_active", 1, 1, Logger_Handler'Access, Logger);
      Register_Command
        (Kernel, "check", 2, 3, Logger_Handler'Access, Logger);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 0,
         Maximum_Args => 5,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "enable_input",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "accept_input",
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "get_text",
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "create_link",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "write_with_links",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);

      Register_Command
        (Kernel, "get_system_dir",
         Handler => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "get_tmp_dir",
         Handler => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "get_home_dir",
         Handler => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "insmod",
         Minimum_Args => 2,
         Maximum_Args => 2,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "lsmod",
         Handler => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "supported_languages",
         Handler => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "execute_action",
         Minimum_Args => 1,
         Maximum_Args => Integer'Last,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "execute_asynchronous_action",
         Minimum_Args => 1,
         Maximum_Args => Integer'Last,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "parse_xml",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "set_busy",
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "get_busy",
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "unset_busy",
         Handler      => Default_Command_Handler'Access);

      Register_Command
        (Kernel, "scenario_variables",
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "set_scenario_variable",
         Class         => Get_Project_Class (Kernel),
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "scenario_variables_cmd_line",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "scenario_variables_values",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel, "language",
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel, "name",
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel, "directory",
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel, "other_file",
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel, "entities",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);
      Register_Command
        (Kernel, "project",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_File_Class (Kernel),
         Handler      => Create_File_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 4,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "name",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "declaration",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "body",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "attributes",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 3,
         Maximum_Args => 3,
         Class        => Get_File_Location_Class (Kernel),
         Handler      => Create_Location_Command_Handler'Access);
      Register_Command
        (Kernel, "line",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Create_Location_Command_Handler'Access);
      Register_Command
        (Kernel, "column",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Create_Location_Command_Handler'Access);
      Register_Command
        (Kernel, "file",
         Class        => Get_File_Location_Class (Kernel),
         Handler      => Create_Location_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "root",
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "recompute",
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "load",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler       => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "name",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "file",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "ancestor_deps",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "dependencies",
         Class        => Get_Project_Class (Kernel),
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Create_Project_Command_Handler'Access);

      if Active (Traces.Testsuite_Handle) then
         Register_Command
           (Kernel, "contextual_menu",
            Class        => Get_Context_Class (Kernel),
            Handler      => Context_Command_Handler'Access);
      end if;

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "file",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "project",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "directory",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "location",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Get_Area_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "start_line",
         Class        => Get_Area_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "end_line",
         Class        => Get_Area_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Get_Entity_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "entity",
         Class        => Get_Entity_Context_Class (Kernel),
         Handler      => Entity_Context_Command_Handler'Access);

      Register_Command
        (Kernel, "current_context",
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "contextual_context",
         Handler      => Context_Command_Handler'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Class   => Get_GUI_Class (Kernel),
         Handler => GUI_Command_Handler'Access);
      Register_Command
        (Kernel, "set_sensitive",
         Maximum_Args => 1,
         Class        => Get_GUI_Class (Kernel),
         Handler      => GUI_Command_Handler'Access);
      Register_Command
        (Kernel, "is_sensitive",
         Maximum_Args => 0,
         Class        => Get_GUI_Class (Kernel),
         Handler      => GUI_Command_Handler'Access);
      Register_Command
        (Kernel, "destroy",
         Class        => Get_GUI_Class (Kernel),
         Handler      => GUI_Command_Handler'Access);
      Register_Command
        (Kernel, "hide",
         Class        => Get_GUI_Class (Kernel),
         Handler      => GUI_Command_Handler'Access);
      Register_Command
        (Kernel, "show",
         Class        => Get_GUI_Class (Kernel),
         Handler      => GUI_Command_Handler'Access);

      GPS.Kernel.Properties.Register_Script_Commands (Kernel);
      GPS.Kernel.Command_API.Register_Commands (Kernel);
   end Register_Default_Script_Commands;

   ----------------------
   -- Get_Entity_Class --
   ----------------------

   function Get_Entity_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Entity_Class_Name);
   end Get_Entity_Class;

   --------------------
   -- Get_File_Class --
   --------------------

   function Get_File_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, File_Class_Name);
   end Get_File_Class;

   -----------------------
   -- Get_Project_Class --
   -----------------------

   function Get_Project_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Project_Class_Name);
   end Get_Project_Class;

   -----------------------------
   -- Get_File_Location_Class --
   -----------------------------

   function Get_File_Location_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, File_Location_Class_Name);
   end Get_File_Location_Class;

   -----------------
   -- Get_Scripts --
   -----------------

   function Get_Scripts
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return GNAT.Scripts.Scripts_Repository is
   begin
      return Kernel.Scripts;
   end Get_Scripts;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String) return String
   is
      Errors : aliased Boolean;
   begin
      Trace (Me, "Executing " & Command);
      return Execute_Command
        (Lookup_Scripting_Language (Kernel.Scripts, GPS_Shell_Name),
         Command, null, True, True, Errors'Unchecked_Access);
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List)
   is
      Output : constant String := Execute_Command_With_Args
        (Lookup_Scripting_Language (Kernel.Scripts, GPS_Shell_Name),
         Command, Args);
      pragma Unreferenced (Output);
   begin
      if Active (Me) then
         Trace
           (Me, "Executing " & Command & " "
            & Argument_List_To_Quoted_String (Args));
      end if;
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String is
   begin
      if Active (Me) then
         Trace (Me, "Executing " & Command & " "
                & Argument_List_To_Quoted_String (Args));
      end if;

      return Execute_Command_With_Args
        (Lookup_Scripting_Language (Kernel.Scripts, GPS_Shell_Name),
         Command, Args);
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String)
   is
      Errors : aliased Boolean;
      Str : constant String := Execute_Command
        (Lookup_Scripting_Language (Kernel.Scripts, GPS_Shell_Name),
         Command, null, True, True, Errors'Unchecked_Access);
      pragma Unreferenced (Str);
   begin
      Trace (Me, "Executing " & Command);
   end Execute_GPS_Shell_Command;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Data : Callback_Data'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Data).all).Kernel;
   end Get_Kernel;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Script : access GNAT.Scripts.Scripting_Language_Record'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Script).all).Kernel;
   end Get_Kernel;

   -------------------
   -- Create_Entity --
   -------------------

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Entity_Information) return Class_Instance
   is
      Instance : Class_Instance;
   begin
      if Entity = null then
         return No_Class_Instance;
      else
         Instance := New_Instance
           (Script, New_Class (Get_Repository (Script), Entity_Class_Name));
         Set_Data (Instance, Entity);
         return Instance;
      end if;
   end Create_Entity;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : Virtual_File) return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, New_Class (Get_Repository (Script), File_Class_Name));
   begin
      Set_Data (Instance, File);
      return Instance;
   end Create_File;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : Project_Type) return Class_Instance
   is
      Instance : Class_Instance := No_Class_Instance;
   begin
      if Project /= No_Project then
         Instance := New_Instance
           (Script, New_Class (Get_Repository (Script), Project_Class_Name));
         Set_Data (Instance, Project);
      end if;
      return Instance;
   end Create_Project;

   --------------------------
   -- Create_File_Location --
   --------------------------

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type) return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script,
         New_Class (Get_Repository (Script), File_Location_Class_Name));
      Info     : constant File_Location_Info := (File, Line, Column);

   begin
      Set_Data (Instance, Info);
      return Instance;
   end Create_File_Location;

   --------------
   -- Get_File --
   --------------

   function Get_File (Location : File_Location_Info) return Class_Instance is
   begin
      return Location.File;
   end Get_File;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Location : File_Location_Info) return Integer is
   begin
      return Location.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Location : File_Location_Info) return Visible_Column_Type is
   begin
      return Location.Column;
   end Get_Column;

   -----------------------
   -- Get_Context_Class --
   -----------------------

   function Get_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return Get_Context_Class (Kernel.Scripts);
   end Get_Context_Class;

   function Get_Context_Class
     (Repo : Scripts_Repository) return Class_Type is
   begin
      return New_Class (Repo, Context_Class_Name);
   end Get_Context_Class;

   --------------------
   -- Get_Hook_Class --
   --------------------

   function Get_Hook_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Hook_Class_Name);
   end Get_Hook_Class;

   ----------------------------
   -- Get_Area_Context_Class --
   ----------------------------

   function Get_Area_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return Get_Area_Context_Class (Kernel.Scripts);
   end Get_Area_Context_Class;

   function Get_Area_Context_Class
     (Repo : Scripts_Repository) return Class_Type is
   begin
      return New_Class
        (Repo, "AreaContext", Base => Get_File_Context_Class (Repo));
   end Get_Area_Context_Class;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : GPS.Kernel.Selection_Context) return Class_Instance is
   begin
      if Context = No_Context then
         Trace (Me, "Null context passed to Create_Context");
         return No_Class_Instance;

      elsif Has_Area_Information (Context) then
         return Get_Or_Create_Context
           (Script,
            Get_Area_Context_Class (Get_Repository (Script)),
            Context);

      elsif Has_Entity_Name_Information (Context) then
         return Get_Or_Create_Context
           (Script,
            Get_Entity_Context_Class (Get_Repository (Script)),
            Context);

      elsif Has_File_Information (Context)
        or else Has_Project_Information (Context)
        or else Has_Directory_Information (Context)
      then
         return Get_Or_Create_Context
           (Script,
            Get_File_Context_Class (Get_Repository (Script)),
            Context);

      else
         Trace (Me, "Context type is not supported by GPS");
         return Get_Or_Create_Context
           (Script,
            Get_Context_Class (Get_Repository (Script)),
            Context);
      end if;
   end Create_Context;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance) return GPS.Kernel.Selection_Context
   is
      Value : constant Instance_Property :=
        Get_Data (Instance, Context_Class_Name);
   begin
      if Value = null then
         return No_Context;
      else
         return GPS_Properties (Value).Context;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Data : Callback_Data'Class; N : Positive)
                      return GPS.Kernel.Selection_Context is
   begin
      return Get_Data
        (Nth_Arg (Data, N, Get_Context_Class (Get_Kernel (Data))));
   end Get_Data;

   ----------------------------
   -- Get_File_Context_Class --
   ----------------------------

   function Get_File_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return Get_File_Context_Class (Kernel.Scripts);
   end Get_File_Context_Class;

   function Get_File_Context_Class
     (Repo : Scripts_Repository) return Class_Type is
   begin
      return New_Class
        (Repo, "FileContext", Base => Get_Context_Class (Repo));
   end Get_File_Context_Class;

   ------------------------------
   -- Get_Entity_Context_Class --
   ------------------------------

   function Get_Entity_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return Get_Entity_Context_Class (Kernel.Scripts);
   end Get_Entity_Context_Class;

   function Get_Entity_Context_Class
     (Repo : Scripts_Repository) return Class_Type is
   begin
      return New_Class
        (Repo, "EntityContext", Base => Get_File_Context_Class (Repo));
   end Get_Entity_Context_Class;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Context  : Selection_Context) is
   begin
      if not Is_Subclass (Instance, Context_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, Context_Class_Name,
         GPS_Properties_Record'(Typ => Contexts, Context => Context));
   end Set_Data;

   ---------------------------
   -- Get_Or_Create_Context --
   ---------------------------

   function Get_Or_Create_Context
     (Script  : access Scripting_Language_Record'Class;
      Class   : Class_Type;
      Context : GPS.Kernel.Selection_Context)
      return Class_Instance
   is
      Instance : Class_Instance;
      Initial_Ref      : constant Integer := Context.Data.Data.Ref_Count;
   begin
      if Active (Ref_Me) then
         Increase_Indent
           (Ref_Me, "Get_Or_Create_Context, context=("
            & System.Address_Image (To_Address (Context.Data.Data))
            & Initial_Ref'Img & ")");
      end if;

      if Context.Data.Data.Instances = null then
         Context.Data.Data.Instances := new Instance_List'(Null_Instance_List);
      end if;

      Instance :=
        Get (Instance_List (Context.Data.Data.Instances.all), Script);

      if Instance = No_Class_Instance then
         Trace (Me, "Create a new instance for the current context");
         Instance := New_Instance (Script, Class);
         Set_Data (Instance, Context);
         Set (Instance_List (Context.Data.Data.Instances.all),
              Script, Instance);

         if Active (Ref_Me) then
--              Assert (Ref_Me,
--                      Instance.Data.Data.Refcount = 3,
--                      "After Get_Or_Create_Context, CI.refcount ("
--                      & Instance.Data.Data.Refcount'Img
--                      & ") should be 3", Raise_Exception => False);
            Assert (Ref_Me,
                    Context.Data.Data.Ref_Count = Initial_Ref + 1,
                    "After Get_Or_Create_Context, context.refcount ("
                    & Context.Data.Data.Ref_Count'Img
                    & ") should be 1+" & Initial_Ref'Img,
                    Raise_Exception => False);
         end if;

      else
         if Active (Ref_Me) then
            Trace
              (Ref_Me, "Get_Or_Create_Context: context already has instance");
         end if;
      end if;

--        if Active (Ref_Me) then
--           Decrease_Indent (Ref_Me, "End of Get_Or_Create_Context "
--                            & Print_Refcount (Instance.Data.Data));
--        end if;

      return Instance;
   end Get_Or_Create_Context;

   -------------------
   -- Get_GUI_Class --
   -------------------

   function Get_GUI_Class
     (Kernel : access Kernel_Handle_Record'Class) return Class_Type is
   begin
      return New_Class (Kernel.Scripts, GUI_Class_Name);
   end Get_GUI_Class;

   -------------------------
   -- GUI_Command_Handler --
   -------------------------

   procedure GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Class : constant Class_Type := Get_GUI_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance := Nth_Arg (Data, 1, Class);
      W     : Gtk_Widget;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, -("Cannot build instances of GPS.GUI, these are returned"
            & " by other functions"));

      elsif Command = "set_sensitive" then
         Name_Parameters (Data, Set_Sensitive_Parameters);
         W := Gtk_Widget (GObject'(Get_Data (Inst)));
         if W /= null then
            Set_Sensitive (W, Nth_Arg (Data, 2, True));
         else
            Set_Error_Msg (Data, "Widget has been destroyed");
         end if;

      elsif Command = "is_sensitive" then
         W := Gtk_Widget (GObject'(Get_Data (Inst)));
         if W /= null then
            Set_Return_Value (Data, Gtk.Widget.Is_Sensitive (W));
         else
            Set_Error_Msg (Data, "Widget has been destroyed");
         end if;

      elsif Command = "destroy" then
         W := Gtk_Widget (GObject'(Get_Data (Inst)));
         if W /= null then
            Destroy (W);
         else
            Set_Error_Msg
              (Data,
               "Widget has already been destroyed, can't destroy it again");
         end if;

      elsif Command = "hide" then
         W := Gtk_Widget (GObject'(Get_Data (Inst)));
         if W /= null then
            Set_Child_Visible (W, False);
            Hide (W);
         else
            Set_Error_Msg (Data, "Widget has been destroyed, can't hide it");
         end if;

      elsif Command = "show" then
         W := Gtk_Widget (GObject'(Get_Data (Inst)));
         if W /= null then
            Set_Child_Visible (W, True);
            Show (W);
         else
            Set_Error_Msg (Data, "Widget has been destroyed, can't show it");
         end if;
      end if;
   end GUI_Command_Handler;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Prop : in out GPS_Properties_Record) is
   begin
      case Prop.Typ is
         when Contexts =>
            --  Nothing to do either. The ref to the class instance that is
            --  hold by the context will be automatically freed when the
            --  context itself is destroyed.
            null;

         when Files | Projects | Debug_Handles =>
            null;

         when Entities =>
            Unref (Prop.Entity);

         when File_Locations =>
            Prop.Location := No_File_Location;
            --  This might also destroy the class instance Data.Location.File
      end case;
   end Destroy;

   -------------------
   -- Get_Instances --
   -------------------

   function Get_Instances
     (Prop : GPS_Properties_Record) return Instance_List_Access
   is
   begin
      case Prop.Typ is
         when Contexts =>
            if Prop.Context.Data.Data /= null then
               return Instance_List_Access (Prop.Context.Data.Data.Instances);
            end if;

         when Files | Entities | Projects | File_Locations | Debug_Handles =>
            null;
      end case;

      return null;
   end Get_Instances;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Link : access Hyper_Link_Subprogram; Text : String)
   is
      Data : Callback_Data'Class :=
        Create (Get_Script (Link.Subprogram.all), 1);
      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      Set_Nth_Arg (Data, 1, Text);
      Result := Execute (Link.Subprogram, Data);
      Free (Data);
   end On_Click;

end GPS.Kernel.Scripts;
