------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with System;                  use System;
with System.Address_Image;

with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Regpat;             use GNAT.Regpat;

with GNATCOLL.Memory;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;      use GNATCOLL.VFS_Utils;

with Gdk.Types;               use Gdk.Types;
with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gtkada.Handlers;
with Gtk.Accel_Group;
with Gtk.Arguments;           use Gtk.Arguments;
with Gtk.Label;               use Gtk.Label;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Font;              use Pango.Font;
with Pango.Layout;            use Pango.Layout;

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
with GPS.Kernel.Modules.UI;   use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Properties;   use GPS.Kernel.Properties;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel.Command_API;  use GPS.Kernel.Command_API;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with Histories;               use Histories;
with Interactive_Consoles;    use Interactive_Consoles;
with Language_Handlers;       use Language_Handlers;
with Projects;                use Projects;
with Remote;                  use Remote;
with String_List_Utils;
with Traces;
with OS_Utils;                use OS_Utils;

package body GPS.Kernel.Scripts is

   Me     : constant Trace_Handle :=
              Create ("GPS.Kernel.Scripts", GNATCOLL.Traces.Off);
   Ref_Me : constant Trace_Handle :=
              Create ("Scripts.Ref", GNATCOLL.Traces.Off);

   Entity_Class_Name        : constant String := "Entity";
   File_Class_Name          : constant String := "File";
   Project_Class_Name       : constant String := "Project";
   Context_Class_Name       : constant String := "Context";
   File_Location_Class_Name : constant String := "FileLocation";
   Hook_Class_Name          : constant String := "Hook";

   function To_Address is new Ada.Unchecked_Conversion
     (Selection_Context_Data, System.Address);

   type GPS_Properties_Type is
     (Files, Contexts, Entities, Projects, File_Locations);

   type GPS_Properties_Record (Typ : GPS_Properties_Type)
     is new Instance_Property_Record
   with record
      case Typ is
         when Files =>
            File : Virtual_File;
         when Contexts =>
            Context : Selection_Context := No_Context;
         when Entities =>
            Entity  : Standard.Entities.Entity_Information;
         when Projects =>
            Project : Project_Type;
         when File_Locations =>
            Location : File_Location_Info;
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

   procedure On_Console_Resize
     (Console    : access Gtk_Widget_Record'Class;
      Args       : Glib.Values.GValues;
      Subprogram : Subprogram_Type);
   --  Called when an interactive console is resized

   function On_Console_Completion
     (Input     : String;
      View      : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      User_Data : System.Address)
      return String_List_Utils.String_List.List;
   --  Called when the user has pressed <tab>

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

   procedure Context_Getters
     (Data : in out Callback_Data'Class; Command : String);
   --  Getter for the "module_name" property

   procedure Entity_Context_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all entity_context-related commands

   procedure GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for all GUI class commands

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

   function On_Console_Key
     (Console   : access Interactive_Console_Record'Class;
      Modifier  : Gdk.Types.Gdk_Modifier_Type;
      Key       : Gdk.Types.Gdk_Key_Type := 0;
      Uni       : Glib.Gunichar := 0;
      User_Data : System.Address) return Boolean;
   --  Called when a key was pressed by the user in the console

   function On_Console_Interrupt
     (Console : access Interactive_Console_Record'Class;
      Data    : System.Address) return Boolean;
   --  Called when the user has pressed control-c in the console and a custom
   --  callback was set from GPS.Console()

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
   Server_Cst     : aliased constant String := "remote_server";
   Fast_Cst       : aliased constant String := "fast";

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
                                 Line_Cst'Access, Col_Cst'Access,
                                 Fast_Cst'Access);
   File_Cmd_Parameters      : constant Cst_Argument_List :=
                                (1 => Name_Cst'Access,
                                 2 => Local_Cst'Access);
   File_Name_Parameters  : constant Cst_Argument_List :=
                                (1 => Server_Cst'Access);
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

   Accept_Input_Cst  : aliased constant String := "accept_input";
   On_Input_Cst      : aliased constant String := "on_input";
   On_Destroy_Cst    : aliased constant String := "on_destroy";
   On_Resize_Cst     : aliased constant String := "on_resize";
   On_Interrupt_Cst  : aliased constant String := "on_interrupt";
   On_Completion_Cst : aliased constant String := "on_completion";
   On_Key_Cst        : aliased constant String := "on_key";
   Manage_Prompt_Cst : aliased constant String := "manage_prompt";
   ANSI_Cst          : aliased constant String := "ansi";

   Console_Constructor_Args : constant Cst_Argument_List :=
     (Name_Cst'Access, Force_Cst'Access,
      On_Input_Cst'Access, On_Destroy_Cst'Access, Accept_Input_Cst'Access,
      On_Resize_Cst'Access, On_Interrupt_Cst'Access,
      On_Completion_Cst'Access, On_Key_Cst'Access,
      Manage_Prompt_Cst'Access, ANSI_Cst'Access);

   Enable_Cst         : aliased constant String := "enable";

   Enable_Input_Args  : constant Cst_Argument_List := (1 => Enable_Cst'Access);

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Entity : Standard.Entities.Entity_Information) is
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
     (Data : Callback_Data'Class;
      N    : Positive) return Standard.Entities.Entity_Information
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

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive) return Virtual_File
   is
      Class : constant Class_Type := Get_File_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance := Nth_Arg (Data, N, Class);
   begin
      return Get_Data (Inst);
   end Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : in out Callback_Data'Class;
      N    : Positive;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Inst  : constant Class_Instance := Create_File (Get_Script (Data), File);
   begin
      Set_Nth_Arg (Data, N, Inst);
   end Set_Nth_Arg;

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
         return GNATCOLL.VFS.No_File;
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
      Inst  : constant Class_Instance :=
        Nth_Arg (Data, N, Class, Allow_Null => True);
      Value : Instance_Property;

   begin
      if Inst = No_Class_Instance then
         return No_Project;
      end if;

      Value := Get_Data (Inst, Project_Class_Name);
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
         Set_Return_Value (Data, +Get_System_Dir (Kernel).Full_Name);

      elsif Command = "get_tmp_dir" then
         Set_Return_Value (Data, +Get_Tmp_Directory.Full_Name);

      elsif Command = "get_home_dir" then
         Set_Return_Value (Data, +Get_Home_Dir (Kernel).Full_Name);

      elsif Command = "debug_memory_usage" then
         GNATCOLL.Memory.Dump
           (Size   => Nth_Arg (Data, 1, 3),
            Report => GNATCOLL.Memory.Report_Type'Val (Nth_Arg (Data, 2, 1)));

      elsif Command = "debug_memory_reset" then
         GNATCOLL.Memory.Reset;

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
                       From_File  => +Current_Script (Get_Script (Data)),
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
            Action_Name : constant String := Nth_Arg (Data, 1);
            Action      : constant Action_Record_Access := Lookup_Action
              (Kernel, Action_Name);
            Context     : constant Selection_Context :=
                            Get_Current_Context (Kernel);
            Custom      : Command_Access;
            Args        : String_List_Access;
         begin
            if Action = null then
               --  If the action was not found, this may be a dynamic menu:
               --  attempt to execute it.

               if Action_Name (Action_Name'First) = '/' then
                  GPS.Kernel.Modules.UI.Execute_Menu (Kernel, Action_Name);
               else
                  Set_Error_Msg (Data, -"No such registered action");
               end if;

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
                              Dir         => No_File,
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
               Set_Return_Value (Data, Value (Vars (V)));
               Set_Return_Value_Key
                 (Data, External_Name (Vars (V)));
            end loop;
         end;

      elsif Command = "set_scenario_variable" then
         Name_Parameters (Data, Set_Scenario_Parameters);
         declare
            Name  : constant String := Nth_Arg (Data, 1);
            Value : constant String := Nth_Arg (Data, 2);
            Var   : Scenario_Variable :=
              Get_Registry (Kernel).Tree.Scenario_Variables (Name);
         begin
            Set_Value (Var, Value);
            Get_Registry (Kernel).Tree.Change_Environment ((1 => Var));
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

                  Name   : constant String := External_Name (Vars (V));
                  Values : String_List :=
                    Get_Registry (Kernel).Tree.Possible_Values_Of (Vars (V));
               begin
                  for Iter in Values'Range loop
                     Set_Return_Value (Data, Values (Iter).all);
                     Set_Return_Value_Key (Data, Name, True);
                  end loop;

                  Free (Values);
               end;
            end loop;
         end;

      elsif Command = "freeze_xref" then
         Freeze (Get_Database (Kernel));

      elsif Command = "thaw_xref" then
         Thaw (Get_Database (Kernel));
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
      Entity : Standard.Entities.Entity_Information;
      Ref    : Standard.Entities.Entity_Reference;

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
            Fuzzy_Expected : Boolean;

         begin
            Fuzzy_Expected := Number_Of_Arguments (Data) < 5;

            if File = No_Class_Instance then
               --  ??? Don't know what Handler to pass here, since we do
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
               Status            => Status,
               Fuzzy_Expected    => Fuzzy_Expected);

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

      elsif Command = "full_name" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Get_Full_Name (Entity));

      elsif Command = "name" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Get (Get_Name (Entity)).all);

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
            Location     : File_Location := Standard.Entities.No_File_Location;
            Cur_Location : File_Location := Standard.Entities.No_File_Location;
            Count        : Integer := Nth_Arg (Data, 2, 1);
         begin
            Entity := Get_Data (Data, 1);
            while Count > 0 loop
               Find_Next_Body
                 (Entity,
                  Current_Location     => Cur_Location,
                  Location             => Location,
                  No_Location_If_First => True);
               Count := Count - 1;

               Cur_Location := Location;
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

      elsif Command = "category" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Category_To_String (Get_Category (Entity)));

      elsif Command = "end_of_scope" then
         declare
            Location : File_Location := Standard.Entities.No_File_Location;
            Kind     : Reference_Kind;
         begin
            Entity := Get_Data (Data, 1);
            Get_End_Of_Scope (Entity, Location, Kind);

            if Location /= Standard.Entities.No_File_Location then
               Set_Return_Value
                 (Data, Create_File_Location
                    (Get_Script (Data),
                     File   => Create_File
                       (Get_Script (Data), Get_Filename (Get_File (Location))),
                     Line   => Get_Line (Location),
                     Column => Get_Column (Location)));

            else
               Set_Error_Msg (Data, -"end-of-scope not found for the entity");
            end if;
         end;
      end if;
   end Create_Entity_Command_Handler;

   ---------------------------------
   -- Create_File_Command_Handler --
   ---------------------------------

   procedure Create_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Info    : Virtual_File;
      Project : Project_Type;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, File_Cmd_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_File_Class (Kernel));
            Name     : constant Filesystem_String := Nth_Arg (Data, 2);
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
                        Create_From_Dir (Get_Current_Dir, Name));
                     return;
                  end if;
               end;
            end if;

            --  Kernel's Create_From_Base will override File if needed

            File := Create_From_Base (Name);
            Set_Data
              (Instance, Create_From_Base (Full_Name (File), Kernel));
         end;

      elsif Command = "name" then
         Name_Parameters (Data, File_Name_Parameters);
         Info := Nth_Arg (Data, 1);

         declare
            Server : Server_Type;
         begin
            --  Get the Server_Type value
            begin
               Server := Server_Type'Value (Nth_Arg (Data, 2, "GPS_Server"));
            exception
               when Constraint_Error =>
                  Server := GPS_Server;
            end;

            if Server = GPS_Server then
               Set_Return_Value (Data, Full_Name (Info));
            else
               Set_Return_Value
                 (Data,
                  Full_Name (To_Remote (Info, Get_Nickname (Server))));
            end if;
         end;

      elsif Command = "project" then
         Name_Parameters (Data, File_Project_Parameters);
         Info := Nth_Arg (Data, 1);
         Project := Get_Registry (Kernel).Tree.Info (Info).Project;

         if Project = No_Project and then Nth_Arg (Data, 2, True) then
            Project := Get_Project (Kernel);
         end if;

         Set_Return_Value (Data, Create_Project (Get_Script (Data), Project));

      elsif Command = "directory" then
         Info := Nth_Arg (Data, 1);
         Set_Return_Value (Data, Dir_Name (Info));

      elsif Command = "language" then
         Info := Nth_Arg (Data, 1);
         Set_Return_Value
           (Data, Get_Language_From_File
              (Get_Language_Handler (Kernel), Info));

      elsif Command = "other_file" then
         Info  := Nth_Arg (Data, 1);
         Set_Return_Value
           (Data,
            Create_File (Get_Script (Data),
                         Get_Registry (Kernel).Tree.Other_File (Info)));

      elsif Command = "entities" then
         Name_Parameters (Data, File_Entities_Parameters);
         Info := Nth_Arg (Data, 1);
         declare
            Iter   : Entity_Iterator;
            Defined_In_File : constant Boolean := Nth_Arg (Data, 2, True);
            Ent    : Standard.Entities.Entity_Information;
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
                       No_Save      => Nth_Arg (Data, 2, False),
                       Keep_Desktop => Nth_Arg (Data, 3, False),
                       Clear        => not Nth_Arg (Data, 3, False));

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
            Project  := Get_Registry (Kernel).Tree.Project_From_Name
              (Nth_Arg (Data, 2));

            if Project = No_Project then
               Set_Error_Msg (Data, -"No such project: " & Nth_Arg (Data, 2));
            else
               Instance := Nth_Arg (Data, 1, Get_Project_Class (Kernel));
               Set_Data (Instance, Project);
            end if;

         elsif Command = "name" then
            Project := Get_Data (Data, 1);
            Set_Return_Value (Data, Project.Name);

         elsif Command = "file" then
            Project := Get_Data (Data, 1);
            Set_Return_Value
              (Data,
               Create_File (Get_Script (Data), Project_Path (Project)));

         elsif Command = "ancestor_deps" then
            declare
               Iter : Project_Iterator;
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
               Iter : Project_Iterator;
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

         elsif Command = "update_xref" then
            Name_Parameters (Data, (1 => Recursive_Cst'Access));
            Parse_All_LI_Information
              (Kernel    => Kernel,
               Project   => Get_Data (Data, 1),
               Recursive => Nth_Arg (Data, 2, False));
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

   ---------------------
   -- Context_Getters --
   ---------------------

   procedure Context_Getters
     (Data : in out Callback_Data'Class; Command : String)
   is
      Context : Selection_Context;
   begin
      Context := Get_Data (Data, 1);
      if Command = "module_name" then
         Set_Return_Value (Data, Get_Name (Module_ID (Get_Creator (Context))));
      end if;
   end Context_Getters;

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
              (Data,
               Create_File
                 (Get_Script (Data),
                  File_Information (Context)));
         else
            Set_Error_Msg (Data, -"No file information stored in the context");
         end if;

      elsif Command = "files" then
         Context := Get_Data (Data, 1);
         if Has_File_Information (Context) then
            Set_Return_Value_As_List (Data);

            declare
               Files : constant File_Array := File_Information (Context);
            begin
               for J in Files'Range loop
                  Set_Return_Value
                    (Data, Create_File (Get_Script (Data), Files (J)));
               end loop;
            end;
         else
            Set_Error_Msg (Data, -"No file information stored in the context");
         end if;

      elsif Command = "location" then
         Context := Get_Data (Data, 1);
         if Has_Line_Information (Context) then
            L := Line_Information (Context);
         else
            L := 1;
         end if;

         if Has_Column_Information (Context) then
            C := Integer (Column_Information (Context));
         else
            C := 1;
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
                  Get_Registry (Kernel).Tree.Info
                     (File_Information (Context)).Project));

         else
            Set_Error_Msg (Data, -"No project stored in the context");
         end if;

      elsif Command = "directory" then
         Context := Get_Data (Data, 1);
         if Has_Directory_Information (Context) then
            --  ??? We should return the Virtual_File here ?
            --  Set_Return_Value
            --    (Data,
            --     Create_File
            --       (Get_Script (Data),
            --        Directory_Information (Context)));
            Set_Return_Value
              (Data, Directory_Information (Context).Full_Name);
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
            Create_Contextual_Menu
              (Kernel, Object, Context, Menu);
            if Menu /= null then
               Set_Return_Value_As_List (Data);
               Recursive_Analyze_Menu (1, Menu);
            else
               Set_Return_Value (Data, String'("<empty menu>"));
            end if;
            Destroy (Menu);
         else
            Set_Error_Msg (Data, -"Seems like no window has focus...");
         end if;
      end if;
   end Context_Command_Handler;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Subprogram_Type);

   ---------------------------
   -- On_Console_Completion --
   ---------------------------

   function On_Console_Completion
     (Input     : String;
      View      : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      User_Data : System.Address)
      return String_List_Utils.String_List.List
   is
      On_Completion : constant Subprogram_Type := Convert (User_Data);
      Console  : constant Interactive_Console := From_View (View);
      Instance : constant Class_Instance :=
        Get_Instance (Get_Script (On_Completion.all), Console);
      C        : Callback_Data'Class :=
        Create (Get_Script (On_Completion.all), 2);
      Tmp      : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg (C, 1, Instance);
      Set_Nth_Arg (C, 2, Input);
      Tmp := Execute (On_Completion, C);
      Free (C);
      return String_List_Utils.String_List.Null_List;
   end On_Console_Completion;

   ----------------------
   -- On_Console_Input --
   ----------------------

   function On_Console_Input
     (Console : access Interactive_Console_Record'Class;
      Input   : String; User_Data : System.Address) return String
   is
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

   --------------------
   -- On_Console_Key --
   --------------------

   function On_Console_Key
     (Console   : access Interactive_Console_Record'Class;
      Modifier  : Gdk.Types.Gdk_Modifier_Type;
      Key       : Gdk.Types.Gdk_Key_Type := 0;
      Uni       : Glib.Gunichar := 0;
      User_Data : System.Address) return Boolean
   is
      On_Key   : constant Subprogram_Type := Convert (User_Data);
      Instance : constant Class_Instance :=
                   Get_Instance (Get_Script (On_Key.all), Console);
      C        : Callback_Data'Class := Create (Get_Script (On_Key.all), 4);
      Tmp      : Boolean;

      --  Remove any num-lock and caps-lock modifiers
      M        : constant Gdk_Modifier_Type :=
        Modifier and Gtk.Accel_Group.Get_Default_Mod_Mask;

   begin
      Set_Nth_Arg (C, 1, Instance);
      Set_Nth_Arg (C, 2, Integer (Key));
      Set_Nth_Arg (C, 3, Integer (Uni));
      Set_Nth_Arg (C, 4, Gdk_Modifier_Type'Pos (M));
      Tmp := Execute (On_Key, C);
      Free (C);
      return Tmp;
   end On_Console_Key;

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

   -----------------------
   -- On_Console_Resize --
   -----------------------

   procedure On_Console_Resize
     (Console    : access Gtk_Widget_Record'Class;
      Args       : Glib.Values.GValues;
      Subprogram : Subprogram_Type)
   is
      Script : constant Scripting_Language := Get_Script (Subprogram.all);
      Inst   : constant Class_Instance     := Get_Instance (Script, Console);
      Alloc  : constant Gtk_Allocation_Access := To_Allocation (Args, 1);
   begin
      if Script /= null then
         declare
            Font : constant Pango_Font_Description :=
              GPS.Kernel.Preferences.Default_Style.Get_Pref_Font;
            W2, H2, Tmp2 : Gint;
            Layout : Pango_Layout;

            C : Callback_Data'Class := Create (Script, 3);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Layout := Create_Pango_Layout
              (Get_View (Interactive_Console (Console)));
            Set_Font_Description (Layout, Font);
            Set_Text (Layout, "mmmmmmmmmmm");
            Get_Pixel_Size (Layout, W2, Tmp2);
            Set_Text (Layout, "fp");
            Get_Pixel_Size (Layout, Tmp2, H2);
            Unref (Layout);

            Set_Nth_Arg (C, 1, Inst);
            Set_Nth_Arg (C, 2, Integer (Alloc.Height / H2));
            Set_Nth_Arg (C, 3, Integer (Alloc.Width * 10 / W2));
            Tmp := Execute (Subprogram, C);
            Free (C);
         end;
      end if;
   end On_Console_Resize;

   --------------------------
   -- On_Console_Interrupt --
   --------------------------

   function On_Console_Interrupt
     (Console : access Interactive_Console_Record'Class;
      Data    : System.Address) return Boolean
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Subprogram_Type);
      Subprogram : constant Subprogram_Type := Convert (Data);
      Script : constant Scripting_Language := Get_Script (Subprogram.all);
      Inst   : constant Class_Instance     := Get_Instance (Script, Console);
      C      : Callback_Data'Class         := Create (Script, 1);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg (C, 1, Inst);
      Tmp := Execute (Subprogram, C);
      Free (C);

      return True;
   end On_Console_Interrupt;

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
            On_Resize    : constant Subprogram_Type := Nth_Arg (Data, 7, null);
            On_Interrupt : constant Subprogram_Type := Nth_Arg (Data, 8, null);
            On_Completion : constant Subprogram_Type :=
              Nth_Arg (Data, 9, null);
            On_Key      : constant Subprogram_Type := Nth_Arg (Data, 10, null);
            Manage_Prompt : constant Boolean := Nth_Arg (Data, 11, True);
            ANSI_Support  : constant Boolean := Nth_Arg (Data, 12, False);
         begin
            Console := Create_Interactive_Console
              (Kernel              => Get_Kernel (Data),
               Title               => Title,
               History             => History_Key ("console_" & Title),
               Create_If_Not_Exist => Title /= "Python"
                 and then Title /= "Shell",
               Force_Create        => Force,
               Manage_Prompt       => Manage_Prompt,
               ANSI_Support        => ANSI_Support,
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

            GNATCOLL.Scripts.Set_Data
              (Inst, Get_Or_Create_Virtual_Console (Console));

            if Title /= ""
              and then On_Input /= null
            then
               Set_Command_Handler
                 (Console, On_Console_Input'Access, On_Input.all'Address);
            end if;

            if On_Destroy /= null then
               Subprogram_Callback.Connect
                 (Console, Signal_Destroy, On_Console_Destroy'Access,
                  User_Data => On_Destroy);
            end if;

            if On_Resize /= null then
               Subprogram_Callback.Connect
                 (Console, Signal_Size_Allocate,
                  On_Console_Resize'Access,
                  User_Data => On_Resize);
            end if;

            if On_Interrupt /= null then
               Set_Interrupt_Handler
                 (Console, On_Console_Interrupt'Access,
                  User_Data => On_Interrupt.all'Address);
            end if;

            if On_Completion /= null then
               Set_Completion_Handler
                 (Console, On_Console_Completion'Access,
                  User_Data => On_Completion.all'Address);
            end if;

            if On_Key /= null then
               Set_Key_Handler
                 (Console, On_Console_Key'Access,
                  User_Data => On_Key.all'Address);
            end if;
         end;

      elsif Command = "accept_input" then
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Set_Return_Value (Data, Is_Editable (Console));
         else
            Set_Error_Msg (Data, -"Console was closed by user");
         end if;

      elsif Command = "clear_input" then
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Clear_Input (Console);
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

      elsif Command = "add_input" then
         Name_Parameters (Data, (1 => Text_Cst'Access));
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         Insert (Console, Nth_Arg (Data, 2), Add_LF => False,
                 Add_To_History => False, Text_Is_Input => True,
                 Show_Prompt => False);

      elsif Command = "write_with_links" then
         Name_Parameters (Data, Write_With_Link_Args);
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         Insert_With_Links
           (Console,
            Text      => Nth_Arg (Data, 2),
            Add_LF    => False);
      elsif Command = "select_all" then
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Gtkada.Handlers.Widget_Callback.Emit_By_Name
              (Console.Get_View, Gtk.Text_View.Signal_Select_All, Gint'(-1));
         else
            Set_Error_Msg (Data, -"Console was closed by user");
         end if;
      elsif Command = "copy_clipboard" then
         Console := Interactive_Console (GObject'(Get_Data (Inst)));
         if Console /= null then
            Gtkada.Handlers.Widget_Callback.Emit_By_Name
              (Console.Get_View, Gtk.Text_View.Signal_Copy_Clipboard);
         else
            Set_Error_Msg (Data, -"Console was closed by user");
         end if;
      end if;
   end Console_Command_Handler;

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
      if Kernel = null then
         return No_Class;
      else
         return New_Class (Get_Scripts (Kernel), Name, Base);
      end if;
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
      Tmp : String_Access;
   begin
      GNATCOLL.Scripts.Register_Standard_Classes
        (Get_Scripts (Kernel),
         Console_Class_Name => Console_Class_Name,
         Logger_Class_Name  => "Logger");

      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 0,
         Maximum_Args => 11,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "enable_input",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "add_input",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "accept_input",
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "clear_input",
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
        (Kernel, "select_all",
         Class        => Console_Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Kernel, "copy_clipboard",
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

      Tmp := Getenv ("GPS_MEMORY_MONITOR" & ASCII.NUL);
      if Tmp.all /= "" then
         Free (Tmp);
         Register_Command
           (Kernel, "debug_memory_usage",
            Minimum_Args => 0,
            Maximum_Args => 2,
            Handler      => Default_Command_Handler'Access);
         Register_Command
           (Kernel, "debug_memory_reset",
            Minimum_Args => 0,
            Maximum_Args => 0,
            Handler      => Default_Command_Handler'Access);
      end if;
      Free (Tmp);

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
        (Kernel, "freeze_xref",
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "thaw_xref",
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
         Minimum_Args => 0,
         Maximum_Args => 1,
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
         Maximum_Args => 5,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "name",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "full_name",
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
        (Kernel, "category",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Create_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "end_of_scope",
         Minimum_Args => 0,
         Maximum_Args => 1,
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
         Maximum_Args  => 3,
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
      Register_Command
        (Kernel, "update_xref",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);

      if Active (Traces.Testsuite_Handle) then
         Register_Command
           (Kernel, "contextual_menu",
            Class        => Get_Context_Class (Kernel),
            Handler      => Context_Command_Handler'Access);
      end if;

      Register_Property (Repo   => Kernel.Scripts,
                         Name   => "module_name",
                         Class  => Get_Context_Class (Kernel),
                         Setter => null,
                         Getter => Context_Getters'Access);

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "file",
         Class        => Get_File_Context_Class (Kernel),
         Handler      => Context_Command_Handler'Access);
      Register_Command
        (Kernel, "files",
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
      return GNATCOLL.Scripts.Scripts_Repository is
   begin
      return Kernel.Scripts;
   end Get_Scripts;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      CL      : Arg_List) return String
   is
      Errors : aliased Boolean;
   begin
      Trace (Me, "Executing " & To_Debug_String (CL));
      return Execute_Command
        (Lookup_Scripting_Language (Kernel.Scripts, GPS_Shell_Name),
         CL, null, True, True, Errors'Unchecked_Access);
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   procedure Execute_GPS_Shell_Command
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      CL     : Arg_List)
   is
      Output : constant String := Execute_Command_With_Args
        (Lookup_Scripting_Language (Kernel.Scripts, GPS_Shell_Name),
         CL);
      pragma Unreferenced (Output);
   begin
      if Active (Me) then
         Trace (Me, "Executing " & To_Display_String (CL));
      end if;
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
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Script).all).Kernel;
   end Get_Kernel;

   -------------------
   -- Create_Entity --
   -------------------

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Standard.Entities.Entity_Information) return Class_Instance
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

   function Get_Data
     (Data : Callback_Data'Class; N : Positive)
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
      Context : GPS.Kernel.Selection_Context) return Class_Instance
   is
      Initial_Ref : constant Integer := Context.Data.Data.Ref_Count;
      Instance    : Class_Instance;
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

      Instance := Get (Context.Data.Data.Instances.all, Script);

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

      if Active (Ref_Me) then
         Decrease_Indent (Ref_Me);
      end if;

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

   overriding procedure Destroy (Prop : in out GPS_Properties_Record) is
   begin
      case Prop.Typ is
         when Contexts =>
            --  Nothing to do either. The ref to the class instance that is
            --  hold by the context will be automatically freed when the
            --  context itself is destroyed.
            null;

         when Files | Projects =>
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

   overriding function Get_Instances
     (Prop : GPS_Properties_Record) return Instance_List_Access is
   begin
      case Prop.Typ is
         when Contexts =>
            if Prop.Context.Data.Data /= null then
               return Prop.Context.Data.Data.Instances;
            end if;

         when Files | Entities | Projects | File_Locations =>
            null;
      end case;

      return null;
   end Get_Instances;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Link : access Hyper_Link_Subprogram; Text : String)
   is
      Data   : Callback_Data'Class :=
                 Create (Get_Script (Link.Subprogram.all), 1);
      Ignore : Boolean;
      pragma Unreferenced (Ignore);
   begin
      Set_Nth_Arg (Data, 1, Text);
      Ignore := Execute (Link.Subprogram, Data);
      Free (Data);
   end On_Click;

end GPS.Kernel.Scripts;
