------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Scripts.Gtkada;  use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with Glib;
with Glib.Object;

with VSS.Strings.Conversions;

with Basic_Types;

with GPS.Editors;              use GPS.Editors;
with GPS.Kernel.Project;
with GPS.Kernel.Scripts;       use GPS.Kernel.Scripts;

with DAP.Types;                use DAP.Types;
with DAP.Module;
with DAP.Types.Breakpoints;
with DAP.Module.Breakpoints;
with DAP.Modules.Variables.Items;

with DAP.Clients.Breakpoint_Managers;
with DAP.Clients.Stack_Trace;  use DAP.Clients.Stack_Trace;
with DAP.Clients.Variables;    use DAP.Clients.Variables;

with DAP.Views.Consoles;

with Interactive_Consoles;    use Interactive_Consoles;

package body DAP.Modules.Scripts is

   Me : constant Trace_Handle := Create ("DAP.Modules.Scripts", On);

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module

   -- Breakpoint_Info_Property --

   Debugger_Breakpoint_Class_Name    : constant String := "DebuggerBreakpoint";
   Debugger_Variable_Type_Class_Name : constant String := "DebuggerVariable";

   -- Breakpoint_Info_Property --

   type Breakpoint_Info_Property is new Instance_Property_Record with record
      Data : DAP.Types.Breakpoints.Breakpoint_Data;
   end record;
   function Create_Debugger_Breakpoint
     (Script : not null access Scripting_Language_Record'Class;
      Data   : DAP.Types.Breakpoints.Breakpoint_Data) return Class_Instance;

   procedure Breakpoint_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for Debugger_Breakpoint class

   function Get_Breakpoint
     (Inst : Class_Instance)
      return DAP.Types.Breakpoints.Breakpoint_Data;

   -- Debugger_Variable_Property --

   type Debugger_Variable_Property is new Instance_Property_Record with record
      Visual : DAP_Visual_Debugger_Access;
      Data   : Variable_Data;
   end record;

   function Create_Debugger_Variable
     (Script : not null access Scripting_Language_Record'Class;
      Visual : DAP_Visual_Debugger_Access;
      Data   : Variable_Data) return Class_Instance;

   procedure Variable_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handler for Debugger_Variable class

   function Get_Subprogram
     (Data     : in out Callback_Data'Class;
      Position : Integer)
      return Subprogram_Type;
   --  Return the subprogram parameter from the data/position if any or null.
   --  The caller side is responsible for freeing returned Subprogram.

   --------------------------------
   -- Create_Debugger_Breakpoint --
   --------------------------------

   function Create_Debugger_Breakpoint
     (Script : not null access Scripting_Language_Record'Class;
      Data   : DAP.Types.Breakpoints.Breakpoint_Data) return Class_Instance
   is
      Inst : constant Class_Instance := Script.New_Instance
        (Script.Get_Repository.New_Class (Debugger_Breakpoint_Class_Name));
   begin
      Set_Data
        (Inst, Debugger_Breakpoint_Class_Name,
         Breakpoint_Info_Property'(Data => Data));
      return Inst;
   end Create_Debugger_Breakpoint;

   ------------------------------
   -- Create_Debugger_Variable --
   ------------------------------

   function Create_Debugger_Variable
     (Script : not null access Scripting_Language_Record'Class;
      Visual : DAP_Visual_Debugger_Access;
      Data   : Variable_Data) return Class_Instance
   is
      Inst : constant Class_Instance := Script.New_Instance
        (Script.Get_Repository.New_Class (Debugger_Variable_Type_Class_Name));
   begin
      Set_Data
        (Inst, Debugger_Variable_Type_Class_Name,
         Debugger_Variable_Property'
           (Visual => Visual, Data => Data));

      return Inst;
   end Create_Debugger_Variable;

   -------------------------------------------
   -- Create_Debugger_Variable_For_Callback --
   -------------------------------------------

   procedure Create_Debugger_Variable_For_Callback
     (Callback : GNATCOLL.Scripts.Subprogram_Type;
      Client   : not null access DAP.Clients.DAP_Client'Class;
      Data     : Variable_Data) is
   begin
      if Callback = null then
         return;
      end if;

      declare
         Script    : constant Scripting_Language := Callback.Get_Script;
         Inst      : constant Class_Instance := Create_Debugger_Variable
           (Script, Client.Get_Visual, Data);
         Arguments : Callback_Data'Class := Script.Create (1);
         Dummy     : Boolean;
      begin
         Set_Nth_Arg (Arguments, 1, Inst);
         Dummy := Callback.Execute (Arguments);
         Free (Arguments);

      exception
         when E : others =>
            Free (Arguments);
            Trace (Me, E);
      end;
   end Create_Debugger_Variable_For_Callback;

   --------------------------------------------
   -- Create_Debugger_Variables_For_Callback --
   --------------------------------------------

   procedure Create_Debugger_Variables_For_Callback
     (Callback : GNATCOLL.Scripts.Subprogram_Type;
      Client   : not null access DAP.Clients.DAP_Client'Class;
      Data     : Variable_Data_Vector.Vector) is
   begin
      if Callback = null then
         return;
      end if;

      declare
         Script : constant Scripting_Language := Callback.Get_Script;
         List   : List_Instance'Class := New_List (Script);
         Pos    : Integer := 1;
      begin
         for D of Data loop
            declare
               Inst : constant Class_Instance :=
                 Create_Debugger_Variable (Script, Client.Get_Visual, D);
            begin
               List.Set_Nth_Arg (Pos, Inst);
               Pos := Pos + 1;
            end;
         end loop;

         declare
            Arguments : Callback_Data'Class := Script.Create (1);
            Dummy     : Boolean;
         begin
            Set_Nth_Arg (Arguments, 1, List);
            Dummy := Callback.Execute (Arguments);
            Free (Arguments);
            Free (List);

         exception
            when E : others =>
               Free (Arguments);
               Free (List);
               Trace (Me, E);
         end;
      end;
   end Create_Debugger_Variables_For_Callback;

   ----------------------------------------------
   -- Create_No_Debugger_Variable_For_Callback --
   ----------------------------------------------

   procedure Create_No_Debugger_Variable_For_Callback
     (Params : DAP.Clients.Variables.Request_Parameters) is
   begin
      --  We did not found the variable for some reason

      if Params.On_Result = null
        and then Params.On_Error = null
      then
         return;
      end if;

      declare
         Dummy : Boolean;
      begin
         if Params.On_Error = null then
            --  We do not have On_Error callback, use On_Result and
            --  send None object.
            declare
               Arguments : Callback_Data'Class :=
                 Params.On_Result.Get_Script.Create (1);
            begin
               Set_Nth_Arg (Arguments, 1, No_Class_Instance);
               Dummy := Params.On_Result.Execute (Arguments);
               Free (Arguments);
            exception
               when E : others =>
                  Free (Arguments);
                  Trace (Me, E);
            end;

         else
            --  We have On_Error callback, call it with "not found" message
            declare
               Arguments : Callback_Data'Class :=
                 Params.On_Error.Get_Script.Create (1);
            begin
               Set_Nth_Arg (Arguments, 1, String'("Not found."));
               Dummy := Params.On_Error.Execute (Arguments);
               Free (Arguments);

            exception
               when E : others =>
                  Free (Arguments);
                  Trace (Me, E);
            end;
         end if;
      end;
   end Create_No_Debugger_Variable_For_Callback;

   --------------------
   -- Get_Breakpoint --
   --------------------

   function Get_Breakpoint
     (Inst : Class_Instance)
      return DAP.Types.Breakpoints.Breakpoint_Data
   is
      Data : constant Instance_Property :=
        Get_Data (Inst, Debugger_Breakpoint_Class_Name);
   begin
      return Breakpoint_Info_Property (Data.all).Data;
   end Get_Breakpoint;

   --------------------
   -- Get_Subprogram --
   --------------------

   function Get_Subprogram
     (Data     : in out Callback_Data'Class;
      Position : Integer)
      return Subprogram_Type is
   begin
      return Nth_Arg (Data, Position);

   exception
      when No_Such_Parameter =>
         return null;
   end Get_Subprogram;

   ------------------------
   -- Breakpoint_Handler --
   ------------------------

   procedure Breakpoint_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = Constructor_Method then
         Data.Set_Error_Msg
           ("Cannot construct instances of DebuggerBreakpoint");

      elsif Command = "num" then
         Data.Set_Return_Value
           (Integer (Get_Breakpoint (Data.Nth_Arg (1)).Num));

      elsif Command = "file" then
         Data.Set_Return_Value
           (Create_File
              (Data.Get_Script,
               Get_File
                 (DAP.Types.Breakpoints.Get_Location
                      (Get_Breakpoint (Data.Nth_Arg (1))))));

      elsif Command = "line" then
         Data.Set_Return_Value
           (Natural
              (Get_Line
                   (DAP.Types.Breakpoints.Get_Location
                        (Get_Breakpoint (Data.Nth_Arg (1))))));
      end if;
   end Breakpoint_Handler;

   ----------------------
   -- Variable_Handler --
   ----------------------

   procedure Variable_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      function Get (Inst : Class_Instance) return Variable_Data;
      function Visual
        (Inst : Class_Instance) return DAP_Visual_Debugger_Access;

      function Get (Inst : Class_Instance) return Variable_Data
      is
         Data : constant Instance_Property :=
           Get_Data (Inst, Debugger_Variable_Type_Class_Name);
      begin
         return Debugger_Variable_Property (Data.all).Data;
      end Get;

      function Visual
        (Inst : Class_Instance) return DAP_Visual_Debugger_Access
      is
         Data : constant Instance_Property :=
           Get_Data (Inst, Debugger_Variable_Type_Class_Name);
      begin
         return Debugger_Variable_Property (Data.all).Visual;
      end Visual;

   begin
      if Command = Constructor_Method then
         Data.Set_Error_Msg
           ("Cannot construct instances of DebuggerVariable");

      elsif Command = "simple_value" then
         Data.Set_Return_Value
           (VSS.Strings.Conversions.To_UTF_8_String
              (Get (Data.Nth_Arg (1)).Data.value));

      elsif Command = "type_description" then
         Data.Set_Return_Value (String'(""));

      elsif Command = "type_name" then
         Data.Set_Return_Value
           (VSS.Strings.Conversions.To_UTF_8_String
              (Get (Data.Nth_Arg (1)).Data.a_type));

      elsif Command = "children" then
         declare
            Holder : DAP.Modules.Variables.Items.Item_Holder;
         begin
            DAP.Modules.Variables.Items.Set
              (Holder, DAP.Modules.Variables.Items.Create
                 (Variable => Get (Data.Nth_Arg (1)).Full_Name));

            declare
               Params : Request_Parameters :=
                 (Kind        => Python_API,
                  Item        => Holder,
                  Children    => True,
                  On_Result   => Get_Subprogram (Data, 2),
                  On_Error    => Get_Subprogram (Data, 3),
                  On_Rejected => Get_Subprogram (Data, 4));
            begin
               Visual (Data.Nth_Arg (1)).Client.
                 Get_Variables.Get_Variable (Params);
            end;
         end;
      end if;
   end Variable_Handler;

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
      Visual : aliased DAP_Visual_Debugger_Access;
      Client : DAP.Clients.DAP_Client_Access;
      Inst   : Class_Instance;

   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, "Cannot create instances of Debugger directly"
            & ASCII.LF
            & "Use GPS.Debugger.get() or GPS.Debugger.spawn() instead");

      elsif Command = "get" then
         declare
            procedure Process_By_Id (Dbg : DAP.Clients.DAP_Client_Access);
            procedure Process_By_File (Dbg : DAP.Clients.DAP_Client_Access);

            Id   : Natural;
            File : Virtual_File;

            -------------------
            -- Process_By_Id --
            -------------------

            procedure Process_By_Id (Dbg : DAP.Clients.DAP_Client_Access) is
            begin
               if Dbg.Id = Id then
                  Client := Dbg;
               end if;
            end Process_By_Id;

            ---------------------
            -- Process_By_File --
            ---------------------

            procedure Process_By_File (Dbg : DAP.Clients.DAP_Client_Access)
            is
            begin
               if Dbg.Get_Executable = File then
                  Client := Dbg;
               end if;
            end Process_By_File;

            File_Inst : Class_Instance;

         begin
            if Number_Of_Arguments (Data) = 0 then
               Client := DAP.Module.Get_Current_Debugger;
            else
               Id := Nth_Arg (Data, 1);
               DAP.Module.For_Each_Debugger (Process_By_Id'Access);
            end if;

         exception
            when Invalid_Data =>
               --  We got pass a file as Id
               File_Inst := Nth_Arg
                 (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
               File := Get_Data (File_Inst);
               DAP.Module.For_Each_Debugger (Process_By_File'Access);
         end;

         if Client = null then
            Set_Error_Msg (Data, "No such debugger");
         else
            Set_Return_Value
              (Data, Get_Or_Create_Instance
                 (Get_Script (Data), Client.Get_Visual));
         end if;

      elsif Command = "spawn" then
         declare
            File_Inst       : constant Class_Instance := Nth_Arg
              (Data, 1, Get_File_Class (Kernel));
            File            : constant Virtual_File := Get_Data (File_Inst);
            Remote_Target   : constant String := Nth_Arg (Data, 3, "");
            --  Remote_Protocol : constant String := Nth_Arg (Data, 4, "");
            --  Load_Executable : constant Boolean := Nth_Arg (Data, 5, False);
         begin
            Visual := DAP.Module.Initialize_Debugger
              (Kernel          => Kernel,
               Project         => GPS.Kernel.Project.Get_Project (Kernel),
               File            => File,
               Executable_Args => Nth_Arg (Data, 2, ""),
               Remote_Target   => Remote_Target).Get_Visual;

            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Visual));
         end;

      elsif Command = "is_busy" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Command_In_Process (Visual));

      elsif Command = "breakpoints" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value_As_List;
         for B of Visual.Client.Get_Breakpoints_Manager.Get_Breakpoints loop
            Data.Set_Return_Value
              (Create_Debugger_Breakpoint (Data.Get_Script, B));
         end loop;

      elsif Command = "start" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));

         if Visual.Client.Is_Ready then
            DAP.Module.Start_Executable
              (Kernel               => Kernel,
               Client               => Visual.Client,
               Display_Start_Dialog => False);
         else
            Data.Set_Error_Msg
              ("Can't start the executable: the debugger is"
               & " not in 'ready' state (current state: "
               & Visual.Client.Get_Status'Img);
         end if;

      elsif Command = "send" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));

         Visual.Client.Process_User_Command
           (Cmd               => Nth_Arg (Data, 2),
            Output_Command    => Nth_Arg (Data, 3, True),
            Result_In_Console => Nth_Arg (Data, 4, False),
            On_Result_Message => Get_Subprogram (Data, 5),
            On_Error_Message  => Get_Subprogram (Data, 6),
            On_Rejected       => Get_Subprogram (Data, 7));

      elsif Command = "non_blocking_send" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Process_User_Command
           (Nth_Arg (Data, 2), Nth_Arg (Data, 3, True));

      elsif Command = "close" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Quit;

      elsif Command = "current_file" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value
           (Create_File
              (Data.Get_Script, Visual.Current_File));

      elsif Command = "current_line" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Visual.Current_Line);

      elsif Command = "break_at_location" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         DAP.Module.Breakpoints.Break_Source
           (Kernel => Kernel,
            File   => Nth_Arg (Data, 2),
            Line   => Basic_Types.Editable_Line_Type
              (Integer'(Data.Nth_Arg (3))));

      elsif Command = "break_at_exception" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         DAP.Module.Breakpoints.Break_On_All_Exceptions
           (Kernel    => Kernel,
            Unhandled => Nth_Arg (Data, 2));

      elsif Command = "unbreak_at_location" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         DAP.Module.Breakpoints.Unbreak_Source
           (Kernel,
            File  => Nth_Arg (Data, 2),
            Line  => Basic_Types.Editable_Line_Type
              (Integer'(Data.Nth_Arg (3))));

      elsif Command = "get_executable" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value
           (Create_File (Data.Get_Script, Visual.Client.Get_Executable));

      elsif Command = "frames" then
         declare
            Bt : DAP.Types.Frames_Vectors.Vector;
         begin
            Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
            Visual := DAP_Visual_Debugger_Access
              (Glib.Object.GObject'(Get_Data (Inst)));
            Bt := Visual.Client.Get_Stack_Trace.Get_Trace;

            Data.Set_Return_Value_As_List;
            for Frame of Bt loop
               declare
                  List   : List_Instance'Class := New_List (Get_Script (Data));
                  Params : constant List_Instance'Class :=
                    New_List (Get_Script (Data));
                  Empty  : constant String := "<>";
               begin
                  Set_Nth_Arg (List, 1, Frame.Id);

                  if Frame.Address /= Invalid_Address then
                     Set_Nth_Arg
                       (List, 2, +(Address_To_String (Frame.Address)));
                  else
                     Set_Nth_Arg (List, 2, Empty);
                  end if;

                  if Frame.Name /= Null_Unbounded_String then
                     Set_Nth_Arg (List, 3, To_String (Frame.Name));
                  else
                     Set_Nth_Arg (List, 3, Empty);
                  end if;

                  if Frame.File /= No_File then
                     Set_Nth_Arg (List, 4,
                       (Create_File_Location
                          (Script => Get_Script (Data),
                           File   => Frame.File,
                           Line   => Frame.Line,
                           Column => 0)));
                  else
                     Set_Nth_Arg (List, 4, Empty);
                  end if;

                  Set_Nth_Arg (List, 5, Params);

                  Data.Set_Return_Value (List);
               end;
            end loop;
         end;

      elsif Command = "current_frame" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value
           (Visual.Client.Get_Stack_Trace.Get_Current_Frame_Id);

      elsif Command = "frame_up" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Get_Stack_Trace.Frame_Up (Visual.Client);

      elsif Command = "frame_down" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Get_Stack_Trace.Frame_Down (Visual.Client);

      elsif Command = "select_frame" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Get_Stack_Trace.Select_Frame
           (Nth_Arg (Data, 2, 0), Visual.Client);

      elsif Command = "get_console" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));

         declare
            Console : constant Interactive_Console :=
              DAP.Views.Consoles.Get_Debugger_Interactive_Console
                (Visual.Client.all);

         begin
            if Console /= null then
               Data.Set_Return_Value
                 (Get_Or_Create_Instance (Data.Get_Script, Console));
            end if;
         end;

      elsif Command = "interrupt" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Interrupt;

      elsif Command = "get_variable_by_name" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));

         declare
            Name   : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (String'(Nth_Arg (Data, 2)));
            Holder : DAP.Modules.Variables.Items.Item_Holder;

         begin
            DAP.Modules.Variables.Items.Set
              (Holder, DAP.Modules.Variables.Items.Create
                 (Variable => Name));
            declare
               Params : Request_Parameters :=
                 (Kind        => Python_API,
                  Item        => Holder,
                  Children    => False,
                  On_Result   => Get_Subprogram (Data, 3),
                  On_Error    => Get_Subprogram (Data, 4),
                  On_Rejected => Get_Subprogram (Data, 5));

            begin
               Visual.Client.Get_Variables.Get_Variable (Params);
            end;
         end;
      end if;
   end Shell_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class    : constant Class_Type := New_Class (Kernel, "Debugger");
      Bp       : constant Class_Type := New_Class
        (Kernel, Debugger_Breakpoint_Class_Name);
      Variable : constant Class_Type := New_Class
        (Kernel, Debugger_Variable_Type_Class_Name);

   begin
      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("get",
         Params       => (1 => Param ("id", Optional => True)),
         Handler      => Shell_Handler'Access,
         Class        => Class,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("spawn",
         Params =>
           (1 => Param ("executable"),
            2 => Param ("args", Optional => True),
            3 => Param ("remote_target", Optional => True),
            4 => Param ("remote_protocol", Optional => True),
            5 => Param ("load_executable", Optional => True)),
         Handler       => Shell_Handler'Access,
         Class         => Class,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("is_busy",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Property
        ("breakpoints",
         Getter       => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("start",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("send",
         Params =>
           (1 => Param ("cmd"),
            2 => Param ("output", Optional => True),
            3 => Param ("show_in_console", Optional => True),
            4 => Param ("on_result_message", Optional => True),
            5 => Param ("on_error_message", Optional => True),
            6 => Param ("on_rejected", Optional => True)),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("non_blocking_send",
         Params =>
           (1 => Param ("cmd"),
            2 => Param ("output", Optional => True)),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("close",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Property
        ("current_file",
         Class        => Class,
         Getter       => Shell_Handler'Access);
      Kernel.Scripts.Register_Property
        ("current_line",
         Class        => Class,
         Getter       => Shell_Handler'Access);
      Kernel.Scripts.Register_Command
        ("break_at_location",
         Params       => (1 => Param ("file"),
                          2 => Param ("line")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("break_at_exception",
         Params       => (1 => Param ("unhandled")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("unbreak_at_location",
         Params       => (1 => Param ("file"),
                          2 => Param ("line")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("get_executable",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("frames",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("current_frame",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("frame_up",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("frame_down",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("select_frame",
         Params       => (1 => Param ("num")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("get_console",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("interrupt",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("get_variable_by_name",
         Handler      => Shell_Handler'Access,
         Params       =>
           (1 => Param ("name"),
            2 => Param ("on_result", Optional => True),
            3 => Param ("on_error", Optional => True),
            4 => Param ("on_rejected", Optional => True)),
         Class        => Class);

      --  Breakpoint --

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Handler      => Breakpoint_Handler'Access,
         Class        => Bp);
      Kernel.Scripts.Register_Property
        ("num",
         Getter => Breakpoint_Handler'Access,
         Class  => Bp);
      Kernel.Scripts.Register_Property
        ("file",
         Getter => Breakpoint_Handler'Access,
         Class  => Bp);
      Kernel.Scripts.Register_Property
        ("line",
         Getter => Breakpoint_Handler'Access,
         Class  => Bp);

      -- DebuggerVariable --

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Handler      => Variable_Handler'Access,
         Class        => Variable);

      Kernel.Scripts.Register_Property
        ("simple_value",
         Getter => Variable_Handler'Access, Class => Variable);
      Kernel.Scripts.Register_Property
        ("type_description",
         Getter => Variable_Handler'Access, Class => Variable);
      Kernel.Scripts.Register_Property
        ("type_name",
         Getter => Variable_Handler'Access, Class => Variable);
      Kernel.Scripts.Register_Command
        ("children",
         Handler => Variable_Handler'Access,
         Params       =>
           (1 => Param ("on_result", Optional => True),
            2 => Param ("on_error", Optional => True),
            3 => Param ("on_rejected", Optional => True)),
         Class   => Variable);
   end Register_Module;

end DAP.Modules.Scripts;
