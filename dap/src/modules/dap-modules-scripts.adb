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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Glib;
with Glib.Object;

with GPS.Editors;             use GPS.Editors;
with GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;

with DAP.Breakpoint_Maps;
with DAP.Clients;             use DAP.Clients;
with DAP.Module;

package body DAP.Modules.Scripts is

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module

   -- Breakpoint_Info_Property --

   Debugger_Breakpoint_Class_Name : constant String := "DebuggerBreakpoint";

   type Breakpoint_Info_Property is new Instance_Property_Record with record
      Data : DAP.Breakpoint_Maps.Breakpoint_Data;
   end record;
   function Create_Debugger_Breakpoint
     (Script : not null access Scripting_Language_Record'Class;
      Data   : DAP.Breakpoint_Maps.Breakpoint_Data) return Class_Instance;

   function Get_Breakpoint
     (Inst : Class_Instance)
      return DAP.Breakpoint_Maps.Breakpoint_Data;

   procedure Breakpoint_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hander for Debugger_Breakpoint class

   --------------------------------
   -- Create_Debugger_Breakpoint --
   --------------------------------

   function Create_Debugger_Breakpoint
     (Script : not null access Scripting_Language_Record'Class;
      Data   : DAP.Breakpoint_Maps.Breakpoint_Data) return Class_Instance
   is
      Inst : constant Class_Instance := Script.New_Instance
        (Script.Get_Repository.New_Class (Debugger_Breakpoint_Class_Name));
   begin
      Set_Data
        (Inst, Debugger_Breakpoint_Class_Name,
         Breakpoint_Info_Property'(Data => Data));
      return Inst;
   end Create_Debugger_Breakpoint;

   --------------------
   -- Get_Breakpoint --
   --------------------

   function Get_Breakpoint
     (Inst : Class_Instance)
      return DAP.Breakpoint_Maps.Breakpoint_Data
   is
      Data : constant Instance_Property :=
        Get_Data (Inst, Debugger_Breakpoint_Class_Name);
   begin
      return Breakpoint_Info_Property (Data.all).Data;
   end Get_Breakpoint;

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
                 (DAP.Breakpoint_Maps.Get_Location
                      (Get_Breakpoint (Data.Nth_Arg (1))))));

      elsif Command = "line" then
         Data.Set_Return_Value
           (Natural
              (Get_Line
                   (DAP.Breakpoint_Maps.Get_Location
                        (Get_Breakpoint (Data.Nth_Arg (1))))));
      end if;
   end Breakpoint_Handler;

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
            --  Remote_Target   : constant String := Nth_Arg (Data, 3, "");
            --  Remote_Protocol : constant String := Nth_Arg (Data, 4, "");
            --  Load_Executable : constant Boolean := Nth_Arg (Data, 5, False);
         begin
            Visual := DAP.Module.Initialize_Debugger
              (Kernel  => Kernel,
               Project => GPS.Kernel.Project.Get_Project (Kernel),
               File    => File,
               Args    => Nth_Arg (Data, 2, "")).Get_Visual;

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
         for B of Visual.Client.Get_Breakpoints loop
            Data.Set_Return_Value
              (Create_Debugger_Breakpoint (Data.Get_Script, B));
         end loop;

      elsif Command = "start" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         DAP.Module.Start_Program (Kernel, Visual.Client);

      elsif Command = "close" then
         Inst   := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Visual := DAP_Visual_Debugger_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Visual.Client.Quit;
      end if;
   end Shell_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class : constant Class_Type := New_Class (Kernel, "Debugger");
      Bp    : constant Class_Type := New_Class
        (Kernel, Debugger_Breakpoint_Class_Name);

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
        ("close",
         Handler      => Shell_Handler'Access,
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

   end Register_Module;

end DAP.Modules.Scripts;