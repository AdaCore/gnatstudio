------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GPS.Debuggers;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;

with DAP.Breakpoint_Maps;
with DAP.Clients;
with DAP.Module;

package body DAP.Scripts is

   type DAP_Proxy is
     new GPS.Debuggers.Base_Visual_Debugger with record
      Client : DAP.Clients.DAP_Client_Access;
   end record;
   type DAP_Proxy_Access is access all DAP_Proxy'Class;

   overriding function Get_Num
     (Self : not null access DAP_Proxy) return Glib.Gint
      is (Glib.Gint (Self.Client.Id));

   overriding function Command_In_Process
     (Self : not null access DAP_Proxy) return Boolean
      is (not Self.Client.Is_Stopped);

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

   procedure Info_Handler
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

   ------------------
   -- Info_Handler --
   ------------------

   procedure Info_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = Constructor_Method then
         Data.Set_Error_Msg
           ("Cannot construct instances of DebuggerBreakpoint");

      elsif Command = "num" then
         Data.Set_Return_Value
           (Integer (Get_Breakpoint (Data.Nth_Arg (1)).Num));
      end if;
   end Info_Handler;

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use type DAP.Clients.DAP_Client_Access;

      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
      Proxy  : aliased DAP_Proxy_Access;
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
            Proxy := new DAP_Proxy;
            Glib.Object.Initialize (Proxy);
            Proxy.Client := Client;
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Proxy));
         end if;

      elsif Command = "breakpoints" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Proxy := DAP_Proxy_Access
           (Glib.Object.GObject'(Get_Data (Inst)));
         Data.Set_Return_Value_As_List;
         for Vector of Proxy.Client.Get_Breakpoints.Sources loop
            for B of Vector loop
               Data.Set_Return_Value
                 (Create_Debugger_Breakpoint (Data.Get_Script, B));
            end loop;
         end loop;
         for B of Proxy.Client.Get_Breakpoints.Subprograms loop
            Data.Set_Return_Value
              (Create_Debugger_Breakpoint (Data.Get_Script, B));
         end loop;
      end if;
   end Shell_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class : constant Class_Type := New_Class (Kernel, "Debugger");
      Info  : constant Class_Type := New_Class
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
      Kernel.Scripts.Register_Property
        ("breakpoints",
         Getter       => Shell_Handler'Access,
         Class        => Class);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Handler      => Info_Handler'Access,
         Class        => Info);
      Kernel.Scripts.Register_Property
        ("num",
         Getter => Info_Handler'Access,
         Class => Info);
   end Register_Module;

end DAP.Scripts;
