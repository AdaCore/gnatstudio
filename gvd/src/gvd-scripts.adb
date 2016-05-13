------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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

with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Debugger;                use Debugger;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with GPS.Debuggers;           use GPS.Debuggers;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Intl;                use GPS.Intl;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Types;
with GVD_Module;              use GVD_Module;

package body GVD.Scripts is

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
      Process    : Visual_Debugger;
      Inst       : Class_Instance;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, -("Cannot create instances of Debugger directly"
            & ASCII.LF
            & "Use GPS.Debugger.get() or GPS.Debugger.spawn() instead"));

      elsif Command = "get" then
         declare
            procedure Process_By_Id
              (Object : not null access Base_Visual_Debugger'Class);

            procedure Process_By_File
              (Object : not null access Base_Visual_Debugger'Class);

            Id   : Natural;
            File : Virtual_File;

            -------------------
            -- Process_By_Id --
            -------------------

            procedure Process_By_Id
              (Object : not null access Base_Visual_Debugger'Class)
            is
               Visual : constant Visual_Debugger := Visual_Debugger (Object);
            begin
               if Get_Num (Visual) = Gint (Id) then
                  Process := Visual;
               end if;
            end Process_By_Id;

            ---------------------
            -- Process_By_File --
            ---------------------

            procedure Process_By_File
              (Object : not null access Base_Visual_Debugger'Class)
            is
               Visual : constant Visual_Debugger := Visual_Debugger (Object);
            begin
               if Get_Executable (Visual.Debugger) = File then
                  Process := Visual;
               end if;
            end Process_By_File;

            File_Inst : Class_Instance;
         begin
            if Number_Of_Arguments (Data) = 0 then
               Process := Visual_Debugger (Get_Current_Debugger (Kernel));
            else
               Id := Nth_Arg (Data, 1);
               For_Each_Debugger (Kernel, Process_By_Id'Access);
            end if;

         exception
            when Invalid_Data =>
               --  We got pass a file as Id
               File_Inst := Nth_Arg
                 (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
               File := Get_Data (File_Inst);
               For_Each_Debugger (Kernel, Process_By_File'Access);
         end;

         if Process = null then
            Set_Error_Msg (Data, "No such debugger");
         else
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
         end if;

      elsif Command = "list" then
         declare
            procedure Callback
              (Object : not null access Base_Visual_Debugger'Class);

            --------------
            -- Callback --
            --------------

            procedure Callback
              (Object : not null access Base_Visual_Debugger'Class)
            is
               Process : constant Visual_Debugger := Visual_Debugger (Object);
            begin
               Set_Return_Value
                 (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
            end Callback;

         begin
            Set_Return_Value_As_List (Data);
            For_Each_Debugger (Kernel, Callback'Access);
         end;

      elsif Command = "send" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));

         if Nth_Arg (Data, 4, False) then
            Process_User_Command
              (Debugger       => Process,
               Command        => Nth_Arg (Data, 2),
               Output_Command => False,  --  Done by Visible parameter
               Mode           => GVD.Types.Visible);
         else
            Set_Return_Value
              (Data, Process_User_Command
                 (Debugger       => Process,
                  Command        => Nth_Arg (Data, 2),
                  Output_Command => Nth_Arg (Data, 3, True),
                  Mode           => GVD.Types.Hidden));
         end if;

      elsif Command = "non_blocking_send" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Process_User_Command
           (Process, Nth_Arg (Data, 2),
            Output_Command => Nth_Arg (Data, 3, True),
            Mode           => GVD.Types.User);

      elsif Command = "command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Get_Command (Process));

      elsif Command = "is_exec_command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Is_Execution_Command (Process));

      elsif Command = "is_context_command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         if Process.Current_Command /= null then
            Set_Return_Value
              (Data,
               Command_Kind
                 (Process.Debugger, Process.Current_Command.all)
               = Context_Command);
         else
            Set_Return_Value (Data, False);
         end if;

      elsif Command = "is_break_command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         if Process.Current_Command /= null then
            Data.Set_Return_Value
              (Breakpoints_Changed
                 (Process.Debugger, Process.Current_Command.all));
         else
            Data.Set_Return_Value (False);
         end if;

      elsif Command = "get_executable" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value
           (Create_File (Data.Get_Script, Get_Executable (Process.Debugger)));

      elsif Command = "get_num" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Integer (Get_Num (Process)));

      elsif Command = "is_busy" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Command_In_Process (Process));

      elsif Command = "close" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Close_Debugger (Process);

      elsif Command = "current_file" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value
           (Create_File
              (Data.Get_Script, Process.Editor_Text.Get_Current_File));

      elsif Command = "current_line" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Process.Editor_Text.Get_Line);

      elsif Command = "spawn" then
         declare
            File_Inst       : constant Class_Instance := Nth_Arg
              (Data, 1, Get_File_Class (Kernel));
            File            : constant Virtual_File := Get_Data (File_Inst);
            Remote_Target   : constant String := Nth_Arg (Data, 3, "");
            Remote_Protocol : constant String := Nth_Arg (Data, 4, "");
         begin
            Process := Spawn
              (Kernel          => Kernel,
               Kind            => Debugger_Kind.Get_Pref,
               File            => File,
               Project         => Get_Project (Kernel),
               Args            => Nth_Arg (Data, 2, ""),
               Remote_Target   => Remote_Target,
               Remote_Protocol => Remote_Protocol);
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
         end;
      end if;
   end Shell_Handler;

   ------------------
   -- Create_Hooks --
   ------------------

   procedure Create_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class : constant Class_Type := New_Class (Kernel, "Debugger");
   begin
      Kernel.Scripts.Register_Command
        (Constructor_Method,
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
        ("get",
         Params       => (1 => Param ("id", Optional => True)),
         Handler      => Shell_Handler'Access,
         Class        => Class,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("list",
         Handler      => Shell_Handler'Access,
         Class        => Class,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("send",
         Params =>
           (1 => Param ("cmd"),
            2 => Param ("output", Optional => True),
            3 => Param ("show_in_console", Optional => True)),
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
        ("get_executable",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("get_num",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("is_busy",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("close",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("spawn",
         Params =>
           (1 => Param ("executable"),
            2 => Param ("args", Optional => True),
            3 => Param ("remote_target", Optional => True),
            4 => Param ("remote_protocol", Optional => True)),
         Handler       => Shell_Handler'Access,
         Class         => Class,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("command",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("is_exec_command",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("is_context_command",
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("is_break_command",
         Handler      => Shell_Handler'Access,
         Class        => Class);
   end Create_Hooks;

end GVD.Scripts;
