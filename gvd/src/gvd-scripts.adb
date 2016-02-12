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
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Intl;                use GPS.Intl;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Types;
with GVD_Module;              use GVD_Module;

package body GVD.Scripts is

   Show_In_Console_Cst : aliased constant String := "show_in_console";

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
      Arg_Cmd    : aliased constant String := "cmd";
      Arg_ID     : aliased constant String := "id";
      Arg_Exec   : aliased constant String := "executable";
      Arg_Args   : aliased constant String := "args";
      Arg_Output : aliased constant String := "output";
      Process    : Visual_Debugger;
      Inst       : Class_Instance;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, -("Cannot create instances of Debugger directly"
            & ASCII.LF
            & "Use GPS.Debugger.get() or GPS.Debugger.spawn() instead"));

      elsif Command = "get" then
         Name_Parameters (Data, (1 => Arg_ID'Unchecked_Access));
         declare
            Id   : Natural;
            File_Inst : Class_Instance;
            File : Virtual_File;
            List : Debugger_List_Link := Get_Debugger_List (Kernel);
         begin
            if Number_Of_Arguments (Data) = 0 then
               Process := Visual_Debugger (Get_Current_Debugger (Kernel));

            else
               Id := Nth_Arg (Data, 1);
               while List /= null loop
                  Process := Visual_Debugger (List.Debugger);
                  exit when Get_Num (Process) = Gint (Id);
                  Process := null;
                  List := List.Next;
               end loop;
            end if;

         exception
            when Invalid_Data =>
               --  We got pass a file as Id
               File_Inst := Nth_Arg
                 (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
               File := Get_Data (File_Inst);

               while List /= null loop
                  Process := Visual_Debugger (List.Debugger);
                  exit when Get_Executable (Process.Debugger) = File;
                  Process := null;
                  List := List.Next;
               end loop;
         end;

         if Process = null then
            Set_Error_Msg (Data, "No such debugger");
         else
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
         end if;

      elsif Command = "list" then
         declare
            List : Debugger_List_Link := Get_Debugger_List (Kernel);
         begin
            Set_Return_Value_As_List (Data);
            while List /= null loop
               Process := Visual_Debugger (List.Debugger);
               Set_Return_Value
                 (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
               List := List.Next;
            end loop;
         end;

      elsif Command = "send" then
         Name_Parameters
           (Data,
            (1 => Arg_Cmd'Unchecked_Access,
             2 => Arg_Output'Unchecked_Access,
             3 => Show_In_Console_Cst'Access));
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
         Name_Parameters
           (Data,
            (1 => Arg_Cmd'Unchecked_Access,
             2 => Arg_Output'Unchecked_Access));
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
            Set_Return_Value
              (Data,
               Breakpoints_Changed
                 (Process.Debugger, Process.Current_Command.all));
         else
            Set_Return_Value (Data, False);
         end if;

      elsif Command = "get_executable" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value
           (Data, Create_File
              (Get_Script (Data), Get_Executable (Process.Debugger)));

      elsif Command = "get_num" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Integer (Get_Num (Process)));

      elsif Command = "is_busy" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Command_In_Process (Process));

      elsif Command = "close" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Close_Debugger (Process);

      elsif Command = "spawn" then
         Name_Parameters (Data, (1 => Arg_Exec'Unchecked_Access,
                                 2 => Arg_Args'Unchecked_Access));
         declare
            File_Inst : constant Class_Instance := Nth_Arg
              (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
            File : constant Virtual_File := Get_Data (File_Inst);
         begin
            Process := Spawn
              (Kernel => Kernel,
               Kind   => Debugger_Kind.Get_Pref,
               File   => File,
               Project => Get_Project (Kernel),
               Args    => Nth_Arg (Data, 2, ""));
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
      Register_Command
        (Kernel, Constructor_Method, 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "get", 0, 1, Shell_Handler'Access, Class,
         Static_Method => True);
      Register_Command
        (Kernel, "list", 0, 0, Shell_Handler'Access, Class,
         Static_Method => True);
      Register_Command (Kernel, "send", 1, 3, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "non_blocking_send", 1, 1, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "get_executable", 0, 0, Shell_Handler'Access, Class);
      Register_Command (Kernel, "get_num", 0, 0, Shell_Handler'Access, Class);
      Register_Command (Kernel, "is_busy", 0, 0, Shell_Handler'Access, Class);
      Register_Command (Kernel, "close", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "spawn", 1, 2, Shell_Handler'Access, Class,
         Static_Method => True);
      Register_Command
        (Kernel, "command", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "is_exec_command", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "is_context_command", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "is_break_command", 0, 0, Shell_Handler'Access, Class);
   end Create_Hooks;

end GVD.Scripts;
