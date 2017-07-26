------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Gtk.Widget;              use Gtk.Widget;

with Basic_Types;             use Basic_Types;
with Debugger;                use Debugger;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with GPS.Debuggers;           use GPS.Debuggers;
with GPS.Editors;             use GPS.Editors;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Intl;                use GPS.Intl;
with GVD.Breakpoints_List;    use GVD.Breakpoints_List;
with GVD.Process;             use GVD.Process;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Types;               use GVD.Types;
with GVD_Module;              use GVD_Module;
with Interactive_Consoles;    use Interactive_Consoles;
with GVD.Consoles;            use GVD.Consoles;

package body GVD.Scripts is

   Debugger_Breakpoint_Class_Name : constant String := "DebuggerBreakpoint";

   type Breakpoint_Info_Property is new Instance_Property_Record with record
      Data : Breakpoint_Data;
   end record;
   function Create_Debugger_Breakpoint
     (Script : not null access Scripting_Language_Record'Class;
      Data   : Breakpoint_Data) return Class_Instance;
   function Get_Breakpoint (Inst : Class_Instance) return Breakpoint_Data;
   --  Class instances for a Debugger_Breakpoint

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module

   procedure Info_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hander for Debugger_Breakpoint class

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

      elsif Command = "type" then
         case Get_Breakpoint (Data.Nth_Arg (1)).The_Type is
            when Breakpoint =>
               Data.Set_Return_Value (String'("breakpoint"));
            when Watchpoint =>
               Data.Set_Return_Value (String'("watchpoint"));
         end case;

      elsif Command = "enabled" then
         Data.Set_Return_Value (Get_Breakpoint (Data.Nth_Arg (1)).Enabled);

      elsif Command = "watched" then
         Data.Set_Return_Value
           (To_String (Get_Breakpoint (Data.Nth_Arg (1)).Expression));

      elsif Command = "file" then
         Data.Set_Return_Value
           (Create_File
              (Data.Get_Script,
               Get_File (Get_Breakpoint (Data.Nth_Arg (1)).Location)));

      elsif Command = "line" then
         Data.Set_Return_Value
           (Natural
              (Get_Line (Get_Breakpoint (Data.Nth_Arg (1)).Location)));
      end if;
   end Info_Handler;

   --------------------------------
   -- Create_Debugger_Breakpoint --
   --------------------------------

   function Create_Debugger_Breakpoint
     (Script : not null access Scripting_Language_Record'Class;
      Data   : Breakpoint_Data) return Class_Instance
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

   function Get_Breakpoint (Inst : Class_Instance) return Breakpoint_Data is
      Data : constant Instance_Property :=
        Get_Data (Inst, Debugger_Breakpoint_Class_Name);
   begin
      return Breakpoint_Info_Property (Data.all).Data;
   end Get_Breakpoint;

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

      elsif Command = "value_of" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         --   ??? Should return None if variable is undefined
         Data.Set_Return_Value
           (Process.Debugger.Value_Of (Entity => Data.Nth_Arg (2)));

      elsif Command = "set_variable" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Process.Debugger.Set_Variable
           (Var_Name => Data.Nth_Arg (2),
            Value    => Data.Nth_Arg (3));

      elsif Command = "break_at_location" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Break_Source
           (Kernel => Kernel,
            File  => Nth_Arg (Data, 2),
            Line   => Editable_Line_Type (Integer'(Data.Nth_Arg (3))));

      elsif Command = "unbreak_at_location" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Unbreak_Source
           (Kernel,
            File  => Nth_Arg (Data, 2),
            Line  => Editable_Line_Type (Integer'(Data.Nth_Arg (3))));

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

      elsif Command = "remote_target" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Process.Debugger.Get_Remote_Target);

      elsif Command = "remote_protocol" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Process.Debugger.Get_Remote_Protocol);

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
              (Data.Get_Script, Process.Current_File));

      elsif Command = "current_line" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Process.Current_Line);

      elsif Command = "get_console" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));

         declare
            Console : constant Interactive_Console :=
                        Get_Debugger_Interactive_Console (Process);
         begin
            if Console /= null then
               Data.Set_Return_Value
                 (Get_Or_Create_Instance (Data.Get_Script, Console));
            end if;
         end;

      elsif Command = "spawn" then
         declare
            File_Inst       : constant Class_Instance := Nth_Arg
              (Data, 1, Get_File_Class (Kernel));
            File            : constant Virtual_File := Get_Data (File_Inst);
            Remote_Target   : constant String := Nth_Arg (Data, 3, "");
            Remote_Protocol : constant String := Nth_Arg (Data, 4, "");
            Load_Executable : constant Boolean := Nth_Arg (Data, 5, False);
         begin
            Process := Spawn
              (Kernel          => Kernel,
               Kind            => Debugger_Kind.Get_Pref,
               File            => File,
               Project         => Get_Project (Kernel),
               Args            => Nth_Arg (Data, 2, ""),
               Remote_Target   => Remote_Target,
               Remote_Protocol => Remote_Protocol,
               Load_Executable => Load_Executable);
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
         end;

      elsif Command = "breakpoints" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value_As_List;
         for B of Get_Stored_List_Of_Breakpoints (Process).List loop
            Data.Set_Return_Value
              (Create_Debugger_Breakpoint (Data.Get_Script, B));
         end loop;

      elsif Command = "frames" then
         declare
            Bt : Backtrace_Vector;
         begin
            Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
            Process := Visual_Debugger (GObject'(Get_Data (Inst)));
            Process.Debugger.Backtrace (Bt);

            Data.Set_Return_Value_As_List;
            for Frame of Bt loop
               declare
                  List   : List_Instance'Class := New_List (Get_Script (Data));
                  Params : List_Instance'Class := New_List (Get_Script (Data));
                  Empty  : constant String := "<>";
                  Idx    : Natural := 1;
               begin
                  Set_Nth_Arg (List, 1, Frame.Frame_Id);

                  if Frame.Address /= Invalid_Address then
                     Set_Nth_Arg
                       (List, 2, +(Address_To_String (Frame.Address)));
                  else
                     Set_Nth_Arg (List, 2, Empty);
                  end if;

                  if Frame.Subprogram /= null then
                     Set_Nth_Arg (List, 3, Frame.Subprogram.all);
                  else
                     Set_Nth_Arg (List, 3, Empty);
                  end if;

                  if Frame.File /= No_File then
                     Set_Nth_Arg (List, 4,
                       (Create_File_Location
                          (Script => Get_Script (Data),
                           File   => Create_File
                             (Get_Script (Data), Frame.File),
                           Line   => Frame.Line,
                           Column => 0)));
                  else
                     Set_Nth_Arg (List, 4, Empty);
                  end if;

                  for Param of Frame.Parameters loop
                     Set_Nth_Arg (Params, Idx, Param.Value.all);
                     Idx := Idx + 1;
                  end loop;

                  Set_Nth_Arg (List, 5, Params);

                  Data.Set_Return_Value (List);
               end;
            end loop;
         end;

      elsif Command = "current_frame" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Data.Set_Return_Value (Process.Debugger.Current_Frame);

      elsif Command = "frame_up" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Process.Debugger.Stack_Up;

      elsif Command = "frame_down" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Process.Debugger.Stack_Down;

      elsif Command = "select_frame" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Process.Debugger.Stack_Frame (Nth_Arg (Data, 2, 0) + 1);
      end if;
   end Shell_Handler;

   ------------------
   -- Create_Hooks --
   ------------------

   procedure Create_Hooks
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
      Kernel.Scripts.Register_Property
        ("remote_target",
         Getter       => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Property
        ("remote_protocol",
         Getter       => Shell_Handler'Access,
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
        ("get_console",
         Handler      => Shell_Handler'Access,
         Class        => Class);
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
      Kernel.Scripts.Register_Command
        ("value_of",
         Params       => (1 => Param ("expression")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("set_variable",
         Params       => (1 => Param ("variable"),
                          2 => Param ("value")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("break_at_location",
         Params       => (1 => Param ("file"),
                          2 => Param ("line")),
         Handler      => Shell_Handler'Access,
         Class        => Class);
      Kernel.Scripts.Register_Command
        ("unbreak_at_location",
         Params       => (1 => Param ("file"),
                          2 => Param ("line")),
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

      Kernel.Scripts.Register_Property
        ("breakpoints",
         Getter       => Shell_Handler'Access,
         Class        => Class);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Handler      => Info_Handler'Access,
         Class        => Info);
      Kernel.Scripts.Register_Property
        ("num", Getter => Info_Handler'Access, Class => Info);
      Kernel.Scripts.Register_Property
        ("type", Getter => Info_Handler'Access, Class => Info);
      Kernel.Scripts.Register_Property
        ("enabled", Getter => Info_Handler'Access, Class => Info);
      Kernel.Scripts.Register_Property
        ("watched", Getter => Info_Handler'Access, Class => Info);
      Kernel.Scripts.Register_Property
        ("file", Getter => Info_Handler'Access, Class => Info);
      Kernel.Scripts.Register_Property
        ("line", Getter => Info_Handler'Access, Class => Info);
   end Create_Hooks;

end GVD.Scripts;
