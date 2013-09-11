------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

with Glib.Object;               use Glib.Object;
with Gtk.Widget;                use Gtk.Widget;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Task_Manager.Shell;        use Task_Manager.Shell;
with Commands.Custom;           use Commands.Custom;

package body GPS.Kernel.Task_Manager is
   Me : constant Trace_Handle := Create ("Tasks");

   Command_Class_Name       : constant String := "Command";

   type Command_Property is new Instance_Property_Record with record
      Command : Scheduled_Command_Access;
   end record;
   type Command_Property_Access is access all Command_Property'Class;

   function Create_Wrapper
     (Command         : access Root_Command'Class;
      Destroy_On_Exit : Boolean) return Scheduled_Command_Access;
   --  Create a new wrapper

   ----------------------
   -- Set_Task_Manager --
   ----------------------

   procedure Set_Task_Manager
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : Task_Manager_Access)
   is
   begin
      Kernel.Tasks := Manager;
   end Set_Task_Manager;

   ----------------------
   -- Get_Task_Manager --
   ----------------------

   function Get_Task_Manager
     (Kernel : access Kernel_Handle_Record'Class) return Task_Manager_Access is
   begin
      Assert
        (Me, Kernel.Tasks /= No_Task_Manager,
         "Task manager has not been created yet");
      return Kernel.Tasks;
   end Get_Task_Manager;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (Command : access Scheduled_Command'Class)
      return Command_Access is
   begin
      return Command.Command;
   end Get_Command;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scheduled_Command) return Command_Return_Type is
   begin
      return Execute (Command.Command);
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Scheduled_Command) return String is
   begin
      return Name (Command.Command);
   end Name;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access Scheduled_Command) return Progress_Record is
   begin
      return Progress (Command.Command);
   end Progress;

   ------------------
   -- Set_Progress --
   ------------------

   overriding procedure Set_Progress (Command : access Scheduled_Command;
                                      Progress : Progress_Record) is
   begin
      Set_Progress (Command.Command, Progress);
   end Set_Progress;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Scheduled_Command) is
   begin
      Interrupt (Command.Command.all);
   end Interrupt;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Command : in out Scheduled_Command) is
      Instances : constant Instance_Array := Get_Instances (Command.Instances);
   begin
      if Command.Is_Dead then
         for J in Instances'Range loop
            if Instances (J) /= No_Class_Instance then
               return;
            end if;
         end loop;

         if Command.Destroy_On_Exit then
            Unref (Command.Command);
         end if;

         Free (Command.Instances);
      else
         declare
            Dead_Command : Scheduled_Command_Access :=
                             new Scheduled_Command;
            Found        : Boolean := False;
         begin
            Dead_Command.Command := Command.Command;
            Dead_Command.Destroy_On_Exit := Command.Destroy_On_Exit;
            Dead_Command.Is_Dead := True;

            for J in Instances'Range loop
               if Instances (J) /= No_Class_Instance then
                  Found := True;
                  Set_Data
                    (Instances (J), Command_Class_Name, Command_Property'
                       (Command => Dead_Command));
               end if;
            end loop;

            --  If the command is not referenced by any script, then we just
            --  remove it.

            if not Found then
               Unref (Command_Access (Dead_Command));
            end if;

            Free (Command.Instances);
         end;
      end if;
   end Free;

   ----------
   -- Undo --
   ----------

   overriding function Undo (This : access Scheduled_Command) return Boolean is
   begin
      return Undo (This.Command);
   end Undo;

   --------------------
   -- Create_Wrapper --
   --------------------

   function Create_Wrapper
     (Command : access Root_Command'Class; Destroy_On_Exit : Boolean)
      return Scheduled_Command_Access
   is
      C     : constant Scheduled_Command_Access := new Scheduled_Command;
   begin
      C.Command := Command_Access (Command);
      C.Destroy_On_Exit := Destroy_On_Exit;

      return C;
   end Create_Wrapper;

   -------------------------------
   -- Launch_Background_Command --
   -------------------------------

   procedure Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True;
      Block_Exit      : Boolean := True)
   is
      Wrapper : constant Scheduled_Command_Access := Launch_Background_Command
        (Kernel          => Kernel,
         Command         => Command,
         Active          => Active,
         Show_Bar        => Show_Bar,
         Queue_Id        => Queue_Id,
         Destroy_On_Exit => Destroy_On_Exit,
         Block_Exit      => Block_Exit);
      pragma Unreferenced (Wrapper);
   begin
      null;
   end Launch_Background_Command;

   -------------------------------
   -- Launch_Background_Command --
   -------------------------------

   function Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True;
      Block_Exit      : Boolean := True) return Scheduled_Command_Access
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      Wrapper : constant Scheduled_Command_Access :=
                  Create_Wrapper (Command, Destroy_On_Exit);
   begin
      Add_Command
        (Manager,
         Command_Access (Wrapper),
         Active, Show_Bar, Queue_Id, Block_Exit);

      return Wrapper;
   end Launch_Background_Command;

   -------------------------------
   -- Launch_Foreground_Command --
   -------------------------------

   procedure Launch_Foreground_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Destroy_On_Exit : Boolean := True)
   is
      Wrapper : Scheduled_Command_Access :=
                  Create_Wrapper (Command, Destroy_On_Exit);
      Result  : Command_Return_Type;
   begin
      Push_State (Kernel, Busy);
      loop
         begin
            Result := Execute (Command);
         exception
            when E : others =>
               Trace (Me, E);
               Result := Failure;
         end;

         exit when Result = Success or Result = Failure;
      end loop;

      Pop_State (Kernel);
      Unref (Command_Access (Wrapper));
   end Launch_Foreground_Command;

   ---------------------------
   -- Interrupt_Latest_Task --
   ---------------------------

   procedure Interrupt_Latest_Task
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Interrupt_Latest_Task (Get_Task_Manager (Kernel));
   end Interrupt_Latest_Task;

   ---------------------
   -- Interrupt_Queue --
   ---------------------

   procedure Interrupt_Queue
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Command_Access)
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      Interrupt_Queue (Manager, Command);
   end Interrupt_Queue;

   ---------------
   -- Has_Queue --
   ---------------

   function Has_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String) return Boolean
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      return Has_Queue (Manager, Queue_Id);
   end Has_Queue;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Push_Command, Pop_Command : Custom_Command_Access;
      Script                    : Scripting_Language;
   begin
      Script := Lookup_Scripting_Language
        (Get_Scripts (Kernel), GPS_Shell_Name);
      Create
        (Push_Command, "set_busy", Kernel_Handle (Kernel), "set_busy", Script);
      Create
        (Pop_Command, "unset_busy",
         Kernel_Handle (Kernel), "unset_busy", Script);

      Set_Busy_Commands
        (Get_Task_Manager (Kernel),
         Command_Access (Push_Command),
         Command_Access (Pop_Command));

      Standard.Task_Manager.Shell.Register_Commands (Kernel);
   end Register_Module;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Command       : access Scheduled_Command'Class;
      Language      : access Scripting_Language_Record'Class;
      Command_Class : Class_Type := No_Class)
      return Class_Instance
   is
      Inst : Class_Instance := Get (Command.Instances, Language);
   begin
      if Inst = No_Class_Instance then

         if Command_Class = No_Class then
            declare
               Root_Command_Class : constant Class_Type :=
                                      New_Class
                                        (Get_Kernel (Language),
                                         Command_Class_Name);
            begin
               Inst := New_Instance (Language, Root_Command_Class);
            end;
         else
            Inst := New_Instance (Language, Command_Class);
         end if;

         Set_Instance (Command, Language, Inst);
      end if;

      return Inst;
   end Get_Instance;

   ------------------
   -- Set_Instance --
   ------------------

   procedure Set_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class;
      Instance : Class_Instance) is
   begin
      Set_Data
        (Instance, Command_Class_Name, Command_Property'
           (Command => Scheduled_Command_Access (Command)));
      Ref (Command_Access (Command));  --  unrefed in GPS.Command.__del__
      Set (Command.Instances, Language, Instance);
   end Set_Instance;

   ---------------------
   -- Remove_Instance --
   ---------------------

   procedure Remove_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class) is
   begin
      Set (Command.Instances, Language, No_Class_Instance);
   end Remove_Instance;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance) return Scheduled_Command_Access
   is
      Cmd : Command_Property_Access;
   begin
      Cmd := Command_Property_Access
        (Instance_Property'(Get_Data (Instance, Command_Class_Name)));
      if Cmd = null then
         return null;
      else
         return Cmd.Command;
      end if;
   exception
      when Invalid_Data =>
         return null;
   end Get_Data;

end GPS.Kernel.Task_Manager;
