------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Task_Manager.Shell;        use Task_Manager.Shell;
with Commands.Custom;           use Commands.Custom;

package body GPS.Kernel.Task_Manager is
   Me : constant Trace_Handle := Create ("Tasks");

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
      pragma Unreferenced (Kernel);
      Wrapper : Scheduled_Command_Access :=
                  Create_Wrapper (Command, Destroy_On_Exit);
      Result  : Command_Return_Type;
   begin
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

end GPS.Kernel.Task_Manager;
