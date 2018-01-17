------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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
with Task_Manager.Shell;        use Task_Manager.Shell;

with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;

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
     (Kernel            : access Kernel_Handle_Record'Class;
      Command           : access Root_Command'Class;
      Active            : Boolean;
      Show_Bar          : Boolean;
      Queue_Id          : String := "";
      Block_Exit        : Boolean := True;
      Start_Immediately : Boolean := False)
   is
      Wrapper : constant Scheduled_Command_Access := Launch_Background_Command
        (Kernel            => Kernel,
         Command           => Command,
         Active            => Active,
         Show_Bar          => Show_Bar,
         Queue_Id          => Queue_Id,
         Start_Immediately => Start_Immediately,
         Block_Exit        => Block_Exit);
      pragma Unreferenced (Wrapper);
   begin
      null;
   end Launch_Background_Command;

   -------------------------------
   -- Launch_Background_Command --
   -------------------------------

   function Launch_Background_Command
     (Kernel            : access Kernel_Handle_Record'Class;
      Command           : access Root_Command'Class;
      Active            : Boolean;
      Show_Bar          : Boolean;
      Queue_Id          : String := "";
      Block_Exit        : Boolean := True;
      Start_Immediately : Boolean := False) return Scheduled_Command_Access
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      Wrapper : constant Scheduled_Command_Access :=
                  Create_Wrapper (Command);
   begin
      Task_Started_Hook.Run (Kernel);
      Add_Command
        (Manager, Wrapper,
         Active            => Active,
         Show_Bar          => Show_Bar,
         Queue_Id          => Queue_Id,
         Block_Exit        => Block_Exit,
         Start_Immediately => Start_Immediately);
      return Wrapper;
   end Launch_Background_Command;

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
      Command : Scheduled_Command_Access)
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      Interrupt_Queue (Manager, Command);
   end Interrupt_Queue;

   procedure Interrupt_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String)
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
   begin
      Interrupt_Queue (Manager, Queue_Id);
   end Interrupt_Queue;

   ---------------
   -- Has_Queue --
   ---------------

   function Has_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String) return Boolean is
   begin
      return Get_Task_Manager (Kernel).Has_Queue (Queue_Id);
   end Has_Queue;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Manager : Task_Manager_Access;
   begin
      Manager := new Task_Manager_Record;
      Set_Task_Manager (Kernel, Manager);

      Standard.Task_Manager.Shell.Register_Commands (Kernel);
   end Register_Module;

end GPS.Kernel.Task_Manager;
