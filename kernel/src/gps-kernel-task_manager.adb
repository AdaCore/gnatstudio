------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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
with Commands.Interactive;      use Commands.Interactive;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Task_Manager.Shell;        use Task_Manager.Shell;

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
      Result  : Command_Return_Type;
      C : Command_Access;
   begin
      loop
         begin
            --  ??? Not elegant. We should refactor commands so that Execute
            --  takes a context as parameter, and Interactive_Context is just
            --  a child of Context (MB20-044)

            if Command.all in Interactive_Command'Class then
               --  We want to make sure that the context provides access to
               --  the kernel.
               Result := Interactive_Command_Access (Command).Execute
                 (Create_Null_Context (New_Context (Kernel => Kernel)));
            else
               Result := Command.Execute;
            end if;

         exception
            when E : others =>
               Trace (Me, E);
               Result := Failure;
         end;

         exit when Result = Success or Result = Failure;
      end loop;

      if Destroy_On_Exit then
         C := Command_Access (Command);
         Unref (C);
      end if;
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
   begin
      Standard.Task_Manager.Shell.Register_Commands (Kernel);
   end Register_Module;

end GPS.Kernel.Task_Manager;
