-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003 - 2004                     --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Main;         use Gtk.Main;
with Traces;           use Traces;
with Ada.Exceptions;   use Ada.Exceptions;

with Task_Manager.GUI; use Task_Manager.GUI;

package body Task_Manager is

   Me : constant Debug_Handle := Create ("Task Manager");

   Timeout : constant := 100;

   package Task_Manager_Idle is new Gtk.Main.Idle (Task_Manager_Access);
   package Task_Manager_Timeout is new Gtk.Main.Timeout (Task_Manager_Access);

   function Get_Or_Create_Task_Queue
     (Manager  : Task_Manager_Access;
      Queue_Id : String;
      Active   : Boolean;
      Show_Bar : Boolean) return Integer;
   --  Return an index in Manager.Queues corresponding to Queue_Id;

   function Active_Incremental
     (Manager : in Task_Manager_Access) return Boolean;
   --  Incremental function for the active loop.

   function Passive_Incremental
     (Manager : in Task_Manager_Access) return Boolean;
   --  Incremental function for the passive loop.

   function Execute_Incremental
     (Manager : in Task_Manager_Access;
      Active  : Boolean) return Boolean;
   --  Incremental function to execute the task manager.

   procedure Run
     (Manager : Task_Manager_Access;
      Active  : Boolean);
   --  Runs the task manager, if it is not already running.

   function Safe_Execute
     (Command : Command_Access) return Command_Return_Type;
   --  Executes command, and returns the result. If an exception occurs during
   --  execution of the command, return Failure.

   ------------------
   -- Safe_Execute --
   ------------------

   function Safe_Execute
     (Command : Command_Access) return Command_Return_Type is
   begin
      return Execute (Command);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return Failure;
   end Safe_Execute;

   ------------------------
   -- Active_Incremental --
   ------------------------

   function Active_Incremental
     (Manager : in Task_Manager_Access) return Boolean
   is
      Result : Boolean;
      Return_Type : Command_Return_Type;
      pragma Unreferenced (Result, Return_Type);

   begin
      Result := Execute_Incremental (Manager, True);

      --  The active loop ends when there are no more active queues left.

      if Manager.Queues /= null
        and then Manager.Passive_Index > Manager.Queues'First
      then
         return True;

      else
         Manager.Running_Active := False;
         Return_Type := Execute (Manager.Pop_Command);
         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Active_Incremental;

   -------------------------
   -- Passive_Incremental --
   -------------------------

   function Passive_Incremental
     (Manager : in Task_Manager_Access) return Boolean
   is
      Result : Boolean;
      Return_Type : Command_Return_Type;
      pragma Unreferenced (Result, Return_Type);
   begin
      Result := Execute_Incremental (Manager, False);

      Refresh (Manager);

      --  The passive loop ends when there are no more queues left.

      if Manager.Queues /= null then
         return True;
      else
         Return_Type := Execute (Manager.Pop_Command);
         Manager.Running_Passive := False;
         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Passive_Incremental;

   -------------------------
   -- Execute_Incremental --
   -------------------------

   function Execute_Incremental
     (Manager : in Task_Manager_Access;
      Active  : Boolean) return Boolean
   is
      Lowest  : Integer := Integer'Last;
      Result  : Command_Return_Type;
      First   : Integer;
      Last    : Integer;
      Command : Command_Access;
      Previous_Prio : Integer;

      This_Running_Queue : constant Integer := Manager.Running_Queue;
   begin
      if Manager.Queues = null then
         return False;

      else
         --  Find the queue with the lowest current priority.
         --  Add their priority to each queue.

         if Active then
            First := Manager.Queues'First;
            Last  := Manager.Passive_Index - 1;
            Previous_Prio := Manager.Minimal_Active_Priority;
         else
            First := Manager.Passive_Index;
            Last  := Manager.Queues'Last;
            Previous_Prio := Manager.Minimal_Passive_Priority;
         end if;

         if First > Last then
            return False;
         end if;

         for Q in First .. Last loop
            if Manager.Queues (Q).Current_Priority < Lowest then
               Lowest := Manager.Queues (Q).Current_Priority;
               Manager.Running_Queue := Q;
            end if;

            Manager.Queues (Q).Current_Priority :=
              Manager.Queues (Q).Current_Priority - Previous_Prio;
         end loop;

         Manager.Queues (Manager.Running_Queue).Current_Priority :=
           Manager.Queues (Manager.Running_Queue).Current_Priority
           + Manager.Queues (Manager.Running_Queue).Priority;

         if Active then
            Manager.Minimal_Active_Priority := Lowest;
         else
            Manager.Minimal_Passive_Priority := Lowest;
         end if;

         if Manager.Queues (Manager.Running_Queue).Status = Paused then
            return False;
         end if;

         Command := Command_Queues.Head
           (Manager.Queues (Manager.Running_Queue).Queue);

         if Manager.Queues (Manager.Running_Queue).Status = Interrupted then
            Result := Success;
         else
            Result := Safe_Execute (Command);
         end if;

         Manager.Queues (Manager.Running_Queue).Need_Refresh := True;

         case Result is
            when Success | Failure =>
               --  ??? add the command to the list of done or failed commands.

               if Manager.Queues
                 (Manager.Running_Queue).Status = Interrupted
               then
                  Command_Queues.Free
                    (Manager.Queues (Manager.Running_Queue).Queue);
               else
                  if Result = Success then
                     declare
                        New_Queue : constant Command_Queues.List :=
                          Get_Consequence_Actions (Command);
                     begin
                        Manager.Queues (Manager.Running_Queue).Total :=
                          Manager.Queues (Manager.Running_Queue).Total
                          + Command_Queues.Length (New_Queue);
                        Command_Queues.Concat
                          (Manager.Queues (Manager.Running_Queue).Queue,
                           New_Queue);
                     end;

                     Free_Alternate_Actions (Command, True);
                     Free_Consequence_Actions (Command, False);
                  else
                     declare
                        New_Queue : constant Command_Queues.List :=
                          Get_Alternate_Actions (Command);
                     begin
                        Manager.Queues (Manager.Running_Queue).Total :=
                          Manager.Queues (Manager.Running_Queue).Total
                          + Command_Queues.Length (New_Queue);
                        Command_Queues.Concat
                          (Manager.Queues (Manager.Running_Queue).Queue,
                           New_Queue);
                     end;

                     Free_Consequence_Actions (Command, True);
                     Free_Alternate_Actions (Command, False);
                  end if;

                  Command_Queues.Next
                    (Manager.Queues (Manager.Running_Queue).Queue);

                  Manager.Queues (Manager.Running_Queue).Done :=
                    Manager.Queues (Manager.Running_Queue).Done + 1;
               end if;
               --  If it was the last command in the queue, free the queue.

               if Command_Queues.Is_Empty
                 (Manager.Queues (Manager.Running_Queue).Queue)
               then
                  Free (Manager.Queues (Manager.Running_Queue).Id);

                  if Manager.Queues (Manager.Running_Queue).Bar /= null then
                     Destroy (Manager.Queues (Manager.Running_Queue).Bar);
                     Manager.Queues (Manager.Running_Queue).Bar := null;
                  end if;

                  Manager.Need_Global_Refresh := True;

                  if Manager.Queues'Length = 1 then
                     Unchecked_Free (Manager.Queues);
                     Manager.Referenced_Command := -1;
                     Manager.Running_Queue := This_Running_Queue;
                     return False;

                  else
                     declare
                        New_Queues : Task_Queue_Array
                          (Manager.Queues'First .. Manager.Queues'Last - 1);
                     begin
                        New_Queues
                          (Manager.Queues'First
                           .. Manager.Running_Queue - 1) :=
                          Manager.Queues
                            (Manager.Queues'First
                             .. Manager.Running_Queue - 1);

                        New_Queues
                          (Manager.Running_Queue .. Manager.Queues'Last - 1) :=
                          Manager.Queues
                            (Manager.Running_Queue + 1 .. Manager.Queues'Last);

                        if Active then
                           Manager.Passive_Index
                             := Manager.Passive_Index - 1;
                        end if;

                        if Manager.Referenced_Command =
                          Manager.Running_Queue
                        then
                           Manager.Referenced_Command := -1;

                        elsif Manager.Referenced_Command >
                          Manager.Running_Queue
                        then
                           Manager.Referenced_Command :=
                             Manager.Referenced_Command - 1;
                        end if;

                        Unchecked_Free (Manager.Queues);
                        Manager.Queues := new Task_Queue_Array'(New_Queues);
                     end;
                  end if;
               end if;

            when Raise_Priority =>
               if Manager.Queues (Manager.Running_Queue).Priority > 1 then
                  Manager.Queues (Manager.Running_Queue).Priority :=
                    Manager.Queues (Manager.Running_Queue).Priority - 1;
               end if;

            when Lower_Priority =>
               if Manager.Queues (Manager.Running_Queue).Priority < 3 then
                  Manager.Queues (Manager.Running_Queue).Priority :=
                    Manager.Queues (Manager.Running_Queue).Priority + 1;
               end if;

            when Execute_Again =>
               null;

         end case;

         Manager.Running_Queue := This_Running_Queue;
         return True;
      end if;
   end Execute_Incremental;

   ---------
   -- Run --
   ---------

   procedure Run
     (Manager : Task_Manager_Access;
      Active  : Boolean)
   is
      Idle_Handler    : Idle_Handler_Id;
      Timeout_Handler : Timeout_Handler_Id;
      Result          : Command_Return_Type;
      pragma Unreferenced (Idle_Handler, Timeout_Handler, Result);
   begin
      if not Manager.Running_Passive then
         Manager.Running_Passive := True;

         Timeout_Handler := Task_Manager_Timeout.Add
           (Timeout, Passive_Incremental'Access, Manager);

         Result := Execute (Manager.Push_Command);
      end if;

      if Active
        and then not Manager.Running_Active
      then
         Manager.Running_Active := True;

         Idle_Handler := Task_Manager_Idle.Add
           (Active_Incremental'Access, Manager);

         Result := Execute (Manager.Push_Command);
      end if;
   end Run;

   ------------------------------
   -- Get_Or_Create_Task_Queue --
   ------------------------------

   function Get_Or_Create_Task_Queue
     (Manager  : Task_Manager_Access;
      Queue_Id : String;
      Active   : Boolean;
      Show_Bar : Boolean) return Integer is
   begin
      if Manager.Queues = null then
         Manager.Queues := new Task_Queue_Array (1 .. 1);
         Manager.Queues (1).Id := new String'(Queue_Id);
         Manager.Queues (1).Show_Bar := Show_Bar;

         if Active then
            Manager.Passive_Index := 2;
         else
            Manager.Passive_Index := 1;
         end if;

         Manager.Need_Global_Refresh := True;

         return 1;

      else
         if Queue_Id /= "" then
            for J in Manager.Queues'Range loop
               if Manager.Queues (J).Id /= null
                 and then Manager.Queues (J).Id.all = Queue_Id
               then
                  return J;
               end if;
            end loop;
         end if;

         Manager.Need_Global_Refresh := True;

         declare
            New_Queues : Task_Queue_Array
              (Manager.Queues'First .. Manager.Queues'Last + 1);
         begin
            if Active then
               New_Queues
                 (Manager.Queues'First + 1 .. Manager.Queues'Last + 1) :=
                 Manager.Queues.all;
               New_Queues (New_Queues'First).Id := new String'(Queue_Id);

               New_Queues (New_Queues'First).Show_Bar := Show_Bar;

               Unchecked_Free (Manager.Queues);
               Manager.Queues := new Task_Queue_Array'(New_Queues);

               Manager.Passive_Index := Manager.Passive_Index + 1;

               --  We may need to modify the current working queue here, since
               --  adding an active action causes the queues to be renumbered.
               --  This is to avoid the following circuitry:
               --  -  An action A is being executed in queue 1
               --  -  in the execution of this action, a new action B is added
               --  -  this action is active, and added to the queue 1
               --  -  queue 1 becomes queue 2
               --  -  when A ends, queue 1 is freed and therefore B is never
               --      executed.

               if Manager.Running_Queue >= 1 then
                  Manager.Running_Queue := Manager.Running_Queue + 1;
               end if;

               return Manager.Queues'First;

            else
               New_Queues
                 (Manager.Queues'First .. Manager.Queues'Last) :=
                 Manager.Queues.all;
               New_Queues (New_Queues'Last).Id := new String'(Queue_Id);

               New_Queues (New_Queues'Last).Show_Bar := Show_Bar;

               Unchecked_Free (Manager.Queues);
               Manager.Queues := new Task_Queue_Array'(New_Queues);

               return Manager.Queues'Last;
            end if;
         end;
      end if;
   end Get_Or_Create_Task_Queue;

   -----------------
   -- Add_Command --
   -----------------

   procedure Add_Command
     (Manager  : Task_Manager_Access;
      Command  : Command_Access;
      Active   : Boolean;
      Show_Bar : Boolean;
      Queue_Id : String := "")
   is
      Task_Queue : constant Integer :=
        Get_Or_Create_Task_Queue (Manager, Queue_Id, Active, Show_Bar);
   begin
      Command_Queues.Append (Manager.Queues (Task_Queue).Queue, Command);

      Manager.Queues (Task_Queue).Total :=
        Manager.Queues (Task_Queue).Total + 1;

      Run (Manager, Active);
   end Add_Command;

   -----------------------
   -- Set_Progress_Area --
   -----------------------

   procedure Set_Progress_Area
     (Manager : Task_Manager_Access;
      Area    : Gtk.Box.Gtk_Hbox) is
   begin
      Manager.Progress_Area := Area;
   end Set_Progress_Area;

   -----------------------
   -- Set_Busy_Commands --
   -----------------------

   procedure Set_Busy_Commands
     (Manager      : Task_Manager_Access;
      Push_Command : Command_Access;
      Pop_Command  : Command_Access) is
   begin
      if Manager.Push_Command /= null then
         Destroy (Manager.Push_Command);
      end if;

      if Manager.Pop_Command /= null then
         Destroy (Manager.Pop_Command);
      end if;

      Manager.Push_Command := Push_Command;
      Manager.Pop_Command := Pop_Command;
   end Set_Busy_Commands;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Manager : Task_Manager_Access) is
   begin
      if Manager.Queues /= null then
         for J in Manager.Queues'Range loop
            Command_Queues.Free (Manager.Queues (J).Queue);
         end loop;

         Unchecked_Free (Manager.Queues);
      end if;

      if Manager.Push_Command /= null then
         Destroy (Manager.Push_Command);
      end if;

      if Manager.Pop_Command /= null then
         Destroy (Manager.Pop_Command);
      end if;
   end Destroy;

   -------------
   -- Get_GUI --
   -------------

   function Get_GUI (Manager : Task_Manager_Access) return Gtk_Widget is
   begin
      return Manager.GUI;
   end Get_GUI;

   -------------
   -- Set_GUI --
   -------------

   procedure Set_GUI
     (Manager : Task_Manager_Access;
      GUI     : Gtk_Widget)
   is
   begin
      Manager.GUI := GUI;
   end Set_GUI;

   --------------------------
   -- Has_Running_Commands --
   --------------------------

   function Has_Running_Commands
     (Manager : Task_Manager_Access) return Boolean is
   begin
      if Manager.Queues = null then
         return False;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Status in Running .. Paused then
            return True;
         end if;
      end loop;

      return False;
   end Has_Running_Commands;

end Task_Manager;
