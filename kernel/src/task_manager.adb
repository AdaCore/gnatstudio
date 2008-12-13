-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

with Glib.Main;        use Glib.Main;

with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Main;         use Gtk.Main;
with Traces;           use Traces;

with Task_Manager.GUI; use Task_Manager.GUI;

package body Task_Manager is

   Timeout : constant := 100;

   package Task_Manager_Idle is new Gtk.Main.Idle (Task_Manager_Access);
   package Task_Manager_Timeout is new Glib.Main.Generic_Sources
     (Task_Manager_Access);

   function Get_Or_Create_Task_Queue
     (Manager    : Task_Manager_Access;
      Queue_Id   : String;
      Active     : Boolean;
      Show_Bar   : Boolean;
      Block_Exit : Boolean) return Integer;
   --  Return an index in Manager.Queues corresponding to Queue_Id

   function Active_Incremental
     (Manager : Task_Manager_Access) return Boolean;
   --  Incremental function for the active loop

   function Passive_Incremental
     (Manager : Task_Manager_Access) return Boolean;
   --  Incremental function for the passive loop

   function Execute_Incremental
     (Manager : Task_Manager_Access;
      Active  : Boolean) return Boolean;
   --  Incremental function to execute the task manager

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
         Trace (Exception_Handle, E);
         return Failure;
   end Safe_Execute;

   ------------------------
   -- Active_Incremental --
   ------------------------

   function Active_Incremental
     (Manager : Task_Manager_Access) return Boolean
   is
      Result      : Boolean;
      Return_Type : Command_Return_Type;
      pragma Unreferenced (Result, Return_Type);

   begin
      Result := Execute_Incremental (Manager, True);

      --  The active loop ends when there are no more active queues left

      if Manager.Queues /= null
        and then Manager.Passive_Index > Manager.Queues'First
      then
         --  Run only if one queue is actually running; do not run if all
         --  queues are paused, for instance.
         for J in Manager.Queues'First .. Manager.Passive_Index - 1 loop
            if Manager.Queues (J).Status = Running then
               return True;
            end if;
         end loop;

         Manager.Running_Active := False;
         return False;

      else
         Manager.Running_Active := False;
         return False;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Active_Incremental;

   -------------------------
   -- Passive_Incremental --
   -------------------------

   function Passive_Incremental
     (Manager : Task_Manager_Access) return Boolean
   is
      Result      : Boolean;
      Return_Type : Command_Return_Type;
      pragma Unreferenced (Result, Return_Type);
   begin
      Result := Execute_Incremental (Manager, False);

      Refresh (Manager);

      --  The passive loop ends when there are no more queues left

      if Manager.Queues /= null then
         return True;
      else
         Manager.Running_Passive := False;
         return False;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Passive_Incremental;

   -------------------------
   -- Execute_Incremental --
   -------------------------

   function Execute_Incremental
     (Manager : Task_Manager_Access;
      Active  : Boolean) return Boolean
   is
      Lowest        : Integer := Integer'Last;
      Result        : Command_Return_Type;
      First         : Integer;
      Last          : Integer;
      Command       : Command_Access;
      Previous_Prio : Integer;
      Queue         : Task_Queue_Access;

      function Free_Queue return Boolean;
      --  Free queue referenced by Queue.
      --  Return True if the callback should be called again, False otherwise.

      ----------------
      -- Free_Queue --
      ----------------

      function Free_Queue return Boolean is
      begin
         GNAT.Strings.Free (Queue.Id);

         if Queue.Bar /= null then
            Destroy (Get_Parent (Queue.Bar));
            Queue.Bar := null;
         end if;

         Manager.Need_Global_Refresh := True;

         if Manager.Queues'Length = 1 then
            Unchecked_Free (Queue);
            Unchecked_Free (Manager.Queues);
            Manager.Referenced_Command := -1;
            return False;

         else
            declare
               New_Queues : Task_Queue_Array
                 (Manager.Queues'First .. Manager.Queues'Last - 1);
               Index      : Integer := -1;
            begin
               --  Find the index of the current running queue

               for J in Manager.Queues'Range loop
                  if Manager.Queues (J) = Queue then
                     Index := J;
                     exit;
                  end if;
               end loop;

               New_Queues (Manager.Queues'First .. Index - 1) :=
                 Manager.Queues (Manager.Queues'First .. Index - 1);

               New_Queues (Index .. Manager.Queues'Last - 1) :=
                 Manager.Queues (Index + 1 .. Manager.Queues'Last);

               if Active then
                  Manager.Passive_Index := Manager.Passive_Index - 1;
               end if;

               if Manager.Referenced_Command = Index then
                  Manager.Referenced_Command := -1;

               elsif Manager.Referenced_Command > Index then
                  Manager.Referenced_Command :=
                    Manager.Referenced_Command - 1;
               end if;

               Unchecked_Free (Queue);
               Unchecked_Free (Manager.Queues);
               Manager.Queues := new Task_Queue_Array'(New_Queues);
            end;
         end if;

         return True;
      end Free_Queue;

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
               Queue := Manager.Queues (Q);
            end if;

            Manager.Queues (Q).Current_Priority :=
              Manager.Queues (Q).Current_Priority - Previous_Prio;
         end loop;

         Queue.Current_Priority := Queue.Current_Priority + Queue.Priority;

         if Active then
            Manager.Minimal_Active_Priority := Lowest;
         else
            Manager.Minimal_Passive_Priority := Lowest;
         end if;

         if Queue.Status = Paused then
            return False;
         end if;

         if Command_Queues.Is_Empty (Queue.Queue) then
            --  The queue is empty: this can happen when scripts call the
            --  interrupt function.
            --  In this case, free the queue and return.
            return Free_Queue;
         end if;

         Command := Command_Queues.Head (Queue.Queue);

         if Queue.Status = Interrupted then
            Result := Success;
         else
            Result := Safe_Execute (Command);
         end if;

         Queue.Need_Refresh := True;

         case Result is
            when Success | Failure =>
               --  ??? add the command to the list of done or failed commands

               if Queue.Status = Interrupted then
                  Command_Queues.Free (Queue.Queue);

               else
                  declare
                     use Command_Queues;
                     Cons_Actions : constant Command_Queues.List :=
                                      Get_Consequence_Actions (Command);
                  begin
                     if Command.Is_Continuation_Action then
                        if Cons_Actions /= Command_Queues.Null_List
                          and then Head (Cons_Actions).Is_Continuation_Action
                        then
                           --  Next action still part of the group fail, record
                           --  status and keep going.
                           if Result = Failure then
                              Queue.Stored_Status := Result;
                           end if;
                           Result := Success;

                        else
                           --  We reach the end of a group fail section, the
                           --  status of the last action is set to false if one
                           --  of the grouped action failed.
                           if Queue.Stored_Status = Failure then
                              Result := Failure;
                           end if;
                           Queue.Stored_Status := Success;
                        end if;
                     end if;
                  end;

                  if Result = Success then
                     declare
                        New_Queue : constant Command_Queues.List :=
                                      Get_Consequence_Actions (Command);
                     begin
                        Queue.Total :=
                          Queue.Total + Command_Queues.Length (New_Queue);
                        Command_Queues.Concat (Queue.Queue, New_Queue);
                     end;

                     Free_Alternate_Actions (Command, True, False);
                     Free_Consequence_Actions (Command, False, False);

                  else
                     declare
                        New_Queue : constant Command_Queues.List :=
                                      Get_Alternate_Actions (Command);
                     begin
                        Queue.Total :=
                          Queue.Total
                          + Command_Queues.Length (New_Queue);
                        Command_Queues.Concat (Queue.Queue, New_Queue);
                     end;

                     Free_Consequence_Actions (Command, True, False);
                     Free_Alternate_Actions (Command, False, False);
                  end if;

                  Command_Queues.Next (Queue.Queue);

                  Queue.Done := Queue.Done + 1;
               end if;
               --  If it was the last command in the queue, free the queue

               if Command_Queues.Is_Empty (Queue.Queue) then
                  return Free_Queue;
               end if;

            when Raise_Priority =>
               if Queue.Priority > 1 then
                  Queue.Priority := Queue.Priority - 1;
               end if;

            when Lower_Priority =>
               if Queue.Priority < 3 then
                  Queue.Priority := Queue.Priority + 1;
               end if;

            when Execute_Again =>
               null;

         end case;

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
      Timeout_Handler : G_Source_Id;
      Result          : Command_Return_Type;
      pragma Unreferenced (Idle_Handler, Timeout_Handler, Result);
   begin
      if not Manager.Running_Passive then
         Manager.Running_Passive := True;

         Timeout_Handler := Task_Manager_Timeout.Timeout_Add
           (Timeout, Passive_Incremental'Access, Manager,
            Priority => Glib.Main.Priority_Default_Idle);
      end if;

      if Active and then not Manager.Running_Active then
         Manager.Running_Active := True;

         if Active_Incremental (Manager) then
            Idle_Handler := Task_Manager_Idle.Add
              (Active_Incremental'Access, Manager);
         end if;
      end if;
   end Run;

   ------------------------------
   -- Get_Or_Create_Task_Queue --
   ------------------------------

   function Get_Or_Create_Task_Queue
     (Manager    : Task_Manager_Access;
      Queue_Id   : String;
      Active     : Boolean;
      Show_Bar   : Boolean;
      Block_Exit : Boolean) return Integer
   is
      use GNAT.Strings;
   begin
      if Manager.Queues = null then
         Manager.Queues := new Task_Queue_Array (1 .. 1);
         Manager.Queues (1) := new Task_Queue_Record;
         Manager.Queues (1).Id := new String'(Queue_Id);
         Manager.Queues (1).Show_Bar := Show_Bar;
         Manager.Queues (1).Block_Exit := Block_Exit;

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

               New_Queues (Manager.Queues'First) := new Task_Queue_Record;
               New_Queues (New_Queues'First).Id := new String'(Queue_Id);

               New_Queues (New_Queues'First).Show_Bar := Show_Bar;
               New_Queues (New_Queues'First).Block_Exit := Block_Exit;

               Unchecked_Free (Manager.Queues);
               Manager.Queues := new Task_Queue_Array'(New_Queues);

               Manager.Passive_Index := Manager.Passive_Index + 1;

               return Manager.Queues'First;

            else
               New_Queues
                 (Manager.Queues'First .. Manager.Queues'Last) :=
                 Manager.Queues.all;
               New_Queues (New_Queues'Last) := new Task_Queue_Record;
               New_Queues (New_Queues'Last).Id := new String'(Queue_Id);

               New_Queues (New_Queues'Last).Show_Bar := Show_Bar;
               New_Queues (New_Queues'Last).Block_Exit := Block_Exit;

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
     (Manager    : Task_Manager_Access;
      Command    : Command_Access;
      Active     : Boolean;
      Show_Bar   : Boolean;
      Queue_Id   : String := "";
      Block_Exit : Boolean := True)
   is
      Task_Queue : constant Integer :=
                     Get_Or_Create_Task_Queue
                       (Manager, Queue_Id, Active, Show_Bar, Block_Exit);
   begin
      Command_Queues.Append (Manager.Queues (Task_Queue).Queue, Command);

      Manager.Queues (Task_Queue).Total :=
        Manager.Queues (Task_Queue).Total + 1;

      Run (Manager, Active);
   end Add_Command;

   ---------------------
   -- Interrupt_Queue --
   ---------------------

   procedure Interrupt_Queue
     (Manager : Task_Manager_Access;
      Command : Command_Access)
   is
      use Command_Queues;
      Node : Command_Queues.List_Node;
   begin
      for J in Manager.Queues'Range loop
         Node := First (Manager.Queues (J).Queue);

         while Node /= Null_Node loop
            if Data (Node) = Command then
               Interrupt_Command (Manager, J);
               Free (Manager.Queues (J).Queue);
               return;
            end if;

            Node := Next (Node);
         end loop;
      end loop;
   end Interrupt_Queue;

   ----------
   -- Head --
   ----------

   function Head
     (Manager : Task_Manager_Access; Id : String) return Command_Access
   is
      use Command_Queues;
   begin
      if Manager.Queues = null then
         return null;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Id.all = Id then
            return Head (Manager.Queues (J).Queue);
         end if;
      end loop;

      return null;
   end Head;

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
         Unref (Manager.Push_Command);
      end if;

      if Manager.Pop_Command /= null then
         Unref (Manager.Pop_Command);
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
            Unchecked_Free (Manager.Queues (J));
         end loop;

         Unchecked_Free (Manager.Queues);
      end if;

      if Manager.Push_Command /= null then
         Unref (Manager.Push_Command);
      end if;

      if Manager.Pop_Command /= null then
         Unref (Manager.Pop_Command);
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

   procedure Set_GUI (Manager : Task_Manager_Access; GUI : Gtk_Widget) is
   begin
      Manager.GUI := GUI;
   end Set_GUI;

   ---------------------------
   -- Interrupt_Latest_Task --
   ---------------------------

   procedure Interrupt_Latest_Task (Manager : Task_Manager_Access) is
   begin
      if Manager.Queues /= null then
         Interrupt_Command (Manager, Manager.Queues'Last);
      end if;
   end Interrupt_Latest_Task;

   -------------------------
   -- Interrupt_All_Tasks --
   -------------------------

   procedure Interrupt_All_Tasks (Manager : Task_Manager_Access) is
      Commands : constant Command_Array := Get_Scheduled_Commands (Manager);
   begin
      for C in Commands'Range loop
         Interrupt_Queue (Manager, Commands (C));
      end loop;
   end Interrupt_All_Tasks;

   --------------------------
   -- Has_Running_Commands --
   --------------------------

   function Has_Running_Commands
     (Manager         : Task_Manager_Access;
      Consider_Silent : Boolean) return Boolean is
   begin
      if Manager.Queues = null then
         return False;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Status in Running .. Paused then
            if Manager.Queues (J).Block_Exit
              and then (Consider_Silent
                        or else Manager.Queues (J).Show_Bar)
            then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Has_Running_Commands;

   ----------------------------
   -- Get_Scheduled_Commands --
   ----------------------------

   function Get_Scheduled_Commands
     (Manager : Task_Manager_Access) return Command_Array
   is
      use Commands.Command_Queues;

      Total       : Integer := 0;
      Empty_Array : Command_Array (1 .. 0);
   begin
      if Manager.Queues = null then
         return Empty_Array;
      end if;

      for J in Manager.Queues.all'Range loop
         Total := Total + Length (Manager.Queues (J).Queue);
      end loop;

      declare
         Result       : Command_Array (1 .. Total);
         Node         : Command_Queues.List_Node;
         Result_Index : Integer := 1;
      begin
         for J in Manager.Queues.all'Range loop
            Node := First (Manager.Queues (J).Queue);

            while Node /= Null_Node loop
               Result (Result_Index) := Data (Node);
               Node := Next (Node);
               Result_Index := Result_Index + 1;
            end loop;
         end loop;

         return Result;
      end;
   end Get_Scheduled_Commands;

end Task_Manager;
