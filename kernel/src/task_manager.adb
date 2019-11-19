------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GPS.Kernel.Hooks;

package body Task_Manager is
   Me : constant Trace_Handle := Create ("GPS.KERNEL.TASK");

   Timeout : constant := 100;

   Queue_Id_Counter : Natural := 0;
   --  This is a counter used to generate unique queue Ids - OK to leave this
   --  a global variable.

   package Task_Manager_Idle is new Glib.Main.Generic_Sources
     (Task_Manager_Access);
   package Task_Manager_Timeout is new Glib.Main.Generic_Sources
     (Task_Manager_Access);

   function Get_Or_Create_Task_Queue
     (Manager    : not null access Task_Manager_Record'Class;
      Queue_Id   : String;
      Active     : Boolean;
      Show_Bar   : Boolean;
      Block_Exit : Boolean;
      Status     : Queue_Status) return Integer;
   --  Return an index in Manager.Queues corresponding to Queue_Id
   --  If Queue_Id corresponds to an identified queue, the index for this
   --  queue is returned.
   --  If Queue_Id is empty, the queue will be given an unique identifier.

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
     (Command : in out Scheduled_Command_Access) return Command_Return_Type;
   --  Executes command, and returns the result. If an exception occurs during
   --  execution of the command, return Failure.

   procedure Run
     (Manager : Task_Manager_Access;
      Active  : Boolean);
   --  Runs the task manager, if it is not already running

   procedure Interrupt_Queue_N
     (Manager : not null access Task_Manager_Record;
      Index   : Natural);
   --  Internal factorization function.
   --  Interrupt the queue at the given index.

   procedure Interrupt_Task
     (Manager : not null access Task_Manager_Record;
      Task_Q  : Task_Queue_Access);
   --  Interrupt the given task

   -------------------
   -- Queue_From_Id --
   -------------------

   function Queue_From_Id
     (Manager : not null access Task_Manager_Record;
      Id      : String) return Task_Queue_Access is
   begin
      if Manager.Queues = null then
         return null;
      end if;
      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Id = Id then
            return Manager.Queues (J);
         end if;
      end loop;
      return null;
   end Queue_From_Id;

   --------------------
   -- Interrupt_Task --
   --------------------

   procedure Interrupt_Task
     (Manager : not null access Task_Manager_Record;
      Task_Q  : Task_Queue_Access)
   is
      Command : Command_Access;
   begin
      if not Task_Q.Queue.Is_Empty then
         --  Safety checks, allowing clients to interrupt
         --  several times the same queue.
         Command := Task_Q.Queue.First_Element;

         if Command /= null then
            Interrupt (Command.all);
         end if;
      end if;

      Task_Q.Status := Completed;
      Run (Task_Manager_Access (Manager), Active => Task_Q.Active);
   end Interrupt_Task;

   -----------------------
   -- Interrupt_Command --
   -----------------------

   procedure Interrupt_Command
     (Manager : not null access Task_Manager_Record;
      Id      : String)
   is
      Task_Q  : constant Task_Queue_Access := Queue_From_Id (Manager, Id);
   begin
      if Task_Q /= null then
         Interrupt_Task (Manager, Task_Q);
      end if;
   end Interrupt_Command;

   ------------------
   -- Safe_Execute --
   ------------------

   function Safe_Execute
     (Command : in out Scheduled_Command_Access) return Command_Return_Type
   is
      Result : Command_Return_Type;
   begin
      --  Ref/Unref Command, to protect the command from being freed by reentry
      --  in the Task Manager while it is itself running.
      Ref (Command);
      Result := Command.Execute;
      Unref (Command_Access (Command));
      return Result;
   exception
      when E : others =>
         Trace (Me, E);
         Unref (Command_Access (Command));
         return Failure;
   end Safe_Execute;

   ------------------------
   -- Active_Incremental --
   ------------------------

   function Active_Incremental
     (Manager : Task_Manager_Access) return Boolean
   is
      Ignore      : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Ignore := Execute_Incremental (Manager, True);

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
      end if;

      Manager.Active_Handler_Id := No_Source_Id;
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         Manager.Active_Handler_Id := No_Source_Id;
         return False;
   end Active_Incremental;

   -------------------------
   -- Passive_Incremental --
   -------------------------

   function Passive_Incremental
     (Manager : Task_Manager_Access) return Boolean
   is
      Ignore      : Boolean;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Execute_Incremental (Manager, False);

      --  The passive loop ends when there are no more queues left

      if Manager.Queues /= null then
         return True;
      else
         Manager.Passive_Handler_Id := No_Source_Id;
         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Manager.Passive_Handler_Id := No_Source_Id;
         return False;
   end Passive_Incremental;

   -------------------------
   -- Execute_Incremental --
   -------------------------

   function Execute_Incremental
     (Manager : Task_Manager_Access;
      Active  : Boolean) return Boolean
   is
      function Free_Queue (Index : Natural) return Boolean;
      --  Free queue referenced by Queue.
      --  Return True if the callback should be called again, False otherwise.

      ----------------
      -- Free_Queue --
      ----------------

      function Free_Queue (Index : Natural) return Boolean is
         Queue : Task_Queue_Access := Manager.Queues (Index);
         New_Queues : Task_Queue_Array
           (Manager.Queues'First .. Manager.Queues'Last - 1);
      begin
         if Manager.Queues'Length = 1 then
            Unchecked_Free (Queue);
            Unchecked_Free (Manager.Queues);
            return False;

         else
            New_Queues (Manager.Queues'First .. Index - 1) :=
              Manager.Queues (Manager.Queues'First .. Index - 1);
            New_Queues (Index .. Manager.Queues'Last - 1) :=
              Manager.Queues (Index + 1 .. Manager.Queues'Last);

            if Active then
               Manager.Passive_Index := Manager.Passive_Index - 1;
            end if;

            Unchecked_Free (Queue);
            Unchecked_Free (Manager.Queues);
            Manager.Queues := new Task_Queue_Array'(New_Queues);
            return True;
         end if;
      end Free_Queue;

      Lowest        : Integer := Integer'Last;
      Result        : Command_Return_Type;
      First         : Integer;
      Last          : Integer;
      Command       : Scheduled_Command_Access;
      Previous_Prio : Integer;
      Queue         : Task_Queue_Access;
      Index         : Natural := 0;

   begin
      if Manager.Queues = null then
         return False;

      else
         --  Free all Queues that are marked as Completed

         for J in Manager.Queues'Range loop
            if Manager.Queues (J).Status = Completed then
               GPS.Kernel.Hooks.Task_Finished_Hook.Run (Manager.Kernel);
               Free (Command_Lists.List (Manager.Queues (J).Queue));
            end if;
         end loop;

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
            --   ??? Should we disable the corresponding timeout or idle ?
            return False;
         end if;

         --  ??? Should use a priority queue here
         for Q in First .. Last loop
            Manager.Queues (Q).Current_Priority :=
              Manager.Queues (Q).Current_Priority - Previous_Prio;

            if Manager.Queues (Q).Current_Priority < Lowest then
               Lowest := Manager.Queues (Q).Current_Priority;
               Index := Q;
               Queue := Manager.Queues (Q);
            end if;
         end loop;

         Queue.Current_Priority := Queue.Current_Priority + 1;

         if Active then
            Manager.Minimal_Active_Priority := Lowest;
         else
            Manager.Minimal_Passive_Priority := Lowest;
         end if;

         --   ??? Shouldn't we be looking for another unpaused queue with
         --  a lower priority ? Or ignore paused queues in the loop above
         if Queue.Status = Paused then
            return False;
         end if;

         --  ??? Shouldn't we always start by cleaning up empty queues (the
         --  result of Interrupt or finished commands)
         if Queue.Queue.Is_Empty then
            --  The queue is empty: this can happen when scripts call the
            --  interrupt function.
            --  In this case, free the queue and return.
            return Free_Queue (Index);
         end if;

         Command := Scheduled_Command_Access (Queue.Queue.First_Element);

         if Queue.Status = Completed then
            Result := Success;
         else
            Result := Safe_Execute (Command);
         end if;

         --  Warning: the index might have changed here, if a new queue was
         --  created by the command. So Index should not be used anymore
         Index := 0;
         for Q in Manager.Queues'Range loop
            if Manager.Queues (Q) = Queue then
               Index := Q;
               exit;
            end if;
         end loop;

         case Result is
            when Success | Failure =>
               --  ??? add the command to the list of done or failed commands

               if Queue.Status /= Completed then
                  Next (Queue.Queue);
                  Queue.Done := Queue.Done + 1;
               end if;

               --  If it was the last command in the queue, free the queue

               if Queue.Queue.Is_Empty then
                  return Free_Queue (Index);
               end if;

            when Execute_Again =>
               null;
         end case;

         return True;
      end if;
   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Execute_Incremental;

   ---------
   -- Run --
   ---------

   procedure Run
     (Manager : Task_Manager_Access;
      Active  : Boolean) is
   begin
      if Manager.Passive_Handler_Id = No_Source_Id then
         --  ??? we should fix the task_manager so that it does not run
         --  iterations of the Passive loop when only Active commands
         --  are running.
         Manager.Passive_Handler_Id := Task_Manager_Timeout.Timeout_Add
           (Timeout, Passive_Incremental'Access, Manager,
            Priority => Glib.Main.Priority_Default_Idle);
      end if;

      if Active then
         --  Running an Active command

         --  There is a possible infinite loop here:
         --   - the call to Active_Incremental below runs the current Active
         --     command known to the task manager,
         --   - if, as a result of this run, another Active command is added,
         --     we would come back here and run this current Active command
         --     again
         --  We use Prevent_Active_Reentry as a flag to prevent this reentry.

         if Manager.Prevent_Active_Reentry then
            return;
         end if;

         Manager.Prevent_Active_Reentry := True;

         --  If the active loop is already running, do nothing and wait for
         --  the next iteration of the loop.
         --  ??? This test means that there is an unpredictable behavior in
         --  the task_manager: a task added to the Active loop is executed
         --  once immediately, but this is only the case if there isn't already
         --  an Active loop running. We should fix this impredictability.
         if Manager.Active_Handler_Id = No_Source_Id

         --  When running an Active command, we first do one iteration
         --  immediately, using Active_Incremental here.

           and then Active_Incremental (Manager)
         then

            --  If Active_Incremental returned True, it means "keep going": run
            --  the active idle loop to continue processing.

            Manager.Active_Handler_Id := Task_Manager_Idle.Idle_Add
              (Active_Incremental'Access, Manager);
         end if;

         Manager.Prevent_Active_Reentry := False;
      end if;
   end Run;

   ------------------------------
   -- Get_Or_Create_Task_Queue --
   ------------------------------

   function Get_Or_Create_Task_Queue
     (Manager    : not null access Task_Manager_Record'Class;
      Queue_Id   : String;
      Active     : Boolean;
      Show_Bar   : Boolean;
      Block_Exit : Boolean;
      Status     : Queue_Status) return Integer
   is
      function To_Queue_Id return String;
      --  Return an unique queue Id, either based on Queue_Id or based on
      --  an internal counter.

      function To_Queue_Id return String is
      begin
         if Queue_Id /= "" then
            return Queue_Id;
         else
            if Queue_Id_Counter = Natural'Last then
               --  So extremely unlikely, but...
               Queue_Id_Counter := 0;
            else
               Queue_Id_Counter := Queue_Id_Counter + 1;
            end if;
            return Queue_Id_Counter'Img;
         end if;
      end To_Queue_Id;

      The_Queue_Id : constant String := To_Queue_Id;

      procedure Init (Q : out Task_Queue_Access);
      --  Set the fields of the queue

      procedure Init (Q : out Task_Queue_Access) is
      begin
         Q := new Task_Queue_Record;
         Q.Id := To_Unbounded_String (The_Queue_Id);
         Q.Show_Bar := Show_Bar;
         Q.Block_Exit := Block_Exit;
         Q.Active := Active;
         Q.Status := Status;
      end Init;

   begin
      if Manager.Queues = null then
         Manager.Queues := new Task_Queue_Array (1 .. 1);
         Init (Manager.Queues (1));

         if Active then
            Manager.Passive_Index := 2;
         else
            Manager.Passive_Index := 1;
         end if;

         return 1;

      else
         for J in Manager.Queues'Range loop
            if Manager.Queues (J).Id /= Null_Unbounded_String
              and then Manager.Queues (J).Id = The_Queue_Id
              and then Manager.Queues (J).Status /= Completed
            then
               Manager.Queues (J).Show_Bar := Show_Bar;
               return J;
            end if;
         end loop;

         declare
            New_Queues : Task_Queue_Array
              (Manager.Queues'First .. Manager.Queues'Last + 1);
         begin
            if Active then
               New_Queues
                 (Manager.Queues'First + 1 .. Manager.Queues'Last + 1) :=
                 Manager.Queues.all;
               Init (New_Queues (Manager.Queues'First));
               Unchecked_Free (Manager.Queues);
               Manager.Queues := new Task_Queue_Array'(New_Queues);

               Manager.Passive_Index := Manager.Passive_Index + 1;

               return Manager.Queues'First;

            else
               New_Queues
                 (Manager.Queues'First .. Manager.Queues'Last) :=
                 Manager.Queues.all;
               Init (New_Queues (New_Queues'Last));
               Unchecked_Free (Manager.Queues);
               Manager.Queues := new Task_Queue_Array'(New_Queues);

               return Manager.Queues'Last;
            end if;
         end;
      end if;
   end Get_Or_Create_Task_Queue;

   ---------------
   -- Has_Queue --
   ---------------

   function Has_Queue
     (Manager  : not null access Task_Manager_Record;
      Queue_Id : String) return Boolean
   is
   begin
      if Manager.Queues = null then
         return False;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Id /= Null_Unbounded_String
           and then Manager.Queues (J).Id = Queue_Id
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Queue;

   -----------------
   -- Add_Command --
   -----------------

   procedure Add_Command
     (Manager           : not null access Task_Manager_Record;
      Command           : not null access Scheduled_Command'Class;
      Active            : Boolean;
      Show_Bar          : Boolean;
      Queue_Id          : String := "";
      Block_Exit        : Boolean := True;
      Start_Immediately : Boolean := False)
   is
      Task_Queue : Integer;
      Status     : Queue_Status := Running;
   begin
      Task_Queue := Get_Or_Create_Task_Queue
        (Manager, Queue_Id, Active, Show_Bar, Block_Exit,
         Status => Status);

      Manager.Queues (Task_Queue).Queue.Append (Command_Access (Command));
      Manager.Queues (Task_Queue).Total :=
        Manager.Queues (Task_Queue).Total + 1;

      --  Active commands are always started immediately
      if Start_Immediately and then not Active then
         case Command.Execute is
            when Success | Failure =>
               --  Can't free the command immediately, so we still queue it.
               Status := Completed;
            when Execute_Again =>
               Status := Running;
         end case;
      end if;

      Run (Task_Manager_Access (Manager),  Active);
   end Add_Command;

   -----------------------
   -- Interrupt_Queue_N --
   -----------------------

   procedure Interrupt_Queue_N
     (Manager : not null access Task_Manager_Record;
      Index   : Natural) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      Interrupt_Task (Manager, Manager.Queues (Index));

      if Index >= Manager.Passive_Index then
         Manager.Minimal_Passive_Priority := 0;
      else
         Manager.Minimal_Active_Priority := 0;
      end if;

      --  ??? How can this be null ? or could Index
      --  now point to another queue, not the original one ?
      if Manager.Queues /= null then
         if Manager.Queues (Index) /= null then
            --  Mark the queue as Completed: the actual freeing
            --  of the queue will occur at the next call to
            --  Execute_Incremental
            Manager.Queues (Index).Status := Completed;
         end if;

         --  ??? Why do we reset the priorities ?
         for K in Manager.Queues'Range loop
            Manager.Queues (K).Current_Priority := 0;
         end loop;
      end if;
   end Interrupt_Queue_N;

   ---------------------
   -- Interrupt_Queue --
   ---------------------

   procedure Interrupt_Queue
     (Manager : not null access Task_Manager_Record;
      Command : not null access Scheduled_Command'Class)
   is
      use Command_Lists;
      C : Command_Lists.Cursor;
   begin
      if Manager.Queues = null then
         return;
      end if;

      for J in Manager.Queues'Range loop
         C := Manager.Queues (J).Queue.First;
         while Has_Element (C) loop
            if Element (C) = Command_Access (Command) then
               Interrupt_Queue_N (Manager, J);
               return;
            end if;

            Next (C);
         end loop;
      end loop;
   end Interrupt_Queue;

   ---------------------
   -- Interrupt_Queue --
   ---------------------

   procedure Interrupt_Queue
     (Manager  : not null access Task_Manager_Record;
      Queue_Id : String) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Id /= Null_Unbounded_String
           and then Manager.Queues (J).Id = Queue_Id
         then
            Interrupt_Queue_N (Manager, J);
            return;
         end if;
      end loop;
   end Interrupt_Queue;

   ----------
   -- Head --
   ----------

   function Head
     (Manager : not null access Task_Manager_Record; Id : String)
     return Scheduled_Command_Access is
   begin
      if Manager.Queues /= null then
         for J in Manager.Queues'Range loop
            if Manager.Queues (J).Id /= Null_Unbounded_String
              and then Manager.Queues (J).Id = Id
            then
               if Manager.Queues (J).Queue.Is_Empty then
                  return null;
               else
                  return Scheduled_Command_Access
                     (Manager.Queues (J).Queue.First_Element);
               end if;
            end if;
         end loop;
      end if;

      return null;
   end Head;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Manager : Task_Manager_Access) is
   begin
      if Manager /= null and then Manager.Queues /= null then
         Manager.Interrupt_All_Tasks;
         for J in Manager.Queues'Range loop
            Free (Manager.Queues (J).Queue);
            Unchecked_Free (Manager.Queues (J));
         end loop;

         Unchecked_Free (Manager.Queues);
      end if;
   end Destroy;

   -------------------
   -- Pause_Command --
   -------------------

   procedure Pause_Command
     (Manager : not null access Task_Manager_Record;
      Id      : String)
   is
      Task_Q : constant Task_Queue_Access := Queue_From_Id (Manager, Id);
   begin
      if Task_Q /= null then
         if Task_Q.Status = Running then
            Task_Q.Status := Paused;
         end if;
      end if;
   end Pause_Command;

   --------------------
   -- Resume_Command --
   --------------------

   procedure Resume_Command
     (Manager : not null access Task_Manager_Record;
      Id      : String)
   is
      Task_Q : constant Task_Queue_Access := Queue_From_Id (Manager, Id);
   begin
      if Task_Q /= null then
         if Task_Q.Status = Paused then
            Task_Q.Status := Running;
            Run (Task_Manager_Access (Manager), Active => Task_Q.Active);
         end if;
      end if;
   end Resume_Command;

   ---------------------------
   -- Interrupt_Latest_Task --
   ---------------------------

   procedure Interrupt_Latest_Task
      (Manager : not null access Task_Manager_Record) is
   begin
      if Manager.Queues /= null then
         --  Suboptimal to get the ID from the queue and then look up this
         --  queue again, but this operation should be rare.
         Interrupt_Command
           (Manager,
            To_String (Manager.Queues (Manager.Queues'Last).Id));
      end if;
   end Interrupt_Latest_Task;

   -------------------------
   -- Interrupt_All_Tasks --
   -------------------------

   procedure Interrupt_All_Tasks
      (Manager : not null access Task_Manager_Record)
   is
      Commands : constant Command_Array := Get_Scheduled_Commands (Manager);
   begin
      for C in Commands'Range loop
         Interrupt_Queue (Manager, Commands (C));
      end loop;

      if Manager.Passive_Handler_Id /= No_Source_Id then
         Remove (Manager.Passive_Handler_Id);
         Manager.Passive_Handler_Id := No_Source_Id;
      end if;

      if Manager.Active_Handler_Id /= No_Source_Id then
         Remove (Manager.Active_Handler_Id);
         Manager.Active_Handler_Id := No_Source_Id;
      end if;
   end Interrupt_All_Tasks;

   ----------------------------
   -- Get_Scheduled_Commands --
   ----------------------------

   function Get_Scheduled_Commands
     (Manager : not null access Task_Manager_Record) return Command_Array
   is
      use Commands.Command_Lists;
      Total        : Integer := 0;
      Result_Index : Integer;
   begin
      if Manager.Queues = null then
         return (1 .. 0 => <>);
      end if;

      for J in Manager.Queues'Range loop
         Total := Total + Integer (Manager.Queues (J).Queue.Length);
      end loop;

      return Result : Command_Array (1 .. Total) do
         Result_Index := Result'First;
         for J in Manager.Queues'Range loop
            for C of Manager.Queues (J).Queue loop
               Result (Result_Index) := Scheduled_Command_Access (C);
               Result_Index := Result_Index + 1;
            end loop;
         end loop;
      end return;
   end Get_Scheduled_Commands;

   ------------------------------------
   -- Scheduled_Command_From_Command --
   ------------------------------------

   function Scheduled_Command_From_Command
     (Manager : not null access Task_Manager_Record;
      Command : access Root_Command'Class)
      return Scheduled_Command_Access is
   begin
      if Manager.Queues /= null and then Command /= null then
         for Q of Manager.Queues.all loop
            for C of Q.Queue loop
               if Scheduled_Command_Access (C).Get_Command =
                 Command_Access (Command)
               then
                  return Scheduled_Command_Access (C);
               end if;
            end loop;
         end loop;
      end if;
      return null;
   end Scheduled_Command_From_Command;

end Task_Manager;
