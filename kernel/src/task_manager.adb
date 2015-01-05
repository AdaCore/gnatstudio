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

with Glib.Main;        use Glib.Main;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

package body Task_Manager is
   Me : constant Trace_Handle := Create ("TASK");

   Timeout : constant := 100;

   package Task_Manager_Idle is new Glib.Main.Generic_Sources
     (Task_Manager_Access);
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

   -----------------------
   -- Interrupt_Command --
   -----------------------

   procedure Interrupt_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         Interrupt (Manager.Queues (Index).Queue.First_Element.all);

         Manager.Queues (Index).Status := Completed;
         Queue_Changed (Manager, Index, True);
         Run (Task_Manager_Access (Manager),
              Active => Index < Manager.Passive_Index);
      end if;
   end Interrupt_Command;

   ------------------
   -- Safe_Execute --
   ------------------

   function Safe_Execute
     (Command : Command_Access) return Command_Return_Type is
   begin
      return Execute (Command);

   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Safe_Execute;

   ------------------------
   -- Active_Incremental --
   ------------------------

   function Active_Incremental
     (Manager : Task_Manager_Access) return Boolean
   is
      Ignore      : Boolean;
      Return_Type : Command_Return_Type;
      pragma Unreferenced (Ignore, Return_Type);

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

         Manager.Running_Active := False;
         return False;

      else
         Manager.Running_Active := False;
         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Active_Incremental;

   -------------------------
   -- Passive_Incremental --
   -------------------------

   function Passive_Incremental
     (Manager : Task_Manager_Access) return Boolean
   is
      Ignore      : Boolean;
      Return_Type : Command_Return_Type;
      pragma Unreferenced (Ignore, Return_Type);
   begin
      Ignore := Execute_Incremental (Manager, False);

      --  The passive loop ends when there are no more queues left

      if Manager.Queues /= null then
         return True;
      else
         Manager.Running_Passive := False;
         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
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

         if Manager.Queues'Length = 1 then
            Queue_Removed (Manager, 1);
            Unchecked_Free (Queue);
            Unchecked_Free (Manager.Queues);
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

               Queue_Removed (Manager, Index);

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
            end;
         end if;

         return True;
      end Free_Queue;

      Index : Natural := 0;

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
            Manager.Queues (Q).Current_Priority :=
              Manager.Queues (Q).Current_Priority - Previous_Prio;

            if Manager.Queues (Q).Current_Priority < Lowest then
               Lowest := Manager.Queues (Q).Current_Priority;
               Index := Q;
               Queue := Manager.Queues (Q);
            end if;
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

         if Queue.Queue.Is_Empty then
            --  The queue is empty: this can happen when scripts call the
            --  interrupt function.
            --  In this case, free the queue and return.
            return Free_Queue;
         end if;

         Command := Queue.Queue.First_Element;

         if Queue.Status = Completed then
            Result := Success;
         else
            Result := Safe_Execute (Command);
         end if;

         case Result is
            when Success | Failure =>
               --  ??? add the command to the list of done or failed commands

               if Queue.Status = Completed then
                  Free (Queue.Queue);

               else
                  Next (Queue.Queue);

                  Queue.Done := Queue.Done + 1;
               end if;
               --  If it was the last command in the queue, free the queue

               if Queue.Queue.Is_Empty then
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

         Queue_Changed (Manager, Index, False);

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
      Unused_Handler  : G_Source_Id;
      Unused_Id       : G_Source_Id;
      Result          : Command_Return_Type;
      pragma Unreferenced (Unused_Handler, Unused_Id, Result);
   begin
      if not Manager.Running_Passive then
         Manager.Running_Passive := True;

         Unused_Id := Task_Manager_Timeout.Timeout_Add
           (Timeout, Passive_Incremental'Access, Manager,
            Priority => Glib.Main.Priority_Default_Idle);
      end if;

      if Active and then not Manager.Running_Active then
         Manager.Running_Active := True;

         if Active_Incremental (Manager) then
            Unused_Handler := Task_Manager_Idle.Idle_Add
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

         return 1;

      else
         if Queue_Id /= "" then
            for J in Manager.Queues'Range loop
               if Manager.Queues (J).Id /= null
                 and then Manager.Queues (J).Id.all = Queue_Id
               then
                  Manager.Queues (J).Show_Bar := Show_Bar;
                  return J;
               end if;
            end loop;
         end if;

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

   ---------------
   -- Has_Queue --
   ---------------

   function Has_Queue
     (Manager  : Task_Manager_Access;
      Queue_Id : String) return Boolean
   is
      use GNAT.Strings;
   begin
      if Manager.Queues = null then
         return False;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Id /= null
           and then Manager.Queues (J).Id /= null
           and then Manager.Queues (J).Id.all = Queue_Id
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
      Manager.Queues (Task_Queue).Queue.Append (Command);

      Manager.Queues (Task_Queue).Total :=
        Manager.Queues (Task_Queue).Total + 1;

      Queue_Added (Manager, Task_Queue);

      Run (Manager, Active);
   end Add_Command;

   ---------------------
   -- Interrupt_Queue --
   ---------------------

   procedure Interrupt_Queue
     (Manager : Task_Manager_Access;
      Command : Command_Access)
   is
      use Command_Lists;
      Node : Cursor;
   begin
      for J in Manager.Queues'Range loop
         Node := First (Manager.Queues (J).Queue);

         while Has_Element (Node) loop
            if Element (Node) = Command then
               Interrupt_Command (Manager, J);

               if J >= Manager.Passive_Index then
                  Manager.Minimal_Passive_Priority := 0;
               else
                  Manager.Minimal_Active_Priority := 0;
               end if;

               if Manager.Queues /= null then
                  if Manager.Queues (J) /= null then
                     Free (Manager.Queues (J).Queue);
                  end if;

                  for K in Manager.Queues'Range loop
                     Manager.Queues (K).Current_Priority := 0;
                  end loop;
               end if;

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
      use GNAT.Strings;
   begin
      if Manager.Queues = null then
         return null;
      end if;

      for J in Manager.Queues'Range loop
         if Manager.Queues (J).Id /= null
           and then Manager.Queues (J).Id.all = Id
         then
            if Manager.Queues (J).Queue.Is_Empty then
               return null;
            else
               return Manager.Queues (J).Queue.First_Element;
            end if;
         end if;
      end loop;

      return null;
   end Head;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Manager : Task_Manager_Access) is
   begin
      if Manager.Queues /= null then
         for J in Manager.Queues'Range loop
            Free (Manager.Queues (J).Queue);
            GNAT.Strings.Free (Manager.Queues (J).Id);
            Unchecked_Free (Manager.Queues (J));
         end loop;

         Unchecked_Free (Manager.Queues);
      end if;
   end Destroy;

   -------------------
   -- Pause_Command --
   -------------------

   procedure Pause_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         if Manager.Queues (Index).Status = Running then
            Manager.Queues (Index).Status := Paused;
         end if;

         Queue_Changed (Manager, Index, True);
      end if;
   end Pause_Command;

   --------------------
   -- Resume_Command --
   --------------------

   procedure Resume_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         if Manager.Queues (Index).Status = Paused then
            Manager.Queues (Index).Status := Running;
            Run (Task_Manager_Access (Manager),
                 Active => Index < Manager.Passive_Index);
         end if;

         Queue_Changed (Manager, Index, True);
      end if;
   end Resume_Command;

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

   ----------------------------
   -- Get_Scheduled_Commands --
   ----------------------------

   function Get_Scheduled_Commands
     (Manager : Task_Manager_Access) return Command_Array
   is
      use Commands.Command_Lists;

      Total       : Integer := 0;
      Empty_Array : Command_Array (1 .. 0);
   begin
      if Manager.Queues = null then
         return Empty_Array;
      end if;

      for J in Manager.Queues.all'Range loop
         Total := Total + Integer (Manager.Queues (J).Queue.Length);
      end loop;

      declare
         Result       : Command_Array (1 .. Total);
         Node         : Cursor;
         Result_Index : Integer := 1;
      begin
         for J in Manager.Queues.all'Range loop
            Node := First (Manager.Queues (J).Queue);

            while Has_Element (Node) loop
               Result (Result_Index) := Element (Node);
               Node := Next (Node);
               Result_Index := Result_Index + 1;
            end loop;
         end loop;

         return Result;
      end;
   end Get_Scheduled_Commands;

end Task_Manager;
