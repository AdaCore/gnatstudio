------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Commands is

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command'Class;
      Modify_Group  : Boolean);
   --  Internal version of Enqueue.
   --  Modify_Group indicates whether we should modify the current group.

   use Command_Lists;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Execute_Next_Action (Queue : Command_Queue);
   --  Execute the next action in the queue, or do nothing if there is none

   procedure Change_Group (Queue : Command_Queue);
   --  Change the current group, so that following actions are not grouped
   --  in the same undo/redo group.

   ---------------
   -- New_Queue --
   ---------------

   function New_Queue return Command_Queue is
   begin
      return new Command_Queue_Record;
   end New_Queue;

   ----------------
   -- Free_Queue --
   ----------------

   procedure Free_Queue (Q : in out Command_Queue) is
      procedure Free_Queue_Access is
        new Unchecked_Deallocation (Command_Queue_Record, Command_Queue);

      use Identifier_And_Command_List;
      C : Identifier_And_Command_List.Cursor;
      Command : Command_Access;
   begin
      if Q /= null then
         Free (Q.Undo_Queue);
         Free (Q.Redo_Queue);

         C := Q.Queue_Change_Hook.First;
         while Has_Element (C) loop
            Command := Element (C).Command;
            Unref (Command);

            C := Next (C);
         end loop;
         Q.Queue_Change_Hook.Clear;

         Free (Q.The_Queue);

         Q.Queue_Change_Hook.Clear;

         Free_Queue_Access (Q);
      end if;
   end Free_Queue;

   -----------------
   -- Empty_Queue --
   -----------------

   procedure Empty_Queue (Q : Command_Queue) is
      use Identifier_And_Command_List;
      Node : Identifier_And_Command_List.Cursor;
   begin
      Free (Q.Undo_Queue);
      Free (Q.Redo_Queue);
      Free (Q.The_Queue);

      --  Execute the queue change hooks

      Node := First (Q.Queue_Change_Hook);

      while Has_Element (Node) loop
         Execute (Element (Node).Command);
         Node := Next (Node);
      end loop;
   end Empty_Queue;

   ---------
   -- Ref --
   ---------

   procedure Ref (Command : access Root_Command'Class) is
   begin
      Command.Ref_Count := Command.Ref_Count + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Command : in out Command_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Root_Command'Class, Command_Access);
   begin
      if Command /= null then
         Command.Ref_Count := Command.Ref_Count - 1;
         if Command.Ref_Count = 0 then
            --  Do not free commands registered as actions, except if we are
            --  freeing the actions themselves
            Primitive_Free (Command.all);
            Unchecked_Free (Command);
         end if;
         Command := null;
      end if;
   end Unref;

   ----------
   -- Name --
   ----------

   function Name (Command : access Root_Command) return String is
      pragma Unreferenced (Command);
   begin
      return "Generic command";
   end Name;

   --------------
   -- Progress --
   --------------

   function Progress (Command : access Root_Command) return Progress_Record is
   begin
      return Command.Progress;
   end Progress;

   ------------------
   -- Set_Progress --
   ------------------

   procedure Set_Progress
     (Command  : access Root_Command;
      Progress : Progress_Record) is
   begin
      Command.Progress := Progress;
   end Set_Progress;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Command : in out Root_Command) is
      pragma Unreferenced (Command);
   begin
      null;
   end Interrupt;

   ----------
   -- Undo --
   ----------

   function Undo (Command : access Root_Command) return Boolean is
   begin
      Command_Finished (Command, False);
      return False;
   end Undo;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command) is
   begin
      Enqueue (Queue, Action, True);
   end Enqueue;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command'Class;
      Modify_Group  : Boolean) is
   begin
      if Queue = Null_Command_Queue then
         return;
      end if;

      Action.Queue := Queue;

      if Modify_Group then
         Action.Group := Queue.Current_Group_Number;
      end if;

      Append (Queue.The_Queue, Command_Access (Action));

      if not Queue.Command_In_Progress then
         Execute_Next_Action (Queue);
      end if;
   end Enqueue;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Queue : Command_Queue) return Integer is
   begin
      return Queue.Position;
   end Get_Position;

   -------------------------
   -- Execute_Next_Action --
   -------------------------

   procedure Execute_Next_Action (Queue : Command_Queue) is
   begin
      --  If there is already a command running, then do nothing

      if Queue.Command_In_Progress then
         return;
      end if;

      --  If the queue is empty, set its state accordingly

      if Is_Empty (Queue.The_Queue) then
         Queue.Command_In_Progress := False;
         return;
      end if;

      Queue.Command_In_Progress := True;

      declare
         Action : constant Command_Access := Queue.The_Queue.First_Element;
         Ignore : Boolean;
         Ignore_Command : Command_Return_Type;
         pragma Unreferenced (Ignore, Ignore_Command);

      begin
         Queue.The_Queue.Delete_First;

         case Action.Mode is
            when Normal | Undone =>
               Ignore_Command := Execute (Action);

            when Done =>
               Ignore := Undo (Action);
         end case;
      end;
   end Execute_Next_Action;

   -----------------------------
   -- Command_Finished_Status --
   -----------------------------

   procedure Command_Finished_Status
     (Action  : access Root_Command'Class;
      Success : in out Boolean)
   is
      Queue : Command_Queue renames Action.Queue;
      use Identifier_And_Command_List;
      Node  : Identifier_And_Command_List.Cursor;

   begin
      if Queue = null then
         --  If we are not calling the commands in "Queue" mode, do not execute
         --  the Next/Alternate command.
         return;
      end if;

      if Action.Group_Fail then
         if Has_Element (Node) and then Element (Node).Command.Group_Fail then
            --  Next action still part of the group fail, record status and
            --  keep going.
            Action.Queue.Stored_Status :=
              Action.Queue.Stored_Status and then Success;
            Success := True;

         else
            --  We reach the end of a group fail section, the status of the
            --  last action is set to false if one of the grouped action
            --  failed.
            Success := Success and then Action.Queue.Stored_Status;
            Action.Queue.Stored_Status := True;
         end if;
      end if;

      Queue.Command_In_Progress := False;

      --  And release action from the current list

      case Action.Mode is
         when Normal =>
            Action.Mode := Done;
            Prepend (Queue.Undo_Queue, Command_Access (Action));
            --  When a normal command is finished, purge the redo queue
            Free (Queue.Redo_Queue);
            Queue.Position := Queue.Position + 1;

         when Done =>
            Action.Mode := Undone;
            Prepend (Queue.Redo_Queue, Command_Access (Action));
            Queue.Position := Queue.Position - 1;

         when Undone =>
            Action.Mode := Done;
            Prepend (Queue.Undo_Queue, Command_Access (Action));
            Queue.Position := Queue.Position + 1;
      end case;

      --  Execute the registered hooks

      Node := First (Queue.Queue_Change_Hook);

      while Has_Element (Node) loop
         Execute (Element (Node).Command);
         Node := Next (Node);
      end loop;

      Execute_Next_Action (Action.Queue);
   end Command_Finished_Status;

   ----------------------
   -- Command_Finished --
   ----------------------

   procedure Command_Finished
     (Action  : access Root_Command'Class;
      Success : Boolean)
   is
      S : Boolean := Success;
   begin
      Command_Finished_Status (Action, S);
   end Command_Finished;

   ---------------------------
   -- Add_Queue_Change_Hook --
   ---------------------------

   procedure Add_Queue_Change_Hook
     (Queue      : Command_Queue;
      Command    : Command_Access;
      Identifier : String)
   is
      use Identifier_And_Command_List;
      Hook_Node : Identifier_And_Command_List.Cursor;
      Previous_Command   : Command_Access;
   begin
      Hook_Node := First (Queue.Queue_Change_Hook);

      while Has_Element (Hook_Node) loop
         if To_String (Element (Hook_Node).Identifier) = Identifier then
            Previous_Command := Element (Hook_Node).Command;
            Unref (Previous_Command);
            Queue.Queue_Change_Hook.Replace_Element
              (Hook_Node, (To_Unbounded_String (Identifier), Command));
            return;
         end if;

         Hook_Node := Next (Hook_Node);
      end loop;

      Append (Queue.Queue_Change_Hook,
              (To_Unbounded_String (Identifier), Command));
   end Add_Queue_Change_Hook;

   -------------
   -- Execute --
   -------------

   procedure Execute (Command : access Root_Command) is
      Ignore : Command_Return_Type;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Execute (Command_Access (Command));
   end Execute;

   --------------------------
   -- Get_Previous_Command --
   --------------------------

   function Get_Previous_Command
     (Queue : Command_Queue) return Command_Access is
   begin
      if not Is_Empty (Queue.Undo_Queue) then
         return First_Element (Queue.Undo_Queue);
      else
         return null;
      end if;
   end Get_Previous_Command;

   ----------------------
   -- Get_Next_Command --
   ----------------------

   function Get_Next_Command
     (Queue : Command_Queue) return Command_Access is
   begin
      if not Is_Empty (Queue.Redo_Queue) then
         return First_Element (Queue.Redo_Queue);
      else
         return null;
      end if;
   end Get_Next_Command;

   ----------
   -- Undo --
   ----------

   procedure Undo (Queue : Command_Queue) is
      Action : Command_Access;
      Group  : Natural := 0;
   begin
      while not Is_Empty (Queue.Undo_Queue) loop
         Action := First_Element (Queue.Undo_Queue);

         exit when Group /= 0 and then Group /= Action.Group;

         Queue.Group_Level := 0;
         Change_Group (Queue);

         Group := Action.Group;

         Queue.Undo_Queue.Delete_First;
         Enqueue (Queue, Action, False);

         exit when Group = 0;
      end loop;
   end Undo;

   ----------
   -- Redo --
   ----------

   procedure Redo (Queue : Command_Queue) is
      Action : Command_Access;
      Group  : Natural := 0;
   begin
      while not Is_Empty (Queue.Redo_Queue) loop
         Action := First_Element (Queue.Redo_Queue);

         exit when Group /= 0 and then Group /= Action.Group;

         Queue.Group_Level := 0;
         Change_Group (Queue);

         Group := Action.Group;

         Queue.Redo_Queue.Delete_First;
         Enqueue (Queue, Action, False);

         exit when Group = 0;
      end loop;
   end Redo;

   ----------------------
   -- Undo_Queue_Empty --
   ----------------------

   function Undo_Queue_Empty (Queue : Command_Queue) return Boolean is
   begin
      return Is_Empty (Queue.Undo_Queue);
   end Undo_Queue_Empty;

   ----------------------
   -- Redo_Queue_Empty --
   ----------------------

   function Redo_Queue_Empty (Queue : Command_Queue) return Boolean is
   begin
      return Is_Empty (Queue.Redo_Queue);
   end Redo_Queue_Empty;

   ------------------------
   -- Launch_Synchronous --
   ------------------------

   procedure Launch_Synchronous
     (Command : access Root_Command'Class;
      Wait    : Duration := 0.0)
   is
      Result : Command_Return_Type;
   begin
      loop
         Result := Execute (Command_Access (Command));

         exit when Result = Success or else Result = Failure;

         if Wait /= 0.0 then
            delay Wait;
         end if;
      end loop;
   end Launch_Synchronous;

   -----------------
   -- Start_Group --
   -----------------

   procedure Start_Group (Q : Command_Queue) is
   begin
      if Q /= null then
         Q.Group_Level := Q.Group_Level + 1;
         Change_Group (Q);
         Q.Current_Group_Number := Q.Last_Group_Number;
      end if;
   end Start_Group;

   ---------------
   -- End_Group --
   ---------------

   procedure End_Group (Q : Command_Queue) is
   begin
      if Q /= null and then Q.Group_Level > 0 then
         Q.Group_Level := Q.Group_Level - 1;
      end if;
   end End_Group;

   ------------------
   -- Change_Group --
   ------------------

   procedure Change_Group (Queue : Command_Queue) is
   begin
      if Queue.Last_Group_Number = Natural'Last then
         Queue.Last_Group_Number := 1;
      else
         Queue.Last_Group_Number := Queue.Last_Group_Number + 1;
      end if;
   end Change_Group;

   --------------------------
   -- Debug_Get_Undo_Queue --
   --------------------------

   function Debug_Get_Undo_Queue
     (Q : Command_Queue) return Command_Lists.List is
   begin
      return Q.Undo_Queue;
   end Debug_Get_Undo_Queue;

   --------------------------
   -- Debug_Get_Redo_Queue --
   --------------------------

   function Debug_Get_Redo_Queue
     (Q : Command_Queue) return Command_Lists.List is
   begin
      return Q.Redo_Queue;
   end Debug_Get_Redo_Queue;

   ---------------------
   -- Debug_Get_Group --
   ---------------------

   function Debug_Get_Group
     (C : Command_Access) return Natural is
   begin
      return C.Group;
   end Debug_Get_Group;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Command_Lists.List) is
      Command : Command_Access;
      Tmp : Command_Lists.List := List;
   begin
      --  Clear the list, so that when we unref below, we do not
      --  end up executing other commands in the list
      List.Clear;

      while not Tmp.Is_Empty loop
         Command := Tmp.First_Element;
         Unref (Command);
         Tmp.Delete_First;
      end loop;
   end Free;

   ----------
   -- Next --
   ----------

   procedure Next (List : in out Command_Lists.List) is
      Command : Command_Access;
   begin
      if not List.Is_Empty then
         Command := List.First_Element;
         List.Delete_First;
         Unref (Command);
      end if;
   end Next;

end Commands;
