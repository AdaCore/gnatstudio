------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2022, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body Commands.Controls is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Undo_Redo_Information, Undo_Redo);

   -------------------------
   -- Set_Undo_Redo_Queue --
   -------------------------

   procedure Set_Undo_Redo_Queue (UR : Undo_Redo; Queue : Command_Queue) is
   begin
      if Queue /= UR.Queue then
         UR.Queue := Queue;
      end if;
   end Set_Undo_Redo_Queue;

   ---------------------------
   -- Unset_Undo_Redo_Queue --
   ---------------------------

   procedure Unset_Undo_Redo_Queue (UR : Undo_Redo) is
   begin
      UR.Queue := Null_Command_Queue;
   end Unset_Undo_Redo_Queue;

   -------------------------
   -- Get_Undo_Redo_Queue --
   -------------------------

   function Get_Undo_Redo_Queue (UR : Undo_Redo) return Command_Queue is
   begin
      return UR.Queue;
   end Get_Undo_Redo_Queue;

   ------------------------
   -- Set_Global_Command --
   ------------------------

   procedure Set_Global_Command (UR : Undo_Redo; Command : Command_Access) is
   begin
      --  Do nothing while executing the global command: can't invalidate it
      --  while it's still running.
      if UR.Executing_Global then
         return;
      end if;

      if Command /= null then
         --  Memory management to prevent the global command to be freed
         Ref (Command);
      end if;
      if UR.Global_Command /= null then
         Unref (UR.Global_Command);
      end if;
      UR.Redo_Global := False;
      UR.Global_Command := Command;
   end Set_Global_Command;

   ----------------------------
   -- Execute_Global_Command --
   ----------------------------

   function Execute_Global_Command (UR : Undo_Redo) return Command_Return_Type
   is
      Res : Command_Return_Type := Success;
   begin
      if UR.Global_Command /= null then
         UR.Executing_Global := True;
         Res := UR.Global_Command.Execute;
         UR.Executing_Global := False;
      end if;
      return Res;
   end Execute_Global_Command;

   ----------
   -- Free --
   ----------

   procedure Free (UR : in out Undo_Redo) is
   begin
      Unchecked_Free (UR);
   end Free;

   ----------
   -- Undo --
   ----------

   procedure Undo (UR : Undo_Redo)
   is
      Dummy : Boolean;
   begin
      if UR.Global_Command /= null and then not UR.Redo_Global then
         --  Undo the global command
         UR.Executing_Global := True;
         Dummy := Undo (UR.Global_Command);
         UR.Executing_Global := False;
         UR.Redo_Global := True;
      elsif UR.Queue /= Null_Command_Queue
        and then not Undo_Queue_Empty (UR.Queue)
      then
         --  Clean the global command
         Unref (UR.Global_Command);
         UR.Global_Command := null;
         --  Undo the last command in the local queue
         Undo (UR.Queue);
      end if;
   end Undo;

   ----------
   -- Redo --
   ----------

   procedure Redo (UR : Undo_Redo)
   is
      Dummy : Command_Return_Type;
   begin
      if UR.Global_Command /= null and then UR.Redo_Global then
         --  Redo the global command
         Dummy := Execute_Global_Command (UR);
         UR.Redo_Global := False;
      elsif UR.Queue /= Null_Command_Queue
        and then not Redo_Queue_Empty (UR.Queue)
      then
         --  Clean the global command
         Unref (UR.Global_Command);
         UR.Global_Command := null;
         --  Redo the previous command in the local queue
         Redo (UR.Queue);
      end if;
   end Redo;

   --------------
   -- Can_Undo --
   --------------

   function Can_Undo (UR : Undo_Redo) return Boolean is
   begin
      return
        (UR.Global_Command /= null and then not UR.Redo_Global)
        or else (UR.Queue /= Null_Command_Queue
                 and then not Undo_Queue_Empty (UR.Queue));
   end Can_Undo;

   --------------
   -- Can_Redo --
   --------------

   function Can_Redo (UR : Undo_Redo) return Boolean is
   begin
      return
        (UR.Global_Command /= null and then UR.Redo_Global)
        or else (UR.Queue /= Null_Command_Queue
                 and then not Redo_Queue_Empty (UR.Queue));
   end Can_Redo;
end Commands.Controls;
