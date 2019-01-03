------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

package body Commands.Controls is

   -------------------------
   -- Set_Undo_Redo_Queue --
   -------------------------

   procedure Set_Undo_Redo_Queue (Queue  : Command_Queue; UR : Undo_Redo) is
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

      --  UR.Commands still monitors the old queue, but its execution has no
      --  effect.
   end Unset_Undo_Redo_Queue;

end Commands.Controls;
