------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Ada.Task_Identification;

package body Templates_Parser_Tasking is

   use Ada.Task_Identification;

   --  Simple semaphore

   protected Semaphore is
      entry Lock;
      procedure Unlock;
   private
      entry Lock_Internal;
      TID        : Task_Id := Null_Task_Id;
      Lock_Count : Natural := 0;
   end Semaphore;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Semaphore.Lock;
   end Lock;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      ----------
      -- Lock --
      ----------

      entry Lock when True is
      begin
         if TID = Lock'Caller then
            Lock_Count := Lock_Count + 1;
         else
            requeue Lock_Internal;
         end if;
      end Lock;

      -------------------
      -- Lock_Internal --
      -------------------

      entry Lock_Internal when Lock_Count = 0 is
      begin
         TID := Lock_Internal'Caller;
         Lock_Count := 1;
      end Lock_Internal;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         if TID = Current_Task then
            Lock_Count := Lock_Count - 1;
         else
            raise Tasking_Error;
         end if;
      end Unlock;

   end Semaphore;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Semaphore.Unlock;
   end Unlock;

end Templates_Parser_Tasking;
