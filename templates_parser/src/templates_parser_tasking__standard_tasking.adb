------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2005-2012, AdaCore                   --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
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
