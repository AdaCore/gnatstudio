-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtkada.Types;

package body GVD.Types is

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out String_Array) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Br : in out Breakpoint_Data) is
   begin
      Free (Br.Address);
      Free (Br.Expression);
      Free (Br.Except);
      Free (Br.File);
      Free (Br.Info);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Br_Array : in out Breakpoint_Array) is
   begin
      for B in Br_Array'Range loop
         Free (Br_Array (B));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Br_Access : in out Breakpoint_Array_Ptr) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Breakpoint_Array, Breakpoint_Array_Ptr);
   begin
      if Br_Access /= null then
         Free (Br_Access.all);
         Internal_Free (Br_Access);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Exception_Access : in out Exception_Array) is
   begin
      for E in Exception_Access'Range loop
         Free (Exception_Access (E).Name);
      end loop;
   end Free;
end GVD.Types;
