-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

package body SN is

   ---------
   -- "<" --
   ---------

   function "<" (P1, P2 : Point) return Boolean is
   begin
      if P1.Line = P2.Line then
         return P1.Column < P2.Column;
      else
         return P1.Line < P2.Line;
      end if;
   end "<";

   ------------
   -- Length --
   ------------

   function Length (S : Segment) return Integer is
   begin
      return S.Last - S.First + 1;
   end Length;

end SN;
