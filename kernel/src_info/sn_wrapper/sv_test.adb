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

with Simple_Vector;

procedure SV_Test is
   --  Simple test for Simple_Vector package.
   --  Test is PASSED if test executable is finished with exit code 0
   --  and without any messages on stderr/stdout.
   --  Otherwise test is FAILED.
   subtype T is Integer;

   package IntVec is new Simple_Vector (T);
   use IntVec;

   type List is new Node_Access;

   L : List := null;

begin

   for I in 1 .. 10 loop
      Append (L, I);
      Append (I, L);
   end loop;

   pragma Assert (Size (L) = 20,
     "Incorrect Simple_Vector behaviour for 'Append' operation");

   for I in 1 .. 10 loop
      pragma Assert
        (Get_Element_At (L, I) = Integer (10 - I + 1),
         "Incorrect Simple_Vector behaviour for 'Get_Element_At' operation");
      null;
   end loop;

   Release_Vector (L);

   pragma Assert (Size (L) = 0,
     "Incorrect Simple_Vector behaviour for 'Release' operation");

end SV_Test;
