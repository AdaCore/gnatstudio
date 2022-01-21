------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.VFS;
with GPS.Editors;

package body DAP.Breakpoint_Maps is

   -----------------------------
   -- Breakpoint_Vector_Equal --
   -----------------------------

   function Breakpoint_Vector_Equal
     (L, R : Breakpoint_Vectors.Vector) return Boolean
   is
   begin
      if Positive (L.Length) /= Positive (R.Length) then
         return False;
      end if;

      for I in L.First_Index .. L.Last_Index loop
         if Breakpoint_Data_Equal (L.Element (I), R.Element (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Breakpoint_Vector_Equal;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : in out Breakpoint_Map; Data : Breakpoint_Data)
   is
      C    : Breakpoint_Hash_Maps.Cursor;
      path : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS.Full_Name (GPS.Editors.Get_File (Data.Location));
      V    : Breakpoint_Vectors.Vector;
   begin
      C := Self.Find (String (path));
      if C /= No_Element then
         if not Breakpoint_Vectors.Vector
           (Self.Reference (C).Element.all).Contains (Data)
         then
            Breakpoint_Vectors.Vector
              (Self.Reference (C).Element.all).Append (Data);
         end if;
      else
         V.Append (Data);
         Self.Insert (Key => String (path), New_Item => V);
      end if;
   end Add;

end DAP.Breakpoint_Maps;
