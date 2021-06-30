------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.VFS;
with GPS.Editors;

package body DAP.breakpoint_map is

   function Breakpoint_Data_Equal
     (L, R : GVD.Breakpoints_List.Breakpoint_Data) return Boolean
   is
   begin
      --  GPS.Editors.Get_Line (B.Location) and path
      --       path : constant Filesystem_String :=
      --  GNATCOLL.VFS.Full_Name (GPS.Editors.Get_File (B.Location));
      return Natural (L.Num) = Natural (R.Num); -- will be sufisent for testing
   end Breakpoint_Data_Equal;

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

   procedure Add
     (Self : in out Breakpoint_Map; B : GVD.Breakpoints_List.Breakpoint_Data)
   is
      C    : Breakpoint_Hash_Maps.Cursor;
      path : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS.Full_Name (GPS.Editors.Get_File (B.Location));
      v : Breakpoint_Vectors.Vector;
   begin
      C := Self.Find (String (path));
      if C /= No_Element then
         if not Breakpoint_Vectors.Vector (Self.Reference (C).Element.all)
             .Contains
             (B)
         then
            Breakpoint_Vectors.Vector (Self.Reference (C).Element.all).Append
              (B);
         end if;
      else
         v.Append (B);
         Self.Insert (Key => String (path), New_Item => v);
      end if;
   end Add;

   procedure Dump (Self : in out Breakpoint_Map) is
   begin
      Put_Line ("===== Dump BP Map =====");
      for C in Self.Iterate loop
         Put_Line (Key (C) & ":");
         for E in Self (C).Iterate loop
            Put_Line ("   -" & Self (C).Element (E).Num'Image);
         end loop;
      end loop;
      Put_Line ("===== Dump BP Map =====");
   end Dump;

end DAP.breakpoint_map;
