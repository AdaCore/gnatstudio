------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               String_Utils                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2001 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body String_Utils is
   --  String utilities used in source file generation

   --------------------
   -- Skip_To_String --
   --------------------

   procedure Skip_To_String
     (Type_Str  : String;
      Index     : in out Natural;
      Substring : String)
   is
      L : constant Natural := Substring'Length - 1;
   begin
      while Index + L <= Type_Str'Last
        and then Type_Str (Index .. Index + L) /= Substring
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_String;

   ------------------
   -- To_File_Name --
   ------------------
   function To_File_Name (Name : in String) return String
   is
      Result : String (1 .. Name'Length) := To_Lower (Name);
   begin
      for J in Result'First .. Result'Last loop
         if Result (J) = '.' then
               Result (J) := '-';
         end if;
      end loop;
      return Result;
   end To_File_Name;

   --------------
   -- Ada_Case --
   --------------

   procedure Ada_Case (S : in out String) is
   begin
      S (S'First) := To_Upper (S (S'First));

      for J in S'First + 1 .. S'Last loop
         if S (J - 1) = '_' then
            S (J) := To_Upper (S (J));
         end if;
      end loop;
   end Ada_Case;

   ------------------
   -- Strip_Quotes --
   ------------------

   function Strip_Quotes (S : in String) return String is
      S_First : Integer := S'First;
      S_Last  : Integer := S'Last;
   begin
      while S (S_First) = ' '
        or else S (S_First) = '"'
      loop
         S_First := S_First + 1;
      end loop;

      while S (S_Last) = ' '
        or else S (S_Last) = '"'
      loop
         S_Last := S_Last - 1;
      end loop;
      return S (S_First .. S_Last);
   end Strip_Quotes;

end String_Utils;
