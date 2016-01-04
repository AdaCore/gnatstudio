------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Basic_Types is

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (List1, List2   : GNAT.OS_Lib.Argument_List;
      Case_Sensitive : Boolean := True;
      Ordered        : Boolean := False) return Boolean is
   begin
      if List1'Length /= List2'Length then
         return False;

      elsif Ordered then
         for L in List1'Range loop
            if List1 (L).all /= List2 (L - List1'First + List2'First).all then
               return False;
            end if;
         end loop;
         return True;

      else
         declare
            L1 : GNAT.OS_Lib.Argument_List := List1;
            L2 : GNAT.OS_Lib.Argument_List := List2;
         begin
            for A in L1'Range loop
               for B in L2'Range loop
                  if L2 (B) /= null and then
                    ((Case_Sensitive and then L1 (A).all = L2 (B).all)
                     or else
                     (not Case_Sensitive
                      and then To_Lower (L1 (A).all) = To_Lower (L2 (B).all)))
                  then
                     L1 (A) := null;
                     L2 (B) := null;
                     exit;
                  end if;
               end loop;
            end loop;

            return L1 = (L1'Range => null)
              and then L2 = (L2'Range => null);
         end;
      end if;
   end Is_Equal;

   --------------
   -- Contains --
   --------------

   function Contains
     (List           : GNAT.OS_Lib.Argument_List;
      Str            : String;
      Case_Sensitive : Boolean := True) return Boolean is
   begin
      if not Case_Sensitive then
         declare
            S : constant String := To_Lower (Str);
         begin
            for L in List'Range loop
               if To_Lower (List (L).all) = S then
                  return True;
               end if;
            end loop;
         end;
      else
         for L in List'Range loop
            if List (L).all = Str then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Contains;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Date_Type) return Boolean is
   begin
      if Left.Year < Right.Year then
         return True;
      elsif Left.Year = Right.Year then
         if Left.Month < Right.Month then
            return True;
         elsif Left.Month = Right.Month then
            if Left.Day < Right.Day then
               return True;
            end if;
         end if;
      end if;

      return False;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Date_Type) return Boolean is
   begin
      return Left < Right or else Left = Right;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Date_Type) return Boolean is
   begin
      return not (Left < Right or else Left = Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Date_Type) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

end Basic_Types;
