-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Generic_List;
with List_Utils;
with GNAT.OS_Lib;

package String_List_Utils is

   procedure String_Free (S : in out String);
   --  Free memory associated with S.

   package String_List is new Generic_List (String, Free => String_Free);

   function Copy_String_List
     (S : in String_List.List) return String_List.List;
   --  Return a deep copy of S.

   procedure Sort is new List_Utils.Sort (String_List);
   --  Sort L alphabetically.

   function Less_Than_Case_Insensitive (Item1, Item2 : String) return Boolean;
   --  Compare two strings case insensitive

   procedure Sort_Case_Insensitive is new List_Utils.Sort
     (String_List, Less_Than_Case_Insensitive);

   procedure Remove_From_List
     (L               : in out String_List.List;
      S               : String;
      All_Occurrences : Boolean := True);
   --  Remove S from L. If All_Occurrences is True, remove all occurrences,
   --  otherwise remove only the first occurrence.

   function Is_In_List
     (L : String_List.List;
      S : String)
     return Boolean;
   --  Return True if S is in L.

   function Longest_Prefix (L : String_List.List) return String;
   --  Return the longest prefix of all the strings in L. The empty string is
   --  returned if there is no common suffix.

   procedure Add_Unique_Sorted
     (L : in out String_List.List;
      S : String);
   --  Insert S in L, sorted, if S does not already exist in L.
   --  L is supposed to be sorted before calling this subprogram.

   function List_To_Argument_List
     (L : String_List.List) return GNAT.OS_Lib.Argument_List;
   --  Convert the list.
   --  The returned memory must be freed by the user

end String_List_Utils;
