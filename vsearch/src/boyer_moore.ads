-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  This package implements the Boyer-Moore algorithm for string searching,
--  as described in the book "Algorithms" by T. Cormen (McGrawHill edts)
--
--  A pattern must first be compiled before being searched.
--
--  Some comparisons between the string-searching algorithms provided in the
--  GNAT technology:
--    (1)  Trivial inlined loop (test string equality for each position)
--    (2)  Ada.Strings.Fixed.Index
--    (3)  GNAT.Regpat.Match
--    (4)  String_Search2.Search
--  These algorithms have been tests on a series of strings. They
--  always matched on the last line of the string.
--  The compilations were done with "-O2 -gnatN -gnatnp"
--  The time was mesured with Ada.Calendar.
--
--    File size:    21       2037     36039    180186   1801851  10811101
--    Pattern size: 11          17       15       15        15         15
--    Trivial       0.000008 0.000413 0.007039 0.036004 0.363151  2.218296
--    Index()       0.000013 0.000621 0.010739 0.055460 0.559520  error
--    Match()       0.000091 0.000449 0.005449 0.028747 0.291147  error
--    Search()      0.000092 0.000137 0.001035 0.004863 0.051008  0.257947
--
--  Converted in percent, we get:
--
--    File size:    21       2037     36039    180186   1801851  10811101
--    Pattern size: 11          17       15       15        15         15
--    Trivial        61.5%    66.5%    65.5%   64.9%    64.9%       717.4%
--    Index()       100%     100%     100%     100%     100%      error
--    Match()       700%      72.3%    50.7%    51.8%    52.0%    error
--    Search()      700%      22.1%     9.6%     8.8%     9.1%      100%
--
--  The trivial loop is something similar to:
--      for Index in Source'Range loop
--          if Source (Index .. Index + Pattern'Length - 1) = Pattern then
--              return Index;
--          end if;
--      end loop;
--
--  Complexity
--  ==========
--   n = length of the source string in which we search
--   m = length of the pattern to match
--   sigma = length of the alphabet (ie number of different letters in the
--       pattern).
--   The worst-case running time is O((n - m + 1)m + sigma).
--   However, in pratice this is often the best algorithm.

package Boyer_Moore is

   type Pattern is private;

   Max_Pattern_Length : constant := 127;
   --  Maximal length for patterns that can be searched.
   --  Changing this means that patterns will simply use more space.

   procedure Compile
     (Motif          : in out Pattern;
      From_String    : String;
      Case_Sensitive : Boolean := True);
   --  Compile the required tables to match From_String anywhere.
   --  Motif needs to be freed when you are done using it.
   --
   --  Note: A case_sensitive search is always more efficient, and should
   --  be used if you don't specifically need a case insensitive search.

   procedure Free (Motif : in out Pattern);
   --  Free the memory occupied by the motif.

   function Search (Motif : Pattern; In_String : String) return Integer;
   --  Return the location of the match for Motif in In_String, or -1 if there
   --  is no match;

private
   subtype Offset is Natural range 0 .. Max_Pattern_Length;
   --  This is the maximal offset reported by pattern. This might result in
   --  a slightly less efficient processing for patterns longer than this in
   --  extreme cases, but these are for very rare cases.

   type Occurence_Array is array (Character) of Offset;
   type Offset_Array is array (Natural range <>) of Offset;
   type Offset_Array_Access is access Offset_Array;
   type String_Access is access String;

   type Pattern is record
      Last_Occurence : Occurence_Array;
      Good_Suffix    : Offset_Array_Access;
      Motif          : String_Access;
      Case_Sensitive : Boolean;
   end record;
end Boyer_Moore;
