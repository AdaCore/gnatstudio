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

--  This package is used to preload text files into the memory.
--  It provides ability for fast getting arbitrary line from that
--  preloaded file.
--  Also it transforms ASCII.HT characters to a sequence of spaces
--  using this rule:

with SN; use SN;

package File_Buffer is

   A : Segment;

   procedure Init (File_Name : String);
   --  Preloads specified file. Exceptions from Ada.Text_IO for
   --  Open/Close procedures can be raised in a case of IO errors.

   procedure Get_Line
     (Line   : in  Integer;
      Buffer : out SN.String_Access;
      Slice  : out Segment);
   --  Returns specified line from preloaded file.
   --  Result is in Buffer.all (Slice.First .. Slice.Last).
   --  User should not free Buffer by (s)heself.

   procedure Done;
   --  Signals that preloaded text file is not needed any more.

end File_Buffer;
