-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

--  This package provides a multi-purpose C parser designed to be very
--  fast and usable on non compilable sources.
--  Typical use of this package includes: source highlighting, block folding,
--  source reformatting, ...

with Language; use Language;

package C_Analyzer is

   procedure Analyze_C_Source
     (Buffer           : String;
      Indent_Params    : Indent_Parameters;
      Format           : Boolean               := True;
      From, To         : Natural               := 0;
      Replace          : Replace_Text_Callback := null;
      Constructs       : Construct_List_Access := null;
      Callback         : Entity_Callback       := null;
      Enable_Cpp       : Boolean               := False);
   --  Analyze a given C/C++ source in Buffer, and perform source reformatting
   --  between lines From .. To if Format is True.
   --  If Constructs is not null, store the list of constructs analyzed.
   --  If Callback is not null, call it for each Source_Entity_Kind.

end C_Analyzer;
