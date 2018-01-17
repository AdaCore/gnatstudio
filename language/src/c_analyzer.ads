------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  This package provides a multi-purpose C parser designed to be very
--  fast and usable on non compilable sources.
--  Typical use of this package includes: source highlighting, block folding,
--  source reformatting, ...

with Language; use Language;
with GNATCOLL.Symbols;

package C_Analyzer is

   procedure Analyze_C_Source
     (Buffer           : String;
      Symbols          : GNATCOLL.Symbols.Symbol_Table_Access;
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
