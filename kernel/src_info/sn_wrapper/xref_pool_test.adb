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

with SN;            use SN;
with SN.Xref_Pools; use SN.Xref_Pools;
with GNAT.IO;       use GNAT.IO;

procedure Xref_Pool_Test is

   Xrefs : Xref_Pool;

   D  : String := ".";

   F1 : String := "test.c";
   F2 : String := "dir/nested.c";
   F3 : String := "a/twins.c";
   F4 : String := "b/twins.c";

begin
   Init (Xrefs);

   Put_Line (Xref_Filename_For (F1, D, Xrefs).all);
   Put_Line (Xref_Filename_For (F2, D, Xrefs).all);
   Put_Line (Xref_Filename_For (F3, D, Xrefs).all);
   Put_Line (Xref_Filename_For (F4, D, Xrefs).all);

   Save (Xrefs, "XXX"); -- save
   Save (Xrefs, "XXX"); -- overwrite XXX
   Free (Xrefs);
   Load (Xrefs, "XXX");

   Free (Xrefs);
end Xref_Pool_Test;
