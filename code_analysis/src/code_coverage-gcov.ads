------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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
with GPS.Kernel;
with GPS.Editors;
with GPS.Editors.Line_Information;

package Code_Coverage.Gcov is

   type Gcov_Line_Coverage_Status is
     (No_Code,
      Not_Covered,
      Covered,
      Undetermined);

   type Gcov_Line_Coverage is new Code_Analysis.Line_Coverage with record
      Status : Gcov_Line_Coverage_Status := Undetermined;
   end record;

   overriding function Is_Valid (Self : Gcov_Line_Coverage) return Boolean;

   overriding function Line_Coverage_Info
     (Coverage : access Gcov_Line_Coverage;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Bin_Mode : Boolean := False)
      return GPS.Editors.Line_Information.Line_Information_Record;

   overriding procedure Add_Location_If_Uncovered
     (Coverage    : Gcov_Line_Coverage;
      Kernel      : GPS.Kernel.Kernel_Handle;
      File        : GNATCOLL.VFS.Virtual_File;
      Line_Number : Positive;
      Line_Text   : String_Access;
      Added       : in out Boolean;
      Allow_Auto_Jump_To_First : Boolean);
   --  Adds location of the uncovered line to the location window. Set Added to
   --  True if line has been added; otherwise preserve Added value.

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access);
   --  Parse the File_Contents and fill the File_Node with gcov info
   --  And set Line_Count and Covered_Lines

end Code_Coverage.Gcov;
