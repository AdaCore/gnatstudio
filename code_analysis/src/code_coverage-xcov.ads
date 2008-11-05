-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2008, AdaCore                     --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with GNATCOLL.VFS;
with GPS.Kernel.Standard_Hooks;

package Code_Coverage.Xcov is

   type Xcov_Line_Coverage_Status is
     (Undetermined,
      No_Code,
      Not_Covered,
      Partially_Covered,
      Branch_Partially_Covered,
      Branch_Taken,
      Branch_Fallthrough,
      Branch_Covered,
      Covered_No_Branch);

   subtype Xcov_Partially_Covered is
     Xcov_Line_Coverage_Status range Partially_Covered .. Branch_Fallthrough;

   subtype Xcov_Fully_Covered is
     Xcov_Line_Coverage_Status range Branch_Covered .. Covered_No_Branch;

   type Xcov_Line_Coverage is new Code_Analysis.Line_Coverage with record
      Status : Xcov_Line_Coverage_Status := Undetermined;
   end record;

   type Xcov_Line_Coverage_Access is access all Xcov_Line_Coverage'Class;

   overriding function Is_Valid (Self : Xcov_Line_Coverage) return Boolean;

   overriding function Line_Coverage_Info
     (Coverage : Xcov_Line_Coverage;
      Bin_Mode : Boolean := False)
      return GPS.Kernel.Standard_Hooks.Line_Information_Record;

   overriding procedure Add_Location_If_Uncovered
     (Coverage    : Xcov_Line_Coverage;
      Kernel      : GPS.Kernel.Kernel_Handle;
      File        : GNATCOLL.VFS.Virtual_File;
      Line_Number : Positive;
      Line_Text   : String_Access;
      Added       : in out Boolean);
   --  Adds location of the uncovered line to the location window. Set Added to
   --  True if line has been added; otherwise preserve Added value.

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access);
   --  Parse the File_Contents and fill the File_Node with gcov info
   --  And set Line_Count and Covered_Lines

end Code_Coverage.Xcov;
