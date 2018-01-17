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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Basic_Types;
with Commands;
with GNATCOLL.VFS;

with GPS.Default_Styles;           use GPS.Default_Styles;
with GPS.Editors.Line_Information;
with GPS.Kernel;

package Code_Coverage.GNATcov is

   type GNATcov_Line_Coverage_Status is
     (Undetermined,
      No_Code,
      Not_Covered,
      Partially_Covered,
      Branch_Taken,
      Branch_Fallthrough,
      Exempted_Violated,
      Exempted_Not_Violated,
      Covered_No_Branch);

   subtype GNATcov_Partially_Covered is GNATcov_Line_Coverage_Status
     range Partially_Covered .. Branch_Fallthrough;

   subtype GNATcov_Exempted_Line is GNATcov_Line_Coverage_Status
     range Exempted_Violated .. Exempted_Not_Violated;

   subtype GNATcov_Fully_Covered is GNATcov_Line_Coverage_Status
     range Covered_No_Branch .. Covered_No_Branch;

   type GNATcov_Message_Style_Categories is
     array (GNATcov_Line_Coverage_Status) of Analysis_Message_Category;

   GNATcov_Style_Categories : constant GNATcov_Message_Style_Categories :=
                                (Not_Covered                     =>
                                    High_Importance,
                                 GNATcov_Partially_Covered'Range =>
                                    Medium_Importance,
                                 others                          =>
                                    Low_Importance);

   type GNATcov_Item_Coverage is record
      Column  : Basic_Types.Visible_Column_Type;
      Message : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  One line can contain multiple coverage items, and each coverage item can
   --  have independant coverage issues.

   package Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => GNATcov_Item_Coverage);

   type GNATcov_Line_Coverage is new Code_Analysis.Line_Coverage with record
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Positive;
      --  Coordinates of the corresponding line

      Status : GNATcov_Line_Coverage_Status := Undetermined;
      --  Simple coverage status

      Items  : Item_Vectors.Vector;
      --  Detailed description about what/why not covered for each not fully
      --  covered item.
   end record;

   overriding function Is_Exempted
     (Self : GNATcov_Line_Coverage) return Boolean
   is
     (Self.Status in GNATcov_Exempted_Line);

   type GNATcov_Line_Coverage_Access is access all GNATcov_Line_Coverage'Class;

   overriding function Is_Valid (Self : GNATcov_Line_Coverage) return Boolean;

   overriding function Line_Coverage_Info
     (Coverage : access GNATcov_Line_Coverage;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Bin_Mode : Boolean := False)
      return GPS.Editors.Line_Information.Line_Information_Record;

   overriding procedure Add_Location_If_Uncovered
     (Coverage    : GNATcov_Line_Coverage;
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

   function "=" (Left, Right : GPS.Editors.Editor_Mark'Class) return Boolean;
   --  Dummy equality operator for marks. Always return False.

   package Mark_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => GPS.Editors.Editor_Mark'Class,
      "="          => "=");

   type Detail_Messages_Command is new Commands.Root_Command with record
      Line   : GNATcov_Line_Coverage_Access;
      --  Corresponding coverage line

      Kernel : GPS.Kernel.Kernel_Handle;
      --  Kernel this command applies to

      Added  : Boolean;
      --  Whether the special lines have been added

      Marks  : Mark_Vectors.Vector;
      --  When Added is True, contain markers to remove detailed messages
   end record;

   overriding function Execute
     (Self : access Detail_Messages_Command)
      return Commands.Command_Return_Type;

   overriding procedure Primitive_Free
     (Self : in out Detail_Messages_Command);

end Code_Coverage.GNATcov;
