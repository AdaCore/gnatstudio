-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This package provides a dummy (but non abstract) LI_Handler, so that
--  custom languages can take advantage of Src_Info.Parse_File_Constructs.

package Src_Info.Dummy is

   type Dummy_LI_Handler_Record is new LI_Handler_Record with private;
   type Dummy_LI_Handler is access all Dummy_LI_Handler_Record'Class;

   Dummy_Handler : constant Dummy_LI_Handler;

   procedure Create_Or_Complete_LI
     (Handler         : access Dummy_LI_Handler_Record;
      File            : in out LI_File_Ptr;
      Source_Filename : String;
      List            : LI_File_List;
      Project         : Projects.Project_Type);
   --  Dummy routine. Do nothing.

   function LI_Filename_From_Source
     (Handler         : access Dummy_LI_Handler_Record;
      Source_Filename : String;
      Project         : Projects.Project_Type) return String;
   --  Dummy routine. Return "".

   function Case_Insensitive_Identifiers
     (Handler : access Dummy_LI_Handler_Record) return Boolean;
   --  Dummy routine. Return False.

   procedure Parse_All_LI_Information
     (Handler      : access Dummy_LI_Handler_Record;
      List         : LI_File_List;
      In_Directory : String;
      Project      : Projects.Project_Type);
   --  Dummy routine, do nothing.

   function Generate_LI_For_Source
     (Handler       : access Dummy_LI_Handler_Record;
      Root_Project  : Projects.Project_Type;
      File_Project  : Projects.Project_Type;
      Full_Filename : String) return LI_Handler_Iterator'Class;
   --  Dummy routine. Raise Program_Error if called.

   function Generate_LI_For_Project
     (Handler       : access Dummy_LI_Handler_Record;
      Root_Project  : Projects.Project_Type;
      Project       : Projects.Project_Type;
      Recursive     : Boolean := False) return LI_Handler_Iterator'Class;
   --  Dummy routine. Raise Program_Error if called.

private
   type Dummy_LI_Handler_Record is new LI_Handler_Record with null record;

   Dummy_Handler : constant Dummy_LI_Handler := new Dummy_LI_Handler_Record;

end Src_Info.Dummy;
