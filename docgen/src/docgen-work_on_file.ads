-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

--  This package is the part of the Docgen tool responsable for the
--  processing of the program structure information (for Ada it is ALI)
--  for the source file list passed by the procedure Docgen.

--  In the procedure Process_Files each file from the list will be passed
--  to the procedure Process_One_File, while collecting information about
--  types and subprograms of all spec files, to be able to create index
--  lists of these entities by calling the procedures Process_Type_Index,
--  Process_Subprogram_Index and Process_Unit_Index (the latter for the
--  source file list) in the package Docgen.Work_On_File.

--  The procedure Process_One_File creates for each file the lists of
--  subprograms, types, exceptions, variables and packages and passes them
--  to the procedure Process_Source in Docgen.Work_On_Source.

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Src_Info;                  use Src_Info;
with Language_Handlers;         use Language_Handlers;

package Docgen.Work_On_File is

   procedure Process_Files
     (Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options);
   --  process all files, by creating the index lists of the type
   --  and subprogram entities and by calling Process_One_File
   --  for each file from the list.

private

   procedure Process_One_File
     (Doc_File           : File_Type;
      First_File         : Boolean;
      Last_File          : Boolean;
      Source_Filename    : String;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Def_In_Line        : Integer;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Info_List   : in out Src_Info.LI_File_List;
      Handler            : in out Language_Handler;
      Project_Tree       : in out Project_Node_Id;
      Project_View       : in out Project_Id;
      Options            : All_Options;
      Process_Body_File  : Boolean);
   --  called by Process_Files for each file from the given list
   --  will examine that file and call the function Work_On_Source
   --  from Docgen.Work_On_File.

end Docgen.Work_On_File;
