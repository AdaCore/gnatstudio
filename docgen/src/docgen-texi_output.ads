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

--  This package provides the subprograms needed to create a TEXI
--  documentation of the project.

--  The procedure Doc_TEXI_Create of the Doc_Subprogram_Type in the
--  package Docgen will be called each time a piece of the TEXI
--  documentation should be created. By regarding the contents of
--  the passed parameter Info : Doc_Info the subprogram will know which
--  of the private subprograms is to be called to generate the right
--  part of the documentation.
--
--  For each processed source file a .texi file will be generated,
--  but the program will also generate some index packages
--  (unit_index.texi, sub_index.texi, type_index.texi).

--  If the -onetexi option was set, another file (project.texi) will
--  be created containing all the information of the project. It will
--  include the other texi files. If this option was set, the other
--  texi files are different from the texi files created with the
--  -texi option, as in order to be able to include them to project.texi
--  other information are needed (ie. the header info will be replaced by
--  node and chapter info for the project file and the bye removed.


with Ada.Text_IO; use Ada.Text_IO;

package Docgen.Texi_Output is

   procedure Doc_TEXI_Create
     (File : Ada.Text_IO.File_Type;
      Info : in out Doc_Info);
   --  This procedure is called every time the TEXI files are concerned.
   --  What happens with the given information (which of the procedures
   --  below will be called) depands on the contents and the kind of the
   --  Doc_Info type.

private

   procedure Doc_TEXI_Open
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Called each time a new file has been created

   procedure Doc_TEXI_Close
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Called each time the file should be closed

   procedure Doc_TEXI_Subtitle
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subtitle for the entity type to the documentation

   procedure Doc_TEXI_Entry
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add aa entry or entry family to the documentation

   procedure Doc_TEXI_Subprogram
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subprogram to the documentation

   procedure Doc_TEXI_Pack_Desc
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the package description to the documentation

   procedure Doc_TEXI_Package
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the renamed and instantiated package to the documentation

   procedure Doc_TEXI_With
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the dependencies to the documentation

   procedure Doc_TEXI_Var
     (File    : Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  Add a constant or named number to the documentation

   procedure Doc_TEXI_Exception
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an exception to the documentation

   procedure Doc_TEXI_Type
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a type to the documentation

   procedure Format_TEXI
     (File          : Ada.Text_IO.File_Type;
      Entity_List   : Type_Entity_List.List;
      Text          : String;
      File_Name     : String;
      Entity_Name   : String;
      Entity_Line   : Natural;
      Is_Body       : Boolean;
      Process_Body  : Boolean;
      Do_Checks     : Boolean);
   --  Formatted Text as TEXI code and write to the docfile.

   procedure Doc_TEXI_Unit_Index_Header
        (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all packages
   --  and also create the whole index.htm for the frames

   procedure Doc_TEXI_Sub_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all subprograms

   procedure Doc_TEXI_Type_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all types

   procedure Doc_TEXI_Index_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an item to an index, used for all 3 index types

   procedure Doc_TEXI_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the footer to the index, used for all 3 indes files

   procedure Doc_TEXI_Body
     (File   : Ada.Text_IO.File_Type;
      Info   : in out Doc_Info);
   --  Format the body by calling Format_TEXI for the whole body file
   --  and write it to the doc file

   function Get_Texi_File_Name (File : String) return String;
   --  Create a .htm file name from the full path of the source file
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created

end Docgen.Texi_Output;
