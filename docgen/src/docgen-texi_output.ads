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

--  This package provides the subprograms needed to create a TexInfo
--  documentation of the project.

--  The procedure Doc_TEXI_Create of the Doc_Subprogram_Type in the
--  package Docgen will be called each time a piece of the TexInfo
--  documentation should be created. By regarding the contents of
--  the passed parameter Info : Doc_Info the subprogram will know which
--  of the private subprograms is to be called to generate the right
--  part of the documentation.

with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

package Docgen.Texi_Output is

   procedure Doc_TEXI_Create
     (File        : in Ada.Text_IO.File_Type;
      Info        : in out Doc_Info);
   --  This procedure is called every time the TEXI file is concerned.
   --  What happens with the given information (which of the procedures
   --  below will be called) depands on the contents and the kind of the
   --  Doc_Info type.

private

   procedure Doc_TEXI_Open
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);

   procedure Doc_TEXI_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);

   procedure Doc_TEXI_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a subtitle to the documentation

   procedure Doc_TEXI_Subprogram
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a subprogram description to the documentation

   procedure Doc_TEXI_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  add the package description to the doc file

   procedure Doc_TEXI_Package
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the renamed and instantiated packages

   procedure Doc_TEXI_With
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the dependencies to the documentation

   procedure Doc_TEXI_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  add a constant or named number descriptio to the doc.

   procedure Doc_TEXI_Exception
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add an exception description to the documentation

   procedure Doc_TEXI_Type
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a type description to the documentation

   function Format_TEXI
     (Entity_List       : Type_Entity_List.List;
      Text              : String;
      File_Name         : String;
      Entity_Name       : String;
      Is_Body           : Boolean) return GNAT.OS_Lib.String_Access;
   --  returns the formatted Text as TEXI code

   procedure Doc_TEXI_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the header to the documentation file of the package

   procedure Doc_TEXI_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the footer to the documentation file of the package

   procedure Doc_TEXI_Body
   (File   : in Ada.Text_IO.File_Type;
    Info   : in out Doc_Info);
   --  write one line of the body file, after formatting it, in the doc file

   function Chars_Before
     (Line    : String;
      Line_Nr : Natural) return Natural;
   --  returns the sum of the number of chars in the lines
   --  until Line_Nr-1 in the Line string

   function Get_TEXI_File_Name
   (File : String) return String;
   --  creates a .htm file from the full path of the source file
   --  from util/src/docgen.adb the name docgen_adb.htm is created

end Docgen.Texi_Output;
