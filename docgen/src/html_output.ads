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

--  This package provides the subprograms needed to create a HTML
--  documentation of the project.

--  The procedure Doc_HTML_Create of the Doc_Subprogram_Type in the
--  package Docgen.Doc_Types will be called each time a piece of the
--  HTML documentation should be created. By regarding the contents of
--  the passed parameter Info : Doc_Info the subprogram will know which
--  of the private subprograms is to be called to generate the right
--  part of the documentation.


with Ada.Text_IO;           use Ada.Text_IO;
with Doc_Types;             use Doc_Types;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

package Html_Output is

   procedure Doc_HTML_Create
     (File        : in Ada.Text_IO.File_Type;
      Info        : in out Doc_Info);
   --  This procedure is called every time the HTML files are concerned.
   --  What happens with the given information (which of the procedures
   --  below will be called) depands on the contents and the kind of the
   --  Doc_Info type.

private

   procedure Doc_HTML_Open
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  called each time a new file has been created

   procedure Doc_HTML_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  called each time the file should be closed

   procedure Doc_HTML_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a subtitle for the entity type to the documentation

   procedure Doc_HTML_Subprogram
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a subprogram to the documentation

   procedure Doc_HTML_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  add the package description to the documentation

   procedure Doc_HTML_Package
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the renamed and instantiated package to the documentation

   procedure Doc_HTML_With
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the dependencies to the documentation

   procedure Doc_HTML_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  add a constant or named number to the documentation

   procedure Doc_HTML_Exception
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add an exception to the documentation

   procedure Doc_HTML_Type
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a type to the documentation

   function Format_HTML
     (Entity_List       : Type_Entity_List.List;
      Text              : String;
      File_Name         : String;
      Entity_Name       : String;
      Entity_Line       : Natural;
      Is_Body           : Boolean;
      Do_Checks         : Boolean) return GNAT.OS_Lib.String_Access;
   --  returns the formatted Text as HTML code

   procedure Doc_HTML_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the header of a package to the documentation

   procedure Doc_HTML_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the footer of a package to the documentation

   procedure Doc_HTML_Unit_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  create the header of the index of all packages
   --  and also create the whole index.htm for the frames

   procedure Doc_HTML_Sub_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  create the header of the index of all subprograms

   procedure Doc_HTML_Type_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  create the header of the index of all types

   procedure Doc_HTML_Index_Item
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add an item to an index, used for all 3 index types

   procedure Doc_HTML_Index_End
   (File   : in Ada.Text_IO.File_Type;
    Info   : Doc_Info);
   --  add the footer to the index, used for all 3 indes files

   procedure Doc_HTML_Body
   (File   : in Ada.Text_IO.File_Type;
    Info   : in out Doc_Info);
   --  format the body by calling Format_HTML for the whole body file
   --  and write it to the doc file

   function Chars_Before    --  still used ???
     (Line    : String;
      Line_Nr : Natural) return Natural;
   --  returns the sum of the number of chars in the
   --  lines until Line_Nr-1 in the Line string

   function Get_Html_File_Name
   (File : String) return String;
   --  creates a .htm file name from the full path of the source file
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created

end Html_Output;
