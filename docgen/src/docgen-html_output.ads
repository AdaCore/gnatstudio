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
--  package Docgen will be called each time a piece of the HTML
--  documentation should be created. By regarding the contents of
--  the passed parameter Info : Doc_Info the subprogram will know which
--  of the private subprograms is to be called to generate the right
--  part of the documentation.
--
--  For each processed source file a .htm file will be generated,
--  but the program will also generate some index packages
--  (unit_index.htm, sub_index.htm, type_index.htm) and for using
--  frames the index file will be created too (index.htm).

with Ada.Text_IO; use Ada.Text_IO;

package Docgen.Html_Output is

   procedure Doc_HTML_Create
     (File : Ada.Text_IO.File_Type;
      Info : in out Doc_Info);
   --  This procedure is called every time the HTML files are concerned.
   --  What happens with the given information (which of the procedures
   --  below will be called) depands on the contents and the kind of the
   --  Doc_Info type.

private

   procedure Doc_HTML_Open
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Called each time a new file has been created

   procedure Doc_HTML_Close
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Called each time the file should be closed

   procedure Doc_HTML_Subtitle
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subtitle for the entity type to the documentation

   procedure Doc_HTML_Entry
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add aa entry or entry family to the documentation

   procedure Doc_HTML_Subprogram
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subprogram to the documentation

   procedure Doc_HTML_Pack_Desc
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the package description to the documentation

   procedure Doc_HTML_Package
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the renamed and instantiated package to the documentation

   procedure Doc_HTML_With
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the dependencies to the documentation

   procedure Doc_HTML_Var
     (File    : Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  Add a constant or named number to the documentation

   procedure Doc_HTML_Exception
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an exception to the documentation

   procedure Doc_HTML_Type
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a type to the documentation

   procedure Format_HTML
     (File             : Ada.Text_IO.File_Type;
      LI_Unit          : LI_File_Ptr;
      Text             : String;
      File_Name        : String;
      Entity_Name      : String;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_List.List;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Do_Check_Pack    : Boolean);
   --  Formatted Text as HTML code and write to the docfile.
   --  Line_In_Body is used only for subprograms to create not regular
   --  links (in this case it is not the line number of the declaration
   --  which is needed, but the line of the definition in the body.
   --  If Do_Check_Pack is set, the procedure will check if a link
   --  should be set to First_Package_Line or link it to its declaration
   --  line.

   procedure Doc_HTML_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the header of a package to the documentation

   procedure Doc_HTML_Footer
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the footer of a package to the documentation

   procedure Doc_HTML_Unit_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all packages
   --  and also create the whole index.htm for the frames

   procedure Doc_HTML_Sub_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all subprograms

   procedure Doc_HTML_Type_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all types

   procedure Doc_HTML_Index_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an item to an index, used for all 3 index types

   procedure Doc_HTML_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the footer to the index, used for all 3 indes files

   procedure Doc_HTML_Body
     (File   : Ada.Text_IO.File_Type;
      Info   : in out Doc_Info);
   --  Format the body by calling Format_HTML for the whole body file
   --  and write it to the doc file

   function Get_Html_File_Name (File : String) return String;
   --  Create a .htm file name from the full path of the source file
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created

end Docgen.Html_Output;
