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

with Ada.Text_IO;           use Ada.Text_IO;
with Doc_Types;             use Doc_Types;
with Language;              use Language;

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

   procedure Doc_HTML_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);

   procedure Doc_HTML_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a subtitle to the documentation

   procedure Doc_HTML_Subprogram
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a subprogram description to the documentation

   procedure Doc_HTML_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  add the package description to the doc file

   procedure Doc_HTML_Package
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the renamed and instantiated packages

   procedure Doc_HTML_With
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the dependencies to the documentation

   procedure Doc_HTML_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  add a constant or named number descriptio to the doc.

   procedure Doc_HTML_Exception
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add an exception description to the documentation

   procedure Doc_HTML_Type
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add a type description to the documentation

   procedure Format_HTML_Entity_Header
     (File              : in Ada.Text_IO.File_Type;
      New_Entity_List   : Type_Entity_List.List;
      Here_Process_Body : Boolean;
      Nr_Lines          : Natural;
      Header            : String;
      File_Name         : String;
      Def_Line          : Natural;
      Is_Private        : Boolean);
   --  passes the headers of entities from the specs to the parser

   procedure Doc_HTML_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the header to the documentation file of the package

   procedure Doc_HTML_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add the footer to the documentation file of the package

   procedure Doc_HTML_Unit_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  create the file for the index of the packages

   procedure Doc_HTML_Sub_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  create the file for the index of all subprograms

   procedure Doc_HTML_Type_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  create the file for the index of all types

   procedure Doc_HTML_Index_Item
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  add an item to the index file, used for all 3 index files

   procedure Doc_HTML_Index_End
   (File   : in Ada.Text_IO.File_Type;
    Info   : Doc_Info);
   --  add the footer to the index file, used for all 3 indes files

   procedure Doc_HTML_Body_Line
   (File   : in Ada.Text_IO.File_Type;
    Info   : in out Doc_Info);
   --  write one line of the body file, after formatting it, in the doc file

   function HTML_Body_Callback
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean;
   --  the callback function for the parser used
   --  to format one (!) line from the body file

   function Replace_HTML_Tags
     (Input_Line : String) return String;
   --  replaces all "<" in the line by "&lt;" which are NOT in a comment

   function Chars_Before
     (Line    : String;
      Line_Nr : Natural) return Natural;
   --  returns the sum of the number of chars in the
   --  lines until Line_Nr-1 in the Line string

   function Get_Html_File_Name
   (File : String) return String;
   --  creates a .htm file from the full path of the source file
   --  from util/src/docgen.adb the name docgen_adb.htm is created

   function HTML_Spec_Callback
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean;
   --  here for each entity found in the line,
   --  the New_Line string will be formatted

end Html_Output;
