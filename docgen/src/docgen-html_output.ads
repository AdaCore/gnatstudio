-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Ada.Text_IO;
with Glide_Kernel;
with Docgen_Backend_HTML;       use Docgen_Backend_HTML;
with VFS;                       use VFS;

package Docgen.Html_Output is

   procedure Doc_HTML_Create
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Docgen.Doc_Info;
      Doc_Directory    : String;
      Doc_Suffix       : String);
   --  Called every time the HTML files are concerned.
   --  What happens with the given information (which of the procedures
   --  below will be called) depends on the contents and the kind of the
   --  Doc_Info type.
   --  ??? Need to document parameters, in particular List_Reg_In_File and Info

   procedure Callback_Output
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Text        : String;
      Start_Index : Natural;
      Start_Line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Prefix      : String;
      Suffix      : String;
      Entity_Line : Natural;
      Check_Tags  : Boolean);
   --  Write the formatted text since the last output to doc file.
   --  Prefix and Suffix are the HTML code to be put around the
   --  parsed entity. Both index values are needed, as for comment
   --  lines the ASCII.LF at the line should be ignored, so you can't
   --  always use the Sloc_Index values.

   procedure Set_Name_Tags
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Input_Text  : String;
      Entity_Line : Natural);
   --  Set a "<a name="line_number"> <a>" in front of each line in the
   --  given strings (if in body file) and writes it to the doc file.

   function Get_Html_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return String;
   --  Create a .htm file name from the full path of the source file
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created

end Docgen.Html_Output;
