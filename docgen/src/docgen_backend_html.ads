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

--  This package contains the spec of the object Backend_HTML. It inherits
--  from the abstract object Backend (see docgen.ads). It's responsible
--  for the documentation in the HTML format.

with Docgen;            use Docgen;
with VFS;               use VFS;
with Src_Info;          use Src_Info;
with Ada.Text_IO;       use Ada.Text_IO;
with Src_Info.Queries;  use Src_Info.Queries;
with Glide_Kernel;      use Glide_Kernel;

package Docgen_Backend_HTML is

   use Docgen.Docgen_Backend;

   HTML_Comment_Prefix : constant String := "<FONT color=""green"">";
   HTML_Comment_Suffix : constant String := "</FONT>";
   HTML_Keyword_Prefix : constant String := "<B>";
   HTML_Keyword_Suffix : constant String := "</B>";
   HTML_String_Prefix  : constant String := "<FONT color=""red"">";
   HTML_String_Suffix  : constant String := "</FONT>";
   HTML_Char_Prefix    : constant String := "<FONT color=""red"">";
   HTML_Char_Suffix    : constant String := "</FONT>";

   type Backend_HTML is new Backend with null record;

   type Backend_HTML_Handle is access all Backend_HTML'Class;

   procedure Initialize (B : access Backend_HTML; Text : String);
   --  Initialize the private fields before the documentation process

   procedure Doc_Create
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Docgen.Doc_Info;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : Natural);
   --  Start the process producing the documentation for one file.
   --  Use the html language.

   procedure Format_Comment
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural);
   --  Generate a comment in html format.

   procedure Format_Keyword
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural);
   --  Generate a keyword in html format.

   procedure Format_String
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural);
   --  Generate a string (between two ") in html format.

   procedure Format_Character
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural);
   --  Generate a character (between two ') in html format.

   procedure Format_Identifier
     (B                   : access Backend_HTML;
      List_Ref_In_File    :  in out List_Reference_In_File.List;
      Start_Index         : Natural;
      Start_Line          : Natural;
      Start_Column        : Natural;
      End_Index           : Natural;
      End_Line            : Natural;
      Kernel              : access Kernel_Handle_Record'Class;
      File                : Ada.Text_IO.File_Type;
      LI_Unit             : LI_File_Ptr;
      Text                : String;
      File_Name           : VFS.Virtual_File;
      Entity_Line         : Natural;
      Line_In_Body        : Natural;
      Source_File_List    : Type_Source_File_List.List;
      Link_All            : Boolean;
      Is_Body             : Boolean;
      Process_Body        : Boolean);
   --  Generate an identifier in html format.

   procedure Format_Link
     (B                : access Backend_HTML;
      Start_Index      : Natural;
      Start_Line       : Natural;
      Start_Column     : Natural;
      End_Index        : Natural;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      LI_Unit          : LI_File_Ptr;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_List.List;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Loc_End          : Natural;
      Loc_Start        : Natural;
      Entity_Info      : Entity_Information;
      Entity_Abstract  : in out Boolean);
   --  Generate a link for the element Entity_Info on its declaration.

   procedure Finish
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Text        : String;
      Entity_Line : Natural);
   --  Terminate the handling of a set of code which has been analysed
   --  by Parse_Entities + Callback + Format_xxx

   function Get_Extension (B : access Backend_HTML) return String;
   --  Return the extension of doc files: ".htm"

   function Get_Doc_Directory
     (B : access Backend_HTML;
      Kernel : Kernel_Handle) return String;
   --  Return the path which must contains the documentation, e.g:
   --  "/..../gps/glide/obj/html/"

end Docgen_Backend_HTML;
