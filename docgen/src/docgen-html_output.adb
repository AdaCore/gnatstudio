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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with String_Utils;              use String_Utils;
with Glide_Kernel;              use Glide_Kernel;
--  with Traces;                    use Traces;
with Basic_Types;
with Docgen;


package body Docgen.Html_Output is

   --  Me : constant Debug_Handle := Create ("Docgen-html_output");

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
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add a subtitle for the entity type to the documentation
   --  Level : level of the current package.
   --  Indent : number of space by step of indentation. This value is defined
   --  in docgen.ads (attribute Indent of the type Backend).

   procedure Doc_HTML_Entry
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add an entry or entry family to the documentation

   procedure Doc_References_HTML
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info             : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add the callgraph of the subprogram Info.
   --  Use the following subprogram.

   procedure Print_Ref_List_HTML
     (Kernel      : access Kernel_Handle_Record'Class;
      File        : in Ada.Text_IO.File_Type;
      Local_List  : Type_Reference_List.List;
      Called_Subp : Boolean;
      Level       : Natural;
      Indent      : Natural);
   --  For the current entity which is a subprogram, print the list
   --  of called subprograms (if Called_Sub = True) or the list of
   --  subprograms which call it (if Called_Sub = False).

   procedure Doc_HTML_Subprogram
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add a subprogram to the documentation

   procedure Doc_HTML_Pack_Desc
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the package description to the documentation

   procedure Doc_HTML_Package
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add the renamed and instantiated package to the documentation
   --  For an inner package, it adds its header (package ... is) and its
   --  footer (end ...;)

   procedure Doc_HTML_With
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Indent : Natural);
   --  Add the dependencies to the documentation

   procedure Doc_HTML_Var
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add a constant or named number to the documentation

   procedure Doc_HTML_Exception
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add an exception to the documentation

   procedure Doc_HTML_Type
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add a type to the documentation

   procedure Doc_Family_HTML
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add the lists of parents and children for a tagged type

   procedure Doc_HTML_Header
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the header of a package to the documentation

   procedure Doc_HTML_Header_Private
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add the title "Private"

   procedure Doc_HTML_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Kernel : access Kernel_Handle_Record'Class);
   --  Add the footer of a package to the documentation

   procedure Doc_HTML_Unit_Index_Header
     (File          : in Ada.Text_IO.File_Type;
      Info          : Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String);
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

   procedure Doc_HTML_Tagged_Type_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the tagged types index

   procedure Doc_HTML_Tagged_Type_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an item (a tagged type or its parent/child) to the tagged types
   --  index

   procedure Doc_HTML_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Close the index file

   procedure Doc_HTML_Index_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an item to an index, used for 3 index types: units, types and
   --  subprograms index

   procedure Doc_HTML_Private_Index
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Set the title "Private" in the index file

   procedure Doc_HTML_Public_Index
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Set the title "Public" in the index file

   procedure Doc_HTML_Body
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info : in out Doc_Info);
   --  Format the body by calling Format_HTML for the whole body file
   --  and write it to the doc file

   procedure Doc_HTML_Description
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural);
   --  Add the description given for the source code

   procedure Replace_HTML_Tags
     (Input_Text : String;
      File : in Ada.Text_IO.File_Type);
   --  Replaces all "<"  which are by "&lt;" and all ">" by "&gt;"
   --  and writes the output to the doc file.

   ---------------------
   -- Doc_HTML_Create --
   ---------------------

   procedure Doc_HTML_Create
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Docgen.Doc_Info;
      Doc_Directory    : String;
      Doc_Suffix       : String;
      Level            : Natural) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_HTML_Open (File, Info);
         when Close_Info            => Doc_HTML_Close (File, Info);
         when Header_Info           => Doc_HTML_Header (Kernel, File, Info);
         when Header_Private_Info   =>
            Doc_HTML_Header_Private
              (Kernel, File, Info, Level, Get_Indent (B.all));
         when Footer_Info           => Doc_HTML_Footer (File, Info, Kernel);
         when Subtitle_Info         =>
            Doc_HTML_Subtitle (File, Info, Level, Get_Indent (B.all));
         when Package_Desc_Info     => Doc_HTML_Pack_Desc (File, Info);
         when With_Info             =>
            Doc_HTML_With
              (B, Kernel, File,
               List_Ref_In_File, Info, Get_Indent (B.all));
         when Package_Info_Open_Close =>
            Doc_HTML_Package
              (B, Kernel, File, List_Ref_In_File, Info, Level,
               Get_Indent (B.all));
         when Package_Info          =>
            Doc_HTML_Package
              (B, Kernel, File, List_Ref_In_File, Info, Level,
               Get_Indent (B.all));
         when Var_Info              =>
            Doc_HTML_Var
              (B, Kernel, File, List_Ref_In_File, Info, Level,
               Get_Indent (B.all));
         when Entry_Info            =>
            Doc_HTML_Entry
              (B, Kernel, File, List_Ref_In_File, Info, Level,
               Get_Indent (B.all));
         when References_Info       =>
            Doc_References_HTML
              (Kernel, File, Info, Level,
               Get_Indent (B.all));
         when Subprogram_Info       =>
            Doc_HTML_Subprogram
              (B, Kernel, File, List_Ref_In_File, Info, Level,
               Get_Indent (B.all));
         when Type_Info             =>
            Doc_HTML_Type
              (B, Kernel, File,
               List_Ref_In_File, Info, Level, Get_Indent (B.all));
         when Tagged_Type_Info      =>
            Doc_Family_HTML
              (File, Info, Level, Get_Indent (B.all));
         when Exception_Info        =>
            Doc_HTML_Exception
              (B, Kernel, File, List_Ref_In_File, Info, Level,
               Get_Indent (B.all));
         when Description_Info     =>
            Doc_HTML_Description (File, Info, Level, Get_Indent (B.all));

            --  For the index
         when Unit_Index_Info       =>
            Doc_HTML_Unit_Index_Header
              (File, Info, Doc_Directory, Doc_Suffix);
         when Subprogram_Index_Info =>
            Doc_HTML_Sub_Index_Header (File, Info);
         when Type_Index_Info       =>
            Doc_HTML_Type_Index_Header (File, Info);
         when Tagged_Type_Index_Info =>
            Doc_HTML_Tagged_Type_Index_Header (File, Info);
         when Index_Tagged_Type_Item =>
            Doc_HTML_Tagged_Type_Item (File, Info);
         when Private_Index_Info     =>
            Doc_HTML_Private_Index (File, Info);
         when Public_Index_Info      =>
            Doc_HTML_Public_Index (File, Info);
         when Index_Item_Info       => Doc_HTML_Index_Item (File, Info);
         when End_Of_Index_Info     => Doc_HTML_Index_End  (File, Info);

            --  For the body file
         when Body_Line_Info        =>
            Doc_HTML_Body
              (B, Kernel, File, List_Ref_In_File, Info);
      end case;
   end Doc_HTML_Create;

   -------------------
   -- Doc_HTML_Open --
   -------------------

   procedure Doc_HTML_Open
     (File : Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, "<HTML>");
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<TITLE>" & Info.Open_Title.all & "</TITLE>");
      Put_Line (File, "<META NAME=""generator"" CONTENT=""DocGen"">");
      Put_Line
        (File, "<META HTTP-EQUIV=""Content-" &
         "Type"" CONTENT=""text/html; CHARSET=ISO-8859-1"">");
      Put_Line (File, "</HEAD>");
      Put_Line (File, "<BODY bgcolor=""white"">");
   end Doc_HTML_Open;

   --------------------
   -- Doc_HTML_Close --
   --------------------

   procedure Doc_HTML_Close (File : Ada.Text_IO.File_Type; Info : Doc_Info) is
      pragma Unreferenced (Info);
   begin
      Put_Line (File, "</BODY>");
      Put_Line (File, "</HTML>");
   end Doc_HTML_Close;

   -----------------------
   -- Doc_HTML_Subtitle --
   -----------------------

   procedure Doc_HTML_Subtitle
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural) is
   begin
      Put_Line
        (File,
         "<TABLE WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#9999FF""><PRE>"
         & "<H" & Image (Level) & "><B>"
         & Info.Subtitle_Name.all
         & "</B></H" & Image (Level) & ">"
         & "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Subtitle;

   ------------------------
   -- Doc_HTML_Pack_Desc --
   ------------------------

   procedure Doc_HTML_Pack_Desc
     (File : Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line
        (File,
         "<H4><PRE>" & Info.Package_Desc_Description.all & " </PRE></H4>");
   end Doc_HTML_Pack_Desc;

   ----------------------
   -- Doc_HTML_Package --
   ----------------------

   procedure Doc_HTML_Package
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural)
   is
      Space : GNAT.OS_Lib.String_Access;
   begin
      Space := new String'(1 .. Level * Indent => ' ');

      if Info.Info_Type = Package_Info then
         --  This package doesn't contain any declaration. Its header is
         --  simply printed.
         Put_Line
           (File, "  <A NAME="""
            & Image (Get_Declaration_Line_Of (Info.Package_Entity.Entity))
            & """></A>  <BR>");
         Put_Line
           (File,
            "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
            & "CELLPADDING=""0"" CELLSPACING=""0"">"
            & "<TR><TD><PRE>"
            & Space.all
            & "</PRE></TD>"
            & "<TD bgcolor=""#DDDDDD""><PRE>");

         Format_File
           (B,
            Kernel,
            File,
            List_Ref_In_File,
            Info.Doc_LI_Unit,
            Info.Package_Header.all,
            Get_Declaration_File_Of (Info.Package_Entity.Entity),
            Get_Declaration_Line_Of (Info.Package_Entity.Entity),
            No_Body_Line_Needed,
            Info.Doc_File_List,
            Info.Doc_Info_Options.Link_All,
            False,
            Info.Doc_Info_Options.Process_Body_Files,
            Info);
      else
         --  This package contains declarations.
         --  Here we print either the header (package ... is)
         --  or the footer (end ...;)
         Put_Line
           (File, "  <A NAME="""
            & Image (Get_Declaration_Line_Of
                       (Info.Package_Open_Close_Entity.Entity))
            & """></A>  <BR>");
         Put_Line
           (File,
            "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
            & "CELLPADDING=""0"" CELLSPACING=""0"">"
            & "<TR><TD><PRE>"
            & Space.all
            & "</PRE></TD>"
            & "<TD bgcolor=""#DDDDDD""><PRE>");
         Format_File
           (B,
            Kernel,
            File,
            List_Ref_In_File,
            Info.Doc_LI_Unit,
            Info.Package_Open_Close_Header.all,
            Get_Declaration_File_Of (Info.Package_Open_Close_Entity.Entity),
            Get_Declaration_Line_Of (Info.Package_Open_Close_Entity.Entity),
            No_Body_Line_Needed,
            Info.Doc_File_List,
            Info.Doc_Info_Options.Link_All,
            False,
            Info.Doc_Info_Options.Process_Body_Files,
            Info);
      end if;

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Free (Space);
   end Doc_HTML_Package;

   -------------------
   -- Doc_HTML_With --
   -------------------

   procedure Doc_HTML_With
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Indent : Natural) is
   begin
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");

      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.With_Header.all,
         Info.With_File,
         Info.With_Header_Line,
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_With;

   ------------------
   -- Doc_HTML_Var --
   ------------------

   procedure Doc_HTML_Var
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : Doc_Info;
      Level            : Natural;
      Indent           : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Var_Entity.Entity))
         & """></A>  <BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");

      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.Var_Header.all,
         Get_Declaration_File_Of (Info.Var_Entity.Entity),
         Get_Declaration_Line_Of (Info.Var_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Var;

   ------------------------
   -- Doc_HTML_Exception --
   ------------------------

   procedure Doc_HTML_Exception
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Exception_Entity.Entity))
         & """></A>  <BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");

      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.Exception_Header.all,
         Get_Declaration_File_Of (Info.Exception_Entity.Entity),
         Get_Declaration_Line_Of (Info.Exception_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Exception;

   -------------------
   -- Doc_HTML_Type --
   -------------------

   procedure Doc_HTML_Type
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : Doc_Info;
      Level            : Natural;
      Indent           : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Type_Entity.Entity))
         & """></A>  <BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");

      Format_File
        (B,
         Kernel,
         File,
         --  Entity_List,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.Type_Header.all,
         Get_Declaration_File_Of (Info.Type_Entity.Entity),
         Get_Declaration_Line_Of (Info.Type_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Type;

   -----------------------
   --  Doc_Family_HTML  --
   -----------------------

   procedure Doc_Family_HTML
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural)
   is
      use Type_List_Tagged_Element;
      use List_Entity_Handle;
      Parent_Node   : List_Entity_Handle.List_Node;
      Child_Node    : List_Entity_Handle.List_Node;
      Item_Doc_File : String_Access;
      Space         : GNAT.OS_Lib.String_Access;
   begin
      Put_Line
        (File, "<TABLE BGCOLOR=""white"" WIDTH=""100%""><TR><TD>");
      Space := new String'(1 .. Level * Indent => ' ');

      --  Print parents
      if Info.Tagged_Entity.Number_Of_Parents > 0 then
         --  There is at least one parent
         Put_Line (File, "<TR><TD><PRE>"
                   & Space.all
                   & "<B>Parents</B>"
                   & "</PRE></TD></TR>");

         Parent_Node := List_Entity_Handle.First
           (Info.Tagged_Entity.My_Parents);

         while Parent_Node /= List_Entity_Handle.Null_Node loop
            if List_Entity_Handle.Data (Parent_Node) /= null then
               if Source_File_In_List
                 (Info.Tagged_Source_File_List,
                  Get_Declaration_File_Of
                    (List_Entity_Handle.Data (Parent_Node).all))
               then
                  --  Linkage is possible
                  Item_Doc_File := new String'
                    (Base_Name (Get_Doc_File_Name
                                  (Get_Declaration_File_Of
                                     (List_Entity_Handle.Data
                                        (Parent_Node).all),
                                   Info.Tagged_Directory.all,
                                   Info.Tagged_Suffix.all)));
                  Put_Line
                    (File, "<TR><TD><PRE>"
                     & Space.all
                     & "<A HREF="""
                     & Item_Doc_File.all
                     & "#" & Image (Get_Declaration_Line_Of
                                      (List_Entity_Handle.Data
                                         (Parent_Node).all))
                     & """ TARGET=""main"">"
                     & Get_Name (List_Entity_Handle.Data (Parent_Node).all)
                     & "</A> at&nbsp;"
                     & Base_Name
                       (Get_Declaration_File_Of
                          (List_Entity_Handle.Data (Parent_Node).all))
                     & "&nbsp;"
                     & Image (Get_Declaration_Line_Of
                                (List_Entity_Handle.Data (Parent_Node).all))
                     & ":"
                     & Image
                       (Get_Declaration_Column_Of
                          (List_Entity_Handle.Data (Parent_Node).all))
                     & "</PRE></TD><TR>");
                  Free (Item_Doc_File);
               else
                  --  No link for this parent
                  Put_Line (File, "<TR><TD><PRE>"
                            & Space.all
                            & Get_Name (List_Entity_Handle.Data
                                          (Parent_Node).all)
                            & " at&nbsp;"
                            & Base_Name
                              (Get_Declaration_File_Of
                                 (List_Entity_Handle.Data (Parent_Node).all))
                            & "&nbsp;"
                            & Image
                              (Get_Declaration_Line_Of
                                 (List_Entity_Handle.Data (Parent_Node).all))
                            & ":"
                            & Image
                              (Get_Declaration_Column_Of
                                 (List_Entity_Handle.Data (Parent_Node).all))
                            & "</PRE></TD><TR>");
               end if;
            end if;

            Parent_Node
              := List_Entity_Handle.Next (Parent_Node);
         end loop;

      else
         --  There's no parent
         Put_Line (File, "<TR><TD><PRE>"
                   & Space.all
                   & "<B>No parent</B>"
                   & "</PRE></TD></TR>");
      end if;

      --  Print chidren
      if Info.Tagged_Entity.Number_Of_Children > 0 then
         --  There is at least one child
         Put_Line (File, "<TR><TD><PRE>"
                   & Space.all
                   & "<B>Children</B>"
                   & "</PRE></TD></TR>");

         Child_Node := List_Entity_Handle.First
           (Info.Tagged_Entity.My_Children);

         while Child_Node /= List_Entity_Handle.Null_Node loop
            if List_Entity_Handle.Data (Child_Node) /= null then
               if Source_File_In_List
                 (Info.Tagged_Source_File_List, Get_Declaration_File_Of
                    (List_Entity_Handle.Data (Child_Node).all))
               then
                  --  Linkage is possible
                  Item_Doc_File := new String'
                    (Base_Name (Get_Doc_File_Name
                                  (Get_Declaration_File_Of
                                     (List_Entity_Handle.Data
                                        (Child_Node).all),
                                   Info.Tagged_Directory.all,
                                   Info.Tagged_Suffix.all)));
                  Put_Line
                    (File, "<TR><TD><PRE>"
                     & Space.all
                     & "<A HREF="""
                     & Item_Doc_File.all
                     & "#" & Image (Get_Declaration_Line_Of
                                      (List_Entity_Handle.Data
                                         (Child_Node).all))
                     & """ TARGET=""main"">"
                     & Get_Name (List_Entity_Handle.Data (Child_Node).all)
                     & "</A> at&nbsp;"
                     & Base_Name
                       (Get_Declaration_File_Of
                          (List_Entity_Handle.Data (Child_Node).all))
                     & "&nbsp;"
                     & Image (Get_Declaration_Line_Of
                                (List_Entity_Handle.Data (Child_Node).all))
                     & ":"
                     & Image
                       (Get_Declaration_Column_Of
                          (List_Entity_Handle.Data (Child_Node).all))
                     & "</PRE></TD><TR>");
                  Free (Item_Doc_File);
               else
                  --  No link for this child
                  Put_Line (File, "<TR><TD><PRE>"
                            & Space.all
                            & "<B>Child object : </B>"
                            & Get_Name (List_Entity_Handle.Data
                                          (Child_Node).all)
                            & " at&nbsp;"
                            & Base_Name
                              (Get_Declaration_File_Of
                                 (List_Entity_Handle.Data (Child_Node).all))
                            & "&nbsp;"
                            & Image
                              (Get_Declaration_Line_Of
                                 (List_Entity_Handle.Data (Child_Node).all))
                            & ":"
                            & Image
                              (Get_Declaration_Column_Of
                                 (List_Entity_Handle.Data (Child_Node).all))
                            & "</PRE></TD><TR>");
               end if;
            end if;

            Child_Node
              := List_Entity_Handle.Next (Child_Node);
         end loop;
      else
         --  There's no child
         Put_Line (File, "<TR><TD><PRE>"
                   & Space.all
                   & "<B>No child</B>"
                   & "</PRE></TD></TR>");
      end if;

      Free (Space);
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_Family_HTML;

   --------------------
   -- Doc_HTML_Entry --
   --------------------

   procedure Doc_HTML_Entry
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : Doc_Info;
      Level            : Natural;
      Indent           : Natural) is
   begin
      Put_Line
        (File,
         "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Entry_Entity.Entity))
         & """></A>  <BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");

      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.Entry_Header.all,
         Get_Declaration_File_Of (Info.Entry_Entity.Entity),
         Get_Declaration_Line_Of (Info.Entry_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Entry;

   -------------------------
   -- Doc_References_HTML --
   -------------------------

   procedure Doc_References_HTML
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural) is
   begin
      Put_Line
        (File, "<TABLE BGCOLOR=""white"" WIDTH=""100%""><TR><TD>");
      Print_Ref_List_HTML
        (Kernel, File, Info.References_Entity.Called_List, True,
         Level, Indent);
      Print_Ref_List_HTML
        (Kernel, File, Info.References_Entity.Calls_List, False,
         Level, Indent);
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_References_HTML;

   ---------------------------
   --  Print_Ref_List_HTML  --
   ---------------------------

   procedure Print_Ref_List_HTML
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Local_List  : Type_Reference_List.List;
      Called_Subp : Boolean;
      Level  : Natural;
      Indent : Natural)
   is
      use Type_Reference_List;
      use type Basic_Types.String_Access;
      Node   : Type_Reference_List.List_Node;
      Space  : GNAT.OS_Lib.String_Access;
   begin
      if not Type_Reference_List.Is_Empty (Local_List) then
         Space := new String'(1 .. Level * Indent => ' ');

         if Called_Subp then
            Put_Line (File, "<TR><TD><PRE>"
                      & Space.all
                      & "<B>Subprogram is called by: </B>"
                      & "</PRE></TD><TR>");
         else
            Put_Line (File, "<TR><TD><PRE>"
                      & Space.all
                      & "<B>Subprogram calls: </B>"
                      & "</PRE></TD><TR>");
         end if;

         Node := Type_Reference_List.First (Local_List);

         --  For every reference found write the information to doc file
         while Node /= Type_Reference_List.Null_Node loop
            --  Check if the creating of a link is possible
            if Type_Reference_List.Data (Node).Set_Link then
               --  If a called subprogram => link to spec
               Put_Line
                 (File,
                  "<TR><TD><PRE>"
                  & Space.all
                  & "<A HREF="""
                  & Get_Html_File_Name
                    (Kernel,
                     Get_Declaration_File_Of
                       (Type_Reference_List.Data (Node).Entity))
                  & "#"
                  & Image
                    (Get_Declaration_Line_Of
                       (Type_Reference_List.Data (Node).Entity))
                     & """>"
                     & Get_Name (Type_Reference_List.Data (Node).Entity)
                  & "</A> at&nbsp;"
                  & Base_Name
                    (Get_Declaration_File_Of
                       (Type_Reference_List.Data (Node).Entity))
                  & "&nbsp;"
                  & Image (Get_Declaration_Line_Of
                             (Type_Reference_List.Data (Node).Entity))
                  & ":"
                  & Image
                    (Get_Declaration_Column_Of
                       (Type_Reference_List.Data (Node).Entity))
                  & "</PRE></TD><TR>");

            else
               --  No link at all
               Put_Line
                 (File,
                  "<TR><TD><PRE>"
                  & Space.all
                  & Get_Name (Type_Reference_List.Data (Node).Entity)
                  & " at&nbsp;"
                  & Base_Name
                    (Get_Declaration_File_Of
                       (Type_Reference_List.Data (Node).Entity))
                  & "&nbsp;"
                  & Image
                    (Get_Declaration_Line_Of
                       (Type_Reference_List.Data (Node).Entity))
                  & ":"
                  & Image
                    (Get_Declaration_Column_Of
                       (Type_Reference_List.Data (Node).Entity))
                  & "</PRE></TD></TR>");
            end if;

            Node := Type_Reference_List.Next (Node);
         end loop;

         Free (Space);
      end if;
   end Print_Ref_List_HTML;

   -------------------------
   -- Doc_HTML_Subprogram --
   -------------------------

   procedure Doc_HTML_Subprogram
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Subprogram_Entity.Entity))
         & """></A>  <BR> ");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Indent => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");

      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.Subprogram_Header.all,
         Get_Declaration_File_Of (Info.Subprogram_Entity.Entity),
         Get_Declaration_Line_Of (Info.Subprogram_Entity.Entity),
         Get_Line (Info.Subprogram_Entity.Line_In_Body),
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Subprogram;

   ---------------------
   -- Doc_HTML_Header --
   ---------------------

   procedure Doc_HTML_Header
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>");
      Put_Line (File, " <H1>  Package <I>");
      Put_Line (File, " <A NAME=""" & Image (First_File_Line) & """>");
      --  Static anchor used by the unit index file
      Put_Line (File, " <A NAME=""" & Image (Info.Header_Line) & """>");

      --  check if should set a link to the body file
      if Info.Header_Link then
         Put_Line
           (File, "<A HREF="""
            & Get_Html_File_Name (Kernel, Other_File_Name
                                    (Kernel, Info.Header_File))
            & """> ");
         Put_Line (File, Info.Header_Package.all & "</A></I></H1>");
      else
         Put_Line (File, Info.Header_Package.all & "</A></I></H1>");
      end if;
      Put_Line (File, "</TD></TR></TABLE>");
      Put_Line (File, "<PRE>");
   end Doc_HTML_Header;

   -----------------------------
   -- Doc_HTML_Header_Private --
   -----------------------------

   procedure Doc_HTML_Header_Private
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural)
   is
      pragma Unreferenced (Kernel);
   begin
      Put_Line
        (File,
         "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD><PRE>");

      Put_Line (File, "<H" & Image (Level) &"><B>"
                & (1 .. Level * Indent => ' ')
                & Info.Header_Title.all
                & "</B></H" & Image (Level) & ">");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_HTML_Header_Private;

   ---------------------
   -- Doc_HTML_Footer --
   ---------------------

   procedure Doc_HTML_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Kernel : access Kernel_Handle_Record'Class) is
      pragma Unreferenced (Info, Kernel);
   begin
      Put_Line (File, "</PRE>");
   end Doc_HTML_Footer;

   --------------------------------
   -- Doc_HTML_Unit_Index_Header --
   --------------------------------

   procedure Doc_HTML_Unit_Index_Header
     (File          : in Ada.Text_IO.File_Type;
      Info          : Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String)
   is
      Frame_File       : File_Type;
      Source_File_Node : constant Type_Source_File_List.List_Node :=
        Type_Source_File_List.First (Info.Unit_File_List);
   begin
      --  Create the main frame file
      Create (Frame_File, Out_File, Doc_Directory & "index.htm");
      Put_Line (Frame_File, "<HTML>");
      Put_Line (Frame_File, "<HEAD>");
      Put_Line (Frame_File, "<TITLE> Index </TITLE>");
      Put_Line (Frame_File, "</HEAD>");
      New_Line (Frame_File);
      Put_Line (Frame_File, "<FRAMESET COLS=""30%,70%"">");
      Put_Line (Frame_File, "<FRAME SRC=""index_unit.htm"" NAME=""index"" >");
      Put_Line (Frame_File, "<FRAME SRC=""" &
                Base_Name
                  (Get_Doc_File_Name
                     (Type_Source_File_List.Data (Source_File_Node).File_Name,
                      Doc_Directory, Doc_Suffix)) & """ NAME=""main"" >");
      New_Line (Frame_File);
      Put_Line (Frame_File, "</FRAMESET>");
      Put_Line (Frame_File, "<NOFRAMES>");
      Put_Line (Frame_File, "<BODY>");
      Put_Line (Frame_File, "</BODY>");
      Put_Line (Frame_File, "</NOFRAMES>");
      Put_Line (Frame_File, "</HTML>");
      Close (Frame_File);

      --  Create the header for the unit index file

      Put_Line (File, "<HTML> ");
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<BASE TARGET=""main"">");
      Put_Line (File, "<META http-equiv=""Content-Type"" " &
                "content=""text/html; charset=ISO-8859-1" & """>");
      Put_Line (File, "</HEAD>");
      Put_Line (File, "<BODY BGCOLOR=""white"">");
      Put_Line (File, "<TABLE BGCOLOR=""#9999FF"" " &
                "WIDTH=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Unit Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, "<H4> <A HREF=""index_sub.htm"" " &
                "TARGET=""index""> Subprogram Index </A> <BR>");

      if Info.Doc_Info_Options.Tagged_Types then
         Put_Line (File, "<A HREF=""index_tagged_type.htm"" " &
                   "TARGET=""index""> Tagged Type Index </A> <BR>");
      end if;

      Put_Line (File, " <A HREF=""index_type.htm"" " &
                "TARGET=""index""> Type Index </A> </H4><BR>");
      Put_Line (File, "<HR> <BR>");
   end Doc_HTML_Unit_Index_Header;

   --------------------------------
   -- Doc_HTML_Sub_Index_Header --
   --------------------------------

   procedure Doc_HTML_Sub_Index_Header
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, "<HTML> ");
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<BASE TARGET=""main"">");
      Put_Line (File, "<META http-equiv=""Content-" &
                "Type"" content=""text/html; charset=ISO-8859-1"">");
      Put_Line (File, "</HEAD>");
      New_Line (File);
      Put_Line (File, "<BODY BGCOLOR=""white"">");
      New_Line (File);
      Put_Line (File, "<TABLE  BGCOLOR=""#9999FF"" " &
                "WIDTH=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Subprogram Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      New_Line (File);
      Put_Line (File, "<H4> <A HREF=""index_unit.htm""  " &
                "target=""index""> Unit Index </A> <BR>");
      New_Line (File);

      if Info.Doc_Info_Options.Tagged_Types then
         Put_Line (File, "<A HREF=""index_tagged_type.htm""  " &
                "TARGET=""index""> Tagged Type Index </A> <BR>");
         New_Line (File);
      end if;

      Put_Line (File, " <A HREF=""index_type.htm"" " &
                "TARGET=""index""> Type Index </A> </H4><BR>");
      New_Line (File);
      Put_Line (File, "<HR> <BR>");
      New_Line (File);
   end Doc_HTML_Sub_Index_Header;

   --------------------------------
   -- Doc_HTML_Type_Index_Header --
   --------------------------------

   procedure Doc_HTML_Type_Index_Header
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, "<HTML> ");
      New_Line (File);
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<BASE TARGET=""main"">");
      New_Line (File);
      Put_Line (File, "<META http-equiv" &
                "=""Content-Type"" content=""" &
                "text/html; charset=" & "ISO-8859-1" & """>");
      Put_Line (File, "</HEAD>");
      New_Line (File);
      Put_Line (File, "<BODY BGCOLOR=""white"">");
      New_Line (File);
      Put_Line
        (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Type Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      New_Line (File);
      Put_Line (File, "<H4> <A HREF=""index_unit.htm"" " &
                "TARGET=""index""> Unit Index </A> <BR>");
      New_Line (File);

      if Info.Doc_Info_Options.Tagged_Types then
         Put_Line (File, "<A HREF=""index_tagged_type.htm"" " &
                   "TARGET=""index""> Tagged Type Index </A> <BR>");
         New_Line (File);
      end if;

      Put_Line (File, " <A HREF=""index_sub.htm"" " &
                "TARGET=""index""> Subprogram Index </A></H4> <BR>");
      New_Line (File);
      Put_Line (File, "<HR> <BR>");
      New_Line (File);
   end Doc_HTML_Type_Index_Header;

   ---------------------------------------
   -- Doc_HTML_Tagged_Type_Index_Header --
   ---------------------------------------

   procedure Doc_HTML_Tagged_Type_Index_Header
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info)
   is
      pragma Unreferenced (Info);
   begin
      Put_Line (File, "<HTML> ");
      New_Line (File);
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<BASE TARGET=""main"">");
      New_Line (File);
      Put_Line (File, "<META http-equiv" &
                "=""Content-Type"" content=""" &
                "text/html; charset=" & "ISO-8859-1" & """>");
      Put_Line (File, "</HEAD>");
      New_Line (File);
      Put_Line (File, "<BODY BGCOLOR=""white"">");
      New_Line (File);
      Put_Line
        (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Tagged Type Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      New_Line (File);
      Put_Line (File, "<H4> <A HREF=""index_unit.htm"" " &
                "TARGET=""index""> Unit Index </A> <BR>");
      New_Line (File);
      Put_Line (File, "<A HREF=""index_type.htm"" " &
                "TARGET=""index""> Type Index </A> <BR>");
      New_Line (File);
      Put_Line (File, " <A HREF=""index_sub.htm"" " &
                "TARGET=""index""> Subprogram Index </A></H4> <BR>");
      New_Line (File);
      Put_Line (File, "<HR> <BR>");
      New_Line (File);
   end Doc_HTML_Tagged_Type_Index_Header;

   -------------------------------
   -- Doc_HTML_Tagged_Type_Item --
   -------------------------------
   procedure Doc_HTML_Tagged_Type_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info)
   is
      Item_Doc_File : String_Access;
   begin
      case Info.Doc_Family is
         when Main =>
            --  The tagged type itself
            Item_Doc_File := new String'
              (Base_Name (Get_Doc_File_Name
                            (Get_Declaration_File_Of
                               (Info.Doc_Tagged_Type),
                             Info.Directory.all,
                             Info.Suffix.all)));
            Put_Line
              (File, "<BR><A HREF=""" & Item_Doc_File.all
               & "#" & Image (Get_Declaration_Line_Of (Info.Doc_Tagged_Type))
               & """ target=""main""><B>"
               & Get_Name (Info.Doc_Tagged_Type) & "</B></A><BR>");
            New_Line (File);
            Free (Item_Doc_File);

         when No_Parent =>
            Put_Line (File, "No parent.<BR>");

         when Parent_With_Link =>
            --  The parent of the tagged type is declared in one of the
            --  processed files.
            --  A link can be made.
            Item_Doc_File := new String'
              (Base_Name (Get_Doc_File_Name
                            (Get_Declaration_File_Of
                               (Info.Doc_Tagged_Type),
                             Info.Directory.all,
                             Info.Suffix.all)));
            Put_Line
              (File, "<B>Parent object : </B><A HREF=""" & Item_Doc_File.all
               & "#" & Image (Get_Declaration_Line_Of (Info.Doc_Tagged_Type))
               & """ TARGET=""main"">"
               & Get_Name (Info.Doc_Tagged_Type) & "</A><BR>");
            New_Line (File);
            Free (Item_Doc_File);

         when Parent_Without_Link =>
            --  The parent of the tagged type is not declared in the processed
            --  files. Link can't be made.
            Put_Line (File,
                      "<B>Parent object : </B>"
                      & Get_Name (Info.Doc_Tagged_Type)
                      & "<BR>");

         when No_Child =>
            Put_Line (File, "No child.<BR>");

         when Child_With_Link =>
            --  This child of the tagged type is declared in one of the
            --  processed files.
            --  Link can be made.
            Item_Doc_File := new String'
              (Base_Name (Get_Doc_File_Name
                            (Get_Declaration_File_Of
                               (Info.Doc_Tagged_Type),
                             Info.Directory.all,
                             Info.Suffix.all)));
            Put_Line
              (File, "<B>Child object : </B><A HREF=""" & Item_Doc_File.all
               & "#" & Image (Get_Declaration_Line_Of (Info.Doc_Tagged_Type))
               & """ TARGET=""main"">"
               & Get_Name (Info.Doc_Tagged_Type) & "</A><BR>");
            New_Line (File);
            Free (Item_Doc_File);

         when Child_Without_Link =>
            --  This child of the tagged type is not declared in the processed
            --  files. Link can't be made.
            Put_Line (File, "<B>Child object : </B>"
                      & Get_Name (Info.Doc_Tagged_Type)
                      & "<BR>");
      end case;
   end Doc_HTML_Tagged_Type_Item;

   -------------------------
   -- Doc_HTML_Index_Item --
   -------------------------

   procedure Doc_HTML_Index_Item
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line
        (File, " <A HREF=""" & Info.Item_Doc_File.all
         & "#" & Image (Info.Item_Line)
         & """ TARGET=""main""> "
         & Info.Item_Name.all & "</A>");

      Put_Line
        (File, " <BR> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; in " &
         Base_Name (Info.Item_File));
      New_Line (File);
      Put_Line (File, "<BR>");
      New_Line (File);
   end Doc_HTML_Index_Item;

   ----------------------------
   -- Doc_HTML_Private_Index --
   ----------------------------

   procedure Doc_HTML_Private_Index
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>");
      Put_Line (File, " <BR><b> " & Info.Private_Index_Title.all & "</b><BR>");
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_HTML_Private_Index;

   ---------------------------
   -- Doc_HTML_Public_Index --
   ---------------------------

   procedure Doc_HTML_Public_Index
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>");
      Put_Line (File, " <BR><b> " & Info.Public_Index_Title.all & "</b><BR>");
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_HTML_Public_Index;

   ------------------------
   -- Doc_HTML_Index_End --
   ------------------------

   procedure Doc_HTML_Index_End
     (File : Ada.Text_IO.File_Type; Info : Doc_Info)
   is
      pragma Unreferenced (Info);
   begin
      Put_Line (File, "</BODY> ");
      New_Line (File);
      Put_Line (File, "</HTML>");
      New_Line (File);
   end Doc_HTML_Index_End;

   -------------------
   -- Doc_HTML_Body --
   -------------------

   procedure Doc_HTML_Body
     (B      : access Backend_HTML;
      Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info   : in out Doc_Info) is
   begin
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Doc_LI_Unit,
         Info.Body_Text.all,
         Info.Body_File,
         First_File_Line,
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         True,
         Info.Doc_Info_Options.Process_Body_Files,
         Info);
   end Doc_HTML_Body;

   --------------------------
   -- Doc_HTML_Description --
   --------------------------

   procedure Doc_HTML_Description
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info;
      Level  : Natural;
      Indent : Natural)
   is
      Space : GNAT.OS_Lib.String_Access;
   begin
      Space := new String'(1 .. Level * Indent => ' ');
      Put_Line
        (File,
         "<TABLE BGCOLOR=""white"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & Space.all
         & "</PRE></TD>"
         & "<TD><PRE>"
         & "<B>Description</B>"
         & "</PRE></TD></TR>"
         & "<TR><TD><PRE>"
         & Space.all
         & "</PRE></TD>"
         & "<TD><PRE>"
         & Info.Description.all
         & "</PRE></TD></TR>"
         & "</TABLE>");
      Free (Space);
   end Doc_HTML_Description;

   ------------------------
   -- Get_Html_File_Name --
   ------------------------

   function Get_Html_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return String
   is
      pragma Unreferenced (Kernel);
      Ext  : constant String := File_Extension (File);
      Temp : constant String := Base_Name (File, Ext) & '_'
        & Ext (Ext'First + 1 .. Ext'Last) & ".htm";
   begin
      return Temp;
   end Get_Html_File_Name;

   -----------------------
   -- Replace_HTML_Tags --
   -----------------------

   procedure Replace_HTML_Tags
     (Input_Text : String;
      File : in Ada.Text_IO.File_Type)
   is
      Last_Index : Natural := Input_Text'First;
   begin
      for J in Input_Text'First .. Input_Text'Last - 1 loop
         if Input_Text (J) = '<' then
            Put (File, Input_Text (Last_Index .. J - 1) & "&lt;");
            Last_Index := J + 1;
         elsif Input_Text (J) = '>' then
            Put (File, Input_Text (Last_Index .. J - 1) & "&gt;");
            Last_Index := J + 1;
         elsif Input_Text (J) = '&' then
            Put (File, Input_Text (Last_Index .. J - 1) & "&amp;");
            Last_Index := J + 1;
         end if;
      end loop;
      Put (File, Input_Text (Last_Index .. Input_Text'Last));
   end Replace_HTML_Tags;

   ---------------------
   -- Callback_Output --
   ---------------------

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
      Check_Tags  : Boolean) is
   begin
      if Start_Line > Get_Last_Line (B.all) then
         Set_Name_Tags
           (B,
            File,
            Text (Get_Last_Index (B.all) .. Start_Index - 1),
            Entity_Line);
      else
         Put (File, Text (Get_Last_Index (B.all) .. Start_Index - 1));
      end if;

      if Check_Tags then
         Put (File, Prefix);
         Replace_HTML_Tags (Text (Start_Index .. End_Index), File);
         Put (File, Suffix);
      else
         Put (File,
              Prefix & Text (Start_Index .. End_Index) & Suffix);
      end if;

      Set_Last_Index (B.all, End_Index + 1);
      Set_Last_Line (B.all, End_Line);
   end Callback_Output;

   -------------------
   -- Set_Name_Tags --
   -------------------

   procedure Set_Name_Tags
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
      Input_Text  : String;
      Entity_Line : Natural)
   is
      HTML_Name_Head   : constant String := "<A name=""";
      HTML_Name_Middle : constant String := """>";
      HTML_Name_End    : constant String := "</A>";
      Last_Written     : Natural := Input_Text'First - 1;

   begin
      for J in Input_Text'Range loop
         if Input_Text (J) = ASCII.LF then
            Set_Last_Line (B.all, Get_Last_Line (B.all) + 1);
            Put
              (File, Input_Text (Last_Written + 1 .. J)
               & HTML_Name_Head
               & Image (Get_Last_Line (B.all) + Entity_Line - 1)
               & HTML_Name_Middle
               & HTML_Name_End);
            Last_Written := J;
         end if;
      end loop;
      Put (File, Input_Text (Last_Written + 1 .. Input_Text'Last));
   end Set_Name_Tags;

end Docgen.Html_Output;
