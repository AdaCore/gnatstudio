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

with OS_Utils;             use OS_Utils;
with String_Utils;         use String_Utils;
with Projects.Registry;    use Projects.Registry;
with Projects;             use Projects;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with File_Utils;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;

package body Docgen_Backend_HTML is

   --  Me : constant Debug_Handle := Create ("Docgen_backend_html");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Callback_Output
     (B           : access Backend_HTML;
      File        : File_Descriptor;
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

   function Get_Html_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File_Access) return String;
   --  Create a .htm file name from the full path of the source file
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created

   procedure Set_Name_Tags
     (B           : access Backend_HTML;
      File        : File_Descriptor;
      Input_Text  : String;
      Entity_Line : Natural);
   --  Set a "<a name="line_number"> <a>" in front of each line in the
   --  given strings (if in body file) and writes it to the doc file.

   procedure Print_Ref_List_HTML
     (Kernel      : access Kernel_Handle_Record'Class;
      File        : File_Descriptor;
      Local_List  : Type_Reference_List.List;
      Called_Subp : Boolean;
      Level       : Natural;
      Indent      : Natural);
   --  For the current entity which is a subprogram, print the list
   --  of called subprograms (if Called_Sub = True) or the list of
   --  subprograms which call it (if Called_Sub = False).

   procedure Replace_HTML_Tags
     (Input_Text : String;
      File       : File_Descriptor);
   --  Replaces all "<"  which are by "&lt;" and all ">" by "&gt;"
   --  and writes the output to the doc file.

   --------------
   -- Doc_Open --
   --------------

   procedure Doc_Open
     (B      : access Backend_HTML;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : File_Descriptor;
      Info   : in out Docgen.Doc_Info_Open)
   is
      pragma Unreferenced (B, Kernel);
   begin
      Put_Line (File, "<HTML>" & ASCII.LF & "<HEAD>");
      Put_Line (File, "<TITLE>" & Info.Open_Title.all & "</TITLE>");
      Put_Line
        (File,
         "<META NAME=""generator"" CONTENT=""DocGen"">" & ASCII.LF &
         "<META HTTP-EQUIV=""Content-Type"" CONTENT=""text/html; " &
         "CHARSET=ISO-8859-1"">" & ASCII.LF &
         "</HEAD>" & ASCII.LF &
         "<BODY bgcolor=""white"">");
   end Doc_Open;

   ---------------
   -- Doc_Close --
   ---------------

   procedure Doc_Close
     (B      : access Backend_HTML;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : File_Descriptor;
      Info   : in out Docgen.Doc_Info_Base)
   is
      pragma Unreferenced (B, Kernel, Info);
   begin
      Put_Line (File, "</BODY>" & ASCII.LF & "</HTML>");
   end Doc_Close;

   ------------------
   -- Doc_Subtitle --
   ------------------

   procedure Doc_Subtitle
     (B      : access Backend_HTML;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : File_Descriptor;
      Info   : in out Docgen.Doc_Info_Subtitle;
      Level  : Natural)
   is
      pragma Unreferenced (Kernel);
   begin
      Put_Line
        (File,
         "<TABLE WIDTH=""1%"" CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#9999FF"">"
         & "<H" & Image (Level) & "><B>"
         & Info.Subtitle_Name.all
         & "</B></H" & Image (Level) & ">"
         & "</TD></TR></TABLE>");
   end Doc_Subtitle;

   ----------------------
   -- Doc_Package_Desc --
   ----------------------

   procedure Doc_Package_Desc
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in File_Descriptor;
      Info             : in out Docgen.Doc_Info_Package_Desc;
      Level            : Natural)
   is
      pragma Unreferenced (B, Kernel, Level);
   begin
      Put_Line
        (File,
         "<H4><PRE>" & Info.Package_Desc_Description.all & "</PRE></H4>");
   end Doc_Package_Desc;

   -----------------
   -- Doc_Package --
   -----------------

   procedure Doc_Package
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Package;
      Level            : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Package_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Package_Header.all,
         Get_Declaration_File_Of (Info.Package_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Package_Entity.Entity),
         No_Body_Line_Needed,
         False,
         Info,
         Level,
         Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Package;

   ----------------------------
   -- Doc_Package_Open_Close --
   ----------------------------

   procedure Doc_Package_Open_Close
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Package_Open_Close;
      Level            : Natural) is
   begin
      --  This package contains declarations.
      --  Here we print either the header (package ... is)
      --  or the footer (end ...;)
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of
                    (Info.Package_Open_Close_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Package_Open_Close_Header.all,
         Get_Declaration_File_Of (Info.Package_Open_Close_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Package_Open_Close_Entity.Entity),
         No_Body_Line_Needed,
         False, Info, Level, Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Package_Open_Close;

   --------------
   -- Doc_With --
   --------------

   procedure Doc_With
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_With;
      Level            : Natural)
   is
      pragma Unreferenced (Level);
   begin
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.With_Header.all,
         Info.With_File.all,
         Info.With_Header_Line,
         No_Body_Line_Needed,
         False, Info, 0, Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_With;

   -------------
   -- Doc_Var --
   -------------

   procedure Doc_Var
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Var;
      Level            : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Var_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Var_Header.all,
         Get_Declaration_File_Of (Info.Var_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Var_Entity.Entity),
         No_Body_Line_Needed,
         False,
         Info,
         Level,
         Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Var;

   -------------------
   -- Doc_Exception --
   -------------------

   procedure Doc_Exception
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Exception;
      Level            : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Exception_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Exception_Header.all,
         Get_Declaration_File_Of (Info.Exception_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Exception_Entity.Entity),
         No_Body_Line_Needed,
         False, Info, Level, Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Exception;

   --------------
   -- Doc_Type --
   --------------

   procedure Doc_Type
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Type;
      Level            : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Type_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Type_Header.all,
         Get_Declaration_File_Of (Info.Type_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Type_Entity.Entity),
         No_Body_Line_Needed,
         False, Info, Level, Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Type;

   ---------------------
   -- Doc_Tagged_Type --
   ---------------------

   procedure Doc_Tagged_Type
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Doc_Info_Tagged_Type;
      Level            : Natural)
   is
      pragma Unreferenced (Kernel);
      use Type_List_Tagged_Element;
      use List_Entity_Handle;
      Parent_Node : List_Entity_Handle.List_Node;
      Child_Node  : List_Entity_Handle.List_Node;
      Entity      : Entity_Handle;
      F           : VFS.Virtual_File_Access;
      Space       : constant String :=
        (1 .. Level * Get_Indent (B.all) => ' ');

   begin
      Put_Line
        (File, "<TABLE BGCOLOR=""white"" WIDTH=""100%""><TR><TD>");

      --  Print parents
      if Info.Tagged_Entity.Number_Of_Parents > 0 then
         --  There is at least one parent
         Put_Line
           (File, "<TR><TD><PRE>"
            & Space
            & "<B>Parents</B>"
            & "</PRE></TD></TR>");
         Parent_Node := List_Entity_Handle.First
           (Info.Tagged_Entity.My_Parents);

         while Parent_Node /= List_Entity_Handle.Null_Node loop
            Entity := List_Entity_Handle.Data (Parent_Node);

            if Entity /= null then
               F := Get_Declaration_File_Of (Entity.all);

               if Source_File_In_List (Info.Tagged_Source_File_List, F) then
                  --  Linkage is possible
                  Put_Line
                    (File, "<TR><TD><PRE>"
                     & Space
                     & "<A HREF="""
                     & Base_Name
                       (Get_Doc_File_Name
                          (F,
                           Info.Tagged_Directory.all,
                           Info.Tagged_Suffix.all))
                     & "#" & Image (Get_Declaration_Line_Of (Entity.all))
                     & """ TARGET=""main"">"
                     & Get_Name (Entity.all)
                     & "</A> at&nbsp;"
                     & Base_Name (F.all)
                     & "&nbsp;"
                     & Image (Get_Declaration_Line_Of (Entity.all))
                     & ":"
                     & Image (Get_Declaration_Column_Of (Entity.all))
                     & "</PRE></TD><TR>");

               else
                  --  No link for this parent
                  Put_Line
                    (File, "<TR><TD><PRE>"
                     & Space
                     & Get_Name (Entity.all)
                     & " at&nbsp;"
                     & Base_Name (F.all)
                     & "&nbsp;"
                     & Image (Get_Declaration_Line_Of (Entity.all))
                     & ":"
                     & Image (Get_Declaration_Column_Of (Entity.all))
                     & "</PRE></TD><TR>");
               end if;
            end if;

            Parent_Node := List_Entity_Handle.Next (Parent_Node);
         end loop;

      else
         --  There's no parent
         Put_Line
           (File, "<TR><TD><PRE>"
            & Space
            & "<B>No parent</B>"
            & "</PRE></TD></TR>");
      end if;

      --  Print chidren
      if Info.Tagged_Entity.Number_Of_Children > 0 then
         --  There is at least one child
         Put_Line
           (File, "<TR><TD><PRE>"
            & Space
            & "<B>Children</B>"
            & "</PRE></TD></TR>");

         Child_Node := List_Entity_Handle.First
           (Info.Tagged_Entity.My_Children);

         while Child_Node /= List_Entity_Handle.Null_Node loop
            Entity := List_Entity_Handle.Data (Child_Node);

            if Entity /= null then
               F := Get_Declaration_File_Of (Entity.all);

               if Source_File_In_List (Info.Tagged_Source_File_List, F) then
                  --  Linkage is possible

                  Put_Line
                    (File, "<TR><TD><PRE>"
                     & Space
                     & "<A HREF="""
                     & Base_Name
                       (Get_Doc_File_Name
                          (F,
                           Info.Tagged_Directory.all,
                           Info.Tagged_Suffix.all))
                     & "#" & Image (Get_Declaration_Line_Of (Entity.all))
                     & """ TARGET=""main"">"
                     & Get_Name (Entity.all)
                     & "</A> at&nbsp;"
                     & Base_Name (F.all)
                     & "&nbsp;"
                     & Image (Get_Declaration_Line_Of (Entity.all))
                     & ":"
                     & Image (Get_Declaration_Column_Of (Entity.all))
                     & "</PRE></TD><TR>");

               else
                  --  No link for this child
                  Put_Line
                    (File, "<TR><TD><PRE>"
                     & Space
                     & Get_Name (Entity.all)
                     & " at&nbsp;"
                     & Base_Name (F.all)
                     & "&nbsp;"
                     & Image (Get_Declaration_Line_Of (Entity.all))
                     & ":"
                     & Image (Get_Declaration_Column_Of (Entity.all))
                     & "</PRE></TD><TR>");
               end if;
            end if;

            Child_Node := List_Entity_Handle.Next (Child_Node);
         end loop;
      else
         --  There's no child
         Put_Line
           (File, "<TR><TD><PRE>" & Space & "<B>No child</B></PRE></TD></TR>");
      end if;

      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_Tagged_Type;

   ---------------
   -- Doc_Entry --
   ---------------

   procedure Doc_Entry
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Entry;
      Level            : Natural) is
   begin
      Put_Line
        (File,
         "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Entry_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Entry_Header.all,
         Get_Declaration_File_Of (Info.Entry_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Entry_Entity.Entity),
         No_Body_Line_Needed,
         False, Info, Level, Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Entry;

   --------------------
   -- Doc_References --
   --------------------

   procedure Doc_References
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Doc_Info_References;
      Level            : Natural) is
   begin
      Put_Line
        (File, "<TABLE BGCOLOR=""white"" WIDTH=""100%""><TR><TD>");
      Print_Ref_List_HTML
        (Kernel, File, Info.References_Entity.Called_List, True,
         Level, Get_Indent (B.all));
      Print_Ref_List_HTML
        (Kernel, File, Info.References_Entity.Calls_List, False,
         Level, Get_Indent (B.all));
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_References;

   -------------------------
   -- Print_Ref_List_HTML --
   -------------------------

   procedure Print_Ref_List_HTML
     (Kernel      : access Kernel_Handle_Record'Class;
      File        : File_Descriptor;
      Local_List  : Type_Reference_List.List;
      Called_Subp : Boolean;
      Level       : Natural;
      Indent      : Natural)
   is
      use Type_Reference_List;
      Node : Type_Reference_List.List_Node;
      Data : Type_Reference_List.Data_Access;
      F    : VFS.Virtual_File_Access;

   begin
      if Type_Reference_List.Is_Empty (Local_List) then
         return;
      end if;

      declare
         Space : constant String := (1 .. Level * Indent => ' ');
      begin
         if Called_Subp then
            Put_Line
              (File, "<TR><TD><PRE>"
               & Space
               & "<B>Subprogram is called by: </B>"
               & "</PRE></TD><TR>");
         else
            Put_Line
              (File, "<TR><TD><PRE>"
               & Space
               & "<B>Subprogram calls: </B>"
               & "</PRE></TD><TR>");
         end if;

         Node := Type_Reference_List.First (Local_List);

         --  For every reference found write the information to doc file

         while Node /= Type_Reference_List.Null_Node loop
            --  Check if the creating of a link is possible

            Data := Type_Reference_List.Data_Ref (Node);
            F    := Get_Declaration_File_Of (Data.Entity);

            if Data.Set_Link then
               --  If a called subprogram => link to spec

               Put_Line
                 (File,
                  "<TR><TD><PRE>"
                  & Space
                  & "<A HREF="""
                  & Get_Html_File_Name (Kernel, F)
                  & "#"
                  & Image (Get_Declaration_Line_Of (Data.Entity))
                  & """>"
                  & Get_Name (Data.Entity)
                  & "</A> declared at&nbsp;"
                  & Base_Name (F.all)
                  & "&nbsp;"
                  & Image (Get_Declaration_Line_Of (Data.Entity))
                  & ":"
                  & Image (Get_Declaration_Column_Of (Data.Entity))
                  & "</PRE></TD><TR>");

            else
               --  No link at all
               Put_Line
                 (File,
                  "<TR><TD><PRE>"
                  & Space
                  & Get_Name (Data.Entity)
                  & " declared at&nbsp;"
                  & Base_Name (F.all)
                  & "&nbsp;"
                  & Image (Get_Declaration_Line_Of (Data.Entity))
                  & ":"
                  & Image (Get_Declaration_Column_Of (Data.Entity))
                  & "</PRE></TD></TR>");
            end if;

            Node := Type_Reference_List.Next (Node);
         end loop;
      end;
   end Print_Ref_List_HTML;

   --------------------
   -- Doc_Subprogram --
   --------------------

   procedure Doc_Subprogram
     (B                : access Backend_HTML;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Doc_Info_Subprogram;
      Level            : Natural) is
   begin
      Put_Line
        (File, "  <A NAME="""
         & Image (Get_Declaration_Line_Of (Info.Subprogram_Entity.Entity))
         & """></A><BR>");
      Put_Line
        (File,
         "<TABLE BGCOLOR=""WHITE"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & "</PRE></TD>"
         & "<TD bgcolor=""#DDDDDD""><PRE>");
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Subprogram_Header.all,
         Get_Declaration_File_Of (Info.Subprogram_Entity.Entity).all,
         Get_Declaration_Line_Of (Info.Subprogram_Entity.Entity),
         Get_Line (Info.Subprogram_Entity.Line_In_Body),
         False, Info, Level, Get_Indent (B.all));
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Subprogram;

   ----------------
   -- Doc_Header --
   ----------------

   procedure Doc_Header
     (B       : access Backend_HTML;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      LI_Unit : LI_File_Ptr;
      File    : File_Descriptor;
      Info    : in out Docgen.Doc_Info_Header)
   is
      pragma Unreferenced (B);
   begin
      Put_Line
        (File,
         "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>" & ASCII.LF &
         " <H1>Package<I>" & ASCII.LF &
         " <A NAME=""" & Image (First_File_Line) & """>");
      --  Static anchor used by the unit index file
      Put_Line (File, " <A NAME=""" & Image (Info.Header_Line) & """>");

      --  Check if should set a link to the body file

      if Info.Header_Link then
         Put_Line
           (File, "<A HREF=""" &
            Get_Html_File_Name
              (Kernel, Get_Other_File_Of (LI_Unit, Info.Header_File.all))
            & """> ");
         Put_Line (File, Info.Header_Package.all & "</A></I></H1>");

      else
         Put_Line (File, Info.Header_Package.all & "</A></I></H1>");
      end if;

      Put_Line (File, "</TD></TR></TABLE>" & ASCII.LF & "<PRE>");
   end Doc_Header;

   ------------------------
   -- Doc_Header_Private --
   ------------------------

   procedure Doc_Header_Private
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Doc_Info_Header_Private;
      Level            : Natural)
   is
      pragma Unreferenced (Kernel);
   begin
      Put_Line
        (File,
         "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD><PRE>");
      Put_Line
        (File,
         "<H" & Image (Level) & "><B>"
         & (1 .. Level * Get_Indent (B.all) => ' ')
         & Info.Header_Title.all
         & "</B></H" & Image (Level) & ">");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Doc_Header_Private;

   ----------------
   -- Doc_Footer --
   ----------------

   procedure Doc_Footer
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Footer)
   is
      pragma Unreferenced (B, Kernel, Info);
   begin
      Put_Line (File, "</PRE>");
   end Doc_Footer;

   --------------------
   -- Doc_Unit_Index --
   --------------------

   procedure Doc_Unit_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Unit_Index;
      Level            : Natural;
      Doc_Directory    : String;
      Doc_Suffix       : String)
   is
      pragma Unreferenced (B, Kernel, Level);
      Frame_File       : File_Descriptor;
      Source_File_Node : constant Type_Source_File_List.List_Node :=
        Type_Source_File_List.First (Info.Unit_File_List);

   begin
      --  Create the main frame file
      Frame_File := Create_File (Doc_Directory & "index.htm", Binary);
      Put_Line
        (Frame_File,
         "<HTML>" & ASCII.LF &
         "<HEAD>" & ASCII.LF &
         "<TITLE> Index </TITLE>" & ASCII.LF &
         "</HEAD>" & ASCII.LF &
         "<FRAMESET COLS=""30%,70%"">" & ASCII.LF &
         "<FRAME SRC=""index_unit.htm"" NAME=""index"" >");
      Put_Line
        (Frame_File,
         "<FRAME SRC=""" &
         Base_Name
           (Get_Doc_File_Name
              (Type_Source_File_List.Data_Ref
                 (Source_File_Node).File_Name'Unchecked_Access,
               Doc_Directory, Doc_Suffix)) & """ NAME=""main"" >");
      Put_Line
        (Frame_File,
         "</FRAMESET>" & ASCII.LF &
         "<NOFRAMES>" & ASCII.LF &
         "<BODY></BODY>" & ASCII.LF &
         "</NOFRAMES>" & ASCII.LF &
         "</HTML>");
      Close (Frame_File);

      --  Create the header for the unit index file

      Put_Line
        (File,
         "<HTML>" & ASCII.LF &
         "<HEAD>" & ASCII.LF &
         "<BASE TARGET=""main"">" & ASCII.LF &
         "<META http-equiv=""Content-Type"" " &
         "content=""text/html; charset=ISO-8859-1" & """>" & ASCII.LF &
         "</HEAD>" & ASCII.LF &
         "<BODY BGCOLOR=""white"">" & ASCII.LF &
         "<TABLE BGCOLOR=""#9999FF"" " &
         "WIDTH=""100%""><TR><TD> <PRE>" & ASCII.LF &
         "<H2>Unit Index</H2>" & ASCII.LF &
         "</PRE></TD></TR></TABLE>" & ASCII.LF &
         "<H4> <A HREF=""index_sub.htm"" " &
         "TARGET=""index"">Subprogram Index</A><BR>");

      if Info.Doc_Info_Options.Tagged_Types then
         Put_Line (File, "<A HREF=""index_tagged_type.htm"" " &
                   "TARGET=""index""> Tagged Type Index </A><BR>");
      end if;

      Put_Line
        (File,
         "<A HREF=""index_type.htm"" " &
         "TARGET=""index""> Type Index </A> </H4><BR>" & ASCII.LF &
         "<HR><BR>");
   end Doc_Unit_Index;

   --------------------------
   -- Doc_Subprogram_Index --
   --------------------------

   procedure Doc_Subprogram_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Subprogram_Index)
   is
      pragma Unreferenced (B, Kernel);
   begin
      Put_Line
        (File,
         "<HTML> " & ASCII.LF &
         "<HEAD>" & ASCII.LF &
         "<BASE TARGET=""main"">" & ASCII.LF &
         "<META http-equiv=""Content-" &
         "Type"" content=""text/html; charset=ISO-8859-1"">" & ASCII.LF &
         "</HEAD>" & ASCII.LF &
         "<BODY BGCOLOR=""white"">" & ASCII.LF &
         "<TABLE  BGCOLOR=""#9999FF"" " &
         "WIDTH=""100%""><TR><TD> <PRE>" & ASCII.LF &
         "<H2>Subprogram Index</H2>" & ASCII.LF &
         "</PRE></TD></TR></TABLE>" & ASCII.LF &
         "<H4> <A HREF=""index_unit.htm""  " &
         "target=""index"">Unit Index</A><BR>");

      if Info.Doc_Info_Options.Tagged_Types then
         Put_Line
           (File,
            "<A HREF=""index_tagged_type.htm""  " &
            "TARGET=""index"">Tagged Type Index</A><BR>");
      end if;

      Put_Line
        (File,
         "<A HREF=""index_type.htm"" " &
         "TARGET=""index"">Type Index</A></H4><BR>" & ASCII.LF &
         "<HR><BR>");
   end Doc_Subprogram_Index;

   --------------------
   -- Doc_Type_Index --
   --------------------

   procedure Doc_Type_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in File_Descriptor;
      Info             : in out Docgen.Doc_Info_Type_Index)
   is
      pragma Unreferenced (B, Kernel);
   begin
      Put_Line
        (File,
         "<HTML>" & ASCII.LF &
         "<HEAD>" & ASCII.LF &
         "<BASE TARGET=""main"">" & ASCII.LF &
         "<META http-equiv=""Content-Type"" content=""" &
         "text/html; charset=" & "ISO-8859-1" & """>" & ASCII.LF &
         "</HEAD>");
      Put_Line
        (File,
         "<BODY BGCOLOR=""white"">" & ASCII.LF & ASCII.LF &
         "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD> <PRE>" &
         ASCII.LF &
         "<H2> Type Index </H2> " & ASCII.LF &
         "</PRE></TD></TR></TABLE>" & ASCII.LF & ASCII.LF &
         "<H4> <A HREF=""index_unit.htm"" " &
         "TARGET=""index"">Unit Index</A><BR>");

      if Info.Doc_Info_Options.Tagged_Types then
         Put_Line (File, "<A HREF=""index_tagged_type.htm"" " &
                   "TARGET=""index""> Tagged Type Index </A><BR>");
      end if;

      Put_Line
        (File,
         " <A HREF=""index_sub.htm"" " &
         "TARGET=""index"">Subprogram Index</A></H4><BR>" &
         ASCII.LF & "<HR><BR>");
   end Doc_Type_Index;

   ---------------------------
   -- Doc_Tagged_Type_Index --
   ---------------------------

   procedure Doc_Tagged_Type_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Tagged_Type_Index)
   is
      pragma Unreferenced (B, Kernel, Info);
   begin
      Put_Line
        (File,
         "<HTML>" & ASCII.LF &
         "<HEAD>" & ASCII.LF &
         "<BASE TARGET=""main"">" & ASCII.LF &
         "<META http-equiv" &
         "=""Content-Type"" content=""" &
         "text/html; charset=" & "ISO-8859-1" & """>" & ASCII.LF &
         "</HEAD>" & ASCII.LF & ASCII.LF &
         "<BODY BGCOLOR=""white"">" & ASCII.LF &
         "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>" &
         "<PRE>" & ASCII.LF &
         "<H2>Tagged Type Index</H2>" & ASCII.LF &
         "</PRE></TD></TR></TABLE>" & ASCII.LF &
         "<H4><A HREF=""index_unit.htm"" " &
         "TARGET=""index"">Unit Index</A><BR>" & ASCII.LF &
         "<A HREF=""index_type.htm"" " &
         "TARGET=""index"">Type Index</A><BR>" & ASCII.LF &
         " <A HREF=""index_sub.htm"" " &
         "TARGET=""index"">Subprogram Index</A></H4><BR>" & ASCII.LF &
         "<HR><BR>");
   end Doc_Tagged_Type_Index;

   ---------------------------
   -- Doc_Index_Tagged_Type --
   ---------------------------

   procedure Doc_Index_Tagged_Type
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Index_Tagged_Type)
   is
      pragma Unreferenced (B, Kernel);
   begin
      case Info.Doc_Family is
         when Main =>
            --  The tagged type itself
            Put_Line
              (File,
               "<BR><A HREF="""
               & Base_Name
                 (Get_Doc_File_Name
                    (Get_Declaration_File_Of (Info.Doc_Tagged_Type),
                     Info.Directory.all,
                     Info.Suffix.all))
               & "#" & Image (Get_Declaration_Line_Of (Info.Doc_Tagged_Type))
               & """ target=""main""><B>"
               & Get_Name (Info.Doc_Tagged_Type) & "</B></A><BR>" & ASCII.LF);

         when No_Parent =>
            Put_Line (File, "No parent.<BR>");

         when Parent_With_Link =>
            --  The parent of the tagged type is declared in one of the
            --  processed files.
            --  A link can be made.
            Put_Line
              (File, "<B>Parent object: </B><A HREF=""" &
               Base_Name
                 (Get_Doc_File_Name
                    (Get_Declaration_File_Of (Info.Doc_Tagged_Type),
                     Info.Directory.all,
                     Info.Suffix.all))
               & "#" & Image (Get_Declaration_Line_Of (Info.Doc_Tagged_Type))
               & """ TARGET=""main"">"
               & Get_Name (Info.Doc_Tagged_Type) & "</A><BR>"
               & ASCII.LF);

         when Parent_Without_Link =>
            --  The parent of the tagged type is not declared in the processed
            --  files. Link can't be made.
            Put_Line (File,
                      "<B>Parent object: </B>"
                      & Get_Name (Info.Doc_Tagged_Type)
                      & "<BR>");

         when No_Child =>
            Put_Line (File, "No child.<BR>");

         when Child_With_Link =>
            --  This child of the tagged type is declared in one of the
            --  processed files.
            --  Link can be made.
            Put_Line
              (File, "<B>Child object: </B><A HREF=""" &
               Base_Name
                 (Get_Doc_File_Name
                    (Get_Declaration_File_Of (Info.Doc_Tagged_Type),
                     Info.Directory.all,
                     Info.Suffix.all))
               & "#" & Image (Get_Declaration_Line_Of (Info.Doc_Tagged_Type))
               & """ TARGET=""main"">"
               & Get_Name (Info.Doc_Tagged_Type) & "</A><BR>" & ASCII.LF);

         when Child_Without_Link =>
            --  This child of the tagged type is not declared in the processed
            --  files. Link can't be made.
            Put_Line (File, "<B>Child object: </B>"
                      & Get_Name (Info.Doc_Tagged_Type)
                      & "<BR>");
      end case;
   end Doc_Index_Tagged_Type;

   --------------------
   -- Doc_Index_Item --
   --------------------

   procedure Doc_Index_Item
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Index_Item)
   is
      pragma Unreferenced (B, Kernel);
   begin
      Put_Line
        (File, " <A HREF=""" & Info.Item_Doc_File.all
         & "#" & Image (Info.Item_Line)
         & """ TARGET=""main""> "
         & Info.Item_Name.all & "</A>");
      Put_Line
        (File, " <BR>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; in " &
         Base_Name (Info.Item_File.all) &
         ASCII.LF & ASCII.LF & "<BR>" & ASCII.LF);
   end Doc_Index_Item;

   -----------------------
   -- Doc_Private_Index --
   -----------------------

   procedure Doc_Private_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Private_Index)
   is
      pragma Unreferenced (B, Kernel);
   begin
      Put_Line (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>");
      Put_Line (File, " <BR><B>" & Info.Private_Index_Title.all & "</B><BR>");
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_Private_Index;

   ----------------------
   -- Doc_Public_Index --
   ----------------------

   procedure Doc_Public_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Public_Index)
   is
      pragma Unreferenced (B, Kernel);
   begin
      Put_Line (File, "<TABLE BGCOLOR=""#9999FF"" WIDTH=""100%""><TR><TD>");
      Put_Line (File, " <BR><b> " & Info.Public_Index_Title.all & "</b><BR>");
      Put_Line (File, "</TD></TR></TABLE>");
   end Doc_Public_Index;

   ----------------------
   -- Doc_End_Of_Index --
   ----------------------

   procedure Doc_End_Of_Index
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_End_Of_Index)
   is
      pragma Unreferenced (B, Kernel, Info);
   begin
      Put_Line (File, "</BODY>" & ASCII.LF & "</HTML>");
   end Doc_End_Of_Index;

   -------------------
   -- Doc_Body_Line --
   -------------------

   procedure Doc_Body_Line
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Docgen.Doc_Info_Body_Line;
      Level            : Natural) is
   begin
      Format_File
        (B,
         Kernel,
         File,
         List_Ref_In_File,
         Info.Body_Text.all,
         Info.Body_File.all,
         First_File_Line,
         No_Body_Line_Needed,
         True, Info, Level, Get_Indent (B.all));
   end Doc_Body_Line;

   ---------------------
   -- Doc_Description --
   ---------------------

   procedure Doc_Description
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : File_Descriptor;
      Info             : in out Docgen.Doc_Info_Description;
      Level            : Natural)
   is
      pragma Unreferenced (Kernel);
      Space : constant String := (1 .. Level * Get_Indent (B.all) => ' ');
   begin
      Put_Line
        (File,
         "<TABLE BGCOLOR=""white"" WIDTH=""1%"" "
         & "CELLPADDING=""0"" CELLSPACING=""0"">"
         & "<TR><TD><PRE>" & Space & "</PRE></TD>"
         & "<TD><PRE><I>" & Info.Description.all & "</I></PRE></TD></TR>"
         & "</TABLE>");
   end Doc_Description;

   ------------------------
   -- Get_Html_File_Name --
   ------------------------

   function Get_Html_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File_Access) return String
   is
      pragma Unreferenced (Kernel);
      Ext  : constant String := File_Extension (File.all);
      Temp : constant String := Base_Name (File.all, Ext) & '_'
        & Ext (Ext'First + 1 .. Ext'Last) & ".htm";
   begin
      return Temp;
   end Get_Html_File_Name;

   -----------------------
   -- Replace_HTML_Tags --
   -----------------------

   procedure Replace_HTML_Tags
     (Input_Text : String;
      File       : File_Descriptor)
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
      File        : File_Descriptor;
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
      File        : File_Descriptor;
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (B : access Backend_HTML; Text : String) is
   begin
      Set_Last_Line (B.all, 0);
      Set_Last_Index (B.all, Text'First);
   end Initialize;

   --------------------
   -- Format_Comment --
   --------------------

   procedure Format_Comment
     (B           : access Backend_HTML;
      File        : File_Descriptor;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B,
         File,
         Text,
         Start_Index,
         Start_line,
         End_Index,
         End_Line,
         HTML_Comment_Prefix,
         HTML_Comment_Suffix,
         Entity_Line,
         False);
   end Format_Comment;

   --------------------
   -- Format_Keyword --
   --------------------

   procedure Format_Keyword
     (B           : access Backend_HTML;
      File        : File_Descriptor;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B,
         File,
         Text,
         Start_Index,
         Start_line,
         End_Index,
         End_Line,
         HTML_Keyword_Prefix,
         HTML_Keyword_Suffix,
         Entity_Line,
         False);
   end Format_Keyword;

   -------------------
   -- Format_String --
   -------------------

   procedure Format_String
     (B           : access Backend_HTML;
      File        : File_Descriptor;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B,
         File,
         Text,
         Start_Index,
         Start_line,
         End_Index,
         End_Line,
         HTML_String_Prefix,
         HTML_String_Suffix,
         Entity_Line,
         True);
   end Format_String;

   ----------------------
   -- Format_Character --
   ----------------------

   procedure Format_Character
     (B           : access Backend_HTML;
      File        : File_Descriptor;
      Text        : String;
      Start_Index : Natural;
      Start_line  : Natural;
      End_Index   : Natural;
      End_Line    : Natural;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B,
         File,
         Text,
         Start_Index,
         Start_line,
         End_Index,
         End_Line,
         HTML_Char_Prefix,
         HTML_Char_Suffix,
         Entity_Line,
         False);
   end Format_Character;

   -----------------------
   -- Format_Identifier --
   -----------------------

   procedure Format_Identifier
     (B                   : access Backend_HTML;
      List_Ref_In_File    : in out List_Reference_In_File.List;
      Start_Index         : Natural;
      Start_Line          : Natural;
      Start_Column        : Natural;
      End_Index           : Natural;
      End_Line            : Natural;
      Kernel              : access Kernel_Handle_Record'Class;
      File                : File_Descriptor;
      LI_Unit             : LI_File_Ptr;
      Text                : String;
      File_Name           : VFS.Virtual_File;
      Entity_Line         : Natural;
      Line_In_Body        : Natural;
      Source_File_List    : Type_Source_File_List.List;
      Link_All            : Boolean;
      Is_Body             : Boolean;
      Process_Body        : Boolean;
      Level               : Natural;
      Indent              : Natural)
   is
      pragma Unreferenced (End_Line);
      Line_Body : Natural := Line_In_Body;

   begin
      --  In html, each identifier may have a link,
      --  Each link is made by the subprogram Format_Link (see just below).
      --  But before this step, we must search for declaration: this is done
      --  in Format_All_Link (whose body contains the call to Format_Link.

      Format_All_Link
        (B,
         List_Ref_In_File,
         Start_Index,
         Start_Line,
         Start_Column,
         End_Index,
         Kernel,
         File,
         LI_Unit,
         Text,
         File_Name,
         Entity_Line,
         Line_Body,
         Source_File_List,
         Link_All,
         Is_Body,
         Process_Body,
         Level,
         Indent);
   end Format_Identifier;

   -----------------
   -- Format_Link --
   -----------------

   procedure Format_Link
     (B                : access Backend_HTML;
      Start_Index      : Natural;
      Start_Line       : Natural;
      Start_Column     : Natural;
      End_Index        : Natural;
      Kernel           : access Kernel_Handle_Record'Class;
      File             : File_Descriptor;
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
      Entity_Abstract  : in out Boolean)
   is
      pragma Unreferenced (Start_Index, Start_Column, End_Index);

      procedure Create_Regular_Link;
      --  will create a regular link to the entity, links to both spec
      --  and body files are possible.

      procedure Create_Special_Link_To_Body;
      --  Create a link to the reference of the entity in the body

      function Link_Should_Be_Set return Boolean;
      --  Check if a link to that entity should be set

      function Special_Link_Should_Be_Set return Boolean;
      --  Check if a special link to the body should be set
      --  (a special link, because it doesn't link to the declaration
      --  of the entity, but to a reference somewhere in the body)

      function Regular_Link_Should_Be_Set return Boolean;
      --  Check if a regular link to the body should be set
      --  (a regular link is a link to the entity's declaration)

      ---------------------------------
      -- Create_Special_Link_To_Body --
      ---------------------------------

      procedure Create_Special_Link_To_Body is
      begin
         if Start_Line > Get_Last_Line (B.all) then
            Set_Name_Tags
              (B,
               File,
               Text (Get_Last_Index (B.all) .. Loc_Start - 1),
               Entity_Line);
         else
            Put (File, Text (Get_Last_Index (B.all) .. Loc_Start - 1));
         end if;

         Put (File,
              "<A HREF="""
              & Get_Html_File_Name
                (Kernel, Get_Other_File_Of
                   (LI_Unit, Get_Declaration_File_Of (Entity_Info).all))
              & '#' & Image (Line_In_Body)
              & """>" & Text (Loc_Start .. Loc_End) & "</A>");
         Set_Last_Index (B.all, Loc_End + 1);
      end Create_Special_Link_To_Body;

      -------------------------
      -- Create_Regular_Link --
      -------------------------

      procedure Create_Regular_Link is
         Line_To_Use : Natural;
      begin
         if Start_Line > Get_Last_Line (B.all) then
            Set_Name_Tags
              (B,
               File,
               Text (Get_Last_Index (B.all) .. Loc_Start - 1),
               Entity_Line);
         else
            Put (File, Text (Get_Last_Index (B.all) .. Loc_Start - 1));
         end if;

         Line_To_Use := Get_Declaration_Line_Of (Entity_Info);
         Put (File,
              "<A HREF="""
              & Get_Html_File_Name
                (Kernel, Get_Declaration_File_Of (Entity_Info))
              & "#" & Image (Line_To_Use) &
              """>" & Text (Loc_Start .. Loc_End) & "</A>");
         Set_Last_Index (B.all, Loc_End + 1);
      end Create_Regular_Link;

      ------------------------
      -- Link_Should_Be_Set --
      ------------------------

      function Link_Should_Be_Set return Boolean is
      begin
         --  If no links should be set to entities declared in not
         --  processed source files => filter them out

         return
           (not Entity_Abstract
            and then
              (Link_All
               or else Source_File_In_List
                 (Source_File_List, Get_Declaration_File_Of (Entity_Info)))
         --  create no links if it is the declaration line itself;
         --  only if it's a subprogram or entry in a spec sometimes
         --  a link can be created to it body, so don't filter these ones.
            and then
             (not Is_Declaration_File_Of (Entity_Info, File_Name)
              or else Get_Declaration_Line_Of (Entity_Info) /=
                Start_Line + Entity_Line - 1
              or else Special_Link_Should_Be_Set));
      end Link_Should_Be_Set;

      --------------------------------
      -- Special_Link_Should_Be_Set --
      --------------------------------

      function Special_Link_Should_Be_Set return Boolean is
      begin
         return not Is_Body
           and then Process_Body
           and then
             (Get_Kind (Entity_Info).Kind = Entry_Or_Entry_Family
              or else Get_Kind (Entity_Info).Kind = Procedure_Kind
              or else Get_Kind (Entity_Info).Kind = Function_Or_Operator);
      end Special_Link_Should_Be_Set;

      --------------------------------
      -- Regular_Link_Should_Be_Set --
      --------------------------------

      function Regular_Link_Should_Be_Set return Boolean is
      begin
         --  No subprograms/tasks are processed here, if working on a spec
         --  file
         return Is_Body
           or else not
             (Get_Kind (Entity_Info).Kind = Entry_Or_Entry_Family
              or else Get_Kind (Entity_Info).Kind = Procedure_Kind
              or else Get_Kind (Entity_Info).Kind = Function_Or_Operator);
      end Regular_Link_Should_Be_Set;

   begin  --  Format_Link
      if Link_Should_Be_Set then
         if Special_Link_Should_Be_Set then
            Create_Special_Link_To_Body;
         elsif Regular_Link_Should_Be_Set then
            Create_Regular_Link;
         end if;
      end if;
   end Format_Link;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (B           : access Backend_HTML;
      File        : File_Descriptor;
      Text        : String;
      Entity_Line : Natural) is
   begin
      if Get_Last_Index (B.all) < Text'Last then
         Set_Name_Tags
           (B,
            File,
            Text (Get_Last_Index (B.all) .. Text'Last),
            Entity_Line);
      end if;
   end Finish;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension (B : access Backend_HTML) return String is
      pragma Unreferenced (B);
   begin
      return ".htm";
   end Get_Extension;

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory
     (B      : access Backend_HTML;
      Kernel : Kernel_Handle) return String
   is
      pragma Unreferenced (B);
   begin
      return File_Utils.Name_As_Directory
        (Object_Path (Get_Root_Project (Get_Registry (Kernel)),
                      False)) & "html/";
   end Get_Doc_Directory;

end Docgen_Backend_HTML;
