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

with Docgen.Html_Output;   use Docgen.Html_Output;
with String_Utils;         use String_Utils;
with Projects.Registry;    use Projects.Registry;
with Projects;             use Projects;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with File_Utils;

package body Docgen_Backend_HTML is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (B : access Backend_HTML; Text : String) is
   begin
      Set_Last_Line (B.all, 0);
      Set_Last_Index (B.all, Text'First);
   end Initialize;

   ----------------
   -- Doc_Create --
   ----------------

   procedure Doc_Create
     (B                : access Backend_HTML;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      File             : in Ada.Text_IO.File_Type;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Info             : in out Docgen.Doc_Info;
      Doc_Directory    : String;
      Doc_Suffix       : String) is
   begin
      --  We call the subprogram responsible for the documentation
      --  process in html: see docgen_html_output.ads

      Doc_HTML_Create
        (B,
         Kernel,
         File,
         Entity_List,
         List_Ref_In_File,
         Info,
         Doc_Directory,
         Doc_Suffix);
   end Doc_Create;

   --------------------
   -- Format_Comment --
   --------------------

   procedure Format_Comment
     (B           : access Backend_HTML;
      File        : Ada.Text_IO.File_Type;
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
      File        : Ada.Text_IO.File_Type;
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
      File        : Ada.Text_IO.File_Type;
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
      File        : Ada.Text_IO.File_Type;
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
     (B                : access Backend_HTML;
      Entity_List      : in out Type_Entity_List.List;
      List_Ref_In_File   : in out List_Reference_In_File.List;
      Start_Index      : Natural;
      Start_Line       : Natural;
      Start_Column     : Natural;
      End_Index        : Natural;
      End_Line         : Natural;
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
      Info             : Doc_Info)
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
         Entity_List,
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
         Info);
   end  Format_Identifier;

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
      Entity_Abstract  : in out Boolean)
   is
      pragma Unreferenced (Start_Index, Start_Column, End_Index, LI_Unit);

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
              "<A href="""
              & Get_Html_File_Name
                (Kernel, Other_File_Name
                   (Kernel, Get_Declaration_File_Of (Entity_Info)))
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
              "<A href="""
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
             (Get_Declaration_File_Of (Entity_Info) /= File_Name
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
      File        : Ada.Text_IO.File_Type;
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
     (B : access Backend_HTML;
      Kernel : Kernel_Handle) return String is
      pragma Unreferenced (B);
   begin
      return File_Utils.Name_As_Directory
           (Object_Path (Get_Root_Project (Get_Registry (Kernel)),
                         False)) & "html/";
   end Get_Doc_Directory;

end Docgen_Backend_HTML;
