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
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with Language.Ada;              use Language.Ada;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with String_Utils;              use String_Utils;
with VFS;                       use VFS;
with Glide_Kernel;              use Glide_Kernel;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;

package body Docgen.Html_Output is

   Me : constant Debug_Handle := Create ("Docgen-html_output");

   package TEL renames Type_Entity_List;

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
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add aa entry or entry family to the documentation

   procedure Doc_HTML_Subprogram
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subprogram to the documentation

   procedure Doc_HTML_Pack_Desc
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the package description to the documentation

   procedure Doc_HTML_Package
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the renamed and instantiated package to the documentation

   procedure Doc_HTML_With
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the dependencies to the documentation

   procedure Doc_HTML_Var
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a constant or named number to the documentation

   procedure Doc_HTML_Exception
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an exception to the documentation

   procedure Doc_HTML_Type
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a type to the documentation

   procedure Format_HTML
     (Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      LI_Unit          : LI_File_Ptr;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_List.List;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean);
   --  Formatted Text as HTML code and write to the docfile.
   --  Line_In_Body is used only for subprograms to create not regular
   --  links (in this case it is not the line number of the declaration
   --  which is needed, but the line of the definition in the body.
   --  If Do_Check_Pack is set, the procedure will check if a link
   --  should be set to First_Package_Line or link it to its declaration
   --  line.

   procedure Doc_HTML_Header
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the header of a package to the documentation

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

   procedure Doc_HTML_Index_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an item to an index, used for all 3 index types

   procedure Doc_HTML_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the footer to the index, used for all 3 indes files

   procedure Doc_HTML_Body
     (Kernel : access Kernel_Handle_Record'Class;
      File : in Ada.Text_IO.File_Type;
      Info : in out Doc_Info);
   --  Format the body by calling Format_HTML for the whole body file
   --  and write it to the doc file

   function Get_Html_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return String;
   --  Create a .htm file name from the full path of the source file
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created

   ---------------------
   -- Doc_HTML_Create --
   ---------------------

   procedure Doc_HTML_Create
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      File          : in Ada.Text_IO.File_Type;
      Info          : in out Docgen.Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_HTML_Open (File, Info);
         when Close_Info            => Doc_HTML_Close (File, Info);
         when Header_Info           => Doc_HTML_Header (Kernel, File, Info);
         when Footer_Info           => Doc_HTML_Footer (File, Info, Kernel);
         when Subtitle_Info         => Doc_HTML_Subtitle (File, Info);
         when Package_Desc_Info     => Doc_HTML_Pack_Desc (File, Info);
         when With_Info             => Doc_HTML_With (Kernel, File, Info);
         when Package_Info          => Doc_HTML_Package (Kernel, File, Info);
         when Var_Info              => Doc_HTML_Var (Kernel, File, Info);
         when Entry_Info            => Doc_HTML_Entry (Kernel, File, Info);
         when Subprogram_Info      => Doc_HTML_Subprogram (Kernel, File, Info);
         when Type_Info             => Doc_HTML_Type (Kernel, File, Info);
         when Exception_Info        => Doc_HTML_Exception (Kernel, File, Info);
         when Unit_Index_Info       =>
            Doc_HTML_Unit_Index_Header (File, Info, Doc_Directory, Doc_Suffix);
         when Subprogram_Index_Info => Doc_HTML_Sub_Index_Header  (File, Info);
         when Type_Index_Info       => Doc_HTML_Type_Index_Header (File, Info);
         when Index_Item_Info       => Doc_HTML_Index_Item (File, Info);
         when End_Of_Index_Info     => Doc_HTML_Index_End  (File, Info);
         when Body_Line_Info        => Doc_HTML_Body (Kernel, File, Info);
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
      Put_Line (File, "<META name=""generator"" CONTENT=""DocGen ");
      Put_Line
        (File, "<META http-equiv=""Content-" &
         "Type"" content="" text/html; charset=ISO-8859-1"">");
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
     (File : Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, "<BR>");
      Put_Line (File, "<TABLE  bgcolor=""#9999FF"" width=""100%""><TR><TD> ");
      Put_Line (File, "<H3> " & Info.Subtitle_Name.all & " </H3>");
      Put_Line (File, "</TD></TR></TABLE>");
      New_Line (File);
   end Doc_HTML_Subtitle;

   ------------------------
   -- Doc_HTML_Pack_Desc --
   ------------------------

   procedure Doc_HTML_Pack_Desc
     (File : Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line
        (File,
         "<h4><PRE>" & Info.Package_Desc_Description.all & " </PRE></H4>");
      Put_Line (File, "<HR> ");
   end Doc_HTML_Pack_Desc;

   ----------------------
   -- Doc_HTML_Package --
   ----------------------

   procedure Doc_HTML_Package
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line
        (File, "  <A name="""
         & Image (Get_Declaration_Line_Of (Info.Package_Entity.Entity))
         & """></A>  <BR>");
      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      if Info.Package_Entity.Is_Private then
         Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Package_Header.all,
         Get_Declaration_File_Of (Info.Package_Entity.Entity),
         Get_Declaration_Line_Of (Info.Package_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, Info.Package_Description.all);
      Put_Line (File, "<HR> ");
   end Doc_HTML_Package;

   -------------------
   -- Doc_HTML_With --
   -------------------

   procedure Doc_HTML_With
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.With_Header.all,
         Info.With_File,
         Info.With_Header_Line,
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, "<HR> ");
   end Doc_HTML_With;

   ------------------
   -- Doc_HTML_Var --
   ------------------

   procedure Doc_HTML_Var
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line
        (File, "  <A name="""
         & Image (Get_Declaration_Line_Of (Info.Var_Entity.Entity))
         & """></A>  <BR>");

      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      if Info.Var_Entity.Is_Private then
         Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Var_Header.all,
         Get_Declaration_File_Of (Info.Var_Entity.Entity),
         Get_Declaration_Line_Of (Info.Var_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, Info.Var_Description.all);
      Put_Line (File, "<HR> ");
   end Doc_HTML_Var;

   ------------------------
   -- Doc_HTML_Exception --
   ------------------------

   procedure Doc_HTML_Exception
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line
        (File, "  <A name="""
         & Image (Get_Declaration_Line_Of (Info.Exception_Entity.Entity))
         & """></A>  <BR>");

      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      if Info.Exception_Entity.Is_Private then
         Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Exception_Header.all,
         Get_Declaration_File_Of (Info.Exception_Entity.Entity),
         Get_Declaration_Line_Of (Info.Exception_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, Info.Exception_Description.all);
      Put_Line (File, "<HR> ");
   end Doc_HTML_Exception;

   -------------------
   -- Doc_HTML_Type --
   -------------------

   procedure Doc_HTML_Type
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line
        (File, "  <A name="""
         & Image (Get_Declaration_Line_Of (Info.Type_Entity.Entity))
         & """></A>  <BR>");

      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      if Info.Type_Entity.Is_Private then
         Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Type_Header.all,
         Get_Declaration_File_Of (Info.Type_Entity.Entity),
         Get_Declaration_Line_Of (Info.Type_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, Info.Type_Description.all);
      Put_Line (File, "<HR> ");
   end Doc_HTML_Type;

   --------------------
   -- Doc_HTML_Entry --
   --------------------

   procedure Doc_HTML_Entry
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line
        (File,
         "  <A name="""
         & Image (Get_Declaration_Line_Of (Info.Entry_Entity.Entity))
         & """></A>  <BR>");
      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      if Info.Entry_Entity.Is_Private then
         Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Entry_Header.all,
         Get_Declaration_File_Of (Info.Entry_Entity.Entity),
         Get_Declaration_Line_Of (Info.Entry_Entity.Entity),
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, Info.Entry_Description.all);
      Put_Line (File, "<HR> ");
   end Doc_HTML_Entry;

   -------------------------
   -- Doc_HTML_Subprogram --
   -------------------------

   procedure Doc_HTML_Subprogram
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info)
   is
      package TRL renames Type_Reference_List;

      procedure Print_Ref_List
        (Local_List    : TRL.List;
         Called_Subp   : Boolean);
      --  Processes the Ref_List to the output file.
      --  If Called_Subp is True, the list of the subprograms
      --  calling the current subprogram will be printed,
      --  if False, the list of the subprograms called within it.

      ----------------------
      --  Print_Ref_List  --
      ----------------------

      procedure Print_Ref_List
        (Local_List    : TRL.List;
         Called_Subp   : Boolean) is
         Node      : TRL.List_Node;
      begin
         if not TRL.Is_Empty (Local_List) then
            if Called_Subp then
               Put_Line (File, "<H5> Subprogram is called by: </H5>");
            else
               Put_Line (File, "<H5> Subprogram calls: </H5>");
            end if;

            Put_Line (File, "<TABLE>");

            Node := TRL.First (Local_List);

            --  For every reference found write the information to doc file

            for J in 1 .. TRL.Length (Local_List) loop
               --  Check if the creating of a link is possible

               if TRL.Data (Node).Set_Link then
                  --  If a called subprogram => link to spec
                  Put_Line
                    (File,
                     "<TR><TD><A href="""
                     & Get_Html_File_Name
                       (Kernel,
                        Get_Declaration_File_Of (TRL.Data (Node).Entity))
                     & "#"
                     & Image
                       (Get_Declaration_Line_Of (TRL.Data (Node).Entity))
                     & """>"
                     & Get_Name (TRL.Data (Node).Entity)
                     & "</A></TD><TD> at &nbsp<I>"
                     & Base_Name
                       (Get_Declaration_File_Of (TRL.Data (Node).Entity))
                     & "</I></TD><TD>:"
                     & Image (Get_Declaration_Line_Of (TRL.Data (Node).Entity))
                     & "</TD><TD>:"
                     & Image
                       (Get_Declaration_Column_Of (TRL.Data (Node).Entity))
                     & "</TD><TR>");

               else
                  --  No link at all
                  Put_Line
                    (File,
                     "<TR><TD>"
                     & Get_Name (TRL.Data (Node).Entity)
                     & "</TD><TD> at &nbsp<I>"
                     & Base_Name
                       (Get_Declaration_File_Of (TRL.Data (Node).Entity))
                     & "</I></TD><TD>:"
                     & Image
                       (Get_Declaration_Line_Of (TRL.Data (Node).Entity))
                     & "</TD><TD>:"
                     & Image
                       (Get_Declaration_Column_Of (TRL.Data (Node).Entity))
                     & "</TD></TR>");
               end if;
               Node := TRL.Next (Node);
            end loop;

            Put_Line (File, "</TABLE>");
         end if;
      end Print_Ref_List;

   begin
      Put_Line
        (File, "  <A name="""
         & Image (Get_Declaration_Line_Of (Info.Subprogram_Entity.Entity))
         & """></A>  <BR> ");
      Put_Line
        (File, "<TABLE  bgcolor=""#DDDDDD"" width=""100%""><TR><TD> <PRE>");

      if Info.Subprogram_Entity.Is_Private then
         Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Subprogram_Header.all,
         Get_Declaration_File_Of (Info.Subprogram_Entity.Entity),
         Get_Declaration_Line_Of (Info.Subprogram_Entity.Entity),
         Get_Line (Info.Subprogram_Entity.Line_In_Body),
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files);

      Put_Line (File, "</PRE></TD></TR></TABLE>");

      --  Write the description to doc file

      Put_Line (File, Info.Subprogram_Description.all);
      Put_Line (File, " <BR>  ");

      Print_Ref_List (Info.Subprogram_Entity.Called_List, True);
      Print_Ref_List (Info.Subprogram_Entity.Calls_List, False);

      Put_Line (File, "<HR> ");
   end Doc_HTML_Subprogram;

   -----------------
   -- Format_HTML --
   -----------------

   procedure Format_HTML
     (Kernel           : access Kernel_Handle_Record'Class;
      File             : Ada.Text_IO.File_Type;
      LI_Unit          : LI_File_Ptr;
      Text             : String;
      File_Name        : VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_List.List;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean)
   is
      Last_Index, Last_Line : Natural;
      Loc_Start, Loc_End    : Natural;
      Point_In_Column       : Natural;

      procedure Set_Name_Tags (Input_Text : String);
      --  Sets a "<A name="lind_number"> <A>" in front of each line in the
      --  given strings (if in body file) and writes it to the doc file.

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  the callback function to add HTML code to the text being parsed.
      --  for each possible kind of parsed entity some HTML code will
      --  be added to the Result_Line and the Already_Added_Chars
      --  increased by the number of the added chars in order to keep
      --  track of the current position in the original string.

      -------------------
      -- Set_Name_Tags --
      -------------------

      procedure Set_Name_Tags (Input_Text : String) is
         HTML_Name_Head   : constant String := "<A name=""";
         HTML_Name_Middle : constant String := """>";
         HTML_Name_End    : constant String := "</A>";
         Last_Written     : Natural := Input_Text'First - 1;
      begin
         for J in Input_Text'Range loop
            if Input_Text (J) = ASCII.LF then
               Last_Line := Last_Line + 1;
               Put
                 (File, Input_Text (Last_Written + 1 .. J)
                  & HTML_Name_Head
                  & Image (Last_Line + Entity_Line - 1)
                  & HTML_Name_Middle
                  & HTML_Name_End);
               Last_Written := J;
            end if;
         end loop;

         Put (File, Input_Text (Last_Written + 1 .. Input_Text'Last));
      end Set_Name_Tags;

      -------------------
      -- HTML_Callback --
      -------------------

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);

         Entity_Info : Entity_Information;
         Status      : Find_Decl_Or_Body_Query_Status;

         HTML_Comment_Prefix : constant String := "<FONT color=""green"">";
         HTML_Comment_Suffix : constant String := "</FONT>";
         HTML_Keyword_Prefix : constant String := "<B>";
         HTML_Keyword_Suffix : constant String := "</B>";
         HTML_String_Prefix  : constant String := "<FONT color=""red"">";
         HTML_String_Suffix  : constant String := "</FONT>";
         HTML_Char_Prefix    : constant String := "<FONT color=""red"">";
         HTML_Char_Suffix    : constant String := "</FONT>";

         procedure Callback_Output
           (Start_Index : Natural;
            End_Index   : Natural;
            Prefix     : String;
            Suffix     : String;
            Check_Tags : Boolean);
         --  Write the formatted text since the last output to doc file
         --  Prefix and Suffix are the HTML code to be put around the
         --  parsed entity. The both index values are needed, as for comment
         --  lines the ASCII.LF at the line should be ignored, so you can't
         --  used always the Sloc_Index values.

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

         procedure Replace_HTML_Tags (Input_Text : String);
         --  Replaces all "<"  which are by "&lt;" and all ">" by "&gt;"
         --  and writes the output to the doc file.

         ---------------------
         -- Callback_Output --
         ---------------------

         procedure Callback_Output
           (Start_Index : Natural;
            End_Index   : Natural;
            Prefix      : String;
            Suffix      : String;
            Check_Tags  : Boolean) is
         begin
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Start_Index - 1));
            else
               Put (File, Text (Last_Index .. Start_Index - 1));
            end if;

            if Check_Tags then
               Put (File, Prefix);
               Replace_HTML_Tags (Text (Start_Index .. End_Index));
               Put (File, Suffix);
            else
               Put (File,
                    Prefix & Text (Start_Index .. End_Index) & Suffix);
            end if;
            Last_Index := End_Index + 1;
            Last_Line  := Sloc_End.Line;
         end Callback_Output;

         ---------------------------------
         -- Create_Special_Link_To_Body --
         ---------------------------------

         procedure Create_Special_Link_To_Body is
         begin
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Loc_Start - 1));
            else
               Put (File, Text (Last_Index .. Loc_Start - 1));
            end if;

            Put (File,
                 "<A href="""
                 & Get_Html_File_Name
                   (Kernel, Get_Declaration_File_Of (Entity_Info))
                 & '#' & Image (Line_In_Body)
                 & """>" & Text (Loc_Start .. Loc_End) & "</A>");

            Last_Index := Loc_End + 1;
         end Create_Special_Link_To_Body;

         -------------------------
         -- Create_Regular_Link --
         -------------------------

         procedure Create_Regular_Link is
            Line_To_Use : Natural;
         begin
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Loc_Start - 1));
            else
               Put (File, Text (Last_Index .. Loc_Start - 1));
            end if;

            if Get_Kind (Entity_Info).Kind = Package_Kind then
               Line_To_Use := First_File_Line;
            else
               Line_To_Use := Get_Declaration_Line_Of (Entity_Info);
            end if;

            Put (File,
                 "<A href="""
                 & Get_Html_File_Name
                   (Kernel, Get_Declaration_File_Of (Entity_Info))
                 & "#" & Image (Line_To_Use) &
                 """>" & Text (Loc_Start .. Loc_End) & "</A>");
            Last_Index := Loc_End + 1;
         end Create_Regular_Link;

         ------------------------
         -- Link_Should_Be_Set --
         ------------------------

         function Link_Should_Be_Set return Boolean is
         begin
            --  If no links should be set to entities declared in not
            --  processed source files => filter them out
            return
              (Link_All
               or  else Source_File_In_List
                 (Source_File_List, Get_Declaration_File_Of (Entity_Info)))

            --  create no links if it is the declaration line itself;
            --  only if it's a subprogram or entry in a spec sometimes
            --  a link can be created to it body, so don't filter these ones.
              and then
                (Get_Declaration_File_Of (Entity_Info) /= File_Name
                 or else Get_Declaration_Line_Of (Entity_Info) /=
                   Sloc_Start.Line + Entity_Line - 1
                 or else Special_Link_Should_Be_Set);
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

         -----------------------
         -- Replace_HTML_Tags --
         -----------------------

         procedure Replace_HTML_Tags (Input_Text : String) is
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

      begin  -- HTML_Callback
         case Entity is
            when Comment_Text =>
               Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index - 1,
                  HTML_Comment_Prefix, HTML_Comment_Suffix, False);

            when Keyword_Text =>
               Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_Keyword_Prefix, HTML_Keyword_Suffix, False);

            when String_Text =>
               Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_String_Prefix, HTML_String_Suffix, True);

            when Character_Text =>
               Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_Char_Prefix, HTML_Char_Suffix, False);

            when Identifier_Text =>   --  perhaps links can be set:
               Loc_Start := Sloc_Start.Index;

               --  Take apart parsed entites with any "."'s in the middle
               for J in 1 ..
                 1 + Count_Points (Text (Sloc_Start.Index .. Sloc_End.Index))
               loop
                  Point_In_Column :=
                    Index (Text (Loc_Start .. Sloc_End.Index), ".");

                  if Point_In_Column > 0 then
                     Loc_End := Point_In_Column - 1;
                  else
                     Loc_End := Sloc_End.Index;
                  end if;

                  Find_Declaration
                    (LI_Unit,
                     File_Name,
                     Text (Loc_Start .. Loc_End),
                     Sloc_Start.Line + Entity_Line - 1,
                     Sloc_Start.Column + Loc_Start - Sloc_Start.Index,
                     Entity_Info,
                     Status);

                  if Status = Success or Status = Fuzzy_Match then
                     if Link_Should_Be_Set then
                        if Special_Link_Should_Be_Set then
                           Create_Special_Link_To_Body;
                        elsif Regular_Link_Should_Be_Set then
                           Create_Regular_Link;
                        end if;
                     end if;
                  end if;

                  if Point_In_Column > 0 then
                     Loc_Start := Point_In_Column + 1;
                  end if;
               end loop;

            when others =>
               null;
         end case;

         return False;
      end HTML_Callback;

   begin
      Last_Index := Text'First;
      Last_Line  := 0;
      Parse_Entities (Ada_Lang, Text, HTML_Callback'Unrestricted_Access);

      if Last_Index < Text'Last then
         Set_Name_Tags (Text (Last_Index .. Text'Last));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Format_HTML;

   ---------------------
   -- Doc_HTML_Header --
   ---------------------

   procedure Doc_HTML_Header
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Put_Line (File, "<TABLE  bgcolor=""#9999FF"" width=""100%""><TR><TD>");
      Put_Line (File, " <H1>  Package <I>");
      Put_Line (File, " <A NAME=""" & Image (Info.Header_Line) & """>");

      --  check if should set a link to the body file

      if Info.Header_Link then
         Put_Line
           (File, "<A HREF="""
            & Get_Html_File_Name (Kernel, Info.Header_File)
            & """> ");
         Put_Line (File, Info.Header_Package.all & "</A>");
      end if;

      Put_Line (File, Info.Header_Package.all & "</A></I></H1>");
      Put_Line (File, "</TD></TR></TABLE>");
      Put_Line (File, "<PRE>");
      Put_Line (File, "<HR>");
   end Doc_HTML_Header;

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
      Put_Line (Frame_File, "<FRAMESET cols=""30%,70%"">");
      Put_Line (Frame_File, "<FRAME src=""index_unit.htm"" NAME=""index"" >");
      Put_Line (Frame_File, "<FRAME src=""" &
                Base_Name
                  (Get_Doc_File_Name
                     (Type_Source_File_List.Data (Source_File_Node).File_Name,
                      Doc_Directory, Doc_Suffix)) & """ name=""main"" >");
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
      Put_Line (File, "<BASE target=""main"">");
      Put_Line (File, "<META http-equiv=""Content-Type"" " &
                "content=""text/html; charset=ISO-8859-1" & """>");
      Put_Line (File, "</HEAD>");
      Put_Line (File, "<BODY bgcolor=""white"">");
      Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                "width=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Unit Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      Put_Line (File, "<H4> <a href=""index_sub.htm"" " &
                "target=""index""> Subprogram Index </a> <br>");
      Put_Line (File, " <A href=""index_type.htm"" " &
                "target=""index""> Type Index </A> </H4><BR>");
      Put_Line (File, "<HR> <BR>");
   end Doc_HTML_Unit_Index_Header;

   --------------------------------
   -- Doc_HTML_Sub_Index_Header --
   --------------------------------

   procedure Doc_HTML_Sub_Index_Header
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info)
   is
      pragma Unreferenced (Info);
   begin
      Put_Line (File, "<HTML> ");
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<BASE target=""main"">");
      Put_Line (File, "<META http-equiv=""Content-" &
                "Type"" content=""text/html; charset=ISO-8859-1"">");
      Put_Line (File, "</HEAD>");
      New_Line (File);
      Put_Line (File, "<BODY bgcolor=""white"">");
      New_Line (File);
      Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                "width=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Subprogram Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      New_Line (File);
      Put_Line (File, "<H4> <A href=""index_unit.htm""  " &
                "target=""index""> Unit Index </A> <BR>");
      New_Line (File);
      Put_Line (File, " <A href=""index_type.htm"" " &
                "target=""index""> Type Index </A> </H4><BR>");
      New_Line (File);
      Put_Line (File, "<HR> <BR>");
      New_Line (File);
   end Doc_HTML_Sub_Index_Header;

   --------------------------------
   -- Doc_HTML_Type_Index_Header --
   --------------------------------

   procedure Doc_HTML_Type_Index_Header
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info)
   is
      pragma Unreferenced (Info);
   begin
      Put_Line (File, "<HTML> ");
      New_Line (File);
      Put_Line (File, "<HEAD>");
      Put_Line (File, "<BASE target=""main"">");
      New_Line (File);
      Put_Line (File, "<META http-equiv" &
                "=""Content-Type"" content=""" &
                "text/html; charset=" & "ISO-8859-1" & """>");
      Put_Line (File, "</HEAD>");
      New_Line (File);
      Put_Line (File, "<BODY bgcolor=""white"">");
      New_Line (File);
      Put_Line
        (File, "<TABLE  bgcolor=""#9999FF"" width=""100%""><TR><TD> <PRE>");
      Put_Line (File, "<H2> Type Index </H2> ");
      Put_Line (File, "</PRE></TD></TR></TABLE>");
      New_Line (File);
      Put_Line (File, "<H4> <A href=""index_unit.htm"" " &
                "target = ""index""> Unit Index </A> <BR>");
      New_Line (File);
      Put_Line (File, " <A href=""index_sub.htm"" " &
                "target=""index""> Subprogram Index </A></H4> <BR>");
      New_Line (File);
      Put_Line (File, "<HR> <BR>");
      New_Line (File);
   end Doc_HTML_Type_Index_Header;

   -------------------------
   -- Doc_HTML_Index_Item --
   -------------------------

   procedure Doc_HTML_Index_Item
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line
        (File, " <A href=""" & Info.Item_Doc_File.all
         & "#" & Image (Info.Item_Line)
         & """ target=""main""> "
         & Info.Item_Name.all & "</A>");

      Put_Line
        (File, " <BR> &nbsp&nbsp&nbsp&nbsp&nbsp in " &
         Base_Name (Info.Item_File));
      New_Line (File);
      Put_Line (File, "<BR>");
      New_Line (File);
   end Doc_HTML_Index_Item;

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
     (Kernel : access Kernel_Handle_Record'Class;
      File : in Ada.Text_IO.File_Type;
      Info : in out Doc_Info) is
   begin
      Format_HTML
        (Kernel,
         File,
         Info.Doc_LI_Unit,
         Info.Body_Text.all,
         Info.Body_File,
         First_File_Line,
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         True,
         Info.Doc_Info_Options.Process_Body_Files);
   end Doc_HTML_Body;

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
      Trace (Me, "Get_Html_File_Name: " & Temp);
      return Temp;
   end Get_Html_File_Name;

end Docgen.Html_Output;
