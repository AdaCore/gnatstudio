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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Language.Ada;              use Language.Ada;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;

package body Docgen.Html_Output is

   package TEL renames Type_Entity_List;

   procedure Doc_HTML_Create
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_HTML_Open (File, Info);
         when Close_Info            => Doc_HTML_Close (File, Info);
         when Header_Info           => Doc_HTML_Header (File, Info);
         when Footer_Info           => Doc_HTML_Footer (File, Info);
         when Subtitle_Info         => Doc_HTML_Subtitle (File, Info);
         when Package_Desc_Info     => Doc_HTML_Pack_Desc (File, Info);
         when With_Info             => Doc_HTML_With (File, Info);
         when Package_Info          => Doc_HTML_Package (File, Info);
         when Var_Info              => Doc_HTML_Var (File, Info);
         when Entry_Info            => Doc_HTML_Entry (File, Info);
         when Subprogram_Info       => Doc_HTML_Subprogram (File, Info);
         when Type_Info             => Doc_HTML_Type (File, Info);
         when Exception_Info        => Doc_HTML_Exception (File, Info);
         when Unit_Index_Info       => Doc_HTML_Unit_Index_Header (File, Info);
         when Subprogram_Index_Info => Doc_HTML_Sub_Index_Header  (File, Info);
         when Type_Index_Info       => Doc_HTML_Type_Index_Header (File, Info);
         when Index_Item_Info       => Doc_HTML_Index_Item (File, Info);
         when End_Of_Index_Info     => Doc_HTML_Index_End  (File, Info);
         when Body_Line_Info        => Doc_HTML_Body (File, Info);
         when others => null;
      end case;

   end Doc_HTML_Create;

   -------------------
   -- Doc_HTML_Open --
   -------------------

   procedure Doc_HTML_Open
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "<HTML>");
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<TITLE>" &
                            Info.Open_Title.all &
                            "</TITLE>");
      Ada.Text_IO.Put_Line (File, "<META name=""generator"" " &
                            "CONTENT=""DocGen ");
      Ada.Text_IO.Put_Line (File, "<META http-equiv=""Content-" &
                            "Type"" content=""" &
                            "text/html; charset=" & "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
   end Doc_HTML_Open;

   --------------------
   -- Doc_HTML_Close --
   --------------------

   procedure Doc_HTML_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);

   begin
      Ada.Text_IO.Put_Line (File, "</BODY>");
      Ada.Text_IO.Put_Line (File, "</HTML>");
   end Doc_HTML_Close;

   -----------------------
   -- Doc_HTML_Subtitle --
   -----------------------

   procedure Doc_HTML_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "<BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD> ");
      Ada.Text_IO.Put_Line (File, "<H3> "  &
                            Info.Subtitle_Name.all &
                            " </H3>");
      Ada.Text_IO.Put_Line (File, "</TD></TR></TABLE>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Subtitle;

   ------------------------
   -- Doc_HTML_Pack_Desc --
   ------------------------

   procedure Doc_HTML_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      Ada.Text_IO.Put_Line (File, "<h4><PRE>"  &
                            Info.Package_Desc_Description.all &
                            " </PRE></H4>");

      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Pack_Desc;

   ----------------------
   -- Doc_HTML_Package --
   ----------------------

   procedure Doc_HTML_Package
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Package_Entity.Line)
                            & """></A>  <BR>");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Package_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML
        (File,
         Info.Doc_LI_Unit,
         Info.Package_Header.all,
         Info.Package_Entity.File_Name.all,
         Info.Package_Entity.Short_Name.all,
         Info.Package_Entity.Line,
         No_Body_Line_Needed,
         Info.Doc_File_List,
         Info.Doc_Info_Options.Link_All,
         False,
         Info.Doc_Info_Options.Process_Body_Files,
         True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Package_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Package;

   -------------------
   -- Doc_HTML_With --
   -------------------

   procedure Doc_HTML_With
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.With_Header.all,
                   Info.With_File.all,
                   "",
                   First_File_Line,
                   Info.With_Header_Line,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   False);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_With;

   ------------------
   -- Doc_HTML_Var --
   ------------------

   procedure Doc_HTML_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Var_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Var_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.Var_Header.all,
                   Info.Var_Entity.File_Name.all,
                   Info.Var_Entity.Short_Name.all,
                   Info.Var_Entity.Line,
                   No_Body_Line_Needed,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Var_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Var;

   ------------------------
   -- Doc_HTML_Exception --
   ------------------------

   procedure Doc_HTML_Exception
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Exception_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Exception_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.Exception_Header.all,
                   Info.Exception_Entity.File_Name.all,
                   Info.Exception_Entity.Short_Name.all,
                   Info.Exception_Entity.Line,
                   No_Body_Line_Needed,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Exception_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Exception;

   -------------------
   -- Doc_HTML_Type --
   -------------------

   procedure Doc_HTML_Type
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Type_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Type_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.Type_Header.all,
                   Info.Type_Entity.File_Name.all,
                   Info.Type_Entity.Short_Name.all,
                   Info.Type_Entity.Line,
                   No_Body_Line_Needed,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Type_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");

   end Doc_HTML_Type;

   --------------------
   -- Doc_HTML_Entry --
   --------------------

   procedure Doc_HTML_Entry
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Entry_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Entry_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.Entry_Header.all,
                   Info.Entry_Entity.File_Name.all,
                   Info.Entry_Entity.Short_Name.all,
                   Info.Entry_Entity.Line,
                   No_Body_Line_Needed,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Entry_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");

   end Doc_HTML_Entry;

   -------------------------
   -- Doc_HTML_Subprogram --
   -------------------------

   procedure Doc_HTML_Subprogram
     (File      : in Ada.Text_IO.File_Type;
      Info      : Doc_Info) is

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
         Suffix    : GNAT.OS_Lib.String_Access;
      begin
         if not TRL.Is_Empty (Local_List) then
            if Called_Subp then
               Ada.Text_IO.Put_Line (File,
                                     "<H5> Subprogram is called by: </H5>");
            else
               Ada.Text_IO.Put_Line (File,
                                     "<H5> Subprogram calls: </H5>");
            end if;
            Ada.Text_IO.Put_Line (File, "<TABLE>");

            Node := TRL.First (Local_List);

            --  for every reference found write the information to doc file
            for J in 1 .. TRL.Length (Local_List) loop

               --  check if the creating of a link is possible
               if TRL.Data (Node).Set_Link then

                  --  if a called subprogram => link to spec
                  if Called_Subp then
                     Suffix :=
                       new String'("_" &
                                   Body_Suffix (TRL.Data (Node).File_Name.all)
                                   & ".htm");
                  else
                     Suffix :=
                       new String'("_" &
                                   Spec_Suffix (TRL.Data (Node).File_Name.all)
                                   & ".htm");
                  end if;

                  declare
                     Body_File : constant String :=
                       File_Name_Without_Suffix
                         (TRL.Data (Node).File_Name.all)
                     & Suffix.all;
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        "<TR><TD><A href="""
                        & Body_File & "#"
                        & Integer'Image (TRL.Data (Node).Line)
                        & """>"
                        & TRL.Data
                          (Node).Subprogram_Name.all
                        & "</A></TD><TD> in &nbsp&nbsp<I>"
                        & TRL.Data (Node).File_Name.all
                        & "</I></TD><TD>, line: "
                        & Integer'Image (TRL.Data (Node).Line)
                        & "</TD><TD>, column: "
                        & Integer'Image (TRL.Data (Node).Column)
                        & "</TD><TR>");
                  end;
               --  no link at all
               else Ada.Text_IO.Put_Line (File,
                                          "<TR><TD>"
                                          & TRL.Data (Node).Subprogram_Name.all
                                          & "</TD><TD> in &nbsp&nbsp<I>"
                                          & TRL.Data (Node).File_Name.all
                                          & "</I></TD><TD>, line: "
                                          & Integer'Image
                                            (TRL.Data (Node).Line)
                                          & "</TD><TD>, column: "
                                          & Integer'Image
                                            (TRL.Data (Node).Column)
                                          & "</TD></TR>");
               end if;
               Node := TRL.Next (Node);
            end loop;
               Ada.Text_IO.Put_Line (File, "</TABLE>");
         end if;
      end Print_Ref_List;

   begin  --  Doc_HTML_Subprogram

   --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Subprogram_Entity.Line)
                            & """></A>  <BR> ");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Subprogram_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.Subprogram_Header.all,
                   Info.Subprogram_Entity.File_Name.all,
                   Info.Subprogram_Entity.Short_Name.all,
                   Info.Subprogram_Entity.Line,
                   Info.Subprogram_Entity.Line_In_Body,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");

      --  write the description to doc file
      Ada.Text_IO.Put_Line (File, Info.Subprogram_Description.all);
      Ada.Text_IO.Put_Line (File, " <BR>  ");

      Print_Ref_List (Info.Subprogram_Entity.Called_List, True);
      Print_Ref_List (Info.Subprogram_Entity.Calls_List, False);

      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Subprogram;

   -----------------
   -- Format_HTML --
   -----------------

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
      Do_Check_Pack    : Boolean) is
      pragma Unreferenced (Do_Check_Pack);

      --  global variables for the callback function
      Last_Index, Last_Line : Natural;
      Loc_Start, Loc_End    : Natural;
      Point_In_Column       : Natural;

      procedure Set_Name_Tags
        (Input_Text : String);
      --  sets a "<A name="lind_number"> <A>" in front of each line in the
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

      procedure Set_Name_Tags
        (Input_Text : String) is
         Last_Written : Natural;

         HTML_Name_Head   : constant String := "<A name=""";
         HTML_Name_Middle : constant String := """>";
         HTML_Name_End    : constant String := "</A>";

      begin
         Last_Written := Input_Text'First - 1;
         for J in Input_Text'First .. Input_Text'Last loop
            if Input_Text (J) = ASCII.LF then
               Last_Line := Last_Line + 1;
               Ada.Text_IO.Put (File, Input_Text
                                     (Last_Written + 1 .. J) &
                                   HTML_Name_Head &
                                   Integer'Image
                                     (Last_Line + Entity_Line - 1) &
                                   HTML_Name_Middle &
                                   HTML_Name_End);
               Last_Written := J;
            end if;
         end loop;
         Ada.Text_IO.Put (File,
                             Input_Text
                               (Last_Written + 1 .. Input_Text'Last));
      end Set_Name_Tags;

      -------------------
      -- HTML_Callback --
      -------------------

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is
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
         --  will create a link to the reference of the entity in the body

         function Link_Should_Be_Set return Boolean;
         --  check if a link to that entity should be set

         function Special_Link_Should_Be_Set return Boolean;
         --  check if a special link to the body should be set
         --  (a special link, because it doesn't link to the declaration
         --  of the entity, but to a reference somewhere in the body)

         function Regular_Link_Should_Be_Set return Boolean;
         --  check if a regular link to the body should be set
         --  (a regular link is a link to the entity's declaration)

         procedure Replace_HTML_Tags
           (Input_Text : String);
         --  replaces all "<"  which are by "&lt;" and all ">" by "&gt;"
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
               Ada.Text_IO.Put (File, Text (Last_Index .. Start_Index - 1));
            end if;

            if Check_Tags then
               Ada.Text_IO.Put (File, Prefix);
               Replace_HTML_Tags (Text (Start_Index .. End_Index));
               Ada.Text_IO.Put (File, Suffix);
            else
               Ada.Text_IO.Put (File,
                                Prefix &
                                  Text (Start_Index .. End_Index) &
                                  Suffix);
            end if;
            Last_Index := End_Index + 1;
            Last_Line  := Sloc_End.Line;
         end Callback_Output;

         ---------------------------------
         -- Create_Special_Link_To_Body --
         ---------------------------------

         procedure Create_Special_Link_To_Body is
            Spec_File : constant String :=
              GNAT.Directory_Operations.File_Name
                (Get_Declaration_File_Of (Entity_Info));
         begin
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Loc_Start - 1));
            else
               Ada.Text_IO.Put (File,
                                Text (Last_Index .. Loc_Start - 1));
            end if;

            Ada.Text_IO.Put (File,
                             "<A href=""" &
                             File_Name_Without_Suffix (Spec_File) &
                             "_" &
                             Body_Suffix (Spec_File)
                             & ".htm" &
                             "#" &
                             Integer'Image (Line_In_Body) &
                             """>" &
                             Text (Loc_Start ..  Loc_End) &
                             "</A>");

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
               Ada.Text_IO.Put (File,
                                Text (Last_Index .. Loc_Start - 1));
            end if;

            if Get_Kind (Entity_Info).Kind = Package_Kind then
               Line_To_Use := First_File_Line;
            else
               Line_To_Use := Get_Declaration_Line_Of (Entity_Info);
            end if;

            Ada.Text_IO.Put (File,
                             "<A href=""" &
                             GNAT.Directory_Operations.File_Name
                               (Get_Html_File_Name
                                  (Get_Declaration_File_Of (Entity_Info))) &
                             "#" &
                               Integer'Image (Line_To_Use) &
                             """>" &
                             Text (Loc_Start .. Loc_End) &
                             "</A>");

            Last_Index := Loc_End + 1;
         end Create_Regular_Link;

         ------------------------
         -- Link_Should_Be_Set --
         ------------------------

         function Link_Should_Be_Set return Boolean is
         begin

            --  if no links should be set to entities declared in not
            --  processed source files => filter them out
            return (Link_All or
              Source_File_In_List (Source_File_List,
                                   Get_Declaration_File_Of (Entity_Info)))
            --  create no links if it is the declaration line itself;
            --  only if it's a subprogram or entry in a spec sometimes
            --  a link CAN be created to it body, so don't filter these ones.
              and
                ((GNAT.Directory_Operations.File_Name
                    (Get_Declaration_File_Of (Entity_Info)) /=
                   GNAT.Directory_Operations.File_Name (File_Name) or
                    Get_Declaration_Line_Of (Entity_Info) /=
                    Sloc_Start.Line + Entity_Line - 1)
                   or (not Is_Body and
                         (Get_Kind (Entity_Info).Kind = Entry_Or_Entry_Family
                          or Get_Kind (Entity_Info).Kind = Procedure_Kind
                          or Get_Kind (Entity_Info).Kind =
                          Function_Or_Operator)))
            --  if in the spec: no links for entities defined in supbrogram
            --  or within types
              and (Is_Body or
                     not (Is_Defined_In_Subprogram
                       (Entity_Name,
                        Get_Name (Entity_Info),
                        Get_Unit_Name (LI_Unit,
                                       GNAT.Directory_Operations.File_Name
                                         (File_Name)))
                   and
                   (Get_Kind (Entity_Info).Kind = Entry_Or_Entry_Family
                          or Get_Kind (Entity_Info).Kind = Procedure_Kind
                          or Get_Kind (Entity_Info).Kind =
                          Function_Or_Operator)));
         end Link_Should_Be_Set;

         --------------------------------
         -- Special_Link_Should_Be_Set --
         --------------------------------

         function Special_Link_Should_Be_Set return Boolean is
         begin

            --  checks if a special link, a link not to a declaration,
            --  should be set.
            return
            not Is_Body and
              Process_Body and
            (Get_Kind (Entity_Info).Kind = Entry_Or_Entry_Family
               or Get_Kind (Entity_Info).Kind = Procedure_Kind
               or Get_Kind (Entity_Info).Kind = Function_Or_Operator);
         end Special_Link_Should_Be_Set;

         --------------------------------
         -- Regular_Link_Should_Be_Set --
         --------------------------------

         function Regular_Link_Should_Be_Set return Boolean is
         begin
            --  a regular link is a link to the entity's declaration
            return
            --  no subprograms/tasks are processed here,
            --  if working on a spec file
            (not (Get_Kind (Entity_Info).Kind = Entry_Or_Entry_Family
                  or Get_Kind (Entity_Info).Kind = Procedure_Kind
                  or Get_Kind (Entity_Info).Kind = Function_Or_Operator)
               or Is_Body);
         end Regular_Link_Should_Be_Set;

         -----------------------
         -- Replace_HTML_Tags --
         -----------------------

         procedure Replace_HTML_Tags
           (Input_Text : String) is

            Last_Index : Natural;
         begin
            Last_Index := Input_Text'First;

            for J in Input_Text'First .. Input_Text'Last - 1 loop
               if Input_Text (J) = '<' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "&lt;");
                  Last_Index := J + 1;
               elsif Input_Text (J) = '>' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "&gt;");
                  Last_Index := J + 1;
               elsif Input_Text (J) = '&' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "&amp;");
                  Last_Index := J + 1;
               end if;
            end loop;
            Ada.Text_IO.Put (File, Input_Text (Last_Index .. Input_Text'Last));
         end Replace_HTML_Tags;

      begin  -- HTML_Callback

         case Entity is
            when Comment_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index - 1,
                  HTML_Comment_Prefix, HTML_Comment_Suffix, False);
            when Keyword_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_Keyword_Prefix, HTML_Keyword_Suffix, False);
            when String_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_String_Prefix, HTML_String_Suffix, True);
            when Character_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_Char_Prefix, HTML_Char_Suffix, False);
            when Identifier_Text =>   --  perhaps links can be set:

               Loc_Start := Sloc_Start.Index;

               --  take apart parsed entites with any "."'s in the middle
               for J in 1 .. 1 +
                 Count_Points (Text (Sloc_Start.Index .. Sloc_End.Index)) loop

                  Point_In_Column :=
                    Get_String_Index (Text (Loc_Start .. Sloc_End.Index),
                                      Loc_Start, ".");
                  if Point_In_Column > 0 then
                     Loc_End := Point_In_Column - 1;
                  else
                     Loc_End := Sloc_End.Index;
                  end if;

                  Find_Declaration (LI_Unit,
                                    GNAT.Directory_Operations.File_Name
                                      (File_Name),
                                    Text (Loc_Start .. Loc_End),
                                    Sloc_Start.Line + Entity_Line - 1,
                                    Sloc_Start.Column +
                                      (Loc_Start - Sloc_Start.Index),
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

            when others => null;
         end case;
         return False;
      end HTML_Callback;

   begin  --  Format_HTML

      --  set the global varibales
      Last_Index := 1;
      Last_Line  := 0;

      --  parse the entities in Text
      Parse_Entities (Ada_Lang,
                      Text,
                      HTML_Callback'Unrestricted_Access);

      --  write the rest of the text, since the last found parsed entity
      if Last_Index < Text'Last then
         Set_Name_Tags (Text (Last_Index .. Text'Last));
         --  Ada.Text_IO.Put (File, Text (Last_Index .. Text'Last));
      end if;
   end Format_HTML;

   ---------------------
   -- Doc_HTML_Header --
   ---------------------

   procedure Doc_HTML_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Body_File_Name : GNAT.OS_Lib.String_Access;
   begin

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD>");
      Ada.Text_IO.Put_Line (File, " <H1>  Package <I>");
      Ada.Text_IO.Put_Line (File, " <A NAME=""" &
                            Integer'Image (Info.Header_Line)
                            & """>");

      --  check if should set a link to the body file
      if Info.Header_Link and
        Is_Spec_File (Info.Header_File.all) then
--           Put_Line (Info.Header_File.all & "   "  &
--                     File_Name_Without_Suffix (Info.Header_File.all) &
--                     "   gibt body suffix:  " &
--                     Body_Suffix (Info.Header_File.all)
--                     & "  weil spec ist:  " &
--                     Boolean'Image (Is_Spec_File
--                                      (Info.Header_File.all)));
         Body_File_Name :=
           new String '(File_Name_Without_Suffix (Info.Header_File.all)
                        & "_" &
                        Body_Suffix (Info.Header_File.all)
                        & ".htm");
         Ada.Text_IO.Put_Line (File, "<A HREF=""" &
                               File_Name (Body_File_Name.all) &
                               """> ");
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A></A></I></H1>");
      --  check if should set a link to the spec file
      elsif not Is_Spec_File (Info.Header_File.all) then
         Body_File_Name :=
           new String '(File_Name_Without_Suffix (Info.Header_File.all)
                        & "_" &
                        Spec_Suffix (Info.Header_File.all)
                        &  ".htm");

         Ada.Text_IO.Put_Line (File, "<A href=""" &
                               File_Name (Body_File_Name.all) &
                               """> ");
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A></A></I></H1> ");
      else
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A> </I> </H1>");
      end if;

      Ada.Text_IO.Put_Line (File, "</TD></TR></TABLE>");
      if not Is_Spec_File (Info.Header_File.all) then
         Ada.Text_IO.Put_Line (File, "<PRE>");
      end if;
      Ada.Text_IO.Put_Line (File, "<HR>");
   end Doc_HTML_Header;

   ---------------------
   -- Doc_HTML_Footer --
   ---------------------

   procedure Doc_HTML_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      if not Is_Spec_File (Info.Footer_File.all) then
         Ada.Text_IO.Put_Line (File, "</PRE>");
      end if;
   end Doc_HTML_Footer;

   --------------------------------
   -- Doc_HTML_Unit_Index_Header --
   --------------------------------

   procedure Doc_HTML_Unit_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Frame_File       : File_Type;
      Source_File_Node : Type_Source_File_List.List_Node;
   begin

      Source_File_Node :=
        Type_Source_File_List.First (Info.Unit_File_List);

      --  create the main frame file
      Create (Frame_File,
              Out_File,
              Info.Doc_Info_Options.Doc_Directory.all & "index.htm");
      Ada.Text_IO.Put_Line (Frame_File, "<HTML> ");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<HEAD>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<TITLE> Index </TITLE>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</HEAD>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<FRAMESET cols=""30%,70%"">");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<FRAME src=""index_unit.htm"" " &
                            "NAME=""index"" >");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<FRAME src=""" &
                            Base_Name
                              (Get_Doc_File_Name
                                 (Type_Source_File_List.Data
                                    (Source_File_Node).File_Name.all,
                                  Info.Doc_Info_Options.Doc_Directory.all,
                                  Info.Doc_Info_Options.Doc_Suffix.all)) &
                            """ name=""main"" >");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</FRAMESET>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<NOFRAMES>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<BODY>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</BODY>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</NOFRAMES>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</HTML>");
      Ada.Text_IO.New_Line (Frame_File);
      Close (Frame_File);

      --  create the header for the unit index file
      Ada.Text_IO.Put_Line (File, "<HTML> ");
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<BASE target=""main"">");
      Ada.Text_IO.Put_Line (File, "<META http-equiv=""Content-Type"" " &
                            "content=""" &
                            "text/html; charset=" &
                            "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                                  "width=""100%""><TR><TD> <PRE>");
      Ada.Text_IO.Put_Line (File, "<H2> Unit Index </H2> ");
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "<H4> <a href=""index_sub.htm"" " &
                            "target=""index""> Subprogram " &
                            "Index </a> <br>");
      Ada.Text_IO.Put_Line (File, " <A href=""index_type.htm"" " &
                              "target=""index""> Type Index </A> </H4><BR>");
      Ada.Text_IO.Put_Line (File, "<HR> <BR>");
   end Doc_HTML_Unit_Index_Header;

   --------------------------------
   -- Doc_HTML_Sub_Index_Header --
   --------------------------------

   procedure Doc_HTML_Sub_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin
      Ada.Text_IO.Put_Line (File, "<HTML> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<BASE target=""main"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<META http-equiv=""Content-" &
                            "Type"" content=""" &
                            "text/html; charset=" &
                            "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD> <PRE>");
      Ada.Text_IO.Put_Line (File, "<H2> Subprogram Index </H2> ");
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<H4> <A href=""index_unit.htm""  " &
                            "target=""index""> Unit Index </A> <BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, " <A href=""index_type.htm"" " &
                            "target=""index""> Type Index </A> </H4><BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HR> <BR>");
      Ada.Text_IO.New_Line (File);

   end Doc_HTML_Sub_Index_Header;

   --------------------------------
   -- Doc_HTML_Type_Index_Header --
   --------------------------------

   procedure Doc_HTML_Type_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin

      Ada.Text_IO.Put_Line (File, "<HTML> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<BASE target=""main"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<META http-equiv" &
                            "=""Content-Type"" content=""" &
                            "text/html; charset=" & "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                              "width=""100%""><TR><TD> <PRE>");
      Ada.Text_IO.Put_Line (File, "<H2> Type Index </H2> ");
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<H4> <A href=""index_unit.htm"" " &
                            "target = ""index""> Unit Index </A> <BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, " <A href=""index_sub.htm"" " &
                            "target=""index""> Subprogram " &
                            "Index </A></H4> <BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HR> <BR>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Type_Index_Header;

   -------------------------
   -- Doc_HTML_Index_Item --
   -------------------------

   procedure Doc_HTML_Index_Item
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, " <A href=""" & Info.Item_Doc_File.all
                            & "#" & Integer'Image (Info.Item_Line)
                            & """ target=""main""> "
                            & Info.Item_Name.all & "</A>");

      Ada.Text_IO.Put_Line (File, " <BR> &nbsp&nbsp&nbsp&nbsp&nbsp in " &
                            Info.Item_File.all);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BR>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Index_Item;

   ------------------------
   -- Doc_HTML_Index_End --
   ------------------------

   procedure Doc_HTML_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin
      Ada.Text_IO.Put_Line (File, "</BODY> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "</HTML>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Index_End;

   -------------------
   -- Doc_HTML_Body --
   -------------------

   procedure Doc_HTML_Body
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      Format_HTML (File,
                   Info.Doc_LI_Unit,
                   Info.Body_Text.all,
                   Info.Body_File.all,
                   "",
                   First_File_Line,
                   No_Body_Line_Needed,
                   Info.Doc_File_List,
                   Info.Doc_Info_Options.Link_All,
                   True,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
   end Doc_HTML_Body;

   ------------------------
   -- Get_Html_File_Name --
   ------------------------

   function Get_Html_File_Name
     (File : String) return String is
   begin
      if Is_Spec_File (File) then
         return File_Name_Without_Suffix (File) & "_" &
                Spec_Suffix (File) & ".htm";
      else
         return File_Name_Without_Suffix (File) & "_" &
                Body_Suffix (File) & ".htm";
      end if;
   end Get_Html_File_Name;

end Docgen.Html_Output;
