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
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Language.Ada;              use Language.Ada;

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

   begin
      if False then        --  just to avoid the warning
         Put_Line (Info.Close_Title.all);
      end if;

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

      Ada.Text_IO.Put_Line (File, "<h4><PRE> "  &
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
      Line_Nr : constant String := Info.Package_Entity.Line'Img;
      New_Text : GNAT.OS_Lib.String_Access;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Package_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      New_Text := Format_HTML (Info.Package_List,
                               Info.Package_Header.all,
                               Info.Package_Entity.File_Name.all,
                               Info.Package_Entity.Short_Name.all,
                               Info.Package_Entity.Line,
                               False,
                               True);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
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
      New_Text : GNAT.OS_Lib.String_Access;
   begin

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      New_Text := Format_HTML (Info.With_List,
                               Info.With_Header.all,
                               Info.With_File.all,
                               "",
                               0,
                               False,
                               False);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_With;

   ------------------
   -- Doc_HTML_Var --
   ------------------

   procedure Doc_HTML_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
      Line_Nr  : constant String := Info.Var_Entity.Line'Img;
      New_Text : GNAT.OS_Lib.String_Access;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Var_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      New_Text := Format_HTML (Info.Var_List,
                               Info.Var_Header.all,
                               Info.Var_Entity.File_Name.all,
                               Info.Var_Entity.Short_Name.all,
                               Info.Var_Entity.Line,
                               False,
                               True);

      Ada.Text_IO.Put_Line (File, New_Text.all);

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
      Line_Nr  : constant String := Info.Exception_Entity.Line'Img;
      New_Text : GNAT.OS_Lib.String_Access;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Exception_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      New_Text := Format_HTML (Info.Exception_List,
                               Info.Exception_Header.all,
                               Info.Exception_Entity.File_Name.all,
                               Info.Exception_Entity.Short_Name.all,
                               Info.Exception_Entity.Line,
                               False,
                               True);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
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
      Line_Nr  : constant String := Info.Type_Entity.Line'Img;
      New_Text : GNAT.OS_Lib.String_Access;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Type_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      New_Text := Format_HTML (Info.Type_List,
                               Info.Type_Header.all,
                               Info.Type_Entity.File_Name.all,
                               Info.Type_Entity.Short_Name.all,
                               Info.Type_Entity.Line,
                               False,
                               True);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Type_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");

   end Doc_HTML_Type;

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
                     Suffix := new String'("_adb.htm");
                  else
                     Suffix := new String'("_ads.htm");
                  end if;
                  declare
                     Body_File : constant String :=
                       TRL.Data (Node).File_Name.all
                       (TRL.Data (Node).File_Name'First ..
                          TRL.Data (Node).File_Name'Last - 4)
                     & Suffix.all;
                     Number : constant String :=
                       TRL.Data (Node).Line'Img;
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        "<TR><TD><A href="""
                        & Body_File & "#"
                        & Number (2 .. Number'Last)
                        & """>"
                        & TRL.Data
                          (Node).Subprogram_Name.all
                        & "</A></TD><TD> in &nbsp&nbsp<I>"
                        & TRL.Data (Node).File_Name.all
                        & "</I></TD><TD>, line: "
                        & TRL.Data (Node).Line'Img
                        & "</TD><TD>, column: "
                        & TRL.Data (Node).Column'Img
                        & "</TD><TR>");
                  end;
               --  no link at all
               else Ada.Text_IO.Put_Line (File,
                                          "<TR><TD>"
                                          & TRL.Data (Node).Subprogram_Name.all
                                          & "</TD><TD> in &nbsp&nbsp<I>"
                                          & TRL.Data (Node).File_Name.all
                                          & "</I></TD><TD>, line: "
                                          & TRL.Data (Node).Line'Img
                                          & "</TD><TD>, column: "
                                          & TRL.Data (Node).Column'Img
                                          & "</TD></TR>");
               end if;
               Node := TRL.Next (Node);
            end loop;
               Ada.Text_IO.Put_Line (File, "</TABLE>");
         end if;
      end Print_Ref_List;

      Line_Nr      : constant String := Info.Subprogram_Entity.Line'Img;
      New_Text     : GNAT.OS_Lib.String_Access;

   begin  --  Doc_HTML_Subprogram

   --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR> ");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Subprogram_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      New_Text := Format_HTML (Info.Subprogram_List,
                               Info.Subprogram_Header.all,
                               Info.Subprogram_Entity.File_Name.all,
                               Info.Subprogram_Entity.Short_Name.all,
                               Info.Subprogram_Entity.Line,
                               False,
                               True);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
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

   function Format_HTML
     (Entity_List   : Type_Entity_List.List;
      Text          : String;
      File_Name     : String;
      Entity_Name   : String;
      Entity_Line   : Natural;
      Is_Body       : Boolean;
      Do_Checks     : Boolean) return GNAT.OS_Lib.String_Access is
      Old_Line, Result_Line : GNAT.OS_Lib.String_Access;
      --  used to save the lines processed by the callback function

      Already_Added_Chars  : Integer;
      --  how many chars have been already added in this line

      Replacing_Tags_Chars  : Integer;
      --  how many chars added while replacing HTML tags

      function Is_Called_Reference_Found
        (Line_Nr     : Natural;
         Source_File : String;
         Called_Node :  Entity_List_Information) return Boolean;
      --  looks in the Called_List if the entity is called called in the
      --  given file at the given line.

      function Replace_HTML_Tags
        (Input_Text : String) return String;
      --  replaces all "<"  which are by "&lt;" and all ">" by "&gt;"

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

      ---------------------------------
      --  Is_Called_Reference_Found  --
      ---------------------------------

      function Is_Called_Reference_Found
        (Line_Nr     : Natural;
         Source_File : String;
         Called_Node : Entity_List_Information) return Boolean is

         use Type_Reference_List;

         Ref_Node : List_Node;
      begin
         if not Is_Empty (Called_Node.Called_List) then
            Ref_Node := First (Called_Node.Called_List);
            for J in 1 .. Length (Called_Node.Called_List) loop
               if Data (Ref_Node).Line = Line_Nr and
                 Data (Ref_Node).File_Name.all =
                 GNAT.Directory_Operations.File_Name (Source_File) then
                  return True;
               end if;
               Ref_Node := Next (Ref_Node);
            end loop;
         end if;
         return False;
      end Is_Called_Reference_Found;

      -----------------------
      -- Replace_HTML_Tags --
      -----------------------

      function Replace_HTML_Tags
        (Input_Text : String) return String is

         Old_Text, Result_Text : GNAT.OS_Lib.String_Access;
      begin
         Result_Text := new String'(Input_Text);

         for J in Input_Text'First .. Input_Text'Last - 1 loop
            if Input_Text (J) = '<' then
               Old_Text := Result_Text;
               Result_Text :=
                 new String'(Result_Text (Input_Text'First
                                           .. J - 1
                                             + Replacing_Tags_Chars) &
                             "&lt;" &
                             Result_Text (J + 1 + Replacing_Tags_Chars
                                           .. Input_Text'Last
                                             + Replacing_Tags_Chars));
               Free (Old_Text);
               Replacing_Tags_Chars := Replacing_Tags_Chars + 3;
               --  1 char replaced by 4 new chars => + 3
            elsif Input_Text (J) = '>' then
               Old_Text := Result_Text;
               Result_Text :=
                 new String'(Result_Text (Input_Text'First
                                           .. J - 1
                                             + Replacing_Tags_Chars) &
                             "&gt;" &
                             Result_Text (J + 1 + Replacing_Tags_Chars
                                           .. Input_Text'Last
                                             + Replacing_Tags_Chars));
               Free (Old_Text);
               Replacing_Tags_Chars := Replacing_Tags_Chars + 3;
               --  1 char replaced by 4 new chars => + 3
            end if;
         end loop;
         return Result_Text.all;
      end Replace_HTML_Tags;

      -------------------
      -- HTML_Callback --
      -------------------

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is

         Entity_Node       : TEL.List_Node;

      begin  -- HTML_Spec_Callback

         --  DON'T FORGET: when you change the HTML code to be
         --  added, change also the Already_Added_Chars

         if Partial_Entity then  --  just to avoid the warning
            null;
         end if;

         Old_Line := Result_Line;
         Replacing_Tags_Chars := 0;

         case Entity is
            when Comment_Text => Result_Line := new String'
                 (Result_Line.all
                    (Result_Line'First .. Sloc_Start.Index
                       + Already_Added_Chars - 1) &
                  "<FONT color=""green"">" &
                  Result_Line.all
                    (Sloc_Start.Index + Already_Added_Chars ..
                       Sloc_End.Index + Already_Added_Chars) &
                  "</FONT>" &
                  Result_Line.all
                    (Sloc_End.Index + Already_Added_Chars + 1 ..
                       Result_Line'Last));
               Already_Added_Chars := Already_Added_Chars + 27;
               --  27 is the number of chars added here
               Free (Old_Line);
            when Keyword_Text =>
               Result_Line := new String'
                 (Result_Line.all
                    (Result_Line'First .. Sloc_Start.Index
                       + Already_Added_Chars - 1) &
                  "<B>" &
                  Result_Line.all
                    (Sloc_Start.Index + Already_Added_Chars ..
                       Sloc_End.Index + Already_Added_Chars) &
                  "</B>" &
                  Result_Line.all
                    (Sloc_End.Index + Already_Added_Chars + 1 ..
                       Result_Line'Last));
               Already_Added_Chars := Already_Added_Chars + 7;
               --  7 is the number of chars in "<B></B>"
               Free (Old_Line);
            when String_Text =>
               Result_Line := new String'
                 (Result_Line.all
                    (Result_Line'First ..
                       Sloc_Start.Index + Already_Added_Chars - 1) &
                  "<FONT color=""red"">" &
                  Replace_HTML_Tags
                    (Result_Line.all
                       (Sloc_Start.Index + Already_Added_Chars ..
                          Sloc_End.Index + Already_Added_Chars)) &
                  "</FONT>" &
                  Result_Line.all
                    (Sloc_End.Index + Already_Added_Chars + 1
                       .. Result_Line'Last));
               Already_Added_Chars := Already_Added_Chars + 25 +
                 Replacing_Tags_Chars;
               --  25 is the number of chars added here
               Free (Old_Line);
            when Character_Text => Result_Line := new String'
                 (Result_Line.all
                    (Result_Line'First .. Sloc_Start.Index
                       + Already_Added_Chars - 1) &
                  "<FONT color=""red"">" &
                  Result_Line.all
                    (Sloc_Start.Index + Already_Added_Chars ..
                       Sloc_End.Index + Already_Added_Chars) &
                  "</FONT>" &
                  Result_Line.all
                    (Sloc_End.Index + Already_Added_Chars + 1 ..
                       Result_Line'Last));
               Already_Added_Chars := Already_Added_Chars + 25;
               --  25 is the number of chars added here
               Free (Old_Line);
            when Identifier_Text =>

               --  perhaps links can be set:

               --  look in the list, if the identifier is there
               --  if found => make a link; if not found => ignore!
               if not TEL.Is_Empty (Entity_List) then
                  Entity_Node := TEL.First (Entity_List);

                  for J in 1 .. TEL.Length (Entity_List) loop
                     --  check if the entity name is also the identiefier name
                     if TEL.Data (Entity_Node).Short_Name.all
                       = To_Lower (Text (Sloc_Start.Index ..
                                           Sloc_End.Index))
                     --  and the lines for the entity found in the list
                     --  are the same as for the entity passed to Format_HTML,
                     --  in the spec of in the body (in the latter case
                     --  it's important to look at the declaration line in the
                     --  body but also all references where it is called.
                     --  Or No Checks should be made.
                       and ((not Is_Body and
                               TEL.Data (Entity_Node).Line =
                               Entity_Line)
                            or (Is_Body and
                                  TEL.Data (Entity_Node).Line_In_Body =
                                  Sloc_Start.Line)
                            or (Is_Body and
                                  (TEL.Data (Entity_Node).Kind =
                                     Procedure_Entity or
                                       TEL.Data (Entity_Node).Kind
                                       = Function_Entity) and
                                      Is_Called_Reference_Found
                                        (Sloc_Start.Line,
                                         File_Name,
                                         TEL.Data (Entity_Node)))
                            or not Do_Checks)
                     --  and the kind of the entity is not Other_Entity
                     --  ONLY possible: subprograms, exceptions,
                     --  types and packages
                       and TEL.Data (Entity_Node).Kind /= Other_Entity
                       and TEL.Data (Entity_Node).Kind /= Var_Entity
                     --  and this line is not the declararion of the entity
                     --  (allowed for subprograms)
                       and (not (TEL.Data (Entity_Node).Line
                                   = Sloc_Start.Line and
                                     TEL.Data (Entity_Node).File_Name.all
                                     = File_Name) or
                                  TEL.Data (Entity_Node).Kind =
                                  Procedure_Entity or
                              TEL.Data (Entity_Node).Kind = Function_Entity)
                     then
                        --  if entity a subprogram and a link should and can be
                        --  set => creat link to body
                        if not Is_Body and
                          TEL.Data (Entity_Node).Line_In_Body > 0 and
                          (TEL.Data (Entity_Node).Kind = Procedure_Entity or
                             TEL.Data (Entity_Node).Kind = Function_Entity)
                        then
                           declare
                              Number : constant String :=
                                TEL.Data (Entity_Node).Line_In_Body'Img;
                              Spe_File  : GNAT.OS_Lib.String_Access;
                              Bod_File : GNAT.OS_Lib.String_Access;
                              Node      : GNAT.OS_Lib.String_Access;
                           begin
                              Node := TEL.Data (Entity_Node).File_Name;
                              Spe_File := new String'
                                (GNAT.Directory_Operations.File_Name
                                   (Node.all));
                              Bod_File := new String '(Spe_File.all
                                                      (Spe_File'First
                                                         .. Spe_File'Last - 4)
                                                    & "_adb.htm");
                              Result_Line := new String'
                                (Result_Line.all
                                   (Result_Line'First .. Sloc_Start.Index +
                                      Already_Added_Chars - 1) &
                                 "<A href=""" &
                                 Bod_File.all & "#" &
                                 Number (2 .. Number'Last) & """>" &
                                 Result_Line.all
                                   (Sloc_Start.Index + Already_Added_Chars ..
                                      Sloc_End.Index + Already_Added_Chars) &
                                 "</A>" &
                                 Result_Line.all
                                   (Sloc_End.Index + Already_Added_Chars + 1 ..
                                      Result_Line'Last));
                              Already_Added_Chars := Already_Added_Chars + 16 +
                                Bod_File'Length + Number'Length - 1;
                              Free (Old_Line);
                              Free (Spe_File);
                              Free (Bod_File);
                           end;
                           --  elsif: no subprograms, if working on the body
                           --  file => processed above and not here
                           --  here subprograms only if working on the
                           --  spec file
                        elsif (not (TEL.Data (Entity_Node).Kind =
                                      Procedure_Entity or
                                     TEL.Data (Entity_Node).Kind
                                     = Function_Entity) or Is_Body) and
                        --  and don't link if it is the entity itself, which
                        --  was found in its header => except for subprograms
                        --  (above) which are linked to the body, no linking
                        --  here.
                          To_Lower (TEL.Data (Entity_Node).Short_Name.all) /=
                          To_Lower (Entity_Name) then
                           --  for the rest: create the link for the entity
                           declare
                              Number    : constant String
                                := TEL.Data (Entity_Node).Line'Img;
                              Local_File : constant String
                                := Get_Html_File_Name
                                  (TEL.Data (Entity_Node).File_Name.all);
                              HTML_File : constant String :=
                                GNAT.Directory_Operations.File_Name
                                  (Local_File);
                           begin
                              Result_Line := new String'
                                (Result_Line.all
                                   (Result_Line'First .. Sloc_Start.Index +
                                      Already_Added_Chars - 1) &
                                 "<A href=""" &
                                 HTML_File & "#" &
                                 Number (2 .. Number'Last) & """>" &
                                 Result_Line.all
                                   (Sloc_Start.Index + Already_Added_Chars ..
                                      Sloc_End.Index + Already_Added_Chars) &
                                 "</A>" &
                                 Result_Line.all
                                   (Sloc_End.Index + Already_Added_Chars + 1 ..
                                      Result_Line'Last));
                              Already_Added_Chars := Already_Added_Chars +
                                16 + HTML_File'Length + Number'Length - 1;
                              Free (Old_Line);
                           end;
                        end if;
                        return False;
                     end if;
                     Entity_Node := TEL.Next (Entity_Node);
                  end loop;
               end if;
            when others => null;
         end case;
         return False;  --  later: use false or not?
      end HTML_Callback;

   begin  --  Format_HTML

      Already_Added_Chars := 0;
      Result_Line := new String'(Text);

      --  parse the entities in Text
      Parse_Entities (Ada_Lang,
                      Text,
                      HTML_Callback'Unrestricted_Access);
      return Result_Line;
   end Format_HTML;

   ---------------------
   -- Doc_HTML_Header --
   ---------------------

   procedure Doc_HTML_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Body_File_Name : GNAT.OS_Lib.String_Access;
      Line_Nr        : constant String := Info.Header_Line'Img;
   begin

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD>");
      Ada.Text_IO.Put_Line (File, " <H1>  Package <I>");
      Ada.Text_IO.Put_Line (File, " <A NAME=""" &
                            Line_Nr (2 .. Line_Nr'Length)
                            & """>");

      --  check if should set a link to the body file
      if Info.Header_Link then
         Body_File_Name :=
           new String '(Info.Header_File.all (Info.Header_File'First
                                                .. Info.Header_File'Last - 4)
                           & "_adb.htm");

         Ada.Text_IO.Put_Line (File, "<A HREF=""" &
                               File_Name (Body_File_Name.all) &
                               """> ");
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A></A></I></H1>");
      --  check if should set a link to the spec file
      elsif File_Extension (Info.Header_File.all) = ".adb" then
         Body_File_Name :=
           new String '(Info.Header_File.all (Info.Header_File'First
                                                .. Info.Header_File'Last - 4)
                           & "_ads.htm");

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

      if File_Extension (Info.Header_File.all) = ".adb" then
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
      if File_Extension (Info.Footer_File.all) = ".adb" then
         Ada.Text_IO.Put_Line (File, "</PRE>");
      end if;
   end Doc_HTML_Footer;

   --------------------------------
   -- Doc_HTML_Unit_Index_Header --
   --------------------------------

   procedure Doc_HTML_Unit_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Frame_File : File_Type;

   begin

      --  create the main frame file
      Create (Frame_File, Out_File, Info.Doc_Directory.all & "index.htm");
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
                            Info.First_File.all &
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
   begin
      --  just to avoid the warning that First_Dummy not used
      if False then
         Put_Line (Info.First_Dummy.all);
      end if;
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
   begin
      --  just to avoid the warning that Second_Dummy not used
      if False then
         Put_Line (Info.Second_Dummy.all);
      end if;
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
      Line_Nr : constant String := Info.Item_Line'Img;
   begin
      Ada.Text_IO.Put_Line (File, " <A href=""" & Info.Item_Doc_File.all
                            & "#" & Line_Nr (2 .. Line_Nr'Length)
                            & """ target=""main""> "
                            & Info.Item_Name.all & "</A>");

      --  Add the information where to find the type or subprogram
      --  Process only for the type index and the subprogram index,
      --  ignore for the file index.
      --  In the file index is always name = file!
      if Info.Item_Name.all /= Info.Item_File.all then
         Ada.Text_IO.Put_Line (File, " <BR> &nbsp&nbsp&nbsp&nbsp&nbsp in " &
                                 Info.Item_File.all);
      end if;

      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BR>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Index_Item;

   ------------------------
   -- Doc_HTML_Index_End --
   ------------------------

   procedure Doc_HTML_Index_End
   (File   : in Ada.Text_IO.File_Type;
    Info   : Doc_Info) is
   begin
      --  just to avoid the warning that End_Index_Title not used
      if False then
         Put_Line (Info.End_Index_Title.all);
      end if;
      Ada.Text_IO.Put_Line (File, "</BODY> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "</HTML>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Index_End;

   ------------------
   -- Chars_Before --
   ------------------

   function Chars_Before
     (Line    : String;
      Line_Nr : Natural) return Natural is
      Chars_Counter, Line_Counter  : Natural;
   begin
      Line_Counter  := 1;
      Chars_Counter := 1;
      while Line_Counter < Line_Nr loop
         if Line (Chars_Counter) = ASCII.LF then
            Line_Counter  := Line_Counter + 1;
         end if;

         Chars_Counter := Chars_Counter + 1;

      end loop;
      return Chars_Counter - Line_Nr + 1;
   end Chars_Before;

   --------------------
   --  Doc_HTML_Body --
   --------------------

   procedure Doc_HTML_Body
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is

      New_Text : GNAT.OS_Lib.String_Access;

      function Add_Address_Marks
        (Input_String : String) return GNAT.OS_Lib.String_Access;
      --  puts each line of the Input_String between
      --  <A name="Number of Line"> and </A>

      ------------------------
      --  Add_Address_Marks --
      ------------------------

      function Add_Address_Marks
        (Input_String : String) return GNAT.OS_Lib.String_Access is
         New_String : GNAT.OS_Lib.String_Access;
         Old_String : GNAT.OS_Lib.String_Access;
         Line_Nr, Already_Added_Ads : Natural;
      begin
         if Input_String'Last > 2 then
            New_String := new String'(" <A name=""1"">" &
                                      Input_String);
            --  here already added 13 chars
            Already_Added_Ads := 13;
            Line_Nr := 1;

            for J in 2 .. Input_String'Last loop
               if Input_String (J) = ASCII.LF then
                  Line_Nr := Line_Nr + 1;
                  declare
                     Number : constant String := Line_Nr'Img;
                  begin
                     Old_String := New_String;
                     New_String :=
                       new String'(New_String.all (1 .. J - 1 +
                                                     Already_Added_Ads) &
                                   " </A> " &
                                   ASCII.LF &
                                   " <A name=""" &
                                   Number (2 .. Number'Last) &
                                   """> " &
                                   Input_String
                                     (J + 1 .. Input_String'Last));
                     --  19 chars added here + (the length of the number - 1)
                     Already_Added_Ads := Already_Added_Ads + 19 +
                       Number'Length - 1;
                     Free (Old_String);
                  end;
               end if;
            end loop;
            Old_String := New_String;
            New_String := new String'(New_String.all & " </A>");
            Free (Old_String);

            return New_String;
         end if;
         return new String'(Input_String);
      end Add_Address_Marks;

   begin

      New_Text :=  Format_HTML (Info.Body_Entity_List,
                                Info.Body_Text.all,
                                Info.Body_File.all,
                                "",
                                0,
                                True,
                                True);

      New_Text := Add_Address_Marks (New_Text.all);

      --  write the changed body to the html file
      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
   end Doc_HTML_Body;

   ------------------------
   -- Get_Html_File_Name --
   ------------------------

   function Get_Html_File_Name
     (File : String) return String is
   begin
      if File_Extension (File) = ".ads" then
         return File (File'First .. File'Last - 4) & "_ads.htm";
      else
         return File (File'First .. File'Last - 4) & "_adb.htm";
      end if;
   end Get_Html_File_Name;

end Docgen.Html_Output;
