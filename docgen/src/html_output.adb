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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Language.Ada;              use Language.Ada;

package body Html_Output is

   package TEL renames Type_Entity_List;
   package ASU renames Ada.Strings.Unbounded;

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

      New_Text := Format_HTML (Info.Package_List,
                               Info.Package_Header.all,
                               Info.Package_Entity.File_Name.all,
                               Info.Package_Entity.Short_Name.all,
                               False);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "---" & Info.Package_Description.all);
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

      New_Text := Format_HTML (Info.Var_List,
                               Info.Var_Header.all,
                               Info.Var_Entity.File_Name.all,
                               Info.Var_Entity.Short_Name.all,
                               False);

      Ada.Text_IO.Put_Line (File, New_Text.all);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "---" & Info.Var_Description.all);
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

      New_Text := Format_HTML (Info.Exception_List,
                               Info.Exception_Header.all,
                               Info.Exception_Entity.File_Name.all,
                               Info.Exception_Entity.Short_Name.all,
                               False);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "---" & Info.Exception_Description.all);
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

      New_Text := Format_HTML (Info.Type_List,
                               Info.Type_Header.all,
                               Info.Type_Entity.File_Name.all,
                               Info.Type_Entity.Short_Name.all,
                               False);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "---" & Info.Type_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");

   end Doc_HTML_Type;

   -------------------------
   -- Doc_HTML_Subprogram --
   -------------------------

   procedure Doc_HTML_Subprogram
     (File      : in Ada.Text_IO.File_Type;
      Info      : Doc_Info) is

      package TRL renames Type_Reference_List;
      Node      : TRL.List_Node;

      Line_Nr      : constant String := Info.Subprogram_Entity.Line'Img;
      Body_File    : ASU.Unbounded_String;
      New_Text     : GNAT.OS_Lib.String_Access;
   begin
   --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR> ");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      New_Text := Format_HTML (Info.Subprogram_List,
                               Info.Subprogram_Header.all,
                               Info.Subprogram_Entity.File_Name.all,
                               Info.Subprogram_Entity.Short_Name.all,
                               False);

      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");

      --  write the description to doc file
      Ada.Text_IO.Put_Line (File, "---" & Info.Subprogram_Description.all);
      Ada.Text_IO.Put_Line (File, " <BR>  ");

      --  write the "subprogram is called be:" references
      --  if Info.Subprogram_Entity.Is_Ref_List_Set then
      if not TRL.Is_Empty (Info.Subprogram_Entity.Ref_List) then
         Ada.Text_IO.Put_Line (File, "<H5> Subprogram is called in: </H5> ");

         Node := TRL.First (Info.Subprogram_Entity.Ref_List);
         --  for every reference found write the information to doc file
         for J in 1 .. TRL.Length (Info.Subprogram_Entity.Ref_List) loop

            --  check if the creating of a link is possible
            if TRL.Data (Node).File_Found then
               Body_File :=
                 ASU.To_Unbounded_String (TRL.Data (Node).File_Name.all);
               Body_File := ASU.Replace_Slice (Body_File,
                                                ASU.Index (Body_File, "."),
                                                ASU.Index (Body_File, ".") + 3,
                                                "_adb.htm");
               declare
                  Number : constant String := TRL.Data (Node).Line'Img;
               begin
                  Ada.Text_IO.Put_Line (File, "  <A href="""
                                         & ASU.To_String (Body_File) & "#"
                                         & Number (2 .. Number'Last)
                                         & """> In file: </A> "
                                         & TRL.Data (Node).File_Name.all
                                         & "  / line: "
                                         & TRL.Data (Node).Line'Img & "<BR>");
               end;

            else
               Ada.Text_IO.Put_Line (File, "   In file:  "  &
                                     TRL.Data (Node).File_Name.all &
                                     " / Line: "   &
                                     TRL.Data (Node).Line'Img &
                                     "<BR>");

            end if;
            Node := TRL.Next (Node);
         end loop;
      end if;

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
      Is_Body       : Boolean) return GNAT.OS_Lib.String_Access is

      --  used to save the lines processed by the callback function
      Old_Line, Result_Line : GNAT.OS_Lib.String_Access;
      --  how many chars have been already added in this line
      Already_Added_Chars  : Integer;
      --  how many chars added while replacing HTML tags
      Replacing_Tags_Chars  : Integer;

      function Replace_HTML_Tags
        (Input_Text : String) return String;
      --  replaces all "<"  which are by "&lt;" and all ">" by "&gt;"

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      -----------------------
      -- Replace_HTML_Tags --
      -----------------------

      function Replace_HTML_Tags
        (Input_Text : String) return String is

         Old_Text, Result_Text : GNAT.OS_Lib.String_Access;
      begin
         Result_Text := new String'(Input_Text);

         for J in 1 .. Input_Text'Last - 1 loop
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
               --  1 char replaced by 4 new chars
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
               --  1 char replaced by 4 new chars
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

         --  Put_Line (Current_Error, Result_Line.all
         --            (Sloc_Start.Index + Already_Added_Chars ..
         --               Sloc_End.Index + Already_Added_Chars));


         --  DON'T FORGET: when you change the HTML code to
         --  added, change also the Already_Added_Chars
         --  AND add the tag beginning in Replace_HTML_Tags !!!

         if Partial_Entity then  --  just to avoid the warning
            null;
         end if;

         --  to free the access string later
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
            when String_Text => Result_Line := new String'
                 (Result_Line.all
                    (Result_Line'First ..
                       Sloc_Start.Index + Already_Added_Chars - 1) &
                  "<FONT color=""red"">" &
                  Replace_HTML_Tags (Result_Line.all
                    (Sloc_Start.Index + Already_Added_Chars ..
                       Sloc_End.Index + Already_Added_Chars)) &
                  "</FONT>" &
                  Result_Line.all
                    (Sloc_End.Index + Already_Added_Chars + 1
                    .. Result_Line'Last));
               Already_Added_Chars := Already_Added_Chars + 25 +
                 Replacing_Tags_Chars;
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
               Free (Old_Line);
            when Identifier_Text =>

               --  look in the list, if the identifier is there
               --  if found => make a link; if not found => ignore!
               if not TEL.Is_Empty (Entity_List) then
                  Entity_Node := TEL.First (Entity_List);
               end if;

               for J in 1 .. TEL.Length (Entity_List) loop
                  --  check if the entity name is also the identiefier name
                  if TEL.Data (Entity_Node).Short_Name.all
                    = To_Lower (Text (Sloc_Start.Index ..
                                                 Sloc_End.Index))
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
                        TEL.Data (Entity_Node).Kind = Procedure_Entity or
                        TEL.Data (Entity_Node).Kind = Function_Entity)
                  then
                     --  if entity a subprogram and a link should and can be
                     --  set => creat link to body
                     if not Is_Body and TEL.Data (Entity_Node).File_Found and
                       (TEL.Data (Entity_Node).Kind = Procedure_Entity or
                          TEL.Data (Entity_Node).Kind = Function_Entity) then
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
                        --  elsif: no subprograms, if should be
                        --  => above and not here
                     elsif not (TEL.Data (Entity_Node).Kind =
                                  Procedure_Entity or
                                    TEL.Data (Entity_Node).Kind
                                    = Function_Entity) and
                     --  and don't link if it is the entity itself, which was
                     --  found in its header => except for subprograms (above)
                     --  which are linked to the body, no linking here
                       To_Lower (TEL.Data (Entity_Node).Short_Name.all) /=
                       To_Lower (Entity_Name) then
                        --  for the rest: create the link for the entity
                        declare
                           Number    : constant String
                             := TEL.Data (Entity_Node).Line'Img;
                           HTML_File : constant String
                             := Get_Html_File_Name
                               (TEL.Data (Entity_Node).File_Name.all);
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
            when others => null;
         end case;
         return False;  --  later: use false or not?
      end HTML_Callback;

   begin  --  Format_HTML

      Already_Added_Chars := 0;
      Result_Line := new String'(Text);

      --  parse the entities in this line
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

      --  check if set a link to the body file
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
      --  check if set a link to the spec file
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
      if False then        --  just to avoid the warning
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
      if False then        --  just to avoid the warning
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
      if False then        --  just to avoid the warning
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

   --  returns the sum of the number of chars in
   --  the lines until Line_Nr-1 in the Line string
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
     (File : String) return String
   is
      Html_File : ASU.Unbounded_String;
   begin
      Html_File := ASU.To_Unbounded_String (File_Name (File));
      if File_Extension (File) = ".ads" then
         return ASU.To_String
           ((ASU.Replace_Slice (Html_File,
                                ASU.Index (Html_File, "."),
                                ASU.Index (Html_File, ".") + 3, "_ads.htm")));
      else
         return ASU.To_String
           ((ASU.Replace_Slice (Html_File,
                                ASU.Index (Html_File, "."),
                                ASU.Index (Html_File, ".") + 3, "_adb.htm")));
      end if;
   end Get_Html_File_Name;

end Html_Output;
