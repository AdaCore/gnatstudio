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
         when Body_Line_Info        => Doc_HTML_Body_Line (File, Info);
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
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Format_HTML_Entity_Header (File,
                                 Info.Package_List,
                                 False,
                                 Info.Package_Entity.Header_Lines,
                                 Info.Package_Entity.Header.all,
                                 Info.Package_Entity.File_Name.all,
                                 Info.Package_Entity.Line,
                                 Info.Package_Entity.Is_Private);
      Ada.Text_IO.Put_Line (File, "---" & Info.Package_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Package;

   -------------------
   -- Doc_HTML_With --
   -------------------

   procedure Doc_HTML_With
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Format_HTML_Entity_Header (File, Info.With_List, False,
                                 Count_Lines (Info.With_Lines.all),
                                 Info.With_Lines.all,
                                 Info.With_File.all,
                                 1,
                                 False);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_With;

   ------------------
   -- Doc_HTML_Var --
   ------------------

   procedure Doc_HTML_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
      Line_Nr : constant String := Info.Var_Entity.Line'Img;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Format_HTML_Entity_Header (File, Info.Var_List, False,
                                 Info.Var_Entity.Header_Lines,
                                 Info.Var_Entity.Header.all,
                                 Info.Var_Entity.File_Name.all,
                                 Info.Var_Entity.Line,
                                 Info.Var_Entity.Is_Private);
      Ada.Text_IO.Put_Line (File, "---" & Info.Var_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Var;

   ------------------------
   -- Doc_HTML_Exception --
   ------------------------

   procedure Doc_HTML_Exception
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
      Line_Nr : constant String := Info.Exception_Entity.Line'Img;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Format_HTML_Entity_Header (File, Info.Exception_List, False,
                                 Info.Exception_Entity.Header_Lines,
                                 Info.Exception_Entity.Header.all,
                                 Info.Exception_Entity.File_Name.all,
                                 Info.Exception_Entity.Line,
                                 Info.Exception_Entity.Is_Private);
      Ada.Text_IO.Put_Line (File, "---" & Info.Exception_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Exception;

   -------------------
   -- Doc_HTML_Type --
   -------------------

   procedure Doc_HTML_Type
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
      Line_Nr : constant String := Info.Type_Entity.Line'Img;
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR>");

      Format_HTML_Entity_Header (File, Info.Type_List, False,
                                 Info.Type_Entity.Header_Lines,
                                 Info.Type_Entity.Header.all,
                                 Info.Type_Entity.File_Name.all,
                                 Info.Type_Entity.Line,
                                 Info.Type_Entity.Is_Private);
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
   begin
   --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Line_Nr (2 .. Line_Nr'Length)
                            & """></A>  <BR> ");

      Format_HTML_Entity_Header (File,
                                 Info.Subprogram_List,
                                 Info.Subprogram_Link,
                                 Info.Subprogram_Entity.Header_Lines,
                                 Info.Subprogram_Entity.Header.all,
                                 Info.Subprogram_Entity.File_Name.all,
                                 Info.Subprogram_Entity.Line,
                                 Info.Subprogram_Entity.Is_Private);


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

   -------------------------------
   -- Format_HTML_Entity_Header --
   -------------------------------

   procedure Format_HTML_Entity_Header
     (File              : in Ada.Text_IO.File_Type;
      New_Entity_List   : Type_Entity_List.List;
      Here_Process_Body : Boolean;
      Nr_Lines          : Natural;
      Header            : String;
      File_Name         : String;
      Def_Line          : Natural;
      Is_Private        : Boolean) is

      --  used to save the lines processed by the callback function
      Old_Line, New_Line   : Unbounded_String;
      --  how many chars have been already added in this line
      Already_Added_Chars  : Integer;
      --  how many chars are being added for this entity
      New_Added_Chars      : Integer;
      --  to garantee access to the entity list
      Entity_List          : TEL.List;
      --  how many chars in all the lines before in this header
      Chars_Nr_Before      : Integer;
      --  name of the file
      Source_File          : Unbounded_String;
      --  will also the body file be processed
      Process_Body         : Boolean;
      --  the line of the spec being processed
      Line_Number          : Natural;

      Chars_In_Old_Lines : Natural;
      Chars_In_New_Lines : Natural;


      function Replace_HTML_Tags
        (Input_Line : String) return String;
      --   replaces all "<"  which are NOT in a commentin by "&lt;"

      function HTML_Spec_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      -----------------------
      -- Replace_HTML_Tags --
      -----------------------

      function Replace_HTML_Tags
        (Input_Line : String) return String is

         Line : ASU.Unbounded_String := ASU.To_Unbounded_String (Input_Line);
      begin
         while ASU.Index (Line, "<") > 0 loop
            if ASU.Index (Line, "--") = 0
              or (ASU.Index (Line, "--") > ASU.Index (Line, "<")) then
               Line := ASU.Replace_Slice (Line,
                                       ASU.Index (Line, "<"),
                                       ASU.Index (Line, "<"),
                                       "&lt;");
               New_Added_Chars := New_Added_Chars + 3;
            else
               return ASU.To_String (Line);
            end if;
         end loop;
         return ASU.To_String (Line);
      end Replace_HTML_Tags;

      ------------------------
      -- HTML_Spec_Callback --
      ------------------------

      function HTML_Spec_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is

         Entity_Node       : TEL.List_Node;

         function Is_Variable return Boolean;
         --  checks if the position of the entity in the
         --  string is followed by a ":" => then variable

         function Is_Variable return Boolean is
         begin
            if Sloc_Start.Column < Get_String_Index
              (To_String (Old_Line), 1, ":") then
               return True;
            else
               return False;
            end if;
         end Is_Variable;

      begin  -- HTML_Spec_Callback

         --  DON'T FORGET: when you change the HTML code to
         --  added, change also the Already_Added_Chars!!!

         if Partial_Entity then  --  just to avoid the warning
            null;
         end if;

         New_Added_Chars := 0;

         case Entity is
            when Comment_Text => null;
            when Keyword_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<B>"
                    & To_String (Old_Line)(Sloc_Start.Column
                                          .. Sloc_End.Column)
                  & "</B>");
               Already_Added_Chars := Already_Added_Chars + 7;
               --  7 is the number of chars in "<B></B>"
            when String_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<FONT color=""red"">"
                  --  To_String(Old_Line)(Sloc_Start.Column..Sloc_End.Column)
                    & Replace_HTML_Tags
                      (To_String (Old_Line)
                         (Sloc_Start.Column .. Sloc_End.Column))
                  & "</FONT>");
               Already_Added_Chars := Already_Added_Chars +
                 25 + New_Added_Chars;
            when Character_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<FONT color=""#666666"">"
                    & To_String (Old_Line)(Sloc_Start.Column
                                          .. Sloc_End.Column)
                  & "</FONT>");
               Already_Added_Chars := Already_Added_Chars + 29;
            when Identifier_Text =>

               --  look in the list, if the identifier is there
               --  if found => make a link; if not found => ignore!
               if not TEL.Is_Empty (Entity_List) then
                  Entity_Node := TEL.First (Entity_List);
               end if;
               for J in 1 .. TEL.Length (Entity_List) loop

                  --  check if the entity name is also the identiefier name
                  if TEL.Data (Entity_Node).Short_Name.all
                    = To_Lower (To_String (Old_Line)
                               (Sloc_Start.Column .. Sloc_End.Column))
                  --  and the kind of the entity is not Other_Entity
                  --  ONLY possible: subprograms, exceptions,
                  --  types and packages
                    and TEL.Data (Entity_Node).Kind /= Other_Entity
                  --  and this line is not the declararion of the entity
                  --  (allowed for subprograms)
                    and (not (TEL.Data (Entity_Node).Line = Line_Number and
                             TEL.Data (Entity_Node).File_Name.all
                             = ASU.To_String (Source_File)) or
                        TEL.Data (Entity_Node).Kind = Procedure_Entity or
                        TEL.Data (Entity_Node).Kind = Function_Entity)
                  then
                     --  if entity a subprogram and a link should and can be
                     --  set => creat link to body
                     if Process_Body and TEL.Data (Entity_Node).File_Found and
                       (TEL.Data (Entity_Node).Kind = Procedure_Entity or
                          TEL.Data (Entity_Node).Kind = Function_Entity) then
                        declare
                           Number : constant String :=
                             TEL.Data (Entity_Node).Line_In_Body'Img;
                           Spec_File, Body_File : ASU.Unbounded_String;

                           Node : GNAT.OS_Lib.String_Access;
                        begin
                           Node := TEL.Data (Entity_Node).File_Name;
                           Spec_File := ASU.To_Unbounded_String
                             (GNAT.Directory_Operations.File_Name
                                (Node.all));
                           Body_File :=
                             ASU.Replace_Slice
                               (Spec_File,
                                ASU.Index (Spec_File, "."),
                                ASU.Index (Spec_File, ".") + 3, "_adb.htm");
                           New_Line := ASU.Replace_Slice
                             (New_Line, Sloc_Start.Column +
                                Already_Added_Chars,
                              Sloc_End.Column   + Already_Added_Chars,
                              "<A href="""
                                & ASU.To_String (Body_File)
                              & "#"
                                & Number (2 .. Number'Last)
                              & """>"
                                & To_String (Old_Line)
                                (Sloc_Start.Column .. Sloc_End.Column)
                              & "</A>");
                           Already_Added_Chars := Already_Added_Chars + 16 +
                             ASU.Length (Body_File) + Number'Length - 1;
                        end;
                     elsif not Is_Variable and
                     not (TEL.Data (Entity_Node).Kind = Procedure_Entity or
                         TEL.Data (Entity_Node).Kind = Function_Entity)
                     then
                        --  for the rest: create the link for the entity
                        declare
                           Number    : constant String
                             := TEL.Data (Entity_Node).Line'Img;
                           HTML_File : constant String
                             := Get_Html_File_Name
                               (TEL.Data (Entity_Node).File_Name.all);
                        begin
                           New_Line := ASU.Replace_Slice
                             (New_Line, Sloc_Start.Column +
                                Already_Added_Chars,
                              Sloc_End.Column   + Already_Added_Chars,
                              "<A href="""
                                & HTML_File
                              & "#"
                                & Number (2 .. Number'Last)
                              & """>"
                                & To_String
                                  (Old_Line)
                                  (Sloc_Start.Column .. Sloc_End.Column)
                              & "</A>");
                           Already_Added_Chars := Already_Added_Chars +
                             16 + HTML_File'Length + Number'Length - 1;
                        end;
                     end if;
                     return False;
                  end if;
                  Entity_Node := TEL.Next (Entity_Node);
               end loop;
            when others => New_Line :=
                 ASU.To_Unbounded_String (Replace_HTML_Tags
                                         (To_String (Old_Line)
                                            (Sloc_Start.Column
                                               .. Sloc_End.Column)));

               Already_Added_Chars := Already_Added_Chars + New_Added_Chars;
         end case;
         return False;  --  later: use false or not?
      end HTML_Spec_Callback;

   begin
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                              "width=""100%""><TR><TD> <PRE>");

      if Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>");
         Ada.Text_IO.New_Line (File);
      end if;


      --  for each line in the header
      for J in 1 .. Nr_Lines loop
         --  find out the number of chars in all the lines before this line
         Chars_In_Old_Lines
           := Chars_Before (Header, J);
         if J = Nr_Lines then
            Chars_In_New_Lines := Header'Length - (J - 1);
         else
            Chars_In_New_Lines := Chars_Before (Header, J + 1);
         end if;

         --  the global variables for the callback function of the parser
         New_Line            :=
           ASU.To_Unbounded_String (Header (Chars_In_Old_Lines + (J - 1) ..
                                              Chars_In_New_Lines + (J - 2)));
         Old_Line            := New_Line;
         Already_Added_Chars := 0;
            --  how many chars in the lines before the current line
         Chars_Nr_Before     := 0;

         Entity_List         := New_Entity_List;
         Process_Body        := Here_Process_Body;
         Line_Number         := Def_Line;
         Source_File         := ASU.To_Unbounded_String (File_Name);

         --  parse the entities in this line
         Parse_Entities (Ada_Lang,
                            Header (Chars_In_Old_Lines .. Chars_In_New_Lines),
                            HTML_Spec_Callback'Unrestricted_Access);

         Ada.Text_IO.Put_Line (File, To_String (New_Line));
      end loop;
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Format_HTML_Entity_Header;

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
           new String '(Info.Header_File.all (1 .. Info.Header_File'Last - 4)
                           & "_adb.htm");

         Ada.Text_IO.Put_Line (File, "<A HREF=""" &
                               File_Name (Body_File_Name.all) &
                               """> ");
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A></A></I></H1>");
      --  check if set a link to the spec file
      elsif File_Extension (Info.Header_File.all) = ".adb" then
         Body_File_Name :=
           new String '(Info.Header_File.all (1 .. Info.Header_File'Last - 4)
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

   ------------------------
   -- Doc_HTML_Body_Line --
   ------------------------

   procedure Doc_HTML_Body_Line
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is


      --   replaces all "<"  which are NOT in a commentin by "&lt;"
      function Replace_HTML_Tags
        (Input_Line : String) return String;

      function HTML_Body_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      --  used to save the lines processed by the callback function
      Old_Line, New_Line   : Unbounded_String;
      --  how many chars have been already added in this line
      Already_Added_Chars  : Integer;
      --  how many chars are being added for this entity
      New_Added_Chars      : Integer;
      --  to garantee access to the entity list
      Entity_List          : TEL.List;
      --  how many chars in all the lines before in this header
      Chars_Nr_Before      : Integer;
      --  name of the file
      Source_File          : Unbounded_String;

      -----------------------
      -- Replace_HTML_Tags --
      -----------------------

      --   replaces all "<"  which are NOT in a commentin by "&lt;"
      function Replace_HTML_Tags
        (Input_Line : String) return String is

         Line : ASU.Unbounded_String := ASU.To_Unbounded_String (Input_Line);
      begin
         while ASU.Index (Line, "<") > 0 loop
            if ASU.Index (Line, "--") = 0
              or (ASU.Index (Line, "--") > ASU.Index (Line, "<")) then
               Line := ASU.Replace_Slice (Line,
                                       ASU.Index (Line, "<"),
                                       ASU.Index (Line, "<"),
                                       "&lt;");
               New_Added_Chars := New_Added_Chars + 3;
            else
               return ASU.To_String (Line);
            end if;
         end loop;
         return ASU.To_String (Line);
      end Replace_HTML_Tags;

      ------------------------
      -- HTML_Body_Callback --
      ------------------------

      --  here for each entity found in the line,
      --  the New_Line string will be formatted
      function HTML_Body_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is

         Entity_Node    : TEL.List_Node;

         function Is_Declaration (Line      : String;
                               Start_Col : Integer;
                               End_Col   : Integer) return Boolean;
         --  checks if the last Entity found in this line is declared here
         --  => check if the entity name is followed by a ":"

         function Is_Declaration (Line      : String;
                               Start_Col : Integer;
                               End_Col   : Integer) return Boolean
         is
            Col  : Integer;
         begin
            Col := Start_Col;  --  just to avoid the warning
            Col := End_Col + 1;

            --  find the first character after the entity
            while Line (Col) = ' ' loop
               Col := Col + 1;
            end loop;

            if Line (Col) = ';' then
               return False;
            end if;

            if Col + 6 < Line'Last then
               if Line (Col .. Col + 6) = "renames" then
                  return True;
               end if;
            end if;

            if Line (Col) = ':' then
               if Col + 1 < Line'Last then
                  if Line (Col + 1) = '=' then
                     return False;
                  else
                     return True;
                  end if;
               else
                  return True;
               end if;
            end if;

            if  Line (Col) = ',' then
               while Col < Line'Last loop
                  Col := Col + 1;
                  if Line (Col) = ';' then
                     return False;
                  end if;
                  if Line (Col) = ':' and Line (Col + 1) /= '=' then
                     return True;
                  end if;
               end loop;
               return False;
            end if;

            return False;
         end Is_Declaration;

      begin  --  HTML_Body_Callback

         --  DON'T FORGET: when you change the HTML code to added,
         --  change also the Already_Added_Chars!!!

         if Partial_Entity then  --  just to avoid the warning
            null;
         end if;

         New_Added_Chars := 0;

         case Entity is
            when Comment_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<FONT color=""green"">"
                    & To_String
                      (Old_Line)(Sloc_Start.Column .. Sloc_End.Column)
                  & "</FONT>");
               Already_Added_Chars := Already_Added_Chars + 27;
               --  27 is the number of chars in "<FONT c.."
            when Keyword_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<B>"
                    & To_String
                      (Old_Line)(Sloc_Start.Column .. Sloc_End.Column)
                  & "</B>");
               Already_Added_Chars := Already_Added_Chars + 7;
               --  7 is the number of chars in "<B></B>"
            when String_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<FONT color=""red"">"
                  --  To_String(Old_Line)(Sloc_Start.Column..Sloc_End.Column)
                    & Replace_HTML_Tags
                      (To_String (Old_Line)(Sloc_Start.Column ..
                                           Sloc_End.Column))
                  & "</FONT>");
               Already_Added_Chars := Already_Added_Chars +
                 25 + New_Added_Chars;
            when Character_Text => New_Line := ASU.Replace_Slice
                 (New_Line, Sloc_Start.Column + Already_Added_Chars,
                  Sloc_End.Column   + Already_Added_Chars,
                  "<FONT color=""#666666"">"
                    & To_String
                      (Old_Line)(Sloc_Start.Column .. Sloc_End.Column)
                  & "</FONT>");
               Already_Added_Chars := Already_Added_Chars + 29;
            when Identifier_Text =>

               --  look in the list, if the identifier is there
               --  if found => make a link; if not found => ignore!
               --  put_line ("elements in list: " & Entity_List_Length'Img);

               if not TEL.Is_Empty (Entity_List) then
                  Entity_Node := TEL.First (Entity_List);
               end if;
               for J in 1 .. TEL.Length (Entity_List) loop

                  --  check if the entity name is also the identiefier name
                  if TEL.Data (Entity_Node).Short_Name.all
                    = To_Lower (To_String (Old_Line)
                               (Sloc_Start.Column .. Sloc_End.Column))
                  then
                     --  and if this line is the declaration
                     --  of the entity. if so => ignore!
                     if not Is_Declaration
                       (ASU.To_String (Old_Line),
                        Sloc_Start.Column,
                        Sloc_End.Column)
                       and
                     --  ignore also all variables NOT defined in this
                     --  source file
                       (TEL.Data (Entity_Node).Kind /= Other_Entity or
                          TEL.Data (Entity_Node).File_Name.all =
                          To_String (Source_File))
                     then
                        --  create make the link
                        declare
                           Number    : constant String :=
                             TEL.Data (Entity_Node).Line'Img;
                           HTML_File : constant String :=
                             Get_Html_File_Name
                               (TEL.Data (Entity_Node).File_Name.all);
                        begin
                           New_Line := ASU.Replace_Slice
                             (New_Line, Sloc_Start.Column +
                                Already_Added_Chars,
                              Sloc_End.Column   + Already_Added_Chars,
                              "<A href="""
                                & HTML_File
                              & "#"
                                & Number (2 .. Number'Last)
                              & """>"
                                & To_String (Old_Line)(Sloc_Start.Column
                                                      .. Sloc_End.Column)
                              & "</A>");
                           Already_Added_Chars := Already_Added_Chars +
                             16 + HTML_File'Length + Number'Length - 1;
                        end;
                        return False;
                     end if;
                  end if;
                  Entity_Node := TEL.Next (Entity_Node);
               end loop;

            when others => New_Line := ASU.To_Unbounded_String
                 (Replace_HTML_Tags
                    (To_String (Old_Line)
                       (Sloc_Start.Column .. Sloc_End.Column)));
               Already_Added_Chars := Already_Added_Chars +
                 New_Added_Chars;
         end case;
         return False;  --  later: use false or not?
      end HTML_Body_Callback;

   begin
      --  the global variables for the callback function of the parser
      New_Line            := ASU.To_Unbounded_String (Info.Body_Text.all);
      Old_Line            := New_Line;
      Already_Added_Chars := 0;
      Chars_Nr_Before     := 0;
      Source_File         := ASU.To_Unbounded_String (Info.Body_File.all);
      Entity_List         := Info.Body_List;

      --  parse the entities in this line
      Parse_Entities (Ada_Lang,
                      Info.Body_Text.all,
                      HTML_Body_Callback'Unrestricted_Access);

      --  each line will get a mark with its number as name
      --  Add_Mark_For_Entity :
      --  declare
      --  Number : String := Info.Body_Line_Nr'Img;
      --  begin
      --   New_Line := "<A name=""" & Number (2 .. Number'Last) &
      --  """> " & New_Line & " </A>";
      --  end Add_Mark_For_Entity;

      --  write the line to the htm-file
      Ada.Text_IO.Put_Line (File, To_String (New_Line));
   end Doc_HTML_Body_Line;

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
