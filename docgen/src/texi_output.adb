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

package body Texi_Output is

   package TEL renames Type_Entity_List;
   package ASU renames Ada.Strings.Unbounded;

   procedure Doc_TEXI_Create
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_TEXI_Open (File, Info);
         when Close_Info            => Doc_TEXI_Close (File, Info);
         when Header_Info           => Doc_TEXI_Header (File, Info);
         when Footer_Info           => Doc_TEXI_Footer (File, Info);
         when Subtitle_Info         => Doc_TEXI_Subtitle (File, Info);
         when Package_Desc_Info     => Doc_TEXI_Pack_Desc (File, Info);
         when With_Info             => Doc_TEXI_With (File, Info);
         when Package_Info          => Doc_TEXI_Package (File, Info);
         when Var_Info              => Doc_TEXI_Var (File, Info);
         when Subprogram_Info       => Doc_TEXI_Subprogram (File, Info);
         when Type_Info             => Doc_TEXI_Type (File, Info);
         when Exception_Info        => Doc_TEXI_Exception (File, Info);
            --  when Body_Line_Info        => Doc_TEXI_Body_Line (File, Info);
         when others => null;
      end case;

   end Doc_TEXI_Create;

   -------------------
   -- Doc_TEXI_Open --
   -------------------

   procedure Doc_TEXI_Open
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Node : Type_Source_File_List.List_Node;

   begin
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "\input texinfo  @c -*-texinfo-*-");
      Ada.Text_IO.Put_Line (File, "@c %**start of header");
      Ada.Text_IO.Put_Line (File, "@setfilename "
                              & Info.Open_File.all
                              & ".info");
      Ada.Text_IO.Put_Line (File, "@settitle "
                                  & Info.Open_Title.all);
      Ada.Text_IO.Put_Line (File, "@c %**end of header");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "@titlepage");
      Ada.Text_IO.Put_Line (File, "@title "
                                  & Info.Open_Title.all);
      Ada.Text_IO.Put_Line (File, "@end titlepage");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "@c    Node  Next Last  Father");
      Ada.Text_IO.Put_Line (File, "@node Top,   ,   ,   (dir)");
      Ada.Text_IO.New_Line (File);

      --  print the mune of all packages
      if not Type_Source_File_List.Is_Empty (Info.Open_Package_List) then
         Ada.Text_IO.Put_Line (File, "@menu");
         Node := Type_Source_File_List.First (Info.Open_Package_List);
         for J in 1 .. Type_Source_File_List.Length (Info.Open_Package_List)
         loop
            Ada.Text_IO.Put_Line (File, "* " &
                   Type_Source_File_List.Data (Node).Package_Name.all);
            Node := Type_Source_File_List.Next (Node);
         end loop;
         Ada.Text_IO.Put_Line (File, "@end menu");
         Ada.Text_IO.New_Line (File);
      end if;

   end Doc_TEXI_Open;

   --------------------
   -- Doc_TEXI_Close --
   --------------------

   procedure Doc_TEXI_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      if false then      --  just to avoid the warning
         Ada.Text_IO.Put_Line (File, Info.Close_Title.all);
      end if;
      Ada.Text_IO.Put_Line (File, "@node index, subprograms,   , docu");
      Ada.Text_IO.Put_Line (File, "@printindex fn");
      Ada.Text_IO.Put_Line (File, "@printindex tp");
      Ada.Text_IO.Put_Line (File, "@contents");
      Ada.Text_IO.Put_Line (File, "@bye");
   end Doc_TEXI_Close;

   -----------------------
   -- Doc_TEXI_Subtitle --
   -----------------------

   procedure Doc_TEXI_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

   begin
      --  if the order of the information is changed in work_on_source:
      --  Process_Source, this contents of the strings must be changed too!
      --  and replace the last type also in doc_TEXI_Close

      Ada.Text_IO.Put_Line (File, "@c -------------------- SECTION " &
                            "-------------------");

      case Info.Subtitle_Kind is
         when Package_Desc_Info     =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                    "_description, " &
                                    Info.Subtitle_Package.all &
                                    "_withs, " &
                                    Info.Subtitle_Package.all &
                                    ", " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                    "_description");
         when With_Info             =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                     "_withs, " &
                                    Info.Subtitle_Package.all &
                                    "_packages, " &
                                    Info.Subtitle_Package.all &
                                    "_description, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                     "_withs");
         when Package_Info          =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_packages, " &
                                    Info.Subtitle_Package.all &
                                  "_vars, " &
                                    Info.Subtitle_Package.all &
                                  "_withs, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_packages");
         when Var_Info              =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_vars, " &
                                    Info.Subtitle_Package.all &
                                  "_exceptions, " &
                                    Info.Subtitle_Package.all &
                                  "_packages, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_vars");
         when Exception_Info        =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_exceptions, " &
                                    Info.Subtitle_Package.all &
                                  "_types, " &
                                    Info.Subtitle_Package.all &
                                  "_vars, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_exceptions");
         when Type_Info             =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_types, " &
                                    Info.Subtitle_Package.all &
                                  "_subprograms, " &
                                  Info.Subtitle_Package.all &
                                    "_exceptions, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_types");
         when Subprogram_Info       =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_subprograms,   , " &
                                    Info.Subtitle_Package.all &
                                      "_types,   , " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_subprograms");
         when others => null;
      end case;

      Ada.Text_IO.Put_Line (File, "@c ----------------------" &
                            "--------------------------");
      Ada.Text_IO.New_Line (File);

   end Doc_TEXI_Subtitle;

   ------------------------
   -- Doc_TEXI_Pack_Desc --
   ------------------------

   procedure Doc_TEXI_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, Info.Package_Desc_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Pack_Desc;

   ----------------------
   -- Doc_TEXI_Package --
   ----------------------

   procedure Doc_TEXI_Package
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      --  Format_TEXI_Entity_Header (File,
      --                             Info.Package_List,
      --                             false,
      --                             Info.Package_Entity.Header_Lines,
      --                             Info.Package_Entity.Header.all,
      --                             Info.Package_Entity.File_Name.all,
      --                             Info.Package_Entity.Line,
      --                             Info.Package_Entity.Is_Private);

      --  Ada.Text_IO.Put_Line (File, Info.Package_Entity.Header.all);
      Ada.Text_IO.Put_Line (File, "---" & Info.Package_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Package;

   -------------------
   -- Doc_TEXI_With --
   -------------------

   procedure Doc_TEXI_With
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      --  Format_TEXI_Entity_Header (File, Info.With_List, false,
      --                           Count_Lines (Info.With_Lines.all),
      --                           Info.With_Lines.all,
      --                           Info.With_File.all,
      --                           1,
      --                           false);


      Ada.Text_IO.Put_Line (File, Info.With_Header.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_With;

   ------------------
   -- Doc_TEXI_Var --
   ------------------

   procedure Doc_TEXI_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  Format_TEXI_Entity_Header (File, Info.Var_List, false,
      --                           Info.Var_Entity.Header_Lines,
      --                           Info.Var_Entity.Header.all,
      --                           Info.Var_Entity.File_Name.all,
      --                           Info.Var_Entity.Line,
      --                           Info.Var_Entity.Is_Private);

      --  Ada.Text_IO.Put_Line (File, Info.Var_Entity.Header.all);
      Ada.Text_IO.Put_Line (File, "---" & Info.Var_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Var;

   ------------------------
   -- Doc_TEXI_Exception --
   ------------------------

   procedure Doc_TEXI_Exception
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  Format_TEXI_Entity_Header (File, Info.Exception_List, false,
      --                           Info.Exception_Entity.Header_Lines,
      --                           Info.Exception_Entity.Header.all,
      --                           Info.Exception_Entity.File_Name.all,
      --                           Info.Exception_Entity.Line,
      --                           Info.Exception_Entity.Is_Private);

      --  Ada.Text_IO.Put_Line (File, Info.Exception_Entity.Header.all);
      Ada.Text_IO.Put_Line (File, "---" & Info.Exception_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Exception;

   -------------------
   -- Doc_TEXI_Type --
   -------------------

   procedure Doc_TEXI_Type
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      --  Format_TEXI_Entity_Header (File, Info.Type_List, false,
      --                           Info.Type_Entity.Header_Lines,
      --                           Info.Type_Entity.Header.all,
      --                           Info.Type_Entity.File_Name.all,
      --                           Info.Type_Entity.Line,
      --                           Info.Type_Entity.Is_Private);

      --  Ada.Text_IO.Put_Line (File, Info.Type_Entity.Header.all);
      Ada.Text_IO.Put_Line (File, "---" & Info.Type_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Type;

   -------------------------
   -- Doc_TEXI_Subprogram --
   -------------------------

   procedure Doc_TEXI_Subprogram
     (File      : in Ada.Text_IO.File_Type;
      Info      : Doc_Info) is

      package TRL renames Type_Reference_List;
   begin

      --  Format_TEXI_Entity_Header
      --  (File, Info.Subprogram_List, Info.Subprogram_Link,
      --                           Info.Subprogram_Entity.Header_Lines,
      --                           Info.Subprogram_Entity.Header.all,
      --                           Info.Subprogram_Entity.File_Name.all,
      --                           Info.Subprogram_Entity.Line,
      --                           Info.Subprogram_Entity.Is_Private);

      --  Ada.Text_IO.Put_Line (File, Info.Subprogram_Entity.Header.all);
      Ada.Text_IO.Put_Line (File, "---" & Info.Subprogram_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Subprogram;

   ------------------
   --  Format_TEXI --
   ------------------

   function Format_TEXI
     (Entity_List       : Type_Entity_List.List;
      Text              : String;
      File_Name         : String;
      Entity_Name       : String;
      Is_Body           : Boolean) return GNAT.OS_Lib.String_Access is

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

         Put_Line (Entity_Name);

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

         function Is_Variable return Boolean;
         --  checks if the position of the entity in the
         --  string is followed by a ":" => then variable

         function Is_Variable return Boolean is
         begin
            return False;
         end Is_Variable;

      begin  -- HTML_Spec_Callback

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
                  --  ONLY possible: subprograms, exceptions, variables,
                  --  types and packages
                    and TEL.Data (Entity_Node).Kind /= Other_Entity
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
                           Spec_File : GNAT.OS_Lib.String_Access;
                           Body_File : GNAT.OS_Lib.String_Access;
                           Node      : GNAT.OS_Lib.String_Access;
                        begin
                           Put_Line ("in: *" &
                                     TEL.Data (Entity_Node).Name.all);

                           Node := TEL.Data (Entity_Node).File_Name;
                           Spec_File := new String'
                             (GNAT.Directory_Operations.File_Name
                                (Node.all));
                           Body_File := new String'(Spec_File
                             (1 .. Get_String_Index (Spec_File.all,
                                                     1,
                                                     ".") - 1) &
                           "_adb.htm");
                           Result_Line := new String'
                             (Result_Line.all
                                (Result_Line'First .. Sloc_Start.Index +
                                   Already_Added_Chars - 1) &
                              "<A href=""" &
                              Body_File.all & "#" &
                              Number (2 .. Number'Last) & """>" &
                              Result_Line.all
                                (Sloc_Start.Index + Already_Added_Chars ..
                                   Sloc_End.Index + Already_Added_Chars) &
                              "</A>" &
                              Result_Line.all
                                (Sloc_End.Index + Already_Added_Chars + 1 ..
                                   Result_Line'Last));
                           Already_Added_Chars := Already_Added_Chars + 16 +
                             Body_File'Length + Number'Length - 1;
                           Free (Old_Line);
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
                             := Get_TEXI_File_Name
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

      Put_Line (File_Name);

      Already_Added_Chars := 0;
      Result_Line := new String'(Text);

      --  parse the entities in this line
      Parse_Entities (Ada_Lang,
                      Text,
                      HTML_Callback'Unrestricted_Access);
      return Result_Line;
   end Format_TEXI;

   ---------------------
   -- Doc_TEXI_Header --
   ---------------------

   procedure Doc_TEXI_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "@c -------------------- CHAPTER" &
                            " -------------------");
      Ada.Text_IO.Put_Line (File, "@node " &
                                Info.Header_Package.all & ", " &
                                Info.Header_Package_Next.all & ", " &
                                Info.Header_Package_Prev.all &
                                 ", Top");
      Ada.Text_IO.Put_Line (File, "@chapter " &
                                Info.Header_Package.all);
      Ada.Text_IO.Put_Line (File, "@c ---------------------" &
                            "---------------------------");
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Header;

   ---------------------
   -- Doc_TEXI_Footer --
   ---------------------

   procedure Doc_TEXI_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      if False then      --  just to avoid the warning
         Ada.Text_IO.Put_Line (File, Info.Footer_Title.all);
      end if;
   end Doc_TEXI_Footer;

   ------------------
   -- Chars_Before --
   ------------------

   --  returns the sum of the number of chars in the lines
   --  until Line_Nr-1 in the Line string
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

   -------------------
   -- Doc_TEXI_Body --
   -------------------

   procedure Doc_TEXI_Body
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is

      New_Text : GNAT.OS_Lib.String_Access;
   begin
      New_Text :=  Format_TEXI (Info.Body_Entity_List,
                                Info.Body_Text.all,
                                Info.Body_File.all,
                                "",
                                True);

      --  write the changed body to the html file
      Ada.Text_IO.Put_Line (File, New_Text.all);
      Free (New_Text);
   end Doc_TEXI_Body;

   ------------------------
   -- Get_TEXI_File_Name --
   ------------------------

   function Get_TEXI_File_Name
     (File : String) return String
   is
      TEXI_File : ASU.Unbounded_String;
   begin
      TEXI_File := ASU.To_Unbounded_String (File_Name (File));
      if File_Extension (File) = ".ads" then
         return ASU.To_String
           ((ASU.Replace_Slice (TEXI_File,
                               ASU.Index (TEXI_File, "."),
                               ASU.Index (TEXI_File, ".") + 3, "_ads.texi")));
      else
         return ASU.To_String
           ((ASU.Replace_Slice (TEXI_File,
                               ASU.Index (TEXI_File, "."),
                               ASU.Index (TEXI_File, ".") + 3, "_adb.texi")));
      end if;
   end Get_TEXI_File_Name;

end Texi_Output;
