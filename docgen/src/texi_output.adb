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
with Language;                  use Language;

package body Texi_Output is

   package TEL renames Type_Entity_List;
   package ASU renames Ada.Strings.Unbounded;

   --  All 9 are used for the callback function in Doc_TEXI_Body_Line.
   --  They must be global, because it is not possible to
   --  give further parameters to the callback function
   Old_Line, New_Line   : Unbounded_String;
   --  used to save the lines processed by the callback function
   Already_Added_Chars  : Integer;
   --  how many chars have been already added in this line
   New_Added_Chars      : Integer;
   --  how many chars are being added for this entity
   Entity_List          : TEL.List;
   --  to garantee access to the entity list
   Chars_Nr_Before      : Integer;
   --  how many chars in all the lines before in this header
   Source_File          : Unbounded_String;
   --  name of the file
   Process_Body         : Boolean;
   --  will also the body file be processed
   Line_Number          : Natural;
   --  the line of the spec being processed

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

      Ada.Text_IO.Put_Line (File, Info.Package_Entity.Header.all);
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


      Ada.Text_IO.Put_Line (File, Info.With_Lines.all);
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

      Ada.Text_IO.Put_Line (File, Info.Var_Entity.Header.all);
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

      Ada.Text_IO.Put_Line (File, Info.Exception_Entity.Header.all);
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

      Ada.Text_IO.Put_Line (File, Info.Type_Entity.Header.all);
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

      Ada.Text_IO.Put_Line (File, Info.Subprogram_Entity.Header.all);
      Ada.Text_IO.Put_Line (File, "---" & Info.Subprogram_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Subprogram;

   -------------------------------
   -- Format_TEXI_Entity_Header --
   -------------------------------

   procedure Format_TEXI_Entity_Header
     (File              : in Ada.Text_IO.File_Type;
      New_Entity_List   : Type_Entity_List.List;
      Here_Process_Body : Boolean;
      Nr_Lines          : Natural;
      Header            : String;
      File_Name         : String;
      Def_Line          : Natural;
      Is_Private        : Boolean) is

      Chars_In_Old_Lines : Natural;
      Chars_In_New_Lines : Natural;
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
         New_Line            := ASU.To_Unbounded_String
           (Header (Chars_In_Old_Lines + (J - 1) ..
                      Chars_In_New_Lines + (J - 2)));
         Old_Line            := New_Line;
         Already_Added_Chars := 0;
         Chars_Nr_Before     := 0;
         --  how many chars in the lines before the current line
         Entity_List         := New_Entity_List;
         Process_Body        := Here_Process_Body;
         Line_Number         := Def_Line;
         Source_File         := ASU.To_Unbounded_String (File_Name);

         --  parse the entities in this line
         Parse_Entities (Ada_Lang,
                            Header (Chars_In_Old_Lines .. Chars_In_New_Lines),
                            TEXI_Spec_Callback'Access);

         Ada.Text_IO.Put_Line (File, To_String (New_Line));
      end loop;
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
   end Format_TEXI_Entity_Header;

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
      if false then      --  just to avoid the warning
         Ada.Text_IO.Put_Line (File, Info.Footer_Title.all);
      end if;
   end Doc_TEXI_Footer;

   -----------------------
   -- Replace_TEXI_Tags --
   -----------------------

   function Replace_TEXI_Tags
     (Input_Line : String) return String is
      --  replaces all "<"  which are NOT in a commentin by "&lt;"
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
   end Replace_TEXI_Tags;

   ------------------------
   -- TEXI_Body_Callback --
   ------------------------

   function TEXI_Body_Callback
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean is
      --  here for each entity found in the line, the New_Line
      --  string will be formatted
      Entity_Node    : TEL.List_Node;

      function Is_Declaration (Line      : String;
                               Start_Col : Integer;
                               End_Col   : Integer) return Boolean;
      --  checks if the last Entity found in this line is declared here
      --  => check if the entity name is followed by a ":"


      function Is_Declaration (Line      : String;
                               Start_Col : Integer;
                               End_Col   : Integer) return Boolean is
         Col  : Integer;
      begin
         Col := End_Col + 1;

         if Partial_Entity and False then      --  just to avoid the warning
            Put_Line (Start_Col'Img);
         end if;

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

   begin  -- TEXI_Body_Callback


      --  DON'T FORGET: when you change the TEXI code to added,
      --  change also the Already_Added_Chars!!!

      New_Added_Chars := 0;

      case Entity is
         when Comment_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<FONT color=""green"">"
                 & To_String (Old_Line) (Sloc_Start.Column .. Sloc_End.Column)
               & "</FONT>");
            Already_Added_Chars := Already_Added_Chars + 27;
            --  27 is the number of chars in "<FONT c.."
         when Keyword_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<B>"
                 & To_String (Old_Line) (Sloc_Start.Column .. Sloc_End.Column)
               & "</B>");
            Already_Added_Chars := Already_Added_Chars + 7;
            --  7 is the number of chars in "<B></B>"
         when String_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<FONT color=""red"">"
               --  To_String(Old_Line)(Sloc_Start.Column..Sloc_End.Column)
                 & Replace_TEXI_Tags
                   (To_String
                        (Old_Line)(Sloc_Start.Column .. Sloc_End.Column))
               & "</FONT>");
            Already_Added_Chars := Already_Added_Chars +
              25 + New_Added_Chars;
         when Character_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<FONT color=""#666666"">"
                 & To_String (Old_Line) (Sloc_Start.Column .. Sloc_End.Column)
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
                 = To_Lower (To_String
                               (Old_Line) (Sloc_Start.Column ..
                                             Sloc_End.Column))
               then
                  --  and if this line is the declaration of the entity.
                  --  if so => ignore!
                  if not Is_Declaration (ASU.To_String (Old_Line),
                                         Sloc_Start.Column,
                                         Sloc_End.Column)
                    and
                  --  ignore also all variables NOT defined in this source file
                    (TEL.Data (Entity_Node).Kind /= Other_Entity or
                       TEL.Data (Entity_Node).File_Name.all =
                       To_String (Source_File))
                  then
                     --  create make the link
                     declare
                        Number    : constant String
                          := TEL.Data (Entity_Node).Line'Img;
                        TEXI_File : constant String
                          := Get_TEXI_File_Name
                          (TEL.Data (Entity_Node).File_Name.all);
                     begin
                        New_Line := ASU.Replace_Slice
                          (New_Line, Sloc_Start.Column + Already_Added_Chars,
                           Sloc_End.Column   + Already_Added_Chars,
                           "<A href="""
                             & TEXI_File
                           & "#"
                             & Number (2 .. Number'Last)
                           & """>"
                             & To_String (Old_Line) (Sloc_Start.Column
                                                     .. Sloc_End.Column)
                           & "</A>");
                        Already_Added_Chars := Already_Added_Chars +
                          16 + TEXI_File'Length + Number'Length - 1;
                     end;
                     return False;
                  end if;
               end if;
               Entity_Node := TEL.Next (Entity_Node);
            end loop;

         when others => New_Line :=
              ASU.To_Unbounded_String (Replace_TEXI_Tags
                                         (To_String (Old_Line)
                                            (Sloc_Start.Column
                                               .. Sloc_End.Column)));
            Already_Added_Chars := Already_Added_Chars
              + New_Added_Chars;
      end case;
      return False;  --  later: use false or not?
   end TEXI_Body_Callback;

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

   ------------------------
   -- TEXI_Spec_Callback --
   ------------------------

   function TEXI_Spec_Callback
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean is
      --  here for each entity found in the line, the New_Line
      --  string will be formatted
      Entity_Node       : TEL.List_Node;

      function Is_Variable return Boolean;
      --  checks if the position of the entity in the string is
      --  followed by a ":" => then variable

      function Is_Variable return Boolean is
         --  checks if the position of the entity in the string is
         --  followed by a ":" => then variable
      begin
         if Sloc_Start.Column < Get_String_Index
           (To_String (Old_Line), 1, ":") then
            return True;
         else
            return False;
         end if;
      end Is_Variable;

   begin  --  TEXI_Spec_Callback

      --  DON'T FORGET: when you change the TEXI code to added,
      --  change also the Already_Added_Chars!!!

      if Partial_Entity and False then      --  just to avoid the warning
         null;
      end if;

      New_Added_Chars := 0;

      case Entity is
         when Comment_Text => null;
         when Keyword_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<B>"
                 & To_String (Old_Line) (Sloc_Start.Column .. Sloc_End.Column)
               & "</B>");
            Already_Added_Chars := Already_Added_Chars + 7;
            --  7 is the number of chars in "<B></B>"
         when String_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<FONT color=""red"">"
               --  To_String(Old_Line)(Sloc_Start.Column..Sloc_End.Column)
                 & Replace_TEXI_Tags
                   (To_String (Old_Line)
                      (Sloc_Start.Column .. Sloc_End.Column))
               & "</FONT>");
            Already_Added_Chars := Already_Added_Chars + 25 + New_Added_Chars;
         when Character_Text => New_Line := ASU.Replace_Slice
              (New_Line, Sloc_Start.Column + Already_Added_Chars,
               Sloc_End.Column   + Already_Added_Chars,
               "<FONT color=""#666666"">"
                 & To_String (Old_Line) (Sloc_Start.Column .. Sloc_End.Column)
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
               --  ONLY possible: subprograms, exceptions, types and packages
                 and TEL.Data (Entity_Node).Kind /= Other_Entity
               --  and this line is not the declararion of the
               --  entity (allowed for subprograms)
                 and (not (TEL.Data (Entity_Node).Line = Line_Number and
                             TEL.Data (Entity_Node).File_Name.all =
                             ASU.To_String (Source_File)) or
                        TEL.Data (Entity_Node).Kind = Procedure_Entity or
                        TEL.Data (Entity_Node).Kind = Function_Entity)
               then
                  --  if entity a subprogram and a link should and can be set
                  --  => creat link to body
                  if Process_Body and TEL.Data (Entity_Node).File_Found and
                    (TEL.Data (Entity_Node).Kind = Procedure_Entity or
                       TEL.Data (Entity_Node).Kind = Function_Entity) then
                     declare
                        Number    : constant String :=
                                    TEL.Data (Entity_Node).Line_In_Body'Img;
                        Spec_File, Body_File : ASU.Unbounded_String;
                     begin
                        Spec_File := ASU.To_Unbounded_String
                          (File_Name (TEL.Data (Entity_Node).File_Name.all));
                        Body_File :=
                          ASU.Replace_Slice (Spec_File,
                                             ASU.Index (Spec_File, "."),
                                             ASU.Index (Spec_File, ".") + 3,
                                             "_adb.htm");

                        New_Line := ASU.Replace_Slice
                          (New_Line, Sloc_Start.Column + Already_Added_Chars,
                           Sloc_End.Column   + Already_Added_Chars,
                           "<A href="""
                             & ASU.To_String (Body_File)
                           & "#"
                             & Number (2 .. Number'Last)
                           & """>"
                             & To_String (Old_Line)(Sloc_Start.Column ..
                                                      Sloc_End.Column)
                           & "</A>");
                        Already_Added_Chars := Already_Added_Chars + 16 +
                          ASU.Length (Body_File) +
                          Number'Length - 1;
                     end;
                  elsif not Is_Variable then
                     --  for the rest: create the link for the entity
                     declare
                        Number    : constant String
                          := TEL.Data (Entity_Node).Line'Img;
                        TEXI_File : constant String :=
                          Get_TEXI_File_Name (TEL.Data
                                                (Entity_Node).File_Name.all);
                     begin
                        New_Line := ASU.Replace_Slice
                          (New_Line, Sloc_Start.Column + Already_Added_Chars,
                           Sloc_End.Column   + Already_Added_Chars,
                           "<A href="""
                             & TEXI_File
                           & "#"
                             & Number (2 .. Number'Last)
                           & """>"
                             & To_String (Old_Line)
                             (Sloc_Start.Column .. Sloc_End.Column)
                           & "</A>");
                        Already_Added_Chars := Already_Added_Chars + 16
                          + TEXI_File'Length + Number'Length - 1;
                     end;
                  end if;
                  return False;
               end if;
               Entity_Node := TEL.Next (Entity_Node);
            end loop;

         when others => New_Line :=
              ASU.To_Unbounded_String (Replace_TEXI_Tags
                                         (To_String (Old_Line)
                                            (Sloc_Start.Column
                                               .. Sloc_End.Column)));
            Already_Added_Chars := Already_Added_Chars + New_Added_Chars;
      end case;
      return False;  --  later: use false or not?
   end TEXI_Spec_Callback;

   ------------------------
   -- Doc_TEXI_Body_Line --
   ------------------------

   procedure Doc_TEXI_Body_Line
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin

      --  the global variables for the callback function of the parser
      New_Line            := ASU.To_Unbounded_String (Info.Body_Text.all);
      Old_Line            := New_Line;
      Already_Added_Chars := 0;
      Chars_Nr_Before     := 0;
      --  how many chars in the lines before the current line
      Source_File         := ASU.To_Unbounded_String (Info.Body_Text.all);
      Entity_List         := Info.Body_List;

      --  parse the entities in this line
      Parse_Entities (Ada_Lang, Info.Body_Text.all, TEXI_Body_Callback'Access);

      --  each line will get a mark with its number as name
      --  Add_Mark_For_Entity :
      --  declare
      --   Number : constant String := Info.Body_Line_Nr'Img;
      --  begin
      --   New_Line := "<A name=""" & Number (2 .. Number'Last) &
      --   """> " & New_Line & " </A>";
      --  end Add_Mark_For_Entity;

      --  write the line to the htm-file
      Ada.Text_IO.Put_Line (File, To_String (New_Line));
   end Doc_TEXI_Body_Line;

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
