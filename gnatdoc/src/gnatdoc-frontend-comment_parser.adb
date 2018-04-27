------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2018, AdaCore                   --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Traces;         use GNATCOLL.Traces;

with GNATdoc.Comment;         use GNATdoc.Comment;
with GNATdoc.Errout;          use GNATdoc.Errout;
with GNATdoc.Utils;           use GNATdoc.Utils;
with Xref.Docgen;             use Xref.Docgen;

package body GNATdoc.Frontend.Comment_Parser is

   Me : constant Trace_Handle := Create ("GNATdoc.1-Frontend-Comment_Parser");

   XML_Regpat : constant Pattern_Matcher :=
     Compile (" *<([/]?) *([^ </>]+) *([^<>]*)>", Single_Line);

   -------------
   -- Parsers --
   -------------

   package Parsers is
      Private_Entities_List : aliased EInfo_List.Vector;

      procedure Parse_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : String);
      --  Parse the contents of Text and store its contents in the structured
      --  comment of E (ie. E.Comment)

      procedure Parse_Extract_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : in out Unbounded_String);
      --  Parse the contents of Text extracting the documentation associated
      --  with formals and generic formals of E, and storing it store on the
      --  corresponding formals. On Text returns all the documentation that
      --  could not be attached to any formal. This subprogram must be invoked
      --  by the caller before invoking Parse_Doc, and the Text returned by
      --  this subprogram must be the input text processed by Parse_Doc (since
      --  it is the actual documentation of E).

      procedure Parse_XML_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : String);
      --  Parse the contents of S and store its contents in the structured
      --  comment of E (ie. E.Comment)
   end Parsers;
   use Parsers;

   -------------
   -- Parsers --
   -------------

   package body Parsers is
      Context       : access constant Docgen_Context;
      S             : String_Access;
      Tag_Indicator : constant Character := '@';

      type Location is record
         First : Natural;
         Last  : Natural;
      end record;

      No_Location : constant Location := (0, 0);

      procedure Error
        (Entity  : Entity_Id;
         Msg     : String);
      --  Report the error message Msg on the location of Entity and store it
      --  on the entity.

      procedure Finalize_Parser;
      --  Clear the memory allocated by the parser

      procedure Initialize_Parser
        (Context : access constant Docgen_Context;
         Text    : String);
      --  Initialize the parser context and the text to be parsed

      function Is_Custom_Tag (Tag : String) return Boolean;
      --  Return True if Tag is a supported tag.
      --  ??? This info should be configurable in a separate file to allow
      --  customers to define their own tags

      function No (Loc : Location) return Boolean;
      --  True if Loc = No_Location

      function Present (Loc : Location) return Boolean;
      --  True if Loc /= No_Location

      function Scan_Tag (Index : in out Natural) return Location;
      --  Scan text in S searching for the next tag located after Index

      procedure Scan_Word
        (J   : in out Natural;
         Loc : out Location);
      --  Scan next word in S

      procedure Scan_Line
        (J   : in out Natural;
         Loc : out Location);
      --  Scan S searching for the next end of line

      -----------
      -- Error --
      -----------

      procedure Error
        (Entity : Entity_Id;
         Msg    : String) is
      begin
         Error (Context, LL.Get_Entity (Entity), Msg);

         if No (Get_Error_Msg (Entity)) then
            Set_Error_Msg (Entity, To_Unbounded_String (Msg));
         end if;
      end Error;

      ---------------------
      -- Finalize_Parser --
      ---------------------

      procedure Finalize_Parser is
      begin
         Free (S);
      end Finalize_Parser;

      -----------------------
      -- Initialize_Parser --
      -----------------------

      procedure Initialize_Parser
        (Context : access constant Docgen_Context;
         Text    : String) is
      begin
         Parsers.Context := Context;
         S := new String'(Text);
      end Initialize_Parser;

      -------------------
      -- Is_Custom_Tag --
      -------------------
      --  http://docs.oracle.com/javase/1.4.2/docs/tooldocs/windows/
      --    javadoc.html#javadoctags
      --  file:///home/miranda/gps520/share/doc/gps/html/users_guide/
      --    tools.html#documentation-generation

      function Is_Custom_Tag (Tag : String) return Boolean is
      begin
         return Tag = "description"
            --  Full description of a package or method
           or else Tag = "summary"
            --  Short description of a package or method
           or else Tag = "param"
            --  Description of a parameter
           or else Tag = "exception"
            --  Description of possible exceptions raised by the
            --  method/subprogram
           or else Tag = "seealso"
            --  Reference to another package, method, type, etc.
           or else Tag = "c_version"
            --  For bindings, the version of the C file
           or else Tag = "group"
            --  For grouping of packages in index
            --  or else Tag = "code"  --  ???

            --  JavaDoc possible additional tags???
            --  or else Tag = "author"
            --  or else Tag = "deprecated"
           or else Tag = "return"
            --  or else Tag = "serial"
            --  or else Tag = "since"
            --  or else Tag = "version"

            --  GNATdoc enhancements
           or else Tag = "field"
           or else Tag = "value"
           or else Tag = "private";
      end Is_Custom_Tag;

      --------
      -- No --
      --------

      function No (Loc : Location) return Boolean is
      begin
         return Loc = No_Location;
      end No;

      ---------------
      -- Parse_Doc --
      ---------------

      procedure Parse_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : String)
      is
         Comment : constant Structured_Comment := Get_Comment (E);
         Current : Tag_Cursor := New_Cursor (Comment);

         procedure Parse (S : String);
         --  Parse the contents of S searching for the next tag

         procedure Parse (S : String) is

            procedure Check_Tag (Tag_Name : String);
            --  Check if this tag is applicable to entity E

            ---------------
            -- Check_Tag --
            ---------------

            procedure Check_Tag (Tag_Name : String) is

               function KindToText return String;
               --  Convert the kind of E into a lower-case text replacing
               --  underscores by spaces (for example, E_Record_Type is
               --  returned as "record type").

               procedure Report_Error;
               --  Report the error

               ----------------
               -- KindToText --
               ----------------

               function KindToText return String is
                  Low_Ekind_Img : constant String :=
                                    To_Lower (Get_Kind (E)'Img);
                  Ekind_Img     : constant String :=
                                    Low_Ekind_Img (Low_Ekind_Img'First + 2
                                                     .. Low_Ekind_Img'Last);
                  Result : String (Ekind_Img'Range);
               begin
                  for J in Ekind_Img'Range loop
                     if Ekind_Img (J) = '_' then
                        Result (J) := ' ';
                     else
                        Result (J) := Ekind_Img (J);
                     end if;
                  end loop;

                  return Result;
               end KindToText;

               ------------------
               -- Report_Error --
               ------------------

               procedure Report_Error is
               begin
                  Error (E,
                    "@" & Tag_Name & " not applicable to " & KindToText);
               end Report_Error;

               --  Report the error

            begin
               if Tag_Name = "description"
                 or else Tag_Name = "summary"
               then
                  if not Is_Package (E)
                    and then not Is_Subprogram_Or_Entry (E)
                  then
                     Report_Error;
                  end if;

               elsif Tag_Name = "exception"
                 or else Tag_Name = "param"
               then
                  if not Is_Subprogram_Or_Entry (E) then
                     Report_Error;
                  end if;

               elsif Tag_Name = "field" then
                  if not Is_Record_Type (E) then
                     Report_Error;
                  end if;

               elsif Tag_Name = "return" then
                  if Get_Kind (E) /= E_Function then
                     Report_Error;
                  end if;
               end if;
            end Check_Tag;

            --  Local variables

            Index   : Natural := S'First;
            Tag_Loc : constant Location := Scan_Tag (Index);
         begin
            --  Regular string. Let's append it to the current node value.

            if No (Tag_Loc) then
               Append_Text (Current, S);
               return;
            end if;

            --  Append characters to the last opened tag.

            if Tag_Loc.First > S'First then
               Append_Text
                 (Current, Filter (S (S'First .. Tag_Loc.First - 2)));
            end if;

            declare
               Field_Tag   : constant String := "field";
               Param_Tag   : constant String := "param";
               Private_Tag : constant String := "private";
               Value_Tag   : constant String := "value";

               Tag_Text  : constant String :=
                             To_Lower (S (Tag_Loc.First + 1 .. Tag_Loc.Last));
               Attr_Loc  : Location;
               Text_Loc  : Location;
               Line_Last : Natural;
               J         : Natural;
            begin
               --  If we found an unexpected tag, then treat it like raw text

               if not Is_Custom_Tag (Tag_Text) then
                  Trace (Me, "--> Unknown tag: >" & Tag_Text & "<");

                  Append_Text (Current, S (Tag_Loc.First .. Tag_Loc.Last));
                  Line_Last := Tag_Loc.Last + 1;

               else
                  J := Tag_Loc.Last + 1;

                  Scan_Word
                    (J   => J,
                     Loc => Attr_Loc);

                  Scan_Line
                    (J   => J,
                     Loc => Text_Loc);

                  Line_Last := J;
                  pragma Assert (J > S'Last or else S (J) = ASCII.LF);

                  Check_Tag (Tag_Text);

                  if Tag_Text = Private_Tag then
                     Private_Entities_List.Append (E);
                  end if;

                  if Tag_Text = Field_Tag
                    or else Tag_Text = Param_Tag
                    or else Tag_Text = Value_Tag
                  then
                     if No (Attr_Loc) then
                        if Tag_Text = Field_Tag then
                           Error (E, "missing field name");
                        elsif Tag_Text = Param_Tag then
                           Error (E, "missing parameter name");
                        else
                           Error (E, "missing value name");
                        end if;

                     else
                        declare
                           Attr_Name : String renames
                             S (Attr_Loc.First .. Attr_Loc.Last);
                           Cursor    : Tag_Cursor;
                        begin
                           Cursor :=
                             Search_Param (Comment, Attr_Name);

                           if Cursor = No_Cursor then
                              if Tag_Text = Field_Tag then
                                 Error (E,
                                   "wrong field name '"
                                   & Attr_Name & "'");
                              elsif Tag_Text = Param_Tag then
                                 Error (E,
                                   "wrong parameter name '"
                                   & Attr_Name & "'");
                              else
                                 Error (E,
                                   "wrong value name '"
                                   & Attr_Name & "'");
                              end if;

                           elsif Present (Get (Cursor).Text) then
                              Current := Cursor;

                              declare
                                 Entity : constant Root_Entity'Class :=
                                   Get (Cursor).Entity.Element;
                              begin
                                 for Ent of Get_Entities (E).all loop
                                    if LL.Get_Entity (Ent) = Entity then
                                       if Tag_Text = Field_Tag then
                                          Error
                                            (Ent,
                                             "field '"
                                               & Attr_Name
                                               & "' documented twice");
                                       elsif Tag_Text = Param_Tag then
                                          Error
                                            (Ent,
                                             "parameter '"
                                               & Attr_Name
                                               & "' documented twice");
                                       else
                                          Error
                                            (Ent,
                                             "value '"
                                               & Attr_Name
                                               & "' documented twice");
                                       end if;

                                       exit;
                                    end if;
                                 end loop;
                              end;

                           elsif Present (Text_Loc) then
                              Current := Cursor;

                              declare
                                 Text     : String renames
                                   S (Text_Loc.First .. Text_Loc.Last);
                                 Next_Loc : Location;
                                 Offset   : Natural := 0;

                              begin
                                 --  Lookup for indentation of next line to use
                                 --  it on current line to be in synch with
                                 --  comment parser of HTML backend.

                                 J := Text_Loc.Last + 2;
                                 Scan_Line (J, Next_Loc);

                                 if Present (Next_Loc) then
                                    for K in
                                      Next_Loc.First .. Next_Loc.Last
                                    loop
                                       if S (K) /= ' ' then
                                          Offset := K - Next_Loc.First;

                                          exit;
                                       end if;
                                    end loop;

                                    Append_Text (Current, Offset * ' ');
                                 end if;

                                 Append_Text (Current, Text);

                                 if Present (Next_Loc) then
                                    Append_Text (Current, (1 => ASCII.LF));
                                 end if;
                              end;

                           else
                              Current := Cursor;
                           end if;
                        end;
                     end if;

                  --  Opening tag

                  else

                     --  Now initialize the attributes field
                     if Present (Attr_Loc) then
                        declare
                           Text : String renames
                             S (Attr_Loc.First .. Attr_Loc.Last);
                        begin
                           if Tag_Text = "seealso" then
                              Current :=
                                Append_Tag
                                  (Comment,
                                   Tag       => To_Unbounded_String (Tag_Text),
                                   Entity    => No_Root_Entity,
                                   Attribute => To_Unbounded_String (Text));
                           else
                              Current :=
                                Append_Tag
                                  (Comment,
                                   Tag       => To_Unbounded_String (Tag_Text),
                                   Entity    => No_Root_Entity,
                                   Attribute => Null_Unbounded_String);
                              Append_Text (Current, Text);
                           end if;
                        end;
                     else
                        Current :=
                          Append_Tag
                            (Comment,
                             Tag       => To_Unbounded_String (Tag_Text),
                             Entity    => No_Root_Entity,
                             Attribute => Null_Unbounded_String);
                     end if;

                     if Present (Text_Loc) then
                        declare
                           Backend_Name : constant String :=
                             To_String (Context.Options.Backend_Name);
                           In_CM_Backend : constant Boolean :=
                             Backend_Name = "cm";
                           Text : constant String :=
                             S (Text_Loc.First .. Text_Loc.Last);
                        begin
                           if In_CM_Backend then
                              Append_Text (Current, Text & ASCII.LF);
                           else
                              Append_Text (Current, Text);
                           end if;
                        end;
                     end if;
                  end if;
               end if;

               --  Tail recursivity should be converted into a loop???

               if Line_Last < S'Last then
                  Parse (S (Line_Last + 1 .. S'Last));
               end if;
            end;
         end Parse;

      --  Start of processing for Parse_Doc

      begin
         if Text = "" then
            return;
         end if;

         Initialize_Parser (Context, Text);
         Parse (S.all);
         Finalize_Parser;
         Set_Comment (E, Comment);
      end Parse_Doc;

      -----------------------
      -- Parse_Extract_Doc --
      -----------------------

      procedure Parse_Extract_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : in out Unbounded_String)
      is
         S              : constant String := To_String (Text);
         Current_E      : Entity_Id := No_Entity;
         Current_E_Text : Unbounded_String;
         E_Text         : Unbounded_String;

         procedure Append_Text (Text : String);
         --  Append Text to Current_E_Text if we are accumulating documentation
         --  for Current_E; otherwise accumulate it in E_Text (for entity E).

         procedure Parse (S : String);
         --  Parse the contents of S searching for tags containing information
         --  associated with a parameter of E or a generic formal of E.

         -----------------
         -- Append_Text --
         -----------------

         procedure Append_Text (Text : String) is
         begin
            if Present (Current_E) then
               Current_E_Text := Current_E_Text & Filter (Text);

            --  Accumulate this text without filtering (since filtering of this
            --  text will be performed at a later stage by Parse_Doc).

            else
               E_Text := E_Text & Text;
            end if;
         end Append_Text;

         -----------
         -- Parse --
         -----------

         procedure Parse (S : String) is

            procedure Process_Tag
              (Tag_Loc : Location;
               Index   : in out Natural);
            --  Process the given tag: for tags with documentation of generic
            --  formals (or subprogram formals) attach their documentation to
            --  the corresponding formal; for any other tag accumulate their
            --  documentation to attach it to E for processing at a latter
            --  stage. Update Index to reference the next input character.

            procedure Set_Current_Entity_Doc;
            --  Attach to Current_E its extracted documentation.

            ----------------------------
            -- Set_Current_Entity_Doc --
            ----------------------------

            procedure Set_Current_Entity_Doc is
               Doc : Unbounded_String_Vectors.Vector;
            begin
               Doc.Append (Current_E_Text);
               Set_Doc (Current_E,
                 Comment_Result'(Text => Doc,
                                 Start_Line => -1));
            end Set_Current_Entity_Doc;

            -----------------
            -- Process_Tag --
            -----------------

            procedure Process_Tag
              (Tag_Loc : Location;
               Index   : in out Natural)
            is
               Tag_Name : constant String :=
                            To_Lower (S (Tag_Loc.First + 1 .. Tag_Loc.Last));

               procedure Search_Generic_Formal (Name : String);
               --  Search for the entity associated with the generic formal
               --  Name

               procedure Search_Parameter (Name : String);
               --  Search for the entity associated with parameter Name

               ---------------------------
               -- Search_Generic_Formal --
               ---------------------------

               procedure Search_Generic_Formal (Name : String) is
                  Found : Boolean := False;

               begin
                  for Param of Get_Generic_Formals (E).all loop
                     if Get_Short_Name (Param) = Name then
                        if Present (Get_Doc (Param)) then
                           Error (Param,
                             "generic formal '" & Name & "' documented twice");
                        end if;

                        Current_E := Param;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Error (E, "generic formal '" & Name & "' not found");
                  end if;
               end Search_Generic_Formal;

               ----------------------
               -- Search_Parameter --
               ----------------------

               procedure Search_Parameter (Name : String) is
                  Found : Boolean := False;

               begin
                  for Param of Get_Entities (E).all loop
                     if Get_Short_Name (Param) = Name then
                        if Present (Get_Doc (Param)) then
                           Error (Param,
                             "parameter '" & Name & "' documented twice");
                        end if;

                        Current_E := Param;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Error (E, "parameter '" & Name & "' not found");
                  end if;
               end Search_Parameter;

               --  Local variables

               New_Tag   : constant String := "gen_param";
               Param_Tag : constant String := "param";
               Attr_Loc  : Location;
               Text_Loc  : Location;
               Line_Last : Natural;
               J         : Natural;

            --  Start of processing for Process_Tag

            begin
               --  Handle unexpected tags like raw text (that is, attaching
               --  them and their documentation to E to process it at a latter
               --  stage).

               if Tag_Name /= Param_Tag
                 and then Tag_Name /= New_Tag
               then
                  Append_Text
                    (ASCII.LF & " "
                      & S (Tag_Loc.First .. Tag_Loc.Last) & " ");
                  Line_Last := Tag_Loc.Last;

               else
                  J := Tag_Loc.Last + 1;

                  Scan_Word
                    (J   => J,
                     Loc => Attr_Loc);

                  Scan_Line
                    (J   => J,
                     Loc => Text_Loc);

                  Line_Last := J;
                  pragma Assert (J > S'Last or else S (J) = ASCII.LF);

                  if No (Attr_Loc) then
                     Error (E, "missing parameter name");

                  elsif Tag_Name = Param_Tag then
                     Search_Parameter
                       (S (Attr_Loc.First .. Attr_Loc.Last));

                  elsif Tag_Name = New_Tag then
                     Search_Generic_Formal
                       (S (Attr_Loc.First .. Attr_Loc.Last));
                  end if;

                  if Present (Text_Loc) then
                     Append_Text (S (Text_Loc.First .. Text_Loc.Last));
                  end if;
               end if;

               Index := Line_Last + 1;
            end Process_Tag;

            --  Local variables

            Index   : Natural := S'First;
            Tag_Loc : Location;

         --  Start of processing for Parse

         begin
            while Index < S'Last loop
               declare
                  Index_Before_Scanning : constant Natural := Index;

               begin
                  --  Search for the next tag in the input text

                  Tag_Loc := Scan_Tag (Index);

                  --  No tag found: append all the pending text to the current
                  --  entity and stop processing.

                  if No (Tag_Loc) then
                     Append_Text (S (Index_Before_Scanning .. S'Last));

                     if Present (Current_E) then
                        Set_Current_Entity_Doc;
                     end if;

                     return;
                  end if;

                  --  Append the text located before the next tag to the
                  --  current entity.

                  if Tag_Loc.First > Index_Before_Scanning then
                     Append_Text
                       (S (Index_Before_Scanning .. Tag_Loc.First - 1));

                     if Present (Current_E) then
                        Set_Current_Entity_Doc;
                     end if;
                  end if;
               end;

               --  Reset the entity associated with the current formal (or
               --  generic formal)

               Current_E := No_Entity;
               Set_Unbounded_String (Current_E_Text, "");

               --  Process the documentation of this tag

               Process_Tag (Tag_Loc, Index);
            end loop;
         end Parse;

      --  Start of processing for Parse_Extract_Doc

      begin
         if S = "" then
            return;
         end if;

         Initialize_Parser (Context, S);
         Parse (S);
         Finalize_Parser;

         --  Return that text that could not be attached to any formal or
         --  generic formal of E

         Text := E_Text;
      end Parse_Extract_Doc;

      -------------------
      -- Parse_XML_Doc --
      -------------------

      procedure Parse_XML_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         Text         : String)
      is
         pragma Unreferenced (Context);

         Comment : constant Structured_Comment := Get_Comment (E);
         Current : Tag_Cursor := New_Cursor (Comment);
         S       : String renames Text;

         procedure Parse (S : String);
         --  Parse the contents of S searching for the next tag

         procedure Parse (S : String) is
            Matches  : Match_Array (0 .. 3);
            Tag_Text : Unbounded_String;

         begin
            Match (XML_Regpat, S, Matches);

            --  Regular string. Let's append it to the current node value.

            if Matches (0) = No_Match then
               Append_Text (Current, S);
               return;
            end if;

            --  Append characters to the last opened tag.
            if Matches (0).First > S'First then
               Append_Text (Current, S (S'First .. Matches (0).First - 1));
            end if;

            declare
               Full_Text : String renames
                 S (Matches (0).First .. Matches (0).Last);
               Prefix    : String renames
                 S (Matches (1).First .. Matches (1).Last);
               Tag       : String renames
                 S (Matches (2).First .. Matches (2).Last);

               Attribute  : String renames
                 S (Matches (3).First .. Matches (3).Last);
               Attr_First : constant Natural := Matches (3).First;
               Attr_Last  : Natural := Matches (3).Last;

               Closing_Tag  : constant Boolean := Prefix = "/";
               Is_Param_Tag : Boolean := False;
            begin
               if To_Lower (Tag) = "parameter" then
                  Tag_Text     := To_Unbounded_String ("param");
                  Is_Param_Tag := True;
               else
                  Tag_Text := To_Unbounded_String (To_Lower (Tag));
               end if;

               if Tag_Text = "private" then
                  Private_Entities_List.Append (E);
               end if;

               --  Treat closing tags; missing check???

               if Closing_Tag then
                  null;

               --  If we found an unexpected tag, then treat it like raw text

               elsif not Is_Custom_Tag (To_String (Tag_Text)) then
                  Append_Text (Current, Full_Text);

               --  Opening parameter tag

               elsif Is_Param_Tag then

                  --  Example of contents in attribute: `name="parameter name"`

                  declare
                     function Get_Name (S : String) return String;

                     function Get_Name (S : String) return String is
                        First : Natural := S'First;
                        Last  : Natural := S'Last;
                     begin
                        while First < Last
                          and then S (First) /= '"'
                        loop
                           First := First + 1;
                        end loop;

                        while First < Last
                          and then S (First) = '"'
                        loop
                           First := First + 1;
                        end loop;

                        while Last > First
                          and then S (Last) /= '"'
                        loop
                           Last := Last - 1;
                        end loop;

                        while Last > First
                          and then S (Last) = '"'
                        loop
                           Last := Last - 1;
                        end loop;

                        return S (First .. Last);
                     end Get_Name;

                     Param_Name : constant String := Get_Name (Attribute);
                     Cursor     : constant Tag_Cursor :=
                                    Search_Param (Comment, Param_Name);

                  begin
                     if Cursor = No_Cursor then
                        Error (E,
                          "wrong parameter name '"
                          & Param_Name & "'");
                     else
                        Current := Cursor;
                     end if;
                  end;

               --  Opening other tag

               else
                  declare
                     Attribute : Unbounded_String;

                  begin
                     --  See if the tag finishes by '/>'
                     --  Stand_Alone := False;

                     if Matches (3).First >= Matches (3).Last
                       and then S (Matches (3).Last) = '/'
                     then
                        --  Stand_Alone := True;
                        Attr_Last := Attr_Last - 1;
                     end if;

                     --  Now initialize the attributes field
                     if Attr_First <= Attr_Last then
                        Attribute :=
                          To_Unbounded_String (S (Attr_First .. Attr_Last));
                     end if;

                     Current :=
                       Append_Tag
                         (Comment,
                          Tag       => Tag_Text,
                          Entity    => No_Root_Entity,
                          Attribute => Attribute);
                  end;
               end if;

               if Matches (0).Last < S'Last then
                  Parse (S (Matches (0).Last + 1 .. S'Last));
               end if;
            end;

         end Parse;

      --  Start of processing for Parse_XML_Doc

      begin
         if S = "" then
            return;
         end if;

         Parse (S);

         --  Check unclosed XML tags: not implemented yet???

         Set_Comment (E, Comment);
      end Parse_XML_Doc;

      -------------
      -- Present --
      -------------

      function Present (Loc : Location) return Boolean is
      begin
         return Loc /= No_Location;
      end Present;

      --------------
      -- Scan_Tag --
      --------------

      function Scan_Tag (Index : in out Natural) return Location is
         J     : Natural renames Index;
         First : Natural;
         Last  : Natural;
      begin
         while J <= S'Last
           and then S (J) /= Tag_Indicator
         loop
            J := J + 1;
         end loop;

         if J <= S'Last then
            First := J;

            J := J + 1; --  past '@'
            while J <= S'Last
              and then S (J) /= ' '
              and then S (J) /= ASCII.LF
            loop
               J := J + 1;
            end loop;
            Last := J - 1;

            if Last > First then
               return Location'(First, Last);
            end if;
         end if;

         return No_Location;
      end Scan_Tag;

      ---------------
      -- Scan_Word --
      ---------------

      procedure Scan_Word
        (J   : in out Natural;
         Loc : out Location)
      is
         First : Natural;
         Last  : Natural;
      begin
         Loc := No_Location;

         while J <= S'Last
           and then S (J) = ' '
           and then S (J) /= ASCII.LF
         loop
            J := J + 1;
         end loop;

         First := J;
         while J <= S'Last
           and then S (J) /= ' '
           and then S (J) /= ASCII.LF
         loop
            J := J + 1;
         end loop;
         Last := J - 1;

         if Last >= First then
            Loc := Location'(First, Last);
         end if;
      end Scan_Word;

      ---------------
      -- Scan_Line --
      ---------------

      procedure Scan_Line
        (J   : in out Natural;
         Loc : out Location)
      is
         First : constant Natural := J;
         Last  : Natural;
      begin
         while J <= S'Last
           and then S (J) /= ASCII.LF
         loop
            J := J + 1;
         end loop;

         Last := J - 1;
         if Last > First then
            Loc := Location'(First, Last);
         else
            Loc := No_Location;
         end if;
      end Scan_Line;
   end Parsers;

   -------------------------------
   -- Build_Structured_Comments --
   -------------------------------

   procedure Build_Structured_Comments
     (Context : access constant Docgen_Context;
      Root    : Entity_Id)
   is
      procedure Parse_Enumeration_Comments (Enum : Entity_Id);
      --  Initialize the structured comment associated with Enum, parse the
      --  block of comments retrieved from sources (and clean it), and report
      --  errors/warnings on missing documentation.

      procedure Parse_Record_Comments (Rec : Entity_Id);
      --  Initialize the structured comment associated with Rec, parse the
      --  block of comments retrieved from sources (and clean it), and report
      --  errors/warnings on missing documentation.

      procedure Parse_Subprogram_Comments (Subp : Entity_Id);
      --  Initialize the structured comment associated with Subp, parse the
      --  block of comments retrieved from sources (and clean it), and report
      --  errors/warnings on missing documentation.

      procedure Parse_Doc_Wrapper
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String);
      --  Perform a fast analysis of S and invoke Parse_Doc or Parse_XML_Doc

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Dispatch a call to build an structured comment between routines
      --  Parse_Doc, Parse_Enumeration_Comments, Parse_Record_Comments,
      --  and Parse_Subprogram_Comments.

      procedure Remove_Private_Entities;
      --  Private entities are those annotated with the private tag. This
      --  routine takes care of removing them from the tree.

      -----------------------
      -- Parse_Doc_Wrapper --
      -----------------------

      procedure Parse_Doc_Wrapper
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         S       : String)
      is
         Matches : Match_Array (0 .. 3);
         Str     : Unbounded_String := To_Unbounded_String (S);

      begin
         if Index (S, "@") > 0 then
            if Context.Options.Extensions_Enabled then
               Parse_Extract_Doc (Context, E, Str);
            end if;

            Parse_Doc (Context, E, To_String (Str));
            return;
         end if;

         Match (XML_Regpat, S, Matches);

         if Matches (0) /= No_Match then
            Parse_XML_Doc (Context, E, S);
         else
            Parse_Doc (Context, E, S);
         end if;
      end Parse_Doc_Wrapper;

      --------------------------------
      -- Parse_Enumeration_Comments --
      --------------------------------

      procedure Parse_Enumeration_Comments (Enum : Entity_Id) is
         Has_Values : constant Boolean := Present (Get_Entities (Enum));
      begin
         --  No action needed if the enumeration has no values. This case
         --  occurs in enumeration subtypes and generic formals.

         if not Has_Values then
            return;
         end if;

         --  Initialize the structured comment associated with this entity

         Set_Comment (Enum, New_Structured_Comment);

         --  Search for documentation located in middle of the declaration

         for Value of Get_Entities (Enum).all loop
            pragma Assert (Get_Kind (Value) = E_Enumeration_Literal);
            Append_Value_Tag
              (Comment    => Get_Comment (Enum),
               Entity     => LL.Get_Entity (Value),
               Value_Name => To_Unbounded_String (Get_Short_Name (Value)),
               Text       => Get_Doc (Value).Text);
         end loop;

         --  Parse the documentation

         if Get_Doc (Enum) /= No_Comment_Result then
            Parse_Doc_Wrapper (Context, Enum, To_String (Get_Doc (Enum).Text));
            Set_Doc (Enum, No_Comment_Result);
         end if;

         --  Report warning on undocumented values

         declare
            Cursor   : Tag_Cursor := First_Value (Get_Comment (Enum));
            Tag_Info : Tag_Info_Ptr;
         begin
            loop
               Tag_Info := Get (Cursor);

               if No (Tag_Info.Text) then
                  Warning
                    (Context,
                     Tag_Info.Entity.Element,
                     "undocumented value ("
                     & To_String (Tag_Info.Attr)
                     & ")");
               end if;

               exit when Cursor = Last_Value (Get_Comment (Enum));
               Next (Cursor);
            end loop;
         end;
      end Parse_Enumeration_Comments;

      ---------------------------
      -- Parse_Record_Comments --
      ---------------------------

      procedure Parse_Record_Comments (Rec : Entity_Id) is
         Has_Components : constant Boolean := Present (Get_Entities (Rec));
      begin
         --  Initialize the structured comment associated with this entity

         Set_Comment (Rec, New_Structured_Comment);

         --  Search for documentation located in the subprogram profile
         --  (that is, comments located close to the parameter declarations)

         for Comp of Get_Entities (Rec).all loop
            --  Disabling temporarily this assertion since Xref has problems in
            --  service Xref.Fields() with records defined in generic units???

            --  pragma Assert (Get_Kind (Comp) = E_Discriminant
            --    or else Get_Kind (Comp) = E_Component);

            Append_Field_Tag
              (Comment    => Get_Comment (Rec),
               Entity     => LL.Get_Entity (Comp),
               Field_Name => To_Unbounded_String (Get_Short_Name (Comp)),
               Text       => Get_Doc (Comp).Text);
         end loop;

         --  Parse the documentation of the record

         if Get_Doc (Rec) /= No_Comment_Result then
            Parse_Doc_Wrapper (Context, Rec, To_String (Get_Doc (Rec).Text));
            Set_Doc (Rec, No_Comment_Result);
         end if;

         --  Report warning on undocumented parameters

         if Has_Components then
            declare
               C        : Tag_Cursor := First_Field (Get_Comment (Rec));
               Tag_Info : Tag_Info_Ptr;
            begin
               loop
                  Tag_Info := Get (C);

                  if No (Tag_Info.Text) then
                     Warning
                       (Context,
                        Tag_Info.Entity.Element,
                        "undocumented field ("
                        & To_String (Tag_Info.Attr)
                        & ")");
                  end if;

                  exit when C = Last_Field (Get_Comment (Rec));
                  Next (C);
               end loop;
            end;
         end if;
      end Parse_Record_Comments;

      -------------------------------
      -- Parse_Subprogram_Comments --
      -------------------------------

      procedure Parse_Subprogram_Comments (Subp : Entity_Id) is
         Has_Params : constant Boolean := Present (Get_Entities (Subp));
      begin
         --  Initialize the structured comment associated with this entity

         Set_Comment (Subp, New_Structured_Comment);

         --  For backward compatibility it is still missing to append the
         --  tags of the generic formals???

         --  Search for documentation located in the subprogram profile
         --  (that is, comments located close to the parameter declarations)

         if True then
            for Param of Get_Entities (Subp).all loop
               Append_Param_Tag
                 (Comment    => Get_Comment (Subp),
                  Entity     => LL.Get_Entity (Param),
                  Param_Name => To_Unbounded_String (Get_Short_Name (Param)),
                  Text       => Get_Doc (Param).Text);
            end loop;
         end if;

         --  Parse the documentation of the subprogram

         if Get_Doc (Subp) /= No_Comment_Result then
            Parse_Doc_Wrapper (Context, Subp, To_String (Get_Doc (Subp).Text));
            Set_Doc (Subp, No_Comment_Result);
         end if;

         --  Report warning on undocumented parameters

         if Has_Params then
            declare
               C        : Tag_Cursor := First_Param (Get_Comment (Subp));
               Tag_Info : Tag_Info_Ptr;
            begin
               loop
                  Tag_Info := Get (C);

                  if No (Tag_Info.Text) then
                     Warning
                       (Context,
                        Tag_Info.Entity.Element,
                        "undocumented parameter ("
                        & To_String (Tag_Info.Attr)
                        & ")");
                  end if;

                  exit when C = Last_Param (Get_Comment (Subp));
                  Next (C);
               end loop;
            end;
         end if;
      end Parse_Subprogram_Comments;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result is
         pragma Unreferenced (Scope_Level);
      begin
         --  Do not document again C/C++ entities which are already documented

         if Get_Comment (Entity) /= No_Structured_Comment then
            return Skip;
         end if;

         if In_Private_Part (Entity)
           and then not Context.Options.Show_Private
         then
            return Skip;
         end if;

         if Is_Subprogram_Or_Entry (Entity) then
            Parse_Subprogram_Comments (Entity);
            return Skip;

         elsif Is_Record_Type (Entity) then
            Parse_Record_Comments (Entity);
            return Skip;

         elsif Get_Kind (Entity) = E_Enumeration_Type
           and then not Is_Subtype (Entity)
         then
            Parse_Enumeration_Comments (Entity);
            return Skip;

         elsif Present (Get_Doc (Entity).Text) then
            Set_Comment (Entity, New_Structured_Comment);
            Parse_Doc_Wrapper
              (Context, Entity, To_String (Get_Doc (Entity).Text));
            Set_Doc (Entity, No_Comment_Result);
         end if;

         return OK;
      end Process_Node;

      -----------------------------
      -- Remove_Private_Entities --
      -----------------------------

      procedure Remove_Private_Entities is
      begin
         for E of Private_Entities_List loop

            --  Tagged type primitives must be removed from the list of
            --  primitives of its corresponding tagged type

            if LL.Is_Primitive (E) then

               --  Search for the tagged type traversing the formals

               for Formal of Get_Entities (E).all loop
                  declare
                     Typ : constant Entity_Id := Get_Etype (Formal);
                  begin
                     if Present (Typ)
                       and then Is_Tagged (Typ)
                       and then Get_Scope (Typ) = Get_Scope (E)
                       and then Get_Methods (Typ).Contains (E)
                     then
                        Remove_From_List (Get_Methods (Typ), E);
                     end if;
                  end;
               end loop;

               --  Check functions dispatching on their result type

               if Get_Kind (E) = E_Function
                 and then Present (Get_Etype (E))
                 and then Is_Tagged (Get_Etype (E))
                 and then Get_Scope (Get_Etype (E)) = Get_Scope (E)
                 and then Get_Methods (Get_Etype (E)).Contains (E)
               then
                  Remove_From_List (Get_Methods (Get_Etype (E)), E);
               end if;
            end if;

            Remove_From_Scope (E);
            Set_Is_Excluded (E);
         end loop;

         Private_Entities_List.Clear;
      end Remove_Private_Entities;

      use type Ada.Containers.Count_Type;
   begin
      Private_Entities_List.Clear;
      Traverse_Tree (Root, Process_Node'Access);

      if Private_Entities_List.Length > 0 then
         Remove_Private_Entities;
      end if;
   end Build_Structured_Comments;

   -------------------
   -- May_Have_Tags --
   -------------------

   function May_Have_Tags
     (Text : Unbounded_String_Vectors.Vector) return Boolean is
   begin
      for Line of Text loop
         if Index (Line, "@") > 0 then
            return True;

         else
            declare
               S       : constant String := To_String (Line);
               Matches : Match_Array (0 .. 3);

            begin
               Match (XML_Regpat, S, Matches);

               if Matches (0) /= No_Match then
                  return True;
               end if;
            end;
         end if;
      end loop;

      return False;
   end May_Have_Tags;

end GNATdoc.Frontend.Comment_Parser;
