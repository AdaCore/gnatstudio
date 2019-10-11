------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Traces;         use GNATCOLL.Traces;

with GNATdoc.Comment;         use GNATdoc.Comment;
with GNATdoc.Errout;          use GNATdoc.Errout;
with GNATdoc.Text_Buffers;
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
         Text    : Unbounded_String_Vectors.Vector);
      --  Parse the contents of Text and store its contents in the structured
      --  comment of E (ie. E.Comment)

      procedure Parse_Extract_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : in out Unbounded_String_Vectors.Vector);
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
      Tag_Indicator : constant Character := '@';

      type Location is record
         First : Natural;
         Last  : Natural;
      end record;

      No_Location : constant Location := (0, 0);

      procedure Check_Tag
         (E        : Entity_Id;
          Tag_Name : String);
      --  Check if this tag is applicable to entity E

      procedure Error
        (Entity  : Entity_Id;
         Msg     : String);
      --  Report the error message Msg on the location of Entity and store it
      --  on the entity.

      procedure Initialize_Parser
        (Context : access constant Docgen_Context);
      --  Initialize the parser context

      function Is_Custom_Tag (Tag : String) return Boolean;
      --  Return True if Tag is a supported tag.
      --  ??? This info should be configurable in a separate file to allow
      --  customers to define their own tags

      function No (Loc : Location) return Boolean;
      --  True if Loc = No_Location

      function Present (Loc : Location) return Boolean;
      --  True if Loc /= No_Location

      function Scan_Tag
        (S : Unbounded_String; Index : in out Natural) return Location;
      --  Scan text in S searching for the next tag located after Index

      procedure Scan_Word
        (S   : Unbounded_String;
         J   : in out Natural;
         Loc : out Location);
      --  Scan next word in S

      ---------------
      -- Check_Tag --
      ---------------

      procedure Check_Tag (E : Entity_Id; Tag_Name : String) is

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
            Low_Ekind_Img : constant String := To_Lower (Get_Kind (E)'Img);
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
            Error (E, "@" & Tag_Name & " not applicable to " & KindToText);
         end Report_Error;

      --  Start of processing for Check_Tag

      begin
         if Tag_Name = "description"
           or else Tag_Name = "summary"
         then
            if not Is_Package (E)
              and then not Is_Subprogram_Or_Entry (E)
              and then not Is_Access_To_Subprogram_Type (E)
            then
               Report_Error;
            end if;

         elsif Tag_Name = "exception"
           or else Tag_Name = "param"
         then
            if not Is_Subprogram_Or_Entry (E)
              and then not Is_Access_To_Subprogram_Type (E)
            then
               Report_Error;
            end if;

         elsif Tag_Name = "field" then
            if not Is_Record_Type (E) then
               Report_Error;
            end if;

         elsif Tag_Name = "return" then
            if not Kind_In (Get_Kind (E), E_Function,
                                          E_Generic_Function,
                                          E_Access_Function_Type)
            then
               Report_Error;
            end if;
         end if;
      end Check_Tag;

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

      -----------------------
      -- Initialize_Parser --
      -----------------------

      procedure Initialize_Parser
        (Context : access constant Docgen_Context) is
      begin
         Parsers.Context := Context;
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
           or else Tag = "private"
           or else Tag = "format"
           or else Tag = "noformat";
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
         Text    : Unbounded_String_Vectors.Vector)
      is
         Comment : constant Structured_Comment := Get_Comment (E);
         Current : Tag_Cursor := New_Cursor (Comment);

         procedure Parse_Line
           (Line : Unbounded_String;
            Next : Unbounded_String);
         --  Parse the contents of the line. Next line is used to compute
         --  offset for the text after the tag.

         ----------------
         -- Parse_Line --
         ----------------

         procedure Parse_Line
           (Line : Unbounded_String;
            Next : Unbounded_String)
         is
            Offset  : Natural := 0;
            First   : Positive := 1;
            Index   : Natural;
            Tag_Loc : Location;

         begin
            loop
               Index := First;
               Tag_Loc := Scan_Tag (Line, Index);

               --  Regular string. Let's append it to the current node value.

               if No (Tag_Loc) then
                  Append_Text_String (Current, Offset * ' ');
                  Append_Text_Line
                    (Current,
                     Trim
                       (Unbounded_Slice (Line, First, Length (Line)),
                        (if Offset = 0
                         then Ada.Strings.Right else Ada.Strings.Both)));
                  Offset := 0;

                  return;
               end if;

               declare
                  Field_Tag   : constant String := "field";
                  Param_Tag   : constant String := "param";
                  Private_Tag : constant String := "private";
                  Value_Tag   : constant String := "value";

                  Tag_Text    : constant String :=
                    To_Lower
                      (Slice (Line, Tag_Loc.First + 1, Tag_Loc.Last));
                  Attr_Loc    : Location;
                  J           : Natural;

               begin
                  --  If we found an unexpected tag, then treat it like raw
                  --  text

                  if not Is_Custom_Tag (Tag_Text) then
                     Trace (Me, "--> Unknown tag: >" & Tag_Text & "<");

                     Append_Text_String
                       (Current, Unbounded_Slice (Line, First, Tag_Loc.Last));
                     First := Tag_Loc.Last + 1;

                  else
                        --  Append characters to the last opened tag.

                     if Tag_Loc.First > First then
                        declare
                           Aux : constant Unbounded_String :=
                             Trim
                               (Unbounded_Slice
                                  (Line, First, Tag_Loc.First - 2),
                                Ada.Strings.Right);

                        begin
                           if Aux /= Null_Unbounded_String then
                              Append_Text_String (Current, Aux);
                           end if;
                        end;
                     end if;

                     J := Tag_Loc.Last + 1;

                     Scan_Word
                       (Line,
                        J   => J,
                        Loc => Attr_Loc);

                     Check_Tag (E, Tag_Text);

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

                           First := Tag_Loc.Last + 1;

                        else
                           declare
                              Attr_Name : String renames
                                Slice (Line, Attr_Loc.First, Attr_Loc.Last);
                              Cursor    : Tag_Cursor;

                           begin
                              Cursor := Search_Param (Comment, Attr_Name);

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

                                 First := Attr_Loc.Last + 1;

                              elsif not Get (Cursor).Text.Is_Empty then
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

                                 First := Attr_Loc.Last + 1;

                              else
                                 Current := Cursor;

                                 First := Attr_Loc.Last + 1;

                                 --  Lookup for indentation of next line to
                                 --  use it on current line to be in synch
                                 --  with omment parser of the HTML backend.

                                 if Next /= Null_Unbounded_String then
                                    for K in 1 .. Length (Next) loop
                                       if Element (Next, K) /= ' ' then
                                          Offset := K - 1;

                                          exit;
                                       end if;
                                    end loop;
                                 end if;
                              end if;
                           end;
                        end if;

                     --  Opening tag

                     else

                        --  Now initialize the attributes field
                        if Present (Attr_Loc) then
                           declare
                              Text : Unbounded_String renames
                                Unbounded_Slice
                                  (Line, Attr_Loc.First, Attr_Loc.Last);

                           begin
                              if Tag_Text = "seealso" then
                                 Current :=
                                   Append_Tag
                                     (Comment,
                                      Tag       =>
                                        To_Unbounded_String (Tag_Text),
                                      Entity    => No_Root_Entity,
                                      Attribute => Text);

                              else
                                 Current :=
                                   Append_Tag
                                     (Comment,
                                      Tag       =>
                                        To_Unbounded_String (Tag_Text),
                                      Entity    => No_Root_Entity,
                                      Attribute => Null_Unbounded_String);
                                 Append_Text_String (Current, Text);
                              end if;
                           end;

                           First := Attr_Loc.Last + 1;

                        else
                           Current :=
                             Append_Tag
                               (Comment,
                                Tag       => To_Unbounded_String (Tag_Text),
                                Entity    => No_Root_Entity,
                                Attribute => Null_Unbounded_String);

                           First := Tag_Loc.Last + 1;
                        end if;
                     end if;
                  end if;
               end;
            end loop;
         end Parse_Line;

      --  Start of processing for Parse_Doc

      begin
         if Text.Is_Empty then
            return;
         end if;

         Initialize_Parser (Context);

         for J in Text.First_Index .. Text.Last_Index loop
            Parse_Line
              (Text.Element (J),
               (if J /= Text.Last_Index
                then Text.Element (J + 1) else Null_Unbounded_String));
         end loop;

         Set_Comment (E, Comment);
      end Parse_Doc;

      -----------------------
      -- Parse_Extract_Doc --
      -----------------------

      procedure Parse_Extract_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         Text    : in out Unbounded_String_Vectors.Vector)
      is
         Current_E      : Entity_Id := No_Entity;
         Current_E_Text : GNATdoc.Text_Buffers.Text_Buffer;
         E_Text         : GNATdoc.Text_Buffers.Text_Buffer;

         procedure Append_Text_Line (Item : Unbounded_String);
         procedure Append_Text_String (Item : Unbounded_String);
         --  Append Item to Current_E_Text if we are accumulating documentation
         --  for Current_E; otherwise accumulate it in E_Text (for entity E).

         procedure Set_Current_Entity_Doc;
         --  Attach to Current_E its extracted documentation.

         procedure Parse_Line (S : Unbounded_String);
         --  Parse the contents of S searching for tags containing information
         --  associated with a parameter of E or a generic formal of E.

         ----------------------
         -- Append_Text_Line --
         ----------------------

         procedure Append_Text_Line (Item : Unbounded_String) is
         begin
            if Present (Current_E) then
               Current_E_Text.Append_Line (Item);

            --  Accumulate this text without filtering (since filtering of this
            --  text will be performed at a later stage by Parse_Doc).

            else
               E_Text.Append_Line (Item);
            end if;
         end Append_Text_Line;

         ------------------------
         -- Append_Text_String --
         ------------------------

         procedure Append_Text_String (Item : Unbounded_String) is
         begin
            if Present (Current_E) then
               Current_E_Text.Append (Item);

            --  Accumulate this text without filtering (since filtering of this
            --  text will be performed at a later stage by Parse_Doc).

            else
               E_Text.Append (Item);
            end if;
         end Append_Text_String;

         ----------------
         -- Parse_Line --
         ----------------

         procedure Parse_Line (S : Unbounded_String) is

            procedure Process_Tag
              (First   : Positive;
               Tag_Loc : Location;
               Index   : in out Natural);
            --  Process the given tag: for tags with documentation of generic
            --  formals (or subprogram formals) attach their documentation to
            --  the corresponding formal; for any other tag accumulate their
            --  documentation to attach it to E for processing at a latter
            --  stage. Update Index to reference the next input character.

            -----------------
            -- Process_Tag --
            -----------------

            procedure Process_Tag
              (First   : Positive;
               Tag_Loc : Location;
               Index   : in out Natural)
            is
               Tag_Name : constant String :=
                 To_Lower (Slice (S, Tag_Loc.First + 1, Tag_Loc.Last));

               procedure Extract_Operator_Name (Loc : in out Location);
               --  Modify Loc to point to the name of the Ada operator.

               procedure Search_Generic_Formal (Name : Unbounded_String);
               --  Search for the entity associated with the generic formal
               --  Name

               procedure Search_Parameter (Name : Unbounded_String);
               --  Search for the entity associated with parameter Name

               ---------------------------
               -- Extract_Operator_Name --
               ---------------------------

               procedure Extract_Operator_Name (Loc : in out Location) is
               begin
                  if Present (Loc) then
                     --  Ada's operators starts from quotation mark, drop them.

                     if Element (S, Loc.First) = '"' then
                        Loc.First := Loc.First + 1;
                        Loc.Last  := Loc.Last - 1;
                     end if;
                  end if;
               end Extract_Operator_Name;

               ---------------------------
               -- Search_Generic_Formal --
               ---------------------------

               procedure Search_Generic_Formal (Name : Unbounded_String) is
                  Found : Boolean := False;

               begin
                  Set_Current_Entity_Doc;

                  for Param of Get_Generic_Formals (E).all loop
                     if Get_Short_Name (Param) = Name then
                        if Present (Get_Doc (Param)) then
                           Error
                             (Param,
                              "generic formal '"
                              & To_String (Name)
                              & "' documented twice");
                        end if;

                        Current_E := Param;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Error
                       (E,
                        "wrong generic formal name '"
                        & To_String (Name)
                        & "'");
                  end if;
               end Search_Generic_Formal;

               ----------------------
               -- Search_Parameter --
               ----------------------

               procedure Search_Parameter (Name : Unbounded_String) is
                  Found : Boolean := False;

               begin
                  Set_Current_Entity_Doc;

                  for Param of Get_Entities (E).all loop
                     if Get_Short_Name (Param) = Name then
                        if Present (Get_Doc (Param)) then
                           Error
                             (Param,
                              "parameter '"
                              & To_String (Name)
                              & "' documented twice");
                        end if;

                        Current_E := Param;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Error
                       (E, "wrong parameter name '" & To_String (Name) & "'");
                  end if;
               end Search_Parameter;

               --  Local variables

               New_Tag    : constant String := "gen_param";
               Param_Tag  : constant String := "param";
               Return_Tag : constant String := "return";
               Attr_Loc   : Location;
               Line_Last  : Natural;
               J          : Natural;

            --  Start of processing for Process_Tag

            begin
               --  Handle unexpected tags like raw text (that is, attaching
               --  them and their documentation to E to process it at a latter
               --  stage).

               if Tag_Name /= Param_Tag
                 and then Tag_Name /= Return_Tag
                 and then Tag_Name /= New_Tag
               then
                  Set_Current_Entity_Doc;

                  Append_Text_String
                    (Unbounded_Slice (S, First, Tag_Loc.Last));
                  Line_Last := Tag_Loc.Last;

               --  Handle Tags without entity name (for now only @return)

               elsif Tag_Name = Return_Tag then
                  Line_Last := Tag_Loc.Last + 1;

                  --  Attach this documentation to the internal entity
                  --  added by the frontend.

                  if Kind_In (Get_Kind (E), E_Access_Function_Type,
                                            E_Function,
                                            E_Generic_Function)
                  then
                     Set_Current_Entity_Doc;

                     Current_E := Get_Internal_Return (E);
                     pragma Assert (Present (Current_E));

                     if Present (Get_Doc (Current_E)) then
                        Error (Current_E, "return documented twice");
                     end if;

                  --  Report the error (wrong use of @return)

                  else
                     Check_Tag (E, Tag_Name);
                  end if;

               --  Handle Tags with entity name

               else
                  J := Tag_Loc.Last + 1;

                  Scan_Word
                    (S,
                     J   => J,
                     Loc => Attr_Loc);

                  Line_Last := J;

                  if No (Attr_Loc)
                    and then (Tag_Name = Param_Tag
                                or else Tag_Name = New_Tag)
                  then
                     Error (E, "missing parameter name");

                  elsif Tag_Name = Param_Tag then
                     Search_Parameter
                       (Unbounded_Slice (S, Attr_Loc.First, Attr_Loc.Last));

                  elsif Tag_Name = New_Tag then
                     if Is_Generic (E) then
                        Extract_Operator_Name (Attr_Loc);
                        Search_Generic_Formal
                          (Unbounded_Slice (S, Attr_Loc.First, Attr_Loc.Last));
                     else
                        Error (E, "wrong use of @" & New_Tag);
                     end if;
                  end if;
               end if;

               Index := Line_Last + 1;
            end Process_Tag;

            --  Local variables

            Index   : Natural := 1;
            Tag_Loc : Location;

         --  Start of processing for Parse

         begin
            while Index < Length (S) loop
               declare
                  Index_Before_Scanning : constant Natural := Index;

               begin
                  --  Search for the next tag in the input text

                  Tag_Loc := Scan_Tag (S, Index);

                  --  No tag found: append all the pending text to the current
                  --  entity and stop processing.

                  if No (Tag_Loc) then
                     Append_Text_Line
                       (Unbounded_Slice
                          (S, Index_Before_Scanning, Length (S)));

                     return;
                  end if;

                  --  Process the documentation of this tag

                  Process_Tag (Index_Before_Scanning, Tag_Loc, Index);
               end;
            end loop;
         end Parse_Line;

         ----------------------------
         -- Set_Current_Entity_Doc --
         ----------------------------

         procedure Set_Current_Entity_Doc is
         begin
            if Present (Current_E) then
               Set_Doc
                 (Current_E,
                  Comment_Result'(Text       => Current_E_Text.Text,
                                  Start_Line => -1));
               Current_E := No_Entity;
               Current_E_Text.Clear;
            end if;
         end Set_Current_Entity_Doc;

      --  Start of processing for Parse_Extract_Doc

      begin
         if Text.Is_Empty then
            return;
         end if;

         Initialize_Parser (Context);

         for Line of Text loop
            Parse_Line (Line);
         end loop;

         --  Set documentation for the current entity if any.

         if Present (Current_E) then
            Set_Current_Entity_Doc;
         end if;

         --  Return that text that could not be attached to any formal or
         --  generic formal of E

         Text := E_Text.Text;
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
               Append_Text (Current, Split_Lines (S));

               return;
            end if;

            --  Append characters to the last opened tag.
            if Matches (0).First > S'First then
               Append_Text
                 (Current,
                  Split_Lines (S (S'First .. Matches (0).First - 1)));
            end if;

            declare
               Full_Text : String renames
                 S (Matches (0).First .. Matches (0).Last);
               Prefix     : String renames
                 S (Matches (1).First .. Matches (1).Last);
               Tag        : String renames
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
                  Append_Text_String
                    (Current, To_Unbounded_String (Full_Text));

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

      function Scan_Tag
        (S : Unbounded_String; Index : in out Natural) return Location
      is
         J     : Natural renames Index;
         First : Natural;
         Last  : Natural;
      begin
         while J <= Length (S)
           and then Element (S, J) /= Tag_Indicator
         loop
            J := J + 1;
         end loop;

         if J <= Length (S) then
            First := J;

            J := J + 1; --  past '@'
            while J <= Length (S)
              and then Element (S, J) /= ' '
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
        (S   : Unbounded_String;
         J   : in out Natural;
         Loc : out Location)
      is
         First : Natural;
         Last  : Natural;
      begin
         Loc := No_Location;

         while J <= Length (S)
           and then Element (S, J) = ' '
         loop
            J := J + 1;
         end loop;

         First := J;
         while J <= Length (S)
           and then Element (S, J) /= ' '
         loop
            J := J + 1;
         end loop;
         Last := J - 1;

         if Last >= First then
            Loc := Location'(First, Last);
         end if;
      end Scan_Word;

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
         S            : Unbounded_String_Vectors.Vector);
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
         S       : Unbounded_String_Vectors.Vector)
      is
         Matches : Match_Array (0 .. 3);

      begin
         for Line of S loop
            if Index (Line, "@") /= 0 then
               --  Tag has been found.

               exit;

            else
               Match (XML_Regpat, To_String (Line), Matches);

               if Matches (0) /= No_Match then
                  --  XML tag has been found

                  Parse_XML_Doc (Context, E, To_String (S));

                  return;
               end if;
            end if;
         end loop;

         Parse_Doc (Context, E, S);
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
            Parse_Doc_Wrapper (Context, Enum, Get_Doc (Enum).Text);
            Set_Doc (Enum, No_Comment_Result);
         end if;

         --  Report warning on undocumented values

         declare
            Cursor   : Tag_Cursor := First_Value (Get_Comment (Enum));
            Tag_Info : Tag_Info_Ptr;
         begin
            loop
               Tag_Info := Get (Cursor);

               if Tag_Info.Text.Is_Empty then
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
            Parse_Doc_Wrapper (Context, Rec, Get_Doc (Rec).Text);
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

                  if Tag_Info.Text.Is_Empty then
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
         Doc        : Comment_Result   := Get_Doc (Subp);

      begin
         --  Move documentation of generic formals and subprogram formals to
         --  the corresponding entity; on functions move also documentation
         --  of returned info to the internal return entity built by the
         --  frontend.

         declare
            Text : Unbounded_String_Vectors.Vector := Doc.Text;

         begin
            if Context.Options.Extensions_Enabled then
               Parse_Extract_Doc (Context, Subp, Text);

               --  Update the subprogram documentation not moved to its
               --  formals and return entities. This pending documentation
               --  will be parsed by Parse_Doc_Wrapper.

               Doc.Text.Clear;
               Doc.Text.Append (Text);
            end if;
         end;

         --  Initialize the structured comment associated with this entity

         Set_Comment (Subp, New_Structured_Comment);

         --  For backward compatibility it is still missing to append the
         --  tags of the generic formals???

         --  Search for documentation located in the subprogram profile
         --  (that is, comments located close to the parameter declarations)

         for Param of Get_Entities (Subp).all loop
            if Context.Options.Extensions_Enabled then
               Append_Param_Tag
                 (Comment    => Get_Comment (Subp),
                  Entity     => LL.Get_Entity (Param),
                  Param_Name => To_Unbounded_String (Get_Short_Name (Param)),
                  Text       => Unbounded_String_Vectors.Empty_Vector);
            else
               Append_Param_Tag
                 (Comment    => Get_Comment (Subp),
                  Entity     => LL.Get_Entity (Param),
                  Param_Name => To_Unbounded_String (Get_Short_Name (Param)),
                  Text       => Get_Doc (Param).Text);
            end if;
         end loop;

         --  Parse the documentation of the subprogram

         if Get_Doc (Subp) /= No_Comment_Result then
            Parse_Doc_Wrapper (Context, Subp, Doc.Text);
            Set_Doc (Subp, No_Comment_Result);
         end if;

         --  Report warning on undocumented parameters

         if Has_Params then
            if Context.Options.Extensions_Enabled then
               if Is_Generic (Subp) then
                  for Param of Get_Generic_Formals (Subp).all loop
                     if No (Get_Doc (Param)) then
                        Warning (Context, LL.Get_Entity (Param),
                          "undocumented generic formal ("
                          & Get_Short_Name (Param)
                          & ")");
                     end if;
                  end loop;
               end if;

               if Kind_In (Get_Kind (Subp),
                    E_Function,
                    E_Generic_Function)
                 and then No (Get_Doc (Get_Internal_Return (Subp)))
               then
                  --  Check if the spec has the documentation

                  if Present (Get_Corresponding_Spec (Subp))
                    and then
                      Present
                        (Get_Doc
                          (Get_Internal_Return
                            (Get_Corresponding_Spec (Subp))))
                  then
                     null;
                  else
                     Warning (Context, LL.Get_Entity (Subp),
                       "undocumented return value");
                  end if;
               end if;

               for Param of Get_Entities (Subp).all loop
                  if No (Get_Doc (Param)) then

                     --  Check if the spec has the documentation

                     if Present (Get_Corresponding_Spec (Param))
                       and then
                         Present (Get_Doc (Get_Corresponding_Spec (Param)))
                     then
                        null;

                     else
                        Warning (Context, LL.Get_Entity (Param),
                          "undocumented parameter ("
                          & Get_Short_Name (Param)
                          & ")");
                     end if;
                  end if;
               end loop;

            else
               declare
                  C        : Tag_Cursor := First_Param (Get_Comment (Subp));
                  Tag_Info : Tag_Info_Ptr;
               begin
                  loop
                     Tag_Info := Get (C);

                     if Tag_Info.Text.Is_Empty then
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

         elsif Is_Access_To_Subprogram_Type (Entity) then
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
              (Context, Entity, Get_Doc (Entity).Text);
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
