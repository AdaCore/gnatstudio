------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2017, AdaCore                   --
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

   -------------------------------
   -- Build_Structured_Comments --
   -------------------------------

   procedure Build_Structured_Comments
     (Context : access constant Docgen_Context;
      Root    : Entity_Id)
   is
      Private_Entities_List : aliased EInfo_List.Vector;

      procedure Error
        (Entity  : Entity_Id;
         Msg     : String);
      --  Report the error message Msg on the location of Entity and store it
      --  on the entity.

      function Is_Custom_Tag (Tag : String) return Boolean;
      --  Return True if Tag is a supported tag.
      --  ??? This info should be configurable in a separate file to allow
      --  customers to define their own tags

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

      procedure Parse_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String);
      --  Parse the contents of S and store its contents in the structured
      --  comment of E (ie. E.Comment)

      procedure Parse_XML_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String);
      --  Parse the contents of S and store its contents in the structured
      --  comment of E (ie. E.Comment)

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Dispatch a call to build an structured comment between routines
      --  Parse_Doc, Parse_Enumeration_Comments, Parse_Record_Comments,
      --  and Parse_Subprogram_Comments.

      procedure Remove_Private_Entities;
      --  Private entities are those annotated with the private tag. This
      --  routine takes care of removing them from the tree.

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

      -----------------------
      -- Parse_Doc_Wrapper --
      -----------------------

      procedure Parse_Doc_Wrapper
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         S       : String)
      is
         Matches : Match_Array (0 .. 3);

      begin
         if Index (S, "@") > 0 then
            Parse_Doc (Context, E, S);
            return;
         end if;

         Match (XML_Regpat, S, Matches);

         if Matches (0) /= No_Match then
            Parse_XML_Doc (Context, E, S);
         else
            Parse_Doc (Context, E, S);
         end if;
      end Parse_Doc_Wrapper;

      ---------------
      -- Parse_Doc --
      ---------------

      procedure Parse_Doc
        (Context : access constant Docgen_Context;
         E       : Entity_Id;
         S       : String)
      is
         pragma Unreferenced (Context);
         Comment : constant Structured_Comment := Get_Comment (E);
         Current : Tag_Cursor := New_Cursor (Comment);

         procedure Parse (S : String);
         --  Parse the contents of S searching for the next tag

         procedure Parse (S : String) is
            Tag_Indicator : constant Character := '@';

            type Location is record
               First : Natural;
               Last  : Natural;
            end record;
            No_Location : constant Location := (0, 0);

            procedure Check_Tag (Tag_Name : String);
            --  Check if this tag is applicable to entity E

            function No (Loc : Location) return Boolean;
            --  True if Loc = No_Location

            function Present (Loc : Location) return Boolean;
            --  True if Loc /= No_Location

            function Scan_Tag return Location;
            --  Scan text in S searching for the next tag

            procedure Scan_Word
              (J   : in out Natural;
               Loc : out Location);
            --  Scan next word in S

            procedure Scan_Line
              (J   : in out Natural;
               Loc : out Location);
            --  Scan S searching for the next end of line

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

               elsif Tag_Name = "exception" then
                  if not Is_Subprogram_Or_Entry (E) then
                     Report_Error;
                  end if;

               elsif Tag_Name = "field" then
                  if not Is_Record_Type (E) then
                     Report_Error;
                  end if;

               elsif Tag_Name = "param" then
                  if not Is_Subprogram_Or_Entry (E) then
                     Report_Error;
                  end if;

               elsif Tag_Name = "return" then
                  if Get_Kind (E) /= E_Function then
                     Report_Error;
                  end if;
               end if;
            end Check_Tag;

            --------
            -- No --
            --------

            function No (Loc : Location) return Boolean is
            begin
               return Loc = No_Location;
            end No;

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

            function Scan_Tag return Location is
               J     : Natural := S'First;
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

            Tag_Loc : Location;
         begin
            Tag_Loc := Scan_Tag;

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
                           Text : constant String :=
                             S (Text_Loc.First .. Text_Loc.Last);
                        begin
                           Append_Text (Current, Text);
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
         if S = "" then
            return;
         end if;

         Parse (S);
         Set_Comment (E, Comment);
      end Parse_Doc;

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

         for Param of Get_Entities (Subp).all loop
            --  Temporarily disabling this assertion since the Xref.Field
            --  service sometimes fails with protected objects returning
            --  entities which correspond to formals of anonymous access to
            --  subprograms of other entities defined in the same package ???

--            pragma Assert (Get_Kind (Param) = E_Formal);

            Append_Param_Tag
              (Comment    => Get_Comment (Subp),
               Entity     => LL.Get_Entity (Param),
               Param_Name => To_Unbounded_String (Get_Short_Name (Param)),
               Text       => Get_Doc (Param).Text);
         end loop;

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

      -------------------
      -- Parse_XML_Doc --
      -------------------

      procedure Parse_XML_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String)
      is
         pragma Unreferenced (Context);

         Comment : constant Structured_Comment := Get_Comment (E);
         Current : Tag_Cursor := New_Cursor (Comment);

         procedure Parse (S : String);
         --  Parse the contents of S searching for the next tag

         procedure Parse (S : String) is
            Matches      : Match_Array (0 .. 3);
            Tag_Text     : Unbounded_String;
            --  Stand_Alone  : Boolean;

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
      Traverse_Tree (Root, Process_Node'Access);

      if Private_Entities_List.Length > 0 then
         Remove_Private_Entities;
      end if;
   end Build_Structured_Comments;

   -------------------
   -- May_Have_Tags --
   -------------------

   function May_Have_Tags (Text : Unbounded_String) return Boolean is
   begin
      if Index (Text, "@") > 0 then
         return True;

      else
         declare
            S       : constant String := To_String (Text);
            Matches : Match_Array (0 .. 3);

         begin
            Match (XML_Regpat, S, Matches);

            return Matches (0) /= No_Match;
         end;
      end if;
   end May_Have_Tags;

end GNATdoc.Frontend.Comment_Parser;
