------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
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

with Ada.Strings.Fixed;
with GNAT.Regpat;            use GNAT.Regpat;

with GNATdoc.Customization.Markup_Generators;
use GNATdoc.Customization.Markup_Generators;
with GNATdoc.Customization.Tag_Handlers;
use GNATdoc.Customization.Tag_Handlers;

package body GNATdoc.Backend.Text_Parser is

   use GNATdoc.Markup_Streams;

   type State_Kinds is (Initial, Paragraph, Itemized_List, Code);

   type State_Type (Kind : State_Kinds := Initial) is record
      case Kind is
         when Initial =>
            Last_Para_Offset : Positive := Positive'Last;
            --  Offset of last paragraph. It is used to detect code blocks
            --  which is at least three characters deeper then last paragraph.

         when Paragraph | Itemized_List =>
            Para_Offset : Positive;

            case Kind is
               when Initial | Code =>
                  null;

               when Paragraph =>
                  Emit_After  : Event_Vectors.Vector;
                  --  Sequence of events which will be emitted into the result
                  --  stream after close of paragraph.

               when Itemized_List =>
                  Item_Offset : Positive;
            end case;

         when Code =>
            Code_Offset : Positive;
      end case;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, State_Type);

   function Build_Doc_Tag_Pattern return Pattern_Matcher;
   --  Builds regular expression to detect all known inline tags

   LI_Pattern        : constant Pattern_Matcher :=
     Compile ("^\s+([-*])\s*(\S)");
   P_Pattern         : constant Pattern_Matcher := Compile ("\s*(\S)");
   Parameter_Pattern : constant Pattern_Matcher := Compile ("\s*(\S*)");

   ---------------------------
   -- Build_Doc_Tag_Pattern --
   ---------------------------

   function Build_Doc_Tag_Pattern return Pattern_Matcher is
      Expression : Unbounded_String;

   begin
      for Tag of Get_Inline_Tags loop
         if Expression = "" then
            Append (Expression, "@(");

         else
            Append (Expression, '|');
         end if;

         Append (Expression, Tag);
      end loop;

      if Expression /= "" then
         Append (Expression, ")");
      end if;

      return Compile (To_String (Expression));
   end Build_Doc_Tag_Pattern;

   ----------------
   -- Parse_Text --
   ----------------

   function Parse_Text
     (Comment_Text : Unbounded_String_Vectors.Vector)
      return Event_Vectors.Vector
   is
      Doc_Tag_Pattern : constant Pattern_Matcher := Build_Doc_Tag_Pattern;
      Result          : Event_Vectors.Vector;
      Current         : Positive := Comment_Text.First_Index;
      State           : State_Type :=
        ((Kind => Initial, Last_Para_Offset => <>));
      State_Stack     : State_Vectors.Vector;
      LI_Matches      : Match_Array (0 .. 2);
      P_Matches       : Match_Array (0 .. 1);

      procedure Parse_Line
        (Line        : String;
         Line_Events : out Event_Vectors.Vector;
         Emit_After  : out Event_Vectors.Vector);
      --  Parse tags in line and process them. Result line is returned in
      --  Text_Line parameter, set of events to be emitted after close of
      --  current event is returned in Emit_After parameter.

      procedure Close_P_And_Pop;

      procedure Close_Pre_And_Pop;

      procedure Close_LI_UL_And_Pop;

      procedure Open_P_And_Push;

      procedure Open_UL_LI_And_Push;

      -------------------------
      -- Close_LI_UL_And_Pop --
      -------------------------

      procedure Close_LI_UL_And_Pop is
      begin
         Result.Append ((End_Tag, To_Unbounded_String ("li")));
         Result.Append ((End_Tag, To_Unbounded_String ("ul")));
         State := State_Stack.Last_Element;
         State_Stack.Delete_Last;
      end Close_LI_UL_And_Pop;

      ---------------------
      -- Close_P_And_Pop --
      ---------------------

      procedure Close_P_And_Pop is
         Para_Offset : constant Positive := State.Para_Offset;

      begin
         Result.Append ((End_Tag, To_Unbounded_String ("p")));
         Result.Append (State.Emit_After);
         State := State_Stack.Last_Element;
         State_Stack.Delete_Last;

         if State.Kind = Initial then
            State.Last_Para_Offset := Para_Offset;
         end if;
      end Close_P_And_Pop;

      -----------------------
      -- Close_Pre_And_Pop --
      -----------------------

      procedure Close_Pre_And_Pop is
      begin
         Result.Append ((End_Tag, To_Unbounded_String ("pre")));
         State := State_Stack.Last_Element;
         State_Stack.Delete_Last;

         if State.Kind = Initial then
            State.Last_Para_Offset := Positive'Last;
         end if;
      end Close_Pre_And_Pop;

      ---------------------
      -- Open_P_And_Push --
      ---------------------

      procedure Open_P_And_Push is
         Line_Events : Event_Vectors.Vector;
         Emit_After  : Event_Vectors.Vector;

      begin
         Parse_Line
           (Slice
              (Comment_Text (Current),
               P_Matches (1).First,
               Length (Comment_Text (Current))),
            Line_Events,
            Emit_After);

         if not Line_Events.Is_Empty then
            State_Stack.Append (State);
            State :=
              (Kind        => Paragraph,
               Para_Offset => P_Matches (1).First,
               Emit_After  => Emit_After);
            Result.Append
              ((Kind       => Start_Tag,
                Name       => To_Unbounded_String ("p"),
                Attributes => <>));
            Result.Append (Line_Events);

         else
            Result.Append (Emit_After);
         end if;
      end Open_P_And_Push;

      -------------------------
      -- Open_UL_LI_And_Push --
      -------------------------

      procedure Open_UL_LI_And_Push is
      begin
         State_Stack.Append (State);
         State :=
           (Kind        => Itemized_List,
            Item_Offset => LI_Matches (1).First,
            Para_Offset => LI_Matches (2).First);
         Result.Append
           ((Kind       => Start_Tag,
             Name       => To_Unbounded_String ("ul"),
             Attributes => <>));
         Result.Append
           ((Kind       => Start_Tag,
             Name       => To_Unbounded_String ("li"),
             Attributes => <>));
         Result.Append
           ((Text,
            Unbounded_Slice
              (Comment_Text (Current),
               State.Para_Offset,
               Length (Comment_Text (Current)))));
      end Open_UL_LI_And_Push;

      ----------------
      -- Parse_Line --
      ----------------

      procedure Parse_Line
        (Line        : String;
         Line_Events : out Event_Vectors.Vector;
         Emit_After  : out Event_Vectors.Vector)
      is
         First             : Positive := Line'First;
         Doc_Tag_Matches   : Match_Array (0 .. 1);
         Parameter_Matches : Match_Array (0 .. 1);
         Handler           : Inline_Tag_Handler_Access;
         Writer            : Markup_Generator;
         Parameter         : Unbounded_String;

      begin
         --  Parse line to extract embedded tags and process them

         loop
            Match
              (Doc_Tag_Pattern, Line (First .. Line'Last), Doc_Tag_Matches);

            if Doc_Tag_Matches (0) = No_Match then
               if First <= Line'Last then
                  Line_Events.Append
                    ((Text, To_Unbounded_String (Line (First .. Line'Last))));
               end if;

               exit;

            else
               if First <= Doc_Tag_Matches (0).First - 1 then
                  Line_Events.Append
                    ((Text,
                      To_Unbounded_String
                       (Ada.Strings.Fixed.Trim
                          (Line (First .. Doc_Tag_Matches (0).First - 1),
                           Ada.Strings.Right))));
               end if;

               First := Doc_Tag_Matches (0).Last + 1;

               --  Lookup for tag handler

               Handler :=
                 Get_Inline_Tag_Handler
                   (Line
                      (Doc_Tag_Matches (1).First .. Doc_Tag_Matches (1).Last));

               --  Process tag's parameter when necessary

               if Handler.Has_Parameter then
                  Match
                    (Parameter_Pattern,
                     Line (First .. Line'Last),
                     Parameter_Matches);

                  if Parameter_Matches (0) /= No_Match then
                     First := Parameter_Matches (0).Last + 1;
                     Parameter :=
                       To_Unbounded_String
                         (Line
                            (Parameter_Matches (1).First
                             .. Parameter_Matches (1).Last));

                  else
                     --  Ignore tag due to absence of parameter.

                     --  ??? Error should be reported here

                     Handler := null;
                  end if;
               end if;

               --  Run tag handler

               if Handler /= null then
                  Handler.To_Markup (To_String (Parameter), Writer);
                  Line_Events.Append (Writer.Get_Inline_Stream);
                  Emit_After.Append (Writer.Get_After_Stream);
               end if;
            end if;
         end loop;
      end Parse_Line;

   begin
      while Current <= Comment_Text.Last_Index loop
         Match (LI_Pattern, To_String (Comment_Text (Current)), LI_Matches);
         Match (P_Pattern, To_String (Comment_Text (Current)), P_Matches);

         <<Restart>>
         case State.Kind is
            when Initial =>
               --  All empty lines at the beginning are ignored.

               if LI_Matches (0) /= No_Match then
                  Open_UL_LI_And_Push;

               elsif P_Matches (0) /= No_Match then
                  --  Check whether this is start of code block

                  if State.Last_Para_Offset <= P_Matches (1).First - 3 then
                     Result.Append
                       ((Kind       => Start_Tag,
                         Name       => To_Unbounded_String ("pre"),
                         Attributes => <>));
                     Result.Append
                       ((Text,
                        Unbounded_Slice
                          (Comment_Text (Current),
                           P_Matches (1).First,
                           Length (Comment_Text (Current)))));
                     State_Stack.Append (State);
                     State :=
                       ((Kind => Code, Code_Offset => P_Matches (1).First));

                  else
                     Open_P_And_Push;
                  end if;
               end if;

            when Paragraph =>
               if LI_Matches (0) /= No_Match then
                  --  Nested constructions must be handled here!!!

                  if LI_Matches (1).First < State.Para_Offset then
                     Close_P_And_Pop;

                     goto Restart;

                  else
                     Open_UL_LI_And_Push;
                  end if;

               elsif P_Matches (0) /= No_Match then
                  if State.Para_Offset <= P_Matches (1).First - 3 then
                     --  This line is deep enough to be processed as code block

                     Close_P_And_Pop;

                     goto Restart;

                  else
                     declare
                        Line_Events : Event_Vectors.Vector;

                     begin
                        Parse_Line
                          (Slice
                             (Comment_Text (Current),
                              P_Matches (1).First,
                              Length (Comment_Text (Current))),
                           Line_Events,
                           State.Emit_After);

                        if not Line_Events.Is_Empty then
                           Result.Append (Line_Events);
                        end if;
                     end;
                  end if;

               else
                  --  Empty line means paragraph separator.

                  Close_P_And_Pop;
               end if;

            when Code =>
               if P_Matches (0) /= No_Match then
                  if P_Matches (1).First >= State.Code_Offset then
                     Result.Append
                       ((Text,
                        Unbounded_Slice
                          (Comment_Text (Current),
                           State.Code_Offset,
                           Length (Comment_Text (Current)))));

                  else
                     Close_Pre_And_Pop;

                     goto Restart;
                  end if;

               else
                  Result.Append ((Text, Null_Unbounded_String));
               end if;

            when Itemized_List =>
               if LI_Matches (0) /= No_Match then
                  if LI_Matches (1).First = State.Item_Offset then
                     --  Continue previous itemized list

                     Result.Append ((End_Tag, To_Unbounded_String ("li")));
                     Result.Append
                       ((Kind       => Start_Tag,
                         Name       => To_Unbounded_String ("li"),
                         Attributes => <>));
                     Result.Append
                       ((Text,
                        Unbounded_Slice
                          (Comment_Text (Current),
                           State.Para_Offset,
                           Length (Comment_Text (Current)))));
                     State.Para_Offset := LI_Matches (2).First;

                  elsif LI_Matches (1).First > State.Item_Offset then
                     --  Nested list

                     Open_UL_LI_And_Push;

                  else
                     --  Returns to parent list.

                     Close_LI_UL_And_Pop;

                     goto Restart;
                  end if;

               elsif P_Matches (0) /= No_Match then
                  --  Paragraph after list item, it can be additional paragraph
                  --  of list item or new paragraph.

                  if P_Matches (1).First = State.Para_Offset then
                     Open_P_And_Push;

                  else
                     --  Close current list

                     Close_LI_UL_And_Pop;

                     goto Restart;
                  end if;
               end if;
         end case;

         Current := Current + 1;
      end loop;

      loop
         case State.Kind is
            when Initial =>
               null;

            when Paragraph =>
               Close_P_And_Pop;

            when Code =>
               Close_Pre_And_Pop;

            when Itemized_List =>
               Close_LI_UL_And_Pop;
         end case;

         exit when State_Stack.Is_Empty;
      end loop;

      return Result;
   end Parse_Text;

end GNATdoc.Backend.Text_Parser;
