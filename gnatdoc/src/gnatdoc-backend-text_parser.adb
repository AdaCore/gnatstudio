------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2014, AdaCore                   --
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with GNAT.Regpat;            use GNAT.Regpat;

package body GNATdoc.Backend.Text_Parser is

   package Unbounded_String_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Ada.Strings.Unbounded.Unbounded_String,
        Ada.Strings.Unbounded."=");

   function Split_Lines (Text : String) return Unbounded_String_Vectors.Vector;

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
               when Initial | Paragraph | Code =>
                  null;

               when Itemized_List =>
                  Item_Offset : Positive;
            end case;

         when Code =>
            Code_Offset : Positive;
      end case;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, State_Type);

   LI_Pattern : constant Pattern_Matcher := Compile ("\s*([-*])\s*(\S)");
   P_Pattern  : constant Pattern_Matcher := Compile ("\s*(\S)");

   ----------------
   -- Parse_Text --
   ----------------

   function Parse_Text (Comment_Text : String) return Event_Vectors.Vector is
      Lines       : constant Unbounded_String_Vectors.Vector :=
        Split_Lines (Comment_Text);
      Result      : Event_Vectors.Vector;
      Current     : Positive := Lines.First_Index;
      State       : State_Type := ((Kind => Initial, Last_Para_Offset => <>));
      State_Stack : State_Vectors.Vector;
      LI_Matches  : Match_Array (0 .. 2);
      P_Matches   : Match_Array (0 .. 1);

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
      begin
         State_Stack.Append (State);
         State := (Kind => Paragraph, Para_Offset => P_Matches (1).First);
         Result.Append ((Start_Tag, To_Unbounded_String ("p")));
         Result.Append
           ((Text,
            Unbounded_Slice
              (Lines (Current),
               P_Matches (1).First,
               Length (Lines (Current)))));
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
         Result.Append ((Start_Tag, To_Unbounded_String ("ul")));
         Result.Append ((Start_Tag, To_Unbounded_String ("li")));
         Result.Append
           ((Text,
            Unbounded_Slice
              (Lines (Current),
               State.Para_Offset,
               Length (Lines (Current)))));
      end Open_UL_LI_And_Push;

   begin
      while Current <= Lines.Last_Index loop
         Match (LI_Pattern, To_String (Lines (Current)), LI_Matches);
         Match (P_Pattern, To_String (Lines (Current)), P_Matches);

         <<Restart>>
         case State.Kind is
            when Initial =>
               --  All empty lines at the beginning are ignored.

               if LI_Matches (0) /= No_Match then
                  Open_UL_LI_And_Push;

               elsif P_Matches (0) /= No_Match then
                  --  Check whether this is start of code block

                  if State.Last_Para_Offset <= P_Matches (1).First - 3 then
                     Result.Append ((Start_Tag, To_Unbounded_String ("pre")));
                     Result.Append
                       ((Text,
                        Unbounded_Slice
                          (Lines (Current),
                           P_Matches (1).First,
                           Length (Lines (Current)))));
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
                     Result.Append
                       ((Text,
                        Unbounded_Slice
                          (Lines (Current),
                           P_Matches (1).First,
                           Length (Lines (Current)))));
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
                          (Lines (Current),
                           State.Code_Offset,
                           Length (Lines (Current)))));

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
                     Result.Append ((Start_Tag, To_Unbounded_String ("li")));
                     Result.Append
                       ((Text,
                        Unbounded_Slice
                          (Lines (Current),
                           State.Para_Offset,
                           Length (Lines (Current)))));
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

   -----------------
   -- Split_Lines --
   -----------------

   function Split_Lines
     (Text : String) return Unbounded_String_Vectors.Vector
   is
      First   : Positive := Text'First;
      Current : Positive := Text'First;
      Result  : Unbounded_String_Vectors.Vector;

   begin
      while Current <= Text'Last loop
         if Text (Current) = CR or Text (Current) = LF then
            Result.Append (To_Unbounded_String (Text (First .. Current - 1)));

            --  CR & LF combination is handled as single line separator

            if Text (Current) = CR
              and then Current < Text'Last
              and then Text (Current + 1) = LF
            then
               Current := Current + 2;

            else
               Current := Current + 1;
            end if;

            First := Current;

         else
            Current := Current + 1;
         end if;
      end loop;

      if First /= Current then
         --  Append content of last non terminated line

         Result.Append (To_Unbounded_String (Text (First .. Text'Last)));
      end if;

      return Result;
   end Split_Lines;

end GNATdoc.Backend.Text_Parser;
