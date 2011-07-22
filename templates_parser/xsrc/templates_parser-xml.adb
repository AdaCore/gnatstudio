------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2010, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;

with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.File;
with Input_Sources.Strings;
with Sax.Readers;
with Unicode.CES.Basic_8bit;
with Unicode.CES.Utf8;

with Templates_Parser.Utils;

package body Templates_Parser.XML is

   Labels_Suffix      : constant String := "_LABELS";
   Description_Suffix : constant String := "_DESCRIPTION";

   package Str_Map is new Containers.Indefinite_Hashed_Maps
     (String, Unbounded_String, Strings.Hash, "=", "=");

   function Parse_Document (Doc : DOM.Core.Node) return Translate_Set;
   --  Parse a document node and return the corresponding Translate_Set

   -----------
   -- Image --
   -----------

   function Image (Translations : Translate_Set) return Unbounded_String is
      Result : Unbounded_String;

      procedure Process (Cursor : Association_Map.Cursor);
      --  Iterator

      procedure Add (Str : String);
      pragma Inline (Add);
      --  Add a new line (str) into Result, a LF is added at the end of Str

      function To_Utf8 (Str : Unbounded_String) return String;
      --  Convert Str to UTF8

      ---------
      -- Add --
      ---------

      procedure Add (Str : String) is
      begin
         Append (Result, Str & ASCII.LF);
      end Add;

      -------------
      -- Process --
      -------------

      procedure Process (Cursor : Association_Map.Cursor) is

         Item : constant Association := Association_Map.Element (Cursor);
         --  Current item

         Var  : constant String := To_Utf8 (Item.Variable);
         --  Item variable name

         procedure Process_Std;
         --  Handles standard variables

         procedure Process_Composite;
         --  Handles composite variables

         procedure Add_Description (Var : String);
         --  Add var description for Var if found in the translation set

         function Is_Labels return Boolean;
         --  Returns True if Item is a Label entry

         function Is_Description return Boolean;
         --  Returns True if Item is a Description entry

         ---------------------
         -- Add_Description --
         ---------------------

         procedure Add_Description (Var : String) is
            Var_Description : constant String := Var & Description_Suffix;
         begin
            if Translations.Set.Contains (Var_Description) then
               --  There is probably a label encoded into this set
               declare
                  Description : constant Association :=
                                  Translations.Set.Element (Var_Description);
               begin
                  if Description.Kind = Std
                    and then Description.Value /= ""
                  then
                        --  Definitly a label for this variable
                     Add ("         <Description>"
                          & To_Utf8 (Description.Value) & "</Description>");
                  end if;
               end;
            end if;
         end Add_Description;

         --------------------
         -- Is_Description --
         --------------------

         function Is_Description return Boolean is
            N, L : Natural;
         begin
            if Var'Length > Description_Suffix'Length
              and then
                Var (Var'Last - Description_Suffix'Length + 1 .. Var'Last)
                  = Description_Suffix
              and then Translations.Set.Contains
                (Var (Var'First .. Var'Last - Description_Suffix'Length))
            then
               return True;
            end if;

            --  Nested tag description

            N := Strings.Fixed.Index (Var, "_DIM");

            if N = 0 or else Var (N + 4) not in '0' .. '9' then
               return False;

            else
               L := N - 1; -- Last character for the tag name
               N := N + 4; -- First character after _DIM

               loop
                  N := N + 1;
                  exit when Var (N) = '_' or else N = Var'Last;

                  if Var (N) not in '0' .. '9' then
                     --  Not a digit here, this is not a label
                     return False;
                  end if;
               end loop;

               return Var (N .. Var'Last) = Description_Suffix
                 and then Translations.Set.Contains (Var (Var'First .. L));
            end if;
         end Is_Description;

         ---------------
         -- Is_Labels --
         ---------------

         function Is_Labels return Boolean is
            N, L : Natural;
         begin
            N := Strings.Fixed.Index (Var, "_DIM");

            if N = 0 or else Var (N + 4) not in '0' .. '9' then
               return False;

            else
               L := N - 1; -- Last character for the tag name
               N := N + 4; -- First character after _DIM

               loop
                  N := N + 1;
                  exit when Var (N) = '_' or else N = Var'Last;

                  if Var (N) not in '0' .. '9' then
                     --  Not a digit here, this is not a label
                     return False;
                  end if;
               end loop;

               return Var (N .. Var'Last) = Labels_Suffix
                 and then Translations.Set.Contains (Var (Var'First .. L));
            end if;
         end Is_Labels;

         -----------------------
         -- Process_Composite --
         -----------------------

         procedure Process_Composite is

            Null_Indice : constant Indices := (2 .. 1 => 0);

            procedure Output_Tag (T : Tag; Pos : Indices := Null_Indice);
            --  Output recursively tag T, Pos is the current indices for the
            --  parsed items.

            procedure Output_Axis (N : Positive; T : Tag);
            --  Output labels and description for axis number N. Labels are
            --  found in tag T. T must be a vector tag (Nested_Level = 1).

            -----------------
            -- Output_Axis --
            -----------------

            procedure Output_Axis (N : Positive; T : Tag) is
               P : Tag_Node_Access := T.Data.Head;
               K : Positive := 1;
            begin
               pragma Assert (T.Data.Nested_Level = 1);

               Add ("      <Dim n=""" & Utils.Image (N) & """>");
               Add_Description (Var & "_DIM" & Utils.Image (N));
               Add ("         <Labels>");

               while P /= null loop
                  Add ("            <Label ind=""" & Utils.Image (K) & """>"
                       & To_Utf8 (P.V) & "</Label>");
                  K := K + 1;
                  P := P.Next;
               end loop;

               Add ("         </Labels>");
               Add ("      </Dim>");
            end Output_Axis;

            ----------------
            -- Output_Tag --
            ----------------

            procedure Output_Tag
              (T   : Tag;
               Pos : Indices := Null_Indice)
            is
               use type Indices;

               procedure Output_Value (Pos : Indices; Value : String);
               --  Output value whose Tag indices is given by Pos

               ------------------
               -- Output_Value --
               ------------------

               procedure Output_Value (Pos : Indices; Value : String) is
                  V : Unbounded_String;
               begin
                  Append (V, "      <Entry>");

                  for K in Pos'Range loop
                     Append
                       (V,
                        "<Ind n=""" & Utils.Image (K) & """>"
                        & Utils.Image (Pos (K)) & "</Ind>");
                  end loop;

                  Append (V, "<V>" & Value & "</V></Entry>");

                  Add (To_Utf8 (V));
               end Output_Value;

               N : Tag_Node_Access := T.Data.Head;
               P : Positive := 1;
            begin
               while N /= null loop
                  if N.Kind = Value then
                     Output_Value (Pos & Indices'(1 => P), To_Utf8 (N.V));
                  else
                     Output_Tag (N.VS.all, Pos & Indices'(1 => P));
                  end if;

                  P := P + 1;
                  N := N.Next;
               end loop;
            end Output_Tag;

         begin
            Add ("   <CompositeTag>");
            Add ("      <Tag>");
            Add ("         <Name>" & Var & "</Name>");
            Add_Description (Var);
            Add ("      </Tag>");

            --  Output axis labels

            for K in 1 .. Item.Comp_Value.Data.Nested_Level loop
               declare
                  Label_Var : constant String :=
                                Var & "_DIM" & Utils.Image (K) & Labels_Suffix;
               begin
                  if Translations.Set.Contains (Label_Var) then
                     declare
                        Item : constant Association :=
                                 Translations.Set.Element (Label_Var);
                     begin
                        if Item.Kind = Composite
                          and then Item.Comp_Value.Data.Nested_Level = 1
                        then
                           --  This is a vector tag, labels are expected to
                           --  be found on this vector.
                           Output_Axis (K, Item.Comp_Value);
                        end if;
                     end;
                  end if;
               end;
            end loop;

            --  Output values
            Output_Tag (Item.Comp_Value);

            Add ("   </CompositeTag>");
         end Process_Composite;

         -----------------
         -- Process_Std --
         -----------------

         procedure Process_Std is
         begin
            Add ("   <SimpleTag>");
            Add ("      <Tag>");
            Add ("         <Name>" & Var & "</Name>");
            Add_Description (Var);
            Add ("      </Tag>");
            Add ("      <V>" & To_Utf8 (Item.Value) & "</V>");
            Add ("   </SimpleTag>");
         end Process_Std;

      begin
         --  Do not process labels encoded for another variable

         if not Is_Labels and then not Is_Description then
            case Item.Kind is
               when Std       => Process_Std;
               when Composite => Process_Composite;
            end case;
         end if;
      end Process;

      -------------
      -- To_Utf8 --
      -------------

      function To_Utf8 (Str : Unbounded_String) return String is
         use Unicode.CES;
      begin
         return Utf8.From_Utf32 (Basic_8bit.To_Utf32 (To_String (Str)));
      end To_Utf8;

   begin
      --  XML header

      Add ("<?xml version=""1.0"" encoding=""UTF-8"" ?>");
      Add ("<Tagged xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">");

      Translations.Set.Iterate (Process'Access);

      --  XML footer

      Add ("</Tagged>");

      return Result;
   end Image;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Translate_Set is
      use DOM.Core;
      use DOM.Core.Nodes;
      use DOM.Readers;
      use Input_Sources;
      use Sax.Readers;

      Reader : Tree_Reader;
      Input  : File.File_Input;
      Doc    : DOM.Core.Document;
      Result : Translate_Set;

   begin
      File.Open (Filename, Input);

      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Parse (Reader, Input);

      File.Close (Input);

      Doc := Get_Tree (Reader);

      Result := Parse_Document (Doc);

      Free (Doc);

      return Result;
   end Load;

   --------------------
   -- Parse_Document --
   --------------------

   function Parse_Document
     (Doc : DOM.Core.Node)
      return Translate_Set is

      use DOM.Core;
      use DOM.Core.Nodes;
      use DOM.Readers;
      use Input_Sources;
      use Sax.Readers;

      procedure Error (Node : DOM.Core.Node; Message : String);
      pragma No_Return (Error);
      --  Raises Constraint_Error with the Message as exception message

      function First_Child (Parent : DOM.Core.Node) return DOM.Core.Node;
      --  Returns first child, skips #text node

      function Next_Sibling (N : DOM.Core.Node) return DOM.Core.Node;
      --  Returns next sibling, skip #text nodes

      function Parse_Tagged (N : DOM.Core.Node) return Translate_Set;
      --  Parse tagged entity

      function Parse_SimpleTag (N : DOM.Core.Node) return Translate_Set;
      --  Parse a SimpleTag entity

      function Parse_CompositeTag (N : DOM.Core.Node) return Translate_Set;
      --  Parse CompositeTag entity

      procedure Parse_Tag
        (N                 : DOM.Core.Node;
         Name, Description :    out Unbounded_String);
      --  Parse a Tag node, set Name and Description

      function Get_Value (N : DOM.Core.Node) return String;
      --  Returns N value or the empty string if N is null

      -----------
      -- Error --
      -----------

      procedure Error (Node : DOM.Core.Node; Message : String) is
         Name : constant String := Local_Name (Node);
      begin
         raise Constraint_Error with Name & " - " & Message;
      end Error;

      -----------------
      -- First_Child --
      -----------------

      function First_Child (Parent : DOM.Core.Node) return DOM.Core.Node is
         N : DOM.Core.Node;
      begin
         N := DOM.Core.Nodes.First_Child (Parent);

         while N /= null and then DOM.Core.Nodes.Node_Name (N) = "#text" loop
            N := DOM.Core.Nodes.Next_Sibling (N);
         end loop;

         return N;
      end First_Child;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (N : DOM.Core.Node) return String is
         use Unicode.CES;
      begin
         if N = null then
            return "";
         else
            return Basic_8bit.From_Utf32 (Utf8.To_Utf32 (Node_Value (N)));
         end if;
      end Get_Value;

      ------------------
      -- Next_Sibling --
      ------------------

      function Next_Sibling (N : DOM.Core.Node) return DOM.Core.Node is
         M : DOM.Core.Node := N;
      begin
         loop
            M := DOM.Core.Nodes.Next_Sibling (M);
            exit when M = null or else DOM.Core.Nodes.Node_Name (M) /= "#text";
         end loop;

         return M;
      end Next_Sibling;

      ------------------------
      -- Parse_CompositeTag --
      ------------------------

      function Parse_CompositeTag
        (N : DOM.Core.Node) return Translate_Set
      is
         function Parse_Dim (N : DOM.Core.Node) return Translate_Set;
         --  Parse a Dim node

         procedure Parse_Entry (N : DOM.Core.Node);
         --  Parse an Entry node

         function Build_Tag return Tag;
         --  Build tag from Data map

         Name        : Unbounded_String; -- current tag name
         Description : Unbounded_String; -- current tag description

         Data        : Str_Map.Map;
         --  All data inserted into this map from Entry nodes, the key is the
         --  indexes separated with '_'.

         Level       : Natural := 0;
         --  Number of nested level for the data

         ---------------
         -- Build_Tag --
         ---------------

         function Build_Tag return Tag is

            function B_Tag (Key : String; N : Positive) return Tag;
            --  Recursive routine, will build the Tag in the right order with
            --  the right nested levels.

            -----------
            -- B_Tag --
            -----------

            function B_Tag (Key : String; N : Positive) return Tag is
               use type Str_Map.Cursor;
               Max_Key : constant String := Utils.Image (N) & "_MAX";
               Cursor  : Str_Map.Cursor;
               Max     : Natural;
               T       : Tag;
            begin
               Cursor := Data.Find (Max_Key);

               Max := Natural'Value (To_String (Str_Map.Element (Cursor)));

               if N = Level then
                  --  We have reached the last level

                  for K in 1 .. Max loop
                     Cursor := Data.Find (Key & "_" & Utils.Image (K));
                     exit when Cursor = Str_Map.No_Element;
                     T := T & Str_Map.Element (Cursor);
                  end loop;

               else
                  for K in 1 .. Max loop
                     T := T & B_Tag ("_" & Utils.Image (K), N + 1);
                  end loop;
               end if;

               return T;
            end B_Tag;

         begin
            return B_Tag ("", 1);
         end Build_Tag;

         ---------------
         -- Parse_Dim --
         ---------------

         function Parse_Dim (N : DOM.Core.Node) return Translate_Set is

            function Parse_Labels (N : DOM.Core.Node) return Translate_Set;
            --  Parse Labels node

            D : Positive; -- current Dim level

            ------------------
            -- Parse_Labels --
            ------------------

            function Parse_Labels
              (N : DOM.Core.Node)
               return Translate_Set
            is
               use Str_Map;
               C       : DOM.Core.Node := First_Child (N);
               Result  : Translate_Set;
               T       : Tag;
               Max     : Natural := 0;
               Map     : Str_Map.Map;
               K       : Positive;
               Cursor  : Str_Map.Cursor;
               Success : Boolean;
            begin
               while C /= null loop
                  declare
                     N_Name : constant String := Local_Name (C);
                     Atts   : constant DOM.Core.Named_Node_Map
                       := DOM.Core.Nodes.Attributes (C);
                  begin
                     if N_Name = "Label" then
                        if Length (Atts) = 1
                          and then Local_Name (Item (Atts, 0)) = "ind"
                        then
                           K   := Positive'Value (Node_Value (Item (Atts, 0)));
                           Max := Natural'Max (Max, K);

                           Map.Insert
                             (Node_Value (Item (Atts, 0)),
                              To_Unbounded_String
                                (Get_Value (DOM.Core.Nodes.First_Child (C))),
                              Cursor,
                              Success);

                           if not Success then
                              Error
                                (C,
                                 "Duplicate label for ind "
                                 & Node_Value (Item (Atts, 0)));
                           end if;
                        else
                           Error (C, "A single attribute ind expected");
                        end if;
                     else
                        Error (C, "Entity Label expected, found " & N_Name);
                     end if;
                  end;
                  C := Next_Sibling (C);
               end loop;

               --  Now we have all labels indexed into the Map (key being the
               --  order number. Place them in the right order into T.

               for K in 1 .. Max loop
                  declare
                     K_Img : constant String := Utils.Image (K);
                  begin
                     Cursor := Map.Find (K_Img);

                     if Str_Map.Has_Element (Cursor) then
                        T := T & Str_Map.Element (Cursor);
                     else
                        T := T & "";
                     end if;
                  end;
               end loop;

               --  The vector tag is now ready, build the association

               Insert
                 (Result,
                  Assoc
                    (To_String (Name) & "_DIM" & Utils.Image (D) & "_LABELS",
                     T));

               return Result;
            end Parse_Labels;

            Atts   : constant DOM.Core.Named_Node_Map
              := DOM.Core.Nodes.Attributes (N);
            Result : Translate_Set;
            C      : DOM.Core.Node;

         begin
            if Length (Atts) /= 1 then
               Error (N, "A Dim node can have a single attribute");
            end if;

            declare
               A     : constant DOM.Core.Node := Item (Atts, 0);
               Name  : constant String := Local_Name (A);
               Value : constant String := Node_Value (A);
            begin
               if Name = "n" then
                  D := Positive'Value (Value);
               else
                  Error (A, "Attribute name n expected, found " & Name);
               end if;
            end;

            --  Now look for all nodes

            C := First_Child (N);

            while C /= null loop
               declare
                  N_Name : constant String := Local_Name (C);
               begin
                  if N_Name = "Description" then
                     Insert
                       (Result,
                        Assoc
                          (To_String (Name) & "_DIM"
                           & Utils.Image (D) & "_DESCRIPTION",
                           Get_Value (DOM.Core.Nodes.First_Child (C))));

                  elsif N_Name = "Labels" then
                     Insert (Result, Parse_Labels (C));
                  end if;
               end;
               C := Next_Sibling (C);
            end loop;

            return Result;
         end Parse_Dim;

         -----------------
         -- Parse_Entry --
         -----------------

         procedure Parse_Entry (N : DOM.Core.Node) is

            procedure Insert (Key, Value : String);
            --  Insert key/value into Map and keep Max value found for this
            --  key inside Data map.

            C     : DOM.Core.Node;
            Map   : Str_Map.Map;
            V     : Unbounded_String;
            Max   : Natural := 0;
            Count : Natural := 0;

            ------------
            -- Insert --
            ------------

            procedure Insert (Key, Value : String) is
               Max_Key : constant String := Key & "_MAX";
               Cursor  : Str_Map.Cursor;
               Success : Boolean;
            begin
               Map.Insert (Key, To_Unbounded_String (Value), Cursor, Success);

               if not Success then
                  Error (C, "Duplicate attribute found for n " & Key);
               end if;

               --  Set Max

               Cursor := Data.Find (Max_Key);

               if Str_Map.Has_Element (Cursor) then
                  declare
                     Item : constant Natural :=
                              Natural'Value
                                (To_String (Str_Map.Element (Cursor)));
                  begin
                     Data.Replace_Element
                       (Cursor,
                        To_Unbounded_String
                          (Utils.Image
                             (Natural'Max (Item, Natural'Value (Value)))));
                  end;

               else
                  Data.Insert
                    (Max_Key, To_Unbounded_String (Value), Cursor, Success);
               end if;
            end Insert;

         begin
            --  We need first to set V, value for this entry

            C := First_Child (N);

            declare
               Found : Boolean := False;
            begin
               while C /= null loop
                  declare
                     N_Name : constant String := Local_Name (C);
                  begin
                     if N_Name = "V" then
                        V := To_Unbounded_String
                          (Get_Value (DOM.Core.Nodes.First_Child (C)));
                        Found := True;
                     end if;
                  end;
                  C := Next_Sibling (C);
               end loop;

               if not Found then
                  Error (N, "Entity V not found");
               end if;
            end;

            --  Now check for the indexes

            C := First_Child (N);

            while C /= null loop
               declare
                  N_Name : constant String := Local_Name (C);
               begin
                  if N_Name = "Ind" then
                     Count := Count + 1;

                     declare
                        Atts    : constant DOM.Core.Named_Node_Map
                          := DOM.Core.Nodes.Attributes (C);
                        K       : Natural;
                     begin
                        if Length (Atts) = 1
                          and then Local_Name (Item (Atts, 0)) = "n"
                        then
                           K   := Positive'Value (Node_Value (Item (Atts, 0)));
                           Max := Natural'Max (Max, K);

                           Insert
                             (Node_Value (Item (Atts, 0)),
                              Get_Value (DOM.Core.Nodes.First_Child (C)));

                        else
                           Error (C, "A single attribute named n expected");
                        end if;
                     end;

                  elsif N_Name = "V" then
                     null;

                  else
                     Error (C, "Entity Ind or V expected, found " & N_Name);
                  end if;
               end;
               C := Next_Sibling (C);
            end loop;

            --  Check validity

            if Level = 0 and then Max = Count then
               Level := Max;
            elsif Level /= Max or else Max /= Count then
               Error (N, "This entity has a wrong number of indices");
            end if;

            --  Insert corresponding entry into Data

            declare
               Key     : Unbounded_String;
               Cursor  : Str_Map.Cursor;
               Success : Boolean;
            begin
               for K in 1 .. Level loop
                  Cursor := Map.Find (Utils.Image (K));

                  Append (Key, "_" & To_String (Str_Map.Element (Cursor)));
               end loop;

               Data.Insert (To_String (Key), V, Cursor, Success);

               if not Success then
                  Error (N, "Duplicate entry found");
               end if;
            end;
         end Parse_Entry;

         C      : DOM.Core.Node;
         Result : Translate_Set;

      begin
         --  First we need to look for the Tag entity

         C := First_Child (N);

         while C /= null loop
            declare
               N_Name : constant String := Local_Name (C);
            begin
               if N_Name = "Tag" then
                  Parse_Tag (C, Name, Description);
               end if;
            end;
            C := Next_Sibling (C);
         end loop;

         if Name = Null_Unbounded_String then
            Error (N, "Missing entity Tag");
         end if;

         --  Add tag description

         Insert
           (Result,
            Assoc (To_String (Name) & "_DESCRIPTION",
                   To_String (Description)));

         --  Now handles other nodes

         C := First_Child (N);

         while C /= null loop
            declare
               N_Name : constant String := Local_Name (C);
            begin
               if N_Name = "Tag" then
                  --  Already parsed above
                  null;

               elsif N_Name = "Dim" then
                  Insert (Result, Parse_Dim (C));

               elsif N_Name = "Entry" then
                  Parse_Entry (C);

               else
                  Error
                    (C,
                     "Entity Tag, Dim or Entry expected, found " & N_Name);
               end if;
            end;
            C := Next_Sibling (C);
         end loop;

         --  Now we have all entities in the Data map

         Insert (Result, Assoc (To_String (Name), Build_Tag));

         return Result;
      end Parse_CompositeTag;

      ---------------------
      -- Parse_SimpleTag --
      ---------------------

      function Parse_SimpleTag (N : DOM.Core.Node) return Translate_Set is
         C           : DOM.Core.Node;
         Name        : Unbounded_String;
         Description : Unbounded_String;
         Value       : Unbounded_String;
         Result      : Translate_Set;
      begin
         C := First_Child (N);

         while C /= null loop
            declare
               N_Name : constant String := Local_Name (C);
            begin
               if N_Name = "Tag" then
                  Parse_Tag (C, Name, Description);

               elsif N_Name = "V" then
                  Value := To_Unbounded_String
                    (Get_Value (DOM.Core.Nodes.First_Child (C)));

               else
                  Error (C, "Entity Tag or V expected, found " & N_Name);
               end if;
            end;
            C := Next_Sibling (C);
         end loop;

         Insert (Result, Assoc (To_String (Name), To_String (Value)));

         if Description /= Null_Unbounded_String then
            Insert
              (Result,
               Assoc (To_String (Name) & "_DESCRIPTION",
                      To_String (Description)));
         end if;

         return Result;
      end Parse_SimpleTag;

      ---------------
      -- Parse_Tag --
      ---------------

      procedure Parse_Tag
        (N                 : DOM.Core.Node;
         Name, Description :    out Unbounded_String)
      is
         C : DOM.Core.Node := First_Child (N);
      begin
         while C /= null loop
            declare
               N_Name  : constant String := Local_Name (C);
               N_Value : constant String
                 := Get_Value (DOM.Core.Nodes.First_Child (C));
            begin
               if N_Name = "Name" then
                  Name := To_Unbounded_String (N_Value);

               elsif N_Name = "Description" then
                  Description := To_Unbounded_String (N_Value);

               else
                  Error
                    (N,
                     "Entity Name or Description expected, found " & N_Name);
               end if;
            end;
            C := Next_Sibling (C);
         end loop;
      end Parse_Tag;

      ------------------
      -- Parse_Tagged --
      ------------------

      function Parse_Tagged (N : DOM.Core.Node) return Translate_Set is
         C : DOM.Core.Node;
         T : Translate_Set;
      begin
         C := First_Child (N);

         while C /= null loop
            declare
               Name : constant String := Local_Name (C);
            begin
               if Name = "SimpleTag" then
                  Insert (T, Parse_SimpleTag (C));

               elsif Name = "CompositeTag" then
                  Insert (T, Parse_CompositeTag (C));

               else
                  Error
                    (C, "SimpleTag or CompositeTag expected, found" & Name);
               end if;
            end;
            C := Next_Sibling (C);
         end loop;

         return T;
      end Parse_Tagged;

      NL : constant DOM.Core.Node_List := Child_Nodes (Doc);
   begin
      if Length (NL) = 1 then
         return Parse_Tagged (First_Child (Doc));
      else
         Error (Doc, "Document must have a single node, found "
                & Natural'Image (Length (NL)));
      end if;
   end Parse_Document;

   ----------
   -- Save --
   ----------

   procedure Save (Filename : String; Translations : Translate_Set) is
      File : Text_IO.File_Type;
   begin
      Text_IO.Create (File, Text_IO.Out_File, Filename);
      Text_IO.Put (File, To_String (Image (Translations)));
      Text_IO.Close (File);
   end Save;

   -----------
   -- Value --
   -----------

   function Value (Translations : String) return Translate_Set is
      use DOM.Core.Nodes;
      use DOM.Readers;

      Reader : Tree_Reader;
      Input  : Input_Sources.Strings.String_Input;
      Doc    : DOM.Core.Document;
      Result : Translate_Set;

   begin
      Input_Sources.Strings.Open
        (Translations'Unrestricted_Access,
         Unicode.CES.Utf8.Utf8_Encoding,
         Input);

      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Parse (Reader, Input);

      Input_Sources.Strings.Close (Input);

      Doc := Get_Tree (Reader);

      Result := Parse_Document (Doc);

      Free (Doc);

      return Result;
   end Value;

   function Value (Translations : Unbounded_String) return Translate_Set is
      use DOM.Core.Nodes;
      use DOM.Readers;

      S : String_Access := new String (1 .. Length (Translations));

   begin
      --  Copy XML content to local S string
      for I in 1 .. Length (Translations) loop
         S (I) := Element (Translations, I);
      end loop;

      declare
         Result : constant Translate_Set := Value (S.all);
      begin
         Free (S);
         return Result;
      end;
   end Value;

end Templates_Parser.XML;
