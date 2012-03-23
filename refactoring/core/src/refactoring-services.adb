------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Refactoring.Buffer_Helpers; use Refactoring.Buffer_Helpers;

with Ada_Semantic_Tree.Parts; use Ada_Semantic_Tree.Parts;
with Basic_Types;             use Basic_Types;
with Entities;                use Entities;
with Entities.Queries;        use Entities.Queries;
with GNAT.Strings;            use GNAT.Strings;
with GPS.Editors;             use GPS.Editors;
with Language;                use Language;
with Language.Ada;            use Language.Ada;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with String_Utils;            use String_Utils;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.Traces;         use GNATCOLL.Traces;

package body Refactoring.Services is
   Me : constant Trace_Handle := Create ("Refactoring");

   procedure Skip_Keyword
     (Str : String; Index : in out Integer; Word : String);
   --  Skip the next word in Str if it is Word

   procedure Remove_Blanks
     (From : in out Editor_Location'Class; Direction : Integer := 1);
   --  Remove all blanks from From onwards (spaces, tabs and newlines)

   function Skip_Comments
     (From : Editor_Location'Class;
      Direction : Integer := 1) return Editor_Location'Class;
   --  Skip any following or preceding comment lines (depending on Direction).
   --  If there are no comments immediately before or after, From is returned.
   --
   --  ??? The implementation is specific to Ada comments for now

   function Default_Insertion_Line (Tree : Construct_Tree) return Integer;
   --  A line where we could insert code (preferably before the "end pkg" line,
   --  or if there is no package default to the first line in the file

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ada_Statement) is
   begin
      null;
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Ada_Statement;
      Context  : Factory_Context;
      Location : Universal_Location)
   is
      use Tokens_List;

      Loc : aliased Universal_Location := Location;

      Start_Index : Integer := Integer (Get_Index_In_File (Loc'Access));
      End_Index   : Integer := Start_Index;
      Buffer      : constant GNAT.Strings.String_Access :=
                      Get_Buffer (Get_File (Loc'Access));
      Paren_Depth : Integer := 0;

      procedure Backwards_Callback
        (Token : Token_Record;
         Stop : in out Boolean);

      function Forwards_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      -----------------------
      -- Backwards_Callback --
      -----------------------

      procedure Backwards_Callback
        (Token : Token_Record;
         Stop : in out Boolean) is
      begin
         Stop := False;

         Trace (Me, "[" & Buffer
                           (Integer (Token.Token_First)
                            .. Integer (Token.Token_Last)) & "]");

         case Token.Tok_Type is
            when Tok_Semicolon | Tok_Arrow =>
               Stop := True;
               return;

               --  ??? This does not take into account cases where we have
               --  P (A => 0);
               --  and the cursor is placed after the arrow.

            when Tok_Declare
               | Tok_Begin
               | Tok_Is
               | Tok_Then
               | Tok_Loop
               | Tok_Record
               | Tok_Else
               | Tok_Do
               | Tok_Select
               | Tok_Private
               | Tok_Exception =>

               --  ??? This does not take into account "or else" and
               --  "and then" operators. Cursor place after won't come
               --  back enough.
               --  "is private; may not be correctly analyzed either.

               Stop := True;
               return;

            when others =>
               null;
         end case;

         if Token.Tok_Type /= Tok_Blank then
            Start_Index := Integer (Token.Token_First);
         end if;
      end Backwards_Callback;

      -----------------------
      -- Forwards_Callback --
      -----------------------

      function Forwards_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
         Name : constant String :=
           To_Lower (Buffer (Sloc_Start.Index .. Sloc_End.Index));

         procedure Add_To_List;

         -----------------
         -- Add_To_List --
         -----------------

         procedure Add_To_List is
         begin
            if Entity in Identifier_Entity then
               if Self.Sloc_First_Id = Null_Universal_Location then
                  Self.Sloc_First_Id := To_Location
                    (Get_File (Loc'Access),
                     String_Index_Type (Sloc_Start.Index));
               end if;

               Self.Tokens.Append
                 (Token_Record'
                    (Tok_Type    => Tok_Identifier,
                     Token_First =>
                       String_Index_Type (Sloc_Start.Index),
                     Token_Last  =>
                       String_Index_Type (Sloc_End.Index)));

               Self.Number_Of_Elements := Self.Number_Of_Elements + 1;
            elsif Name = "," then
               Self.Tokens.Append
                 (Token_Record'
                    (Tok_Type    => Tok_Comma,
                     Token_First =>
                       String_Index_Type (Sloc_Start.Index),
                     Token_Last  =>
                       String_Index_Type (Sloc_End.Index)));
            end if;
         end Add_To_List;

      begin
         End_Index := Sloc_End.Index;

         if Self.Kind = Unknown_Kind then
            if Name = "pragma" then
               Self.Kind := Pragma_Kind;
            elsif Name = "with" then
               Self.Kind := With_Kind;
            elsif Name = "use" then
               Self.Kind := Use_Kind;
            elsif Name = "type" then
               Self.Kind := Type_Kind;
            elsif Name = "when" then
               Self.Kind := When_Kind;
            elsif Entity = Identifier_Text then
               Self.Kind := Variable_Kind;
            end if;
         elsif Self.Kind = Use_Kind then
            if Name = "type" then
               Self.Kind := Use_Type_Kind;
            end if;
         end if;

         if Name = "(" then
            Paren_Depth := Paren_Depth + 1;
         elsif Name = ")" then
            Paren_Depth := Paren_Depth - 1;
         elsif Name = ";" or else (Name = "=>" and then Paren_Depth = 0) then
            return True;
         elsif Name = ":" then
            Self.Sloc_Column :=
              To_Location
                (Get_File (Loc'Access), String_Index_Type (Sloc_Start.Index));
         else
            case Self.Kind is
               when Pragma_Kind | Type_Kind =>
                  if Paren_Depth = 1 then
                     Add_To_List;
                  end if;

               when Clause_Kind =>
                  Add_To_List;

               when Unknown_Kind | Variable_Kind | When_Kind =>
                  --  we're on a declaration, like a, b : Integer;

                  if Self.Sloc_Column = Null_Universal_Location then
                     Add_To_List;
                  end if;
            end case;
         end if;

         return False;
      end Forwards_Callback;

   begin

      --  First, get to the beginning of the statement

      Ada_Lang.Parse_Tokens_Backwards
        (Buffer       => Buffer.all,
         Start_Offset => String_Index_Type (Start_Index),
         End_Offset   => 0,
         Callback     => Backwards_Callback'Access);

      --  Then, parse the statement until the end

      Paren_Depth := 0;

      Ada_Lang.Parse_Entities
        (Buffer   => Buffer.all (Start_Index .. Buffer.all'Last),
         Callback => Forwards_Callback'Unrestricted_Access);

      Self.Sloc_Start := To_Location
        (Get_File (Loc'Access), String_Index_Type (Start_Index));
      Self.Sloc_End := To_Location
        (Get_File (Loc'Access), String_Index_Type (End_Index));
      Self.Context := Context;
   end Initialize;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Self : Ada_Statement) return Statement_Kind is
   begin
      return Self.Kind;
   end Get_Kind;

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : in out Ada_Statement) is
      Start_To_Remove : aliased Universal_Location;
      Stop_To_Remove  : aliased Universal_Location;
   begin
      if Self.Kind = When_Kind then
         Start_To_Remove := Self.Sloc_First_Id;
         Stop_To_Remove := Self.Sloc_Column;
      else
         Start_To_Remove := Self.Sloc_Start;
         Stop_To_Remove := Self.Sloc_End;
      end if;

      declare
         Line           : constant String :=
                            Get_Line (Self.Context, Stop_To_Remove'Access, 1);
         Last_To_Remove : String_Index_Type;
      begin
         Last_To_Remove := Get_Index_In_Line (Stop_To_Remove'Access);

         for J in Integer (Get_Index_In_Line (Stop_To_Remove'Access)) + 1
           .. Line'Last
         loop
            exit when Line (J) /= ASCII.HT and then Line (J) /= ' ';

            Last_To_Remove := String_Index_Type (J);
         end loop;

         Set_Index_In_Line (Stop_To_Remove'Access, Last_To_Remove);

         Remove_Code
           (Self.Context, Start_To_Remove'Access, Stop_To_Remove'Access);
      end;
   end Remove;

   -------------
   -- Comment --
   -------------

   procedure Comment (Self : in out Ada_Statement) is
   begin
      if Self.Kind = When_Kind then
         Comment_Code
           (Self.Context, Self.Sloc_First_Id'Access, Self.Sloc_Column'Access);
      else
         Comment_Code
           (Self.Context, Self.Sloc_Start'Access, Self.Sloc_End'Access);
      end if;
   end Comment;

   ---------------
   -- Get_Start --
   ---------------

   function Get_Start (Self : Ada_Statement) return Universal_Location is
   begin
      return Self.Sloc_Start;
   end Get_Start;

   -------------
   -- Get_End --
   -------------

   function Get_End (Self : Ada_Statement) return Universal_Location is
   begin
      return Self.Sloc_End;
   end Get_End;

   ----------------------
   -- Contains_Element --
   ----------------------

   function Contains_Element
     (Self : Ada_Statement;
      Name : Language.Tree.Normalized_Symbol) return Boolean
   is
      use Tokens_List;

      Loc_Copy : aliased Universal_Location := Self.Sloc_Start;
      Buffer   : constant GNAT.Strings.String_Access :=
                   Get_Buffer (Get_File (Loc_Copy'Access));
      Cur      : Tokens_List.Cursor;
   begin
      Cur := First (Self.Tokens);

      while Cur /= Tokens_List.No_Element loop
         declare
            Token        : constant Token_Record := Element (Cur);
            Name_In_List : constant String := Buffer
              (Integer (Token.Token_First)
               .. Integer (Token.Token_Last));
         begin
            if Token.Tok_Type = Tok_Identifier then
               if Name = Find_Normalized
                 (Symbols => Get_Symbols (Self.Context.Entity_Db),
                  Name    => Name_In_List)
               then
                  return True;
               end if;
            end if;
         end;

         Cur := Next (Cur);
      end loop;

      return False;
   end Contains_Element;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Element
     (Self  : in out Ada_Statement;
      Mode  : Remove_Code_Mode;
      Name  : Language.Tree.Normalized_Symbol)
   is
      Str : GNAT.Strings.String_Access;
   begin
      Extract_Element (Self, Str, Name, Mode);
      Free (Str);
   end Remove_Element;

   ----------------------
   -- Extract_Elements --
   ----------------------

   procedure Extract_Element
     (Self      : in out Ada_Statement;
      Extracted : out GNAT.Strings.String_Access;
      Name      : Language.Tree.Normalized_Symbol;
      Mode      : Remove_Code_Mode := Erase)
   is
      use Tokens_List;

      Cur    : Tokens_List.Cursor;
      Buffer : constant GNAT.Strings.String_Access :=
                 Get_Buffer (Get_File (Self.Sloc_Start'Access));

      First_To_Delete, Last_To_Delete : String_Index_Type := 0;
      Token_Deleted : Token_Record;

      Last_Comma : Token_Record := Null_Token;
   begin
      Cur := First (Self.Tokens);

      while Cur /= Tokens_List.No_Element loop
         declare
            Token        : constant Token_Record := Element (Cur);
            Name_In_List : constant String := Buffer
              (Integer (Token.Token_First)
               .. Integer (Token.Token_Last));
         begin
            if Token.Tok_Type = Tok_Identifier then
               if Name = Find_Normalized
                 (Symbols => Get_Symbols (Self.Context.Entity_Db),
                  Name    => Name_In_List)
               then
                  First_To_Delete := Token.Token_First;
                  Last_To_Delete := Token.Token_Last;
                  Token_Deleted := Token;

                  if Last_Comma /= Null_Token then
                     First_To_Delete := Last_Comma.Token_First;

                     exit;
                  end if;
               end if;
            elsif Token.Tok_Type = Tok_Comma then
               Last_Comma := Token;

               if First_To_Delete /= 0 then
                  Last_To_Delete := Last_Comma.Token_First;

                  while Last_To_Delete < String_Index_Type (Buffer'Last)
                    and then
                      (Buffer (Integer (Last_To_Delete) + 1) = ASCII.HT
                       or else Buffer (Integer (Last_To_Delete) + 1) = ' ')
                  loop
                     Last_To_Delete := Last_To_Delete + 1;
                  end loop;

                  exit;
               end if;
            end if;
         end;

         Cur := Next (Cur);
      end loop;

      if First_To_Delete /= 0 then
         if Self.Number_Of_Elements = 1 then
            Extracted :=
              new String'
                (Get
                     (Self.Context,
                      Self.Sloc_Start'Access,
                      Self.Sloc_End'Access));

            case Mode is
               when Erase =>
                  Remove (Self);
               when Comment =>
                  Comment (Self);
            end case;
         else
            declare
               End_Location : constant GPS.Editors.Editor_Location'Class :=
                 To_Location (Self.Context, Self.Sloc_End'Access);
               End_Mark     : constant GPS.Editors.Editor_Mark'Class :=
                 End_Location.Create_Mark;

               First, Last : aliased Universal_Location;
               Token_First, Token_Last : aliased Universal_Location;
            begin
               First := To_Location
                 (Get_File (Self.Sloc_Start'Access), First_To_Delete);
               Trace (Me, "LAST TO DELETE = " & Last_To_Delete'Img);
               Last := To_Location
                 (Get_File (Self.Sloc_Start'Access), Last_To_Delete);
               Trace (Me, "LAST COL = " & Get_Column (Last'Access)'Img);

               Token_First := To_Location
                 (Get_File
                    (Self.Sloc_Start'Access), Token_Deleted.Token_First);
               Token_Last := To_Location
                 (Get_File
                    (Self.Sloc_Start'Access), Token_Deleted.Token_Last);

               if Self.Sloc_Column = Null_Universal_Location then
                  Extracted :=
                    new String'
                      (Get
                           (Self.Context,
                            Token_First'Access,
                            Token_Last'Access));
               else
                  Extracted :=
                    new String'
                      (Get
                           (Self.Context,
                            Token_First'Access,
                            Token_Last'Access)
                       & " "
                       & Get
                         (Self.Context,
                          Self.Sloc_Column'Access,
                          Self.Sloc_End'Access));
               end if;

               case Mode is
                  when Erase =>
                     Remove_Code
                       (Context => Self.Context,
                        Start   => First'Access,
                        Stop    => Last'Access);
                  when Comment =>
                     Comment_Code
                       (Context => Self.Context,
                        Start   => First'Access,
                        Stop    => Last'Access);
               end case;

               Self.Sloc_End := To_Location (Self.Context, End_Mark.Location);
            end;
         end if;
      end if;
   end Extract_Element;

   ----------------------------
   -- Number_Of_Declarations --
   ----------------------------

   function Number_Of_Declarations (Self : Ada_Statement) return Integer is
   begin
      return Self.Number_Of_Elements;
   end Number_Of_Declarations;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Context : not null access Factory_Context_Record'Class;
      Entity  : Entities.Entity_Information) return Entity_Declaration
   is
      Equal : Integer;
      EA    : Entity_Access;

   begin
      if Get_Kind (Entity).Is_Type then
         return No_Entity_Declaration;
      end if;

      EA := Get_Entity_Access (Context, Entity);
      if EA = Null_Entity_Access then
         return No_Entity_Declaration;
      end if;

      declare
         Buffer : constant GNAT.Strings.String_Access :=
           Get_Buffer (Get_File (EA));
         Last   : constant Integer := Get_Construct (EA).Sloc_End.Index;
         First  : constant Integer := Get_Construct (EA).Sloc_Entity.Index;
         Index : Natural := First;
         Shared : Boolean := False;
         Iter   : Construct_Tree_Iterator;
         Tree   : Construct_Tree;
      begin
         Skip_To_Char (Buffer (Index .. Last), Index, ':');
         Equal := Index;
         Skip_To_String (Buffer (Index .. Last), Equal, ":=");

         --  unbounded strings are always indexed at 1, so normalize
         Equal := Equal - Index + 1;

         Tree := Get_Tree (Get_File (EA));
         Iter := To_Construct_Tree_Iterator (EA);
         Shared := Get_Construct (Prev (Tree, Iter)).Sloc_End.Index = Last
           or else Get_Construct (Next (Tree, Iter)).Sloc_End.Index = Last;

         return (File      => Get_File (EA),
                 Entity    => Entity,
                 First     => null,
                 Last      => null,
                 SFirst    => Get_Construct (EA).Sloc_Entity,
                 SLast     => Get_Construct (EA).Sloc_End,
                 Equal_Loc => Equal,
                 Shared    => Shared,
                 Decl      => To_Unbounded_String (Buffer (Index .. Last)));
      end;
   end Get_Declaration;

   ------------------
   -- Create_Marks --
   ------------------

   procedure Create_Marks
     (Self   : in out Entity_Declaration;
      Buffer : Editor_Buffer'Class)
   is
      First_Line, Last_Line     : Integer;
      First_Column, Last_Column : Visible_Column_Type;
   begin
      if Self.First = null and then Self.Entity /= null then
         To_Line_Column
           (Self.File,
            String_Index_Type (Self.SFirst.Index),
            First_Line,
            First_Column);

         To_Line_Column
           (Self.File,
            String_Index_Type (Self.SLast.Index),
            Last_Line,
            Last_Column);

         declare
            Start  : constant Editor_Location'Class := Buffer.New_Location
              (Line   => First_Line,
               Column => First_Column);
         begin
            Self.First := new Editor_Mark'Class'(Start.Create_Mark);
            Self.Last  := new Editor_Mark'Class'
              (Buffer.New_Location
                 (Line   => Last_Line,
                  Column => Last_Column).Create_Mark);
         end;
      end if;
   end Create_Marks;

   -------------------
   -- Initial_Value --
   -------------------

   function Initial_Value (Self : Entity_Declaration) return String is
   begin
      --  These cannot have an initial value, so we save time
      if Self = No_Entity_Declaration
        or else Is_Container (Get_Kind (Self.Entity).Kind)
      then
         return "";
      end if;

      declare
         Text  : constant String := To_String (Self.Decl);
         Index : Natural;
      begin

         if Text = "" then
            return "";
         end if;

         Index := Text'First + 1;  --  skip ':'

         while Index < Text'Last loop
            if Text (Index .. Index + 1) = ":=" then
               Index := Index + 2;
               Skip_Blanks (Text, Index);

               if Active (Me) then
                  Trace (Me, "  Initial value of "
                         & Debug_Name (Self.Entity) & " is "
                         & Text (Index .. Text'Last - 1));
               end if;
               return Text (Index .. Text'Last - 1);

            elsif Text (Index) = ';'
              or else Text (Index) = ')'
            then
               exit;
            end if;

            Index := Index + 1;
         end loop;
      end;

      return "";
   end Initial_Value;

   ------------------
   -- Skip_Keyword --
   ------------------

   procedure Skip_Keyword
     (Str : String; Index : in out Integer; Word : String) is
   begin
      if Index + Word'Length <= Str'Last
        and then Looking_At (Str, Index, Word)
        and then Is_Blank (Str (Index + Word'Length))
      then
         Index := Index + Word'Length;
         Skip_Blanks (Str, Index);
      end if;
   end Skip_Keyword;

   --------------------------
   -- Display_As_Parameter --
   --------------------------

   function Display_As_Parameter
     (Self    : Entity_Declaration;
      Context : not null access Factory_Context_Record'Class;
      PType   : Entities.Queries.Parameter_Type) return String
   is
      Result : Unbounded_String;
      Decl   : constant String := To_String (Self.Decl);
      Index  : Natural := Decl'First;
      Last   : Integer := Self.Equal_Loc - 1;
   begin
      Append (Result, Get (Get_Name (Self.Entity), True).all & " : ");

      case PType is
         when Out_Parameter =>
            Append (Result, "out ");
         when In_Out_Parameter =>
            Append (Result, "in out ");
         when Access_Parameter =>
            Append (Result, "access ");
         when In_Parameter =>
            if Context.Add_In_Keyword then
               Append (Result, "in ");
            end if;
      end case;

      --  Skip ":" in the declaration (always the first character)

      Index := Index + 1;
      Skip_Blanks (Decl, Index);

      --  Skip any keyword coming from the original declaration
      --  Keep "access" since this is part of the type

      Skip_Keyword (Decl, Index, "constant");
      Skip_Keyword (Decl, Index, "in");
      Skip_Keyword (Decl, Index, "out");

      if PType = Access_Parameter then
         Skip_Keyword (Decl, Index, "access");
      end if;

      if Last <= 0 then
         Last := Decl'Last;
      end if;

      Skip_Blanks_Backward (Decl, Last);

      Append (Result, Decl (Index .. Last));

      return To_String (Result);
   end Display_As_Parameter;

   -------------------------
   -- Display_As_Variable --
   -------------------------

   function Display_As_Variable
     (Self  : Entity_Declaration) return String is
   begin
      return Get (Get_Name (Self.Entity)).all & " "
        & To_String (Self.Decl);
   end Display_As_Variable;

   -----------------------
   -- Get_Entity_Access --
   -----------------------

   function Get_Entity_Access
     (Context : not null access Factory_Context_Record'Class;
      Entity  : Entities.Entity_Information)
      return Language.Tree.Database.Entity_Access
   is
      EDecl  : File_Location;
      Struct : Structured_File_Access;
   begin
      if Entity /= null then
         EDecl  := Get_Declaration_Of (Entity);

         Struct := Get_Or_Create
           (Db   => Context.Construct_Db,
            File => Get_Filename (EDecl.File));
         Update_Contents (Struct);

         if Struct /= null then
            return Find_Declaration
              (Get_Tree_Language (Struct),
               Struct,
               Line   => EDecl.Line,
               Column => To_Line_String_Index
                 (File   => Struct,
                  Line   => EDecl.Line,
                  Column => EDecl.Column));
         end if;
      end if;

      return Null_Entity_Access;
   end Get_Entity_Access;

   -------------------
   -- Remove_Blanks --
   -------------------

   procedure Remove_Blanks
     (From : in out Editor_Location'Class; Direction : Integer := 1)
   is
      EoB : constant Editor_Location'Class := From.Buffer.End_Of_Buffer;
   begin
      while From /= EoB loop
         case From.Get_Char is
            when Character'Pos (' ')
               | Character'Pos (ASCII.HT)
               | Character'Pos (ASCII.LF) =>
               From.Buffer.Delete (From, From);

            when others =>
               exit;
         end case;

         if Direction < 0 then
            From := From.Forward_Char (-1);
            exit when From.Offset = 0;
         end if;
      end loop;
   end Remove_Blanks;

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : Entity_Declaration) is
   begin
      if Self.First = null then
         return;
      end if;

      declare
         From : Editor_Location'Class := Self.First.Location;
         To   : constant Editor_Location'Class := Self.Last.Location;

      begin
         --  ??? Should take into account the case where there are several
         --  declarations grouped as in "A, B : Integer", and we only want to
         --  remove one of them. The one we want to remove starts exactly at
         --  From but there might be others afterward.

         if Self.Shared then
            declare
               TM : constant Editor_Mark'Class := To.Create_Mark;
            begin
               --  Only remove the name of the entity and the preceding or
               --  leading comma

               From.Buffer.Delete
                 (From, From.Forward_Char
                    (Get (Get_Name (Self.Entity))'Length - 1));
               Remove_Blanks (From);

               if From.Get_Char = Character'Pos (',') then
                  From.Buffer.Delete (From, From);
                  Remove_Blanks (From);

               elsif From.Get_Char = Character'Pos (':') then
                  From := From.Forward_Char (-1);
                  --  Remove backward instead
                  Remove_Blanks (From, -1);

                  if From.Get_Char = Character'Pos (',') then
                     From.Buffer.Delete (From, From);
                     From := From.Forward_Char (-1);
                     Remove_Blanks (From, -1);

                     --  We have now removed " *, *", so we should put back one
                     --  space

                     From.Buffer.Insert (From.Forward_Char (1), " ");

                  else
                     --  After all the declaration is not shared (perhaps we
                     --  removed all the variables on the line already)

                     From.Buffer.Delete (From.Forward_Char (1), TM.Location);
                  end if;
               end if;

               TM.Delete;
            end;

         else
            From.Buffer.Delete (From, To);

            --  Are we leaving an empty line ? If yes, remove it too

            declare
               Bol : constant Editor_Location'Class := From.Beginning_Of_Line;
            begin
               --  From points to the character that was just after the initial
               --  decl, ie the newline. We'll need to move backward

               if Trim (From.Buffer.Get_Chars (Bol, From), Both) =
                 "" & ASCII.LF
               then
                  From.Buffer.Delete (Bol, From);
               end if;
            end;
         end if;
      end;
   end Remove;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Entity_Declaration) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Editor_Mark'Class, Editor_Mark_Access);
   begin
      if Self.First /= null then
         Self.First.Delete;
         Unchecked_Free (Self.First);
         Self.Last.Delete;
         Unchecked_Free (Self.Last);
      end if;
   end Free;

   ---------------------------
   -- Accepts_Primitive_Ops --
   ---------------------------

   function Accepts_Primitive_Ops
     (Context        : not null access Factory_Context_Record'Class;
      Entity         : Entity_Information;
      Current_Offset : String_Index_Type) return Boolean
   is
      EA  : Entity_Access := Get_Entity_Access (Context, Get_Type_Of (Entity));
   begin
      if EA /= Null_Entity_Access
        and then Get_Kind (Entity).Kind /= Class_Wide
      then
         EA := Get_Last_Visible_Declaration
           (EA, Get_File (EA), Offset => Current_Offset);

         return Get_Construct (EA).Attributes (Ada_Tagged_Attribute)
           or else Get_Construct (EA).Attributes (Ada_Interface_Attribute);
      else
         return False;
      end if;
   end Accepts_Primitive_Ops;

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   procedure Is_Parameter_Of
     (Entity       : Entity_Information;
      Is_Parameter : out Boolean;
      PType        : out Parameter_Type)
   is
      Sub   : constant Entity_Information := Is_Parameter_Of (Entity);
      Iter  : Subprogram_Iterator;
      Param : Entity_Information;
   begin
      if Sub /= null then
         Iter := Get_Subprogram_Parameters (Sub);
         loop
            Get (Iter, Param);
            exit when Param = null;

            if Param = Entity then
               Is_Parameter := True;
               PType        := Get_Type (Iter);
               return;
            end if;

            Next (Iter);
         end loop;
      end if;

      Is_Parameter := False;
   end Is_Parameter_Of;

   ------------------
   -- Create_Range --
   ------------------

   function Create_Range
     (Context     : not null access Factory_Context_Record'Class;
      File        : GNATCOLL.VFS.Virtual_File;
      From_Line   : Integer;
      To_Line     : Integer) return Range_Of_Code
   is
   begin
      return Range_Of_Code'
        (Context   => Factory_Context (Context),
         File      => File,
         From_Line => From_Line,
         To_Line   => To_Line,
         Source    => Get_Or_Create (Context.Entity_Db, File),
         Parent    => null);
   end Create_Range;

   ----------------
   -- Get_Parent --
   ----------------

   procedure Get_Parent
     (Self   : in out Range_Of_Code;
      Parent : out Entities.Entity_Information)
   is
      Scopes : constant Lines_To_Scope := Compute_Scopes (Self.Source);
   begin
      Self.Parent := Get_Scope (Scopes, Self.From_Line);

      if Active (Me) and then Self.Parent /= null then
         Trace (Me, "Range.Parent=" & Debug_Name (Self.Parent));
      end if;

      if Get_Scope (Scopes, Self.To_Line) /= Self.Parent then
         Self.Context.Report_Error
           ("The selected code does not belong to a single subprogram");
         Self.Parent := null;
      end if;

      Parent := Self.Parent;
   end Get_Parent;

   -------------------------------
   -- For_All_Variable_In_Range --
   -------------------------------

   procedure For_All_Variable_In_Range
     (Self        : in out Range_Of_Code;
      Callback    : not null access procedure
        (Entity : Entities.Entity_Information;
         Flags  : Entity_References_Flags);
      Success            : out Boolean;
      Omit_Library_Level : Boolean := False)
   is
      Ref_Iter                 : Entity_Reference_Iterator;
      Iter                     : Entity_Iterator;
      Caller                   : Entity_Information;
      Parent                   : Entity_Information;
      Ref                      : Entity_Reference;
      Decl, Location, Body_Loc : File_Location;
      Entity                   : Entity_Information;
      Flags                    : Entity_References_Flags;
      Is_Global                : Boolean;
      Is_Param                 : Boolean;
      PType                    : Parameter_Type;
      Struct                   : Structured_File_Access;
      ERef                     : Entity_Reference_Details;

   begin
      Success := True;

      --  This code might require loading a file, so we only freeze afterward
      Find_All_Entities_In_File (Iter, Self.Source);
      Freeze (Self.Context.Entity_Db, Mode => No_Create_Or_Update);

      Self.Get_Parent (Parent);
      if Parent = null then
         Success := False;
         Thaw (Self.Context.Entity_Db);
         return;
      end if;

      Struct := Get_Or_Create (Self.Context.Construct_Db, File => Self.File);
      Update_Contents (Struct);

      while not At_End (Iter) loop
         Entity := Get (Iter);
         Flags  := (others => False);

         Decl := Get_Declaration_Of (Entity);
         Find_Next_Body (Entity   => Entity, Location => Body_Loc);

         --  An entity is "global" (ie does not need an entry in the parameter
         --  list) if it is defined in another file, or in the current file at
         --  library level. We however need to accept in case the entity is
         --  declared in the '.ads' but we are working in the '.adb' (for
         --  instance the parameter to a subprogram).

         Is_Global := Omit_Library_Level
           and then (Get_LI (Decl.File) /= Get_LI (Self.Source)
                     or else not Is_Subprogram
                       (Get_Caller (Declaration_As_Reference (Entity))));

         if Active (Me) then
            Trace (Me, "Entity=" & Debug_Name (Entity)
                   & " Is_Global=" & Is_Global'Img);
         end if;

         if not Is_Global then
            Find_All_References (Ref_Iter, Entity, In_File => Self.Source);

            For_Each_Ref :
            while not At_End (Ref_Iter) loop
               Ref      := Get (Ref_Iter);
               Location := Get_Location (Ref);
               Caller   := Get_Caller (Ref);

               if Parent /= null and then Caller /= Parent then
                  --  A reference outside of the current subprogram
                  Flags (Flag_Ref_Outside_Parent) := True;

                  --  No interest in further references, since they can't be
                  --  in the extracted code and we already know the entity
                  --  is ref outside of that code

                  exit For_Each_Ref when Location.Line > Self.To_Line;

               else
                  --  A reference within the current subprogram

                  if Location.Line > Self.To_Line then
                     Flags (Flag_Read_After) := True;

                  elsif Location.Line < Self.From_Line then
                     if Location.Line /= Decl.Line
                       and then Location.Line /= Body_Loc.Line
                     then
                        if Is_Write_Reference (Get_Kind (Ref)) then
                           Flags (Flag_Modified_Before) := True;
                        elsif Is_Read_Reference (Get_Kind (Ref)) then
                           Flags (Flag_Read_Before) := True;
                        end if;

                     else
                        Is_Parameter_Of (Entity, Is_Param, PType);
                        if Is_Param then
                           case PType is
                              when Out_Parameter =>
                                 --  the entity is needed outside of the
                                 --  extracted code (both to read its value and
                                 --  set it for the caller)
                                 Flags (Flag_Modified_After) := True;
                                 Flags (Flag_Read_After) := True;

                              when others =>
                                 --  An initial value might be passed through
                                 --  the parameter
                                 Flags (Flag_Modified_Before) := True;
                           end case;

                        else
                           declare
                              Entity_Decl : constant Entity_Declaration :=
                                Get_Declaration (Self.Context, Entity);
                           begin
                              if Entity_Decl.Initial_Value /= "" then
                                 Flags (Flag_Modified_Before) := True;
                              end if;
                           end;
                        end if;
                     end if;

                  else
                     --  A reference within the extracted code

                     if Location.Line = Decl.Line then
                        --  If this is the declaration, it means we have a
                        --  "declare" block, and as such we can simply ignore
                        --  this entity, it will be automatically extracted.
                        --
                        --  ??? Of course, the user could also have selected
                        --  the local vars in the original subprogram, or only
                        --  part of a declare block, in which case we should
                        --  really display an error

                        exit For_Each_Ref;
                     end if;

                     --  Should ignore the reference if it is a named
                     --  parameter in a subprogram call, as in
                     --  "Foo (Title => ...)"

                     ERef := Find_Reference_Details
                       (Get_Tree_Language (Struct),
                        Struct,
                        To_String_Index
                          (Struct, Location.Line, Location.Column));

                     if not ERef.Is_Named_Parameter then
                        --  If we are calling a subprogram nested within the
                        --  parent, we can't extract the code.
                        --  ??? We could if we extract to another nested
                        --  subprogram.
                        --  ??? We also could if the only reference to that
                        --  nested is within the extracted code, in which
                        --  case we should extract the subprogram too

                        if Is_Subprogram (Entity) then
                           Caller := Get_Caller
                             (Declaration_As_Reference (Entity));

                           --  ??? We should test if it is nested within
                           --  Context.Parent, when that is set
                           if Caller /= null
                             and then Is_Subprogram (Caller)
                           then
                              Self.Context.Report_Error
                                ("A call to the nested subprogram "
                                 & Get (Get_Name (Entity)).all
                                 & " prevents the refactoring");
                              Success := False;
                              Thaw (Self.Context.Entity_Db);
                              return;
                           end if;
                        end if;

                        if Is_Write_Reference (Get_Kind (Ref)) then
                           Flags (Flag_Modified) := True;
                        elsif Is_Read_Reference (Get_Kind (Ref)) then
                           Flags (Flag_Read) := True;
                        end if;
                     end if;
                  end if;
               end if;

               Next (Ref_Iter);
            end loop For_Each_Ref;

            Destroy (Ref_Iter);

            if Flags (Flag_Modified) or else Flags (Flag_Read) then
               if Active (Me) then
                  Trace (Me, "Extract references """
                         & Debug_Name (Entity)
                         & """ r=" & Flags (Flag_Read)'Img
                         & " w=" & Flags (Flag_Modified)'Img
                         & " before/r=" & Flags (Flag_Read_Before)'Img
                         & " w=" & Flags (Flag_Modified_Before)'Img
                         & " after/r=" & Flags (Flag_Read_After)'Img
                         & " w=" & Flags (Flag_Modified_After)'Img
                         & " outside=" & Flags (Flag_Ref_Outside_Parent)'Img);
               end if;

               Callback (Entity => Entity, Flags => Flags);
            end if;
         end if;

         Next (Iter);
      end loop;

      Thaw (Self.Context.Entity_Db);
      Success := True;

   exception
      when E : others =>
         Trace (Me, E);
         Thaw (Self.Context.Entity_Db);
         raise;
   end For_All_Variable_In_Range;

   ----------
   -- File --
   ----------

   function File (Self : Range_Of_Code) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end File;

   ---------------
   -- From_Line --
   ---------------

   function From_Line (Self : Range_Of_Code) return Integer is
   begin
      return Self.From_Line;
   end From_Line;

   -------------
   -- To_Line --
   -------------

   function To_Line (Self : Range_Of_Code) return Integer is
   begin
      return Self.To_Line;
   end To_Line;

   -------------------
   -- Skip_Comments --
   -------------------

   function Skip_Comments
     (From      : Editor_Location'Class;
      Direction : Integer := 1) return Editor_Location'Class
   is
      pragma Unreferenced (Direction);
      Loc          : Editor_Location'Class := From;
      Seen_Comment : Boolean := False;
   begin
      loop
         --  Skip backward until we find a non blank line that is not a comment

         declare
            Loc2  : constant Editor_Location'Class := Loc.Forward_Line (-1);
            C     : constant String :=
                      Loc.Buffer.Get_Chars (Loc2, Loc2.End_Of_Line);
            Index : Natural := C'First;
         begin
            exit when Loc2 = Loc;  --  Beginning of buffer

            Skip_Blanks (C, Index);

            if Index > C'Last then
               null;   --  blank line

            elsif Index < C'Last
              and then C (Index .. Index + 1) = "--"
            then
               Seen_Comment := True;  --  comment line

            else
               exit;
            end if;

            Loc := Loc2;
         end;
      end loop;

      if Seen_Comment then
         return Loc;  --  return the next line
      else
         return From;
      end if;
   end Skip_Comments;

   -----------------
   -- Insert_Text --
   -----------------

   function Insert_Text
     (Context                   : not null access Factory_Context_Record'Class;
      In_File                   : GNATCOLL.VFS.Virtual_File;
      Line                      : Integer;
      Column                    : Visible_Column_Type := 1;
      Text                      : String;
      Indent                    : Boolean;
      Skip_Comments_Backward    : Boolean := False;
      Surround_With_Blank_Lines : Boolean := False;
      Replaced_Length           : Integer := 0;
      Only_If_Replacing         : String := "") return Boolean
   is
      Editor    : constant Editor_Buffer'Class :=
                    Context.Buffer_Factory.Get (In_File);
      Loc_Start : Editor_Location'Class :=
                    Editor.New_Location (Line, Column);
      Loc_End   : constant Editor_Location'Class :=
                    Loc_Start.Forward_Char (Replaced_Length - 1);
   begin
      if Replaced_Length /= 0 and then Only_If_Replacing /= "" then
         declare
            Replacing_Str : constant String := To_Lower (Only_If_Replacing);
            Str           : constant String :=
                              To_Lower (Editor.Get_Chars (Loc_Start, Loc_End));
         begin
            if Str /= Replacing_Str then
               return False;
            end if;
         end;
      end if;

      Editor.Start_Undo_Group;

      if Replaced_Length > 0 then
         Editor.Delete (Loc_Start, Loc_End);
      end if;

      if Text = "" then
         Editor.Finish_Undo_Group;
         return True;
      end if;

      if Skip_Comments_Backward then
         Loc_Start := Skip_Comments (Loc_Start, Direction => -1);
      end if;

      --  Insert the trailing space if needed
      if Surround_With_Blank_Lines
        and then
          (Text'Length < 2
           or else Text (Text'Last - 1 .. Text'Last) /= ASCII.LF & ASCII.LF)
      then
         declare
            L : constant Editor_Location'Class := Loc_Start.Beginning_Of_Line;
         begin
            if Editor.Get_Chars (L, L) /= "" & ASCII.LF then
               Editor.Insert (Loc_Start, "" & ASCII.LF);
            end if;
         end;
      end if;

      Editor.Insert (Loc_Start, Text);

      --  Insert the leading space if needed

      if Surround_With_Blank_Lines
        and then Text (Text'First) /= ASCII.LF
      then
         declare
            L : constant Editor_Location'Class :=
              Loc_Start.Forward_Line (-1).Beginning_Of_Line;
         begin
            if Editor.Get_Chars (L, L) /= "" & ASCII.LF then
               Editor.Insert (Loc_Start, "" & ASCII.LF);
            end if;
         end;
      end if;

      if Indent then
         Editor.Indent
           (Loc_Start,
            Editor.New_Location
              (Line + Lines_Count (Text) - 1, 0).End_Of_Line);
      end if;

      Editor.Finish_Undo_Group;
      return True;
   end Insert_Text;

   ----------------------------
   -- Default_Insertion_Line --
   ----------------------------

   function Default_Insertion_Line (Tree : Construct_Tree) return Integer is
      Iter   : Construct_Tree_Iterator := First (Tree);
      Constr : Simple_Construct_Information;
   begin
      while Iter /= Null_Construct_Tree_Iterator loop
         Constr := Get_Construct (Iter).all;
         if Constr.Category = Cat_Package then
            return Constr.Sloc_End.Line;
         end if;
         Iter := Next (Tree, Iter);
      end loop;

      --  Still not found ? Insert at the beginning of the file (random
      --  choice)
      return 1;
   end Default_Insertion_Line;

   -----------------------------------
   -- Insert_Subprogram_Declaration --
   -----------------------------------

   procedure Insert_Subprogram_Declaration
     (Context  : not null access Factory_Context_Record'Class;
      In_File  : GNATCOLL.VFS.Virtual_File;
      Decl     : String;
      Category : String := "")
   is
      Struct   : Structured_File_Access;
      Tree     : Construct_Tree;
      Iter     : Construct_Tree_Iterator;
      Constr   : Simple_Construct_Information;
      Line     : Integer := Integer'Last;
      Inserted : Boolean;
   begin
      if not Context.Create_Subprogram_Decl then
         --  Nothing to do, the user doesn't want specs
         return;
      end if;

      Struct := Get_Or_Create (Db => Context.Construct_Db, File => In_File);
      Update_Contents (Struct);
      Tree := Get_Tree (Struct);  --  Will take into account existing editors

      Iter := First (Tree);
      while Iter /= Null_Construct_Tree_Iterator loop
         Constr := Get_Construct (Iter).all;
         if Constr.Category in Subprogram_Category then
            Line := Constr.Sloc_Start.Line;
            exit;
         end if;
         Iter := Next (Tree, Iter);
      end loop;

      if Line = Integer'Last then
         Line := Default_Insertion_Line (Tree);
      end if;

      Inserted :=
        Insert_Text (Context, In_File, Line, 1, Decl,
                     Indent                    => True,
                     Surround_With_Blank_Lines => True,
                     Skip_Comments_Backward    => True);
      if not Inserted then
         Context.Report_Error
           ("Could not insert the subprogram declaration at "
            & In_File.Display_Full_Name & ":" & Image (Line, 1));
      end if;

      if Category /= "" then
         Context.Report_Location
           (Category => Category,
            File     => In_File,
            Line     => Line,
            Column   => 1,
            Text     => "Subprogram declaration inserted");
      end if;
   end Insert_Subprogram_Declaration;

   ----------------------------
   -- Insert_Subprogram_Body --
   ----------------------------

   procedure Insert_Subprogram_Body
     (Context     : not null access Factory_Context_Record'Class;
      In_File     : GNATCOLL.VFS.Virtual_File;
      Name        : String;
      Code        : String;
      Before_Line : Integer := Integer'Last;
      Category    : String := "")
   is
      Struct   : Structured_File_Access;
      Tree     : Construct_Tree;
      Iter     : Construct_Tree_Iterator;
      Constr   : Simple_Construct_Information;
      Line     : Integer := Before_Line;
      Inserted : Boolean;
      Result   : Unbounded_String;
   begin
      Struct := Get_Or_Create (Db => Context.Construct_Db, File => In_File);
      Update_Contents (Struct);
      Tree := Get_Tree (Struct);  --  Will take into account existing editors

      Iter := First (Tree);
      while Iter /= Null_Construct_Tree_Iterator loop
         Constr := Get_Construct (Iter).all;
         if Constr.Category in Subprogram_Category
           and then Constr.Sloc_Start.Line <= Line
           and then Constr.Sloc_End.Line > Line
         then
            Line := Constr.Sloc_Start.Line;
            exit;
         end if;
         Iter := Next (Tree, Iter);
      end loop;

      if Line = Integer'Last then
         Line := Default_Insertion_Line (Tree);
      end if;

      if Context.Add_Subprogram_Box then
         Append (Result, (1 .. Name'Length + 6 => '-') & ASCII.LF);
         Append (Result, "-- " & Name & " --" & ASCII.LF);
         Append (Result, (1 .. Name'Length + 6 => '-') & ASCII.LF);
         Append (Result, ASCII.LF);
      end if;

      Append (Result, Code);

      Inserted :=
        Insert_Text (Context, In_File, Line, 1, To_String (Result),
                     Indent                    => True,
                     Surround_With_Blank_Lines => True,
                     Skip_Comments_Backward    => True);
      if not Inserted then
         Context.Report_Error
           ("Could not insert the subprogram body at "
            & In_File.Display_Full_Name & ":" & Image (Line, 1));
      end if;

      if Category /= "" then
         Context.Report_Location
           (Category => Category,
            File     => In_File,
            Line     => Line,
            Column   => 1,
            Text     => "Subprogram body inserted");
      end if;
   end Insert_Subprogram_Body;

end Refactoring.Services;
