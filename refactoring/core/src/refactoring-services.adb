-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
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

with Ada.Strings.Fixed;         use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off, "*is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "*is an internal GNAT unit");

with Ada_Semantic_Tree.Parts; use Ada_Semantic_Tree.Parts;
with Basic_Types;             use Basic_Types;
with Entities;                use Entities;
with Entities.Queries;        use Entities.Queries;
with GNAT.Strings;            use GNAT.Strings;
with GPS.Editors;             use GPS.Editors;
with Language.Ada;            use Language.Ada;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with String_Utils;            use String_Utils;
with GNATCOLL.Traces;         use GNATCOLL.Traces;

package body Refactoring.Services is
   Me : constant Trace_Handle := Create ("Refactoring");

   procedure Skip_Keyword
     (Str : String; Index : in out Integer; Word : String);
   --  Skip the next word in Str if it is Word

   procedure Remove_Blanks
     (From : in out Editor_Location'Class; Direction : Integer := 1);
   --  Remove all blanks from From onwards (spaces, tabs and newlines)

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Context : Factory_Context;
      Entity  : Entities.Entity_Information) return Entity_Declaration
   is
      Equal  : Integer;
      EA     : Entity_Access;

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

         return (Entity    => Entity,
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
      Buffer : Editor_Buffer'Class) is
   begin
      if Self.First = null and then Self.Entity /= null then
         declare
            Start  : constant Editor_Location'Class := Buffer.New_Location
              (Line   => Self.SFirst.Line,
               Column => Self.SFirst.Column);
         begin
            Self.First := new Editor_Mark'Class'(Start.Create_Mark);
            Self.Last  := new Editor_Mark'Class'
              (Buffer.New_Location
                 (Line   => Self.SLast.Line,
                  Column => Self.SLast.Column).Create_Mark);
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
         Text  : Big_String_Access;
         Index : Natural;
         Last  : Natural;
      begin
         Get_String (Self.Decl, Text, Last);

         if Last < Text'First then
            return "";
         end if;

         Index := Text'First + 1;  --  skip ':'

         while Index < Last loop
            if Text (Index .. Index + 1) = ":=" then
               Index := Index + 2;
               Skip_Blanks (Text.all, Index);

               if Active (Me) then
                  Trace (Me, "  Initial value of "
                         & Get_Name (Self.Entity).all & " is "
                         & Text (Index .. Last - 1));
               end if;
               return Text (Index .. Last - 1);

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
      Context : Factory_Context;
      PType   : Entities.Queries.Parameter_Type) return String
   is
      Result : Unbounded_String;
      Decl   : constant String := To_String (Self.Decl);
      Index  : Natural := Decl'First;
      Last   : Natural := Self.Equal_Loc - 1;
   begin
      Append (Result, Get_Name (Self.Entity).all & " : ");

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

      Skip_Blanks (Decl, Last, Step => -1);

      Append (Result, Decl (Index .. Last));

      return To_String (Result);
   end Display_As_Parameter;

   -------------------------
   -- Display_As_Variable --
   -------------------------

   function Display_As_Variable
     (Self  : Entity_Declaration) return String
   is
   begin
      return Get_Name (Self.Entity).all & " "
        & To_String (Self.Decl);
   end Display_As_Variable;

   -----------------------
   -- Get_Entity_Access --
   -----------------------

   function Get_Entity_Access
     (Context : Factory_Context;
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
                 (From, From.Forward_Char (Get_Name (Self.Entity)'Length - 1));
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
     (Context        : Factory_Context;
      Entity         : Entity_Information;
      Current_Offset : String_Index_Type) return Boolean
   is
      EA  : Entity_Access := Get_Entity_Access (Context, Get_Type_Of (Entity));
   begin
      if EA /= Null_Entity_Access then
         EA := Get_Last_Visible_Declaration
           (EA, Get_File (EA), Offset => Current_Offset);

         return Get_Construct (EA).Attributes (Ada_Tagged_Attribute)
           or else Get_Construct (EA).Attributes (Ada_Interface_Attribute);
      else
         return False;
      end if;
   end Accepts_Primitive_Ops;

end Refactoring.Services;
