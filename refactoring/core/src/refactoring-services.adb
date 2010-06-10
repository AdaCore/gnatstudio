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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
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

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Context : not null access Factory_Context_Record'Class;
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
                         & Debug_Name (Self.Entity) & " is "
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
      Context : not null access Factory_Context_Record'Class;
      PType   : Entities.Queries.Parameter_Type) return String
   is
      Result : Unbounded_String;
      Decl   : constant String := To_String (Self.Decl);
      Index  : Natural := Decl'First;
      Last   : Natural := Self.Equal_Loc - 1;
   begin
      Append (Result, Get (Get_Name (Self.Entity)).all & " : ");

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
      Ref_Iter : Entity_Reference_Iterator;
      Iter     : Entity_Iterator;
      Caller   : Entity_Information;
      Parent   : Entity_Information;
      Ref : Entity_Reference;
      Decl, Location, Body_Loc : File_Location;
      Entity    : Entity_Information;
      Flags     : Entity_References_Flags;
      Is_Global : Boolean;
      Is_Param  : Boolean;
      PType     : Parameter_Type;
      Struct    : Structured_File_Access;
      ERef      : Entity_Reference_Details;

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
     (From : Editor_Location'Class;
      Direction : Integer := 1) return Editor_Location'Class
   is
      Loc : Editor_Location'Class := From;
      Seen_Comment : Boolean := False;
   begin
      loop
         --  Skip backward until we find a non blank line that is not a comment

         declare
            Loc2 : constant Editor_Location'Class := Loc.Forward_Line (-1);
            C    : constant String :=
              Loc.Buffer.Get_Chars (Loc2, Loc2.End_Of_Line);
            Index : Natural := C'First;
         begin
            exit when Loc2 = Loc;  --  Beginning of buffer

            Skip_Blanks (C, Index, Step => 1);

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
      Editor : constant Editor_Buffer'Class :=
        Context.Buffer_Factory.Get (In_File);
      Loc_Start : Editor_Location'Class := Editor.New_Location
        (Line, Integer (Column));
      Loc_End   : constant Editor_Location'Class :=
        Loc_Start.Forward_Char (Replaced_Length - 1);
   begin
      if Replaced_Length /= 0 and then Only_If_Replacing /= "" then
         declare
            Replacing_Str : constant String := To_Lower (Only_If_Replacing);
            Str : constant String :=
              To_Lower (Editor.Get_Chars (Loc_Start, Loc_End));
         begin
            if Str /= Replacing_Str then
               return False;
            end if;
         end;
      end if;

      if Replaced_Length > 0 then
         Editor.Delete (Loc_Start, Loc_End);
      end if;

      if Text = "" then
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
      Struct : Structured_File_Access;
      Tree   : Construct_Tree;
      Iter   : Construct_Tree_Iterator;
      Constr : Simple_Construct_Information;
      Line   : Integer := Integer'Last;
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
      Struct : Structured_File_Access;
      Tree   : Construct_Tree;
      Iter   : Construct_Tree_Iterator;
      Constr : Simple_Construct_Information;
      Line   : Integer := Before_Line;
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
