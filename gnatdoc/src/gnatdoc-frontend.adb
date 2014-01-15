------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Basic_Types;              use Basic_Types;
with GNATdoc.Comment;          use GNATdoc.Comment;
with GNATdoc.Utils;            use GNATdoc.Utils;
with GNATdoc.Errout;           use GNATdoc.Errout;
with GNATdoc.Frontend.Builder; use GNATdoc.Frontend.Builder;
with GNATdoc.Time;             use GNATdoc.Time;
with GNAT.Expect;
with GNAT.Regpat;              use GNAT.Regpat;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Utils;
with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Language.Tree;            use Language.Tree;
with Language.Tree.Database;   use Language.Tree.Database;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with Xref.Docgen;              use Xref.Docgen;
with Xref;
with Ada.Unchecked_Deallocation;
with GNAT.IO;

package body GNATdoc.Frontend is
   Me : constant Trace_Handle := Create ("GNATdoc.1-Frontend");
   Enhancements : constant Boolean := False;

   XML_Regpat : constant Pattern_Matcher :=
     Compile (" *<([/]?) *([^ </>]+) *([^<>]*)>", Single_Line);

   type Tokens is
     (Tok_Unknown,
      Tok_Id,          --  Expanded names & identifiers
      Tok_Operator,

      --  Reserved words
      Tok_Abstract,
      Tok_And,
      Tok_Aliased,
      Tok_Case,
      Tok_End,
      Tok_Entry,
      Tok_For,
      Tok_Function,
      Tok_Generic,
      Tok_Interface,
      Tok_Is,
      Tok_Limited,
      Tok_New,
      Tok_Null,
      Tok_Others,
      Tok_Package,
      Tok_Pragma,
      Tok_Private,
      Tok_Procedure,
      Tok_Protected,
      Tok_Record,
      Tok_Renames,
      Tok_Return,
      Tok_Subtype,
      Tok_Tagged,
      Tok_Task,
      Tok_Type,
      Tok_Use,
      Tok_When,
      Tok_With,
      --  Other tokens
      Tok_Left_Paren,
      Tok_Right_Paren,
      Tok_Semicolon);

   ----------------------
   -- Local_Subrograms --
   ----------------------

   procedure Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type);
   --  Add to the nodes their blocks of documentation & sources

   procedure Build_Structured_Comments
     (Context   : access constant Docgen_Context;
      Root      : Entity_Id;
      In_C_Lang : Boolean);
   --  Traverse the Tree of entities and replace blocks of comments by
   --  structured comments.

   function Get_Token (S : String) return Tokens;
   --  Return the token associated with S

   package Debug is
      function To_Str (E : Entity_Id) return String;
      procedure Print_Entity (E : Entity_Id; Prefix : String);
   end Debug;
   use Debug;

   package Scopes_Stack is
      type Scope_Info is private;
      type Scope_Id is access all Scope_Info;

      procedure Disable_Enter_Scope;
      procedure Enable_Enter_Scope;

      function Current_Context return Scope_Id;
      --  Return the context in the top of the stack

      procedure Enter_Scope (Entity : Entity_Id);
      --  Enter in the syntax scope of Entity

      procedure Exit_Scope;
      --  Leave a syntax scope

      -- Getters --------------------------------------------------

      function Get_Current_Entity
        (Context : Scope_Id) return Entity_Id;
      --  Return the current entity of Context

      function Get_End_Decl_Found
        (Scope : Scope_Id) return Boolean;

      function Get_Prev_Entity_In_Scope
        (Scope : Scope_Id) return Entity_Id;

      function Get_Scope (Context : Scope_Id) return Entity_Id;
      --  Return scope of the next entity (No_Entity if no scope available)

      function Tok_Subprogram_Seen
        (Scope : Scope_Id) return Boolean;

      -- Setters --------------------------------------------------

      procedure Replace_Current_Entity
        (Context : Scope_Id; Entity : Entity_Id);

      procedure Reset_End_Decl_Found
        (Scope : Scope_Id);

      procedure Set_Current_Entity
        (Context : Scope_Id; Entity : Entity_Id);

      procedure Set_End_Decl_Found
        (Scope : Scope_Id);

      procedure Set_Token_Seen
        (Scope : Scope_Id; Token : Tokens);

      procedure Reset_Tok_Subprogram_Seen
        (Scope : Scope_Id);

      procedure Print_Scopes;
      --  For debugging

   private
      type Token_Flags is array (Tokens'Range) of Boolean;

      type Scope_Info is record
         Scope                : Entity_Id;

         Prev_Entity_In_Scope : Entity_Id;
         Current_Entity       : Entity_Id;

         --  Parser state
         End_Decl_Found      : Boolean := False;
         Token_Seen          : Token_Flags := (others => False);
      end record;

      procedure Free is
        new Ada.Unchecked_Deallocation (Scope_Info, Scope_Id);

      pragma Inline (Current_Context);
      pragma Inline (Enter_Scope);
      pragma Inline (Exit_Scope);

      pragma Inline (Get_Current_Entity);
      pragma Inline (Get_Scope);
   end Scopes_Stack;
   use Scopes_Stack;

   ---------------------------------
   -- Append_Comments_And_Sources --
   ---------------------------------

   procedure Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type)
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang     : constant Boolean := not In_Ada_Lang;

      Body_File        : Virtual_File := No_File;
      Buffer           : GNAT.Strings.String_Access;
      Buffer_Body      : GNAT.Strings.String_Access;
      C_Headers_Buffer : GNAT.Strings.String_Access;
      Buffers_Swapped  : Boolean;

      package Compiler_Workaround is

         procedure Register_Delayed_Remove_From_Scope
           (Scope : Entity_Id; Entity : Entity_Id);
         --  Register Entity to be removed from Scope.

         function Get_Entity
           (Context : access constant Docgen_Context;
            Name    : String;
            Loc     : General_Location) return Entity_Id;
         --  Retrieve the entity referenced at the given location. Name can be
         --  an expanded name. This also works for operators, whether they are
         --  quoted ("=") or not (=).

         procedure Workaround_Compiler_Inner_Scope_Problem (E : Entity_Id);
         --  This subprogram workarounds a compiler problem in formals of
         --  subprograms with renamings and also in entities defined in single
         --  tasks and single protected objects: the compiler does not generate
         --  the correct scope of their entities.

      private
         type Scope_Remove_Info is record
            Scope  : Entity_Id;
            Entity : Entity_Id;
         end record;

         package Scope_Remove_List is new Ada.Containers.Vectors
           (Index_Type => Natural, Element_Type => Scope_Remove_Info);

         Entities_Removed_From_Scope : Scope_Remove_List.Vector;

      end Compiler_Workaround;
      use Compiler_Workaround;

      package body Compiler_Workaround is

         ----------------
         -- Get_Entity --
         ----------------

         function Get_Entity
           (Context : access constant Docgen_Context;
            Name    : String;
            Loc     : General_Location) return Entity_Id
         is
            function Entity_Name (Name : String) return String;
            --  For expanded names return the name without its prefix; for
            --  single names return Name.

            function Entity_Name (Name : String) return String is
               J : Integer := Name'Last;
            begin
               while J >= Name'First and then Name (J) /= '.' loop
                  J := J - 1;
               end loop;

               --  Handle operators

               if Name (J + 1) = '"' then
                  return Name (J + 2 .. Name'Last - 1);

               --  Common case

               else
                  return Name (J + 1 .. Name'Last);
               end if;
            end Entity_Name;

            Entity : constant General_Entity :=
                       Xref.Get_Entity
                         (Db   => Context.Database,
                          Name => Entity_Name (Name),
                          Loc  => Loc);

         begin
            if Present (Entity)
              and then not Is_Fuzzy (Entity)
            then
               return Get_Unique_Entity (Context, Loc.File, Entity);
            else
               return null;
            end if;
         end Get_Entity;

         ----------------------------------------
         -- Register_Delayed_Remove_From_Scope --
         ----------------------------------------

         procedure Register_Delayed_Remove_From_Scope
           (Scope : Entity_Id; Entity : Entity_Id)
         is
            New_Entry : constant Scope_Remove_Info := (Scope, Entity);
         begin
            Entities_Removed_From_Scope.Append (New_Entry);
         end Register_Delayed_Remove_From_Scope;

         ------------------------------------
         -- Workaround_Inner_Scope_Problem --
         ------------------------------------

         procedure Workaround_Compiler_Inner_Scope_Problem (E : Entity_Id) is
            Scope     : constant Entity_Id := Get_Scope (E);
            Cursor    : EInfo_List.Cursor;
            Entity    : Entity_Id;
            Location  : General_Location;
            Loc_End   : General_Location;
            Loc_Start : General_Location;

            procedure Check_Ordered_Entities_In_Scope;
            procedure Check_Ordered_Entities_In_Scope is
               Cursor    : EInfo_List.Cursor;
               Entity    : Entity_Id;
               Location  : General_Location;
               Prev_Loc  : General_Location := No_Location;

            begin
               Cursor := Get_Entities (Scope).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Entity   := EInfo_List.Element (Cursor);
                  Location := LL.Get_Location (Entity);

                  if Present (Prev_Loc) then
                     pragma Assert (Location.File = Prev_Loc.File
                                    and then (Location.Line > Prev_Loc.Line
                                      or else (Location.Line = Prev_Loc.Line
                                        and then
                                        Location.Column > Prev_Loc.Column)));
                     Prev_Loc := Location;
                  end if;

                  EInfo_List.Next (Cursor);
               end loop;
            end Check_Ordered_Entities_In_Scope;

            Check_Generic_Formals : constant Boolean :=
              Is_Generic (E)
                and then Is_Subprogram (E)
                and then Present (Get_Generic_Formals_Loc (E))
                and then not Has_Generic_Formals (E);

         begin
            Check_Ordered_Entities_In_Scope;

            if Is_Concurrent_Object (E) then
               Loc_Start := LL.Get_Location (E);
               Loc_End := Get_End_Of_Syntax_Scope_Loc (E);

            elsif Get_Kind (E) = E_Entry then
               Loc_Start := LL.Get_Location (E);
               Loc_End := Get_End_Of_Profile_Location (E);

            elsif Is_Subprogram (E) then
               if Is_Generic (E) then
                  pragma Assert (Present (Get_Generic_Formals_Loc (E)));
                  Loc_Start := Get_Generic_Formals_Loc (E);
               else
                  Loc_Start := LL.Get_Location (E);
               end if;

               Loc_End := Get_End_Of_Profile_Location (E);

            else pragma Assert (Get_Kind (E) = E_Generic_Package);
               pragma Assert (Present (Get_Generic_Formals_Loc (E)));
               Loc_Start := Get_Generic_Formals_Loc (E);
               Loc_End := LL.Get_Location (E);
            end if;

            Cursor := Get_Entities (Scope).First;
            while EInfo_List.Has_Element (Cursor) loop
               Entity   := EInfo_List.Element (Cursor);
               Location := LL.Get_Location (Entity);

               exit when Location.Line > Loc_End.Line;

               if Entity /= E then

                  --  We cannot remove the entities from their scope since at
                  --  current stage we have two iterators active traversing
                  --  the entities of the tree. Hence we delay its removal
                  --  from the wrong scope until we can safely do it without
                  --  corrupting the tree.

                  if Check_Generic_Formals
                    and then Location.Line >= Get_Generic_Formals_Loc (E).Line
                    and then Location.Line < LL.Get_Location (E).Line
                    and then Get_Scope (Entity) /= E
                  then
                     Register_Delayed_Remove_From_Scope
                       (Scope  => Get_Scope (Entity),
                        Entity => Entity);

                     Set_Is_Generic_Formal (Entity);

                     --  Adding minimum decoration to undecorated generic
                     --  formals

                     if Get_Kind (Entity) = E_Unknown then
                        Set_Kind (Entity, E_Generic_Formal);
                     end if;

                     Append_Generic_Formal (E, Entity);
                     Set_Scope (Entity, E);

                  elsif Location.Line >= Loc_Start.Line
                    and then Location.Line <= Loc_End.Line
                    and then Get_Scope (Entity) /= E
                  then

                     Register_Delayed_Remove_From_Scope
                       (Scope  => Get_Scope (Entity),
                        Entity => Entity);

                     Append_To_Scope (E, Entity);

                     if Is_Subprogram_Or_Entry (E) then
                        Set_Kind (Entity, E_Formal);
                     end if;

                     Set_Scope (Entity, E);
                  end if;
               end if;

               EInfo_List.Next (Cursor);
            end loop;
         end Workaround_Compiler_Inner_Scope_Problem;

      end Compiler_Workaround;

      procedure Ada_Get_Doc (E : Entity_Id);
      pragma Unreferenced (Ada_Get_Doc);
      --  Retrieve the documentation associated with E

      procedure Ada_Get_Source (E : Entity_Id);
      pragma Unreferenced (Ada_Get_Source);
      --  Retrieve the Ada source associated with E

      procedure Ada_Set_Doc (E : Entity_Id);
      --  Set the documentation of E

      procedure CPP_Get_Doc (E : Entity_Id);
      --  Retrieve the C/C++ documentation associated with E

      procedure CPP_Get_Source (E : Entity_Id);
      --  Retrieve the C/C++ source associated with E

      procedure Filter_Doc (E : Entity_Id);
      --  Filter the documentation using the user-defined filter

      procedure Parse_Ada_Profile
        (E        : Entity_Id;
         Buffer   : GNAT.Strings.String_Access;
         Location : General_Location;
         Is_Body  : Boolean);
      --  Parse the profile of a subprogram or an entry and complete the
      --  decoration of entity E setting attributes:
      --   - End_Of_Profile_Location (or End_Of_Profile_Location_In_Body)

      procedure Parse_Package_Header
        (E        : Entity_Id;
         Buffer   : GNAT.Strings.String_Access);
      --  Parse the profile of a package to complete the decoration of E
      --  setting attribute Parent_Package

      procedure Parse_Ada_File
        (Buffer   : GNAT.Strings.String_Access);

      procedure Parse_Ada_Profile
        (E        : Entity_Id;
         Buffer   : GNAT.Strings.String_Access;
         Location : General_Location;
         Is_Body  : Boolean;
         Profile  : out Unbounded_String);
      --  Parse the profile of a subprogram or an entry and complete the
      --  decoration of entity E. Similar to the previous one but returns
      --  the scanned profile.

      procedure Previous_Word
        (Index           : Natural;
         Prev_Word_Begin : out Natural;
         Prev_Word_End   : out Natural);
      --  Return the indexes to the first word in Buffer located before Index

      function Search_Backward
        (Lines_Skipped : out Natural;
         Buffer : GNAT.Strings.String_Access;
         From   : Natural;
         Word_1 : String;
         Word_2 : String := "") return Natural;
      --  Case insensitive search backward in Buffer (starting at index
      --  From) for the index of the first occurrence of Word_1 (or Word_2
      --  if present).

      function Skip_Blanks_Backward
        (Buffer : GNAT.Strings.String_Access;
         Index  : Natural) return Natural;
      --  Displace Idx backwards skipping character ' '

      procedure Swap_Buffers;
      --  Swap the contents of Buffer and C_Headers_Buffer. Used to retrieve
      --  the sources and the documentation located in the header file.

      -----------------
      -- Ada_Get_Doc --
      -----------------

      procedure Ada_Get_Doc (E : Entity_Id) is
      begin
         --  Skip processing entities defined in other files (for example,
         --  primitives inherited from the parent type).

         if LL.Get_Location (E).File /= File then
            return;
         end if;

         Set_Doc (E,
           Xref.Docgen.Get_Docgen_Documentation
             (Self =>
                General_Xref_Database_Record (Context.Database.all)'Access,
              Handler  => Context.Lang_Handler,
              Buffer   => Buffer,
              Location => LL.Get_Location (E),
              End_Loc  => (if not Is_Subprogram_Or_Entry (E) then No_Location
                           else Get_End_Of_Profile_Location (E))));

         --  For nested packages, if no documentation was found then we try
         --  locating the documentation immediately after the package spec.
         --  Required to retrieve the documentation of some GNATCOLL nested
         --  packages.

         if Get_Doc (E) = No_Comment_Result
           and then Is_Package (E)
           and then not Is_Standard_Entity (Get_Scope (E))
         then
            Set_Doc (E,
              Xref.Docgen.Get_Docgen_Documentation
                (Self =>
                   General_Xref_Database_Record (Context.Database.all)'Access,
                 Handler  => Context.Lang_Handler,
                 Buffer   => Buffer,
                 Location => Get_End_Of_Syntax_Scope_Loc (E)));
         end if;

         if Is_Partial_View (E)
           and then Context.Options.Show_Private
         then
            Set_Full_View_Doc (E,
              Xref.Docgen.Get_Docgen_Documentation
                (Self =>
                   General_Xref_Database_Record (Context.Database.all)'Access,
                 Handler => Context.Lang_Handler,
                 Buffer  => Buffer,
                 Location => LL.Get_Body_Loc (E)));
         end if;

         --  If no documentation is available in the spec of a subprogram and
         --  we are allowed to retrieve documentation from the body then we
         --  try retrieving it!

         if Get_Doc (E) = No_Comment_Result
           and then Context.Options.Process_Bodies
           and then Buffer_Body /= null
           and then (Is_Subprogram (E) or else Get_Kind (E) = E_Formal)
           and then Present (LL.Get_Body_Loc (E))
         then
            if LL.Get_Body_Loc (E).File = Body_File then
               Set_Doc (E,
                 Xref.Docgen.Get_Docgen_Documentation
                   (Self     => General_Xref_Database_Record
                                  (Context.Database.all)'Access,
                    Handler  => Context.Lang_Handler,
                    Buffer   => Buffer_Body,
                    Location => LL.Get_Body_Loc (E)));

               --  Locate the end of the body profile

               if Is_Subprogram (E) then
                  Parse_Ada_Profile
                    (E        => E,
                     Buffer   => Buffer_Body,
                     Location => LL.Get_Body_Loc (E),
                     Is_Body  => True);
               end if;

            --  Retrieve documentation located in separate compilation unit

            elsif Present (LL.Get_Body_Loc (E)) then
               declare
                  Buffer : GNAT.Strings.String_Access;
               begin
                  Buffer := LL.Get_Body_Loc (E).File.Read_File;

                  Set_Doc (E,
                    Xref.Docgen.Get_Docgen_Documentation
                      (Self     => General_Xref_Database_Record
                                     (Context.Database.all)'Access,
                       Handler  => Context.Lang_Handler,
                       Buffer   => Buffer,
                       Location => LL.Get_Body_Loc (E)));

                  --  Locate the end of the body profile

                  if Is_Subprogram (E) then
                     Parse_Ada_Profile
                       (E        => E,
                        Buffer   => Buffer_Body,
                        Location => LL.Get_Body_Loc (E),
                        Is_Body  => True);
                  end if;

                  Free (Buffer);
               end;
            end if;

            if Get_Doc (E) /= No_Comment_Result then
               Set_Is_Doc_From_Body (E);
            end if;
         end if;
      end Ada_Get_Doc;

      -----------------
      -- Ada_Set_Doc --
      -----------------

      Prev_Comment_Line : Natural := 0;

      procedure Ada_Set_Doc (E : Entity_Id) is

         function May_Have_Tags (Text : Unbounded_String) return Boolean;
         --  Return true if Text may contain some tag

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

      begin
         if LL.Get_Location (E).File /= File then
            return;
         end if;

         if Context.Options.Leading_Doc then

            if Is_Compilation_Unit (E) then
               --  For documentation located before compilation units it is
               --  mandatory to use some tag. Required to safely differentiate
               --  copyright banners, etc.

               if Present (Get_Doc_Before (E))
                 and then May_Have_Tags (Get_Doc_Before (E).Text)
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Prev_Comment_Line := Get_Doc_Before (E).Start_Line;

               elsif Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Prev_Comment_Line := Get_Doc_After (E).Start_Line;
               end if;
            else
               if Present (Get_Doc_Before (E))
                 and then Get_Doc_Before (E).Start_Line /= Prev_Comment_Line
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Prev_Comment_Line := Get_Doc_Before (E).Start_Line;

               elsif Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Prev_Comment_Line := Get_Doc_After (E).Start_Line;
               end if;
            end if;

         else
            if Is_Compilation_Unit (E) then

               if Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Prev_Comment_Line := Get_Doc_After (E).Start_Line;

               --  For documentation located before compilation units it is
               --  mandatory to use some tag. Required to safely differentiate
               --  copyright banners, etc.

               elsif Present (Get_Doc_Before (E))
                 and then May_Have_Tags (Get_Doc_Before (E).Text)
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Prev_Comment_Line := Get_Doc_Before (E).Start_Line;
               end if;
            else
               if Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Prev_Comment_Line := Get_Doc_After (E).Start_Line;

               elsif Present (Get_Doc_Before (E))
                 and then Get_Doc_Before (E).Start_Line /= Prev_Comment_Line
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Prev_Comment_Line := Get_Doc_Before (E).Start_Line;
               end if;
            end if;
         end if;

         Set_Doc_Before (E, No_Comment_Result);
         Set_Doc_After (E, No_Comment_Result);
      end Ada_Set_Doc;

      --------------------
      -- Ada_Get_Source --
      --------------------

      procedure Ada_Get_Source (E : Entity_Id) is

         function Get_Declaration_Source return Unbounded_String;
         --  Retrieve the source of the declaration E.

         function Get_Discriminant_Doc return Comment_Result;
         --  Return the comment of the discriminant (if any). Must be called
         --  after Get_Discriminant_Source.

         function Get_Discriminant_Source return Unbounded_String;
         --  Retrieve the source of the discriminant E

         function Get_Instance_Source return Unbounded_String;
         --  Retrieve the source of the generic instance E

         function Get_Record_Type_Source
           (Loc          : General_Location;
            Is_Full_View : Boolean) return Unbounded_String;
         --  Retrieve the source of record type E

         function Get_Concurrent_Type_Source
           (Loc : General_Location) return Unbounded_String;
         --  Retrieve the source of a protected type or a task type (including
         --  the case of single protected object and single task type) In
         --  addition, for single tasks and single protected objects modify the
         --  tree moving their inner entities into their scopes; required to
         --  workaround a problem with their scope caused by the compiler.

         function Get_Type_Declaration_Source return Unbounded_String;
         --  Retrieve the source of the declaration E

         function Is_Single_Protected_Object (E : Entity_Id) return Boolean;
         --  Return True if the previous word of the variable E is "protected"
         --  (done to workaround the missing identification of single protected
         --  objects in Xref)

         ----------------------------
         -- Get_Declaration_Source --
         ----------------------------

         function Get_Declaration_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "(" then
                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     --  Workaround problem caused by the actual parameters
                     --  of generic instantiations???
                     if Par_Count = 0 then
                        return True;
                     end if;
                     Par_Count := Par_Count - 1;
                  elsif S = ";" then
                     if Par_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Me, E);
                  return True;
            end CB;

            Index         : Natural;
            Lines_Skipped : Natural;

         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            if Is_Generic_Formal (E) then
               declare
                  Prev_Word_Begin : Natural;
                  Prev_Word_End   : Natural;
               begin
                  Previous_Word (Index, Prev_Word_Begin, Prev_Word_End);

                  declare
                     Word : constant String :=
                       To_Lower
                         (Buffer.all (Prev_Word_Begin .. Prev_Word_End));
                  begin
                     if Word = "type" or else Word = "subtype" then
                        Index := Prev_Word_Begin;
                     end if;
                  end;
               end;
            end if;

            Parse_Entities
              (Lang, Buffer.all (Index .. Buffer'Last),
               CB'Unrestricted_Access);

            return Printout;
         end Get_Declaration_Source;

         --------------------------
         -- Get_Discriminant_Doc --
         --------------------------

         function Get_Discriminant_Doc return Comment_Result is
            Src    : constant Unbounded_String := Get_Src (E);
            Len    : constant Natural := Length (Src);
            J      : Natural;
            Line   : Natural := LL.Get_Location (E).Line;
            Result : Comment_Result;
         begin
            if Src = Null_Unbounded_String then
               return No_Comment_Result;
            else
               J := 1;
               while J < Len loop
                  while J < Len and then Element (Src, J) /= '-' loop
                     if Element (Src, J) = ASCII.LF then
                        Line := Line + 1;
                     end if;

                     J := J + 1;
                  end loop;

                  if J < Len then
                     if Element (Src, J + 1) = '-' then
                        Result :=
                          Comment_Result'(
                            Text => Unbounded_Slice (Src, J + 2, Len),
                            Start_Line => Line);
                        return Result;
                     else
                        J := J + 1;
                     end if;
                  end if;
               end loop;

               return No_Comment_Result;
            end if;
         end Get_Discriminant_Doc;

         -----------------------------
         -- Get_Discriminant_Source --
         -----------------------------

         function Get_Discriminant_Source return Unbounded_String is
            Printout      : Unbounded_String;
            From          : Natural;
            Idx           : Natural;
            Index         : Natural;
            Lines_Skipped : Natural;

         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);
            From := Index;

            Idx := Index;
            while Idx < Buffer'Last
              and then Buffer (Idx) /= ';'
              and then Buffer (Idx) /= ')'
            loop
               Idx := Idx + 1;
            end loop;

            if Buffer (Idx) = ')' then
               Idx := Idx - 1;
            end if;

            Printout := To_Unbounded_String (Buffer (From .. Idx));

            return Printout;
         end Get_Discriminant_Source;

         -------------------------
         -- Get_Instance_Source --
         -------------------------

         function Get_Instance_Source return Unbounded_String is
            Printout      : Unbounded_String;
            From          : Natural;
            Idx           : Natural;
            Index         : Natural;
            Lines_Skipped : Natural;
            Par_Count     : Natural;

         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            --  Locate the beginning of the type declaration. This is a naive
            --  approach used in the prototype since it does not handle the
            --  word "type" located in a comment. A backward parser is required
            --  here. Must be improved???

            if Is_Subprogram (E) then
               Index :=
                 Search_Backward (Lines_Skipped,
                   Buffer => Buffer,
                   From   => Index - 1,
                   Word_1 => "procedure",
                   Word_2 => "function");
            else
               Index :=
                 Search_Backward (Lines_Skipped,
                   Buffer => Buffer,
                   From   => Index - 1,
                   Word_1 => "package");
            end if;

            --  Append tabulation

            if Buffer (Index - 1) = ' ' then
               From := Skip_Blanks_Backward (Buffer, Index - 1);
               Printout :=
                 To_Unbounded_String (Buffer.all (From .. Index - 1));
            end if;

            --  Parenthesis count used to handle access to subprogram types

            Par_Count := 0;

            Idx := Index;
            while Idx < Buffer'Last
              and then (Par_Count > 0 or else Buffer (Idx) /= ';')
            loop
               if Buffer (Idx) = '(' then
                  Par_Count := Par_Count + 1;
               elsif Buffer (Idx) = ')' then
                  Par_Count := Par_Count - 1;
               end if;

               Idx := Idx + 1;
            end loop;

            Printout :=
              Printout & To_Unbounded_String (Buffer (Index .. Idx));

            return Printout;
         end Get_Instance_Source;

         ----------------------------
         -- Get_Record_Type_Source --
         ----------------------------

         function Get_Record_Type_Source
           (Loc          : General_Location;
            Is_Full_View : Boolean) return Unbounded_String
         is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            End_Record_Found : Boolean := False;
            In_Parent_Part   : Boolean := False;
            --  In_Parent_Part is set when we identify the sequence "is new"
            --  or the sequence "interface and" which indicate that the next
            --  token corresponds with the parent type of a tagged type or an
            --  interface type.

            Is_Interface   : Boolean := False;
            Is_Null        : Boolean := False;
            Tagged_Null    : Boolean := False;
            Tagged_Private : Boolean := False;
            With_Null      : Boolean := False;
            With_Private   : Boolean := False;

            Prev_Token : Tokens := Tok_Unknown;
            Token      : Tokens := Tok_Unknown;
            End_Loc    : Source_Location;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               End_Loc := Sloc_End;

               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Identifier_Text then

                  --  Check if the parent type is visible in the partial view

                  if not Is_Full_View
                    and then In_Parent_Part
                  then
                     Set_Has_Private_Parent (E, False);
                  end if;

                  --  No action needed if we already know the parent type

                  if In_Parent_Part
                    and then No (Get_Parent (E))
                  then
                     --  In the partial view Xref returns the parent in the
                     --  list of parents (and hence, at current stage it is
                     --  stored in the list of progenitors). We localize it
                     --  and remove it from the list of progenitors.

                     if not Is_Full_View
                       and then Is_Tagged (E)
                     then
                        declare
                           Parent : Entity_Id;
                        begin
                           Parent :=
                             Find_Entity (Get_Progenitors (E).all, Name => S);
                           pragma Assert (Present (Parent));

                           Set_Parent (E, Parent);
                           Delete_Entity (Get_Progenitors (E).all, Parent);
                        end;

                     --  Derivation of untagged type. For instance:
                     --     type Rec_A is record ...
                     --     subtype Rec_B is Rec_A;

                     elsif not Is_Full_View
                       and then not Is_Tagged (E)
                     then
                        --  Unhandled yet???
                        null;

                     --  If the parent of a private type is only specified
                     --  in its full view, then Xref cannot locate it because
                     --  currently the compiler does not generate such
                     --  information in the ALI file (see comment in
                     --  gnatdoc-atree.adb).

                     else
                        --  We don't know the exact location associated with
                        --  the entity in the database. Hence for now we take a
                        --  conservative approach and we first retry the entity
                        --  from the database and use its location to retry its
                        --  associated unique high-level entity.

                        declare
                           Tok_Loc   : General_Location;
                           LL_Parent : General_Entity;
                           Parent    : Entity_Id := Atree.No_Entity;
                           Dot_Pos   : Natural := 0;

                        begin
                           if Is_Expanded_Name (S) then
                              declare
                                 Idx : Natural;
                              begin
                                 Parent  := Find_Unique_Entity (S);
                                 Idx     := Index (S, ".", Going => Backward);
                                 Dot_Pos := Idx - S'First + 1;
                              end;
                           end if;

                           if No (Parent) then
                              Tok_Loc :=
                                General_Location'
                                  (File   => File,
                                   Line   => Loc.Line + Sloc_Start.Line - 1,
                                   Column =>
                                     Visible_Column_Type
                                       (Sloc_Start.Column + Dot_Pos));

                              LL_Parent :=
                                Xref.Get_Entity
                                  (Db   => Context.Database,
                                   Name => Get_Short_Name (S),
                                   Loc  => Tok_Loc);

                              --  Tolerate the case in which the package
                              --  containing the parent type is not
                              --  available.

                              if Present (LL_Parent)
                                and then not Is_Fuzzy (LL_Parent)
                              then
                                 Parent :=
                                   Builder.Get_Unique_Entity
                                     (Context, File, LL_Parent);
                              end if;
                           end if;

                           if Present (Parent) then
                              pragma Assert (LL.Is_Type (Parent));
                              Set_Parent (E, Parent);

                              if Is_Tagged (Parent) then
                                 Set_Is_Tagged (E);
                              end if;

                              if Get_Progenitors (E).Contains (Parent) then
                                 Delete_Entity
                                   (Get_Progenitors (E).all, Parent);
                              end if;
                           end if;
                        end;
                     end if;

                     In_Parent_Part := False;
                  end if;

               elsif Entity = Keyword_Text then
                  Prev_Token := Token;
                  Token      := Get_Token (S);

                  --  Private types are not well recognized by Xref. Hence
                  --  when the entity was built it was initially decorated
                  --  as an Incomplete declaration. Now we complete its
                  --  decoration.

                  if Is_Partial_View (E)
                    and then Token = Tok_Private
                    and then not Is_Private (E)
                  then
                     pragma Assert (Is_Incomplete (E));
                     Set_Is_Incomplete (E, False);
                     Set_Is_Private (E);
                  end if;

                  if Token = Tok_Type
                    or else Token = Tok_Is
                  then
                     null;

                  elsif Prev_Token = Tok_Is then
                     if Token = Tok_New then
                        In_Parent_Part := True;

                     elsif Token = Tok_Null then
                        Is_Null := True;

                     elsif Token = Tok_Tagged then

                        --  Complete the decoration of this type since Xref
                        --  does not facilitate decorating well tagged types
                        --  that have no primitives (for example, a root of
                        --  derivation defined as abstract tagged null record
                        --  without primitives)

                        if not Is_Tagged (E) then
                           Set_Is_Tagged (E);
                           Set_Kind (E, E_Tagged_Record_Type);
                        end if;

                     elsif Token = Tok_Abstract then
                        --  Missing Xref decoration???
                        --  pragma Assert (LL.Is_Abstract (E));
                        --  Is_Abstract := True;

                        null;

                     elsif Token = Tok_Interface then
                        pragma Assert (Get_Kind (E) = E_Interface);
                        Is_Interface := True;
                     end if;

                  elsif Token = Tok_Tagged then
                     if Prev_Token = Tok_Abstract then

                        --  Complete the decoration of this type since Xref
                        --  does not facilitate decorating well tagged types
                        --  that have no primitives (for example, a root of
                        --  derivation defined as abstract tagged null record
                        --  without primitives)

                        if not Is_Tagged (E) then
                           Set_Is_Tagged (E);
                           Set_Kind (E, E_Tagged_Record_Type);
                        end if;
                     end if;

                  elsif Token = Tok_Limited then
                     null;

                  elsif Token = Tok_Interface then
                     pragma Assert (Get_Kind (E) = E_Interface);
                     Is_Interface := True;

                  elsif Token = Tok_And then
                     if Prev_Token = Tok_Interface then
                        In_Parent_Part := True;
                     end if;

                  elsif Token = Tok_Null then
                     if Prev_Token = Tok_Tagged
                       or else Prev_Token = Tok_Limited
                     then
                        Tagged_Null := True;

                     elsif Prev_Token = Tok_With then
                        With_Null := True;
                     end if;

                  elsif Token = Tok_Private then
                     if Prev_Token = Tok_With then
                        With_Private := True;
                     elsif Prev_Token = Tok_Tagged then
                        Tagged_Private := True;
                     end if;

                  elsif (Is_Null
                           or else Tagged_Null
                           or else With_Null
                           or else Prev_Token = Tok_End)
                    and then Token = Tok_Record
                  then
                     End_Record_Found := True;
                  end if;

               elsif Entity = Operator_Text then
                  Prev_Token := Token;

                  if S = "(" then
                     Token := Tok_Left_Paren;
                     Par_Count := Par_Count + 1;

                  elsif S = ")" then
                     Token := Tok_Right_Paren;
                     Par_Count := Par_Count - 1;

                  elsif S = ";" then
                     Token := Tok_Semicolon;

                     if Par_Count = 0 then
                        return not Is_Full_View
                          or else Is_Subtype (E)
                          or else Is_Interface
                          or else End_Record_Found
                          or else (With_Private
                                     and then Prev_Token = Tok_Private)
                          or else (Tagged_Private
                                     and then Prev_Token = Tok_Private);
                     end if;
                  end if;
               end if;

               return False; --  Continue
            exception
               when E : others =>
                  Trace (Me, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            From            : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;

         --  Start of processing for Get_Record_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => Loc.Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (Loc.Column),
               Index         => Entity_Index);

            --  Locate the beginning of the type declaration. This is a naive
            --  approach used in the prototype since it does not handle the
            --  word "type" located in a comment. A backward parser is required
            --  here. Must be improved???

            Prev_Word_Begin :=
              Search_Backward (Lines_Skipped,
                Buffer => Buffer,
                From   => Entity_Index - 1,
                Word_1 => "type",
                Word_2 => "subtype");

            declare
               Word : constant String := "subtype";
            begin
               if Buffer.all
                    (Prev_Word_Begin .. Prev_Word_Begin + Word'Length - 1)
                 = Word
               then
                  Set_Is_Subtype (E);
               end if;
            end;

            --  Append tabulation

            if Buffer (Prev_Word_Begin - 1) = ' ' then
               From := Skip_Blanks_Backward (Buffer, Prev_Word_Begin - 1);

               if Buffer.all (From) = ASCII.LF then
                  From := From + 1;
               end if;

               Append (Buffer.all (From .. Prev_Word_Begin - 1));
            else
               From := Prev_Word_Begin;
            end if;

            Parse_Entities
              (Lang, Buffer.all (From .. Buffer'Last),
               CB'Unrestricted_Access);

            Set_End_Of_Syntax_Scope_Loc (E,
               General_Location'
                 (File => File,
                  Line => LL.Get_Location (E).Line + End_Loc.Line - 1,
                  Column => Visible_Column (End_Loc.Column)));

            return Printout;
         end Get_Record_Type_Source;

         --------------------------------
         -- Get_Concurrent_Type_Source --
         --------------------------------

         function Get_Concurrent_Type_Source
           (Loc : General_Location) return Unbounded_String
         is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            In_Definition  : Boolean := False;
            In_Item_Decl   : Boolean := False;
            End_Decl_Found : Boolean := False;

            Prev_Token : Tokens := Tok_Unknown;
            pragma Unreferenced (Prev_Token);

            Token      : Tokens := Tok_Unknown;
            End_Loc    : Source_Location;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               End_Loc := Sloc_End;

               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Identifier_Text then
                  null;

               elsif Entity = Keyword_Text then
                  Prev_Token := Token;
                  Token      := Get_Token (S);

                  if Token = Tok_Is then
                     In_Definition := True;

                  elsif Token = Tok_Procedure
                    or else Token = Tok_Function
                    or else Token = Tok_Entry
                  then
                     In_Item_Decl := True;

                  elsif Token = Tok_End
                    and then not In_Item_Decl
                    and then In_Definition
                  then
                     End_Decl_Found := True;
                  end if;

               elsif Entity = Operator_Text then
                  Prev_Token := Token;

                  if S = "(" then
                     Token := Tok_Left_Paren;
                     Par_Count := Par_Count + 1;

                  elsif S = ")" then
                     Token := Tok_Right_Paren;
                     Par_Count := Par_Count - 1;

                  elsif S = ";" then
                     Token := Tok_Semicolon;

                     if Par_Count = 0 then

                        --  Handle simple cases: "task T;" or "task type T;"

                        if not In_Definition then
                           return True;  --  Stop

                        elsif In_Item_Decl then
                           In_Item_Decl := False;

                        elsif End_Decl_Found then
                           return True; --  Stop
                        end if;
                     end if;
                  end if;
               end if;

               return False; --  Continue
            exception
               when E : others =>
                  Trace (Me, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            From            : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;

         --  Start of processing for Get_Record_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => Loc.Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (Loc.Column),
               Index         => Entity_Index);

            --  Locate the beginning of the type declaration. This is a naive
            --  approach used in the prototype since it does not handle the
            --  word "type" located in a comment. A backward parser is required
            --  here. Must be improved???

            if Get_Kind (E) = E_Task_Type
              or else Get_Kind (E) = E_Single_Task
            then
               Prev_Word_Begin :=
                 Search_Backward (Lines_Skipped,
                   Buffer => Buffer,
                   From   => Entity_Index - 1,
                   Word_1 => "task");
            else
               Prev_Word_Begin :=
                 Search_Backward (Lines_Skipped,
                   Buffer => Buffer,
                   From   => Entity_Index - 1,
                   Word_1 => "protected");
            end if;

            --  Append tabulation

            if Buffer (Prev_Word_Begin - 1) = ' ' then
               From := Skip_Blanks_Backward (Buffer, Prev_Word_Begin - 1);

               if Buffer.all (From) = ASCII.LF then
                  From := From + 1;
               end if;

               Append (Buffer.all (From .. Prev_Word_Begin - 1));
            else
               From := Prev_Word_Begin;
            end if;

            Parse_Entities
              (Lang, Buffer.all (From .. Buffer'Last),
               CB'Unrestricted_Access);

            Set_End_Of_Syntax_Scope_Loc (E,
               General_Location'
                 (File => File,
                  Line => LL.Get_Location (E).Line + End_Loc.Line - 1,
                  Column => Visible_Column (End_Loc.Column)));

            if Is_Concurrent_Object (E) then
               Workaround_Compiler_Inner_Scope_Problem (E);
            end if;

            return Printout;
         end Get_Concurrent_Type_Source;

         ---------------------------------
         -- Get_Type_Declaration_Source --
         ---------------------------------

         function Get_Type_Declaration_Source return Unbounded_String is
            Printout      : Unbounded_String;
            From          : Natural;
            Idx           : Natural;
            Index         : Natural;
            Lines_Skipped : Natural;
            Par_Count     : Natural;

         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            --  Locate the beginning of the type declaration. This is a naive
            --  approach used in the prototype since it does not handle the
            --  word "type" located in a comment. A backward parser is required
            --  here. Must be improved???

            Index :=
              Search_Backward (Lines_Skipped,
                Buffer => Buffer,
                From   => Index - 1,
                Word_1 => "type",
                Word_2 => "subtype");

            --  Append tabulation

            if Buffer (Index - 1) = ' ' then
               From := Skip_Blanks_Backward (Buffer, Index - 1);
               Printout :=
                 To_Unbounded_String (Buffer.all (From .. Index - 1));
            end if;

            --  Parenthesis count used to handle access to subprogram types

            Par_Count := 0;

            Idx := Index;
            while Idx < Buffer'Last
              and then (Par_Count > 0 or else Buffer (Idx) /= ';')
            loop
               if Buffer (Idx) = '(' then
                  Par_Count := Par_Count + 1;
               elsif Buffer (Idx) = ')' then
                  Par_Count := Par_Count - 1;
               end if;

               Idx := Idx + 1;
            end loop;

            Printout :=
              Printout & To_Unbounded_String (Buffer (Index .. Idx));

            return Printout;
         end Get_Type_Declaration_Source;

         --------------------------------
         -- Is_Single_Protected_Object --
         --------------------------------

         function Is_Single_Protected_Object (E : Entity_Id) return Boolean is
            Index           : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;
         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            Previous_Word (Index, Prev_Word_Begin, Prev_Word_End);

            return To_Lower (Buffer.all (Prev_Word_Begin .. Prev_Word_End))
                     = "protected";
         end Is_Single_Protected_Object;

      --  Start of processing for Ada_Get_Source

      begin
         --  No action needed if we already retrieved the documentation of this
         --  entity

         if Get_Src (E) /= Null_Unbounded_String then
            return;

         --  Skip processing entities defined in other files (for example,
         --  primitives inherited of the parent type).

         elsif LL.Get_Location (E).File /= File then
            return;

         --  Handle generic instances

         elsif Present (LL.Get_Instance_Of (E)) then
            Set_Src (E, Get_Instance_Source);
            return;

         elsif Is_Generic_Formal (E) then
            Set_Src (E, Get_Declaration_Source);
            return;
         end if;

         case Get_Kind (E) is

            when E_Enumeration_Literal |
                 E_Formal              |
                 E_Named_Number =>
               null;

            when E_Access_Type              |
                 E_Access_Subprogram_Type   |
                 E_Array_Type               |
                 E_Boolean_Type             |
                 E_Decimal_Fixed_Point_Type |
                 E_Enumeration_Type         |
                 E_Fixed_Point_Type         |
                 E_Floating_Point_Type      |
                 E_Integer_Type             |
                 E_String_Type              =>
               Set_Src (E, Get_Type_Declaration_Source);

            when E_Variable |
                 E_Exception =>
               if Is_Single_Protected_Object (E) then

                  --  Correct previous wrong decoration (done by
                  --  Atree.New_Internal_Entity).

                  Set_Is_Incomplete (E, False);
                  Remove_Full_View (E);

                  Set_Kind (E, E_Single_Protected);
                  Set_Src (E,
                    Get_Concurrent_Type_Source (LL.Get_Location (E)));
               else
                  Set_Src (E, Get_Declaration_Source);
               end if;

            when E_Discriminant =>
               if Present (Get_Scope (E))
                 and then Is_Concurrent_Type_Or_Object (Get_Scope (E))
                 and then False --  Disabled for now???
               then
                  Set_Src (E, Get_Discriminant_Source);
                  Set_Doc (E, Get_Discriminant_Doc);
               end if;

            when E_Component =>
               if Present (Get_Scope (E))
                 and then Is_Concurrent_Type_Or_Object (Get_Scope (E))
                 and then False --  Disabled for now???
               then
                  Set_Src (E, Get_Declaration_Source);
               end if;

            when E_Abstract_Record_Type |
                 E_Private_Object       |
                 E_Record_Type          |
                 E_Tagged_Record_Type   |
                 E_Interface            |
                 E_Class_Wide_Type      =>

               pragma Assert (Get_Kind (E) /= E_Private_Object);

               --  Xref is not able to indicate us if the parent type is
               --  visible only in the full-view of a private type. Hence
               --  we decorate it here as visible only in the full-view and
               --  Get_Record_Type_Src takes care of removing this flag if
               --  it is also visible in the partial view.

               if Is_Partial_View (E) then
                  Set_Has_Private_Parent (E);
               end if;

               --  Public full type declaration of a record

               if not Is_Partial_View (E) then
                  Set_Src (E,
                           Get_Record_Type_Source
                             (LL.Get_Location (E), Is_Full_View => True));
               else
                  Set_Src (E,
                    Get_Record_Type_Source
                      (LL.Get_Location (E), Is_Full_View => False));

                  if Context.Options.Show_Private
                    and then LL.Get_Body_Loc (E).File = File
                  then
                     Set_Full_View_Src (E,
                       Get_Record_Type_Source
                         (LL.Get_Body_Loc (E), Is_Full_View => True));
                  end if;
               end if;

            when E_Single_Task      |
                 E_Task_Type        |
                 E_Protected_Type   =>
               if not Is_Partial_View (E) then
                  Set_Src (E,
                    Get_Concurrent_Type_Source (LL.Get_Location (E)));

               else
                  Set_Src (E,
                    Get_Record_Type_Source
                      (LL.Get_Location (E), Is_Full_View => False));

                  if Context.Options.Show_Private then
                     Set_Full_View_Src (E,
                       Get_Concurrent_Type_Source (LL.Get_Body_Loc (E)));
                  end if;
               end if;

            when E_Single_Protected =>
               --  At current stage this case never occurs because the compiler
               --  references single protected objects as E_Variable (see above
               --  the management of E_Variable covering this case).
               pragma Assert (False);

            when E_Generic_Formal =>
               --  Although all generic formals should be catched and handled
               --  before we enter into this case-statement (see above)
               --  we must still process here generic formals that
               --  have not been yet moved into their correct scope
               --  by the routine which completes its decoration (cf.
               --  Workaround_Compiler_Inner_Scope_Problem).

               Set_Src (E, Get_Declaration_Source);

            when E_Abstract_Function  |
                 E_Abstract_Procedure |
                 E_Function           |
                 E_Procedure          |
                 E_Generic_Function   |
                 E_Generic_Procedure  |
                 E_Entry              =>
               declare
                  Profile : Unbounded_String;
               begin
                  Parse_Ada_Profile
                    (E        => E,
                     Buffer   => Buffer,
                     Location => LL.Get_Location (E),
                     Is_Body  => False,
                     Profile  => Profile);

                  Set_Src (E, Profile);
               end;

            when E_Package         |
                 E_Generic_Package =>
               if No (LL.Get_Scope (E))
                 and then No (LL.Get_Parent_Package (E))
               then
                  Parse_Package_Header (E, Buffer);
               end if;

            --  C and C++ values

            when E_Macro          |
                 E_Function_Macro |
                 E_Class          |
                 E_Class_Instance |
                 E_Include_File   |
                 E_Attribute      |
                 E_Unknown        =>
               pragma Assert (False);

         end case;
      end Ada_Get_Source;

      -----------------
      -- CPP_Get_Doc --
      -----------------

      procedure CPP_Get_Doc (E : Entity_Id) is
      begin
         --  No action needed if we already retrieved the documentation of
         --  this entity

         if Get_Doc (E) /= No_Comment_Result then
            return;
         end if;

         --  Documentation of C/C++ macros unsupported yet???

         if Get_Kind (E) = E_Macro then
            return;
         end if;

         Buffers_Swapped := False;

         if LL.Get_Location (E).File /= File then
            if LL.Get_Location (E).File /= File_Entities.Header_File then
               return;
            else
               if C_Headers_Buffer = null then
                  C_Headers_Buffer := LL.Get_Location (E).File.Read_File;
               end if;

               Swap_Buffers;
            end if;
         end if;

         --  Skip processing entities defined in other files (for example,
         --  primitives inherited from the parent type).

         if LL.Get_Location (E).File /= File then
            return;
         end if;

         Set_Doc (E,
           Xref.Docgen.Get_Docgen_Documentation
             (Self =>
                General_Xref_Database_Record (Context.Database.all)'Access,
              Handler  => Context.Lang_Handler,
              Buffer   => Buffer,
              Location => LL.Get_Location (E)));

         --  Restore original contents of buffers

         if Buffers_Swapped then
            Swap_Buffers;
         end if;
      end CPP_Get_Doc;

      --------------------
      -- CPP_Get_Source --
      --------------------

      procedure CPP_Get_Source (E : Entity_Id) is

         function Get_Class_Type_Source return Unbounded_String;
         --  Retrieve the source of class type E

         function Get_Struct_Type_Source return Unbounded_String;
         --  Retrieve the source of record type E (To be removed???)

         function Get_Subprogram_Source return Unbounded_String;
         --  Retrieve the source of subprogram E

         function Get_Variable_Declaration_Source return Unbounded_String;
         --  Retrieve the source of the variable/object declaration E

         ---------------------------
         -- Get_Class_Type_Source --
         ---------------------------

         function Get_Class_Type_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Brackets_Count : Natural := 0;
            Last_Idx       : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "{" then
                     Brackets_Count := Brackets_Count + 1;
                  elsif S = "}" then
                     Brackets_Count := Brackets_Count - 1;
                  elsif S = ";"
                     and then Brackets_Count = 0
                  then
                     return True;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Me, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;

         --  Start of processing for Get_Class_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Entity_Index);

            --  Locate the beginning of the previous word
            declare
               Idx : Natural := Entity_Index - 1;
            begin
               while Idx > Buffer'First
                 and then (Buffer (Idx) = ' '
                           or else Buffer (Idx) = ASCII.LF)
               loop
                  Idx := Idx - 1;
               end loop;
               Prev_Word_End := Idx;

               while Idx > Buffer'First
                 and then (Buffer (Idx) /= ' '
                             and then Buffer (Idx) /= ASCII.LF)
               loop
                  Idx := Idx - 1;
               end loop;
               Prev_Word_Begin := Idx + 1;
            end;

            --  Xref returns E_Class for unions???

            declare
               Prev_Word : constant String :=
                 To_Lower (Buffer.all (Prev_Word_Begin .. Prev_Word_End));
            begin
               pragma Assert (Prev_Word = "class" or else Prev_Word = "union");
            end;

            Last_Idx := Prev_Word_Begin - 1;
            Parse_Entities
              (Lang, Buffer.all (Prev_Word_Begin .. Buffer'Last),
               CB'Unrestricted_Access);

            return Printout;
         end Get_Class_Type_Source;

         ----------------------------
         -- Get_Record_Type_Source --
         ----------------------------

         function Get_Struct_Type_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Brackets_Count : Natural := 0;
            Last_Idx       : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "{" then
                     Brackets_Count := Brackets_Count + 1;
                  elsif S = "}" then
                     Brackets_Count := Brackets_Count - 1;
                  elsif S = ";" then
                     if Brackets_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Me, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;

         --  Start of processing for Get_Struct_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Entity_Index);

            --  Supported cases of C structs:

            --     struct Foo                 --  [1]
            --     | typedef struct ...       --  [2]
            --     | static const struct ...  --  [3]

            --  For [1] Entity_Index references Foo; for [2] and [3] it points
            --  to the first letter of the word "struct"

            declare
               Index : Natural := Entity_Index;

            begin
               loop
                  Previous_Word (Index, Prev_Word_Begin, Prev_Word_End);

                  declare
                     Prev_Word : constant String :=
                                   To_Lower (Buffer.all
                                     (Prev_Word_Begin .. Prev_Word_End));
                  begin
                     exit when  Prev_Word /= "struct"
                       and then Prev_Word /= "const"
                       and then Prev_Word /= "static";

                     Index := Prev_Word_Begin;
                  end;
               end loop;

               Parse_Entities
                 (Lang, Buffer.all (Index .. Buffer'Last),
                  CB'Unrestricted_Access);
            end;

            return Printout;
         end Get_Struct_Type_Source;

         ---------------------------
         -- Get_Subprogram_Source --
         ---------------------------

         function Get_Subprogram_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "(" then
                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     Par_Count := Par_Count - 1;

                     if Par_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Me, E);
                  return True;
            end CB;

            Index         : Natural;
            Lines_Skipped : Natural;

         --  Start of processing for Get_Subprogram_Source

         begin
            --  Displace the pointer to the beginning of the subprogram
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            Parse_Entities
              (Lang, Buffer.all (Index .. Buffer'Last),
               CB'Unrestricted_Access);

            return Printout;
         end Get_Subprogram_Source;

         -------------------------------------
         -- Get_Variable_Declaration_Source --
         -------------------------------------

         function Get_Variable_Declaration_Source return Unbounded_String is
            Printout        : Unbounded_String;
            Idx             : Natural;
            Index           : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;
         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            if Index > Buffer'First then
               Previous_Word (Index - 1, Prev_Word_Begin, Prev_Word_End);
            else
               Prev_Word_Begin := Index;
            end if;

            Idx := Index;
            while Idx < Buffer'Last
              and then Buffer (Idx) /= ';'
            loop
               Idx := Idx + 1;
            end loop;

            Printout := To_Unbounded_String (Buffer (Prev_Word_Begin .. Idx));

            return Printout;
         end Get_Variable_Declaration_Source;

         Under_Development_Text : constant String :=
           "<<Get_Source under development for this kind of entity>>";

      --  Start of processing for CPP_Get_Source

      begin
         --  Documentation of C/C++ macros unsupported yet???

         if Get_Kind (E) = E_Macro then
            return;
         end if;

         Buffers_Swapped := False;

         if LL.Get_Location (E).File /= File then
            if LL.Get_Location (E).File /= File_Entities.Header_File then
               return;
            else
               if C_Headers_Buffer = null then
                  C_Headers_Buffer := LL.Get_Location (E).File.Read_File;
               end if;

               Swap_Buffers;
            end if;
         end if;

         if Is_Subprogram (E) then
            Set_Src (E, Get_Subprogram_Source);

         elsif Get_Kind (E) = E_Class then
            Set_Src (E, Get_Class_Type_Source);

         elsif Is_Class_Or_Record_Type (E) then
            --  There is no need to check that it is NOT a class since classes
            --  have been handled immediately before!
            Set_Src (E, Get_Struct_Type_Source);

         elsif Get_Kind (E) = E_Variable then
            Set_Src (E, Get_Variable_Declaration_Source);

         --  C and C++ typedef

         elsif LL.Is_Type (E) then
            Set_Src (E, Get_Variable_Declaration_Source);

         --  Do not add any message to a C/C++ entity which already has the
         --  "under development" text

         elsif not In_Ada_Language (E)
           and then Get_Src (E) /= Null_Unbounded_String
           and then Get_Src (E) = Under_Development_Text
         then
            null;

         else
            Set_Src (E,
              To_Unbounded_String (Under_Development_Text));
         end if;

         --  Restore original contents of buffers

         if Buffers_Swapped then
            Swap_Buffers;
         end if;
      end CPP_Get_Source;

      ----------------
      -- Filter_Doc --
      ----------------

      procedure Filter_Doc (E : Entity_Id) is
         use type GNAT.Expect.Pattern_Matcher_Access;

      begin
         if Get_Doc (E) = No_Comment_Result
           or else Context.Options.Comments_Filter = null
         then
            return;
         end if;

         declare
            Doc     : Comment_Result := Get_Doc (E);
            S       : constant String := To_String (Doc.Text);
            Matches : Match_Array (0 .. 0);
            New_Doc : Unbounded_String;
            F       : Natural;
            L       : Natural;

         begin
            L := S'First;

            while L <= S'Last loop

               --  Identify the next comment line

               F := L;

               while L <= S'Last and then S (L) /= ASCII.LF loop
                  L := L + 1;
               end loop;

               --  Apply to it the user-defined filter

               declare
                  Line : constant String := S (F .. L - 1);

               begin
                  Match
                    (Context.Options.Comments_Filter.all, Line, Matches);

                  --  If the line has the pattern then remove from it
                  --  the matching pattern and append it to the new
                  --  block of comments

                  if Matches (0) /= No_Match then
                     declare
                        F1 : constant Natural := Matches (0).First;
                        L1 : constant Natural := Matches (0).Last;

                        Filtered_Line : constant String :=
                          Line (Line'First .. F1 - 1) &
                          Line (L1 + 1 .. Line'Last);
                     begin
                        Append (New_Doc, Filtered_Line & ASCII.LF);
                     end;
                  end if;
               end;

               --  Skip line terminators

               while L <= S'Last and then S (L) = ASCII.LF loop
                  L := L + 1;
               end loop;
            end loop;

            Doc.Text := New_Doc;
            Set_Doc (E, Doc);
         end;
      end Filter_Doc;

      ----------------------------------
      -- Parse_Ada_Subprogram_Profile --
      ----------------------------------

      procedure Parse_Ada_Profile
        (E        : Entity_Id;
         Buffer   : GNAT.Strings.String_Access;
         Location : General_Location;
         Is_Body  : Boolean)
      is
         Profile : Unbounded_String;
         pragma Unreferenced (Profile);
      begin
         Parse_Ada_Profile
           (E        => E,
            Buffer   => Buffer,
            Location => Location,
            Is_Body  => Is_Body,
            Profile  => Profile);
      end Parse_Ada_Profile;

      -----------------------
      -- Parse_Ada_Profile --
      -----------------------

      procedure Parse_Ada_Profile
        (E        : Entity_Id;
         Buffer   : GNAT.Strings.String_Access;
         Location : General_Location;
         Is_Body  : Boolean;
         Profile  : out Unbounded_String)
      is
         procedure Append (Text : String);
         --  Append Text to Profile

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  Callback for entity parser

         ------------
         -- Append --
         ------------

         procedure Append (Text : String) is
         begin
            Profile := Profile & Text;
         end Append;

         --------
         -- CB --
         --------

         Start_Line  : Natural;
         In_Profile  : Boolean := False;
         Is_Renaming : Boolean := False;
         Last_Idx    : Natural := 0;
         Par_Count   : Natural := 0;

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);

            S : String renames
              Buffer (Sloc_Start.Index .. Sloc_End.Index);

         begin
            --  Append all text between previous call and current one

            if Last_Idx /= 0 then
               Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
            end if;

            Last_Idx := Sloc_End.Index;
            Append (S);

            if Entity = Comment_Text then
               return False; --  continue

            elsif Entity = Keyword_Text then
               declare
                  Keyword : constant String := To_Lower (S);
               begin
                  if Keyword = "procedure"
                    or else Keyword = "function"
                    or else Keyword = "entry"
                  then
                     In_Profile := True;

                  elsif Is_Body
                    and then In_Profile
                    and then Par_Count = 0
                    and then Keyword = "is"
                  then
                     Set_End_Of_Profile_Location_In_Body (E,
                       General_Location'
                         (File => File,
                          Line => Start_Line + Sloc_End.Line - 1,
                          Column => Visible_Column_Type (Sloc_End.Column)));
                     return True;

                  elsif Par_Count = 0
                    and then Keyword = "renames"
                  then
                     Is_Renaming := True;
                  end if;
               end;

            --  Complete decoration of subprogram renamings since Xref does
            --  not provide the renamed entity when it is located in the same
            --  package spec than the renaming subprogram.

            elsif Is_Renaming
              and then Entity = Identifier_Text
              and then No (Get_Alias (E))
            then
               declare
                  Current_Line   : constant Natural :=
                    LL.Get_Location (E).Line + Sloc_Start.Line - 1;
                  Current_Column : constant Visible_Column :=
                    Visible_Column (Sloc_Start.Column);
                  Current_Loc    : constant General_Location :=
                    General_Location'(File   => File,
                                      Line   => Current_Line,
                                      Column => Current_Column);
                  Entity         : constant Entity_Id :=
                    Get_Entity
                      (Context => Context,
                       Name    => S,
                       Loc     => Current_Loc);
               begin
                  if Present (Entity) then
                     Set_Alias (E, Entity);
                  end if;
               end;

            elsif Entity = Operator_Text then
               if S = "(" then
                  Par_Count := Par_Count + 1;

               elsif S = ")" then
                  Par_Count := Par_Count - 1;

               elsif S = ";" then
                  if In_Profile and then Par_Count = 0 then
                     if Is_Body then
                        Set_End_Of_Profile_Location_In_Body (E,
                          General_Location'
                            (File => File,
                             Line => Start_Line + Sloc_End.Line - 1,
                             Column => Visible_Column_Type (Sloc_End.Column)));
                     else
                        Set_End_Of_Profile_Location (E,
                          General_Location'
                            (File => File,
                             Line => Start_Line + Sloc_End.Line - 1,
                             Column => Visible_Column_Type (Sloc_End.Column)));
                     end if;

                     return True;
                  end if;
               end if;
            end if;

            return False;
         exception
            when E : others =>
               Trace (Me, E);
               return True;
         end CB;

         --  Local variables

         From          : Natural;
         Index         : Natural;
         Lines_Skipped : Natural;

      --  Start of processing for Parse_Ada_Subprogram_Profile

      begin
         --  Displace the pointer to the beginning of the subprogram
         Index := Buffer'First;
         GNATCOLL.Utils.Skip_Lines
           (Str           => Buffer.all,
            Lines         => Location.Line - 1,
            Index         => Index,
            Lines_Skipped => Lines_Skipped);

         GNATCOLL.Utils.Skip_To_Column
           (Str           => Buffer.all,
            Columns       => Natural (Location.Column),
            Index         => Index);

         if Is_Generic_Formal (E) then
            Index :=
              Search_Backward (Lines_Skipped,
                               Buffer => Buffer,
                               From   => Index - 1,
                               Word_1 => "with");

         elsif Is_Generic (E) then
            declare
               Formals_Loc : General_Location;
               Idx         : Natural;
            begin
               Index :=
                 Search_Backward (Lines_Skipped,
                                  Buffer => Buffer,
                                  From   => Index - 1,
                                  Word_1 => "generic");

               --  For now we only set the attribute Generic_Formals_Loc when
               --  processing generics defined in compilation units containing
               --  specs ???

               if not Is_Body then
                  Idx := Index;
                  while Idx > Buffer'First
                    and then Buffer (Idx) /= ASCII.LF
                  loop
                     Idx := Idx - 1;
                  end loop;

                  Formals_Loc :=
                    General_Location'
                      (File => File,
                       Line => Location.Line - Lines_Skipped,
                       Column => Visible_Column (Index - Idx));

                  Set_Generic_Formals_Loc (E, Formals_Loc);
               end if;
            end;

         elsif Get_Kind (E) = E_Entry then
            Index :=
              Search_Backward (Lines_Skipped,
                               Buffer => Buffer,
                               From   => Index - 1,
                               Word_1 => "entry");

         else
            Index :=
              Search_Backward (Lines_Skipped,
                               Buffer => Buffer,
                               From   => Index - 1,
                               Word_1 => "procedure",
                               Word_2 => "function");
         end if;

         Start_Line := Location.Line - Lines_Skipped;

         --  Append tabulation

         if Buffer.all (Index - 1) = ' ' then
            From := Skip_Blanks_Backward (Buffer, Index - 1);
            Append (Buffer.all (From .. Index - 1));
         end if;

         Parse_Entities
           (Lang, Buffer.all (Index .. Buffer'Last),
            CB'Unrestricted_Access);

         --  Initially we tried to restrict the invocation of the following
         --  routine to subprogram renamings; unfortunately similar problems
         --  occur with the formals of annonymous access to subprograms.

         --  For now we only set the attribute Generic_Formals_Loc when
         --  processing generics found in generics defined in compilation
         --  units containing specs (and hence we cannot rely on that attribute
         --  to workaround wrong decoration of formals) ???

         if not Is_Body then
            Workaround_Compiler_Inner_Scope_Problem (E);
         end if;
      end Parse_Ada_Profile;

      --------------------------
      -- Parse_Package_Header --
      --------------------------

      procedure Parse_Package_Header
        (E        : Entity_Id;
         Buffer   : GNAT.Strings.String_Access)
      is
         Index : Natural;

         procedure CB (Token : Token_Record;
                       Stop  : in out Boolean);

         procedure Workaround_Compiler_Get_Parent_Problem (Full_Name : String);
         --  This subprogram workarounds a compiler problem in entities
         --  associated with child packages: the compiler does not generate
         --  their scope and hence Xref cannot compute its Parent entity.

         procedure Workaround_Compiler_Get_Parent_Problem (Full_Name : String)
         is
            Parent : Entity_Id;
         begin
            if Full_Name = "" then
               return;
            end if;

            Parent := Builder.Find_Unique_Entity (Full_Name);

            if Present (Parent) then
               pragma Assert (Is_Package (Parent));
               Set_Parent_Package (E, Parent);
            end if;
         end Workaround_Compiler_Get_Parent_Problem;

         --------
         -- CB --
         --------

         Is_First_Dot : Boolean := True;
         Full_Name    : Unbounded_String;

         procedure CB (Token : Token_Record;
                       Stop  : in out Boolean) is
         begin
            case Token.Tok_Type is
               when Tok_Dot =>
                  if Is_First_Dot then
                     Is_First_Dot := False;
                  else
                     Full_Name := "." & Full_Name;
                  end if;

               when Tok_Identifier =>
                  declare
                     Name : String renames
                        Buffer.all
                          (Integer (Token.Token_First)
                           .. Integer (Token.Token_Last));

                  begin
                     Full_Name := Name & Full_Name;
                  end;

               when others =>
                  Workaround_Compiler_Get_Parent_Problem
                    (To_String (Full_Name));
                  Stop := True;
            end case;
         end CB;

         --  Local variables

         Location      : constant General_Location := LL.Get_Location (E);
         Lines_Skipped : Natural;

      begin
         --  Displace the pointer to the beginning of the package spec

         Index := Buffer'First;
         GNATCOLL.Utils.Skip_Lines
           (Str           => Buffer.all,
            Lines         => Location.Line - 1,
            Index         => Index,
            Lines_Skipped => Lines_Skipped);

         GNATCOLL.Utils.Skip_To_Column
           (Str           => Buffer.all,
            Columns       => Natural (Location.Column),
            Index         => Index);

         if Is_Generic (E) then
            declare
               Formals_Loc : General_Location;
               Idx         : Natural;
            begin
               Index :=
                 Search_Backward (Lines_Skipped,
                                  Buffer => Buffer,
                                  From   => Index - 1,
                                  Word_1 => "generic");

               Idx := Index;
               while Idx > Buffer'First
                 and then Buffer (Idx) /= ASCII.LF
               loop
                  Idx := Idx - 1;
               end loop;

               Formals_Loc :=
                 General_Location'
                   (File => File,
                    Line => Location.Line - Lines_Skipped,
                    Column => Visible_Column (Index - Idx));

               Set_Generic_Formals_Loc (E, Formals_Loc);
            end;
         end if;

         --  Locate the end of the package name

         while Index > Buffer'First
           and then Buffer (Index) /= '.'
           and then Buffer (Index) /= ' '
           and then Buffer (Index) /= ASCII.LF
           and then Buffer (Index) /= ASCII.HT
         loop
            Index := Index - 1;
         end loop;

         if Index > Buffer'First
           and then Buffer (Index) = '.'
         then
            Index := Index - 1;

            Parse_Tokens_Backwards
              (Lang         => Lang,
               Buffer       => Buffer.all (Buffer'First .. Index),
               Start_Offset => String_Index_Type (Index),
               Callback     => CB'Unrestricted_Access);
         end if;

         --  For generic packages the information provided by the compiler is
         --  correct. However, for homogeneity in the management of generic
         --  formals we invoke here the workaround routine to move all the
         --  generic formals into the scope of the generic package.

         if Is_Generic (E) then
            Workaround_Compiler_Inner_Scope_Problem (E);
         end if;
      end Parse_Package_Header;

      --------------------
      -- Parse_Ada_File --
      --------------------

      procedure Parse_Ada_File
        (Buffer   : GNAT.Strings.String_Access)
      is
         No_Line        : constant Natural := 0;
         Doc_Start_Line : Natural := No_Line;
         Doc_End_Line   : Natural := No_Line;
         Doc            : Unbounded_String;

         Printout       : Unbounded_String;
         Printout_Plain : Unbounded_String;

         New_Entities : EInfo_List.Vector;

         package Extended_Cursor is
            type Extended_Cursor is private;
            function Has_Entity  (Cursor : Extended_Cursor) return Boolean;
            function Entity      (Cursor : Extended_Cursor) return Entity_Id;
            function Prev_Entity (Cursor : Extended_Cursor) return Entity_Id;

            procedure Next_Entity (Cursor : in out Extended_Cursor);
            procedure Initialize (Cursor : in out Extended_Cursor);

            procedure Set_Next_Entity
              (Cursor : in out Extended_Cursor;
               Entity : Entity_Id);
            --  This subprogram is used to workaround missing entities

         private
            type Extended_Cursor is record
               Cursor       : EInfo_List.Cursor;
               Element      : Entity_Id := Atree.No_Entity;
               Prev_Element : Entity_Id := Atree.No_Entity;
            end record;
         end Extended_Cursor;

         procedure Append_Comment (Text : String);
         pragma Inline (Append_Comment);
         --  Append Text to Comment

         procedure Append_Plain_Sources (Text : String);
         pragma Inline (Append_Plain_Sources);
         --  Append Text to Printout

         procedure Append_Sources (Text : String);
         pragma Inline (Append_Sources);
         --  Append Text to Printout

         procedure Clear_Doc;
         --  Clear the accumulated comment

         procedure Clear_Sources;
         --  Clear the accumulated sources

         procedure Clear_Plain_Sources;
         --  Clear the accumulated sources

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  Callback for entity parser

         ------------
         -- Append --
         ------------

         procedure Append_Comment (Text : String) is
            Comment_Prefix : constant String := "--";
         begin
            pragma Assert
              (Text'Length > Comment_Prefix'Length
               and then Comment_Prefix =
                 Text (Text'First .. Text'First + Comment_Prefix'Length - 1));
            Doc := Doc & Text (Text'First + 2 .. Text'Last);
         end Append_Comment;

         procedure Append_Plain_Sources (Text : String) is
         begin
            Printout_Plain := Printout_Plain & Text;
         end Append_Plain_Sources;

         procedure Append_Sources (Text : String) is
         begin
            Printout := Printout & Text;
         end Append_Sources;

         -----------
         -- Clear --
         -----------

         procedure Clear_Doc is
         begin
            if Doc_Start_Line /= No_Line then
               Doc_Start_Line := No_Line;
               Doc            := Null_Unbounded_String;
            end if;
         end Clear_Doc;

         procedure Clear_Plain_Sources is
         begin
            if Present (Printout_Plain) then
               Printout_Plain := Null_Unbounded_String;
            end if;
         end Clear_Plain_Sources;

         procedure Clear_Sources is
         begin
            if Present (Printout) then
               Printout := Null_Unbounded_String;
            end if;
         end Clear_Sources;

         Scope_Level : Natural := 0;
         Scope_Tab : constant String :=
           "| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | ";
         --------
         -- CB --
         --------

         Cursor     : Extended_Cursor.Extended_Cursor;
         Last_Idx   : Natural := 0;
         Par_Count  : Natural := 0;
         Prev_Token : Tokens := Tok_Unknown;
         Token      : Tokens := Tok_Unknown;

         Nested_Variants_Count  : Natural := 0;

         In_Compilation_Unit    : Boolean := False;
         Generics_Nesting_Level : Natural := 0;
         In_Generic_Formals     : Boolean := False;
         In_Generic_Decl        : Boolean := False;
         Generic_Formals        : EInfo_List.Vector;
         Generic_Formals_Loc    : General_Location := No_Location;
         --  Location of the word "generic"

         In_Definition    : Boolean := False;
         In_Item_Decl     : Boolean := False;

         In_Parent_Part   : Boolean := False;
         --  In_Parent_Part is set when we identify the sequence "is new"
         --  or the sequence "interface and" which indicate that the next
         --  token corresponds with the parent type of a tagged type or an
         --  interface type.

         In_Pragma                       : Boolean := False;
         In_Representation_Clause        : Boolean := False;

         In_Record_Representation_Clause : Boolean := False;
         --  Used to avoid premature output of record representation clauses
         --  when processing ";"

         In_Skipped_Declaration : Boolean := False;

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);

            S : String renames
                  Buffer (Sloc_Start.Index .. Sloc_End.Index);

            procedure Accumulate_Comments;

            procedure Complete_Decoration (End_Decl_Found : Boolean);

            procedure Handle_Doc;
            procedure Handle_Scopes (End_Decl_Found : Boolean);
            procedure Handle_Sources (End_Decl_Found : Boolean);
            procedure Handle_Tokens;

            function Has_Scope (E : Entity_Id) return Boolean;
            function In_Next_Entity return Boolean;

            procedure Set_Doc_After_Current_Entity;
            procedure Set_Doc_After_Previous_Entity_In_Scope;

            procedure Print_State;

            -------------------------
            -- Accumulate_Comments --
            -------------------------

            procedure Accumulate_Comments is
            begin
               --  Clear the previously accumulated documentation if the
               --  current one is not its continuation

               if Doc_End_Line /= No_Line
                 and then Sloc_Start.Line /= Doc_End_Line + 1
               then
                  declare
                     Current_Entity : constant Entity_Id :=
                       Get_Current_Entity (Current_Context);
                  begin
                     if Present (Current_Entity)
                       and then
                         Doc_Start_Line
                           >= LL.Get_Location (Current_Entity).Line
                     then
                        Set_Doc_After_Current_Entity;
                     else
                        Set_Doc_After_Previous_Entity_In_Scope;
                     end if;
                  end;

                  Clear_Doc;
               end if;

               if Doc_Start_Line = No_Line then
                  Doc_Start_Line := Sloc_Start.Line;
               end if;

               Append_Comment (S);
               Doc_End_Line := Sloc_End.Line;
            end Accumulate_Comments;

            -------------------------
            -- Complete_Decoration --
            -------------------------

            procedure Complete_Decoration (End_Decl_Found : Boolean) is

               procedure Decorate_Scope (E : Entity_Id);
               --  Workaround missing decoration of the Scope

               procedure Decorate_Scope (E : Entity_Id) is
                  Scope : constant Entity_Id := Get_Scope (Current_Context);

                  procedure Update_Scope;
                  procedure Update_Scope is
                  begin
                     if Is_Partial_View (E) then
                        Remove_From_Scope (Get_Full_View (E));
                        Append_To_Scope (Scope, Get_Full_View (E));
                        Set_Scope (Get_Full_View (E), Scope);
                     end if;

                     Remove_From_Scope (E);
                     Append_To_Scope (Scope, E);
                     Set_Scope (E, Scope);
                  end Update_Scope;

               begin
                  if not End_Decl_Found
                    and then Is_Concurrent_Type_Or_Object (Scope)
                  then
                     Remove_From_Scope (E);
                     Append_To_Scope (Scope, E);
                     Set_Scope (E, Scope);

                  elsif not End_Decl_Found
                    and then Is_Subprogram (Scope)
                    and then Get_Kind (E) = E_Variable
                  then
                     Set_Kind (E, E_Formal);
                     Remove_From_Scope (E);
                     Append_To_Scope (Scope, E);
                     Set_Scope (E, Scope);

                  elsif Is_Subprogram (E)
                    and then Is_Generic (Scope)
                  then
                     Remove_From_Scope (E);
                     Append_To_Scope (Scope, E);
                     Set_Scope (E, Scope);

                  --  This should be the general case for generics

                  elsif Is_Generic (Scope) then
                     Update_Scope;

                  elsif No (Get_Scope (E)) then
                     Update_Scope;

                  --  Workaround wrong decoration in enumeration types???

                  elsif Get_Kind (E) = E_Enumeration_Type
                    and then Get_Scope (E) /= Scope
                  then
                     pragma Assert (Is_Package (Scope));
                     Update_Scope;

                  elsif Get_Scope (E) /= Scope then
                     Update_Scope;

                  else
                     null;
                  end if;
               end Decorate_Scope;

            --  Start of processing for Complete_Decoration

            begin
               case Token is
                  when Tok_Generic =>
                     In_Generic_Formals := True;
                     Generic_Formals_Loc :=
                       General_Location'
                         (File   => File,
                          Line   => Sloc_Start.Line,
                          Column => Visible_Column (Sloc_Start.Column));

                  when Tok_Package |
                       Tok_Procedure |
                       Tok_Function  =>
                     if In_Generic_Formals
                       and then Par_Count = 0
                       and then (Prev_Token = Tok_Generic
                                  or else Prev_Token = Tok_Semicolon)
                     then
                        In_Generic_Decl := True;
                     end if;
                  when Tok_New =>
                     if Prev_Token = Tok_Is then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           if Is_Record_Type (Scope) then
                              In_Parent_Part := True;
                           end if;
                        end;
                     end if;

                  when Tok_And =>
                     if Prev_Token = Tok_Interface then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           pragma Assert (Get_Kind (Scope) = E_Interface);
                           In_Parent_Part := True;
                        end;
                     end if;

                  when Tok_Tagged =>
                     declare
                        Scope : constant Entity_Id :=
                          Get_Scope (Current_Context);
                     begin
                        pragma Assert (Is_Record_Type (Scope));
                        Set_Is_Tagged (Scope);
                        Set_Kind (Scope, E_Tagged_Record_Type);
                     end;

                  when Tok_Private =>
                     if In_Generic_Formals then
                        null;

                     elsif In_Compilation_Unit then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           E     : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);

                        begin
                           if Is_Partial_View (Scope) then
                              Set_Is_Incomplete (Scope, False);
                              Set_Is_Private (Scope);

                           elsif Present (E)
                             and then Is_Generic_Formal (E)
                           then
                              pragma Assert (Is_Standard_Entity (Scope));
                              null;

                           --  We have found the beginning of the private
                           --  part of a package spec or a concurrent type
                           --  spec

                           else
                              pragma Assert (Is_Package (Scope)
                                or else Is_Concurrent_Type_Or_Object (Scope));
                           end if;
                        end;
                     end if;

                     --  Expanded names & identifiers

                  when Tok_Id =>
                     if In_Parent_Part then
                        declare
                           E : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           pragma Assert (Is_Record_Type (E));

                           if Present (Get_Parent (E)) then

                              if Is_Full_View (E)
                                and then Is_Private (Get_Partial_View (E))
                                and then
                                  No (Get_Parent (Get_Partial_View (E)))
                              then
                                 Set_Has_Private_Parent (Get_Partial_View (E));
                              end if;

                           else
                              --  In the partial view Xref returns the parent
                              --  in the list of parents (and hence, at
                              --  current stage it is stored in the list of
                              --  progenitors). We localize it and remove it
                              --  from the list of progenitors.

                              if Is_Partial_View (E) then

                                 if Present
                                      (Get_Parent (Get_Full_View (E)))
                                 then
                                    Set_Parent (E,
                                      Get_Parent (Get_Full_View (E)));

                                 --  Derivation of untagged type. For instance:
                                 --     type Rec_A is record ...
                                 --     subtype Rec_B is Rec_A;

                                 elsif not Is_Tagged (E) then
                                    null; -- Unhandled yet???

                                 else
                                    declare
                                       Parent : Entity_Id;
                                    begin
                                       Parent :=
                                         Find_Entity
                                           (Get_Progenitors (E).all,
                                            Name => S);
                                       pragma Assert (Present (Parent));

                                       Set_Parent (E, Parent);
                                       Delete_Entity
                                         (Get_Progenitors (E).all, Parent);
                                    end;
                                 end if;

                              --  We don't know the exact location associated
                              --  with the entity in the database. Hence for
                              --  now we take a conservative approach and we
                              --  first retry the entity from the database
                              --  and use its location to retry its associated
                              --  unique high-level entity.

                              else
                                 declare
                                    Tok_Loc   : General_Location;
                                    LL_Parent : General_Entity;
                                    Parent    : Entity_Id := Atree.No_Entity;
                                    Dot_Pos   : Natural   := 0;

                                 begin
                                    if Is_Expanded_Name (S) then
                                       declare
                                          Idx : Natural;
                                       begin
                                          Parent := Find_Unique_Entity (S);
                                          Idx :=
                                            Index (S, ".", Going => Backward);
                                          Dot_Pos := Idx - S'First + 1;
                                       end;
                                    end if;

                                    if No (Parent) then
                                       Tok_Loc :=
                                         General_Location'
                                           (File   => File,
                                            Line   => Sloc_Start.Line,
                                            Column =>
                                              Visible_Column_Type
                                                (Sloc_Start.Column + Dot_Pos));

                                       LL_Parent :=
                                         Xref.Get_Entity
                                           (Db   => Context.Database,
                                            Name => Get_Short_Name (S),
                                            Loc  => Tok_Loc);

                                       --  Tolerate the case in which the
                                       --  package containing the parent
                                       --  type is not available.

                                       if Present (LL_Parent)
                                         and then not Is_Fuzzy (LL_Parent)
                                       then
                                          Parent :=
                                            Builder.Get_Unique_Entity
                                              (Context, File, LL_Parent);
                                       end if;
                                    end if;

                                    if Present (Parent) then
                                       pragma Assert (LL.Is_Type (Parent));
                                       Set_Parent (E, Parent);

                                       if Get_Progenitors (E)
                                         .Contains (Parent)
                                       then
                                          Delete_Entity
                                            (Get_Progenitors (E).all, Parent);
                                       end if;

                                       --  Complete the decoration of E

                                       if Is_Tagged (Parent) then
                                          Set_Is_Tagged (E);
                                       end if;

                                       if Present (Get_Partial_View (E))
                                         and then
                                           Is_Private (Get_Partial_View (E))
                                         and then
                                           No (Get_Parent
                                                 (Get_Partial_View (E)))
                                       then
                                          Set_Has_Private_Parent
                                            (Get_Partial_View (E));
                                       end if;

                                    end if;
                                 end;
                              end if;
                           end if;

                           In_Parent_Part := False;
                        end;

                     elsif In_Next_Entity then
                        declare
                           E : constant Entity_Id :=
                             Extended_Cursor.Entity (Cursor);

                        begin
                           if not In_Generic_Formals
                             and then Is_Compilation_Unit (E)
                           then
                              --  Given that the scope available at current
                              --  stage is not reliable this assertion is
                              --  not reliable and must be disabled???

                              --  pragma Assert (not In_Compilation_Unit);

                              In_Compilation_Unit := True;
                           end if;

                           if Is_Generic (E) then
                              pragma Assert (Present (Generic_Formals_Loc));
                              Set_Generic_Formals_Loc (E, Generic_Formals_Loc);
                              Generic_Formals_Loc := No_Location;
                              In_Generic_Formals := False;
                              In_Generic_Decl := False;

                              --  For subprograms found in generic formals
                              --  this code is erroneously decorating their
                              --  formals as generic formals???

                              declare
                                 Cursor : EInfo_List.Cursor;
                                 Formal : Entity_Id;
                              begin
                                 Cursor := Generic_Formals.First;

                                 while EInfo_List.Has_Element (Cursor) loop
                                    Formal := EInfo_List.Element (Cursor);

                                    Set_Is_Generic_Formal (Formal);

                                    --  Adding minimum decoration to
                                    --  undecorated generic formals

                                    if Get_Kind (E) = E_Unknown then
                                       Set_Kind (E, E_Generic_Formal);
                                    end if;

                                    if Get_Scope (Formal) /= E
                                      or else not
                                        Get_Generic_Formals
                                          (E).Contains (Formal)
                                    then
                                       Remove_From_Scope (Formal);
                                       Append_Generic_Formal (E, Formal);
                                       Set_Scope (Formal, E);
                                    end if;

                                    EInfo_List.Next (Cursor);
                                 end loop;
                              end;

                              Generic_Formals.Clear;
                              Decorate_Scope (E);

                           elsif In_Generic_Formals then
                              Generic_Formals.Append (E);

                           else
                              case Prev_Token is
                              when Tok_Subtype =>
                                 pragma Assert (LL.Is_Type (E));
                                 Set_Is_Subtype (E);

                              when Tok_Protected =>
                                 Set_Kind (E, E_Single_Protected);
                                 Set_Is_Incomplete (E, False);
                                 Remove_Full_View (E);

                              when others =>
                                 null;
                              end case;

                              Decorate_Scope (E);
                           end if;
                        end;

                     elsif Prev_Token = Tok_Renames then
                        declare
                           E : Entity_Id;
                        begin
                           if Present
                                (Get_Current_Entity (Current_Context))
                             and then
                                Get_Kind (Get_Current_Entity (Current_Context))
                                  /= E_Formal
                           then
                              E := Get_Current_Entity (Current_Context);
                           else
                              E := Get_Scope (Current_Context);
                           end if;

                           if No (Get_Alias (E)) then
                              declare
                                 Current_Loc : constant General_Location :=
                                   General_Location'(File   => File,
                                                     Line   => Sloc_Start.Line,
                                                     Column => Visible_Column
                                                       (Sloc_Start.Column));
                                 Entity      : constant Entity_Id :=
                                   Get_Entity
                                     (Context               => Context,
                                      Name                  => S,
                                      Loc                   => Current_Loc);

                              begin
                                 if Present (Entity) then
                                    Set_Alias (E, Entity);

                                 --  Adding a minimum decoration otherwise
                                 --  which will be used to handle the scopes

                                 else
                                    Set_Is_Alias (E);
                                 end if;
                              end;
                           end if;

                        end;
                     end if;

                  when Tok_Semicolon =>
                     if Par_Count = 0 then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           E : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if End_Decl_Found then
                              if Is_Record_Type (Scope)
                                and then Is_Private (Scope)
                                and then Is_Full_View (Scope)
                                and then Present (Get_Parent (Scope))
                                and then
                                  No (Get_Parent (Get_Partial_View (Scope)))
                              then
                                 Set_Has_Private_Parent
                                   (Get_Partial_View (Scope));
                              end if;

                              --  If we have processed a single declaration
                              --  then E references it; if we have processed
                              --  all the formals of a subprogram or entry
                              --  then there is no current entity available
                              --  in the scope

                              declare
                                 Loc : constant General_Location :=
                                   General_Location'
                                     (File => File,
                                      Line => Sloc_Start.Line,
                                      Column =>
                                        Visible_Column (Sloc_Start.Column));
                              begin
                                 if Present (E)
                                   and then Get_Kind (E) /= E_Formal
                                 then
                                    if not Enhancements
                                      and then
                                        (Get_Kind (E) = E_Variable
                                         or else Get_Kind (E) = E_Component
                                         or else Is_Generic_Formal (E)
                                         or else Get_Kind (E)
                                                   = E_Generic_Formal
                                         or else
                                           (LL.Is_Type (E)
                                             and then not Is_Record_Type (E)
                                             and then not
                                               Is_Concurrent_Type_Or_Object
                                                 (E)))
                                    then
                                       null;

                                    --  No action needed if this attribute
                                    --  is already set. This case occurs
                                    --  with pragmas located after E.

                                    elsif
                                        Present
                                          (Get_End_Of_Profile_Location (E))
                                      or else
                                        Present
                                         (Get_End_Of_Syntax_Scope_Loc (E))
                                    then
                                       null;
                                    elsif Is_Subprogram_Or_Entry (E) then
                                       Set_End_Of_Profile_Location (E, Loc);
                                    else
                                       Set_End_Of_Syntax_Scope_Loc (E, Loc);
                                    end if;

                                 else
                                    --  Probably I should simplify these two
                                    --  attributes in a single one???

                                    if not Enhancements
                                      and then
                                        (Present
                                           (LL.Get_Instance_Of (Scope))
                                         or else
                                           Get_Kind (Scope) = E_Access_Type)
                                    then
                                       null;

                                    --  No action needed if this attribute
                                    --  is already set. This case occurs
                                    --  with pragmas located after Scope.

                                    elsif
                                      Present
                                        (Get_End_Of_Profile_Location
                                           (Scope))
                                      or else
                                        Present
                                          (Get_End_Of_Syntax_Scope_Loc
                                             (Scope))
                                    then
                                       null;

                                    elsif
                                      Is_Subprogram_Or_Entry (Scope)
                                    then
                                       Set_End_Of_Profile_Location
                                         (Scope, Loc);
                                    else
                                       Set_End_Of_Syntax_Scope_Loc
                                         (Scope, Loc);
                                    end if;
                                 end if;
                              end;
                           end if;
                        end;
                     end if;

                  when others =>
                     null;
               end case;
            end Complete_Decoration;

            ----------------
            -- Handle_Doc --
            ----------------

            procedure Handle_Doc is

               procedure Set_Doc_After (E : Entity_Id);
               procedure Set_Doc_After_Previous_Entity;
               procedure Set_Doc_Before (E : Entity_Id);
               procedure Set_Doc_Before_Current_Entity;

               -------------------
               -- Set_Doc_After --
               -------------------

               procedure Set_Doc_After (E : Entity_Id) is
               begin
                  if Present (Doc)
                    and then Present (E)
                    and then No (Get_Doc_After (E))
                  then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));
                  end if;
               end Set_Doc_After;

               -----------------------------------
               -- Set_Doc_After_Previous_Entity --
               -----------------------------------

               procedure Set_Doc_After_Previous_Entity is
                  Prev_Entity : constant Entity_Id :=
                    Extended_Cursor.Prev_Entity (Cursor);
               begin
                  if Present (Doc) then
                     if Present (Prev_Entity) then
                        Set_Doc_After (Prev_Entity);
                     else
                        declare
                           Scope : constant Entity_Id :=
                              Get_Scope (Current_Context);
                        begin
                           if Is_Package (Scope)
                             or else Is_Concurrent_Type_Or_Object (Scope)
                           then
                              Set_Doc_After (Scope);
                           end if;
                        end;
                     end if;
                  end if;
               end Set_Doc_After_Previous_Entity;

               --------------------
               -- Set_Doc_Before --
               --------------------

               procedure Set_Doc_Before (E : Entity_Id) is
               begin
                  if Present (Doc)
                    and then Present (E)
                    and then No (Get_Doc_Before (E))
                  then
                     Set_Doc_Before (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));
                  end if;
               end Set_Doc_Before;

               -----------------------------------
               -- Set_Doc_Before_Current_Entity --
               -----------------------------------

               procedure Set_Doc_Before_Current_Entity is
               begin
                  if In_Next_Entity
                    and then Present (Doc)
                  then
                     Set_Doc_Before (Get_Current_Entity (Current_Context),
                        Comment_Result'
                          (Text       => Doc,
                           Start_Line => Doc_Start_Line));
                  end if;
               end Set_Doc_Before_Current_Entity;

            --  Start of processing for Handle_Doc

            begin
               case Token is

                  when Tok_Private =>
                     if Present (Doc) then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           Curr_Entity_In_Scope : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Is_Package (Scope)
                             or else Is_Concurrent_Type_Or_Object (Scope)
                           then
                              Set_Doc_After (Curr_Entity_In_Scope);
                           end if;

                           Clear_Doc;
                        end;
                     end if;

                  --  Expanded names & identifiers

                  when Tok_Id =>
                     if In_Next_Entity
                       and then Present (Doc)
                     then
                        declare
                           E : constant Entity_Id :=
                             Extended_Cursor.Entity (Cursor);
                           Prev_E : Entity_Id;

                        begin
                           if No (Get_Prev_Entity_In_Scope
                                   (Current_Context))
                           then
                              declare
                                 Scope : constant Entity_Id :=
                                   Get_Scope (Current_Context);
                              begin
                                 if Is_Concurrent_Type_Or_Object (Scope)
                                   or else
                                     (Is_Package (Scope)
                                        and then not
                                      Is_Standard_Entity (Scope))
                                 then
                                    Prev_E := Scope;
                                    --  Do not attach the comment to the
                                    --  previous line if it precedes the
                                    --  current entity.

                                    if Doc_End_Line
                                      /= LL.Get_Location (E).Line - 1
                                    then
                                       Set_Doc_After (Scope);
                                    end if;
                                 end if;
                              end;
                           else
                              Prev_E :=
                                Get_Prev_Entity_In_Scope (Current_Context);

                              if Get_Kind (E) = E_Formal then
                                 Set_Doc_After_Previous_Entity_In_Scope;

                              --  Do not attach the comment to the previous
                              --  line if it precedes the current entity.

                              elsif Doc_End_Line
                                /= LL.Get_Location (E).Line - 1
                              then
                                 Set_Doc_After_Previous_Entity_In_Scope;
                              end if;
                           end if;

                           --  Do not attach the comment to the current entity
                           --  line if continuates the previous entity.

                           if No (Prev_E)
                             or else
                               Doc_Start_Line
                                 /= LL.Get_Location (Prev_E).Line + 1
                           then
                              Set_Doc_Before (E);
                           end if;

                           --  Reset the documentation
                           Clear_Doc;
                        end;
                     end if;

                  when Tok_Is  =>
                     Clear_Doc;

                  when Tok_Procedure |
                       Tok_Function  |
                       Tok_Entry =>
                     Set_Doc_Before_Current_Entity;

                  when Tok_Right_Paren =>
                     if Present (Doc) then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           Curr_Entity_In_Scope : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Is_Subprogram_Or_Entry (Scope)
                             and then
                               Get_Kind (Curr_Entity_In_Scope) = E_Formal
                           then
                              Set_Doc_After (Curr_Entity_In_Scope);
                           end if;

                           Clear_Doc;
                        end;
                     end if;

                  when Tok_Semicolon =>
                     if Par_Count = 0
                       and then Present (Doc)
                     then
                        declare
                           Prev_Entity_In_Scope : constant Entity_Id :=
                             Get_Prev_Entity_In_Scope (Current_Context);
                        begin
                           if Present (Prev_Entity_In_Scope)
                             and then
                               (Is_Package (Prev_Entity_In_Scope)
                                  or else Is_Concurrent_Type_Or_Object
                                            (Prev_Entity_In_Scope))
                           then
                              Set_Doc_After_Previous_Entity;
                           end if;

                           Clear_Doc;
                        end;
                     end if;

                  when Tok_End =>
                     declare
                        Scope : constant Entity_Id :=
                          Get_Scope (Current_Context);
                     begin
                        pragma Assert
                          (Is_Record_Type (Scope)
                           or else Is_Package (Scope)
                           or else Is_Concurrent_Type_Or_Object (Scope));

                        if Present (Doc) then
                           declare
                              Curr_Entity_In_Scope : constant Entity_Id :=
                                Get_Current_Entity (Current_Context);
                           begin
                              if Present (Curr_Entity_In_Scope) then
                                 Set_Doc_After (Curr_Entity_In_Scope);
                              end if;

                              Clear_Doc;
                           end;
                        end if;
                     end;

                  when others =>
                     null;
               end case;
            end Handle_Doc;

            ---------------------
            -- Handle_Entities --
            ---------------------

            In_Debug_File : constant Boolean :=
              File.Base_Name = " disabled";

            procedure Handle_Scopes (End_Decl_Found : Boolean) is
               procedure Do_Breakpoint;
               procedure Do_Breakpoint is
               begin
                  if False
                    and then File.Base_Name = " disabled"
                    and then Sloc_Start.Line = 1
                  then
                     Print_State;
                  end if;
               end Do_Breakpoint;

               procedure Do_Exit;
               procedure Do_Exit is
               begin
                  if In_Debug_File then
                     GNAT.IO.Put_Line
                       (Scope_Tab (1 .. 2 * Scope_Level)
                        & Sloc_Start.Line'Img
                        & ":"
                        & Sloc_Start.Column'Img
                        & " ---------------- "
                        & Get_Short_Name
                           (Get_Scope (Current_Context)));
                  end if;

                  if Is_Generic (Get_Scope (Current_Context)) then
                     Generics_Nesting_Level := Generics_Nesting_Level - 1;
                  end if;

                  if not In_Generic_Formals
                    and then Is_Compilation_Unit (Get_Scope (Current_Context))
                  then
                     Disable_Enter_Scope;
                  end if;

                  Exit_Scope;
                  Scope_Level := Scope_Level - 1;
               end Do_Exit;

            begin
               case Token is

                  when Tok_Use =>
                     pragma Assert (Par_Count = 0);
                     if not In_Representation_Clause then
                        In_Skipped_Declaration := True;
                     end if;

                  --  We have to increment the generics nesting level here
                  --  (instead of when we process the entity) to be able to
                  --  generate their corresponding entity (required to
                  --  workaround the problem of their missing entity)

                  when Tok_Generic =>
                     Generics_Nesting_Level := Generics_Nesting_Level + 1;

                  when Tok_For =>
                     if Par_Count = 0 then
                        In_Representation_Clause := True;
                     end if;

                  when Tok_Record =>
                     if In_Representation_Clause then
                        if Prev_Token = Tok_Use then
                           In_Record_Representation_Clause := True;
                        elsif Prev_Token = Tok_End then
                           In_Record_Representation_Clause := False;
                        end if;
                     end if;

                  when Tok_Pragma =>
                     In_Pragma := True;

                  --  Handle variants of record types adding again their
                  --  scope (for homogeneity in the management of their
                  --  closing syntax scope)

                  when Tok_Case =>

                     pragma Assert
                       (Is_Record_Type (Get_Scope (Current_Context)));

                     --  "end case"

                     if Prev_Token = Tok_End then
                        Nested_Variants_Count := Nested_Variants_Count - 1;
                     else
                        Nested_Variants_Count := Nested_Variants_Count + 1;
                     end if;

                  --  Expanded names & identifiers

                  when Tok_Id =>
                     Do_Breakpoint;

                     if In_Pragma then
                        null;

                     elsif Prev_Token = Tok_End then
                        null;

                     elsif In_Next_Entity then
                        declare
                           E : constant Entity_Id :=
                             Extended_Cursor.Entity (Cursor);

                        begin
                           if Has_Scope (E) then
                              Enter_Scope (E);
                              Scope_Level := Scope_Level + 1;

                              if In_Debug_File then
                                 GNAT.IO.Put_Line
                                   (Scope_Tab (1 .. 2 * Scope_Level)
                                    & Sloc_Start.Line'Img
                                    & ":"
                                    & Sloc_Start.Column'Img
                                    & ": "
                                    & Get_Short_Name (E));
                              end if;
                           end if;

                           Extended_Cursor.Next_Entity (Cursor);
                        end;
                     end if;

                  when Tok_Right_Paren |
                       Tok_Return      =>
                     null;

                  when Tok_Semicolon =>
                     Do_Breakpoint;

                     if Par_Count = 0 and then In_Pragma then
                        In_Pragma := False;

                     elsif Par_Count = 0
                       and then In_Representation_Clause
                       and then not In_Record_Representation_Clause
                     then
                        In_Representation_Clause := False;

                     elsif Par_Count = 0
                       and then Nested_Variants_Count /= 0
                     then
                        null;

                     elsif Par_Count = 0
                       and then In_Skipped_Declaration
                     then
                        In_Skipped_Declaration := False;

                     elsif Par_Count = 0 then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           E : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if End_Decl_Found then
                              begin
                                 if Is_Partial_View (Scope) then
                                    pragma Assert (No (E)
                                      or else Get_Kind (E) = E_Discriminant);

                                    Do_Exit;

                                 elsif No (E)
                                   and then Has_Scope (Scope)
                                 then
                                    --  Skip "null;" found in a component of a
                                    --  record type declaration

                                    if Is_Record_Type (Scope)
                                      and then Prev_Token = Tok_Null
                                    then
                                       null;
                                    else
                                       Do_Exit;
                                    end if;

                                 --  For packages we exit from the scope when
                                 --  we see their "end" token

                                 elsif Is_Package (Scope) then
                                    null;

                                 elsif Is_Subprogram_Or_Entry (Scope) then
                                    Do_Exit;

                                 --  Handle taft ammendment

                                 elsif Is_Concurrent_Type_Or_Object (Scope)
                                   and then not Has_Entities (Scope)
                                 then
                                    Do_Exit;

                                 elsif Get_Kind (Scope) = E_Access_Type then
                                    Do_Exit;

                                 elsif
                                   Get_Kind (Scope) = E_Enumeration_Type
                                 then
                                    Do_Exit;

                                 elsif Present
                                         (LL.Get_Instance_Of (Scope))
                                 then
                                    Do_Exit;

                                 elsif Is_Alias (Scope) then
                                    Do_Exit;
                                 end if;

                              exception
                                 when others =>
                                    Print_State;
                                    raise;
                              end;
                           end if;
                        end;
                     end if;

                  when Tok_End =>
                     pragma Assert (not In_Pragma);
                     Do_Breakpoint;

                     if In_Record_Representation_Clause then
                        null;
                     elsif Nested_Variants_Count /= 0 then
                        null;
                     else
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           pragma Assert
                             (Is_Record_Type (Scope)
                              or else Is_Package (Scope)
                              or else Is_Concurrent_Type_Or_Object (Scope));

                           Do_Exit;
                        end;
                     end if;

                  when others =>
                     null;
               end case;
            end Handle_Scopes;

            --------------------
            -- Handle_Sources --
            --------------------

            procedure Handle_Sources (End_Decl_Found : Boolean) is

               procedure Append_Src
                 (Text : String; With_Tabulation : Boolean := False);

               procedure Clear_Src;

               ----------------
               -- Append_Src --
               ----------------

               procedure Append_Src
                 (Text : String; With_Tabulation : Boolean := False) is
               begin
                  if not With_Tabulation then
                     Append_Sources (Text);
                  else
                     declare
                        Spaces : constant String (1 .. Sloc_Start.Column - 1)
                          := (others => ' ');
                     begin
                        Append_Sources (Spaces & Text);
                     end;
                  end if;

                  Append_Plain_Sources (Text);
               end Append_Src;

               ---------------
               -- Clear_Src --
               ---------------

               procedure Clear_Src is
               begin
                  Clear_Sources;
               end Clear_Src;

            --  Start of processing for Handle_Sources

            begin
               --  Append all text between previous call and current one

               if Last_Idx /= 0 then
                  Append_Sources
                    (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
                  Append_Plain_Sources
                    (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;

               case Token is
                  when Tok_Id =>
                     if In_Next_Entity then
                        declare
                           E : constant Entity_Id :=
                             Extended_Cursor.Entity (Cursor);
                        begin
                           if Get_Kind (E) = E_Variable then
                              Clear_Src;
                           end if;
                        end;
                     end if;

                     Append_Src (S);

                  when Tok_Task      |
                       Tok_Protected =>
                     declare
                        Scope : constant Entity_Id :=
                          Get_Scope (Current_Context);
                     begin
                        if Get_Kind (Scope) = E_Interface then
                           Append_Src (S);
                        else
                           Clear_Src;
                           Clear_Plain_Sources;

                           declare
                              Spaces : constant String
                                (1 .. Sloc_Start.Column - 1)
                                := (others => ' ');
                           begin
                              Append_Plain_Sources (Spaces & S);
                           end;
                        end if;
                     end;

                  when Tok_Type =>
                     if Prev_Token = Tok_Task
                       or else Prev_Token = Tok_Protected
                     then
                        Append_Src (S);
                     else
                        Clear_Src;
                        Append_Src (S, With_Tabulation => True);
                     end if;

                  when Tok_Procedure |
                       Tok_Function  |
                       Tok_Entry =>
                     if Par_Count /= 0 then
                        Append_Src (S);
                     else
                        Clear_Src;
                        Append_Src (S, With_Tabulation => True);
                     end if;

                  when Tok_Package =>
                     Clear_Src;
                     Append_Src (S);

                  when Tok_Semicolon =>
                     Append_Src (S);

                     if Par_Count = 0
                       and then End_Decl_Found
                     then
                        declare
                           Scope : constant Entity_Id :=
                                     Get_Scope (Current_Context);
                           E : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Present (E)
                             and then Get_Kind (E) /= E_Formal
                           then
                              if Is_Concurrent_Type_Or_Object (E) then
                                 Set_Src (E, Printout_Plain);
                                 Clear_Plain_Sources;

                                 Clear_Src;

                              elsif not Is_Package (E)
                                and then Get_Kind (E) /= E_Component
                              then
                                 --  ??? For now we skip adding any extra
                                 --  documentation (most probably pragmas)

                                 if Present (Get_Src (E)) then
                                    null;
                                 else
                                    Set_Src (E, Printout);
                                 end if;

                                 Clear_Src;
                              end if;

                           elsif Present (LL.Get_Instance_Of (Scope)) then
                              Set_Src (Scope, Printout);
                              Clear_Src;

                           elsif Is_Partial_View (Scope) then
                              Set_Src (Scope, Printout);
                              Clear_Src;

                           elsif Is_Subprogram_Or_Entry (Scope) then
                              Set_Src (Scope, Printout);
                              Clear_Src;

                           elsif Is_Record_Type (Scope)
                           --    and then not Has_Entities (Scope)
                           then
                              Set_Src (Scope, Printout);
                              Clear_Src;

                           elsif Get_Kind (Scope) = E_Access_Type then
                              Set_Src (Scope, Printout);
                              Clear_Src;
                           end if;
                        end;
                     end if;

                  when others =>
                     Append_Src (S);
                     null;
               end case;
            end Handle_Sources;

            -------------------
            -- Handle_Tokens --
            -------------------

            procedure Handle_Tokens is
            begin
               case Entity is
                  when Type_Text =>
                     null;

                  when Block_Text      |
                       Identifier_Text =>
                     Prev_Token := Token;
                     Token := Tok_Id;

                  when Keyword_Text    =>
                     Prev_Token := Token;
                     Token := Get_Token (S);
                     Set_Token_Seen (Current_Context, Token);

                     case Token is
                        when Tok_Is =>
                           In_Definition := True;

                        when Tok_Procedure |
                             Tok_Function  |
                             Tok_Entry =>
                           In_Item_Decl := True;

                        when Tok_Package =>
                           null;

                        when Tok_End =>
                           if not In_Item_Decl
                             and then In_Definition
                           then
                              Set_End_Decl_Found (Current_Context);
                           end if;

                        when Tok_Type =>
                           null;

                        when others =>
                           null;
                     end case;

                  when Operator_Text  =>
                     Prev_Token := Token;
                     Token := Tok_Operator;

                     if S = "(" then
                        Token := Tok_Left_Paren;
                        Par_Count := Par_Count + 1;

                     elsif S = ")" then
                        Token := Tok_Right_Paren;
                        Par_Count := Par_Count - 1;

                     elsif S = ";" then
                        Token := Tok_Semicolon;

                        if Par_Count = 0 then
                           Set_End_Decl_Found (Current_Context);

                           --  ???may fail with access to subprogram formals
                           if Tok_Subprogram_Seen (Current_Context) then
                              Reset_Tok_Subprogram_Seen (Current_Context);
                           end if;

                           In_Item_Decl := False;
                        end if;
                     end if;

                  when others =>
                     null;
               end case;
            end Handle_Tokens;

            ---------------
            -- Has_Scope --
            ---------------

            function Has_Scope (E : Entity_Id) return Boolean is
            begin
               return Is_Package (E)
                 or else Is_Subprogram_Or_Entry (E)
                 or else Is_Record_Type (E)
                 or else Is_Concurrent_Type_Or_Object (E)
                  --  Include access types to handle the formals of access to
                  --  subprograms
                 or else Get_Kind (E) = E_Access_Type
                 or else Get_Kind (E) = E_Enumeration_Type;
            end Has_Scope;

            --------------------
            -- In_Next_Entity --
            --------------------

            function In_Next_Entity return Boolean is
               Next_Entity : constant Entity_Id :=
                 Extended_Cursor.Entity (Cursor);
               Loc : General_Location;
            begin
               if No (Next_Entity) then
                  return False;
               end if;

               Loc := LL.Get_Location (Extended_Cursor.Entity (Cursor));

               return Sloc_Start.Line = Loc.Line
                 and then Sloc_End.Line = Loc.Line
                 and then Natural (Loc.Column) >= Sloc_Start.Column
                 and then Natural (Loc.Column) <= Sloc_End.Column;
            end In_Next_Entity;

            ----------------------------------
            -- Set_Doc_After_Current_Entity --
            ----------------------------------

            procedure Set_Doc_After_Current_Entity is
               Current : constant Entity_Id :=
                 Get_Current_Entity (Current_Context);
            begin
               if Present (Doc)
                 and then Present (Current)
                 and then No (Get_Doc_After (Current))
               then
                  Set_Doc_After (Current,
                    Comment_Result'
                      (Text       => Doc,
                       Start_Line => Doc_Start_Line));
               end if;
            end Set_Doc_After_Current_Entity;

            --------------------------------------------
            -- Set_Doc_After_Previous_Entity_In_Scope --
            --------------------------------------------

            procedure Set_Doc_After_Previous_Entity_In_Scope is
               Prev_Entity_In_Scope : constant Entity_Id :=
                 Get_Prev_Entity_In_Scope (Current_Context);
            begin
               if Present (Doc)
                 and then Present (Prev_Entity_In_Scope)
                 and then No (Get_Doc_After (Prev_Entity_In_Scope))
               then
                  Set_Doc_After (Prev_Entity_In_Scope,
                    Comment_Result'
                      (Text       => Doc,
                       Start_Line => Doc_Start_Line));
               end if;
            end Set_Doc_After_Previous_Entity_In_Scope;

            -----------------
            -- Print_State --
            -----------------

            procedure Print_State is
               With_Doc : constant Boolean := True;
            begin
               GNAT.IO.Put_Line ("----------------------------");
               GNAT.IO.Put_Line (+File.Full_Name);

               if With_Doc then
                  if Present (Printout) then
                     GNAT.IO.Put_Line ("--- Printout");
                     GNAT.IO.Put ('"');
                     GNAT.IO.Put (To_String (Printout));
                     GNAT.IO.Put ('"');
                     GNAT.IO.New_Line;
                  end if;

                  if Doc_Start_Line /= No_Line then
                     GNAT.IO.Put_Line
                       ("Doc_Start_Line : " & Doc_Start_Line'Img);
                     GNAT.IO.Put_Line
                       ("  Doc_End_Line : " & Doc_End_Line'Img);

                     GNAT.IO.Put_Line
                       ('"' & To_String (Doc) & '"');
                  end if;
               end if;

               GNAT.IO.Put
                 ("Sloc: "
                  & To_String (Sloc_Start.Line)
                  & ":"
                  & To_String (Sloc_Start.Column));
               GNAT.IO.Put (" .. ");
               GNAT.IO.Put
                 (To_String (Sloc_End.Line)
                  & ":"
                  & To_String (Sloc_End.Column));

               GNAT.IO.Put      (" " & Entity'Img & " ");
               GNAT.IO.Put_Line ('"' & S & '"');

               if In_Next_Entity then
                  GNAT.IO.Put_Line ("In_Tree_Entity");
               end if;

               --  --------------------- Scopes
               Print_Scopes;

               declare
                  Next_E : constant Entity_Id :=
                    Extended_Cursor.Entity (Cursor);
                  Get_Prev_Entity : constant Entity_Id :=
                    Extended_Cursor.Prev_Entity (Cursor);
               begin
                  GNAT.IO.Put_Line ("--- Extended cursor");

                  if Present (Get_Prev_Entity) then
                     Print_Entity (Get_Prev_Entity, "Prev_Entity:");
                  end if;

                  if Present (Next_E) then
                     Print_Entity (Next_E, "Next_Entity:");
                  end if;
               end;
            end Print_State;

            procedure Build_Missing_Entity;
            procedure Build_Missing_Entity is
               Loc   : constant General_Location :=
                 (File, Sloc_Start.Line, Visible_Column (Sloc_Start.Column));
               Lang  : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
               New_E : constant Entity_Id :=
                 New_Internal_Entity (Context, Lang, S);
            begin
               LL.Set_Location (New_E, Loc);

               case Prev_Token is
                  when Tok_Package =>
                     Set_Kind (New_E, E_Generic_Package);
                  when Tok_Procedure =>
                     Set_Kind (New_E, E_Generic_Procedure);
                  when Tok_Function =>
                     Set_Kind (New_E, E_Generic_Function);
                  when others =>
                     raise Program_Error;
               end case;

               New_Entities.Append (New_E);

               --  Place this internal entity as the next entity of the
               --  entities iterator

               Extended_Cursor.Set_Next_Entity (Cursor, New_E);
               Replace_Current_Entity (Current_Context, New_E);
            end Build_Missing_Entity;

         begin
            --  Accumulate documentation found in consecutive comments

            if Entity = Comment_Text
              or else Entity = Annotated_Comment_Text
            then
               Accumulate_Comments;
               return False; -- Continue
            end if;

            Handle_Tokens;

            declare
               End_Decl_Found : constant Boolean :=
                 Get_End_Decl_Found (Current_Context);
            begin
               declare
                  E : Entity_Id :=
                    Extended_Cursor.Entity (Cursor);
               begin
                  --  Skip entity internally generated by the compiler

                  while Present (E)
                    and then
                      (Sloc_Start.Line > LL.Get_Location (E).Line
                         or else
                           (Sloc_Start.Line = LL.Get_Location (E).Line
                              and then
                            Sloc_Start.Column
                               > Natural (LL.Get_Location (E).Column)))
                  loop
                     Extended_Cursor.Next_Entity (Cursor);
                     E := Extended_Cursor.Entity (Cursor);
                  end loop;

                  if In_Next_Entity then
                     Set_Current_Entity (Current_Context, E);

                  elsif Token = Tok_Id
                    and then In_Generic_Decl
                    and then
                      (Prev_Token = Tok_Package
                         or else Prev_Token = Tok_Procedure
                         or else Prev_Token = Tok_Function)
                  then
                     pragma Assert (Generics_Nesting_Level > 0);
                     Build_Missing_Entity;
                  end if;
               end;

               if not In_Pragma then
                  Complete_Decoration (End_Decl_Found);

                  Handle_Doc;
                  --  Processes: Tok_Id, Tok_End, Tok_Semicolon
                  --    Tok_Is, Tok_Procedure, Tok_Function, Tok_Entry

                  Handle_Sources (End_Decl_Found);
               end if;

               Handle_Scopes (End_Decl_Found);
               --  Processes: Tok_Id, Tok_Semicolon (End_Decl)

               if End_Decl_Found then
                  Reset_End_Decl_Found (Current_Context);
               end if;

               return False; --  Continue
            end;
         end CB;

         ---------------------
         -- Extended_Cursor --
         ---------------------

         package body Extended_Cursor is
            Saved_Next_Entity : Entity_Id := Atree.No_Entity;

            procedure Update_Entity (Cursor : in out Extended_Cursor);
            procedure Update_Entity (Cursor : in out Extended_Cursor) is
            begin
               if EInfo_List.Has_Element (Cursor.Cursor) then
                  Cursor.Element := EInfo_List.Element (Cursor.Cursor);
               else
                  Cursor.Element := Atree.No_Entity;
               end if;
            end Update_Entity;

            function Has_Entity (Cursor : Extended_Cursor) return Boolean is
            begin
               return EInfo_List.Has_Element (Cursor.Cursor);
            end Has_Entity;

            function Entity (Cursor : Extended_Cursor) return Entity_Id is
            begin
               return Cursor.Element;
            end Entity;

            function Prev_Entity (Cursor : Extended_Cursor) return Entity_Id
            is
            begin
               return Cursor.Prev_Element;
            end Prev_Entity;

            procedure Next_Entity (Cursor : in out Extended_Cursor) is
            begin
               if Present (Saved_Next_Entity) then
                  Cursor.Element := Saved_Next_Entity;
                  Saved_Next_Entity := Atree.No_Entity;
               else
                  Cursor.Prev_Element := Cursor.Element;
                  EInfo_List.Next (Cursor.Cursor);
                  Update_Entity (Cursor);
               end if;
            end Next_Entity;

            procedure Set_Next_Entity
              (Cursor : in out Extended_Cursor;
               Entity : Entity_Id) is
            begin
               pragma Assert (No (Saved_Next_Entity));
               Saved_Next_Entity := Cursor.Element;
               Cursor.Element := Entity;
            end Set_Next_Entity;

            procedure Initialize (Cursor : in out Extended_Cursor) is
            begin
               Cursor.Cursor := File_Entities.All_Entities.First;
               Update_Entity (Cursor);
            end Initialize;

         end Extended_Cursor;

         --  Start of processing for Parse_Ada_File

      begin

         if False and then File.Base_Name = "s-htable.ads" then
            GNAT.IO.Put_Line
              ("------------- File_Entities.All_Entities (begin)");

            pv (File_Entities.All_Entities);

            GNAT.IO.Put_Line
              ("------------- File_Entities.All_Entities (end)");
         end if;

         Extended_Cursor.Initialize (Cursor);

         if Extended_Cursor.Has_Entity (Cursor) then
            declare
               E : constant Entity_Id := Extended_Cursor.Entity (Cursor);
            begin
               pragma Assert
                 (Present (Get_Scope (E))
                    and then
                      (Get_Kind (E) = E_Generic_Formal
                         or else Is_Generic_Formal (E)
                         or else Is_Standard_Entity (Get_Scope (E))));
               --  Set the next entity to look for in the parser

               --  Enter the entity of the Standard scope

               declare
                  S : Entity_Id := Get_Scope (E);
               begin
                  while not Is_Standard_Entity (S) loop
                     pragma Assert (Present (Get_Scope (S)));
                     S := Get_Scope (S);
                  end loop;

                  Enable_Enter_Scope;
                  Enter_Scope (S);

                  Parse_Entities
                    (Lang, Buffer.all (Buffer'First .. Buffer'Last),
                     CB'Unrestricted_Access);

                  if Natural (New_Entities.Length) > 0 then
                     for E of New_Entities loop
                        File_Entities.All_Entities.Append (E);
                     end loop;

                     EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);
                     --  pv (File_Entities.All_Entities);
                  end if;

                  pragma Assert (Get_Scope (Current_Context) = S);
                  Exit_Scope;
               end;
            end;
         end if;
      end Parse_Ada_File;

      -------------------
      -- Previous_Word --
      -------------------

      procedure Previous_Word
        (Index           : Natural;
         Prev_Word_Begin : out Natural;
         Prev_Word_End   : out Natural)
      is
         Idx : Natural := Index;
      begin
         --  Skip current word
         while Idx > Buffer'First
           and then (Buffer (Idx) /= ' '
                     and then Buffer (Idx) /= ASCII.LF)
         loop
            Idx := Idx - 1;
         end loop;

         --  Skip spaces and line terminators

         while Idx > Buffer'First
           and then (Buffer (Idx) = ' '
                     or else Buffer (Idx) = ASCII.LF)
         loop
            Idx := Idx - 1;
         end loop;
         Prev_Word_End := Idx;

         while Idx > Buffer'First
           and then (Buffer (Idx) /= ' '
                     and then Buffer (Idx) /= ASCII.LF)
         loop
            Idx := Idx - 1;
         end loop;
         Prev_Word_Begin := Idx + 1;
      end Previous_Word;

      ---------------------
      -- Search_Backward --
      ---------------------

      function Search_Backward
        (Lines_Skipped : out Natural;
         Buffer : GNAT.Strings.String_Access;
         From   : Natural;
         Word_1 : String;
         Word_2 : String := "") return Natural
      is
         Lowercase_Word_1 : constant String := To_Lower (Word_1);
         Lowercase_Word_2 : constant String := To_Lower (Word_2);
         Idx              : Natural := From;
         Prev_Word_Begin  : Natural := Buffer.all'First;
         Prev_Word_End    : Natural;

      begin
         Lines_Skipped := 0;

         --  Locate the beginning of the type declaration. This is a naive
         --  approach used in the prototype since we do not skip comments.
         --  A backward parser is required here. Must be improved???

         loop
            while Idx > Buffer.all'First
              and then (Buffer (Idx) = ' '
                          or else Buffer (Idx) = ASCII.LF)
            loop
               if Buffer (Idx) = ASCII.LF then
                  Lines_Skipped := Lines_Skipped + 1;
               end if;

               Idx := Idx - 1;
            end loop;
            Prev_Word_End := Idx;

            while Idx > Buffer.all'First
              and then (Buffer (Idx) /= ' '
                          and then Buffer (Idx) /= ASCII.LF)
            loop
               Idx := Idx - 1;
            end loop;
            Prev_Word_Begin := Idx + 1;

            exit when Idx = Buffer.all'First
              or else
                To_Lower (Buffer.all (Prev_Word_Begin .. Prev_Word_End))
                  = Lowercase_Word_1

              or else (Word_2 /= ""
                and then
                  To_Lower (Buffer.all (Prev_Word_Begin .. Prev_Word_End))
                    = Lowercase_Word_2);
         end loop;

         return Prev_Word_Begin;
      end Search_Backward;

      --------------------------
      -- Skip_Blanks_Backward --
      --------------------------

      function Skip_Blanks_Backward
        (Buffer : GNAT.Strings.String_Access;
         Index  : Natural) return Natural
      is
         Idx : Natural := Index;
      begin
         while Idx > Buffer'First
           and then Buffer (Idx) = ' '
         loop
            Idx := Idx - 1;
         end loop;

         return Idx;
      end Skip_Blanks_Backward;

      ------------------
      -- Swap_Buffers --
      ------------------

      procedure Swap_Buffers is
         Tmp : constant GNAT.Strings.String_Access := Buffer;
      begin
         Buffer           := C_Headers_Buffer;
         C_Headers_Buffer := Tmp;

         Buffers_Swapped  := True;
      end Swap_Buffers;

   --  Start of processing for Add_Documentation_From_Sources

   begin
      EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);

      Buffer := File.Read_File;

      if Is_Spec_File (Context.Kernel, File)
        and then Context.Options.Process_Bodies
      then
         declare
            P_Tree : Project_Tree_Access renames Context.Kernel.Registry.Tree;

         begin
            Body_File := P_Tree.Other_File (File);

            if Body_File /= File and then Is_Regular_File (Body_File) then
               Buffer_Body := Body_File.Read_File;
            else
               Body_File := No_File;
            end if;
         end;
      end if;

      if In_Ada_Lang then
         Parse_Ada_File (Buffer);

         Prev_Comment_Line := 0;
         For_All (File_Entities.All_Entities, Ada_Set_Doc'Access);
         For_All (File_Entities.All_Entities, Filter_Doc'Access);

      else pragma Assert (In_C_Lang);
         For_All (File_Entities.All_Entities, CPP_Get_Source'Access);
         For_All (File_Entities.All_Entities, CPP_Get_Doc'Access);

         For_All (File_Entities.All_Entities, Filter_Doc'Access);
      end if;

      Free (Buffer_Body);
      Free (Buffer);
   end Add_Documentation_From_Sources;

   -------------------------------
   -- Build_Structured_Comments --
   -------------------------------

   procedure Build_Structured_Comments
     (Context   : access constant Docgen_Context;
      Root      : Entity_Id;
      In_C_Lang : Boolean)
   is
      pragma Unreferenced (In_C_Lang);

      function Is_Custom_Tag (Tag : String) return Boolean;
      --  Return True if Tag is a supported tag.
      --  ??? This info should be configurable in a separate file to allow
      --  customers to define their own tags

      procedure Parse_Subprogram_Comments (Subp : Entity_Id);
      --  Initialize the structured comment associated with Subp, parse the
      --  block of comments retrieved from sources (and clean it), and report
      --  errors/warnings on missing documentation.

      procedure Parse_Doc_Wrapper
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False);
      --  Perform a fast analysis of S and invoke Parse_Doc or Parse_XML_Doc

      procedure Parse_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False);
      --  Parse the contents of S and store its contents in the structured
      --  comment of E (ie. E.Comment)

      procedure Parse_XML_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False);
      --  Parse the contents of S and store its contents in the structured
      --  comment of E (ie. E.Comment)

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Dispatch a call to build an structured comment between routines
      --  Parse_Doc and Parse_Subprogram_Comments.

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
            --  or else Tag = "group" --  ???
            --  or else Tag = "code"  --  ???

            --  JavaDoc possible additional tags???
            --  or else Tag = "author"
            --  or else Tag = "deprecated"
           or else Tag = "return";
            --  or else Tag = "serial"
            --  or else Tag = "since"
            --  or else Tag = "version"

            --  Proposed enhancements
            --  or else Tag = "field";
      end Is_Custom_Tag;

      -----------------------
      -- Parse_Doc_Wrapper --
      -----------------------

      procedure Parse_Doc_Wrapper
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False)
      is
         Matches : Match_Array (0 .. 3);

      begin
         if Index (S, "@") > 0 then
            Parse_Doc (Context, E, S, Is_Full_View);
            return;
         end if;

         Match (XML_Regpat, S, Matches);

         if Matches (0) /= No_Match then
            Parse_XML_Doc (Context, E, S, Is_Full_View);
         else
            Parse_Doc (Context, E, S, Is_Full_View);
         end if;
      end Parse_Doc_Wrapper;

      ---------------
      -- Parse_Doc --
      ---------------

      procedure Parse_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False)
      is
         Comment : constant Structured_Comment :=
                    (if Is_Full_View then Get_Full_View_Comment (E)
                                     else Get_Comment (E));
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

            if Tag_Loc = No_Location then
               Append_Text (Current, S);
               return;
            end if;

            --  Append characters to the last opened tag.

            if Tag_Loc.First > S'First then
               Append_Text
                 (Current, Filter (S (S'First .. Tag_Loc.First - 2)));
            end if;

            declare
               Tag_Text  : constant String :=
                             To_Lower (S (Tag_Loc.First + 1 .. Tag_Loc.Last));
               Attr_Loc  : Location;
               Text_Loc  : Location;
               Line_Last : Natural;
               J : Natural;
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

                  if Tag_Text = "return"
                    and then Get_Kind (E) = E_Procedure
                  then
                     Error
                       (Context,
                        LL.Get_Entity (E),
                        "@return not applicable to procedures");
                  end if;

                  if Tag_Text = "param" then
                     if Attr_Loc = No_Location then
                        Error
                          (Context,
                           LL.Get_Entity (E),
                           "missing parameter name");

                     else
                        declare
                           Param_Name : String renames
                             S (Attr_Loc.First .. Attr_Loc.Last);
                           Cursor : Tag_Cursor;
                        begin
                           Cursor :=
                             Search_Param (Comment, Param_Name);

                           if Cursor = No_Cursor then
                              Error
                                (Context,
                                 LL.Get_Entity (E),
                                 Msg =>
                                   "wrong parameter name '"
                                 & Param_Name & "'");

                           elsif Get (Cursor).Text
                             /= Null_Unbounded_String
                           then
                              Error
                                (Context,
                                 Get (Cursor).Entity,
                                 "parameter '"
                                 & Param_Name & "' documented twice");

                           elsif Text_Loc /= No_Location then
                              Current := Cursor; -- needed???

                              declare
                                 Text : String renames
                                   S (Text_Loc.First .. Text_Loc.Last);
                              begin
                                 Append_Text (Current, Text);
                              end;
                           end if;
                        end;
                     end if;

                     --  Opening tag

                  else

                     --  Now initialize the attributes field
                     if Attr_Loc /= No_Location then
                        declare
                           Text : String renames
                             S (Attr_Loc.First .. Attr_Loc.Last);
                        begin
                           if Tag_Text = "seealso" then
                              Current :=
                                Append_Tag
                                  (Comment,
                                   Tag       => To_Unbounded_String (Tag_Text),
                                   Entity    => No_General_Entity,
                                   Attribute => To_Unbounded_String (Text));
                           else
                              Current :=
                                Append_Tag
                                  (Comment,
                                   Tag       => To_Unbounded_String (Tag_Text),
                                   Entity    => No_General_Entity,
                                   Attribute => Null_Unbounded_String);
                              Append_Text (Current, Text);
                           end if;
                        end;
                     else
                        Current :=
                          Append_Tag
                            (Comment,
                             Tag       => To_Unbounded_String (Tag_Text),
                             Entity    => No_General_Entity,
                             Attribute => Null_Unbounded_String);
                     end if;

                     if Text_Loc /= No_Location then
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

         if Is_Full_View then
            if Context.Options.Show_Private then
               Set_Full_View_Comment (E, Comment);
            end if;
         else
            Set_Comment (E, Comment);
         end if;
      end Parse_Doc;

      -------------------------------
      -- Parse_Subprogram_Comments --
      -------------------------------

      procedure Parse_Subprogram_Comments (Subp : Entity_Id) is
         Has_Params : constant Boolean := Has_Entities (Subp);
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

                  if Tag_Info.Text = Null_Unbounded_String then
                     Warning
                       (Context,
                        Tag_Info.Entity, --  LL.Get_Entity (Subp),
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
         S            : String;
         Is_Full_View : Boolean := False)
      is
         Comment : constant Structured_Comment :=
                    (if Is_Full_View then Get_Full_View_Comment (E)
                                     else Get_Comment (E));
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
                  Tag_Text := To_Unbounded_String (Tag);
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
                  begin
                     Current :=
                       Append_Tag
                         (Comment,
                          Tag       => Tag_Text,
                          Entity    => No_General_Entity,
                          Attribute => To_Unbounded_String (Param_Name));
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
                          Entity    => No_General_Entity,
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

         if Is_Full_View then
            if Context.Options.Show_Private then
               Set_Full_View_Comment (E, Comment);
            end if;
         else
            Set_Comment (E, Comment);
         end if;
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

         if Is_Subprogram_Or_Entry (Entity) then
            Parse_Subprogram_Comments (Entity);
            return Skip;

         elsif Get_Doc (Entity).Text /= Null_Unbounded_String then
            Set_Comment (Entity, New_Structured_Comment);
            Parse_Doc_Wrapper
              (Context, Entity, To_String (Get_Doc (Entity).Text));
            Set_Doc (Entity, No_Comment_Result);

            if Is_Partial_View (Entity)
              and then Context.Options.Show_Private
            then
               Set_Full_View_Comment (Entity, New_Structured_Comment);
               Parse_Doc_Wrapper
                 (Context      => Context,
                  E            => Entity,
                  S            => To_String (Get_Full_View_Doc (Entity).Text),
                  Is_Full_View => True);
               Set_Full_View_Doc (Entity, No_Comment_Result);
            end if;
         end if;

         return OK;
      end Process_Node;

   begin
      Traverse_Tree (Root, Process_Node'Access);
   end Build_Structured_Comments;

   ----------------
   -- Build_Tree --
   ----------------

   function Build_Tree
     (Context : access constant Docgen_Context;
      File    : Virtual_File) return Tree_Type
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang     : constant Boolean := not In_Ada_Lang;

      Built_Tree    : Entity_Id;
      Tree          : aliased Tree_Type;

      My_Time       : Delay_Time;
      Tree_Time     : Delay_Time;
      Doc_Time      : Delay_Time;
      Comments_Time : Delay_Time;

   begin
      Trace (Me, "Build_Tree " & (+File.Base_Name));
      Start (My_Time);

      --  Step 1: Build the tree

      Start (Tree_Time);

      Built_Tree := Build_File_Tree (Context, File, Tree'Access);

      --  ??? Until we improve the performance Build_File_Tree does not
      --  generate the tree associated with large files.

      if No (Built_Tree) then
         return No_Tree;
      end if;

      Tree.Tree_Root := Built_Tree;
      Tree.File      := File;

      Stop (Tree_Time, Build_Tree_Time);
      --  Step 2: Add documentation from sources

      Start (Doc_Time);
      Add_Documentation_From_Sources (Context, File, Tree'Access);
      Stop (Doc_Time, GetDoc_Time);

      --  Step 3: Convert blocks of comments into structured comments

      Start (Comments_Time);
      Build_Structured_Comments (Context, Tree.Tree_Root, In_C_Lang);
      Stop (Comments_Time, Build_Comments_Time);

      Stop (My_Time, Frontend_Time);
      return Tree;

   exception
      when E : others =>
         Trace (Me, E);
         raise;
   end Build_Tree;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Builder.Initialize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Unique_Entity
     (Location      : General_Location;
      In_References : Boolean := False) return Entity_Id is
   begin
      return Builder.Find_Unique_Entity (Location, In_References);
   end Find_Unique_Entity;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (S : String) return Tokens is
      Keyword : constant String := To_Lower (S);

   begin
      if Keyword = "abstract" then
         return Tok_Abstract;
      elsif Keyword = "aliased" then
         return Tok_Aliased;
      elsif Keyword = "and" then
         return Tok_And;
      elsif Keyword = "case" then
         return Tok_Case;
      elsif Keyword = "end" then
         return Tok_End;
      elsif Keyword = "entry" then
         return Tok_Entry;
      elsif Keyword = "for" then
         return Tok_For;
      elsif Keyword = "function" then
         return Tok_Function;
      elsif Keyword = "generic" then
         return Tok_Generic;
      elsif Keyword = "is" then
         return Tok_Is;
      elsif Keyword = "interface" then
         return Tok_Interface;
      elsif Keyword = "limited" then
         return Tok_Limited;
      elsif Keyword = "new" then
         return Tok_New;
      elsif Keyword = "null" then
         return Tok_Null;
      elsif Keyword = "others" then
         return Tok_Others;
      elsif Keyword = "package" then
         return Tok_Package;
      elsif Keyword = "pragma" then
         return Tok_Pragma;
      elsif Keyword = "private" then
         return Tok_Private;
      elsif Keyword = "procedure" then
         return Tok_Procedure;
      elsif Keyword = "protected" then
         return Tok_Protected;
      elsif Keyword = "record" then
         return Tok_Record;
      elsif Keyword = "renames" then
         return Tok_Renames;
      elsif Keyword = "return" then
         return Tok_Return;
      elsif Keyword = "subtype" then
         return Tok_Subtype;
      elsif Keyword = "tagged" then
         return Tok_Tagged;
      elsif Keyword = "task" then
         return Tok_Task;
      elsif Keyword = "type" then
         return Tok_Type;
      elsif Keyword = "use" then
         return Tok_Use;
      elsif Keyword = "when" then
         return Tok_When;
      elsif Keyword = "with" then
         return Tok_With;
      else
         return Tok_Unknown;
      end if;
   end Get_Token;

   -----------
   -- Debug --
   -----------

   package body Debug is

      function To_Str (E : Entity_Id) return String is
         With_Id   : constant Boolean := True;
         With_Kind : constant Boolean := False;

         Uid : constant String :=
           (if not Present (E) or else not With_Id then ""
            else "[" & To_String (Get_Unique_Id (E)) & "] ");

         Kind : constant String :=
           (if not With_Kind then ""
            else Get_Kind (E)'Img & " : ");
      begin
         if Present (E) then
            return Uid
               & Get_Short_Name (E)
               & ":"
               & Kind
               & Image (LL.Get_Location (E), With_Filename => False);
         end if;

         return "";
      end To_Str;

      ------------------
      -- Print_Entity --
      ------------------

      procedure Print_Entity (E : Entity_Id; Prefix : String) is
      begin
         if Present (E) then
            GNAT.IO.Put_Line (Prefix & To_Str (E));
         else
            GNAT.IO.Put_Line
              (Prefix & ": No entity");
         end if;
      end Print_Entity;
   end Debug;

   ------------------
   -- Scopes_Stack --
   ------------------

   package body Scopes_Stack is

      package Stack_Entities_List is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => Scope_Id);
      --  procedure Free (List : in out Alloc_Entity_List.Vector);

      Stack : Stack_Entities_List.Vector;

      Enter_Scope_Enabled : Boolean;

      procedure Disable_Enter_Scope is
      begin
         Enter_Scope_Enabled := False;
      end Disable_Enter_Scope;

      procedure Enable_Enter_Scope is
      begin
         Enter_Scope_Enabled := True;
      end Enable_Enter_Scope;

      function Current_Context return Scope_Id is
      begin
         return Stack.First_Element;
      end Current_Context;

      -----------------
      -- Enter_Scope --
      -----------------

      procedure Enter_Scope (Entity : Entity_Id) is
         use type Ada.Containers.Count_Type;
         New_Scope : constant Scope_Id := new Scope_Info;
      begin
         pragma Assert (Enter_Scope_Enabled);

         --  Initialize the context of the new syntax scope locating its
         --  cursor before its first entity

         New_Scope.Scope := Entity;

         Stack.Prepend (New_Scope);
      end Enter_Scope;

      ----------------
      -- Exit_Scope --
      ----------------

      procedure Exit_Scope is
         Scope : Scope_Id := Current_Context;
      begin
         Free (Scope);
         Stack.Delete_First;
      end Exit_Scope;

      -------------
      -- Getters --
      -------------

      function Get_Current_Entity
        (Context : Scope_Id) return Entity_Id is
      begin
         return Context.Current_Entity;
      end Get_Current_Entity;

      function Get_End_Decl_Found (Scope : Scope_Id) return Boolean is
      begin
         return Scope.End_Decl_Found;
      end Get_End_Decl_Found;

      function Get_Prev_Entity_In_Scope
        (Scope : Scope_Id) return Entity_Id is
      begin
         return Scope.Prev_Entity_In_Scope;
      end Get_Prev_Entity_In_Scope;

      function Get_Scope (Context : Scope_Id) return Entity_Id is
      begin
         return Context.Scope;
      end Get_Scope;

      function Tok_Subprogram_Seen (Scope : Scope_Id) return Boolean is
      begin
         return Scope.Token_Seen (Tok_Procedure)
           or else Scope.Token_Seen (Tok_Function)
           or else Scope.Token_Seen (Tok_Entry);
      end Tok_Subprogram_Seen;

      --  Setters ---------------------------------------------------

      procedure Set_Current_Entity
        (Context : Scope_Id; Entity : Entity_Id) is
      begin
         Context.Prev_Entity_In_Scope := Context.Current_Entity;
         Replace_Current_Entity (Context, Entity);
      end Set_Current_Entity;

      procedure Replace_Current_Entity
        (Context : Scope_Id; Entity : Entity_Id) is
      begin
         Context.Current_Entity := Entity;
      end Replace_Current_Entity;

      procedure Set_Token_Seen
        (Scope : Scope_Id; Token : Tokens) is
      begin
         Scope.Token_Seen (Token) := True;
      end Set_Token_Seen;

      procedure Reset_Tok_Subprogram_Seen
        (Scope : Scope_Id) is
      begin
         Scope.Token_Seen (Tok_Procedure) := False;
         Scope.Token_Seen (Tok_Function) := False;
         Scope.Token_Seen (Tok_Entry) := False;
      end Reset_Tok_Subprogram_Seen;

      procedure Reset_End_Decl_Found (Scope : Scope_Id) is
      begin
         Scope.End_Decl_Found := False;
      end Reset_End_Decl_Found;

      procedure Set_End_Decl_Found (Scope : Scope_Id) is
      begin
         Scope.End_Decl_Found := True;
      end Set_End_Decl_Found;

      procedure Print_Scopes is
         Scope : Entity_Id;
         Count : Integer := Natural (Stack.Length) - 1;
      begin
         GNAT.IO.Put_Line ("--- Scopes");
         for Elmt of Stack loop
            Scope := Elmt.Scope;

            if Present (Scope) then
               declare
                  Prev : constant String :=
                    (if No (Elmt.Prev_Entity_In_Scope) then ""
                     else "; P=" & To_Str (Elmt.Prev_Entity_In_Scope));
                  Curr : constant String :=
                    (if No (Elmt.Current_Entity) then ""
                     else "; C=" & To_Str (Elmt.Current_Entity));
               begin
                  GNAT.IO.Put_Line
                    (To_String (Count)
                     & ":"
                     & " S="
                     & To_Str (Scope)
                     & Prev
                     & Curr);
               end;
            end if;

            Count := Count - 1;
         end loop;
         null;
      end Print_Scopes;
   end Scopes_Stack;

end GNATdoc.Frontend;
