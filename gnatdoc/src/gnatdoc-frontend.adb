------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;                     use Ada.Strings;
with Ada.Strings.Fixed;               use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Basic_Types;                     use Basic_Types;
with GNAT.HTable;
with GNAT.IO;
with GNAT.Regpat;                     use GNAT.Regpat;
with GNAT.Strings;                    use GNAT.Strings;
with GNATCOLL.Utils;
with Language;                        use Language;
with Language.Ada;                    use Language.Ada;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with String_Utils;

with GNATdoc.Frontend.Builder;        use GNATdoc.Frontend.Builder;
with GNATdoc.Frontend.Comment_Parser; use GNATdoc.Frontend.Comment_Parser;
with GNATdoc.Time;                    use GNATdoc.Time;
with GNATdoc.Utils;                   use GNATdoc.Utils;
with Xref.Docgen;                     use Xref.Docgen;

package body GNATdoc.Frontend is
   Me : constant Trace_Handle := Create ("GNATdoc.1-Frontend");
   Enhancements : constant Boolean := False;

   type Tokens is
     (Tok_Unknown,
      Tok_Char_Literal,
      Tok_String_Literal,
      Tok_Id,                --  Expanded names & identifiers
      Tok_Number,
      Tok_Operator,

      --  Reserved Ada 83 words
      Tok_And,
      Tok_Begin,
      Tok_Body,
      Tok_Case,
      Tok_End,
      Tok_Entry,
      Tok_For,
      Tok_Function,
      Tok_Generic,
      Tok_Is,
      Tok_Limited,
      Tok_New,
      Tok_Null,
      Tok_Others,
      Tok_Package,
      Tok_Pragma,
      Tok_Private,
      Tok_Procedure,
      Tok_Record,
      Tok_Renames,
      Tok_Return,
      Tok_Separate,
      Tok_Subtype,
      Tok_Task,
      Tok_Type,
      Tok_Use,
      Tok_When,
      Tok_With,

      --  Reserved Ada 95 words
      Tok_Abstract,
      Tok_Aliased,
      Tok_Protected,
      Tok_Until,
      Tok_Requeue,
      Tok_Tagged,

      --  Reserved Ada 2005 words
      Tok_Interface,
      Tok_Overriding,
      Tok_Synchronized,

      --  Reserved Ada 2012 words
      Tok_Some,

      --  Other tokens
      Tok_Arrow,
      Tok_Assignment,
      Tok_Left_Paren,
      Tok_Right_Paren,
      Tok_Left_Square_Bracket,
      Tok_Right_Square_Bracket,
      Tok_Semicolon);

   subtype Reserved_Word_Kind is Tokens range Tok_And .. Tok_With;

   ----------------------
   -- Local_Subrograms --
   ----------------------

   function "="
     (Left  : General_Location;
      Right : General_Location) return Boolean renames Atree."=";
   --  Return True if Left and right are references to the same location in
   --  the same file.

   function "<"
     (Left  : General_Location;
      Right : General_Location) return Boolean renames Atree."<";
   --  Return True if Left is located before Right in the same file

   function Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type;
      Tree_Spec     : access Tree_Type) return Boolean;
   --  Add to the nodes their blocks of documentation & sources. Returns false
   --  if we found discrepancies between the contents of the database and the
   --  parsed sources. Tree_Spec contanins the tree built for the specification
   --  of this file (the null value means that we are building the tree of a
   --  compilation unit specification; the No_Tree value means that we are
   --  processing a compilation unit body that has no spec; and other values
   --  mean that we are processing a compilation unit body that has the given
   --  compilation unit specification tree).

   function To_Visible_Column
     (Buffer          : String;
      Index_In_Line   : Natural;
      Index_In_Buffer : Natural) return Visible_Column_Type;
   --  Returns number of visible column for given column and index in the
   --  buffer. This subprogram handles tab expansion and multibyte sequences
   --  of UTF-8 encoding.

   -----------
   -- Debug --
   -----------

   package Debug is
      function To_Str (E : Entity_Id) return String;
      procedure Print_Entity (E : Entity_Id; Prefix : String);
   end Debug;
   use Debug;

   --------------------------
   -- Reserved_Words_Table --
   --------------------------

   package Reserved_Words_Table is
      type Ada_Version_Type is (Ada_83, Ada_95, Ada_2005, Ada_2012);

      procedure Enable_Ada_Version (Version : Ada_Version_Type);
      --  Enable the reserved words set of the given version of Ada

      function Get_Token (Word : String) return Tokens;
      --  Return the token associated with Word

   end Reserved_Words_Table;
   use Reserved_Words_Table;

   ------------------
   -- Scopes_Stack --
   ------------------

   package Scopes_Stack is
      type Context_Info is private;
      type Context_Id is access all Context_Info;

      procedure Disable_Enter_Scope;
      procedure Enable_Enter_Scope;

      function Current_Context return Context_Id;
      --  Return the context in the top of the stack

      procedure Enter_Scope (Entity : Entity_Id);
      --  Enter in the syntax scope of Entity

      procedure Exit_Scope;
      --  Leave a syntax scope

      procedure Reset_Stack;
      --  Reset the scopes stack

      -- Getters --------------------------------------------------

      function Get_Current_Entity
        (Context : Context_Id) return Entity_Id;
      --  Return the current entity of Context

      function Get_End_Decl_Found
        (Context : Context_Id) return Boolean;

      function Get_Prev_Entity_In_Scope
        (Context : Context_Id) return Entity_Id;

      function Get_Scope (Context : Context_Id) return Entity_Id;
      --  Return scope of the next entity (No_Entity if no scope available)

      function In_Body
        (Context : Context_Id) return Boolean;
      --  Return True if we have seen the 'begin' token or if we have seen the
      --  the first internal entity of the current package, subprogram or
      --  concurrent type body (excluding its formals).

      function In_Concurrent_Type_Definition
        (Context : Context_Id) return Boolean;
      --  Return True if this context corresponds with task or protected type

      function In_Private_Part
        (Context : Context_Id) return Boolean;

      function Tok_Record_Seen
        (Context : Context_Id) return Boolean;

      function Tok_Subprogram_Seen
        (Context : Context_Id) return Boolean;

      -- Setters --------------------------------------------------

      procedure Replace_Current_Entity
        (Context : Context_Id; Entity : Entity_Id);

      procedure Reset_End_Decl_Found
        (Context : Context_Id);

      procedure Reset_Tok_Subprogram_Seen
        (Context : Context_Id);

      procedure Set_Current_Entity
        (Context : Context_Id; Entity : Entity_Id);

      procedure Set_End_Decl_Found
        (Context : Context_Id);

      procedure Set_In_Body
        (Context : Context_Id);

      procedure Set_In_Concurrent_Type_Definition
        (Context : Context_Id);

      procedure Set_In_Private_Part
        (Context : Context_Id);

      procedure Set_Token_Seen
        (Context : Context_Id; Token : Tokens);

      procedure Print_Scopes;
      --  For debugging

   private
      type Token_Flags is array (Tokens'Range) of Boolean;

      type Context_Info is record
         Scope                : Entity_Id;

         Prev_Entity_In_Scope : Entity_Id;
         Current_Entity       : Entity_Id;

         --  Parser state
         End_Decl_Found                : Boolean := False;
         In_Body                       : Boolean := False;
         In_Concurrent_Type_Definition : Boolean := False;
         In_Private_Part               : Boolean := False;

         Token_Seen           : Token_Flags := (others => False);
      end record;

      procedure Free is
        new Ada.Unchecked_Deallocation (Context_Info, Context_Id);

      pragma Inline (Current_Context);
      pragma Inline (Enter_Scope);
      pragma Inline (Exit_Scope);
      pragma Inline (Reset_Stack);

      pragma Inline (Get_Current_Entity);
      pragma Inline (Get_End_Decl_Found);
      pragma Inline (In_Body);
      pragma Inline (In_Concurrent_Type_Definition);
      pragma Inline (In_Private_Part);
      pragma Inline (Get_Prev_Entity_In_Scope);
      pragma Inline (Get_Scope);
      pragma Inline (Tok_Record_Seen);
      pragma Inline (Tok_Subprogram_Seen);

      pragma Inline (Replace_Current_Entity);
      pragma Inline (Reset_End_Decl_Found);
      pragma Inline (Reset_Tok_Subprogram_Seen);

      pragma Inline (Set_Current_Entity);
      pragma Inline (Set_End_Decl_Found);
      pragma Inline (Set_In_Body);
      pragma Inline (Set_In_Concurrent_Type_Definition);
      pragma Inline (Set_In_Private_Part);
      pragma Inline (Set_Token_Seen);

   end Scopes_Stack;
   use Scopes_Stack;

   --------------------------
   -- Ada_Compilation_Unit --
   --------------------------

   function Ada_Compilation_Unit (Tree : Tree_Type) return Entity_Id is
   begin
      pragma Assert (Is_Standard_Entity (Tree.Tree_Root));

      for E of Get_Entities (Tree.Tree_Root).all loop
         if Is_Compilation_Unit (E) then
            return E;
         end if;

         pragma Assert (Is_Generic_Formal (E));
      end loop;

      return Atree.No_Entity;
   end Ada_Compilation_Unit;

   ------------------------------------
   -- Add_Documentation_From_Sources --
   ------------------------------------

   function Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type;
      Tree_Spec     : access Tree_Type) return Boolean
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      In_Ada_Body   : constant Boolean :=
                        In_Ada_Lang
                          and then not Is_Spec_File (Context.Kernel, File);
      In_Ada_Spec   : constant Boolean :=
                        In_Ada_Lang
                          and then Is_Spec_File (Context.Kernel, File);
      In_C_Lang     : constant Boolean := not In_Ada_Lang;

      Processing_Body : constant Boolean := Tree_Spec /= null;

      Processing_Body_With_Spec : constant Boolean :=
        Processing_Body and then Tree_Spec.all /= No_Tree;

      Processing_Body_Without_Spec : constant Boolean :=
        Processing_Body and then Tree_Spec.all = No_Tree;

      Body_File        : Virtual_File := No_File;
      Buffer           : GNAT.Strings.String_Access;
      Buffer_Body      : GNAT.Strings.String_Access;
      C_Headers_Buffer : GNAT.Strings.String_Access;
      Buffers_Swapped  : Boolean;

      File_Successfully_Parsed : Boolean := True;

      package Compiler_Workaround is

         function Get_Entity
           (Context : access constant Docgen_Context;
            Name    : String;
            Loc     : General_Location) return Entity_Id;
         --  Retrieve the entity referenced at the given location. Name can be
         --  an expanded name. This also works for operators, whether they are
         --  quoted ("=") or not (=).

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

            Entity : constant Root_Entity'Class :=
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

      end Compiler_Workaround;

      procedure Ada_Set_Doc (E : Entity_Id);
      --  Set the documentation of E

      procedure Append_Corresponding_Body_Of_Entities_Defined_In_Body;
      --  Append to the list of entities of File_Entities the corresponding
      --  body of entities defined in the body (that is, entities whose spec
      --  is defined in a compilation unit body).

      procedure Append_Corresponding_Body_Of_Entities_Defined_In_Spec;
      --  Append to the list of entities of File_Entities the corresponding
      --  body of entities defined in the spec (that is, entities whose spec
      --  is defined in a compilation unit spec).

      procedure Clean_Entities;
      --  Remove sources and documentation of skipped entities. Remove also
      --  sources attached to the internal entities associated with the
      --  corresponding bodies, and entities defined in subprogram bodies.

      function Build_Corresponding_Body
        (E       : Entity_Id;
         New_Loc : General_Location) return Entity_Id;
      --  Build the corresponding body entity of E

      procedure CPP_Get_Doc (E : Entity_Id);
      --  Retrieve the C/C++ documentation associated with E

      procedure CPP_Get_Source (E : Entity_Id);
      --  Retrieve the C/C++ source associated with E

      procedure Filter_Doc (E : Entity_Id);
      --  Filter the documentation using the user-defined filter

      procedure Parse_Ada_File
        (Buffer   : GNAT.Strings.String_Access);

      procedure Previous_Word
        (Index           : Natural;
         Prev_Word_Begin : out Natural;
         Prev_Word_End   : out Natural);
      --  Return the indexes to the first word in Buffer located before Index

      procedure Remove_Internal_FE_Entities;
      --  Remove from the list of entities of File_Entities internal entities
      --  available in the ALI files which GNATdoc does not need to process.

      procedure Remove_Task_Body_Entities;
      --  Remove from the list of entities all the entities defined in task
      --  bodies.

      procedure Swap_Buffers;
      --  Swap the contents of Buffer and C_Headers_Buffer. Used to retrieve
      --  the sources and the documentation located in the header file.

      -----------------
      -- Ada_Set_Doc --
      -----------------

      Prev_Comment_Line : Natural := 0;

      procedure Ada_Set_Doc (E : Entity_Id) is

         procedure Update_Prev_Comment_Line (Line : Natural);

         ------------------------------
         -- Update_Prev_Comment_Line --
         ------------------------------

         procedure Update_Prev_Comment_Line (Line : Natural) is
         begin
            --  Skip entities whose documentation was retrieved from the body

            if No (Get_End_Of_Profile_Location_In_Body (E)) then
               Prev_Comment_Line := Line;
            end if;
         end Update_Prev_Comment_Line;

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
                  Update_Prev_Comment_Line (Get_Doc_Before (E).Start_Line);

               elsif Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Update_Prev_Comment_Line (Get_Doc_After (E).Start_Line);
               end if;
            else
               --  Documentation retrieved from the body

               if Context.Options.Process_Bodies
                 and then Present (Get_Doc_Before (E))
                 and then Present (Get_End_Of_Profile_Location_In_Body (E))
               then
                  Set_Doc (E, Get_Doc_Before (E));

               elsif Present (Get_Doc_Before (E))
                 and then Get_Doc_Before (E).Start_Line /= Prev_Comment_Line
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Update_Prev_Comment_Line (Get_Doc_Before (E).Start_Line);

               elsif Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Update_Prev_Comment_Line (Get_Doc_After (E).Start_Line);
               end if;
            end if;

         else
            if Is_Compilation_Unit (E) then

               if Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Update_Prev_Comment_Line (Get_Doc_After (E).Start_Line);

               --  For documentation located before compilation units it is
               --  mandatory to use some tag. Required to safely differentiate
               --  copyright banners, etc.

               elsif Present (Get_Doc_Before (E))
                 and then May_Have_Tags (Get_Doc_Before (E).Text)
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Update_Prev_Comment_Line (Get_Doc_Before (E).Start_Line);
               end if;
            else
               if Present (Get_Doc_After (E)) then
                  Set_Doc (E, Get_Doc_After (E));
                  Update_Prev_Comment_Line (Get_Doc_After (E).Start_Line);

               --  Documentation retrieved from the body

               elsif Context.Options.Process_Bodies
                 and then Present (Get_Doc_Before (E))
                 and then Present (Get_End_Of_Profile_Location_In_Body (E))
               then
                  Set_Doc (E, Get_Doc_Before (E));

               elsif Present (Get_Doc_Before (E))
                 and then Get_Doc_Before (E).Start_Line /= Prev_Comment_Line
               then
                  Set_Doc (E, Get_Doc_Before (E));
                  Update_Prev_Comment_Line (Get_Doc_Before (E).Start_Line);
               end if;
            end if;
         end if;

         Set_Doc_Before (E, No_Comment_Result);
         Set_Doc_After (E, No_Comment_Result);
      end Ada_Set_Doc;

      -----------------------------------------------------------
      -- Append_Corresponding_Body_Of_Entities_Defined_In_Body --
      -----------------------------------------------------------

      procedure Append_Corresponding_Body_Of_Entities_Defined_In_Body is
         Body_Entities : aliased EInfo_List.Vector;
         Subp_E        : Entity_Id := Atree.No_Entity;
         Cloned_E      : Entity_Id;

      begin
         pragma Assert (In_Ada_Body);

         for E of File_Entities.All_Entities loop
            if Is_Concurrent_Type_Or_Object (E) then
               declare
                  Body_Loc : constant General_Location :=
                    LL.Get_Begin_Of_Concurrent_Type_Body_Loc (E);
               begin
                  --  For iterators the compiler may generate references to
                  --  internal tasks that do not have its Body_Loc attribute

                  if Present (Body_Loc) then
                     Cloned_E := Build_Corresponding_Body (E, Body_Loc);

                     if Present (Cloned_E) then
                        Set_First_Private_Entity_Loc (Cloned_E, No_Location);

                        if not File_Entities.All_Entities.Contains (Cloned_E)
                          and then not Body_Entities.Contains (Cloned_E)
                        then
                           Body_Entities.Append (Cloned_E);
                        end if;
                     end if;

                     Subp_E := Atree.No_Entity;
                  end if;
               end;

            elsif Is_Package (E)
              or else Is_Subprogram_Or_Entry (E)
            then
               if Is_Separate_Unit (E) then
                  Cloned_E := Build_Corresponding_Body (E,
                                LL.Get_Separate_Stub_Body_Loc (E));

                  LL.Set_Body_Loc (Cloned_E, LL.Get_Body_Loc (E));
                  LL.Set_Separate_Stub_Body_Loc (Cloned_E, No_Location);

                  if Present (Cloned_E)
                    and then not
                      File_Entities.All_Entities.Contains (Cloned_E)
                  then
                     Body_Entities.Append (Cloned_E);
                  end if;
               else
                  declare
                     Loc      : constant General_Location :=
                                  LL.Get_Location (E);
                     Body_Loc : constant General_Location :=
                                  LL.Get_Body_Loc (E);
                  begin
                     if Loc.File = Body_Loc.File
                       and then Body_Loc.Line /= Loc.Line
                     then
                        Cloned_E :=
                          Build_Corresponding_Body (E, LL.Get_Body_Loc (E));

                        if Present (Cloned_E)
                          and then not
                            File_Entities.All_Entities.Contains (Cloned_E)
                          and then not Body_Entities.Contains (Cloned_E)
                        then
                           Body_Entities.Append (Cloned_E);
                        end if;

                        Subp_E := E;
                     end if;
                  end;
               end if;

            elsif Present (Subp_E)
              and then Get_Kind (E) = E_Formal
              and then Present (LL.Get_Body_Loc (E))
            then
               Cloned_E :=
                 Build_Corresponding_Body (E, LL.Get_Body_Loc (E));

               if Present (Cloned_E)
                 and then not
                   File_Entities.All_Entities.Contains (Cloned_E)
                 and then not Body_Entities.Contains (Cloned_E)
               then
                  Body_Entities.Append (Cloned_E);
               end if;
            else
               Subp_E := Atree.No_Entity;
            end if;
         end loop;

         for E of Body_Entities loop
            File_Entities.All_Entities.Append (E);
         end loop;
      end Append_Corresponding_Body_Of_Entities_Defined_In_Body;

      -----------------------------------------------------------
      -- Append_Corresponding_Body_Of_Entities_Defined_In_Spec --
      -----------------------------------------------------------

      procedure Append_Corresponding_Body_Of_Entities_Defined_In_Spec is
         Cloned_E : Entity_Id;
         Body_Loc : General_Location;

      begin
         pragma Assert (In_Ada_Body
           and then Processing_Body_With_Spec);

         for E of Tree_Spec.All_Entities loop
            if Is_Concurrent_Type_Or_Object (E) then
               Body_Loc :=
                 LL.Get_Begin_Of_Concurrent_Type_Body_Loc (E);

               if Body_Loc /= No_Location
                 and then Body_Loc.File = File
               then
                  Cloned_E :=
                    Build_Corresponding_Body (E, Body_Loc);

                  if Present (Cloned_E) then
                     if not File_Entities.All_Entities.Contains (Cloned_E)
                     then
                        File_Entities.All_Entities.Append (Cloned_E);
                     end if;
                  end if;
               end if;

            elsif Is_Separate_Unit (E) then
               Body_Loc := LL.Get_Separate_Stub_Body_Loc (E);
               Cloned_E := Build_Corresponding_Body (E, Body_Loc);
               LL.Set_Body_Loc (Cloned_E, LL.Get_Body_Loc (E));
               LL.Set_Separate_Stub_Body_Loc (Cloned_E, No_Location);

               if Present (Cloned_E)
                 and then not
                   File_Entities.All_Entities.Contains (Cloned_E)
               then
                  File_Entities.All_Entities.Append (Cloned_E);
               end if;

            else
               Body_Loc := LL.Get_Body_Loc (E);

               if Body_Loc /= No_Location
                 and then Body_Loc.File = File
               then
                  Cloned_E :=
                    Build_Corresponding_Body (E, Body_Loc);

                  if Present (Cloned_E)
                    and then not
                      File_Entities.All_Entities.Contains (Cloned_E)
                  then
                     File_Entities.All_Entities.Append (Cloned_E);
                  end if;
               end if;
            end if;
         end loop;
      end Append_Corresponding_Body_Of_Entities_Defined_In_Spec;

      ------------------------------
      -- Build_Corresponding_Body --
      ------------------------------

      function Build_Corresponding_Body
        (E       : Entity_Id;
         New_Loc : General_Location) return Entity_Id
      is
         Body_E : Entity_Id := Find_Unique_Entity (New_Loc,
                                 In_References => False);
      begin
         --  In instantiations the attribute Body_Loc references the location
         --  of the generic unit and hence there is no need to build the
         --  corresponding body.

         if Present (LL.Get_Instance_Of (E)) then
            return Atree.No_Entity;
         end if;

         if No (Body_E) then
            Body_E := New_Internal_Entity (Context, E);
            LL.Set_Location (Body_E, New_Loc);
            Append_To_Map (Body_E);
         end if;

         LL.Set_Body_Loc (Body_E, No_Location);

         Set_Corresponding_Body (E, Body_E);
         Set_Corresponding_Spec (Body_E, E);

         --  Propagate decoration & fix decoration of the low-level builder

         if Get_Kind (E) = E_Formal then
            Set_Kind (Body_E, E_Formal);
            Set_Full_View (E, Atree.No_Entity);
            Set_Partial_View (Body_E, Atree.No_Entity);
         else
            if LL.Get_End_Of_Body_Loc (E)
              /= Get_End_Of_Scope_Loc (Body_E)
            then
               Set_End_Of_Scope_Loc (Body_E, LL.Get_End_Of_Body_Loc (E));
            end if;
         end if;

         return Body_E;
      end Build_Corresponding_Body;

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

            ------------
            -- Append --
            ------------

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
         -- Get_Struct_Type_Source --
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

            ------------
            -- Append --
            ------------

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

            ------------
            -- Append --
            ------------

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
           and then Present (Get_Src (E))
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
         Src_Conc_Type  : Unbounded_String;

         New_Entities       : EInfo_List.Vector;
         Body_File_Entities : aliased EInfo_List.Vector;
         Current_Body_File  : Virtual_File;

         Database_Not_Up_To_Date : exception;
         --  Raised when the parser detects discrepancies between the contents
         --  of the database and the sources.

         Separate_Unit : exception;
         --  Raised when the parser skips processing a separate unit.

         package Extended_Cursor is
            type Extended_Cursor is private;
            function Has_Entity  (Cursor : Extended_Cursor) return Boolean;
            function Entity      (Cursor : Extended_Cursor) return Entity_Id;
            function Prev_Entity (Cursor : Extended_Cursor) return Entity_Id;

            procedure Next_Entity
              (Cursor         : in out Extended_Cursor;
               Check_Disabled : Boolean := False);
            --  Move the cursor to the next entity. If Disable_Check is set
            --  then no ckeck on marked entities is performed (flag used to
            --  disable checks processing entities internally generated by
            --  the compiler which are not visible parsing the sources).

            procedure Initialize
              (Cursor         : in out Extended_Cursor;
               Entities       : access EInfo_List.Vector;
               Marks_Required : Boolean);
            --  If Marks_Required is True then Mark_Next_Entity_Seen() must be
            --  called with the current entity before calling Next_Entity().

            procedure Set_Next_Entity
              (Cursor : in out Extended_Cursor;
               Entity : Entity_Id);
            --  This subprogram is used to workaround missing entities

            procedure Mark_Next_Entity_Seen
              (Cursor : in out Extended_Cursor);
            --  Mark the next entity as seen

         private
            type Extended_Cursor is record
               Entities          : access EInfo_List.Vector;
               Cursor            : EInfo_List.Cursor;
               Element           : Entity_Id := Atree.No_Entity;
               Prev_Element      : Entity_Id := Atree.No_Entity;
               Saved_Next_Entity : Entity_Id := Atree.No_Entity;

               --  Internal flags
               Marks_Required : Boolean := False;
               Element_Seen   : Boolean := False;
            end record;
         end Extended_Cursor;

         procedure Append_Comment (Text : String);
         pragma Inline (Append_Comment);
         --  Append Text to Comment

         procedure Append_Conc_Type_Sources (Text : String);
         pragma Inline (Append_Conc_Type_Sources);
         --  Append Text to Src_Conc_Type

         procedure Append_Sources (Text : String);
         pragma Inline (Append_Sources);
         --  Append Text to Printout

         function At_Valid_Line_After
           (Doc_Line       : Natural;
            Reference_Line : Natural) return Boolean;
         --  Return True if Doc_Line is Reference_Line or the next line (that,
         --  is Reference_Line + 1).

         function At_Valid_Line_Before
           (Doc_Line       : Natural;
            Reference_Line : Natural) return Boolean;
         --  Return True if Doc_Line is Reference_Line - 1

         procedure Clear_Doc;
         --  Clear the accumulated comment

         procedure Clear_Sources;
         --  Clear the accumulated sources

         procedure Clear_Conc_Type_Sources;
         --  Clear the accumulated sources

         procedure Clear_Parser_State;

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  Callback for entity parser

         function CB_Body_File
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;
         --  Callback for entity parser

         procedure Set_Ada_Version_From_Project_Switches;
         --  Read the compiler switches associated with File and enable the set
         --  of reserved words associated with the given Ada version. Ada 2005
         --  is enabled by default if no switch was specified.

         --------------------
         -- Append_Comment --
         --------------------

         procedure Append_Comment (Text : String) is
            Comment_Prefix : constant String := "--";
         begin
            pragma Assert
              (Text'Length > Comment_Prefix'Length
               and then Comment_Prefix =
                 Text (Text'First .. Text'First + Comment_Prefix'Length - 1));
            Doc := Doc & Text (Text'First + 2 .. Text'Last);
         end Append_Comment;

         procedure Append_Conc_Type_Sources (Text : String) is
         begin
            Src_Conc_Type := Src_Conc_Type & Text;
         end Append_Conc_Type_Sources;

         procedure Append_Sources (Text : String) is
         begin
            Printout := Printout & Text;
         end Append_Sources;

         -------------------------
         -- At_Valid_Line_After --
         -------------------------

         function At_Valid_Line_After
           (Doc_Line       : Natural;
            Reference_Line : Natural) return Boolean is
         begin
            return Doc_Line = Reference_Line
              or else Doc_Line = Reference_Line + 1;
         end At_Valid_Line_After;

         --------------------------
         -- At_Valid_Line_Before --
         --------------------------

         function At_Valid_Line_Before
           (Doc_Line       : Natural;
            Reference_Line : Natural) return Boolean is
         begin
            return Reference_Line > 0
              and then Doc_Line = Reference_Line - 1;
         end At_Valid_Line_Before;

         ---------------
         -- Clear_Doc --
         ---------------

         procedure Clear_Doc is
         begin
            if Doc_Start_Line /= No_Line then
               Doc_Start_Line := No_Line;
               Doc_End_Line   := No_Line;
               Doc            := Null_Unbounded_String;
            end if;
         end Clear_Doc;

         procedure Clear_Conc_Type_Sources is
         begin
            if Present (Src_Conc_Type) then
               Src_Conc_Type := Null_Unbounded_String;
            end if;
         end Clear_Conc_Type_Sources;

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

         Cursor                 : Extended_Cursor.Extended_Cursor;
         Last_Idx               : Natural := 0;
         Par_Count              : Natural := 0;
         Prev_Token             : Tokens := Tok_Unknown;
         Prev_Token_Loc         : Source_Location;
         Token                  : Tokens := Tok_Unknown;
         Token_Loc              : Source_Location;

         Body_Files_List        : Files_List.Vector;
         Entities_Without_Doc   : aliased EInfo_List.Vector;

         Nested_Variants_Count  : Natural := 0;

         In_Compilation_Unit    : Boolean := False;
         In_Generic_Formals     : Boolean := False;
         In_Generic_Decl        : Boolean := False;
         Generic_Formals        : EInfo_List.Vector;
         Generic_Formals_Loc    : General_Location := No_Location;
         --  Location of the word "generic"

         Max_Aggregate_Length : constant := 25;
         In_Aggregate         : Boolean := False;
         Aggr_Begin_Line      : Natural := 0;

         In_Subtype_Declaration : Boolean := False;
         --  Set to true when we see the token "subtype"

         In_Type_Definition   : Boolean := False;
         --  Set to true when we see the token "is"

         In_Derived_Type_Definition : Boolean := False;
         --  Set to true when we see the sequence of tokens "is new"

         In_Item_Decl     : Boolean := False;
         --  Set to true when we see "procedure", "function" or "entry"

         In_Aspect_Spec   : Boolean := False;
         --  Set to true when (In_Item_Decl or In_Subtype_Declaration) is True
         --  and we see "with"

         In_Null_Record   : Boolean := False;
         --  Set to true when we see the sequence of tokens "record null"

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

         ------------------------
         -- Clear_Parser_State --
         ------------------------

         procedure Clear_Parser_State is
            No_Source_Location : constant Source_Location := (0, 0, 0);
         begin
            Last_Idx       := 0;
            Par_Count      := 0;
            Prev_Token     := Tok_Unknown;
            Prev_Token_Loc := No_Source_Location;
            Token          := Tok_Unknown;
            Token_Loc      := No_Source_Location;

            Nested_Variants_Count  := 0;
            In_Compilation_Unit    := False;
            In_Generic_Formals     := False;
            In_Generic_Decl        := False;
            Generic_Formals.Clear;
            Generic_Formals_Loc    := No_Location;

            In_Aggregate    := False;
            Aggr_Begin_Line := 0;

            In_Subtype_Declaration := False;
            In_Type_Definition := False;
            In_Derived_Type_Definition := False;
            In_Item_Decl    := False;
            In_Null_Record  := False;
            In_Aspect_Spec  := False;

            In_Parent_Part  := False;

            In_Pragma                := False;
            In_Representation_Clause := False;

            In_Record_Representation_Clause := False;
            In_Skipped_Declaration          := False;
         end Clear_Parser_State;

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

            function Aggregate_Length return Natural;
            --  Return the number of lines of the current aggregate

            procedure Clear_Accumulated_Sources;
            procedure Complete_Decoration (End_Decl_Found : Boolean);

            procedure Handle_Doc;
            procedure Handle_Pragma;
            procedure Handle_Scopes (End_Decl_Found : Boolean);
            procedure Handle_Sources (End_Decl_Found : Boolean);
            procedure Handle_Tokens;

            function Has_Scope (E : Entity_Id) return Boolean;
            function In_Next_Entity return Boolean;

            procedure Set_Doc_After
              (E             : Entity_Id;
               Forced_Update : Boolean := False);
            --  Attach Doc as the documentation located after E. No action is
            --  performed if E already has such documentation (unless formal
            --  Forced_Update is True).

            procedure Set_Doc_After_Current_Entity;
            procedure Set_Doc_After_Previous_Entity_In_Scope;

            procedure Print_State;

            -------------------------
            -- Accumulate_Comments --
            -------------------------

            procedure Accumulate_Comments is
               Scope : constant Entity_Id :=
                 Get_Scope (Current_Context);
               Current_Entity : constant Entity_Id :=
                 Get_Current_Entity (Current_Context);

            begin
               --  If the current comment is not a continuation then check
               --  if the previous documentation can be associated with the
               --  enclosing scope (or with the previous entity), and clear
               --  the previous documentation.

               if Doc_End_Line /= No_Line
                 and then Sloc_Start.Line /= Doc_End_Line + 1
               then
                  --  Documentation after the beginning of a concurrent type,
                  --  subprogram or package.

                  if (Is_Concurrent_Type_Or_Object (Scope)
                        or else Is_Subprogram (Scope)
                        or else Is_Package (Scope))
                    and then Present (Get_End_Of_Profile_Location (Scope))
                    and then At_Valid_Line_After (Doc_Start_Line,
                               Get_End_Of_Profile_Location (Scope).Line)
                  then
                     Set_Doc_After (Scope, Forced_Update => True);

                  --  Documentation after the declaration of a concurrent type,
                  --  subprogram or package.

                  elsif Present (Current_Entity)
                    and then
                      (Is_Concurrent_Type_Or_Object (Current_Entity)
                        or else Is_Subprogram (Current_Entity)
                        or else Is_Package (Current_Entity))
                    and then
                      Present (Get_End_Of_Syntax_Scope_Loc (Current_Entity))
                    and then
                      At_Valid_Line_After (Doc_Start_Line,
                        Get_End_Of_Syntax_Scope_Loc (Current_Entity).Line)
                  then
                     Set_Doc_After (Current_Entity);

                  elsif Present (Current_Entity)
                    and then
                      Doc_Start_Line
                        >= LL.Get_Location (Current_Entity).Line
                  then
                     Set_Doc_After_Current_Entity;

                  else
                     Set_Doc_After_Previous_Entity_In_Scope;
                  end if;

                  Clear_Doc;
               end if;

               if Doc_Start_Line = No_Line then
                  Doc_Start_Line := Sloc_Start.Line;
               end if;

               Append_Comment (S);
               Doc_End_Line := Sloc_End.Line;
            end Accumulate_Comments;

            ----------------------
            -- Aggregate_Length --
            ----------------------

            function Aggregate_Length return Natural is
            begin
               pragma Assert (In_Aggregate);
               return Sloc_Start.Line - Aggr_Begin_Line;
            end Aggregate_Length;

            -------------------------------
            -- Clear_Accumulated_Sources --
            -------------------------------

            procedure Clear_Accumulated_Sources is
            begin
               Last_Idx := 0;
            end Clear_Accumulated_Sources;

            -------------------------
            -- Complete_Decoration --
            -------------------------

            procedure Complete_Decoration (End_Decl_Found : Boolean) is
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

               procedure Decorate_Entity (E : Entity_Id);
               --  Complete the decoration of entity E

               procedure Decorate_Parent_And_Progenitors (E : Entity_Id);
               --  Complete the decoration of entity E

               procedure Decorate_Renaming (E : Entity_Id);
               --  Complete the decoration of a renaming entity

               procedure Decorate_Scope (E : Entity_Id);
               --  Workaround missing decoration of the Scope

               ---------------------
               -- Decorate_Entity --
               ---------------------

               procedure Decorate_Entity (E : Entity_Id) is

                  procedure Fix_Wrong_Xref_Decoration (E : Entity_Id);
                  --  Add minimum correct decoration to an entity which is
                  --  erroneously decorated by Xref

                  -------------------------------
                  -- Fix_Wrong_Xref_Decoration --
                  -------------------------------

                  procedure Fix_Wrong_Xref_Decoration (E : Entity_Id) is
                  begin
                     Set_Has_Incomplete_Decoration (E);

                     if Prev_Token = Tok_Function then
                        Set_Kind (E, E_Function);
                     else
                        Set_Kind (E, E_Procedure);
                     end if;

                     if Get_Short_Name (E) = "" then
                        --  For consistency we remove the string
                        --  terminator to the name of operators

                        declare
                           From : Natural := S'First;
                           To   : Natural := S'Last;
                        begin
                           while From <= To
                             and then S (From) = '"'
                           loop
                              From := From + 1;
                           end loop;

                           while From <= To
                             and then S (To) = '"'
                           loop
                              To := To - 1;
                           end loop;

                           Set_Short_Name
                             (Context, E, S (From .. To));
                        end;
                     end if;
                  end Fix_Wrong_Xref_Decoration;

               begin
                  Decorate_Scope (E);

                  if In_Private_Part (Current_Context)
                    and then Get_Kind (E) /= E_Formal
                  then
                     Set_In_Private_Part (E);

                     if No (Get_First_Private_Entity_Loc
                             (Get_Scope (Current_Context)))
                     then
                        Set_First_Private_Entity_Loc
                          (Get_Scope (Current_Context),
                           General_Location'
                             (File    => File,
                              Project => Context.Project,
                              Line    => Sloc_Start.Line,
                              Column  => To_Visible_Column
                                (Buffer.all,
                                 Sloc_Start.Column,
                                 Sloc_Start.Index)));
                     end if;
                  end if;

                  if (Prev_Token = Tok_Function
                        or else Prev_Token = Tok_Procedure)
                    and then Get_Kind (E) = E_Variable
                  then
                     Fix_Wrong_Xref_Decoration (E);
                  end if;

                  if not In_Generic_Formals
                    and then Is_Compilation_Unit (E)
                  then
                     In_Compilation_Unit := True;
                  end if;

                  if Is_Package (E)
                    and then Is_Expanded_Name (S)
                  then
                     declare
                        Last_Dot_Index : Natural := 0;
                        Parent         : Entity_Id;
                     begin
                        for J in reverse S'Range loop
                           if S (J) = '.' then
                              Last_Dot_Index := J;
                              exit;
                           end if;
                        end loop;

                        Parent :=
                          Builder.Find_Unique_Entity
                            (S (S'First .. Last_Dot_Index - 1),
                             Must_Be_Package => True);

                        if Present (Parent) then
                           pragma Assert (Is_Package (Parent));
                           Set_Parent_Package (E, Parent);
                        end if;
                     end;
                  end if;

                  if Is_Generic (E)
                    and then No (Get_Corresponding_Spec (E))
                  then
                     pragma Assert (Present (Generic_Formals_Loc));
                     Set_Generic_Formals_Loc (E, Generic_Formals_Loc);
                     Generic_Formals_Loc := No_Location;
                     In_Generic_Formals := False;
                     In_Generic_Decl := False;

                     --  For subprograms found in generic formals this code
                     --  is erroneously decorating their formals as generic
                     --  formals???

                     for Formal of Generic_Formals loop
                        Set_Is_Generic_Formal (Formal);

                        --  Adding minimum decoration to undecorated generic
                        --  formals

                        pragma Assert (Get_Kind (E) /= E_Unknown);
                        if Get_Kind (E) = E_Unknown then
                           Set_Kind (E, E_Generic_Formal);
                        end if;

                        if Get_Scope (Formal) /= E
                          or else not Get_Generic_Formals (E).Contains (Formal)
                        then
                           Remove_From_Scope (Formal);
                           Append_Generic_Formal (E, Formal);
                           Set_Scope (Formal, E);
                        end if;
                     end loop;

                     Generic_Formals.Clear;

                  elsif In_Generic_Formals then
                     Generic_Formals.Append (E);

                  else
                     case Prev_Token is
                        when Tok_Subtype =>
                           --  Assertion disabled on generic formals because
                           --  Xref decorates subtypes of generic formals as
                           --  generic formals whose decoration is incomplete
                           --  and we don't have yet a way to fix this wrong
                           --  decoration in the frontend ???

                           if Get_Kind (E) /= E_Generic_Formal then
                              pragma Assert (LL.Is_Type (E));
                           end if;

                           Set_Is_Subtype (E);

                        when Tok_Protected =>
                           Set_Kind (E, E_Single_Protected);

                           if Is_Partial_View (E) then
                              Set_Kind (Get_Full_View (E), E_Single_Protected);

                              if not Processing_Body then
                                 Remove_Full_View (E);

                              else
                                 Set_Corresponding_Body (E, Get_Full_View (E));
                                 Set_Corresponding_Spec (Get_Full_View (E), E);

                                 --  Remove wrong decoration

                                 Set_Partial_View
                                   (Get_Full_View (E), Atree.No_Entity);
                                 Set_Full_View (E, Atree.No_Entity);
                              end if;
                           end if;

                           Set_Is_Incomplete (E, False);

                        when others =>
                           null;
                     end case;
                  end if;
               end Decorate_Entity;

               -------------------------------------
               -- Decorate_Parent_And_Progenitors --
               -------------------------------------

               procedure Decorate_Parent_And_Progenitors (E : Entity_Id) is
               begin
                  pragma Assert (Is_Record_Type (E));

                  if Present (Get_Parent (E)) then

                     if Is_Full_View (E)
                       and then Is_Private (Get_Partial_View (E))
                       and then No (Get_Parent (Get_Partial_View (E)))
                     then
                        Set_Has_Private_Parent (Get_Partial_View (E));
                     end if;

                  else
                     --  In the partial view Xref returns the parent in the
                     --  list of parents (and hence, at current stage it is
                     --  stored in the list of progenitors). We localize it
                     --  and remove it from the list of progenitors.

                     if Is_Partial_View (E) then

                        if Present (Get_Parent (Get_Full_View (E))) then
                           Set_Parent (E, Get_Parent (Get_Full_View (E)));

                        elsif Present (Get_Progenitors (E)) then
                           declare
                              Parent : Entity_Id;
                           begin
                              Parent :=
                                Find_Entity
                                  (Get_Progenitors (E).all,
                                   Name => S);
                              pragma Assert (Present (Parent));

                              Set_Parent (E, Parent);
                              Delete_Entity (Get_Progenitors (E).all, Parent);
                           end;
                        end if;
                     end if;

                     --  We don't know the exact location associated with
                     --  the entity in the database. Hence for now we take a
                     --  conservative approach and we first retry the entity
                     --  from the database and use its location to retry its
                     --  associated unique high-level entity.

                     if No (Get_Parent (E)) then
                        declare
                           Tok_Loc : General_Location;
                           Parent  : Entity_Id := Atree.No_Entity;
                           Dot_Pos : Natural   := 0;

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
                                  (File    => File,
                                   Project => Context.Project,
                                   Line    => Sloc_Start.Line,
                                   Column  => To_Visible_Column
                                     (Buffer.all,
                                      Sloc_Start.Column + Dot_Pos,
                                      Sloc_Start.Index + Dot_Pos));
                              --  ??? multiline qualified identifiers

                              declare
                                 LL_Parent : constant Root_Entity'Class :=
                                   Xref.Get_Entity
                                     (Db   => Context.Database,
                                      Name => Get_Short_Name (S),
                                      Loc  => Tok_Loc);
                              begin
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
                              end;
                           end if;

                           if Present (Parent) then
                              pragma Assert (LL.Is_Type (Parent));
                              Set_Parent (E, Parent);

                              if Get_Progenitors (E) .Contains (Parent) then
                                 Delete_Entity
                                   (Get_Progenitors (E).all, Parent);
                              end if;

                              --  Complete the decoration of E

                              if Is_Tagged (Parent) then
                                 Set_Is_Tagged (E);
                              end if;

                              if Present (Get_Partial_View (E))
                                and then Is_Private (Get_Partial_View (E))
                                and then No (Get_Parent (Get_Partial_View (E)))
                              then
                                 Set_Has_Private_Parent (Get_Partial_View (E));
                              end if;
                           end if;
                        end;
                     end if;
                  end if;
               end Decorate_Parent_And_Progenitors;

               -----------------------
               -- Decorate_Renaming --
               -----------------------

               procedure Decorate_Renaming (E : Entity_Id) is
               begin
                  --  No action needed if already decorated

                  if Present (Get_Alias (E)) then
                     return;
                  end if;

                  --  First try: if the Xref entity of the renaming is
                  --  available then use it to search for the alias in the
                  --  hash table

                  if Present (LL.Get_Alias (E)) then
                     declare
                        Alias : constant Entity_Id :=
                          Find_Unique_Entity
                           (Get_Location (LL.Get_Alias (E)));
                     begin
                        if Present (Alias) then
                           Set_Alias (E, Alias);
                        end if;
                     end;
                  end if;

                  --  Second try: search for it in the xref database using the
                  --  name of the renamed entity and the current location.

                  if No (Get_Alias (E)) then
                     declare
                        Alias : constant Entity_Id :=
                          Get_Entity
                            (Context => Context,
                             Name => S,
                             Loc  => General_Location'
                               (File    => File,
                                Project => Context.Project,
                                Line    => Sloc_Start.Line,
                                Column  =>
                                  To_Visible_Column
                                    (Buffer.all,
                                     Sloc_Start.Column,
                                     Sloc_Start.Index)));
                     begin
                        if Present (Alias) then
                           Set_Alias (E, Alias);
                        end if;
                     end;
                  end if;

                  --  If the renamed entity cannot be located we add a minimum
                  --  decoration which will be used to handle the scopes.

                  if No (Get_Alias (E)) then
                     Set_Is_Alias (E);
                  end if;
               end Decorate_Renaming;

               --------------------
               -- Decorate_Scope --
               --------------------

               procedure Decorate_Scope (E : Entity_Id) is
                  Scope : constant Entity_Id := Get_Scope (Current_Context);

                  procedure Update_Scope;
                  procedure Update_Scope (E : Entity_Id);

                  procedure Update_Scope is
                  begin
                     if Is_Partial_View (E) then
                        Update_Scope (Get_Full_View (E));
                     end if;

                     Update_Scope (E);
                  end Update_Scope;

                  procedure Update_Scope (E : Entity_Id) is
                  begin
                     Remove_From_Scope (E);
                     Append_To_Scope (Scope, E);
                     Set_Scope (E, Scope);
                  end Update_Scope;

               begin
                  if not End_Decl_Found
                    and then Is_Concurrent_Type_Or_Object (Scope)
                  then
                     Update_Scope (E);

                  elsif not End_Decl_Found
                    and then Is_Subprogram_Or_Entry (Scope)
                    and then Get_Kind (E) = E_Variable
                    and then not In_Body (Current_Context)

                     --  Handle formals of anonymous access to subprograms
                    and then Par_Count >= 1
                  then
                     Set_Kind (E, E_Formal);
                     Update_Scope (E);

                  elsif Is_Subprogram (E)
                    and then Is_Generic (Scope)
                  then
                     Update_Scope (E);

                  --  This should be the general case for generics

                  elsif Is_Generic (Scope) then
                     Update_Scope;

                  elsif No (Get_Scope (E)) then
                     Update_Scope;

                  --  Workaround wrong decoration in enumeration types???

                  elsif Get_Kind (E) = E_Enumeration_Type
                    and then Get_Scope (E) /= Scope
                  then
                     Update_Scope;

                  elsif Get_Scope (E) /= Scope then
                     Update_Scope;

                  elsif Is_Partial_View (E)
                    and then Get_Kind (E) /= E_Discriminant
                    and then
                      (Get_Scope (Get_Full_View (E)) /= Scope
                         or else not
                       Get_Entities (Scope).Contains (Get_Full_View (E)))
                  then
                     Update_Scope (Get_Full_View (E));

                  elsif not (Get_Entities (Scope).Contains (E)) then
                     Update_Scope;
                  end if;
               end Decorate_Scope;

               ------------------------------------
               -- Update_End_Of_Profile_Location --
               ------------------------------------

               procedure Update_End_Of_Profile_Location;
               procedure Update_End_Of_Profile_Location is
                  Current_Entity : constant Entity_Id :=
                                     Get_Current_Entity (Current_Context);

                  Scope : constant Entity_Id :=
                            Get_Scope (Current_Context);

                  E : constant Entity_Id :=
                        (if Present (Current_Entity)
                           and then Get_Kind (Current_Entity)
                                      /= E_Discriminant
                           and then Get_Kind (Current_Entity) /= E_Formal
                         then
                            Current_Entity
                         else
                            Scope);
               begin
                  --  If we have processed a single declaration then E
                  --  references it; if we have processed all the formals of
                  --  a subprogram or entry then there is no current entity
                  --  available in the scope

                  if Present (Get_End_Of_Profile_Location (E)) then
                     return;
                  end if;

                  if (Token = Tok_Semicolon
                        and then Par_Count = 0
                        and then Is_Subprogram (E))
                    or else
                      (Token = Tok_Is
                         and then Par_Count = 0
                         and then
                           (Is_Subprogram_Or_Entry (E)
                              or else Is_Concurrent_Type_Or_Object (E)
                              or else Is_Package (E))
                         and then No (LL.Get_Instance_Of (E)))
                  then
                     Set_End_Of_Profile_Location (E,
                       General_Location'
                         (File    => File,
                          Project => Context.Project,
                          Line    => Sloc_Start.Line,
                          Column  => To_Visible_Column
                                       (Buffer.all,
                                        Sloc_Start.Column,
                                        Sloc_Start.Index)));
                  end if;
               end Update_End_Of_Profile_Location;

               ------------------------------------
               -- Update_End_Of_Syntax_Scope_Loc --
               ------------------------------------

               procedure Update_End_Of_Syntax_Scope_Loc;
               procedure Update_End_Of_Syntax_Scope_Loc is
                  Current_Entity : constant Entity_Id :=
                                     Get_Current_Entity (Current_Context);

                  Scope : constant Entity_Id :=
                            Get_Scope (Current_Context);

                  E : constant Entity_Id :=
                        (if Present (Current_Entity)
                           and then Get_Kind (Current_Entity)
                                      /= E_Discriminant
                           and then Get_Kind (Current_Entity) /= E_Formal
                         then
                            Current_Entity
                         else
                            Scope);
               begin
                  if Present (Get_End_Of_Syntax_Scope_Loc (E)) then
                     return;
                  end if;

                  --  If we have processed a single declaration then E
                  --  references it; if we have processed all the formals of
                  --  a subprogram or entry then there is no current entity
                  --  available in the scope

                  --  No action needed if this attribute is already set. When
                  --  we are processing an unit spec this case occurs with
                  --  pragmas located after E.

                  if (Token = Tok_Semicolon and then not Is_Subprogram (E))
                    or else
                      (Token = Tok_Semicolon
                         and then Par_Count = 0
                         and then Is_Subprogram (E))
                  then
                     Set_End_Of_Syntax_Scope_Loc (E,
                       General_Location'
                         (File    => File,
                          Project => Context.Project,
                          Line    => Sloc_Start.Line,
                          Column  => To_Visible_Column
                                       (Buffer.all,
                                        Sloc_Start.Column,
                                        Sloc_Start.Index)));

                     --  Workaround decoration of end location in instances
                     --  Required to handle scopes.

                     if Par_Count = 0
                       and then No (Get_Current_Entity (Current_Context))
                       and then
                         Kind_In (Get_Kind (Get_Scope (Current_Context)),
                           E_Package, E_Procedure, E_Function)
                       and then
                         Present
                           (LL.Get_Instance_Of (Get_Scope (Current_Context)))
                       and then
                         No (Get_End_Of_Scope_Loc
                              (Get_Scope (Current_Context)))
                     then
                        Set_End_Of_Scope_Loc
                          (Get_Scope (Current_Context),
                           General_Location'
                             (File    => File,
                              Project => Context.Project,
                              Line    => Sloc_Start.Line,
                              Column  => To_Visible_Column
                                (Buffer.all,
                                 Sloc_Start.Column,
                                 Sloc_Start.Index)));
                     end if;
                  end if;
               end Update_End_Of_Syntax_Scope_Loc;

            --  Start of processing for Complete_Decoration

            begin
               case Token is
                  when Tok_Generic =>
                     In_Generic_Formals := True;
                     Generic_Formals_Loc :=
                       General_Location'
                         (File    => File,
                          Project => Context.Project,
                          Line    => Sloc_Start.Line,
                          Column  => To_Visible_Column
                            (Buffer.all,
                             Sloc_Start.Column,
                             Sloc_Start.Index));

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

                  when Tok_Pragma =>
                     if In_Private_Part (Current_Context)
                       and then No (Get_First_Private_Entity_Loc
                                     (Get_Scope (Current_Context)))
                     then
                        Set_First_Private_Entity_Loc
                          (Get_Scope (Current_Context),
                           General_Location'
                             (File    => File,
                              Project => Context.Project,
                              Line    => Sloc_Start.Line,
                              Column  => To_Visible_Column
                                (Buffer.all,
                                 Sloc_Start.Column,
                                 Sloc_Start.Index)));
                     end if;

                  when Tok_New =>
                     if Prev_Token = Tok_Is then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           if Is_Record_Type (Scope) then
                              In_Parent_Part := True;
                           elsif Get_Kind (Scope) = E_Enumeration_Type then
                              Set_Is_Subtype (Scope);
                           else
                              In_Derived_Type_Definition := True;
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
                        --  Include in this assertion class-wide types because
                        --  the Xref database decorates abstract tagged records
                        --  as E_Class_Wide types.
                        pragma Assert (Is_Record_Type (Scope)
                          or else Get_Kind (Scope) = E_Class_Wide_Type);
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

                           elsif not In_Type_Definition then
                              pragma Assert (Is_Package (Scope)
                                or else Is_Concurrent_Type_Or_Object (Scope));
                              Set_In_Private_Part (Current_Context);
                           end if;
                        end;
                     end if;

                  --  Expanded names & identifiers

                  when Tok_Id =>
                     Do_Breakpoint;

                     if In_Parent_Part then
                        Decorate_Parent_And_Progenitors
                          (Get_Scope (Current_Context));
                        In_Parent_Part := False;

                     elsif In_Next_Entity then
                        Decorate_Entity (Extended_Cursor.Entity (Cursor));

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

                           Decorate_Renaming (E);
                        end;

                     --  No action yet on derived type definitions but this
                     --  is the right place to complete their decoration.
                     --  More work needed in this area???

                     elsif In_Derived_Type_Definition then
                        null;
                     end if;

                  when Tok_Is =>
                     if Par_Count = 0 then
                        Update_End_Of_Profile_Location;
                     end if;

                  when Tok_Semicolon =>
                     if Par_Count = 0 then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
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

                              Update_End_Of_Syntax_Scope_Loc;
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

               procedure Set_Doc_After_Previous_Entity;
               procedure Set_Doc_Before (E : Entity_Id);

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
                     --  Support for floating comments (currently disabled)

                     if Enhancements then
                        Set_Doc_Before (E,
                          Comment_Result'
                            (Text       => Doc,
                             Start_Line => Doc_Start_Line));

                     elsif Kind_In (Get_Kind (E), E_Enumeration_Literal,
                                                  E_Formal)
                       or else Doc_End_Line = LL.Get_Location (E).Line - 1
                     then
                        Set_Doc_Before (E,
                          Comment_Result'
                            (Text       => Doc,
                             Start_Line => Doc_Start_Line));
                     end if;
                  end if;
               end Set_Doc_Before;

            --  Start of processing for Handle_Doc

            begin
               case Token is

                  --  Expanded names & identifiers

                  when Tok_Id =>

                     --  Nested entities of bodies have been removed and
                     --  need special management here.

                     if Processing_Body
                       and then Present (Doc)
                       and then Present (Get_Scope (Current_Context))
                       and then Is_Concurrent_Type_Or_Object_Body
                                  (Get_Scope (Current_Context))
                       and then
                         Doc_Start_Line =
                           Get_End_Of_Profile_Location
                             (Get_Scope (Current_Context)).Line + 1
                     then
                        Set_Doc_After (Get_Scope (Current_Context));
                        Clear_Doc;

                     --  Documentation located before the first internal
                     --  entity of a subprogram

                     elsif Processing_Body
                       and then In_Next_Entity
                       and then Present (Doc)
                       and then Present (Get_Scope (Current_Context))
                       and then Is_Subprogram (Get_Scope (Current_Context))
                       and then
                         Present
                           (Get_First_Local (Get_Scope (Current_Context)))
                       and then
                         Get_First_Local (Get_Scope (Current_Context))
                           = Extended_Cursor.Entity (Cursor)
                     then
                        if At_Valid_Line_After (Doc_Start_Line,
                             Get_End_Of_Profile_Location
                               (Get_Scope (Current_Context)).Line)
                        then
                           Set_Doc_After (Get_Scope (Current_Context),
                             Forced_Update => True);
                           Clear_Doc;
                        end if;

                     elsif In_Next_Entity
                       and then Present (Doc)
                     then
                        declare
                           E : constant Entity_Id :=
                             Extended_Cursor.Entity (Cursor);
                           Prev_E         : Entity_Id;
                           Prev_E_End_Loc : General_Location := No_Location;

                        begin
                           Prev_E :=
                             Get_Prev_Entity_In_Scope (Current_Context);

                           --  Attach the documentation after the previous
                           --  entity

                           if No (Prev_E) then
                              declare
                                 Scope : constant Entity_Id :=
                                   Get_Scope (Current_Context);
                              begin
                                 if Is_Standard_Entity (Scope) then
                                    Set_Doc_Before (E);

                                 elsif Kind_In (Get_Kind (E),
                                         E_Enumeration_Literal,
                                         E_Formal)
                                 then
                                    Set_Doc_Before (E);

                                 elsif Is_Package (Scope)
                                   or else Is_Concurrent_Type_Or_Object (Scope)
                                 then
                                    Prev_E := Scope;
                                    --  Do not attach this comment to the
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
                              Prev_E_End_Loc :=
                                Get_End_Of_Syntax_Scope_Loc (Prev_E);

                              if Kind_In (Get_Kind (E), E_Enumeration_Literal,
                                                        E_Formal)
                              then
                                 Set_Doc_After (Prev_E);

                              --  Attach the comment to the previous entity
                              --  if the comment is located immediately
                              --  after the end of the previous entity

                              elsif Present (Prev_E_End_Loc)
                                and then
                                  (Prev_E_End_Loc.Line = Doc_Start_Line
                                     or else
                                   Prev_E_End_Loc.Line = Doc_Start_Line - 1)
                              then
                                 Set_Doc_After (Prev_E);
                              end if;
                           end if;

                           --  Attach the documentation before the current
                           --  entity

                           if No (Prev_E) then
                              Set_Doc_Before (E);

                           elsif Get_Kind (E) = E_Enumeration_Literal then
                              Set_Doc_Before (E);

                           --  Do not attach this comment to the current
                           --  entity if it begins in the line where the
                           --  declaration of the previous entity finishes

                           elsif Prev_E_End_Loc.Line = Doc_Start_Line then
                              null;

                           elsif Doc_End_Line
                             = LL.Get_Location (E).Line - 1
                           then
                              Set_Doc_Before (E);

                           --  Floating comment

                           elsif Doc_Start_Line
                             /= LL.Get_Location (Prev_E).Line + 1
                           then
                              Set_Doc_Before (E);
                           end if;

                           --  Reset the documentation
                           Clear_Doc;
                        end;
                     end if;

                  when Tok_Begin =>
                     if Processing_Body and then Present (Doc) then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           if Present (Scope) then
                              if Is_Subprogram_Or_Entry (Scope) then
                                 if At_Valid_Line_After (Doc_Start_Line,
                                      Get_End_Of_Profile_Location
                                        (Scope).Line)
                                 then
                                    Set_Doc_After (Scope,
                                      Forced_Update => True);
                                 end if;

                              elsif Is_Concurrent_Type_Or_Object (Scope) then
                                 Set_Doc_After (Scope);
                              end if;
                           end if;
                        end;
                     end if;

                     Clear_Doc;

                  when Tok_Is =>
                     if Processing_Body and then Present (Doc) then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           if Present (Scope) then
                              if Is_Subprogram_Or_Entry (Scope) then
                                 if At_Valid_Line_Before (Doc_End_Line,
                                      Sloc_Start.Line)
                                 then
                                    Set_Doc_After (Scope);
                                 end if;

                              elsif Is_Concurrent_Type_Or_Object (Scope) then
                                 Set_Doc_After (Scope);
                              end if;
                           end if;
                        end;
                     end if;

                     Clear_Doc;

                  when Tok_Private =>
                     --  No action needed for library level private packages
                     --  since the previous documentation (if any). It will
                     --  be attached before the package.

                     if Is_Standard_Entity (Get_Scope (Current_Context)) then
                        null;

                     elsif Present (Doc) then
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

                  when Tok_Procedure |
                       Tok_Function  |
                       Tok_Entry =>
                     if In_Next_Entity then
                        Set_Doc_Before (Get_Current_Entity (Current_Context));
                     end if;

                  when Tok_Right_Paren =>
                     if Present (Doc) then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           Curr_Entity_In_Scope : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Get_Kind (Scope) = E_Enumeration_Type
                             and then Present (Curr_Entity_In_Scope)
                             and then Get_Kind (Curr_Entity_In_Scope)
                                        = E_Enumeration_Literal
                           then
                              Set_Doc_After (Curr_Entity_In_Scope);

                           elsif Is_Subprogram_Or_Entry (Scope)
                             and then Present (Curr_Entity_In_Scope)
                             and then
                               Get_Kind (Curr_Entity_In_Scope) = E_Formal
                           then
                              Set_Doc_After (Curr_Entity_In_Scope);

                           elsif Present (Curr_Entity_In_Scope)
                             and then Get_Kind (Curr_Entity_In_Scope)
                                         = E_Discriminant
                           then
                              Set_Doc_After (Curr_Entity_In_Scope);
                           end if;

                           Clear_Doc;
                        end;
                     end if;

                  when Tok_Return |
                       Tok_With   =>
                     --  Skip comments located in the profile of subprograms
                     --  before "with" or "return"

                     if Present (Doc)
                       and then Par_Count = 0
                       and then Is_Subprogram (Get_Scope (Current_Context))
                     then
                        Clear_Doc;
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
                        Curr_Entity_In_Scope : constant Entity_Id :=
                          Get_Current_Entity (Current_Context);
                        Scope : constant Entity_Id :=
                          Get_Scope (Current_Context);
                     begin
                        if (Is_Record_Type (Scope)
                             or else Is_Package (Scope)
                             or else Is_Concurrent_Type_Or_Object (Scope))
                          and then Present (Doc)
                        then
                           if Present (Curr_Entity_In_Scope) then
                              Set_Doc_After (Curr_Entity_In_Scope);
                           end if;

                           Clear_Doc;
                        end if;
                     end;

                  when others =>
                     null;
               end case;
            end Handle_Doc;

            -------------------
            -- Handle_Pragma --
            -------------------

            procedure Handle_Pragma is
               Pragma_Id : constant String := To_Lower (S);

            begin
               if Prev_Token /= Tok_Pragma then
                  return;
               end if;

               if Pragma_Id = "ada_83" then
                  Enable_Ada_Version (Ada_83);

               elsif Pragma_Id = "ada_95" then
                  Enable_Ada_Version (Ada_95);

               elsif Pragma_Id = "ada_2005" then
                  Enable_Ada_Version (Ada_2005);

               elsif Pragma_Id = "ada_2012" then
                  Enable_Ada_Version (Ada_2012);
               end if;
            end Handle_Pragma;

            -------------------
            -- Handle_Scopes --
            -------------------

            In_Debug_File : constant Boolean :=
              File.Base_Name = " disabled";

            procedure Handle_Scopes (End_Decl_Found : Boolean) is
               procedure Do_Breakpoint;
               procedure Do_Breakpoint is
               begin
                  if In_Debug_File
                    and then Sloc_Start.Line = 1
                    and then Sloc_Start.Column = 1
                  then
                     Print_State;
                  end if;
               end Do_Breakpoint;

               procedure Do_Exit;
               procedure Do_Exit is

                  procedure Complete_Tagged_Types_Decoration;
                  --  At this stage we have all the information what we can
                  --  collect about tagged types defined in this scope but
                  --  their decoration may be still incomplete. This routine
                  --  is responsible of:
                  --   1) Identifying primitives defined in external libraries.
                  --      They are moved to the list of inherited primitives.
                  --   2) For private types, leave visible in the partial view
                  --      their visible primitives

                  procedure Complete_Tagged_Types_Decoration is

                     function Has_Parent_In_External_Library
                       (Typ : Entity_Id) return Boolean;
                     --  True if tagged type Typ has some parent defined in an
                     --  external library (relies on In_External_Library)

                     function In_External_Library
                       (E : Entity_Id) return Boolean;
                     --  If at this stage we still don't know the scope of an
                     --  entity then we assume that it is defined in a file
                     --  which is not directly part of this project (that is,
                     --  an entity defined in the runtime of the compiler or
                     --  in a library).

                     ------------------------------------
                     -- Has_Parent_In_External_Library --
                     ------------------------------------

                     function Has_Parent_In_External_Library
                       (Typ : Entity_Id) return Boolean
                     is
                        Parent : Entity_Id := Get_Parent (Typ);
                     begin
                        while Present (Parent) loop
                           if In_External_Library (Parent) then
                              return True;
                           end if;

                           Parent := Get_Parent (Parent);
                        end loop;

                        return False;
                     end Has_Parent_In_External_Library;

                     -------------------------
                     -- In_External_Library --
                     -------------------------

                     function In_External_Library
                       (E : Entity_Id) return Boolean is
                     begin
                        return No (Get_Scope (E));
                     end In_External_Library;

                  --  Start of processing for Complete_Tagged_Types_Decoration

                  begin
                     for E of Get_Entities (Get_Scope (Current_Context)).all
                     loop
                        if LL.Is_Type (E)
                          and then Is_Tagged (E)
                          and then (not Is_Partial_View (E)
                                    or else Is_Full_View (E))
                        then
                           declare
                              In_External_Libraries : EInfo_List.Vector;
                           begin
                              --  Collect methods without scope

                              for M of Get_Methods (E).all loop
                                 if In_External_Library (M) then
                                    In_External_Libraries.Append (M);
                                 end if;
                              end loop;

                              --  Append them to the list of inherited prims

                              for M of In_External_Libraries loop
                                 Append_Inherited_Method (E, M);
                              end loop;

                              --  Remove the from the list of new methods

                              for M of In_External_Libraries loop
                                 Remove_From_List (Get_Methods (E), M);
                              end loop;

                              In_External_Libraries.Clear;
                           end;
                        end if;
                     end loop;

                     --  Complete the decoration of tagged private types
                     --  leaving visible in the partial view their visible
                     --  primitives.

                     for E of Get_Entities (Get_Scope (Current_Context)).all
                     loop
                        if LL.Is_Type (E)
                          and then Is_Tagged (E)
                          and then Is_Partial_View (E)
                        then
                           --  Handle case in which this partial view has a
                           --  parent defined in some external library.

                           if Present (Get_Parent (E))
                             and then Has_Parent_In_External_Library (E)
                           then
                              --  In this case, given that we don't have
                              --  available their sources, we assume that
                              --  inherited primitives defined in external
                              --  libraries are visible

                              for M of Get_Inherited_Methods
                                         (Get_Full_View (E)).all
                              loop
                                 if In_External_Library (M) then
                                    Append_Inherited_Method (E, M);
                                 end if;
                              end loop;
                           end if;

                           for M of Get_Methods (Get_Full_View (E)).all loop

                              --  Public primitive

                              if not In_Private_Part (M)
                                and then not Get_Methods (E).Contains (M)
                              then
                                 Append_Method (E, M);
                              end if;
                           end loop;
                        end if;
                     end loop;
                  end Complete_Tagged_Types_Decoration;

               --  Start of processing of Do_Exit

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

                  if not In_Generic_Formals
                    and then Is_Compilation_Unit (Get_Scope (Current_Context))
                  then
                     Disable_Enter_Scope;
                  end if;

                  if Is_Package (Get_Scope (Current_Context)) then
                     Complete_Tagged_Types_Decoration;
                  end if;

                  Exit_Scope;
                  Scope_Level := Scope_Level - 1;
               end Do_Exit;

            --  Start of processing of Handle_Scopes

            begin
               case Token is

                  when Tok_Use =>
                     pragma Assert (Par_Count = 0);
                     if not In_Representation_Clause then
                        In_Skipped_Declaration := True;
                     end if;

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

                  --  Handle variants of record types

                  when Tok_Case =>
                     if Is_Record_Type (Get_Scope (Current_Context)) then

                        --  "end case"

                        if Prev_Token = Tok_End then
                           Nested_Variants_Count := Nested_Variants_Count - 1;
                        else
                           Nested_Variants_Count := Nested_Variants_Count + 1;
                        end if;
                     end if;

                  when Tok_Char_Literal   |
                       Tok_String_Literal =>
                     Do_Breakpoint;

                     if In_Next_Entity then
                        --  For character literals this case occurs when a
                        --  defining a value of an enumeration type. For
                        --  example:
                        --    type Code is ('X', 'Y');

                        --  For String_Literals this case occurs when the wide
                        --  character encoding is used to name the identifier.
                        --  For example:
                        --    ["03C0"] : constant := Pi;

                        Extended_Cursor.Mark_Next_Entity_Seen (Cursor);
                     end if;

                  --  Expanded names & identifiers

                  when Tok_Id =>
                     Do_Breakpoint;

                     if In_Next_Entity then
                        Extended_Cursor.Mark_Next_Entity_Seen (Cursor);
                     end if;

                     if In_Pragma then
                        null;

                     --  Subtype declarations don't open a new scope

                     elsif In_Subtype_Declaration then
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

                  when Tok_Null =>
                     if In_Null_Record then
                        declare
                           E : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Present (E)
                             and then Get_Kind (E) = E_Discriminant
                           then
                              Do_Exit;
                           end if;
                        end;
                     end if;

                  when Tok_Return =>
                     null;

                  when Tok_Right_Paren =>
                     if Par_Count = 0 then
                        declare
                           Scope : constant Entity_Id :=
                                     Get_Scope (Current_Context);
                           E : constant Entity_Id :=
                                 Get_Current_Entity (Current_Context);
                        begin
                           if Present (E) then
                              if In_Ada_Spec then
                                 if Kind_In (Get_Kind (E),
                                      E_Enumeration_Literal,
                                      E_Formal)
                                 then
                                    Do_Exit;
                                 end if;
                              else pragma Assert (In_Ada_Body);
                                 pragma Assert
                                   (Context.Options.Document_Bodies);

                                 if Get_Kind (E) = E_Enumeration_Literal then
                                    Do_Exit;

                                 elsif Is_Subprogram (Scope)
                                   and then not Is_Subprogram_Body (Scope)
                                 then
                                    Do_Exit;
                                 end if;
                              end if;
                           end if;
                        end;
                     end if;

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

                                 elsif Present (E)
                                   and then Get_Kind (E) = E_Discriminant
                                   and then Is_Record_Type (Scope)
                                 then
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

                                    --  Don't leave the scope of full record
                                    --  type declaration until we process "end
                                    --  record" (see management of Tok_End).
                                    --  The exception to this rule are tagged
                                    --  null record declarations. That is:
                                    --
                                    --    type T is tagged null record;

                                    elsif Is_Record_Type (Scope)
                                      and then Is_Full_View (Scope)
                                      and then
                                        Tok_Record_Seen (Current_Context)
                                      and then Prev_Token /= Tok_Record
                                    then
                                       null;

                                    elsif Processing_Body
                                      and then not
                                        In_Concurrent_Type_Definition
                                          (Current_Context)
                                      and then
                                        Present
                                          (Get_Corresponding_Spec (Scope))
                                    then
                                       if (Is_Subprogram (Scope)
                                             or else Is_Package (Scope))
                                         and then Prev_Token = Tok_Separate
                                       then
                                          Do_Exit;

                                       elsif Present
                                               (Get_End_Of_Scope_Loc (Scope))
                                       then
                                          if Get_End_Of_Scope_Loc (Scope).Line
                                            = Sloc_Start.Line
                                          then
                                             Do_Exit;
                                          end if;

                                       elsif Get_Kind (Scope) = E_Entry then
                                          Do_Exit;
                                       end if;

                                    else
                                       Do_Exit;
                                    end if;

                                 --  For packages we exit from the scope when
                                 --  we see their "end" token

                                 elsif Is_Package (Scope) then
                                    null;

                                 elsif Get_Kind (Scope) = E_Entry then
                                    Do_Exit;

                                 elsif Is_Subprogram (Scope) then
                                    if Prev_Token = Tok_Separate then
                                       Do_Exit;
                                    elsif Is_Alias (Scope) then
                                       Do_Exit;
                                    elsif not Processing_Body then
                                       Do_Exit;
                                    else
                                       declare
                                          Loc : General_Location;
                                       begin
                                          Loc := Get_End_Of_Scope_Loc (Scope);

                                          if Loc.File = File
                                            and then Loc.Line = Sloc_Start.Line
                                            and then Natural (Loc.Column)
                                                       = Sloc_Start.Column
                                          then
                                             Do_Exit;
                                          end if;
                                       end;
                                    end if;

                                 --  Handle taft ammendment

                                 elsif Is_Concurrent_Type_Or_Object (Scope)
                                   and then not Present (Get_Entities (Scope))
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
                     elsif In_Null_Record then
                        null;
                     elsif Nested_Variants_Count /= 0 then
                        null;
                     else
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           if not Is_Record_Type (Scope)
                             and then not Is_Package (Scope)
                             and then not Is_Concurrent_Type_Or_Object (Scope)
                           then
                              return;
                           end if;

                           if Is_Record_Type (Scope) then
                              Do_Exit;

                           elsif Processing_Body then
                              if Get_End_Of_Scope_Loc (Scope).Line
                                = Sloc_Start.Line
                              then
                                 Do_Exit;
                              end if;
                           else
                              Do_Exit;
                           end if;
                        end;
                     end if;

                  when Tok_Subtype =>
                     In_Subtype_Declaration := True;

                  when others =>
                     null;
               end case;
            end Handle_Scopes;

            --------------------
            -- Handle_Sources --
            --------------------

            procedure Handle_Sources (End_Decl_Found : Boolean) is

               procedure Append_Src
                 (Text : String; Column : Natural := 0);
               --  Append Text to the accumulated sources stored on Printout
               --  prepending Column spaces. Append it also to Src_Conc_Type
               --  (without prepending any space).

               procedure Clear_Src;
               --  Clear sources accumulated in Printout and Src_Conc_Type.

               procedure Set_Src
                 (E : Entity_Id; Value : Unbounded_String);
               --  Set the sources of E if not previously set; otherwise do
               --  nothing.

               ----------------
               -- Append_Src --
               ----------------

               procedure Append_Src
                 (Text : String; Column : Natural := 0) is
               begin
                  if Column = 0 then
                     Append_Sources (Text);
                  else
                     declare
                        Spaces : constant String (1 .. Column - 1)
                          := (others => ' ');
                     begin
                        Append_Sources (Spaces & Text);
                     end;
                  end if;

                  Append_Conc_Type_Sources (Text);
               end Append_Src;

               ---------------
               -- Clear_Src --
               ---------------

               procedure Clear_Src is
               begin
                  Clear_Sources;
               end Clear_Src;

               --------------
               --  Set_Src --
               --------------

               procedure Set_Src (E : Entity_Id; Value : Unbounded_String) is
               begin
                  if No (Get_Src (E)) then
                     Atree.Set_Src (E, Value);
                  end if;
               end Set_Src;

               --  Local variables

               Scope : constant Entity_Id := Get_Scope (Current_Context);

            --  Start of processing for Handle_Sources

            begin
               if In_Body (Current_Context) then
                  return;
               end if;

               --  Append all text between previous call and current one

               if Last_Idx /= 0 then
                  Append_Sources
                    (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
                  Append_Conc_Type_Sources
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
                           if Get_Kind (E) = E_Variable
                             or else Get_Kind (E) = E_Exception
                           then
                              Clear_Src;
                           end if;
                        end;
                     end if;

                     Append_Src (S);

                  when Tok_Is =>

                     Append_Src (S);

                     --  For bodies of library level tasks, protected types,
                     --  subprograms and entries store only their profile.

                     if Processing_Body then

                        --  Handle body of library level packages, concurrent
                        --  types and subprograms.

                        if Is_Library_Level_Entity (Scope)
                          and then (Present (Get_Corresponding_Spec (Scope))
                                      or else Acts_As_Spec (Scope))
                        then
                           if Present (Get_End_Of_Profile_Location (Scope))
                             and then
                               Sloc_Start.Line
                                 = Get_End_Of_Profile_Location (Scope).Line
                             and then
                               Sloc_Start.Column
                                 = Natural (Get_End_Of_Profile_Location
                                              (Scope).Column)
                           then
                              if Is_Concurrent_Type_Or_Object (Scope) then
                                 Set_Src (Scope, Src_Conc_Type);
                                 Clear_Conc_Type_Sources;

                              elsif Is_Package (Scope)
                                or else Is_Subprogram (Scope)
                              then
                                 Set_Src (Scope, Printout);
                                 Clear_Src;
                              end if;
                           end if;

                        --  Handle entries and subprogram bodies of library
                        --  level concurrent types.

                        elsif (Is_Subprogram_Body (Scope)
                                 or else Is_Entry_Body (Scope))
                          and then
                            Is_Concurrent_Type_Or_Object (Get_Scope (Scope))
                          and then Is_Library_Level_Entity (Get_Scope (Scope))
                        then
                           Set_Src (Scope, Printout);
                           Clear_Src;
                        end if;
                     end if;

                  when Tok_Task      |
                       Tok_Protected =>
                     if Get_Kind (Scope) = E_Interface then
                        Append_Src (S);
                     else
                        Clear_Src;
                        Clear_Conc_Type_Sources;

                        declare
                           Spaces : constant String
                             (1 .. Sloc_Start.Column - 1)
                             := (others => ' ');
                        begin
                           Append_Conc_Type_Sources (Spaces & S);
                        end;
                     end if;

                  when Tok_Type    |
                       Tok_Subtype =>
                     if Prev_Token = Tok_Task
                       or else Prev_Token = Tok_Protected
                     then
                        Append_Src (S);
                     else
                        Clear_Src;
                        Append_Src (S, Sloc_Start.Column);
                     end if;

                  when Tok_Procedure |
                       Tok_Function  |
                       Tok_Entry =>

                     if Par_Count /= 0 then
                        Append_Src (S);
                     else
                        Clear_Src;

                        if Prev_Token = Tok_Overriding then
                           Append_Src
                             ("overriding", Prev_Token_Loc.Column);

                           if Prev_Token_Loc.Line = Sloc_Start.Line then
                              Append_Src (" " & S);
                           else
                              Append_Src ("" & ASCII.LF);
                              Append_Src (S, Sloc_Start.Column);
                           end if;

                        else
                           Append_Src (S, Sloc_Start.Column);
                        end if;
                     end if;

                  when Tok_Package =>
                     Clear_Src;
                     Append_Src (S);

                  when Tok_Semicolon =>
                     Append_Src (S);

                     if Par_Count = 0
                       and then Nested_Variants_Count /= 0
                     then
                        null;

                     elsif Par_Count = 0
                       and then End_Decl_Found
                     then
                        declare
                           E : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Present (E)
                             and then Get_Kind (E) /= E_Formal
                             and then Get_Kind (E) /= E_Discriminant
                           then
                              if Is_Concurrent_Type_Or_Object (E)
                                and then No (Get_Corresponding_Spec (E))
                              then
                                 Set_Src (E, Src_Conc_Type);
                                 Clear_Conc_Type_Sources;

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
                              if No (Get_Src (Scope)) then
                                 Set_Src (Scope, Printout);
                              end if;

                              Clear_Src;

                           elsif Is_Partial_View (Scope) then
                              Set_Src (Scope, Printout);
                              Clear_Src;

                           elsif Is_Subprogram_Or_Entry (Scope) then
                              if not Processing_Body then
                                 Set_Src (Scope, Printout);
                                 Clear_Src;
                              else
                                 if Get_End_Of_Syntax_Scope_Loc (Scope).Line
                                      = Sloc_Start.Line
                                   and then
                                     Natural (Get_End_Of_Syntax_Scope_Loc
                                                (Scope).Column)
                                      = Sloc_Start.Column
                                 then
                                    Set_Src (Scope, Printout);
                                    Clear_Src;
                                 end if;
                              end if;

                           elsif Is_Concurrent_Type_Or_Object (Scope)
                             and then No (E)
                             and then No (Get_Corresponding_Spec (Scope))
                           then
                              Set_Src (Scope, Src_Conc_Type);
                              Clear_Conc_Type_Sources;

                              Clear_Src;

                           elsif Is_Record_Type (Scope) then
                              Set_Src (Scope, Printout);
                              Clear_Src;

                           elsif Get_Kind (Scope) = E_Access_Type then
                              if In_Representation_Clause then
                                 --  We should append here to Scope the
                                 --  sources of the representation clause???
                                 null;
                              else
                                 Set_Src (Scope, Printout);
                              end if;

                              Clear_Src;
                           end if;
                        end;
                     end if;

                  when others =>
                     Append_Src (S);
               end case;
            end Handle_Sources;

            -------------------
            -- Handle_Tokens --
            -------------------

            procedure Handle_Tokens is
               procedure Update_Prev_Known_Token;
               pragma Inline (Update_Prev_Known_Token);

               procedure Update_Prev_Known_Token is
               begin
                  if Token /= Tok_Unknown then
                     Prev_Token := Token;
                     Prev_Token_Loc := Token_Loc;
                  end if;
               end Update_Prev_Known_Token;

            begin
               Update_Prev_Known_Token;

               case Entity is
                  when Aspect_Text =>
                     Token := Tok_Unknown;

                     --  Workaround a problem in the low level parser (that
                     --  is, in Parse_Entities), which may silently skip
                     --  several tokens when processing an Aspect_Text. For
                     --  example, the following text may be associated with
                     --  an Aspect_Text: " Dynamic_Predicate => ("

                     for J in S'Range loop
                        if S (J) = '(' then
                           Par_Count := Par_Count + 1;

                        elsif S (J) = ')' then
                           Par_Count := Par_Count - 1;
                        end if;
                     end loop;

                  when Block_Text | Identifier_Text =>
                     Token := Tok_Id;

                  when Character_Text =>
                     Token := Tok_Char_Literal;

                  when Number_Text =>
                     Token := Tok_Number;

                  when String_Text =>
                     Token := Tok_String_Literal;

                  when Keyword_Text =>
                     Token := Get_Token (S);
                     Set_Token_Seen (Current_Context, Token);

                     case Token is
                        when Tok_Is =>
                           In_Type_Definition := True;

                        when Tok_Procedure |
                             Tok_Function  |
                             Tok_Entry =>
                           In_Item_Decl := True;

                        when Tok_Package =>
                           null;

                        when Tok_Begin =>
                           Set_In_Body (Current_Context);

                        when Tok_End =>
                           declare
                              Scope : constant Entity_Id :=
                                Get_Scope (Current_Context);
                           begin
                              if not In_Item_Decl
                                and then In_Type_Definition
                              then
                                 Set_End_Decl_Found (Current_Context);
                              end if;

                              if In_Concurrent_Type_Definition
                                   (Current_Context)
                              then
                                 Set_End_Of_Scope_Loc (Scope,
                                   General_Location'
                                     (File    => File,
                                      Project => Context.Project,
                                      Line    => Sloc_Start.Line,
                                      Column  => To_Visible_Column
                                        (Buffer.all,
                                         Sloc_Start.Column,
                                         Sloc_Start.Index)));

                              --  Workaround decoration of the end location of
                              --  package declarations whose body is located
                              --  in a separate file. Required to handle its
                              --  scope.

                              elsif Get_Kind (Scope) = E_Package
                                and then No (Get_Corresponding_Spec (Scope))
                                and then
                                  Get_End_Of_Scope_Loc (Scope).File
                                    /= LL.Get_Location (Scope).File
                              then
                                 Set_End_Of_Scope_Loc (Scope,
                                   General_Location'
                                     (File    => File,
                                      Project => Context.Project,
                                      Line    => Sloc_Start.Line,
                                      Column  => To_Visible_Column
                                        (Buffer.all,
                                         Sloc_Start.Column,
                                         Sloc_Start.Index)));
                              end if;
                           end;

                        when Tok_Null =>
                           if Prev_Token = Tok_Record then
                              In_Null_Record := True;
                           end if;

                        when Tok_Record =>
                           if In_Null_Record and then Prev_Token = Tok_End then
                              In_Null_Record := False;
                           end if;

                        when Tok_With =>
                           if In_Item_Decl
                             or else In_Subtype_Declaration
                           then
                              In_Aspect_Spec := True;
                           end if;

                        when others =>
                           null;
                     end case;

                  when Operator_Text  =>
                     Token := Tok_Operator;

                     if S = "(" then

                        --  Skip processing bodies of separate units

                        if Prev_Token = Tok_Separate then
                           pragma Assert (In_Ada_Body
                             and then Processing_Body_Without_Spec);

                           GNAT.IO.Put_Line
                             ("warning: skip processing separate unit "
                              & (+File.Base_Name));

                           raise Separate_Unit;
                        end if;

                        Token := Tok_Left_Paren;
                        Par_Count := Par_Count + 1;

                        if Prev_Token = Tok_Assignment
                          and then Par_Count = 1
                        then
                           In_Aggregate := True;
                           Aggr_Begin_Line := Sloc_Start.Line;
                        end if;

                     elsif S = ")" then
                        if In_Aggregate and then Par_Count = 1 then
                           if Aggregate_Length > Max_Aggregate_Length then
                              Clear_Accumulated_Sources;
                              Append_Sources (" ... ");
                           end if;

                           In_Aggregate := False;
                           Aggr_Begin_Line := 0;
                        end if;

                        Token := Tok_Right_Paren;
                        Par_Count := Par_Count - 1;

                     elsif S = "[" then
                        Token := Tok_Left_Square_Bracket;

                     elsif S = "]" then
                        Token := Tok_Right_Square_Bracket;

                     elsif S = ":=" then
                        Token := Tok_Assignment;

                     elsif S = "=>" then
                        Token := Tok_Arrow;

                     elsif S = ";" then
                        Token := Tok_Semicolon;

                        if Par_Count = 0
                          and then not In_Null_Record
                        then
                           --  Workaround missing decoration of the end
                           --  location of expression functions. Required
                           --  to handle their scope.

                           if Get_Kind (Get_Scope (Current_Context))
                                 = E_Function
                             and then
                               No (Get_End_Of_Scope_Loc
                                     (Get_Scope (Current_Context)))
                             and then
                               Present (Get_End_Of_Profile_Location
                                         (Get_Scope (Current_Context)))
                           then
                              Set_End_Of_Scope_Loc
                                (Get_Scope (Current_Context),
                                 General_Location'
                                   (File    => File,
                                    Project => Context.Project,
                                    Line    => Sloc_Start.Line,
                                    Column  => To_Visible_Column
                                      (Buffer.all,
                                       Sloc_Start.Column,
                                       Sloc_Start.Index)));
                           end if;

                           Set_End_Decl_Found (Current_Context);

                           --  ???may fail with access to subprogram formals
                           if Tok_Subprogram_Seen (Current_Context) then
                              Reset_Tok_Subprogram_Seen (Current_Context);
                           end if;

                           In_Item_Decl := False;
                           In_Subtype_Declaration := False;
                           In_Type_Definition := False;
                           In_Derived_Type_Definition := False;
                        end if;
                     end if;

               when Normal_Text             |
                    Partial_Identifier_Text |
                    Type_Text               |
                    Comment_Text            |
                    Annotated_Keyword_Text  |
                    Annotated_Comment_Text  |
                    Aspect_Comment_Text     |
                    Aspect_Keyword_Text     =>
                  Token := Tok_Unknown;
               end case;

               if Token /= Tok_Unknown then
                  Token_Loc := Sloc_Start;
               end if;
            end Handle_Tokens;

            ---------------
            -- Has_Scope --
            ---------------

            function Has_Scope (E : Entity_Id) return Boolean is
            begin
               return Is_Package (E)
                 or else Is_Subprogram_Or_Entry (E)
                 or else Is_Generic_Subprogram (E)
                 or else Is_Record_Type (E)
                 or else Is_Concurrent_Type_Or_Object (E)
                  --  Include access types to handle the formals of access to
                  --  subprograms
                 or else Get_Kind (E) = E_Access_Type
                 or else Get_Kind (E) = E_Enumeration_Type
                  --  Include class-wide types because the Xref database
                  --  decorates abstract tagged records as E_Class_Wide types
                 or else Get_Kind (E) = E_Class_Wide_Type;
            end Has_Scope;

            --------------------
            -- In_Next_Entity --
            --------------------

            function In_Next_Entity return Boolean is
               Loc         : General_Location;
               Next_Entity : constant Entity_Id :=
                               Extended_Cursor.Entity (Cursor);
            begin
               if No (Next_Entity) then
                  return False;
               end if;

               Loc := LL.Get_Location (Extended_Cursor.Entity (Cursor));

               pragma Assert (Sloc_Start.Line <= Loc.Line
                  or else
                    (Sloc_Start.Line = Loc.Line
                       and then Sloc_Start.Column <= Natural (Loc.Column)));

               --  Handle wide character encoding in identifiers. For example:
               --    ["03C0"] : constant := Pi;

               if Prev_Token = Tok_Left_Square_Bracket
                 and then Token = Tok_String_Literal
               then
                  return Sloc_Start.Line = Loc.Line
                    and then Sloc_End.Line = Loc.Line
                    and then Loc.Column = To_Visible_Column
                     (Buffer.all, Sloc_Start.Column - 1, Sloc_Start.Index - 1);
               else
                  return Sloc_Start.Line = Loc.Line
                    and then Sloc_End.Line = Loc.Line
                    and then Loc.Column >= To_Visible_Column
                      (Buffer.all, Sloc_Start.Column, Sloc_Start.Index)
                    and then Loc.Column <= To_Visible_Column
                      (Buffer.all, Sloc_End.Column, Sloc_End.Index);
               end if;
            end In_Next_Entity;

            -------------------
            -- Set_Doc_After --
            -------------------

            procedure Set_Doc_After
              (E             : Entity_Id;
               Forced_Update : Boolean := False) is
            begin
               if Present (Doc)
                 and then Present (E)
                 and then (No (Get_Doc_After (E)) or else Forced_Update)
               then
                  --  Support for floating comments (currently disabled)

                  if Enhancements
                    or else Kind_In
                      (Get_Kind (E), E_Enumeration_Literal,
                                     E_Formal,
                                     E_Discriminant)
                  then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  elsif Processing_Body
                    and then (Is_Subprogram_Body (E)
                                or else Is_Entry_Body (E))
                  then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  else
                     declare
                        End_Loc : constant General_Location :=
                          Get_End_Of_Syntax_Scope_Loc (E);
                     begin
                        if No (End_Loc) then
                           --  Documentation located immediately after the
                           --  header of a package or concurrent type

                           if (Is_Package (E)
                                 or else Is_Concurrent_Type_Or_Object (E))
                             and then Present (Get_End_Of_Profile_Location (E))
                             and then At_Valid_Line_After (Doc_Start_Line,
                                        Get_End_Of_Profile_Location (E).Line)
                           then
                              Set_Doc_After (E,
                                Comment_Result'
                                  (Text       => Doc,
                                   Start_Line => Doc_Start_Line));
                           end if;

                        --  Documentation located immediately after the end of
                        --  the previous declaration.

                        elsif At_Valid_Line_After (Doc_Start_Line,
                                End_Loc.Line)
                        then
                           Set_Doc_After (E,
                             Comment_Result'
                               (Text       => Doc,
                                Start_Line => Doc_Start_Line));
                        end if;
                     end;
                  end if;
               end if;
            end Set_Doc_After;

            ----------------------------------
            -- Set_Doc_After_Current_Entity --
            ----------------------------------

            procedure Set_Doc_After_Current_Entity is
               Current : constant Entity_Id :=
                 Get_Current_Entity (Current_Context);
            begin
               if Present (Current) then
                  Set_Doc_After (Current);
               end if;
            end Set_Doc_After_Current_Entity;

            --------------------------------------------
            -- Set_Doc_After_Previous_Entity_In_Scope --
            --------------------------------------------

            procedure Set_Doc_After_Previous_Entity_In_Scope is
               Prev_Entity_In_Scope : constant Entity_Id :=
                 Get_Prev_Entity_In_Scope (Current_Context);
            begin
               if Present (Prev_Entity_In_Scope) then
                  Set_Doc_After (Prev_Entity_In_Scope);
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
                 (File,
                  Context.Project,
                  Sloc_Start.Line,
                  To_Visible_Column
                    (Buffer.all, Sloc_Start.Column, Sloc_Start.Index));
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
               Append_To_Map (New_E);

               --  Place this internal entity as the next entity of the
               --  entities iterator

               Extended_Cursor.Set_Next_Entity (Cursor, New_E);
               Replace_Current_Entity (Current_Context, New_E);
            end Build_Missing_Entity;

            procedure Fix_Wrong_Location (E : Entity_Id);
            procedure Fix_Wrong_Location (E : Entity_Id) is
               Loc : constant General_Location :=
                 (File,
                  Context.Project,
                  Sloc_Start.Line,
                  To_Visible_Column
                    (Buffer.all, Sloc_Start.Column, Sloc_Start.Index));
            begin
               LL.Set_Location (E, Loc);

               --  Place E as the next entity of the entities iterator

               Extended_Cursor.Set_Next_Entity (Cursor, E);
               Replace_Current_Entity (Current_Context, E);
            end Fix_Wrong_Location;

            -----------------------------
            -- Handle_Skipped_Entities --
            -----------------------------

            procedure Handle_Skipped_Entities;
            procedure Handle_Skipped_Entities is
            begin
               if In_Next_Entity then
                  declare
                     Scope : constant Entity_Id := Get_Scope (Current_Context);
                     E     : constant Entity_Id :=
                               Extended_Cursor.Entity (Cursor);
                  begin
                     if In_Body (Current_Context) then
                        Set_Is_Skipped (E);

                     elsif Is_Subprogram (Scope)
                       and then Get_Kind (E) /= E_Formal
                     then
                        Set_In_Body (Current_Context);
                        Set_Is_Skipped (E);
                     end if;
                  end;
               end if;
            end Handle_Skipped_Entities;

         --  Start of processing for Parse_Ada_File.CB

         begin
            --  Accumulate documentation found in consecutive comments

            if Entity = Comment_Text
              or else Entity = Annotated_Comment_Text
              or else Entity = Aspect_Comment_Text
            then
               if not In_Private_Part (Current_Context)
                 or else Context.Options.Show_Private
               then
                  Accumulate_Comments;
               end if;

               return False; -- Continue

            elsif Entity = Aspect_Text then

               --  Workaround a wrong token assignment of the underlying
               --  parser because an aspect_text should never have a text
               --  without contents.

               declare
                  Is_Empty_String : Boolean := True;

               begin
                  for J in S'Range loop
                     if S (J) /= ' ' then
                        Is_Empty_String := False;
                        exit;
                     end if;
                  end loop;

                  if not Is_Empty_String then
                     Clear_Doc;
                  end if;
               end;
            end if;

            Handle_Tokens;

            --  Optimization to improve performance processing large
            --  aggregates

            if In_Aggregate then
               if Aggregate_Length <= Max_Aggregate_Length then
                  Handle_Sources (End_Decl_Found => False);
               end if;

               return False; --  Continue
            end if;

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
                            To_Visible_Column
                              (Buffer.all, Sloc_Start.Column, Sloc_Start.Index)
                                 > LL.Get_Location (E).Column))
                  loop
                     --  Handle wide character encoding of identifiers. For
                     --  example:
                     --    ["03C0"] : constant := Pi;

                     if Prev_Token = Tok_Left_Square_Bracket
                       and then Token = Tok_String_Literal
                       and then Sloc_Start.Line = LL.Get_Location (E).Line
                       and then To_Visible_Column
                         (Buffer.all, Sloc_Start.Column, Sloc_Start.Index)
                            = LL.Get_Location (E).Column + 1
                     then
                        exit;
                     end if;

                     --  Disable the check in case of generic packages since
                     --  the compiler may have generated a wrong column
                     --  for its entity (wrong decoration that is fixed
                     --  by this parser at later stages by routine
                     --  Fix_Wrong_Location()). This issue is reproducible
                     --  processing the file s-htable.ads. More work needed
                     --  in this area???

                     --  Disable also the check on entities found in aspect
                     --  specifications since we do not need to process them.

                     Extended_Cursor.Next_Entity (Cursor,
                       Check_Disabled => Get_Kind (E) = E_Generic_Package
                                           or else In_Aspect_Spec);
                     E := Extended_Cursor.Entity (Cursor);
                  end loop;

                  if In_Next_Entity then

                     --  Workaround a problem in the Xref database. Sometimes
                     --  we have 2 entities associated with the name of
                     --  user-defined predefined operators: the first entity
                     --  is erroneously decorated as E_Variable, and the second
                     --  one is the one decorated as E_Function.

                     if S (S'First) = '"'
                       and then
                         (Prev_Token = Tok_Function
                            or else Prev_Token = Tok_Procedure)
                       and then Get_Kind (E) = E_Variable
                     then
                        Extended_Cursor.Next_Entity
                          (Cursor, Check_Disabled => True);
                        E := Extended_Cursor.Entity (Cursor);
                        pragma Assert (In_Next_Entity);
                        pragma Assert (Is_Subprogram (E));
                     end if;

                     Set_Current_Entity (Current_Context, E);

                  elsif Token = Tok_Id
                    and then In_Generic_Decl
                    and then
                      (Prev_Token = Tok_Package
                         or else Prev_Token = Tok_Procedure
                         or else Prev_Token = Tok_Function)
                  then
                     declare
                        Prev_Entity : constant Entity_Id :=
                          Extended_Cursor.Prev_Entity (Cursor);
                     begin
                        if Present (Prev_Entity)
                          and then Is_Package (Prev_Entity)
                          and then Is_Generic (Prev_Entity)
                          and then Get_Short_Name (Prev_Entity) = S
                        then
                           Fix_Wrong_Location (Prev_Entity);
                        else
                           Build_Missing_Entity;
                        end if;
                     end;
                  end if;
               end;

               if In_Pragma then
                  Handle_Pragma;
               else
                  Complete_Decoration (End_Decl_Found);

                  if In_Private_Part (Current_Context)
                    and then not Context.Options.Show_Private
                  then
                     --  Document the last entity of the public part

                     if Token = Tok_Private then
                        Handle_Doc;
                     end if;
                  else
                     Handle_Doc;
                  end if;

                  Handle_Sources (End_Decl_Found);
               end if;

               if Processing_Body then
                  Handle_Skipped_Entities;
               end if;

               Handle_Scopes (End_Decl_Found);

               if End_Decl_Found then
                  Reset_End_Decl_Found (Current_Context);
               end if;
            end;

            return False; --  Continue
         exception
            when Database_Not_Up_To_Date =>
               File_Successfully_Parsed := False;
               return True; --  Stop

            when Separate_Unit =>
               File_Entities.Is_Separate_Unit := True;
               File_Successfully_Parsed := False;
               return True; --  Stop
         end CB;

         ------------------
         -- CB_Body_File --
         ------------------

         After_Subp_Decl : Boolean := False;
         Formal_Count    : Natural := 0;
         Current_Formal  : Entity_Id;

         In_Formal_Default_Value : Boolean := False;

         function CB_Body_File
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);

            S : String renames
                  Buffer_Body (Sloc_Start.Index .. Sloc_End.Index);

            procedure Accumulate_Comments_In_Body;
            procedure Complete_Decoration;
            procedure Handle_Body_Doc;
            procedure Handle_Body_Scopes (End_Decl_Found : Boolean);
            procedure Handle_Body_Tokens;
            function In_Next_Entity return Boolean;
            procedure Set_Doc_After (E : Entity_Id);
            procedure Set_Doc_After_Current_Entity;
            procedure Set_Doc_After_Previous_Entity_In_Scope;

            procedure Print_State;

            ---------------------------------
            -- Accumulate_Comments_In_Body --
            ---------------------------------

            procedure Accumulate_Comments_In_Body is
            begin
               --  Clear the previously accumulated documentation if the
               --  current one is not its continuation

               if Doc_End_Line /= No_Line
                 and then Sloc_Start.Line /= Doc_End_Line + 1
               then
                  declare
                     Scope : constant Entity_Id :=
                       Get_Scope (Current_Context);
                     Current_Entity : constant Entity_Id :=
                       Get_Current_Entity (Current_Context);
                  begin
                     if No (Current_Entity)
                       and then Is_Concurrent_Type_Or_Object (Scope)
                       and then
                         Doc_Start_Line
                           = LL.Get_Location (Scope).Line + 1
                     then
                        Set_Doc_After (Scope);

                     elsif Present (Current_Entity)
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
            end Accumulate_Comments_In_Body;

            -------------------------
            -- Complete_Decoration --
            -------------------------

            procedure Complete_Decoration is

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

            --  Start of processing for Complete_Decoration

            begin
               case Token is

                  when Tok_Right_Paren |
                       Tok_Is          =>
                     if Par_Count = 0
                       and then In_Item_Decl
                     then
                        declare
                           E : constant Entity_Id :=
                             Get_Scope (Current_Context);
                        begin
                           Do_Breakpoint;

                           --  No action needed if this attribute is already
                           --  set. This case occurs with pragmas located
                           --  after E.

                           if Present (E)
                             and then Is_Subprogram (E)
                             and then
                               No (Get_End_Of_Profile_Location_In_Body (E))
                           then
                              declare
                                 Loc : constant General_Location :=
                                   General_Location'
                                     (File    => Current_Body_File,
                                      Project => Context.Project,
                                      Line    => Sloc_Start.Line,
                                      Column  =>
                                        To_Visible_Column
                                          (Buffer_Body.all,
                                           Sloc_Start.Column,
                                           Sloc_Start.Index));
                              begin
                                 Set_End_Of_Profile_Location_In_Body (E, Loc);
                              end;
                           end if;
                        end;
                     end if;

                  when others =>
                     null;
               end case;
            end Complete_Decoration;

            ---------------------
            -- Handle_Body_Doc --
            ---------------------

            procedure Handle_Body_Doc is

               procedure Set_Doc_After_Previous_Entity;
               procedure Set_Doc_Before (E : Entity_Id);

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
                     --  Support for floating comments (currently disabled)

                     if Enhancements then
                        Set_Doc_Before (E,
                          Comment_Result'
                            (Text       => Doc,
                             Start_Line => Doc_Start_Line));

                     elsif Get_Kind (E) = E_Formal then
                        null;

                     elsif Doc_End_Line = LL.Get_Body_Loc (E).Line - 1 then
                        Set_Doc_Before (E,
                          Comment_Result'
                            (Text       => Doc,
                             Start_Line => Doc_Start_Line));
                     end if;
                  end if;
               end Set_Doc_Before;

            --  Start of processing for Handle_Body_Doc

            begin
               --  Handle documentation located after the previous subprogram

               if After_Subp_Decl then
                  declare
                     Prev_E : constant Entity_Id :=
                       Extended_Cursor.Prev_Entity (Cursor);
                  begin
                     --  Attach the comment to the previous entity
                     --  if the comment is located immediately
                     --  after the end of the previous entity

                     if Prev_Token_Loc.Line = Doc_Start_Line
                       or else Prev_Token_Loc.Line = Doc_Start_Line - 1
                     then
                        Set_Doc_After (Prev_E);
                     end if;

                     return;
                  end;
               end if;

               case Token is

                  --  Expanded names & identifiers

                  when Tok_Id =>
                     if In_Next_Entity
                       and then Present (Doc)
                     then
                        declare
                           E : constant Entity_Id :=
                             Extended_Cursor.Entity (Cursor);

                        begin
                           if Doc_End_Line
                             = LL.Get_Body_Loc (E).Line - 1
                           then
                              Set_Doc_Before (E);
                           end if;

                           Clear_Doc;
                        end;

                     --  Appending documentation to the previous formal

                     elsif In_Item_Decl
                       and then Present (Current_Formal)
                     then
                        Set_Doc_After (Current_Formal);
                     end if;

                  when Tok_Is  =>
                     Clear_Doc;

                  when Tok_Procedure |
                       Tok_Function  |
                       Tok_Entry =>
                     if In_Next_Entity then
                        Set_Doc_Before (Get_Current_Entity (Current_Context));
                     end if;

                  when Tok_Right_Paren =>
                     if Present (Doc) then
                        declare
                           Scope : constant Entity_Id :=
                             Get_Scope (Current_Context);
                           Curr_Entity_In_Scope : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Is_Subprogram_Or_Entry (Scope)
                             and then Present (Curr_Entity_In_Scope)
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

                  when others =>
                     null;
               end case;
            end Handle_Body_Doc;

            ------------------------
            -- Handle_Body_Scopes --
            ------------------------

            In_Debug_File : constant Boolean :=
              Current_Body_File.Base_Name = " disabled";

            procedure Handle_Body_Scopes (End_Decl_Found : Boolean) is
               procedure Do_Breakpoint;
               procedure Do_Breakpoint is
               begin
                  if In_Debug_File
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

                  Exit_Scope;
                  Scope_Level := Scope_Level - 1;
               end Do_Exit;

            begin
               case Token is

                  when Tok_Assignment =>
                     if In_Item_Decl
                       and then Par_Count = 1
                     then
                        In_Formal_Default_Value := True;
                     end if;

                  when Tok_Use =>
                     pragma Assert (Par_Count = 0);
                     if not In_Representation_Clause then
                        In_Skipped_Declaration := True;
                     end if;

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
                           pragma Assert (Is_Subprogram (E));
                           Enter_Scope (E);
                           Scope_Level := Scope_Level + 1;

                           if In_Debug_File then
                              GNAT.IO.Put_Line
                                (Scope_Tab (1 .. 2 * Scope_Level)
                                 & Sloc_Start.Line'Img
                                 & " : "
                                 & Sloc_Start.Column'Img
                                 & " : "
                                 & Get_Short_Name (E));
                           end if;

                           Extended_Cursor.Next_Entity (Cursor);
                        end;

                     --  Subprogram formals

                     elsif Par_Count = 1
                       and then In_Item_Decl
                       and then (Prev_Token = Tok_Left_Paren
                                   or else Prev_Token = Tok_Semicolon)
                        --  Skip references found in the expression of a formal
                        --  default value
                       and then not In_Formal_Default_Value
                     then
                        declare
                           E : constant Entity_Id :=
                                 Get_Scope (Current_Context);
                        begin
                           if Present (E)
                             and then Is_Subprogram (E)
                             and then Has_Formals (E)
                           then
                              Formal_Count := Formal_Count + 1;

                              --  We cannot rely on counting the formals since
                              --  Xref does not ensure that formals are
                              --  provided in their order of declaration.

                              Current_Formal := Atree.No_Entity;

                              for Formal of Get_Entities (E).all loop
                                 if Get_Short_Name (Formal) = S then
                                    Current_Formal := Formal;
                                    exit;
                                 end if;
                              end loop;

                              pragma Assert (Present (Current_Formal));
                              Set_Current_Entity
                                (Current_Context, Current_Formal);
                              Clear_Doc;
                           end if;
                        end;
                     end if;

                  when Tok_Right_Paren =>

                     if Par_Count = 0 then
                        if In_Item_Decl then
                           In_Formal_Default_Value := False;
                        end if;

                        declare
                           E : constant Entity_Id :=
                                 Get_Current_Entity (Current_Context);
                        begin
                           if Present (E)
                             and then Get_Kind (E) = E_Formal
                           then
                              Do_Exit;
                           end if;
                        end;
                     end if;

                  when Tok_Semicolon =>
                     Do_Breakpoint;

                     if Par_Count = 1 and then In_Item_Decl then
                        In_Formal_Default_Value := False;

                     elsif Par_Count = 0 and then In_Pragma then
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
                                 if No (E)
                                   and then Is_Subprogram (Scope)
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
                                   and then not Present (Get_Entities (Scope))
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

                  when others =>
                     null;
               end case;
            end Handle_Body_Scopes;

            ------------------------
            -- Handle_Body_Tokens --
            ------------------------

            procedure Handle_Body_Tokens is
               procedure Update_Prev_Known_Token;
               pragma Inline (Update_Prev_Known_Token);

               procedure Update_Prev_Known_Token is
               begin
                  if Token /= Tok_Unknown then
                     Prev_Token := Token;
                     Prev_Token_Loc := Token_Loc;
                  end if;
               end Update_Prev_Known_Token;

            begin
               After_Subp_Decl := False;
               Update_Prev_Known_Token;

               if Prev_Token = Tok_Is
                 and then In_Item_Decl
               then
                  In_Item_Decl := False;
                  In_Formal_Default_Value := False;
                  After_Subp_Decl := True;
               end if;

               case Entity is
                  when Block_Text | Identifier_Text =>
                     Token := Tok_Id;

                     if In_Next_Entity then
                        pragma Assert (In_Item_Decl = False);
                        pragma Assert (In_Formal_Default_Value = False);
                        In_Item_Decl := True;
                        After_Subp_Decl := False;
                        Formal_Count := 0;
                        Current_Formal := Atree.No_Entity;
                     end if;

                  when Number_Text =>
                     Token := Tok_Number;

                  when Keyword_Text =>
                     Token := Get_Token (S);
                     Set_Token_Seen (Current_Context, Token);

                  when Operator_Text  =>
                     Token := Tok_Operator;

                     if S = "(" then
                        Token := Tok_Left_Paren;
                        Par_Count := Par_Count + 1;

                        if Prev_Token = Tok_Assignment
                          and then Par_Count = 1
                        then
                           In_Aggregate := True;
                           Aggr_Begin_Line := Sloc_Start.Line;
                        end if;

                     elsif S = ")" then
                        if In_Aggregate and then Par_Count = 1 then
                           In_Aggregate := False;
                           Aggr_Begin_Line := 0;
                        end if;

                        Token := Tok_Right_Paren;
                        Par_Count := Par_Count - 1;

                     elsif S = ":=" then
                        Token := Tok_Assignment;

                     elsif S = "=>" then
                        Token := Tok_Arrow;

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

               when Normal_Text             |
                    Partial_Identifier_Text |
                    Type_Text               |
                    Comment_Text            |
                    Annotated_Keyword_Text  |
                    Annotated_Comment_Text  |
                    Aspect_Comment_Text     |
                    Aspect_Keyword_Text     |
                    Aspect_Text             |
                    Character_Text          |
                    String_Text             =>
                  Token := Tok_Unknown;
               end case;

               if Token /= Tok_Unknown then
                  Token_Loc := Sloc_Start;
               end if;
            end Handle_Body_Tokens;

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

               Loc := LL.Get_Body_Loc (Extended_Cursor.Entity (Cursor));

               return Sloc_Start.Line = Loc.Line
                 and then Sloc_End.Line = Loc.Line
                 and then Loc.Column >= To_Visible_Column
                   (Buffer_Body.all, Sloc_Start.Column, Sloc_Start.Index)
                 and then Loc.Column <= To_Visible_Column
                   (Buffer_Body.all, Sloc_End.Column, Sloc_End.Index);
            end In_Next_Entity;

            -------------------
            -- Set_Doc_After --
            -------------------

            procedure Set_Doc_After (E : Entity_Id) is
            begin
               if Present (Doc)
                 and then Present (E)
                 and then No (Get_Doc_After (E))
               then
                  --  Support for floating comments (currently disabled)

                  if Enhancements
                    or else Get_Kind (E) = E_Formal
                  then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  elsif Is_Subprogram (E) then
                     declare
                        End_Loc : constant General_Location :=
                          Get_End_Of_Profile_Location_In_Body (E);
                     begin
                        if At_Valid_Line_After (Doc_Start_Line,
                             End_Loc.Line)
                        then
                           Set_Doc_After (E,
                             Comment_Result'
                               (Text       => Doc,
                                Start_Line => Doc_Start_Line));
                        end if;
                     end;
                  end if;
               end if;
            end Set_Doc_After;

            ----------------------------------
            -- Set_Doc_After_Current_Entity --
            ----------------------------------

            procedure Set_Doc_After_Current_Entity is
               Current : constant Entity_Id :=
                 Get_Current_Entity (Current_Context);
            begin
               if Present (Current) then
                  Set_Doc_After (Current);
               end if;
            end Set_Doc_After_Current_Entity;

            --------------------------------------------
            -- Set_Doc_After_Previous_Entity_In_Scope --
            --------------------------------------------

            procedure Set_Doc_After_Previous_Entity_In_Scope is
               Prev_Entity_In_Scope : constant Entity_Id :=
                 Get_Prev_Entity_In_Scope (Current_Context);
            begin
               if Present (Prev_Entity_In_Scope) then
                  Set_Doc_After (Prev_Entity_In_Scope);
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
               GNAT.IO.Put_Line (+Current_Body_File.Full_Name);

               GNAT.IO.Put_Line ("Prev_Token: " & Prev_Token'Img);
               GNAT.IO.Put_Line ("     Token: " & Token'Img);

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
                     GNAT.IO.Put ("Prev_Entity: ");
                     pnsb (Get_Prev_Entity);
                  end if;

                  if Present (Next_E) then
                     GNAT.IO.Put ("Next_Entity:");
                     pnsb (Next_E);
                  end if;
               end;
            end Print_State;

         --  Start of processing for CB_Body_File

         begin
            --  Accumulate documentation found in consecutive comments

            if Entity = Comment_Text
              or else Entity = Annotated_Comment_Text
            then
               Accumulate_Comments_In_Body;
               return False; -- Continue
            end if;

            Handle_Body_Tokens;

            declare
               End_Decl_Found : constant Boolean :=
                 Get_End_Decl_Found (Current_Context);
            begin
               if In_Next_Entity then
                  Set_Current_Entity (Current_Context,
                    Extended_Cursor.Entity (Cursor));
               end if;

               if not In_Pragma then
                  Complete_Decoration;
                  Handle_Body_Doc;
                  --  Processes: Tok_Id, Tok_End, Tok_Semicolon
                  --    Tok_Is, Tok_Procedure, Tok_Function, Tok_Entry
               end if;

               Handle_Body_Scopes (End_Decl_Found);
               --  Processes: Tok_Id, Tok_Semicolon (End_Decl)

               if End_Decl_Found then
                  Reset_End_Decl_Found (Current_Context);
               end if;
            end;

            return False; --  Continue
         end CB_Body_File;

         ---------------------
         -- Extended_Cursor --
         ---------------------

         package body Extended_Cursor is

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

            procedure Next_Entity
              (Cursor         : in out Extended_Cursor;
               Check_Disabled : Boolean := False) is
            begin
               if Present (Cursor.Saved_Next_Entity) then
                  Cursor.Element := Cursor.Saved_Next_Entity;
                  Cursor.Saved_Next_Entity := Atree.No_Entity;
               else
                  if Cursor.Marks_Required
                    and then not Check_Disabled
                    and then not Cursor.Element_Seen
                    and then not In_Pragma
                    and then not In_Body (Current_Context)
                    and then not Is_Separate_Unit (Cursor.Element)
                  then
                     raise Database_Not_Up_To_Date;
                  end if;

                  Cursor.Prev_Element := Cursor.Element;
                  EInfo_List.Next (Cursor.Cursor);
                  Update_Entity (Cursor);
               end if;

               Cursor.Element_Seen := False;
            end Next_Entity;

            procedure Set_Next_Entity
              (Cursor : in out Extended_Cursor;
               Entity : Entity_Id) is
            begin
               pragma Assert (No (Cursor.Saved_Next_Entity));
               pragma Assert (not Cursor.Element_Seen);
               Cursor.Saved_Next_Entity := Cursor.Element;
               Cursor.Element := Entity;
            end Set_Next_Entity;

            procedure Mark_Next_Entity_Seen
              (Cursor : in out Extended_Cursor) is
            begin
               Cursor.Element_Seen := True;
            end Mark_Next_Entity_Seen;

            procedure Initialize
              (Cursor         : in out Extended_Cursor;
               Entities       : access EInfo_List.Vector;
               Marks_Required : Boolean) is
            begin
               Cursor :=
                 (Entities          => Entities,
                  Cursor            => Entities.First,
                  Element           => Atree.No_Entity,
                  Prev_Element      => Atree.No_Entity,
                  Saved_Next_Entity => Atree.No_Entity,
                  Marks_Required    => Marks_Required,
                  Element_Seen      => False);

               Update_Entity (Cursor);
            end Initialize;

         end Extended_Cursor;

         -------------------------------------------
         -- Set_Ada_Version_From_Project_Switches --
         -------------------------------------------

         procedure Set_Ada_Version_From_Project_Switches is
            Is_Default_Value : Boolean;
            Value            : GNAT.Strings.String_List_Access;

         begin
            --  Set gnatdoc default Ada version

            Enable_Ada_Version (Ada_2005);

            --  Check if these sources were compiled with other version

            Switches
              (Project          => Context.Project,
               In_Pkg           => Compiler_Package,
               File             => File,
               Language         => "Ada",
               Value            => Value,
               Is_Default_Value => Is_Default_Value);

            for J in Value.all'Range loop
               declare
                  Switch : constant String := Value.all (J).all;
               begin
                  if Switch = "-gnat83" then
                     Enable_Ada_Version (Ada_83);

                  elsif Switch = "-gnat95" then
                     Enable_Ada_Version (Ada_95);

                  elsif Switch = "-gnat05"
                    or else Switch = "-gnat2005"
                  then
                     Enable_Ada_Version (Ada_2005);

                  elsif Switch = "-gnat12"
                    or else Switch = "-gnat2012"
                  then
                     Enable_Ada_Version (Ada_2012);
                  end if;
               end;
            end loop;

            Free (Value);
         end Set_Ada_Version_From_Project_Switches;

         --  Local variables

         Std_Entity : Entity_Id;

      --  Start of processing for Parse_Ada_File

      begin
         Set_Ada_Version_From_Project_Switches;

         Extended_Cursor.Initialize
           (Cursor, File_Entities.All_Entities'Access, Marks_Required => True);

         if Extended_Cursor.Has_Entity (Cursor) then
            declare
               E : constant Entity_Id := Extended_Cursor.Entity (Cursor);
            begin
               pragma Assert (Is_Standard_Entity (E));

               Std_Entity := E;
               Reset_Stack;
               Enable_Enter_Scope;
               Enter_Scope (Std_Entity);
               Extended_Cursor.Mark_Next_Entity_Seen (Cursor);

               Extended_Cursor.Next_Entity (Cursor);

               Parse_Entities
                 (Lang, Buffer.all (Buffer'First .. Buffer'Last),
                  CB'Unrestricted_Access);

               if not File_Successfully_Parsed then
                  return;
               end if;

               if Natural (New_Entities.Length) > 0 then
                  for E of New_Entities loop
                     File_Entities.All_Entities.Append (E);
                  end loop;

                  EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);
               end if;

               --  Workaround missing decoration of end_location in bodies
               --  of child packages.

               if Get_Scope (Current_Context) /= Std_Entity
                 and then Is_Package_Body (Get_Scope (Current_Context))
                 and then
                   Is_Standard_Entity (Get_Scope (Get_Scope (Current_Context)))
               then
                  Exit_Scope;
               end if;

               if Get_Scope (Current_Context) /= Std_Entity then
                  GNAT.IO.Put_Line
                    (">>> GNATdoc frontend internal error (Code: 05)");
                  raise Database_Not_Up_To_Date;
               end if;

               pragma Assert (Get_Scope (Current_Context) = Std_Entity);
               Exit_Scope;
            end;

            if Context.Options.Process_Bodies then
               declare
                  E : Entity_Id;
               begin
                  Clear_Doc;
                  Clear_Sources;
                  Clear_Conc_Type_Sources;

                  Enable_Enter_Scope;
                  Enter_Scope (Std_Entity);

                  Extended_Cursor.Initialize
                    (Cursor         => Cursor,
                     Entities       => File_Entities.All_Entities'Access,
                     Marks_Required => False);

                  while Extended_Cursor.Has_Entity (Cursor) loop
                     E := Extended_Cursor.Entity (Cursor);

                     if Is_Subprogram (E)
                       and then No (Get_Doc_Before (E))
                       and then No (Get_Doc_After (E))
                       and then Present (LL.Get_Body_Loc (E))
                       and then No (Get_Alias (E))
                     then
                        declare
                           Body_File : constant Virtual_File :=
                             LL.Get_Body_Loc (E).File;
                           End_Scope_File : constant Virtual_File :=
                             Get_End_Of_Scope_Loc (E).File;
                           Lang      : constant Language_Access :=
                             Get_Language_From_File
                               (Context.Lang_Handler, Body_File);
                           In_Ada_Lang : constant Boolean :=
                             Lang.all in Language.Ada.Ada_Language'Class;
                        begin
                           if In_Ada_Lang then
                              Entities_Without_Doc.Append (E);

                              if Body_File /= File
                                  and then Is_Regular_File (Body_File)
                                  and then not
                                    Body_Files_List.Contains (Body_File)

                                 --  Protect us against wrong info in the
                                 --  database

                                  and then Body_File = End_Scope_File
                              then
                                 Body_Files_List.Append (Body_File);
                              end if;
                           end if;
                        end;
                     end if;

                     Extended_Cursor.Next_Entity (Cursor);
                  end loop;

                  EInfo_Vector_Sort_Body_Loc.Sort (Entities_Without_Doc);

                  for Body_File of Body_Files_List loop
                     pragma Assert (Body_File /= File);

                     Current_Body_File := Body_File;
                     Body_File_Entities.Clear;

                     for E of Entities_Without_Doc loop
                        if LL.Get_Body_Loc (E).File = Body_File then
                           Body_File_Entities.Append (E);
                        end if;
                     end loop;

                     Buffer_Body := Read_Source_File (Context, Body_File);

                     Clear_Doc;
                     Extended_Cursor.Initialize
                       (Cursor         => Cursor,
                        Entities       => Body_File_Entities'Access,
                        Marks_Required => False);

                     Clear_Parser_State;

                     Parse_Entities
                       (Lang,
                        Buffer_Body.all
                          (Buffer_Body'First .. Buffer_Body'Last),
                        CB_Body_File'Unrestricted_Access);

                     Free (Buffer_Body);
                  end loop;
               end;
            end if;
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

      ---------------------------------
      -- Remove_Internal_FE_Entities --
      ---------------------------------

      procedure Remove_Internal_FE_Entities is
         Removed_Entities : aliased EInfo_List.Vector;

      begin
         for E of File_Entities.All_Entities loop
            --  The compiler generates some internal functions with name
            --  "type" that confuse the GNATdoc cursors.

            if Get_Short_Name (E) = "declare"
              or else Get_Short_Name (E) = "new"
              or else Get_Short_Name (E) = "type"
            then
               Removed_Entities.Append (E);
            end if;
         end loop;

         for E of Removed_Entities loop
            declare
               Cursor : EInfo_List.Cursor;
            begin
               Cursor := File_Entities.All_Entities.Find (E);
               File_Entities.All_Entities.Delete (Cursor);
            end;
         end loop;
      end Remove_Internal_FE_Entities;

      -------------------------------
      -- Remove_Task_Body_Entities --
      -------------------------------

      procedure Remove_Task_Body_Entities is
         Removed_Entities  : aliased EInfo_List.Vector;
         Entity_With_Scope : Entity_Id := Atree.No_Entity;

      begin
         pragma Assert (In_Ada_Body
           and then Processing_Body_With_Spec);

         for E of File_Entities.All_Entities loop
            if Is_Task_Body (E) then
               pragma Assert (Present (Get_End_Of_Scope_Loc (E)));
               Entity_With_Scope := E;

            elsif Present (Entity_With_Scope) then
               if LL.Get_Location (E)
                    < Get_End_Of_Scope_Loc (Entity_With_Scope)
               then
                  Removed_Entities.Append (E);
               end if;
            end if;
         end loop;

         for E of Removed_Entities loop
            declare
               Cursor : EInfo_List.Cursor;
            begin
               Cursor := File_Entities.All_Entities.Find (E);
               File_Entities.All_Entities.Delete (Cursor);
            end;
         end loop;
      end Remove_Task_Body_Entities;

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

      --------------------
      -- Clean_Entities --
      --------------------

      procedure Clean_Entities is
         Removed_Entities  : aliased EInfo_List.Vector;

      begin
         for E of File_Entities.All_Entities loop
            if Is_Skipped (E) then
               Remove_Doc (E);
               Remove_Src (E);

               Removed_Entities.Append (E);
            end if;
         end loop;

         for E of Removed_Entities loop
            declare
               Cursor : EInfo_List.Cursor;
            begin
               Cursor := File_Entities.All_Entities.Find (E);
               File_Entities.All_Entities.Delete (Cursor);
            end;
         end loop;

         --  Clean sources of internal entities

         for E of File_Entities.All_Entities loop
            if Present (Get_Corresponding_Spec (E)) then

               --  Do not remove the sources of the body of library level
               --  packages, concurrent types and subprograms.

               if Is_Library_Level_Entity (E)
                 and then (Present (Get_Corresponding_Spec (E))
                            or else Acts_As_Spec (E))
                 and then (Is_Package (E)
                            or else Is_Concurrent_Type_Or_Object (E)
                            or else Is_Subprogram (E))
               then
                  null;

               --  Do not remove the sources of the body of subprograms and
               --  entry bodies of library level concurrent types.

               elsif (Is_Subprogram_Body (E) or else Is_Entry_Body (E))
                 and then Present (Get_Scope (E))
                 and then Is_Concurrent_Type_Or_Object (Get_Scope (E))
                 and then Is_Library_Level_Entity (Get_Scope (E))
               then
                  null;

               else
                  Remove_Src (E);
               end if;
            end if;
         end loop;

         --  Remove nested entities of subprogram bodies

         for E of File_Entities.All_Entities loop
            if Present (Get_Corresponding_Spec (E))
              and then Is_Subprogram_Or_Entry (E)
            then
               declare
                  Removed_Entities : aliased EInfo_List.Vector;

               begin
                  for Entity of Get_Entities (E).all loop
                     if Get_Kind (Entity) /= E_Formal then
                        Removed_Entities.Append (Entity);
                     end if;
                  end loop;

                  for Entity of Removed_Entities loop
                     Remove_From_List (Get_Entities (E), Entity);
                  end loop;
               end;
            end if;
         end loop;
      end Clean_Entities;

      -------------------------------
      -- Debug_Output_All_Entities --
      -------------------------------

      procedure Debug_Output_All_Entities;
      procedure Debug_Output_All_Entities is
         In_Debug_File : constant Boolean :=
           (+File.Base_Name) = " disabled";

      begin
         if not In_Debug_File then
            return;
         end if;

         if In_Ada_Body
           and then Processing_Body_With_Spec
         then
            GNAT.IO.Put_Line ("----- All_Entities (Spec)");
            for E of Tree_Spec.All_Entities loop
               pnsb (E);
            end loop;

            GNAT.IO.Put_Line ("----- All_Entities (Body)");
            for E of File_Entities.All_Entities loop
               pnsb (E);
            end loop;
            GNAT.IO.Put ("----- File_Entities.All_Entities (end)");
         else
            GNAT.IO.Put_Line ("----- All_Entities");
            for E of File_Entities.All_Entities loop
               pns (E);
            end loop;
            GNAT.IO.Put ("----- File_Entities.All_Entities (end)");
         end if;

         GNAT.IO.New_Line;
      end Debug_Output_All_Entities;

   --  Start of processing for Add_Documentation_From_Sources

   begin
      Remove_Internal_FE_Entities;
      EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);

      --  Append to the list of entities defined in the body that have a body

      if In_Ada_Body then
         Append_Corresponding_Body_Of_Entities_Defined_In_Body;
      end if;

      --  Append to the list of entities of the body all the entities defined
      --  in the spec that have a body.

      if In_Ada_Body
        and then Processing_Body_With_Spec
      then
         Append_Corresponding_Body_Of_Entities_Defined_In_Spec;
      end if;

      --  Remove from the list of entities all the entities defined in task
      --  bodies since they are not processed by GNATdoc.

      if In_Ada_Body
        and then Processing_Body_With_Spec
      then
         EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);
         Remove_Task_Body_Entities;
      end if;

      EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);
      Debug_Output_All_Entities;

      Buffer := Read_Source_File (Context, File);

      if Is_Spec_File (Context.Kernel, File)
        and then Context.Options.Process_Bodies
      then
         declare
            P_Tree : Project_Tree_Access renames Context.Kernel.Registry.Tree;

         begin
            Body_File := P_Tree.Other_File (File);

            if Body_File /= File and then Is_Regular_File (Body_File) then
               Buffer_Body := Read_Source_File (Context, Body_File);
            else
               Body_File := No_File;
            end if;
         end;
      end if;

      if In_Ada_Lang then
         Parse_Ada_File (Buffer);

         if File_Successfully_Parsed then
            Clean_Entities;
            Prev_Comment_Line := 0;
            For_All (File_Entities.All_Entities, Ada_Set_Doc'Access);
            For_All (File_Entities.All_Entities, Filter_Doc'Access);
         end if;

      else pragma Assert (In_C_Lang);
         For_All (File_Entities.All_Entities, CPP_Get_Source'Access);
         For_All (File_Entities.All_Entities, CPP_Get_Doc'Access);

         For_All (File_Entities.All_Entities, Filter_Doc'Access);
      end if;

      Free (Buffer_Body);
      Free (Buffer);

      return File_Successfully_Parsed;
   end Add_Documentation_From_Sources;

   ----------------
   -- Build_Tree --
   ----------------

   function Build_Tree
     (Context   : access constant Docgen_Context;
      File      : Virtual_File;
      Tree_Spec : access Tree_Type := null) return Tree_Type
   is
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

      if not Add_Documentation_From_Sources
          (Context, File, Tree'Access, Tree_Spec)
      then
         Stop (Doc_Time, GetDoc_Time);

         if not Tree.Is_Separate_Unit then
            Report_Skipped_File (Context.Kernel, File);
         end if;

         return No_Tree;
      end if;

      Stop (Doc_Time, GetDoc_Time);

      --  Step 3: Convert blocks of comments into structured comments

      Start (Comments_Time);
      Build_Structured_Comments (Context, Tree.Tree_Root);
      Stop (Comments_Time, Build_Comments_Time);

      Stop (My_Time, Frontend_Time);

      if Context.Lang_Handler.Get_Language_From_File (Tree.File).all
           in Language.Ada.Ada_Language'Class
        and then Get_Entities (Tree.Tree_Root).Is_Empty
      then
         --  When Ada's compulation unit marked as excluded from documentation
         --  by '@private' tag it doesn't include and entities except empty
         --  'Standard' root package.

         return No_Tree;
      end if;

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

   ------------------------
   -- Find_Unique_Entity --
   ------------------------

   function Find_Unique_Entity
     (Location      : General_Location;
      In_References : Boolean := False) return Entity_Id is
   begin
      return Builder.Find_Unique_Entity (Location, In_References);
   end Find_Unique_Entity;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Tree : Tree_Type) return Boolean is
   begin
      if Tree = No_Tree then
         return True;
      else
         return Get_Entities (Tree.Tree_Root).Is_Empty;
      end if;
   end Is_Empty;

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

   --------------------------
   -- Reserved_Words_Table --
   --------------------------

   package body Reserved_Words_Table is
      Ada_Version : Ada_Version_Type;

      function Hash (Key : String) return Ada.Containers.Hash_Type;

      function Equivalent_Keys (Left, Right : String) return Boolean;

      package Reserved_Words is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Tokens,
         Hash            => Hash,
         Equivalent_Keys => Equivalent_Keys);

      Keywords_Map : Reserved_Words.Map;

      ------------------------
      -- Enable_Ada_Version --
      ------------------------

      procedure Enable_Ada_Version (Version : Ada_Version_Type) is
      begin
         Ada_Version := Version;
      end Enable_Ada_Version;

      ---------------------
      -- Equivalent_Keys --
      ---------------------

      function Equivalent_Keys (Left, Right : String) return Boolean is
      begin
         return Left = Right;
      end Equivalent_Keys;

      ----------
      -- Hash --
      ----------

      function Hash (Key : String) return Ada.Containers.Hash_Type
      is
         type Internal_Hash_Type is range 0 .. 2 ** 31 - 1;
         function Internal is new GNAT.HTable.Hash
           (Header_Num => Internal_Hash_Type);
      begin
         return Ada.Containers.Hash_Type (Internal (Key));
      end Hash;

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token (Word : String) return Tokens is
         Lower_Word : constant String := To_Lower (Word);

         function Get_Ada_95_Token return Tokens;
         function Get_Ada_95_Token return Tokens is
         begin
            if Lower_Word = "abstract" then
               return Tok_Abstract;

            elsif Lower_Word = "aliased" then
               return Tok_Aliased;

            elsif Lower_Word = "protected" then
               return Tok_Protected;

            elsif Lower_Word = "until" then
               return Tok_Until;

            elsif Lower_Word = "requeue" then
               return Tok_Requeue;

            elsif Lower_Word = "tagged" then
               return Tok_Tagged;

            else
               return Tok_Unknown;
            end if;
         end Get_Ada_95_Token;

         function Get_Ada_2005_Token return Tokens;
         function Get_Ada_2005_Token return Tokens is
         begin
            if Lower_Word = "interface" then
               return Tok_Interface;

            elsif Lower_Word = "overriding" then
               return Tok_Overriding;

            elsif Lower_Word = "synchronized" then
               return Tok_Synchronized;

            else
               return Tok_Unknown;
            end if;
         end Get_Ada_2005_Token;

         function Get_Ada_2012_Token return Tokens;
         function Get_Ada_2012_Token return Tokens is
         begin
            if Lower_Word = "some" then
               return Tok_Some;

            else
               return Tok_Unknown;
            end if;
         end Get_Ada_2012_Token;

         --  Local variables

         Cursor  : constant Reserved_Words.Cursor :=
                     Keywords_Map.Find (Lower_Word);
         use type Reserved_Words.Cursor;
      begin
         --  Look for the token in the hash table containing the common
         --  reserved words

         if Cursor /= Reserved_Words.No_Element then
            return Reserved_Words.Element (Cursor);
         end if;

         case Ada_Version is
            when Ada_83 =>
               if Get_Ada_95_Token /= Tok_Unknown
                 or else Get_Ada_2005_Token /= Tok_Unknown
                 or else Get_Ada_2012_Token /= Tok_Unknown
               then
                  return Tok_Id;
               else
                  return Tok_Unknown;
               end if;

            when Ada_95 =>
               if Get_Ada_2005_Token /= Tok_Unknown
                 or else Get_Ada_2012_Token /= Tok_Unknown
               then
                  return Tok_Id;
               else
                  return Get_Ada_95_Token;
               end if;

            when Ada_2005 =>
               if Get_Ada_2012_Token /= Tok_Unknown then
                  return Tok_Id;

               elsif Get_Ada_95_Token /= Tok_Unknown then
                  return Get_Ada_95_Token;

               else
                  return Get_Ada_2005_Token;
               end if;

            when Ada_2012 =>
               if Get_Ada_95_Token /= Tok_Unknown then
                  return Get_Ada_95_Token;

               elsif Get_Ada_2005_Token /= Tok_Unknown then
                  return Get_Ada_2005_Token;

               else
                  return Get_Ada_2012_Token;
               end if;
         end case;
      end Get_Token;

   begin
      for Token in Reserved_Word_Kind'Range loop
         declare
            S    : constant String := Token'Img;
            Word : constant String := To_Lower (S (S'First + 4 .. S'Last));
         begin
            Keywords_Map.Include (Word, Token);
         end;
      end loop;
   end Reserved_Words_Table;

   ------------------
   -- Scopes_Stack --
   ------------------

   package body Scopes_Stack is

      package Stack_Entities_List is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => Context_Id);
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

      function Current_Context return Context_Id is
      begin
         return Stack.First_Element;
      end Current_Context;

      -----------------
      -- Enter_Scope --
      -----------------

      procedure Enter_Scope (Entity : Entity_Id) is
         use type Ada.Containers.Count_Type;
         New_Scope : constant Context_Id := new Context_Info;
      begin
         pragma Assert (Enter_Scope_Enabled);

         --  Initialize the context of the new syntax scope locating its
         --  cursor before its first entity

         New_Scope.Scope := Entity;

         if Is_Record_Type (Entity)
           and then In_Private_Part (Current_Context)
         then
            Set_In_Private_Part (New_Scope);
         end if;

         if Is_Concurrent_Type_Or_Object (Entity)
           and then No (Get_Corresponding_Spec (Entity))
         then
            Set_In_Concurrent_Type_Definition (New_Scope);
         end if;

         --  Propagate attributes to inner scopes

         if not Is_Standard_Entity (Entity) then
            if In_Concurrent_Type_Definition (Current_Context) then
               Set_In_Concurrent_Type_Definition (New_Scope);
            end if;

            if In_Body (Current_Context) then
               Set_In_Body (New_Scope);
            end if;
         end if;

         Stack.Prepend (New_Scope);
      end Enter_Scope;

      ----------------
      -- Exit_Scope --
      ----------------

      procedure Exit_Scope is
         Scope : Context_Id := Current_Context;
      begin
         Free (Scope);
         Stack.Delete_First;
      end Exit_Scope;

      -----------------
      -- Reset_Stack --
      -----------------

      procedure Reset_Stack is
      begin
         Stack.Clear;
      end Reset_Stack;

      -------------
      -- Getters --
      -------------

      function Get_Current_Entity
        (Context : Context_Id) return Entity_Id is
      begin
         return Context.Current_Entity;
      end Get_Current_Entity;

      function Get_End_Decl_Found (Context : Context_Id) return Boolean is
      begin
         return Context.End_Decl_Found;
      end Get_End_Decl_Found;

      function In_Body
        (Context : Context_Id) return Boolean is
      begin
         return Context.In_Body;
      end In_Body;

      function In_Concurrent_Type_Definition
        (Context : Context_Id) return Boolean is
      begin
         return Context.In_Concurrent_Type_Definition;
      end In_Concurrent_Type_Definition;

      function In_Private_Part
        (Context : Context_Id) return Boolean is
      begin
         return Context.In_Private_Part;
      end In_Private_Part;

      function Get_Prev_Entity_In_Scope
        (Context : Context_Id) return Entity_Id is
      begin
         return Context.Prev_Entity_In_Scope;
      end Get_Prev_Entity_In_Scope;

      function Get_Scope (Context : Context_Id) return Entity_Id is
      begin
         return Context.Scope;
      end Get_Scope;

      function Tok_Record_Seen (Context : Context_Id) return Boolean is
      begin
         return Context.Token_Seen (Tok_Record);
      end Tok_Record_Seen;

      function Tok_Subprogram_Seen (Context : Context_Id) return Boolean is
      begin
         return Context.Token_Seen (Tok_Procedure)
           or else Context.Token_Seen (Tok_Function)
           or else Context.Token_Seen (Tok_Entry);
      end Tok_Subprogram_Seen;

      --  Setters ---------------------------------------------------

      procedure Set_Current_Entity
        (Context : Context_Id; Entity : Entity_Id) is
      begin
         Context.Prev_Entity_In_Scope := Context.Current_Entity;
         Replace_Current_Entity (Context, Entity);
      end Set_Current_Entity;

      procedure Replace_Current_Entity
        (Context : Context_Id; Entity : Entity_Id) is
      begin
         Context.Current_Entity := Entity;
      end Replace_Current_Entity;

      procedure Reset_Tok_Subprogram_Seen
        (Context : Context_Id) is
      begin
         Context.Token_Seen (Tok_Procedure) := False;
         Context.Token_Seen (Tok_Function) := False;
         Context.Token_Seen (Tok_Entry) := False;
      end Reset_Tok_Subprogram_Seen;

      procedure Reset_End_Decl_Found (Context : Context_Id) is
      begin
         Context.End_Decl_Found := False;
      end Reset_End_Decl_Found;

      procedure Set_End_Decl_Found (Context : Context_Id) is
      begin
         Context.End_Decl_Found := True;
      end Set_End_Decl_Found;

      procedure Set_In_Body
        (Context : Context_Id) is
      begin
         Context.In_Body := True;
      end Set_In_Body;

      procedure Set_In_Concurrent_Type_Definition
        (Context : Context_Id) is
      begin
         Context.In_Concurrent_Type_Definition := True;
      end Set_In_Concurrent_Type_Definition;

      procedure Set_In_Private_Part
        (Context : Context_Id) is
      begin
         Context.In_Private_Part := True;
      end Set_In_Private_Part;

      procedure Set_Token_Seen
        (Context : Context_Id; Token : Tokens) is
      begin
         Context.Token_Seen (Token) := True;
      end Set_Token_Seen;

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
      end Print_Scopes;
   end Scopes_Stack;

   -----------------------
   -- To_Visible_Column --
   -----------------------

   function To_Visible_Column
     (Buffer          : String;
      Index_In_Line   : Natural;
      Index_In_Buffer : Natural) return Visible_Column_Type
   is
      Aux_Index : String_Index_Type :=
        String_Index_Type
          (GNATCOLL.Utils.Line_Start (Buffer, Index_In_Buffer));
      Column    : Visible_Column_Type;

   begin
      String_Utils.Skip_To_Index
        (Buffer, Column, String_Index_Type (Index_In_Line), Aux_Index);

      return Column;
   end To_Visible_Column;

end GNATdoc.Frontend;
