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

with Ada.Containers.Indefinite_Hashed_Maps;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Basic_Types;              use Basic_Types;
with GNATdoc.Comment;          use GNATdoc.Comment;
with GNATdoc.Utils;            use GNATdoc.Utils;
with GNATdoc.Errout;           use GNATdoc.Errout;
with GNATdoc.Frontend.Builder; use GNATdoc.Frontend.Builder;
with GNATdoc.Time;             use GNATdoc.Time;
with GNATCOLL.Utils;
with GNAT.Expect;
with GNAT.HTable;
with GNAT.Regpat;              use GNAT.Regpat;
with GNAT.Strings;             use GNAT.Strings;
with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Language.Tree;            use Language.Tree;
with Language.Tree.Database;   use Language.Tree.Database;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with String_Utils;
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
      Tok_Char_Literal,
      Tok_String_Literal,
      Tok_Id,                --  Expanded names & identifiers
      Tok_Number,
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
      Tok_Overriding,
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
      Tok_Arrow,
      Tok_Assignment,
      Tok_Left_Paren,
      Tok_Right_Paren,
      Tok_Left_Square_Bracket,
      Tok_Right_Square_Bracket,
      Tok_Semicolon);

   subtype Reserved_Word_Kind is Tokens range Tok_Abstract .. Tok_With;

   ----------------------
   -- Local_Subrograms --
   ----------------------

   function Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Boolean;
   --  Add to the nodes their blocks of documentation & sources. Returns false
   --  if we found discrepancies between the contents of the database and the
   --  parsed sources.

   procedure Build_Structured_Comments
     (Context : access constant Docgen_Context;
      Root    : Entity_Id);
   --  Traverse the Tree of entities and replace blocks of comments by
   --  structured comments.

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
         End_Decl_Found       : Boolean := False;
         In_Private_Part      : Boolean := False;
         Token_Seen           : Token_Flags := (others => False);
      end record;

      procedure Free is
        new Ada.Unchecked_Deallocation (Context_Info, Context_Id);

      pragma Inline (Current_Context);
      pragma Inline (Enter_Scope);
      pragma Inline (Exit_Scope);

      pragma Inline (Get_Current_Entity);
      pragma Inline (Get_End_Decl_Found);
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
      pragma Inline (Set_In_Private_Part);
      pragma Inline (Set_Token_Seen);

   end Scopes_Stack;
   use Scopes_Stack;

   ------------------------------------
   -- Add_Documentation_From_Sources --
   ------------------------------------

   function Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Boolean
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

      procedure Swap_Buffers;
      --  Swap the contents of Buffer and C_Headers_Buffer. Used to retrieve
      --  the sources and the documentation located in the header file.

      -----------------
      -- Ada_Set_Doc --
      -----------------

      Prev_Comment_Line : Natural := 0;

      procedure Ada_Set_Doc (E : Entity_Id) is

         function May_Have_Tags (Text : Unbounded_String) return Boolean;
         --  Return true if Text may contain some tag

         procedure Update_Prev_Comment_Line (Line : Natural);

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
         Printout_Plain : Unbounded_String;

         New_Entities       : EInfo_List.Vector;
         Body_File_Entities : aliased EInfo_List.Vector;
         Current_Body_File  : Virtual_File;

         Database_Not_Up_To_Date : exception;
         --  Raised when the parser detects discrepancies between the contents
         --  of the database and the sources.

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
               Entities       : access EInfo_List.Vector;
               Cursor         : EInfo_List.Cursor;
               Element        : Entity_Id := Atree.No_Entity;
               Prev_Element   : Entity_Id := Atree.No_Entity;
               Marks_Required : Boolean := False;
               Element_Seen   : Boolean := False;
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
         Generics_Nesting_Level : Natural := 0;
         In_Generic_Formals     : Boolean := False;
         In_Generic_Decl        : Boolean := False;
         Generic_Formals        : EInfo_List.Vector;
         Generic_Formals_Loc    : General_Location := No_Location;
         --  Location of the word "generic"

         Max_Aggregate_Length : constant := 25;
         In_Aggregate         : Boolean := False;
         Aggr_Begin_Line      : Natural := 0;

         In_Type_Definition   : Boolean := False;
         --  Set to true when we see the token "is"

         In_Derived_Type_Definition : Boolean := False;
         --  Set to true when we see the sequence of tokens "is new"

         In_Item_Decl     : Boolean := False;
         --  Set to true when we see "procedure", "function" or "entry"

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
            Generics_Nesting_Level := 0;
            In_Generic_Formals     := False;
            In_Generic_Decl        := False;
            Generic_Formals.Clear;
            Generic_Formals_Loc    := No_Location;

            In_Aggregate    := False;
            Aggr_Begin_Line := 0;

            In_Type_Definition := False;
            In_Derived_Type_Definition := False;
            In_Item_Decl    := False;

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
            procedure Handle_Scopes (End_Decl_Found : Boolean);
            procedure Handle_Sources (End_Decl_Found : Boolean);
            procedure Handle_Tokens;

            function Has_Scope (E : Entity_Id) return Boolean;
            function In_Next_Entity return Boolean;

            procedure Set_Doc_After (E : Entity_Id);
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
                              Project => No_Project,  --  ??? unknown
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

                  if Is_Generic (E) then
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
                           pragma Assert (LL.Is_Type (E));
                           Set_Is_Subtype (E);

                        when Tok_Protected =>
                           Set_Kind (E, E_Single_Protected);
                           Set_Is_Incomplete (E, False);
                           Remove_Full_View (E);

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
                           Tok_Loc   : General_Location;
                           Parent    : Entity_Id := Atree.No_Entity;
                           Dot_Pos   : Natural   := 0;

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
                                   Project => No_Project, --  ??? unknown
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
                                Project => No_Project, --  ???
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
                    and then Is_Subprogram (Scope)
                    and then Get_Kind (E) = E_Variable
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

            --  Start of processing for Complete_Decoration

            begin
               case Token is
                  when Tok_Generic =>
                     In_Generic_Formals := True;
                     Generic_Formals_Loc :=
                       General_Location'
                         (File    => File,
                          Project => GNATCOLL.Projects.No_Project, --  ???
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
                              Project => GNATCOLL.Projects.No_Project, --  ???
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

                              --  If we have processed a single declaration
                              --  then E references it; if we have processed
                              --  all the formals of a subprogram or entry
                              --  then there is no current entity available
                              --  in the scope

                              declare
                                 Loc : constant General_Location :=
                                   General_Location'
                                     (File    => File,
                                      Project => No_Project, --  ???
                                      Line    => Sloc_Start.Line,
                                      Column  =>
                                        To_Visible_Column
                                          (Buffer.all,
                                           Sloc_Start.Column,
                                           Sloc_Start.Index));
                                 Current_Entity : constant Entity_Id :=
                                   Get_Current_Entity (Current_Context);
                                 E : constant Entity_Id :=
                                   (if Present (Current_Entity)
                                      and then Get_Kind (Current_Entity)
                                                 /= E_Discriminant
                                    then
                                       Current_Entity
                                    else
                                       Scope);

                              begin
                                 --  No action needed if this attribute is
                                 --  already set. This case occurs with
                                 --  pragmas located after E.

                                 if No (Get_End_Of_Syntax_Scope_Loc (E)) then
                                    Set_End_Of_Syntax_Scope_Loc (E, Loc);
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
                                 if Kind_In (Get_Kind (E),
                                      E_Enumeration_Literal,
                                      E_Formal)
                                 then
                                    Set_Doc_Before (E);

                                 elsif Is_Concurrent_Type_Or_Object (Scope)
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

                              --  Do not attach the comment to the previous
                              --  line if it precedes the current entity

                              elsif Doc_End_Line
                                /= LL.Get_Location (E).Line - 1
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

            -------------------
            -- Handle_Scopes --
            -------------------

            In_Debug_File : constant Boolean :=
              File.Base_Name = " disabled";

            procedure Handle_Scopes (End_Decl_Found : Boolean) is
               procedure Do_Breakpoint;
               procedure Do_Breakpoint is
               begin
                  if False
                    and then In_Debug_File
                    and then Sloc_Start.Line = 1
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

                  if Is_Generic (Get_Scope (Current_Context)) then
                     Generics_Nesting_Level := Generics_Nesting_Level - 1;
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

                  --  Handle variants of record types

                  when Tok_Case =>

                     pragma Assert
                       (Is_Record_Type (Get_Scope (Current_Context)));

                     --  "end case"

                     if Prev_Token = Tok_End then
                        Nested_Variants_Count := Nested_Variants_Count - 1;
                     else
                        Nested_Variants_Count := Nested_Variants_Count + 1;
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

                  when Tok_Return =>
                     null;

                  when Tok_Right_Paren =>
                     if Par_Count = 0 then
                        declare
                           E : constant Entity_Id :=
                                 Get_Current_Entity (Current_Context);
                        begin
                           if Present (E)
                             and then Kind_In (Get_Kind (E),
                                        E_Enumeration_Literal,
                                        E_Formal)
                           then
                              Do_Exit;
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
                 (Text : String; Column : Natural := 0);

               procedure Clear_Src;

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
                           if Get_Kind (E) = E_Variable
                             or else Get_Kind (E) = E_Exception
                           then
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
                           Scope : constant Entity_Id :=
                                     Get_Scope (Current_Context);
                           E : constant Entity_Id :=
                             Get_Current_Entity (Current_Context);
                        begin
                           if Present (E)
                             and then Get_Kind (E) /= E_Formal
                             and then Get_Kind (E) /= E_Discriminant
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

                           elsif Is_Record_Type (Scope) then
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

                        when Tok_End =>
                           if not In_Item_Decl
                             and then In_Type_Definition
                           then
                              Set_End_Decl_Found (Current_Context);
                           end if;

                        when Tok_Type =>
                           null;

                        when others =>
                           null;
                     end case;

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

                        if Par_Count = 0 then
                           Set_End_Decl_Found (Current_Context);

                           --  ???may fail with access to subprogram formals
                           if Tok_Subprogram_Seen (Current_Context) then
                              Reset_Tok_Subprogram_Seen (Current_Context);
                           end if;

                           In_Item_Decl := False;
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
                    Aspect_Keyword_Text     |
                    Aspect_Text             =>
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

            procedure Set_Doc_After (E : Entity_Id) is
            begin
               if Present (Doc)
                 and then Present (E)
                 and then No (Get_Doc_After (E))
               then
                  --  Support for floating comments (currently disabled)

                  if Enhancements then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  elsif Kind_In (Get_Kind (E), E_Enumeration_Literal,
                                               E_Formal)
                  then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  else
                     declare
                        End_Loc : constant General_Location :=
                          (if Is_Subprogram_Or_Entry (E) then
                              Get_End_Of_Profile_Location (E)
                           else Get_End_Of_Syntax_Scope_Loc (E));
                     begin
                        if No (End_Loc) then
                           --  Documentation located immediately after the
                           --  header of a package or concurrent type

                           if Is_Concurrent_Type_Or_Object (E)
                             or else Is_Package (E)
                           then
                              if Doc_Start_Line = LL.Get_Location (E).Line
                                or else
                                 Doc_Start_Line = LL.Get_Location (E).Line + 1
                              then
                                 Set_Doc_After (E,
                                   Comment_Result'
                                     (Text       => Doc,
                                      Start_Line => Doc_Start_Line));
                              end if;
                           end if;

                        elsif Doc_Start_Line = End_Loc.Line
                          or else Doc_Start_Line = End_Loc.Line + 1
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
                  GNATCOLL.Projects.No_Project,  --  ??? unknown
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
                  GNATCOLL.Projects.No_Project, --  ??? unknown
                  Sloc_Start.Line,
                  To_Visible_Column
                    (Buffer.all, Sloc_Start.Column, Sloc_Start.Index));
            begin
               LL.Set_Location (E, Loc);

               --  Place E as the next entity of the entities iterator

               Extended_Cursor.Set_Next_Entity (Cursor, E);
               Replace_Current_Entity (Current_Context, E);
            end Fix_Wrong_Location;

         --  Start of processing for Parse_Ada_File.CB

         begin
            --  Accumulate documentation found in consecutive comments

            if Entity = Comment_Text
              or else Entity = Annotated_Comment_Text
            then
               if not In_Private_Part (Current_Context)
                 or else Context.Options.Show_Private
               then
                  Accumulate_Comments;
               end if;

               return False; -- Continue
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

                     Extended_Cursor.Next_Entity (Cursor,
                       Check_Disabled => Get_Kind (E) = E_Generic_Package);
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
                     pragma Assert (Generics_Nesting_Level > 0);

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

               if not In_Pragma then
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

               Handle_Scopes (End_Decl_Found);

               if End_Decl_Found then
                  Reset_End_Decl_Found (Current_Context);
               end if;
            end;

            return False; --  Continue
         exception
            when Database_Not_Up_To_Date =>
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

            -------------------------
            -- Accumulate_Comments --
            -------------------------

            procedure Accumulate_Comments_In_Body is
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
                                      Project => No_Project,  --  ??? unknown
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

            -------------------
            -- Handle_Scopes --
            -------------------

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

            -------------------
            -- Handle_Tokens --
            -------------------

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

                  if Enhancements then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  elsif Get_Kind (E) = E_Formal then
                     Set_Doc_After (E,
                       Comment_Result'
                         (Text       => Doc,
                          Start_Line => Doc_Start_Line));

                  elsif Is_Subprogram (E) then
                     declare
                        End_Loc : constant General_Location :=
                          Get_End_Of_Profile_Location_In_Body (E);
                     begin
                        if Doc_Start_Line = End_Loc.Line
                          or else Doc_Start_Line = End_Loc.Line + 1
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

            procedure Next_Entity
              (Cursor         : in out Extended_Cursor;
               Check_Disabled : Boolean := False) is
            begin
               if Present (Saved_Next_Entity) then
                  Cursor.Element := Saved_Next_Entity;
                  Saved_Next_Entity := Atree.No_Entity;
               else
                  if Cursor.Marks_Required
                    and then not Check_Disabled
                    and then not Cursor.Element_Seen
                  then
                     File_Successfully_Parsed := False;
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
               pragma Assert (No (Saved_Next_Entity));
               pragma Assert (not Cursor.Element_Seen);
               Saved_Next_Entity := Cursor.Element;
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
                 (Entities       => Entities,
                  Cursor         => Entities.First,
                  Element        => Atree.No_Entity,
                  Prev_Element   => Atree.No_Entity,
                  Marks_Required => Marks_Required,
                  Element_Seen   => False);

               Update_Entity (Cursor);
            end Initialize;

         end Extended_Cursor;

         --  Local variables

         Std_Entity : Entity_Id;

      --  Start of processing for Parse_Ada_File

      begin
         Extended_Cursor.Initialize
           (Cursor, File_Entities.All_Entities'Access, Marks_Required => True);

         if Extended_Cursor.Has_Entity (Cursor) then
            declare
               E : constant Entity_Id := Extended_Cursor.Entity (Cursor);
            begin
               pragma Assert (Is_Standard_Entity (E));

               Std_Entity := E;
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

               pragma Assert (Get_Scope (Current_Context) = Std_Entity);
               Exit_Scope;
            end;

            if Context.Options.Process_Bodies then
               declare
                  E : Entity_Id;
               begin
                  Clear_Doc;
                  Clear_Sources;
                  Clear_Plain_Sources;

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
                             LL.Get_End_Of_Scope_Loc (E).File;
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

               elsif Tag_Name = "description"
                 or else Tag_Name = "summary"
               then
                  if not Is_Package (E)
                    and then not Is_Subprogram_Or_Entry (E)
                  then
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
                           Cursor : Tag_Cursor;
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
                                 Text : String renames
                                   S (Text_Loc.First .. Text_Loc.Last);
                              begin
                                 Append_Text (Current, Text);
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

         elsif Get_Kind (Entity) = E_Enumeration_Type then
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

   ----------------
   -- Build_Tree --
   ----------------

   function Build_Tree
     (Context : access constant Docgen_Context;
      File    : Virtual_File) return Tree_Type
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

      if not Add_Documentation_From_Sources (Context, File, Tree'Access) then
         Stop (Doc_Time, GetDoc_Time);
         Report_Skipped_File (Context.Kernel, File);

         return No_Tree;
      end if;

      Stop (Doc_Time, GetDoc_Time);

      --  Step 3: Convert blocks of comments into structured comments

      Start (Comments_Time);
      Build_Structured_Comments (Context, Tree.Tree_Root);
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

      function Hash (Key : String) return Ada.Containers.Hash_Type;

      function Equivalent_Keys (Left, Right : String) return Boolean;

      package Reserved_Words is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Tokens,
         Hash            => Hash,
         Equivalent_Keys => Equivalent_Keys);

      Keywords_Map : Reserved_Words.Map;

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
         Cursor  : constant Reserved_Words.Cursor :=
                     Keywords_Map.Find (To_Lower (Word));
         use type Reserved_Words.Cursor;
      begin
         if Cursor /= Reserved_Words.No_Element then
            return Reserved_Words.Element (Cursor);
         else
            return Tok_Unknown;
         end if;
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
         null;
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
