-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Language_Custom is

   type String_Access is access all String;

   --------------------------------
   -- Highlighting in the editor --
   --------------------------------

   type Language_Entity is
     (Normal_Text,
      Identifier_Text,
      Keyword_Text,
      Comment_Text,
      Character_Text,
      String_Text);
   pragma Convention (C, Language_Entity);
   --  The entities found in a language, and that can have a different scheme
   --  for colors highlighting.

   type Casing_Type is (Unchanged, Upper, Lower, Mixed, Smart_Mixed);
   for Casing_Type'Size use Integer'Size;
   pragma Convention (C, Casing_Type);
   --  Casing used for identifiers and reserved words.
   --  Only relevant for case insensitive languages.
   --  - Mixed: Set first character of each word and characters after an
   --    underscore to upper-case, all other characters are set to lower-case.
   --  - Smart_Mixed: As Mixed but never force an upper-case to lower-case.

   type Source_Location is record
      Line   : Natural := 0;
      --  Line number for this entity. Line numbers start at 1.

      Column : Natural := 0;
      --  Column number for this entity. This is an index in bytes, not
      --  characters. This is set to 0 if the end is on the last character of
      --  the previous line.
      --  Column numbers start at 1 otherwise.

      Index  : Natural := 0;
      --  Index in the buffer for this entity
   end record;
   pragma Convention (C, Source_Location);

   type Indent_Parameters is record
      Indent_Level      : Natural;
      Indent_Continue   : Natural;
      Indent_Decl       : Natural;
      Tab_Width         : Natural;
      Indent_Case_Extra : Boolean;
      Reserved_Casing   : Casing_Type;
      Ident_Casing      : Casing_Type;
      Format_Operators  : Boolean;
      Use_Tabs          : Boolean;
      Align_On_Colons   : Boolean;
      Align_On_Arrows   : Boolean;
   end record;
   pragma Convention (C, Indent_Parameters);
   --  Define all parameters to indent a source code.
   --  Note that some of these parameters will be ignored, depending on the
   --  actual language.
   --
   --  Indent_Level      number of spaces when indenting a block.
   --  Indent_Continue   number of spaces for a continuation line.
   --  Indent_Decl       number of spaces for multi-line variables declaration.
   --  Tab_Width         number of spaces for a tab character.
   --  Indent_Case_Extra if true, add extra indent level for case statements
   --  Reserved_Casing   casing of reserved words.
   --  Indent_Casing     casing of identifiers.
   --  Format_Operators  whether operators should be reformatted (e.g. spaces
   --                    added around "<")
   --  Use_Tabs          whether tabs should be used instead of spaces.
   --  Align_On_Colons   perform alignment on colons in declarations
   --  Align_On_Arrows   perform alignment on arrows in associations

   type Language_Category is
     (Cat_Unknown,

      ------------------------
      -- Enclosing Entities --
      ------------------------

      Cat_Package,
      Cat_Namespace,
      Cat_Task,        --  Subprogram
      Cat_Procedure,   --  Subprogram, Subprogram_Explorer
      Cat_Function,    --  Subprogram, Subprogram_Explorer
      Cat_Method,      --  Subprogram, Subprogram_Explorer
      Cat_Constructor, --  Subprogram, Subprogram_Explorer
      Cat_Destructor,  --  Subprogram, Subprogram_Explorer
      Cat_Protected,   --  Subprogram
      Cat_Entry,       --  Subprogram

      ----------------
      -- Data/Types --
      ----------------

      Cat_Class,
      Cat_Structure,
      Cat_Union,
      Cat_Type,
      Cat_Subtype,
      Cat_Variable,
      Cat_Local_Variable,
      Cat_Parameter,
      Cat_Literal,
      Cat_Representation_Clause,

      ----------------
      -- Dependency --
      ----------------

      Cat_With,
      Cat_Use,
      Cat_Include,

      ----------------
      -- Constructs --
      ----------------

      Cat_Loop_Statement,
      Cat_If_Statement,
      Cat_Case_Statement,
      Cat_Select_Statement,
      Cat_Accept_Statement,
      Cat_Declare_Block,
      Cat_Simple_Block,
      Cat_Exception_Handler);

   type Construct_Information;
   type Construct_Access is access Construct_Information;

   type Construct_Information is record
      Category       : Language_Category;
      --  Define the kind of construct

      Name           : String_Access;
      --  Name of the enclosing token. Null if not relevant for Token.

      Profile        : String_Access;
      --  Subprogram profile, if Category is in Subprogram_Category.
      --  Note that even for Subprogram_Category, Profile can be null if the
      --  subprogram does not have any parameter.

      Sloc_Start     : Source_Location;
      --  Location of beginning of the construct

      Sloc_Entity    : Source_Location;
      --  Location of beginning of the name of the entity. Only relevant if
      --  Name is non null. This is different from Sloc_Start since Sloc_Start
      --  is the beginning of the construct itself, e.g for
      --  "procedure Foo;", Sloc_Start will point to the first character, while
      --  Sloc_Entity will point to the 11th character.

      Sloc_End       : Source_Location;
      --  Location of end of the construct

      Is_Declaration : Boolean;
      --  Is this a declaration (e.g function specification) ?

      Prev, Next     : Construct_Access;
      --  Links to the previous and the next construct info
   end record;
   --  Information needed to define a language construct (e.g procedure,
   --  loop statement, ...).

   type Construct_List is record
      First, Current, Last : Construct_Access;
   end record;

   type Construct_List_Access is access all Construct_List;

   type Comment_Line_Proc is access
     function (Line : String; Comment : Boolean; Reserved : Integer)
               return chars_ptr;
   pragma Convention (C, Comment_Line_Proc);
   --  Profile for Comment_Line routine
   --  If Comment is True, comment one line of code.
   --  Otherwise, uncomment one line of code.
   --  Comment_Line (Comment_Line (A), Comment => False) should return A.

   type Parse_Constructs_Proc is access
     procedure (Buffer    : String;
                Result    : out Construct_List;
                Reserved1 : Integer;
                Reserved2 : System.Address;
                Reserved3 : System.Address);
   pragma Convention (C, Parse_Constructs_Proc);
   --  Profile for Parse_Construct routine
   --  Parse the constructs contained in Buffer and store all the language
   --  constructs with their source location in Result.

   type Replace_Text_Callback is access procedure
     (Line    : Integer;
      First   : Integer;
      Last    : Integer;
      Replace : chars_ptr);
   --  Replacement procedure used by Format_Buffer below.
   --  Replace the slice First .. Last by contents of Replace.
   --  First and Last are byte offsets from the start of the line, not
   --  character counts.

   type Format_Buffer_Proc is access
     procedure (Buffer        : String;
                Replace       : Replace_Text_Callback;
                From, To      : Integer;
                Indent_Params : Indent_Parameters;
                Reserved      : Integer);
   pragma Convention (C, Format_Buffer_Proc);
   --  Profile for Format_Buffer routine.
   --  Given a Buffer, reformat it, based on Indent_Params.
   --  Reformat only lines comprised between From and To.

   type Entity_Callback is access function
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean;
   pragma Convention (C, Entity_Callback);
   --  Callback during parsing of entities.
   --  Partial_Entity is True if parsing is at the end of the string with a
   --  non terminated entity (e.g String or Comment).
   --  If Callback returns True, the parsing should be stopped.

   type Parse_Entities_Proc is access
     procedure (Buffer   : String;
                Callback : Entity_Callback;
                Length   : Integer);
   pragma Convention (C, Parse_Entities_Proc);
   --  Profile for Parse_Entities procedure.
   --  Parse entities (as defined by Language_Entity) contained in buffer.
   --  For each match, call Callback. Stops at the end of Buffer or when
   --  callback returns True.

end Language_Custom;
