-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2000-2007, AdaCore                 --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Maps;  use Ada.Strings.Maps;

with Glib;
with Case_Handling;

with GNAT.Regpat;       use GNAT;
with GNAT.Strings;

package Language is

   type Language_Root is abstract tagged limited private;
   type Language_Access is access all Language_Root'Class;

   Unexpected_Type : exception;

   procedure Free (Lang : in out Language_Access);
   --  Free the memory pointed to by Lang and set it to null

   function Get_Name (Lang : access Language_Root) return String is abstract;
   --  Return the name of the language

   ------------------------
   -- Types manipulation --
   ------------------------
   --  The following functions are provided to manipulate types and variables
   --  for each language.

   function Is_Simple_Type
     (Lang : access Language_Root; Str : String) return Boolean is abstract;
   --  Return True if Str is a simple type, like integer, ...
   --  These are the types that don't need information from the debugger to
   --  be known, ie we can save a call to the debugger when parsing the value
   --  of a variable.

   --------------------------------
   -- Highlighting in the editor --
   --------------------------------

   type Language_Entity is
     (Normal_Text,
      Identifier_Text,
      Keyword_Text,
      Comment_Text,
      Character_Text,
      String_Text,
      Partial_Identifier_Text,
      Operator_Text);
   pragma Convention (C, Language_Entity);
   --  The entities found in a language.

   subtype Standout_Language_Entity is Language_Entity
     range Keyword_Text .. String_Text;
   --  All the entities that have a special significance. Used for syntax
   --  highlighting for example.

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      First     : Natural;
      Entity    : out Language_Entity;
      Next_Char : out Positive);
   --  Should return the type of entity that is present at the first position
   --  in the buffer (starting at First).
   --  Next_Char should be set to the index of the first character after the
   --  entity.
   --  First is required so that regexps can be used to match on e.g. start
   --  of lines.

   type Pattern_Matcher_Access is access all Regpat.Pattern_Matcher;

   function Keywords
     (Lang : access Language_Root) return Strings.String_Access is abstract;
   --  Returns the uncompiled keyword regular expression. This string is used
   --  to create the pattern matcher as returned by the version above.

   function Keywords
     (Lang : access Language_Root) return Pattern_Matcher_Access is abstract;
   --  Return a regular expression that matches the keywords for the current
   --  language.
   --  Note: we return an access type (instead of a Pattern_Matcher) for
   --  efficiency.

   function Is_Word_Char
     (Lang : access Language_Root; Char : Glib.Gunichar) return Boolean;
   --  Return True if Char belongs to the set of characters that compose a
   --  keyword for this language. By default, this returns true for letters,
   --  digits and underscore characters.

   ----------------------------
   -- Tooltips in the editor --
   ----------------------------

   function Can_Tooltip_On_Entity
     (Lang   : access Language_Root;
      Entity : String) return Boolean;
   --  Return True if we should display a tooltip for the Entity.
   --  Note that Entity is analyzed in the current context. This is used at
   --  least for the gdb Ada mode, since we don't want to evaluate subprograms
   --  when the type of tooltips in Simple.
   --  By default, this simply returns True.

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Language_Root;
      Name : String) return String is abstract;
   --  Return the name to use to dereference Name (ie in Ada "Name.all", in
   --  C "*Name", ...). Note that Name can be a composite name (Name.Field),
   --  and thus might have to be protected with parentheses.

   function Array_Item_Name
     (Lang  : access Language_Root;
      Name  : String;
      Index : String) return String is abstract;
   --  Return the name to use to access a specific element of an array.
   --  Index is a comma-separated list of the indexes for all the dimensions,
   --  as in "1,2".

   function Record_Field_Name
     (Lang  : access Language_Root;
      Name  : String;
      Field : String) return String is abstract;
   --  Return the name to use for a specific field of a record.

   ---------------------
   -- Project support --
   ---------------------

   type Project_Field is record
      Attribute_Name  : Strings.String_Access;
      Attribute_Index : Strings.String_Access := null;
      Description     : Strings.String_Access;
      Values          : Strings.String_List_Access := null;
      Editable        : Boolean := True;
   end record;
   No_Project_Field : constant Project_Field := (null, null, null, null, True);
   type Project_Field_Array is array (Natural range <>) of Project_Field;

   function Get_Project_Fields
     (Lang : access Language_Root) return Project_Field_Array is abstract;
   --  Return the list of project attributes needed to support this language.
   --  In the project, this attribute is set with a line like:
   --     for Attribute_Name ("Attribute_Index") use "...";
   --  The list of valid attribute names is restricted, since the project
   --  parser will complain if an unknown attribute is found in the project.
   --  It is valid to have a null Attribute_Index, in case the project
   --  attribute is not indexed. In this case, the line would look like:
   --     for Attribute_Name use "...";
   --
   --  Description is the string used in the project editor to describe the
   --  attribute.
   --  Values is a set of possible values (in which case a combo box is used
   --  when editing the attribute). If Editable is False, then the user can
   --  only select one of these values. If Editable is True, any other value
   --  can be specified by the user.
   --
   --  The returned value mustn't be freed by the caller.

   procedure Free (Fields : in out Project_Field_Array);
   --  Free the contents of the array

   ----------------------
   -- Language Context --
   ----------------------

   type Language_Context
     (Comment_Start_Length : Natural;
      Comment_End_Length   : Natural) is
   --  Set any of the length to 0 if there is no such comment
   record
         Comment_Start                 : String (1 .. Comment_Start_Length);
         --  How comments start for this language. This is for comments that
         --  do not end on Newline, but with Comment_End.

         Comment_End                   : String (1 .. Comment_End_Length);
         --  How comments end for this language

         New_Line_Comment_Start        : Strings.String_Access;
         --  How comments start. These comments end on the next newline
         --  character. If null, use New_Line_Comment_Start_Regexp instead.

         New_Line_Comment_Start_Regexp : Pattern_Matcher_Access;
         --  How comments start. These comments end on the next newline
         --  character. If null, use New_Line_Comment_Start instead.

         String_Delimiter              : Character;
         --  How strings start and end

         Quote_Character               : Character;
         --  The character used to quote (protect) the following one. If this
         --  is set to ASCII.NUL, then there is no such character in the
         --  language. For instance, it should be set to \ for C.

         Constant_Character            : Character;
         --  The character that starts and ends constant characters

         Can_Indent                    : Boolean;
         --  Whether indentation is supported by this language

         Syntax_Highlighting           : Boolean;
         --  Whether syntax highlighting is relevant to this language

         Case_Sensitive                : Boolean;
         --  Whether the language is case sensitive
   end record;
   --  This record describes the syntax of the language (for color
   --  highlighting purposes). All the fields in this record are language
   --  specific, and do not depend on the debugger used.

   type Language_Context_Access is access all Language_Context;

   function Get_Language_Context
     (Lang : access Language_Root) return Language_Context_Access is abstract;
   --  Return the context to use for a specific language

   -------------------
   -- Parsing files --
   -------------------

   procedure Skip_To_Current_Comment_Block_Start
     (Context : Language_Context;
      Buffer  : String;
      Index   : in out Natural);
   --  Assuming that Index is at the beginning or inside a comment line, moves
   --  upward in the file till the end of the current block of comments.
   --  This block is defined as a group of commented out lines, until a
   --  non-comment line is seen.
   --  If Index is not at the beginning or inside a comment line, Index is set
   --  to 0.

   procedure Skip_To_Current_Comment_Block_End
     (Context            : Language_Context;
      Buffer             : String;
      Index              : in out Natural;
      Ignore_Blank_Lines : Boolean := False);
   --  Same as Skip_To_Current_Comment_Block_Start, except we move forward
   --  to the beginning of the last line of comments in the block.
   --  If Ignore_Blank_Lines is set to True, blocks separated from one another
   --  with blank lines are considered as a single one.

   procedure Skip_To_Next_Comment_Start
     (Context : Language_Context;
      Buffer  : String;
      Index   : in out Natural);
   --  Skip lines of code until we find the beginning of a comment.
   --  If we see an empty line first Index is set to 0.
   --  Likewise if no comment is found before the end of the buffer.

   procedure Skip_To_Previous_Comment_Start
     (Context : Language_Context;
      Buffer  : String;
      Index   : in out Natural);
   --  Skip lines of code (backward) until we find the start of a comment.
   --  If  we see an empty line first Index is set to 0.
   --  Likewise if no comment is found before the beginning of the buffer.

   ----------------------
   -- Source Analyzing --
   ----------------------

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

   function ">" (S1, S2 : Source_Location) return Boolean;
   function ">=" (S1, S2 : Source_Location) return Boolean;
   function "<" (S1, S2 : Source_Location) return Boolean;
   function "<=" (S1, S2 : Source_Location) return Boolean;
   function "=" (S1, S2 : Source_Location) return Boolean;

   type Indent_Style is (Automatic, RM_Style, Non_RM_Style);
   for Indent_Style'Size use Integer'Size;
   pragma Convention (C, Indent_Style);
   --  Indentation style used by some constructs (e.g. case statements in
   --  Ada).
   --  Automatic means that the engine will guess the best value.
   --  RM_Style means follows the style recommended by the RM, if any
   --  Non_RM_Style means do not follow the style recommended by the RM.

   type Indent_Parameters is record
      Indent_Level        : Natural;
      Indent_Continue     : Natural;
      Indent_Decl         : Natural;
      Indent_Conditional  : Natural;
      Indent_Record       : Natural;
      Tab_Width           : Natural;
      Indent_Case_Extra   : Indent_Style;
      Casing_Policy       : Case_Handling.Casing_Policy;
      Reserved_Casing     : Case_Handling.Casing_Type;
      Ident_Casing        : Case_Handling.Casing_Type;
      Format_Operators    : Boolean;
      Use_Tabs            : Boolean;
      Align_On_Colons     : Boolean;
      Align_On_Arrows     : Boolean;
      Align_Decl_On_Colon : Boolean;
      --  ??? Missing alignment parameters:
      --      - assignments in declarations
      --      - assignments in assignment statements

      Indent_Comments     : Boolean;
      Stick_Comments      : Boolean;
   end record;
   pragma Convention (C, Indent_Parameters);
   --  Define all parameters to indent a source code.
   --  Note that some of these parameters will be ignored, depending on the
   --  actual language.
   --
   --  Indent_Level        number of spaces when indenting a block.
   --  Indent_Continue     number of spaces for a continuation line.
   --  Indent_Decl         number of spaces for multi-line variables
   --                      declaration.
   --  Indent_Conditional  extra number of spaces when indenting multi-line
   --                      conditionals.
   --  Indent_Record       extra number of spaces when indenting record types
   --  Tab_Width           number of spaces for a tab character.
   --  Indent_Case_Extra   whether to add extra indent level for case
   --                      statements
   --  Reserved_Casing     casing of reserved words.
   --  Indent_Casing       casing of identifiers.
   --  Format_Operators    whether operators should be reformatted (e.g. spaces
   --                      added around "<")
   --  Use_Tabs            whether tabs should be used instead of spaces.
   --  Align_On_Colons     perform alignment on colons in declarations
   --  Align_On_Arrows     perform alignment on arrows in associations
   --  Align_Decl_On_Colon align variable declarations based on the ':' of the
   --                      variable decl.
   --  Indent_Comments     whether comments should be indented or left as is
   --  Stick_Comments      whether comments should stick to previous line
   --                      indentation in some cases (language defined).

   Default_Indent_Parameters : constant Indent_Parameters :=
     (Indent_Level        => 8,
      Indent_Continue     => 2,
      Indent_Decl         => 0,
      Indent_Conditional  => 0,
      Indent_Record       => 8,
      Tab_Width           => 8,
      Indent_Case_Extra   => Automatic,
      Casing_Policy       => Case_Handling.Disabled,
      Reserved_Casing     => Case_Handling.Unchanged,
      Ident_Casing        => Case_Handling.Unchanged,
      Format_Operators    => False,
      Use_Tabs            => False,
      Align_On_Colons     => False,
      Align_On_Arrows     => False,
      Align_Decl_On_Colon => False,
      Indent_Comments     => True,
      Stick_Comments      => False);

   type Indentation_Kind is (None, Simple, Extended);
   for Indentation_Kind'Size use Integer'Size;
   pragma Convention (C, Indentation_Kind);
   --  Indentation kinds:
   --  None: no indentation should be performed
   --  Simple: use the amount of white spaces from previous line
   --  Extended: use a language specific parser to compute indentation

   procedure Get_Indentation_Parameters
     (Lang         : access Language_Root;
      Params       : out Indent_Parameters;
      Indent_Style : out Indentation_Kind);
   --  Return the indentation parameters for this language

   procedure Set_Indentation_Parameters
     (Lang         : access Language_Root;
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind);
   --  Set the indentation parameters to use for this language.
   --  ??? This wouldn't be necessary if we had access to the preferences from
   --  the language hierarchy.

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
      Cat_Case_Inside_Record,
      Cat_Union,
      Cat_Type,
      Cat_Subtype,
      Cat_Variable,
      Cat_Local_Variable,
      Cat_Parameter,
      Cat_Field,
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
      Cat_Return_Block,
      Cat_Simple_Block,

      --------------------
      -- Sub-constructs --
      --------------------

      Cat_Exception_Handler);

   subtype Enclosing_Entity_Category is Language_Category
     range Cat_Package .. Cat_Union;

   subtype Namespace_Category is Enclosing_Entity_Category
     range Cat_Package .. Cat_Namespace;

   subtype Subprogram_Category is Enclosing_Entity_Category
     range Cat_Task .. Cat_Entry;

   subtype Subprogram_Explorer_Category is Subprogram_Category
     range Cat_Procedure .. Cat_Destructor;
   --  Subprograms, as displayed in the explorer

   subtype Data_Type_Category is Language_Category
     range Cat_Class .. Cat_Variable;

   subtype Type_Category is Data_Type_Category
     range Cat_Class .. Cat_Subtype;

   subtype Data_Category is Language_Category
     range Cat_Variable .. Cat_Field;

   subtype Dependency_Category is Language_Category
     range Cat_With .. Cat_Include;

   subtype Construct_Category is Language_Category
     range Cat_Loop_Statement .. Cat_Simple_Block;

   function Category_Name
     (Category : Language.Language_Category) return String;
   --  Return the external name to display in GUIs for a given category.

   type Construct_Visibility is
     (Visibility_Private,
      Visibility_Protected,
      Visibility_Public);
   --  Represents the visibility of a construct from the enclosing entity.

   type Construct_Att_Key is range 1 .. 32;
   --  This is the type of keys attributes. In order to minimize size, there
   --  cannot be more than 32 attributes at all for a given language. Some
   --  of them are language independent (up to Last_Gen_Att).
   --  Specific languages might want to implement they own keys, which have to
   --  start at Last_Gen_Att + 1.
   --  We migh consider raising the limit if we have specific needs.

   type Construct_Attribute_Map is array (Construct_Att_Key) of Boolean;
   pragma Pack (Construct_Attribute_Map);

   No_Attribute : constant Construct_Attribute_Map := (others => False);

   Access_Attribute : constant Construct_Att_Key  := 1;
   Array_Attribute  : constant Construct_Att_Key  := 2;
   --  ??? This list is currently incomplete. To be completed.

   Last_Gen_Att : constant Construct_Att_Key := 2;

   type Construct_Information;
   type Construct_Access is access all Construct_Information;

   type Construct_Information is record
      Category       : Language_Category;
      --  Define the kind of construct

      Is_Declaration : Boolean;
      --  Is this a declaration (e.g function specification) ?

      Visibility     : Construct_Visibility := Visibility_Public;
      --  Is the construct public, private or protected ?

      Name           : Strings.String_Access;
      --  Name of the enclosing token. Null if not relevant for Token

      Profile        : Strings.String_Access;
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

      Prev, Next     : Construct_Access;
      --  Links to the previous and the next construct info

      Attributes     : Construct_Attribute_Map := (others => False);
      --  Set of construct attributes
   end record;
   --  Information needed to define a language construct (e.g procedure,
   --  loop statement, ...).

   type Simple_Construct_Information is record
      Category       : Language_Category;
      Is_Declaration : Boolean;
      Visibility     : Construct_Visibility := Visibility_Public;
      Name           : Strings.String_Access;
      Sloc_Start     : Source_Location;
      Sloc_Entity    : Source_Location;
      Sloc_End       : Source_Location;
      Attributes     : Construct_Attribute_Map;
   end record;
   --  Same as above, but containing only the needed construct information, no
   --  list constructions.

   procedure To_Simple_Construct_Information
     (Construct : Construct_Information;
      Simple    : out Simple_Construct_Information;
      Full_Copy : Boolean);
   --  Convert a Construct_Information into a simple construct information

   Null_Construct_Info : constant Construct_Information;

   Null_Simple_Construct_Info : constant Simple_Construct_Information;

   type Construct_List is record
      First, Current, Last : Construct_Access;
      Size                 : Integer := 0;
   end record;

   type Construct_List_Access is access all Construct_List;

   procedure Free (List : in out Construct_List);
   --  Free the contents of List.

   function Word_Character_Set
     (Lang : access Language_Root)
      return Character_Set;
   --  Returns the character set used for the language identifiers

   function Comment_Block
     (Lang    : access Language_Root;
      Block   : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String;
   --  Comment or uncomment (if Comment is false) a text block
   --  Comment_Block (L, Comment_Block (L, A), Comment => False)
   --  should return A.
   --  If Clean is True, a clean up of of the block should be performed
   --  (e.g. leading spaces are removed for each line).

   procedure Parse_Constructs
     (Lang   : access Language_Root;
      Buffer : String;
      Result : out Construct_List);
   --  Parse the constructs contained in Buffer and store all the language
   --  constructs with their source location in Result.

   type Replace_Text_Callback is access procedure
     (Line    : Natural;
      First   : Natural;
      Last    : Natural;
      Replace : String);
   --  Replacement procedure used by Format_Buffer below.
   --  Replace the slice First .. Last by contents of Replace.
   --  First and Last are byte offsets from the start of the line, not
   --  character counts.

   procedure Format_Buffer
     (Lang            : access Language_Root;
      Buffer          : String;
      Replace         : Replace_Text_Callback;
      From, To        : Natural := 0;
      Indent_Params   : Indent_Parameters := Default_Indent_Parameters;
      Indent_Offset   : Natural := 0;
      Case_Exceptions : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception);
   --  Given a Buffer, reformat it, based on Indent_Params.
   --  Reformat only lines comprised between From and To.
   --  If Indent_Offset is > 0, it represents an additional level of
   --  indentation when e.g. formatting a substring within a bigger
   --  construct. Format_Buffer will take this value into account when
   --  calling the Replace callback.

   type Entity_Callback is access function
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean;
   --  Callback during parsing of entities.
   --  Partial_Entity is True if parsing is at the end of the string with a
   --  non terminated entity (e.g string or multi-line comment).
   --  If Callback returns True, the parsing should be stopped.

   procedure Parse_Entities
     (Lang     : access Language_Root;
      Buffer   : String;
      Callback : Entity_Callback);
   --  Parse entities (as defined by Language_Entity) contained in buffer.
   --  For each match, call Callback. Stops at the end of Buffer or when
   --  callback returns True.

   --  These functions are provided as a support for the source code explorer.

   procedure Get_Referenced_Entity
     (Lang       : access Language_Root;
      Buffer     : String;
      Construct  : Simple_Construct_Information;
      Sloc_Start : out Source_Location;
      Sloc_End   : out Source_Location;
      Success    : out Boolean;
      From_Index : Natural := 0);
   --  Some constructs are referencing one or more entities. The purpose of
   --  this procedure is to extract them. It can be the type of a variable,
   --  the parent type of a class, the generic package from wich a package is
   --  instantiated, the returned type of a subprogram... If no reference
   --  entity is found, then Success is set to False, True otherwise. The
   --  From_Index parameter is used to initialize the search at a given offset
   --  (in bytes). It helps to handle cases where the construct is referencing
   --  more than one element (e.g. extension from interfaces in Java and Ada,
   --  multiple inheritance in C++). If From_Index is 0 the search will start
   --  at the begining of the construct.

   type Make_Entry_Func is access function
     (Str      : String;
      Matched  : Regpat.Match_Array) return String;
   --  Function that builds the string to be inserted in the tree.

   type Explorer_Category is record
      Category       : Language_Category;
      Regexp         : Pattern_Matcher_Access;
      Position_Index : Natural;
      Make_Entry     : Make_Entry_Func;
   end record;
   --  Definition for a category (ie one of the subtrees of the explorer).
   --  Icon is the icon to use for items in this category.
   --  Regexp is the general regular expression to use for entries in this
   --  category, while Make_Entry is the function that will return the
   --  actual string to be displayed in the explorer.
   --  Position_Index is the index of the parenthesis-pair that the entity
   --  name starts at. When the user clicks on this item in the explorer,
   --  the cursor will be moved to that location in the editor.
   --
   --  If Make_Entry is null, then Position_Index is used to compute the
   --  string to display.

   type Explorer_Categories is
     array (Positive range <>) of Explorer_Category;
   --  A list of categories. Each category is assigned an internal number which
   --  is the index in this table, and is passed to each Make_Entry_Func
   --  functions.

   function Explorer_Regexps
     (Lang : access Language_Root) return Explorer_Categories;
   --  Return the list of categories for a given language.
   --  By default, no category is defined, and thus the explorer is empty.

   function Is_System_File
     (Lang      : access Language_Root;
      File_Name : String) return Boolean;
   --  Return True if File_Name is the name of a system file (standard include
   --  files in C or run-time file in Ada). These files are displayed
   --  separately in the explorer.

private
   type Language_Root is abstract tagged limited record
      Indent_Params   : Indent_Parameters := Default_Indent_Parameters;
      Indent_Style    : Indentation_Kind  := Extended;
      Actual_Language : Language_Access   := Language_Root'Unchecked_Access;
   end record;

   function Comment_Line
     (Lang    : access Language_Root;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String;
   --  Comment or uncomment (if Comment is false) one line of code.
   --  Comment_Line (Comment_Line (A), Comment => False) should return A.
   --  If Clean is True, a clean up of of the line should be performed
   --  (e.g. leading spaces are removed).

   Null_Construct_Info : constant Construct_Information :=
     (Category       => Cat_Unknown,
      Is_Declaration => False,
      Visibility     => Visibility_Public,
      Name           => null,
      Profile        => null,
      Sloc_Start     => (0, 0, 0),
      Sloc_Entity    => (0, 0, 0),
      Sloc_End       => (0, 0, 0),
      Prev           => null,
      Next           => null,
      Attributes     => (others => False));

   Null_Simple_Construct_Info : constant Simple_Construct_Information :=
     (Category       => Cat_Unknown,
      Is_Declaration => False,
      Visibility     => Visibility_Public,
      Name           => null,
      Sloc_Start     => (0, 0, 0),
      Sloc_Entity    => (0, 0, 0),
      Sloc_End       => (0, 0, 0),
      Attributes     => (others => False));

end Language;
