-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.Regpat;
with Basic_Types;
with VFS;

package Language is

   type Language_Root is abstract tagged private;
   type Language_Access is access all Language_Root'Class;

   Unexpected_Type : exception;

   procedure Free (Lang : in out Language_Access);
   --  Free the memory pointed to by Lang and set it to null.

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
      String_Text);
   --  The entities found in a language, and that can have a different scheme
   --  for colors highlighting.

   subtype Standout_Language_Entity is Language_Entity
     range Keyword_Text .. String_Text;
   --  All the entities that have a special significance. Used for syntax
   --  highlighting for example.

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      Entity    : out Language_Entity;
      Next_Char : out Positive);
   --  Should return the type of entity that is present at the first position
   --  in the buffer (ie starting at Buffer'First).
   --  Next_Char should be set to the index of the first character after the
   --  entity.

   function Keywords
     (Lang : access Language_Root)
      return GNAT.Regpat.Pattern_Matcher is abstract;
   --  Return a regular expression that matches the keywords for the current
   --  language.

   ----------------------------
   -- Tooltips in the editor --
   ----------------------------

   function Can_Tooltip_On_Entity
     (Lang : access Language_Root;
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
      Attribute_Name  : Basic_Types.String_Access;
      Attribute_Index : Basic_Types.String_Access := null;
      Description     : Basic_Types.String_Access;
      Values          : Basic_Types.String_Array_Access := null;
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
     (Comment_Start_Length          : Natural;
      Comment_End_Length            : Natural;
      New_Line_Comment_Start_Length : Natural) is
   --  Set any of the length to 0 if there is no such comment
   record
      Comment_Start : String (1 .. Comment_Start_Length);
      --  How comments start for this language. This is for comments that
      --  do not end on Newline, but with Comment_End.

      Comment_End : String (1 .. Comment_End_Length);
      --  How comments end for this language

      New_Line_Comment_Start : String (1 .. New_Line_Comment_Start_Length);
      --  How comments start. These comments end on the next newline character.

      String_Delimiter : Character;
      --  How strings start and end

      Quote_Character : Character;
      --  The character used to quote (protect) the following one. If this is
      --  set to ASCII.NUL, then there is no such character in the
      --  language. For instance, it should be set to \ for C.

      Constant_Character : Character;
      --  The character that starts and ends constant characters

      Can_Indent         : Boolean;
      --  Whether indentation is supported by this language.

      Syntax_Highlighting : Boolean;
      --  Whether syntax highlighting is relevant to this language.

      Case_Sensitive      : Boolean;
      --  Whether the language is case sensitive.
   end record;
   --  This record describes the syntax of the language (for color
   --  highlighting purposes). All the fields in this record are language
   --  specific, and do not depend on the debugger used.

   function Get_Language_Context
     (Lang : access Language_Root) return Language_Context is abstract;
   --  Return the context to use for a specific language

   ----------------------
   -- Source Analyzing --
   ----------------------

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
      --  ??? Missing alignment parameters:
      --      - colons in declarations
      --      - assignments in declarations
      --      - assignments in assignment statements
      --      - arrow delimiters in associations
   end record;
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

   Default_Indent_Parameters : constant Indent_Parameters :=
     (Indent_Level      => 3,
      Indent_Continue   => 2,
      Indent_Decl       => 0,
      Tab_Width         => 8,
      Indent_Case_Extra => True,
      Reserved_Casing   => Unchanged,
      Ident_Casing      => Unchanged,
      Format_Operators  => False,
      Use_Tabs          => False);

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

   subtype Enclosing_Entity_Category is Language_Category
     range Cat_Package .. Cat_Entry;

   subtype Subprogram_Category is Enclosing_Entity_Category
     range Cat_Task .. Cat_Entry;

   subtype Subprogram_Explorer_Category is Subprogram_Category
     range Cat_Procedure .. Cat_Destructor;
   --  Subprograms, as displayed in the explorer

   subtype Data_Type_Category is Language_Category
     range Cat_Class .. Cat_Variable;

   subtype Type_Category is Data_Type_Category
     range Cat_Class .. Cat_Subtype;

   subtype Dependency_Category is Language_Category
     range Cat_With .. Cat_Include;

   subtype Construct_Category is Language_Category
     range Cat_Loop_Statement .. Cat_Exception_Handler;

   function Category_Name
     (Category : Language.Language_Category) return String;
   --  Return the external name to display in GUIs for a given category.

   type Construct_Information;
   type Construct_Access is access Construct_Information;

   type Construct_Information is record
      Category        : Language_Category;
      --  Define the kind of construct

      Name            : Basic_Types.String_Access;
      --  Name of the enclosing token. Null if not relevant for Token.

      Profile         : Basic_Types.String_Access;
      --  Subprogram profile, if Category is in Subprogram_Category.
      --  Note that even for Subprogram_Category, Profile can be null if the
      --  subprogram does not have any parameter.

      Sloc_Start      : Source_Location;
      --  Location of beginning of the construct

      Sloc_Entity     : Source_Location;
      --  Location of beginning of the name of the entity. Only relevant if
      --  Name is non null. This is different from Sloc_Start since Sloc_Start
      --  is the beginning of the construct itself, e.g for
      --  "procedure Foo;", Sloc_Start will point to the first character, while
      --  Sloc_Entity will point to the 11th character.

      Sloc_End        : Source_Location;
      --  Location of end of the construct

      Is_Declaration  : Boolean;
      --  Is this a declaration (e.g function specification) ?

      Prev, Next      : Construct_Access;
      --  Links to the previous and the next construct info
   end record;
   --  Information needed to define a language construct (e.g procedure,
   --  loop statement, ...).

   type Construct_List is record
      First, Current, Last : Construct_Access;
   end record;

   type Construct_List_Access is access all Construct_List;

   procedure Free (List : in out Construct_List);
   --  Free the contents of List.

   function Comment_Line
     (Lang : access Language_Root;
      Line : String) return String;
   --  Comment one line of code.
   --  See Uncomment_Line below.

   function Uncomment_Line
     (Lang : access Language_Root;
      Line : String) return String;
   --  Uncomment one line of code.
   --  Does nothing on a non-commented line.
   --  Uncomment_Line (Comment_Line (A)) should return A.

   procedure Parse_Constructs
     (Lang   : access Language_Root;
      Buffer : String;
      Result : out Construct_List);
   --  Parse the constructs contained in Buffer and store all the language
   --  constructs with their source location in Result.

   procedure Parse_File_Constructs
     (Lang      : access Language_Root'Class;
      File_Name : VFS.Virtual_File;
      Result    : out Construct_List);
   --  Same as Parse_Constructs, but works on a given file.
   --  Since Parse_File_Constructs calls Parse_Constructs, this function does
   --  not need to be dispatching.

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
     (Lang          : access Language_Root;
      Buffer        : String;
      Replace       : Replace_Text_Callback;
      From, To      : Natural := 0;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters);
   --  Given a Buffer, reformat it, based on Indent_Params.
   --  Reformat only lines comprised between From and To.

   type Entity_Callback is access function
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean;
   --  Callback during parsing of entities.
   --  Partial_Entity is True if parsing is at the end of the string with a
   --  non terminated entity (e.g String or Comment).
   --  If Callback returns True, the parsing should be stopped.

   procedure Parse_Entities
     (Lang          : access Language_Root;
      Buffer        : String;
      Callback      : Entity_Callback);
   --  Parse entities (as defined by Language_Entity) contained in buffer.
   --  For each match, call Callback. Stops at the end of Buffer or when
   --  callback returns True.

   --  These functions are provided as a support for the source code explorer.

   type Make_Entry_Func is access function
     (Str      : String;
      Matched  : GNAT.Regpat.Match_Array) return String;
   --  Function that builds the string to be inserted in the tree.

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   type Explorer_Category is record
      Category       : Language_Category;
      Regexp         : Pattern_Matcher_Access;
      Position_Index : Natural;
      Icon           : Basic_Types.Pixmap_Access;
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
   type Language_Root is abstract tagged record
      Indent_Params : Indent_Parameters := Default_Indent_Parameters;
      Indent_Style  : Indentation_Kind  := Extended;
   end record;
end Language;
