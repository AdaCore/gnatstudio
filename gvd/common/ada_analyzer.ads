with Scans;       use Scans;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Source_Analyzer is

   type Casing_Type is (Unchanged, Upper, Lower, Mixed);
   --  Casing used for identifiers and reserved words.

   type Source_Location is record
      Line   : Natural := 0;
      --  Line number for this entity

      Column : Natural := 0;
      --  Column number for this entity
   end record;

   type Construct_Information;
   type Construct_Access is access Construct_Information;

   type Construct_Information is record
      Token           : Token_Type;
      --  Token defining the kind of construct

      Name            : String_Access;
      --  Name of the enclosing token

      Sloc_Start      : Source_Location;
      --  Location of beginning of the construct

      Sloc_End        : Source_Location;
      --  Location of end of the construct

      Subprogram_Spec : Boolean;
      --  Is this a subprogram specification ?

      Prev, Next      : Construct_Access;
      --  Links to the previous and the next construct info
   end record;
   --  Information needed to define a language construct (e.g procedure,
   --  loop statement, ...).

   type Construct_List is record
      First, Current, Last : Construct_Access;
   end record;

   procedure Free (List : in out Construct_List);
   --  Free the contents of List.

   procedure Format_Ada
     (Buffer           : String;
      Indent_Level     : Natural     := 3;
      Indent_Continue  : Natural     := 2;
      Indent_Decl      : Natural     := 0;
      Reserved_Casing  : Casing_Type := Lower;
      Ident_Casing     : Casing_Type := Mixed;
      Format_Operators : Boolean     := True);
   --  Format Buffer and output the result on standard output.
   --  Indent_Level is the number of spaces when indenting a block.
   --  Indent_Continue is the number of spaces for a continuation line.
   --  Indent_Decl is the number of spaces for variables declaration.
   --  Reserved_Casing specifies the casing for reserved words.
   --  Ident_Casing specifies the casing for identifiers.
   --  If Format_Operators is True, spaces are added when appropriate around
   --  operators (e.g a space after commas, before left paren, etc...).

   procedure Parse_Ada_Constructs
     (Buffer          : String;
      Result          : out Construct_List;
      Indent          : out Natural;
      Next_Indent     : out Natural;
      Indent_Level    : Natural := 3;
      Indent_Continue : Natural := 2;
      Indent_Decl     : Natural := 0);
   --  Parse the constructs contained in Buffer and store all the Ada
   --  constructs with their source location in Result.
   --  As a bonus (since it is computed anyway), store the current and
   --  next indentation levels.

   procedure Next_Ada_Indentation
     (Buffer          : String;
      Indent          : out Natural;
      Next_Indent     : out Natural;
      Indent_Level    : Natural := 3;
      Indent_Continue : Natural := 2;
      Indent_Decl     : Natural := 0);
   --  Given a Buffer, return the indentation level for the last character
   --  in the buffer and for the next line.

end Source_Analyzer;
