with Generic_Stack;
with Basic_Types;
with Ada.Unchecked_Deallocation;
with Language;             use Language;

package Ada_Analyzer is

   --  Note: the following type is taken from Scans (GNAT sources).
   --  The following type is used to identify token types returned by Scan.
   --  The class column in this table indicates the token classes which
   --  apply to the token, as defined by subsquent subtype declarations.

   type Token_Type is (

      --  Token name          Token type   Class(es)

      Tok_Integer_Literal, -- numeric lit  Literal, Lit_Or_Name

      Tok_Real_Literal,    -- numeric lit  Literal, Lit_Or_Name

      Tok_String_Literal,  -- string lit   Literal. Lit_Or_Name

      Tok_Char_Literal,    -- char lit     Name, Literal. Lit_Or_Name

      Tok_Operator_Symbol, -- op symbol    Name, Literal, Lit_Or_Name, Desig

      Tok_Identifier,      -- identifer    Name, Lit_Or_Name, Desig

      Tok_Double_Asterisk, -- **

      Tok_Ampersand,       -- &            Binary_Addop
      Tok_Minus,           -- -            Binary_Addop, Unary_Addop
      Tok_Plus,            -- +            Binary_Addop, Unary_Addop

      Tok_Asterisk,        -- *            Mulop
      Tok_Mod,             -- MOD          Mulop
      Tok_Rem,             -- REM          Mulop
      Tok_Slash,           -- /            Mulop

      Tok_New,             -- NEW

      Tok_Abs,             -- ABS
      Tok_Others,          -- OTHERS
      Tok_Null,            -- NULL

      Tok_Dot,             -- .            Namext
      Tok_Apostrophe,      -- '            Namext

      Tok_Left_Paren,      -- (            Namext, Consk

      Tok_Delta,           -- DELTA        Atkwd, Sterm, Consk
      Tok_Digits,          -- DIGITS       Atkwd, Sterm, Consk
      Tok_Range,           -- RANGE        Atkwd, Sterm, Consk

      Tok_Right_Paren,     -- )            Sterm
      Tok_Comma,           -- ,            Sterm

      Tok_And,             -- AND          Logop, Sterm
      Tok_Or,              -- OR           Logop, Sterm
      Tok_Xor,             -- XOR          Logop, Sterm

      Tok_Less,            -- <            Relop, Sterm
      Tok_Equal,           -- =            Relop, Sterm
      Tok_Greater,         -- >            Relop, Sterm
      Tok_Not_Equal,       -- /=           Relop, Sterm
      Tok_Greater_Equal,   -- >=           Relop, Sterm
      Tok_Less_Equal,      -- <=           Relop, Sterm

      Tok_In,              -- IN           Relop, Sterm
      Tok_Not,             -- NOT          Relop, Sterm

      Tok_Box,             -- <>           Relop, Eterm, Sterm
      Tok_Colon_Equal,     -- :=           Eterm, Sterm
      Tok_Colon,           -- :            Eterm, Sterm
      Tok_Greater_Greater, -- >>           Eterm, Sterm

      Tok_Abstract,        -- ABSTRACT     Eterm, Sterm
      Tok_Access,          -- ACCESS       Eterm, Sterm
      Tok_Aliased,         -- ALIASED      Eterm, Sterm
      Tok_All,             -- ALL          Eterm, Sterm
      Tok_Array,           -- ARRAY        Eterm, Sterm
      Tok_At,              -- AT           Eterm, Sterm
      Tok_Body,            -- BODY         Eterm, Sterm
      Tok_Constant,        -- CONSTANT     Eterm, Sterm
      Tok_Do,              -- DO           Eterm, Sterm
      Tok_Is,              -- IS           Eterm, Sterm
      Tok_Limited,         -- LIMITED      Eterm, Sterm
      Tok_Of,              -- OF           Eterm, Sterm
      Tok_Out,             -- OUT          Eterm, Sterm
      Tok_Record,          -- RECORD       Eterm, Sterm
      Tok_Renames,         -- RENAMES      Eterm, Sterm
      Tok_Reverse,         -- REVERSE      Eterm, Sterm
      Tok_Tagged,          -- TAGGED       Eterm, Sterm
      Tok_Then,            -- THEN         Eterm, Sterm

      Tok_Less_Less,       -- <<           Eterm, Sterm, After_SM

      Tok_Abort,           -- ABORT        Eterm, Sterm, After_SM
      Tok_Accept,          -- ACCEPT       Eterm, Sterm, After_SM
      Tok_Case,            -- CASE         Eterm, Sterm, After_SM
      Tok_Delay,           -- DELAY        Eterm, Sterm, After_SM
      Tok_Else,            -- ELSE         Eterm, Sterm, After_SM
      Tok_Elsif,           -- ELSIF        Eterm, Sterm, After_SM
      Tok_End,             -- END          Eterm, Sterm, After_SM
      Tok_Exception,       -- EXCEPTION    Eterm, Sterm, After_SM
      Tok_Exit,            -- EXIT         Eterm, Sterm, After_SM
      Tok_Goto,            -- GOTO         Eterm, Sterm, After_SM
      Tok_If,              -- IF           Eterm, Sterm, After_SM
      Tok_Pragma,          -- PRAGMA       Eterm, Sterm, After_SM
      Tok_Raise,           -- RAISE        Eterm, Sterm, After_SM
      Tok_Requeue,         -- REQUEUE      Eterm, Sterm, After_SM
      Tok_Return,          -- RETURN       Eterm, Sterm, After_SM
      Tok_Select,          -- SELECT       Eterm, Sterm, After_SM
      Tok_Terminate,       -- TERMINATE    Eterm, Sterm, After_SM
      Tok_Until,           -- UNTIL        Eterm, Sterm, After_SM
      Tok_When,            -- WHEN         Eterm, Sterm, After_SM

      Tok_Begin,           -- BEGIN        Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_Declare,         -- DECLARE      Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_For,             -- FOR          Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_Loop,            -- LOOP         Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_While,           -- WHILE        Eterm, Sterm, After_SM, Labeled_Stmt

      Tok_Entry,           -- ENTRY        Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Protected,       -- PROTECTED    Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Task,            -- TASK         Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Type,            -- TYPE         Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Subtype,         -- SUBTYPE      Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Use,             -- USE          Eterm, Sterm, Declk, Deckn, After_SM

      Tok_Function,        -- FUNCTION     Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Generic,         -- GENERIC      Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Package,         -- PACKAGE      Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Procedure,       -- PROCEDURE    Eterm, Sterm, Cunit, Declk, After_SM

      Tok_Private,         -- PRIVATE      Eterm, Sterm, Cunit, After_SM
      Tok_With,            -- WITH         Eterm, Sterm, Cunit, After_SM
      Tok_Separate,        -- SEPARATE     Eterm, Sterm, Cunit, After_SM

      Tok_EOF,             -- End of file  Eterm, Sterm, Cterm, After_SM

      Tok_Semicolon,       -- ;            Eterm, Sterm, Cterm

      Tok_Arrow,           -- =>           Sterm, Cterm, Chtok

      Tok_Vertical_Bar,    -- |            Cterm, Sterm, Chtok

      Tok_Dot_Dot,         -- ..           Sterm, Chtok

      --  The following three entries are used only when scanning
      --  project files.

      Tok_Project,
      Tok_Modifying,
      Tok_External,

      No_Token);
      --  No_Token is used for initializing Token values to indicate that
      --  no value has been set yet.

   -----------
   -- Types --
   -----------

   Max_Identifier : constant := 256;
   --  Maximum length of an identifier.

   type Extended_Token is record
      Token         : Token_Type := No_Token;
      --  Enclosing token

      Declaration   : Boolean := False;
      --  Are we inside a declarative part ?

      Record_Start_New_Line : Boolean := False;
      --  For a Tok_Record, set to True if the keyword "record" was found on
      --  its own line (only used internally).

      Type_Declaration : Boolean := False;
      --  Is it a type declaration ?

      Record_Type   : Boolean := False;
      --  Is it a record type definition ?

      Tagged_Type   : Boolean := False;
      --  Is it a tagged type definition ?

      Identifier    : String (1 .. Max_Identifier);
      --  Name of the enclosing token
      --  The actual name is Identifier (1 .. Ident_Len)

      Ident_Len     : Natural := 0;
      --  Actual length of Indentifier

      Profile_Start : Natural := 0;
      --  Position in the buffer where the profile of the current subprogram
      --  starts.

      Profile_End   : Natural := 0;
      --  Position in the buffer where the profile of the current subprogram
      --  ends.

      Sloc          : Source_Location;
      --  Source location for this entity
   end record;
   --  Extended information for a token

   package Token_Stack is new Generic_Stack (Extended_Token);
   use Token_Stack;

   package Indent_Stack is new Generic_Stack (Integer);
   use Indent_Stack;

   --------------------------
   -- Line Buffer Handling --
   --------------------------

   --  The line buffer represents a buffer (e.g contents of a file) line
   --  by line. Line separators (LF or CR/LF) are kept at the end of the buffer
   --  It is recommended to take advantage of the bound information that comes
   --  with a String_Access so that there can be a direct mapping between
   --  the original raw buffer and a line buffer.
   --  Len is used to keep the length of the original line stored. Since this
   --  type is intended for making changes in buffers at a minimal cost
   --  (e.g avoiding copies of complete buffers when inserting a few
   --  characters), being able to convert from the original buffer's position
   --  information to the line buffer is critical and is achieved using the Len
   --  field.

   type Line_Buffer_Record;
   type Line_Buffer is access Line_Buffer_Record;
   type Line_Buffer_Record is record
      Line : Basic_Types.String_Access;
      Len  : Natural;
      Next : Line_Buffer;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Line_Buffer_Record, Line_Buffer);

   type Extended_Line_Buffer is record
      First   : Line_Buffer;
      Current : Line_Buffer;
   end record;

   function To_Line_Buffer (Buffer : String) return Extended_Line_Buffer;
   --  Convert a string to a line buffer.
   --  CR/LF and LF are treated as end of lines.

   procedure Print (Buffer : Extended_Line_Buffer);
   --  Output the contents of Buffer on standard output.

   procedure Free (Buffer : in out Extended_Line_Buffer);
   --  Free the contents of buffer.

   ----------------------
   -- Parsing Routines --
   ----------------------

   type Construct_List_Access is access all Construct_List;

   procedure Analyze_Ada_Source
     (Buffer           : Basic_Types.Unchecked_String_Access;
      Buffer_Length    : Natural;
      New_Buffer       : in out Extended_Line_Buffer;
      Indent_Params    : Indent_Parameters;
      Reserved_Casing  : Casing_Type           := Lower;
      Ident_Casing     : Casing_Type           := Mixed;
      Format_Operators : Boolean               := True;
      Indent           : Boolean               := True;
      Constructs       : Construct_List_Access := null;
      Current_Indent   : out Natural;
      Prev_Indent      : out Natural;
      Callback         : Entity_Callback := null);
   --  Analyze a given Ada source in Buffer, and store the result in New_Buffer
   --  if Indent is True.
   --  If Callback is not null, call it for each Source_Entity_Kind.

end Ada_Analyzer;
