------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 1999-2019, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar.Time_IO;
with GNAT.Regpat;

with Templates_Parser.Configuration;
with Templates_Parser.Input;
with Templates_Parser.Utils;

package body Templates_Parser is

   use Ada.Exceptions;
   use Ada.Strings;

   Internal_Error : exception;

   Blank : constant Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);

   function Get_Parameters (Parameters : String) return Parameter_Set;
   --  Returns the parameters set as parsed into Parameters. This routines
   --  handles both positional and named parameters. It is currently used for
   --  include and macro parameters.

   --------------
   -- Tag Info --
   --------------

   Begin_Tag : Unbounded_String := To_Unbounded_String (Default_Begin_Tag);
   End_Tag   : Unbounded_String := To_Unbounded_String (Default_End_Tag);

   Set_Token                  : constant String := "@@SET@@";
   Table_Token                : constant String := "@@TABLE@@";
   Terminate_Sections_Token   : constant String := "@@TERMINATE_SECTIONS@@";
   Begin_Token                : constant String := "@@BEGIN@@";
   End_Token                  : constant String := "@@END@@";
   Section_Token              : constant String := "@@SECTION@@";
   End_Table_Token            : constant String := "@@END_TABLE@@";
   Macro_Token                : constant String := "@@MACRO@@";
   End_Macro_Token            : constant String := "@@END_MACRO@@";
   If_Token                   : constant String := "@@IF@@";
   Elsif_Token                : constant String := "@@ELSIF@@";
   Else_Token                 : constant String := "@@ELSE@@";
   End_If_Token               : constant String := "@@END_IF@@";
   Include_Token              : constant String := "@@INCLUDE@@";
   Inline_Token               : constant String := "@@INLINE@@";
   End_Inline_Token           : constant String := "@@END_INLINE@@";
   A_Terminate_Sections_Token : constant String := "TERMINATE_SECTIONS";
   A_Reverse_Token            : constant String := "REVERSE";
   A_Terse_Token              : constant String := "TERSE";

   ------------
   -- Filter --
   ------------

   package Filter is

      ----------------------
      --  Filters setting --
      ----------------------

      --  A filter appear just before a tag variable (e.g. @_LOWER:SOME_VAR_@
      --  and means that the filter LOWER should be applied to SOME_VAR before
      --  replacing it in the template file.

      type Mode is
        (Multiply,
         --  Multiply the given parameter to the string (operator "*")

         Plus,
         --  Add the given parameter to the string (operator "+")

         Minus,
         --  Substract the given parameter to the string (operator "-")

         Divide,
         --  Divide the given parameter to the string (operator "/")

         Absolute,
         --  Returns the abosulte value

         Add,
         --  Add the given parameter to the string

         Add_Param,
         --  Add an HTTP parameter to the string, add the '&' parameter
         --  separator if needed.

         BR_2_EOL,
         --  Replaces all <BR> HTML tags by a given end-of-line sequence

         BR_2_LF,
         --  Replaces all <BR> HTML tags by LF characters

         Capitalize,
         --  Lower case except char before spaces and underscores

         Clean_Text,
         --  Only letter/digits all other chars are changed to spaces

         Coma_2_Point,
         --  Replaces comas by points

         Contract,
         --  Replaces a suite of spaces by a single space character

         Del_Param,
         --  Delete an HTTP parameter from the string, removes the '&'
         --  Parameter separator if needed.

         Div,
         --  Divide the given parameter to the string

         Exist,
         --  Returns "TRUE" if var is not empty and "FALSE" otherwise

         File_Exists,
         --  Returns "TRUE" if var is the name of an existing file and "FALSE"
         --  otherwise.

         Format_Date,
         --  Returns the date formatted using the format parameter. This
         --  format is following the GNU/date as implemented in
         --  GNAT.Calendar.Time_IO. The date must be in the ISO format
         --  YYYY-MM-DD eventually followed by a space and the time with the
         --  format HH:MM:SS. So the string must be either 10 or 19 characters
         --  long.

         Format_Number,
         --  Returns the number with a space added between each 3 digits
         --  blocks. The decimal part is not transformed. If the data is not a
         --  number nothing is done. The data is trimmed before processing it.

         Is_Empty,
         --  Returns "TRUE" if var is empty and "FALSE" otherwise

         LF_2_BR,
         --  Replaces all LF character to <BR> HTML tag

         Lower,
         --  Lower case

         Match,
         --  Returns "TRUE" if var match the pattern passed as argument

         Max,
         --  Returns the max between the filter parameter and the value

         Min,
         --  Returns the min between the filter parameter and the value

         Modulo,
         --  Returns current value modulo N (N is the filter parameter)

         Mult,
         --  Multiply the given parameter to the string

         Neg,
         --  Change the size of the value

         No_Digit,
         --  Replace all digits by spaces

         No_Dynamic,
         --  This is a command filter, it indicates that the variable even if
         --  not found in the translation table will not be looked up into the
         --  dynamic context (Lazy_Tag). This filter just returns the string
         --  as-is.

         No_Letter,
         --  Removes all letters by spaces

         No_Space,
         --  Removes all spaces found in the value

         Oui_Non,
         --  If True return Oui, If False returns Non, else do nothing

         Point_2_Coma,
         --  Replaces points by comas

         Repeat,
         --  Returns N copy of the original string. The number of copy is
         --  passed as parameter.

         Replace,
         --  Replaces part of the string using a regultar expression. This is
         --  equivalent to the well known "s/<regexp>/<new value>/" sed
         --  command. It replaces only the first match.

         Replace_All,
         --  Idem as above, but replace all matches. This equivalent to the
         --  well known "s/<regexp>/<new value>/g" sed command.

         Replace_Param,
         --  Idem as @_ADD_PARAM(key=value):DEL_PARAM(key):VAR_@

         Invert,
         --  Reverse string

         Size,
         --  Returns the number of characters in the string value

         Slice,
         --  Returns a slice of the string

         Sub,
         --  Substract the given parameter to the string

         Trim,
         --  Trim leading and trailing space

         Upper,
         --  Upper case

         User_Defined,
         --  A user's defined filter

         Web_Encode,
         --  Idem as Web_Escape and also encode non 7-bit ASCII characters as
         --  &#xxx;.

         Web_Escape,
         --  Convert characters <>&" to HTML equivalents: &lt;, &gt; and &amp;
         --  and &quot;

         Web_NBSP,
         --  Convert spaces to HTML &nbsp; - non breaking spaces

         Wrap,
         --  Wrap lines longer than a given number of characters

         Yes_No
         --  If True return Yes, If False returns No, else do nothing
        );

      type User_CB_Type is (With_Param, No_Param, As_Tagged);
      type User_CB (Typ : User_CB_Type := With_Param) is record
         case Typ is
            when With_Param  => CBP : Callback;
            when No_Param    => CB  : Callback_No_Param;
            when As_Tagged   => CBT : User_Filter_Access;
         end case;
      end record;

      type Parameter_Mode is (Str, Regexp, Regpat, Slice, User_Callback);

      function Parameter (Mode : Filter.Mode) return Parameter_Mode;
      --  Returns the parameter mode for the given filter

      type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;

      type Parameter_Data (Mode : Parameter_Mode := Slice) is record
         case Mode is
            when Str =>
               S : Unbounded_String;

            when Regexp =>
               R_Str   : Unbounded_String;
               Regexp  : Pattern_Matcher_Access;

            when Regpat =>
               P_Str   : Unbounded_String;
               Regpat  : Pattern_Matcher_Access;
               Param   : Unbounded_String;

            when Slice =>
               First   : Integer;
               Last    : Integer;

            when User_Callback =>
               Handler : User_CB;
               P       : Unbounded_String;
         end case;
      end record;

      No_Parameter : constant Parameter_Data :=
                       Parameter_Data'(Slice, 0, -1);

      function Image (P : Parameter_Data) return String;
      --  Returns parameter string representation

      procedure Release (P : in out Parameter_Data);
      pragma Inline (Release);
      --  Release all memory allocated P

      type Filter_Context (P_Size : Natural) is record
         Translations : Translate_Set;
         Lazy_Tag     : Dynamic.Lazy_Tag_Access;
         I_Parameters : Parameter_Set (1 .. P_Size);
      end record;

      type Callback is access function
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;
      --  P is the filter parameter, no parameter by default. Parameter are
      --  untyped and will be parsed by the filter function if needed.

      type Routine is record
         Handle     : Callback;
         Parameters : Parameter_Data;
      end record;

      type Set is array (Positive range <>) of Routine;
      type Set_Access is access Set;

      procedure Release (S : in out Set);
      --  Release all memory allocated P

      type String_Access is access constant String;

      type Filter_Record is record
         Name   : String_Access;
         Handle : Callback;
      end record;

      --  User's filter

      procedure Register
        (Name    : String;
         Handler : Templates_Parser.Callback);

      procedure Register
        (Name    : String;
         Handler : Callback_No_Param);

      procedure Register
        (Name    : String;
         Handler : not null access User_Filter'Class);

      procedure Free_Filters;

      function User_Handle (Name : String) return User_CB;
      --  Returns the registered user's callback for the given filter name

      --  filter functions, see above

      procedure Check_Null_Parameter (P : Parameter_Data);
      --  Raises Template_Error if P is not equal to Null_Parameter

      function Absolute
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Add_Param
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function BR_2_EOL
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function BR_2_LF
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Capitalize
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Clean_Text
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Coma_2_Point
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Contract
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Del_Param
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Exist
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function File_Exists
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Format_Date
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Format_Number
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Is_Empty
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function LF_2_BR
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Lower
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Match
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Max
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Min
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Neg
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function No_Digit
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function No_Dynamic
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function No_Letter
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function No_Space
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Oui_Non
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Point_2_Coma
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Repeat
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Replace
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Replace_All
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Replace_Param
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Reverse_Data
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Size
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Slice
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Trim
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Upper
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function User_Defined
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Web_Encode
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Web_Escape
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Web_NBSP
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Wrap
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Yes_No
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Plus
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Minus
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Divide
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Multiply
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Modulo
        (S : String;
         C : not null access Filter_Context;
         P : Parameter_Data := No_Parameter) return String;

      function Handle (Name : String) return Callback;
      --  Returns the filter function for the given filter name

      function Handle (Mode : Filter.Mode) return Callback;
      --  Returns the filter function for the given filter mode

      function Mode_Value (Name : String) return Mode;
      --  Returns the Mode for filter named Name. This is the internal
      --  representation for this filter name.

      function Name (Handle : Callback) return String;
      --  Returns the filter name for the given filter function

      function Is_No_Dynamic (Filters : Set_Access) return Boolean;
      --  Returns True if Filters contains NO_CONTEXT

   end Filter;

   ---------------
   -- Main tree --
   ---------------

   type Node;
   type Tree is access Node;

   ----------
   -- Data --
   ----------

   package Data is

      type Node;
      type Tree is access Node;

      type NKind is (Text, Var);

      type Attribute is (Nil, Length, Line, Min_Column, Max_Column, Up_Level);

      type Internal_Tag
        is (No, Now, Year, Month, Month_Name, Day, Day_Name, Hour, Minute,
            Second, Number_Line, Table_Line, Table_Level, Up_Table_Line);

      type Attribute_Data is record
         Attr  : Attribute := Nil;
         Value : Integer;
      end record;

      No_Attribute : constant Attribute_Data := (Nil, 0);

      type Parameter_Set is array (Natural range <>) of Tree;
      type Parameters is access all Parameter_Set;

      type Tag_Var is record
         Name       : Unbounded_String;
         Filters    : Filter.Set_Access;
         Attribute  : Attribute_Data;
         N          : Integer;           -- Include variable index
         Internal   : Internal_Tag;      -- No if not an internal variable
         Is_Macro   : Boolean := False;  -- True if this is a macro call
         Parameters : Data.Parameters;
         Def        : Templates_Parser.Tree;
      end record;

      type Node (Kind : NKind) is record
         Next : Tree;
         case Kind is
            when Text =>
               Value : Unbounded_String;
            when Var =>
               Var   : Tag_Var;
         end case;
      end record;

      function Is_Include_Variable (T : Tag_Var) return Boolean;
      pragma Inline (Is_Include_Variable);
      --  Returns True if T is an include variable (Name is $<n>)

      function To_Data_Parameters
        (Parameters : Templates_Parser.Parameter_Set) return Data.Parameters;
      --  Returns Parameters as a Data.Tree set

      function Build (Str : String) return Tag_Var;
      --  Create a Tag from Str. A tag is composed of a name and a set of
      --  filters.

      function Image (T : Tag_Var) return String;
      --  Returns string representation for the Tag variable

      function Translate
        (T       : Tag_Var;
         Value   : String;
         Context : not null access Filter.Filter_Context) return String;
      --  Returns the result of Value after applying all filters for tag T

      procedure Release (T : in out Tag_Var);
      --  Release all memory associated with Tag

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Parameter_Set, Parameters);

      function Parse (Line : String) return Tree;
      --  Parse text line and returns the corresponding tree representation

      procedure Print_Tree (D : Tree);
      --  Decend the text tree and print it to the standard output

      procedure Release (D : in out Tree; Single : Boolean := False);
      --  Release all memory used by the tree or only the pointed node if
      --  Single is set to True.

      function Clone (D : Tree) return Tree;
      --  Returns a Clone of D

      function Clone (V : Tag_Var) return Tag_Var;
      --  Returns a Clone of V

   end Data;

   -----------------
   -- Definitions --
   -----------------

   package Definitions is

      type NKind is (Const, Ref, Ref_Default);

      type Node (Kind : NKind := Const) is record
         Value : Unbounded_String;
         Ref   : Positive;
      end record;

      type Def is record
         Name : Unbounded_String;
         N    : Node;
      end record;

      type Tree is access Def;

      function Parse (Line : String) return Tree;
      --  Returns a defintion data

      package Def_Map is new
        Containers.Indefinite_Hashed_Maps
          (String, Node, Ada.Strings.Hash, "=", "=");
      subtype Map is Def_Map.Map;

      procedure Print_Tree (D : Tree);
      --  Decend the text tree and print it to the standard output

      procedure Release (D : in out Tree);
      --  Release all memory used by the tree

      function Clone (D : Tree) return Tree;
      --  Returns a Clone of D

   end Definitions;

   ------------------
   --  Expressions --
   ------------------

   package Expr is

      type Ops is (O_And, O_Or, O_Xor,
                   O_Sup, O_Inf, O_Esup, O_Einf, O_Equal, O_Diff, O_In);

      function Image (O : Ops) return String;
      --  Returns Ops string representation

      type U_Ops is (O_Not);

      function Image (O : U_Ops) return String;
      --  Returns U_Ops string representation

      type Node;
      type Tree is access Node;

      type NKind is (Value, Var, Op, U_Op);
      --  The node is a value, a variable a binary operator or an unary
      --  operator.

      type Node (Kind : NKind) is record
         case Kind is
            when Value =>
               V   : Unbounded_String;

            when Var =>
               Var : Data.Tag_Var;

            when Op =>
               O           : Ops;
               Left, Right : Tree;

            when U_Op =>
               U_O         : U_Ops;
               Next        : Tree;
         end case;
      end record;

      function Is_True (Str : String) return Boolean;
      pragma Inline (Is_True);
      --  Return True if Str is one of "TRUE" or "T", the test is not
      --  case sensitive.

      function Parse (Expression : String) return Tree;
      --  Parse Expression and returns the corresponding tree representation

      procedure Print_Tree (E : Tree);
      --  Decend the expression's tree and print the expression. It outputs the
      --  expression with all parenthesis to show without ambiguity the way the
      --  expression has been parsed.

      procedure Release (E : in out Tree; Single : Boolean := False);
      --  Release all associated memory with the tree

      function Clone (E : Tree) return Tree;
      --  Returns a Clone of E

      Unknown : constant String := "*";
      --  Value returned when the expression cannot be determined statically

      function Analyze (E : Expr.Tree) return String;
      --  Static analysis of expresssion E, returns TRUE, FALSE or value
      --  Unknown if the value cannot be determined statically (some variables
      --  are in the expression).

   end Expr;

   --------------------------------
   --  Template Tree definitions --
   --------------------------------

   type Nkind is (Info,          --  first node is tree infos
                  C_Info,        --  second node is cache tree info
                  Text,          --  this is a text line
                  Set_Stmt,      --  a definition statement
                  If_Stmt,       --  an IF tag statement
                  Table_Stmt,    --  a TABLE tag statement
                  Section_Block, --  a TABLE block (common, section)
                  Section_Stmt,  --  a TABLE section
                  Include_Stmt,  --  an INCLUDE tag statement
                  Inline_Stmt);  --  an INLINE tag statement

   --  A template line is coded as a suite of Data and Var elements

   --  The first node in the tree is of type Info and should never be release
   --  and changed. This ensure that included tree will always be valid
   --  otherwise will would have to parse all the current trees in the cache
   --  to update the reference.
   --
   --  Static_Tree represent a Tree immune to cache changes. Info point to the
   --  first node and C_Info to the second one. C_Info could be different to
   --  Info.Next in case of cache changes. This way we keep a pointer to the
   --  old tree to be able to release it when not used anymore. This way it is
   --  possible to use the cache in multitasking program without trouble. The
   --  changes in the cache are either because more than one task is parsing
   --  the same template at the same time, they will update the cache with the
   --  same tree at some point, or because a newer template was found in the
   --  file system.

   type Static_Tree is record
      Info   : Tree;
      C_Info : Tree;
   end record;

   Null_Static_Tree : constant Static_Tree := (null, null);

   type Node (Kind : Nkind) is record
      Next : Tree;
      Line : Natural;

      case Kind is
         when Info =>
            Filename  : Unbounded_String;         -- Name of the file
            Timestamp : Configuration.Time_Stamp; -- Date/Time of last change
            I_File    : Tree;                     -- Included file references

         when C_Info =>
            Obsolete  : Boolean := False;    -- True if newer version in cache
            Used      : Natural := 0;        -- >0 if currently used

         when Text =>
            Text      : Data.Tree;

         when Set_Stmt =>
            Def       : Definitions.Tree;

         when If_Stmt =>
            Cond      : Expr.Tree;
            N_True    : Tree;
            N_False   : Tree;

         when Table_Stmt =>
            Terminate_Sections : Boolean;
            Reverse_Index      : Boolean;
            Terse              : Boolean;
            Blocks             : Tree;
            Blocks_Count       : Natural;     -- Number if blocks

         when Section_Block =>
            Common         : Tree;
            Sections       : Tree;
            Sections_Count : Natural;         -- Number of sections

         when Section_Stmt =>
            N_Section : Tree;

         when Include_Stmt =>
            File       : Static_Tree;
            I_Filename : Data.Tree;
            I_Params   : Data.Parameters;

         when Inline_Stmt =>
            Before  : Unbounded_String;
            Sep     : Unbounded_String;
            After   : Unbounded_String;
            I_Block : Tree;
      end case;
   end record;

   procedure Release (T : in out Tree; Include : Boolean := True);
   --  Release all memory associated with the tree

   function Clone (T : Tree) return Tree;
   --  Returns a Clone of T

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Node, Tree);

   -------------------
   --  Cached Files --
   -------------------

   --  Cached_Files keep the parsed Tree for a given file in memory. This
   --  package has two implementations one is thread safe so it is possible to
   --  use the cache in a multitasking program. The other is meant to be used
   --  for configuration that do not want to drag the tasking runtime.

   package Cached_Files is

      procedure Add
        (Filename : String;
         T        : Tree;
         Old      : out Tree);
      --  Add Filename/T to the list of cached files. If Filename is
      --  already in the list, replace the current tree with T. Furthermore
      --  if Filename tree is already in use, Old will be set with the
      --  previous C_Info node otherwise Old will be T.Next (C_Info node
      --  for current tree).

      procedure Get
        (Filename : String;
         Result   : out Static_Tree);
      --  Returns the Tree for Filename or Null_Static_Tree if Filename has
      --  not been cached or is obsolete.

      procedure Release (T : in out Static_Tree);
      --  After loading a tree and using it, it is required that it be
      --  released. This will ensure that a tree marked as obsolete (a new
      --  version being now in the cache) will be released from the memory.

      procedure Release;
      --  Release the internal cache. This free the memory used for all
      --  currently loaded template trees.

   end Cached_Files;

   -----------
   -- Macro --
   -----------

   package Macro is

      procedure Register (Name : String; T : Tree);
      --  Register a new macro definition, if previous definition exists,
      --  replace with the new definition.

      function Get (Name : String) return Tree;
      --  Get macro tree, returns null if this macro has no definition

      procedure Rewrite
        (T          : in out Tree;
         Parameters : not null access Data.Parameter_Set);
      --  Rewrite the macro tree with the given parameters. All @_1_@
      --  parameters in the macro definition are replaced with the
      --  corresponding value in the parameter set.

      procedure Print_Defined_Macros;
      --  Print the defined macros (for debug purpose)

      Callback : Macro_Callback;

   end Macro;

   ----------------
   -- Simplifier --
   ----------------

   package Simplifier is

      procedure Run (T : in out Tree);
      --  Execute a tree simplifier pass, removes statically known IF branch
      --  and merges all consecutive TEXT node.

   end Simplifier;

   ---------
   -- Tag --
   ---------

   procedure Field
     (T      : Tag;
      N      : Positive;
      Result : out Tag_Node_Access;
      Found  : out Boolean);
   --  Returns the Nth item in Tag

   procedure Field
     (T        : Tag;
      Cursor   : Indices;
      Up_Value : Natural;
      Result   : out Unbounded_String;
      Found    : out Boolean);
   --  Returns Value in Tag at position Cursor. Found is set to False if
   --  there is no such value in Tag.

   function No_Quote (Str : String) return String;
   --  Removes quotes around Str. If Str (Str'First) and Str (Str'Last)
   --  are quotes return Str (Str'First + 1 ..  Str'Last - 1) otherwise
   --  return Str as-is.

   function Quote (Str : String) return String;
   --  Returns Str quoted if it contains spaces, otherwise just returns Str

   function Is_Number (S : String) return Boolean;
   pragma Inline (Is_Number);
   --  Returns True if S is a decimal number

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (Integer, Integer_Access);

   procedure Unchecked_Free is
     new Unchecked_Deallocation (Tag_Node_Arr, Tag_Node_Arr_Access);

   function Build_Include_Pathname
     (Filename, Include_Filename : String) return String;
   --  Returns the full pathname to the include file (Include_Filename). It
   --  returns Include_Filename if there is a pathname specified, or the
   --  pathname of the main template file as a prefix of the include
   --  filename.

   function Load
     (Filename     : String;
      Cached       : Boolean := False;
      Include_File : Boolean := False) return Static_Tree;
   --  Load a template file and returns the semantic tree. The template file is
   --  cached if Cached is set to true. If cached next Load will use the
   --  preparsed tree.

   procedure Print_Tree (T : Tree; Level : Natural := 0);
   --  Print the semantic tree, this is mostly for debugging purpose

   ---------
   -- "&" --
   ---------

   function "&" (T : Tag; Value : String) return Tag is
      Item : constant Tag_Node_Access :=
               new Tag_Node'
                 (Templates_Parser.Value, null,
                  V => To_Unbounded_String (Value));
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;

      Unchecked_Free (T.Data.Tag_Nodes);

      if T.Data.Head = null then
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => 1,
            Separator    => To_Unbounded_String (Default_Separator),
            Head         => Item,
            Last         => Item,
            Tag_Nodes    => null,
            Values       => null);

         return (Ada.Finalization.Controlled with T.Ref_Count, Data => T.Data);

      else
         T.Data.Last.Next := Item;
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => T.Data.Nested_Level,
            Separator    => T.Data.Separator,
            Head         => T.Data.Head,
            Last         => Item,
            Tag_Nodes    => null,
            Values       => null);

         return (Ada.Finalization.Controlled with T.Ref_Count, Data => T.Data);
      end if;
   end "&";

   function "&" (Value : String; T : Tag) return Tag is
      Item : constant Tag_Node_Access :=
               new Tag_Node'
                 (Templates_Parser.Value, T.Data.Head,
                  V => To_Unbounded_String (Value));
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;

      Unchecked_Free (T.Data.Tag_Nodes);

      if T.Data.Head = null then
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => 1,
            Separator    => To_Unbounded_String (Default_Separator),
            Head         => Item,
            Last         => Item,
            Tag_Nodes    => null,
            Values       => null);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);

      else
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => T.Data.Nested_Level,
            Separator    => T.Data.Separator,
            Head         => Item,
            Last         => T.Data.Last,
            Tag_Nodes    => null,
            Values       => null);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);
      end if;
   end "&";

   function "&" (T : Tag; Value : Tag) return Tag is
      Item   : constant Tag_Node_Access :=
                 new Tag_Node'(Value_Set, null, new Tag'(Value));
      T_Size : constant Natural := Size (Value);
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;

      Unchecked_Free (T.Data.Tag_Nodes);

      if T.Data.Head = null then
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, T_Size),
            Max          => Natural'Max (T.Data.Max, T_Size),
            Nested_Level => Value.Data.Nested_Level + 1,
            Separator    => To_Unbounded_String ((1 => ASCII.LF)),
            Head         => Item,
            Last         => Item,
            Tag_Nodes    => null,
            Values       => null);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);

      else
         T.Data.Last.Next := Item;

         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, T_Size),
            Max          => Natural'Max (T.Data.Max, T_Size),
            Nested_Level =>
              Positive'Max
                (T.Data.Nested_Level, Value.Data.Nested_Level + 1),
            Separator    => T.Data.Separator,
            Head         => T.Data.Head,
            Last         => Item,
            Tag_Nodes    => null,
            Values       => null);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);
      end if;
   end "&";

   function "&" (T : Tag; Value : Character) return Tag is
   begin
      return T & String'(1 => Value);
   end "&";

   function "&" (T : Tag; Value : Boolean) return Tag is
   begin
      return T & Boolean'Image (Value);
   end "&";

   function "&" (T : Tag; Value : Unbounded_String) return Tag is
   begin
      return T & To_String (Value);
   end "&";

   function "&" (T : Tag; Value : Integer) return Tag is
   begin
      return T & Utils.Image (Value);
   end "&";

   function "&" (Value : Character; T : Tag) return Tag is
   begin
      return String'(1 => Value) & T;
   end "&";

   function "&" (Value : Boolean; T : Tag) return Tag is
   begin
      return Boolean'Image (Value) & T;
   end "&";

   function "&" (Value : Unbounded_String; T : Tag) return Tag is
   begin
      return To_String (Value) & T;
   end "&";

   function "&" (Value : Integer; T : Tag) return Tag is
   begin
      return Utils.Image (Value) & T;
   end "&";

   function "&"
     (Left : Association; Right : Association) return Translate_Set
   is
      T : Translate_Set;
   begin
      Insert (T, Left);
      Insert (T, Right);
      return T;
   end "&";

   function "&"
     (Set : Translate_Set; Item : Association) return Translate_Set
   is
      T : Translate_Set := Set;
   begin
      Insert (T, Item);
      return T;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (Value : String) return Tag is
      Item : constant Tag_Node_Access :=
               new Tag_Node'(Templates_Parser.Value,
                             null,
                             V => To_Unbounded_String (Value));
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count    => new Integer'(1),
              Data         => new Tag_Data'
                (Count        => 1,
                 Min          => 1,
                 Max          => 1,
                 Nested_Level => 1,
                 Separator    => To_Unbounded_String (Default_Separator),
                 Head         => Item,
                 Last         => Item,
                 Tag_Nodes    => null,
                 Values       => null));
   end "+";

   function "+" (Value : Character) return Tag is
   begin
      return +String'(1 => Value);
   end "+";

   function "+" (Value : Boolean) return Tag is
   begin
      return +Boolean'Image (Value);
   end "+";

   function "+" (Value : Unbounded_String) return Tag is
   begin
      return +To_String (Value);
   end "+";

   function "+" (Value : Integer) return Tag is
   begin
      return +Utils.Image (Value);
   end "+";

   function "+" (Value : Tag) return Tag is
      Result : Tag;
   begin
      Result := Result & Value;
      --  This is an embedded tag, set separator to LF
      Set_Separator (Result, (1 => ASCII.LF));
      return Result;
   end "+";

   function "+" (Item : Association) return Translate_Set is
      T : Translate_Set;
   begin
      Insert (T, Item);
      return T;
   end "+";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Set : in out Translate_Set) is
   begin
      Templates_Parser_Tasking.Lock;

      if Set.Ref_Count = null then
         --  This is a not yet initialized null set. This case happens when
         --  assigning Null_Set to a Translate_Set variable for example.
         Initialize (Set);

      else
         Set.Ref_Count.all := Set.Ref_Count.all + 1;
      end if;

      Templates_Parser_Tasking.Unlock;
   end Adjust;

   overriding procedure Adjust (T : in out Tag) is
   begin
      Templates_Parser_Tasking.Lock;
      T.Ref_Count.all := T.Ref_Count.all + 1;
      Templates_Parser_Tasking.Unlock;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Tag; Value : Tag) is
      Item   : constant Tag_Node_Access :=
                 new Tag_Node'(Value_Set, null, new Tag'(Value));
      T_Size : constant Natural := Size (Value);
   begin
      if T.Data.Head = null then
         T.Data.Nested_Level := Value.Data.Nested_Level + 1;
         T.Data.Separator    := To_Unbounded_String ((1 => ASCII.LF));
         T.Data.Head         := Item;
      else
         T.Data.Last.Next := Item;
         T.Data.Nested_Level :=
           Positive'Max
             (T.Data.Nested_Level, Value.Data.Nested_Level + 1);
      end if;

      Unchecked_Free (T.Data.Tag_Nodes);
      T.Data.Tag_Nodes := null;
      T.Data.Count     := T.Data.Count + 1;
      T.Data.Min       := Natural'Min (T.Data.Min, T_Size);
      T.Data.Max       := Natural'Max (T.Data.Max, T_Size);
      T.Data.Last      := Item;
   end Append;

   procedure Append (T : in out Tag; Value : Unbounded_String) is
      Item : constant Tag_Node_Access :=
               new Tag_Node'(Templates_Parser.Value, null, Value);
   begin
      if T.Data.Head = null then
         T.Data.Head         := Item;
         T.Data.Nested_Level := 1;
         T.Data.Separator    := To_Unbounded_String (Default_Separator);

      else
         T.Data.Last.Next := Item;
      end if;

      Unchecked_Free (T.Data.Tag_Nodes);
      T.Data.Tag_Nodes := null;
      T.Data.Count     := T.Data.Count + 1;
      T.Data.Min       := Natural'Min (T.Data.Min, 1);
      T.Data.Max       := Natural'Max (T.Data.Max, 1);
      T.Data.Last      := Item;
   end Append;

   procedure Append (T : in out Tag; Value : String) is
   begin
      Append (T, To_Unbounded_String (Value));
   end Append;

   procedure Append (T : in out Tag; Value : Character) is
   begin
      Append (T, To_Unbounded_String (String'(1 => Value)));
   end Append;

   procedure Append (T : in out Tag; Value : Boolean) is
   begin
      Append (T, To_Unbounded_String (Boolean'Image (Value)));
   end Append;

   procedure Append (T : in out Tag; Value : Integer) is
   begin
      Append (T, To_Unbounded_String (Utils.Image (Value)));
   end Append;

   -----------
   -- Assoc --
   -----------

   function Assoc
     (Variable : String;
      Value    : String) return Association is
   begin
      return Association'
        (Std,
         Variable => To_Unbounded_String (Variable),
         Value    => To_Unbounded_String (Value));
   end Assoc;

   function Assoc
     (Variable : String;
      Value    : Ada.Strings.Unbounded.Unbounded_String)
      return Association is
   begin
      return Assoc (Variable, To_String (Value));
   end Assoc;

   function Assoc
     (Variable : String;
      Value    : Integer) return Association is
   begin
      return Assoc (Variable, Utils.Image (Value));
   end Assoc;

   function Assoc
     (Variable : String;
      Value    : Boolean) return Association is
   begin
      if Value then
         return Assoc (Variable, "TRUE");
      else
         return Assoc (Variable, "FALSE");
      end if;
   end Assoc;

   function Assoc
     (Variable  : String;
      Value     : Tag;
      Separator : String := Default_Separator) return Association
   is
      T : Tag := Value;
   begin
      if Separator /= Default_Separator then
         Set_Separator  (T, Separator);
      end if;

      return Association'
        (Composite,
         Variable   => To_Unbounded_String (Variable),
         Comp_Value => T);
   end Assoc;

   ----------------------------
   -- Build_Include_Pathname --
   ----------------------------

   function Build_Include_Pathname
     (Filename, Include_Filename : String) return String
   is
      Dir_Seps : constant Maps.Character_Set := Maps.To_Set ("/\");
   begin
      if Include_Filename'Length > 1
        and then Maps.Is_In
          (Include_Filename (Include_Filename'First), Dir_Seps)
      then
         --  Include filename is an absolute path, return it without the
         --  leading directory separator.
         return Include_Filename
           (Include_Filename'First + 1 .. Include_Filename'Last);

      else
         declare
            K : constant Natural :=
                  Fixed.Index (Filename, Dir_Seps, Going => Strings.Backward);
         begin
            if K = 0 then
               return Include_Filename;
            else
               return Filename (Filename'First .. K) & Include_Filename;
            end if;
         end;
      end if;
   end Build_Include_Pathname;

   ------------------
   -- Cached_Files --
   ------------------

   package body Cached_Files is separate;

   -----------
   -- Macro --
   -----------

   package body Macro is separate;

   ----------------
   -- Simplifier --
   ----------------

   package body Simplifier is separate;

   -----------
   -- Clear --
   -----------

   procedure Clear (T : in out Tag) is
      NT : Tag;
   begin
      --  Here we just separate current vector from the new one
      T := NT;
   end Clear;

   -----------
   -- Clone --
   -----------

   function Clone (T : Tree) return Tree is
      N : Tree;
   begin
      if T /= null then
         N := new Node'(T.all);

         case N.Kind is
            when Text =>
               N.Text := Data.Clone (N.Text);

            when Info =>
               N.I_File := Clone (N.I_File);

            when If_Stmt =>
               N.Cond := Expr.Clone (N.Cond);
               N.N_True := Clone (N.N_True);
               N.N_False := Clone (N.N_False);

            when Table_Stmt =>
               N.Blocks := Clone (N.Blocks);

            when Section_Block =>
               N.Common := Clone (N.Common);
               N.Sections := Clone (N.Sections);

            when Section_Stmt =>
               N.N_Section := Clone (N.N_Section);

            when Inline_Stmt =>
               N.I_Block := Clone (N.I_Block);

            when Set_Stmt =>
               N.Def := Definitions.Clone (N.Def);

            when Include_Stmt =>
               N.I_Filename := Data.Clone (N.I_Filename);
               N.I_Params := new Data.Parameter_Set'(N.I_Params.all);
               for K in N.I_Params'Range loop
                  N.I_Params (K) := Data.Clone (N.I_Params (K));
               end loop;

            when C_Info =>
               null;
         end case;

         N.Next := Clone (N.Next);
      end if;

      return N;
   end Clone;

   ---------------
   -- Composite --
   ---------------

   function Composite (T : Tag; N : Positive) return Tag is
      Result : Tag;
      Found  : Boolean;
   begin
      Field (T, N, Result, Found);

      if Found then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end Composite;

   ----------
   -- Data --
   ----------

   package body Data is separate;

   -----------------
   -- Definitions --
   -----------------

   package body Definitions is separate;

   ------------
   -- Exists --
   ------------

   function Exists
     (Set : Translate_Set; Variable : String) return Boolean is
   begin
      return Set.Set.Contains (Variable);
   end Exists;

   ----------
   -- Expr --
   ----------

   package body Expr is separate;

   -----------
   -- Field --
   -----------

   procedure Field
     (T      : Tag;
      N      : Positive;
      Result : out Tag_Node_Access;
      Found  : out Boolean) is
   begin
      Found := True;

      --  First check that the array access is present

      if T.Data.Tag_Nodes = null then
         T.Data.Tag_Nodes := new Tag_Node_Arr (1 .. T.Data.Count);

         declare
            P : Tag_Node_Access := T.Data.Head;
         begin
            for K in T.Data.Tag_Nodes'Range loop
               T.Data.Tag_Nodes (K) := P;
               P := P.Next;
            end loop;
         end;
      end if;

      if N > T.Data.Count then
         --  No such item for this position
         Result := null;
         Found  := False;

      else
         Result := T.Data.Tag_Nodes (N);
      end if;
   end Field;

   procedure Field
     (T      : Tag;
      N      : Positive;
      Result : out Tag;
      Found  : out Boolean)
   is
      R : Tag_Node_Access;
   begin
      Field (T, N, R, Found);

      if Found and then R.Kind = Value_Set then
         --  There is a Tag at this position, return it
         Result := R.VS.all;
      else
         Found := False;
      end if;
   end Field;

   procedure Field
     (T        : Tag;
      Cursor   : Indices;
      Up_Value : Natural;
      Result   : out Unbounded_String;
      Found    : out Boolean)
   is

      function Image (T : Tag) return Unbounded_String;
      --  Returns T string representation

      -----------
      -- Image --
      -----------

      function Image (T : Tag) return Unbounded_String is

         function Image (N : Tag_Node) return Unbounded_String;
         --  Returns N string representation

         -----------
         -- Image --
         -----------

         function Image (N : Tag_Node) return Unbounded_String is
         begin
            if N.Kind = Value then
               return N.V;
            else
               return Image (N.VS.all);
            end if;
         end Image;

         Result : Unbounded_String;
         N      : Tag_Node_Access := T.Data.Head;
      begin
         while N /= null loop
            if Result /= Null_Unbounded_String then
               Append (Result, T.Data.Separator);
            end if;
            Append (Result, Image (N.all));
            N := N.Next;
         end loop;
         return Result;
      end Image;

      C       : Natural := 0;
      P       : Natural := 0;
      R       : Tag_Node_Access;
      Inlined : Boolean := False;
   begin
      Found := True;

      if Cursor'Length <= Up_Value then
         --  The current cursor length is smaller than the up_level attribute
         --  in this case we just inline the tag.
         Inlined := True;

      elsif Cursor'Length > T.Data.Nested_Level then
         C := Cursor'Last - T.Data.Nested_Level + 1 - Up_Value;
         P := Cursor (C);

      elsif Cursor'Length /= 0 then
         C := Cursor'First;
         P := Cursor (C);
      end if;

      if Inlined then
         Result := Image (T);

      else
         Field (T, P, R, Found);
      end if;

      if R /= null then
         --  We have found something at this indice

         if C + Up_Value = Cursor'Last then
            --  This is the last position

            if R.Kind = Value then
               --  Found a leaf, just return the value
               Result := R.V;
            else
               Result := Image (R.VS.all);
            end if;

         else
            --  There is more position to look for in the cursor

            if R.Kind = Value then
               --  This is a leaf, there is nothing more to look for
               Found  := False;
               Result := Null_Unbounded_String;

            else
               --  Look into next dimension
               Field
                 (R.VS.all,
                  Cursor (C + 1 .. Cursor'Last), Up_Value,
                  Result, Found);
            end if;
         end if;

      else
         Found := False;
      end if;
   end Field;

   ------------
   -- Filter --
   ------------

   package body Filter is separate;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Set : in out Translate_Set) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Association_Map.Map, Map_Access);
      Ref_Count : Integer_Access := Set.Ref_Count;
   begin
      --  Ensure call is idempotent

      Set.Ref_Count := null;

      if Ref_Count /= null then
         Templates_Parser_Tasking.Lock;
         Ref_Count.all := Ref_Count.all - 1;

         if Ref_Count.all = 0 then
            Unchecked_Free (Ref_Count);
            Unchecked_Free (Set.Set);
         end if;
         Templates_Parser_Tasking.Unlock;
      end if;
   end Finalize;

   overriding procedure Finalize (T : in out Tag) is
      Ref_Count : Integer_Access := T.Ref_Count;
   begin
      --  Ensure call is idempotent

      T.Ref_Count := null;

      if Ref_Count /= null then
         Templates_Parser_Tasking.Lock;

         Ref_Count.all := Ref_Count.all - 1;

         if Ref_Count.all = 0 then
            Templates_Parser_Tasking.Unlock;

            declare
               procedure Unchecked_Free is new Ada.Unchecked_Deallocation
                 (Tag_Node, Tag_Node_Access);

               procedure Unchecked_Free is new Ada.Unchecked_Deallocation
                 (Tag, Tag_Access);

               procedure Unchecked_Free is new Ada.Unchecked_Deallocation
                 (Tag_Data, Tag_Data_Access);

               procedure Unchecked_Free is new Ada.Unchecked_Deallocation
                 (Tag_Values.Set, Tag_Values_Access);

               P, N : Tag_Node_Access;
            begin
               P := T.Data.Head;

               while P /= null loop
                  N := P.Next;

                  if P.Kind = Value_Set then
                     Unchecked_Free (P.VS);
                  end if;

                  Unchecked_Free (P);

                  P := N;
               end loop;

               T.Data.Head := null;
               T.Data.Last := null;

               Unchecked_Free (Ref_Count);
               Unchecked_Free (T.Data.Tag_Nodes);
               Unchecked_Free (T.Data.Values);
               Unchecked_Free (T.Data);
            end;

         else
            Templates_Parser_Tasking.Unlock;
         end if;
      end if;
   end Finalize;

   ---------------------------
   -- For_Every_Association --
   ---------------------------

   procedure For_Every_Association (Set : Translate_Set) is
      Pos  : Association_Map.Cursor;
      Quit : Boolean := False;
   begin
      Pos := Set.Set.First;

      while Association_Map.Has_Element (Pos) loop
         Action (Association_Map.Element (Pos), Quit);
         exit when Quit;
         Pos := Association_Map.Next (Pos);
      end loop;
   end For_Every_Association;

   ---------
   -- Get --
   ---------

   function Get
     (Set  : Translate_Set;
      Name : String) return Association
   is
      Pos : Association_Map.Cursor;
   begin
      Pos := Set.Set.all.Find (Name);

      if Association_Map.Has_Element (Pos) then
         return Association_Map.Element (Pos);
      else
         return Null_Association;
      end if;
   end Get;

   function Get (Assoc : Association) return Tag is
   begin
      if Assoc.Kind = Composite then
         return Assoc.Comp_Value;
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get (Assoc : Association) return String is
   begin
      if Assoc.Kind = Std then
         return To_String (Assoc.Value);
      else
         raise Constraint_Error;
      end if;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Set : in out Translate_Set) is
   begin
      Set.Ref_Count := new Integer'(1);
      Set.Set       := new Association_Map.Map;
   end Initialize;

   overriding procedure Initialize (T : in out Tag) is
   begin
      T.Ref_Count         := new Integer'(1);
      T.Data              := new Tag_Data;
      T.Data.Count        := 0;
      T.Data.Min          := Natural'Last;
      T.Data.Max          := 0;
      T.Data.Nested_Level := 1;
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert (Set : in out Translate_Set; Item : Association) is
   begin
      Set.Set.Include (To_String (Item.Variable), Item);
   end Insert;

   procedure Insert (Set : in out Translate_Set; Items : Translate_Set) is
      Pos : Association_Map.Cursor;
   begin
      if Items.Set = null then
         return;
      end if;

      Pos := Items.Set.First;

      while Association_Map.Has_Element (Pos) loop
         Insert (Set, Association_Map.Element (Pos));
         Pos := Association_Map.Next (Pos);
      end loop;
   end Insert;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (S : String) return Boolean is
      use Strings.Maps;
   begin
      return S'Length > 0
        and then Is_Subset
          (To_Set (S), Constants.Decimal_Digit_Set or To_Set ("-"));
   end Is_Number;

   ----------
   -- Item --
   ----------

   function Item (T : Tag; N : Positive) return String is
      Result : Unbounded_String;
      Found  : Boolean;
   begin
      Field (T, (1 => N), 0, Result, Found);

      if not Found then
         raise Constraint_Error;
      else
         return To_String (Result);
      end if;
   end Item;

   ----------
   -- Load --
   ----------

   function Load
     (Filename     : String;
      Cached       : Boolean := False;
      Include_File : Boolean := False) return Static_Tree
   is
      use type Definitions.NKind;

      File   : Input.File_Type;     --  file beeing parsed

      Buffer : String (1 .. 2_048); --  current line content
      Last   : Natural;             --  index of last characters read in buffer
      First  : Natural;             --  first non blank characters in buffer

      Line   : Natural := 0;

      I_File : Tree;                --  list of includes

      Error_Include_Message : Unbounded_String;
      --  Message as reported while parsing the include file

      --  Line handling

      procedure Fatal_Error (Message : String);
      pragma No_Return (Fatal_Error);
      --  raise Template_Error exception with message

      function Get_Next_Line return Boolean;
      --  Get new line in File and set Buffer, Last and First. Returns True if
      --  end of file reached.

      function Get_First_Parameter return Unbounded_String;
      --  Get first parameter in current line (second word), words being
      --  separated by a set of blank characters (space or horizontal
      --  tabulation).

      function Get_All_Parameters
        (At_Least_One : Boolean := True) return String;
      --  Get all parameters on the current line

      function Count_Tag_Attributes return Natural;
      --  Returns the number of tag attributes present

      function Get_Tag_Attribute (N : Positive) return String;
      --  Returns the Nth tag attribute

      function Get_Tag_Parameter (N : Positive) return String;
      --  Returns the Nth tag parameter found between parenthesis

      function Get_Tag_Parameter_Count return Natural;
      --  Returns the number of parameter

      function Is_Stmt
        (Stmt : String; Extended : Boolean := False) return Boolean;
      pragma Inline (Is_Stmt);
      --  Returns True if Stmt is found at the begining of the current line
      --  ignoring leading blank characters. If Extended is True it recognize
      --  statement attributes or parameter.

      function EOF return Boolean;
      pragma Inline (EOF);
      --  Returns True if the end of file has been reach

      type Parse_Mode is
        (Parse_Std,              --  in standard line
         Parse_If,               --  in a if statement
         Parse_Elsif,            --  in elsif part of a if statement
         Parse_Else,             --  in else part of a if statement
         Parse_Table,            --  in a table statement
         Parse_Block,            --  in a table block statement
         Parse_Section,          --  in new section
         Parse_Section_Content,  --  in section content
         Parse_Inline,           --  in an inline block statement
         Parse_Macro             --  in a macro definition
        );

      function Parse
        (Mode    : Parse_Mode;
         In_If   : Boolean;
         No_Read : Boolean := False) return Tree;
      --  Get a line in File and returns the Tree

      --------------------------
      -- Count_Tag_Attributes --
      --------------------------

      function Count_Tag_Attributes return Natural is
         K : constant Natural :=
               Strings.Fixed.Index (Buffer (First + 2 .. Last), "@@");
      begin
         return Strings.Fixed.Count (Buffer (First + 2 .. K), "'");
      end Count_Tag_Attributes;

      ---------
      -- EOF --
      ---------

      function EOF return Boolean is
      begin
         return Last = 0;
      end EOF;

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error (Message : String) is
      begin
         if Message (Message'Last) /= '.' then
            raise Template_Error
              with Message & ", in " & Filename
                & " at line" & Natural'Image (Line);
         else
            raise Template_Error
              with Message & ", included from " & Filename
                & " at line" & Natural'Image (Line);
         end if;
      end Fatal_Error;

      ------------------------
      -- Get_All_Parameters --
      ------------------------

      function Get_All_Parameters
        (At_Least_One : Boolean := True) return String is
         Start : Natural;
      begin
         --  Skip first word (tag statement, or include file name)

         Start := Strings.Fixed.Index (Buffer (First .. Last), Blank);

         if Start = 0 then
            if At_Least_One then
               Fatal_Error ("missing parameter");
            else
               Start := Last + 1;
            end if;
         end if;

         if Buffer (Last) = ASCII.CR then
            --  Last character is a DOS CR (certainly because the template
            --  file is in DOS format), ignore it as this is not part of the
            --  parameter.
            Last := Last - 1;
         end if;

         return Strings.Fixed.Trim (Buffer (Start .. Last), Strings.Both);
      end Get_All_Parameters;

      -------------------------
      -- Get_First_Parameter --
      -------------------------

      function Get_First_Parameter return Unbounded_String is
         Start, Stop : Natural;
      begin
         Start := Strings.Fixed.Index (Buffer (First .. Last), Blank);

         if Start = 0 then
            return Null_Unbounded_String;
         end if;

         Start := Strings.Fixed.Index (Buffer (Start .. Last), Blank, Outside);

         if Start = 0 then
            --  We have only spaces after the first word, there is no
            --  parameter in this case.
            return Null_Unbounded_String;
         end if;

         Stop  := Strings.Fixed.Index (Buffer (Start .. Last), Blank);

         if Stop = 0 then
            Stop := Last;
         else
            Stop := Stop - 1;
         end if;

         return To_Unbounded_String (Buffer (Start .. Stop));
      end Get_First_Parameter;

      -------------------
      -- Get_Next_Line --
      -------------------

      function Get_Next_Line return Boolean is
         use type Maps.Character_Set;

         Skip_End : constant Maps.Character_Set :=
                      Blank or Maps.To_Set (ASCII.CR);
      begin
         if Input.End_Of_File (File) then
            Last := 0;
            return True;

         else
            loop
               Line := Line + 1;

               Input.Get_Line (File, Buffer, Last);

               First := Strings.Fixed.Index
                 (Buffer (1 .. Last), Blank, Outside);

               exit when First + 3 > Last
                 or else (First >= Buffer'First
                          and then Buffer (First .. First + 3) /= "@@--");

               if Input.End_Of_File (File) then
                  --  We have reached the end of file, exit now
                  Last := 0;
                  return True;
               end if;
            end loop;

            if First = 0 then
               --  There is only spaces on this line, this is an empty line
               --  we just have to skip it.
               Last := 0;
               return False;
            end if;

            --  Skip ending comments

            declare
               C : Natural;
            begin
               C := Strings.Fixed.Index (Buffer (First .. Last), "@@--");

               if C /= 0 then
                  Last := C - 1;
               end if;
            end;

            Last := Strings.Fixed.Index
              (Buffer (First .. Last), Skip_End, Outside, Strings.Backward);

            return False;
         end if;
      end Get_Next_Line;

      -----------------------
      -- Get_Tag_Attribute --
      -----------------------

      function Get_Tag_Attribute (N : Positive) return String is
         S : Positive := First + 2;
         L : constant Natural :=
               Strings.Fixed.Index (Buffer (S .. Last), "@@");
         E : Natural;
      begin
         for I in 1 .. N loop
            S := Strings.Fixed.Index (Buffer (S + 1 .. L), "'");
         end loop;

         --  Check for the end of this attribute

         E := Strings.Fixed.Index (Buffer (S + 1 .. L), "'");

         if E = 0 then
            E := L;
         end if;

         return Buffer (S + 1 .. E - 1);
      end Get_Tag_Attribute;

      -----------------------
      -- Get_Tag_Parameter --
      -----------------------

      function Get_Tag_Parameter (N : Positive) return String is

         I_Last : constant Natural :=
                    Strings.Fixed.Index (Buffer (First .. Last), ")@@");

         function Find_Matching
           (From : Natural; Char : Character) return Natural;
         --  Returns the position of Char in Buffer, handle escaped characters

         -------------------
         -- Find_Matching --
         -------------------

         function Find_Matching
           (From : Natural; Char : Character) return Natural
         is
            K      : Natural := From;
            Level  : Integer;
            Escape : Integer := 0;
         begin
            if Char = ')' and then Buffer (K) = '(' then
               Level := 0;
            elsif Char = '(' then
               Level := -1;
            else
               Level := 1;
            end if;

            Look_For_Char : while K < I_Last loop
               if Buffer (K) = '\' and then Escape = 0 then
                  Escape := 2;
               elsif Escape /= 0 then
                  Escape := Escape - 1;
               end if;

               if Escape = 0 then
                  if Buffer (K) = '(' then
                     Level := Level + 1;
                  elsif Buffer (K) = ')' then
                     Level := Level - 1;
                  end if;
                  exit Look_For_Char when Buffer (K) = Char and then Level = 0;
               end if;

               K := K + 1;
            end loop Look_For_Char;

            return K;
         end Find_Matching;

         F, L : Natural;

      begin
         if I_Last = 0 then
            Fatal_Error ("No tag parameter");

         else
            F := First;
            L := First - 1;

            for K in 1 .. N loop
               F := Find_Matching (L + 1, '(');

               if F = 0 then
                  Fatal_Error ("Missing parenthesis in tag command");

               else
                  --  Look for matching closing parenthesis
                  L := Find_Matching (F, ')');

                  if Buffer (L) /= ')' then
                     Fatal_Error
                       ("Missing closing parenthesis in tag command");
                  end if;
               end if;
            end loop;

            return Buffer (F + 1 .. L - 1);
         end if;
      end Get_Tag_Parameter;

      -----------------------------
      -- Get_Tag_Parameter_Count --
      -----------------------------

      function Get_Tag_Parameter_Count return Natural is
         I_Last : constant Natural :=
                    Strings.Fixed.Index (Buffer (First .. Last), ")@@");
         Count  : Natural := 0;
         Level  : Natural := 0;
         Escape : Integer := 0;
      begin
         if I_Last = 0 then
            return 0;

         else
            for K in First .. I_Last loop
               if Buffer (K) = '\' and then Escape = 0 then
                  Escape := 2;
               elsif Escape /= 0 then
                  Escape := Escape - 1;
               end if;

               if Escape = 0 then
                  if Buffer (K) = '(' then
                     if Level = 0 then
                        Count := Count + 1;
                     end if;
                     Level := Level + 1;
                  elsif Buffer (K) = ')' then
                     Level := Level - 1;
                  end if;
               end if;
            end loop;

            return Count;
         end if;
      end Get_Tag_Parameter_Count;

      -------------
      -- Is_Stmt --
      -------------

      function Is_Stmt
        (Stmt : String; Extended : Boolean := False) return Boolean
      is
         Offset : Natural := 0;
      begin
         if Extended then
            Offset := 2;
         end if;

         return Last /= 0
           and then Buffer (First .. First + Stmt'Length - 1 - Offset) =
                      Stmt (Stmt'First .. Stmt'Last - Offset)
           and then (not Extended
                     or else
                       (Buffer (First + Stmt'Length - Offset) = '''
                        or else Buffer (First + Stmt'Length - Offset) = '('
                        or else Buffer (First + Stmt'Length - Offset) = '@'));
      end Is_Stmt;

      -----------
      -- Parse --
      -----------

      function Parse
        (Mode    : Parse_Mode;
         In_If   : Boolean;
         No_Read : Boolean := False) return Tree
      is
         use type Data.NKind;
         use type Data.Tree;

         function Count_Sections (T : Tree) return Natural;
         pragma Inline (Count_Sections);
         --  Returns the number of sections in T (Section_Stmt)

         function Count_Blocks (T : Tree) return Natural;
         pragma Inline (Count_Blocks);
         --  Returns the number of sections in T (Table_Stmt)

         procedure Rewrite_Inlined_Block (T : Tree; Sep : String);
         --  Recursive procedure that rewrite all text nodes in an inlined
         --  block. In such a block the spaces before and after text are
         --  meaningless and LF are replaced by the given separator.

         ------------------
         -- Count_Blocks --
         ------------------

         function Count_Blocks (T : Tree) return Natural is
            C : Natural := 0;
            S : Tree    := T;
         begin
            while S /= null loop
               C := C + 1;
               S := S.Next;
            end loop;
            return C;
         end Count_Blocks;

         --------------------
         -- Count_Sections --
         --------------------

         function Count_Sections (T : Tree) return Natural is
            C : Natural := 0;
            S : Tree    := T;
         begin
            while S /= null loop
               C := C + 1;
               S := S.N_Section;
            end loop;
            return C;
         end Count_Sections;

         ---------------------------
         -- Rewrite_Inlined_Block --
         ---------------------------

         procedure Rewrite_Inlined_Block (T : Tree; Sep : String) is

            procedure Rewrite (T : Tree; Last, In_Table : Boolean);
            --  Last is set to True if we are checking the last node

            -------------
            -- Rewrite --
            -------------

            procedure Rewrite (T : Tree; Last, In_Table : Boolean) is
               N : Tree := T;
               D : Data.Tree;
            begin
               while N /= null loop
                  case N.Kind is
                     when Text =>
                        D := N.Text;

                        --  Trim leading blanks

                        if D /= null and then D.Kind = Data.Text then
                           Trim (D.Value, Side => Left);
                        end if;

                        while D /= null loop
                           case D.Kind is
                              when Data.Text =>
                                 declare
                                    Len : constant Natural := Length (D.Value);
                                    --  Len can be 0 here because of the
                                    --  trimming above.
                                 begin
                                    if Len /= 0
                                      and then
                                        Element (D.Value, Len) = ASCII.LF
                                      and then
                                        (not Last
                                         or else N.Next /= null
                                         or else D.Next /= null
                                         or else In_Table)
                                    then
                                       Delete (D.Value, Len, Len);

                                       if not In_Table then
                                          --  Inside a table we do no want to
                                          --  add the separator, this must be
                                          --  done during the rendering as we
                                          --  need to have the actual vector
                                          --  values.
                                          Append (D.Value, Sep);
                                       end if;
                                    end if;
                                 end;

                              when Data.Var  =>
                                 null;
                           end case;

                           D := D.Next;
                        end loop;

                     when If_Stmt =>
                        Rewrite (N.N_True, N.Next = null, In_Table);
                        Rewrite (N.N_False, N.Next = null, In_Table);

                     when Table_Stmt =>
                        Rewrite (N.Blocks, N.Next = null, In_Table => True);

                     when Section_Block =>
                        Rewrite (N.Common, Last, In_Table);
                        Rewrite (N.Sections, Last, In_Table);

                     when Section_Stmt =>
                        Rewrite (N.N_Section, Last, In_Table);

                     when others =>
                        null;
                  end case;

                  N := N.Next;
               end loop;
            end Rewrite;

         begin
            Rewrite (T, Last => True, In_Table => False);
         end Rewrite_Inlined_Block;

         T : Tree;

      begin
         if not No_Read
           and then Mode /= Parse_Section
           and then Mode /= Parse_Elsif
           and then Mode /= Parse_Block
         then
            if Get_Next_Line then
               return null;
            end if;
         end if;

         --  Check for Utf8 BOM, this can only occurs at the first line

         if Line = 1
           and then Last > Utils.BOM_Utf8'Length
           and then Buffer (1 .. Utils.BOM_Utf8'Length) = Utils.BOM_Utf8
         then
            T := new Node (Text);
            T.Line := Line;
            T.Text := Data.Parse (Buffer (1 .. Utils.BOM_Utf8'Length));

            --  Removes BOM from buffer

            Buffer (1 .. Last - Utils.BOM_Utf8'Length) :=
              Buffer (1 + Utils.BOM_Utf8'Length .. Last);
            Last := Last - Utils.BOM_Utf8'Length;

            --  Continued parsing the remaining of the line

            T.Next := Parse (Mode, In_If, No_Read => True);
            return T;
         end if;

         case Mode is
            when Parse_Std =>
               if Is_Stmt (End_If_Token) then
                  Fatal_Error
                    ("@@END_IF@@ found outside an @@IF@@ statement");
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error
                    ("@@END_TABLE@@ found outside a @@TABLE@@ statement");
               end if;

               if Is_Stmt (End_Inline_Token) then
                  Fatal_Error
                    ("@@END_INLINE@@ found outside an @@INLINE@@ statement");
               end if;

               if Is_Stmt (End_Token) then
                  Fatal_Error
                    ("@@END@@ found outside a @@BEGIN@@ block statement");
               end if;

               if Is_Stmt (End_Macro_Token) then
                  Fatal_Error
                    ("@@END_MACRO@@ found outside a @@MACRO@@ statement");
               end if;

            when Parse_If =>
               if Is_Stmt (Else_Token)
                 or else Is_Stmt (Elsif_Token)
                 or else Is_Stmt (End_If_Token)
               then
                  return null;
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error ("@@END_TABLE@@ found, @@END_IF@@ expected");
               end if;

            when Parse_Elsif =>
               if Is_Stmt (Else_Token)
                 or else Is_Stmt (End_If_Token)
               then
                  return null;
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error ("@@END_TABLE@@ found, @@END_IF@@ expected");
               end if;

            when Parse_Else  =>
               if Is_Stmt (End_If_Token) then
                  return null;
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error ("@@END_TABLE@@ found, @@END_IF@@ expected");
               end if;

               if Is_Stmt (Elsif_Token) then
                  Fatal_Error ("@@ELSIF@@ found after @@ELSE@@");
               end if;

            when Parse_Block =>
               if Is_Stmt (End_Table_Token) then
                  return null;
               end if;

               T := new Node (Section_Block);

               T.Line := Line;

               declare
                  Tmp : Tree;
               begin
                  Tmp := Parse (Parse_Section, In_If);

                  if Tmp = null then
                     --  This section is empty
                     return null;
                  end if;

                  if Is_Stmt (Begin_Token) then
                     --  It means that the section parsed above was common
                     T.Common   := Tmp.Next;

                     --  Now free the Section_Stmt container

                     Unchecked_Free (Tmp);

                     T.Sections := Parse (Parse_Section, In_If);

                  else
                     T.Common   := null;
                     T.Sections := Tmp;
                  end if;
               end;

               --  Count the number of section

               T.Sections_Count := Count_Sections (T.Sections);

               if T.Sections_Count = 1 and then T.Common = null then
                  --  A single section and no common section, rewrite it as a
                  --  simple common section.
                  T.Common := T.Sections.Next;
                  Unchecked_Free (T.Sections);
                  T.Sections_Count := 0;
               end if;

               if Is_Stmt (End_Table_Token) then
                  T.Next := null;
               else
                  T.Next := Parse (Parse_Block, In_If);
               end if;

               return T;

            when Parse_Section =>
               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

               T := new Node (Section_Stmt);

               T.Line := Line;

               T.Next := Parse (Parse_Section_Content, In_If);

               if Is_Stmt (End_Table_Token) and then T.Next = null then
                  --  Check if this section was empty, this happen when
                  --  we parse a section after @@END@@ followed by the end
                  --  of the table.
                  Unchecked_Free (T);
                  return null;
               end if;

               if Is_Stmt (End_Table_Token)
                 or else Is_Stmt (Begin_Token)
                 or else Is_Stmt (End_Token)
               then
                  T.N_Section := null;

               elsif EOF then
                  Fatal_Error ("EOF found, @@END_TABLE@@ expected");

               else
                  T.N_Section := Parse (Parse_Section, In_If);
               end if;

               return T;

            when Parse_Section_Content =>
               if Is_Stmt (Section_Token)
                 or else Is_Stmt (End_Table_Token)
                 or else Is_Stmt (Begin_Token)
                 or else Is_Stmt (End_Token)
               then
                  return null;
               end if;

               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

            when Parse_Table =>
               if Is_Stmt (End_Table_Token) then
                  return null;
               end if;

               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

            when Parse_Inline =>
               if Is_Stmt (End_Inline_Token) then
                  return null;
               end if;

            when Parse_Macro =>
               if Is_Stmt (End_Macro_Token) then
                  return null;
               end if;

               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_MACRO@@ expected");
               end if;
         end case;

         if Is_Stmt (If_Token) or else Is_Stmt (Elsif_Token) then
            T := new Node (If_Stmt);

            T.Line := Line;

            T.Cond   := Expr.Parse (Get_All_Parameters);
            T.N_True := Parse (Parse_If, In_If => True);

            if Is_Stmt (End_If_Token) then
               T.N_False := null;

            elsif Is_Stmt (Elsif_Token) then
               T.N_False := Parse (Parse_Elsif, In_If => True);

            elsif EOF then
               Fatal_Error ("EOF found, @@END_IF@@ expected");

            else
               T.N_False := Parse (Parse_Else, In_If => True);
            end if;

            T.Next := Parse (Mode, In_If);

            return T;

         elsif Is_Stmt (Table_Token, Extended => True) then
            T := new Node (Table_Stmt);

            T.Line := Line;

            T.Terminate_Sections := False;
            T.Reverse_Index      := False;
            T.Terse              := False;

            --  Check if first parameter is @@TERMINATE_SECTION@@, note that
            --  this is an obsolescent feature. It is better now to use the
            --  tag command attributes.

            declare
               Param : constant Unbounded_String := Get_First_Parameter;
            begin
               if Param = Null_Unbounded_String then
                  T.Terminate_Sections := False;
               elsif Param = Terminate_Sections_Token then
                  T.Terminate_Sections := True;
               else
                  Fatal_Error ("Unknown table parameter " & To_String (Param));
               end if;
            end;

            --  Check attributes

            for K in 1 .. Count_Tag_Attributes loop
               declare
                  Att : constant String := Get_Tag_Attribute (K);
               begin
                  if Att = A_Terminate_Sections_Token then
                     T.Terminate_Sections := True;

                  elsif Att = A_Reverse_Token then
                     T.Reverse_Index := True;

                  elsif Att = A_Terse_Token then
                     T.Terse := True;
                  else
                     Fatal_Error ("Unknown table attributes " & Att);
                  end if;
               end;
            end loop;

            T.Blocks       := Parse (Parse_Block, In_If);
            T.Next         := Parse (Mode, In_If);
            T.Blocks_Count := Count_Blocks (T.Blocks);

            --  Check now that if we have TERMINATE_SECTIONS option set and
            --  that there is more than one block, all blocks have the same
            --  number of section.

            if T.Terminate_Sections and then T.Blocks_Count >= 1 then
               declare
                  Size : constant Natural := T.Blocks.Sections_Count;
                  Max  : Natural := Size;
                  B    : Tree    := T.Blocks.Next;
               begin
                  while B /= null loop
                     Max := Natural'Max (Max, B.Sections_Count);

                     if B.Sections_Count /= Size
                       and then B.Sections_Count /= 0
                     then
                        Fatal_Error
                          ("All sections must have the same size "
                           & "when using TERMINATE_SECTIONS option");
                     end if;
                     B := B.Next;
                  end loop;

                  --  Check wether we have sections with the TERMINATE_SECTION
                  --  attribute.

                  if Max = 0 then
                     Fatal_Error
                       ("TERMINATE_SECTIONS attribute given, but no section"
                        & " defined");
                  end if;
               end;
            end if;

            return T;

         elsif Is_Stmt (Include_Token) then
            T := new Node (Include_Stmt);

            T.Line := Line;

            declare
               File : constant String := To_String (Get_First_Parameter);
            begin
               T.I_Filename := Data.Parse (File);

               if T.I_Filename.Kind = Data.Text
                 and then T.I_Filename.Next = null
               then
                  --  In the case of static strings we load the include file
                  --  now.
                  declare
                     I_Filename : constant String :=
                                    Build_Include_Pathname (Filename, File);
                  begin
                     T.File := Load (I_Filename, Cached, True);
                  exception
                     when IO_Exceptions.Name_Error =>
                        --  File not found, this is an error only if we are not
                        --  inside a conditional.
                        if not In_If then
                           Error_Include_Message :=
                             To_Unbounded_String
                               ("Include file " & I_Filename & " not found.");
                           Unchecked_Free (T);
                           return null;
                        end if;

                     when E : others =>
                        --  Error while parsing the include file, record this
                        --  error. Let the parser exit properly from the
                        --  recursion to be able to release properly the memory
                        --  before raising an exception.

                        Error_Include_Message :=
                          To_Unbounded_String (Exception_Message (E));

                        Unchecked_Free (T);
                        return null;
                  end;

                  --  We do not need to keep reference to the include file in
                  --  this case. The filename is static and already loaded.
                  Data.Release (T.I_Filename);
               end if;

               --  Move past @@INCLUDE@@

               First := First + 11;

               while First < Last and then Buffer (First) = ' ' loop
                  First := First + 1;
               end loop;

               declare
                  P_Set : constant Parameter_Set :=
                            (0 => To_Unbounded_String (File))
                            & Get_Parameters
                                (Get_All_Parameters (At_Least_One => False));
               begin
                  T.I_Params := Data.To_Data_Parameters (P_Set);
               end;
            end;

            I_File := new Node'
              (Include_Stmt, I_File, Line, T.File, T.I_Filename, T.I_Params);

            T.Next := Parse (Mode, In_If);

            return T;

         elsif Is_Stmt (Macro_Token, Extended => True) then
            --  Parse a macro definition and register it

            declare
               Name : constant String := Get_Tag_Parameter (1);
               T    : constant Tree := Parse (Parse_Macro, In_If);

               procedure Move_To_Last (T : in out Tree);
               --  Move to last node

               procedure Rewrite (T : in out Tree);
               --  Rewrite this node, this is used to remove all CR/LF for
               --  the last lines which could be output for this tree.

               ------------------
               -- Move_To_Last --
               ------------------

               procedure Move_To_Last (T : in out Tree) is
               begin
                  while T.Next /= null loop
                     T := T.Next;
                  end loop;
               end Move_To_Last;

               -------------
               -- Rewrite --
               -------------

               procedure Rewrite (T : in out Tree) is
                  D : Data.Tree;
               begin
                  if T /= null then
                     Move_To_Last (T);

                     case T.Kind is
                        when Text =>
                           --  A text node

                           D := T.Text;

                           --  Move to the end of this line

                           while D.Next /= null loop
                              D := D.Next;
                           end loop;

                           if D.Kind = Data.Text then
                              Strings.Unbounded.Trim
                                (D.Value,
                                 Left  => Maps.Null_Set,
                                 Right => Maps.To_Set (ASCII.CR & ASCII.LF));
                           end if;

                        when If_Stmt =>
                           Rewrite (T.N_True);
                           Rewrite (T.N_False);

                        when others =>
                           null;
                     end case;
                  end if;
               end Rewrite;

               N : Tree := T;
            begin
               --  We want to trim CR/LF from the last text node

               Rewrite (N);

               Macro.Register (Name, T);
            end;

            --  Then continue parsing the remaining of the file

            return Parse (Mode, In_If);

         elsif Is_Stmt (Set_Token) then
            --  We want to handle multiple SET lines to avoid deep recursion

            declare
               Root, N : Tree;
            begin
               loop
                  N := new Node (Set_Stmt);

                  if Root = null then
                     Root := N;
                  else
                     T.Next := N;
                  end if;

                  T      := N;
                  T.Line := Line;
                  T.Def  := Definitions.Parse (Get_All_Parameters);

                  if Get_Next_Line then
                     --  Nothing more, returns the result now
                     return Root;
                  end if;

                  --  If this is not a SET statement, just call the parsing
                  --  routine and return the root note.

                  if not Is_Stmt (Set_Token) then
                     T.Next := Parse (Mode, In_If, No_Read => True);
                     return Root;
                  end if;
               end loop;
            end;

         elsif Is_Stmt (Inline_Token, Extended => True) then
            T := new Node (Inline_Stmt);

            declare
               function Inline_Parameter
                 (Index : Positive) return Unbounded_String;
               --  Returns Inline_Parameter with the given index

               ----------------------
               -- Inline_Parameter --
               ----------------------

               function Inline_Parameter
                 (Index : Positive) return Unbounded_String
               is
                  P : constant String := Get_Tag_Parameter (Index);
                  N : Natural := P'First;
                  R : String (P'Range);
                  K : Natural := R'First - 1;
               begin
                  while N <= P'Last loop
                     if P (N) = '\' and then N < P'Last then
                        case P (N + 1) is
                           when '\' =>
                              K := K + 1;
                              R (K) := '\';
                              N := N + 1;

                           when 'n' =>
                              if Utils.Is_Windows then
                                 K := K + 2;
                                 R (K - 1 .. K) := (ASCII.CR, ASCII.LF);
                              else
                                 K := K + 1;
                                 R (K) := ASCII.LF;
                              end if;
                              N := N + 1;

                           when 'r' =>
                              K := K + 1;
                              R (K) := ASCII.LF;
                              N := N + 1;

                           when others =>
                              K := K + 1;
                              R (K) := P (N + 1);
                              N := N + 1;
                        end case;

                     else
                        K := K + 1;
                        R (K) := P (N);
                     end if;

                     N := N + 1;
                  end loop;

                  return To_Unbounded_String (R (R'First .. K));
               end Inline_Parameter;

            begin
               case Get_Tag_Parameter_Count is
                  when 0 =>
                     T.Sep    := To_Unbounded_String (" ");
                  when 1 =>
                     T.Sep    := Inline_Parameter (1);
                  when 3 =>
                     T.Before := Inline_Parameter (1);
                     T.Sep    := Inline_Parameter (2);
                     T.After  := Inline_Parameter (3);
                  when others =>
                     Fatal_Error
                       ("Wrong number of tag parameters for INLINE "
                        & "command (0, 1 or 3)");
               end case;
            end;

            T.I_Block := Parse (Parse_Inline, In_If);

            --  Now we have parsed the full tree to inline. Rewrite this tree
            --  to replace all text node by a stripped version.

            Rewrite_Inlined_Block (T.I_Block, To_String (T.Sep));

            T.Next := Parse (Mode, In_If);

            return T;

         else
            declare
               Root, N : Tree;
            begin
               loop
                  N := new Node (Text);

                  if Root = null then
                     Root := N;
                  else
                     T.Next := N;
                  end if;

                  T      := N;
                  T.Line := Line;

                  if Input.LF_Terminated (File)
                    and then (not Input.End_Of_File (File)
                                or else Include_File)
                  then
                     --  Add a LF if the read line is terminated by a LF. Do
                     --  not add this LF if we reach the end of file except for
                     --  included files.

                     T.Text := Data.Parse (Buffer (1 .. Last) & ASCII.LF);
                  else
                     T.Text := Data.Parse (Buffer (1 .. Last));
                  end if;

                  if Get_Next_Line then
                     --  Nothing more, returns the result now
                     return Root;
                  end if;

                  --  If this is a statement just call the parsing routine

                  if Is_Stmt (If_Token)
                    or else Is_Stmt (Elsif_Token)
                    or else Is_Stmt (Else_Token)
                    or else Is_Stmt (End_If_Token)
                    or else Is_Stmt (Include_Token)
                    or else Is_Stmt (Table_Token, Extended => True)
                    or else Is_Stmt (Section_Token)
                    or else Is_Stmt (End_Table_Token)
                    or else Is_Stmt (Begin_Token)
                    or else Is_Stmt (End_Token)
                    or else Is_Stmt (Set_Token)
                    or else Is_Stmt (Inline_Token, Extended => True)
                    or else Is_Stmt (End_Inline_Token)
                    or else Is_Stmt (Macro_Token, Extended => True)
                    or else Is_Stmt (End_Macro_Token)
                  then
                     T.Next := Parse (Mode, In_If, No_Read => True);
                     return Root;
                  end if;
               end loop;
            end;
         end if;
      end Parse;

      T     : Static_Tree;
      New_T : Tree;
      Old   : Tree;

   begin
      Templates_Parser_Tasking.Lock;

      if Cached then
         Cached_Files.Get (Filename, Result => T);

         if T /= Null_Static_Tree then
            pragma Assert (T.C_Info /= null);
            Templates_Parser_Tasking.Unlock;
            return T;
         end if;
      end if;

      Input.Open (File, Filename, Form => "shared=no");

      begin
         New_T := Parse (Parse_Std, False);

         Simplifier.Run (New_T);

         Input.Close (File);
      exception
         when others =>
            Input.Close (File);
            raise;
      end;

      --  T is the tree file, add two nodes (Info and C_Info) in front of
      --  the tree.

      --  Add second node (cache info)

      Old := new Node'(Kind     => C_Info,
                       Next     => New_T,
                       Line     => 0,
                       Obsolete => False,
                       Used     => 1);

      --  Add first node (info about tree)

      New_T := new Node'(Kind      => Info,
                         Next      => Old,
                         Line      => 0,
                         Filename  => To_Unbounded_String (Filename),
                         Timestamp => Configuration.File_Time_Stamp (Filename),
                         I_File    => I_File);

      if Error_Include_Message /= Null_Unbounded_String then
         --  An include filename was not found, release the memory now and
         --  raise a fatal error.

         Release (New_T);

         Fatal_Error (To_String (Error_Include_Message));
      end if;

      if Cached then
         Cached_Files.Add (Filename, New_T, Old);
         pragma Assert (Old /= null);
      end if;

      Templates_Parser_Tasking.Unlock;
      return Static_Tree'(New_T, Old);

   exception
      when E : Internal_Error =>
         Templates_Parser_Tasking.Unlock;
         Fatal_Error (Exceptions.Exception_Message (E));
      when others =>
         Templates_Parser_Tasking.Unlock;
         raise;
   end Load;

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Parameters : String) return Parameter_Set
   is
      function Get (Count : Natural) return Parameter_Set;
      --  Load the Count parameters from Parameters string

      function Next (Char : Character; From : Positive) return Natural;
      --  Returns the position of the next character Char starting from
      --  position From. Returns 0 if the character is not found. This routine
      --  skips characters inside quoted string.

      ---------
      -- Get --
      ---------

      function Get (Count : Natural) return Parameter_Set is

         procedure Get_Named_Parameters (Parameters : String);
         --  Load parameters specified with a name:
         --  (param_a, 5 => param_b, 3 => param_c)
         --  Set Result variable accordingly.

         procedure Get_Next_Parameter
           (Parameters : String;
            First      : in out Positive;
            Last       :    out Natural;
            Next_Last  :    out Natural);
         --  Look for next parameter starting at position First, set First and
         --  Last to the index of this parameter. Next_Last is set to the next
         --  value to assigned to last.

         Result      : Parameter_Set (1 .. Count);
         Index       : Positive := Result'First;
         First, Last : Natural := 0;
         Next_Last   : Natural;

         ------------------------
         -- Get_Next_Parameter --
         ------------------------

         procedure Get_Next_Parameter
           (Parameters : String;
            First      : in out Positive;
            Last       :    out Natural;
            Next_Last  :    out Natural) is
         begin
            --  Skip blanks

            while First < Parameters'Last
              and then (Parameters (First) = ' '
                        or else Parameters (First) = ASCII.HT)
            loop
               First := First + 1;
            end loop;

            --  Look for end of parameter

            Next_Last := First + 1;

            if Parameters (First) = '"' then
               --  Look for closing quote
               while Next_Last < Parameters'Last
                 and then Parameters (Next_Last) /= '"'
               loop
                  Next_Last := Next_Last + 1;
               end loop;

               if Parameters (Next_Last) /= '"' then
                  raise Internal_Error
                    with "Missing closing quote in include parameters";
               end if;

               --  Skip quotes

               First := First + 1;
               Last := Next_Last - 1;

            else
               --  Look for end of word

               while Next_Last < Parameters'Last
                 and then Parameters (Next_Last) /= ' '
                 and then Parameters (Next_Last) /= ASCII.HT
               loop
                  Next_Last := Next_Last + 1;
               end loop;

               if Next_Last /= Parameters'Last then
                  Last := Next_Last - 1;
               else
                  Last := Next_Last;
               end if;
            end if;
         end Get_Next_Parameter;

         --------------------------
         -- Get_Named_Parameters --
         --------------------------

         procedure Get_Named_Parameters (Parameters : String) is

            procedure Parse (Parameter : String);
            --  Parse one parameter

            Named       : Boolean := False;
            First, Last : Natural;

            -----------
            -- Parse --
            -----------

            procedure Parse (Parameter : String) is
               use type Data.Tree;
               Sep : constant Natural := Strings.Fixed.Index (Parameter, "=>");
               Ind : Natural;
            begin
               if Sep = 0 then
                  --  A positional parameter, this is valid only if we have not
                  --  yet found a named parameter.

                  if Named then
                     raise Internal_Error with
                       "Can't have a positional parameter after a named one";
                  else
                     Result (Index) := To_Unbounded_String (Parameter);
                     Index := Index + 1;
                  end if;

               else
                  --  A named parameter, get index
                  Named := True;

                  declare
                     Ind_Str     : constant String :=
                                     Strings.Fixed.Trim
                                       (Parameter (Parameter'First .. Sep - 1),
                                        Strings.Both);
                     First, Last : Natural;
                     Next_Last   : Natural;
                     pragma Unreferenced (Next_Last);
                  begin
                     if Is_Number (Ind_Str) then
                        Ind := Natural'Value (Ind_Str);

                        if Result (Ind) = Null_Unbounded_String then
                           --  This parameter has not yet been found

                           First := Sep + 2;

                           Get_Next_Parameter
                             (Parameter, First, Last, Next_Last);

                           Result (Ind) :=
                             To_Unbounded_String (Parameter (First .. Last));

                        else
                           raise Internal_Error with
                             "Parameter" & Natural'Image (Ind)
                             & " defined multiple time";
                        end if;

                     else
                        raise Internal_Error
                          with "Wrong number in named parameter";
                     end if;
                  end;
               end if;
            end Parse;

         begin
            if Parameters (Parameters'Last) /= ')' then
               raise Internal_Error with
                 "Missing closing parenthesis in named include parameters";
            end if;

            First := Parameters'First + 1;
            --  Skip the parenthesis

            loop
               Last := Strings.Fixed.Index
                 (Parameters (First .. Parameters'Last), ",");
               exit when Last = 0;

               Parse
                 (Strings.Fixed.Trim
                    (Parameters (First .. Last - 1), Strings.Both));
               First := Last + 1;
            end loop;

            --  Handle last parameter

            Parse
              (Strings.Fixed.Trim
                 (Parameters (First .. Parameters'Last - 1), Strings.Both));
         end Get_Named_Parameters;

      begin
         First := Parameters'First;

         while First <= Parameters'Last loop
            --  Skip blanks

            while First < Parameters'Last
              and then (Parameters (First) = ' '
                        or else Parameters (First) = ASCII.HT)
            loop
               First := First + 1;
            end loop;

            --  Check if parameters are specified with a name

            if Index = 1 and then Parameters (First) = '(' then
               --  Stop current processing, load as named parameters
               Get_Named_Parameters (Parameters (First .. Parameters'Last));
               return Result;
            end if;

            Get_Next_Parameter (Parameters, First, Last, Next_Last);

            Result (Index) := To_Unbounded_String (Parameters (First .. Last));
            Index := Index + 1;

            Last := Next_Last;
            First := Last + 1;
         end loop;

         return Result;
      end Get;

      ----------
      -- Next --
      ----------

      function Next (Char : Character; From : Positive) return Natural is
         In_Quote : Boolean := False;
         Index    : Natural := 0;
      begin
         for K in From .. Parameters'Last loop
            if Parameters (K) = '"' then
               In_Quote := not In_Quote;

            elsif Parameters (K) = Char and then not In_Quote then
               Index := K;
               exit;
            end if;
         end loop;
         return Index;
      end Next;

      Count : Natural := 0;
      Index : Natural := Parameters'First - 1;

   begin
      --  Count parameters

      if Parameters'First <= Parameters'Last then
         Count := 1;

         if Parameters (Parameters'First) = '(' then
            --  We are using the parentherized form with parameters separated
            --  with coma and possibly using the named notation.

            --  Count positional parameters

            loop
               Index := Next (',', Index + 1);
               exit when Index = 0;
               Count := Count + 1;
            end loop;

            --  Then check for named ones

            declare
               SP, EP : Natural;
               Sep    : Natural := Parameters'First;
            begin
               loop
                  Sep := Strings.Fixed.Index (Parameters, "=>", From => Sep);
                  exit when Sep = 0;

                  EP := Sep - 1;

                  --  Skip spaces

                  while EP > Parameters'First
                    and then Parameters (EP) = ' '
                  loop
                     EP := EP - 1;
                  end loop;

                  SP := EP;

                  --  Get number

                  while SP > Parameters'First
                    and then Strings.Maps.Is_In
                      (Parameters (SP - 1),
                       Strings.Maps.Constants.Decimal_Digit_Set)
                  loop
                     SP := SP - 1;
                  end loop;

                  if Parameters (EP) in '0' .. '9' then
                     Count := Natural'Max
                       (Count, Natural'Value (Parameters (SP .. EP)));
                  end if;

                  Sep := Sep + 1;
               end loop;
            end;

         else
            --  We are using the standard form, parameters are separated by
            --  spaces.

            loop
               Index := Next (' ', Index + 1);
               exit when Index = 0;

               --  Skip multiple spaces

               while Index < Parameters'Last
                 and then Parameters (Index + 1) = ' '
               loop
                  Index := Index + 1;
               end loop;

               Count := Count + 1;
            end loop;
         end if;
      end if;

      return Get (Count);
   end Get_Parameters;

   --------------
   -- No_Quote --
   --------------

   function No_Quote (Str : String) return String is
   begin
      if Str'Length > 1
        and then Str (Str'First) = '"'
        and then Str (Str'Last) = '"'
      then
         return Str (Str'First + 1 .. Str'Last - 1);
      else
         return Str;
      end if;
   end No_Quote;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filename          : String;
      Translations      : Translate_Table       := No_Translation;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return String is
   begin
      return To_String
        (Parse (Filename, Translations, Cached,
                Keep_Unknown_Tags, Lazy_Tag, Cursor_Tag));
   end Parse;

   function Parse
     (Filename          : String;
      Translations      : Translate_Table       := No_Translation;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return Unbounded_String is
   begin
      return Parse
        (Filename, To_Set (Translations), Cached,
         Keep_Unknown_Tags, Lazy_Tag, Cursor_Tag);
   end Parse;

   function Parse
     (Filename          : String;
      Translations      : Translate_Set;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return String is
   begin
      return To_String
        (Parse (Filename, Translations, Cached,
                Keep_Unknown_Tags, Lazy_Tag, Cursor_Tag));
   end Parse;

   function Parse
     (Filename          : String;
      Translations      : Translate_Set;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return Unbounded_String
   is
      Max_Nested_Levels : constant := 10;
      --  The maximum number of table nested levels

      type Block_State is record
         Section_Number : Positive;
         Section        : Tree;
      end record;

      Empty_Block_State : constant Block_State := (1, null);

      type Parse_State;
      type Parse_State_Access is access constant Parse_State;

      type Parse_State (P_Size : Natural) is record
         Cursor        : Indices (1 .. Max_Nested_Levels);
         Max_Lines     : Natural;
         Max_Expand    : Natural;
         Reverse_Index : Boolean;
         Table_Level   : Natural;
         Inline_Sep    : Unbounded_String;
         Filename      : Unbounded_String;
         Blocks_Count  : Natural;
         I_Params      : Data.Parameters;
         F_Params      : Parameter_Set (1 .. P_Size);
         Block         : Block_State;
         Terse_Table   : Boolean;
         Parent        : Parse_State_Access;
      end record;

      Empty_State : constant Parse_State :=
                      (0, (1 .. Max_Nested_Levels => 0), 0, 0, False, 0,
                       Null_Unbounded_String, Null_Unbounded_String, 0,
                       null, No_Parameter, Empty_Block_State, False, null);

      Results : Unbounded_String := Null_Unbounded_String;

      Buffer  : String (1 .. 4 * 1_024);
      Last    : Natural := 0;
      --  Cache to avoid too many reallocation using Append on Results above

      Last_Was_Sep : Boolean := False;
      --  Used to avoid two consecutive separators

      Now      : Calendar.Time;
      D_Map    : Definitions.Map;
      Lazy_Set : Translate_Set;
      Output   : Boolean;

      procedure Analyze
        (T     : Tree;
         State : Parse_State);
      --  Parse T and build results file. State is needed for Vector_Tag and
      --  Matrix_Tag expansion.

      function Get_Association (Var : Data.Tag_Var) return Association;
      --  Returns association for Name or Null_Association if not found. This
      --  routine also handles lazy tags by calling the appropriate callback
      --  routine. Lazy tag values are then recorded into Lazy_Set.

      -------------
      -- Analyze --
      -------------

      procedure Analyze
        (T     : Tree;
         State : Parse_State)
      is
         use type Association_Map.Cursor;
         use type Data.Tree;

         function Analyze (E : Expr.Tree) return String;
         --  Analyse the expression tree and returns the result as a boolean
         --  The conditional expression must be equal to either TRUE or
         --  FALSE. Note that a string is True if it is equal to string "TRUE"
         --  and False otherwise.

         procedure Analyze (D : Data.Tree);
         --  Analyse the data tree and replace all variables by the
         --  correspinding value specified in Translations. This procedure
         --  catenate the result into Results variable.

         procedure Get_Max
           (T          : Tree;
            Max_Lines  :    out Natural;
            Max_Expand :    out Natural);
         --  Returns the maximum number of lines (Max_Lines) into the
         --  table. This correspond to the length of the shortest vector tag
         --  into the table or the shortest number of lines in sub-table
         --  matrix tag.
         --  Returns also the number of time the table will be expanded
         --  (Max_Expand), this is equal to Max_Lines + offset to terminate
         --  the sections.

         function Translate
           (Var          : Data.Tag_Var;
            State        : Parse_State;
            Is_Composite : access Boolean) return String;
         --  Translate Tag variable using Translation table and apply all
         --  Filters and Atribute recorded for this variable.

         function I_Translate
           (Var : Data.Tag_Var; State : Parse_State) return String;
         --  As above but for an include variable

         procedure Add (S : String; Sep : Boolean := False);
         --  Add S into Results (using Buffer cache if possible). If Sep is
         --  true S is a separator. We keep track of this as we do not want to
         --  have two separators side by side.

         procedure Flush;
         pragma Inline (Flush);
         --  Flush buffer to Results

         function Get_Mark return Natural;
         pragma Inline (Get_Mark);
         --  Get a mark on the current text buffer

         function Get_Marked_Text (Mark : Natural) return String;
         --  Returns the text from the mark to the end of the buffer

         procedure Rollback (Activate : Boolean; Mark : Natural);
         pragma Inline (Rollback);
         --  Commit or rollback added texts for terse output. If no text added
         --  from the vector tag we rollback to the previous mark otherwise the
         --  current result stays. The mark is cleared.

         function Flatten_Parameters
           (I : Data.Parameter_Set) return Parameter_Set;
         --  Returns a flat representation of the include parameters, only the
         --  name or the value are kept. The tree are replaced by an empty
         --  value.

         function Flatten_Parameters
           (I : Data.Parameters) return Parameter_Set;
         --  As above but for an access to a Parameter_Set

         function Inline_Cursor_Tag
           (Cursor_Tag : Dynamic.Cursor_Tag_Access;
            Var_Name   : String;
            Dim        : Positive;
            Path       : Dynamic.Path) return Unbounded_String;
         --  Returns the Cursor_Tag Var_Name inlined for all dimensions
         --  starting from Path.

         procedure Push_Sep (State : Parse_State);
         pragma Inline (Push_Sep);
         --  Append a separator into the current buffer

         procedure Pop_Sep (State : Parse_State);
         pragma Inline (Pop_Sep);
         --  Remove the separator if it is the last input into the buffer

         L_State : aliased constant Parse_State := State;

         ---------
         -- Add --
         ---------

         procedure Add (S : String; Sep : Boolean := False) is
         begin
            if Last + S'Length > Buffer'Last then
               --  Not enough cache space, flush buffer
               Flush;
            end if;

            if S'Length >= Buffer'Length then
               Append (Results, S);
            else
               Buffer (Last + 1 .. Last + S'Length) := S;
               Last := Last + S'Length;
            end if;

            Last_Was_Sep := Sep;
         end Add;

         -------------
         -- Analyze --
         -------------

         procedure Analyze (D : Data.Tree) is
            T : Data.Tree := D;
         begin
            while T /= null loop

               case T.Kind is

                  when Data.Text =>
                     Add (To_String (T.Value));

                  when Data.Var =>
                     if Data.Is_Include_Variable (T.Var) then
                        Add (I_Translate (T.Var, State));

                     else
                        declare
                           Is_Composite : aliased Boolean;
                           Value        : constant String :=
                                            Translate (T.Var, State,
                                                       Is_Composite'Access);
                        begin
                           --  Only adds to the buffer if variable value is not
                           --  empty. This is needed as we want to track empty
                           --  values to be able to rollback if necessary on
                           --  the terse mode. Note that we handle only
                           --  composite tags which are part of the table
                           --  expansion.

                           if Value /= "" then
                              Add (Value);
                              Output := Is_Composite;
                           end if;
                        end;
                     end if;
               end case;

               T := T.Next;
            end loop;
         end Analyze;

         -------------
         -- Analyze --
         -------------

         function Analyze (E : Expr.Tree) return String is

            type Ops_Fct is access function (L, R : Expr.Tree) return String;

            function F_And  (L, R : Expr.Tree) return String;
            function F_Or   (L, R : Expr.Tree) return String;
            function F_Xor  (L, R : Expr.Tree) return String;
            function F_Sup  (L, R : Expr.Tree) return String;
            function F_Esup (L, R : Expr.Tree) return String;
            function F_Einf (L, R : Expr.Tree) return String;
            function F_Inf  (L, R : Expr.Tree) return String;
            function F_Equ  (L, R : Expr.Tree) return String;
            function F_Diff (L, R : Expr.Tree) return String;
            function F_In   (L, R : Expr.Tree) return String;

            type U_Ops_Fct is access function (N : Expr.Tree) return String;

            function F_Not (N : Expr.Tree) return String;

            -----------
            -- F_And --
            -----------

            function F_And (L, R : Expr.Tree) return String is
            begin
               if Expr.Is_True (Analyze (L))
                 and then Expr.Is_True (Analyze (R))
               then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_And;

            ------------
            -- F_Diff --
            ------------

            function F_Diff (L, R : Expr.Tree) return String is
            begin
               if Analyze (L) /= Analyze (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Diff;

            ------------
            -- F_Einf --
            ------------

            function F_Einf (L, R : Expr.Tree) return String is
               LV : constant String := Analyze (L);
               RV : constant String := Analyze (R);
            begin
               if Utils.Is_Number (LV) and then Is_Number (RV) then
                  if Integer'Value (LV) <= Integer'Value (RV) then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;

               else
                  if LV <= RV then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
               end if;
            end F_Einf;

            -----------
            -- F_Equ --
            -----------

            function F_Equ (L, R : Expr.Tree) return String is
            begin
               if Analyze (L) = Analyze (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Equ;

            ------------
            -- F_Esup --
            ------------

            function F_Esup (L, R : Expr.Tree) return String is
               LV : constant String := Analyze (L);
               RV : constant String := Analyze (R);
            begin
               if Utils.Is_Number (LV) and then Is_Number (RV) then
                  if Integer'Value (LV) >= Integer'Value (RV) then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;

               else
                  if LV >= RV then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
               end if;
            end F_Esup;

            ----------
            -- F_In --
            ----------

            function F_In (L, R : Expr.Tree) return String is
               use type Expr.NKind;

               procedure Build_Set (Data : in out Tag_Data);
               --  Returns TRUE or FALSE depending if Value is found in the tag

               ---------------
               -- Build_Set --
               ---------------

               procedure Build_Set (Data : in out Tag_Data) is

                  procedure Process (N : Tag_Node_Access);
                  --  Insert all values pointed to by N

                  -------------
                  -- Process --
                  -------------

                  procedure Process (N : Tag_Node_Access) is
                     L : Tag_Node_Access := N;
                  begin
                     while L /= null loop
                        if L.Kind = Templates_Parser.Value then
                           Data.Values.Include (To_String (L.V));

                        elsif L.Kind = Value_Set then
                           Process (L.VS.Data.Head);
                        end if;
                        L := L.Next;
                     end loop;
                  end Process;

               begin
                  Process (Data.Head);
               end Build_Set;

            begin
               if R.Kind = Expr.Var then
                  declare
                     LL : constant String := Analyze (L);
                     Tk : constant Association := Get_Association (R.Var);
                  begin
                     case Tk.Kind is
                        when Std =>
                           if LL = To_String (Tk.Value) then
                              return "TRUE";
                           else
                              return "FALSE";
                           end if;

                        when Composite =>
                           if Tk.Comp_Value.Data.Values = null then
                              --  Build map of values for fast test
                              Tk.Comp_Value.Data.Values := new Tag_Values.Set;
                              Build_Set (Tk.Comp_Value.Data.all);
                           end if;

                           if Tk.Comp_Value.Data.Values.Contains (LL) then
                              return "TRUE";
                           else
                              return "FALSE";
                           end if;
                     end case;
                  end;

               else
                  raise Template_Error
                    with "in operator right operand must be a tag";
               end if;
            end F_In;

            -----------
            -- F_Inf --
            -----------

            function F_Inf (L, R : Expr.Tree) return String is
               LV : constant String := Analyze (L);
               RV : constant String := Analyze (R);
            begin
               if Utils.Is_Number (LV) and then Is_Number (RV) then
                  if Integer'Value (LV) < Integer'Value (RV) then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;

               else
                  if LV < RV then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
               end if;
            end F_Inf;

            -----------
            -- F_Not --
            -----------

            function F_Not (N : Expr.Tree) return String is
            begin
               if Expr.Is_True (Analyze (N)) then
                  return "FALSE";
               else
                  return "TRUE";
               end if;
            end F_Not;

            ----------
            -- F_Or --
            ----------

            function F_Or (L, R : Expr.Tree) return String is
            begin
               if Expr.Is_True (Analyze (L))
                 or else Expr.Is_True (Analyze (R))
               then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Or;

            -----------
            -- F_Sup --
            -----------

            function F_Sup (L, R : Expr.Tree) return String is
               LV : constant String := Analyze (L);
               RV : constant String := Analyze (R);
            begin
               if Utils.Is_Number (LV) and then Is_Number (RV) then
                  if Integer'Value (LV) > Integer'Value (RV) then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;

               else
                  if LV > RV then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
               end if;
            end F_Sup;

            -----------
            -- F_Xor --
            -----------

            function F_Xor (L, R : Expr.Tree) return String is
            begin
               if Expr.Is_True (Analyze (L))
                 xor Expr.Is_True (Analyze (R))
               then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Xor;

            Op_Table     : constant array (Expr.Ops) of Ops_Fct :=
                             (Expr.O_And   => F_And'Access,
                              Expr.O_Or    => F_Or'Access,
                              Expr.O_Xor   => F_Xor'Access,
                              Expr.O_Sup   => F_Sup'Access,
                              Expr.O_Inf   => F_Inf'Access,
                              Expr.O_Esup  => F_Esup'Access,
                              Expr.O_Einf  => F_Einf'Access,
                              Expr.O_Equal => F_Equ'Access,
                              Expr.O_Diff  => F_Diff'Access,
                              Expr.O_In    => F_In'Access);

            U_Op_Table   : constant array (Expr.U_Ops) of U_Ops_Fct :=
                             (Expr.O_Not => F_Not'Access);

            Is_Composite : aliased Boolean;

         begin
            case E.Kind is
               when Expr.Value =>
                  return To_String (E.V);

               when Expr.Var =>
                  if Data.Is_Include_Variable (E.Var) then
                     return I_Translate (E.Var, State);
                  else
                     return Translate (E.Var, State, Is_Composite'Access);
                  end if;

               when Expr.Op =>
                  return Op_Table (E.O) (E.Left, E.Right);

               when Expr.U_Op =>
                  return U_Op_Table (E.U_O) (E.Next);
            end case;
         end Analyze;

         --------------
         -- Rollback --
         --------------

         procedure Rollback (Activate : Boolean; Mark : Natural) is
         begin
            if Activate then
               --  Rollback

               Rollback : declare
                  To_Delete : constant Natural :=
                                Length (Results) + Last - Mark;
               begin
                  if To_Delete > 0 then
                     if Last >= To_Delete then
                        --  Enough data into the buffer, remove from it
                        Last := Last - To_Delete;

                     else
                        --  Remove remaining data from results
                        Delete
                          (Results,
                           From    => Length (Results) - To_Delete + Last + 1,
                           Through => Length (Results));

                        --  Clear buffer

                        Last := 0;
                     end if;
                  end if;
               end Rollback;
            end if;
         end Rollback;

         ------------------------
         -- Flatten_Parameters --
         ------------------------

         function Flatten_Parameters
           (I : Data.Parameter_Set) return Parameter_Set
         is
            F            : Parameter_Set (I'Range);
            Is_Composite : aliased Boolean;
         begin
            for K in I'Range loop
               if I (K) = null then
                  F (K) := Null_Unbounded_String;
               else
                  case I (K).Kind is
                     when Data.Text =>
                        F (K) := I (K).Value;
                     when Data.Var  =>
                        F (K) := To_Unbounded_String
                          (Translate
                             (I (K).Var, State, Is_Composite'Access));
                  end case;
               end if;
            end loop;
            return F;
         end Flatten_Parameters;

         function Flatten_Parameters
           (I : Data.Parameters) return Parameter_Set
         is
            use type Data.Parameters;
         begin
            if I = null then
               return No_Parameter;
            else
               return Flatten_Parameters (I.all);
            end if;
         end Flatten_Parameters;

         -----------
         -- Flush --
         -----------

         procedure Flush is
         begin
            Append (Results, Buffer (1 .. Last));
            Last := 0;
         end Flush;

         ---------------------
         -- Get_Marked_Text --
         ---------------------

         function Get_Marked_Text (Mark : Natural) return String is
            Len : constant Natural :=
                    Length (Results) + Last - Mark;
         begin
            if Len > 0 then
               if Last >= Len then
                  --  Enough data into the buffer
                  return Buffer (Last - Len + 1 .. Last);

               else
                  --  Get part from result
                  return Slice
                    (Results,
                     Low  => Length (Results) - Len + Last + 1,
                     High => Length (Results)) & Buffer (1 .. Last);
               end if;

            else
               return "";
            end if;
         end Get_Marked_Text;

         -------------
         -- Get_Max --
         -------------

         procedure Get_Max
           (T          : Tree;
            Max_Lines  :    out Natural;
            Max_Expand :    out Natural)
         is

            function Get_Max_Lines
              (T : Tree; N : Positive) return Natural;
            --  Recursivelly descends the tree and compute the max lines that
            --  will be displayed into the table. N is the variable embedded
            --  level regarding the table statement. N=1 means that the
            --  variable is just under the analysed table. N=2 means that the
            --  variable is found inside a nested table statement. And so on.

            -------------------
            -- Get_Max_Lines --
            -------------------

            function Get_Max_Lines
              (T : Tree; N : Positive) return Natural
            is

               function Check (T : Data.Tree) return Natural;
               --  Returns the length of the largest vector tag found on the
               --  subtree.

               function Check (T : Expr.Tree) return Natural;
               --  Idem for an expression subtree as found in a condition

               function Check (T : Data.Tag_Var) return Natural;
               --  Returns the length of Tag T for the current context

               function Check
                 (I : not null access Data.Parameter_Set) return Natural;
               --  Returns the length of the largest vector tag found on the
               --  include parameters.

               -----------
               -- Check --
               -----------

               function Check (T : Data.Tag_Var) return Natural is

                  Table_Level : constant Positive := State.Table_Level + 1;
                  --  This is the current table level, State.Table_Level is
                  --  not yet updated when calling this routine hence the +1.

                  Var_Level   : constant Natural := State.Table_Level + N;
                  --  This is the variable nested table level. O means that the
                  --  variable is not inside a table statement.

                  function Max (T : Tag; N : Natural) return Natural;
                  --  Returns the maximum number of items for the Nth Tag level

                  function Max
                    (Name : String; N : Natural; Path : Dynamic.Path)
                     return Natural;
                  --  Idem for a Cursor_Tag

                  ---------
                  -- Max --
                  ---------

                  function Max (T : Tag; N : Natural) return Natural is
                     Result : Natural := 0;
                     P      : Tag_Node_Access := T.Data.Head;
                  begin
                     while P /= null loop
                        if P.Kind = Value_Set then
                           if N = 1 then
                              Result :=
                                Natural'Max (Result, P.VS.Data.Count);
                           else
                              Result := Natural'Max
                                (Result, Max (P.VS.all, N - 1));
                           end if;
                        end if;
                        P := P.Next;
                     end loop;
                     return Result;
                  end Max;

                  function Max
                    (Name : String; N : Natural; Path : Dynamic.Path)
                     return Natural
                  is
                     use type Dynamic.Path;
                     Result : Natural := 0;
                     L : Natural;
                  begin
                     L := Dynamic.Length (Cursor_Tag, Name, Path);

                     for K in 1 .. L loop
                        if Path'Length = N then
                           Result := Natural'Max
                             (Result,
                              Dynamic.Length (Cursor_Tag, Name, Path & K));
                        else
                           Result := Natural'Max
                             (Result, Max (Name, N, Path & K));
                        end if;
                     end loop;

                     return Result;
                  end Max;

               begin
                  declare
                     use type Dynamic.Cursor_Tag_Access;
                     Tk : constant Association := Get_Association (T);
                  begin
                     if Tk = Null_Association then

                        if Cursor_Tag /= Dynamic.Null_Cursor_Tag then
                           --  Check the Cursor_Tag
                           declare
                              Name : constant String := To_String (T.Name);
                              D, K : Natural;
                              L1   : Natural;
                           begin
                              D := Dynamic.Dimension (Cursor_Tag, Name);

                              if N > D then
                                 --  Ignore this variable as it is deeper than
                                 --  its dimension.
                                 return 0;

                              elsif D /= 0 then
                                 --  This is a Cursor_Tag

                                 K := D - N + 1;
                                 --  K is the variable indice for which
                                 --  the number of items is looked for.

                                 if D > Table_Level then
                                    --  The variable dimensions is bigger than
                                    --  the current table level. This means
                                    --  that the index needs to be updated so
                                    --  that the outer table tag statement will
                                    --  be the first var index.
                                    K := K - (D - Var_Level);
                                 end if;

                                 L1 := Dynamic.Length
                                   (Cursor_Tag, Name, Path => (1 => 1));

                                 if D = 1 and then L1 = 1 then
                                    --  Not a composite tag
                                    return 0;

                                 elsif K = 1 then
                                    return L1;

                                 else
                                    return Max (Name, K - 1, (1 => 1));
                                 end if;
                              end if;
                           end;
                        end if;

                     else
                        if Tk.Kind = Composite then
                           if N > Tk.Comp_Value.Data.Nested_Level then
                              --  Ignore this variable as it is deeper than
                              --  its nested level.
                              return 0;
                           end if;

                           --  We look first at two common cases to handle
                           --  more efficiently tag into a single or two
                           --  table statements.

                           if Table_Level = 1
                             or else Tk.Comp_Value.Data.Nested_Level = 1
                           then
                              --  First table level, or flat composite, the
                              --  number of iterations corresponds to the
                              --  number of item into this tag.
                              return Size (Tk.Comp_Value);

                           elsif Table_Level = 2
                             and then N = 1
                           then
                              --  Table level 2 while looking to nested
                              --  variable.
                              return Tk.Comp_Value.Data.Max;

                           else
                              --  All other cases here
                              declare
                                 K : constant Positive
                                   := Tk.Comp_Value.Data.Nested_Level - N + 1;
                                 --  K is the variable indice for which
                                 --  the number of items is looked for.
                              begin
                                 if K = 1 then
                                    return Size (Tk.Comp_Value);
                                 elsif K = 2 then
                                    return Tk.Comp_Value.Data.Max;
                                 else
                                    return Max (Tk.Comp_Value, K - 1);
                                 end if;
                              end;
                           end if;
                        end if;
                     end if;
                  end;

                  return 0;
               end Check;

               function Check (T : Data.Tree) return Natural is
                  use type Data.Attribute;
                  use type Data.NKind;

                  Iteration : Natural := Natural'First;
                  D         : Data.Tree := T;
               begin
                  while D /= null loop
                     if D.Kind = Data.Var
                       and then D.Var.Attribute.Attr = Data.Nil
                     then
                        Iteration := Natural'Max (Iteration, Check (D.Var));
                     end if;

                     D := D.Next;
                  end loop;

                  return Iteration;
               end Check;

               function Check (T : Expr.Tree) return Natural is
               begin
                  case T.Kind is
                     when Expr.Var   =>
                        return Natural'Max (0, Check (T.Var));
                     when Expr.Op    =>
                        return Natural'Max (Check (T.Left), Check (T.Right));
                     when Expr.U_Op  =>
                        return Natural'Max (0, Check (T.Next));
                     when Expr.Value =>
                        return 0;
                  end case;
               end Check;

               function Check
                 (I : not null access Data.Parameter_Set) return Natural
               is
                  Iteration : Natural := Natural'First;
               begin
                  for K in I'Range loop
                     if I (K) /= null then
                        Iteration := Natural'Max (Iteration, Check (I (K)));
                     end if;
                  end loop;
                  return Iteration;
               end Check;

            begin
               if T = null then
                  return Natural'First;
               end if;

               case T.Kind is
                  when Info | C_Info | Set_Stmt =>
                     return Get_Max_Lines (T.Next, N);

                  when Text =>
                     return Natural'Max
                       (Check (T.Text), Get_Max_Lines (T.Next, N));

                  when If_Stmt =>
                     return Natural'Max
                       (Check (T.Cond),
                        Natural'Max
                          (Get_Max_Lines (T.Next, N),
                           Natural'Max
                             (Get_Max_Lines (T.N_True, N),
                              Get_Max_Lines (T.N_False, N))));

                  when Table_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.Blocks, N + 1),
                        Get_Max_Lines (T.Next, N));

                  when Section_Block =>
                     return Natural'Max
                       (Get_Max_Lines (T.Next, N),
                        Natural'Max
                          (Get_Max_Lines (T.Common, N),
                           Get_Max_Lines (T.Sections, N)));

                  when Section_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.Next, N),
                        Get_Max_Lines (T.N_Section, N));

                  when Include_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.File.Info, N),
                        Natural'Max
                          (Check (T.I_Params),
                           Get_Max_Lines (T.Next, N)));

                  when Inline_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.Next, N),
                        Get_Max_Lines (T.I_Block, N));
               end case;
            end Get_Max_Lines;

            Result : Natural := Get_Max_Lines (T.Blocks, 1);

         begin
            pragma Assert (T.Kind = Table_Stmt);

            Max_Lines := Result;

            if T.Terminate_Sections then
               --  ??? This part of code handle properly only table with a
               --  single block. What should be done if there is multiple
               --  blocks ? Should all blocks be of the same size ?

               declare
                  N_Section : constant Natural := T.Blocks.Sections_Count;
               begin
                  if Result mod N_Section /= 0 then
                     Result := Result + N_Section - (Result mod N_Section);
                  end if;
               end;
            end if;

            Max_Expand := Result;
         end Get_Max;

         -----------------
         -- I_Translate --
         -----------------

         function I_Translate
           (Var   : Data.Tag_Var;
            State : Parse_State) return String
         is
            use type Data.Attribute;
            use type Data.NKind;
            use type Data.Parameters;
         begin
            pragma Assert (Var.N /= -1);

            if State.I_Params /= null
              and then Var.N <= State.I_Params'Last
              and then State.I_Params (Var.N) /= null
            then
               declare
                  T : constant Data.Tree := State.I_Params (Var.N);
                  --  T is the data tree that should be evaluated in place
                  --  of the include variable.
                  C : aliased Filter.Filter_Context :=
                        (State.F_Params'Length,
                         Translations, Lazy_Tag, State.F_Params);
                  Is_Composite : aliased Boolean;
               begin
                  if T.Next = null and then T.Kind = Data.Var then
                     --  Here we have a special case where the include
                     --  variable is replaced by a single variable.

                     declare
                        V : Data.Tag_Var := T.Var;
                     begin
                        if V.N = -1 then
                           --  First thing we want to do is to inherit
                           --  attributes from the include variable if we
                           --  have no attribute.

                           if V.Attribute.Attr = Data.Nil then
                              V.Attribute := Var.Attribute;
                           end if;

                           --  Note that below we pass the parent state. This
                           --  is required as if the variable is an alias to
                           --  to an include parameter we need to get the
                           --  value for this variable in parent state. If the
                           --  variable is a standard one (from a translate
                           --  table) the state will not be used.

                           return Data.Translate
                             (Var,
                              Translate
                                (V, State.Parent.all, Is_Composite'Access),
                                 C'Access);

                        else
                           --  This variable reference a parent include
                           --  variable.

                           return I_Translate (V, State.Parent.all);
                        end if;
                     end;

                  else
                     --  Here we flush the buffer and then we analyse the
                     --  include parameter. The result is contained into
                     --  the buffer which is large enough for an include
                     --  variable.

                     Flush;
                     Analyze (T);

                     declare
                        L : constant Natural := Last;
                     begin
                        Last := 0;
                        return Data.Translate
                          (Var, Buffer (Buffer'First .. L), C'Access);
                     end;
                  end if;
               end;

            else
               return "";
            end if;
         end I_Translate;

         -----------------------
         -- Inline_Cursor_Tag --
         -----------------------

         function Inline_Cursor_Tag
           (Cursor_Tag : Dynamic.Cursor_Tag_Access;
            Var_Name   : String;
            Dim        : Positive;
            Path       : Dynamic.Path) return Unbounded_String
         is
            use type Dynamic.Path;
            Result : Unbounded_String;
            L      : Natural;
         begin
            L := Dynamic.Length (Cursor_Tag, Var_Name, 1 & Path);

            for K in 1 .. L loop
               if Result /= Null_Unbounded_String then
                  Append (Result, ' ');
               end if;

               if Dim = Path'Length + 1 then
                  Append
                    (Result,
                     Dynamic.Value (Cursor_Tag, Var_Name, Path & K));
               else
                  Append
                    (Result,
                     Inline_Cursor_Tag
                       (Cursor_Tag, Var_Name, Dim, Path & K));
               end if;
            end loop;

            return Result;
         end Inline_Cursor_Tag;

         --------------
         -- Get_Mark --
         --------------

         function Get_Mark return Natural is
         begin
            Output := False;
            return Length (Results) + Last;
         end Get_Mark;

         -------------
         -- Pop_Sep --
         -------------

         procedure Pop_Sep (State : Parse_State) is
         begin
            if Last_Was_Sep then
               Last := Last - Length (State.Inline_Sep);
               Last_Was_Sep := False;
            end if;
         end Pop_Sep;

         --------------
         -- Push_Sep --
         --------------

         procedure Push_Sep (State : Parse_State) is
         begin
            if State.Inline_Sep /= Null_Unbounded_String then
               Add (To_String (State.Inline_Sep), Sep => True);
            end if;
         end Push_Sep;

         ---------------
         -- Translate --
         ---------------

         function Translate
           (Var          : Data.Tag_Var;
            State        : Parse_State;
            Is_Composite : access Boolean) return String
         is
            use type Data.Parameters;
            use type Filter.Set_Access;

            C        : aliased Filter.Filter_Context :=
                         (State.F_Params'Length,
                          Translations, Lazy_Tag, State.F_Params);
            D_Pos    : Definitions.Def_Map.Cursor;
            Up_Value : Natural := 0;
         begin
            Is_Composite.all := False;

            if Var.Is_Macro then
               --  Two possibilities, either we have a macro inlined here or a
               --  user macro.

               if Var.Def = null then
                  --  A user defined macro, use callback if any
                  if Macro.Callback /= null then
                     declare
                        Name   : constant String := To_String (Var.Name);
                        Params : Parameter_Set (Var.Parameters'Range);
                     begin
                        --  Set parameters
                        for K in Params'Range loop
                           Flush;
                           Analyze (Var.Parameters (K));

                           Params (K) := To_Unbounded_String
                             (Buffer (Buffer'First .. Last));
                           Last := 0;
                        end loop;

                        return Data.Translate
                          (Var, Macro.Callback (Name, Params), C'Access);
                     end;
                  end if;

               else
                  declare
                     Mark : constant Natural := Get_Mark;
                  begin
                     Analyze (Var.Def, State);

                     --  Apply filters if any to the result

                     if Var.Filters /= null then
                        declare
                           V : constant String := Get_Marked_Text (Mark);
                        begin
                           Rollback (True, Mark);
                           return Data.Translate (Var, V, C'Access);
                        end;

                     else
                        return "";
                     end if;
                  end;
               end if;
            end if;

            D_Pos := Definitions.Def_Map.Find
              (D_Map, To_String (Var.Name));

            if Definitions.Def_Map.Has_Element (D_Pos) then
               --  We have a definition for this variable in the template
               declare
                  N : constant Definitions.Node :=
                        Definitions.Def_Map.Element (D_Pos);
                  V : Data.Tag_Var := Var;
               begin
                  case N.Kind is
                     when Definitions.Const =>
                        return Data.Translate
                          (Var, To_String (N.Value), C'Access);

                     when Definitions.Ref =>
                        V.N := N.Ref;
                        return I_Translate (V, State);

                     when Definitions.Ref_Default =>
                        if State.I_Params = null
                          or else N.Ref > State.I_Params'Last
                          or else State.I_Params (N.Ref) = null
                        then
                           --  This include parameter does not exists, use
                           --  default value.
                           return Data.Translate
                             (Var, To_String (N.Value), C'Access);
                        else
                           V.N := N.Ref;
                           return I_Translate (V, State);
                        end if;
                  end case;
               end;
            end if;

            declare
               use type Data.Attribute;
               use type Dynamic.Cursor_Tag_Access;
               use type Dynamic.Path;
               Tk : constant Association := Get_Association (Var);
            begin
               if Tk = Null_Association then

                  if Cursor_Tag /= Dynamic.Null_Cursor_Tag then
                     --  Check the Cursor_Tag
                     declare
                        Name         : constant String := To_String (Var.Name);
                        D, L         : Natural;
                        Valid_Cursor : Boolean := True;
                     begin
                        D := Dynamic.Dimension (Cursor_Tag, Name);

                        if D /= 0 then
                           if Var.Attribute.Attr /= Data.Nil then
                              --  ??? Would be nice to remove this restriction
                              raise Template_Error with
                                "Attributes not supported for Cursor_Tag.";
                           end if;

                           --  This is a Cursor_Tag, check that the current
                           --  table cursor is valid for it.

                           for K in 1 .. D loop
                              if State.Cursor (K) >
                                Dynamic.Length
                                  (Cursor_Tag, Name,
                                   1 & State.Cursor (1 .. K - 1))
                              then
                                 Valid_Cursor := False;
                              end if;
                           end loop;

                           if Valid_Cursor then

                              L := Dynamic.Length
                                (Cursor_Tag, Name,
                                 1 & State.Cursor
                                   (1 .. State.Table_Level - 1));

                              if D = 1 and then L = 1 then
                                 --  A standard tag (single value)
                                 return Data.Translate
                                   (Var,
                                    Dynamic.Value (Cursor_Tag, Name, (1 => 1)),
                                    C'Access);

                              else
                                 --  A composite tag, check that the dimension
                                 --  of the tag correspond to the current table
                                 --  nested level.

                                 if D = State.Table_Level then
                                    return Data.Translate
                                      (Var,
                                       Dynamic.Value
                                         (Cursor_Tag, Name,
                                          State.Cursor (1 .. D)), C'Access);

                                 else
                                    --  Otherwise we inline the structure
                                    return To_String
                                      (Inline_Cursor_Tag
                                         (Cursor_Tag, Name, D,
                                          State.Cursor
                                            (1 .. State.Table_Level)));
                                 end if;
                              end if;
                           end if;
                        end if;
                     end;
                  end if;

               else
                  case Tk.Kind is

                     when Std =>
                        if Var.Attribute.Attr = Data.Nil then
                           return Data.Translate
                             (Var, To_String (Tk.Value), C'Access);
                        else
                           raise Template_Error
                             with "Attribute not valid on a discrete tag ("
                               & Data.Image (Var) & ')';
                        end if;

                     when Composite =>
                        Is_Composite.all := True;
                        if Tk.Comp_Value.Data.Nested_Level = 1 then
                           --  This is a vector

                           if Var.Attribute.Attr = Data.Length then
                              return Data.Translate
                                (Var,
                                 Utils.Image
                                   (Tk.Comp_Value.Data.Count), C'Access);

                           elsif Var.Attribute.Attr = Data.Up_Level then
                              Up_Value := Var.Attribute.Value;

                           elsif Var.Attribute.Attr /= Data.Nil then
                              raise Template_Error
                                with "This attribute is not valid for a "
                                  & "vector tag (" & Data.Image (Var) & ')';
                           end if;

                        elsif Tk.Comp_Value.Data.Nested_Level = 2 then
                           if Var.Attribute.Attr = Data.Line then
                              --  'Line on a matrix
                              return Data.Translate
                                (Var,
                                 Utils.Image (Tk.Comp_Value.Data.Count),
                                 C'Access);

                           elsif Var.Attribute.Attr = Data.Min_Column then
                              --  'Min_Column on a matrix
                              return Data.Translate
                                (Var,
                                 Utils.Image (Tk.Comp_Value.Data.Min),
                                 C'Access);

                           elsif Var.Attribute.Attr = Data.Max_Column then
                              --  'Max_Column on a matrix
                              return Data.Translate
                                (Var,
                                 Utils.Image (Tk.Comp_Value.Data.Max),
                                 C'Access);

                           elsif Var.Attribute.Attr /= Data.Nil then
                              raise Template_Error
                                with "This attribute is not valid for a "
                                  & "matrix tag (" & Data.Image (Var) & ')';
                           end if;
                        end if;

                        declare
                           Result : Unbounded_String;
                           Found  : Boolean;
                        begin
                           Field
                             (Tk.Comp_Value,
                              State.Cursor (1 .. State.Table_Level),
                              Up_Value,
                              Result, Found);

                           return Data.Translate
                             (Var, To_String (Result), C'Access);
                        end;
                  end case;
               end if;
            end;

            case Var.Internal is
               when Data.Up_Table_Line =>
                  if State.Table_Level < 2 then
                     return Data.Translate (Var, "0", C'Access);
                  else
                     return Data.Translate
                       (Var,
                        Utils.Image (State.Cursor (State.Table_Level - 1)),
                        C'Access);
                  end if;

               when Data.Table_Line =>
                  if State.Table_Level = 0 then
                     return Data.Translate (Var, "0", C'Access);
                  else
                     return Data.Translate
                       (Var,
                        Utils.Image (State.Cursor (State.Table_Level)),
                        C'Access);
                  end if;

               when Data.Number_Line =>
                  return Data.Translate
                    (Var, Utils.Image (State.Max_Lines), C'Access);

               when Data.Table_Level =>
                  return Data.Translate
                    (Var, Utils.Image (State.Table_Level), C'Access);

               when Data.Now =>
                  return Data.Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%Y-%m-%d %H:%M:%S"),
                     C'Access);

               when Data.Year =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%Y"), C'Access);

               when Data.Month =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%m"), C'Access);

               when Data.Day =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%d"), C'Access);

               when Data.Hour =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%H"), C'Access);

               when Data.Minute =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%M"), C'Access);

               when Data.Second =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%S"), C'Access);

               when Data.Month_Name =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%B"), C'Access);

               when Data.Day_Name =>
                  return Data.Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%A"), C'Access);

               when Data.No =>
                  null;
            end case;

            --  The tag was not found in the Translation_Table, we either
            --  returns the empty string or we keep the tag as is.

            if Keep_Unknown_Tags then
               return Data.Image (Var);
            else
               return Data.Translate (Var, "", C'Access);
            end if;
         end Translate;

      begin
         if T = null then
            return;
         end if;

         case T.Kind is

            when Info | C_Info =>
               Analyze (T.Next, State);

            when Text =>
               declare
                  N : Tree := T;
               begin
                  begin
                     --  Handles all consecutive Text nodes

                     while N /= null and then N.Kind = Text loop
                        Analyze (N.Text);
                        N := N.Next;
                     end loop;

                  exception
                     when E : others =>
                        raise Template_Error
                          with Exceptions.Exception_Message (E)
                            & " In " & Filename
                            & " at line" & Natural'Image (N.Line);
                  end;

                  Analyze (N, State);
               end;

            when Set_Stmt =>
               declare
                  N : Tree := T;
               begin
                  begin
                     --  Handles all consecutive Set nodes

                     while N /= null and then N.Kind = Set_Stmt loop
                        Handle_Set : declare
                           Name    : constant String := To_String (N.Def.Name);
                           Pos     : Definitions.Def_Map.Cursor;
                           Success : Boolean;
                        begin
                           Pos := D_Map.Find (Name);

                           if Definitions.Def_Map.Has_Element (Pos) then
                              D_Map.Replace_Element (Pos, New_Item => N.Def.N);
                           else
                              D_Map.Insert (Name, N.Def.N, Pos, Success);
                           end if;
                        end Handle_Set;

                        N := N.Next;
                     end loop;
                  end;

                  Analyze (N, State);
               end;

            when If_Stmt  =>
               begin
                  if Expr.Is_True (Analyze (T.Cond)) then
                     Analyze (T.N_True, State);
                  else
                     Analyze (T.N_False, State);
                  end if;
               exception
                  when E : others =>
                     raise Template_Error
                       with Exceptions.Exception_Message (E)
                         & " In " & Filename
                         & " at line" & Natural'Image (T.Line);
               end;

               Analyze (T.Next, State);

            when Table_Stmt =>
               declare
                  Max_Lines, Max_Expand : Natural;
               begin
                  Get_Max (T, Max_Lines, Max_Expand);

                  Analyze (T.Blocks,
                           Parse_State'(State.F_Params'Length,
                                        State.Cursor,
                                        Max_Lines, Max_Expand,
                                        T.Reverse_Index,
                                        State.Table_Level + 1,
                                        State.Inline_Sep,
                                        State.Filename,
                                        T.Blocks_Count,
                                        State.I_Params,
                                        State.F_Params,
                                        Empty_Block_State,
                                        T.Terse,
                                        L_State'Unchecked_Access));
               end;

               Push_Sep (State);

               Analyze (T.Next, State);

               Pop_Sep (State);

            when Section_Block =>
               declare
                  B_State : array (1 .. State.Blocks_Count) of Block_State;
                  B       : Positive;
                  Mark    : Natural := 0;
               begin
                  for K in 1 .. State.Max_Expand loop
                     if State.Terse_Table then
                        Mark := Get_Mark;
                     end if;

                     declare
                        New_Cursor : Indices := State.Cursor;
                        Block      : Tree    := T;
                     begin
                        if State.Reverse_Index then
                           New_Cursor (State.Table_Level) :=
                             State.Max_Expand - K + 1;
                        else
                           New_Cursor (State.Table_Level) := K;
                        end if;

                        B := 1;

                        while Block /= null loop
                           --  For all blocks in this table

                           if B_State (B).Section = null
                             or else B_State (B).Section.N_Section = null
                           then
                              B_State (B) := (1, Block.Sections);
                           else
                              B_State (B)
                                := (B_State (B).Section_Number + 1,
                                    B_State (B).Section.N_Section);
                           end if;

                           Analyze
                             (Block.Common,
                              Parse_State'(State.F_Params'Length,
                                           New_Cursor,
                                           State.Max_Lines, State.Max_Expand,
                                           State.Reverse_Index,
                                           State.Table_Level,
                                           State.Inline_Sep,
                                           State.Filename,
                                           State.Blocks_Count,
                                           State.I_Params,
                                           State.F_Params,
                                           Empty_Block_State,
                                           State.Terse_Table,
                                           L_State'Unchecked_Access));

                           if Block.Common /= null then
                              Pop_Sep (State);
                              Push_Sep (State);
                           end if;

                           Analyze
                             (Block.Sections,
                              Parse_State'(State.F_Params'Length,
                                           New_Cursor,
                                           State.Max_Lines, State.Max_Expand,
                                           State.Reverse_Index,
                                           State.Table_Level,
                                           State.Inline_Sep,
                                           State.Filename,
                                           State.Blocks_Count,
                                           State.I_Params,
                                           State.F_Params,
                                           B_State (B),
                                           State.Terse_Table,
                                           L_State'Unchecked_Access));

                           Pop_Sep (State);

                           Push_Sep (State);

                           Block := Block.Next;
                           B := B + 1;
                        end loop;

                        if State.Terse_Table then
                           Rollback (Activate => Output = False, Mark => Mark);
                        end if;
                     end;
                  end loop;
                  Pop_Sep (State);
               end;

            when Section_Stmt =>
               Analyze
                 (State.Block.Section.Next,
                  Parse_State'(State.F_Params'Length,
                               State.Cursor,
                               State.Max_Lines, State.Max_Expand,
                               State.Reverse_Index,
                               State.Table_Level,
                               State.Inline_Sep,
                               State.Filename,
                               State.Blocks_Count,
                               State.I_Params,
                               State.F_Params,
                               State.Block,
                               State.Terse_Table,
                               L_State'Unchecked_Access));

            when Include_Stmt =>
               if T.I_Filename /= null then
                  --  This is a deferred include file load as the name of the
                  --  include file was not a static string.
                  Flush;
                  Analyze (T.I_Filename);

                  declare
                     Filename : constant String := Buffer (1 .. Last);
                     S_File   : Static_Tree;
                  begin
                     Last := 0; --  Removes include filename from the buffer

                     S_File := Load (Build_Include_Pathname
                                     (To_String (State.Filename), Filename),
                                     Cached, True);

                     if S_File /= T.File then
                        if Cached then
                           if T.File.C_Info /= null then
                              Cached_Files.Release (T.File);
                           end if;
                        else
                           Release (T.File.Info);
                        end if;
                     end if;

                     T.File := S_File;
                  end;
               end if;

               Analyze (T.File.Info,
                        Parse_State'(T.I_Params'Length,
                                     Cursor        => State.Cursor,
                                     Max_Lines     => State.Max_Lines,
                                     Max_Expand    => State.Max_Expand,
                                     Reverse_Index => State.Reverse_Index,
                                     Table_Level   => State.Table_Level,
                                     Inline_Sep    => State.Inline_Sep,
                                     Filename      => State.Filename,
                                     Blocks_Count  => State.Blocks_Count,
                                     I_Params      => T.I_Params,
                                     F_Params      => Flatten_Parameters
                                                        (T.I_Params),
                                     Block         => State.Block,
                                     Terse_Table   => State.Terse_Table,
                                     Parent      => L_State'Unchecked_Access));
               Analyze (T.Next, State);

            when Inline_Stmt =>
               Add (To_String (T.Before));
               Analyze (T.I_Block,
                        Parse_State'(State.F_Params'Length,
                                     Cursor        => State.Cursor,
                                     Max_Lines     => State.Max_Lines,
                                     Max_Expand    => State.Max_Expand,
                                     Reverse_Index => State.Reverse_Index,
                                     Table_Level   => State.Table_Level,
                                     Inline_Sep    => T.Sep,
                                     Filename      => State.Filename,
                                     Blocks_Count  => State.Blocks_Count,
                                     I_Params      => State.I_Params,
                                     F_Params      => State.F_Params,
                                     Block         => State.Block,
                                     Terse_Table   => State.Terse_Table,
                                     Parent      => L_State'Unchecked_Access));
               Add (To_String (T.After));
               Analyze (T.Next, State);
         end case;
      end Analyze;

      ---------------------
      -- Get_Association --
      ---------------------

      function Get_Association (Var : Data.Tag_Var) return Association is
         use type Data.Internal_Tag;
         use type Dynamic.Lazy_Tag_Access;
         Name : constant String := To_String (Var.Name);
         Pos  : Association_Map.Cursor;
      begin
         Pos := Translations.Set.Find (Name);

         if Association_Map.Has_Element (Pos) then
            return Association_Map.Element (Pos);

         elsif Lazy_Tag /= Dynamic.Null_Lazy_Tag
           and then not Filter.Is_No_Dynamic (Var.Filters)
           and then Var.Internal = Data.No
         then
            --  Look into the Lazy_Set for the cached value

            Pos := Lazy_Set.Set.Find (Name);

            if Association_Map.Has_Element (Pos) then
               return Association_Map.Element (Pos);

            else
               --  Check for Lazy tag

               Dynamic.Value (Lazy_Tag, Name, Lazy_Set);

               return Get (Lazy_Set, Name);
            end if;

         else
            return Null_Association;
         end if;
      end Get_Association;

      T : Static_Tree;

   begin
      T := Load (Filename, Cached);

      Now := Ada.Calendar.Clock;
      --  Used for the time related variable

      Analyze (T.C_Info, Empty_State);

      if Cached then
         Cached_Files.Release (T);
      else
         Release (T.Info);
      end if;

      --  Flush buffer and return result

      Append (Results, Buffer (1 .. Last));

      return Results;
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (T : Tree; Level : Natural := 0) is separate;

   procedure Print_Tree (Filename : String) is
      T : Static_Tree;
   begin
      T := Load (Filename);
      Print_Tree (T.Info);
      Release (T.Info);
   end Print_Tree;

   --------------------------
   -- Print_Defined_Macros --
   --------------------------

   procedure Print_Defined_Macros is
   begin
      Macro.Print_Defined_Macros;
   end Print_Defined_Macros;

   -----------
   -- Quote --
   -----------

   function Quote (Str : String) return String is
      K : constant Natural := Strings.Fixed.Index (Str, " ");
   begin
      if K = 0 and then Str'Length > 0 then
         return Str;
      else
         return '"' & Str & '"';
      end if;
   end Quote;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Name    : String;
      Handler : Callback) renames Filter.Register;

   procedure Register_Filter
     (Name    : String;
      Handler : Callback_No_Param) renames Filter.Register;

   procedure Register_Filter
     (Name   : String;
      Filter : not null access User_Filter'Class) renames Filter.Register;

   procedure Free_Filters renames Filter.Free_Filters;

   ----------------------------
   -- Register_Macro_Handler --
   ----------------------------

   procedure Register_Macro_Handler (Callback : Macro_Callback) is
   begin
      Macro.Callback := Callback;
   end Register_Macro_Handler;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tree; Include : Boolean := True) is
      use type Data.Tree;
   begin
      if T = null then
         return;
      end if;

      case T.Kind is
         when Info =>
            declare
               I : Tree := T.I_File;
               O : Tree;
            begin
               while I /= null loop
                  O := I;
                  I := I.Next;
                  Unchecked_Free (O);
               end loop;
            end;

            Release (T.Next, Include);

         when C_Info =>
            Release (T.Next, Include);

         when Text =>
            declare
               N   : Tree := T;
               Tmp : Tree;
            begin
               --  Handles all consecutive Text nodes

               while N /= null and then N.Kind = Text loop
                  Data.Release (N.Text);
                  Tmp := N;
                  N := N.Next;
                  Unchecked_Free (Tmp);
               end loop;

               Release (N, Include);
            end;

            T := null;
            --  T has been freed, we set the pointer to null to avoid double
            --  deallocation by the call to Free at the end of this routine.

         when Set_Stmt =>
            Definitions.Release (T.Def);
            Release (T.Next, Include);

         when If_Stmt  =>
            Expr.Release (T.Cond);
            Release (T.N_True, Include);
            Release (T.N_False, Include);
            Release (T.Next, Include);

         when Table_Stmt =>
            Release (T.Blocks, Include);
            Release (T.Next, Include);

         when Section_Block =>
            Release (T.Sections, Include);
            Release (T.Common, Include);
            Release (T.Next, Include);

         when Section_Stmt =>
            Release (T.Next, Include);
            Release (T.N_Section, Include);

         when Include_Stmt =>
            if Include then
               Release (T.File.Info, Include);

               for K in T.I_Params'Range loop
                  Data.Release (T.I_Params (K));
               end loop;
               Data.Unchecked_Free (T.I_Params);
            end if;

            Release (T.Next, Include);

         when Inline_Stmt =>
            Release (T.I_Block, Include);
            Release (T.Next, Include);
      end case;

      Unchecked_Free (T);
   end Release;

   -------------------
   -- Release_Cache --
   -------------------

   procedure Release_Cache is
   begin
      Cached_Files.Release;
   end Release_Cache;

   ------------
   -- Remove --
   ------------

   procedure Remove (Set : in out Translate_Set; Name : String) is
   begin
      if Set.Set.Contains (Name) then
         Set.Set.Delete (Name);
      end if;
   end Remove;

   -------------------
   -- Set_Separator --
   -------------------

   procedure Set_Separator (T : in out Tag; Separator : String) is
   begin
      T.Data.Separator := To_Unbounded_String (Separator);
   end Set_Separator;

   ------------------------
   -- Set_Tag_Separators --
   ------------------------

   procedure Set_Tag_Separators
     (Start_With : String := Default_Begin_Tag;
      Stop_With  : String := Default_End_Tag) is
   begin
      Begin_Tag := To_Unbounded_String (Start_With);
      End_Tag   := To_Unbounded_String (Stop_With);
   end Set_Tag_Separators;

   ----------
   -- Size --
   ----------

   function Size (T : Tag) return Natural is
   begin
      return T.Data.Count;
   end Size;

   function Size (Set : Translate_Set) return Natural is
   begin
      return Natural (Set.Set.Length);
   end Size;

   -------------------
   -- Tag_From_Name --
   -------------------

   function Tag_From_Name (Name : String) return String is
   begin
      return To_String (Begin_Tag) & Name & To_String (End_Tag);
   end Tag_From_Name;

   ------------
   -- To_Set --
   ------------

   function To_Set (Table : Translate_Table) return Translate_Set is
      Set : Translate_Set;
   begin
      for K in Table'Range loop
         Insert (Set, Table (K));
      end loop;
      return Set;
   end To_Set;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Template     : String;
      Translations : Translate_Table := No_Translation) return String is
   begin
      return Translate (Template, To_Set (Translations));
   end Translate;

   function Translate
     (Template     : String;
      Translations : Translate_Set) return String
   is
      T : Data.Tree := Data.Parse (Template);
      P : Data.Tree := T;

      Results : Unbounded_String;

      function Translate (Var : Data.Tag_Var) return String;
      --  Returns translation for Var

      ---------------
      -- Translate --
      ---------------

      function Translate (Var : Data.Tag_Var) return String is
         Pos : Association_Map.Cursor;
         C   : aliased Filter.Filter_Context :=
                 (0, Translations, null, No_Parameter);
      begin
         --  ??? we should probably handle macros there too
         Pos := Translations.Set.Find (To_String (Var.Name));

         if Association_Map.Has_Element (Pos) then
            declare
               Item : constant Association := Association_Map.Element (Pos);
            begin
               case Item.Kind is
                  when Std =>
                     return Data.Translate
                       (Var, To_String (Item.Value), C'Access);
                  when others =>
                     return "";
               end case;
            end;
         end if;

         return "";
      end Translate;

      use type Data.Tree;

   begin
      while P /= null loop
         case P.Kind is
            when Data.Text => Append (Results, P.Value);
            when Data.Var  => Append (Results, Translate (P.Var));
         end case;

         P := P.Next;
      end loop;

      Data.Release (T);

      return To_String (Results);
   end Translate;

end Templates_Parser;
