--!!!!! which chars are in a word (Whole_Word) ?
--!!!!! / conv to \, case (Files_Pattern) ?




--  Line-oriented pattern search in source code files

--  This package implements a line-oriented searching for a word or a pattern
--  in given source code files.
--  Scope may be limited to comments and/or strings and/or statements. Syntax
--  detection is done on filename patterns (eg *.c, *.ads, ...). If file type
--  is unknown (probably with *.txt), whole file is scanned, whatever asked
--  scope is.
--  Files may be given as a list or with a pattern and a directory !!!
--  Classic options included: match case, whole word.


--  ??? assume strings can continue over ASCII.LF (wait for end delimiter)
--  ??? in C strings, \ skips ASCII.LF; don't skip LF without \ ?
--  ??? 1 depth level of syntax, eg strings aren't found in comments




with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Regexp; use GNAT.Regexp;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Find_Utils is

   type Code_Search is private;
   --  Private type representing a search in source code files

   type Project_Files is array (Positive range <>) of String_Access;
   --  Type used to store the project file list

   type Project_Files_Access is access Project_Files;

   Search_Error : exception;

   procedure Init_Search
     (Search          : out Code_Search;
      Look_For        : String;
      Files           : Project_Files_Access;
      Match_Case      : Boolean := False;
      Whole_Word      : Boolean := False;
      Regexp          : Boolean := False;
      Scan_Comments   : Boolean := True;
      Scan_Strings    : Boolean := True;
      Scan_Statements : Boolean := True);
   --  Prepare searching for a word or a pattern in given file list.
   --
   --  Look_For         Searched word or mono-line pattern (see g-regpat.ads)
   --  Files            Files to be searched
   --                NOTE: It's your responsability to free Files
   --  Match_Case       CAPITAL & small letters are respected
   --  Whole_Word       Match only full words (eg 'end' not found in 'friend')
   --  Regexp           Look for a pattern instead of a word
   --  Scan_Comments    Allows the word to be matched in comments
   --  Scan_Strings     Allows the word to be matched in strings
   --  Scan_Statements  Allows the word to be matched in statements (everywhere
   --                   except in comments and strings)
   --
   --  Raise Search_Error if:
   --  * Look_For is empty, or can't compile
   --         ? XXX (iff it is a regexp) ?
   --  * Files is null
   --  * Nor Comments nor Strings nor Statements are scanned
   --
   --  ??? Searching " in Ada strings badly implemented: treated as 2 strings
   --  ??? Searching empty lines match none !

   procedure Init_Search
     (Search          : out Code_Search;
      Look_For        : String;
      Files_Pattern   : Regexp;
      Directory       : String  := "";
      Recurse         : Boolean := False;
      Match_Case      : Boolean := False;
      Whole_Word      : Boolean := False;
      Regexp          : Boolean := False;
      Scan_Comments   : Boolean := True;
      Scan_Strings    : Boolean := True;
      Scan_Statements : Boolean := True);
   --  Prepare searching for a word or a pattern in files selected by
   --  Files_Pattern, Directory and Recurse.
   --
   --  Files_Pattern  Pattern selecting files to be scanned (eg globbing *.ad?)
   --  Directory      Where files are selected, "" means current dir; absolute
   --                 or relative to current dir
   --              NOTE: used current dir is the one when Init_Search is called
   --  Recurse        Whether files may also be selected in direct and indirect
   --                 sub-directories
   --  ...            See first Init_Search procedure
   --
   --  Raise Search_Error if:
   --  * Look_For is empty, or can't compile
   --         ? XXX (iff it is a regexp) ?
   --  * Files_Pattern is uninitialized
   --  * Nor Comments nor Strings nor Statements are scanned
   --
   --  ??? Searching " in Ada strings badly implemented: treated as 2 strings
   --  ??? Searching empty lines match none !

   type Poll_Search_Handler is access function
     (Match_Found : Boolean;
      File        : String;
      Line_Nr     : Positive := 1;
      Line_Text   : String   := "") return Boolean;
   --  Function called after a match is found or after a whole file is scanned.
   --  An unopenable file is immediately considered as whole scanned.
   --  The search may be aborted at any call by returning False.
   --
   --  Match_Found  TRUE: a match is found...  FALSE: the whole scanned file...
   --  File               ... in this file...         ... is this one
   --  Line_Nr            ... at this line...          !! IRRELEVANT !!
   --  Line_Text          ... within this text         !! IRRELEVANT !!
   --
   --  Match calls are intended for, eg, updating a list of matches.
   --  Other calls are intended for, eg, updating a list of scanned files.
   --  But you can handle your own business during the call (user events,
   --  animate a search drawing, ...).
   --
   --  NOTE: The handler mustn't raise any exception !

--  !! DELETE >>>

   --  Match_Found  Whether a match is found (Line parameters are relevant) or
   --               it is only a check for aborting (irrelevant parameters).
   --  File         File containing the match or whole scanned
   --  Line_Nr      Matching line number (relevant iff Match_Found)
   --  Line_Text    Matching line text   (relevant iff Match_Found)

--  !! DELETE <<<

   procedure Do_Search
     (Search   : in out Code_Search;
      Callback : Poll_Search_Handler);
   --  Execute the search.
   --  The callback is called whenever a match is found and at the end of each
   --  file until all files are scanned or the callback ask for aborting.
   --  An unopenable file is immediately considered as whole scanned.
   --
   --  NOTE: The callback mustn't be null.

   procedure Free (S : in out Code_Search);
   --  Free the memory occupied by S.
   --
   --  NOTE: S.Files is not freed because it was given (see Init_Search).

private

   type Pattern_Matcher_Access is access Pattern_Matcher;

   type Recognized_Lexical_States is (Statements, Strings,
                                      Mono_Comments, Multi_Comments);
   --  Current lexical state of the currently parsed file

   type Code_Search is record
      Look_For        : String_Access := null;
      Pattern         : Pattern_Matcher_Access := null;

      Files           : Project_Files_Access := null;
      Files_Pattern   : Regexp;
      Directory       : String_Access := null;
      Recurse         : Boolean := False;

      Match_Case      : Boolean := False;
      Whole_Word      : Boolean := False;
      Regexp          : Boolean := False;

      Scan_Comments   : Boolean := True;
      Scan_Strings    : Boolean := True;
      Scan_Statements : Boolean := True;
      Lexical_State   : Recognized_Lexical_States;
   end record;

end Find_Utils;
