-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
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

--  Line-oriented pattern search in source code files.
--
--  This package implements a line-oriented searching for a word or a pattern
--  in given source code files.

--  Scope may be limited to: only comments, only strings, comments and strings,
--  everything but comments or no limitation (i.e. whole file is scanned).
--  String and comment delimiters are found from Language_Context (see file
--  language.ads). If no context is detected for a file, no scope limitation
--  occurs for this file.
--  Files may be given as a list or with a pattern and a directory.
--  Classic options included: match case, whole word.
--  Boyer-Moore algorithm is used when the literal string isn't longer than
--  Boyer_Moore.Max_Pattern_Length.

--  NOTES:
--  * Strings are assumed to end only with the string delimiter (they continue
--    across ASCII.LF).
--  * There is only 1 syntax level (e.g. strings aren't found within comments).
--  * Searching in empty files matches nothing.
--  * This package does *NOT* register any extension (see language.ads).
--  * ^ in a regexp means "start of the comment/string" when on the first line
--    of the comment/string; otherwise "start of the line".
--  * $ in a regexp means "end of the comment/string" when on the last line of
--    the comment/string; otherwise "end of the line".
--  * Don't use ^ or $ in a regexp when the scope is All_But_Comm, they have an
--    undefined behavior.
--  * Allowed chars in a word are found in g-regpat.ads.

with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Regexp; use GNAT.Regexp;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Boyer_Moore; use Boyer_Moore;

package Find_Utils is

   type Code_Search is private;
   --  Type representing a search in source code files

   type Project_Files is array (Positive range <>) of String_Access;
   --  Type used to store the project file list

   type Project_Files_Access is access Project_Files;
   --  Type to get the list of project files.

   type Search_Scope is (Whole, Comm_Only, Comm_Str, Str_Only, All_But_Comm);
   --  Scope wanted for the search. Comm is comments, str is strings.
   --  Whole scope means never use any context (i.e. files are whole scanned).

   Search_Error : exception;
   --  This exception is raised when trying to initialize a search with bad
   --  arguments.

   procedure Init_Search
     (Search     : out Code_Search;
      Look_For   : String;
      Files      : Project_Files_Access;
      Match_Case : Boolean := False;
      Whole_Word : Boolean := False;
      Regexp     : Boolean := False;
      Scope      : Search_Scope := Whole);
   --  Prepare searching for a word or a pattern in given file list.
   --
   --  Look_For         Searched word or single line pattern (see g-regpat.ads)
   --  Files            Files to be searched
   --                   NOTE: It's your responsability to free Files
   --  Match_Case       UPPER & lowercase letters are respected
   --  Whole_Word       Match only full words (eg 'end' not found in 'friend')
   --  Regexp           Look for a pattern instead of a word
   --  Scope            Scope of the search
   --
   --  Raise Search_Error if:
   --  * Look_For is empty, or can't compile
   --  * Files is null

   procedure Init_Search
     (Search        : out Code_Search;
      Look_For      : String;
      Files_Pattern : Regexp;
      Directory     : String  := "";
      Recurse       : Boolean := False;
      Match_Case    : Boolean := False;
      Whole_Word    : Boolean := False;
      Regexp        : Boolean := False;
      Scope         : Search_Scope := Whole);
   --  Prepare searching for a word or a pattern in files selected by
   --  Files_Pattern, Directory and Recurse.
   --
   --  Files_Pattern  Pattern selecting files to be scanned (eg globbing *.ad?)
   --  Directory      Where files are selected, "" means current dir; absolute
   --                 or relative to current dir
   --                 NOTE: used current dir is the current one when calling
   --                       Do_Search.
   --  Recurse        Whether files may also be selected in sub-directories
   --  ...            See first Init_Search procedure
   --
   --  Raise Search_Error if:
   --  * Look_For is empty, or can't compile
   --  * Files_Pattern is uninitialized

   type Poll_Search_Handler is access function
     (Match_Found : Boolean;
      File        : String;
      Line_Nr     : Positive    := 1;
      Line_Text   : String      := "";
      Sub_Matches : Match_Array := (0 => No_Match))
      return Boolean;
   --  Function called after a match is found or after a whole file is scanned.
   --  An unopenable file is immediately considered as scanned.
   --  The search may be aborted at any call by returning False.
   --
   --  Match_Found  True: a match is found...  False: the whole scanned file...
   --  File               ... in this file...         ... is this one
   --  Line_Nr            ... at this line...          !! IRRELEVANT !!
   --  Line_Text          ... within this text:        !! IRRELEVANT !!
   --  Sub_Matches        it matched there.            !! IRRELEVANT !!
   --
   --  Sub-matches indicate where sub-expressions matched in the regexp (see
   --  g-regpat.ads).
   --
   --  Match calls are intended for, e.g. updating a list of matches.
   --  Other calls are intended for, e.g. updating a list of scanned files.
   --  But you can perform your own processing during the call (user events,
   --  update a scroll bar, ...).
   --
   --  NOTES:
   --  * The handler mustn't raise any exception
   --  * Sub_Matches parameter is only filled when searching for a regexp.

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
   --  NOTE: see Init_Search.

private

   subtype RE_Pattern is Pattern_Matcher;
   subtype BM_Pattern is Boyer_Moore.Pattern;

   type RE_Pattern_Access is access RE_Pattern;

   type Match_Array_Access is access Match_Array;

   type Recognized_Lexical_States is
     (Statements, Strings, Mono_Comments, Multi_Comments);
   --  Current lexical state of the currently parsed file.
   --
   --  Statements      all but comments and strings
   --  Strings         string literals
   --  Mono_Comments   end of line terminated comments
   --  Multi_Comments  (possibly) multi-line comments

   type Code_Search is record
      Look_For      : String_Access := null;
      RE_Pat        : RE_Pattern_Access := null;
      BM_Pat        : BM_Pattern;
      Use_BM        : Boolean := False;

      Files         : Project_Files_Access := null;
      Files_Pattern : Regexp;
      Directory     : String_Access := null;
      Recurse       : Boolean := False;

      Match_Case    : Boolean := False;
      Whole_Word    : Boolean := False;
      Regexp        : Boolean := False;

      Scope         : Search_Scope;
      Lexical_State : Recognized_Lexical_States;
      Sub_Matches   : Match_Array_Access := null;
   end record;

end Find_Utils;
