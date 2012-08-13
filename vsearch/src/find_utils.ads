------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with GNATCOLL.Boyer_Moore;
with GNAT.Strings;
with GNAT.Regpat;
with Glib;
with GPS.Kernel;
with Gtk.Widget;

with Basic_Types; use Basic_Types;

package Find_Utils is

   -------------
   -- Options --
   -------------

   type Search_Options is record
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Regexp         : Boolean := False;
   end record;
   --  Defines the context for a search.
   --  Case_Sensitive is set to True if the search is case sensitive
   --  Whole_Word is set to True if only full words should match. For instance,
   --    it would not match "end" in "friend" if True. This also applies to
   --    regular expressions.
   --  Regexp is set to True if the search string should be interpreted as a
   --    regular expression.

   type Search_Options_Mask is mod 256;
   Case_Sensitive    : constant Search_Options_Mask := 2 ** 1;
   Whole_Word        : constant Search_Options_Mask := 2 ** 2;
   Regexp            : constant Search_Options_Mask := 2 ** 3;
   All_Occurrences   : constant Search_Options_Mask := 2 ** 4;
   Search_Backward   : constant Search_Options_Mask := 2 ** 5;
   Supports_Replace  : constant Search_Options_Mask := 2 ** 6;
   All_Options       : constant Search_Options_Mask := 255;

   -------------------
   -- Simple search --
   -------------------
   --  The following types are for use as simple search contexts, that can
   --  search both constant strings and regular expressions.

   type Root_Search_Context is abstract tagged limited private;

   procedure Set_End_Notif_Done
     (Context : in out Root_Search_Context; Value : Boolean);
   --  Set the state of the "end of search" notification for this context

   function Get_End_Notif_Done (Context : Root_Search_Context) return Boolean;
   --  Return the state of the "end of search" notification for this context

   procedure Set_Context
     (Context  : access Root_Search_Context'Class;
      Look_For : String;
      Options  : Search_Options);
   --  Create a new search context

   function Context_Look_For
     (Context : access Root_Search_Context) return String;
   --  Return the search string, or "" if Context does not contain any search
   --  string. If context is a regular expression, return the original regexp
   --  string if any, "" otherwise.

   function Context_As_String
     (Context : access Root_Search_Context) return String;
   --  Return the search string.
   --  Invalid_Context is raised if the user is in fact looking for a regular
   --  expression.

   function Context_As_Regexp
     (Context : access Root_Search_Context) return GNAT.Regpat.Pattern_Matcher;
   --  Return the regular expression to match. The "whole word" and "case
   --  insensitive" options are automatically taken into account when computing
   --  the regular expression.
   --  Note that the regexp is cached for efficiency.
   --  Invalid_Context is raised if the user is not looking for a regular
   --  expression, or the regular expression is invalid.

   function Context_Look_In
     (Self : Root_Search_Context) return String;
   --  Describe the current context (what files are searched,...)
   --  This is intended for display to the user.
   --  The returned string should start with a lower case, and continue a
   --  a sentence that ends with "in ...".

   procedure Context_As_Boyer_Moore
     (Context : access Root_Search_Context;
      Matcher : out GNATCOLL.Boyer_Moore.Pattern);
   --  Return the search string as a Boyer-Moore pattern.
   --  Matcher is computed on demand, and cached for efficiency. It
   --  automatically includes the "case insensitive" options.
   --  Invalid_Context is raised if the user is in fact looking for a regular
   --  expression.

   function Get_Options (Context : access Root_Search_Context)
      return Search_Options;
   --  Return the options currently set for the context

   procedure Free (Context : in out Root_Search_Context);
   --  Free the memory allocated for Context.all. It doesn't free Context
   --  itself.

   function Match
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Positive := Positive'Last) return Integer;
   --  Check if Context matches Buffer (Start_Index .. End_Index), and return
   --  the index of the first match, or -1 if there is no match. Start_Index
   --  defaults to Buffer'First, and End_Index defaults to Buffer'Last.
   --  It is important that Buffer contains the whole file,
   --  since otherwise regular expressions starting with "^" or ending with "$"
   --  will not be properly recognized.
   --  This automatically uses either a regexp or a faster Boyer Moore methode
   --  for constant strings.

   procedure Matched_Subexpressoin
     (Context     : access Root_Search_Context;
      Index       : Natural;
      First       : out Natural;
      Last        : out Natural);
   --  Return regexp subexpression slice for last matched regexp search

   type Operation_Kind is (Replace, Search);

   function Get_Terminate_Message
     (Context : access Root_Search_Context;
      Kind    : Operation_Kind) return String;
   --  The implementer of this function is supposed to return a message that
   --  will be displayed once the search or replace is done. By default, it
   --  return "".
   --  ??? Currently, only the Replace kind is taken into account.

   type Match_Result (Length : Natural) is record
      Index                : Natural;
      Begin_Line           : Natural;
      Begin_Column         : Character_Offset_Type;
      Visible_Begin_Column : Visible_Column_Type;
      End_Line             : Natural;
      End_Column           : Character_Offset_Type;
      Visible_End_Column   : Visible_Column_Type;
      Pattern_Length       : Natural;
      Text : String (1 .. Length);
   end record;
   --  The result of a match. This is a discriminated type so that we don't
   --  have to worry who is responsible to free it.

   No_Result : constant Match_Result;

   type Scan_Callback is access
     function (Match : Match_Result) return Boolean;
   --  Callback for a match in a buffer.
   --  If it returns False, no more match will be checked.

   procedure Scan_Buffer_No_Scope
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Natural;
      End_Index   : Natural;
      Callback    : Scan_Callback;
      Ref_Index   : in out Integer;
      Ref_Line    : in out Natural;
      Ref_Column  : in out Character_Offset_Type;
      Was_Partial : out Boolean);
   --  Find matches of Context in Buffer (Start_Index .. End_Index), and until
   --  either End_Index or Callback returns False.
   --  Note: it is important to pass the full file contents in Buffer, since
   --  otherwise regular expressions starting with "^" or ending with "$" will
   --  not workproperly.
   --  Buffer (Start_Index .. End_Index) is assumes to be a single valid scope,
   --  and thus no scope handling is performed.
   --  The character at index Ref_Index in Buffer is assumed to correspond to
   --  position Ref_Line and Ref_Column in the original file. They are
   --  automatically updated when new positions are computed, so that they can
   --  be used during the next call to Scan_Buffer_No_Scope.
   --
   --  On exit, Was_Partial is set to True if the search was interrupted
   --  because the callback returned False.

   procedure Find_Closest_Match
     (Buffer         : String;
      Line           : in out Natural;
      Column         : in out Character_Offset_Type;
      Found          : out Boolean;
      Str            : String;
      Case_Sensitive : Boolean);
   --  Find the occurence of Str in Buffer closest to (Line, Column).
   --  The latter are modified to point to the closest location if Found is
   --  set, otherwise it is unchanged.

   procedure To_Line_Column
     (Buffer         : Glib.UTF8_String;
      Pos            : Natural;
      Line           : in out Natural;
      Column         : in out Character_Offset_Type;
      Visible_Column : in out Visible_Column_Type;
      Line_Start     : in out Natural);
   --  Set Line and Column to the appropriate for the Pos-th character in
   --  Buffer.
   --  Buffer is expected to be UTF-8.

   Invalid_Context : exception;
   --  Raised when trying to access the components in Search_Context

   --------------
   -- Contexts --
   --------------

   type Search_Context is abstract new Root_Search_Context with private;
   type Search_Context_Access is access all Search_Context'Class;
   --  Defines what the user is looking for.
   --  Although the user always specifies a string, it should sometimes be
   --  interpreted differently based on whether it is a regular expression,...
   --  The subprograms below will compute the fields as needed.

   function Get_Current_Progress
     (Context : access Search_Context) return Integer;
   --  Return the current progress level in Context (ex: the number of file
   --  being searched. By default, return 0.

   function Get_Total_Progress
     (Context : access Search_Context) return Integer;
   --  Return the total progress level in Context (ex: the total number of
   --  files being searched). By default, return 1.

   procedure Reset
     (Context : access Search_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Called whenever a new search will start (as opposed to continuing the
   --  current one through the Next button).
   --  By default, this does nothing

   procedure Search
     (Context         : access Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean) is abstract;
   --  This subprogram should search for the next occurrence of Context.
   --  Found tells whether an occurrence of the context was found.
   --  Continue is set to False if there is no other search to be performed and
   --  to True if a call to this function might lead to another occurrence of
   --  the search string.
   --  If Give_Focus is true, then the widget that contains the match, in case
   --  the user was looking for a single match, should gain the focus.
   --  Otherwise, the focus shouldn't be changed and should remain on the
   --  search window

   function Replace
     (Context         : access Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean;
   --  This subprogram should search for the next occurrence of Context. If
   --  Is_First_Search, the search should start from the beginning
   --  It should set Context to null when there is nothing more to replace.
   --  It should return False if there is no other search to be performed, True
   --  if a call to this function might lead to another occurrence of the
   --  search string.
   --  The default implementation does nothing.

   procedure Free (Context : in out Search_Context_Access);
   --  Free the memory both for the pointer and for the internal fields. It
   --  dispatches to calls to Free for Files_Context

   ---------------
   -- Searching --
   ---------------

   type Module_Search_Context_Factory is access function
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Function called to create the search context.
   --  It should return null if it couldn't create the context, and thus if the
   --  search/replace won't be performed.
   --  The memory will be freed automatically by GPS.
   --  All_Occurrences is set to True if the search will be used to find all
   --  the possible occurrences the first time the user presses First. It could
   --  be used to provide a different algorithm or initial setup.
   --
   --  It shouldn't set the general information like the pattern and the
   --  replacement pattern, since these are set automatically.

private

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;

   type Match_Array_Access is access GNAT.Regpat.Match_Array;

   No_Result : constant Match_Result := (0, 0, 0, 0, 0, 0, 0, 0, 0, "");

   type Root_Search_Context is tagged limited record
      Options        : Search_Options;
      Look_For       : GNAT.Strings.String_Access := null;

      RE_Matcher     : Pattern_Matcher_Access := null;
      Sub_Matches    : Match_Array_Access := null;

      BM_Matcher     : GNATCOLL.Boyer_Moore.Pattern;
      BM_Initialized : Boolean := False;

      End_Notif_Done : Boolean := False;
      --  This variable is true when a notification has been sent to the
      --  user specifiying that the search in finished. This notification can
      --  be done in various places during the search project, but implementers
      --  should always check that is has not already been given before making
      --  it.
   end record;

   type Search_Context is abstract new Root_Search_Context with null record;

end Find_Utils;
