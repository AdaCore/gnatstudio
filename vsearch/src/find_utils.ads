------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with GPS.Kernel;
with GPS.Search;    use GPS.Search;
with Gtk.Widget;
with Basic_Types; use Basic_Types;

package Find_Utils is

   -------------------
   -- Simple search --
   -------------------
   --  The following types are for use as simple search contexts, that can
   --  search both constant strings and regular expressions.

   type Root_Search_Context is abstract tagged limited private;
   type Root_Search_Context_Access is access all Root_Search_Context'Class;

   procedure Set_End_Notif_Done
     (Context : in out Root_Search_Context; Value : Boolean);
   function Get_End_Notif_Done (Context : Root_Search_Context) return Boolean;
   --  Set the state of the "end of search" notification for this context

   procedure Set_Pattern
     (Context        : access Root_Search_Context'Class;
      Pattern        : String;
      Whole_Word     : Boolean;
      Case_Sensitive : Boolean;
      Kind           : GPS.Search.Search_Kind);
   --  Sets the pattern to search for.
   --  Pattern is adopted by Context, and should not be freed by the caller.

   function Context_Look_For
     (Context : access Root_Search_Context) return String;
   --  Return the search string, or "" if Context does not contain any search
   --  string. If context is a regular expression, return the original regexp
   --  string if any, "" otherwise.

   function Context_Look_In
     (Self : Root_Search_Context) return String;
   --  Describe the current context (what files are searched,...)
   --  This is intended for display to the user.
   --  The returned string should start with a lower case, and continue a
   --  a sentence that ends with "in ...".

   function Is_Regexp (Context : access Root_Search_Context) return Boolean;
   --  Whether this is a search for regular expressions. This is used to
   --  find whether \1, \2... replacement should be supported.

   procedure Free (Context : in out Root_Search_Context);
   --  Free the memory allocated for Context.all. It doesn't free Context
   --  itself.

   function Match
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1)
      return GPS.Search.Search_Context;
   --  Check if Context matches Buffer (Start_Index .. End_Index), and return
   --  the index of the first match, or -1 if there is no match. Start_Index
   --  defaults to Buffer'First, and End_Index defaults to Buffer'Last.
   --  It is important that Buffer contains the whole file,
   --  since otherwise regular expressions starting with "^" or ending with "$"
   --  will not be properly recognized.
   --  This automatically uses either a regexp or a faster Boyer Moore methode
   --  for constant strings.

   procedure Matched_Subexpression
     (Result      : GPS.Search.Search_Context;
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

   type Scan_Callback is access function
     (Match : GPS.Search.Search_Context;
      Text  : String) return Boolean;
   --  Callback for a match in a buffer.
   --  Text is the full line that contains the matched text, where the latter
   --  has been highlighted. This is suitable for displaying in the Locations
   --  window.
   --  If it returns False, no more match will be checked.

   procedure Scan_Buffer_No_Scope
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Natural;
      End_Index   : Natural;
      Callback    : Scan_Callback;
      Ref         : in out Buffer_Position;
      Was_Partial : out Boolean);
   --  Find matches of Context in Buffer (Start_Index .. End_Index), and until
   --  either End_Index or Callback returns False.
   --  Note: it is important to pass the full file contents in Buffer, since
   --  otherwise regular expressions starting with "^" or ending with "$" will
   --  not workproperly.
   --  Ref is used to efficiently compute positions in the buffer, without
   --  necessarily restarting from the beginning each time. It is updated after
   --  each call, so that it can be reused in the next call to
   --  Scan_Buffer_No_Scope (provided the same Buffer is passed).
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

   Invalid_Context : exception;
   --  Raised when trying to access the components in Search_Context

   function Get_Current_Progress
     (Context : access Root_Search_Context) return Integer is (0);
   --  Return the current progress level in Context (ex: the number of file
   --  being searched. By default, return 0.

   function Get_Total_Progress
     (Context : access Root_Search_Context) return Integer is (1);
   --  Return the total progress level in Context (ex: the total number of
   --  files being searched). By default, return 1.

   procedure Reset
     (Context : access Root_Search_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Called whenever a new search will start (as opposed to continuing the
   --  current one through the Next button).

   procedure Search
     (Context         : access Root_Search_Context;
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
     (Context         : access Root_Search_Context;
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

   procedure Free (Context : in out Root_Search_Context_Access);
   --  Free the memory both for the pointer and for the internal fields. It
   --  dispatches to calls to Free for Files_Context

   ---------------
   -- Searching --
   ---------------

   type Module_Search_Context_Factory is access function
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget)
      return Root_Search_Context_Access;
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

   type Root_Search_Context is abstract tagged limited record
      Pattern        : GPS.Search.Search_Pattern_Access;
      --  The pattern matcher

      End_Notif_Done : Boolean := False;
      --  This variable is true when a notification has been sent to the
      --  user specifiying that the search in finished. This notification can
      --  be done in various places during the search project, but implementers
      --  should always check that is has not already been given before making
      --  it.
   end record;

end Find_Utils;
