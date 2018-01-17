------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with Gtk.Combo_Box_Text;
with Gtk.Widget;

with Basic_Types;                        use Basic_Types;
with GPS.Kernel;
with GPS.Kernel.Modules;                 use GPS.Kernel.Modules;
with GPS.Search;                         use GPS.Search;

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

   type Search_Occurrence_Record is abstract tagged private;
   type Search_Occurrence is access all Search_Occurrence_Record'Class;
   --  Type representing search occurrences.
   --
   --  This can be used to keep track of all the occurrences that have been
   --  found until now and to display one in particular (see the
   --  Highlight_Occurrence subprogram).

   function Is_Equal
     (Left  : not null access Search_Occurrence_Record;
      Right : not null access Search_Occurrence_Record) return Boolean
      is abstract;
   --  Return true when the given search occurrences are equal.

   procedure Initialize
     (Occurrence : not null access Search_Occurrence_Record'Class;
      Pattern    : String);
   --  Initialize the search occurrence with the search pattern used to match
   --  it.

   function Get_Pattern
     (Occurrence : not null access Search_Occurrence_Record'Class)
      return String;
   --  Get the search pattern used to match this given search occurrence.

   procedure Free (Occurrence : in out Search_Occurrence);
   --  Free the memory associated with the given search occurrence

   function Search
     (Context              : access Root_Search_Context;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean) return Search_Occurrence is abstract;
   --  This subprogram should search for the next occurrence of Context.
   --
   --  Found tells whether an occurrence of the context was found.
   --
   --  Continue is set to False if there is no other search to be performed and
   --  to True if a call to this function might lead to another occurrence of
   --  the search string.
   --
   --  If Give_Focus is true, then the widget that contains the match, in case
   --  the user was looking for a single match, should gain the focus.
   --  Otherwise, the focus shouldn't be changed and should remain on the
   --  search window
   --
   --  Return the search occurrence that has been found or null otherwise.

   procedure Search
     (Context              : access Root_Search_Context'Class;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean);
   --  Same as above, but does not return a search occurrence instead.

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

   --------------------
   -- Search Modules --
   --------------------

   type Scope_Selector_Interface is interface;
   type Scope_Selector is access all Scope_Selector_Interface'Class;
   --  Interface defining a common API for both simple scope selectors and
   --  the advanced ones (e.g: the ones asking for a specific set of files).

   procedure Initialize
     (Selector : not null access Scope_Selector_Interface;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is abstract;
   --  Initialize the Selector's widgets

   function Get_Scope_Combo
     (Selector : not null access Scope_Selector_Interface)
      return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text is abstract;
   --  Return the Selector's scope combo widget

   function Get_Optional_Widget
     (Selector : not null access Scope_Selector_Interface)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the Selector's optional widget container

   type Search_Module_Type is abstract tagged private;
   type Search_Module is
     access all Search_Module_Type'Class;
   --  Type used to represent modules that can be used for searching and/or
   --  replacing purposes.

   type Search_Options_Mask is mod 256;
   Case_Sensitive       : constant Search_Options_Mask := 2 ** 1;
   Whole_Word           : constant Search_Options_Mask := 2 ** 2;
   All_Occurrences      : constant Search_Options_Mask := 2 ** 3;
   Search_Backward      : constant Search_Options_Mask := 2 ** 4;
   Supports_Replace     : constant Search_Options_Mask := 2 ** 5;
   Supports_Incremental : constant Search_Options_Mask := 2 ** 6;
   All_Options          : constant Search_Options_Mask := 255;
   --  Used to know which options are supported by a given search module

   procedure Initialize
     (Module       : not null access Search_Module_Type;
      Label        : String;
      Selector     : access Scope_Selector_Interface'Class := null;
      Id           : access GPS.Kernel.Abstract_Module_ID_Record'Class := null;
      Mask         : Search_Options_Mask := All_Options;
      In_Selection : Boolean := False);
   --  Initialize the given search module.
   --
   --  Label is used to display the search module when listing the different
   --  search modules to the user. It can include %p for the current project
   --  (as understood by the search view).
   --
   --  If Selector is not null, then its associated widgets will be displayed
   --  every time this label is selected. It can be used for instance to ask
   --  for more information like a list of files to search.
   --  Whenever the data in the Selector widgets changes, or for some reason
   --  the current status of GPS no longer permits the search, you should raise
   --  the kernel signal Search_Reset_Signal (or call Reset_Search below).
   --
   --  Mask indicates what options are relevant for that module. Options that
   --  are not set will be greyed out. If Supports_Replace if false, then the
   --  button will be greyed out.
   --
   --  Id can be left null. If not null, it will be used to set the default
   --  search context when the search dialog is popped up (the first
   --  search_module_data that matches the current module is used).

   --  When Id is not null and In_Selection = True it will be used to set the
   --  default search context when there multiline selection in an editor.

   function Create_Context
     (Module          : not null access Search_Module_Type;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access is abstract;
   --  Function called to create the search context.
   --
   --  It should return null if it couldn't create the context, and thus if the
   --  search/replace won't be performed.
   --  The memory will be freed automatically by GPS.
   --  All_Occurrences is set to True if the search will be used to find all
   --  the possible occurrences the first time the user presses First. It could
   --  be used to provide a different algorithm or initial setup.
   --
   --  It shouldn't set the general information like the pattern and the
   --  replacement pattern, since these are set automatically.

   procedure Push_Occurrence
     (Module     : not null access Search_Module_Type;
      Occurrence : not null access Search_Occurrence_Record'Class);
   --  Push the given search occurrence into the search module's search
   --  occurrences stack..

   function Pop_Occurrence
     (Module : not null access Search_Module_Type)
      return Search_Occurrence;
   --  Pop the top element of the search module's search occurrences stack.

   function Get_Last_Occurrence
     (Module : not null access Search_Module_Type)
      return Search_Occurrence;
   --  Get the last search occurrence pushed into the module's search
   --  occurrences stack, without popping it.

   procedure Clear_Occurrences (Module : not null access Search_Module_Type);
   --  Clear the module's search occurrences stack, freeing the memory
   --  associated with the contained search occurrences.

   procedure Highlight_Occurrence
     (Module     : not null access Search_Module_Type;
      Occurrence : not null access Search_Occurrence_Record'Class) is null;
   --  Highlight the given search occurrence.
   --  This procedure needs to be overridden by any search module that is able
   --  to display a particular search occurrence (i.e: highlight the given
   --  search occurrence by selecting it in the currently selected editor).

   procedure Give_Focus_To_Occurrence
     (Module     : not null access Search_Module_Type;
      Occurrence : not null access Search_Occurrence_Record'Class) is null;
   --  Give the focus to the given search occcurrence.
   --  This procedure needs to be overridden by any search module that is able
   --  to give the focus to a particular search occurrence (i.e: give the focus
   --  to the given search occurrence by giving the focus to the editor that
   --  contains it and set the editor's cursor to the occurrence's location).

   function Get_Label
     (Module : not null access Search_Module_Type) return String;
   function Get_Scope_Selector
     (Module : not null access Search_Module_Type) return Scope_Selector;
   function Get_Id
     (Module : not null access Search_Module_Type) return Module_ID;
   function Get_In_Selection
     (Module : not null access Search_Module_Type) return Boolean;
   function Is_Option_Supported
     (Module : not null access Search_Module_Type;
      Option : Search_Options_Mask) return Boolean;
   --  Getters

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

   type Search_Occurrence_Record is abstract tagged record
      Pattern : Unbounded_String;
      --  The search pattern used to match this occurrence
   end record;

   package Search_Occurrences_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Search_Occurrence, "=");

   type Search_Module_Type is abstract tagged record
      Mask         : Search_Options_Mask;

      Selector     : Scope_Selector;
      --  Store the scope selector, which holds a reference to the scope
      --  selection widgets.
      --  We could have factories instead, but that means it would be harder
      --  to preserve the current extra information when users switch between
      --  contexts.

      Id           : Module_ID;
      Label        : Unbounded_String;
      In_Selection : Boolean;

      Search_Occurrences_Stack : Search_Occurrences_Lists.List;
      --  Search occurrences stack. Used to navigate in the module's search
      --  history.
   end record;

end Find_Utils;
