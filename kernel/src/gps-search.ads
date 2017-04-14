------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

--  This package describes the base types used for the various search and
--  completion engines in GPS.

with Ada.Strings.Unbounded;
with Basic_Types;   use Basic_Types;
with GNAT.Regpat;
with GNAT.Strings;
with GNATCOLL.Xref;
private with Ada.Containers.Doubly_Linked_Lists;

package GPS.Search is
   use type GNATCOLL.Xref.Visible_Column;

   type Search_Kind is (Full_Text, Regexp, Fuzzy, Approximate);
   --  A Full_Text match searches the pattern exactly in the contents.
   --
   --  A regexp parses the pattern as a regular expression.
   --
   --  A fuzzy match will search for some contents that contains all the
   --  characters of the pattern, in the same order, but possibly with
   --  other characters in-between. The number of characters in-between is not
   --  limited, so this mode really only makes sense when matching short text
   --  (and not, for instance, in text editors).
   --
   --  Approximate allows one or two errors to appear in the match (character
   --  insertion, deletion or substitution). This is mostly suitable when
   --  matching in long texts. The implementation of this algorithm is
   --  optimized so that characters are matched only once, but the total length
   --  of the pattern is limited to 64 characters. The exact number of errors
   --  depends on the length of the pattern:
   --      patterns of length <= 4  => no error allowed
   --      patterns of length <= 10 => one error allowed
   --      long patterns            => up to two errors

   function Get_Label (Kind : Search_Kind) return String;
   --  Return a suitable label for the given search kind
   --  (e.g : Fuzzy -  > "Fuzzy matching").

   Max_Capturing_Groups : constant := 10;
   --  Maximum number of capturing parenthesis groups for which we want to
   --  store the range in the search_context.

   -------------
   -- Matcher --
   -------------

   type Search_Pattern is abstract tagged private;
   type Search_Pattern_Access is access all Search_Pattern'Class;
   --  A type used to describe the search context.
   --  It can also be used to do the actual matching using the appropriate
   --  algorithm, depending on the search kind.

   type Buffer_Position is record
      Index : Integer;   --  Index in the buffer string, in bytes
      Line  : Natural;   --  line corresponding to this index (starting at 1)
      Column         : Character_Offset_Type;  --  column for this index
      Visible_Column : Visible_Column_Type; --  visible column for this index
   end record;
   Unknown_Position : constant Buffer_Position;
   --  This record describes a position in the buffer, and its mapping to
   --  user-visible line and columns.
   --  This is used for efficiency, to avoid recomputing these line/column
   --  information from the beginning of the buffer every time.

   function Image (Pos : Buffer_Position) return String;
   --  ??? MANU temporary
   --  NICO likes this

   subtype RGB_String is String (1 .. 7);
   Blue : constant RGB_String := "#0000ff";

   function Get_Default_Fg return RGB_String;
   --  Convenience function to get the default foreground for search highlight

   type Search_Context is record
      Start, Finish : Buffer_Position;
      --  Locations of start and end of the current match. When the match is
      --  an empty string set Finish to Unknown_Position.

      Score         : Natural;
      --  The score for the current match

      Buffer_Start  : Integer;
      Buffer_End    : Integer;
      --  The range of the buffer that we are searching

      Ref           : Buffer_Position;
      --  last known position in the buffer

      Groups        : GNAT.Regpat.Match_Array (0 .. Max_Capturing_Groups);
      --  The parenthesis groups that matched. This is only set when matching
      --  a regexp.

      Color_String  : RGB_String := Blue;
   end record;
   No_Match : constant Search_Context;
   --  The current state for a search matcher

   function Tag (Self : Search_Context; Text : String) return String;
   --  Utility function, used to tag a string with pango markup for
   --  highlighting search results.

   function Failed (Self : Search_Context) return Boolean
      is (Self.Start.Index = -1);
   --  Whether Self failed to match. This is somewhat equivalent to comparing
   --  with No_Match, but is more efficient and does not require a
   --  "use type Search_Context.

   function Is_Empty_Match (Self : Search_Context) return Boolean
      is (Self.Finish.Index <= 0);
   --  Whether Self matches an empty string

   function Index_After_Match (Self : Search_Context) return Positive;
   --  Return index of position just after end of match

   procedure Matched_Subexpression
     (Result      : Search_Context;
      Index       : Natural;
      First       : out Natural;
      Last        : out Natural);
   --  Return regexp subexpression slice for last matched regexp search

   procedure Free (Self : in out Search_Pattern);
   procedure Free (Self : in out Search_Pattern_Access);
   --  Free the memory used by the matcher

   overriding function "=" (P1, P2 : Search_Pattern) return Boolean;
   function Equals (P1, P2 : Search_Pattern_Access) return Boolean;
   --  Compares the two patterns

   function Get_Allow_Highlights
     (Self  : not null access Search_Pattern'Class) return Boolean;

   function Build
     (Pattern         : String;
      Case_Sensitive  : Boolean := False;
      Whole_Word      : Boolean := False;
      Negate          : Boolean := False;
      Kind            : Search_Kind := Full_Text;
      Allow_Highlight : Boolean := False)
      return Search_Pattern_Access;
   --  Create a new search matcher.
   --  It can be shared among multiple search providers, since it does not
   --  embed any context.
   --  Allow_Highlight should be True if results should highlight the matching
   --  characters.
   --  If the kind is a regular expression, but the pattern is invalid, this
   --  function falls back to a Full_Text search.
   --  If Negate is true, the search_pattern will always return No_Match if
   --  the pattern in fact matches the text (and a match at position 1 if the
   --  pattern in fact does not match).

   function Build
      (Pattern    : not null access Search_Pattern'Class;
       Text       : String) return Search_Pattern_Access;
   function Build
      (Pattern    : not null access Search_Pattern'Class;
       Kind       : Search_Kind) return Search_Pattern_Access;
   --  Allocates a new pattern, preserving the attributes of pattern,
   --  except the ones given in parameter.
   --  In particular, this can be used to detect particular values in the
   --  pattern, like "filename:line" where only the filename part should
   --  be searched.

   function Build_If_Needed
     (Pattern    : not null access Search_Pattern'Class;
      Kind       : Search_Kind;
      New_Kind   : Search_Kind;
      Built      : out Boolean) return Search_Pattern_Access;
   --  If Pattern.Kind = Kind, allocates a new pattern, preserving the
   --  attributes of pattern, except for Pattern.Kind which is set to New_Kind.
   --  If Pattern.Kind /= Kind, return Pattern.
   --  Built is set to True if a newly allocated pattern is returned and set to
   --  False otherwise.

   function Get_Text
      (Pattern    : not null access Search_Pattern'Class) return String;
   function Get_Kind
     (Pattern    : not null access Search_Pattern'Class) return Search_Kind;
   function Get_Case_Sensitive
     (Pattern    : not null access Search_Pattern'Class) return Boolean;
   function Get_Whole_Word
     (Pattern    : not null access Search_Pattern'Class) return Boolean;
   function Get_Negate
     (Pattern    : not null access Search_Pattern'Class) return Boolean;
   --  Return the text searched by the user.

   function Start
     (Self        : Search_Pattern;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position)
      return Search_Context
      is abstract;
   --  Start searching for Self in Buffer (Start_Index .. End_Index).
   --  Note: it is important to pass the full file contents in Buffer, since
   --  otherwise regular expressions starting with "^" or ending with "$" will
   --  not workproperly.
   --  Start_Index and End_Index default to the string bounds.
   --
   --  Ref provides a reference point for the computation of line/column
   --  information. It is assumed to be located before Start_Index.
   --
   --  Return value is No_Match if the Buffer did not match.

   procedure Next
     (Self    : Search_Pattern;
      Buffer  : String;
      Context : in out Search_Context) is abstract;
   --  Find the next occurrence of Self in Buffer.
   --  Buffer must be the same that was passed to Start (same bounds for
   --  instance).
   --
   --  Context is set to No_Match if the Buffer did not match.

   function Search_Best_Match
     (Self    : not null access Search_Pattern'Class;
      Buffer  : String) return Search_Context;
   --  Search for the best occurence (highest score) of Self in buffer and
   --  return it.
   --  No_Match is returned if the Buffer did not match.

   function Highlight_Match
      (Self      : Search_Pattern;
       Buffer    : String;
       Context   : Search_Context) return String;
   --  Return a copy of Buffer where the substring or characters matching
   --  Context are highlighted.
   --  Buffer must the same one passed to Start or Next, or a substring of it.

   procedure Compute_Suffix
     (Self        : Search_Pattern;
      Context     : Search_Context;
      Text        : String;
      Suffix      : in out Ada.Strings.Unbounded.Unbounded_String;
      Suffix_Last : in out Natural);
   --  Computes the common suffix between the substring following the part of
   --  Text matched by the context and Suffix (Suffix'First .. Suffix_Last).
   --  Changes Suffix_Last as needed.

   ------------
   -- Result --
   ------------

   type Search_Provider is abstract tagged record
      Rank  : Positive := 100;
      Count : Natural := 0;
      Enabled : Boolean := True;
   end record;
   type Search_Provider_Access is access all Search_Provider'Class;
   --  Instances of this type will look for matches of a given pattern, in a
   --  given context.
   --  Each search dialog, completion window or entry field in GPS will create
   --  its own providers, so the life of the providers might be relatively
   --  short.
   --  A provider might do some caching, for instance to optimize the searching
   --  when the pattern is augmented.
   --  Rank is the order in which the user wants to sort the result (1 appears
   --  first,...)
   --  Count is the number of matches (non-null result) since the last call to
   --  Set_Pattern. It must set by the caller to Set_Pattern and Next, not by
   --  each provider, if the caller intends to use it.
   --
   --  Enabled is only relevant for the global search field, and indicates
   --  whether this provider should be used.

   type Search_Result is abstract tagged record
      Score    : Integer := 100;
      Short    : GNAT.Strings.String_Access;
      Long     : GNAT.Strings.String_Access;
      Id       : GNAT.Strings.String_Access;
      Provider : access Search_Provider'Class;  --  do not free
   end record;
   type Search_Result_Access is access all Search_Result'Class;
   --  This type describes a match, as would be displayed in a dialog or a
   --  popup window, or the Locations window.
   --
   --  Short will be used as the first line to describe the result in a dialog.
   --  For instance, it would be the base name of a file (when matching file
   --  names), or the name of an entity.
   --
   --  Long is a more complete, one-line description. It is generally displayed
   --  on the second line, and could for instance be the full path of a file,
   --  or the location for the declaration of an entity.
   --  Long might be left to null.
   --
   --  Short and Long can embed simple pango markup if they need to highlight
   --  part of the string in a gtk+ dialog.
   --
   --  Id is a unique id for the search result, so that the next time the
   --  completion entries is brought up, it can be prefilled with this value.
   --  It is also used to recognize when the proposal was previously selected
   --  and then increase its score. It can point to the same value as Short
   --  or Long, this will not result in double-deallocation.
   --  If unset, the completion entry will not add this proposal to its
   --  history.
   --
   --  The lifetime of a search_result might be much longer than that of the
   --  search_provider that created it. As such, the result should not embed
   --  a pointer to the provider.
   --
   --  Score is used to sort the results within a given provider. Its specific
   --  values are only relevant within the context of that provider (although
   --  scores should be less than 900_000 to keep the various providers
   --  separated in the global search results).

   procedure Free (Self : in out Search_Result);
   procedure Free (Self : in out Search_Result_Access);
   --  Free the memory used by Self.

   function Can_Display_In_Locations
     (Self : not null access Search_Result) return Boolean is (False);
   --  Whether this result can be displayed in the Locations window.

   procedure To_Message (Self : not null access Search_Result) is null;
   --  Insert Self in the locations window, by creating a message.
   --  It could for instance call
   --  GPS.Kernel.Messages.Simple.Create_Simple_Message.
   --  This function does not return the message itself, since this is specific
   --  to GPS.

   procedure Execute
     (Self       : not null access Search_Result;
      Give_Focus : Boolean) is abstract;
   --  Execute the action for Self, when the user selects it in a dialog. For
   --  instance, when search file names, the action could be to open a new
   --  editor for this file; in the completion window, it might be to insert
   --  the name of the entity at the current location; or when searching in
   --  the current editor it might be to jump to the location of Self.
   --
   --  If Give_Focus is true, then the widget that contains the match, in case
   --  the user was looking for a single match, should gain the focus.
   --  Otherwise, the focus shouldn't be changed and should remain on the
   --  search window.

   procedure Replace
     (Self            : not null access Search_Result;
      Replace_With    : String;
      Case_Preserving : Boolean) is null;
   --  Replace the text matched by Self with Replace_With.
   --  This might not have an effect, depending on the type of result.
   --  If the user's replacement string contained references to parenthesis
   --  groups, they have already been replaced when calling this procedure.
   --
   --  If Case_Preserving is True, this procedure might transform Replace_With.
   --  For instance, if the text to be replaced was all lower-cased,
   --  upper-cased or capitalized, so will Replace_With.

   ---------------
   -- Providers --
   ---------------

   procedure Free (Self : in out Search_Provider) is null;
   procedure Free (Self : in out Search_Provider_Access);
   --  Free the memory used by Self.

   function Display_Name
      (Self : not null access Search_Provider) return String is abstract;
   --  Return the name of the provider, as should be displayed to the user.

   function Documentation
      (Self : not null access Search_Provider) return String is ("");
   --  The documentation for this provider. This explains what pattern is
   --  supported, where the search occurs,...

   procedure Set_Pattern
     (Self    : not null access Search_Provider;
      Pattern : not null access Search_Pattern'Class;
      Limit   : Natural := Natural'Last) is abstract;
   --  Sets the pattern to search for in Self's context.
   --  Self might be optimized in case the new pattern is similar to the
   --  previous one, to reuse some of the results.
   --  This procedure might be called several times in the lifetime of Self,
   --  possibly each time the user presses a key.
   --
   --  Limit might be used by GPS to indicate it will never try to fetch more
   --  than that many results. For instance, if it knows it will display a
   --  dialog with at most four lines per context (as spotlight does), it
   --  might pass that information to the context which might chose a different
   --  strategy. When searching for entities in the database, we might limit
   --  ourselves to the first four matches which might result in a more
   --  efficient SQL query.
   --
   --  Pattern must not be freed by Self, it belongs to the search dialog.

   procedure Next
     (Self     : not null access Search_Provider;
      Result   : out Search_Result_Access;
      Has_Next : out Boolean) is abstract;
   --  Returns the next match.
   --  The result might be set to null if Self did not find any occurrence,
   --  but should be called again the next time GPS is idle (in fact, Next
   --  should be called until Has_Next returns False or enough results have
   --  been retrieved).
   --
   --  Search start location depends on the context. When searching in
   --  the current file, it is the location of the cursor (this is
   --  therefore GPS specific). When searching in the list of files,
   --  start might search at the beginning of the list.
   --
   --  Calling Set_Pattern does not restart from the beginning
   --  necessarily. For instance, to implement the interactive-search in
   --  an editor, the behavior is more complex:
   --      * search for "p" jumps to the next occurrence of "p".
   --      * then searching for "pa" jumps to the following occurrence
   --        of "pa" after the now current location. This might skip
   --        a number of occurrences of "p".
   --      * but deleting "a" and searching again for "p" should
   --        restart from the last position where "p" matched, so that
   --        if we now search for "po" we might find an occurrence of
   --        "po" before the occurrence of "pa" we found earlier.
   --
   --  In this case, the context needs to maintain a list of locations
   --  for each pattern that was searched to properly restart at the
   --  right place.
   --
   --  GPS might do two things with the result: either display it in a
   --  dialog, so that the user has multiple results to chose from, or
   --  immediately jumping to that result (as would be the case for an
   --  interactive search).
   --
   --  Some Search_Provider might chose to aggregate results. For instance,
   --  the context used to return the possible entity completions in editors
   --  will aggregate all results with the same name (so that there is a
   --  single entry with "Put_Line", which has a full description equal to
   --  the concatenation of all the entities called Put_Line). It is the
   --  responsability of the context to do the aggregation, not of the
   --  search window.

   procedure On_Result_Executed
      (Self   : not null access Search_Provider;
       Result : not null access Search_Result'Class) is null;
   --  Called when a user has executed Result. It might be used to do various
   --  cleanups or changes in the provider, for instance storing the list of
   --  recent items selected by the user so that the scores can be modified
   --  later on.

   function Complete_Suffix
     (Self      : not null access Search_Provider;
      Pattern   : not null access Search_Pattern'Class)
      return String is ("");
   --  Used to implement completion through <tab>.
   --  Returns the possible completion for pattern.

   --------------
   -- Registry --
   --------------

   type Search_Provider_Registry is tagged private;
   type Search_Provider_Registry_Access is
     access all Search_Provider_Registry'Class;

   procedure Register
     (Self     : in out Search_Provider_Registry;
      Template : not null access Search_Provider'Class);
   --  Register a type of search provider.
   --  Name can be used to retrieve it later (in which case, a copy of Template
   --  will be returned).
   --  It is valid to override a predefined provider.

   procedure Sort_Providers (Self : in out Search_Provider_Registry);
   --  Sort the providers depending on their user-assigned rank. This is done
   --  by default every time Register is called, but should be called when the
   --  ranks are edited.

   function Get
     (Self : Search_Provider_Registry;
      Name : String) return Search_Provider_Access;
   --  Retrieve a copy of the registered provider with this name (or null if
   --  there is no such registered provider.

   function Get
     (Self : Search_Provider_Registry;
      N    : Positive) return Search_Provider_Access;
   --  Retrieve a provider by rank

   procedure Free
     (Self : in out Search_Provider_Registry_Access);
   --  Free all the providers registered in Self.

private

   type Search_Pattern is abstract tagged record
      Text           : GNAT.Strings.String_Access;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Kind           : Search_Kind := Full_Text;
      Allow_Highlight : Boolean := False;
      Negate         : Boolean := False;
   end record;

   Unknown_Position : constant Buffer_Position := (-1, 0, 0, -1);

   No_Match : constant Search_Context :=
     (Start              => Unknown_Position,
      Finish             => Unknown_Position,
      Score              => 0,
      Buffer_Start       => -1,
      Buffer_End         => -1,
      Ref                => Unknown_Position,
      Groups             => (others => GNAT.Regpat.No_Match),
      Color_String       => <>);

   type Provider_Info is record
      Provider : Search_Provider_Access;
   end record;

   package Provider_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Provider_Info);

   type Search_Provider_Registry is tagged record
      Providers : Provider_Lists.List;
   end record;

end GPS.Search;
