-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
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

with Boyer_Moore;
with GNAT.OS_Lib;
with GNAT.Regpat;
with Glide_Kernel;
with Glib.Object;
with Gtk.Widget;

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

   type Root_Search_Context is tagged limited private;

   procedure Set_Context
     (Context  : access Root_Search_Context'Class;
      Look_For : String;
      Options  : Search_Options);
   --  Create a new search context

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

   procedure Context_As_Boyer_Moore
     (Context : access Root_Search_Context;
      Matcher : out Boyer_Moore.Pattern);
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
     (Context : access Root_Search_Context; Buffer : String) return Integer;
   --  Check if Context matches Buffer, and return the index of the first
   --  match, or -1 if there is no match
   --  This automatically uses either a regexp or a faster Boyer Moore methode
   --  for constant strings.

   type Match_Result (Length : Natural) is record
      Index, Line, Column, End_Column : Natural;
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
     (Context    : access Root_Search_Context;
      Buffer     : String;
      Callback   : Scan_Callback;
      Ref_Index  : in out Integer;
      Ref_Line   : in out Integer;
      Ref_Column : in out Integer);
   --  Find matches of Context in Buffer, starting at Buffer'First, and until
   --  either the end of the buffer or Callback returns False.
   --  Buffer is assumes to be a single valid scope, and thus no scope handling
   --  is performed.
   --  The character at index Ref_Index in Buffer is assumed to correspond to
   --  position Ref_Line and Ref_Column in the original file. They are
   --  automatically updated when new positions are computed, so that they can
   --  be used during the next call to Scan_Buffer_No_Scope.

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

   function Search
     (Context         : access Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean is abstract;
   --  This subprogram should search for the next occurrence of Context.
   --  It should return False if there is no other search to be performed, True
   --  if a call to this function might lead to another occurrence of the
   --  search string.

   function Replace
     (Context         : access Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Search_Backward : Boolean) return Boolean;
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
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
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

   --------------------------------
   -- Registering search modules --
   --------------------------------

   type Search_Module_Data (Length : Natural) is record
      Label             : String (1 .. Length);
      Mask              : Search_Options_Mask;
      Factory           : Module_Search_Context_Factory;
      Extra_Information : Gtk.Widget.Gtk_Widget;
      Id                : Glide_Kernel.Module_Id;
   end record;
   --  If Extra_Information is not null, then it will be displayed every time
   --  this label is selected. It can be used for instance to ask for more
   --  information like a list of files to search.
   --  Whenever the data in Extra_Information changes, or for some reason the
   --  current status of GPS no longer permits the search, you should raise the
   --  kernel signal Search_Reset_Signal (or call Reset_Search below).
   --
   --  When the user then selects "Find", the function Factory is called to
   --  create the factory. The options and searched string or regexp will be
   --  set automatically on return of Factory, so you do not need to handle
   --  this.
   --  Mask indicates what options are relevant for that module. Options that
   --  are not set will be greyed out.
   --  If Supports_Replace if false, then the button will be greyed out.
   --
   --  Id can be left null. If not null, it will be used to set the default
   --  search context when the search dialog is popped up (the first
   --  search_module_data that matches the current module is used).

   No_Search : constant Search_Module_Data;

   procedure Register_Search_Function
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Data              : Search_Module_Data);
   --  Register a new search function.
   --  This will be available under the title Label in the search combo box.
   --  This emits the kernel signal "search_functions_changed".

   procedure Reset_Search
     (Object : access Glib.Object.GObject_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle);
   --  Raises the kernel signal Search_Reset_Signal. This is just a convenience
   --  callback function. Object is ignored, and can be anything.

   function Search_Context_From_Module
     (Id : access Glide_Kernel.Module_ID_Record'Class)
      return Find_Utils.Search_Module_Data;
   --  Return the first search context that matches Id, or No_Search if there
   --  is none.

private

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;

   type Match_Array_Access is access GNAT.Regpat.Match_Array;

   No_Result : constant Match_Result := (0, 0, 0, 0, 0, "");

   No_Search : constant Search_Module_Data :=
     (Length            => 0,
      Label             => "",
      Mask              => 0,
      Factory           => null,
      Id                => null,
      Extra_Information => null);

   type Root_Search_Context is tagged limited record
      Options        : Search_Options;
      Look_For       : GNAT.OS_Lib.String_Access := null;

      RE_Matcher     : Pattern_Matcher_Access := null;
      Sub_Matches    : Match_Array_Access := null;

      BM_Matcher     : Boyer_Moore.Pattern;
      BM_Initialized : Boolean := False;
   end record;

   type Search_Context is abstract new Root_Search_Context with null record;

end Find_Utils;
