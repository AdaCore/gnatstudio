-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
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

with Basic_Types;
with Boyer_Moore;
with Generic_List;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Regpat;
with Glide_Kernel;
with Gtk.Widget;

package Find_Utils is

   -------------
   -- Options --
   -------------

   type Search_Scope is (Whole,
                         Comments_Only,
                         Comments_And_Strings,
                         Strings_Only,
                         All_But_Comments);
   --  Scope wanted for the search.
   --  Whole scope means never use any context (i.e. files are whole scanned).
   --  This scope mostly applies to code editors.

   type Search_Options is record
      Scope          : Search_Scope := Whole;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Regexp         : Boolean := False;
   end record;
   --  Defines the context for a search.
   --  All_Occurences is set to True if the search should find all occurences
   --    of the match, instead of just the first one. This might be used for
   --    instance to decide on which algorithm to use
   --  Case_Sensitive is set to True if the search is case sensitive
   --  Whole_Word is set to True if only full words should match. For instance,
   --    it would not match "end" in "friend" if True. This also applies to
   --    regular expressions.
   --  Regexp is set to True if the search string should be interpreted as a
   --    regular expression.

   type Search_Options_Mask is mod 256;
   Scope_Mask      : constant Search_Options_Mask := 2 ** 0;
   Case_Sensitive  : constant Search_Options_Mask := 2 ** 1;
   Whole_Word      : constant Search_Options_Mask := 2 ** 2;
   Regexp          : constant Search_Options_Mask := 2 ** 3;
   All_Occurences  : constant Search_Options_Mask := 2 ** 4;
   Search_Backward : constant Search_Options_Mask := 2 ** 5;
   All_Options     : constant Search_Options_Mask := 255;

   --------------
   -- Contexts --
   --------------

   type Search_Context is tagged limited private;
   type Search_Context_Access is access all Search_Context'Class;
   --  Defines what the user is looking for.
   --  Although the user always specifies a string, it should sometimes be
   --  interpreted differently based on whether it is a regular expression,...
   --  The subprograms below will compute the fields as needed.

   procedure Set_Context
     (Context  : access Search_Context;
      Look_For : String;
      Options  : Search_Options);
   --  Create a new search context

   function Context_As_String (Context : access Search_Context) return String;
   --  Return the search string.
   --  Invalid_Context is raised if the user is in fact looking for a regular
   --  expression.

   function Context_As_Regexp
     (Context : access Search_Context) return GNAT.Regpat.Pattern_Matcher;
   --  Return the regular expression to match. The "whole word" and "case
   --  insensitive" options are automatically taken into account when computing
   --  the regular expression.
   --  Note that the regexp is cached for efficiency.
   --  Invalid_Context is raised if the user is not looking for a regular
   --  expression, or the regular expression is invalid.

   procedure Context_As_Boyer_Moore
     (Context : access Search_Context;
      Matcher : out Boyer_Moore.Pattern);
   --  Return the search string as a Boyer-Moore pattern.
   --  Matcher is computed on demand, and cached for efficiency. It
   --  automatically includes the "case insensitive" options.
   --  Invalid_Context is raised if the user is in fact looking for a regular
   --  expression.

   procedure Free (Context : in out Search_Context);
   --  Free the memory allocated for Context.all. It doesn't free Context
   --  itself.

   procedure Free (Context : in out Search_Context_Access);
   --  Free the memory both for the pointer and for the internal fields. It
   --  dispatches to calls to Free for Files_Context

   Invalid_Context : exception;
   --  Raised when trying to access the components in Search_Context

   -------------------
   -- Files context --
   -------------------

   type Files_Context is new Search_Context with private;
   type Files_Context_Access is access all Files_Context'Class;
   --  A special context for searching in a specific list of files

   procedure Set_File_List
     (Context       : access Files_Context;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : String  := "";
      Recurse       : Boolean := False);
   --  Set the list of files to search

   procedure Free (Context : in out Files_Context);
   --  Free the memory allocated for the context

   ---------------------------------
   --  Files From Project context --
   ---------------------------------

   type Files_Project_Context is new Search_Context with private;
   type Files_Project_Context_Access is access all Files_Project_Context'Class;

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : Basic_Types.String_Array_Access);
   --  Set the list of files to search.
   --  No copy of Files is made, the memory will be freed automatically.

   procedure Free (Context : in out Files_Project_Context);
   --  Free the memory allocated for the context

   ---------------
   -- Searching --
   ---------------

   type Module_Search_Context_Factory is access function
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Function called to create the search context.
   --  It should return null if it couldn't create the context, and thus if the
   --  search/replace won't be performed.
   --  The memory will be freed automatically by Glide2

   type Module_Search_Function is access function
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Context         : access Search_Context'Class;
      Search_Backward : Boolean) return Boolean;
   --  This subprogram should search for the next occurence of Context.
   --  It should return False if there is no other search to be performed, True
   --  if a call to this function might lead to another occurence of the search
   --  string.

   type Module_Replace_Function is access function
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Context         : access Search_Context'Class;
      Replace_String  : String;
      Is_First_Search : Boolean;
      Search_Backward : Boolean) return Boolean;
   --  This subprogram should search for the next occurence of Context. If
   --  Is_First_Search, the search should start from the beginning
   --  It should set Context to null when there is nothing more to replace.
   --  It should return False if there is no other search to be performed, True
   --  if a call to this function might lead to another occurence of the search
   --  string.

   ------------------------------
   --  Standard search support --
   ------------------------------

   function Current_File_Factory
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Current File"

   function Files_From_Project_Factory
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Files From Project"

   function Files_Factory
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Files..."

   function Search_Current_File
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Context         : access Search_Context'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Current File"

   function Search_Files_From_Project
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Context         : access Search_Context'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Files From Project"

   function Search_Files
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Context         : access Search_Context'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Files..."

private

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;

   type Match_Array_Access is access GNAT.Regpat.Match_Array;

   type Search_Context is tagged limited record
      Options        : Search_Options;
      Look_For       : GNAT.OS_Lib.String_Access := null;

      RE_Matcher     : Pattern_Matcher_Access := null;
      Sub_Matches    : Match_Array_Access := null;

      BM_Matcher     : Boyer_Moore.Pattern;
      BM_Initialized : Boolean := False;
   end record;

   type Dir_Data is record
      Name : GNAT.OS_Lib.String_Access;
      Dir  : GNAT.Directory_Operations.Dir_Type;
   end record;
   type Dir_Data_Access is access Dir_Data;
   procedure Free (D : in out Dir_Data_Access);
   package Directory_List is new Generic_List (Dir_Data_Access);

   type Files_Context is new Search_Context with record
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : GNAT.OS_Lib.String_Access := null;
      Recurse       : Boolean := False;
      Dirs          : Directory_List.List;
   end record;

   type Files_Project_Context is new Search_Context with record
      Files         : Basic_Types.String_Array_Access := null;
      Current_File  : Natural;
   end record;

end Find_Utils;
