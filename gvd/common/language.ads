-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Regpat;
with Odd.Types;

package Language is

   type Language_Root is abstract tagged private;
   type Language_Access is access all Language_Root'Class;

   Unexpected_Type : exception;

   procedure Free (Lang : in out Language_Access);
   --  Free the memory pointed to by Lang and set it to null.

   ------------------------
   -- Types manipulation --
   ------------------------
   --  The following functions are provided to manipulate types and variables
   --  for each language.

   function Is_Simple_Type
     (Lang : access Language_Root; Str : String) return Boolean is abstract;
   --  Return True if Str is a simple type, like integer, ...
   --  These are the types that don't need information from the debugger to
   --  be known, ie we can save a call to the debugger when parsing the value
   --  of a variable.

   --------------------------------
   -- Highlighting in the editor --
   --------------------------------

   type Language_Entity is
     (Normal_Text, Keyword_Text, Comment_Text, String_Text);
   --  The entities found in a language, and that can have a different scheme
   --  for colors highlighting.

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      Entity    : out Language_Entity;
      Next_Char : out Positive);
   --  Should return the type of entity that is present at the first position
   --  in the buffer (ie starting at Buffer'First).
   --  Next_Char should be set to the index of the first character after the
   --  entity.

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Language_Root;
      Name : String) return String is abstract;
   --  Return the name to use to dereference Name (ie in Ada "Name.all", in
   --  C "*Name", ...). Note that Name can be a composite name (Name.Field),
   --  and thus might have to be protected with parentheses.

   function Array_Item_Name
     (Lang  : access Language_Root;
      Name  : String;
      Index : String) return String is abstract;
   --  Return the name to use to access a specific element of an array.
   --  Index is a comma-separated list of the indexes for all the dimensions,
   --  as in "1,2".

   function Record_Field_Name
     (Lang  : access Language_Root;
      Name  : String;
      Field : String) return String is abstract;
   --  Return the name to use for a specific field of a record.

   ------------------
   -- The explorer --
   ------------------
   --  These functions are provided as a support for the source code explorer.

   type Category_Index is new Positive;
   type Make_Entry_Func is access function
     (Str      : String;
      Matched  : GNAT.Regpat.Match_Array;
      Category : access Category_Index)
     return String;
   --  Function that builds the string to be inserted in the tree.
   --  It is possible for the function to change the category used for the
   --  item (for instance when subprograms declarations and bodies have
   --  basically the same aspect, it is possible to use only one regular
   --  expression and distinguish only by testing for some special substring
   --  in this function.

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   type Explorer_Category is record
      Name           : Odd.Types.String_Access;
      Regexp         : Pattern_Matcher_Access;
      Position_Index : Natural;
      Icon           : Odd.Types.Pixmap_Access;
      Make_Entry     : Make_Entry_Func;
   end record;
   --  Definition for a category (ie one of the subtrees of the explorer).
   --  Icon is the icon to use for items in this category.
   --  Regexp is the general regular expression to use for entries in this
   --  category, while Make_Entry is the function that will return the
   --  actual string to be displayed in the explorer.
   --  Position_Index is the index of the parenthesis-pair that the entity
   --  name starts at. When the user clicks on this item in the explorer,
   --  the cursor will be moved to that location in the editor.
   --
   --  If Make_Entry is null, then the regexp is never tested against the
   --  source code. You can only add items to this category by modifying the
   --  Category parameter of another category's Make_Entry (see
   --  language-debugger-ada.adb).

   type Explorer_Categories is array (Category_Index range <>)
     of Explorer_Category;
   --  A list of categories. Each category is assigned an internal number which
   --  is the index in this table, and is passed to each Make_Entry_Func
   --  functions.

   function Explorer_Regexps (Lang : access Language_Root)
                             return Explorer_Categories;
   --  Return the list of categories for a given language.
   --  By default, no category is defined, and thus the explorer is empty.

   function Is_System_File
     (Lang : access Language_Root;
      File_Name : String)
     return Boolean;
   --  Return True if File_Name is the name of a system file (standard include
   --  files in C or run-time file in Ada). These files are displayed
   --  separately in the explorer.

   ------------------------
   -- Exception Handling --
   ------------------------

   function Break_Exception
     (Debugger  : access Language_Root;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String;
   --  Return the command used in the current debugger/language to break
   --  on exception. If name is null, break should occur on all exceptions.
   --  The default implementation returns a null String.

   ----------------------
   -- Special commands --
   ----------------------

   function Start
     (Debugger  : access Language_Root)
     return String;
   --  Return the command used in the current debugger to start the program
   --  and stop on the first line of user's code.
   --  The resulting string can be either null (in which case this function
   --  is not available for the given debugger/language combination), a single
   --  command, or multiple commands separated by ASCII.LF characters.

   ----------------------------
   -- Generic Thread Support --
   ----------------------------

   --  The types provided below enable the display debugger to get information
   --  that is both generic enough to handle most debuggers/languages, but
   --  also precise enough that a nice graphical object can be displayed.

   function Thread_List
     (Lang : access Language_Root) return String is abstract;
   --  Return a string that corresponds to the thread list command
   --  for a particular language/debugger.
   --  This should be used by a debugger in cunjunction with Parse_Thread_List
   --  below.

   function Thread_Switch
     (Lang   : access Language_Root;
      Thread : Natural) return String is abstract;
   --  Return a string that corresponds to the thread switch command
   --  for a particular language/debugger.
   --  Thread is a debugger specific thread id.

   subtype Thread_Fields is Interfaces.C.size_t range 1 .. 20;
   --  This represents the maximum number of fields in a thread list output.

   type Thread_Information
     (Num_Fields : Thread_Fields := Thread_Fields'First) is
   record
      Information : chars_ptr_array (1 .. Num_Fields);
   end record;
   --  Represent the information of one thread.

   type Thread_Information_Array is
     array (Positive range <>) of Thread_Information;
   --  List of thread information.

   function Parse_Thread_List
     (Lang   : access Language_Root;
      Output : String) return Thread_Information_Array is abstract;
   --  Parse the result of a thread list command.

   procedure Free (Info : in out Thread_Information_Array);
   --  Free the dyamic memory associated with each element of the array.

private
   type Language_Root is abstract tagged null record;
end Language;
