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

package Language is

   type Language_Root is abstract tagged private;
   type Language_Access is access all Language_Root'Class;

   Unexpected_Type : exception;

   procedure Free (Lang : in out Language_Access);
   --  Free the memory pointed to by Lang and set it to null.

   function Is_Simple_Type
     (Lang : access Language_Root; Str : String) return Boolean is abstract;
   --  Return True if Str is a simple type, like integer, ...
   --  These are the types that don't need information from the debugger to
   --  be known, ie we can save a call to the debugger when parsing the value
   --  of a variable.

   type Language_Entity is (Normal_Text,
                            Keyword_Text,
                            Comment_Text,
                            String_Text);
   --  The entities found in a language, and that can have a different scheme
   --  for colors highlighting.

   procedure Looking_At (Lang      : access Language_Root;
                         Buffer    : String;
                         Entity    : out Language_Entity;
                         Next_Char : out Positive);
   --  Should return the type of entity that is present at the first position
   --  in the buffer (ie starting at Buffer'First).
   --  Next_Char should be set to the index of the first character after the
   --  entity.

   ------------------------
   -- Types manipulation --
   ------------------------
   --  The following functions are provided to manipulate types and variables
   --  for each language.

   function Dereference_Name (Lang : access Language_Root;
                              Name : String)
                             return String is abstract;
   --  Return the name to use to dereference Name (ie in Ada "Name.all", in
   --  C "*Name", ...). Note that Name can be a composite name (Name.Field),
   --  and thus might have to be protected with parentheses.

   function Array_Item_Name (Lang  : access Language_Root;
                             Name  : String;
                             Index : String)
                            return String is abstract;
   --  Return the name to use to access a specific element of an array.
   --  Index is a comma-separated list of the indexes for all the dimensions,
   --  as in "1,2".

   function Record_Field_Name (Lang  : access Language_Root;
                               Name  : String;
                               Field : String)
                              return String is abstract;
   --  Return the name to use for a specific field of a record.

private
   type Language_Root is abstract tagged null record;
end Language;
