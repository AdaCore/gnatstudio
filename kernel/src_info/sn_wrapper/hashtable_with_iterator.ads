-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

generic

   type Key_Hash_Range is range <>;
   --  Range type for key's hash table. Must be a range of Integer.

   type Element_Type is private;
   --  Type of elements in Hashtable

   No_Element : Element_Type;
   --  Value indicating absence of Element

   type Key_Type is private;
   --  Type of key

   with function Key_Hash (Key : Key_Type) return Key_Hash_Range;
   --  This function calculates a hash value of given key. Hash value
   --  should have type Key_Hash_Range specified as a first generic
   --  parameter

   with function "<" (Key1, Key2 : Key_Type) return Boolean;
   with function "=" (Key1, Key2 : Key_Type) return Boolean;

package Hashtable_With_Iterator is

   type Key_Value_Pair is
      record
         Key : Key_Type;
         Value : Element_Type;
      end record;

   procedure Init;
   --  Allocates necessary resources. Should be called before any other
   --  call to function or procedure from this package.

   procedure Set (Key : Key_Type; Value : Element_Type);
   --  Associates key with given value

   function Get (Key : Key_Type) return Element_Type;
   --  Returns value associated with specified key

   procedure Sort;
   --  Sorts elements of the Hashtable in ascending order of keys.
   --  In the current implementation Bubble Sort soting method is used.

   procedure Reset_Cursor;
   --  Resets the cursor. Get_Next will return first element of
   --  the Hashtable after call to this procedure

   function Get_Next return Key_Value_Pair;
   --  Returns next Key-Value pair comparing to previous call to this
   --  funciton or to the Reset_Cursor procedure

   procedure Free;
   --  Releases all resources used by the Hashtable. New call to Init is
   --  needed to start a new session with the package.

end Hashtable_With_Iterator;
