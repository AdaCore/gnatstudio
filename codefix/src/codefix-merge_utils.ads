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

--  Those functions implements the main merge function, and all merge tools
--  used with strings.

with Ada.Unchecked_Deallocation;

package Codefix.Merge_Utils is

   type Merge_Info is
     (Original_Unit, Unit_Modified, Unit_Deleted, Unit_Created);

   generic

      type Merge_Type is private;
      --  Type of objects that can be merged.
      --  Note : units recorded in Merge_Type are not necessary adjacents.

      type Merged_Unit is private;
      --  Type of objects contained in Merge_Type, which are the basic elements
      --  of the merge.

      type Merge_Iterator is private;
      --  Object by what it is possible to access objects in Merge_Type.

      with function First (This : Merge_Type) return Merge_Iterator is <>;
      --  Return an iterator positioned in the first object of This.

      with function Next (This : Merge_Iterator) return Merge_Iterator is <>;
      --  Return the iterator positioned after This.

      with function Data (This : Merge_Iterator) return Merged_Unit is <>;
      --  Return the object referenced by an iterator.

      with function Is_Null (This : Merge_Iterator) return Boolean is <>;
      --  Return True if the iterator doesn't references anything. Typycally,
      --  when the loop has ended.

      with function "<" (Left, Right : Merge_Iterator) return Boolean is <>;
      --  Return True if Left is before Right.

      with function Get_Merge_Info
        (This : Merged_Unit) return Merge_Info is <>;
      --  Return the Merge_Info of an unit.

      with procedure Append
        (This : in out Merge_Type; Object : Merged_Unit) is <>;
      --  Add an unit in This.

      with function Clone (This : Merged_Unit) return Merged_Unit is <>;
      --  Clone all informations contained in This, specially the pointers.

      with procedure Merge_Units
        (Result              : out Merged_Unit;
         Object_1, Object_2  : Merged_Unit;
         Success             : out Boolean;
         Chronologic_Changes : Boolean);
      --  If the units contained in a Merge_Type are themselves mergable, then
      --  this function should is the one that merge them. Otherwise, it
      --  doesn't have to do anything.

      with function "=" (Left, Right : Merged_Unit) return Boolean is <>;
      --  Return True if Left contains same informations as Right.

   procedure Generic_Merge
     (Result              : out Merge_Type;
      --  This is the result of the merge. Note that each unit in this
      --  object is created using clone functions.

      Object_1, Object_2  : Merge_Type;
      --  These are the two objects that have to be merged into result.

      Success             : out Boolean;
      --  Success is false when the merge has non sens.

      Chronologic_Changes : Boolean
      --  If Chronologic_Changes is true, Object_2 should have been created
      --  after Object_1. This information can help the function to solve
      --  some ambiguities.

      );


   type Mergable_String is private;

   procedure Delete
     (This : in out Mergable_String; Start : Natural; Len : Natural := 0);
   --  Delete len characters from Start. If Len = 0 then all characters from
   --  Start will be deleted.

   procedure Insert
     (This : in out Mergable_String; Start : Natural; Value : String);
   --  Insert Value on (means 'before') the Start Position.

   procedure Modify
     (This : in out Mergable_String; Start : Natural; Value : String);
   --  Modify an unresizable portion of text by Value.

   procedure Replace
     (This : in out Mergable_String; Start, Len : Natural; Value : String);
   --  Replace Len characters from Start column by value.

   procedure Merge_String
     (Result              : out Mergable_String;
      Object_1, Object_2  : Mergable_String;
      Success             : out Boolean;
      Chronologic_Changes : Boolean);
   --  Merge two strings into Result. For details see Generic_Merge above.

   procedure Free (This : in out Mergable_String);
   --  Free the memory associated to a Mergable_String.

   function To_String (This : Mergable_String) return String;
   --  Return the current value of the string contained in This.

   procedure Assign (This : in out Mergable_String; Value : Mergable_String);
   --  Assign values from 'Value' to 'This'.

   function Clone (This : Mergable_String) return Mergable_String;
   --  Clone each information contained in This, specially the pointers.

   function To_Mergable_String (This : String) return Mergable_String;
   --  Convert a simple String to a Mergeable_String,

   type Mask_Iterator is private;

   procedure Reset (This : in out Mask_Iterator);
   --  Initialize all fields of This at the beginning of mergeables strings>

   procedure Get_Next_Area
     (This       : Mergable_String;
      It         : in out Mask_Iterator;
      Start, Len : out Natural;
      Info       : out Merge_Info);
   --  Return a text area composed by only one type of information. By using
   --  those informations, it is possible to transform the original string to
   --  the new one. When all areas are gotten, Len will be 0.

private

   type Merge_Array is array (Natural range <>) of Merge_Info;

   type Ptr_Merge_Array is access all Merge_Array;

   procedure Free is new
     Ada.Unchecked_Deallocation (Merge_Array, Ptr_Merge_Array);

   type Mergable_String is record
      Str   : Dynamic_String;
      Infos : Ptr_Merge_Array;
   end record;

   type String_Char is record
      Char : Character;
      Info : Merge_Info;
   end record;

   type String_Iterator is record
      Object   : Mergable_String;
      Position : Natural;
   end record;

   function First (This : Mergable_String) return String_Iterator;
   function Next (This : String_Iterator) return String_Iterator;
   function Data (This : String_Iterator) return String_Char;
   function Is_Null (This : String_Iterator) return Boolean;
   function "<" (Left, Right : String_Iterator) return Boolean;
   function Get_Merge_Info (This : String_Char) return Merge_Info;
   procedure Append (This : in out Mergable_String; Object : String_Char);
   function Clone (This : String_Char) return String_Char;
   procedure Merge_Null
     (Result              : out String_Char;
      Object_1, Object_2  : String_Char;
      Success             : out Boolean;
      Chronologic_Changes : Boolean);
   function "=" (Left, Right : String_Char) return Boolean;

   function Get_Array_Position (Str : Mergable_String; Position : Natural)
     return Natural;

   procedure Delete_Char (This : in out Dynamic_String; Position : Natural);
   procedure Delete_Info (This : in out Ptr_Merge_Array; Position : Natural);

   type Mask_Iterator is record
      Info_Index, Real_Index : Natural;
   end record;

end Codefix.Merge_Utils;
