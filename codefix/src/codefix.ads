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

--  This is the main package of Codefix, it define constants, exceptions and
--  tools that are used in others package.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package Codefix is

   Codefix_Panic : exception;
   --  This exception is raised when a unpredicted error is made,
   --  most of times it is due to a bad usage of the functions by the
   --  programmer.

   Tab_Width : constant Natural := 8;
   --  Width of a tab in GNAT

   Indentation_Width : constant := 3;
   --  Width of an identation in GNAT

   EOL_Str : constant String := (1 => Character'Val (13));
   --  String used to insert an end of line.

   ----------------------------------------------------------------------------
   --  type Dynamic_String
   ----------------------------------------------------------------------------

   --  All the functions of affectation (Affect and Get_Line) destroy the
   --  precedent element in the Dynamic_String. If you don't want to delete
   --  the precedent string, you have to initialize the Dynamic_String with
   --  the value null before calling these functions.

   type Dynamic_String is access all String;

   procedure Assign (This : in out Dynamic_String; Value : String);
   --  Delete the precedent string, and create a new initialized with Value.

   procedure Assign (This : in out Dynamic_String; Value : Dynamic_String);
   --  Delete the precedent string This, and create a new initialized with a
   --  copy of Value.

   procedure Get_Line (This : in out Dynamic_String);
   --  Delete the precedent string, and create a new initialized with the line
   --  red on the standart input.

   procedure Get_Line (File : File_Type; This : in out Dynamic_String);
   --  Delete the precedent string, and create a new initialized with the line
   --  red File.

   procedure Put_Line (This : Dynamic_String);
   --  Put the string referenced on the standart output.

   procedure Put_Line (File : File_Type; This : Dynamic_String);
   --  Put the string referenced on the file specified.

   procedure Free is new Ada.Unchecked_Deallocation (String, Dynamic_String);

   --  ??? Why can't we use GNAT.Dynamic_Tables instead

   generic

      type Data_Type is private;
      --  The type of data inside the array.

      Null_Element : Data_Type;
      --  The default value of data

      Free_Buffer : Positive := 10;
      --  This is the size that the array uses to grow when the memory is
      --  too small.

      with procedure Free (Data : in out Data_Type) is <>;

   package Dynamic_Arrays is

      type Dynamic_Array is private;
      --  This object is a dynamic Array with a virtual infinite size.
      --  By default, each element is initialized with the value Null_Element.

      Null_Array : constant Dynamic_Array;
      --  This is an emty array.

      procedure Set_Element
        (This     : in out Dynamic_Array;
         Element  : Data_Type;
         Position : Positive);
      --  Use to put an element inside the array.

      function Get_Element
        (This     : Dynamic_Array;
         Position : Positive) return Data_Type;
      --  Use to get an element from the array. If there is nothing inside,
      --  Null_Element is returned.

      procedure Delete_Element
        (This : in out Dynamic_Array;
         Position : Positive);
      --  Initialize a position with Null_Element.

      procedure Resize (This : in out Dynamic_Array);
      --  When called, optimise the size taken by the array by using the
      --  Free_Buffer value.

      procedure Free (This : in out Dynamic_Array);
      --  Free the memory used by a dynamic array.

      function Clone (This : Dynamic_Array) return Dynamic_Array;
      --  Copy all the values of an arry into an other.

   private

      type Static_Array is array (Positive range <>) of Data_Type;
      type Dynamic_Array is access Static_Array;

      procedure Resize (This : in out Dynamic_Array; New_Size : Positive);
      function Get_Using_Size (This : Dynamic_Array) return Natural;
      procedure Delete is
         new Ada.Unchecked_Deallocation (Static_Array, Dynamic_Array);

      Null_Array : constant Dynamic_Array := null;

   end Dynamic_Arrays;

end Codefix;
