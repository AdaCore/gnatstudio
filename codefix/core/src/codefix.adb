------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

with String_Utils;   use String_Utils;
with GNATCOLL.Utils; use GNATCOLL.Utils;

package body Codefix is

   -------------------
   -- To_Char_Index --
   -------------------

   function To_Char_Index
     (Index : Visible_Column_Type; Str : String) return String_Index_Type
   is
      Current_Index : Integer := Str'First;
   begin
      Skip_To_Column (Str     => Str,
                      Columns => Integer (Index),
                      Index   => Current_Index);
      return String_Index_Type (Current_Index);
   end To_Char_Index;

   ---------------------
   -- To_Column_Index --
   ---------------------

   function To_Column_Index
     (Index : String_Index_Type; Str : String) return Visible_Column_Type
   is
      Current_Index : String_Index_Type := String_Index_Type (Str'First);
      Current_Col   : Visible_Column_Type := 1;
   begin
      Skip_To_Index
        (Buffer        => Str,
         Columns       => Current_Col,
         Index_In_Line => Index,
         Index         => Current_Index);

      return Current_Col;
   end To_Column_Index;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out String_Access; Value : String) is
      Garbage : String_Access := This;
      --  Used to prevent usage like 'Assign (Str, Str.all)'
   begin
      This := new String'(Value);
      Free (Garbage);
   end Assign;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out String_Access; Value : String_Access) is
      Garbage : String_Access := This;
      --  Used to prevent usage like 'Assign (Str, Str)'
   begin
      This := Clone (Value);
      Free (Garbage);
   end Assign;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : File_Type; This : in out String_Access) is
      Len    : Natural;
      Buffer : String (1 .. 2048);
   begin
      --  We can't read lines longer than 2048 characters. Doesn't seem worth
      --  it anyway, since we are dealing with source files.
      Get_Line (File, Buffer, Len);
      Assign (This,  Buffer (1 .. Len));
   end Get_Line;

   -----------
   -- Clone --
   -----------

   function Clone (This : String_Access) return String_Access is
   begin
      if This = null then
         return null;
      else
         return new String'(This.all);
      end if;
   end Clone;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Mask : Useless_Entity_Operations;
      Flag : Useless_Entity_Operations) return Boolean
   is
   begin
      return (Mask and Flag) = Flag;
   end Is_Set;

   --------------------------
   -- Policy_To_Operations --
   --------------------------

   function Policy_To_Operations
     (Policy : Codefix_Remove_Policy) return Useless_Entity_Operations is
   begin
      case Policy is
         when Always_Remove =>
            return Remove_Entity;
         when Always_Comment =>
            return Comment_Entity;
         when Propose_Both_Choices =>
            return Comment_Entity or Remove_Entity;
      end case;
   end Policy_To_Operations;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out State_Node) is
   begin
      Free (This.Error);
   end Free;

end Codefix;
