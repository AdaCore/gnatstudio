-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package instanciates hash tables based on strings.
--  See HTables for complete documentation.

with HTables;

generic
   type Data_Type is private;
   with procedure Free_Data (X : in out Data_Type);

   Null_Ptr : Data_Type;

   Case_Sensitive : Boolean := True;
   --  Whether keys are case-sensitive

package String_Hash is

   type Name_Htable_Num is new Natural range 0 .. 1000;
   --  ??? This limitation should be raised.

   function Hash (Key : String) return Name_Htable_Num;
   --  See HTables for documentation.

   function Equal (Key1, Key2 : String) return Boolean;
   --  Whether the two keys are equal, depending on Case_Sensitive

   package String_Hash_Table is new HTables.Simple_HTable
     (Header_Num   => Name_Htable_Num,
      Element      => Data_Type,
      Free_Element => Free_Data,
      No_Element   => Null_Ptr,
      Key          => String,
      Hash         => Hash,
      Equal        => Equal);
   --  See HTables for documentation.

end String_Hash;
