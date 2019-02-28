------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

--  This package instantiates hash tables based on strings.
--  See HTables for complete documentation.

with HTables;

generic
   type Data_Type is private;
   with procedure Free_Data (X : in out Data_Type);

   Null_Ptr : Data_Type;

   Case_Sensitive : Boolean := True;
   --  Whether keys are case-sensitive

package String_Hash is

   type Name_Htable_Num is new Natural range 0 .. 6150;
   --  ??? This limitation should be raised (could be through use of
   --  Ada.Containers)
   --  The value here has some impact when loading big projects, so should not
   --  be made too small.

   function Hash (Key : String) return Name_Htable_Num;
   --  See HTables for documentation

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
   --  See HTables for documentation

end String_Hash;
