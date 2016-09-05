------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
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

package body Language.Abstract_Language_Tree is

   ----------
   -- Info --
   ----------

   function Info
     (Self             : Semantic_Node'Class;
      Show_Param_Names : Boolean := True) return Semantic_Node_Info
   is
   begin
      return A : Semantic_Node_Info do
         A := (Category   => Self.Category,
               Name       => Self.Name,
               Profile    =>
                 +Self.Profile (Show_Param_Names => Show_Param_Names),
               Unique_Id  => +Self.Unique_Id,
               Is_Decl    => Self.Is_Declaration,
               Visibility => Self.Visibility,
               Sloc_Start => Self.Sloc_Start,
               Sloc_Def   => Self.Sloc_Def);

         pragma Assert (if A.Name /= No_Symbol
                        then A.Unique_Id /= Null_Unbounded_String
                        else True);
      end return;
   end Info;

end Language.Abstract_Language_Tree;
