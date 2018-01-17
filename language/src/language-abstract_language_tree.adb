------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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
         A := (Category => Self.Category,
               Name     => Self.Name,
               Profile  => Self.Profile (Show_Param_Names => Show_Param_Names),
               Unique_Id  => Self.Unique_Id,
               Is_Decl    => Self.Is_Declaration,
               Visibility => Self.Visibility,
               Sloc_Start_No_Tab => Self.Sloc_Start,
               Sloc_Def_No_Tab   => Self.Sloc_Def);

         pragma Assert (A.Name = No_Symbol or else A.Unique_Id /= No_Symbol);
      end return;
   end Info;

   function No_Semantic_Tree_Iterator return Semantic_Tree_Iterator'Class is
   begin
      return Result : Dummy_Semantic_Tree_Iterator;
   end No_Semantic_Tree_Iterator;

end Language.Abstract_Language_Tree;
