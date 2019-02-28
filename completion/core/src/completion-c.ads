------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

--  This package provides a C-specific completer

with Cpp_Semantic_Tree; use Cpp_Semantic_Tree;

package Completion.C is

   type C_Completion_Manager is new Completion_Manager with private;

   overriding function Get_Initial_Completion_List
     (Manager : access C_Completion_Manager;
      Context : Completion_Context)
      return Completion_List;
   --  See inherited documentation

private
   type C_Completion_Manager is
      new Completion_Manager with null record;

   type C_Completion_Context is new Completion_Context_Record with record
      Expression : Parsed_Expression;
   end record;

end Completion.C;
