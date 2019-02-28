------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  This package offers a completion resolver for language keywords.

package Completion.Keywords is

   type Completion_Keywords is new Completion_Resolver with private;

   type Completion_Keywords_Access is access all Completion_Keywords'Class;

   overriding
   procedure Get_Completion_Root
     (Resolver   : access Completion_Keywords;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Completion_Keywords) return String;
   --  See inherited documentation

   overriding procedure Free (Resolver : in out Completion_Keywords) is null;
   --  See inherited documentation

private

   type Completion_Keywords is new Completion_Resolver with record
      Lang : Language_Access;
   end record;

end Completion.Keywords;
