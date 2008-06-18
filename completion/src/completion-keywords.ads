-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
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

--  This package offers a completion resolver for language keywords.

package Completion.Keywords is

   type Completion_Keywords is new Completion_Resolver with private;

   type Completion_Keywords_Access is access all Completion_Keywords'Class;

   overriding
   procedure Get_Completion_Root
     (Resolver   : access Completion_Keywords;
      Offset     : Integer;
      Context    : Completion_Context;
      Result     : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Completion_Keywords) return String;
   --  See inherited documentation

   overriding
   procedure Free (Resolver : in out Completion_Keywords);
   --  See inherited documentation

private

   type Completion_Keywords is new Completion_Resolver with record
      Lang : Language_Access;
   end record;

end Completion.Keywords;
