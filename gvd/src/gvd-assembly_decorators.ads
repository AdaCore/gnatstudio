------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

--  This package implements class to decorate assembler instructions
--  with Pango markups.

with Ada.Containers.Indefinite_Ordered_Sets;

package GVD.Assembly_Decorators is

   package Registers_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Decorator is tagged private;

   function Decorate
     (Self        : Decorator;
      Instruction : String;
      Registers   : Registers_Set.Set)
      return String;

private

   type Decorator is tagged null record;

end GVD.Assembly_Decorators;
