-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
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

--  This file contains the various "high-level" refactoring operations
--  that can be done on source code, like extract a subprogram, add
--  parameters,...
--  These algorithms do not perform any query on the code (this is
--  implemented in the package Refactoring.Services), and they are
--  implemented mostly in terms of the simpler bricks provided in
--  Refactoring.Services

package Refactoring.Algorithms is

   ----------------------
   -- Code_Refactoring --
   ----------------------
   --  This type is the base of the high-level refactoring hierarchy.
   --  This hierarchy contains all the algorithms that can be performed
   --  on source code.

   type Code_Refactoring (<>) is abstract tagged private;
   type Code_Refactoring_Access is access all Code_Refactoring'Class;
   --  The unknown discriminant is to force creation through one of the
   --  Create-* constructors.

   procedure Free (Self : in out Code_Refactoring) is null;
   procedure Free (Self : in out Code_Refactoring_Access);
   --  Free the memory used by Self

   procedure Execute (Self : in out Code_Refactoring) is abstract;
   --  Execute the refactoring.
   --  Any context must have been passed in the constructor (Create_*)
   --  and has been stored in Self.

   function Create_Extract_Subprogram
     (Context : Factory_Context;
     ) return Code_Refactoring_Access;
   --  Returns a refactoring that extracts part of a source code into a
   --  new subprogram

private
   type Code_Refactoring is abstract tagged null record;
end Refactoring.Algorithms;
