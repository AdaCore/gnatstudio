------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P R J . P P                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------
--
--  This package is used to output a project file from a project file tree.

with Prj.Tree;

package Prj.PP is

   type Write_Char_Ap is access procedure (C : Character);
   type Write_Str_Ap  is access procedure (S : String);

   procedure Pretty_Print
     (Project   : Prj.Tree.Project_Node_Id;
      Increment : Positive := 3;
      Eliminate_Null_Statements : Boolean := False;
      W_Char : Write_Char_Ap := null;
      W_Str  : Write_Str_Ap  := null);
   --  Output a project file, using either the default output
   --  routines, or the ones specified by a call to Initialize.
   --  Increment is the number of spaces for each indentation level.
   --
   --  W_Char and W_Str can be used to change the default output
   --  procedures. The default values force the output to Standard_Output.
   --
   --  If Eliminate_Null_Statements is True, then declarative item lists that
   --  do not have any item will not be output. Otherwise, "null;" is output,
   --  which isn't a valid project syntax.

end Prj.PP;
