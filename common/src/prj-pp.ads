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

with Prj.Tree;  use Prj.Tree;
with Types;     use Types;

package Prj.PP is

   type Write_Eol_Ap is access procedure;

   type Write_Char_Ap is access procedure (C : Character);

   type Write_Str_Ap is access procedure (S : String);

   type Write_Line_Ap is access procedure (S : String);

   type Start_Line_Ap is access procedure (Indent : Natural);

   type Output_String_Ap is access procedure (S : String_Id);

   type Output_Name_Ap is
     access procedure (Name       : Name_Id;
                       Capitalize : Boolean := True);

   procedure Initialize (W_Eol     : Write_Eol_Ap;
                         W_Char    : Write_Char_Ap;
                         W_Str     : Write_Str_Ap;
                         W_Line    : Write_Line_Ap;
                         S_Line    : Start_Line_Ap;
                         O_String  : Output_String_Ap;
                         O_Name    : Output_Name_Ap;
                         Inc       : Positive);
   --  Initialize the project pretty printer.
   --  This procedure does not need to be called before calling
   --  Pretty_Print, because every one of its parameters
   --  have internal default values.

   procedure Pretty_Print (Project : Project_Node_Id);
   --  Output a project file, using either the default output
   --  routines, or the ones specified by a call to Initialize.

end Prj.PP;
