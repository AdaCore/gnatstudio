-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Prj;      use Prj;
with Prj.Tree; use Prj.Tree;
with Ada.Containers.Ordered_Sets;
limited with Toolchains.Project_Parsers;

package Toolchains.Parsers is

   type Toolchain_Parser_Record is private;

   type Toolchain_Parser is access all Toolchain_Parser_Record;

   procedure Parse
     (This      : Toolchain_Parser;
      Parser    : access Toolchains.Project_Parsers.Project_Parser_Record;
      Node_Data : Project_Node_Tree_Ref;
      IDE_Node  : Project_Node_Id);
   --  Parse the toolchain contained in the IDE node given in parameter.

   procedure Set_Toolchains
     (This       : Toolchain_Parser;
      Toolchains : Toolchain_Array);
   --  Modifies the stucture of the project so that it supports the toolchains
   --  given in parameter

private

   package Prj_Node_Sets is new Ada.Containers.Ordered_Sets (Project_Node_Id);

   type Toolchain_Parser_Record is record
      Enclosing_Parser : access
        Toolchains.Project_Parsers.Project_Parser_Record;
      Node_Data        : Project_Node_Tree_Ref;
      Project          : access
        Toolchains.Project_Parsers.Parsed_Project_Record;

      Error            : String_Access;

      IDE_Package      : Project_Node_Id;
      Toolchain_Case_Statement : Project_Node_Id;
      Variable_Node    : Project_Node_Id;

      Attributes       : Prj_Node_Sets.Set;
      Toolchains       : Toolchain_Maps.Map;
   end record;

end Toolchains.Parsers;
