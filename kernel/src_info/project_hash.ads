-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with HTables;
with Prj.Tree;

package Project_Hash is

   type Project_Data_Record is record
      Modified : Boolean := False;
      --  True if the project has been modified by the user, and not saved
      --  yet.

   end record;
   --  The data stored for each project loaded in the kernel (ie the root
   --  project and all its imported projects).

   No_Project_Data : constant Project_Data_Record := (Modified => False);

   Max_Projects_No_Overload : constant Prj.Tree.Project_Node_Id := 99;
   type Project_Header_Num is new Prj.Tree.Project_Node_Id
     range 0 .. Max_Projects_No_Overload;

   function Hash (F : Prj.Tree.Project_Node_Id) return Project_Header_Num;
   --  Hash code to use for the project_htable

   package Project_Htable is new HTables.Simple_HTable
     (Header_Num => Project_Header_Num,
      Element    => Project_Data_Record,
      No_Element => No_Project_Data,
      Key        => Prj.Tree.Project_Node_Id,
      Hash       => Hash,
      Equal      => Prj.Tree."=");

   function Project_Modified
     (Data      : Project_Htable.HTable;
      Project   : Prj.Tree.Project_Node_Id;
      Recursive : Boolean := False) return Boolean;
   --  Return True if Project has been modified, but not saved.
   --  If Recursive is True, this function will also return True if one of the
   --  imported project has been modified.

   procedure Set_Project_Modified
     (Data      : in out Project_Htable.HTable;
      Project   : Prj.Tree.Project_Node_Id;
      Modified  : Boolean);
   --  Set the modified flag for Project.

end Project_Hash;
