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

with Prj.Tree;  use Prj.Tree;
with Prj_API;   use Prj_API;

package body Project_Hash is

   use Project_Htable;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Project_Data_Record) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   ----------
   -- Hash --
   ----------

   function Hash (F : Prj.Tree.Project_Node_Id) return Project_Header_Num is
   begin
      return Project_Header_Num
        ((F - Empty_Node) mod Max_Projects_No_Overload + 1);
   end Hash;

   ----------------------
   -- Project_Modified --
   ----------------------

   function Project_Modified
     (Data      : Project_Htable.HTable;
      Project   : Prj.Tree.Project_Node_Id;
      Recursive : Boolean := False) return Boolean
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive);
   begin
      while Current (Iter) /= Empty_Node loop
         if Get (Data, Current (Iter)).Modified then
            return True;
         end if;
         Next (Iter);
      end loop;

      return False;
   end Project_Modified;

   --------------------------
   -- Set_Project_Modified --
   --------------------------

   procedure Set_Project_Modified
     (Data      : in out Project_Htable.HTable;
      Project   : Prj.Tree.Project_Node_Id;
      Modified  : Boolean)
   is
      Rec : Project_Data_Record := Get (Data, Project);
   begin
      Rec.Modified := Modified;
      Set (Data, Project, Rec);
   end Set_Project_Modified;

end Project_Hash;
