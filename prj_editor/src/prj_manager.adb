-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;            use Glib;
with Glib.Object;     use Glib.Object;

with System;               use System;

with Prj;             use Prj;
with Prj.Ext;         use Prj.Ext;
with Prj.PP;          use Prj.PP;
with Prj.Tree;        use Prj.Tree;

with Prj_API;         use Prj_API;
with Prj_Normalize;   use Prj_Normalize;
with Glide_Kernel;    use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;

package body Prj_Manager is

   ------------------------------
   -- Change_Scenario_Variable --
   ------------------------------

   procedure Change_Scenario_Variable
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Variable : String;
      Value    : String) is
   begin
      Prj.Ext.Add (Variable, Value);
   end Change_Scenario_Variable;

   -----------------------------
   -- Find_Scenario_Variables --
   -----------------------------

   function Find_Scenario_Variables
     (Manager : access Project_Manager_Record)
      return Prj_API.Project_Node_Array is
   begin
      if Manager.Scenario_Variables /= null then
         return Manager.Scenario_Variables.all;
      end if;

      Manager.Scenario_Variables := new Project_Node_Array'
        (Find_Scenario_Variables (Get_Project (Manager.Kernel)));
      return Manager.Scenario_Variables.all;
   end Find_Scenario_Variables;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Manager : out Project_Manager;
      Kernel  : access Kernel_Handle_Record'Class) is
   begin
      Manager := new Project_Manager_Record;
      Initialize (Manager, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Manager : access Project_Manager_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class)
   is
      --  ??? Should be moved to GtkAda
      function Internal
        (Typ : GType; Last : System.Address := System.Null_Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_object_new");

   begin
      Set_Object (Manager, Internal (Gtk.Object.Get_Type));
      Initialize_User_Data (Manager);

      Manager.Kernel := Kernel_Handle (Kernel);
   end Initialize;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize
     (Manager        : access Project_Manager_Record;
      Project_Filter : Prj.Tree.Project_Node_Id)
   is
   begin
      if not Manager.Is_Normalized then
         Normalize_Project (Project_Filter);
         Manager.Is_Normalized := True;
         Pretty_Print (Project_Filter);
      end if;
   end Normalize;
end Prj_Manager;

