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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

with Errout;          use Errout;
with Prj;             use Prj;
with Prj.Com;         use Prj.Com;
with Prj.Ext;         use Prj.Ext;
with Prj.PP;          use Prj.PP;
with Prj.Proc;        use Prj.Proc;
with Prj.Tree;        use Prj.Tree;

with Prj_API;         use Prj_API;
with Prj_Normalize;   use Prj_Normalize;

package body Prj_Manager is

   Manager_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   Signals : constant chars_ptr_array :=
     (1 => New_String ("project_view_changed"));
   --  The list of signals defined for this object

   ------------------------------
   -- Change_Scenario_Variable --
   ------------------------------

   procedure Change_Scenario_Variable
     (Manager  : access Project_Manager_Record;
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
        (Find_Scenario_Variables (Manager.Project));
      return Manager.Scenario_Variables.all;
   end Find_Scenario_Variables;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Manager : access Project_Manager_Record) return Prj.Tree.Project_Node_Id
   is
   begin
      return Manager.Project;
   end Get_Project;

   ----------------------
   -- Get_Project_View --
   ----------------------

   function Get_Project_View
     (Manager : access Project_Manager_Record) return Prj.Project_Id is
   begin
      return Manager.Project_View;
   end Get_Project_View;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Manager : out Project_Manager; Project : Prj.Tree.Project_Node_Id) is
   begin
      pragma Assert (Project /= Empty_Node);
      Manager := new Project_Manager_Record;
      Initialize (Manager, Project);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Manager : access Project_Manager_Record'Class;
      Project : Prj.Tree.Project_Node_Id)
   is
      --  ??? Should be moved to GtkAda
      function Internal
        (Typ : GType; Last : System.Address := System.Null_Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_object_new");

      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));
   begin
      Set_Object (Manager, Internal (Gtk.Object.Get_Type));
      Initialize_User_Data (Manager);

      Initialize_Class_Record
        (Manager, Signals, Manager_Class, "ProjectManager", Signal_Parameters);

      Manager.Project := Project;
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

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View
     (Manager  : access Project_Manager_Record) is
   begin
      --  ??? Need to free as much memory as possible first.
      --  ??? Maybe a call to Prj.Reset is enough
      Errout.Initialize;
      Prj.Com.Units.Set_Last (No_Unit);
      Prj.Com.Units_Htable.Reset;

      Pretty_Print (Manager.Project);
      Process (Manager.Project_View, Manager.Project);

      Errout.Finalize;
      pragma Assert (Manager.Project_View /= No_Project);

      Object_Callback.Emit_By_Name (Manager, "project_view_changed");
   end Recompute_View;
end Prj_Manager;

