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

--  <description>
--
--  This widget presents a summary view of the current scenario (see
--  definition in prj_manager.ads). It also provides various ways to
--  change or edit this scenario.
--
--  </description>

with Gtk.Button;
with Gtk.Table;
with Glide_Kernel;

package Scenario_Views is

   type Scenario_View_Record is new Gtk.Table.Gtk_Table_Record with private;
   type Scenario_View is access all Scenario_View_Record;

   procedure Gtk_New
     (View    : out Scenario_View;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new scenario view associated with Manager.
   --  The view is automatically refreshed every time the project view in
   --  the manager changes.

   procedure Initialize
     (View    : access Scenario_View_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal function for creating new widgets

private
   type Scenario_View_Record is new Gtk.Table.Gtk_Table_Record with record
      Kernel      : Glide_Kernel.Kernel_Handle;
      Edit_Button : Gtk.Button.Gtk_Button;

      Combo_Is_Open : Boolean := False;
      --  Flag temporarily set to True when a user is modifying the value of
      --  one of the scenario variable through the combo boxes.
   end record;
end Scenario_Views;
