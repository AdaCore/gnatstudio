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

with Gtk.Window;      use Gtk.Window;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;

with Project_Trees;   use Project_Trees;
with Project_Viewers; use Project_Viewers;
with Scenario_Views;  use Scenario_Views;
with Prj_Manager;     use Prj_Manager;

with Gtkada.MDI;      use Gtkada.MDI;

package Prj_Editor_Window is

   type Project_Editor_Record is new Gtk_Window_Record with record
      Tree     : Project_Tree;
      Viewer   : Project_Viewer;
      Scenar   : Scenario_View;
      MDI      : MDI_Window;
      Scrolled : Gtk_Scrolled_Window;
      Manager  : Project_Manager;
   end record;
   type Project_Editor is access all Project_Editor_Record;
   --  Window containing a project editor

   procedure Gtk_New
     (Prj          : out Project_Editor;
      Project_Name : String);
   --  Create a new editor window based on the contents of Project_Name

   procedure Initialize
     (Prj          : access Project_Editor_Record'Class;
      Project_Name : String);
   --  Internal initialization function

end Prj_Editor_Window;
