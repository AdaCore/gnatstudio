-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package is a simple widget that groups the project tree (to show the
--  directories and subprojects) and the display of the scenario variables,
--  that the user can modify.
--  This widget also knows how to save its state to an Ada stream, and re-read
--  a previously saved configuration.

with Glide_Kernel;
with Project_Trees;
with Scenario_Views;
with Vsearch_Ext;

with Gtk.Box;

package Project_Explorers is

   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Project_Explorer is access all Project_Explorer_Record'Class;

   procedure Gtk_New
     (Explorer : out Project_Explorer;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new explorer.

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.

   function Get_Tree
     (Explorer : access Project_Explorer_Record)
      return Project_Trees.Project_Tree;
   --  Return the directory/subproject tree displayed in the explorer.

private
   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with record
      Scenario : Scenario_Views.Scenario_View;
      Tree     : Project_Trees.Project_Tree;
      Search   : Vsearch_Ext.Vsearch_Extended;
   end record;

   pragma Inline (Get_Tree);
end Project_Explorers;
