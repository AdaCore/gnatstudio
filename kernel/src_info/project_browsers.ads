-----------------------------------------------------------------------
--                               GPS                                 --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is  free software;  you can redistribute it and/or modify  it --
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

--  This package contains various utilities that are shared by the project
--  editor and the browsers.

with Glib.Graphs;
with Prj.Tree;
with Types;

package Project_Browsers is

   type Name_Id_Array is array (Positive range <>) of Types.Name_Id;

   type Vertex_Factory is access function
     (Project_Name : Types.Name_Id) return Glib.Graphs.Vertex_Access;
   --  This function should return a new vertex to put in the graph. You should
   --  pass your own factory if you want to put the item into the canvas
   --  afterwards.

   type Edge_Factory is access function
     (V1, V2 : access Glib.Graphs.Vertex'Class) return Glib.Graphs.Edge_Access;
   --  This function should return a new edge between the two vertices V1 and
   --  V2.

   function Has_Circular_Dependencies
     (Root_Project : Prj.Tree.Project_Node_Id;
      Factory      : Vertex_Factory := null;
      E_Factory    : Edge_Factory := null) return Boolean;
   --  Return True if there is a circular dependency for the with clauses in
   --  Root_Project.

   function Topological_Sort (Root_Project : Prj.Tree.Project_Node_Id)
      return Name_Id_Array;
   --  Return the projects sorted topologically (first the root project, down
   --  to the last project, which doesn't import any of the others).

end Project_Browsers;
