-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  This package contains various utilities that are shared by the project
--  editor and the browsers.

with Glib.Graphs;
with Gtkada.Canvas;
with Prj.Tree;
with Types;

package Project_Browsers is

   type Vertex_Factory is access function
     (Project_Name : Types.Name_Id) return Gtkada.Canvas.Canvas_Item;
   --  This function should return a new vertex to put in the graph. You should
   --  pass your own factory if you want to put the item into the canvas
   --  afterwards.

   type Edge_Factory is access function
     (V1, V2 : access Gtkada.Canvas.Canvas_Item_Record'Class)
     return Gtkada.Canvas.Canvas_Link;
   --  This function should return a new edge between the two vertices V1 and
   --  V2.

   function Has_Circular_Dependencies
     (Root_Project : Prj.Tree.Project_Node_Id;
      Factory      : Vertex_Factory := null;
      E_Factory    : Edge_Factory := null)
      return Boolean;
   --  Return True if there is a circular dependency for the with clauses in
   --  Root_Project.

   function Dependency_Graph
     (Root_Project : Prj.Tree.Project_Node_Id;
      Factory      : Vertex_Factory := null;
      E_Factory    : Edge_Factory := null)
      return Glib.Graphs.Graph;
   --  Return a graph that represent the dependencies between the projects: the
   --  vertices are the projects themselves, and the links represent a with
   --  clause.
   --
   --  It is the responsability of the caller to destroy the graph.
   --
   --  If Factory is null, then a default vertex is created.
   --  The graph can be used directly in GtkAda.Canvas provided you set up the
   --  vertices correctly in your factory.

end Project_Browsers;
