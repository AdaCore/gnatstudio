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

with Gtkada.Canvas; use Gtkada.Canvas;
with Glib.Graphs;   use Glib.Graphs;
with Prj.Tree;      use Prj.Tree;
with Types;         use Types;

package body Project_Browsers is

   procedure Find_Or_Create_Project_Vertex
     (G       : in out Graph;
      Name    : Name_Id;
      V       : out Project_Vertex_Access;
      Factory : Vertex_Factory := null);
   --  Return the vertex in G for the project Name.
   --  If such a node doesn't exist, a new vertex is created.

   procedure Process_Project
     (G       : in out Graph;
      Project : Project_Node_Id;
      Origin  : Project_Vertex_Access;
      Factory : Vertex_Factory := null;
      E_Factory : Edge_Factory := null);
   --  Add project and its dependencies, recursively, into the graph.

   -----------------------------------
   -- Find_Or_Create_Project_Vertex --
   -----------------------------------

   procedure Find_Or_Create_Project_Vertex
     (G       : in out Graph;
      Name    : Name_Id;
      V       : out Project_Vertex_Access;
      Factory : Vertex_Factory := null)
   is
      Iter : Vertex_Iterator := First (G);
   begin
      while not At_End (Iter) loop
         V := Project_Vertex_Access (Get (Iter));
         if V.Name = Name then
            return;
         end if;
         Next (Iter);
      end loop;

      if Factory = null then
         V := new Project_Vertex;
      else
         V := Factory (Name);
      end if;

      V.Fully_Parsed := False;
      V.Name := Name;

      Add_Vertex (G, V);
   end Find_Or_Create_Project_Vertex;

   ---------------------
   -- Process_Project --
   ---------------------

   procedure Process_Project
     (G       : in out Graph;
      Project : Project_Node_Id;
      Origin  : Project_Vertex_Access;
      Factory : Vertex_Factory := null;
      E_Factory : Edge_Factory := null)
   is
      With_Clause : Project_Node_Id := First_With_Clause_Of (Project);
      Dest : Project_Vertex_Access;
      E : Canvas_Link;
   begin
      pragma Assert (not Origin.Fully_Parsed);
      Origin.Fully_Parsed := True;

      while With_Clause /= Empty_Node loop
         Find_Or_Create_Project_Vertex
           (G, Prj.Tree.Name_Of (With_Clause), Dest, Factory);

         if E_Factory = null then
            E := new Canvas_Link_Record;
         else
            E := E_Factory (Origin, Dest);
         end if;
         Add_Edge (G, E, Origin, Dest);

         if not Dest.Fully_Parsed then
            Process_Project (G, Project_Node_Of (With_Clause), Dest,
                             Factory, E_Factory);
         end if;

         With_Clause := Next_With_Clause_Of (With_Clause);
      end loop;
   end Process_Project;

   ----------------------
   -- Dependency_Graph --
   ----------------------

   function Dependency_Graph
     (Root_Project : Project_Node_Id;
      Factory      : Vertex_Factory := null;
      E_Factory : Edge_Factory := null)
      return Glib.Graphs.Graph
   is
      G : Graph;
      Origin : Project_Vertex_Access;
   begin
      Set_Directed (G, True);
      Find_Or_Create_Project_Vertex
        (G, Prj.Tree.Name_Of (Root_Project), Origin, Factory);
      Process_Project (G, Root_Project, Origin, Factory, E_Factory);
      return G;
   end Dependency_Graph;

   -------------------------------
   -- Has_Circular_Dependencies --
   -------------------------------

   function Has_Circular_Dependencies
     (Root_Project : Project_Node_Id;
      Factory      : Vertex_Factory := null;
      E_Factory    : Edge_Factory := null)
      return Boolean
   is
      G : Graph := Dependency_Graph (Root_Project, Factory, E_Factory);
      Result : constant Boolean := not Is_Acyclic (G);
   begin
      Destroy (G);
      return Result;
   end Has_Circular_Dependencies;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Vertex : access Project_Vertex) return Name_Id is
   begin
      return Vertex.Name;
   end Project_Name;

   ----------------------
   -- Set_Project_Name --
   ----------------------

   procedure Set_Project_Name
     (Vertex : access Project_Vertex; Name : Types.Name_Id) is
   begin
      Vertex.Name := Name;
   end Set_Project_Name;

end Project_Browsers;


