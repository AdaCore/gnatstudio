-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib.Graphs;   use Glib.Graphs;
with Prj.Tree;      use Prj.Tree;
with Types;         use Types;
with HTables;

package body Project_Browsers is

   type Header_Num is range 0 .. 999;

   function Hash (N : Name_Id) return Header_Num;
   --  hash function to use for the htables.

   package Vertex_Htable is new HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Vertex_Access,
      No_Element => null,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => Types."=");
   use Vertex_Htable;

   type Name_Vertex is new Vertex with record
      Name : Name_Id;
   end record;
   type Name_Vertex_Access is access all Name_Vertex'Class;
   --  For internal use only, for storing the names.

   type Name_Edge is new Edge with null record;
   type Name_Edge_Access is access all Name_Edge'Class;

   procedure Destroy (V : in out Name_Vertex);
   procedure Destroy (E : in out Name_Edge);
   --  Dummy function, so that Name_Vertex is no longer abstract

   -------------
   -- Destroy --
   -------------

   procedure Destroy (V : in out Name_Vertex) is
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (E : in out Name_Edge) is
   begin
      null;
   end Destroy;

   ----------
   -- Hash --
   ----------

   function Hash (N : Name_Id) return Header_Num is
   begin
      return Header_Num ((N - Names_Low_Bound) mod 1000);
   end Hash;

   ----------------------
   -- Dependency_Graph --
   ----------------------

   function Dependency_Graph
     (Root_Project : Project_Node_Id;
      Factory      : Vertex_Factory := null;
      E_Factory    : Edge_Factory := null)
      return Glib.Graphs.Graph
   is
      Table : Vertex_Htable.HTable;
      G : Graph;

      function Create_Project_Vertex (Name    : Name_Id)
        return Vertex_Access;
      --  Return the vertex in G for the project Name.
      --  If such a node doesn't exist, a new vertex is created.

      procedure Process_Project
        (Project : Project_Node_Id; Origin  : Vertex_Access);
      --  Add project and its dependencies, recursively, into the graph.

      ---------------------------
      -- Create_Project_Vertex --
      ---------------------------

      function Create_Project_Vertex (Name : Name_Id)
         return Vertex_Access
      is
         V : Vertex_Access;
      begin
         if Factory = null then
            V := new Name_Vertex;
            Name_Vertex_Access (V).Name := Name;
         else
            V := Factory (Name);
         end if;

         Add_Vertex (G, V);
         Set (Table, Name, V);
         return V;
      end Create_Project_Vertex;

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project
        (Project : Project_Node_Id; Origin  : Vertex_Access)
      is
         With_Clause : Project_Node_Id := First_With_Clause_Of (Project);
         Dest        : Vertex_Access;
         E           : Edge_Access;
         New_Item    : Boolean;
      begin
         while With_Clause /= Empty_Node loop
            Dest := Get (Table, Prj.Tree.Name_Of (With_Clause));
            New_Item := Dest = null;

            if New_Item then
               Dest := Create_Project_Vertex (Prj.Tree.Name_Of (With_Clause));
            end if;

            if E_Factory = null then
               E := new Name_Edge;
            else
               E := E_Factory (Origin, Dest);
            end if;
            Add_Edge (G, E, Origin, Dest);

            if New_Item then
               Process_Project (Project_Node_Of (With_Clause), Dest);
            end if;

            With_Clause := Next_With_Clause_Of (With_Clause);
         end loop;
      end Process_Project;

      Origin : Vertex_Access;
   begin
      Set_Directed (G, True);
      Origin := Create_Project_Vertex (Prj.Tree.Name_Of (Root_Project));
      Process_Project (Root_Project, Origin);
      Reset (Table);
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

   ----------------------
   -- Topological_Sort --
   ----------------------

   function Topological_Sort (Root_Project : Prj.Tree.Project_Node_Id)
      return Name_Id_Array
   is
      G : Graph := Dependency_Graph (Root_Project, null);
      Vertices : Depth_Vertices_Array := Depth_First_Search (G);
      List : Name_Id_Array (1 .. Vertices'Length);
   begin
      for J in Vertices'Range loop
         List (J - Vertices'First + List'First) :=
           Name_Vertex_Access (Vertices (J).Vertex).Name;
      end loop;
      Destroy (G);
      return List;
   end Topological_Sort;

end Project_Browsers;
