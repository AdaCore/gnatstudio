-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007, AdaCore              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glib;          use Glib;
with Glib.Graphs;   use Glib.Graphs;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gtkada.Canvas; use Gtkada.Canvas;
with Line_Sweep;    use Line_Sweep;
with Browsers.Canvas; use Browsers.Canvas;
--  with Traces;        use Traces;

package body Layouts is
--     Me : constant Debug_Handle := Create ("Layout");

   Max_Iterations : constant Natural := 21;
   --  Maximum number of iterations in Sort_Barycenter.

   Max_Position_Iteration : constant Natural := 20;
   --  Maximum number of iterations in Position_Node

   Min_Layer_Dist : constant Natural := 20;
   --  Minimum distance between two layers in the graph.

   Min_Node_Dist : constant Natural := 10;
   --  Minimum distance between two nodes on the same layer.

   Min_Node_Simple_Dist : constant Natural := 2 * Min_Node_Dist;
   --  Minimum distance between two nodes on the same layer used by the
   --  simple layout algorithm.
   --  ??? Would be good to merge the two Min_Node constants, but apparently
   --  Layer_Layout takes Min_Node_Dist twice into account, while Simple_Layout
   --  takes it into account once.

   Layer_Align : constant Float := 0.0;
   --  Alignment of items in each layer. This should be a number between 0.0
   --  (0%) and 1.0 (100%)

   type Natural_Array is array (Natural range <>) of Natural;

   type Node_Matrix is array (Natural range <>, Natural range <>)
     of Vertex_Access;
   --  First index is the layer number, starting at 0.
   --  Second index is the column number, starting from 0. These are not the
   --  coordinates of the vertices, but the relative positions on the layer.

   procedure Partition_Topological
     (G              : Graph;
      Layers         : out Natural_Array;
      Num_Layers     : out Natural;
      New_Items_Only : Boolean);
   --  Partition the nodes through a depth-first search and topological sort.
   --  The nodes are organized into several layers.
   --  Layers are numbered from 1 to Num_Layers, and the layer for each node is
   --  available on exit in Layers.
   --  If New_Items_Only is true, then only the items whose position has never
   --  been computed will be taken into account.

   procedure Sort_Layers
     (G               : Graph;
      Lines           : in out Node_Matrix;
      Layers          : Natural_Array;
      X, Y            : out Integer_Array;
      Num_Per_Line    : Natural_Array;
      Vertical_Layout : Boolean);
   --  Sort the nodes on each layer with the barycenter method.
   --  This provides a relative position for the nodes on each layer, such that
   --  there are only few edge crossings.

   procedure Position_Nodes
     (G               : Graph;
      Lines           : Node_Matrix;
      X               : in out Integer_Array;
      Num_Per_Line    : Natural_Array;
      Vertical_Layout : Boolean);
   --  Positions the nodes within each layer.
   --  The nodes are already ordered, we now need to fine tune their location.
   --  Lines represents the location of all the nodes. Num_Per_Line is the
   --  number of nodes per line.

   function Num_Intersections_For_Layer
     (G                  : Graph;
      Layer              : Integer;
      Num_Nodes_On_Layer : Natural;
      X, Y               : Integer_Array;
      VL                 : Boolean;
      Lines              : Node_Matrix) return Natural;
   --  Return the number of intersections between the two layers
   --  Layer - 1 and Layer.

   function Rubber_Band
     (G : Graph; V : Vertex_Access; X : Integer_Array) return Integer;
   --  Return the force that applies to a given vertex. This uses the rubber
   --  band method, where each edge pulls the nodes.

   function Width
     (V : access Vertex'Class;
      Vertical_Layout : Boolean) return Integer;
   pragma Inline (Width);
   --  Return the width of V

   function Height
     (V : access Vertex'Class;
      Vertical_Layout : Boolean) return Integer;
   pragma Inline (Height);
   --  Return the height of V

   procedure Reverse_Edge (G : Graph; E : Edge_Access);
   --  Reverse the two ends of the edge, so as to make the graph acyclick

   function Never_Positionned (V : access Vertex'Class) return Boolean;
   --  Return True if the V has never had a position assigned to it

   -----------------------
   -- Never_Positionned --
   -----------------------

   function Never_Positionned (V : access Vertex'Class) return Boolean is
      Coord : constant Gdk_Rectangle := Get_Coord (Canvas_Item (V));
   begin
      return Coord.X = Gint'First and Coord.Y = Gint'First;
   end Never_Positionned;

   -----------
   -- Width --
   -----------

   function Width
     (V : access Vertex'Class;
      Vertical_Layout : Boolean) return Integer is
   begin
      if Vertical_Layout then
         return Integer (Get_Coord (Canvas_Item (V)).Width);
      else
         return Integer (Get_Coord (Canvas_Item (V)).Height);
      end if;
   end Width;

   ------------
   -- Height --
   ------------

   function Height
     (V : access Vertex'Class;
      Vertical_Layout : Boolean) return Integer is
   begin
      if Vertical_Layout then
         return Integer (Get_Coord (Canvas_Item (V)).Height);
      else
         return Integer (Get_Coord (Canvas_Item (V)).Width);
      end if;
   end Height;

   ------------------
   -- Reverse_Edge --
   ------------------

   procedure Reverse_Edge (G : Graph; E : Edge_Access) is
      Arrow : Arrow_Type := Get_Arrow_Type (Canvas_Link (E));
   begin
      Glib.Graphs.Revert_Edge (G, E);

      case Arrow is
         when No_Arrow | Both_Arrow => null;
         when Start_Arrow => Arrow := End_Arrow;
         when End_Arrow   => Arrow := Start_Arrow;
      end case;

      Configure (Canvas_Link (E), Arrow, Get_Descr (Canvas_Link (E)));
   end Reverse_Edge;

   ---------------------------
   -- Partition_Topological --
   ---------------------------

   procedure Partition_Topological
     (G          : Graph;
      Layers     : out Natural_Array;
      Num_Layers : out Natural;
      New_Items_Only : Boolean)
   is
      Acyclic : aliased Boolean;
      Sorted  : constant Depth_Vertices_Array := Depth_First_Search
        (G, Acyclic'Access, Reverse_Edge'Access);
      Eit     : Edge_Iterator;
      Max     : Natural;
      V       : Vertex_Access;
      Num     : Natural := 0;
   begin
      pragma Assert (Is_Directed (G), "graph must be directed");
      pragma Assert (Acyclic, "graph must be acyclic");

      Layers := (others => 0);

      for S in Sorted'Range loop
         if not New_Items_Only
           or else Never_Positionned (Sorted (S).Vertex)
         then
            Max := 1;

            Eit := First (G, Dest => Sorted (S).Vertex);
            while not At_End (Eit) loop
               V   := Get_Src (Get (Eit));

               if Get_Orthogonal (Browser_Link (Get (Eit))) then
                  --  Preferrable on same layer as Src
                  Max := Natural'Max (Layers (Get_Index (V)), Max);
               else
                  Max := Natural'Max (Layers (Get_Index (V)) + 1, Max);
               end if;

               Next (Eit);
            end loop;

            Layers (Get_Index (Sorted (S).Vertex)) := Max;
            Num := Natural'Max (Num, Max);
         end if;
      end loop;

      Num_Layers := Num;
   end Partition_Topological;

   ---------------------------------
   -- Num_Intersections_For_Layer --
   ---------------------------------

   function Num_Intersections_For_Layer
     (G                  : Graph;
      Layer              : Integer;
      Num_Nodes_On_Layer : Natural;
      X, Y               : Integer_Array;
      VL                 : Boolean;
      Lines              : Node_Matrix) return Natural
   is
      Edges_Count : Natural := 0;
      Iter        : Edge_Iterator;
   begin
      --  ??? Could be initialized once and for all
      for J in 0 .. Num_Nodes_On_Layer - 1 loop
         Iter := First (G, Src => Lines (Layer, J));
         while not At_End (Iter) loop
            Edges_Count := Edges_Count + 1;
            Next (Iter);
         end loop;
      end loop;

      declare
         X1, Y1, X2, Y2 : Integer_Array (1 .. Edges_Count);
         Index : Natural := X1'First;
         V     : Canvas_Item;

      begin
         for J in 0 .. Num_Nodes_On_Layer - 1 loop
            Iter := First (G, Src => Lines (Layer, J));
            while not At_End (Iter) loop
               V := Canvas_Item (Get_Src (Get (Iter)));
               X1 (Index) := X (Get_Index (V)) + Width (V, VL) / 2;
               Y1 (Index) := Y (Get_Index (V)) + Height (V, VL) / 2;

               V := Canvas_Item (Get_Dest (Get (Iter)));
               X2 (Index) := X (Get_Index (V)) + Width (V, VL) / 2;
               Y2 (Index) := Y (Get_Index (V)) + Height (V, VL) / 2;

               Index := Index + 1;
               Next (Iter);
            end loop;
         end loop;

         return Intersections_Count (X1, Y1, X2, Y2);
      end;
   end Num_Intersections_For_Layer;

   -----------------
   -- Sort_Layers --
   -----------------

   procedure Sort_Layers
     (G               : Graph;
      Lines           : in out Node_Matrix;
      Layers          : Natural_Array;
      X, Y            : out Integer_Array;
      Num_Per_Line    : Natural_Array;
      Vertical_Layout : Boolean)
   is
      Relative_Position : Natural_Array (0 .. Max_Index (G) - 1);

      function Barycenter_Weight_P (Vertex : Vertex_Access) return Integer;
      --  Return the weight to use for the vertex Vertex.
      --  This is the weight computed in the top-down loop

      function Barycenter_Weight_S (Vertex : Vertex_Access) return Integer;
      --  Return the weight to use for the vertex Vertex.
      --  This is the weight computed in the bottom-up loop

      function Median_Weight_P (Vertex : Vertex_Access) return Integer;
      pragma Unreferenced (Median_Weight_P);
      --  Return the median weight for Vertex.
      --  In case there is an even number of values, we choose an interpolated
      --  value biased toward the side where vertices are more closely packed.
      --  Currently unused, but could replace barycenter_weight_p function.

      function Median_Weight_S (Vertex : Vertex_Access) return Integer;
      pragma Unreferenced (Median_Weight_S);
      --  Return the median weight for Vertex.
      --  Currently unused, but could replace barycenter_weight_s function.

      procedure Process_Layer (Row : Natural; Top_Bottom : Boolean);
      --  Compute the positions for the nodes on a specific layer.

      procedure Set_Initial_Positions;
      --  Compute the initial position of the vertices.

      ---------------------
      -- Median_Weight_P --
      ---------------------

      function Median_Weight_P (Vertex : Vertex_Access) return Integer is
         Src : Vertex_Access;
         Iter : Edge_Iterator := First (G, Dest => Vertex);
         In_Deg : Natural := 0;
         Index : Natural;
         F, L, Prev : Integer;
      begin
         while not At_End (Iter) loop
            Src := Get_Src (Get (Iter));

            --  The test below is used to support edges that span multiple
            --  layers. In practice, it is better to insert dummy nodes to
            --  split the edges.
            if abs (Layers (Get_Index (Src)) - Layers (Get_Index (Vertex)))
              = 1
            then
               In_Deg := In_Deg + 1;
               L      := Relative_Position (Get_Index (Src));
            end if;
            Next (Iter);
         end loop;

         if In_Deg = 0 then
            return -1;
         end if;

         Iter  := First (G, Dest => Vertex);
         Src   := Get_Src (Get (Iter));
         F     := Relative_Position (Get_Index (Src));
         Prev  := F;
         Index := 1;

         while Index < (In_Deg / 2) loop
            Src := Get_Src (Get (Iter));
            if abs (Layers (Get_Index (Src)) - Layers (Get_Index (Vertex)))
              = 1
            then
               Prev  := Relative_Position (Get_Index (Src));
               Index := Index + 1;
            end if;
            Next (Iter);
         end loop;

         Src := Get_Src (Get (Iter));

         if In_Deg mod 2 = 1 then
            return Relative_Position (Get_Index (Src));

         elsif In_Deg = 2 then
            return (F + L) / 2;

         else
            F := Prev - F;
            L := L - Relative_Position (Get_Index (Src));

            pragma Assert (F + L /= 0,
                           "Edges should not cross multiple-layers");
            return (Prev * L + Relative_Position (Get_Index (Src)) * F)
              / (F + L);
         end if;
      end Median_Weight_P;

      -------------------------
      -- Barycenter_Weight_P --
      -------------------------

      function Barycenter_Weight_P (Vertex : Vertex_Access) return Integer is
         Src    : Vertex_Access;
         Iter   : Edge_Iterator := First (G, Dest => Vertex);
         In_Deg : Natural := 0;
         Weight : Integer := 0;
      begin
         while not At_End (Iter) loop
            In_Deg := In_Deg + 1;
            Src    := Get_Src (Get (Iter));
            Weight := Weight + Relative_Position (Get_Index (Src));
            Next (Iter);
         end loop;

         if In_Deg = 0 then
            return -1;
         else
            return Weight / In_Deg;
         end if;
      end Barycenter_Weight_P;

      ---------------------
      -- Median_Weight_S --
      ---------------------

      function Median_Weight_S (Vertex : Vertex_Access) return Integer is
         Src        : Vertex_Access;
         Iter       : Edge_Iterator := First (G, Src => Vertex);
         In_Deg     : Natural := 0;
         Index      : Natural;
         F, L, Prev : Integer;
      begin
         while not At_End (Iter) loop
            Src := Get_Dest (Get (Iter));
            if abs (Layers (Get_Index (Src)) - Layers (Get_Index (Vertex)))
              = 1
            then
               In_Deg := In_Deg + 1;
               L      := Relative_Position (Get_Index (Src));
            end if;
            Next (Iter);
         end loop;

         if In_Deg = 0 then
            return -1;
         end if;

         Iter := First (G, Src => Vertex);
         Src  := Get_Dest (Get (Iter));
         F    := Relative_Position (Get_Index (Src));
         Prev := F;

         Index := 1;
         while Index < (In_Deg / 2) loop
            Src := Get_Dest (Get (Iter));
            if abs (Layers (Get_Index (Src)) - Layers (Get_Index (Vertex)))
              = 1
            then
               Prev := Relative_Position (Get_Index (Src));
               Index := Index + 1;
            end if;
            Next (Iter);
         end loop;

         Src := Get_Dest (Get (Iter));

         if In_Deg mod 2 = 1 then
            return Relative_Position (Get_Index (Src));

         elsif In_Deg = 2 then
            return (F + L) / 2;

         else
            F := Prev - F;
            L := L - Relative_Position (Get_Index (Src));

            pragma Assert
              (F + L /= 0, "Edges should not cross multiple-layers");
            return
              (Prev * L + Relative_Position (Get_Index (Src)) * F) / (F + L);
         end if;
      end Median_Weight_S;

      -------------------------
      -- Barycenter_Weight_S --
      -------------------------

      function Barycenter_Weight_S (Vertex : Vertex_Access) return Integer is
         Src    : Vertex_Access;
         Iter   : Edge_Iterator := First (G, Src => Vertex);
         In_Deg : Natural := 0;
         Weight : Integer := 0;
      begin
         while not At_End (Iter) loop
            In_Deg := In_Deg + 1;
            Src    := Get_Dest (Get (Iter));
            Weight := Weight + Relative_Position (Get_Index (Src));
            Next (Iter);
         end loop;

         if In_Deg = 0 then
            return -1;
         else
            return Weight / In_Deg;
         end if;
      end Barycenter_Weight_S;

      -------------------
      -- Process_Layer --
      -------------------

      procedure Process_Layer (Row : Natural; Top_Bottom : Boolean) is
         Weights  : array (0 .. Max_Index (G) - 1) of Integer;
         L        : Vertex_Access;
         Switched : Boolean;
         C1, C2   : Natural;
         Xsave    : Integer;
      begin
         --  For each node in that layer.
         --  We try to keep the network stable, we process the vertices in
         --  their current order on the layer.

         for Column in 0 .. Num_Per_Line (Row) - 1 loop
            if Top_Bottom then
               Weights (Get_Index (Lines (Row, Column))) :=
                 Barycenter_Weight_P (Lines (Row, Column));
            else
               Weights (Get_Index (Lines (Row, Column))) :=
                 Barycenter_Weight_S (Lines (Row, Column));
            end if;
         end loop;

         --  Sort the nodes according to their weights.
         --  To ensure stability of the network, we keep the nodes that had no
         --  ancestor in the previous layer at the same location, instead of
         --  bringing them back to the beginning.
         --  Note also that while swapping vertices, we have to change the
         --  coordinates, so that they don't override.
         --  ??? A simple Bubble Sort, which might not be the most efficient

         loop
            Switched := False;

            for Column in 0 .. Num_Per_Line (Row) - 2 loop
               if Weights (Get_Index (Lines (Row, Column))) >
                 Weights (Get_Index (Lines (Row, Column + 1)))
                 and then Weights (Get_Index (Lines (Row, Column + 1))) /= -1
               then
                  Xsave := X (Get_Index (Lines (Row, Column)));
                  X (Get_Index (Lines (Row, Column))) :=
                    X (Get_Index (Lines (Row, Column + 1)))
                    + Width (Lines (Row, Column + 1), Vertical_Layout)
                    - Width (Lines (Row, Column), Vertical_Layout);
                  X (Get_Index (Lines (Row, Column + 1))) := Xsave;

                  L                       := Lines (Row, Column);
                  Lines (Row, Column)     := Lines (Row, Column + 1);
                  Lines (Row, Column + 1) := L;

                  Switched                := True;
               end if;
            end loop;

            exit when not Switched;
         end loop;

         --  Flip the order of the nodes with equal weight. This is used to
         --  shake the graph, so that we do not end up in a local minima.

         if not Top_Bottom then
            C1 := 0;
            while C1 < Num_Per_Line (Row) loop
               C2 := C1 + 1;
               while C2 < Num_Per_Line (Row)
                 and then Weights (Get_Index (Lines (Row, C2))) =
                 Weights (Get_Index (Lines (Row, C1)))
               loop
                  C2 := C2 + 1;
               end loop;

               Xsave := X (Get_Index (Lines (Row, C2 - 1)))
                 + Width (Lines (Row, C2 - 1), Vertical_Layout)
                 - Width (Lines (Row, C1), Vertical_Layout);
               for C in reverse C1 + 1 .. C2 - 1 loop
                  X (Get_Index (Lines (Row, C))) :=
                    X (Get_Index (Lines (Row, C - 1)))
                    + Width (Lines (Row, C - 1), Vertical_Layout)
                    - Width (Lines (Row, C), Vertical_Layout);
               end loop;
               X (Get_Index (Lines (Row, C1))) := Xsave;

               L := Lines (Row, C1);
               for C in C1 .. C2 - 2 loop
                  Lines (Row, C) := Lines (Row, C + 1);
               end loop;
               Lines (Row, C2 - 1) := L;

               C1 := C2;
            end loop;
         end if;

         --  Try and transpose all the pairs of nodes on the layer, and see if
         --  we can reduce the number of edges this way
         --  Switched := True;
         --  while Switched loop
         --     Switched := False;

         --     Intersections := Num_Intersections_For_Layer
         --       (G, Row, Num_Per_Line (Row), Lines);
         --     if Row > Lines'First (1) then
         --        Intersections := Intersections + Num_Intersections_For_Layer
         --          (G, Row - 1, Num_Per_Line (Row - 1), Lines);
         --     end if;

         --     Put_Line ("Intersections on layer " & Row'Img
         --               & " = " & Intersections'Img);

         --     for Column in 0 .. Num_Per_Line (Row) - 2 loop

         --        L := Lines (Row, Column + 1);
         --        Lines (Row, Column + 1) := Lines (Row, Column);
         --        Lines (Row, Column) := L;

         --        Inter := Num_Intersections_For_Layer
         --          (G, Row, Num_Per_Line (Row), Lines);
         --        if Row > Lines'First (1) then
         --           Inter := Inter + Num_Intersections_For_Layer
         --             (G, Row - 1, Num_Per_Line (Row - 1), Lines);
         --        end if;

         --        Put_Line ("   Try switching " & Column'Img
         --                  & Natural'Image (Column + 1)
         --                  & " => Inter=" &  Inter'Img);

         --        if Inter < Intersections then
         --           Put_Line ("Switching on layer " & Row'Img
         --                     & "  Nodes=" & Column'Img
         --                     & Integer'Image (Column + 1));
         --           Intersections := Num_Intersections_For_Layer
         --             (G, Row, Num_Per_Line (Row), Lines);
         --           Switched := True;
         --        else
         --           L := Lines (Row, Column + 1);
         --           Lines (Row, Column + 1) := Lines (Row, Column);
         --           Lines (Row, Column) := L;
         --        end if;

         --     end loop;
         --  end loop;
      end Process_Layer;

      ---------------------------
      -- Set_Initial_Positions --
      ---------------------------

      procedure Set_Initial_Positions is
         Current_X, Max_Height : Natural;
         Current_Y             : Natural := 0;
      begin
         for R in Lines'Range (1) loop
            Current_X := 0;
            Max_Height := 0;

            for C in 0 .. Num_Per_Line (R) - 1 loop
               Relative_Position (Get_Index (Lines (R, C))) := C;
               X (Get_Index (Lines (R, C))) := Current_X;

               Current_X := Current_X + Min_Node_Dist
                 + Width (Lines (R, C), Vertical_Layout);
               Max_Height := Natural'Max
                 (Max_Height, Height (Lines (R, C), Vertical_Layout));
            end loop;

            Current_Y := Current_Y + Min_Layer_Dist + Max_Height / 2;

            for C in 0 .. Num_Per_Line (R) - 1 loop
               Y (Get_Index (Lines (R, C))) := Current_Y
                 - Natural (Layer_Align *
                         Float (Height (Lines (R, C), Vertical_Layout)));
            end loop;

            Current_Y := Current_Y + Max_Height / 2;
         end loop;
      end Set_Initial_Positions;

      Intersections : Natural;
      Iteration : Natural := 0;
      Min_Intersections : Natural := Natural'Last;

   begin
      --  Initialize the internal data, to speed up the other loops.
      --  Note that this also positions the nodes vertically, and this won't
      --  need to be changed afterwards.

      Set_Initial_Positions;

      --  While the number of crossing is not satisfactory

      loop

         --  Two loops are executed: one from top to bottom, the other one in
         --  the opposite direction.

         for Row in Lines'First (1) + 1 .. Lines'Last (1) loop
            Process_Layer (Row, True);
         end loop;

         Iteration := Iteration + 1;
         exit when Iteration > Max_Iterations;

         for Row in reverse Lines'First (1) .. Lines'Last (1) - 1 loop
            Process_Layer (Row, False);
         end loop;

         Iteration := Iteration + 1;
         exit when Iteration > Max_Iterations;

         --  Compute the number of intersections

         Intersections := 0;

         for J in Lines'First (1) .. Lines'Last (1) - 1 loop
            Intersections := Intersections + Num_Intersections_For_Layer
              (G, J, Num_Per_Line (J), X, Y, Vertical_Layout, Lines);
         end loop;

         if Intersections <= Min_Intersections then
            Min_Intersections := Intersections;
         end if;

         --  ??? Should compare the current position with the previous, and
         --  ??? restore the previous if needed.
         exit when Intersections = 0;
      end loop;
   end Sort_Layers;

   -----------------
   -- Rubber_Band --
   -----------------

   function Rubber_Band
     (G : Graph; V : Vertex_Access; X : Integer_Array) return Integer
   is
      E      : Edge_Iterator;
      S      : Vertex_Access;
      Degree : Natural := 0;
      Sum    : Integer := 0;
   begin
      E := First (G, Src => V);
      while not At_End (E) loop
         S      := Get_Dest (Get (E));
         Sum    := Sum + (X (Get_Index (S)) - X (Get_Index (V)));
         Degree := Degree + 1;
         Next (E);
      end loop;

      E := First (G, Dest => V);
      while not At_End (E) loop
         S      := Get_Src (Get (E));
         Sum    := Sum + (X (Get_Index (S)) - X (Get_Index (V)));
         Degree := Degree + 1;
         Next (E);
      end loop;

      if Degree = 0 then
         return 0;
      else
         return Sum / Degree;
      end if;
   end Rubber_Band;

   --------------------
   -- Position_Nodes --
   --------------------

   procedure Position_Nodes
     (G               : Graph;
      Lines           : Node_Matrix;
      X               : in out Integer_Array;
      Num_Per_Line    : Natural_Array;
      Vertical_Layout : Boolean)
   is
      Rubber    : array (0 .. Max_Index (G) - 1) of Integer;
      Iteration : Natural := 0;
      X1, X2, Min, Dist : Integer;
      C1, C2 : Natural;

   begin
      --  While the stability of the graph is not satisfactory
      while Iteration < Max_Position_Iteration loop

         --  For each layer
         for Row in Lines'Range (1) loop

            --  For each node in the row, compute the force that applies to the
            --  node.

            for Column in 0 .. Num_Per_Line (Row) - 1 loop
               Rubber (Get_Index (Lines (Row, Column))) :=
                 Rubber_Band (G, Lines (Row, Column), X);
            end loop;

            --  Move all the nodes according to these forces. This needs to be
            --  done only once we have computed the forces.
            --  The nodes are grouped into regions, so that when two nodes
            --  touch each other, and the left-most one is dragged to the right
            --  when the other one is dragged to the left, we are still able to
            --  find the best location.

            C1 := 0;
            while C1 <= Num_Per_Line (Row) - 1 loop
               C2 := C1 + 1;

               --  Compute the current region (touching nodes, with
               --  interacting forces)

               while C2 < Num_Per_Line (Row)
                 and then Rubber (Get_Index (Lines (Row, C2 - 1))) >=
                 Rubber (Get_Index (Lines (Row, C2)))
                 and then (X (Get_Index (Lines (Row, C2)))
                           - X (Get_Index (Lines (Row, C2 - 1))))
                 = (Width (Lines (Row, C2), Vertical_Layout)
                    + Width (Lines (Row, C2 - 1), Vertical_Layout)) / 2
                 + Min_Node_Dist
               loop
                  C2 := C2 + 1;
               end loop;

               --  Compute the global force for the region

               Dist := 0;
               for C in C1 .. C2 - 1 loop
                  Dist := Dist + Rubber (Get_Index (Lines (Row, C)));
               end loop;

               Dist := Dist / (C2 - C1);

               --  Compute the available space to the left and right

               if Dist < 0 then
                  if C1 > 0 then
                     X1  := X (Get_Index (Lines (Row, C1)))
                       + Width (Lines (Row, C1), Vertical_Layout) / 2;
                     X2 := X (Get_Index (Lines (Row, C1 - 1)))
                       + Width (Lines (Row, C1 - 1), Vertical_Layout) / 2;
                     Min := (Width (Lines (Row, C1 - 1), Vertical_Layout)
                             + Width (Lines (Row, C1), Vertical_Layout)) / 2;
                     Dist :=
                       -Integer'Min (-Dist, X1 - X2 - Min - Min_Node_Dist);
                  end if;
               else
                  if C2 < Num_Per_Line (Row) then
                     X1  := X (Get_Index (Lines (Row, C2 - 1)))
                       + Width (Lines (Row, C2 - 1), Vertical_Layout) / 2;
                     X2 := X (Get_Index (Lines (Row, C2)))
                       + Width (Lines (Row, C2), Vertical_Layout) / 2;
                     Min := (Width (Lines (Row, C2), Vertical_Layout)
                       + Width (Lines (Row, C2 - 1), Vertical_Layout)) / 2;
                     Dist :=
                       Integer'Min (Dist, X2 - X1 - Min - Min_Node_Dist);
                  end if;
               end if;

               --  Move all the nodes in the region

               for C in C1 .. C2 - 1 loop
                  X (Get_Index (Lines (Row, C))) :=
                    X (Get_Index (Lines (Row, C))) + Dist;
               end loop;

               --  Move to the next region
               C1 := C2;
            end loop;
         end loop;

         Iteration := Iteration + 1;
      end loop;
   end Position_Nodes;

   ------------
   -- Layout --
   ------------

   procedure Layer_Layout
     (Canvas          : access Interactive_Canvas_Record'Class;
      Graph           : Glib.Graphs.Graph;
      Force           : Boolean := False;
      Vertical_Layout : Boolean := True)
   is
      pragma Unreferenced (Force);

      Layers : Natural_Array (0 .. Max_Index (Graph) - 1);
      X, Y   : Integer_Array (0 .. Max_Index (Graph) - 1);
      Num_Layers : Natural;
      Iter   : Vertex_Iterator;
   begin
      --  ??? We could also use the network simplex algorithm to assign the
      --  ranks.
      Partition_Topological (Graph, Layers, Num_Layers, False);

      --  Insert dummy nodes so that edges do not cross layers.
      --  Insert_Dummy_Nodes (G);

      --  Initialize a random order in the layers.

      declare
         Lines        : Node_Matrix
           (1 .. Num_Layers, 0 .. Max_Index (Graph) - 1);
         Num_Per_Line : Natural_Array (1 .. Num_Layers) := (others => 0);
         Iter         : Vertex_Iterator := First (Graph);
         Layer        : Natural;
      begin
         while not At_End (Iter) loop
            Layer := Layers (Get_Index (Get (Iter)));

            Lines (Layer, Num_Per_Line (Layer)) := Get (Iter);
            Num_Per_Line (Layer) := Num_Per_Line (Layer) + 1;
            Next (Iter);
         end loop;

         Sort_Layers
           (Graph, Lines, Layers, X, Y, Num_Per_Line, Vertical_Layout);
         Position_Nodes (Graph, Lines, X, Num_Per_Line, Vertical_Layout);
      end;

      Iter := First (Graph);
      while not At_End (Iter) loop
         if Vertical_Layout then
            Move_To
              (Canvas, Canvas_Item (Get (Iter)),
               Gint (X (Get_Index (Get (Iter)))),
               Gint (Y (Get_Index (Get (Iter)))));
         else
            Move_To
              (Canvas, Canvas_Item (Get (Iter)),
               Gint (Y (Get_Index (Get (Iter)))),
               Gint (X (Get_Index (Get (Iter)))));
         end if;
         Next (Iter);
      end loop;
   end Layer_Layout;

   -------------------
   -- Simple_Layout --
   -------------------

   procedure Simple_Layout
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Graph  : Glib.Graphs.Graph;
      Force  : Boolean := False;
      Vertical_Layout : Boolean := True)
   is
      Max_X, Max_Y    : Integer := Integer'First;
      Min_X, Min_Y    : Integer := Integer'Last;
      Layers          : Natural_Array (0 .. Max_Index (Graph) - 1);
      Num_Layers      : Natural;

      procedure Find_Bounding_Boxes (Sizes : in out Integer_Array);
      --  Compute the bounding fox items already positioned, or the size of
      --  each of the new layers. The size of the new layers is stored in
      --  Layers_Size.

      procedure Space_Columns (Pos : in out Integer_Array);
      --  Compute the left-most pixel for each new layer.

      function Compute_Columns_Start (Layer : Natural) return Integer;
      --  Return the start coordinate for the Layer-th column.
      --  We try to organize items so that they are centered around their
      --  parent:                B
      --                     A - C
      --                         D
      --  This is not mandatory (and slightly inefficient), but produces
      --  better results for a minimal cost.
      --  This function could return Min_X instead, but then each column would
      --  start at the same Y pos.
      --                     A - B
      --                         C
      --                         D
      --  As opposed to Layer_Layout, this algorithm doesn't try to split the
      --  items on the layer into multiple groups, but will group all of them
      --  graphically. This is less general, although in the general case this
      --  is enough in the context of GPS.

      procedure Move_Items
        (Layer : Natural; Align_On : Integer);
      --  Move all items for the given layer at the right position.
      --  Align_On indicates the coordinate of the aligned edge for the nodes.

      -------------------------
      -- Find_Bounding_Boxes --
      -------------------------

      procedure Find_Bounding_Boxes (Sizes : in out Integer_Array) is
         Iter  : Vertex_Iterator := First (Graph);
         Rect  : Gdk.Rectangle.Gdk_Rectangle;
         Layer : Natural;
      begin
         while not At_End (Iter) loop
            Rect  := Get_Coord (Canvas_Item (Get (Iter)));

            if not Force and then Rect.Y /= Gint'First then
               Max_X := Integer'Max (Max_X, Integer (Rect.X + Rect.Width));
               Max_Y := Integer'Max (Max_Y, Integer (Rect.Y + Rect.Height));
               Min_X := Integer'Min (Min_X, Integer (Rect.X));
               Min_Y := Integer'Min (Min_Y, Integer (Rect.Y));
            else
               Layer := Layers (Get_Index (Get (Iter)));
               if Vertical_Layout then
                  Sizes (Layer) :=
                    Natural'Max (Sizes (Layer), Natural (Rect.Height));
               else
                  Sizes (Layer) :=
                    Natural'Max (Sizes (Layer), Natural (Rect.Width));
               end if;
            end if;

            Next (Iter);
         end loop;
      end Find_Bounding_Boxes;

      -------------------
      -- Space_Columns --
      -------------------

      procedure Space_Columns (Pos : in out Integer_Array) is
      begin
         if Max_X < Min_X then
            Min_X := 0;
            Min_Y := 0;
         else
            if Vertical_Layout then
               Pos (Pos'First) := Pos (Pos'First) + Max_Y + Min_Layer_Dist;
            else
               Pos (Pos'First) := Pos (Pos'First) + Max_X + Min_Layer_Dist;
            end if;
         end if;

         for L in Pos'First + 1 .. Pos'Last loop
            Pos (L) := Pos (L) + Pos (L - 1) + Min_Layer_Dist;
         end loop;
      end Space_Columns;

      ---------------------------
      -- Compute_Columns_Start --
      ---------------------------

      function Compute_Columns_Start (Layer : Natural) return Integer is
         Current         : Natural := 0;
         Ancestors_Count : Natural := 0;
         Parent          : Integer := 0;
         Iter            : Vertex_Iterator := First (Graph);
         Rect            : Gdk_Rectangle;
         Eit             : Edge_Iterator;
      begin
         while not At_End (Iter) loop
            --  The way we compute the mean position of the ancestors will
            --  count several times a common ancestor for several items, thus
            --  centering more around it. This is in fact a nicer layout.

            --  The following test automatically excludes already
            --  positionned items, since they are in layer 0.

            if Layers (Get_Index (Get (Iter))) = Layer then
               Rect := Get_Coord (Canvas_Item (Get (Iter)));
               if Vertical_Layout then
                  Current := Current + Natural (Rect.Width) + Min_Node_Dist;
               else
                  Current := Current + Natural (Rect.Height) + Min_Node_Dist;
               end if;

               Eit := First (Graph, Dest => Get (Iter));
               while not At_End (Eit) loop
                  if not Never_Positionned (Get_Src (Get (Eit))) then
                     Rect  := Get_Coord (Canvas_Item (Get_Src (Get (Eit))));

                     if Vertical_Layout then
                        Parent := Parent + Integer (Rect.X + Rect.Width / 2);
                     else
                        Parent := Parent + Integer (Rect.Y + Rect.Height / 2);
                     end if;

                     Ancestors_Count := Ancestors_Count + 1;
                  end if;

                  Next (Eit);
               end loop;
            end if;

            Next (Iter);
         end loop;

         if Ancestors_Count = 0 then
            if Vertical_Layout then
               return Min_X;
            else
               return Min_Y;
            end if;
         else
            return Parent / Ancestors_Count
              - (Current - Min_Node_Simple_Dist) / 2;
         end if;
      end Compute_Columns_Start;

      ----------------
      -- Move_Items --
      ----------------

      procedure Move_Items (Layer : Natural; Align_On : Integer) is
         Coord : Integer := Compute_Columns_Start (Layer);
         Iter  : Vertex_Iterator := First (Graph);
         Rect  : Gdk.Rectangle.Gdk_Rectangle;
         Item  : Canvas_Item;
      begin
         while not At_End (Iter) loop
            Item := Canvas_Item (Get (Iter));
            if Layers (Get_Index (Item)) = Layer then
               if Force or else Never_Positionned (Item) then
                  Rect  := Get_Coord (Item);
                  if Vertical_Layout then
                     Move_To (Canvas, Item, Gint (Coord), Gint (Align_On));
                     Coord :=
                       Coord + Natural (Rect.Width) + Min_Node_Simple_Dist;
                  else
                     Move_To (Canvas, Item, Gint (Align_On), Gint (Coord));
                     Coord :=
                       Coord + Natural (Rect.Height) + Min_Node_Simple_Dist;
                  end if;
               end if;
            end if;

            Next (Iter);
         end loop;
      end Move_Items;

   begin
      Partition_Topological (Graph, Layers, Num_Layers, True);

      declare
         Layers_Size  : Integer_Array (0 .. Num_Layers) := (others => 0);
      begin
         Find_Bounding_Boxes (Layers_Size);
         Space_Columns (Layers_Size);

         for L in 1 .. Num_Layers loop
            Move_Items (L, Layers_Size (L - 1));
         end loop;
      end;
   end Simple_Layout;

end Layouts;
