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

--  See paper "A Generic Plane-Sweep For Intersecting Line Segments"
--    W. Freiseisen & P. Pau

with Unchecked_Deallocation;

package body Line_Sweep is

   type Event_Type is (Source_Vertex, Target_Vertex, Intersection_Point);

   subtype Edge_Index is Integer;
   No_Edge : constant Edge_Index := Edge_Index'First;

   type Event_Point is record
      Typ     : Event_Type;
      X, Y    : Integer;
      Edge1   : Edge_Index;
      Edge2   : Edge_Index := No_Edge;
      --  Vertex2 is only meaningful for intersection points
      --  Invariant: Edge1 is always above Edge2.
   end record;
   --  A special point where the sweep line status changes

   Null_Point : constant Event_Point :=
     (Source_Vertex, 0, 0, No_Edge, No_Edge);

   type Event_Queue_Item;
   type Event_Queue_Item_Access is access Event_Queue_Item;
   type Event_Queue_Item is record
      Event : Event_Point;
      Prev  : Event_Queue_Item_Access;
      Next  : Event_Queue_Item_Access;
   end record;
   --  ??? Temporary: the priority queue Event_Queue is simply handled
   --  ??? as a double-linked list for now

   type Event_Queue is record
      Head : Event_Queue_Item_Access;
   end record;
   --  List of all the events.
   --  This must provide fast operations for access/deletion of the first
   --  element, random insertion,...
   --  A priority_queue or balanced_tree could be used.

   type Line_Status_Item;
   type Line_Status_Item_Access is access Line_Status_Item;
   type Line_Status_Item is record
      Edge : Edge_Index;
      Prev : Line_Status_Item_Access;
      Next : Line_Status_Item_Access;
   end record;

   type Line_Status is record
      Head : Line_Status_Item_Access;
   end record;
   --  saves the sweep line status.
   --  This must provide fast insertion and deletion.
   --  The segments are kept ordered, according to the y coordinate of their
   --  intersection with the sweeping line.

   --  Outline of the algorithm:
   --    1: insert the vertices of the initial segments in the event queue
   --    2: line_status := empty
   --    3: while event_queue is not empty loop
   --    4:    next_event := extract_first (event_queue);
   --    5:    if next_event is
   --    6:        initial_vertex: treat the source vertex case
   --    7:        final_vertex:   treat the target vertex case
   --    8:        intersection:   treat the intersection point case

   -------------------------
   -- Intersections_Count --
   -------------------------

   function Intersections_Count
     (X1, Y1, X2, Y2 : Integer_Array) return Integer
   is
      function Point_Before (P1, P2 : Event_Point) return Boolean;
      --  Return True if P1 will be encountered before P2 by the sweeping line.
      --  The two points belong to the two segments S1 and S2

      function Segment_Below (S1, S2 : Edge_Index; X : Integer) return Boolean;
      --  Return True if S1 is below S2 at coordinate X

      procedure Add (Queue : in out Event_Queue; Event : Event_Point);
      --  Insert Event in the Queue

      function Minimum (Queue : Event_Queue) return Event_Point;
      --  Return the minimum element in Queue

      procedure Delete (Queue : in out Event_Queue; Event : Event_Point);
      --  Removes Event from the Queue.

      function Slope (S : Edge_Index) return Float;
      --  Return the slope of the edge

      function Sign (X : Float) return Integer;
      --  Return -1 if X is negative, 1 if positive, 0 if null.

      procedure Insert
        (Line      : in out Line_Status;
         Q         : in out Event_Queue;
         Edge      : Edge_Index;
         X         : Integer;
         Neighbor1 : out Edge_Index;
         Neighbor2 : out Edge_Index);
      --  Insert the Edge into Line

      procedure Delete
        (Line      : in out Line_Status;
         Edge      : Edge_Index;
         Neighbor1 : out Edge_Index;
         Neighbor2 : out Edge_Index);
      --  Remove Edge from Line, and returns its two ex-neighbors in Line

      function Y_At_Point (S : Edge_Index; X : Integer) return Integer;
      --  Return the Y coordinate of the edge at a specific X coordinate

      function Is_Source (S : Edge_Index; X : Integer) return Boolean;
      --  Return True if X is the source coordinate of S (ie the smallest X
      --  coordinate of the two vertices that S connects).

      procedure Reverse_Segments
        (Line                 : in out Line_Status;
         S1, S2               : Edge_Index;
         Neighbor1, Neighbor2 : out Edge_Index);
      --  Reverse the order of the two segments S1 and S2, and return
      --  their ex-neighbords Neighbor1 and Neighbor2.

      procedure Free is new Unchecked_Deallocation
        (Event_Queue_Item, Event_Queue_Item_Access);

      procedure Intersection
        (S1, S2    : Edge_Index;
         Inter     : out Event_Point;
         At_Origin : out Boolean);
      --  Return the intersection of the two segments, or Null_Point if there
      --  is no intersection.

      -------------
      -- Minimum --
      -------------

      function Minimum (Queue : Event_Queue) return Event_Point is
      begin
         if Queue.Head /= null then
            return Queue.Head.Event;
         else
            return Null_Point;
         end if;
      end Minimum;

      ------------
      -- Delete --
      ------------

      procedure Delete (Queue : in out Event_Queue; Event : Event_Point) is
         T : Event_Queue_Item_Access := Queue.Head;
      begin
         while T /= null loop
            if T.Event = Event then
               if T.Next /= null then
                  T.Next.Prev := T.Prev;
               end if;
               if T.Prev /= null then
                  T.Prev.Next := T.Next;
               end if;
               if Queue.Head = T then
                  Queue.Head := T.Next;
                  if Queue.Head /= null then
                     Queue.Head.Prev := null;
                  end if;
               end if;
               Free (T);
               return;
            end if;

            T := T.Next;
         end loop;
      end Delete;

      ---------
      -- Add --
      ---------

      procedure Add (Queue : in out Event_Queue; Event : Event_Point) is
         Tmp : Event_Queue_Item_Access;
         T, Prev : Event_Queue_Item_Access;
      begin
         --  ??? Just a dummy implementation for now

         Tmp := new Event_Queue_Item'
           (Event => Event, Next  => null, Prev  => null);

         if Event.Typ = Intersection_Point
           and then Segment_Below (Event.Edge1, Event.Edge2, Event.X)
         then
            Tmp.Event.Edge1 := Event.Edge2;
            Tmp.Event.Edge2 := Event.Edge1;
         end if;

         if Queue.Head = null then
            Queue.Head := Tmp;
         else
            T := Queue.Head;

            while T /= null loop
               if Event.Typ = Intersection_Point
                 and then T.Event.Typ = Intersection_Point
                 and then T.Event.X = Event.X
                 and then T.Event.Y = Event.Y
               then
                  --  Special case: we only insert intersections once in the
                  --  list, but register the upper-most and lower-most lines
                  if Segment_Below
                    (Tmp.Event.Edge2, T.Event.Edge2, T.Event.X)
                  then
                     T.Event.Edge2 := Tmp.Event.Edge2;
                  end if;

                  if Segment_Below
                    (T.Event.Edge1, Tmp.Event.Edge1, T.Event.X)
                  then
                     T.Event.Edge1 := Tmp.Event.Edge1;
                  end if;

                  --  The point has been registered in the queue.
                  return;

               elsif not Point_Before (T.Event, Event) then
                  exit;
               end if;

               Prev := T;
               T := T.Next;
            end loop;

            if T /= null then
               if T = Queue.Head then
                  Tmp.Next := Queue.Head;
                  Queue.Head.Prev := Tmp;
                  Queue.Head := Tmp;
               else
                  Tmp.Next := T.Prev.Next;
                  T.Prev.Next := Tmp;
                  Tmp.Prev := T.Prev;
                  T.Prev := Tmp;
               end if;
            else
               Prev.Next := Tmp;
               Tmp.Prev := Prev;
            end if;
         end if;
      end Add;

      ------------
      -- Delete --
      ------------

      procedure Delete
        (Line : in out Line_Status;
         Edge : Edge_Index;
         Neighbor1, Neighbor2 : out Edge_Index)
      is
         procedure Internal is new Unchecked_Deallocation
           (Line_Status_Item, Line_Status_Item_Access);

         Current : Line_Status_Item_Access := Line.Head;

      begin
         while Current /= null loop
            if Current.Edge = Edge then

               if Current.Prev /= null then
                  Neighbor1 := Current.Prev.Edge;
               else
                  Neighbor1 := No_Edge;
               end if;

               if Current.Next /= null then
                  Neighbor2 := Current.Next.Edge;
               else
                  Neighbor2 := No_Edge;
               end if;

               if Current.Next /= null then
                  Current.Next.Prev := Current.Prev;
               end if;

               if Current.Prev /= null then
                  Current.Prev.Next := Current.Next;
               end if;

               if Line.Head = Current then
                  Line.Head := Current.Next;

                  if Line.Head /= null then
                     Line.Head.Prev := null;
                  end if;
               end if;

               Internal (Current);
               return;
            end if;

            Current := Current.Next;
         end loop;

         Neighbor1 := No_Edge;
         Neighbor2 := No_Edge;
      end Delete;

      ----------------------
      -- Reverse_Segments --
      ----------------------

      procedure Reverse_Segments
        (Line                 : in out Line_Status;
         S1, S2               : Edge_Index;
         Neighbor1, Neighbor2 : out Edge_Index)
      is
         Current : Line_Status_Item_Access := Line.Head;
         S       : Edge_Index;

      begin
         while Current /= null loop
            if Current.Edge = S1 or else Current.Edge = S2 then
               if Current.Prev /= null then
                  Neighbor1 := Current.Prev.Edge;
               else
                  Neighbor1 := No_Edge;
               end if;

               S := Current.Edge;

               loop
                  pragma Assert (Current.Next /= null);
                  Current.Edge := Current.Next.Edge;
                  Current := Current.Next;
                  exit when Current.Edge = S1 or else Current.Edge = S2;
               end loop;

               Current.Edge := S;

               if Current.Next /= null then
                  Neighbor2 := Current.Next.Edge;
               else
                  Neighbor2 := No_Edge;
               end if;

               return;
            end if;

            Current := Current.Next;
         end loop;

         Neighbor1 := No_Edge;
         Neighbor2 := No_Edge;
      end Reverse_Segments;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Line      : in out Line_Status;
         Q         : in out Event_Queue;
         Edge      : Edge_Index;
         X         : Integer;
         Neighbor1 : out Edge_Index;
         Neighbor2 : out Edge_Index)
      is
         Tmp     : Line_Status_Item_Access;
         T, Prev : Line_Status_Item_Access;

      begin
         pragma Assert (Edge /= No_Edge);

         --  ??? Just a dummy implementation for now

         Tmp := new Line_Status_Item'
           (Edge => Edge, Next  => null, Prev  => null);

         if Line.Head = null then
            Line.Head := Tmp;
         else
            T := Line.Head;

            while T /= null
              and then Segment_Below (T.Edge, Edge, X)
            loop
               Prev := T;
               T := T.Next;
            end loop;

            if T /= null then
               if T.Prev = null then
                  Tmp.Next := Line.Head;
                  Line.Head.Prev := Tmp;
                  Line.Head := Tmp;
               else
                  Tmp.Next := T.Prev.Next;
                  T.Prev.Next := Tmp;
                  Tmp.Prev := T.Prev;
                  T.Prev := Tmp;
               end if;
            else
               Prev.Next := Tmp;
               Tmp.Prev := Prev;
            end if;
         end if;

         if Tmp.Prev /= null then
            Neighbor1 := Tmp.Prev.Edge;
         else
            Neighbor1 := No_Edge;
         end if;

         if Tmp.Next /= null then
            Neighbor2 := Tmp.Next.Edge;
         else
            Neighbor2 := No_Edge;
         end if;
      end Insert;

      -----------
      -- Slope --
      -----------

      function Slope (S : Edge_Index) return Float is
      begin
         if X2 (S) /= X1 (S) then
            return Float (Y2 (S) - Y1 (S)) / Float (X2 (S) - X1 (S));
         else
            return Float'Last;
         end if;
      end Slope;

      ----------------
      -- Y_At_Point --
      ----------------

      function Y_At_Point (S : Edge_Index; X : Integer) return Integer is
      begin
         if X2 (S) /= X1 (S) then
            return Y1 (S) + (Y2 (S) - Y1 (S)) * (X - X1 (S))
              / (X2 (S) - X1 (S));
         else
            return Y1 (S);
         end if;
      end Y_At_Point;

      ---------------
      -- Is_Source --
      ---------------

      function Is_Source  (S : Edge_Index; X : Integer) return Boolean  is
      begin
         if X1 (S) < X2 (S) then
            return X = X1 (S);
         else
            return X = X2 (S);
         end if;
      end Is_Source;

      ----------
      -- Sign --
      ----------

      function Sign (X : Float) return Integer is
      begin
         if X < 0.0 then
            return -1;
         elsif X > 0.0 then
            return 1;
         else
            return 0;
         end if;
      end Sign;

      ------------------
      -- Intersection --
      ------------------

      procedure Intersection
        (S1, S2    : Edge_Index;
         Inter     : out Event_Point;
         At_Origin : out Boolean)
      is
         Slope1 : constant Float :=
           Float (Y2 (S1) - Y1 (S1)) / Float (X2 (S1) - X1 (S1));
         Slope2 : constant Float :=
           Float (Y2 (S2) - Y1 (S2)) / Float (X2 (S2) - X1 (S2));
         Yabs, Yabs2 : Float;
         Cx, Cy : Float;

      begin
         if (X1 (S1) = X1 (S2) and then Y1 (S1) = Y1 (S2))
           or else (X1 (1) = X2 (S2) and then Y1 (S1) = Y2 (S2))
         then
            At_Origin := True;
            Inter := (Intersection_Point, X1 (S1), Y1 (S1), S1, S2);
            return;
         end if;

         if (X2 (S1) = X1 (S2) and then Y2 (S1) = Y1 (S2))
           or else (X2 (S1) = X2 (S2) and then Y2 (S1) = Y2 (S2))
         then
            At_Origin := True;
            Inter := (Intersection_Point, X2 (S1), Y2 (S1), S1, S2);
            return;
         end if;

         At_Origin := False;

         if Slope1 = Slope2 then
            Inter := Null_Point;
            return;
         end if;

         Yabs  := Float (Y1 (S1)) - Slope1 * Float (X1 (S1));
         Yabs2 := Float (Y1 (S2)) - Slope2 * Float (X1 (S2));

         if X1 (S1) = X2 (S1) then
            Cx := Float (X1 (S1));
         elsif X1 (S2) = X2 (S2) then
            Cx := Float (X1 (S2));
         else
            Cx := (Yabs2 - Yabs) / (Slope1 - Slope2);
         end if;

         --  Test whether Cx lies in the X range of both segments
         if (Float (X1 (S1)) < Cx and then Float (X2 (S1)) < Cx)
           or else (Float (X1 (S1)) > Cx and then Float (X2 (S1)) > Cx)
           or else (Float (X1 (S2)) < Cx and then Float (X2 (S2)) < Cx)
           or else (Float (X1 (S2)) > Cx and then Float (X2 (S2)) > Cx)
         then
            Inter := Null_Point;
            return;
         end if;

         if X1 (S1) = X2 (S1) then
            Cy := Slope2 * Cx + Yabs2;
         else
            Cy := Slope1 * Cx + Yabs;
         end if;

         --  Test vertical ranges
         if X1 (S1) = X2 (S2)
           and then ((Float (Y1 (S1)) < Cy and then Float (Y2 (S1)) < Cy)
                  or else (Float (Y1 (S1)) > Cy and then Float (Y2 (S1)) > Cy))
         then
            Inter := Null_Point;
            return;
         end if;

         if X1 (S2) = X2 (S2)
           and then
           ((Float (Y1 (S2)) < Cy and then Float (Y2 (S2)) < Cy)
            or else (Float (Y1 (S2)) > Cy and then Float (Y2 (S2)) > Cy))
         then
            Inter := Null_Point;
            return;
         end if;

         Inter := (Intersection_Point, Integer (Cx), Integer (Cy), S1, S2);
      end Intersection;

      -------------------
      -- Segment_Below --
      -------------------

      function Segment_Below
        (S1, S2 : Edge_Index; X : Integer) return Boolean
      is
         Y3 : constant Integer := Y_At_Point (S1, X);
         Y4 : constant Integer := Y_At_Point (S2, X);
      begin
         return Y3 < Y4
           or else
           (Y3 = Y4
            and then (Is_Source (S1, X) or else Is_Source (S2, X))
            and then Slope (S1) < Slope (S2));
      end Segment_Below;

      ------------------
      -- Point_Before --
      ------------------

      function Point_Before (P1, P2 : Event_Point) return Boolean is
      begin
         if P1.X < P2.X then
            return True;
         end if;

         if P1.X > P2.X then
            return False;
         end if;

         if P1.Y < P2.Y then
            return True;
         end if;

         if P1.Y > P2.Y then
            return False;
         end if;

         if P1.Typ = Source_Vertex and then P2.Typ = Target_Vertex then
            return True;
         end if;

         if P2.Typ = Source_Vertex and then P1.Typ = Target_Vertex then
            return False;
         end if;

         if P2.Typ = Source_Vertex and then P1.Typ = Intersection_Point then
            return True;
         end if;

         if P1.Typ = Intersection_Point and then P2.Typ = Target_Vertex then
            return True;

         elsif P2.Typ = Intersection_Point and then P1.Typ = Target_Vertex then
            return False;

         elsif P1.Typ = Source_Vertex
           and then P2.Typ = Source_Vertex
         then
            if Slope (P1.Edge1) > Slope (P2.Edge1) then
               return True;
            end if;

         elsif P1.Typ = Target_Vertex
           and then P2.Typ = Target_Vertex
         then
            if Slope (P1.Edge1) > Slope (P2.Edge1) then
               return True;
            end if;
         end if;

         return False;
      end Point_Before;

      Count                : Natural := 0;
      Queue                : Event_Queue;
      Line                 : Line_Status;
      E1, E2               : Event_Point;
      Neighbor1, Neighbor2 : Edge_Index;
      At_Origin            : Boolean;

   begin
      --  The arrays must have the same ranges
      pragma Assert
        (X1'Length = Y1'Length
         and then X2'Length = Y2'Length
         and then X1'Length = X2'Length
         and then X1'First = Y1'First
         and then X2'First = Y2'First
         and then X1'First = X2'First);

      --  Initial the event_point queue will all the ends of the edges

      for J in X1'Range loop
         E1 := (Source_Vertex, X1 (J), Y1 (J), J, No_Edge);
         E2 := (Target_Vertex, X2 (J), Y2 (J), J, No_Edge);

         if Point_Before (E2, E1) then
            E1.Typ := Target_Vertex;
            E2.Typ := Source_Vertex;
         end if;

         Add (Queue, E1);
         Add (Queue, E2);
      end loop;

      --  While there are some event points to be processed...

      while Queue.Head /= null loop

         E1 := Minimum (Queue);
         exit when E1 = Null_Point;

         case E1.Typ is
            when Source_Vertex =>
               Insert (Line, Queue, E1.Edge1, E1.X, Neighbor1, Neighbor2);

               if Neighbor1 /= No_Edge then
                  --  ??? Should test colinearity
                  Intersection (Neighbor1, E1.Edge1, E2, At_Origin);
                  if E2 /= Null_Point and then Point_Before (E1, E2) then
                     if not At_Origin then
                        Count := Count + 1;
                     end if;
                     Add (Queue, E2);
                  end if;
               end if;

               if Neighbor2 /= No_Edge then
                  Intersection (Neighbor2, E1.Edge1, E2, At_Origin);
                  if E2 /= Null_Point and then Point_Before (E1, E2) then
                     if not At_Origin then
                        Count := Count + 1;
                     end if;
                     Add (Queue, E2);
                  end if;
               end if;

            when Target_Vertex =>
               Delete (Line, E1.Edge1, Neighbor1, Neighbor2);
               if Neighbor1 /= No_Edge
                 and then Neighbor2 /= No_Edge
               then
                  --  ??? Should test colinearity
                  Intersection (Neighbor1, Neighbor2, E2, At_Origin);
                  if E2 /= Null_Point and then Point_Before (E1, E2) then
                     if not At_Origin then
                        Count := Count + 1;
                     end if;
                     Add (Queue, E2);
                  end if;
               end if;

            when Intersection_Point =>
               Reverse_Segments
                 (Line, E1.Edge1, E1.Edge2, Neighbor1, Neighbor2);

               if Neighbor1 /= No_Edge then
                  Intersection (Neighbor1, E1.Edge1, E2, At_Origin);
                  if E2 /= Null_Point and then Point_Before (E1, E2) then
                     if not At_Origin then
                        Count := Count + 1;
                     end if;
                     Add (Queue, E2);
                  end if;
               end if;

               if Neighbor2 /= No_Edge then
                  Intersection (Neighbor2, E1.Edge2, E2, At_Origin);
                  if E2 /= Null_Point and then Point_Before (E1, E2) then
                     if not At_Origin then
                        Count := Count + 1;
                     end if;
                     Add (Queue, E2);
                  end if;
               end if;
         end case;

         Delete (Queue, E1);
      end loop;

      return Count;
   end Intersections_Count;

end Line_Sweep;

