------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;           use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Cairo;                       use Cairo;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.RGBA;                    use Gdk.RGBA;
with Generic_Views;               use Generic_Views;
with Glib;                        use Glib;
with Glib.Convert;                use Glib.Convert;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNAT.Strings;                use GNAT.Strings;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Search;                  use GPS.Search;
with Gtkada.MDI;                  use Gtkada.MDI;
with Gtkada.Multi_Paned;          use Gtkada.Multi_Paned;
with Gtkada.Style;                use Gtkada.Style;
with Gtkada.Tree_View;            use Gtkada.Tree_View;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Drawing_Area;            use Gtk.Drawing_Area;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Toolbar;                 use Gtk.Toolbar;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.Widget;                  use Gtk.Widget;
with VCS2.Engines;                use VCS2.Engines;
with VCS2.Views;                  use VCS2.Views;

package body VCS2.History is
   Me : constant Trace_Handle := Create ("HISTORY");
--   pragma Unreferenced (Me);

   Column_ID      : constant := 0;
   Column_Author  : constant := 1;
   Column_Date    : constant := 2;
   Column_Subject : constant := 3;
   subtype All_Columns is Gint range Column_ID .. Column_Subject;

   Graph_Width      : constant Gint := 60;
   Inter_Row_Space  : constant Gint := 2;  --  hard-coded in gtk+

   Color_Palettes : constant array (0 .. 9) of Gdk_RGBA :=
     (0   => (0.09, 0.46, 0.72, 1.0),
      1   => (1.00, 0.50, 0.00, 1.0),
      2   => (0.14, 0.63, 0.13, 1.0),
      3   => (0.85, 0.14, 0.12, 1.0),
      4   => (0.58, 0.39, 0.75, 1.0),
      5   => (0.55, 0.34, 0.29, 1.0),
      6   => (0.90, 0.45, 0.77, 1.0),
      7   => (0.50, 0.50, 0.50, 1.0),
      8   => (0.74, 0.75, 0.00, 1.0),
      9   => (0.00, 0.75, 0.82, 1.0));
   --  Color palette from d3js.org

   subtype Graph_Column is Positive;

   package Gdouble_Vectors is new Ada.Containers.Vectors
     (Index_Type => Graph_Column, Element_Type => Gdouble);

   package Boolean_Vectors is new Ada.Containers.Vectors
     (Index_Type => Graph_Column, Element_Type => Boolean);

   type Commit_Data is record
      Col     : Graph_Column;        --  which column to draw in
      Parents : String_List_Access;  --  parent commits
      Child   : GNAT.Strings.String_Access;       --  any of the child commits
   end record;
   procedure Free (Self : in out Commit_Data);

   package Commit_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Commit_Data,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   type Commit_Map is new Commit_Maps.Map with null record;
   overriding procedure Clear (Self : in out Commit_Map);
   overriding procedure Include
     (Self : in out Commit_Map; ID : String; Data : Commit_Data);

   type Coordinate is record
      X, Circle_Top, Circle_Center, Circle_Bottom, H : Gdouble;
   end record;
   --  Coordinates for a node in the graph

   package Coordinate_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Coordinate,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type History_View_Config is record
      null;
   end record;

   type History_Tree_Record is new Tree_View_Record with record
      User_Filter : GPS.Search.Search_Pattern_Access;
      Commits     : Commit_Map;
   end record;
   type History_Tree is access all History_Tree_Record'Class;

   subtype Commit_ID is String;

   function Get_ID_From_Node
     (Self       : not null access Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter) return Commit_ID
     is (Commit_ID (String'(Self.Model.Get_String (Store_Iter, Column_ID))));

   package Expansion is new Expansion_Support
     (Tree_Record        => Tree_View_Record,
      Id                 => Commit_ID,
      Get_Id             => Get_ID_From_Node,
      Hash               => Ada.Strings.Hash);

   type History_View_Record is new Base_VCS_View_Record with record
      Config      : History_View_Config;
      Graph       : Gtk_Drawing_Area;
      Details     : Gtk_Text_View;
   end record;
   overriding procedure Refresh
     (Self : not null access History_View_Record);

   function Initialize
     (Self : access History_View_Record'Class) return Gtk_Widget;
   --  Create a new view

   type History_Child_Record is new GPS_MDI_Child_Record with null record;

   package History_Views is new Generic_Views.Simple_Views
     (Module_Name        => "VCS_History",
      View_Name          => "History",
      Formal_View_Record => History_View_Record,
      Formal_MDI_Child   => History_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Both,
      Position           => Position_Right,
      Initialize         => Initialize);
   use History_Views;
   subtype History_View is History_Views.View_Access;

   function On_Draw_Graph
     (Self : access GObject_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean;
   --  Draws the graph on the side of the tree

   procedure On_Scrolled (Self : access GObject_Record'Class);
   --  Called when the tree is scrolled

   type Detached_Model_Access is access all Expansion.Detached_Model;

   type On_Line_Seen is new History_Visitor with record
      Kernel   : Kernel_Handle;
      Detached : Detached_Model_Access;
      Is_Free  : Boolean_Vectors.Vector;
      --  During layout, computed to know which columns are in use
   end record;
   overriding procedure Free (Self : in out On_Line_Seen);
   overriding procedure On_History_Line
     (Self    : not null access On_Line_Seen;
      ID      : String;
      Author  : String;
      Date    : String;
      Subject : String;
      Parents : in out GNAT.Strings.String_List_Access;
      Names   : in out GNAT.Strings.String_List_Access);
   --  Add a new log entry to the view
   --  Names are freed automatically by this procedure when needed.
   --  Parents is adopted by this procedure and must not be freed by the caller

   -------------------
   -- On_Draw_Graph --
   -------------------

   function On_Draw_Graph
     (Self : access GObject_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean
   is
      View   : constant History_View := History_View (Self);
      Radius : constant Gdouble := 3.0;

      Start, Finish : Gtk_Tree_Path;
      Success       : Boolean;
      Base_Y        : Gint;
      Coordinates   : Coordinate_Maps.Map;

      procedure For_Path (ID : String);
      --  Draw circle for the commit and links to its parents.

      procedure Set_Color (Col : Graph_Column);
      --  Set the color for the corresponding column

      ---------------
      -- Set_Color --
      ---------------

      procedure Set_Color (Col : Graph_Column) is
      begin
         Set_Source_Color
           (Cr, Color_Palettes ((Col - 1) mod Color_Palettes'Length));
      end Set_Color;

      --------------
      -- For_Path --
      --------------

      procedure For_Path (ID : String) is
         Curs    : Coordinate_Maps.Cursor;
         C1, C2  : Coordinate;
         Y3, Y2  : Gdouble;
         Tree    : constant History_Tree := History_Tree (View.Tree);
         Data    : constant Commit_Data := Tree.Commits (ID);
         DP      : Commit_Data;
      begin
         C1 := Coordinates (ID);

         --  If there is at least one invisible child, draw a straight line
         --  leading up, out of the canvas.

         if Data.Child /= null
           and then not Coordinates.Contains (Data.Child.all)
         then
            Move_To (Cr, C1.X, Gdouble (Base_Y));
            Line_To (Cr, C1.X, C1.Circle_Top);
            Stroke (Cr);
         end if;

         Arc (Cr,
              Xc     => C1.X,
              Yc     => C1.Circle_Center,
              Radius => Radius,
              Angle1 => 0.0,
              Angle2 => 6.2831853072);
         Set_Color (Data.Col);
         Stroke (Cr);

         if Data.Parents /= null then
            --  Reverse so that straight lines are displayed on top of curves
            for P in reverse Data.Parents'Range loop
               if Tree.Commits.Contains (Data.Parents (P).all) then
                  DP := Tree.Commits (Data.Parents (P).all);

                  --  Parent might not be visible
                  Curs := Coordinates.Find (Data.Parents (P).all);
                  if Coordinate_Maps.Has_Element (Curs) then
                     C2 := Coordinate_Maps.Element (Curs);
                  else
                     C2 := (X             => Gdouble (DP.Col) * 10.0,
                            H             => 6.0,
                            Circle_Top    => 5000.0,   --  out of visible area
                            Circle_Center => 5000.0,
                            Circle_Bottom => 5000.0);
                  end if;

                  Move_To (Cr, C1.X, C1.Circle_Bottom);

                  if C1.X = C2.X then
                     Line_To (Cr, C2.X, C2.Circle_Top);
                     Set_Color (Data.Col);
                  else
                     --  Move from C1 circle down one full space between two
                     --  nodes
                     Y2 := C1.Circle_Bottom
                       + C1.H / 2.0
                       + C2.H / 2.0
                       - 2.0 * Radius;
                     Y3 := (C1.Circle_Bottom + Y2) / 2.0;
                     Curve_To (Cr, C1.X, Y3,  C2.X, Y3,  C2.X, Y2);

                     if P = Data.Parents'First then
                        Set_Color (Data.Col);
                        Stroke (Cr);
                        Move_To (Cr, C2.X, Y2);
                     end if;

                     Line_To (Cr, C2.X, C2.Circle_Top);
                     Set_Color (DP.Col);
                  end if;

                  Stroke (Cr);
               end if;
            end loop;
         end if;
      end For_Path;

   begin
      Set_Source_Color (Cr, Browsers_Bg_Color.Get_Pref);
      Set_Operator (Cr, Cairo_Operator_Source);
      Paint (Cr);

      Set_Line_Width (Cr, 2.0);

      View.Tree.Get_Visible_Range (Start, Finish, Success);
      if Success then

         --  Compute the coordinate for all nodes in current view layout

         declare
            Base_X : Gint;
            S      : Gtk_Tree_Path;
            Rect   : Gdk_Rectangle;
            Y, H   : Gdouble;
         begin
            View.Tree.Convert_Bin_Window_To_Widget_Coords
              (0, 0, Base_X, Base_Y);

            S := Copy (Start);
            loop
               declare
                  ID : constant String :=
                    View.Tree.Model.Get_String
                      (View.Tree.Get_Store_Iter_For_Filter_Path (S),
                       Column_ID);
                  Data : constant Commit_Data :=
                    History_Tree (View.Tree).Commits (ID);
               begin
                  View.Tree.Get_Cell_Area (S, null, Rect);
                  Y := Gdouble (Base_Y + Rect.Y);
                  H := Gdouble ((Rect.Height + Inter_Row_Space));

                  Coordinates.Include
                    (ID,
                     Coordinate'
                       (X             => Gdouble (Data.Col) * 10.0,
                        H             => H,
                        Circle_Top    => Y + H / 2.0 - Radius,
                        Circle_Center => Y + H / 2.0,
                        Circle_Bottom => Y + H / 2.0 + Radius));

                  exit when Compare (S, Finish) = 0;
                  Next (S);
               end;
            end loop;

            Path_Free (S);
         end;

         --  Setup a clip mask

         Rectangle (Cr, 0.0, Gdouble (Base_Y), 10_000.0, 10_000.0);
         Clip (Cr);

         --  Now draw all commits
         --  Draw from bottom to top, so that straight lines are displayed on
         --  top of curved lines).

         loop
            For_Path
              (View.Tree.Model.Get_String
                 (View.Tree.Get_Store_Iter_For_Filter_Path (Finish),
                  Column_ID));
            exit when Compare (Start, Finish) = 0;
            exit when not Prev (Finish);
         end loop;

         Path_Free (Start);
         Path_Free (Finish);
      end if;

      return True;  --  handled
   end On_Draw_Graph;

   -----------------
   -- On_Scrolled --
   -----------------

   procedure On_Scrolled (Self : access GObject_Record'Class) is
      View : constant History_View := History_View (Self);
   begin
      View.Graph.Queue_Draw;
   end On_Scrolled;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access History_View_Record'Class) return Gtk_Widget
   is
      Scrolled, Scrolled2 : Gtk_Scrolled_Window;
      Paned               : Gtkada_Multi_Paned;
      Text                : Gtk_Cell_Renderer_Text;
      Col                 : Gtk_Tree_View_Column;
      Dummy               : Gint;
      Box                 : Gtk_Box;
   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Paned);
      Paned.Set_Opaque_Resizing (True);
      Self.Pack_Start (Paned);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Paned.Add_Child (Box, Orientation => Orientation_Vertical);

      Gtk_New (Self.Graph);
      Self.Graph.Set_Size_Request (Graph_Width, -1);
      Box.Pack_Start (Self.Graph, Expand => False);
      Self.Graph.On_Draw (On_Draw_Graph'Access, Self);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Box.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Get_Vadjustment.On_Value_Changed (On_Scrolled'Access, Self);

      Gtk_New (Scrolled2);
      Scrolled2.Set_Policy (Policy_Automatic, Policy_Automatic);
      Paned.Split
        (Ref_Widget   => Box,
         New_Child    => Scrolled2,
         Orientation  => Orientation_Vertical,
         Height       => 15);

      Self.Tree := new History_Tree_Record;
      Initialize (Self.Tree,
                  (Column_ID      => GType_String,
                   Column_Author  => GType_String,
                   Column_Date    => GType_String,
                   Column_Subject => GType_String),
                  Filtered         => True,
                  Set_Visible_Func => True);
      Self.Tree.Set_Headers_Visible (True);
      Self.Tree.Set_Fixed_Height_Mode (True);
      Scrolled.Add (Self.Tree);

      Gtk_New (Col);
      Col.Set_Visible (False);
      Col.Set_Title ("ID");
      Col.Set_Sizing (Tree_View_Column_Fixed);
      Dummy := Self.Tree.Append_Column (Col);
      Gtk_New (Text);
      Col.Pack_Start (Text, False);
      Col.Add_Attribute (Text, "text", Column_ID);

      Gtk_New (Col);
      Col.Set_Title ("Author");
      Col.Set_Visible (False);
      Col.Set_Sizing (Tree_View_Column_Fixed);
      Dummy := Self.Tree.Append_Column (Col);
      Gtk_New (Text);
      Col.Pack_Start (Text, False);
      Col.Add_Attribute (Text, "text", Column_Author);

      Gtk_New (Col);
      Col.Set_Title ("Date");
      Col.Set_Visible (False);
      Col.Set_Sizing (Tree_View_Column_Fixed);
      Dummy := Self.Tree.Append_Column (Col);
      Gtk_New (Text);
      Col.Pack_Start (Text, False);
      Col.Add_Attribute (Text, "text", Column_Date);

      Gtk_New (Col);
      Col.Set_Expand (True);
      Col.Set_Title ("Subject");
      Col.Set_Sizing (Tree_View_Column_Fixed);
      Dummy := Self.Tree.Append_Column (Col);
      Gtk_New (Self.Text_Render);
      Col.Pack_Start (Self.Text_Render, True);
      Col.Add_Attribute (Self.Text_Render, "markup", Column_Subject);

      Gtk_New (Self.Details);
      Scrolled2.Add (Self.Details);

      Refresh (Self);

      return Gtk_Widget (Self.Tree);
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Commit_Data) is
   begin
      Free (Self.Parents);
      Free (Self.Child);
   end Free;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out Commit_Map) is
   begin
      for E of Self loop
         Free (E);
      end loop;
      Commit_Maps.Map (Self).Clear;   --  inherited
   end Clear;

   -------------
   -- Include --
   -------------

   overriding procedure Include
     (Self : in out Commit_Map; ID : String; Data : Commit_Data)
   is
      C : constant Commit_Maps.Cursor := Self.Find (ID);
      D : Commit_Data;
   begin
      if Commit_Maps.Has_Element (C) then
         D := Commit_Maps.Element (C);
         Free (D);
      end if;
      Commit_Maps.Map (Self).Include (ID, Data);  --  inherited
   end Include;

   ---------------------
   -- On_History_Line --
   ---------------------

   overriding procedure On_History_Line
     (Self    : not null access On_Line_Seen;
      ID      : String;
      Author  : String;
      Date    : String;
      Subject : String;
      Parents : in out GNAT.Strings.String_List_Access;
      Names   : in out GNAT.Strings.String_List_Access)
   is
      Tree   : constant History_Tree := History_Tree (Self.Detached.Tree);
      Iter   : Gtk_Tree_Iter;
      V      : Glib.Values.GValue_Array (All_Columns);
      Tmp    : Unbounded_String;
      Col, C : Graph_Column;
      Is_First : Boolean;

      function Next_Empty_Col return Graph_Column;
      --  Compute the next empty column

      function Next_Empty_Col return Graph_Column is
         C : Graph_Column;
      begin
         for J in 1 .. Self.Is_Free.Length loop
            if Self.Is_Free (Graph_Column (J)) then
               return Graph_Column (J);
            end if;
         end loop;

         C := Graph_Column (Self.Is_Free.Length + 1);
         Self.Is_Free.Set_Length (Ada.Containers.Count_Type (C));
         return C;
      end Next_Empty_Col;

      Child : GNAT.Strings.String_Access;
   begin
      if Tree = null then
         return;   --  view has been destroyed in between, nothing to do
      end if;

      --  Add in tree model, to preserve the order

      Tree.Model.Append (Iter, Parent => Null_Iter);

      Init_Set_String (V (Column_ID),      ID);
      Init_Set_String (V (Column_Author),  Author);
      Init_Set_String (V (Column_Date),    Date);

      if Names /= null then
         for N of Names.all loop
            Append
              (Tmp, "<span background='#ffd195'>"
               & Escape_Text (Trim (N.all, Both)) & " </span>");
         end loop;
      end if;

      Append (Tmp, Escape_Text (Subject));
      Init_Set_String (V (Column_Subject), To_String (Tmp));

      Tree.Model.Set (Iter, V);

      --  Compute the column. We might already know it the commit is the
      --  parent for one of the existing commits

      if Tree.Commits.Contains (ID) then
         Col := Tree.Commits (ID).Col;

         --  ??? Useless copy and map lookup

         Child := new String'(Tree.Commits (ID).Child.all);
      else
         Child := null;
         Col := Next_Empty_Col;
      end if;

      --  Store data

      Tree.Commits.Include
        (ID, Commit_Data'
           (Col     => Col,
            Child   => Child,
            Parents => Parents));  --  will be freed later
      Self.Is_Free (Col) := False;

      --  Reserve columns for the parent commit

      if Parents /= null then
         C := Col;
         Is_First := True;
         for P in Parents'Range loop
            --  If the parent already has a column do nothing
            if Tree.Commits.Contains (Parents (P).all) then
               null;

            else
               --  If this is the first parent for which we do not already
               --  know the column, reuse the current column.
               if Is_First then
                  C := Col;
               else
                  C := Next_Empty_Col;
               end if;

               Is_First := False;
               Tree.Commits.Include
                 (Parents (P).all,
                  Commit_Data'
                    (Col     => C,
                     Child   => new String'(ID),
                     Parents => null));
               Self.Is_Free (C) := False;
            end if;
         end loop;

         --  If we haven't reused the current column, that's because this
         --  was the start of the branch. We can free the column for an
         --  other branch
         if Is_First then
            Self.Is_Free (Col) := True;
         end if;
      end if;

      Parents := null;   --  adopted
   end On_History_Line;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out On_Line_Seen) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Expansion.Detached_Model, Detached_Model_Access);
      V : constant History_View := History_Views.Retrieve_View (Self.Kernel);
   begin
      Trace (Me, "Finished fetching whole log");
      Unchecked_Free (Self.Detached);

      if V /= null then
         --  Force redisplay of graph
         V.Queue_Draw;
      end if;
   end Free;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh (Self : not null access History_View_Record) is
      VCS        : constant VCS_Engine_Access := Active_VCS (Self.Kernel);
      Seen       : access On_Line_Seen;
   begin
      if VCS /= null then
         Self.Tree.Model.Clear;
         History_Tree (Self.Tree).Commits.Clear;

         Seen := new On_Line_Seen;
         Seen.Kernel := Self.Kernel;
         Seen.Detached := new Expansion.Detached_Model'
           (Expansion.Detach_Model_From_View (Self.Tree));

         VCS.Queue_Fetch_History (Visitor => Seen);
      end if;
   end Refresh;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      History_Views.Register_Module (Kernel);
   end Register_Module;

end VCS2.History;
