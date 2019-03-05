------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNAT.Strings;                use GNAT.Strings;

with Cairo;                       use Cairo;
with Glib;                        use Glib;
with Glib.Convert;                use Glib.Convert;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Glib_Values_Utils;           use Glib_Values_Utils;

with Gdk.Event;                   use Gdk.Event;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.RGBA;                    use Gdk.RGBA;

with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Drawing_Area;            use Gtk.Drawing_Area;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Spinner;                 use Gtk.Spinner;
with Gtk.Text_Tag;                use Gtk.Text_Tag;
with Gtk.Toolbar;                 use Gtk.Toolbar;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.Widget;                  use Gtk.Widget;

with Gtkada.MDI;                  use Gtkada.MDI;
with Gtkada.Style;                use Gtkada.Style;
with Gtkada.Tree_View;            use Gtkada.Tree_View;

with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;              use GNATCOLL.Utils;

with GPS.Kernel.Actions;          use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;       use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Intl;                    use GPS.Intl;
with GPS.Search;                  use GPS.Search;
with GPS_Unbounded_String_Vectors;

with Commands.Interactive;        use Commands, Commands.Interactive;
with Default_Preferences;         use Default_Preferences;
with Generic_Views;               use Generic_Views;
with VCS2.Diff;
with VCS2.Engines;                use VCS2.Engines;
with VCS2.Views;                  use VCS2.Views;
with Filter_Panels;               use Filter_Panels;

package body VCS2.History is
   pragma Warnings (Off);

   Me : constant Trace_Handle := Create ("GPS.VCS.HISTORY");

   Column_Line    : constant := 0;
   Column_Author  : constant := 1;
   Column_Date    : constant := 2;
   Column_Subject : constant := 3;
   subtype All_Columns is Gint range Column_Line .. Column_Subject;

   Inter_Row_Space : constant Gint := 2;  --  hard-coded in gtk+
   Column_Width    : constant Gdouble := 10.0;
   Radius          : constant Gdouble := 3.0;
   Outside_Graph   : constant Gdouble := 10_000.0;
   Number_Commits  : constant Integer := 500;

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

   Show_Author             : Boolean_Preference;
   Show_ID                 : Boolean_Preference;
   Show_Date               : Boolean_Preference;
   Show_All_Branches       : Boolean_Preference;
   Collapse_Simple_Commits : Boolean_Preference;

   subtype Graph_Column is Positive;
   subtype Line_Index is Positive;
   subtype Commit_ID is String;

   No_Graph_Column : constant Graph_Column := Graph_Column'Last;

   package Boolean_Vectors is new Ada.Containers.Vectors
     (Index_Type => Graph_Column, Element_Type => Boolean);

   type History_View_Config is record
      Initialized  : Boolean := False;
      Collapse     : Boolean := False;
      All_Branches : Boolean := False;
      Show_Author  : Boolean := False;
      Show_Date    : Boolean := False;
      Show_Id      : Boolean := False;
   end record;

   type Visibility is new Natural;
   Always_Visible : constant Visibility := 2;
   --  See History_Line.Visible

   type Parent is record
      ID            : Ada.Strings.Unbounded.Unbounded_String;
      Has_Invisible : Boolean := False;
   end record;
   type Parent_Array is array (Natural range <>) of Parent;
   type Parent_Array_Access is access all Parent_Array;

   type Node_Data is record
      ID            : Ada.Strings.Unbounded.Unbounded_String;
      Author        : Ada.Strings.Unbounded.Unbounded_String;
      Date          : Ada.Strings.Unbounded.Unbounded_String;
      Subject       : Ada.Strings.Unbounded.Unbounded_String;
      Parents       : Parent_Array_Access;
      Names         : Commit_Names_Access;

      Col           : Graph_Column := No_Graph_Column;
      --  which column to draw in

      Circle_Center : Gdouble;
      --  coordinate, depending on current scroll value

      Num_Children  : Natural := 0;
      --  Number of children commits

      Line          : Integer := -1;
      --  Line number within the tree model

      Visible       : Visibility;
      --  A node is visible when this field is Always_Visible or more.

      Flags         : Commit_Flags := 0;
   end record;
   type Node_Data_Access is access all Node_Data;

   package Commit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Node_Data_Access,    --  owned
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   package Line_Vectors is new Ada.Containers.Vectors
     (Line_Index, Node_Data_Access);
   --  Information for each commit line.
   --  This comes straight from the various VCS plugins, and are also used when
   --  filtering and laying out the graph.
   --  Elements refer to entries in the commit_maps, and are not owned by the
   --  vector.

   type History_Tree_Record is new Tree_View_Record with record
      Config      : History_View_Config;
      Commits     : Commit_Maps.Map;
      Graph       : Gtk_Drawing_Area;

      Show_Graph  : Boolean := True;
      --  Whether to show the graph, depending on preferences and filtering

      Lines       : Line_Vectors.Vector;
      --  The visible commits, in the order they were returned by the VCS.
      --  Invisible commits are not added

      Max_Columns : Natural := 0;  --  Number of columns in the graph

      User_Filter : History_Filter :=
        (Up_To_Lines   => Number_Commits,
         others        => <>);
      --  Current filter

      Col_Author  : Gtk_Tree_View_Column;
      Col_Date    : Gtk_Tree_View_Column;

      Has_Show_Older : Boolean := False;
      --  Whether the "show older" button is visible
   end record;
   type History_Tree is access all History_Tree_Record'Class;

   type Layout_Idle_Data is record
      Is_Free   : Boolean_Vectors.Vector;
      Current   : Natural;  --  in lines
      Inserted  : Natural;  --  number of items inserted in tree
      To_Select : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Kernel    : Kernel_Handle;
   end record;
   type Layout_Idle_Data_Access is access all Layout_Idle_Data;

   function Get_Parent_Node
     (Tree   : not null access History_Tree_Record'Class;
      N      : Node_Data_Access;
      Parent : Natural) return Node_Data_Access;
   --  Look at the Parent-th parent node of N, and go up the chain until we
   --  find one visible parent (so it could be the Parent-th parent itself, or
   --  its own parent).
   --  Returns null if no such parent is found.

   procedure Add_Commit_Node
     (Tree : not null access History_Tree_Record'Class;
      N    : Node_Data_Access;
      Data : Layout_Idle_Data_Access);
   --  Add N in Tree

   procedure Finish_Filling_Tree
     (Tree : not null access History_Tree_Record'Class;
      Data : Layout_Idle_Data_Access);
   --  Called when the maximum capacity of the tree is hit: will recompute
   --  the graph and add an action

   function Get_ID_From_Node
     (Self       : not null access Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter) return Commit_ID;

   package Expansion is new Expansion_Support
     (Tree_Record        => Tree_View_Record,
      Id                 => Commit_ID,
      Get_Id             => Get_ID_From_Node,
      Hash               => Ada.Strings.Hash);

   type History_Child_Record is new GPS_MDI_Child_Record with null record;

   overriding function Build_Context
     (Self  : not null access History_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   type History_View_Record is new Base_VCS_View_Record with record
      Spinner                 : Gtk_Spinner;
      Box                     : Gtk_Box;
      Refresh_On_Pref_Changed : Boolean := True;
   end record;
   overriding procedure Refresh
     (Self : not null access History_View_Record);
   overriding procedure On_Preferences_Changed
     (Self : not null access History_View_Record;
      Pref : Preference);
   overriding procedure Create_Menu
     (View    : not null access History_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure On_Create
     (Self    : not null access History_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access History_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access);

   function Initialize
     (Self : access History_View_Record'Class) return Gtk_Widget;
   --  Create a new view

   procedure Hide_Spinner (Self : access History_View_Record'Class);
   --  Hide the Spinner and show the tree view after finding a commit

   procedure Show_Spinner (Self : access History_View_Record'Class);
   --  Hide the view and show a spinner before the first commit is found

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

   procedure On_Selection_Changed (View : access GObject_Record'Class);
   --  Called when one or more files are selected

   type On_Line_Seen is new Task_Visitor with record
      Kernel   : Kernel_Handle;

      Data     : Layout_Idle_Data_Access;
      --  The same data that will be used for layout once all lines have been
      --  retrieved.
   end record;
   overriding procedure On_Start (Self : not null access On_Line_Seen);
   overriding procedure Free (Self : in out On_Line_Seen);
   overriding procedure On_History_Line
     (Self    : not null access On_Line_Seen;
      ID      : String;
      Author  : String;
      Date    : String;
      Subject : String;
      Parents : in out GPS_Unbounded_String_Vectors.Vector;
      Names   : in out Commit_Names_Access;
      Flags   : Commit_Flags);
   --  Add a new log entry to the view
   --  Names are freed automatically by this procedure when needed.
   --  Parents is adopted by this procedure and must not be freed by the caller

   type On_Details is new Task_Visitor with record
      Kernel   : Kernel_Handle;
      Multiple : Boolean;
   end record;
   overriding procedure On_Commit_Details
     (Self    : not null access On_Details;
      ID      : String;
      Header  : String;
      Message : String);
   --  Visitor when getting the details for a set of commits

   type On_Active_VCS_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the active VCS changes

   type On_VCS_Refresh is new Vcs_Refresh_Hooks_Function with null record;
   overriding procedure Execute
     (Self          : On_VCS_Refresh;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Is_File_Saved : Boolean);

   type History_For_File is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access History_For_File;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Refresh_History is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Refresh_History;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_History_Command is new Root_Command with record
      Kernel    : Kernel_Handle;
      File      : Virtual_File;
      Commit_Id : Unbounded_String;
   end record;
   overriding function Execute
     (Self    : access Show_History_Command) return Command_Return_Type;

   type On_Checkout is new Task_Visitor with null record;
   overriding procedure On_Success
     (Self   : not null access On_Checkout;
      Kernel : not null access Kernel_Handle_Record'Class);

   type Checkout is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Checkout;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Checkout_File is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Checkout_File;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Is_Commit_Id_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Self    : access Is_Commit_Id_Filter;
      Context : Selection_Context) return Boolean;
   --  Whether the context has commit id information.

   type Is_File_Commit_Id_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Self    : access Is_File_Commit_Id_Filter;
      Context : Selection_Context) return Boolean;
   --  Whether the context has commit id information and file is set.

   function On_Button_Press
     (Self   : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   --  Called when the user selected a new line

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   procedure Reset_Lines (Self : not null access History_Tree_Record'Class);
   --  Reset all lines information

   procedure Clear_View (Self : not null access History_Tree_Record'Class);
   ---  Clear all views

   function Label_For_Checkout_File
     (Context : Selection_Context) return String;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access History_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Context   : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      V         : constant History_View :=
        History_View (GPS_MDI_Child (Self).Get_Actual_Widget);
      Tree      : constant History_Tree :=
        (if V /= null then History_Tree (V.Tree) else null);

      X, Y      : Gdouble;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Buffer_X, Buffer_Y  : Gint;
      Row_Found : Boolean;
      Iter      : Gtk_Tree_Iter := Null_Iter;

      procedure On_Selected
        (M : Gtk_Tree_Model;
         P : Gtk_Tree_Path;
         I : Gtk_Tree_Iter);
      --  Called for each selected row

      -----------------
      -- On_Selected --
      -----------------

      procedure On_Selected
        (M : Gtk_Tree_Model;
         P : Gtk_Tree_Path;
         I : Gtk_Tree_Iter)
      is
         pragma Unreferenced (P);
         N : constant Node_Data_Access :=
           Tree.Lines (Integer (Get_Int (M, I, Column_Line)));
      begin
         Set_Commit_Id_Information (Context, To_String (N.ID));
      end On_Selected;

   begin
      if Tree = null then
         return Context;
      end if;

      if Event /= null then
         Get_Coords (Event, X, Y);
         V.Tree.Get_Path_At_Pos
           (Gint (X), Gint (Y), Path, Column,
            Buffer_X, Buffer_Y, Row_Found);

         if Path /= Null_Gtk_Tree_Path then
            Iter := V.Tree.Model.Get_Iter (Path);
            declare
               N : constant Node_Data_Access := Tree.Lines
                 (Integer (V.Tree.Model.Get_Int (Iter, Column_Line)));
            begin
               Set_Commit_Id_Information (Context, To_String (N.ID));
            end;
         end if;
         Path_Free (Path);

      elsif Tree.Get_Selection.Count_Selected_Rows = 1 then
         Tree.Get_Selection.Selected_Foreach
           (On_Selected'Unrestricted_Access);
      else
         return Context;
      end if;

      return Context;
   end Build_Context;

   ----------------------
   -- Get_ID_From_Node --
   ----------------------

   function Get_ID_From_Node
     (Self       : not null access Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter) return Commit_ID
   is
      Line : constant Integer :=
        Integer (Self.Model.Get_Int (Store_Iter, Column_Line));
   begin
      if Line = -1 then
         return "";
      else
         return Commit_ID (To_String (History_Tree (Self).Lines (Line).ID));
      end if;
   end Get_ID_From_Node;

   ----------------
   -- Clear_View --
   ----------------

   procedure Clear_View (Self : not null access History_Tree_Record'Class) is
   begin
      Self.Model.Clear;
      Self.Graph.Queue_Draw;
   end Clear_View;

   -------------------
   -- On_Draw_Graph --
   -------------------

   function On_Draw_Graph
     (Self : access GObject_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean
   is
      View   : constant History_View := History_View (Self);
      Tree   : constant History_Tree := History_Tree (View.Tree);
      Line_Start, Line_End : Line_Index;

      procedure Draw_Lines;
      --  Draw circle for the commit and links to its parents.

      procedure Set_Color (Col : Graph_Column) with Inline;
      --  Set the color for the corresponding column

      ---------------
      -- Set_Color --
      ---------------

      procedure Set_Color (Col : Graph_Column) is
      begin
         Set_Source_Color
           (Cr, Color_Palettes ((Col - 1) mod Color_Palettes'Length));
      end Set_Color;

      ----------------
      -- Draw_Lines --
      ----------------

      procedure Draw_Lines is
         type Parent_Link_Type is (No_Link, Visible_Parent, Invisible_Parent);
         --  How to display a link to the parent node. This is used when a node
         --  is offscreen.

         Y3, Y2   : Gdouble;
         Data, DP : Node_Data_Access;
         X, X2    : Gdouble;
         Is_Occupied  : array (1 .. Tree.Max_Columns) of Parent_Link_Type :=
           (others => No_Link);
         Is_Merge : Boolean;

      begin
         for Line in 1 .. Line_Start - 1 loop
            Data := Tree.Lines (Line);
            Is_Occupied (Data.Col) := No_Link;

            if Data.Parents /= null then
               for P in Data.Parents'Range loop
                  DP := Get_Parent_Node (Tree, Data, P);
                  if DP /= null then
                     Is_Occupied (DP.Col) :=
                       (if Data.Parents (P).Has_Invisible
                        then Invisible_Parent
                        else Visible_Parent);
                  end if;
               end loop;
            end if;
         end loop;

         Set_Line_Width (Cr, 2.0);
         for Line in Line_Start .. Line_End loop
            Data := Tree.Lines (Line);
            X    := Gdouble (Data.Col) * Column_Width;

            Arc (Cr,
                 Xc     => X,
                 Yc     => Data.Circle_Center,
                 Radius => Radius,
                 Angle1 => 0.0,
                 Angle2 => 6.2831853072);

            if (Data.Flags and Commit_Uncommitted) /= 0 then
               Set_Source_Color (Cr, (0.5, 0.5, 0.5, 1.0));
            else
               Set_Color (Data.Col);
            end if;

            if (Data.Flags and Commit_Unpushed) /= 0 then
               Fill_Preserve (Cr);
            end if;

            Stroke (Cr);

            case Is_Occupied (Data.Col) is
               when No_Link =>
                  null;

               when Visible_Parent =>
                  Move_To (Cr, X, 0.0);
                  Line_To (Cr, X, Data.Circle_Center - Radius);
                  Stroke (Cr);

               when Invisible_Parent =>
                  Save (Cr);
                  Set_Dash (Cr, (3.0, 3.0), 0.0);
                  Move_To (Cr, X, 0.0);
                  Line_To (Cr, X, Data.Circle_Center - Radius);
                  Stroke (Cr);
                  Restore (Cr);
            end case;
            Is_Occupied (Data.Col) := No_Link;

            if Data.Parents /= null then
               --  Reverse to show straight lines on top of curves
               for P in reverse Data.Parents'Range loop
                  DP := Get_Parent_Node (Tree, Data, P);
                  if DP /= null then
                     if DP.Line > Line_End then
                        DP.Circle_Center := Outside_Graph;
                     end if;

                     Save (Cr);

                     if Data.Parents (P).Has_Invisible then
                        Set_Dash (Cr, (3.0, 3.0), 0.0);
                     end if;

                     Move_To (Cr, X, Data.Circle_Center + Radius);

                     if Data.Col = DP.Col then
                        Line_To (Cr, X, DP.Circle_Center - Radius);
                        Set_Color (Data.Col);
                     else
                        X2 := Gdouble (DP.Col) * Column_Width;
                        Y2 := Data.Circle_Center + Radius + 16.0;
                        Y3 := (Data.Circle_Center + Radius + Y2) / 2.0;
                        Curve_To (Cr, X, Y3,  X2, Y3,  X2, Y2);

                        --  Parent has a single child, but current has
                        --  multiple parents: this is a merge
                        Is_Merge := DP.Num_Children = 1
                          and then Data.Parents'Length > 1;

                        if DP.Line = Data.Line + 1 and then not Is_Merge then
                           Set_Color (Data.Col);
                        else
                           Set_Color (DP.Col);
                           Move_To (Cr, X2, Y2);
                           Line_To (Cr, X2, DP.Circle_Center - Radius);
                        end if;
                     end if;

                     Stroke (Cr);
                     Restore (Cr);

                  else
                     --  parent not found in memory for now
                     --  Will draw straight line.
                     Is_Occupied (Data.Col) := Invisible_Parent;
                  end if;
               end loop;
            end if;
         end loop;

         --  Branches for which no commit is currently visible

         for Col in Is_Occupied'Range loop
            if Is_Occupied (Col) /= No_Link then
               Save (Cr);

               if Is_Occupied (Col) = Invisible_Parent then
                  Set_Dash (Cr, (3.0, 3.0), 0.0);
               end if;

               X2 := Gdouble (Col) * Column_Width;
               Move_To (Cr, X2, 0.0);
               Line_To (Cr, X2, Outside_Graph);
               Set_Color (Col);
               Stroke (Cr);

               Restore (Cr);
            end if;
         end loop;
      end Draw_Lines;

      Start, Finish  : Gtk_Tree_Path;
      Success        : Boolean;
      Base_Y, Base_X : Gint;
      Rect           : Gdk_Rectangle;
   begin
      if not Tree.Show_Graph then
         return True;  --  handled
      end if;

      Set_Source_Color (Cr, Browsers_Bg_Color.Get_Pref);
      Set_Operator (Cr, Cairo_Operator_Source);
      Paint (Cr);

      Tree.Get_Visible_Range (Start, Finish, Success);
      if Success then
         Line_Start := Integer
           (Tree.Model.Get_Int
              (Tree.Get_Store_Iter_For_Filter_Path (Start),
               Column_Line));

         declare
            Tmp : Integer;
         begin
            Tmp := Integer
              (Tree.Model.Get_Int
                 (Tree.Get_Store_Iter_For_Filter_Path (Finish),
                  Column_Line));
            if Tree.Has_Show_Older and then Tmp = -1 then
               Line_End := Tree.User_Filter.Up_To_Lines;
            else
               Line_End := Tmp;
            end if;
         end;

         Tree.Convert_Bin_Window_To_Widget_Coords
           (0, 0, Base_X, Base_Y);

         --  Compute the coordinate for all nodes in current view layout

         for Line in Line_Start .. Line_End loop
            if Tree.Lines (Line).Visible >= Always_Visible then
               Tree.Get_Cell_Area (Start, null, Rect);
               Tree.Lines (Line).Circle_Center := Gdouble
                 (Base_Y + Rect.Y + (Rect.Height + Inter_Row_Space) / 2);
               Next (Start);
            end if;
         end loop;

         Path_Free (Start);
         Path_Free (Finish);

         Rectangle
           (Cr, 0.0, Gdouble (Base_Y), Outside_Graph, Outside_Graph);
         Clip (Cr);

         Draw_Lines;
      end if;

      return True;  --  handled
   end On_Draw_Graph;

   -----------------
   -- On_Scrolled --
   -----------------

   procedure On_Scrolled (Self : access GObject_Record'Class) is
      View : constant History_View := History_View (Self);
   begin
      History_Tree (View.Tree).Graph.Queue_Draw;
   end On_Scrolled;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Self   : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean
   is
      View : constant History_View := History_View (Self);
      X, Y           : Gint;
      Found          : Boolean;
      Cell_X, Cell_Y : Gint;
      Column         : Gtk_Tree_View_Column;
      Path           : Gtk_Tree_Path;
   begin
      --  Compute which line the user clicked on
      X := Gint (Event.X);
      Y := Gint (Event.Y);
      Get_Path_At_Pos (View.Tree, X, Y, Path, Column, Cell_X, Cell_Y, Found);

      if Found then
         --  If the user selected the "show older" entry
         if View.Tree.Model.Get_Int
           (View.Tree.Get_Store_Iter_For_Filter_Path (Path),
            Column_Line) = -1
         then
            History_Tree (View.Tree).User_Filter.Up_To_Lines :=
              History_Tree (View.Tree).User_Filter.Up_To_Lines
              + Number_Commits;
            View.Refresh;
         end if;

         Path_Free (Path);
      end if;

      return False;
   end On_Button_Press;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access History_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access)
   is
      Tree : constant History_Tree := History_Tree (Self.Tree);
      Pos  : Natural;
   begin
      if Pattern = null then
         Tree.User_Filter.Filter := Null_Unbounded_String;
         Tree.User_Filter.For_File := No_File;
         Self.Refresh;
         return;
      end if;

      declare
         Text : constant String := Get_Text (Pattern);
      begin
         if Starts_With (Text, "file:") then
            Tree.User_Filter.Filter := Null_Unbounded_String;
            Tree.User_Filter.Select_Id := Null_Unbounded_String;

            Pos := Text'Last;
            while Pos >= Text'First + 5 loop
               if Text (Pos) = '@' then
                  Tree.User_Filter.Select_Id := To_Unbounded_String
                    (Text (Pos + 1 .. Text'Last));

                  --  Will be selected after we have fetched the new contents
                  --  but we do not want to preserve the current selection
                  Tree.Get_Selection.Unselect_All;

                  Pos := Pos - 1;
                  exit;
               end if;
               Pos := Pos - 1;
            end loop;

            if Pos < Text'First + 5 then
               Pos := Text'Last;
            end if;

            Tree.User_Filter.For_File :=
              Self.Kernel.Get_Project_Tree.Create
                (+Text (Text'First + 5 .. Pos));

            if Tree.User_Filter.For_File = No_File then
               --  Create a dummy file
               Tree.User_Filter.For_File :=
                 Create (+Text (Text'First + 5 .. Pos));
            end if;

         else
            Tree.User_Filter.For_File := No_File;

            case Get_Kind (Pattern) is
            when Full_Text | Fuzzy | Approximate =>
               Tree.User_Filter.Filter := To_Unbounded_String
                 (GNAT.Regpat.Quote (Text));
            when Regexp =>
               Tree.User_Filter.Filter := To_Unbounded_String (Text);
            end case;
         end if;

         Self.Refresh;
      end;
   end Filter_Changed;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Self    : access Is_Commit_Id_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Has_Commit_Id_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Self    : access Is_File_Commit_Id_Filter;
      Context : Selection_Context) return Boolean is
   begin
      if not Has_Commit_Id_Information (Context) then
         return False;
      end if;

      declare
         View : constant History_View := History_Views.Retrieve_View
           (Get_Kernel (Context));
         Tree : constant History_Tree :=
           (if View /= null then History_Tree (View.Tree) else null);
      begin
         return Tree /= null
           and then Tree.User_Filter.For_File /= No_File;
      end;
   end Filter_Matches_Primitive;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access History_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, View.Kernel, Show_Ellipsis);
      Append_Menu (Menu, View.Kernel, Show_ID);
      Append_Menu (Menu, View.Kernel, Show_Author);
      Append_Menu (Menu, View.Kernel, Show_Date);
      Append_Menu (Menu, View.Kernel, Show_All_Branches);
      Append_Menu (Menu, View.Kernel, Collapse_Simple_Commits);
   end Create_Menu;

   -----------------------
   -- On_Commit_Details --
   -----------------------

   overriding procedure On_Commit_Details
     (Self    : not null access On_Details;
      ID      : String;
      Header  : String;
      Message : String)
   is
      function To_Title return String;
      --  Use to generate a useful title depending of the ID

      --------------
      -- To_Title --
      --------------

      function To_Title return String is
      begin
         if Self.Multiple then
            return "Overview";
         elsif ID'Length > 1 and then ID (ID'First) not in 'A' .. 'Z' then
            if ID'Length > 8 then
               return "Commit " & ID (ID'First .. (ID'First + 7));
            else
               return "Commit " & ID;
            end if;
         else
            return ID;
         end if;
      end To_Title;

      View  : constant History_View :=
        History_Views.Retrieve_View (Self.Kernel);
      Title : constant String := To_Title;
   begin
      VCS2.Diff.Create_Or_Reuse_Diff_Editor
        (Kernel => View.Kernel,
         Patch  => Message,
         Title  => Title,
         Header => Header);
   end On_Commit_Details;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (View : access GObject_Record'Class) is
      Self  : constant History_View := History_View (View);
      Tree  : constant History_Tree := History_Tree (Self.Tree);
      VCS   : constant VCS_Engine_Access := Active_VCS (Self.Kernel);
      Ids   : String_List_Access;
      Count : Natural := 0;

      procedure On_Selected
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter);
      --  Called for each selected row

      -----------------
      -- On_Selected --
      -----------------

      procedure On_Selected
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter)
      is
         pragma Unreferenced (Path);
         N : constant Node_Data_Access :=
           Tree.Lines (Integer (Get_Int (Model, Iter, Column_Line)));
      begin
         Ids (Count) := new String'(To_String (N.ID));
         Count := Count + 1;
      end On_Selected;

   begin
      if VCS /= null then
         Count := Natural (Tree.Get_Selection.Count_Selected_Rows);

         if Count /= 0 then
            VCS2.Diff.Clear_Diff_Editor (Self.Kernel);
            Ids := new GNAT.Strings.String_List (1 .. Count);
            Count := Ids'First;
            Tree.Get_Selection.Selected_Foreach
              (On_Selected'Unrestricted_Access);

            VCS.Queue_Fetch_Commit_Details
              (Ids     => Ids,
               Visitor =>
                  new On_Details'(Task_Visitor with
                    Kernel   => Self.Kernel,
                    Multiple => Count - Ids'First > 1));
         end if;
      end if;
   end On_Selection_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access History_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;
      Text     : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Dummy    : Gint;
      T        : History_Tree;
   begin
      Initialize_Vbox (Self, Homogeneous => False);
      Self.On_Destroy (On_Destroy'Access);

      Base_VCS_View_Record (Self.all).Filter_Options :=
        Has_Regexp or Debounce;
      Base_VCS_View_Record (Self.all).Filter_Hist_Prefix :=
        To_Unbounded_String ("history");

      T := new History_Tree_Record;
      Self.Tree := Tree_View (T);
      Initialize (Self.Tree,
                  (Column_Line    => GType_Int,
                   Column_Author  => GType_String,
                   Column_Date    => GType_String,
                   Column_Subject => GType_String),
                  Capability_Type  => Filtered,
                  Set_Visible_Func => True);
      Set_Name (Self.Tree, "History Tree");

      Gtk_New (Self.Spinner);
      Self.Pack_Start (Self.Spinner);

      Gtk_New_Hbox (Self.Box, Homogeneous => False);
      Self.Pack_Start (Self.Box);
      Set_No_Show_All (Self.Box, True);

      Gtk_New (T.Graph);
      T.Graph.Set_Size_Request (0, -1);   --  will grow when it has data
      Self.Box.Pack_Start (T.Graph, Expand => False);
      T.Graph.On_Draw (On_Draw_Graph'Access, Self);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Box.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Get_Vadjustment.On_Value_Changed (On_Scrolled'Access, Self);

      Self.Tree.Set_Headers_Visible (True);
      Self.Tree.Set_Fixed_Height_Mode (True);
      Self.Tree.Set_Search_Column (Column_Subject);
      Self.Tree.Set_Show_Expanders (False);
      Self.Tree.On_Button_Press_Event (On_Button_Press'Access, Self);
      Self.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Self.Tree.Get_Selection.On_Changed (On_Selection_Changed'Access, Self);
      Scrolled.Add (Self.Tree);

      Gtk_New (Col);
      Col.Set_Expand (True);
      Col.Set_Title ("Subject");
      Col.Set_Sizing (Tree_View_Column_Fixed);
      Col.Set_Min_Width (200);
      Col.Set_Resizable (True);
      Dummy := Self.Tree.Append_Column (Col);
      Gtk_New (Self.Text_Render);
      Col.Pack_Start (Self.Text_Render, True);
      Col.Add_Attribute (Self.Text_Render, "markup", Column_Subject);

      Gtk_New (T.Col_Author);
      T.Col_Author.Set_Title ("Author");
      T.Col_Author.Set_Sizing (Tree_View_Column_Fixed);
      T.Col_Author.Set_Resizable (True);
      Dummy := Self.Tree.Append_Column (T.Col_Author);
      Gtk_New (Text);
      T.Col_Author.Pack_Start (Text, False);
      T.Col_Author.Add_Attribute (Text, "text", Column_Author);

      Gtk_New (T.Col_Date);
      T.Col_Date.Set_Title ("Date");
      T.Col_Date.Set_Sizing (Tree_View_Column_Fixed);
      T.Col_Date.Set_Resizable (True);
      Dummy := Self.Tree.Append_Column (T.Col_Date);
      Gtk_New (Text);
      T.Col_Date.Pack_Start (Text, False);
      T.Col_Date.Add_Attribute (Text, "text", Column_Date);

      Setup_Contextual_Menu
        (Kernel          => Self.Kernel,
         Event_On_Widget => Self.Tree);

      return Gtk_Widget (Self.Tree);
   end Initialize;

   ------------------
   -- Hide_Spinner --
   ------------------

   procedure Hide_Spinner (Self : access History_View_Record'Class) is
   begin
      if not Self.Box.Is_Visible then
         Self.Spinner.Stop;
         Hide (Self.Spinner);
         Set_No_Show_All (Self.Box, False);
         Show_All (Self.Box);
         Set_No_Show_All (Self.Box, True);
      end if;
   end Hide_Spinner;

   ------------------
   -- Show_Spinner --
   ------------------

   procedure Show_Spinner (Self : access History_View_Record'Class) is
   begin
      Hide (Self.Box);
      Self.Spinner.Start;
      Self.Spinner.Set_No_Show_All (False);
      Show (Self.Spinner);
      Self.Spinner.Set_No_Show_All (True);
   end Show_Spinner;

   -----------------------------
   -- Label_For_Checkout_File --
   -----------------------------

   function Label_For_Checkout_File
     (Context : Selection_Context) return String
   is
      View : constant History_View :=
        History_Views.Get_Or_Create_View (Get_Kernel (Context));
      Tree : constant History_Tree := History_Tree (View.Tree);

   begin
      if Tree /= null
        and then Tree.User_Filter.For_File /= No_File
      then
         return +(Tree.User_Filter.For_File.Base_Name);

      else
         return "";
      end if;
   end Label_For_Checkout_File;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self    : not null access History_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class)
   is
   begin
      Base_VCS_View_Record (Self.all).On_Create (Child);  --  inherited
      Vcs_Active_Changed_Hook.Add (new On_Active_VCS_Changed, Watch => Self);
      Vcs_Refresh_Hook.Add (new On_VCS_Refresh, Watch => Self);
   end On_Create;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   overriding procedure On_Preferences_Changed
     (Self : not null access History_View_Record;
      Pref : Preference)
   is
      T  : constant History_Tree := History_Tree (Self.Tree);
      Config : History_View_Config;
   begin
      Base_VCS_View_Record (Self.all).On_Preferences_Changed (Pref);

      Config :=
        (Initialized  => True,
         Show_Author  => Show_Author.Get_Pref,
         Show_Date    => Show_Date.Get_Pref,
         Show_Id      => Show_ID.Get_Pref,
         All_Branches => Show_All_Branches.Get_Pref,
         Collapse     => Collapse_Simple_Commits.Get_Pref);
      if Config /= T.Config then
         T.Config := Config;
         T.User_Filter.Current_Branch_Only := not T.Config.All_Branches;

         T.Col_Author.Set_Visible (Config.Show_Author);
         T.Col_Date.Set_Visible (Config.Show_Date);
         Self.Tree.Set_Headers_Visible
           (Config.Show_Id or Config.Show_Author or Config.Show_Date);

         if Self.Refresh_On_Pref_Changed then
            Refresh (Self);
         end if;
      end if;
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      View : constant History_View := History_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Refresh (View);
      end if;
   end Execute;

   ---------------------
   -- On_History_Line --
   ---------------------

   overriding procedure On_History_Line
     (Self    : not null access On_Line_Seen;
      ID      : String;
      Author  : String;
      Date    : String;
      Subject : String;
      Parents : in out GPS_Unbounded_String_Vectors.Vector;
      Names   : in out Commit_Names_Access;
      Flags   : Commit_Flags)
   is
      View              : constant History_View :=
                            History_Views.Retrieve_View (Self.Kernel);
      Tree              : constant History_Tree :=
                            (if View /= null then
                               History_Tree (View.Tree) else null);
      C                 : Commit_Maps.Cursor;
      N, Parent_N       : Node_Data_Access;
      Is_Head_Of_Branch : Boolean;

   begin
      if Tree /= null then
         --  Create the new node for this commit (or reuse existing one if
         --  we have already created it.

         C := Tree.Commits.Find (To_Unbounded_String (ID));
         Is_Head_Of_Branch := not Commit_Maps.Has_Element (C);
         if Is_Head_Of_Branch then
            N := new Node_Data;
         else
            N := Commit_Maps.Element (C);
         end if;

         N.ID      := To_Unbounded_String (ID);
         N.Author  := To_Unbounded_String (Author);
         N.Date    := To_Unbounded_String (Date);
         N.Subject := To_Unbounded_String (Subject);
         N.Names   := Names;
         N.Flags   := Flags;

         if not Parents.Is_Empty then
            N.Parents := new Parent_Array (1 .. Natural (Parents.Length));

            for P in N.Parents'Range loop
               N.Parents (P).ID := Parents (P);
            end loop;
         end if;

         --  Compute visibility of the node

         if not Tree.Show_Graph
           or else not Tree.Config.Collapse   --  want to view all
           or else Names /= null       --  named commit
           or else Parents.Length > 1  --  first commit on a branch
                                       --  a branching commit
           or else Is_Head_Of_Branch   --  current head of the branch
         then
            N.Visible := Always_Visible;
         end if;

         for P of Parents loop
            C := Tree.Commits.Find (P);

            if Commit_Maps.Has_Element (C) then
               Parent_N := Commit_Maps.Element (C);
               --  visible if more than one child
               Parent_N.Visible := Parent_N.Visible + 1;
               Parent_N.Num_Children := Parent_N.Num_Children + 1;

               --  If parent is a branchpoint, always make the children
               --  visible (The first parent is not necessarily made
               --  visible, but since it will in general be in the same
               --  column that's OK).
               if Parent_N.Num_Children > 1 then
                  N.Visible := Always_Visible;
               end if;
            else
               --  Include parent in hash
               Parent_N := new Node_Data;
               Parent_N.Visible := 1;   --  one child
               Parent_N.Num_Children := 1;
               Parent_N.Col := No_Graph_Column;
               Tree.Commits.Include (P, Parent_N);
            end if;

            --  If current has multiple parents (a merge), we must
            --  show each of the parent nodes for proper display in
            --  the collapsed mode
            if not Tree.Config.Collapse or else Parents.Length > 1 then
               Parent_N.Visible := Always_Visible;
            end if;
         end loop;

         Tree.Commits.Include (To_Unbounded_String (ID), N);

         if N.Visible >= Always_Visible then
            Tree.Lines.Append (N);
            N.Line := Tree.Lines.Last_Index;
            View.Hide_Spinner;
            Tree.Add_Commit_Node (N, Self.Data);
         end if;

         Parents.Clear;  --  adopted
         Names := null;  --  adopted
      end if;
   end On_History_Line;

   -----------------
   -- Reset_Lines --
   -----------------

   procedure Reset_Lines (Self : not null access History_Tree_Record'Class) is

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Parent_Array, Parent_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Node_Data, Node_Data_Access);

   begin
      for L of Self.Commits loop
         L.Col     := No_Graph_Column;
         L.Visible := 0;
         Free (L.Names);

         if L.Parents /= null then
            Unchecked_Free (L.Parents);
         end if;

         Unchecked_Free (L);
      end loop;

      Self.Lines.Clear;
      Self.Commits.Clear;
      Self.Max_Columns := 0;
   end Reset_Lines;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class) is
      View : constant History_View := History_View (Self);
   begin
      Reset_Lines (History_Tree (View.Tree));
   end On_Destroy;

   ---------------------
   -- Get_Parent_Node --
   ---------------------

   function Get_Parent_Node
     (Tree   : not null access History_Tree_Record'Class;
      N      : Node_Data_Access;
      Parent : Natural) return Node_Data_Access
   is
      Node : Node_Data_Access;
      Curs : Commit_Maps.Cursor;
   begin
      Curs := Tree.Commits.Find (N.Parents (Parent).ID);

      if not Commit_Maps.Has_Element (Curs) then
         return null;
      end if;

      Node := Commit_Maps.Element (Curs);

      --  When we are collapsing, we go up the one-parent chain until we find
      --  the visible parent. All others are ignored (as soon as there are
      --  multiple parents, the node is visible anyway)

      while Node.Visible < Always_Visible loop
         N.Parents (Parent).Has_Invisible := True;

         if Node.Parents = null then
            return null;
         end if;

         Curs := Tree.Commits.Find (Node.Parents (Node.Parents'First).ID);

         if not Commit_Maps.Has_Element (Curs) then
            return null;
         end if;

         Node := Commit_Maps.Element (Curs);
      end loop;

      return Node;
   end Get_Parent_Node;

   ---------------------
   -- Add_Commit_Node --
   ---------------------

   procedure Add_Commit_Node
     (Tree : not null access History_Tree_Record'Class;
      N    : Node_Data_Access;
      Data : Layout_Idle_Data_Access)
   is
      V        : Glib.Values.GValue_Array (All_Columns);
      Iter     : Gtk_Tree_Iter;
      Tmp      : Unbounded_String;
      Is_First : Boolean;
      Parent_N : Node_Data_Access;

      function Next_Empty_Col return Graph_Column;
      --  Compute the next empty column

      --------------------
      -- Next_Empty_Col --
      --------------------

      function Next_Empty_Col return Graph_Column is
         C : Graph_Column;
      begin
         for J in 1 .. Data.Is_Free.Length loop
            if Data.Is_Free (Graph_Column (J)) then
               return Graph_Column (J);
            end if;
         end loop;

         C := Graph_Column (Data.Is_Free.Length + 1);
         Data.Is_Free.Set_Length (Ada.Containers.Count_Type (C));
         return C;
      end Next_Empty_Col;

   begin
      --  If the tree was destroyed, nothing more to do
      if Tree = null then
         return;
      end if;

      if Data.Inserted < Tree.User_Filter.Up_To_Lines
        and then Data.Current <= Tree.Lines.Last_Index
      then
         --  Compute the column. We might already know it the commit is
         --  the parent for one of the existing commits

         if N.Col = No_Graph_Column then
            N.Col := Next_Empty_Col;
         end if;

         Data.Is_Free (N.Col) := True;

         --  Reserve columns for the parent commit

         if N.Parents /= null then
            Is_First := True;
            for P in N.Parents'Range loop
               Parent_N := Get_Parent_Node (Tree, N, P);
               if Parent_N /= null then
                  if Parent_N.Col = No_Graph_Column then
                     --  If this is the first parent for which we do not
                     --  already know the column, reuse the current column.
                     if Is_First then
                        Parent_N.Col := N.Col;
                        Is_First := False;
                     else
                        Parent_N.Col := Next_Empty_Col;
                     end if;

                     Data.Is_Free (Parent_N.Col) := False;
                  end if;
               end if;
            end loop;
         end if;

         Init_Set_Int    (V (Column_Line),   Gint (Data.Current));
         Init_Set_String (V (Column_Author), To_String (N.Author));
         Init_Set_String (V (Column_Date),   To_String (N.Date));

         Tmp := Null_Unbounded_String;
         if N.Names /= null then
            for B in N.Names'Range loop
               case N.Names (B).Kind is
                  when Name_Head =>
                     Append (Tmp, "<span background='#ff6600'");
                  when Name_Local =>
                     Append (Tmp, "<span background='#fee391'");
                  when Name_Remote =>
                     Append (Tmp, "<span background='#a6bddb'");
                  when Name_Tag =>
                     Append (Tmp, "<span background='#a1d99b'");
               end case;

               Append (Tmp, " foreground='black'>");
               Append
                 (Tmp,
                  Escape_Text (To_String (Trim (N.Names (B).Name, Both))));

               if B = N.Names'Last then
                  Append (Tmp, "</span> ");
               else
                  Append (Tmp, " </span>");
               end if;
            end loop;
         end if;

         if (N.Flags and Commit_Uncommitted) /= 0 then
            Append (Tmp, "<span foreground='#555'>");
         end if;

         if Show_ID.Get_Pref then
            Append (Tmp, N.ID & " " & Escape_Text (To_String (N.Subject)));
         else
            Append (Tmp, Escape_Text (To_String (N.Subject)));
         end if;

         if (N.Flags and Commit_Uncommitted) /= 0 then
            Append (Tmp, "</span>");
         end if;

         Init_Set_String (V (Column_Subject), To_String (Tmp));

         Tree.Model.Append (Iter, Parent => Null_Iter);
         Set_All_And_Clear (Tree.Model, Iter, V);

         if N.ID = Tree.User_Filter.Select_Id then
            Data.To_Select := Tree.Get_Filter_Path_For_Store_Iter (Iter);
         end if;

         Data.Inserted := Data.Inserted + 1;
         Data.Current := Data.Current + 1;
         Tree.Max_Columns := Natural (Data.Is_Free.Length);
      end if;
   end Add_Commit_Node;

   -------------------------
   -- Finish_Filling_Tree --
   -------------------------

   procedure Finish_Filling_Tree
     (Tree : not null access History_Tree_Record'Class;
      Data : Layout_Idle_Data_Access)
   is
      V    : Glib.Values.GValue_Array (All_Columns);
      Iter : Gtk_Tree_Iter;
   begin
      Tree.Has_Show_Older := Data.Current <= Tree.Lines.Last_Index
        or else (Tree.Lines.Last_Index = Tree.User_Filter.Up_To_Lines);
      if Tree.Has_Show_Older then
         Init_Set_Int    (V (Column_Line), -1);
         Init_Set_String (V (Column_Author), "");
         Init_Set_String (V (Column_Date), "");
         Init_Set_String
           (V (Column_Subject), "<i>-- Show older commits --</i>");
         Tree.Model.Append (Iter, Parent => Null_Iter);
         Set_All_And_Clear (Tree.Model, Iter, V);
      end if;

      --  Force redisplay of graph
      Trace (Me, "done inserting nodes, max columns="
             & Tree.Max_Columns'Img);

      if Tree.Show_Graph then
         Tree.Graph.Set_Size_Request
           (Gint (1 + Tree.Max_Columns) * Gint (Column_Width), -1);
      else
         Tree.Graph.Set_Size_Request (0, -1);
      end if;
      Tree.Graph.Queue_Draw;

      Tree.Set_Search_Column (Column_Subject);
   end Finish_Filling_Tree;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out On_Line_Seen)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Layout_Idle_Data, Layout_Idle_Data_Access);

      View : constant History_View :=
        History_Views.Retrieve_View (Self.Kernel);

      Tree : constant History_Tree :=
               (if View /= null then History_Tree (View.Tree) else null);
   begin
      if Tree /= null then
         Finish_Filling_Tree (Tree, Self.Data);
         Trace (Me, "Finished fetching whole log");

         if Self.Data.To_Select /= Null_Gtk_Tree_Path then
            Tree.Get_Selection.Select_Path (Self.Data.To_Select);
            Tree.Scroll_To_Cell
              (Path      => Self.Data.To_Select,
               Column    => null,
               Use_Align => False,
               Row_Align => 0.0,
               Col_Align => 0.0);
            Path_Free (Self.Data.To_Select);
         end if;
         Unchecked_Free (Self.Data);
      end if;

      Task_Visitor (Self).Free;  --  inherited
      View.Hide_Spinner;
   end Free;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self          : On_VCS_Refresh;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Is_File_Saved : Boolean)
   is
      pragma Unreferenced (Self);
      View : constant History_View := History_Views.Retrieve_View (Kernel);
   begin
      if View /= null and then not Is_File_Saved then
         Refresh (View);
      end if;
   end Execute;

   --------------
   -- On_Start --
   --------------

   overriding procedure On_Start (Self : not null access On_Line_Seen) is
      View : constant History_View :=
         History_Views.Retrieve_View (Self.Kernel);
      Tree : History_Tree;
   begin
      if View /= null then
         Tree := History_Tree (View.Tree);

         --  Reset layout, and detach model from view.
         --  Do this before we reset the data, since detaching requires
         --  computing the ID of lines.

         declare
            Dummy : constant Expansion.Detached_Model :=
              Expansion.Detach_Model_From_View (Tree, Save_Expansion => False);
         begin
            Self.Data := new Layout_Idle_Data;
            Self.Data.Kernel := View.Kernel;
            Self.Data.Inserted := 0;
            Self.Data.Current := 1;
            Tree.Max_Columns := 0;

            --  If we have a filter, we can't show the graph, since we are
            --  missing too many commits.
            Tree.Show_Graph := Tree.User_Filter.Filter = ""
              and then Tree.User_Filter.For_File = No_File
              and then Tree.User_Filter.Select_Id = "";

            Tree.User_Filter.Branch_Commits_Only := Tree.Config.Collapse;
            Tree.User_Filter.Current_Branch_Only :=
              not Tree.Config.All_Branches;

            --  Remove all data from the tree model

            Reset_Lines (Tree);
            Clear_View (Tree);
         end;
      end if;
   end On_Start;

   ----------------
   -- On_Success --
   ----------------

   overriding procedure On_Success
     (Self   : not null access On_Checkout;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      View : constant History_View :=
        History_Views.Get_Or_Create_View (Kernel);
   begin
      Refresh (View);
   end On_Success;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh (Self : not null access History_View_Record) is
      VCS  : constant VCS_Engine_Access := Active_VCS (Self.Kernel);
      Tree : constant History_Tree := History_Tree (Self.Tree);

   begin
      if VCS /= null then
         Self.Show_Spinner;
         VCS.Queue_Fetch_History
           (Visitor =>
               new On_Line_Seen'
              (Task_Visitor with Kernel => Self.Kernel, others => <>),
            Filter  => Tree.User_Filter);
      end if;
   end Refresh;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Refresh_History;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant History_View  :=
                 History_Views.Get_Or_Create_View (Kernel);
   begin
      Refresh (View);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access History_For_File;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      File   : constant Virtual_File := File_Information (Context.Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : History_View;
      VCS    : VCS_Engine_Access;

      procedure Set_Initial_Filter
         (View : not null access History_View_Record'Class);
      procedure Set_Initial_Filter
         (View : not null access History_View_Record'Class) is
      begin
         View.Refresh_On_Pref_Changed := False;
      end Set_Initial_Filter;

   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         Set_Active_VCS (Kernel, VCS);

         View := History_Views.Get_Or_Create_View
            (Kernel, Focus => True, Init => Set_Initial_Filter'Access);

         --  The view will be refreshed automatically because of the
         --  call to Set_Filter
         View.Refresh_On_Pref_Changed := True;
         View.Set_Filter ("file:" & File.Display_Full_Name);
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Show_History_Command) return Command_Return_Type
   is
      View   : History_View;
      VCS    : VCS_Engine_Access;

      procedure Set_Initial_Filter
         (View : not null access History_View_Record'Class);
      procedure Set_Initial_Filter
         (View : not null access History_View_Record'Class) is
      begin
         View.Refresh_On_Pref_Changed := False;
      end Set_Initial_Filter;

   begin
      VCS := VCS_Engine_Access
        (Self.Kernel.VCS.Guess_VCS_For_Directory (Self.File.Dir));
      Set_Active_VCS (Self.Kernel, VCS);

      View := History_Views.Get_Or_Create_View
         (Self.Kernel, Focus => True, Init => Set_Initial_Filter'Access);
      View.Refresh_On_Pref_Changed := True;
      View.Set_Filter ("file:" & Self.File.Display_Full_Name
                       & "@" & To_String (Self.Commit_Id));
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Checkout;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Commit : constant String := Commit_Id_Information (Context.Context);
      VCS    : constant VCS_Engine_Access := Active_VCS (Kernel);

   begin
      VCS.Queue_Checkout (new On_Checkout, Commit);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Checkout_File;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Commit : constant String := Commit_Id_Information (Context.Context);
      View   : constant History_View := History_Views.Retrieve_View (Kernel);
      Tree   : constant History_Tree :=
        (if View /= null then
            History_Tree (View.Tree) else null);
      VCS    : constant VCS_Engine_Access := Active_VCS (Kernel);

   begin
      if Tree /= null then
         if Tree.User_Filter.For_File /= No_File then
            VCS.Queue_Checkout_File
              (new On_Checkout, Commit, Tree.User_Filter.For_File);
         end if;
      end if;
      return Success;
   end Execute;

   ---------------------------------
   -- Create_Show_History_Command --
   ---------------------------------

   function Create_Show_History_Command
     (Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File;
      Commit_ID : String) return Commands.Command_Access is
   begin
      return new Show_History_Command'
        (Root_Command with
           Kernel    => Kernel_Handle (Kernel),
           File      => File,
           Commit_Id => To_Unbounded_String (Commit_ID));
   end Create_Show_History_Command;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      History_Views.Register_Module (Kernel);

      Show_ID := Kernel.Get_Preferences.Create_Invisible_Pref
        ("vcs-history-show-id",
         Default => False,
         Label   => -"Show ID");

      Show_Author := Kernel.Get_Preferences.Create_Invisible_Pref
        ("vcs-history-show-author",
         Default => False,
         Label   => -"Show Author");

      Show_Date := Kernel.Get_Preferences.Create_Invisible_Pref
        ("vcs-history-show-date",
         Default => False,
         Label   => -"Show Date");

      Show_All_Branches := Kernel.Get_Preferences.Create_Invisible_Pref
        ("vcs-history-show-all-branches",
         Default => False,
         Label   => -"Show All Branches");

      Collapse_Simple_Commits := Kernel.Get_Preferences.Create_Invisible_Pref
        ("vcs-history-collapse",
         Default  => False,
         Label    => -"Hide non-branch related commits");

      Register_Action
        (Kernel, "open history for current file",
         Description =>
           -("Show the History view and display the history of changes for"
           & " the current file only."),
         Command     => new History_For_File,
         Filter      => Lookup_Filter (Kernel, "File"),
         Category    => "VCS2");

      Register_Action
        (Kernel, "history refresh",
         Description =>
           -("Refresh the history view"),
         Command     => new Refresh_History,
         Category    => "VCS2",
         Icon_Name   => "gps-refresh-symbolic");

      Register_Action
        (Kernel,
         "checkout to commit",
         Description => -("Checkout to the selected commit."),
         Command     => new Checkout,
         Filter      => new Is_Commit_Id_Filter,
         Category    => "VCS2");

      Register_Action
        (Kernel,
         "checkout file to commit",
         Description => -("Checkout current file to the selected commit."),
         Command     => new Checkout_File,
         Filter      => new Is_File_Commit_Id_Filter,
         Category    => "VCS2");

      Register_Contextual_Menu
        (Kernel => Kernel,
         Action => "open history for current file",
         Label  => "Version Control/Show history for file");

      Register_Contextual_Menu
        (Kernel => Kernel,
         Action => "checkout to commit",
         Label  => "Checkout to the revision");

      Register_Contextual_Menu
        (Kernel => Kernel,
         Action => "checkout file to commit",
         Label  => "Checkout %C to the revision",
         Custom => Label_For_Checkout_File'Access);

   end Register_Module;

end VCS2.History;
