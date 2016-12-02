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

with Ada.Calendar;                use Ada.Calendar;
with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;           use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Cairo;                       use Cairo;
with Default_Preferences;         use Default_Preferences;
with Gdk.Event;                   use Gdk.Event;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.RGBA;                    use Gdk.RGBA;
with Generic_Views;               use Generic_Views;
with Glib;                        use Glib;
with Glib.Convert;                use Glib.Convert;
with Glib.Main;                   use Glib.Main;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Glib_Values_Utils;           use Glib_Values_Utils;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNAT.Strings;                use GNAT.Strings;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Intl;                    use GPS.Intl;
with GPS.Search;                  use GPS.Search;
with Gtkada.MDI;                  use Gtkada.MDI;
with Gtkada.Multi_Paned;          use Gtkada.Multi_Paned;
with Gtkada.Style;                use Gtkada.Style;
with Gtkada.Tree_View;            use Gtkada.Tree_View;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Drawing_Area;            use Gtk.Drawing_Area;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Menu;                    use Gtk.Menu;
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

   Column_ID      : constant := 0;
   Column_Author  : constant := 1;
   Column_Date    : constant := 2;
   Column_Subject : constant := 3;
   subtype All_Columns is Gint range Column_ID .. Column_Subject;

   Inter_Row_Space  : constant Gint := 2;  --  hard-coded in gtk+
   Column_Width     : constant Gdouble := 10.0;
   Radius           : constant Gdouble := 3.0;
   Outside_Graph    : constant Gdouble := 10_000.0;

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

   Show_Author    : Boolean_Preference;
   Show_ID        : Boolean_Preference;
   Show_Date      : Boolean_Preference;

   subtype Graph_Column is Positive;
   subtype Line_Index is Positive;
   subtype Commit_ID is String;

   No_Graph_Column : constant Graph_Column := Graph_Column'Last;

   package Boolean_Vectors is new Ada.Containers.Vectors
     (Index_Type => Graph_Column, Element_Type => Boolean);

   package Commit_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Line_Index,   --  index into Tree.Lines
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type History_View_Config is record
      Initialized  : Boolean := False;
      Show_Author  : Boolean := True;
      Show_Id      : Boolean := True;
      Show_Date    : Boolean := True;
   end record;

   type History_Line is record
      ID, Author, Date, Subject : GNAT.Strings.String_Access;
      Parents                   : GNAT.Strings.String_List_Access;
      Names                     : GNAT.Strings.String_List_Access;

      Col : Graph_Column := No_Graph_Column;
      --  which column to draw in

      Circle_Center             : Gdouble;
      --  coordinate, depending on current scroll value

      Has_Parent_In_Same_Col    : Boolean;
   end record;
   package Line_Vectors is new Ada.Containers.Vectors
     (Line_Index, History_Line);
   --  Information for each commit line.
   --  This comes straight from the various VCS plugins, and are also used when
   --  filtering and laying out the graph.

   type History_Tree_Record is new Tree_View_Record with record
      User_Filter : GPS.Search.Search_Pattern_Access;
      Commits     : Commit_Maps.Map;
      Graph       : Gtk_Drawing_Area;

      Lines       : Line_Vectors.Vector;

      Max_Columns : Natural := 0;  --  Number of columns in the graph

      Max_Lines   : Line_Index := 2000;
      --  Maximum number of lines to display

      Col_ID      : Gtk_Tree_View_Column;
      Col_Author  : Gtk_Tree_View_Column;
      Col_Date    : Gtk_Tree_View_Column;

      Has_Show_Older : Boolean := False;
      --  Whether the "show older" button is visible
   end record;
   type History_Tree is access all History_Tree_Record'Class;

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
      Details     : Gtk_Text_View;
   end record;
   overriding procedure Refresh
     (Self : not null access History_View_Record);
   overriding procedure On_Preferenced_Changed
     (Self : not null access History_View_Record;
      Pref : Preference);
   overriding procedure Create_Menu
     (View    : not null access History_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure On_Create
     (Self    : not null access History_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class);

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

   type Detached_Model_Access is access Expansion.Detached_Model;

   type Layout_Step is (Step_Compute, Step_Insert);
   type Layout_Idle_Data is record
      Detached : Detached_Model_Access;
      Is_Free  : Boolean_Vectors.Vector;
      Current  : Natural;  --  in lines
      Step     : Layout_Step := Step_Compute;
   end record;
   type Layout_Idle_Data_Access is access all Layout_Idle_Data;

   type On_Line_Seen is new History_Visitor with record
      Kernel   : Kernel_Handle;

      Data     : Layout_Idle_Data_Access;
      --  The same data that will be used for layout once all lines have been
      --  retrieved.
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

   procedure Free (Self : in out Layout_Idle_Data_Access);
   function On_Layout_Idle (Data : Layout_Idle_Data_Access) return Boolean;
   package Layout_Sources is new Glib.Main.Generic_Sources
     (Layout_Idle_Data_Access);
   --  Compute the layout for the tree and graph, and insert lines

   type On_Active_VCS_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the active VCS changes

   function On_Button_Press
     (Self   : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   --  Called when the user selected a new line

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   procedure Reset_Layout (Self : not null access History_Tree_Record'Class);
   --  Reset layout information

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
         Y3, Y2   : Gdouble;
         Data, DP : History_Line;
         X, X2    : Gdouble;
         PLine    : Line_Index;
         Is_Free  : array (1 .. Tree.Max_Columns) of Boolean :=
           (others => True);

      begin
         for Line in 1 .. Line_Start - 1 loop
            Data := Tree.Lines (Line);
            Is_Free (Data.Col) := not Data.Has_Parent_In_Same_Col;
         end loop;

         Set_Line_Width (Cr, 2.0);
         for Line in Line_Start .. Line_End loop
            Data := Tree.Lines (Line);
            X    := Gdouble (Data.Col) * Column_Width;

            if not Is_Free (Data.Col) then
               Move_To (Cr, X, 0.0);
               Line_To (Cr, X, Data.Circle_Center - Radius);
               Is_Free (Data.Col) := True;  --  not needed anymore
            end if;

            Arc (Cr,
                 Xc     => X,
                 Yc     => Data.Circle_Center,
                 Radius => Radius,
                 Angle1 => 0.0,
                 Angle2 => 6.2831853072);
            Set_Color (Data.Col);
            Stroke (Cr);

            if Data.Parents /= null then
               --  Reverse to show straight lines on top of curves
               for P in reverse Data.Parents'Range loop
                  if Tree.Commits.Contains (Data.Parents (P).all) then
                     PLine := Tree.Commits (Data.Parents (P).all);
                     if PLine > Line_End then
                        DP := (Col           => Data.Col,
                               Circle_Center => Outside_Graph,
                               others        => <>);
                     else
                        DP := Tree.Lines (PLine);
                     end if;

                     Move_To (Cr, X, Data.Circle_Center + Radius);

                     if Data.Col = DP.Col then
                        Line_To (Cr, X, DP.Circle_Center - Radius);
                        Set_Color (Data.Col);
                     else
                        X2 := Gdouble (DP.Col) * Column_Width;
                        Y2 := Tree.Lines (Line + 1).Circle_Center - Radius;
                        Y3 := (Data.Circle_Center + Radius + Y2) / 2.0;
                        Curve_To (Cr, X, Y3,  X2, Y3,  X2, Y2);

                        if DP.Col < Data.Col then
                           Set_Color (Data.Col);
                           Stroke (Cr);
                           Move_To (Cr, X2, Y2);
                        end if;

                        Line_To (Cr, X2, DP.Circle_Center - Radius);
                        Set_Color (DP.Col);
                     end if;

                     Stroke (Cr);
                  end if;
               end loop;
            end if;
         end loop;

         --  Branches for which no commit is currently visible

         for Col in Is_Free'Range loop
            if not Is_Free (Col) then
               X2 := Gdouble (Col) * Column_Width;
               Move_To (Cr, X2, 0.0);
               Line_To (Cr, X2, Outside_Graph);
               Set_Color (Col);
               Stroke (Cr);
            end if;
         end loop;
      end Draw_Lines;

      Start, Finish : Gtk_Tree_Path;
      Success       : Boolean;
   begin
      Set_Source_Color (Cr, Browsers_Bg_Color.Get_Pref);
      Set_Operator (Cr, Cairo_Operator_Source);
      Paint (Cr);

      View.Tree.Get_Visible_Range (Start, Finish, Success);
      if Success then
         declare
            First_ID : constant String := Tree.Model.Get_String
              (Tree.Get_Store_Iter_For_Filter_Path (Start),
               Column_ID);
            Last_ID : constant String := Tree.Model.Get_String
              (Tree.Get_Store_Iter_For_Filter_Path (Finish),
               Column_ID);

            Base_Y, Base_X : Gint;
            Rect   : Gdk_Rectangle;
         begin
            Line_Start := Tree.Commits (First_ID);
            Line_End   :=
              (if Tree.Has_Show_Older and then Last_ID = ""
               then Tree.Max_Lines else Tree.Commits (Last_ID));

            Tree.Convert_Bin_Window_To_Widget_Coords
              (0, 0, Base_X, Base_Y);

            --  Compute the coordinate for all nodes in current view layout

            for Line in Line_Start .. Line_End loop
               Tree.Get_Cell_Area (Start, null, Rect);
               Tree.Lines (Line).Circle_Center := Gdouble
                 (Base_Y + Rect.Y + (Rect.Height + Inter_Row_Space) / 2);
               Next (Start);
            end loop;

            Path_Free (Start);
            Path_Free (Finish);

            Rectangle
              (Cr, 0.0, Gdouble (Base_Y), Outside_Graph, Outside_Graph);
            Clip (Cr);

            Draw_Lines;
         end;
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
      Id   : G_Source_Id with Unreferenced;
      Data : Layout_Idle_Data_Access;
      X, Y : Gint;
      Found : Boolean;
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
         if View.Tree.Model.Get_String
           (View.Tree.Get_Store_Iter_For_Filter_Path (Path),
            Column_ID) = ""
         then
            History_Tree (View.Tree).Max_Lines :=
              History_Tree (View.Tree).Max_Lines + 2000;

            Data := new Layout_Idle_Data;
            Data.Detached := new Expansion.Detached_Model'
              (Expansion.Detach_Model_From_View (View.Tree));
            View.Tree.Model.Clear;
            Data.Step := Step_Insert;
            Data.Current := 1;
            Id := Layout_Sources.Idle_Add
              (On_Layout_Idle'Access, Data, Notify => Free'Access);
         end if;

         Path_Free (Path);
      end if;

      return False;
   end On_Button_Press;

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
   end Create_Menu;

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
      T                   : History_Tree;
   begin
      Initialize_Vbox (Self, Homogeneous => False);
      Self.On_Destroy (On_Destroy'Access);

      T := new History_Tree_Record;

      Gtk_New (Paned);
      Paned.Set_Opaque_Resizing (True);
      Self.Pack_Start (Paned);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Paned.Add_Child (Box, Orientation => Orientation_Vertical);

      Gtk_New (T.Graph);
      T.Graph.Set_Size_Request (0, -1);   --  will grow when it has data
      Box.Pack_Start (T.Graph, Expand => False);
      T.Graph.On_Draw (On_Draw_Graph'Access, Self);

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

      Self.Tree := Tree_View (T);
      Initialize (Self.Tree,
                  (Column_ID      => GType_String,
                   Column_Author  => GType_String,
                   Column_Date    => GType_String,
                   Column_Subject => GType_String),
                  Filtered         => True,
                  Set_Visible_Func => True);
      Self.Tree.Set_Headers_Visible (True);
      Self.Tree.Set_Fixed_Height_Mode (True);
      Self.Tree.Set_Show_Expanders (False);
      Self.Tree.On_Button_Press_Event (On_Button_Press'Access, Self);
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

      Gtk_New (T.Col_ID);
      T.Col_ID.Set_Title ("ID");
      T.Col_ID.Set_Sizing (Tree_View_Column_Fixed);
      T.Col_ID.Set_Resizable (True);
      Dummy := Self.Tree.Append_Column (T.Col_ID);
      Gtk_New (Text);
      T.Col_ID.Pack_Start (Text, False);
      T.Col_ID.Add_Attribute (Text, "text", Column_ID);

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

      Gtk_New (Self.Details);
      Scrolled2.Add (Self.Details);

      Self.On_Preferenced_Changed (null);

      Refresh (Self);

      return Gtk_Widget (Self.Tree);
   end Initialize;

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
   end On_Create;

   ----------------------------
   -- On_Preferenced_Changed --
   ----------------------------

   overriding procedure On_Preferenced_Changed
     (Self : not null access History_View_Record;
      Pref : Preference)
   is
      T  : constant History_Tree := History_Tree (Self.Tree);
      Config : History_View_Config;
   begin
      Base_VCS_View_Record (Self.all).On_Preferenced_Changed (Pref);

      Config :=
        (Initialized  => True,
         Show_Id      => Show_ID.Get_Pref,
         Show_Author  => Show_Author.Get_Pref,
         Show_Date    => Show_Date.Get_Pref);
      if Config /= Self.Config then
         Self.Config := Config;

         T.Col_ID.Set_Visible (Config.Show_Id);
         T.Col_Author.Set_Visible (Config.Show_Author);
         T.Col_Date.Set_Visible (Config.Show_Date);

         Self.Tree.Set_Headers_Visible
           (Config.Show_Id or Config.Show_Author or Config.Show_Date);

         T.Graph.Queue_Draw;   --  refresh graph
      end if;
   end On_Preferenced_Changed;

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
      Parents : in out GNAT.Strings.String_List_Access;
      Names   : in out GNAT.Strings.String_List_Access)
   is
      Tree   : constant History_Tree := History_Tree (Self.Data.Detached.Tree);
   begin
      if Tree /= null then
         Tree.Lines.Append
           ((ID       => new String'(ID),
             Author   => new String'(Author),
             Date     => new String'(Date),
             Subject  => new String'(Subject),
             Parents  => Parents,
             Names    => Names,
             Has_Parent_In_Same_Col => False,
             Circle_Center          => 0.0,
             Col                    => No_Graph_Column));
         Tree.Commits.Include (ID, Line_Index (Tree.Lines.Last_Index));
         Parents := null;  --  adopted
         Names   := null;  --  adopted
      end if;
   end On_History_Line;

   ------------------
   -- Reset_Layout --
   ------------------

   procedure Reset_Layout (Self : not null access History_Tree_Record'Class) is
   begin
      for L of Self.Lines loop
         Free (L.ID);
         Free (L.Author);
         Free (L.Date);
         Free (L.Subject);
         Free (L.Parents);
         Free (L.Names);
      end loop;
      Self.Lines.Clear;

      Self.Commits.Clear;

      Self.Max_Columns := 0;
   end Reset_Layout;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class) is
      View : constant History_View := History_View (Self);
   begin
      Reset_Layout (History_Tree (View.Tree));
   end On_Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Layout_Idle_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Layout_Idle_Data, Layout_Idle_Data_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Expansion.Detached_Model, Detached_Model_Access);
      Tree   : constant History_Tree := History_Tree (Self.Detached.Tree);
   begin
      Trace (Me, "Free layout_idle_data");
      if Tree /= null then
         Unchecked_Free (Self.Detached);
         Unchecked_Free (Self);
      end if;
   end Free;

   --------------------
   -- On_Layout_Idle --
   --------------------

   function On_Layout_Idle (Data : Layout_Idle_Data_Access) return Boolean is
      Tree   : constant History_Tree := History_Tree (Data.Detached.Tree);
      Iter   : Gtk_Tree_Iter;
      V      : Glib.Values.GValue_Array (All_Columns);
      Tmp    : Unbounded_String;
      Is_First : Boolean;

      function Next_Empty_Col return Graph_Column;
      --  Compute the next empty column

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

      Start    : constant Time := Clock;
   begin
      --  If the tree was destroyed, nothing more to do
      if Tree = null then
         return False;
      end if;

      case Data.Step is
      when Step_Compute =>

         while Data.Current <= Tree.Lines.Last_Index loop
            declare
               Ref : constant Line_Vectors.Reference_Type :=
                 Tree.Lines.Reference (Data.Current);
            begin
               --  Compute the column. We might already know it the commit is
               --  the parent for one of the existing commits

               if Ref.Col = No_Graph_Column then
                  Ref.Col := Next_Empty_Col;
               end if;

               Data.Is_Free (Ref.Col) := True;
               Ref.Has_Parent_In_Same_Col := False;

               --  Reserve columns for the parent commit

               if Ref.Parents /= null then
                  Is_First := True;
                  for P of Ref.Parents.all loop
                     if Tree.Commits.Contains (P.all) then
                        declare
                           PRef : constant Line_Vectors.Reference_Type :=
                             Tree.Lines.Reference (Tree.Commits (P.all));
                        begin
                           if PRef.Col = No_Graph_Column then
                              --  If this is the first parent for which we
                              --  do not already know the column, reuse the
                              --  current column.
                              if Is_First then
                                 PRef.Col := Ref.Col;
                                 Is_First := False;
                                 Ref.Has_Parent_In_Same_Col := True;
                              else
                                 PRef.Col := Next_Empty_Col;
                              end if;

                              Data.Is_Free (PRef.Col) := False;
                           end if;
                        end;
                     end if;
                  end loop;
               end if;
            end;

            Data.Current := Data.Current + 1;

            if Clock - Start >= 0.3 then
               return True;  --  will try again later
            end if;
         end loop;

         Trace (Me, "done computing graph layout");
         Tree.Max_Columns := Natural (Data.Is_Free.Length);
         Data.Step := Step_Insert;
         Data.Current := Tree.Lines.First_Index;
         return True;  --  Will run again for the actual insert

      when Step_Insert =>
         while Data.Current <= Tree.Lines.Last_Index
           and then Data.Current <= Tree.Max_Lines
         loop
            declare
               Ref : constant Line_Vectors.Constant_Reference_Type :=
                 Tree.Lines.Constant_Reference (Data.Current);
            begin
               Init_Set_String (V (Column_ID),      Ref.ID.all);
               Init_Set_String (V (Column_Author),  Ref.Author.all);
               Init_Set_String (V (Column_Date),    Ref.Date.all);

               Tmp := Null_Unbounded_String;
               if Ref.Names /= null then
                  for N of Ref.Names.all loop
                     Append
                       (Tmp, "<span background='#ffd195'>"
                        & Escape_Text (Trim (N.all, Both)) & " </span>");
                  end loop;
               end if;
               Append (Tmp, Escape_Text (Ref.Subject.all));
               Init_Set_String (V (Column_Subject), To_String (Tmp));

               Tree.Model.Append (Iter, Parent => Null_Iter);
               Set_All_And_Clear (Tree.Model, Iter, V);

               Data.Current := Data.Current + 1;

               if Clock - Start >= 0.3 then
                  return True;  --  will try again later
               end if;
            end;
         end loop;

         Tree.Has_Show_Older := Data.Current < Tree.Lines.Last_Index;
         if Tree.Has_Show_Older then
            Init_Set_String (V (Column_ID), "");
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
         Tree.Graph.Set_Size_Request
           (Gint (1 + Tree.Max_Columns) * Gint (Column_Width), -1);
         Tree.Graph.Queue_Draw;
         return False;  --  All done
      end case;
   end On_Layout_Idle;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out On_Line_Seen) is
      V  : constant History_View := History_Views.Retrieve_View (Self.Kernel);
      Id : G_Source_Id with Unreferenced;
   begin
      if V /= null then
         Trace (Me, "Finished fetching whole log");

         Self.Data.Current  := History_Tree (V.Tree).Lines.First_Index;
         Self.Data.Step     := Step_Compute;

         V.Tree.Model.Clear;

         Id := Layout_Sources.Idle_Add
           (On_Layout_Idle'Access, Self.Data, Notify => Free'Access);
         --  Do not free Self.Data, adopted by the background layout
      else
         Free (Self.Data);
      end if;

      History_Visitor (Self).Free;  --  inherited
   end Free;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh (Self : not null access History_View_Record) is
      VCS        : constant VCS_Engine_Access := Active_VCS (Self.Kernel);
      Seen       : access On_Line_Seen;
   begin
      if VCS /= null then
         Reset_Layout (History_Tree (Self.Tree));

         Seen := new On_Line_Seen;
         Seen.Kernel := Self.Kernel;
         Seen.Data   := new Layout_Idle_Data;
         Seen.Data.Detached := new Expansion.Detached_Model'
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
   end Register_Module;

end VCS2.History;
