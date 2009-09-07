-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Characters.Latin_1;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

with Glib.Object;
with Glib.Values;
with Gdk.Event;
with Gdk.Pixbuf;
with Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Object;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Toggle_Button;
with Gtk.Tree_Model.Utils;
with Gtk.Tree_Selection;
with Gtk.Tree_Sortable;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with GPS.Intl; use GPS.Intl;
with GPS.Kernel.Contexts;
with GPS.Kernel.Project;
with Code_Analysis_GUI;

package body Code_Peer.Summary_Reports is

   use type Glib.Signal_Name;
   use type Gtk.Tree_Model.Gtk_Tree_Path;

   --  use type Code_Analysis.File_Access;
   --  ??? Uncomment this line after I120-013 will be fixed
   use type Code_Analysis.Project_Access;
   use type Code_Analysis.Subprogram_Access;

   package Tree_View_Report_Return_Boolean_Callbacks is
     new Gtk.Handlers.User_Return_Callback
           (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Summary_Report);

   package Summary_Report_Callbacks is new Gtk.Handlers.Callback
     (Summary_Report_Record);

   package Check_Button_Report_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk.Check_Button.Gtk_Check_Button_Record, Summary_Report);

   package Message_Categories_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (Code_Peer.Categories_Criteria_Editors.
             Categories_Criteria_Editor_Record,
           Summary_Report);

   package Compare_Functions is
     new Gtk.Tree_Sortable.Compare_Funcs (Summary_Report);

   procedure On_Destroy (Self : access Summary_Report_Record'Class);

   procedure On_Show_All_Subprograms_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Added_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Unchanged_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Removed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Informational_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Low_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Medium_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_High_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Show_Suppressed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

   procedure On_Categories_Criteria_Changed
     (Object : access
        Code_Peer.Categories_Criteria_Editors.
          Categories_Criteria_Editor_Record'Class;
      Self   : Summary_Report);

   procedure Context_Func
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);

   function On_Analysis_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Summary_Report) return Boolean;

   function Compare
     (Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Summary_Report) return Glib.Gint;
   --  Compare two rows in the model.

   function Is_Messages_Category_Visible
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self  : Summary_Report) return Boolean;
   --  Returns True when specified item in the Entity's Messages Summary
   --  view must be visible. Model must have the same column's layout as
   --  Entity_Messages_Model has.

   package Summary_Report_Visible_Funcs is
     new Gtk.Tree_Model_Filter.Visible_Funcs (Summary_Report);

   package Message_Category_Conversions is
     new System.Address_To_Access_Conversions (Message_Category);

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Glib.Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Activated)),
      2 => Interfaces.C.Strings.New_String (String (Signal_Criteria_Changed)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None),
      2 => (1 => Glib.GType_None));

   -------------
   -- Compare --
   -------------

   function Compare
     (Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Summary_Report) return Glib.Gint
   is
      pragma Unreferenced (Self);

      use type Glib.Gint;
      use type Gtk.Tree_Model.Gtk_Tree_Iter;

      type Column_Sort_Order is
        (High_Added,   Medium_Added,   Low_Added,
         High_Current, Medium_Current, Low_Current);
      --  Literals in this type are in the comparison order. To change
      --  comparison order just reorder literals.

      type Counts is array (Column_Sort_Order) of Glib.Gint;

      A_Counts : Counts;
      B_Counts : Counts;

      function Get
        (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column : Glib.Gint) return Glib.Gint;
      --  Returns value at the specified row and column in the model.

      function Get (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Counts;
      --  Returns counts values for the specified row.

      ---------
      -- Get --
      ---------

      function Get
        (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column : Glib.Gint) return Glib.Gint
      is
         Aux   : Glib.Gint;
         Value : Glib.Values.GValue;

      begin
         Model.Get_Value (Iter, Column, Value);

         declare
            Image : constant String := Glib.Values.Get_String (Value);

         begin
            if Image'Length = 0 then
               Aux := 0;

            else
               Aux := Glib.Gint'Value (Image);
            end if;
         end;

         Glib.Values.Unset (Value);

         return Aux;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Counts is
      begin
         return
           (High_Added =>
              Get (Iter, Code_Peer.Summary_Models.High_Added_Count_Column),
            Medium_Added =>
              Get (Iter, Code_Peer.Summary_Models.Medium_Added_Count_Column),
            Low_Added =>
              Get (Iter, Code_Peer.Summary_Models.Low_Added_Count_Column),
            High_Current =>
              Get (Iter, Code_Peer.Summary_Models.High_Current_Count_Column),
            Medium_Current =>
              Get (Iter, Code_Peer.Summary_Models.Medium_Current_Count_Column),
            Low_Current =>
              Get (Iter, Code_Peer.Summary_Models.Low_Current_Count_Column));
      end Get;

   begin
      if Model.Parent (A) = Gtk.Tree_Model.Null_Iter then
         return 0;
      end if;

      A_Counts := Get (A);
      B_Counts := Get (B);

      for J in A_Counts'Range loop
         if A_Counts (J) < B_Counts (J) then
            return 1;

         elsif A_Counts (J) > B_Counts (J) then
            return -1;
         end if;
      end loop;

      return 0;
   end Compare;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Menu, Kernel, Event_Widget);

      Self       : constant Summary_Report := Summary_Report (Object);
      X          : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_X (Event));
      Y          : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_Y (Event));
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Model_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell_X     : Glib.Gint;
      Cell_Y     : Glib.Gint;
      Column     : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Found      : Boolean;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Project    : Code_Analysis.Project_Access;
      File       : Code_Analysis.File_Access;
      Subprogram : Code_Analysis.Subprogram_Access;

   begin
      Self.Analysis_View.Get_Path_At_Pos
        (X, Y, Path, Column, Cell_X, Cell_Y, Found);

      if Path /= null then
         Self.Analysis_View.Get_Selection.Select_Path (Path);
         Model_Path :=
           Self.Analysis_Sort_Model.Convert_Path_To_Child_Path (Path);
         Iter       := Self.Analysis_Model.Get_Iter (Model_Path);
         Project    := Self.Analysis_Model.Project_At (Iter);
         File       := Self.Analysis_Model.File_At (Iter);
         Subprogram := Self.Analysis_Model.Subprogram_At (Iter);

         if Subprogram /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.Name,
               Files   => (1 => File.Name));
            GPS.Kernel.Contexts.Set_Entity_Information
              (Context     => Context,
               Entity_Name => Subprogram.Name.all);

         elsif File /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.Name,
               Files   => (1 => File.Name));

         elsif Project /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context, Project => Project.Name);
         end if;

         Gtk.Tree_Model.Path_Free (Model_Path);
         Gtk.Tree_Model.Path_Free (Path);
      end if;
   end Context_Func;

   -----------------------
   -- Get_Selected_File --
   -----------------------

   function Get_Selected_File
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.File_Access
   is
      Model      : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Gtk.Tree_Selection.Get_Selected
        (Self.Analysis_View.Get_Selection, Model, Iter);
      Self.Analysis_Sort_Model.Convert_Iter_To_Child_Iter (Model_Iter, Iter);

      return Self.Analysis_Model.File_At (Model_Iter);
   end Get_Selected_File;

   --------------------------
   -- Get_Selected_Project --
   --------------------------

   function Get_Selected_Project
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.Project_Access
   is
      Model      : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Gtk.Tree_Selection.Get_Selected
        (Self.Analysis_View.Get_Selection, Model, Iter);
      Self.Analysis_Sort_Model.Convert_Iter_To_Child_Iter (Model_Iter, Iter);

      return Self.Analysis_Model.Project_At (Model_Iter);
   end Get_Selected_Project;

   -----------------------------
   -- Get_Selected_Subprogram --
   -----------------------------

   function Get_Selected_Subprogram
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.Subprogram_Access
   is
      Model      : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Gtk.Tree_Selection.Get_Selected
        (Self.Analysis_View.Get_Selection, Model, Iter);
      Self.Analysis_Sort_Model.Convert_Iter_To_Child_Iter (Model_Iter, Iter);

      return Self.Analysis_Model.Subprogram_At (Model_Iter);
   end Get_Selected_Subprogram;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Report : out Summary_Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree) is
   begin
      Report := new Summary_Report_Record;
      Initialize (Report, Kernel, Module, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : access Summary_Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree)
   is
      use Gtk.Tree_Model_Sort;

      Scrolled        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Report_Pane     : Gtk.Paned.Gtk_Vpaned;
      Filter_Box      : Gtk.Box.Gtk_Vbox;
      Check           : Gtk.Check_Button.Gtk_Check_Button;
      Label           : Gtk.Label.Gtk_Label;
      Separator       : Gtk.Separator.Gtk_Separator;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Gtk.Paned.Initialize_Hpaned (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "CodePeerSummaryReport",
         Signal_Parameters);
      Summary_Report_Callbacks.Connect
        (Self,
         Gtk.Object.Signal_Destroy,
         Summary_Report_Callbacks.To_Marshaller (On_Destroy'Access));

      Self.Kernel := Kernel;
      Self.Tree   := Tree;

      Project_Icon :=
        Gtk.Widget.Gtk_Widget
          (Kernel.Get_Main_Window).Render_Icon
          (Code_Analysis_GUI.Prj_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      File_Icon :=
        Gtk.Widget.Gtk_Widget
          (Kernel.Get_Main_Window).Render_Icon
          (Code_Analysis_GUI.File_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      Subprogram_Icon :=
        Gtk.Widget.Gtk_Widget
          (Kernel.Get_Main_Window).Render_Icon
          (Code_Analysis_GUI.Subp_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);

      Gtk.Paned.Gtk_New_Vpaned (Report_Pane);
      Self.Pack1 (Report_Pane, Resize => True);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Report_Pane.Pack1 (Scrolled, Resize => True);

      Code_Peer.Summary_Models.Gtk_New
        (Self.Analysis_Model,
         Tree,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).
                    Message_Categories,
         Project_Icon,
         File_Icon,
         Subprogram_Icon);
      Gtk.Tree_Model_Sort.Gtk_New_With_Model
        (Self.Analysis_Sort_Model, Self.Analysis_Model);
      Compare_Functions.Set_Default_Sort_Func
        (+Self.Analysis_Sort_Model, Compare'Access, Summary_Report (Self));
      Gtk.Tree_View.Gtk_New
        (Self.Analysis_View,
         Gtk.Tree_Model.Gtk_Tree_Model (Self.Analysis_Sort_Model));
      Scrolled.Add (Self.Analysis_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Entity");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf_Renderer);
      Column.Pack_Start (Pixbuf_Renderer, False);
      Column.Add_Attribute
        (Pixbuf_Renderer,
         "pixbuf",
         Code_Peer.Summary_Models.Entity_Icon_Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute
        (Text_Renderer, "text", Code_Peer.Summary_Models.Entity_Name_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("+/-");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Entity_Lifeage_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "base"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.High_Base_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("High" & Ada.Characters.Latin_1.LF & "deltas"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.High_Deltas_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "now"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.High_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background",
         Code_Peer.Summary_Models.High_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "base"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Medium_Base_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Medium" & Ada.Characters.Latin_1.LF & "deltas"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Medium_Deltas_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "now"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Medium_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background",
         Code_Peer.Summary_Models.Medium_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "base"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Low_Base_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Low" & Ada.Characters.Latin_1.LF & "deltas"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Low_Deltas_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "now"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Low_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background",
         Code_Peer.Summary_Models.Low_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      --  Message category view

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Size_Request (Height => 200);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Report_Pane.Pack2 (Scrolled);

      Code_Peer.Entity_Messages_Models.Gtk_New
        (Self.Messages_Model,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).
                    Message_Categories);
      Gtk.Tree_Model_Filter.Gtk_New
        (Self.Messages_Filter, Self.Messages_Model);
      Summary_Report_Visible_Funcs.Set_Visible_Func
        (Self.Messages_Filter,
         Is_Messages_Category_Visible'Access,
         Summary_Report (Self));
      Gtk.Tree_View.Gtk_New (Self.Messages_View, Self.Messages_Filter);
      Scrolled.Add (Self.Messages_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Message category");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Category_Name_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"High");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.High_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Medium");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Medium_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Low");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Low_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Tree_View_Report_Return_Boolean_Callbacks.Connect
        (Self.Analysis_View,
         Gtk.Widget.Signal_Button_Press_Event,
         Tree_View_Report_Return_Boolean_Callbacks.To_Marshaller
           (On_Analysis_Click'Access),
         Summary_Report (Self),
         False);

      --  Filter view

      Gtk.Box.Gtk_New_Vbox (Filter_Box);
      Filter_Box.Set_Size_Request (Width => 250);
      Self.Pack2 (Filter_Box);

      Gtk.Check_Button.Gtk_New (Check, -"Show all subprograms");
--      Filter_Box.Pack_Start (Check, False);
--  This check button is not displayed by default, see H519-028 discussion
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_All_Subprograms_Toggled'Access),
         Summary_Report (Self));

      --  Messages lifeage

      Gtk.Label.Gtk_New (Label, -"Message history");
      Filter_Box.Pack_Start (Label, False);

      Gtk.Check_Button.Gtk_New (Check, -"added");
      Check.Set_Active (Self.Show_Lifeage (Added));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Added_Messages_Toggled'Access),
         Summary_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"unchanged");
      Check.Set_Active (Self.Show_Lifeage (Unchanged));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Unchanged_Messages_Toggled'Access),
         Summary_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"removed");
      Check.Set_Active (Self.Show_Lifeage (Removed));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Removed_Messages_Toggled'Access),
         Summary_Report (Self));

      --  Messages probabilities

      Gtk.Separator.Gtk_New_Hseparator (Separator);
      Filter_Box.Pack_Start (Separator, False);

      Gtk.Label.Gtk_New (Label, -"Message ranking");
      Filter_Box.Pack_Start (Label, False);

      Gtk.Check_Button.Gtk_New (Check, -"suppressed");
      Check.Set_Active (Self.Show_Probabilities (Code_Peer.Suppressed));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Suppressed_Messages_Toggled'Access),
         Summary_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"informational");
      Check.Set_Active (Self.Show_Probabilities (Code_Peer.Informational));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Informational_Messages_Toggled'Access),
         Summary_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"low");
      Check.Set_Active (Self.Show_Probabilities (Code_Peer.Low));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Low_Messages_Toggled'Access),
         Summary_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"medium");
      Check.Set_Active (Self.Show_Probabilities (Code_Peer.Medium));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Medium_Messages_Toggled'Access),
         Summary_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"high");
      Check.Set_Active (Self.Show_Probabilities (Code_Peer.High));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_High_Messages_Toggled'Access),
         Summary_Report (Self));

      --  Messages categories

      Gtk.Separator.Gtk_New_Hseparator (Separator);
      Filter_Box.Pack_Start (Separator, False);

      Code_Peer.Categories_Criteria_Editors.Gtk_New
        (Self.Categories_Editor,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).
                    Message_Categories);
      Filter_Box.Pack_Start (Self.Categories_Editor);

      Message_Categories_Criteria_Callbacks.Connect
        (Self.Categories_Editor,
         Code_Peer.Categories_Criteria_Editors.Signal_Criteria_Changed,
         Message_Categories_Criteria_Callbacks.To_Marshaller
           (On_Categories_Criteria_Changed'Access),
         Summary_Report (Self));

      --

      GPS.Kernel.Modules.Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Self.Analysis_View,
         Object          => Self,
         ID              => Module,
         Context_Func    => Context_Func'Access);
   end Initialize;

   ----------------------------------
   -- Is_Messages_Category_Visible --
   ----------------------------------

   function Is_Messages_Category_Visible
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self  : Summary_Report) return Boolean
   is
      use Code_Peer.Categories_Criteria_Editors;

      Value    : Glib.Values.GValue;
      Category : Message_Category_Access;

   begin
      Model.Get_Value
        (Iter,
         Code_Peer.Entity_Messages_Models.Message_Category_Column,
         Value);
      Category :=
        Message_Category_Access
          (Message_Category_Conversions.To_Pointer
               (Glib.Values.Get_Address (Value)));
      Glib.Values.Unset (Value);

      return
        Self.Categories_Editor /= null
        --  Is_Messages_Category_Visible is called during initialization,
        --  thus this member can be null, just because editor is not created.
        and then Self.Categories_Editor.Get_Visible_Categories.Contains
          (Category)
        and then
          (Model.Get_String
               (Iter, Code_Peer.Entity_Messages_Models.Low_Count_Column) /= ""
           or else Model.Get_String
             (Iter, Code_Peer.Entity_Messages_Models.Medium_Count_Column) /= ""
           or else Model.Get_String
             (Iter, Code_Peer.Entity_Messages_Models.High_Count_Column) /= "");
   end Is_Messages_Category_Visible;

   -----------------------
   -- On_Analysis_Click --
   -----------------------

   function On_Analysis_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Summary_Report) return Boolean
   is
      pragma Unreferenced (View);

      use type Glib.Guint;
      use type Gdk.Event.Gdk_Event_Type;

      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sort_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      X         : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_X (Event));
      Y         : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_Y (Event));
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell_X    : Glib.Gint;
      Cell_Y    : Glib.Gint;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Found     : Boolean;

   begin
      if Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Press
      then
         --  When the callback is called the tree selection contains old
         --  selection, so we need to calculate and update selection.

         Self.Analysis_View.Get_Path_At_Pos
           (X, Y, Path, Column, Cell_X, Cell_Y, Found);

         if Path /= null then
            Self.Analysis_View.Get_Selection.Select_Path (Path);
            Sort_Iter := Self.Analysis_Sort_Model.Get_Iter (Path);
            Self.Analysis_Sort_Model.Convert_Iter_To_Child_Iter
              (Iter, Sort_Iter);

            declare
               Project_Node    : constant Code_Analysis.Project_Access :=
                 Self.Analysis_Model.Project_At (Iter);
               File_Node       : constant Code_Analysis.File_Access :=
                 Self.Analysis_Model.File_At (Iter);
               Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                 Self.Analysis_Model.Subprogram_At (Iter);

            begin
               if Subprogram_Node /= null then
                  Self.Messages_Model.Set (Subprogram_Node);

               elsif File_Node /= null then
                  Self.Messages_Model.Set (File_Node);

               elsif Project_Node /= null then
                  Self.Messages_Model.Set (Project_Node);

               elsif not Gtk.Tree_Model.Utils.Is_Null (Iter) then
                  Self.Messages_Model.Set (Self.Tree);
               end if;
            end;
         end if;

      elsif Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_2button_Press
      then
         Emit_By_Name (Self.Get_Object, Signal_Activated & ASCII.NUL);
      end if;

      return False;
   end On_Analysis_Click;

   ------------------------------------
   -- On_Categories_Criteria_Changed --
   ------------------------------------

   procedure On_Categories_Criteria_Changed
     (Object : access
        Code_Peer.Categories_Criteria_Editors.
          Categories_Criteria_Editor_Record'Class;
      Self   : Summary_Report)
   is
   begin
      Self.Analysis_Model.Set_Visible_Message_Categories
        (Object.Get_Visible_Categories);
      Self.Messages_Filter.Refilter;

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Categories_Criteria_Changed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Summary_Report_Record'Class) is
   begin
      --  Models' internal data must be cleaned before the code analysis data
      --  is cleaned, because models catch direct references to the code
      --  analysis data.

      Self.Analysis_Model.Clear;
      Self.Messages_Model.Clear;
   end On_Destroy;

   -------------------------------------
   -- On_Show_All_Subprograms_Toggled --
   -------------------------------------

   procedure On_Show_All_Subprograms_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Analysis_Model.Set_Show_All_Subprograms (Object.Get_Active);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_All_Subprograms_Toggled;

   -----------------------------------
   -- On_Show_Added_Messages_Toggled --
   ------------------------------------

   procedure On_Show_Added_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Lifeage (Added) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Added_Messages_Toggled;

   -----------------------------------
   -- On_Show_High_Messages_Toggled --
   -----------------------------------

   procedure On_Show_High_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Probabilities (High) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_High_Messages_Toggled;

   --------------------------------------------
   -- On_Show_Informational_Messages_Toggled --
   --------------------------------------------

   procedure On_Show_Informational_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Probabilities (Informational) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Informational_Messages_Toggled;

   ----------------------------------
   -- On_Show_Low_Messages_Toggled --
   ----------------------------------

   procedure On_Show_Low_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Probabilities (Low) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Low_Messages_Toggled;

   -------------------------------------
   -- On_Show_Medium_Messages_Toggled --
   -------------------------------------

   procedure On_Show_Medium_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Probabilities (Medium) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Medium_Messages_Toggled;

   --------------------------------------
   -- On_Show_Removed_Messages_Toggled --
   --------------------------------------

   procedure On_Show_Removed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Lifeage (Removed) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Removed_Messages_Toggled;

   -----------------------------------------
   -- On_Show_Suppressed_Messages_Toggled --
   -----------------------------------------

   procedure On_Show_Suppressed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Probabilities (Suppressed) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Suppressed_Messages_Toggled;

   ----------------------------------------
   -- On_Show_Unchanged_Messages_Toggled --
   ----------------------------------------

   procedure On_Show_Unchanged_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report) is
   begin
      Self.Show_Lifeage (Unchanged) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Unchanged_Messages_Toggled;

   ------------
   -- Update --
   ------------

   procedure Update (Self : access Summary_Report_Record'Class) is
   begin
      Self.Analysis_Model.Reconstruct;
      Self.Messages_Model.Update;
   end Update;

   ---------------------
   -- Update_Criteria --
   ---------------------

   procedure Update_Criteria
     (Self     : access Summary_Report_Record'Class;
      Criteria : in out Code_Peer.Message_Filter_Criteria)
   is
   begin
      Criteria.Categories    := Self.Categories_Editor.Get_Visible_Categories;
      Criteria.Probabilities := Self.Show_Probabilities;
      Criteria.Lineages      := Self.Show_Lifeage;
   end Update_Criteria;

end Code_Peer.Summary_Reports;
