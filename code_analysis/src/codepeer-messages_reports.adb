------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Characters.Latin_1;
with Interfaces.C.Strings;
with System;

with Glib.Object;
with Glib.Values;
with Gdk.Event;
with Gdk.Pixbuf;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Toggle_Button;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Sortable;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with Histories;
with GPS.Intl; use GPS.Intl;
with GPS.Kernel.Contexts;
with GPS.Kernel.Project;
with GPS.Kernel.Messages.View;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with Code_Analysis_GUI;
with CodePeer.Module;

package body CodePeer.Messages_Reports is

   use type Glib.Signal_Name;

   --  use type Code_Analysis.File_Access;
   --  ??? Uncomment this line after I120-013 will be fixed
   use type Code_Analysis.Project_Access;
   use type Code_Analysis.Subprogram_Access;

   package Tree_View_Report_Return_Boolean_Callbacks is
     new Gtk.Handlers.User_Return_Callback
           (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Messages_Report);

   package Summary_Report_Callbacks is new Gtk.Handlers.Callback
     (Messages_Report_Record);

   package Check_Button_Report_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk.Check_Button.Gtk_Check_Button_Record, Messages_Report);

   package Message_Categories_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.Categories_Criteria_Editors.
             Categories_Criteria_Editor_Record,
           Messages_Report);

   package Message_Lifeage_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.Lifeage_Criteria_Editors.Lifeage_Criteria_Editor_Record,
           Messages_Report);

   package Compare_Functions is
     new Gtk.Tree_Sortable.Compare_Funcs (Messages_Report);

   procedure On_Destroy (Self : access Messages_Report_Record'Class);

   procedure On_Show_All_Subprograms_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);

   procedure On_Show_Informational_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Low_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Medium_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_High_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Suppressed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   --  Handles change of state of items of ranking filter

   procedure On_Show_Unclassified_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Pending_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Not_A_Bug_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Intentional_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_False_Positive_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   procedure On_Show_Bug_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report);
   --  Handles change of state of items of review status filter

   procedure On_Categories_Criteria_Changed
     (Object : access
        CodePeer.Categories_Criteria_Editors.
          Categories_Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible message's categories.

   procedure On_Lifeage_Criteria_Changed
     (Object : access
        CodePeer.Lifeage_Criteria_Editors.Lifeage_Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible message's lifeages.

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
      Self  : Messages_Report) return Boolean;
   --  Handler of mouse press, double-press and release events. It handle
   --  selection on mouse press, and activation on double-press/release events.

   function Compare
     (Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Messages_Report) return Glib.Gint;
   --  Compare two rows in the model.

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Glib.Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   Ranking_Suppressed_History    : constant Histories.History_Key :=
     "codepeer-summary_report-ranking-suppressed";
   Ranking_Informational_History : constant Histories.History_Key :=
     "codepeer-summary_report-ranking-informational";
   Ranking_Low_History           : constant Histories.History_Key :=
     "codepeer-summary_report-ranking-low";
   Ranking_Medium_History        : constant Histories.History_Key :=
     "codepeer-summary_report-ranking-medium";
   Ranking_High_History          : constant Histories.History_Key :=
     "codepeer-summary_report-ranking-high";

   Status_Unclassified_History   : constant Histories.History_Key :=
     "codepeer-summary_report-status-unclassified";
   Status_Pending_History        : constant Histories.History_Key :=
     "codepeer-summary_report-status-pending";
   Status_Not_A_Bug_History      : constant Histories.History_Key :=
     "codepeer-summary_report-status-not_a_bug";
   Status_False_Positive_History : constant Histories.History_Key :=
     "codepeer-summary_report-status-false_positive";
   Status_Intentional_History    : constant Histories.History_Key :=
     "codepeer-summary_report-status-intentional";
   Status_Bug_History            : constant Histories.History_Key :=
     "codepeer-summary_report-status-bug";

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
      Self      : Messages_Report) return Glib.Gint
   is
      pragma Unreferenced (Self);

      use type Glib.Gint;

      type Column_Sort_Order is
        (High_Current, Medium_Current, Low_Current);
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
           (High_Current =>
              Get
                (Iter,
                 CodePeer.Messages_Summary_Models.High_Current_Count_Column),
            Medium_Current =>
              Get
                (Iter,
                 CodePeer.Messages_Summary_Models.Medium_Current_Count_Column),
            Low_Current =>
              Get
                (Iter,
                 CodePeer.Messages_Summary_Models.Low_Current_Count_Column));
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
      pragma Unreferenced (Menu, Event_Widget, Kernel);

      Self       : constant Messages_Report := Messages_Report (Object);
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
     (Self : access Messages_Report_Record'Class)
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
     (Self : access Messages_Report_Record'Class)
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
     (Self : access Messages_Report_Record'Class)
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
     (Report  : out Messages_Report;
      Kernel  : GPS.Kernel.Kernel_Handle;
      Module  : GPS.Kernel.Modules.Module_ID;
      Version : Supported_Format_Version;
      Tree    : Code_Analysis.Code_Analysis_Tree) is
   begin
      Report := new Messages_Report_Record;
      Initialize (Report, Kernel, Module, Version, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : access Messages_Report_Record'Class;
      Kernel  : GPS.Kernel.Kernel_Handle;
      Module  : GPS.Kernel.Modules.Module_ID;
      Version : Supported_Format_Version;
      Tree    : Code_Analysis.Code_Analysis_Tree)
   is
      use Gtk.Tree_Model_Sort;

      Panel           : Gtk.Paned.Gtk_Hpaned;
      Scrolled        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Message_Box     : Gtk.Box.Gtk_Hbox;
      Category_Box    : Gtk.Box.Gtk_Vbox;
      Filter_Box      : Gtk.Box.Gtk_Vbox;
      Check           : Gtk.Check_Button.Gtk_Check_Button;
      Label           : Gtk.Label.Gtk_Label;
      Separator       : Gtk.Separator.Gtk_Separator;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

      Project_Data : CodePeer.Project_Data'Class renames
        CodePeer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
               (Tree,
                GPS.Kernel.Project.Get_Project
                  (Kernel)).Analysis_Data.CodePeer_Data.all);

   begin
      Gtk.Box.Initialize_Vbox (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "CodePeerSummaryReport",
         Signal_Parameters);
      Summary_Report_Callbacks.Connect
        (Self,
         Gtk.Widget.Signal_Destroy,
         Summary_Report_Callbacks.To_Marshaller (On_Destroy'Access));

      Self.Kernel  := Kernel;
      Self.Version := Version;
      Self.Tree    := Tree;

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

      --  Restore filter settings from histories.

      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Ranking_Suppressed_History, False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Ranking_Informational_History, False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Ranking_Low_History, False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Ranking_Medium_History, True);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Ranking_High_History, True);

      Self.Show_Ranking (CodePeer.Suppressed) :=
        Histories.Get_History
          (Kernel.Get_History.all, Ranking_Suppressed_History);
      Self.Show_Ranking (CodePeer.Informational) :=
        Histories.Get_History
          (Kernel.Get_History.all, Ranking_Informational_History);
      Self.Show_Ranking (CodePeer.Low) :=
        Histories.Get_History (Kernel.Get_History.all, Ranking_Low_History);
      Self.Show_Ranking (CodePeer.Medium) :=
        Histories.Get_History (Kernel.Get_History.all, Ranking_Medium_History);
      Self.Show_Ranking (CodePeer.High) :=
        Histories.Get_History (Kernel.Get_History.all, Ranking_High_History);

      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Status_Unclassified_History, True);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Status_Pending_History, True);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Status_Not_A_Bug_History, False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Status_False_Positive_History, False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Status_Intentional_History, False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all, Status_Bug_History, True);

      Self.Show_Status (Unclassified) :=
        Histories.Get_History
          (Kernel.Get_History.all, Status_Unclassified_History);
      Self.Show_Status (Pending) :=
        Histories.Get_History (Kernel.Get_History.all, Status_Pending_History);
      Self.Show_Status (Not_A_Bug) :=
        Histories.Get_History
          (Kernel.Get_History.all, Status_Not_A_Bug_History);
      Self.Show_Status (False_Positive) :=
        Histories.Get_History
          (Kernel.Get_History.all, Status_False_Positive_History);
      Self.Show_Status (Intentional) :=
        Histories.Get_History
          (Kernel.Get_History.all, Status_Intentional_History);
      Self.Show_Status (Bug) :=
        Histories.Get_History (Kernel.Get_History.all, Status_Bug_History);

      --  Create report's widgets

      Gtk.Paned.Gtk_New_Hpaned (Panel);
      Self.Pack_Start (Panel);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Panel.Pack1 (Scrolled, Resize => True);

      CodePeer.Messages_Summary_Models.Gtk_New
        (Self.Analysis_Model,
         Tree,
         Project_Data.Message_Categories,
         Project_Icon,
         File_Icon,
         Subprogram_Icon);
      Gtk.Tree_Model_Sort.Gtk_New_With_Model
        (Self.Analysis_Sort_Model, Self.Analysis_Model);
      Compare_Functions.Set_Default_Sort_Func
        (+Self.Analysis_Sort_Model, Compare'Access, Messages_Report (Self));
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
         CodePeer.Messages_Summary_Models.Entity_Icon_Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.Entity_Name_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("High"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.High_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background_gdk",
         CodePeer.Messages_Summary_Models.High_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Med"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.Medium_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background_gdk",
         CodePeer.Messages_Summary_Models.Medium_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Low"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.Low_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background_gdk",
         CodePeer.Messages_Summary_Models.Low_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Passed" & Ada.Characters.Latin_1.LF & "checks"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.Passed_Checks_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Total" & Ada.Characters.Latin_1.LF & "checks"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.Total_Checks_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      --  Analysis view callbacks

      Tree_View_Report_Return_Boolean_Callbacks.Connect
        (Self.Analysis_View,
         Gtk.Widget.Signal_Button_Press_Event,
         Tree_View_Report_Return_Boolean_Callbacks.To_Marshaller
           (On_Analysis_Click'Access),
         Messages_Report (Self),
         False);
      Tree_View_Report_Return_Boolean_Callbacks.Connect
        (Self.Analysis_View,
         Gtk.Widget.Signal_Button_Release_Event,
         Tree_View_Report_Return_Boolean_Callbacks.To_Marshaller
           (On_Analysis_Click'Access),
         Messages_Report (Self),
         False);

      --  Message categories box

      Gtk.Box.Gtk_New_Hbox (Message_Box);
      Panel.Pack2 (Message_Box);

      Gtk.Box.Gtk_New_Vbox (Category_Box);
      Category_Box.Set_Size_Request (Width => 200);
      Message_Box.Pack_Start (Category_Box);

      --  Warning messages categories

      CodePeer.Categories_Criteria_Editors.Gtk_New
        (Self.Warning_Categories_Editor,
         Self.Kernel,
         -"Warning categories",
         "codepeer-summary_report-categories-warning",
         Project_Data.Warning_Categories);
      Category_Box.Pack_Start (Self.Warning_Categories_Editor);

      Message_Categories_Criteria_Callbacks.Connect
        (Self.Warning_Categories_Editor,
         CodePeer.Categories_Criteria_Editors.Signal_Criteria_Changed,
         Message_Categories_Criteria_Callbacks.To_Marshaller
           (On_Categories_Criteria_Changed'Access),
         Messages_Report (Self));

      --  Checks messages categories

      CodePeer.Categories_Criteria_Editors.Gtk_New
        (Self.Check_Categories_Editor,
         Self.Kernel,
         -"Check categories",
         "codepeer-summary_report-categories-check",
         Project_Data.Check_Categories);
      Category_Box.Pack_Start (Self.Check_Categories_Editor);

      Message_Categories_Criteria_Callbacks.Connect
        (Self.Check_Categories_Editor,
         CodePeer.Categories_Criteria_Editors.Signal_Criteria_Changed,
         Message_Categories_Criteria_Callbacks.To_Marshaller
           (On_Categories_Criteria_Changed'Access),
         Messages_Report (Self));

      --  Filter view

      Gtk.Box.Gtk_New_Vbox (Filter_Box);
      Filter_Box.Set_Size_Request (Width => 200);
      Message_Box.Pack_Start (Filter_Box);

      Gtk.Check_Button.Gtk_New (Check, -"Show all subprograms");
--  ???    Filter_Box.Pack_Start (Check, False);
--  This check button is not displayed by default, see H519-028 discussion
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_All_Subprograms_Toggled'Access),
         Messages_Report (Self));

      --  Messages history

      CodePeer.Lifeage_Criteria_Editors.Gtk_New
        (Self.Lifeage_Editor,
         Kernel,
         -"Message history",
         "codepeer-summary_report-lifeage");
      Filter_Box.Pack_Start (Self.Lifeage_Editor, False);
      Self.Analysis_Model.Set_Visible_Message_Lifeages
        (Self.Lifeage_Editor.Get_Visible_Lifeages);

      Message_Lifeage_Criteria_Callbacks.Connect
        (Self.Lifeage_Editor,
         CodePeer.Lifeage_Criteria_Editors.Signal_Criteria_Changed,
         Message_Lifeage_Criteria_Callbacks.To_Marshaller
           (On_Lifeage_Criteria_Changed'Access),
         Messages_Report (Self));

      --  Messages ranking

      Gtk.Separator.Gtk_New_Hseparator (Separator);
      Filter_Box.Pack_Start (Separator, False);

      Gtk.Label.Gtk_New (Label, -"Message ranking");
      Filter_Box.Pack_Start (Label, False);

      Gtk.Check_Button.Gtk_New (Check, -"suppressed");
      Check.Set_Active (Self.Show_Ranking (CodePeer.Suppressed));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Suppressed_Messages_Toggled'Access),
         Messages_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"informational");
      Check.Set_Active (Self.Show_Ranking (CodePeer.Informational));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Informational_Messages_Toggled'Access),
         Messages_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"low");
      Check.Set_Active (Self.Show_Ranking (CodePeer.Low));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Low_Messages_Toggled'Access),
         Messages_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"medium");
      Check.Set_Active (Self.Show_Ranking (CodePeer.Medium));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_Medium_Messages_Toggled'Access),
         Messages_Report (Self));

      Gtk.Check_Button.Gtk_New (Check, -"high");
      Check.Set_Active (Self.Show_Ranking (CodePeer.High));
      Filter_Box.Pack_Start (Check, False);
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_High_Messages_Toggled'Access),
         Messages_Report (Self));

      --  Message review status

      case Self.Version is
         when 2 =>
            --  There is no message review status support in this version.

            null;

         when 3 =>
            Gtk.Separator.Gtk_New_Hseparator (Separator);
            Filter_Box.Pack_Start (Separator, False);

            Gtk.Label.Gtk_New (Label, -"Message review status");
            Filter_Box.Pack_Start (Label, False);

            Gtk.Check_Button.Gtk_New (Check, -"unclassified");
            Check.Set_Active (Self.Show_Status (Unclassified));
            Filter_Box.Pack_Start (Check, False);
            Check_Button_Report_Callbacks.Connect
              (Check,
               Gtk.Toggle_Button.Signal_Toggled,
               Check_Button_Report_Callbacks.To_Marshaller
                 (On_Show_Unclassified_Messages_Toggled'Access),
               Messages_Report (Self));

            Gtk.Check_Button.Gtk_New (Check, -"pending");
            Check.Set_Active (Self.Show_Status (Pending));
            Filter_Box.Pack_Start (Check, False);
            Check_Button_Report_Callbacks.Connect
              (Check,
               Gtk.Toggle_Button.Signal_Toggled,
               Check_Button_Report_Callbacks.To_Marshaller
                 (On_Show_Pending_Messages_Toggled'Access),
               Messages_Report (Self));

            Gtk.Check_Button.Gtk_New (Check, -"not a bug");
            Check.Set_Active (Self.Show_Status (Not_A_Bug));
            Filter_Box.Pack_Start (Check, False);
            Check_Button_Report_Callbacks.Connect
              (Check,
               Gtk.Toggle_Button.Signal_Toggled,
               Check_Button_Report_Callbacks.To_Marshaller
                 (On_Show_Not_A_Bug_Messages_Toggled'Access),
               Messages_Report (Self));

            Gtk.Check_Button.Gtk_New (Check, -"false positive");
            Check.Set_Active (Self.Show_Status (False_Positive));
            Filter_Box.Pack_Start (Check, False);
            Check_Button_Report_Callbacks.Connect
              (Check,
               Gtk.Toggle_Button.Signal_Toggled,
               Check_Button_Report_Callbacks.To_Marshaller
                 (On_Show_False_Positive_Messages_Toggled'Access),
               Messages_Report (Self));

            Gtk.Check_Button.Gtk_New (Check, -"intentional");
            Check.Set_Active (Self.Show_Status (Intentional));
            Filter_Box.Pack_Start (Check, False);
            Check_Button_Report_Callbacks.Connect
              (Check,
               Gtk.Toggle_Button.Signal_Toggled,
               Check_Button_Report_Callbacks.To_Marshaller
                 (On_Show_Intentional_Messages_Toggled'Access),
               Messages_Report (Self));

            Gtk.Check_Button.Gtk_New (Check, -"bug");
            Check.Set_Active (Self.Show_Status (Bug));
            Filter_Box.Pack_Start (Check, False);
            Check_Button_Report_Callbacks.Connect
              (Check,
               Gtk.Toggle_Button.Signal_Toggled,
               Check_Button_Report_Callbacks.To_Marshaller
                 (On_Show_Bug_Messages_Toggled'Access),
               Messages_Report (Self));
      end case;

      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);

      --  Register contextual menu handler

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Self.Analysis_View,
         Object          => Self,
         ID              => Module,
         Context_Func    => Context_Func'Access);
   end Initialize;

   -----------------------
   -- On_Analysis_Click --
   -----------------------

   function On_Analysis_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Messages_Report) return Boolean
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
         --  Reset double click flag.

         Self.Double_Click := False;

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
               File_Node       : constant Code_Analysis.File_Access :=
                 Self.Analysis_Model.File_At (Iter);

            begin
               --  Request Locations View to expand corresponding category/file

               if File_Node /= null then
                  GPS.Kernel.Messages.View.Expand_File
                    (Self.Kernel,
                     CodePeer.Module.CodePeer_Category_Name,
                     File_Node.Name);
               end if;
            end;
         end if;

      elsif Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_2button_Press
      then
         --  Set double click flag.

         Self.Double_Click := True;

      elsif Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Release
        and then Self.Double_Click
      then
         --  Reset double-click flag and emit signal.

         Self.Double_Click := False;
         Emit_By_Name (Self.Get_Object, Signal_Activated & ASCII.NUL);
      end if;

      return False;
   end On_Analysis_Click;

   ------------------------------------
   -- On_Categories_Criteria_Changed --
   ------------------------------------

   procedure On_Categories_Criteria_Changed
     (Object : access
        CodePeer.Categories_Criteria_Editors.
          Categories_Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Analysis_Model.Set_Visible_Message_Categories
        (Self.Warning_Categories_Editor.Get_Visible_Categories.Union
           (Self.Check_Categories_Editor.Get_Visible_Categories));

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Categories_Criteria_Changed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Messages_Report_Record'Class) is
   begin
      --  Models' internal data must be cleaned before the code analysis data
      --  is cleaned, because models catch direct references to the code
      --  analysis data.

      Self.Analysis_Model.Clear;
   end On_Destroy;

   -------------------------------------
   -- On_Show_All_Subprograms_Toggled --
   -------------------------------------

   procedure On_Show_All_Subprograms_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Analysis_Model.Set_Show_All_Subprograms (Object.Get_Active);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_All_Subprograms_Toggled;

   ----------------------------------
   -- On_Show_Bug_Messages_Toggled --
   ----------------------------------

   procedure On_Show_Bug_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Status (Bug) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Status_Bug_History,
         Self.Show_Status (Bug));
      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Bug_Messages_Toggled;

   ---------------------------------------------
   -- On_Show_False_Positive_Messages_Toggled --
   ---------------------------------------------

   procedure On_Show_False_Positive_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Status (False_Positive) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Status_False_Positive_History,
         Self.Show_Status (False_Positive));
      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_False_Positive_Messages_Toggled;

   -----------------------------------
   -- On_Show_High_Messages_Toggled --
   -----------------------------------

   procedure On_Show_High_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Ranking (High) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Ranking_High_History,
         Self.Show_Ranking (High));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_High_Messages_Toggled;

   --------------------------------------------
   -- On_Show_Informational_Messages_Toggled --
   --------------------------------------------

   procedure On_Show_Informational_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Ranking (Informational) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Ranking_Informational_History,
         Self.Show_Ranking (Informational));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Informational_Messages_Toggled;

   ------------------------------------------
   -- On_Show_Intentional_Messages_Toggled --
   ------------------------------------------

   procedure On_Show_Intentional_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Status (Intentional) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Status_Intentional_History,
         Self.Show_Status (Intentional));
      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Intentional_Messages_Toggled;

   ----------------------------------
   -- On_Show_Low_Messages_Toggled --
   ----------------------------------

   procedure On_Show_Low_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Ranking (Low) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Ranking_Low_History,
         Self.Show_Ranking (Low));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Low_Messages_Toggled;

   -------------------------------------
   -- On_Show_Medium_Messages_Toggled --
   -------------------------------------

   procedure On_Show_Medium_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Ranking (Medium) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Ranking_Medium_History,
         Self.Show_Ranking (Medium));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Medium_Messages_Toggled;

   ----------------------------------------
   -- On_Show_Not_A_Bug_Messages_Toggled --
   ----------------------------------------

   procedure On_Show_Not_A_Bug_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Status (Not_A_Bug) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Status_Not_A_Bug_History,
         Self.Show_Status (Not_A_Bug));
      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Not_A_Bug_Messages_Toggled;

   --------------------------------------
   -- On_Show_Pending_Messages_Toggled --
   --------------------------------------

   procedure On_Show_Pending_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Status (Pending) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Status_Pending_History,
         Self.Show_Status (Pending));
      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Pending_Messages_Toggled;

   -----------------------------------------
   -- On_Show_Suppressed_Messages_Toggled --
   -----------------------------------------

   procedure On_Show_Suppressed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Ranking (Suppressed) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Ranking_Suppressed_History,
         Self.Show_Ranking (Suppressed));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Suppressed_Messages_Toggled;

   -------------------------------------------
   -- On_Show_Unclassified_Messages_Toggled --
   -------------------------------------------

   procedure On_Show_Unclassified_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Messages_Report) is
   begin
      Self.Show_Status (Unclassified) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Status_Unclassified_History,
         Self.Show_Status (Unclassified));
      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Unclassified_Messages_Toggled;

   ---------------------------------
   -- On_Lifeage_Criteria_Changed --
   ---------------------------------

   procedure On_Lifeage_Criteria_Changed
     (Object : access
        CodePeer.Lifeage_Criteria_Editors.Lifeage_Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Analysis_Model.Set_Visible_Message_Lifeages
        (Self.Lifeage_Editor.Get_Visible_Lifeages);

      --  Emit 'criteria-changed' signal.

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Lifeage_Criteria_Changed;

   ------------
   -- Update --
   ------------

   procedure Update (Self : access Messages_Report_Record'Class) is
   begin
      Self.Analysis_Model.Reconstruct;
   end Update;

   ---------------------
   -- Update_Criteria --
   ---------------------

   procedure Update_Criteria
     (Self     : access Messages_Report_Record'Class;
      Criteria : in out CodePeer.Message_Filter_Criteria)
   is
   begin
      Criteria.Categories :=
        Self.Warning_Categories_Editor.Get_Visible_Categories.Union
          (Self.Check_Categories_Editor.Get_Visible_Categories);
      Criteria.Rankings   := Self.Show_Ranking;
      Criteria.Lineages   := Self.Lifeage_Editor.Get_Visible_Lifeages;

      case Self.Version is
         when 2 =>
            Criteria.Statuses := (others => True);

         when 3 =>
            Criteria.Statuses := Self.Show_Status;
      end case;
   end Update_Criteria;

end CodePeer.Messages_Reports;
