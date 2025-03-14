------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Interfaces.C.Strings;
with System;

with VSS.Strings.Conversions;

with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib.Object;
with Glib.Values;
with Gtk.Box;               use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Sortable;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtkada.MDI;            use Gtkada.MDI;

with Histories;             use Histories;
with GNATCOLL.Projects;
with GNATCOLL.Strings;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GPS.Intl;              use GPS.Intl;
with GPS.Kernel.Contexts;
with GPS.Kernel.Project;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with GPS.Location_View;     use GPS.Location_View;
with GPS.Search;
with GUI_Utils;             use GUI_Utils;
with Projects.Views;
with Tooltips;              use Tooltips;

with CodePeer.Module;

package body CodePeer.Messages_Reports is

   use type Glib.Signal_Name;

   package Tree_View_Report_Return_Boolean_Callbacks is
     new Gtk.Handlers.User_Return_Callback
           (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Messages_Report);

   package Summary_Report_Callbacks is new Gtk.Handlers.Callback
     (Messages_Report_Record);

   package Message_Categories_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.Categories_Criteria_Editors.Criteria_Editor_Record,
           Messages_Report);

   package CWE_Categories_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.CWE_Criteria_Editors.Criteria_Editor_Record,
           Messages_Report);

   package Lifeage_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.Lifeage_Categories_Criteria_Editors.Criteria_Editor_Record,
           Messages_Report);

   package Ranking_Criteria_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.Ranking_Categories_Criteria_Editors.Criteria_Editor_Record,
           Messages_Report);

   package Audit_Statuses_Callbacks is
     new Gtk.Handlers.User_Callback
          (CodePeer.Audit_Statuses_Criteria_Editors.Criteria_Editor_Record,
           Messages_Report);

   package Compare_Functions is
     new Gtk.Tree_Sortable.Set_Default_Sort_Func_User_Data (Messages_Report);

   procedure On_Destroy (Self : access Messages_Report_Record'Class);

   procedure On_Filter_Changed (Self : access Messages_Report_Record'Class);
   --  Callback for filter-canged signal

   procedure On_Categories_Criteria_Changed
     (Object : access
        CodePeer.Categories_Criteria_Editors.Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible message's categories.

   procedure On_CWE_Criteria_Changed
     (Object : access
        CodePeer.CWE_Criteria_Editors.Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible CWE's categories.

   procedure On_Lifeage_Criteria_Changed
     (Object : access
        CodePeer.Lifeage_Categories_Criteria_Editors.
          Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible message's lifeages.

   procedure On_Ranking_Criteria_Changed
     (Object : access
        CodePeer.Ranking_Categories_Criteria_Editors.
          Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible message's ranking.

   procedure On_Audit_Statuses_Changed
     (Object : access
        CodePeer.Audit_Statuses_Criteria_Editors.
          Criteria_Editor_Record'Class;
      Self   : Messages_Report);
   --  Handles change of set of visible message's audit status.

   function On_Analysis_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Messages_Report) return Boolean;
   --  Handler of mouse press, double-press and release events. It handle
   --  selection on mouse press, and activation on double-press/release events.

   function Compare
     (Model     : Gtk_Tree_Model;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Messages_Report) return Glib.Gint;
   --  Compare two rows in the model.

   function Compute_Level
     (Project_Data : CodePeer.Project_Data'Class)
      return Natural;
   --  Using information in the given Project_Data, compute the level of the
   --  current analysis

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Glib.Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   Ranking_History_Prefix : constant Histories.History_Key :=
     "codepeer-summary_report-ranking-";

   Status_History_Prefix : constant Histories.History_Key :=
     "codepeer-summary_report-status-";
   Lifeage_History_Prefix : constant Histories.History_Key :=
     "codepeer-summary_report-lifeage-";

   Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Activated)),
      2 => Interfaces.C.Strings.New_String (String (Signal_Criteria_Changed)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None),
      2 => (1 => Glib.GType_None));

   CWE_Attribute :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build
       (VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name), "CWE");

   --------------
   -- Tooltips --
   --------------

   type Codepeer_Message_View_Tooltip_Handler is
     new Tooltips.Tooltip_Handler with
      record
         Report : Messages_Report;
      end record;

   overriding function Create_Contents
     (Tooltip : not null access Codepeer_Message_View_Tooltip_Handler;
      Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y    : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Add tooltips containing the full path to the hover entity

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip : not null access Codepeer_Message_View_Tooltip_Handler;
      Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y    : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Iter       : Gtk_Tree_Iter;
      Model_Path : Gtk_Tree_Path;
      Path       : Gtk_Tree_Path;
      Area       : Gdk_Rectangle;
      Box        : Gtk_Box;
   begin
      Initialize_Tooltips (Tooltip.Report.Analysis_View, X, Y, Area, Iter);

      if Iter /= Null_Iter then
         Tooltip.Set_Tip_Area (Area);
         Gtk_New_Vbox (Box);

         --  Retrieve the model iter from the sorted iter
         Path := Get_Path (Tooltip.Report.Analysis_View.Get_Model, Iter);
         Model_Path :=
           Tooltip.Report.Analysis_Sort_Model.Convert_Path_To_Child_Path
             (Path);
         Iter := Tooltip.Report.Analysis_Model.Get_Iter (Model_Path);

         --  Get the file/project information from code_analysis
         declare
            File  : constant File_Access :=
              Tooltip.Report.Analysis_Model.File_At (Iter);
            Project : constant Project_Access :=
              Tooltip.Report.Analysis_Model.Project_At (Iter);
            Label : Gtk_Label;
         begin
            if File /= null and then File.Name.Is_Regular_File then
               Gtk_New (Label, File.Name.Display_Full_Name);
               Box.Pack_Start (Label);
            elsif Project /= null
              and then Project.View.Get_Project_Path.Is_Regular_File
            then
               Gtk_New
                 (Label, Project.View.Get_Project_Path.Display_Full_Name);
               Box.Pack_Start (Label);
            else
               return null;
            end if;
         end;
         return Gtk.Widget.Gtk_Widget (Box);
      else
         return null;
      end if;
   end Create_Contents;

   -------------
   -- Compare --
   -------------

   function Compare
     (Model     : Gtk_Tree_Model;
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
         Get_Value (Model, Iter, Column, Value);

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
      if Parent (Model, A) = Gtk.Tree_Model.Null_Iter then
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

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context
     (Self   : not null access Messages_Report_Record'Class;
      Event  : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Model_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Project    : Code_Analysis.Project_Access;
      File       : Code_Analysis.File_Access;
      Subprogram : Code_Analysis.Subprogram_Access;
      Context  : Selection_Context :=
        New_Context
          (Self.Kernel,
           Get_Module_From_Child
             (GPS_MDI_Child (Find_MDI_Child_From_Widget (Self))));
   begin
      Iter := Find_Iter_For_Event (Self.Analysis_View, Event);
      if Iter /= Null_Iter then
         Path := Get_Path (Self.Analysis_View.Get_Model, Iter);

         if Event /= null then
            Self.Analysis_View.Get_Selection.Select_Path (Path);
         end if;

         Model_Path :=
           Self.Analysis_Sort_Model.Convert_Path_To_Child_Path (Path);
         Iter       := Self.Analysis_Model.Get_Iter (Model_Path);
         Project    := Self.Analysis_Model.Project_At (Iter);
         File       := Self.Analysis_Model.File_At (Iter);
         Subprogram := Self.Analysis_Model.Subprogram_At (Iter);

         if Subprogram /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.View.Get_Project_Type,
               Files   => (1 => File.Name));
            GPS.Kernel.Contexts.Set_Entity_Information
              (Context     => Context,
               Entity_Name => Subprogram.Name.all);

         elsif File /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.View.Get_Project_Type,
               Files   => (1 => File.Name));

         elsif Project /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.View.Get_Project_Type);
         end if;

         Gtk.Tree_Model.Path_Free (Model_Path);
         Gtk.Tree_Model.Path_Free (Path);
      end if;
      return Context;
   end Build_Context;

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
      Version : Supported_Format_Version;
      Tree    : Code_Analysis.Code_Analysis_Tree) is
   begin
      Report := new Messages_Report_Record;
      Initialize (Report, Kernel, Version, Tree);
   end Gtk_New;

   -------------------
   -- Compute_Level --
   -------------------

   function Compute_Level
     (Project_Data : CodePeer.Project_Data'Class)
      return Natural
   is
      use GNATCOLL.Strings;

      Library_File : constant String :=
        Ada.Strings.Unbounded.To_String (Project_Data.Current.Library_File);
      Splitted     : constant XString_Array :=
        Split (To_XString (Library_File), '.');
   begin
      if Splitted'Length <= 2 then
         --  This is the previous default level, 3.
         return 3;
      else
         begin
            return Natural'Value (To_String (Splitted (Splitted'First + 1)));
         exception
            when Constraint_Error =>
               --  Let's assume level 3 if for some reason the value cannot be
               --  parsed.
               return 3;
         end;
      end if;
   end Compute_Level;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : access Messages_Report_Record'Class;
      Kernel  : GPS.Kernel.Kernel_Handle;
      Version : Supported_Format_Version;
      Tree    : Code_Analysis.Code_Analysis_Tree)
   is
      use Gtk.Tree_Model_Sort;
      use Filter_Panels;
      use type CodePeer.CWE_Criteria_Editors.Criteria_Editor;

      Panel           : Gtk.Paned.Gtk_Hpaned;
      Filter_Panel    : Gtk.Paned.Gtk_Vpaned;
      Scrolled        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Message_Box     : Gtk.Box.Gtk_Hbox;
      Category_Box    : Gtk.Box.Gtk_Vbox;
      Filter_Box      : Gtk.Box.Gtk_Vbox;
      Separator       : Gtk.Separator.Gtk_Separator;
      Dummy           : Glib.Gint;
      Tooltip         : Tooltips.Tooltip_Handler_Access;
      pragma Warnings (Off, Dummy);

      Project_View    : constant Projects.Views.Project_View_Reference :=
        GPS.Kernel.Project.Get_Root_Project_View (Kernel);
      Project_Data    : CodePeer.Project_Data'Class renames
        CodePeer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
               (Tree, Project_View).Analysis_Data.CodePeer_Data.all);
      Analysis_Level  : constant Natural := Compute_Level (Project_Data);
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Box.Get_Vbox_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "CodePeerSummaryReport",
         Parameters   => Signal_Parameters);
      Glib.Object.G_New (Self, Class_Record);

      Summary_Report_Callbacks.Connect
        (Self,
         Gtk.Widget.Signal_Destroy,
         Summary_Report_Callbacks.To_Marshaller (On_Destroy'Access));

      Self.Kernel  := Kernel;
      Self.Version := Version;
      Self.Tree    := Tree;

      --  Restore filter settings from histories.

      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Ranking_History_Prefix &
           Histories.History_Key (Image (CodePeer.Suppressed)), False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Ranking_History_Prefix &
           Histories.History_Key (Image (CodePeer.Info)), False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Ranking_History_Prefix &
           Histories.History_Key (Image (CodePeer.Low)), False);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Ranking_History_Prefix &
           Histories.History_Key (Image (CodePeer.Medium)), True);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Ranking_History_Prefix &
           Histories.History_Key (Image (CodePeer.High)), True);

      Self.Show_Ranking :=
        (Not_An_Error => False,
         Suppressed   => False,
         Info         =>
           Histories.Get_History
             (Kernel.Get_History.all,
              Ranking_History_Prefix &
                Histories.History_Key (Image (CodePeer.Info))),
         Low          =>
           Histories.Get_History
             (Kernel.Get_History.all,
              Ranking_History_Prefix &
                Histories.History_Key (Image (CodePeer.Low))),
         Medium       =>
           Histories.Get_History
             (Kernel.Get_History.all,
              Ranking_History_Prefix &
                Histories.History_Key (Image (CodePeer.Medium))),
         High         =>
           Histories.Get_History
             (Kernel.Get_History.all,
              Ranking_History_Prefix &
                Histories.History_Key (Image (CodePeer.High))));

      for Status of Audit_Statuses loop
         declare
            Name : constant History_Key :=
              Status_History_Prefix &
              History_Key (Standardize (Image (Status.all)));
         begin
            Histories.Create_New_Boolean_Key_If_Necessary
              (Kernel.Get_History.all, Name, Status.Category /= Not_A_Bug);
            Self.Show_Status (Status.Id) :=
              Histories.Get_History (Kernel.Get_History.all, Name);
         end;
      end loop;

      for Lifeage of Project_Data.Lifeage_Subcategories loop
         declare
            Name : constant History_Key :=
              Lifeage_History_Prefix &
              History_Key (Get_Name (Lifeage.all));
         begin
            Histories.Create_New_Boolean_Key_If_Necessary
              (Kernel.Get_History.all, Name, Lifeage.all /= Removed);
         end;
      end loop;

      --  Create report's widgets

      Gtk.Paned.Gtk_New_Hpaned (Panel);
      Self.Pack_Start (Panel);

      --  Filter. It will be automatically deallocated by gtk+, because it is
      --  put in the toolbar of the view, which is destroyed when the view is
      --  destroyed.

      Gtk.Paned.Gtk_New_Vpaned (Filter_Panel);
      Gtk_New
        (Panel       => Self.Filter,
         Kernel      => Self.Kernel,
         Hist_Prefix => "codepeer-summary_report-filter",
         Tooltip     => "The text pattern or regular expression",
         Placeholder => "filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy,
         Name        => "Report View Filter");

      Summary_Report_Callbacks.Object_Connect
        (Self.Filter,
         Filter_Panels.Signal_Filter_Changed,
         Summary_Report_Callbacks.To_Marshaller
           (On_Filter_Changed'Access),
         Self);
      Filter_Panel.Pack1 (Self.Filter, Resize => False);

      --  Projects/files tree view

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Filter_Panel.Pack2 (Scrolled, Resize => True);

      Panel.Pack1 (Filter_Panel, Resize => True);

      CodePeer.Messages_Summary_Models.Gtk_New
        (Self.Analysis_Model,
         Tree,
         Project_Data.Message_Categories);
      Gtk.Tree_Model_Sort.Gtk_New_With_Model
        (Self.Analysis_Sort_Model, To_Interface (Self.Analysis_Model));
      Compare_Functions.Set_Default_Sort_Func
        (+Self.Analysis_Sort_Model, Compare'Access, Messages_Report (Self));
      Gtk.Tree_View.Gtk_New
        (Self.Analysis_View, +Self.Analysis_Sort_Model);
      Self.Analysis_View.Set_Name ("gnatsas-messages_summary_tree");
      Scrolled.Add (Self.Analysis_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Entity");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf_Renderer);
      Column.Pack_Start (Pixbuf_Renderer, False);
      Column.Add_Attribute
        (Pixbuf_Renderer,
         "icon-name",
         CodePeer.Messages_Summary_Models.Entity_Icon_Name_Column);
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
         "cell-background-rgba",
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
         "cell-background-rgba",
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
         "cell-background-rgba",
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
      Column.Set_Visible (not Is_GNATSAS and then Analysis_Level > 0);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Total" & Ada.Characters.Latin_1.LF & "checks"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         CodePeer.Messages_Summary_Models.Total_Checks_Count_Column);
      Column.Set_Visible (not Is_GNATSAS and then Analysis_Level > 0);
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
        (Editor         => Self.Warning_Categories_Editor,
         Kernel         => Self.Kernel,
         Title          => -"Warning categories",
         History_Prefix => "codepeer-summary_report-categories-warning",
         Items          => Project_Data.Warning_Subcategories,
         Default        => True,
         Tree_Name      => "gnatsas-warning_categories");
      Category_Box.Pack_Start (Self.Warning_Categories_Editor);

      Message_Categories_Criteria_Callbacks.Connect
        (Self.Warning_Categories_Editor,
         CodePeer.Categories_Criteria_Editors.Signal_Criteria_Changed,
         Message_Categories_Criteria_Callbacks.To_Marshaller
           (On_Categories_Criteria_Changed'Access),
         Messages_Report (Self));

      --  Checks messages categories

      CodePeer.Categories_Criteria_Editors.Gtk_New
        (Editor         => Self.Check_Categories_Editor,
         Kernel         => Self.Kernel,
         Title          => -"Check categories",
         History_Prefix => "codepeer-summary_report-categories-check",
         Items          => Project_Data.Check_Subcategories,
         Default        => True,
         Tree_Name      => "gnatsas-check_categories");
      Category_Box.Pack_Start (Self.Check_Categories_Editor);

      Message_Categories_Criteria_Callbacks.Connect
        (Self.Check_Categories_Editor,
         CodePeer.Categories_Criteria_Editors.Signal_Criteria_Changed,
         Message_Categories_Criteria_Callbacks.To_Marshaller
           (On_Categories_Criteria_Changed'Access),
         Messages_Report (Self));

      --  CWEs categories

      if Is_GNATSAS
        or else (Project_View.Has_Attribute (CWE_Attribute)
          and then To_Lower
            (Project_View.Get_Attribute_Value (CWE_Attribute)) = "true")
      then
         CodePeer.CWE_Criteria_Editors.Gtk_New
           (Editor         => Self.CWE_Editor,
            Kernel         => Self.Kernel,
            Title          => -"CWE categories",
            History_Prefix => "codepeer-summary-report-categories-cwe",
            Items          => Project_Data.CWE_Categories,
            Default        => False,
            Tree_Name      => "gnatsas-cwe_categories");
         Category_Box.Pack_Start (Self.CWE_Editor);

         CWE_Categories_Criteria_Callbacks.Connect
           (Self.CWE_Editor,
            CodePeer.CWE_Criteria_Editors.Signal_Criteria_Changed,
            CWE_Categories_Criteria_Callbacks.To_Marshaller
              (On_CWE_Criteria_Changed'Access),
            Messages_Report (Self));
      end if;

      --  Filter view

      Gtk.Box.Gtk_New_Vbox (Filter_Box);
      Filter_Box.Set_Size_Request (Width => 200);
      Message_Box.Pack_Start (Filter_Box);

      --  Messages history

      CodePeer.Lifeage_Categories_Criteria_Editors.Gtk_New
        (Editor         => Self.Lifeage_Editor,
         Kernel         => Self.Kernel,
         Title          => -"Message history",
         History_Prefix => "codepeer-summary_report-lifeage",
         Items          => Project_Data.Lifeage_Subcategories,
         Default        => True,
         Tree_Name      => "gnatsas-message_history");
      Filter_Box.Pack_Start (Self.Lifeage_Editor);

      Lifeage_Criteria_Callbacks.Connect
        (Self.Lifeage_Editor,
         CodePeer.Lifeage_Categories_Criteria_Editors.Signal_Criteria_Changed,
         Lifeage_Criteria_Callbacks.To_Marshaller
           (On_Lifeage_Criteria_Changed'Access),
         Messages_Report (Self));

      --  Messages ranking

      Gtk.Separator.Gtk_New_Hseparator (Separator);
      Filter_Box.Pack_Start (Separator, False);

      CodePeer.Ranking_Categories_Criteria_Editors.Gtk_New
        (Editor         => Self.Ranking_Editor,
         Kernel         => Self.Kernel,
         Title          => -"Message ranking",
         History_Prefix => String
           (Ranking_History_Prefix
                (Ranking_History_Prefix'First ..
                     Ranking_History_Prefix'Last - 1)),  --  Delete last '-'
         Items          => Project_Data.Ranking_Subcategories,
         Default        => True,
         Tree_Name      => "gnatsas-message_ranking");
      Filter_Box.Pack_Start (Self.Ranking_Editor);

      Ranking_Criteria_Callbacks.Connect
        (Self.Ranking_Editor,
         CodePeer.Ranking_Categories_Criteria_Editors.Signal_Criteria_Changed,
         Ranking_Criteria_Callbacks.To_Marshaller
           (On_Ranking_Criteria_Changed'Access),
         Messages_Report (Self));

      --  Message review status

      Gtk.Separator.Gtk_New_Hseparator (Separator);
      Filter_Box.Pack_Start (Separator, False);

      CodePeer.Audit_Statuses_Criteria_Editors.Gtk_New
        (Editor         => Self.Audit_Editor,
         Kernel         => Self.Kernel,
         Title          => -"Message review status",
         History_Prefix => "codepeer-summary_report-status",
         Items          => Audit_Statuses,
         Default        => True,
         Tree_Name      => "gnatsas-message_review_status");
      Filter_Box.Pack_Start (Self.Audit_Editor);

      Audit_Statuses_Callbacks.Connect
        (Self.Audit_Editor,
         CodePeer.Audit_Statuses_Criteria_Editors.Signal_Criteria_Changed,
         Audit_Statuses_Callbacks.To_Marshaller
           (On_Audit_Statuses_Changed'Access),
         Messages_Report (Self));

      --  Set actual filter criteria (criteria are loaded from preferences to
      --  restore values used in last session).

      Self.Analysis_Model.Set_Visible_Message_Lifeages
        (To_Lifeage_Kinds_Flags (Self.Lifeage_Editor.Get_Visible_Items));
      Self.Set_Visible_Message_Status;
      Self.Analysis_Model.Set_Visible_Message_Categories
        (Self.Warning_Categories_Editor.Get_Visible_Items.Union
           (Self.Check_Categories_Editor.Get_Visible_Items));
      Self.Set_Visible_Ranking_Categories;

      if Self.CWE_Editor /= null then
         Self.Analysis_Model.Set_Visible_CWE_Categories
           (Self.CWE_Editor.Get_Visible_Items);
      end if;

      --  Register tooltip handler

      Tooltip := new Codepeer_Message_View_Tooltip_Handler'
        (Tooltips.Tooltip_Handler with Report => Messages_Report (Self));
      Tooltip.Associate_To_Widget (Self.Analysis_View);

      --  Register contextual menu handler

      Setup_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Self.Analysis_View);
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

      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sort_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      X         : constant Glib.Gint := Glib.Gint (Event.Button.X);
      Y         : constant Glib.Gint := Glib.Gint (Event.Button.Y);
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

         if Path /= Null_Gtk_Tree_Path then
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
                  Expand_File
                    (Get_Or_Create_Location_View (Self.Kernel),
                     CodePeer.Module.CodePeer_Category_Name,
                     File_Node.Name,
                     Goto_First => False);
                  Raise_Locations_Window (Self.Kernel);
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
        CodePeer.Categories_Criteria_Editors.Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Analysis_Model.Set_Visible_Message_Categories
        (Self.Warning_Categories_Editor.Get_Visible_Items.Union
           (Self.Check_Categories_Editor.Get_Visible_Items));

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Categories_Criteria_Changed;

   -----------------------------
   -- On_CWE_Criteria_Changed --
   -----------------------------

   procedure On_CWE_Criteria_Changed
     (Object : access
        CodePeer.CWE_Criteria_Editors.Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Analysis_Model.Set_Visible_CWE_Categories
        (Self.CWE_Editor.Get_Visible_Items);

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_CWE_Criteria_Changed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Messages_Report_Record'Class) is
   begin
      --  Models' internal data must be cleaned before the code analysis data
      --  is cleaned, because models catch direct references to the code
      --  analysis data.

      Self.Analysis_Model.Clear;
      Self.Analysis_Model.Set_Pattern (null);
   end On_Destroy;

   -----------------------
   -- On_Filter_Changed --
   -----------------------

   procedure On_Filter_Changed (Self : access Messages_Report_Record'Class)
   is
      P : constant GPS.Search.Search_Pattern_Access :=
        Self.Filter.Get_Filter_Pattern;
   begin
      Self.Analysis_Model.Set_Pattern (P);
   end On_Filter_Changed;

   ---------------------------------
   -- On_Lifeage_Criteria_Changed --
   ---------------------------------

   procedure On_Lifeage_Criteria_Changed
     (Object : access
        CodePeer.Lifeage_Categories_Criteria_Editors.
          Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Analysis_Model.Set_Visible_Message_Lifeages
        (To_Lifeage_Kinds_Flags (Self.Lifeage_Editor.Get_Visible_Items));

      --  Emit 'criteria-changed' signal.

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Lifeage_Criteria_Changed;

   ------------------------------------
   -- Set_Visible_Ranking_Categories --
   ------------------------------------

   procedure Set_Visible_Ranking_Categories
     (Self : in out Messages_Report_Record) is
   begin
      for Item in Ranking_Kinds'Range loop
         Self.Show_Ranking (Item) := False;
      end loop;

      for Item of Self.Ranking_Editor.Get_Visible_Items loop
         Self.Show_Ranking (Item.all) := True;
      end loop;

      Self.Analysis_Model.Set_Visible_Ranking_Categories (Self.Show_Ranking);
   end Set_Visible_Ranking_Categories;

   ---------------------------------
   -- On_Ranking_Criteria_Changed --
   ---------------------------------

   procedure On_Ranking_Criteria_Changed
     (Object : access
        CodePeer.Ranking_Categories_Criteria_Editors.
          Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Set_Visible_Ranking_Categories;

      --  Emit 'criteria-changed' signal.

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Ranking_Criteria_Changed;

   --------------------------------
   -- Set_Visible_Message_Status --
   --------------------------------

   procedure Set_Visible_Message_Status
     (Self : in out Messages_Report_Record) is
   begin
      for Status of Audit_Statuses loop
         Self.Show_Status (Status.Id) := False;
      end loop;

      for Status of Self.Audit_Editor.Get_Visible_Items loop
         Self.Show_Status (Status.Id) := True;
      end loop;

      for Status of Audit_Statuses loop
         Histories.Set_History
           (Self.Kernel.Get_History.all,
            Status_History_Prefix &
              History_Key (Standardize (Image (Status.all))),
            Self.Show_Status (Status.Id));
      end loop;

      Self.Analysis_Model.Set_Visible_Message_Status (Self.Show_Status);
   end Set_Visible_Message_Status;

   -------------------------------
   -- On_Audit_Statuses_Changed --
   -------------------------------

   procedure On_Audit_Statuses_Changed
     (Object : access
        CodePeer.Audit_Statuses_Criteria_Editors.
          Criteria_Editor_Record'Class;
      Self   : Messages_Report)
   is
      pragma Unreferenced (Object);

   begin
      Self.Set_Visible_Message_Status;

      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Audit_Statuses_Changed;

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
      use type CodePeer.CWE_Criteria_Editors.Criteria_Editor;

   begin
      Criteria.Categories :=
        Self.Warning_Categories_Editor.Get_Visible_Items.Union
          (Self.Check_Categories_Editor.Get_Visible_Items);
      Criteria.CWEs       :=
        (if Self.CWE_Editor /= null
         then Self.CWE_Editor.Get_Visible_Items
         else CodePeer.CWE_Category_Sets.Empty_Set);
      Criteria.Rankings   := Self.Show_Ranking;
      Criteria.Lineages   := CodePeer.To_Lifeage_Kinds_Flags
        (Self.Lifeage_Editor.Get_Visible_Items);
      Criteria.Statuses   := Self.Show_Status;
   end Update_Criteria;

end CodePeer.Messages_Reports;
