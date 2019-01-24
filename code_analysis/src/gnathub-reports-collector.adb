------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Glib.Object;                use Glib.Object;
with Gtk.Box;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Paned;                  use Gtk.Paned;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Tree_Model;             use Gtk.Tree_Model;

with Commands.Interactive;       use Commands.Interactive;
with Default_Preferences;        use Default_Preferences;
with Dialog_Utils;               use Dialog_Utils;
with Generic_Views;
with GNAThub.Metrics;            use GNAThub.Metrics;
with GNAThub.Reports.Messages;
with GNAThub.Reports.Metrics;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with Gtkada.MDI;                 use Gtkada.MDI;
with GUI_Utils;                  use GUI_Utils;

package body GNAThub.Reports.Collector is

   Me : constant Trace_Handle := Create ("GNATHUB.REPORTS.COLLECTOR");

   GNAThub_Module : GNAThub.Module.GNAThub_Module_Id;

   ------------------------------
   -- GNAThub_Report_Collector --
   ------------------------------

   type GNAThub_Report_Collector is
     new Generic_Views.View_Record and Metrics_Listener_Interface with record
      Messages_Report : GNAThub.Reports.Messages.GNAThub_Report_Messages;
      Metric_Report   : GNAThub.Reports.Metrics.GNAThub_Report_Metrics;
      Metrics_View    : Dialog_View;
      Help_Label      : Gtk_Label;
   end record;
   type Report is access all GNAThub_Report_Collector'Class;

   function Initialize
     (Self : access GNAThub_Report_Collector'Class)
      return Gtk.Widget.Gtk_Widget;

   overriding procedure Create_Menu
     (Self : not null access GNAThub_Report_Collector;
      Menu : not null access Gtk_Menu_Record'Class);

   overriding procedure Metric_Added
     (Self   : not null access GNAThub_Report_Collector;
      Metric : not null access Metric_Record'Class);

   package GNAThub_Report_Collector_Views is new Generic_Views.Simple_Views
     (Module_Name        => "analysis_report",
      View_Name          => "Analysis Report",
      Formal_View_Record => GNAThub_Report_Collector,
      Formal_MDI_Child   => GPS.Kernel.MDI.GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Central_Only,
      Position           => Gtkada.MDI.Position_Right,
      Initialize         => Initialize);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Report;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Trigger On_Selection_Changed if a preference affect the selection

   type Expand_Or_Collapse_Command (Command : Expansion_Command_Type) is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Expand_Or_Collapse_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Command used to expand/collapse the rows selected in the messages
   --  report.

   procedure On_Selection_Changed (Self : access GObject_Record'Class);
   --  Called when the selection changes in the tree

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access GNAThub_Report_Collector;
      Menu : not null access Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      GPS.Kernel.Preferences.Append_Menu
        (Menu, GNAThub_Module.Kernel,
         GNAThub.Module.Hide_Node_Without_Messages);
   end Create_Menu;

   ------------------------
   -- Get_Or_Create_View --
   ------------------------

   function Get_Or_Create_View
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Module  : not null access GNAThub.Module.GNAThub_Module_Id_Record'Class;
      Created : out Boolean)
      return Gtk.Widget.Gtk_Widget
   is
      use GNAThub_Report_Collector_Views;

      View : GNAThub_Report_Collector_Views.View_Access :=
        GNAThub_Report_Collector_Views.Retrieve_View (Kernel);
   begin
      GNAThub_Module := GNAThub.Module.GNAThub_Module_Id (Module);

      if View = null then
         Created := True;
         View := GNAThub_Report_Collector_Views.Get_Or_Create_View (Kernel);
      else
         declare
            Child : constant MDI_Child :=
              GNAThub_Report_Collector_Views.Child_From_View (View);
         begin
            --  Focus the view's MDI child when it's already visible.
            Child.Raise_Child (Give_Focus => True);
            Set_Focus_Child (Child);
            Created := False;
         end;
      end if;

      return Gtk.Widget.Gtk_Widget (View);
   end Get_Or_Create_View;

   ----------------
   -- Close_View --
   ----------------

   procedure Close_View
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      GNAThub_Report_Collector_Views.Close (Kernel);
   end Close_View;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use GNAThub_Report_Collector_Views;

      View : constant GNAThub_Report_Collector_Views.View_Access :=
        GNAThub_Report_Collector_Views.Retrieve_View
          (Kernel       => Kernel,
           Visible_Only => True);
   begin
      if View /= null then
         --  Clear both messages and metrics report

         View.Messages_Report.Clear;
         View.Metric_Report.Clear;

         --  Hid the metrics report and register ourself again as a metrics
         --  listener in order to show the metrics view if a metric is added
         --  in the future.

         View.Metrics_View.Set_No_Show_All (True);
         View.Metrics_View.Hide;
         GNAThub.Metrics.Register_Listener (View);
      end if;
   end Clear;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access GNAThub_Report_Collector'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Hook         : access On_Pref_Changed;
      Paned    : Gtk_Paned;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Trace (Me, "Creating the GNAThub Analysis Report");

      Gtk.Box.Initialize_Vbox (Self);

      Gtk_New_Vpaned (Paned);
      Self.Pack_Start (Paned);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      GNAThub.Reports.Messages.Gtk_New
        (Self.Messages_Report,
         Kernel     => Self.Kernel,
         Severities => GNAThub_Module.Severities);
      Self.Messages_Report.Set_Name ("messages-report");
      Scrolled.Add (Self.Messages_Report);
      Paned.Pack1 (Scrolled, True, True);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      Self.Metrics_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Self.Metrics_View);
      Paned.Pack2 (Self.Metrics_View, True, True);

      GNAThub.Reports.Metrics.Gtk_New (Self.Metric_Report);
      Self.Metric_Report.Set_Name ("metrics-report");
      Self.Metrics_View.Append (Self.Metric_Report, Expand => False);

      Self.Messages_Report.Get_Selection.On_Changed
        (On_Selection_Changed'Access, Self);

      Hook := new On_Pref_Changed;
      Hook.View := Report (Self);
      Preferences_Changed_Hook.Add (Hook, Watch => Self);

      Gtk_New
        (Self.Help_Label,
         "Click on the tree view's nodes to see associated metrics.");
      Get_Style_Context (Self.Help_Label).Add_Class ("help-label");

      GNAThub.Metrics.Register_Listener (Self);

      --  Don't show the metrics view on Show_All: only show it if metrics
      --  need to be displayed.

      Self.Metrics_View.Set_No_Show_All (True);

      Self.Metrics_View.Append
        (Self.Help_Label,
         Add_Separator => False);

      return Gtk.Widget.Gtk_Widget (Self);
   end Initialize;

   ------------------
   -- Metric_Added --
   ------------------

   overriding procedure Metric_Added
     (Self   : not null access GNAThub_Report_Collector;
      Metric : not null access Metric_Record'Class) is
      pragma Unreferenced (Metric);
   begin
      Self.Metrics_View.Set_No_Show_All (False);
      Self.Metrics_View.Show_All;

      GNAThub.Metrics.Unregister_Listener (Self);
   end Metric_Added;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Kernel);
   begin
      if Pref = null
        or else Pref = Preference (GNAThub.Module.Hide_Node_Without_Messages)
      then
         On_Selection_Changed (Self.View);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Expand_Or_Collapse_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      View : constant GNAThub_Report_Collector_Views.View_Access :=
        GNAThub_Report_Collector_Views.Retrieve_View
          (Get_Kernel (Context.Context));
   begin
      Expand_Or_Collapse_Selected_Rows
        (Tree    => View.Messages_Report,
         Command => Self.Command);

      return Commands.Success;
   end Execute;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (Self : access GObject_Record'Class)
   is
      View        : constant Report := Report (Self);
      Model       : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter;
      Sort_Iter   : Gtk_Tree_Iter;
   begin
      View.Help_Label.Hide;

      View.Messages_Report.Get_First_Selected (Model, Sort_Iter);

      Iter := View.Messages_Report.Convert_To_Store_Iter (Sort_Iter);

      View.Metric_Report.Show_Metrics (View.Messages_Report.Get_ID (Iter));
      View.Messages_Report.Show_Messages (View.Messages_Report.Get_ID (Iter));
   end On_Selection_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Action
        (Kernel, "gnathub report expand rows",
         Command     => new Expand_Or_Collapse_Command (Expand_Rows),
         Category    => "Analyze",
         Icon_Name   => "gps-expand-all-symbolic",
         Description => "Expand the rows selected in the Analysis Report.");

      Register_Action
        (Kernel, "gnathub report collapse rows",
         Command     => new Expand_Or_Collapse_Command (Collapse_Rows),
         Category    => "Analyze",
         Icon_Name   => "gps-collapse-all-symbolic",
         Description => "Collapse the rows selected in the Analysis Report.");
   end Register_Module;

end GNAThub.Reports.Collector;
