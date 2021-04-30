------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2021, AdaCore                     --
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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with GNATCOLL.Traces;          use GNATCOLL.Traces;

with Glib.Object;              use Glib.Object;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Widget;
with Gtkada.MDI;               use Gtkada.MDI;

with Commands.Interactive;     use Commands.Interactive;
with Default_Preferences;      use Default_Preferences;
with GNAThub.Reports.Messages; use GNAThub.Reports.Messages;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GUI_Utils;                use GUI_Utils;

package body GNAThub.Reports.Collector is

   Me : constant Trace_Handle := Create ("GNATHUB.REPORTS.COLLECTOR");

   GNAThub_Module : GNAThub.Module.GNAThub_Module_Id;

   ------------------------------
   -- GNAThub_Report_Collector --
   ------------------------------

   function Initialize
     (Self : access GNAThub_Report_Collector'Class)
      return Gtk.Widget.Gtk_Widget;

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

   type Expand_Or_Collapse_Command (Command : Expansion_Command_Type) is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Expand_Or_Collapse_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Command used to expand/collapse the rows selected in the messages
   --  report.

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
         GNAThub.Module.Hide_Others_Node);
      GPS.Kernel.Preferences.Append_Menu
        (Menu, GNAThub_Module.Kernel,
         GNAThub.Module.Auto_Location_Filtering);
   end Create_Menu;

   ------------------------
   -- Get_Or_Create_View --
   ------------------------

   function Get_Or_Create_View
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Module  : not null access GNAThub.Module.GNAThub_Module_Id_Record'Class;
      Created : out Boolean)
      return GNAThub_Report_Collector_Access
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

      return GNAThub_Report_Collector_Access (View);
   end Get_Or_Create_View;

   ----------------
   -- Close_View --
   ----------------

   procedure Close_View
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      GNAThub_Report_Collector_Views.Close (Kernel);
   end Close_View;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access GNAThub_Report_Collector'Class)
      return Gtk.Widget.Gtk_Widget
   is
   begin
      Trace (Me, "Creating the GNAThub Analysis Report");

      Gtk.Box.Initialize_Vbox (Self);

      Gtk_New (Self.Scrolled);
      Self.Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      GNAThub.Reports.Messages.Gtk_New
        (Self.Messages_Report,
         Kernel     => Self.Kernel,
         Severities => GNAThub_Module.Severities,
         Metrics    => GNAThub_Module.Metrics);
      Self.Messages_Report.Set_Name ("messages-report");
      Self.Scrolled.Add (Self.Messages_Report);
      Self.Pack_Start (Self.Scrolled);

      return Gtk.Widget.Gtk_Widget (Self);
   end Initialize;

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
      GNAThub.Reports.Messages.Expand_Or_Collapse_Selected_Rows
        (Self    => View.Messages_Report,
         Command => Self.Command);

      return Commands.Success;
   end Execute;

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
