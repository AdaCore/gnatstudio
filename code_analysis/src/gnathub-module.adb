------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with GNATCOLL.Traces;              use GNATCOLL.Traces;

with Gtk.Widget;
with Gtkada.Handlers;
with Gtkada.MDI;

with GPS.Default_Styles;           use GPS.Default_Styles;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Hooks;
with GPS.Kernel.Messages;
with GPS.Kernel.Preferences;

with GNAThub.Actions;
with GNAThub.Filters_Views;
with GNAThub.Loader.External;
with GNAThub.Loader.Databases;

package body GNAThub.Module is

   Me : constant Trace_Handle := Create ("GNATHUB");

   type On_Before_Exit is
     new GPS.Kernel.Hooks.Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean;
   --  Called before GPS exits. Switchs perspective to default.

   type On_Project_Changed is
     new GPS.Kernel.Hooks.Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Called when project view is changed. Close report and clean all data.

   procedure On_Report_Destroy
     (View : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when report closing

   -----------
   -- Clean --
   -----------

   procedure Clean (Self : in out GNAThub_Module_Id_Record'Class) is
   begin
      if Self.Report /= null then
         Trace (Me, "Destroying the Analysis report");
         Self.Report.Destroy;
      end if;

      GNAThub.Filters_Views.Close_View (Self.Kernel);

      Self.Filter.Clear;

      --  Reset rules counters
      for Rule of Self.Rules loop
         Rule.Count.Clear;
      end loop;

      Clear_Code_Analysis (Self.Tree);

      Self.Db_Loader.Remove_Messages;
      Self.Ext_Loader.Remove_Messages;
   end Clean;

   ------------------
   -- Display_Data --
   ------------------

   procedure Display_Data (Self : in out GNAThub_Module_Id_Record'Class)
   is
      Has_Data : Boolean;
   begin
      Self.Clean;

      --  Try to load the data collected by the external loader first.
      Has_Data := Self.Ext_Loader.Load;

      --  If there is no data to load in the external loader, try to load the
      --  data from the database loader.
      if not Has_Data then
         Has_Data := Self.Db_Loader.Load;
      end if;

      if Has_Data then
         Trace (Me, "Displaying the Analysis report");

         --  Switch to GNATHub perspective.
         Load_Perspective (Self.Kernel, "Analyze");

         GNAThub.Filters_Views.Open_View (Self.Kernel, Module);

         GNAThub.Reports.Collector.Gtk_New
           (Self.Collector, Self.Kernel, Self.Tree, Self.Severities);

         Self.Report := new GNAThub_Child_Record;
         GPS.Kernel.MDI.Initialize
           (Self.Report, Self.Collector, Self.Kernel, Module => Module);
         Self.Report.Set_Title (-"Analysis Report");

         GPS.Kernel.Hooks.Before_Exit_Action_Hook.Add
           (new On_Before_Exit, Watch => Self.Collector);

         Gtkada.Handlers.Widget_Callback.Connect
           (Self.Report, Gtk.Widget.Signal_Destroy, On_Report_Destroy'Access);

         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Self.Report);
         Self.Report.Raise_Child;

      else
         Self.Get_Kernel.Insert
           ("No analysis data available.");
      end if;
   end Display_Data;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      Module.Clean_Messages := True;
      Module.Clean;
      Module.Clean_Messages := False;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      --  Store GNATHub perspective and prevent use it
      --  as default on next startup
      if GPS.Kernel.Preferences.Save_Desktop_On_Exit.Get_Pref then
         GPS.Kernel.MDI.Save_Desktop (Kernel, "Default");
      end if;

      Module.Clean;

      return True;
   end Execute;

   ------------------------
   -- Get_Or_Create_Rule --
   ------------------------

   function Get_Or_Create_Rule
     (Self       : in out GNAThub_Module_Id_Record'Class;
      Tool       : not null Tool_Access;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String)
      return Rule_Access is
   begin
      for Rule of Self.Rules loop
         if Rule.Identifier = Identifier then
            return Rule;
         end if;
      end loop;

      return Rule : constant Rule_Access :=
        new Rule_Record'
          (Name       => Name,
           Identifier => Identifier,
           Tool       => Tool,
           Count      => <>)
      do
         Tool.Rules.Insert (Rule);
         Self.Rules.Insert (Rule);
      end return;
   end Get_Or_Create_Rule;

   ------------------
   -- Get_Severity --
   ------------------

   function Get_Severity
     (Self    : GNAThub_Module_Id_Record'Class;
      Ranking : Message_Importance_Type)
      return Severity_Access is
   begin
      for Severity of Self.Severities loop
         if Severity.Ranking = Ranking then
            return Severity;
         end if;
      end loop;

      return null;
   end Get_Severity;

   ------------------------
   -- Get_Or_Create_Tool --
   ------------------------

   function Get_Or_Create_Tool
     (Self : in out GNAThub_Module_Id_Record'Class;
      Name : Ada.Strings.Unbounded.Unbounded_String) return Tool_Access is
   begin
      for Tool of Self.Tools loop
         if Tool.Name = Name then
            return Tool;
         end if;
      end loop;

      return Tool : constant Tool_Access :=
        new Tool_Record'(Name => Name, Rules => <>)
      do
         Self.Tools.Insert (Tool);
      end return;
   end Get_Or_Create_Tool;

   -----------------------
   -- On_Report_Destroy --
   -----------------------

   procedure On_Report_Destroy
     (View : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (View);
   begin
      Module.Clean_Messages := True;
      --  Restore default perspective
      GPS.Kernel.MDI.Load_Perspective (Module.Kernel, "Default");
      Module.Report := null;
   end On_Report_Destroy;

   -------------------
   -- Update_Report --
   -------------------

   procedure Update_Report (Self : in out GNAThub_Module_Id_Record'Class) is
      use type Gtkada.MDI.MDI_Child;

      MDI : Gtkada.MDI.MDI_Child;
   begin
      MDI := GPS.Kernel.MDI.Get_MDI
        (Self.Kernel).Find_MDI_Child_By_Tag
        (GNAThub.Reports.Collector.GNAThub_Report_Collector'Tag);

      if MDI /= null then
         GNAThub.Reports.Collector.Report (MDI.Get_Widget).Update;
      end if;
   end Update_Report;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      procedure Load_Severities;

      ---------------------
      -- Load_Severities --
      ---------------------

      procedure Load_Severities is
         Id : Natural := 1;
      begin
         --  Load the severities
         for Ranking in Message_Importance_Type loop
            Module.Severities.Insert
              (new Severity_Record'
                 (Ranking => Ranking,
                  Style   => Messages_Styles (Ranking)));
         end loop;

         --  Associate them with unique IDs
         for Severity of Module.Severities loop
            Module.Severities_Id.Insert (Severity, Id);
            Id := Id + 1;
         end loop;
      end Load_Severities;

   begin

      Module        := new GNAThub_Module_Id_Record;
      Module.Kernel := GPS.Kernel.Kernel_Handle (Kernel);
      Module.Tree   := new Project_Maps.Map;

      Module.Register_Module (Kernel, "GNAThub");
      GNAThub.Actions.Register_Actions (Module);

      Module.Db_Loader := new GNAThub.Loader.Databases.Database_Loader_Type;
      Module.Db_Loader.Initialize (Module);

      Module.Ext_Loader := new GNAThub.Loader.External.External_Loader_Type;
      Module.Ext_Loader.Initialize (Module);

      Load_Severities;

      Module.Filter := new GNAThub.Filters.Message_Filter;

      Kernel.Get_Messages_Container.Register_Filter
        (GPS.Kernel.Messages.Message_Filter_Access (Module.Filter));
      GPS.Kernel.Hooks.Project_Changed_Hook.Add (new On_Project_Changed);

      Hide_Node_Without_Messages :=
        Kernel.Get_Preferences.Create
          (Path    => ":Local Configuration",
           Name    => "hide_node_without_messages",
           Default => False,
           Label   => -"Hide nodes without messages.",
           Doc     => -"Hide the nodes with metrics but no messages.");

      Always_Display_The_Rules :=
        Kernel.Get_Preferences.Create
          (Path    => ":Local Configuration",
           Name    => "always_display_the_rules",
           Default => False,
           Label   => -"Always display rules",
           Doc     =>
             -"If enabled, the rules without messages will be displayed.");
      GNAThub.Filters_Views.Register_Module (Kernel);
   end Register_Module;

end GNAThub.Module;
