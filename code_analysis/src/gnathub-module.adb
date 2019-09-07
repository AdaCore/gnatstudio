------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

with Glib.Object;
with Gtk.Widget;

with GPS.Default_Styles;           use GPS.Default_Styles;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;
with GPS.Kernel.Preferences;

with GNAThub.Actions;
with GNAThub.Filters_Views;
with GNAThub.Loader.External;
with GNAThub.Loader.Databases;
with GNAThub.Messages;
with GNAThub.Metrics;
with GNAThub.Reports.Collector;

package body GNAThub.Module is

   Me : constant Trace_Handle := Create ("GNATHUB");

   type Loader_Listener_Type is
     new GNAThub.Loader.Loader_Listener_Interface with record
      Loaders_Finished_Count : Natural := 0;
   end record;
   --  A listener used to react when a loader starts/finishes loading.
   --
   --  The Filters and Analysys Report views should be opened when a loader
   --  actually starts loading data.
   --
   --  The Analysis_Loading_Finished_Hook should be run when both loaders have
   --  finished loading their data.

   overriding procedure On_Finish_Loading
     (Self : not null access Loader_Listener_Type);

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
     (View   : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when report closing

   -----------
   -- Clean --
   -----------

   procedure Clean (Self : in out GNAThub_Module_Id_Record'Class) is
   begin
      Trace (Me, "Cleaning the GNAThub module");

      --  Close the views

      GNAThub.Reports.Collector.Clear (Self.Kernel);
      GNAThub.Filters_Views.Close_View (Self.Kernel);

      --  Clear the filters

      Self.Filter.Clear;

      --  Clear the loaders

      Self.Ext_Loader.Cleanup;
      Self.Db_Loader.Cleanup;

      --  Reset counters

      for Rule of Self.Rules loop
         Rule.Reset_Counters;
      end loop;

      for Severity of Self.Severities loop
         Severity.Reset_Counters;
      end loop;

      for Tool of Self.Tools loop
         Tool.Reset_Counters;
      end loop;

      --  Remove the loaded messages

      Self.Db_Loader.Remove_Messages;
      Self.Ext_Loader.Remove_Messages;
   end Clean;

   ---------------------
   -- Remove_Database --
   ---------------------

   procedure Remove_Database
     (Self : in out GNAThub_Module_Id_Record'Class) is
   begin
      Self.Db_Loader.Cleanup;
      Self.Db_Loader.Remove_Database;
      Self.Db_Loader.Remove_Messages;
   end Remove_Database;

   ------------------
   -- Display_Data --
   ------------------

   procedure Display_Data (Self : in out GNAThub_Module_Id_Record'Class)
   is
      Ext_Loader_Has_Data : Boolean;
      Db_Loader_Has_Data  : Boolean;
   begin
      Self.Clean;

      Self.Ext_Loader.Prepare_Loading;
      Self.Db_Loader.Prepare_Loading;

      Ext_Loader_Has_Data := Self.Ext_Loader.Has_Data_To_Load;
      Db_Loader_Has_Data := Self.Db_Loader.Has_Data_To_Load;

      --  Switch to the 'Analyze' perspective and display the Analysis Report
      --  and the Filters view if there is some data to load in one of the
      --  loaders.

      if Db_Loader_Has_Data or else Ext_Loader_Has_Data then
         declare
            Report_View         : Gtk.Widget.Gtk_Widget;
            Report_View_Created : Boolean;
         begin
            Trace (Me, "Starting loading the data: open the views");

            --  Switch to GNATHub perspective.
            Load_Perspective (Module.Kernel, "Analyze");

            Self.Kernel.Get_Messages_Container.Register_Filter
              (GPS.Kernel.Messages.Message_Filter_Access (Module.Filter));
            GNAThub.Filters_Views.Open_View (Module.Kernel, Module);

            Report_View :=
              GNAThub.Reports.Collector.Get_Or_Create_View
                (Module.Kernel,
                 Module  => Module,
                 Created => Report_View_Created);

            if Report_View_Created then
               GPS.Kernel.Hooks.Before_Exit_Action_Hook.Add
                 (new On_Before_Exit, Watch => Report_View);

               Kernel_Callback.Connect
                 (Report_View,
                  Gtk.Widget.Signal_Destroy,
                  On_Report_Destroy'Access,
                  User_Data => Module.Kernel);
            end if;
         end;
      elsif not Db_Loader_Has_Data then
         Self.Get_Kernel.Insert
           ("Could not display the Analysis Report: "
            & "GNAThub database not found.",
            Mode => GPS.Kernel.Error);
      end if;

      --  Start loading the data

      Self.Ext_Loader.Load;
      Self.Db_Loader.Load;
   end Display_Data;

   -----------------------
   -- On_Finish_Loading --
   -----------------------

   overriding procedure On_Finish_Loading
     (Self : not null access Loader_Listener_Type) is
   begin
      Self.Loaders_Finished_Count := Self.Loaders_Finished_Count + 1;

      if Self.Loaders_Finished_Count = 2 then
         Trace (Me, "Finished loading all the data");

         Analysis_Loading_Finsished_Hook.Run (Module.Kernel);

         Self.Loaders_Finished_Count := 0;
      end if;
   end On_Finish_Loading;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      Module.Clean;
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
          (Current    => 0,
           Total      => 0,
           Name       => Name,
           Identifier => Identifier,
           Tool       => Tool)
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
        new Tool_Record'
          (Current => 0,
           Total   => 0,
           Name    => Name,
           Rules   => <>)
      do
         Self.Tools.Insert (Tool);
      end return;
   end Get_Or_Create_Tool;

   -----------------------
   -- On_Report_Destroy --
   -----------------------

   procedure On_Report_Destroy
     (View   : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (View);
   begin
      if not Kernel.Is_In_Destruction then
         --  Restore default perspective
         GPS.Kernel.MDI.Load_Perspective (Module.Kernel, "Default");
         Kernel.Get_Messages_Container.Unregister_Filter
           (GPS.Kernel.Messages.Message_Filter_Access (Module.Filter));
      end if;
   end On_Report_Destroy;

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
      begin
         --  Load the severities
         for Ranking in Message_Importance_Type loop
            Module.Severities.Insert
              (new Severity_Record'
                 (Current => 0,
                  Total   => 0,
                  Ranking => Ranking,
                  Style   => Messages_Styles (Ranking)));
         end loop;
      end Load_Severities;

   begin

      Module        := new GNAThub_Module_Id_Record;
      Module.Kernel := GPS.Kernel.Kernel_Handle (Kernel);

      Module.Register_Module (Kernel, "GNAThub");
      GNAThub.Actions.Register_Actions (Module);

      Module.Db_Loader := new GNAThub.Loader.Databases.Database_Loader_Type;
      Module.Db_Loader.Initialize (Module);

      Module.Ext_Loader := new GNAThub.Loader.External.External_Loader_Type;
      Module.Ext_Loader.Initialize (Module);

      Module.Loaders_Listener := new Loader_Listener_Type;

      Module.Ext_Loader.Register_Listener (Module.Loaders_Listener);
      Module.Db_Loader.Register_Listener (Module.Loaders_Listener);

      Load_Severities;

      Module.Filter := new GNAThub.Filters.Message_Filter;

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

      GNAThub.Messages.Register_Module (Kernel, Module);
      GNAThub.Filters_Views.Register_Module (Kernel);
      GNAThub.Metrics.Register_Module (Kernel);
      GNAThub.Reports.Collector.Register_Module (Kernel);
   end Register_Module;

end GNAThub.Module;
