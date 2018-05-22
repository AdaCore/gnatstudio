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
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;

with Gtk.Widget;
with Gtkada.Handlers;
with Gtkada.MDI;

with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Hooks;
with GPS.Kernel.Messages;
with GPS.Kernel.Preferences;

with GNAThub.Actions;
with GNAThub.Filters_Views;
with GNAThub.Loader;
with GNAThub.Messages;

package body GNAThub.Module is

   Module : GNAThub_Module_Id;

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

      generic
         type Element is limited private;
         type Element_Access is access all Element;
         with package Sets is new Ada.Containers.Ordered_Sets
           (Element_Access, others => <>);
      procedure Clean_Set (Set : in out Sets.Set);

      ---------------
      -- Clean_Set --
      ---------------

      procedure Clean_Set (Set : in out Sets.Set) is
         procedure Free is new Ada.Unchecked_Deallocation
           (Element, Element_Access);
      begin
         while not Set.Is_Empty loop
            declare
               Item : Element_Access := Set.First_Element;
            begin
               Set.Delete_First;
               Free (Item);
            end;
         end loop;
      end Clean_Set;

      procedure Clean_Severity_Set is new Clean_Set
        (Severity_Record, Severity_Access, Severities_Ordered_Sets);

      procedure Clean_Tool_Set is new Clean_Set
        (Tool_Record, Tool_Access, Tools_Ordered_Sets);

      procedure Clean_Rule_Set is new Clean_Set
        (Rule_Record, Rule_Access, Rule_Sets);

   begin
      if Self.Report /= null then
         Self.Report.Destroy;
      end if;

      GNAThub.Filters_Views.Close_View (Self.Kernel);

      Self.Filter.Clear;

      Clean_Severity_Set (Self.Severities);
      Clean_Tool_Set (Self.Tools);
      Clean_Rule_Set (Self.Rules);
      Self.Severities_Id.Clear;

      GPS.Kernel.Messages.Remove_Category
        (Self.Kernel.Get_Messages_Container,
         GNAThub.Messages.Category,
         GPS.Kernel.Messages.Empty_Message_Flags);
   end Clean;

   ------------------
   -- Display_Data --
   ------------------

   procedure Display_Data (Self : in out GNAThub_Module_Id_Record'Class)
   is
      Database : constant GNATCOLL.VFS.Virtual_File :=
                   Self.Get_Kernel.Get_Project_Tree.Root_Project.Object_Dir
                     .Create_From_Dir ("gnathub")
                     .Create_From_Dir ("gnathub.db");

      Ignore : Boolean;
   begin
      Self.Clean;

      if Database.Is_Regular_File then
         Self.Loader.Load (Database);

         --  Switch to GNATHub perspective.
         Load_Perspective (Self.Kernel, "Analyze");

         GNAThub.Filters_Views.Open_View (Self.Kernel, Module);

         GNAThub.Reports.Collector.Gtk_New
           (Self.Collector, Self.Kernel, Self.Tree, Self.Severities);

         Self.Report := new GNAThub_Child_Record;
         GPS.Kernel.MDI.Initialize
           (Self.Report, Self.Collector, Self.Kernel, Module => Module);
         Self.Report.Set_Title (-"GNAThub report");

         GPS.Kernel.Hooks.Before_Exit_Action_Hook.Add
           (new On_Before_Exit, Watch => Self.Collector);

         Gtkada.Handlers.Widget_Callback.Connect
           (Self.Report, Gtk.Widget.Signal_Destroy, On_Report_Destroy'Access);

         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Self.Report);
         Self.Report.Raise_Child;

      else
         Self.Get_Kernel.Insert
           (Database.Display_Full_Name &
            (-" does not exist. Analysis information is absent."),
            Mode => GPS.Kernel.Error);
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

   --------------
   -- New_Rule --
   --------------

   function New_Rule
     (Self       : in out GNAThub_Module_Id_Record'Class;
      Tool       : not null Tool_Access;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String)
      return Rule_Access is
   begin
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
   end New_Rule;

   ------------------
   -- New_Severity --
   ------------------

   function New_Severity
     (Self    : in out GNAThub_Module_Id_Record'Class;
      Ranking : Analysis_Message_Category)
      return Severity_Access is
   begin
      return Sev : constant Severity_Access :=
        new Severity_Record'
          (Ranking    => Ranking,
           Style      => Analysis_Styles (Ranking))
      do
         Self.Severities.Insert (Sev);
      end return;
   end New_Severity;

   --------------
   -- New_Tool --
   --------------

   function New_Tool
     (Self : in out GNAThub_Module_Id_Record'Class;
      Name : Ada.Strings.Unbounded.Unbounded_String) return Tool_Access is
   begin
      return Tool : constant Tool_Access :=
        new Tool_Record'(Name => Name, Rules => <>)
      do
         Self.Tools.Insert (Tool);
      end return;
   end New_Tool;

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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Module        := new GNAThub_Module_Id_Record;
      Module.Kernel := GPS.Kernel.Kernel_Handle (Kernel);
      Module.Tree   := new Project_Maps.Map;

      Module.Register_Module (Kernel, "GNAThub");
      GNAThub.Actions.Register_Actions (Module);
      Module.Loader := new GNAThub.Loader.Loader (Module);
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
           Doc     => "");

      GNAThub.Filters_Views.Register_Module (Kernel);
   end Register_Module;

end GNAThub.Module;
