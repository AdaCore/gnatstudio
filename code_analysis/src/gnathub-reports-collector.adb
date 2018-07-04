------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

with Default_Preferences;        use Default_Preferences;

with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Paned;                  use Gtk.Paned;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Widget;
with Gtkada.Handlers;

with GNAThub.Messages;           use GNAThub.Messages;
with GNAThub.Module;
with GNAThub.Reports.Models;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;
with Glib.Object;                use Glib.Object;

package body GNAThub.Reports.Collector is

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Listener'Class, Message_Listener_Access);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Report;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Trigger On_Selection_Changed if a preference affect the selection

   procedure On_Destroy (View : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Selection_Changed (Self : access GObject_Record'Class);
   --  Called when the selection changes in the tree

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget     : out Report;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set) is
   begin
      Widget := new GNAThub_Report_Collector;
      Initialize (Widget, Kernel, Tree, Severities);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access GNAThub_Report_Collector'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set)
   is
      Hook  : access On_Pref_Changed;
      Paned : Gtk_Paned;
   begin
      Gtk.Box.Initialize_Vbox (Self);
      Self.Kernel := Kernel;

      Gtk_New_Vpaned (Paned);
      Self.Pack_Start (Paned);

      GNAThub.Reports.Messages.Gtk_New
        (Self.Messages_Report, Kernel, Tree, Severities);
      Paned.Pack1 (Self.Messages_Report, True, True);

      GNAThub.Reports.Metrics.Gtk_New (Self.Metric_Report);
      Paned.Pack2 (Self.Metric_Report, True, True);

      Self.Listener := new Message_Listener (Gtk.Box.Gtk_Vbox (Self));

      GPS.Kernel.Messages.Register_Listener
        (Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Listener),
         GPS.Kernel.Messages.Locations_Only);

      Self.Messages_Report.Get_Tree.Get_Selection.On_Changed
           (On_Selection_Changed'Access, Self);
      Gtkada.Handlers.Widget_Callback.Connect
        (Self, Gtk.Widget.Signal_Destroy, On_Destroy'Access);

      Hook := new On_Pref_Changed;
      Hook.View := Report (Self.Listener.View);
      Preferences_Changed_Hook.Add (Hook, Watch => Self);
   end Initialize;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
   is
      View : constant Report := Report (Self.View);
   begin
      if Message.all in GNAThub_Message'Class then
         View.Update;
      end if;
   end Message_Added;

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

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Self : constant Report := Report (View);
   begin
      GPS.Kernel.Messages.Unregister_Listener
        (Self.Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Listener));

      Free (Self.Listener);
   end On_Destroy;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (Self : access GObject_Record'Class)
   is
      View            : constant Report                                :=
        Report (Self);
      Tree            : constant Gtk.Tree_View.Gtk_Tree_View           :=
        View.Messages_Report.Get_Tree;
      Analysis_Model  : constant GNAThub.Reports.Models.Messages_Model :=
        View.Messages_Report.Get_Analysis_Model;
      Model           : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter            : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sort_Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Project_Node    : GNAThub_Project_Access;
      File_Node       : GNAThub_File_Access;
      Subprogram_Node : GNAThub_Subprogram_Access;
   begin
      Tree.Get_Selection.Get_Selected (Model, Sort_Iter);

      if Sort_Iter = Null_Iter then
         GNAThub.Reports.Metrics.Clear (View.Metric_Report);
         return;
      end if;

      View.Messages_Report.Get_Sort_Model.Convert_Iter_To_Child_Iter
        (Iter, Sort_Iter);

      Subprogram_Node :=
        GNAThub_Subprogram_Access (Analysis_Model.Subprogram_At (Iter));
      if Subprogram_Node /= null then
         GNAThub.Reports.Metrics.Display_Metrics_Report
           (View.Metric_Report, Subprogram_Node.Metrics);
      else
         File_Node := GNAThub_File_Access (Analysis_Model.File_At (Iter));
         if File_Node  /= null then
            GNAThub.Reports.Metrics.Display_Metrics_Report
              (View.Metric_Report, File_Node.Metrics);
         else
            Project_Node :=
              GNAThub_Project_Access (Analysis_Model.Project_At (Iter));
            if Project_Node /= null then
               GNAThub.Reports.Metrics.Display_Metrics_Report
                 (View.Metric_Report, Project_Node.Metrics);
            else
               GNAThub.Reports.Metrics.Clear (View.Metric_Report);
            end if;
         end if;
      end if;
   end On_Selection_Changed;

   ------------
   -- Update --
   ------------

   procedure Update (Self : not null access GNAThub_Report_Collector'Class) is
   begin
      Self.Messages_Report.Update;
   end Update;

end GNAThub.Reports.Collector;
