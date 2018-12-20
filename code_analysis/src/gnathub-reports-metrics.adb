------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Float_Text_IO;
with GNATCOLL.Utils;
with GNATCOLL.Traces;        use GNATCOLL.Traces;

with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Glib;                   use Glib;
with Glib_Values_Utils;      use Glib_Values_Utils;

with String_Utils;           use String_Utils;
with GUI_Utils;              use GUI_Utils;
with Gtk.Widget;

package body GNAThub.Reports.Metrics is

   Me : constant Trace_Handle := Create ("GNATHUB.REPORTS.METRICS");

   procedure Add_Metric
     (Self   : not null access GNAThub_Report_Metrics_Record'Class;
      Metric : not null access Metric_Record'Class);
   --  Add the given metric to the metrics' report.

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Used to sort the metrics' report columns.

   procedure On_Destroy
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when the metrics report is destroyed.
   --  Unregister it from the metrics' listeners.

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      S_A : constant String := Get_String (Model, A, 1);
      S_B : constant String := Get_String (Model, B, 1);
   begin
      if S_A = "" then
         return 1;
      elsif S_B = "" then
         return -1;
      end if;

      declare
         Val_A : constant Float := Float'Value (S_A);
         Val_B : constant Float := Float'Value (S_B);
      begin
         if Val_A < Val_B then
            return 1;
         else
            return -1;
         end if;
      end;
   end Sort_Func;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out GNAThub_Report_Metrics) is
   begin
      Widget := new GNAThub_Report_Metrics_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access GNAThub_Report_Metrics_Record'Class)
   is
      Dummy    : Gint;
      pragma Unreferenced (Dummy);

      function New_Column
        (Name : String; Column : Gint) return Gtk_Tree_View_Column;

      ----------------
      -- New_Column --
      ----------------

      function New_Column
        (Name : String; Column : Gint) return Gtk_Tree_View_Column
      is
         Col  : Gtk_Tree_View_Column;
         Text : Gtk_Cell_Renderer_Text;
      begin
         Gtk_New (Col);
         Col.Set_Resizable (True);
         Col.Set_Sort_Column_Id (Column);
         Col.Set_Title (Name);
         Gtk_New (Text);
         Col.Pack_Start (Text, True);
         Col.Add_Attribute (Text, "text", Column);
         return Col;
      end New_Column;
   begin
      Trace (Me, "Creating the GNAThub Metrics report");

      Gtkada.Tree_View.Initialize
        (Self,
         Column_Types    => (0 => GType_String,    -- name of the metric
                             1 => GType_String));  -- value of the metric

      Dummy := Self.Append_Column (New_Column ("Metric", 0));
      Dummy := Self.Append_Column (New_Column ("Value", 1));
      Self.Set_Search_Column (0);
      Set_Sort_Func (Self.Model, 1, Sort_Func'Access);

      Self.On_Destroy (On_Destroy'Access);

      GNAThub.Metrics.Register_Listener (Self);
   end Initialize;

   ----------------
   -- Add_Metric --
   ----------------

   procedure Add_Metric
     (Self   : not null access GNAThub_Report_Metrics_Record'Class;
      Metric : not null access Metric_Record'Class)
   is
      Tool_Title  : constant String := Format_Title
        (To_String (Metric.Get_Rule.Tool.Name));
      Tool_Iter   : Gtk_Tree_Iter;
      Metric_Iter : Gtk_Tree_Iter           := Null_Iter;
      Model       : constant Gtk_Tree_Store := Self.Model;
      Dummy       : Boolean;
      pragma Unreferenced (Dummy);

      function Pretty_Print_Value (Value : Float) return String;

      ------------------------
      -- Pretty_Print_Value --
      ------------------------

      function Pretty_Print_Value (Value : Float) return String is
         Tmp   : Unbounded_String;
         Index : Integer := 1;
      begin
         if Float'Truncation (Value) = Value then
            Tmp :=
              To_Unbounded_String
                (Integer'Image (Integer (Float'Truncation (Value))));
         else
            declare
               S : String (1 .. Float'Digits);
            begin
               --  Display only one digit after the '.'
               Ada.Float_Text_IO.Put (S, Value, Aft => 1, Exp => 0);
               Tmp := To_Unbounded_String (S);
            end;
         end if;

         --  Strip the whitespaces at the beginning
         GNATCOLL.Utils.Skip_Blanks (To_String (Tmp), Index);
         Delete (Tmp, 1, Index - 1);

         return To_String (Tmp);
      end Pretty_Print_Value;

   begin
      Tool_Iter := Find_Node
        (Model  => Model,
         Name   => Tool_Title,
         Column => 0,
         Parent => Null_Iter);

      if Tool_Iter = Null_Iter then
         Model.Append (Tool_Iter, Null_Iter);
         Set_And_Clear
           (Model,
            Tool_Iter,
            (0 => As_String (Tool_Title),
             1 => As_String ("")));
      end if;

      Model.Append (Metric_Iter, Tool_Iter);
      Set_And_Clear
        (Model,
         Metric_Iter,
         (0 => As_String
              (Format_Title (To_String (Metric.Get_Rule.Name))),
          1 => As_String (Pretty_Print_Value (Metric.Get_Value))));
   end Add_Metric;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self : not null access GNAThub_Report_Metrics_Record'Class) is
   begin
      Trace (Me, "Clearing the GNAThub Metrics report");
      Self.Model.Clear;
      Self.Metrics.Clear;
   end Clear;

   ------------------
   -- Show_Metrics --
   ------------------

   procedure Show_Metrics
     (Self        : not null access GNAThub_Report_Metrics_Record'Class;
      Location_ID : String) is
   begin
      Trace (Me, "Showing metrics for Location ID: " & Location_ID);

      Self.Model.Clear;

      if not Self.Metrics.Contains (Location_ID) then
         Trace (Me, "No metrics found for Location ID: " & Location_ID);
         return;
      end if;

      for Metric of Self.Metrics (Location_ID) loop
         Self.Add_Metric (Metric);
      end loop;

      Self.Expand_All;
   end Show_Metrics;

   ------------------
   -- Metric_Added --
   ------------------

   overriding procedure Metric_Added
     (Self   : not null access GNAThub_Report_Metrics_Record;
      Metric : not null access Metric_Record'Class)
   is
      Entity      : constant Entity_Data := Metric.Get_Entity;
      Full_Name   : constant String := Metric.Get_File.Display_Full_Name;
      Location_ID : constant String :=
        (if Entity /= No_Entity_Data then
            Full_Name & File_Line_Sep & Integer'Image (Entity.Line)
         else
            Full_Name);
   begin
      if not Self.Metrics.Contains (Location_ID) then
         declare
            New_Set : Metrics_Ordered_Sets.Set;
         begin
            New_Set.Include (Metric);
            Self.Metrics.Insert (Location_ID, New_Set);
         end;
      else
         Self.Metrics.Reference (Location_ID).Include (Metric);
      end if;
   end Metric_Added;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      View : constant GNAThub_Report_Metrics := GNAThub_Report_Metrics (Self);
   begin
      Trace (Me, "Destroying the GNAThub Metrics report");

      GNAThub.Metrics.Unregister_Listener (View);
   end On_Destroy;

end GNAThub.Reports.Metrics;
