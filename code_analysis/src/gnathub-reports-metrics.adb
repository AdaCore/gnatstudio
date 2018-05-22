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

with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Glib;                     use Glib;
with Glib_Values_Utils;        use Glib_Values_Utils;
with GNATCOLL.Utils;

with String_Utils;             use String_Utils;

package body GNAThub.Reports.Metrics is

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      Val_A : constant Float := Float'Value (Get_String (Model, A, 1));
      Val_B : constant Float := Float'Value (Get_String (Model, B, 1));
   begin
      if Val_A < Val_B then
         return 1;
      else
         return -1;
      end if;
   end Sort_Func;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Metrics_Report) is
   begin
      Widget := new GNAThub_Report_Metrics;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access GNAThub_Report_Metrics'Class)
   is
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
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
      Gtk.Box.Initialize_Vbox (Self);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Self.Pack_Start (Scrolled);

      Gtkada.Tree_View.Gtk_New (Self.Metrics_View,
                                (0 => GType_String,   -- name of the metric
                                 1 => GType_String)); -- value of the metric

      Dummy := Self.Metrics_View.Append_Column (New_Column ("Metric", 0));
      Dummy := Self.Metrics_View.Append_Column (New_Column ("Value", 1));
      Self.Metrics_View.Set_Search_Column (0);
      Set_Sort_Func (Self.Metrics_View.Model, 1, Sort_Func'Access);

      Scrolled.Add (Self.Metrics_View);
   end Initialize;

   ----------------------------
   -- Display_Metrics_Report --
   ----------------------------

   procedure Display_Metrics_Report
     (Self    : not null access GNAThub_Report_Metrics'Class;
      Metrics : Metric_Tool_Maps.Map)
   is
      Tool_Iter   : Gtk_Tree_Iter;
      Metric_Iter : Gtk_Tree_Iter           := Null_Iter;
      Model       : constant Gtk_Tree_Store :=
        Gtk_Tree_Store'(-Self.Metrics_View.Get_Model);
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

      use Metric_Tool_Maps;
   begin
      Model.Clear;

      for Cursor in Metrics.Iterate loop
         Model.Append (Tool_Iter, Null_Iter);
         Set_And_Clear (Model,
                        Tool_Iter,
                        (0 =>
                           As_String
                             (Format_Title (To_String (Key (Cursor))))));
         for M of Element (Cursor) loop
            Model.Append (Metric_Iter, Tool_Iter);
            Set_And_Clear (Model,
                           Metric_Iter,
                           (0 =>
                              As_String
                                (Format_Title (To_String (M.Rule.Name))),
                            1 => As_String (Pretty_Print_Value (M.Value))));
         end loop;
         Dummy :=
           Self.Metrics_View.Expand_Row (Model.Get_Path (Tool_Iter), False);
      end loop;
   end Display_Metrics_Report;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access GNAThub_Report_Metrics'Class) is
   begin
      Self.Metrics_View.Model.Clear;
   end Clear;

end GNAThub.Reports.Metrics;
