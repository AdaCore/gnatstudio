------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Glib.Values;
with Gtk.Enums;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_View_Column;
with Gtk.Tree_Sortable;

with GPS.Intl;              use GPS.Intl;

package body GNAThub.Reports.Messages is

   package Compare_Functions is
     new Gtk.Tree_Sortable.Set_Default_Sort_Func_User_Data (Messages_Report);

   function Compare
     (Model     : Gtk_Tree_Model;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Messages_Report) return Glib.Gint;
   --  Compare two rows in the model.

   -------------
   -- Compare --
   -------------

   function Compare
     (Model     : Gtk_Tree_Model;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Messages_Report) return Glib.Gint
   is
      use type Glib.Gint;

      A_Counts : Glib.Gint;
      B_Counts : Glib.Gint;

      function Get (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;
      --  Returns counts values for the specified row.

      ---------
      -- Get --
      ---------

      function Get
        (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint
      is
         Aux   : Glib.Gint;
         Value : Glib.Values.GValue;

      begin
         Get_Value (Model, Iter, Self.Total_Column, Value);

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

   begin
      if Parent (Model, A) = Gtk.Tree_Model.Null_Iter then
         return 0;
      end if;

      A_Counts := Get (A);
      B_Counts := Get (B);

      if A_Counts < B_Counts then
         return 1;
      elsif A_Counts > B_Counts then
         return -1;
      else
         return 0;
      end if;
   end Compare;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget     : out Messages_Report;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set) is
   begin
      Widget := new GNAThub_Report_Messages;
      Initialize (Widget, Kernel, Tree, Severities);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access GNAThub_Report_Messages'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set)
   is

      use Gtk.Tree_Model_Sort;
      use type Glib.Gint;

      Panel           : Gtk.Paned.Gtk_Hpaned;
      Scrolled        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

      Index : Glib.Gint := GNAThub.Reports.Models.Entity_Name_Column + 1;
   begin
      Self.Total_Column := Glib.Gint (Severities.Length) + 2;

      Gtk.Box.Initialize_Vbox (Self);

      --  Create report's widgets

      Gtk.Paned.Gtk_New_Hpaned (Panel);
      Self.Pack_Start (Panel);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Panel.Pack1 (Scrolled, Resize => True);

      GNAThub.Reports.Models.Gtk_New
        (Self.Analysis_Model, Kernel, Tree, Severities);
      Gtk.Tree_Model_Sort.Gtk_New_With_Model
        (Self.Analysis_Sort_Model, To_Interface (Self.Analysis_Model));
      Compare_Functions.Set_Default_Sort_Func
        (+Self.Analysis_Sort_Model, Compare'Access, Messages_Report (Self));
      Gtk.Tree_View.Gtk_New (Self.Analysis_View, +Self.Analysis_Sort_Model);
      Scrolled.Add (Self.Analysis_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Entity");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf_Renderer);
      Column.Pack_Start (Pixbuf_Renderer, False);
      Column.Add_Attribute
        (Pixbuf_Renderer,
         "icon-name",
         GNAThub.Reports.Models.Entity_Icon_Name_Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         GNAThub.Reports.Models.Entity_Name_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Total"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute (Text_Renderer, "text", Self.Total_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      for Severity of Severities loop
         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (Ada.Strings.Unbounded.To_String (Severity.Name));
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);

         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute (Text_Renderer, "text", Index);
         Column.Add_Attribute
           (Text_Renderer, "cell-background-rgba",
            Index + Glib.Gint (Severities.Length) + 1);
         Dummy := Self.Analysis_View.Append_Column (Column);
         Index := Index + 1;
      end loop;

      Gtk.Tree_View_Column.Gtk_New (Column);
      Dummy := Self.Analysis_View.Append_Column (Column);
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update (Self : not null access GNAThub_Report_Messages'Class) is
   begin
      Self.Analysis_Model.Calculate_Total;
      Self.Analysis_Model.Reconstruct;
   end Update;

end GNAThub.Reports.Messages;
