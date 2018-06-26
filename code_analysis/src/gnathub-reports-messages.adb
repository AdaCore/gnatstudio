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

with Glib.Object;
with Glib.Values;

with Gtk.Enums;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View_Column;
with Gtk.Tree_Sortable;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Location_View;

with GNATCOLL.Xref;
with GNAThub.Messages;          use GNAThub.Messages;

with String_Utils;              use String_Utils;

package body GNAThub.Reports.Messages is

   use Gtk.Gesture_Multi_Press;

   package Compare_Functions is
     new Gtk.Tree_Sortable.Set_Default_Sort_Func_User_Data (Messages_Report);

   function Compare
     (Model     : Gtk_Tree_Model;
      A         : Gtk.Tree_Model.Gtk_Tree_Iter;
      B         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self      : Messages_Report) return Glib.Gint;
   --  Compare two rows in the model.

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble);
   --  Called every time a row is clicked

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
      Self.Kernel       := Kernel;
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
         Column.Set_Title
           (Format_Title
              (Ada.Strings.Unbounded.To_String (Get_Name (Severity.all))));
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

      Gtk_New (Self.Multipress, Widget => Self.Analysis_View);
      Self.Multipress.On_Pressed (On_Multipress'Access, Slot => Self);
      Self.Multipress.Watch (Self);
   end Initialize;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble)
   is
      pragma Unreferenced (X, Y);
      use Glib;

      View      : constant Messages_Report := Messages_Report (Self);
      Path      : Gtk_Tree_Path;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sort_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

      procedure Show_Message_Location (Message : GNAThub_Message_Access);
      --  Raise the location view and and show the message
      function Get_First_Message
        (File_Node : GNAThub_File_Access)
         return GNAThub_Message_Access;
      function Get_First_Message
        (Subprogram_Node : GNAThub_Subprogram_Access)
         return GNAThub_Message_Access;

      ---------------------------
      -- Show_Message_Location --
      ---------------------------

      procedure Show_Message_Location (Message : GNAThub_Message_Access) is
      begin
         GPS.Location_View.Expand_File
           (GPS.Location_View.Get_Or_Create_Location_View (View.Kernel),
            To_String (Message.Get_Tool.Name),
            Message.Get_File,
            Goto_First => False);
         GPS.Location_View.Raise_Locations_Window (View.Kernel);

         Open_File_Action_Hook.Run
           (View.Kernel,
            Message.Get_File,
            Project => View.Kernel.Get_Project_Tree.Root_Project,
            Line    => Message.Get_Line,
            Column  => Message.Get_Column);
      end Show_Message_Location;

      -----------------------
      -- Get_First_Message --
      -----------------------

      function Get_First_Message
        (File_Node : GNAThub_File_Access)
         return GNAThub_Message_Access
      is
         Msg : GNAThub_Message_Access;
      begin
         if File_Node.Messages.Is_Empty then
            for Subprogram_Node of File_Node.Subprograms loop
               Msg :=
                 Get_First_Message
                   (GNAThub_Subprogram_Access (Subprogram_Node));
               if Msg /= null then
                  return Msg;
               end if;
            end loop;
            return null;
         end if;

         return
           GNAThub_Message_Access
             (File_Node.Messages.First_Element.Message);
      end Get_First_Message;

      -----------------------
      -- Get_First_Message --
      -----------------------

      function Get_First_Message
        (Subprogram_Node : GNAThub_Subprogram_Access)
         return GNAThub_Message_Access is
      begin
         if Subprogram_Node.Messages.Is_Empty then
            return null;
         end if;

         return
           GNAThub_Message_Access
           (Subprogram_Node.Messages.First_Element.Message);
      end Get_First_Message;

   begin
      if N_Press /= 2 then
         return;
      end if;
      View.Multipress.Set_State (Gtk.Enums.Event_Sequence_Claimed);

      View.Analysis_View.Get_Selection.Get_Selected (Model, Sort_Iter);

      if Sort_Iter = Null_Iter then
         return;
      end if;

      View.Analysis_Sort_Model.Convert_Iter_To_Child_Iter
        (Iter, Sort_Iter);

      declare
         File_Node       : constant GNAThub_File_Access :=
           GNAThub_File_Access
             (View.Get_Analysis_Model.File_At (Iter));
         Subprogram_Node : constant GNAThub_Subprogram_Access :=
           GNAThub_Subprogram_Access
             (View.Get_Analysis_Model.Subprogram_At (Iter));
         Msg             : GNAThub_Message_Access;
      begin
         if Subprogram_Node /= null then
            Msg := Get_First_Message (Subprogram_Node);
            if Msg /= null then
               Show_Message_Location (Msg);
            else
               Open_File_Action_Hook.Run
                 (View.Kernel,
                  File_Node.Name,
                  Project => View.Kernel.Get_Project_Tree.Root_Project,
                  Line    => Subprogram_Node.Line,
                  Column  =>
                    GNATCOLL.Xref.Visible_Column (Subprogram_Node.Column));
            end if;
         elsif File_Node /= null then
            Msg := Get_First_Message (File_Node);
            if Msg /= null then
               Show_Message_Location (Msg);
            else
               Open_File_Action_Hook.Run
                 (View.Kernel,
                  File_Node.Name,
                  Project => View.Kernel.Get_Project_Tree.Root_Project,
                  Line    => 0,
                  Column  => 0);
            end if;
         end if;
      end;

      Path_Free (Path);
   end On_Multipress;

   ------------
   -- Update --
   ------------

   procedure Update (Self : not null access GNAThub_Report_Messages'Class) is
   begin
      Self.Analysis_Model.Calculate_Total;
      Self.Analysis_Model.Reconstruct;
   end Update;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Self : not null access GNAThub_Report_Messages'Class)
      return Gtk.Tree_View.Gtk_Tree_View is
   begin
      return Self.Analysis_View;
   end Get_Tree;

   ------------------------
   -- Get_Analysis_Model --
   ------------------------

   function Get_Analysis_Model
     (Self : not null access GNAThub_Report_Messages'Class)
      return GNAThub.Reports.Models.Messages_Model is
   begin
      return Self.Analysis_Model;
   end Get_Analysis_Model;

   --------------------
   -- Get_Sort_Model --
   --------------------

   function Get_Sort_Model
     (Self : not null access GNAThub_Report_Messages'Class)
      return Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort is
   begin
      return Self.Analysis_Sort_Model;
   end Get_Sort_Model;

end GNAThub.Reports.Messages;
