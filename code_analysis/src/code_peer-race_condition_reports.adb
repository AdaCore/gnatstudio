-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2011, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Gdk.Event;
with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;
with Gtk.Tree_View_Column;
with Gtk.Widget;

package body Code_Peer.Race_Condition_Reports is

   package Race_Condition_Report_Return_Boolean_Callbacks is
     new Gtk.Handlers.User_Return_Callback
       (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Race_Condition_Report);

   function On_Summary_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Race_Condition_Report) return Boolean;
   --  Handles click on summary view

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Report : out Race_Condition_Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree) is
   begin
      Report := new Race_Condition_Report_Record;
      Initialize (Report, Kernel, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Race_Condition_Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree)
   is
      Scrolled      : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Details_View  : Gtk.Tree_View.Gtk_Tree_View;
      Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Renderer      : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy         : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Gtk.Box.Initialize_Vbox (Self);

      --  Summary view

      Code_Peer.Race_Summary_Models.Gtk_New (Self.Summary_Model, Kernel, Tree);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Pack_Start (Scrolled);

      Gtk.Tree_View.Gtk_New (Self.Summary_View, Self.Summary_Model);
      Scrolled.Add (Self.Summary_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Object");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute
        (Renderer, "text", Code_Peer.Race_Summary_Models.Object_Name_Column);
      Dummy := Self.Summary_View.Append_Column (Column);

      --  Details view

      Code_Peer.Race_Details_Models.Gtk_New (Self.Details_Model);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Pack_Start (Scrolled);

      Gtk.Tree_View.Gtk_New (Details_View, Self.Details_Model);
      Scrolled.Add (Details_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Entry point");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute
        (Renderer,
         "text",
         Code_Peer.Race_Details_Models.Entry_Point_Name_Column);
      Dummy := Details_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Access");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute
        (Renderer, "text", Code_Peer.Race_Details_Models.Access_Kind_Column);
      Dummy := Details_View.Append_Column (Column);

      --  Connect callbacks

      Race_Condition_Report_Return_Boolean_Callbacks.Connect
        (Self.Summary_View,
         Gtk.Widget.Signal_Button_Press_Event,
         Race_Condition_Report_Return_Boolean_Callbacks.To_Marshaller
           (On_Summary_Click'Access),
         Race_Condition_Report (Self),
         False);
   end Initialize;

   ----------------------
   -- On_Summary_Click --
   ----------------------

   function On_Summary_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Race_Condition_Report) return Boolean
   is
      pragma Unreferenced (View);

      use type Glib.Guint;
      use type Gdk.Event.Gdk_Event_Type;
      use type Gtk.Tree_Model.Gtk_Tree_Path;

      X      : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_X (Event));
      Y      : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_Y (Event));
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cell_X : Glib.Gint;
      Cell_Y : Glib.Gint;
      Found  : Boolean;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Press
      then
         Self.Summary_View.Get_Path_At_Pos
           (X, Y, Path, Column, Cell_X, Cell_Y, Found);

         if Path /= null then
            Self.Summary_View.Get_Selection.Select_Path (Path);
            Iter := Self.Summary_Model.Get_Iter (Path);
            Self.Details_Model.Set
              (Self.Summary_Model.Get_Entry_Points (Iter));
         end if;
      end if;

      return False;
   end On_Summary_Click;

end Code_Peer.Race_Condition_Reports;
