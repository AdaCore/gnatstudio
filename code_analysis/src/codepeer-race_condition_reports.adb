------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with System.Address_To_Access_Conversions;

with Glib.Values;
with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;

with GPS.Editors.GtkAda;
with GPS.Location_View;

package body CodePeer.Race_Condition_Reports is

   package Race_Condition_Report_Callbacks is
     new Gtk.Handlers.Callback (Race_Condition_Report_Record);

   procedure On_Summary_Activated
     (Self   : access Race_Condition_Report_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Handles click on summary view

   procedure On_Details_Activated
     (Self   : access Race_Condition_Report_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Handles click on details view

   package Message_Conversions is
     new System.Address_To_Access_Conversions
       (GPS.Kernel.Messages.Abstract_Message'Class);

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
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy    : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Gtk.Box.Initialize_Vbox (Self);
      Self.Kernel := Kernel;

      --  Summary view

      CodePeer.Race_Summary_Models.Gtk_New (Self.Summary_Model, Kernel, Tree);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Pack_Start (Scrolled);

      Gtk.Tree_View.Gtk_New (Self.Summary_View, Self.Summary_Model);
      Scrolled.Add (Self.Summary_View);
      Self.Summary_View.Set_Activate_On_Single_Click (True);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Object");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute
        (Renderer, "text", CodePeer.Race_Summary_Models.Object_Name_Column);
      Dummy := Self.Summary_View.Append_Column (Column);

      --  Details view

      CodePeer.Race_Details_Models.Gtk_New (Self.Details_Model, Kernel);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Pack_Start (Scrolled);

      Gtk.Tree_View.Gtk_New (Self.Details_View, Self.Details_Model);
      Scrolled.Add (Self.Details_View);
      Self.Details_View.Set_Activate_On_Single_Click (True);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Entry point");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute
        (Renderer,
         "text",
         CodePeer.Race_Details_Models.Entry_Point_Name_Column);
      Dummy := Self.Details_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title ("Access");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute
        (Renderer, "text", CodePeer.Race_Details_Models.Access_Kind_Column);
      Dummy := Self.Details_View.Append_Column (Column);

      --  Connect callbacks

      Race_Condition_Report_Callbacks.Object_Connect
        (Self.Summary_View,
         Gtk.Tree_View.Signal_Row_Activated,
         Race_Condition_Report_Callbacks.To_Marshaller
           (On_Summary_Activated'Access),
         Self);
      Race_Condition_Report_Callbacks.Object_Connect
        (Self.Details_View,
         Gtk.Tree_View.Signal_Row_Activated,
         Race_Condition_Report_Callbacks.To_Marshaller
           (On_Details_Activated'Access),
         Self);
   end Initialize;

   --------------------------
   -- On_Details_Activated --
   --------------------------

   procedure On_Details_Activated
     (Self   : access Race_Condition_Report_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      pragma Unreferenced (Column);

      Iter    : Gtk_Tree_Iter;
      Value   : Glib.Values.GValue;

   begin
      Iter := Self.Details_Model.Get_Iter (Path);

      Self.Details_Model.Get_Value
        (Iter, CodePeer.Race_Details_Models.Mark_Column, Value);

      declare
         Mark     : constant GPS.Editors.Editor_Mark'Class :=
           GPS.Editors.GtkAda.Get_Mark (Value);
         Location : constant GPS.Editors.Editor_Location'Class :=
           Mark.Location (True);

      begin
         Location.Buffer.Current_View.Cursor_Goto (Location, True);
      end;

      Glib.Values.Unset (Value);
   end On_Details_Activated;

   --------------------------
   -- On_Summary_Activated --
   --------------------------

   procedure On_Summary_Activated
     (Self   : access Race_Condition_Report_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      pragma Unreferenced (Column);

      Iter    : Gtk_Tree_Iter;
      Value   : Glib.Values.GValue;
      Message : GPS.Kernel.Messages.Message_Access;

   begin
      Iter := Self.Summary_Model.Get_Iter (Path);
      Self.Details_Model.Set
        (Self.Summary_Model.Get_Entry_Points (Iter));

      Self.Summary_Model.Get_Value
        (Iter, CodePeer.Race_Summary_Models.Message_Column, Value);
      Message :=
        GPS.Kernel.Messages.Message_Access
          (Message_Conversions.To_Pointer
             (Glib.Values.Get_Address (Value)));
      Glib.Values.Unset (Value);

      GPS.Location_View.Expand_File
        (GPS.Location_View.Get_Or_Create_Location_View (Self.Kernel),
         Ada.Strings.Unbounded.To_String (Message.Get_Category),
         Message.Get_File,
         False);
      GPS.Location_View.Raise_Locations_Window (Self.Kernel);
   end On_Summary_Activated;

end CodePeer.Race_Condition_Reports;
