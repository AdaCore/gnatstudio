------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with Gtk.Notebook;
with Gtk.Widget;
with Gtkada.Handlers;
with GNAThub.Messages;

package body GNAThub.Reports.Collector is

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Listener'Class, Message_Listener_Access);

   procedure On_Destroy (View : access Gtk.Widget.Gtk_Widget_Record'Class);

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
      Notebook : Gtk.Notebook.Gtk_Notebook;
   begin
      Gtk.Box.Initialize_Vbox (Self);
      Self.Kernel := Kernel;

      --  Notebook

      Gtk.Notebook.Gtk_New (Notebook);
      Self.Pack_Start (Notebook);

      --  Messages report tab

      GNAThub.Reports.Messages.Gtk_New
        (Self.Messages_Report, Kernel, Tree, Severities);

      Notebook.Append_Page (Self.Messages_Report);
      Notebook.Set_Tab_Label_Text (Self.Messages_Report, "Messages");

      Self.Listener := new Message_Listener (Gtk.Box.Gtk_Vbox (Self));

      GPS.Kernel.Messages.Register_Listener
        (Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Listener),
         GPS.Kernel.Messages.Locations_Only);

      Gtkada.Handlers.Widget_Callback.Connect
        (Self, Gtk.Widget.Signal_Destroy, On_Destroy'Access);
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
      if Message.all in GNAThub.Messages.Message'Class then
         View.Update;
      end if;
   end Message_Added;

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

   ------------
   -- Update --
   ------------

   procedure Update (Self : not null access GNAThub_Report_Collector'Class) is
   begin
      Self.Messages_Report.Update;
   end Update;

end GNAThub.Reports.Collector;
