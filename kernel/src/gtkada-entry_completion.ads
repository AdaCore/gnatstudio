------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2013, AdaCore                     --
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

--  An entry field that provides on-the-fly completion.
--  This completion is provided by a GPS.Search.Search_Provider.

with Gdk.Device;
with Glib.Main;
with Gtk.Box;
with Gtk.Check_Button;
with Gtk.Combo_Box_Text;
with Gtk.Scrolled_Window;
with Gtk.List_Store;
with Gtkada.Search_Entry;
with Gtk.Tree_View;
with Gtk.Widget;
with Gtk.Window;
with GPS.Kernel;
with GPS.Search;
with Histories;

package Gtkada.Entry_Completion is

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New
     (Self           : out Gtkada_Entry;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : not null access GPS.Search.Search_Provider'Class;
      Name           : Histories.History_Key;
      Case_Sensitive : Boolean := False;
      Preview        : Boolean := True;
      Completion_In_Popup : Boolean := True);
   procedure Initialize
     (Self           : not null access Gtkada_Entry_Record'Class;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : not null access GPS.Search.Search_Provider'Class;
      Name           : Histories.History_Key;
      Case_Sensitive : Boolean := False;
      Preview        : Boolean := True;
      Completion_In_Popup : Boolean := True);
   --  Create a new entry.
   --
   --  Name is a unique name for this entry. It is used to store a number of
   --  information from one session to the next.
   --
   --  Case_Sensitive is the default value the first time (ever) this entry is
   --  displayed. Afterwards, its value is retained in a history key so that
   --  user changes are taken into account.
   --
   --  Completion is the provider to be used to compute the possible
   --  completions. Completion is then owned by Self, and must not be freed
   --  by the caller.
   --
   --  The list of completions can either appear in a popup, or in a widget
   --  below the completion entry. Do not use a popup if the entry is put in a
   --  dialog, since the latter will grab all events and the list of
   --  completions will not receive the mouse events. The layout is configured
   --  via Completion_In_Popup.
   --
   --  Preview indicates whether we want to show the previous window by
   --  default. Like Completion_In_Popup, it is only relevant the first time
   --  the entry is displayed.

   function Get_Type return Glib.GType;
   --  The internal gtk+ type

   function Get_Kernel
      (Self : not null access Gtkada_Entry_Record)
      return GPS.Kernel.Kernel_Handle;
   --  Return a handle to the kernel

   function Get_Text
      (Self : not null access Gtkada_Entry_Record) return String;
   procedure Set_Text
      (Self : not null access Gtkada_Entry_Record;
       Text : String);
   --  Force the text in the entry.

   procedure Set_Completion
      (Self : not null access Gtkada_Entry_Record;
       Completion : not null access GPS.Search.Search_Provider'Class);
   --  Override the provider for the completions

   procedure Popup (Self : not null access Gtkada_Entry_Record);
   procedure Popdown (Self : not null access Gtkada_Entry_Record);
   --  Force the display of the completion list (in case it is in a popup)

   Signal_Escape : constant Glib.Signal_Name := "escape";
   --  Emitted when the user presses <escape> in the search field. This is
   --  called just after calling popdown.

   Signal_Activate : constant Glib.Signal_Name := "activate";
   --  Emitted when the user activates a search proposal.
   --  This is called just prior to executing the action.

private
   type History_Key_Access is access all Histories.History_Key;

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with record
      GEntry           : Gtkada.Search_Entry.Gtkada_Search_Entry;
      Completion       : GPS.Search.Search_Provider_Access;
      Pattern          : GPS.Search.Search_Pattern_Access;
      Kernel           : GPS.Kernel.Kernel_Handle;

      Idle             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Need_Clear       : Boolean := False;

      Name             : History_Key_Access;

      Completion_Box   : Gtk.Box.Gtk_Box;
      --  Box that contains the list of completion and the notes_scroll

      Popup            : Gtk.Window.Gtk_Window;
      --  The popup window

      Grab_Device             : Gdk.Device.Gdk_Device;

      Settings                : Gtk.Box.Gtk_Box;
      Settings_Case_Sensitive : Gtk.Check_Button.Gtk_Check_Button;
      Settings_Whole_Word     : Gtk.Check_Button.Gtk_Check_Button;
      Settings_Preview        : Gtk.Check_Button.Gtk_Check_Button;
      Settings_Kind           : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;

      Completions      : Gtk.List_Store.Gtk_List_Store;
      View             : Gtk.Tree_View.Gtk_Tree_View;
      --  The widget that displays the list of possible completions

      Notes_Popup      : Gtk.Widget.Gtk_Widget;
      --  A Gtk_Window or Gtk_Frame, depending on whether we are using
      --  popup or not for the preview

      Notes_Scroll     : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Notes_Idle       : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --   Display extra information on the currently selected item
   end record;

end Gtkada.Entry_Completion;
