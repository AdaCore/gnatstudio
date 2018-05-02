------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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
with Gdk.RGBA;
with Glib.Main;
with GPS.Kernel;
with GPS.Search;
with Gtk.Box;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.List_Store;
with Gtk.Scrolled_Window;
with Gtk.Spin_Button;
with Gtk.Progress_Bar;
with Gtk.Toggle_Button;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Search_Entry;
with Histories;

package Gtkada.Entry_Completion is

   Max_Idle_Duration : constant Duration := 0.03;
   --  Maximum time spent in the idle callback to insert the possible
   --  completions.

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New
     (Self           : out Gtkada_Entry;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : not null access GPS.Search.Search_Provider'Class;
      Name           : Histories.History_Key;
      Case_Sensitive : Boolean := False;
      Completion_In_Popup : Boolean := True;
      Placeholder         : String := "search");
   procedure Initialize
     (Self           : not null access Gtkada_Entry_Record'Class;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : not null access GPS.Search.Search_Provider'Class;
      Name           : Histories.History_Key;
      Case_Sensitive : Boolean := False;
      Completion_In_Popup : Boolean := True;
      Placeholder         : String := "search");
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
   --  Placeholder is used as the entry's placeholder text.

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

   procedure Reset_Completion (Self : not null access Gtkada_Entry_Record);
   --  Get back to the default provider (i.e: the one passed in parameter
   --  when calling Gtk_New).

   procedure Popup (Self : not null access Gtkada_Entry_Record);
   procedure Popdown (Self : not null access Gtkada_Entry_Record);
   --  Force the display of the completion list (in case it is in a popup)

   procedure Start_Searching
     (Self : not null access Gtkada_Entry_Record'Class);
   --  Start the search engine and start filling the popup window

   Signal_Escape : constant Glib.Signal_Name := "escape";
   --  Emitted when the user presses <escape> in the search field. This is
   --  called just after calling popdown.

   Signal_Activate : constant Glib.Signal_Name := "activate";
   --  Emitted when the user activates a search proposal.
   --  This is called just prior to executing the action.

   Signal_Changed : constant Glib.Signal_Name := "changed";
   --  Emitted when the text changes in the entry

private
   type History_Key_Access is access all Histories.History_Key;

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with record
      GEntry             : Gtkada.Search_Entry.Gtkada_Search_Entry;
      Completion         : GPS.Search.Search_Provider_Access;
      Default_Completion : GPS.Search.Search_Provider_Access;
      Pattern            : GPS.Search.Search_Pattern_Access;
      Kernel             : GPS.Kernel.Kernel_Handle;

      Search_Kind        : GPS.Search.Search_Kind := GPS.Search.Fuzzy;
      --  The currently used search kind

      Idle             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Need_Clear       : Boolean := False;

      Name             : History_Key_Access;

      Completion_Box   : Gtk.Box.Gtk_Box;
      --  Box that contains the list of completion and the notes_scroll

      Popup            : Gtk.Window.Gtk_Window;
      --  The popup window

      Progress_Bar     : Gtk.Progress_Bar.Gtk_Progress_Bar;
      --  Bar displaying the current search's progress

      No_Results_Label : Gtk.Label.Gtk_Label;
      --  The label that is displayed when there is no search results

      Previous_Focus   : Gtk.Widget.Gtk_Widget;
      --  The widget that had the focus before we gave it to the search field
      --  for the last time

      Previous_Focus_Handler_ID : Gtk.Handlers.Handler_Id;
      --  The ID of the handler that is called when the widget that had
      --  previously the focus is destroyed.

      Previous_Context : GPS.Kernel.Selection_Context := GPS.Kernel.No_Context;
      --  The context that was set just before entering the search field.
      --  Used to perform the search with this context.

      Grab_Device      : Gdk.Device.Gdk_Device;

      Column_Provider  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Column_Match     : Gtk.Tree_View_Column.Gtk_Tree_View_Column;

      Color_To_Locations : Gdk.RGBA.Gdk_RGBA;
      --  Color to use for the "=> Locations" text

      Settings                : Gtk.Box.Gtk_Box;
      Settings_Area           : Gtk.Box.Gtk_Box;
      --  The area where the settings are to be displayed

      Settings_Toggle         : Gtk.Toggle_Button.Gtk_Toggle_Button;
      --  The button that toggles the settings on/off

      Settings_Case_Sensitive : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Settings_Whole_Word     : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Settings_Width          : Gtk.Spin_Button.Gtk_Spin_Button;

      Completions      : Gtk.List_Store.Gtk_List_Store;
      View             : Gtk.Tree_View.Gtk_Tree_View;
      --  The widget that displays the list of possible completions

      Notes_Popup      : Gtk.Widget.Gtk_Widget;
      --  A Gtk_Window or Gtk_Frame, depending on whether we are using
      --  popup or not for the preview

      Notes_Scroll     : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Notes_Idle       : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --   Display extra information on the currently selected item

      Focus_Check_Idle : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  Idle handler on focus checking
   end record;

end Gtkada.Entry_Completion;
