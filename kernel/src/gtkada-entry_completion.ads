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

with Glib.Main;
with Gtk.Box;
with Gtk.Check_Button;
with Gtk.Combo_Box_Text;
with Gtk.GEntry;
with Gtk.List_Store;
with Gtk.Scrolled_Window;
with Gtk.Tree_View;
with Gtk.Window;
with GPS.Kernel;
with GPS.Search;
with GNAT.Strings;
with Histories;

package Gtkada.Entry_Completion is

   type Gtkada_Entry_Record is new Gtk.GEntry.Gtk_Entry_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New
     (Self           : out Gtkada_Entry;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : access GPS.Search.Search_Provider'Class;
      Name           : Histories.History_Key;
      Case_Sensitive : Boolean := False);
   procedure Initialize
     (Self           : not null access Gtkada_Entry_Record'Class;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : access GPS.Search.Search_Provider'Class;
      Name           : Histories.History_Key;
      Case_Sensitive : Boolean := False);
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
   --  If the user presses <enter> in the entry when there is no completion
   --  proposal, Fallback is called if specified. If unspecified, nothing
   --  happens.

   function Fallback
      (Self : not null access Gtkada_Entry_Record;
       Text : String) return GPS.Search.Search_Result_Access is (null);
   --  Called when the user has pressed <enter> in the entry and there was
   --  no completion.
   --  The returned value is used as a proposal as if the user had clicked
   --  on it. If not null, the dialog is closed in addition.
   --  The returned value is freed by the entry.

   function Get_Kernel
      (Self : not null access Gtkada_Entry_Record)
      return GPS.Kernel.Kernel_Handle;
   --  Return a handle to the kernel

private
   type History_Key_Access is access all Histories.History_Key;

   type Gtkada_Entry_Record is new Gtk.GEntry.Gtk_Entry_Record with record
      Completion       : GPS.Search.Search_Provider_Access;
      Pattern          : GPS.Search.Search_Pattern_Access;
      Kernel           : GPS.Kernel.Kernel_Handle;

      Idle             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Need_Clear       : Boolean := False;

      Name             : History_Key_Access;

      Hist             : GNAT.Strings.String_List_Access;
      --  Do not free this, this belongs to the history

      Popup            : Gtk.Window.Gtk_Window;
      --  The popup window

      Settings_Case_Sensitive : Gtk.Check_Button.Gtk_Check_Button;
      Settings_Whole_Word     : Gtk.Check_Button.Gtk_Check_Button;
      Settings_Kind           : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;

      Completions      : Gtk.List_Store.Gtk_List_Store;
      View             : Gtk.Tree_View.Gtk_Tree_View;
      --  The widget that displays the list of possible completions

      Notes_Scroll     : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Notes_Box        : Gtk.Box.Gtk_Box;
      --   Display extra information on the currently selected item
   end record;

end Gtkada.Entry_Completion;
