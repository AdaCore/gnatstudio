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
with Gtk.GEntry;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Window;
with GPS.Kernel;
with GPS.Search;
with Histories;

package Gtkada.Entry_Completion is

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New
     (Self           : out Gtkada_Entry;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Completion     : access GPS.Search.Search_Provider'Class;
      Case_Sensitive : Boolean := True;
      History        : Histories.History_Key := "");
   --  Create a new entry.
   --  If Case_Sensitive is False, then the matching of what the user typed
   --  with the completion list is done in a case insensitive manner
   --
   --  Completion is the provider to be used to compute the possible
   --  completions. Completion is then owned by Self, and must not be freed
   --  by the caller.

   function Get_Entry (Self : access Gtkada_Entry_Record)
      return Gtk.GEntry.Gtk_Entry;
   --  Return the top entry, possibly the one inside the combo if there is a
   --  combo. Use this to get the current input from the user. This current
   --  input might not be one of the possible completions, since the user
   --  is free to enter any string.

private
   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with record
      GEntry           : Gtk.GEntry.Gtk_Entry;
      Case_Sensitive   : Boolean;
      Completion       : GPS.Search.Search_Provider_Access;
      Pattern          : GPS.Search.Search_Pattern_Access;

      Idle             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Need_Clear       : Boolean := False;

      Popup            : Gtk.Window.Gtk_Window;
      View             : Gtk.Tree_View.Gtk_Tree_View;
      List             : Gtk.Tree_Store.Gtk_Tree_Store;
      --  The widget that displays the list of possible completions
   end record;

end Gtkada.Entry_Completion;
