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
with Gtk.List_Store;
with Gtk.Tree_View;
with GPS.Kernel;
with GPS.Search;
with GNAT.Strings;
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
   procedure Initialize
     (Self           : not null access Gtkada_Entry_Record'Class;
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

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with record
      GEntry           : Gtk.GEntry.Gtk_Entry;
      Case_Sensitive   : Boolean;
      Completion       : GPS.Search.Search_Provider_Access;
      Pattern          : GPS.Search.Search_Pattern_Access;
      Kernel           : GPS.Kernel.Kernel_Handle;

      Idle             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Need_Clear       : Boolean := False;

      History_Key      : History_Key_Access;

      Hist             : GNAT.Strings.String_List_Access;
      --  Do not free this, this belongs to the history

      Completions      : Gtk.List_Store.Gtk_List_Store;
      View             : Gtk.Tree_View.Gtk_Tree_View;
      --  The widget that displays the list of possible completions
   end record;

end Gtkada.Entry_Completion;
