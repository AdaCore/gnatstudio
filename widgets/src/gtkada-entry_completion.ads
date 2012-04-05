------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with Gtk.Box;
with Gtk.GEntry;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with GNAT.Strings;

package Gtkada.Entry_Completion is

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New
     (The_Entry      : out Gtkada_Entry;
      Case_Sensitive : Boolean := True);
   --  Create a new entry.
   --  If Case_Sensitive is False, then the matching of what the user typed
   --  with the completion list is done in a case insensitive manner

   procedure Initialize
     (The_Entry      : access Gtkada_Entry_Record'Class;
      Case_Sensitive : Boolean := True);
   --  Internal procedure

   function Get_Entry (The_Entry : access Gtkada_Entry_Record)
      return Gtk.GEntry.Gtk_Entry;
   --  Return the top entry, possibly the one inside the combo if there is a
   --  combo. Use this to get the current input from the user. This current
   --  input might not be one of the possible completions, since the user
   --  is free to enter any string.

   -----------------
   -- Completions --
   -----------------
   --  The list of possible completions can be provided in one of two days:
   --   - either as an array of strings
   --   - or as a tagged type which provides the strings one by one. This
   --     should be used if you already have the possible completions in your
   --     own datastructure and wish to avoid extra string copies, or if you
   --     want the user's completion to point into your own datastructure to
   --     some complex data.

   type Completions_Factory is abstract tagged null record;
   --  A type that provides all the possible completions. This is used instead
   --  of Set_Completions for efficiency reasons, or when the choice of the
   --  user should point to a complex datastruture

   function Completion
     (Factory : Completions_Factory; Index : Positive) return String
     is abstract;
   --  Return the Index-th element in the list of completions. This
   --  doesn't necessarily matches the current pattern, the check is done by
   --  the entry itself.
   --  The empty string should be returned if there are no more possible
   --  completions

   function Description
     (Factory : Completions_Factory; Index : Positive) return String;
   --  Return an extra string to display in the list of completions for the
   --  matching item. This can be left blank if no extra info is available

   procedure Destroy (Factory : in out Completions_Factory);
   --  Called when the factory (and the associated list of possible
   --  completions) are no longer needed and can be freed. By default, this
   --  does nothing

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : GNAT.Strings.String_List_Access);
   --  Set the possible completions for The_Entry.
   --  Completions should not be freed by the caller, it is taken care of by
   --  this widget. Current_Completion will always 0 in this case.

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : Completions_Factory'Class);
   --  The list of possible completions is provided by Completions. The user's
   --  choice is returned by Current_Completion

   function Current_Completion
     (The_Entry : access Gtkada_Entry_Record) return Natural;
   --  Return the current completion selected by the user, as an index into
   --  the Completions_Factory object. This function returns 0 if the user
   --  has entered a string not in the list of completions or if the list of
   --  possible completions was provided as an array of strings.

private
   type String_Factory is new Completions_Factory with record
      Completions  : GNAT.Strings.String_List_Access;
   end record;
   overriding function Completion
     (Factory : String_Factory; Index : Positive) return String;
   overriding procedure Destroy (Factory : in out String_Factory);
   --  See inherited documentation

   type Completions_Factory_Access is access Completions_Factory'Class;

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with record
      GEntry           : Gtk.GEntry.Gtk_Entry;
      Case_Sensitive   : Boolean;

      Completions      : Completions_Factory_Access;
      --  The iterator over all possible completions

      Last_Position    : Integer;

      Completion_Index : Integer := Integer'Last;
      --  The index in the Tree_View of the current completion. This is set to
      --  Integer'Last if there have been no completion attempted yet.

      View             : Gtk.Tree_View.Gtk_Tree_View;
      List             : Gtk.Tree_Store.Gtk_Tree_Store;
      --  The widget that displays the list of possible completions
   end record;

end Gtkada.Entry_Completion;
