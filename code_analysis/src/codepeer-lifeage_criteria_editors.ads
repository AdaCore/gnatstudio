------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

--  This package provides criteria editor for message lifeage.

private with Ada.Strings.Unbounded;

with Glib;
private with Gtk.Box;
with Gtk.Widget;

package CodePeer.Lifeage_Criteria_Editors is

   type Lifeage_Criteria_Editor_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;

   type Lifeage_Criteria_Editor is
     access all Lifeage_Criteria_Editor_Record'Class;

   procedure Gtk_New
     (Editor         : out Lifeage_Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String);

   procedure Initialize
     (Self           : not null access Lifeage_Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String);

   function Get_Visible_Lifeages
     (Self : access Lifeage_Criteria_Editor_Record'Class)
      return CodePeer.Lifeage_Kinds_Flags;
   --  Returns a set of selected message lifeages

   Signal_Criteria_Changed : constant Glib.Signal_Name;
   --  This signal emitted by the editor in the case of the criteria change

private

   type Lifeage_Criteria_Editor_Record is
     new Gtk.Box.Gtk_Vbox_Record with record
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : Ada.Strings.Unbounded.Unbounded_String;
      Criteria       : CodePeer.Lifeage_Kinds_Flags := (others => False);
   end record;

   Signal_Criteria_Changed : constant Glib.Signal_Name := "criteria-changed";

end CodePeer.Lifeage_Criteria_Editors;
