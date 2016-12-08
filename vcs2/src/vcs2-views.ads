------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

--  Generic VCS views

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Default_Preferences;    use Default_Preferences;
with Generic_Views;          use Generic_Views;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Toolbar;            use Gtk.Toolbar;
with Gtkada.Tree_View;       use Gtkada.Tree_View;

package VCS2.Views is

   -------------------
   -- Base_VCS_View --
   -------------------

   type Base_VCS_View_Record is new Generic_Views.View_Record with record
      Tree         : Tree_View;
      --  The tree that represents data.

      Text_Render : Gtk_Cell_Renderer_Text;
      --  The text renderer for the longuest cell. This will automatically
      --  be ellipsized depending on the corresponding preference

      Filter_Options  : Filter_Options_Mask :=
        Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy;
      Filter_Hist_Prefix : Unbounded_String;

   end record;
   overriding procedure Create_Toolbar
     (View    : not null access Base_VCS_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure On_Create
     (Self    : not null access Base_VCS_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class);

   procedure On_Preferenced_Changed
     (Self    : not null access Base_VCS_View_Record;
      Pref    : Preference);
   --  Called when preferences change.
   --  Can be overridden.

   procedure Refresh (Self : not null access Base_VCS_View_Record) is null;
   --  Refresh the contents of the view

end VCS2.Views;
