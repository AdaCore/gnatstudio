------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with GPS.Kernel.Custom.GUI;
with GPS.Search;

package GPS.Kernel.Search.Plugins is

   type Plugins_Search_Provider is new Kernel_Search_Provider
   with private;

   overriding function Documentation
     (Self    : not null access Plugins_Search_Provider) return String;
   overriding procedure Free (Self : in out Plugins_Search_Provider);
   overriding procedure Set_Pattern
     (Self    : not null access Plugins_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Plugins_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Plugins_Search_Provider) return String
   is
     (Provider_Plugins);

   type Plugins_Search_Result is new Kernel_Search_Result with record
      Plugin_Page : GPS.Kernel.Custom.GUI.Plugin_Preferences_Page;
      --  The page that has been selected
   end record;

   function Create_Plugins_Search_Result
     (Self        : not null access Plugins_Search_Provider;
      Plugin_Page : not null GPS.Kernel.Custom.GUI.Plugin_Preferences_Page;
      Short       : GNAT.Strings.String_Access;
      Long        : GNAT.Strings.String_Access;
      Score       : Natural) return GPS.Search.Search_Result_Access;
   --  Return a GPS.Search.Search_Result_Access according to the matched
   --  preferences page, a short description and the calculated score for this
   --  match.

   overriding procedure Free (Self : in out Plugins_Search_Result);
   overriding procedure Execute
     (Self       : not null access Plugins_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access Plugins_Search_Result)
      return Gtk.Widget.Gtk_Widget;

private
   type Plugins_Search_Provider is new Kernel_Search_Provider with record
      Pattern            : GPS.Search.Search_Pattern_Access;
      --  Current pattern, do not free.
      Pattern_Needs_Free : Boolean := False;
      --  True if Pattern has been allocated by the provider, False otherwise.
      Iter               : Default_Preferences.Page_Cursor;
      --  The current page iterator.
   end record;

end GPS.Kernel.Search.Plugins;
