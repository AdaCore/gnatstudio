------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

--  A search provider that matches preferences.

with Default_Preferences; use Default_Preferences;
with GPS.Search;

package GPS.Kernel.Search.Preferences is

   type Preferences_Search_Provider is new Kernel_Search_Provider
   with private;

   overriding function Documentation
     (Self    : not null access Preferences_Search_Provider) return String;
   overriding procedure Free (Self : in out Preferences_Search_Provider);
   overriding procedure Set_Pattern
     (Self    : not null access Preferences_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Preferences_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Preferences_Search_Provider) return String
   is
     (Provider_Preferences);

   procedure Set_Search_Among_Hidden
     (Self  : not null access Preferences_Search_Provider;
      Value : Boolean);
   --  Set whether the provider should search among hidden preferences or not.

   type Preferences_Search_Result is new Kernel_Search_Result with record
      Pref : Default_Preferences.Preference;
      --  The preference that has been selected
   end record;

   function Create_Preferences_Search_Result
     (Self  : not null access Preferences_Search_Provider;
      Pref  : not null Default_Preferences.Preference;
      Short : GNAT.Strings.String_Access;
      Long  : GNAT.Strings.String_Access;
      Score : Natural) return GPS.Search.Search_Result_Access;
   --  Return a GPS.Search.Search_Result_Access according to the matched
   --  preference, a short description and the calculated score for this match.

   overriding procedure Execute
     (Self       : not null access Preferences_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access Preferences_Search_Result)
      return Gtk.Widget.Gtk_Widget;

private
   type Preferences_Search_Provider is new Kernel_Search_Provider with record
      Pattern             : GPS.Search.Search_Pattern_Access;
      --  Current pattern, do not free.
      Pattern_Needs_Free  : Boolean := False;
      --  True if Pattern has been allocated by the provider, False otherwise.
      Iter                : Preference_Cursor (From_Manager);
      --  The current iterator
      Search_Among_Hidden : Boolean := False;
      --  Determines whether the provider should search among hidden
      --  preferences too.
      --  The provider searches only among the visible preferences by default.
   end record;

end GPS.Kernel.Search.Preferences;
