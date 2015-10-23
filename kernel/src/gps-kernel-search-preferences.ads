------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2015, AdaCore                     --
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

with Default_Preferences;
with GPS.Search;

package GPS.Kernel.Search.Preferences is

   type Preferences_Search_Provider is new Kernel_Search_Provider
   with private;

   overriding function Documentation
     (Self    : not null access Preferences_Search_Provider) return String;
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
   overriding function Complete_Suffix
     (Self      : not null access Preferences_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

   type Preferences_Search_Result is new Kernel_Search_Result with private;
   overriding procedure Free (Self : in out Preferences_Search_Result);

private
   type Preferences_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      --  Current pattern, do not free
      Iter    : Default_Preferences.Cursor;
      --  The current iterator
   end record;

   type Preferences_Search_Result is new Kernel_Search_Result with record
      Pref : Default_Preferences.Preference;
      --  The preference that has been selected
   end record;

   overriding procedure Execute
     (Self       : not null access Preferences_Search_Result;
      Give_Focus : Boolean);

end GPS.Kernel.Search.Preferences;
