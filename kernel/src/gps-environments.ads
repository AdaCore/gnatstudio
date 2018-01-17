------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2014-2018, AdaCore                  --
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

--  This package holds environment variables overwritten by GPS

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package GPS.Environments is

   type Environment_Record is tagged limited private;
   type Environment is access all Environment_Record;

   procedure Append
     (Self        : in out Environment_Record;
      Name        : String;
      Users_Value : String;
      GPS_Value   : String);

   procedure Apply_Users_Environment (Self : Environment_Record);
   procedure Apply_GPS_Environment (Self : Environment_Record);

private

   type Environment_Values is record
      Users_Value : Unbounded_String;
      GPS_Value   : Unbounded_String;
   end record;

   package Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Environment_Values,
      "<"          => "<",
      "="          => "=");

   type Environment_Record is tagged limited record
      Map : Maps.Map;
   end record;

end GPS.Environments;
