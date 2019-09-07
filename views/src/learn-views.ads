------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

--  This package implements the GPS Learn view

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;

with Default_Preferences; use Default_Preferences;
with GPS.Kernel;

package Learn.Views is

   type Learn_View_Module_Type is new Module_ID_Record with private;
   type Learn_View_Module_Access is access all Learn_View_Module_Type;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

private

   package Boolean_Preference_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Boolean_Preference,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => "=");

   type Learn_View_Module_Type is new Module_ID_Record with record
      Show_Providers_Preferences : Boolean_Preference_Maps.Map;
   end record;

end Learn.Views;
