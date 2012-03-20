------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

package GPS_Preferences_Types is

   type Multi_Language_Builder_Policy is (Auto, Gnatmake, Gprbuild);
   --  The List of possible multi-language builders
   --   - Auto: gnatmake for Ada projects, gprbuild otherwise
   --   - Gnatmake: always use gnatmake (disable multi-language builds)
   --   - Gprbuild: always use gprbuild, even for Ada projects

   Default_Builder : constant Multi_Language_Builder_Policy := Auto;

end GPS_Preferences_Types;
