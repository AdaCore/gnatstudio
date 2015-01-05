------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

--  This package provides the Theme manager.
--  Such themes can be used to customize GPS according finely, by loading
--  specific sections of the customization files only if the user has requested
--  it.
--  The order of loading for GPS is the following:
--    - Load preferences (so that immediate setups like splash screen are
--      taken into account)
--    - Load theme manager module, so that all modules and customization files
--      can define their own themes.
--    - Load customization files, so that all themes are registered in GPS.
--      If the theme is active, we immediately load the relevant XML section
--    - Reload preferences, so that user specific settings override standard
--      settings coming from the preferences.

with GPS.Kernel;

package Theme_Manager_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

end Theme_Manager_Module;
