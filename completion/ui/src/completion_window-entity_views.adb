------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with Completion.Search;     use Completion.Search;
with Gdk.Types.Keysyms;     use Gdk.Types, Gdk.Types.Keysyms;
with GPS.Kernel.Search;     use GPS.Kernel.Search;

package body Completion_Window.Entity_Views is

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      P : Kernel_Search_Provider_Access;
   begin
      P := new Entities_Search_Provider;
      Register_Provider_And_Action
        (Kernel, P,
         Accel_Key  => GDK_LC_t,
         Accel_Mods => Primary_Mod_Mask);
   end Register_Module;

end Completion_Window.Entity_Views;
