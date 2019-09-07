------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

--  This package provides facilities to load target, models and modes from XML

with GNATCOLL.VFS;

with GPS.Core_Kernels;                 use GPS.Core_Kernels;
with GPS.Customizable_Modules;         use GPS.Customizable_Modules;

with XML_Utils;                        use XML_Utils;

package GPS.CLI_Target_Loaders is

   type Target_Loader (Kernel : access Core_Kernel_Record'Class) is
     new Customizable_Module_Record with null record;

   overriding procedure Customize
     (Self   : access Target_Loader;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);

end GPS.CLI_Target_Loaders;
