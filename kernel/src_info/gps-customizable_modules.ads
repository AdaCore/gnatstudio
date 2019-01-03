------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  This package defines base type for modules capable to customize through XML

with GNATCOLL.VFS;

with GPS.Core_Kernels;                 use GPS.Core_Kernels;

with XML_Utils;                        use XML_Utils;

package GPS.Customizable_Modules is

   type Customization_Level is
     (Hard_Coded, System_Wide, Project_Wide, User_Specific, Themes);
   --  The various level of customization (See GPS.Kernel.Custom).
   --  Hard_Coded is used for customization that are hard-coded in the GPS code
   --  System_Wide is used if customization comes from a custom file found in
   --  the installation directory of GPS.
   --  Project_Wide is used if the customization comes from a custom file found
   --  in one of the directories lists in GPS_CUSTOM_PATH.
   --  User_Specific is used if the customization comes from a custom file
   --  found in the user's own directory (see GPS_HOME/.gps/plug-ins).
   --  Themes is used if the customization was found in a theme definition,
   --  wherever that definition was found.

   type Customizable_Module_Record is
     abstract new Abstract_Module_Record with null record;

   procedure Customize
     (Module : access Customizable_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level) is abstract;
   --  Subprogram called when a new customization has been parsed.
   --  It is initially called just after all modules have been registered,
   --  and gets passed a single XML node.
   --  File is the XML file that is currently being parsed.

   procedure Execute_Customization_String
     (Kernel : access Core_Kernel_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Send a signal to all registered modules to indicate a new customization
   --  string.

end GPS.Customizable_Modules;
