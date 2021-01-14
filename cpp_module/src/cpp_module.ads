------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2021, AdaCore                     --
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

with GPS.Kernel;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with Default_Preferences;        use Default_Preferences;

package Cpp_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the C/C++ parsers in GNAT Studio.
   --  If the external source navigator executables are not found on the path,
   --  an error is displayed in the console and the C/C++ browsing will not be
   --  available.

   C_Automatic_Indentation : Indentation_Kind_Preferences.Preference;
   C_Use_Tabs              : Boolean_Preference;
   --  Use tabulations when indenting.

   C_Indentation_Level     : Integer_Preference;
   --  Number of spaces for the default indentation.

   C_Indent_Extra          : Boolean_Preference;
   C_Indent_Comments       : Boolean_Preference;

end Cpp_Module;
