-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2006                            --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles the AUnit templates, and creates the source files
--  from these templates

with GPS.Kernel; use GPS.Kernel;

with Templates_Parser; use Templates_Parser;

package AUnit_Templates is

   function Get_Template_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      Base   : String) return String;
   --  Retrieve the template's full file name from base name

   procedure Create_Files
     (Kernel         : access Kernel_Handle_Record'Class;
      Base_Template  : String;
      Translations   : Translate_Set;
      Directory_Name : String;
      Name           : String;
      Success        : out Boolean);
   --  Create Directory_Name/Name.ads and Directory_Name/Name.adb files using
   --  Base_Template name and the translations

end AUnit_Templates;
