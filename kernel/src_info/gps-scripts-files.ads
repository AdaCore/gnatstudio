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
--  Implementation of File class

with GPS.Core_Kernels;
with GNATCOLL.Scripts.Files;
with GNATCOLL.VFS;

package GPS.Scripts.Files is

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class);
   --  Add script commands for file class.

   function Get_File_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type;
   --  Return the class to use for file types. This encapsulates a File_Info.
   --  This is more efficient than calling directly
   --  GPS.Kernel.Scripts.New_Class particularly when a File class has already
   --  been created.

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive)
      return GNATCOLL.VFS.Virtual_File renames GNATCOLL.Scripts.Files.Nth_Arg;
   procedure Set_Nth_Arg
     (Data : in out Callback_Data'Class;
      N    : Positive;
      File : GNATCOLL.VFS.Virtual_File)
       renames GNATCOLL.Scripts.Files.Set_Nth_Arg;
   function Get_Data
     (Instance : Class_Instance) return GNATCOLL.VFS.Virtual_File
       renames GNATCOLL.Scripts.Files.Get_Data;
   --  Retrieve the file information from an instance. This returns No_File
   --  if no instance is passed

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Class_Instance
       renames GNATCOLL.Scripts.Files.Create_File;
   --  Return a new file

end GPS.Scripts.Files;
