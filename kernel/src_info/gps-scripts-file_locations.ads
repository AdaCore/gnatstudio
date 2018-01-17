------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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
--  Implementation of FileLocation class

with Basic_Types;
with GPS.Core_Kernels;

package GPS.Scripts.File_Locations is

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class);
   --  Add script commands for FileLocation class.

   function Get_File_Location_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type;
   --  Return the class used to represent locations in files. This encapsulates
   --  a File_Location_Info

   type File_Location_Info is private;
   No_File_Location : constant File_Location_Info;

   function Get_File (Location : File_Location_Info) return Class_Instance;
   function Get_Line (Location : File_Location_Info) return Integer;
   function Get_Column
     (Location : File_Location_Info) return Basic_Types.Visible_Column_Type;
   --  Return the information stored in the file location

   function Get_Data (Data : Callback_Data'Class; N : Positive)
      return File_Location_Info;
   --  Retrieve the file location information from an instance

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type) return Class_Instance;
   --  Return a new file.
   --  File mustn't be destroyed after this call.

private

   type File_Location_Info is record
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
   end record;

   No_File_Location : constant File_Location_Info := (No_Class_Instance, 0, 0);

end GPS.Scripts.File_Locations;
