-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Prj;

package Src_Info.ALI is

   type ALI_Handler_Record is new LI_Handler_Record with null record;
   type ALI_Handler is access all ALI_Handler_Record'Class;

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String);

   function LI_Filename_From_Source
     (Handler                : access ALI_Handler_Record;
      Source_Filename        : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String)
      return String;
   --  Converts the given Source Filename into the corresponding ALI filename
   --  using the Project and Predefined Source Path information. Return the
   --  empty string when the given Source_Filename can not be found in the
   --  project exception lists and when the extension does not follow the
   --  project naming scheme.
   --
   --  Side_Effects:
   --  This function destroys the contents of the Namet buffer. Save it before
   --  calling this function if it needs to be preserved.
   --
   --  Limitations:
   --  This function does *not* support separates. This limitation would be
   --  hard to lift with the current parameter profile, because we would need
   --  to do compute the associated unit name in order to deduce the name of
   --  non-separate source file. Unfortunately, the function
   --              Source_Filename = f (Unit_Filename)
   --  is not bijective...
   --  ??? Add a function that computes the "Significant source filename"
   --  ??? from a given unit name that would be a separate. The result of
   --  ??? this function could then be used to call ALI_Filename_From_Source
   --  ??? to get the associated ALI filename.

end Src_Info.ALI;
