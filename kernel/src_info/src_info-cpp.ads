-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with GNAT.Expect; use GNAT.Expect;

package Src_Info.CPP is

   type CPP_LI_Handler_Record is new LI_Handler_Record with null record;
   type CPP_LI_Handler is access all CPP_LI_Handler_Record'Class;

   procedure Create_Or_Complete_LI
     (Handler                : access CPP_LI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String);
   --  Creates or completes Library Information for given source file name
   --  and LI_File_Ptr.

   function Case_Insensitive_Identifiers
     (Handler : access CPP_LI_Handler_Record) return Boolean;
   pragma Inline (Case_Insensitive_Identifiers);
   --  Is identifiers in given language case insensitive? Always returns
   --  False since identifiers are case sensitive in C and C++.

   procedure Parse_All_LI_Information
     (Handler                : access CPP_LI_Handler_Record;
      List                   : in out LI_File_List;
      In_Directory           : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String);

   function LI_Filename_From_Source
     (Handler                : access CPP_LI_Handler_Record;
      Source_Filename        : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String)
      return String;
   --  Converts the given Source Filename into the corresponding LI filename
   --  using the Project and Predefined Source Path information. Return the
   --  empty string when the given Source_Filename can not be found in the
   --  project exception lists and when the extension does not follow the
   --  project naming scheme.
   --  ??? In current implementation for C/C++ this function always
   --  returns Xref_Filename for Source_Filename

   procedure Delete_Database (Root_Project : Prj.Project_Id);
   --  Deletes SN project database

   function Browse_File
     (Source_File            : String;
      Root_Project           : Prj.Project_Id) return Process_Descriptor;
   --  Runs process to create/update SN database for given file.

   function Generate_Xrefs
     (Root_Project : Prj.Project_Id) return Process_Descriptor;

   procedure Add
     (HT      : in out LI_File_List;
      LIFP    : LI_File_Ptr;
      Success : out Boolean);
   --  Just wrapper for internal Add to support extended testing

end Src_Info.CPP;
