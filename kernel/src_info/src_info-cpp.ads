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

with GNAT.Expect;

with SN;             use SN;
with SN.Xref_Pools;  use SN.Xref_Pools;

with Prj;

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

   type CPP_LI_Handler_Iterator is new LI_Handler_Iterator with record
      Root_Project    : Prj.Project_Id;
      Project         : Prj.Project_Id;
      SN_Dir          : SN.String_Access;
      Xrefs           : Xref_Pool;
      Tmp_Filename    : SN.String_Access;
      PD              : GNAT.Expect.Process_Descriptor;
   end record;
   --  An iterator to generate the LI database for a set of source files.

   function Generate_LI_For_Source
     (Handler       : access CPP_LI_Handler_Record;
      Root_Project  : Prj.Project_Id;
      File_Project  : Prj.Project_Id;
      Full_Filename : String) return LI_Handler_Iterator'Class;
   --  Not implemented for C and C++: use Generate_LI_For_Project to update
   --  database for all files in the project.

   function Generate_LI_For_Project
     (Handler       : access CPP_LI_Handler_Record;
      Root_Project  : Prj.Project_Id;
      Project       : Prj.Project_Id;
      Recursive     : Boolean := False)
      return LI_Handler_Iterator'Class;
   --  Generate the LI information for all the source files in Project (and all
   --  its imported projects if Recursive is True).
   --  This function should do as few work as possible, and the iterator will
   --  be called until all the files are processed.

   procedure Continue
     (Iterator : in out CPP_LI_Handler_Iterator;
      Finished : out Boolean);
   --  This function should move to the next source file that has been
   --  analyzed, providing the previous file is fully parsed.
   --  If the files are analyzed by external processes, the call to
   --  Generate_LI_For_Project would for instance start the external process
   --  for the first file, and when Next is called, it should check that the
   --  first process as finished executing before processing the next file.
   --
   --  If an extra phase needs to be done after parsing all the source files,
   --  it should also be done as a result of a call to Continue.
   --
   --  Nothing needs to be done if the previous source file hasn't been fully
   --  analyzed yet.
   --
   --  Finished should be True if the Iterator has finished regenerating the
   --  database.

   procedure Add
     (HT      : in out LI_File_List;
      LIFP    : LI_File_Ptr;
      Success : out Boolean);
   --  Just wrapper for internal Add to support extended testing

end Src_Info.CPP;
