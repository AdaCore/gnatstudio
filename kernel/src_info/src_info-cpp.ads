-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with GNAT.Expect;
with GNAT.OS_Lib;
with SN;
with SN.Xref_Pools;
with Src_Info.Type_Utils;
with Prj;

package Src_Info.CPP is

   type CPP_LI_Handler_Record is new LI_Handler_Record with private;
   type CPP_LI_Handler is access all CPP_LI_Handler_Record'Class;

   type CPP_LI_Handler_Iterator is new LI_Handler_Iterator with private;
   --  An iterator to generate the LI database for a set of source files.

   function Set_Executables
     (Handler : access CPP_LI_Handler_Record) return String;
   --  Compute the location of the external source navigator executables on the
   --  path. If they are not found, this function should return an error
   --  message to print in the GPS console.
   --  The empty string is returned if all went well.

   procedure Reset
     (Handler : access CPP_LI_Handler_Record'Class;
      Project : Prj.Project_Id);
   --  Reset the internal fields for this handler.
   --  This function should be called every time the project view changes, but
   --  it won't do anything if the object directory of Project hasn't changed.

   procedure Create_Or_Complete_LI
     (Handler                : access CPP_LI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String);
   --  See comment in src_info.ads

   function Case_Insensitive_Identifiers
     (Handler : access CPP_LI_Handler_Record) return Boolean;
   pragma Inline (Case_Insensitive_Identifiers);
   --  Always returns False since identifiers are case sensitive in C and C++.

   procedure Parse_All_LI_Information
     (Handler                : access CPP_LI_Handler_Record;
      List                   : in out LI_File_List;
      In_Directory           : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String);
   --  Does nothing for now.

   function LI_Filename_From_Source
     (Handler                : access CPP_LI_Handler_Record;
      Source_Filename        : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String)
      return String;
   --  See comment in src_info.ads.
   --  For C/C++, this function returns the name of the xref filename to
   --  generate.

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
   --  See comment in src_info.ads

   procedure Continue
     (Iterator : in out CPP_LI_Handler_Iterator;
      Finished : out Boolean);
   --  See comment in src_info.ads

   procedure Add
     (HT      : in out LI_File_List;
      LIFP    : LI_File_Ptr;
      Success : out Boolean);
   --  Just wrapper for internal Add to support extended testing

   function Get_DB_Dir (Handler : access CPP_LI_Handler_Record) return String;
   pragma Inline (Get_DB_Dir);
   --  Return the directory that contains the source navigator files

   function Get_Xrefs (Handler : access CPP_LI_Handler_Record)
      return SN.Xref_Pools.Xref_Pool;
   pragma Inline (Get_Xrefs);
   --  Return the database for the mapping from source files to xref files.

private

   type Iterator_State_Type is
     (Analyze_Files, -- parsing the files with cbrowser.
      Process_Xrefs, -- processing xrefs for all files
      Done);         -- updating done

   type CPP_LI_Handler_Record is new LI_Handler_Record with record
      Xrefs          : SN.Xref_Pools.Xref_Pool;
      DB_Dir         : GNAT.OS_Lib.String_Access;
      SN_Table       : Src_Info.Type_Utils.SN_Table_Array;
      DBIMP_Path     : GNAT.OS_Lib.String_Access := null;
      --  full path to dbimp (found in PATH) or null, if DBIMP is not in PATH
      CBrowser_Path  : GNAT.OS_Lib.String_Access := null;
      --  full path to CBrowser (found in PATH) or null, if CBrowser is not in
      --  PATH
   end record;
   --  The fields above are always initialized after calling Reset.

   type CPP_LI_Handler_Iterator is new LI_Handler_Iterator with record
      State           : Iterator_State_Type := Done;
      Root_Project    : Prj.Project_Id;
      Project         : Prj.Project_Id;
      Handler         : CPP_LI_Handler;
      Tmp_Filename    : GNAT.OS_Lib.Temp_File_Name;
      List_Filename   : GNAT.OS_Lib.String_Access;
      PD              : GNAT.Expect.Process_Descriptor;
   end record;
   --  State is an internal state of iterator, it can be inconsistant with
   --  the real iterator state, because real iterator state depends also on
   --  internal process. State is recomputed during call to Continue.
   --
   --  List_Filename is the name of the files that contains the list of C/C++
   --  sources to process, as well as their associated xref file.

end Src_Info.CPP;
