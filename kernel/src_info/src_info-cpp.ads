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

pragma Warnings (Off);
with GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;
with SN;
with SN.Xref_Pools; use SN.Xref_Pools;
with Src_Info.Type_Utils;
with Prj;
with Prj_API;

package Src_Info.CPP is

   type CPP_LI_Handler_Record is new LI_Handler_Record with private;
   type CPP_LI_Handler is access all CPP_LI_Handler_Record'Class;

   type CPP_LI_Handler_Iterator is new LI_Handler_Iterator with private;
   --  An iterator to generate the LI database for a set of source files.

   procedure Destroy (Handler : in out CPP_LI_Handler_Record);
   --  See comment in src_info.ads

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

   procedure Destroy (Iterator : in out CPP_LI_Handler_Iterator);
   --  Free the memory used by the list of source files and save xref
   --  pool.

   procedure Add (HT : in out LI_File_List; LIFP : LI_File_Ptr);
   --  Just wrapper for internal Add to support extended testing

   function Get_DB_Dir (Project : Prj.Project_Id) return String;
   pragma Inline (Get_DB_Dir);
   --  Return the directory that contains the source navigator files
   --  for specified project

   type SN_Prj_HTable is private;
   --  Instances of this type are responsible to store hashed
   --  data specific to SN project.
   Empty_SN_Prj_HTable : constant SN_Prj_HTable;

   type SN_Prj_Data is record
      Pool   : Xref_Pool    := Empty_Xref_Pool;
   end record;

   No_SN_Prj_Data : constant SN_Prj_Data :=
     (Pool   => Empty_Xref_Pool);
      --  Xref pool associated with project

   procedure Init (Prj_HTable : out SN_Prj_HTable);
   --  Creates new SN project hash table.

   procedure Free
     (Keys       : in out GNAT.OS_Lib.String_List_Access;
      Prj_HTable : in out SN_Prj_HTable);
   --  Releases table from memory (using given keys).

   function Get_Xref_Pool
     (Prj_HTable : SN_Prj_HTable;
      DB_Dir     : String) return Xref_Pool;
   pragma Inline (Get_Xref_Pool);
   --  Returns xref pool for specified directory or Empty_Xref_Pool
   --  if Prj_HTable is null or pool does not exist.

   procedure Set_Xref_Pool
     (Prj_HTable : SN_Prj_HTable;
      DB_Dir     : String_Access;
      Pool       : Xref_Pool);
   --  Sets xref pool for specified directory. If pool was already set
   --  the old one is released from memory and replaced by the new one.

   function Xref_Filename_For
     (Filename       : String;
      DB_Dir         : String;
      Prj_HTable     : SN_Prj_HTable) return GNAT.OS_Lib.String_Access;
   --  Returns unique xref file name associated with specified source file
   --  name.

   procedure Xref_Filename_For
     (Filename       : String;
      DB_Dir         : String;
      Prj_HTable     : SN_Prj_HTable;
      Xref_Filename  : out GNAT.OS_Lib.String_Access;
      Pool           : out Xref_Pool);
   --  Returns unique xref file name associated with specified source file
   --  name. Also returns xref pool, where this name resides.

   function Get_Prj_HTable
     (Handler : access Src_Info.CPP.CPP_LI_Handler_Record'Class)
     return SN_Prj_HTable;
   pragma Inline (Get_Prj_HTable);
   --  Returns Prj_HTable field from CPP_LI_Handler. This function is used
   --  outside Src_Info.CPP package to get this field.

   function Get_Root_Project
     (Handler : access Src_Info.CPP.CPP_LI_Handler_Record'Class)
     return Prj.Project_Id;
   pragma Inline (Get_Root_Project);
   --  Returns root project for given Handler. This functions is used
   --  outside Src_Info.CPP package to get Root_Project field.

private

   type Iterator_State_Type is
     (Analyze_Files, -- parsing the files with cbrowser.
      Skip_Project,  -- current project should be skipped
      Process_Xrefs, -- processing xrefs for all files
      Done);         -- updating done

   type CPP_LI_Handler_Record is new LI_Handler_Record with record
      DB_Dirs        : GNAT.OS_Lib.String_List_Access;
      --  list of DB dirs, they are used as keys in Prj_HTable
      Prj_HTable     : SN_Prj_HTable := Empty_SN_Prj_HTable;
      SN_Table       : Src_Info.Type_Utils.SN_Table_Array;
      DBIMP_Path     : GNAT.OS_Lib.String_Access := null;
      --  full path to dbimp (found in PATH) or null, if DBIMP is not in PATH
      CBrowser_Path  : GNAT.OS_Lib.String_Access := null;
      --  full path to CBrowser (found in PATH) or null, if CBrowser is not in
      --  PATH
      Root_Project   : Prj.Project_Id;
      --  root projects for lookups of includes (see Sym_IU_Handler)
   end record;
   --  The fields above are always initialized after calling Reset.

   type Imp_Prj_Iterator_Access is
      access all Prj_API.Imported_Project_Iterator;

   type CPP_LI_Handler_Iterator is new LI_Handler_Iterator with record
      State           : Iterator_State_Type := Done;
      Root_Project    : Prj.Project_Id;
      Project         : Prj.Project_Id;
      Handler         : CPP_LI_Handler;
      Tmp_Filename    : GNAT.OS_Lib.Temp_File_Name;
      List_Filename   : GNAT.OS_Lib.String_Access;
      PD              : GNAT.Expect.TTY.TTY_Process_Descriptor;
      Prj_Iterator    : Imp_Prj_Iterator_Access;
   end record;
   --  State is an internal state of iterator, it can be inconsistant with
   --  the real iterator state, because real iterator state depends also on
   --  internal process. State is recomputed during call to Continue.
   --
   --  List_Filename is the name of the files that contains the list of C/C++
   --  sources to process, as well as their associated xref file.

   HTable_Size : constant := 4097;
   type HTable_Range is range 1 .. HTable_Size;

   function Hash is new HTables.Hash (HTable_Range);

   procedure False_Free_Element (X : in out SN_Prj_Data);

   package SN_Prj_HTables is new HTables.Simple_HTable
     (Header_Num   => HTable_Range,
      Element      => SN_Prj_Data,
      Free_Element => False_Free_Element,
      No_Element   => No_SN_Prj_Data,
      Key          => String,
      Hash         => Hash,
      Equal        => "=");

   subtype SN_Prj_HTable_Record is SN_Prj_HTables.HTable;
   type SN_Prj_HTable is access SN_Prj_HTable_Record;
   Empty_SN_Prj_HTable : constant SN_Prj_HTable := null;

end Src_Info.CPP;
