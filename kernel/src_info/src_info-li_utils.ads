-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with SN;
with VFS;

private package Src_Info.LI_Utils is

   procedure Set_End_Of_Scope
     (Declaration_Info        : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      Location                : SN.Point;
      Kind                    : Reference_Kind := End_Of_Body);
   --  Sets given value for End_Of_Scope attribute of specified declaration

   procedure Insert_Reference
     (Declaration_Info        : in out E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      Location                : SN.Point;
      Kind                    : Reference_Kind);
   --  Inserts new reference to declaration. Declaration here is specified
   --  by pointer to appropriate E_Declaration_Info_Node object

   function Find_Declaration
     (File                    : LI_File_Ptr;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Kind                    : E_Kind := Unresolved_Entity_Kind;
      Location                : SN.Point := SN.Invalid_Point;
      Negate_Kind             : Boolean := False)
      return E_Declaration_Info_List;
   --  Finds declaration in LI tree by it's Name, Location or (and) Kind
   --  If value for some attribute is not given then this attribute doesn't
   --  affect on searching.
   --  Return null if not found.
   --  When Negate_Kind is true, function searches the first declaration
   --  whose kind is NOT equal to the Kind argument.

   function Find_Dependency_Declaration
     (File                    : LI_File_Ptr;
      Symbol_Name             : String := "";
      Class_Name              : String := "";
      Filename                : VFS.Virtual_File := VFS.No_File;
      Kind                    : E_Kind := Unresolved_Entity_Kind;
      Location                : SN.Point := SN.Invalid_Point)
      return E_Declaration_Info_List;
   --  Finds declaration in File by it's Name and Location.
   --  If value for some attribute is not given then this attribute doesn't
   --  affect on searching.
   --  Return null if not found.

   procedure Create_File_Info
     (FI_Ptr         : out File_Info_Ptr;
      Full_Filename  : VFS.Virtual_File;
      Set_Time_Stamp : Boolean := True;
      Unit_Name      : String := "");
   --  Creates an empty File_Info (without declarations)
   --  If Set_Time_Stamp is True, then the timestamp is computed by reading
   --  Full_Filename.

   procedure Create_LI_File
     (File        : out LI_File_Ptr;
      Project     : Projects.Project_Type;
      List        : LI_File_List;
      LI_Filename : VFS.Virtual_File;
      Handler     : LI_Handler);
   --  Creates an empty LI_File structure.
   --  File is set to null if it couldn't be added to the global list of LI
   --  files.
   --  LI_Filename can be the base_name of the LI file, since only this will be
   --  stored in the structure.

   procedure Convert_To_Parsed
     (File               : in out LI_File_Ptr;
      Full_LI_Name       : VFS.Virtual_File;
      Update_Timestamp   : Boolean := True;
      Compilation_Errors : Boolean := False);
   --  Set File as parsed, ie indicate that the actual database has been parsed
   --  and this is no longer only a stub.
   --  If Update_Timestamp is True, then the timestamp of the LI file is also
   --  recomputed.
   --  Compilation_Errors should be true if the xref information is potentially
   --  incomplete because the source file could not be compiled correctly.
   --  Full_LI_Name is the name of the file on which we need to check the
   --  timestamp. This is required since some backends only store basenames in
   --  the LI structure.

   function Find_Reference
     (Declaration_Info        : E_Declaration_Info_List;
      File                    : LI_File_Ptr;
      Location                : SN.Point;
      Kind                    : Reference_Kind)
      return E_Reference_List;
   --  Attempts to find reference of the given type and position
   --  in the reference list of the declaration.
   --  Returns null if no such reference found

end Src_Info.LI_Utils;
