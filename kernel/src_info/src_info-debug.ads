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

--  <description>
--  This package provides a set of services that print to Stdout a text
--  representation of the given object. The format used to represent the
--  data structures closely matches the format of the GNAT ALI files.
--  Most procedures are not explicitely documented because their name is
--  self-explantory.
--
--  These procedures are provided to help during debugging sessions, and hence
--  are all garded against any exception, to avoid interfering with the
--  application. When unhandled exceptions are caught, a message "<unhandled
--  exception 'exception_name' trapped in 'dump_procedure_name'>" is printed
--  to signal it.
--
--  Note that extra care has been taken to make sure that no overloading
--  is done to simplify the task of calling these procedures from the
--  debugger.
--  </description>

package Src_Info.Debug is

   procedure Dump_LI_File_Ptr
     (LIFP : LI_File_Ptr; Show_Timestamps : Boolean := True);

   procedure Dump_LI_File_From_Name
     (LIFL : LI_File_List; Unit_Name : String);
   --  Print to Stdout a text representation of the LI_File whose
   --  unit name is Unit_Name. Print "Unknown unit 'xyz'" if no such
   --  Unit_Name was found.

   procedure Dump (LIFL : LI_File_List);

private

   procedure Dump_Source_File (SF : Source_File);

   procedure Dump_File_Location (FL : File_Location);
   --  Prints the file location in the following format:
   --    filename:line:colum

   procedure Dump_E_Reference (ER : E_Reference);

   procedure Dump_E_Reference_List (ERL : E_Reference_List);

   procedure Dump_E_Declaration (ED : E_Declaration);

   procedure Dump_E_Declaration_Info (EDI : E_Declaration_Info);

   procedure Dump_E_Declaration_Info_List (EDIL : E_Declaration_Info_List);

   procedure Dump_File_Info (FI : File_Info; ALI_Format : Boolean := True);
   --  If ALI_Format is True, then the output printed closely follows the ALI
   --  file format. Otherwise, if the Original_Filename is set, this filename
   --  followed by a ':' and the Original_Line are printed after the
   --  Source_Filename on the 'X' line.

   procedure Dump_Dependency_File_Info
     (DFI        : Dependency_File_Info;
      ALI_Format : Boolean := True);
   --  If ALI_Format is True, then the output printed closely follows the ALI
   --  file format. Otherwise, the Depends_From_Spec/Body fields are
   --  printed on the 'X' line after the Source_Filename.

   procedure Dump_Dependency_File_Info_List
     (DFIL       : Dependency_File_Info_List;
      ALI_Format : Boolean := True);
   --  If ALI_Format is True, then the output printed closely follows the ALI
   --  file format. Otherwise, the Depends_From_Spec/Body fields of each
   --  Dependency_File_Info object are printed after the Source_Filename
   --  on the 'X' lines.

   procedure Dump_Unit_Dependency_Section (LIF : LI_File; Part : Unit_Part);
   --  Dump the 'U' line and the associated 'W' lines. Part must be either
   --  Unit_Spec of Unit_Body.

   procedure Dump_File_Dependency_Section
     (LIF : LI_File; Show_Timestamps : Boolean := True);
   --  Dump all the 'D' lines for the given LI_File.

   procedure Dump_LI_File (LIF : LI_File; Show_Timestamps : Boolean := True);

end Src_Info.Debug;
