-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  This package provides the low-level implementation of the queries that need
--  information from the compiler (like dependencies, cross-references,...
--
--  You shouldn't use this package directly, but instead call the higher-level
--  routines in glide_kernel.*.
--
--  One general note on the design of this package: this package must be
--  independant of the kernel (e.g take explicit an source_path, instead of a
--  handle to the kernel), so that it can eventually be integrated directly
--  into the sources of Gnat and its tools.

package Src_Info.Queries is

   --------------------------------------
   -- Goto Declaration<->Body requests --
   --------------------------------------

   type Find_Decl_Or_Body_Query_Status is
     (Entity_Not_Found,
      Internal_Error,
      No_Body_Entity_Found,
      Success);
   --  The status returned by the Find_Declaration_Or_Body routine.

   procedure Find_Declaration_Or_Body
     (Lib_Info        : LI_File_Ptr;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Find_Decl_Or_Body_Query_Status);
   --  Implement the Goto Declaration<->Body algorithm using the given
   --  Filename, Entity_Name, and Line/Column position.

   ---------------------------
   -- Dependencies requests --
   ---------------------------
   --  The following subprogram can be used to retrieve information about the
   --  dependency between files and units in the current project.
   --    - the name of the unit on which another unit depends
   --    - the name of the file on which this other unit depends
   --    - whether the dependency comes from the spec and/or from the body,
   --      or is implicit.
   --  In the context of Ada, the Unit_Name represents the package name, and
   --  the explicit dependencies represent "with" statements.

   type Dependencies_Query_Status is
     (Failure,
      Internal_Error,
      Success);
   --  The status returned by the Find_Dependencies routine.

   procedure Find_Dependencies
     (Lib_Info     : LI_File_Ptr;
      Dependencies : out Dependency_File_Info_List;
      Status       : out Dependencies_Query_Status);
   --  Return the list of units on which the units associated to the given
   --  LI_File depend.
   --
   --  You shouldn't deallocate the list returned by this procedure.

end Src_Info.Queries;
