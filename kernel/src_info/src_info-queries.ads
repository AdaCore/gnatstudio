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

with Prj;

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

   type Dependency is private;
   --  This type contains the following information:
   --    - the name of the unit on which another unit depends
   --    - the name of the file on which this other unit depends
   --    - whether the dependency comes from the spec and/or from the body,
   --      or is implicit.
   --  In the context of Ada, the Unit_Name represents the package name, and
   --  the explicit dependencies represent "with" statements.

   procedure Get_Unit_Name
     (Source_Info_List : in out Src_Info.LI_File_List;
      Project          : Prj.Project_Id;
      Source_Path      : String;
      Object_Path      : String;
      Dep              : in out Dependency;
      Unit_Name        : out String_Access);
   --  Return the Unit Name from the given Dep. The returned string must not be
   --  freed by the caller.
   --
   --  Note that, for implicit dependencies, the unit name is sometimes
   --  computed in a lazy manor, that is only when read for the first time. In
   --  cases where computing the unit_name fails, null is returned.

   function Get_Filename (Dep : Dependency) return String;
   --  Return the Filename for the given Dep.

   function Get_Depends_From_Spec (Dep : Dependency) return Boolean;
   --  Return True if the given Dep is an explicit dependency from the
   --  specificiations part.

   function Get_Depends_From_Body (Dep : Dependency) return Boolean;
   --  Return True if the given Dep is an explicit dependency from the
   --  implementation part.

   type Dependency_Node;
   type Dependency_List is access Dependency_Node;
   type Dependency_Node is record
      Value : Dependency;
      Next  : Dependency_List;
   end record;
   --  A chained list of Dependency objects.

   procedure Destroy (List : in out Dependency_List);
   --  Destroy the given list, and deallocates all the memory associated.
   --  Has no effect if List is null.

   type Dependencies_Query_Status is
     (Failure,
      Internal_Error,
      Success);
   --  The status returned by the Find_Dependencies routine.

   procedure Find_Dependencies
     (Lib_Info     : LI_File_Ptr;
      Dependencies : out Dependency_List;
      Status       : out Dependencies_Query_Status);
   --  Return the list of units on which the units associated to the given
   --  LI_File depend.
   --
   --  The list returned by this procedure should be deallocated after use.

private

   type Dependency is record
      Unit_Name         : String_Access;
      Filename          : String_Access;
      Depends_From_Spec : Boolean;
      Depends_From_Body : Boolean;
   end record;

end Src_Info.Queries;
