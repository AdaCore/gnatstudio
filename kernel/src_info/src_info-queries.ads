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
   --  If not reference to the entity could be found, then File_Name_Found is
   --  set to null, and the other values are undefined.
   --
   --  The memory allocated for File_Name_Found must be deallocated after use.

   ---------------------------
   -- Dependencies requests --
   ---------------------------

   type Dependency is private;
   --  This type contains the following information:
   --    - Information on the file on which we depend
   --    - Information on the dependency itself: whether it comes from the spec
   --      and/or from the body, or is implicit.
   --  In the context of Ada, explicit dependencies represent "with" statements

   type Dependency_Node;
   type Dependency_List is access Dependency_Node;
   type Dependency_Node is record
      Value : Dependency;
      Next  : Dependency_List;
   end record;
   --  A list of dependencies.

   procedure Destroy (List : in out Dependency_List);
   --  Destroy the given list, and deallocates all the memory associated.
   --  Has no effect if List is null.

   function File_Information (Dep : Dependency) return Internal_File;
   --  Return the information on the file that Dep depends on.
   --  You mustn't free the returned value, since it points to internal
   --  data. However, you must keep a copy if you intend to store it somewhere.

   function Dependency_Information (Dep : Dependency) return Dependency_Info;
   --  Return the information on the dependency itself. This doesn't contain
   --  information about the files.

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
      File  : Src_Info.Internal_File;
      Dep   : Src_Info.Dependency_Info;
   end record;

   pragma Inline (File_Information);
   pragma Inline (Dependency_Information);
end Src_Info.Queries;
