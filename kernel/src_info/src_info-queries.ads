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

with Traces;

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

   ----------------
   -- Scope tree --
   ----------------
   --  A scope tree is the base structure for the call graph and the type
   --  browser.
   --  Such a tree is generated from an LI structure, and becomes obsolete as
   --  soon as that structure is scanned again (since we keep pointers to the
   --  internal nodes of the structure

   type Scope_Tree is private;
   Null_Scope_Tree : constant Scope_Tree;

   type Scope_Tree_Node is private;
   Null_Scope_Tree_Node : constant Scope_Tree_Node;

   function Create_Tree (Lib_Info : LI_File_Ptr) return Scope_Tree;
   --  Create a new scope tree from an already parsed Library information.
   --  Note that the resulting tree needs to be freed whenever Lib_Info
   --  changes, since the tree points to internal nodes of Lib_Info.

   procedure Free (Tree : in out Scope_Tree);
   --  Free the memory occupied by Tree.

   procedure Trace_Dump (Handler : Traces.Debug_Handle; Tree : Scope_Tree);
   --  Dump the contentns of the tree to standard_output.

   function Find_Entity_Declaration
     (Tree : Scope_Tree; Name : String; Line, Column : Integer)
      return Scope_Tree_Node;
   --  Return the declaration node for the entity Name at position Line, Column

   function Is_Subprogram (Node : Scope_Tree_Node) return Boolean;
   --  Return True if Node is associated with a subprogram

   -------------------------
   --  Entity information --
   -------------------------
   --  This type groups information about entities that allow an exact
   --  identification of that entity, including handling of overriding
   --  subprograms,... This information has a life-cycle independent from the
   --  tree itself, and thus can be kept independently in a browser.

   type Entity_Information is private;

   function Get_Entity (Tree : Scope_Tree; Node : Scope_Tree_Node)
      return Entity_Information;
   --  Return the information for the entity defined in Node.
   --  You must to call Destroy on the returned information.

   procedure Destroy (Entity : in out Entity_Information);
   --  Free the memory associated with the entity;

   function Get_Name (Entity : Entity_Information) return String;
   --  Return the name of the entity associated with Node.

   function Get_Declaration_Line_Of
     (Entity : Entity_Information) return Positive;
   function Get_Declaration_Column_Of
     (Entity : Entity_Information) return Natural;
   function Get_Declaration_File_Of
     (Entity : Entity_Information) return String;
   --  Return the location of the declaration for Entity. Note that this
   --  location remains valid only until the source file are changed. It is not
   --  magically updated when the source file is changed.

   --------------------------
   -- Scope tree iterators --
   --------------------------

   type Scope_Tree_Node_Iterator is private;

   function Start (Node : Scope_Tree_Node) return Scope_Tree_Node_Iterator;
   --  Return the first child of Node

   procedure Next (Iter : in out Scope_Tree_Node_Iterator);
   --  Move to the next sibling of Iter

   function Get (Iter : Scope_Tree_Node_Iterator) return Scope_Tree_Node;
   --  Return the node pointed to by Iter, or null if Iter is invalid.

private

   type Dependency is record
      File  : Src_Info.Internal_File;
      Dep   : Src_Info.Dependency_Info;
   end record;

   type Scope_Type is (Declaration, Reference);
   --  The type for the elements in the scope: these are either a
   --  declaration, with subranges or subdeclarations, or a reference to
   --  another entity.

   type Entity_Information is record
      Name        : String_Access;
      Decl_Line   : Positive;
      Decl_Column : Natural;
      Decl_File   : String_Access;
   end record;

   type Scope_Node;
   type Scope_List is access Scope_Node;
   type Scope_Node (Typ : Scope_Type) is record
      Sibling : Scope_List;
      --  Pointer to the next item at the same level.

      Decl : E_Declaration_Access;
      --  The declaration of the entity

      case Typ is
         when Declaration =>
            Start_Of_Scope : File_Location;
            Contents : Scope_List;

         when Reference =>
            Ref : E_Reference_Access;
      end case;
   end record;

   type Scope_Tree is record
      Lib_Info    : LI_File_Ptr;
      LI_Filename : String_Access;
      Time_Stamp  : Timestamp;
      --  For efficiency, we keep an access to the LI file that was used to
      --  create the tree. However, we also keep the file name itself, so that
      --  we can check whether the LI file was updated, and the tree is no
      --  longer valid.

      Body_Tree : Scope_List;
      --  The information for the body file.
   end record;
   --  This tree represents the global scope information for the files
   --  associated with Lib_Info (spec, body and separate).

   type Scope_Tree_Node is new Scope_List;
   type Scope_Tree_Node_Iterator is new Scope_List;

   Null_Scope_Tree_Node : constant Scope_Tree_Node := null;

   Null_Scope_Tree : constant Scope_Tree :=
     (Lib_Info    => null,
      LI_Filename => null,
      Time_Stamp  => 0,
      Body_Tree   => null);

   pragma Inline (File_Information);
   pragma Inline (Dependency_Information);
end Src_Info.Queries;
