-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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
with Unchecked_Deallocation;
with Language_Handlers;
with Prj.Tree;
with Prj_API;
with Basic_Types;
with String_Hash;

package Src_Info.Queries is

   ----------------
   -- Scope Tree --
   ----------------

   --  A scope tree is the base structure for the call graph and the type
   --  browser.
   --  Such a tree is generated from an LI structure, and becomes obsolete as
   --  soon as that structure is scanned again (since we keep pointers to the
   --  internal nodes of the structure

   type Scope_Tree is private;
   Null_Scope_Tree : constant Scope_Tree;

   --  See below for operations on Scope_Tree

   -------------------------
   --  Entity information --
   -------------------------
   --  This type groups information about entities that allow an exact
   --  identification of that entity, including handling of overriding
   --  subprograms,... This information has a life-cycle independent from the
   --  tree itself, and thus can be kept independently in a browser.

   type Entity_Information is private;
   No_Entity_Information         : constant Entity_Information;

   type Entity_Information_Array is array (Natural range <>)
     of Entity_Information;

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
   --  The declaration file might be the empty string if the exact location for
   --  the declaration could not be resolved (case of overloaded entities, or
   --  predefined entities)

   function Get_Kind (Entity : Entity_Information) return E_Kind;
   --  Return the kind of the entity. See glide_kernel.ads on how to convert
   --  this to a string.

   function Is_Predefined_Entity (Entity : Entity_Information) return Boolean;
   --  Return True if the entity is one of the predefined entities for the
   --  language, ie there are no declaration for it in the user's code (Like
   --  Integer in Ada, or int in C).

   function Get_Scope (Entity : Entity_Information) return E_Scope;
   --  Return the scope of the entity.  See glide_kernel.ads on how to convert
   --  this to a string.

   function Get_Full_Name
     (Entity    : Entity_Information;
      Decl_File : LI_File_Ptr;
      Separator : String := ".";
      Scope     : Scope_Tree := Null_Scope_Tree) return String;
   --  Compute the fully qualified name of the entity. For an entity defined in
   --  the function F of package P, the name would
   --     "P" & Separator & "F" & Separator & Get_Name (Entity)
   --  Decl_File must be the file in which the entity is declared
   --  Scope, if provided, is the scope tree corresponding to Decl_File.

   function Copy (Entity : Entity_Information) return Entity_Information;
   --  Return a copy of Entity. The result must be explicitely destroyed.

   function Create
     (File   : String;
      Line   : Positive;
      Column : Natural;
      Name   : String;
      Scope  : E_Scope;
      Kind   : E_Kind) return Entity_Information;
   --  Return a new entity information structure. It is the responsability of
   --  the user to free the allocated memory.
   --  If File is the empty string, the entity will be considered as a
   --  predefined entity.

   procedure Renaming_Of
     (List           : LI_File_List;
      Entity         : Entity_Information;
      Is_Renaming    : out Boolean;
      Renamed_Entity : out Entity_Information);
   --  If Entity is a renaming of another entity (or a typedef for another
   --  type), then Is_Renaming is set to True, and Renamed_Entity points to the
   --  entity that is renamed.
   --  It might happen if some ALI files were not found that Is_Renaming is
   --  True but Renamed_Entity is No_Entity_Information. This should be
   --  reported as an error to the user.
   --
   --  You must call destroy on the Renamed_Entity.

   function Is_Equal (Entity1, Entity2 : Entity_Information) return Boolean;
   --  Return True if both Entity1 and Entity2 represent the same source entity
   --  (same name and declaration location)

   --------------------------------------
   -- Goto Declaration<->Body requests --
   --------------------------------------

   type Find_Decl_Or_Body_Query_Status is
     (Entity_Not_Found,
      Internal_Error,
      No_Body_Entity_Found,
      Overloaded_Entity_Found,
      Fuzzy_Match,
      Success);
   --  The status returned by the Find_Declaration_Or_Body routine.
   --  Fuzzy_Match is returned if the exact location wasn't found (e.g the LI
   --  file wasn't up-to-date), and the returned location is the closest that
   --  matched.

   procedure Find_Declaration
     (Lib_Info      : LI_File_Ptr;
      File_Name     : String;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Entity        : out Entity_Information;
      Status        : out Find_Decl_Or_Body_Query_Status);
   --  Find the location of the location of the declaration for the given
   --  entity.
   --
   --  If no entity could be found, Status is set to a value other than
   --  Success. In that case, Entity are irrelevant.
   --
   --  If the entity is an overloaded entity that the LI parser could not fully
   --  resolve, then Status is set to Overloaded_Entity_Found and Entity is set
   --  to No_Entity_Information.
   --
   --  The memory occupied by Entity must be freed by the caller.

   procedure Find_Next_Body
     (Lib_Info               : LI_File_Ptr;
      File_Name              : String;
      Entity_Name            : String;
      Line                   : Positive;
      Column                 : Positive;
      Handler                : access LI_Handler_Record'Class;
      Source_Info_List       : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String;
      Location               : out File_Location;
      Status                 : out Find_Decl_Or_Body_Query_Status);
   --  Find the location of the body for the entity. If the entity has multiple
   --  bodies (as is the case for instance for separates in Ada), and
   --  (Line,Column) is already the location of one of the bodies, then this
   --  procedure returns the location of the next body.
   --
   --  If no entity could be found, Status is set to a value other than
   --  Success. In that case, Location is irrelevant.
   --
   --  If the entity is in fact an overloaded entity that the LI handler
   --  couldn't resolve, Status is set to Overloaded_Entity_Found. In that
   --  case, Location is irrelevant. The user should be asked what exact
   --  definition of the entity he wants, and then you should call
   --  Find_Next_Body again with the location of the declaration.
   --
   --  Note: Location has a short term life: it can no longer be used once you
   --  reparse Lib_Info, or update its contents.
   --
   --  Most of the time, you should use this function through the equivalent
   --  call in Glide_Kernel.

   ---------------------------
   -- Spec <-> Body queries --
   ---------------------------

   function Get_Other_File_Of
     (Lib_Info : LI_File_Ptr; Source_Filename : String) return String;
   --  Return the name of the spec or body for Source_Filename.
   --  If Source_Filename is a separate, then the spec of the unit is returned.
   --  The empty string is returned if there is no other file (for instance, a
   --  body without a spec).
   --  Only the short path name is returned.
   --
   --  This method is based on LI files.
   --  See also Prj_API.Other_File_Name for a method based on naming schemes

   ------------------
   -- Declarations --
   ------------------

   type Entity_Declaration_Iterator is private;

   function Find_All_Possible_Declarations
     (Lib_Info    : LI_File_Ptr;
      Entity_Name : String := "";
      In_Source_File : String := "") return Entity_Declaration_Iterator;
   --  Return the first entity declaration in Lib_Info or its imported units
   --  whose name is Entity_Name. Note that the fake declarations for
   --  unresolved overloaded entities (with E_Kind = Overloaded_Entity) are not
   --  returned.  The entity is search in all source files associated with
   --  Lib_Info and its dependencies
   --  If Entity_Name is the empty string, all entities declared in Lib_Info
   --  will be returned.
   --  If In_Source_File is not the empty string, the search is limited to that
   --  file and doesn't include the dependencies.

   function Get (Iterator : Entity_Declaration_Iterator)
      return Entity_Information;
   --  Return the current entity, or No_Entity_Information if there is no more
   --  matching entity.
   --  It is the responsability of the caller to free the returned value.

   procedure Next (Iterator : in out Entity_Declaration_Iterator);
   --  Move to the next matching entity.
   --  The iterator is automatically destroyed upon seeing the last entity.

   function At_End (Iterator : Entity_Declaration_Iterator) return Boolean;
   --  Return True if there are no more matching entities.

   procedure Destroy (Iterator : in out Entity_Declaration_Iterator);
   --  Free the memory used by the iterator.
   --  It is safe to call this function multiple times.

   ----------------
   -- References --
   ----------------

   type Entity_Reference_Iterator is private;
   type Entity_Reference_Iterator_Access is access Entity_Reference_Iterator;

   procedure Find_All_References
     (Root_Project           : Prj.Tree.Project_Node_Id;
      Lang_Handler           : Language_Handlers.Language_Handler;
      Entity                 : Entity_Information;
      List                   : in out LI_File_List;
      Iterator               : out Entity_Reference_Iterator;
      Project                : Prj.Project_Id := Prj.No_Project;
      LI_Once                : Boolean := False;
      In_File                : String := "";
      Predefined_Source_Path : String := "";
      Predefined_Object_Path : String := "");
   --  Find all the references to the entity described in Decl.
   --  Root_Project should be the root project under which we are looking.
   --  Source files that don't belong to Root_Project or one of its imported
   --  project will not be searched.
   --  Project is the project to which the file where the declaration is found
   --  belongs. It can optionally be left to Empty_Node if this is not known,
   --  but the search will take a little bit longer.
   --  Note also that the declaration itself is not returned.
   --
   --  if LI_Once is True, then a single reference will be returned for each LI
   --  file. This can be used for instance if you are only looking for matching
   --  LI files.
   --
   --  If In_File is a full file name, then only the references in that file
   --  will be displayed. This is much faster, since it only requires the
   --  parsing of a single LI structure.
   --
   --  You must destroy the iterator when you are done with it, to avoid memory
   --  leaks.

   procedure Next
     (Lang_Handler : Language_Handlers.Language_Handler;
      Iterator : in out Entity_Reference_Iterator;
      List     : in out LI_File_List);
   --  Get the next reference to the entity

   function Get (Iterator : Entity_Reference_Iterator) return E_Reference;
   --  Return the reference currently pointed to. No_Reference is returned if
   --  there are no more reference.

   function Get_LI (Iterator : Entity_Reference_Iterator) return LI_File_Ptr;
   --  Return the current LI file

   function Get (Iterator : Entity_Reference_Iterator) return Prj.Project_Id;
   --  Return the current project the iterator is looking at.

   procedure Destroy (Iterator : in out Entity_Reference_Iterator);
   procedure Destroy (Iterator : in out Entity_Reference_Iterator_Access);
   --  Free the memory occupied by the iterator.

   -------------
   -- Parents --
   -------------

   function Get_Parent_Package
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Entity_Information;
   --  Return the parent package of Entity.
   --  For instance, if we are in package A.B.C, and Entity is "C", this
   --  returns "B". You then need to call this function again for "B", with the
   --  same Lib_Info to get to "A".
   --  No_Entity_Information is returned if Entity is not a package or doesn't
   --  have a parent package.
   --
   --  Lib_Info must already have been parsed.
   --
   --  The returned entity must be freed by the user.

   type Parent_Iterator is private;

   function Get_Parent_Types
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Parent_Iterator;
   --  Return a pointer to the first parent type for the type Entity. In
   --  Object-oriented languages, this would be the classes Entity derives
   --  from. In Ada, this includes the parent type of a type or subtype

   procedure Next (Iter : in out Parent_Iterator);
   --  Move to the next parent of the entity

   function Get (Iter : Parent_Iterator) return Entity_Information;
   --  Return the current parent of the entity. No_Entity_Information is
   --  returned if there are no more parents

   --------------------------------------
   -- Methods and primitive operations --
   --------------------------------------

   type Primitive_Iterator is private;

   function Get_Primitive_Operations
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Primitive_Iterator;
   --  Return the first primitive operation for the type Entity. This will not
   --  return anything if Entity is a variable.

   function Get (Iter : Primitive_Iterator) return Entity_Information;
   --  Return the current primitive operation, or No_Entity_Information if
   --  there are no more.
   --  The returned entity must be freed by the user.

   procedure Next (Iter : in out Primitive_Iterator);
   --  Move the next primitive operation.

   function Length (Iter : Primitive_Iterator) return Natural;
   --  Return the number of primitive operations that remain to be returned by
   --  Iter, including the current one. If Iter is the direct result of
   --  Get_Primitive_Operations, this is the total number of primitive
   --  operations for the entity.

   ---------------
   -- Variables --
   ---------------

   function Get_Variable_Type
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Entity_Information;
   --  Return a pointer to the variable's type. Lib_Info is the LI file that
   --  contains the declaration of the variable.
   --  If the type of the entity is one of the predefined types for the
   --  language (Integer for Ada for instance), the returned entity will have
   --  Is_Predefined_Entity returning true.

   function Array_Contents_Type
     (Lib_Info   : LI_File_Ptr;
      Array_Type : Entity_Information) return Entity_Information
      renames Get_Variable_Type;
   --  Return the type of data contained in an array type.

   function Pointed_Type
     (Lib_Info   : LI_File_Ptr;
      Access_Type : Entity_Information) return Entity_Information;
   --  Return the type of data pointed to by a pointer type.

   function Returned_Type
     (Lib_Info        : LI_File_Ptr;
      Subprogram_Type : Entity_Information) return Entity_Information;
   --  Return the type returned by a function, or No_Entity_Information for a
   --  procedure.

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

   procedure Destroy (Dep  : in out Dependency);
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
     (Lib_Info        : LI_File_Ptr;
      Source_Filename : String;
      Dependencies    : out Dependency_List;
      Status          : out Dependencies_Query_Status);
   --  Return the list of units on which the units associated to the given
   --  LI_File directly depend.
   --  Note that only the direct dependencies for Source_Filename are returned.
   --  If Source_Filename is a spec, then the files imported from the body are
   --  not returned.
   --
   --  A separate unit always depends on the body, and a body always depends on
   --  the separate units. All their dependencies are shared.
   --
   --  The list returned by this procedure should be deallocated after use.

   type Dependency_Iterator is private;
   type Dependency_Iterator_Access is access Dependency_Iterator;

   procedure Find_Ancestor_Dependencies
     (Root_Project    : Prj.Tree.Project_Node_Id;
      Lang_Handler    : Language_Handlers.Language_Handler;
      Source_Filename : String;
      List            : in out LI_File_List;
      Iterator        : out Dependency_Iterator;
      Project         : Prj.Project_Id := Prj.No_Project;
      Include_Self    : Boolean := False;
      Predefined_Source_Path : String := "";
      Predefined_Object_Path : String := "";
      LI_Once         : Boolean := False;
      Single_Source_File : Boolean := False);
   --  Prepare Iterator to return the list of all files that directly import
   --  Source_Filename. The rule is the following:
   --     - bodies and separate units always depend on specs
   --     - bodies always depend on separates
   --     - separates always depend on other separates.
   --  The model is that the body and the separate units are considered as a
   --  single virtual file.
   --
   --  No indirect dependency is considered
   --
   --  Root_Project should be the root project under which we are looking.
   --  Source files that don't belong to Root_Project or one of its imported
   --  project will not be searched.
   --  Project is the project to which the file where the declaration is found
   --  belongs. It can optionally be left to Empty_Node if this is not known,
   --  but the search will take a little bit longer.
   --
   --  If Include_Self is true, then Source_Filename itself will be
   --  returned. Otherwise, only the other source files are returned, even if
   --  they belong to the same LI file.
   --
   --  If Single_Source_File is True, then the iterator returned will only
   --  return Source_Filename. This might be used in special contexts to either
   --  work on multiple LI files or a single source file.
   --
   --  If LI_Once is true, then for each LI file only one source file will be
   --  returned. This is for use when you are using the version of Get that
   --  returns a LI_File_Ptr.
   --
   --  You must destroy the iterator when you are done with it, to avoid memory
   --  leaks.

   procedure Next
     (Lang_Handler : Language_Handlers.Language_Handler;
      Iterator : in out Dependency_Iterator;
      List     : in out LI_File_List);
   --  Get the next reference to the entity

   function Get (Iterator : Dependency_Iterator) return Dependency;
   --  Return the file pointed to. You must free the returned value.

   function Get (Iterator : Dependency_Iterator) return LI_File_Ptr;
   --  Return the LI for the file that contains the dependency. Note that this
   --  is not the LI file for Dependency, as returned by Get.
   --  Consider setting LI_Once to True when calling Find_Ancestor_Dependencies
   --  if you are calling this function.

   function Get (Iterator : Dependency_Iterator) return Prj.Project_Id;
   --  Return the current project the iterator is looking at.

   procedure Destroy (Iterator : in out Dependency_Iterator);
   procedure Destroy (Iterator : in out Dependency_Iterator_Access);
   --  Free the memory occupied by the iterator.

   ----------------------------
   -- Subprograms parameters --
   ----------------------------

   type Subprogram_Iterator is private;

   function Get_Subprogram_Parameters
     (Lib_Info   : LI_File_Ptr;
      Subprogram : Entity_Information)
      return Subprogram_Iterator;
   --  Return an iterator that will get all the parameters associated with the
   --  subprogram.
   --  If Subprogram doesn't have any, or isn't a subprogram, the iterator will
   --  not return any value.
   --
   --  Lib_Info must already have been parsed (through Create_Or_Complete_LI)

   procedure Next (Iterator : in out Subprogram_Iterator);
   --  Move to the next parameter

   function Get (Iterator : Subprogram_Iterator) return Entity_Information;
   --  Return the current parameter.
   --  The returned value must be freed by the user
   --  No_Entity_Information is returned if there are no more parameters

   type Parameter_Type is
     (In_Parameter,
      Out_Parameter,
      In_Out_Parameter,
      Access_Parameter);

   function Get_Type (Iterator : Subprogram_Iterator) return Parameter_Type;
   --  Return information on how the parameter is passed to the subprogram.

   function Image (Param : Parameter_Type) return String;
   --  Return a string suitable for display for the parameter type.

   ----------------
   -- Scope tree --
   ----------------

   type Scope_Tree_Node is private;
   Null_Scope_Tree_Node : constant Scope_Tree_Node;

   function Create_Tree (Lib_Info : LI_File_Ptr) return Scope_Tree;
   --  Create a new scope tree from an already parsed Library information.
   --  Note that the resulting tree needs to be freed whenever Lib_Info
   --  changes, since the tree points to internal nodes of Lib_Info.
   --
   --  Consider using Glide_Kernel.Get_Scope_Tree instead, since it will parse
   --  the right source file for an entity.

   procedure Free (Tree : in out Scope_Tree);
   --  Free the memory occupied by Tree.

   procedure Trace_Dump
     (Handler              : Traces.Debug_Handle;
      Tree                 : Scope_Tree;
      Node                 : Scope_Tree_Node := Null_Scope_Tree_Node;
      Subprograms_Pkg_Only : Boolean := True);
   --  Dump the contentns of the tree to standard_output.

   function Find_Entity_Scope
     (Tree : Scope_Tree; Entity : Entity_Information) return Scope_Tree_Node;
   --  Return the declaration node for the entity Name that is referenced
   --  at position Line, Column.

   type Node_Callback is access procedure
     (Node : Scope_Tree_Node; Is_Renaming : Boolean);
   --  Called for each node matching a given criteria.
   --  Is_Renaming is set to true if Node is not a direct reference to the
   --  entity, but the declaration of an entity that is a renaming of the
   --  entity.
   --  Node is a reference to the entity that was search, except when
   --  Is_Renaming is True, in which case this is a reference to the renaming
   --  entity.

   procedure Find_Entity_References
     (Tree : Scope_Tree;
      Entity : Entity_Information;
      Callback : Node_Callback);
   --  Search all the references to the entity Decl in the tree

   function Get_Parent (Node : Scope_Tree_Node) return Scope_Tree_Node;
   --  Return the parent of Node, or Null_Scope_Tree_Node if there is no
   --  parent.

   function Is_Subprogram (Node : Scope_Tree_Node) return Boolean;
   --  Return True if Node is associated with a subprogram (either its
   --  declaration or a call to it).

   function Is_Label (Node : Scope_Tree_Node) return Boolean;
   --  Return True if Node is a label, ie is not part of a scope (for instance,
   --  in Ada:
   --      loop
   --         declare
   --            A : Integer;
   --  A would be inside a Scope_Tree_Node for "loop", which we don't want to
   --  show in full names).

   function Is_Declaration (Node : Scope_Tree_Node) return Boolean;
   --  Return True if the node is a declaration (as opposed to a reference).

   function Get_Entity (Node : Scope_Tree_Node) return Entity_Information;
   --  Return the information for the entity defined in Node.
   --  You must call Destroy on the returned information.

   function Get_Reference (Node : Scope_Tree_Node) return E_Reference;
   --  Return the reference that Node represents (ie the line/column for this
   --  node in the source file).
   --  If the node is in fact a declaration, this returns the line/column for
   --  the declaration

   function Get_Kind (Node : Scope_Tree_Node) return E_Kind;
   --  Return the type of the entity pointed to by Node.

   function Is_Renaming_Of
     (Entity : Entity_Information; Node : Scope_Tree_Node) return Boolean;
   --  Return True if Node points to an entity that is a renaming of Entity.

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

   function Get_Declaration
     (List : LI_File_List; Entity : Entity_Information) return E_Declaration;
   --  Return the declaration matching Entity, from the LI file and source file
   --  that contains that declaration.
   --  No_Declaration is returned if it wasn't found.

   function Get_Declaration
     (Location : File_Location; Entity_Name : String := "")
      return E_Declaration;
   --  Return the declaration of the entity declaration at Location.
   --  If Entity_Name is not the empty string, then this declaration must match
   --  Entity_Name, or No_Declaration is returned.

   function Get_Source_File
     (List : LI_File_List; Entity : Entity_Information) return Source_File;
   --  Return the source file for the declaration of Entity.

   function Get_Declaration_Location
     (List : LI_File_List; Entity : Entity_Information) return File_Location;
   --  Return the location of the declaration of Entity

   function Get_Declaration
     (List : E_Declaration_Info_List;
      Decl_Line, Decl_Column : Natural; Entity_Name : String := "")
      return E_Declaration_Info_List;
   --  Return the declaration in List that matches entity.
   --  If Entity_Name is not the empty string, then the declaration must match
   --  Entity_Name, or null is returned.

   function Get_Entity (Decl : E_Declaration) return Entity_Information;
   --  Return the entity declared at Decl.
   --  You must call Entity_Information on the returned entity

   type Dependency is record
      File : Src_Info.Internal_File;
      Dep  : Src_Info.Dependency_Info;
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
      Scope       : E_Scope;
      Kind        : E_Kind;
   end record;
   --  If Decl_File is null, this is one of the predefined entities for its
   --  language.

   No_Entity_Information : constant Entity_Information :=
     (null, 1, 0, null, Global_Scope, Unresolved_Entity_Kind);

   type Scope_Node;
   type Scope_List is access Scope_Node;
   type Scope_Node (Typ : Scope_Type) is record
      Sibling : Scope_List;
      Parent  : Scope_List;
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

   type Scope_List_Array is array (Natural range <>) of Scope_List;
   type Scope_List_Array_Access is access Scope_List_Array;
   procedure Free is new Unchecked_Deallocation
     (Scope_List_Array, Scope_List_Array_Access);

   type Scope_Tree is record
      Lib_Info    : LI_File_Ptr;
      LI_Filename : String_Access;
      Time_Stamp  : Timestamp;
      --  For efficiency, we keep an access to the LI file that was used to
      --  create the tree. However, we also keep the file name itself, so that
      --  we can check whether the LI file was updated, and the tree is no
      --  longer valid.

      Body_Tree : Scope_List;
      Spec_Tree : Scope_List;
      Separate_Trees : Scope_List_Array_Access;
      --  The information for the source files associated with Lib_Info.
   end record;
   --  This tree represents the global scope information for the files
   --  associated with Lib_Info (spec, body and separate).

   type Scope_Tree_Node is new Scope_List;
   type Scope_Tree_Node_Iterator is new Scope_List;

   Null_Scope_Tree_Node : constant Scope_Tree_Node := null;

   Null_Scope_Tree : constant Scope_Tree :=
     (Lib_Info       => null,
      LI_Filename    => null,
      Time_Stamp     => 0,
      Body_Tree      => null,
      Spec_Tree      => null,
      Separate_Trees => null);

   procedure Free_Boolean (X : in out Boolean);
   --  Free memory associated to X.

   package Name_Htable is new String_Hash (Boolean, Free_Boolean, False);

   type Analyzed_Part is
     (None, Unit_Spec, Unit_Body, Unit_Separate, Unit_Dependency);

   type Dependency_Iterator is record
      Decl_LI : LI_File_Ptr;
      --  The file we are looking for.

      Source_Filename : String_Access;
      --  Name of the source file that we are examining.

      Importing : Prj_API.Project_Id_Array_Access;
      --  List of projects to check

      Current_Project : Natural;
      --  The current project in the list above

      Examined     : Name_Htable.String_Hash_Table.HTable;
      --  List of source files in the current project that have already been
      --  examined.

      Source_Files : Basic_Types.String_Array_Access;
      --  The list of source files in the current project

      Current_File : Natural;
      --  The current source file

      Current_Decl : Dependency_File_Info_List;
      --  The current declaration

      Include_Self : Boolean;
      --  Whether we should return the LI file for Decl_LI

      LI : LI_File_Ptr;

      Current_Separate : File_Info_Ptr_List;
      --  The current separate in case LI is Decl_LI (since bodies depend on
      --  separates). If null, the last returned value was the spec, otherwise
      --  it was Current_Separate

      Current_Part : Unit_Part;
      --  The part of LI that was returned

      LI_Once : Boolean;
      --  True if only one source file per LI should be returned.
   end record;

   type Entity_Reference_Iterator is record
      Entity    : Entity_Information;
      Decl_Iter : Dependency_Iterator;

      References : E_Reference_List;
      --  The current list of references we are processing.

      LI_Once : Boolean;
      --  True if we should return only one reference per LI file

      Part : Analyzed_Part := None;
      Current_Separate : File_Info_Ptr_List;
      Current_Dependency : Dependency_File_Info_List;
      --  If the LI file we are examining is the file in which the entity was
      --  declared, we need to examine the body, spec, and separates, and part
      --  indicates which part we are examining. Otherwise, an entity declared
      --  in the body cannot be seen from other files, so we don't need to
      --  analyze them.
   end record;

   type Entity_Declaration_Iterator is record
      Current_Dep : Dependency_File_Info_List;
      Lib_Info    : LI_File_Ptr;
      --  The LI file we are parsing

      Entity_Name : String_Access;
      --  The name of the entity we are looking for

      Current     : E_Declaration_Info_List;
      --  The current declaration.

      Part        : Analyzed_Part;
      Sep_Source  : File_Info_Ptr_List;
      --  The source file we are parsing.

      Uniq_List   : Boolean;
      --  If True, the search will stop as soon as Current becomes null. No
      --  search will be done in other files
   end record;

   type Subprogram_Iterator is record
      Lib_Info    : LI_File_Ptr;
      Current     : E_Reference_List;
   end record;

   type Parent_Iterator is record
      Lib_Info    : LI_File_Ptr;
      Current     : File_Location_List;
   end record;

   type Primitive_Iterator is record
      Lib_Info    : LI_File_Ptr;
      Current     : E_Reference_List;
   end record;

   pragma Inline (File_Information);
   pragma Inline (Dependency_Information);
   pragma Inline (Is_Label);
end Src_Info.Queries;
