-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
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
--  independent of the kernel (e.g take explicit an source_path, instead of a
--  handle to the kernel), so that it can eventually be integrated directly
--  into the sources of Gnat and its tools.

with Traces;
with Language_Handlers;
with Projects;
with String_Hash;

package Src_Info.Queries is

   type E_Kind_Set is array (E_Kinds) of Boolean;
   pragma Pack (E_Kind_Set);
   --  General type to implement sets of E_Kind

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
   type Entity_Information_Array_Access is access
     Entity_Information_Array;

   procedure Destroy (Entity : in out Entity_Information);
   --  Free the memory associated with the entity;

   function Get_Name (Entity : Entity_Information) return String;
   --  Return the name of the entity associated with Node. The returned string
   --  is UTF8-encoded

   function Get_Declaration_Line_Of
     (Entity : Entity_Information) return Positive;
   function Get_Declaration_Column_Of
     (Entity : Entity_Information) return Natural;
   function Get_Declaration_File_Of
     (Entity : Entity_Information) return VFS.Virtual_File_Access;
   --  Return the location of the declaration for Entity. Note that this
   --  location remains valid only until the source file are changed. It is not
   --  magically updated when the source file is changed.
   --  The declaration file might be the empty string if the exact location for
   --  the declaration could not be resolved (case of overloaded entities, or
   --  predefined entities)
   --  The file is set to VFS.No_File if this is a predefined entity, although
   --  it is better to check with Is_Predefined_Entity

   function Is_Declaration_File_Of
     (Entity : Entity_Information; File : VFS.Virtual_File) return Boolean;
   --  Return True if Entity is declared in File.

   function Get_Kind (Entity : Entity_Information) return E_Kind;
   --  Return the kind of the entity. See glide_kernel.ads on how to convert
   --  this to a string.

   function Is_Subtype
     (Decl_File : LI_File_Ptr;
      Entity    : Entity_Information) return Boolean;
   --  Return true if Entity is a subtype, as opposed to a type in Ada.

   function Is_Predefined_Entity (Entity : Entity_Information) return Boolean;
   --  Return True if the entity is one of the predefined entities for the
   --  language, ie there are no declaration for it in the user's code (Like
   --  Integer in Ada, or int in C).

   function Is_Subprogram (Entity : Entity_Information) return Boolean;
   --  Return True if Entity is a subprogram (procedure, function,...)

   function Is_Container (Entity : Entity_Information) return Boolean;
   --  Return True if Entity may contain calls or declarations of other
   --  entities (packages, namespaces, subprograms,...)

   function Get_Scope (Entity : Entity_Information) return E_Scope;
   --  Return the scope of the entity.  See glide_kernel.ads on how to convert
   --  this to a string.

   function Get_Full_Name
     (Entity    : Entity_Information;
      Decl_File : LI_File_Ptr;
      Separator : String := ".") return String;
   --  Compute the fully qualified name of the entity. For an entity defined in
   --  the function F of package P, the name would
   --     "P" & Separator & "F" & Separator & Get_Name (Entity)
   --  Decl_File must be the file in which the entity is declared
   --  Scope, if provided, is the scope tree corresponding to Decl_File.

   function Copy (Entity : Entity_Information) return Entity_Information;
   --  Return a copy of Entity. The result must be explicitely destroyed.

   function Create
     (File   : VFS.Virtual_File;
      Line   : Positive;
      Column : Natural;
      Name   : String;
      Scope  : E_Scope;
      Kind   : E_Kind) return Entity_Information;
   --  Return a new entity information structure. It is the responsability of
   --  the user to free the allocated memory.
   --  If File is the empty string, the entity will be considered as a
   --  predefined entity.

   function Create_Predefined_Entity (Name : String; Kind : E_Kind)
      return Entity_Information;
   --  Return a new predefined entity for Name.

   procedure Renaming_Of
     (Handler        : access LI_Handler_Record'Class;
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

   function Is_Discriminant
     (Discr            : Entity_Information;
      Lib_Info_For_Typ : LI_File_Ptr;
      Typ              : Entity_Information) return Boolean;
   --  Whether Discr is a discriminant of Type

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
      File_Name     : VFS.Virtual_File;
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
      File_Name              : VFS.Virtual_File;
      Entity_Name            : String;
      Line                   : Positive;
      Column                 : Positive;
      Handler                : access LI_Handler_Record'Class;
      Project                : Projects.Project_Type;
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
     (Lib_Info        : LI_File_Ptr;
      Source_Filename : VFS.Virtual_File) return VFS.Virtual_File_Access;
   --  Return the name of the spec or body for Source_Filename.
   --  If Source_Filename is a separate, then the spec of the unit is returned.
   --  VFS.No_File is returned if there is no other file (for instance, a
   --  body without a spec).
   --  Only the short path name is returned.
   --
   --  This method is based on LI files.
   --  See also Projects.Other_File_Name for a method based on naming schemes

   ------------------
   -- Declarations --
   ------------------

   type Entity_Declaration_Iterator is private;

   function Find_All_Possible_Declarations
     (Lib_Info       : LI_File_Ptr;
      Entity_Name    : String := "";
      In_Source_File : VFS.Virtual_File := VFS.No_File)
      return Entity_Declaration_Iterator;
   --  Return the first entity declaration in Lib_Info or its imported units
   --  whose name is Entity_Name. Note that the fake declarations for
   --  unresolved overloaded entities (with E_Kind = Overloaded_Entity) are not
   --  returned.  The entity is search in all source files associated with
   --  Lib_Info and its dependencies
   --  If Entity_Name is the empty string, all entities declared in Lib_Info
   --  will be returned.
   --  If In_Source_File is not the empty string, the search is limited to that
   --  file and doesn't include the dependencies.

   function Get
     (Iterator : Entity_Declaration_Iterator) return Entity_Information;
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

   type File_Error_Reporter_Record is abstract tagged null record;
   type File_Error_Reporter is access all File_Error_Reporter_Record'Class;
   procedure Error
     (Report : in out File_Error_Reporter_Record; File : VFS.Virtual_File)
     is abstract;
   --  Used to report errors while parsing files

   procedure Find_All_References
     (Root_Project           : Projects.Project_Type;
      Lang_Handler           : Language_Handlers.Language_Handler;
      Entity                 : Entity_Information;
      Iterator               : out Entity_Reference_Iterator;
      Project                : Projects.Project_Type := Projects.No_Project;
      LI_Once                : Boolean := False;
      File_Has_No_LI_Report  : File_Error_Reporter := null;
      In_File                : VFS.Virtual_File := VFS.No_File);
   --  Find all the references to the entity described in Decl. It doesn't
   --  return the declaration location for the entity however.
   --
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
   --  Source files with no LI file are reported through File_Has_No_LI_Report
   --
   --  You must destroy the iterator when you are done with it, to avoid memory
   --  leaks.

   procedure Next
     (Lang_Handler : Language_Handlers.Language_Handler;
      Iterator     : in out Entity_Reference_Iterator);
   --  Get the next reference to the entity

   function Get (Iterator : Entity_Reference_Iterator) return E_Reference;
   --  Return the reference currently pointed to. No_Reference is returned if
   --  there are no more reference.

   function Get_LI (Iterator : Entity_Reference_Iterator) return LI_File_Ptr;
   --  Return the current LI file

   function Get (Iterator : Entity_Reference_Iterator)
      return Projects.Project_Type;
   --  Return the current project the iterator is looking at.

   procedure Destroy (Iterator : in out Entity_Reference_Iterator);
   procedure Destroy (Iterator : in out Entity_Reference_Iterator_Access);
   --  Free the memory occupied by the iterator.

   function Get_Total_Progress
     (Iterator : Entity_Reference_Iterator) return Natural;
   function Get_Current_Progress
     (Iterator : Entity_Reference_Iterator) return Natural;
   --  The two functions above return a progress indicator, indicating the
   --  current state of the search. This is based on the number of files, not
   --  on the number of entities.

   ----------------------
   -- Local references --
   ----------------------

   type Local_Entities_Iterator is private;

   function Find_All_References_In_File
     (Lib_Info    : LI_File_Ptr;
      Source_File : VFS.Virtual_File) return Local_Entities_Iterator;
   --  Return the list of all entities referenced in Source_File, as well as
   --  all the locations at which they are referenced.
   --  This function is a lot more efficient than searching for all the
   --  entities with Find_All_Possible_Declarations, and then the lists of
   --  references with Find_All_References.
   --
   --  An example of usage is:
   --     Iter := Find_All_References_In_File (...);
   --     loop
   --        Ref := Get (Iter);
   --        if Ref = No_Reference then
   --           --  we have a new entity, analyze it
   --           Destroy (Current_Entity);
   --           Current_Entity := Get (Iter);
   --           exit when Current_Entity = No_Entity_Information;
   --        end if;
   --        Next (Iter);
   --     end loop;

   function Get (Iterator : Local_Entities_Iterator) return Entity_Information;
   --  Return the current entity.
   --  The returned value must be freed by the caller
   --  It is set to No_Entity_Information when there are no more entities in
   --  the file.

   function Get (Iterator : Local_Entities_Iterator) return E_Reference;
   --  Return the current reference
   --  Return No_Reference if the iterator is now working on a new entity.
   --  A reference will be returned for the declaration of an entity, if that
   --  declaration takes place in the file passed in argument to
   --  Find_All_References_In_File.

   procedure Next (Iterator : in out Local_Entities_Iterator);
   --  Move to the next reference

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
   No_Parent_Iterator : constant Parent_Iterator;

   function Get_Parent_Types
     (Lib_Info  : LI_File_Ptr;
      Entity    : Entity_Information;
      Recursive : Boolean := False) return Parent_Iterator;
   --  Return a pointer to the first parent type for the type Entity. In
   --  Object-oriented languages, this would be the classes Entity derives
   --  from. In Ada, this includes the parent type of a type or subtype

   procedure Next (Iter : in out Parent_Iterator);
   --  Move to the next parent of the entity

   function Get (Iter : Parent_Iterator) return Entity_Information;
   --  Return the current parent of the entity. No_Entity_Information is
   --  returned if there are no more parents

   procedure Destroy (Iter : in out Parent_Iterator);
   --  Free the memory occupied by the iterator

   --------------------
   -- Children types --
   --------------------

   type Child_Type_Iterator is private;

   function Get_Children_Types
     (Lib_Info : LI_File_Ptr; Entity : Entity_Information)
      return Child_Type_Iterator;
   --  Return the first type that derives from Entity in the files associated
   --  with Lib_Info. The iterator must be destroyed to prevent memory leaks.
   --  Entity must be destroyed by the caller

   procedure Next (Iter : in out Child_Type_Iterator);
   --  Move the next child type

   function Get (Iter : Child_Type_Iterator) return Entity_Information;
   --  Return the current child of the entity, or No_Entity_Information if
   --  there are no more children.

   procedure Destroy (Iter : in out Child_Type_Iterator);
   --  Free the memory used by the iterator.

   --------------------------------------------------
   -- Methods, primitive operations, discriminants --
   --------------------------------------------------

   type Special_Iterator is private;
   subtype Primitive_Iterator    is Special_Iterator;
   subtype Discriminant_Iterator is Special_Iterator;

   function Get_Primitive_Operations
     (Lib_Info          : LI_File_Ptr;
      Entity            : Entity_Information;
      Include_Inherited : Boolean) return Primitive_Iterator;
   --  Return the first primitive operation for the type Entity. This will not
   --  return anything if Entity is a variable.

   function Get_Discriminants
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Discriminant_Iterator;
   --  Return all discriminants of Entity. This is only relevant for Ada
   --  entities currently.
   --  Returned value must be freed by the user

   function Get (Iter : Special_Iterator) return Entity_Information;
   --  Return the current entity, or No_Entity_Information if there are no
   --  more.
   --  The returned entity must be freed by the user.

   procedure Next (Iter : in out Special_Iterator);
   --  Move the next entity.

   procedure Destroy (Iter : in out Special_Iterator);
   --  Free the memory occupied by Iter

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

   function Pointed_Type
     (Lib_Info   : LI_File_Ptr;
      Access_Type : Entity_Information) return Entity_Information;
   --  Return the type of data pointed to by a pointer type.

   function Array_Contents_Type
     (Lib_Info   : LI_File_Ptr;
      Array_Type : Entity_Information) return Entity_Information
      renames Pointed_Type;
   --  Return the type of data contained in an array type.

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
      Source_Filename : VFS.Virtual_File;
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
     (Root_Project          : Projects.Project_Type;
      Lang_Handler          : Language_Handlers.Language_Handler;
      Source_Filename       : VFS.Virtual_File;
      Iterator              : out Dependency_Iterator;
      Project               : Projects.Project_Type := Projects.No_Project;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Include_Self          : Boolean := False;
      LI_Once               : Boolean := False;
      Indirect_Imports      : Boolean := False;
      Single_Source_File    : Boolean := False);
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
   --  Sources file with no LI file are reported through File_Has_No_LI_Report
   --  if set.
   --
   --  You must destroy the iterator when you are done with it, to avoid memory
   --  leaks.
   --
   --  If Indirect_Imports is true, then files that indirectly import
   --  Source_Filename are also returned. In this case, one of the Get
   --  subprogram is not callable.

   procedure Next
     (Lang_Handler : Language_Handlers.Language_Handler;
      Iterator : in out Dependency_Iterator);
   --  Get the next reference to the entity

   function Get (Iterator : Dependency_Iterator) return Dependency;
   --  Return the file pointed to. You must free the returned value.
   --  This function is not callable if you set Indirect_Imports to True in the
   --  call to Find_Ancestor_Dependencies.

   function Get (Iterator : Dependency_Iterator) return LI_File_Ptr;
   --  Return the LI for the file that contains the dependency. Note that this
   --  is not the LI file for Dependency, as returned by Get.
   --  Consider setting LI_Once to True when calling Find_Ancestor_Dependencies
   --  if you are calling this function.

   function Get (Iterator : Dependency_Iterator) return Projects.Project_Type;
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

   procedure Trace_Tree_Dump
     (Handler              : Traces.Debug_Handle;
      Lib_Info             : in out LI_File_Ptr;
      Node                 : Scope_Tree_Node := Null_Scope_Tree_Node;
      Subprograms_Pkg_Only : Boolean := True);
   --  Dump the contents of the tree to standard_output.

   function Find_Entity_Scope
     (Lib_Info : LI_File_Ptr; Entity : Entity_Information)
      return Scope_Tree_Node;
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
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information;
      Callback : Node_Callback);
   --  Search all the references to the entity Decl in the tree

   function Get_Parent (Node : Scope_Tree_Node) return Scope_Tree_Node;
   --  Return the parent of Node, or Null_Scope_Tree_Node if there is no
   --  parent.

   function Is_Subprogram (Node : Scope_Tree_Node) return Boolean;
   --  Return True if Node is associated with a subprogram (either its
   --  declaration or a call to it).

   function Is_Container (Node : Scope_Tree_Node) return Boolean;
   --  Return True if Node is associated with a container (see Is_Container
   --  above).

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
     (Handler : access LI_Handler_Record'Class;
      Entity  : Entity_Information) return E_Declaration;
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
     (Handler : access LI_Handler_Record'Class;
      Entity  : Entity_Information) return Source_File;
   --  Return the source file for the declaration of Entity. Return value
   --  must be freed by the caller.

   function Get_Declaration_Location
     (Handler : access LI_Handler_Record'Class;
      Entity  : Entity_Information) return File_Location;
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

   type Entity_Information is record
      Name        : String_Access;
      Decl_Line   : Positive;
      Decl_Column : Natural;
      Decl_File   : aliased VFS.Virtual_File;
      Scope       : E_Scope;
      Kind        : E_Kind;
   end record;
   --  If Decl_File is null, this is one of the predefined entities for its
   --  language.

   No_Entity_Information : constant Entity_Information :=
     (null, 1, 0, VFS.No_File, Global_Scope, Unresolved_Entity_Kind);

   procedure Free_Boolean (X : in out Boolean);
   --  Free memory associated to X.

   package Name_Htable is new String_Hash (Boolean, Free_Boolean, False);

   type Analyzed_Part is
     (None, Unit_Spec, Unit_Body, Unit_Separate, Unit_Dependency);

   type Dependency_Iterator is record
      Decl_LI : LI_File_Ptr;
      --  The file we are looking for.

      Source_Filename : VFS.Virtual_File;
      --  Name of the source file that we are examining.

      Importing : Projects.Imported_Project_Iterator;
      --  List of projects to check

      Examined     : Name_Htable.String_Hash_Table.HTable;
      --  List of source files in the current project that have already been
      --  examined.

      Source_Files : VFS.File_Array_Access;
      --  The list of source files in the current project

      Current_File : Natural;
      --  The current source file

      Current_Decl : Dependency_File_Info_List;
      --  The current declaration

      Include_Self : Boolean;
      --  Whether we should return the LI file for Decl_LI

      Total_Progress   : Natural;
      Current_Progress : Natural;

      LI : LI_File_Ptr;

      File_Has_No_LI_Report  : File_Error_Reporter := null;

      Current_Separate : File_Info_Ptr_List;
      --  The current separate in case LI is Decl_LI (since bodies depend on
      --  separates). If null, the last returned value was the spec, otherwise
      --  it was Current_Separate

      Current_Part : Projects.Unit_Part;
      --  The part of LI that was returned

      LI_Once : Boolean;
      --  True if only one source file per LI should be returned.

      Indirect_Imports : Boolean;
      --  True if the iterator should also return true for indirect imports of
      --  Source_Filename.
   end record;

   type Local_Entities_Iterator is record
      Current_Decl : E_Declaration_Info_List;
      Reference : E_Reference_List;
      File      : VFS.Virtual_File;
      New_Decl  : Boolean;
      LI        : LI_File_Ptr;

      Part               : Analyzed_Part := None;
      Current_Separate   : File_Info_Ptr_List;
      Current_Dep        : Dependency_File_Info_List;
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

   type File_Location_Array is array (Natural range <>) of File_Location_List;
   type File_Location_Array_Access is access File_Location_Array;

   type Parent_Iterator is record
      Lib_Info    : LI_File_Ptr;
      Parents     : File_Location_Array_Access;
      Current     : Natural;
   end record;

   No_Parent_Iterator : constant Parent_Iterator := (null, null, 0);

   type Child_Type_Iterator is record
      Lib_Info    : LI_File_Ptr;
      Part        : Projects.Unit_Part;
      File        : File_Info_Ptr_List;
      Entity      : Entity_Information;
      Current     : E_Declaration_Info_List;
   end record;

   type Special_Iterator is record
      Kind        : Reference_Kind;
      Lib_Info    : LI_File_Ptr;
      Current     : E_Reference_List;

      Processing_Parents : Boolean;
      Parent_Iter       : Parent_Iterator;
      --  The parent entities to examine. This is No_Parent_Iterator if we are
      --  not returning the inherited subprograms
   end record;

   type Scope_Tree_Node_Iterator is new Scope_List;

   procedure Create_Tree (Lib_Info : LI_File_Ptr);
   --  Create a new scope tree from an already parsed Library information.
   --  Note that the resulting tree needs to be freed whenever Lib_Info
   --  changes, since the tree points to internal nodes of Lib_Info.
   --
   --  If Declarations_Only is true, then only declarations are inserted into
   --  the tree, no reference.

   pragma Inline (Is_Container);
   pragma Inline (Get_Scope);
   pragma Inline (File_Information);
   pragma Inline (Dependency_Information);
   pragma Inline (Is_Label);
end Src_Info.Queries;
