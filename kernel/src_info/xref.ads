------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package provides high-level cross-reference operations.
--  This should be the preferred entry point for accessing entity
--  information from Ada/C/C++ code.
--
--  This package should be usable for other IDEs like GNATBench. GPS-specific
--  features should go to GPS.Kernel.Xref.

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Basic_Types;            use Basic_Types;
with GNATCOLL.Projects;
with GNATCOLL.Symbols;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.Xref;          use GNATCOLL.Xref;
with GNAT.Strings;
with Old_Entities;
with Old_Entities.Queries;   use Old_Entities.Queries;
with Projects;
with Language_Handlers;
with Language.Tree.Database;

package Xref is

   SQLITE : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("Entities.SQLITE", GNATCOLL.Traces.On);
   --  Whether to use the sqlite-based cross-reference system

   ---------------
   --  Entities
   ---------------

   type General_Entity is private;
   No_General_Entity : aliased constant General_Entity;
   --  aliased is added to let AJIS make it accessible to GNATbench

   overriding function "=" (E1, E2 : General_Entity) return Boolean;
   --  Whether the two entities are the same

   type Entity_Array is array (Natural range <>) of General_Entity;

   -----------
   --  The sqlite-based database
   -----------

   type Extended_Xref_Database is new GNATCOLL.Xref.Xref_Database
      with private;
   type Extended_Xref_Database_Access is
     access all Extended_Xref_Database'Class;
   --  This database overrides a number of subprograms so that we use
   --  either the constructs database or the LI database.
   --  It is further extended (GPS-specific) in GPS.Kernel.Xref.

   ---------------------
   --  The set of all databases
   ---------------------

   type General_Xref_Database_Record is tagged record
      Entities   : aliased Old_Entities.Entities_Database;
      --  The "legacy" LI database
      --  aliased is added to let AJIS make this field accessible to GNATbench

      Xref       : Extended_Xref_Database_Access;
      --  The "new" LI database

      Constructs : Language.Tree.Database.Construct_Database_Access;
      --  The constructs database

      Lang_Handler : Language.Tree.Database.Abstract_Language_Handler;
      --  The type used to convert from file names to languages. This is used
      --  by the constructs database.

      Symbols : GNATCOLL.Symbols.Symbol_Table_Access;

   end record;
   type General_Xref_Database is access all General_Xref_Database_Record'Class;

   procedure Destroy (Self : in out General_Xref_Database);
   --  Destroy the xref database (in memory)

   procedure Reset (Self : access General_Xref_Database_Record);
   --  Empty the contents of the xref database.

   procedure Initialize
     (Self         : access General_Xref_Database_Record;
      Lang_Handler :
         access Language.Tree.Database.Abstract_Language_Handler_Record'Class;
      Symbols      : GNATCOLL.Symbols.Symbol_Table_Access;
      Registry     : Projects.Project_Registry_Access;
      Subprogram_Ref_Is_Call : Boolean := False);
   --  Initialize various internal fields for the constructs. It is assumed
   --  that the xref and LI databases have already been initialized.
   --  It is possible to pre-allocate Xref and/or Entities database if you want
   --  specific instances to be used, instead of the default ones.
   --
   --  Subprogram_Ref_Is_Call should be True for old GNAT versions, which were
   --  using 'r' for subprogram calls, instead of 's' in more recent versions.

   function Select_Entity_Declaration
     (Self   : access General_Xref_Database_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Entity : General_Entity) return General_Entity;
   --  The user has requested a xref, but the information is not up-to-date and
   --  there is an ambiguity. This function is responsible for asking the user
   --  to chose among all the homonym entities in File.
   --  Entity is set initially to the best choice, and its name is the one
   --  that should be looked for in the file.
   --  Should return No_General_Entity if the user has cancelled the action.

   -----------------------
   --  File location
   -----------------------

   type General_Location is record
      File   : aliased Virtual_File := No_File;
      --  aliased is added to let AJIS make this field accessible to GNATbench
      Line   : Integer := 0;
      Column : Visible_Column_Type := 0;
   end record;
   No_Location : aliased constant General_Location := (No_File, 0, 0);
   --  ??? Should we also cache the Old_Entities.Source_File ?
   --  aliased is added to let AJIS make it accessible to GNATbench

   type General_Entity_Declaration is record
      Loc  : aliased General_Location;
      --  aliased is added to let AJIS make this field accessible to GNATbench
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Body_Is_Full_Declaration : Boolean;
   end record;
   No_General_Entity_Declaration : constant General_Entity_Declaration;
   --  Various pieces of information computed for the entity, including the
   --  location of its declaration.

   --  Entity references

   type General_Entity_Reference is private;
   No_General_Entity_Reference : aliased constant General_Entity_Reference;
   --  aliased is added to let AJIS make it accessible to GNATbench

   overriding function "="
     (Ref1, Ref2 : General_Entity_Reference) return Boolean;
   --  Whether the two references point to the same location.

   -----------
   -- Files --
   -----------

   function Is_Up_To_Date
     (Self : access General_Xref_Database_Record;
      File : Virtual_File) return Boolean;
   --  Whether the xref information for this file is up-to-date in the
   --  database.

   type File_Iterator is tagged private;
   type File_Iterator_Access is access File_Iterator'Class;
   function Has_Element (Iter : File_Iterator) return Boolean;
   procedure Next (Iter : in out File_Iterator);
   function Element (Iter : File_Iterator) return GNATCOLL.VFS.Virtual_File;

   procedure Destroy (Iter : in out File_Iterator_Access);
   procedure Destroy (Iter : in out File_Iterator);
   --  ??? Only because of the old LI database

   function Find_Dependencies
     (Self : access General_Xref_Database_Record'Class;
      File : GNATCOLL.VFS.Virtual_File) return File_Iterator;
   --  Return the list of files that File depends on.

   function Find_Ancestor_Dependencies
     (Self                  : access General_Xref_Database_Record'Class;
      File                  : GNATCOLL.VFS.Virtual_File) return File_Iterator;
   --  Return the list of files that depend on File. The rule is the following:
   --    - bodies, specs and separates always depend on each other

   ---------------------------
   -- High level operations --
   ---------------------------
   --  These functions provide high-level facilities not exposing the
   --  entities backend, and are back-end independent.
   --
   --  Some operations might even query using one back-end, then fall back
   --  on a less precise back-end if the first query is not precise enough.

   type Reference_Kind_Filter is access function
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;

   function Reference_Is_Body
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Reference_Is_Declaration
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Real_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Real_Or_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Read_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Write_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Read_Or_Write_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Read_Or_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   function Is_Read_Or_Write_Or_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;
   --  Various filters for references.
   --  Some references kinds are special, and mark for instance the end of
   --  scope. They should not in general be visible to users. One special kind
   --  are implicit references.

   procedure For_Each_Dispatching_Call
     (Dbase     : access General_Xref_Database_Record;
      Entity    : General_Entity;
      Ref       : General_Entity_Reference;
      On_Callee : access function
                    (Callee, Primitive_Of : General_Entity) return Boolean;
      Filter    : Reference_Kind_Filter := null;
      Policy    : Basic_Types.Dispatching_Menu_Policy);
   --  If Ref references a dispatching call then call On_Callee with all the
   --  overridding primitives (that is, all the primitives that might possibly
   --  be called instead of Entity). For example, if you have:
   --         procedure Dispatch (Self : Base'Class) is
   --         begin
   --            Proc (Self);
   --         end Dispatch;
   --  and call For_Each_Dispatching_Call on Proc, you will get the primitive
   --  operation of Base and all the overriding primitive ops of its children.
   --
   --  Filter can be used to make sure the entity has some specific type of
   --  reference. The most common use is to ensure that the entity does have
   --  a body (ie is not abstract), in which case the filter is set to
   --  Entity_Has_Body.
   --
   --  Search stops when On_Callee returns False
   --
   --  Nothing is done if Ref does not point to a dispatching call.
   --  This procedure does not propagate any exception.

   function Get_Entity
     (Db   : access General_Xref_Database_Record;
      Name : String;
      Loc  : General_Location) return General_Entity;
   --  Retrieve the entity referenced at the given location.
   --  This also works for operators, whether they are quoted ("=") or
   --  not (=).

   procedure Find_Declaration_Or_Overloaded
     (Self              : access General_Xref_Database_Record;
      Loc               : General_Location;
      Entity_Name       : String;
      Ask_If_Overloaded : Boolean := False;
      Entity            : out General_Entity;
      Closest_Ref       : out General_Entity_Reference);
   --  Similar to Get_Entity, but also returns the closest reference, and
   --  handles interaction with the user if possible
   --
   --  Find the declaration of the given entity in the file.
   --  If Ask_If_Overloaded is True and there are several possible matches for
   --  the entity (for instance because the xref info is not up-to-date), an
   --  interactive dialog is opened.
   --
   --  This also works for operators, whether they are quoted ("=") or
   --  not (=).
   --
   --  Passing No_Location for Loc will search for predefined entities.

   function Get_Entity (Ref : General_Entity_Reference) return General_Entity;
   --  Return the entity the reference is pointing to

   function Is_Fuzzy (Entity : General_Entity) return Boolean;
   --  Whether the entity is just a guess (because the xref info generated by
   --  the compiler was not up-to-date).

   function Get_Name
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return String;
   --  Return the name of the entity

   function Get_Display_Kind
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return String;
   --  Return a general kind for the entity. This is not its type, more like a
   --  metaclass. Its exact value will vary depending on what was inserted in
   --  the database, and new language can insert new kinds at any time. So this
   --  string should only be used for display.

   function Qualified_Name
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return String;
   --  Return the fully qualified name of the entity

   function Hash
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return Integer;
   --  Return a hash code for the entity

   function Cmp
     (Self   : access General_Xref_Database_Record;
      Entity1, Entity2 : General_Entity) return Integer;
   --  Return -1, 0 or 1 to sort the two entities

   function Get_Location
     (Ref : General_Entity_Reference) return General_Location;
   --  Return the location of this reference

   function Get_Declaration
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity_Declaration;
   --  Return the location of the entity declaration

   function Caller_At_Declaration
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the entity whose scope contains the declaration of Entity

   function Get_Body
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity;
      After  : General_Location := No_Location) return General_Location;
   --  Return the location of the first body for this entity

   function Get_Type_Of
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the type of the entity

   function Returned_Type
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the type returned by a function.

   function Parent_Package
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the parent package (if Entity is a package itself)

   function Pointed_Type
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  The type pointed to by an access type. Returns null for a variable.

   function Renaming_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the entity that Entity renames (or No_General_Entity)

   function Is_Primitive_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Returns the entity for which Entity is a method/primitive operation

   function Has_Methods
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True if the entity might have methods

   function Is_Access
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True if E is a type or a variable, and it points to some other type.
   --  This is an Ada access type, an Ada access variable, a C pointer,...

   function Is_Abstract
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  Whether the entity is abstract (ie cannot be instantiated

   function Is_Array
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  Whether E is an array type or variable. This is mostly used in the
   --  debugger to find whether the user should be able to dereference the
   --  variable.

   function Is_Printable_In_Debugger
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  Whether we can execute a "print" on E in a debugger, and get a value
   --  that can be shown to the user.
   --  ??? We could perhaps try the command in gdb directly and guess from
   --  there

   function Is_Type
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True f E is a type (not a variable or a package for instance)

   function Is_Subprogram
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True if E is a subprogram

   function Is_Container
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True if E can contain other entities (a record, struct,...)

   function Is_Generic
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  Whether the entity is a 'generic' or 'template'

   function Is_Global
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  Whether the entity is a global entity (library-level in Ada)

   function Is_Static_Local
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  Whether the entity is a static, in the C/C++ sense.

   function Is_Predefined_Entity
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True if E is a predefined entity

   function Documentation
     (Self             : access General_Xref_Database_Record;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : General_Entity;
      Color_For_Optional_Param : String := "#555555";
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String;
   --  Return the documentation (tooltips,...) for the entity.
   --  If Raw_Format is False, the documentation is formated in HTML (using
   --  Color_For_Optional_Param to highlight optional parameters).
   --
   --  Check_Constructs should be False to disable the use of the constructs
   --  database.

   function End_Of_Scope
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Location;
   --  Return the end of scope location for this entity.
   --  ??? Not clear whether we return the end of scope for the spec or body.

   function Get_Caller
     (Ref : General_Entity_Reference) return General_Entity;
   --  Return the enclosing scope at the given location

   function Is_Parameter_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the subprogram for which entity is a parameter

   function Overrides
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  The entity that Entity overrides.

   function Instance_Of
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return General_Entity;
   --  Return the generic entity instantiated by Entity

   function Methods
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity;
       Include_Inherited : Boolean) return Entity_Array;
   --  The list of methods of an entity.

   function Fields
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array;
   --  The fields of an Ada record or C struct

   function Literals
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array;
   --  Return the literals of an enumeration

   function Formal_Parameters
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array;
   --  The formal parameters for a generic entity.

   function Discriminant_Of
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return General_Entity;
   --  Return the Ada record for which Entity is a discriminant

   function Discriminants
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array;
   --  Return the list of discriminants for the entity

   function Component_Type
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return General_Entity;
   function Index_Types
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return Entity_Array;
   --  Index and components types for an array

   function Child_Types
      (Self      : access General_Xref_Database_Record;
       Entity    : General_Entity;
       Recursive : Boolean) return Entity_Array;
   --  Return the list of types derived from Entity (in the type-extension
   --  sense).

   function Parent_Types
      (Self      : access General_Xref_Database_Record;
       Entity    : General_Entity;
       Recursive : Boolean) return Entity_Array;
   --  Return the list of types that Entity extends.

   function From_Instances
     (Self   : access General_Xref_Database_Record;
      Ref    : General_Entity_Reference) return Entity_Array;
   --  Indicates the instantiation chain for the given reference.
   --  If we have a nested generic, as in:
   --     generic package B is
   --          type T is new Integer;
   --     end B;
   --     generic package A is
   --        package BI is new B;
   --     end A;
   --     package AI is new A;
   --     C : AI.BI.T;
   --
   --  And we start from the reference to T on the last line, the array will
   --  contain BI:a.ads:2, then AI:...:1, since T is from the package BI (an
   --  instantiation of B), which itself is part of AI, an instantiation of A.
   --
   --  When you retrieve BI or AI, you can use Instance_Of to get access to
   --  resp. B and A.

   type General_Parameter is record
      Parameter : General_Entity;
      Kind      : GNATCOLL.Xref.Parameter_Kind;
   end record;
   type Parameter_Array is array (Natural range <>) of General_Parameter;

   function Parameters
     (Dbase  : access General_Xref_Database_Record;
      Entity : General_Entity) return Parameter_Array;
   --  Return the list of parameters for a given subprogram

   --------------------------------
   -- Entity and reference kinds --
   --------------------------------
   --  In the sqlite-backend, entity kinds are extensible by the user, and as
   --  such cannot be represented as an enumeration type on the Ada side. So
   --  instead we provide a number of subprograms to perform the expected
   --  tests on the kind.

   function Show_In_Callgraph
     (Db  : access General_Xref_Database_Record;
      Ref : General_Entity_Reference) return Boolean;
   --  Whether to show the corresponding entity in a callgraph.

   function Is_Dispatching_Call
     (Db  : access General_Xref_Database_Record;
      Ref : General_Entity_Reference) return Boolean;
   --  Whether ref is dispatching call for a subprogram

   function Get_Display_Kind
     (Ref  : General_Entity_Reference) return String;
   --  A displayable version of the type of reference. This should not be used
   --  for comparison purposes (better to use one of the Is_* subprograms).

   function All_Real_Reference_Kinds
     (Db  : access General_Xref_Database_Record)
      return GNAT.Strings.String_List;
   --  The list of all "Get_Display_Kind" for real references to entities.
   --  The returned value must be freed by the user.

   ----------------------
   -- Entities in file --
   ----------------------

   type Base_Entities_Cursor is tagged private;

   function At_End (Iter : Base_Entities_Cursor) return Boolean;
   function Get (Iter : Base_Entities_Cursor) return General_Entity;
   procedure Next (Iter : in out Base_Entities_Cursor);
   --  Iterate and retrieve an entity at each iteration

   type Entities_In_File_Cursor is new Base_Entities_Cursor with private;

   function Entities_In_File
     (Self   : access General_Xref_Database_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Name   : String := "") return Entities_In_File_Cursor;
   --  Return all entities referenced or declared in file.

   overriding function At_End (Iter : Entities_In_File_Cursor) return Boolean;
   overriding function Get
     (Iter : Entities_In_File_Cursor) return General_Entity;
   overriding procedure Next (Iter : in out Entities_In_File_Cursor);

   type Entities_In_Project_Cursor is new Base_Entities_Cursor with private;

   function All_Entities_From_Prefix
     (Self       : access General_Xref_Database_Record'Class;
      Prefix     : String;
      Is_Partial : Boolean := True) return Entities_In_Project_Cursor;
   --  Returns all entities in the project whose name starts with Prefix (if
   --  Is_Partial is True) or whose name is exactly Prefix (if Is_Partial is
   --  False).

   overriding function At_End
     (Iter : Entities_In_Project_Cursor) return Boolean;
   overriding function Get
     (Iter : Entities_In_Project_Cursor) return General_Entity;
   overriding procedure Next (Iter : in out Entities_In_Project_Cursor);

   procedure Destroy (Iter : in out Entities_In_Project_Cursor);

   ---------------
   -- Callgraph --
   ---------------

   type Calls_Iterator is new Base_Entities_Cursor with private;

   function Get_All_Called_Entities
     (Self   : access General_Xref_Database_Record'Class;
      Entity : General_Entity) return Calls_Iterator;
   --  Return all the entities that are found in the scope of Entity. This is
   --  not necessarily a subprogram call, but can be many things.
   --  All entities returned are unique. If you need to find the specific
   --  reference(s) to that entity, you'll need to search for the references in
   --  the right scope through the iterators above.

   overriding function At_End (Iter : Calls_Iterator) return Boolean;
   overriding function Get (Iter : Calls_Iterator) return General_Entity;
   overriding procedure Next (Iter : in out Calls_Iterator);
   --  Move to the next entity

   procedure Destroy (Iter : in out Calls_Iterator);
   --  Free the memory used by the iterator

   ----------------
   -- References --
   ----------------
   --  While we have the old LI engine, we keep an asynchronous iterator to
   --  find all references. This will no longer be needed when we only use the
   --  sqlite backend.

   type Entity_Reference_Iterator is private;
   type Entity_Reference_Iterator_Access
     is access all Entity_Reference_Iterator;

   procedure Find_All_References
     (Self                  : access General_Xref_Database_Record;
      Iter                  : out Entity_Reference_Iterator;
      Entity                : General_Entity;
      File_Has_No_LI_Report : Basic_Types.File_Error_Reporter := null;
      In_File              : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      In_Scope              : General_Entity := No_General_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "");
   --  Find all the references to the entity. This also return the location
   --  for the declaration of the entity.
   --  If In_File is specified, then only the references in that file will be
   --  returned. This is also more efficient. Alternatively, In_Scope can be
   --  specified to limit the list of references to the ones that appear
   --  in the scope of In_Scope.
   --  Source files with no LI file are reported through File_Has_No_LI_Report.
   --  You must destroy the iterator when you are done with it, to avoid
   --  memory leaks.
   --  If Include_Overriding or Include_Overridden are True, then all
   --  references to an overriding or Overriden subprogram will also be
   --  returned. If Entity is a parameter of subprogram A, this will also
   --  return the parameters of subprograms that override A.
   --
   --  If Include_Implicit is False, then implicit references will not be
   --  returned.
   --  If Include_All is True, then references like end-of-spec and other
   --  information on the entity will be returned.
   --  Kind can be used to filter the reference kinds that should be returned.
   --  If it is specified, Include_Implicit and Include_All are ignored. It is
   --  a list of comma-separated strings.

   subtype References_Sort is GNATCOLL.Xref.References_Sort;
   procedure Find_All_References
      (Self     : access General_Xref_Database_Record;
       Iter     : out Entity_Reference_Iterator;
       File     : GNATCOLL.VFS.Virtual_File;
       Kind     : String := "";
       Sort     : References_Sort := GNATCOLL.Xref.By_Location);
   --  Return references to all entities in the file, possibly filtering by
   --  entity kind.
   --  ??? This will always return an empty list when using the old xref
   --  engine.

   function At_End (Iter : Entity_Reference_Iterator) return Boolean;
   --  Whether there are no more reference to return

   procedure Next (Iter : in out Entity_Reference_Iterator);
   --  Move to the next reference to the entity

   function Get
     (Iter : Entity_Reference_Iterator) return General_Entity_Reference;
   --  Return the current reference. This might be No_Entity_Reference if the
   --  iterator needs to parse more source files to get that information.
   --  The search is done with small steps, so that this can be easily put in
   --  the background, including the parsing of the source files.

   function Get_Entity
     (Iter : Entity_Reference_Iterator) return General_Entity;
   --  Return the entity referenced at the current location. Most of the time,
   --  it will be the entity passed in argument to Find_All_Reference. However,
   --  if Is_Real_Reference is false, it might be a different one, such as
   --  the name of a discriminant or a subprogram parameter for instance

   procedure Destroy (Iter : in out Entity_Reference_Iterator);
   procedure Destroy (Iter : in out Entity_Reference_Iterator_Access);
   --  Free the memory used by Iter

   function Get_Current_Progress
     (Iter : Entity_Reference_Iterator) return Integer;
   function Get_Total_Progress
     (Iter : Entity_Reference_Iterator) return Integer;
   --  Return the progress indicators for the iterator

   -------------------------
   -- Life cycle handling --
   -------------------------

   procedure Ref (Entity : General_Entity);
   procedure Unref (Entity : in out General_Entity);
   --  Increase/Decrease the reference counter on entities.
   --  ??? This is needed only as long as the legacy system is in place.

   type Database_Lock is private;
   No_Lock : constant Database_Lock;

   function Freeze
     (Self : access General_Xref_Database_Record) return Database_Lock;
   procedure Thaw
     (Self : access General_Xref_Database_Record;
      Lock : in out Database_Lock);
   --  Freeze the update of the xref database

   -------------
   -- private --
   -------------
   --  The following subprograms should only be used from GPS.Kernel.Xref.

   function To_Old
     (Entity : General_Entity) return Old_Entities.Entity_Information;
   function From_Old
     (Entity : Old_Entities.Entity_Information) return General_Entity;
   --  Convert to or from an old format entity

   function To_New
     (Entity : General_Entity) return GNATCOLL.Xref.Entity_Information;
   function From_New
     (Entity : GNATCOLL.Xref.Entity_Information) return General_Entity;
   --  Convert to or from a new format entity

   function From_Constructs
     (Entity : Language.Tree.Database.Entity_Access) return General_Entity;

   function Get_Entity_Reference
     (Old_Ref : Old_Entities.Entity_Reference) return General_Entity_Reference;
   --  Return the associated general entity reference

   procedure Project_Changed (Self : General_Xref_Database);
   --  The project has changed, we need to reset the xref database. This is
   --  called at least once prior to calls to Project_View_Changed.
   --  At this stage, the view of the project hasn't been computed, so you can
   --  not do any query on the project itself.

   procedure Project_View_Changed
     (Self   : General_Xref_Database;
      Tree   : GNATCOLL.Projects.Project_Tree_Access);
   --  The view of the project has changed, we need to refresh the xref
   --  databases.

private
   type Extended_Xref_Database is new GNATCOLL.Xref.Xref_Database with
      null record;

   type General_Entity is record
      Old_Entity : Old_Entities.Entity_Information := null;
      Is_Fuzzy   : Boolean := False;  --  Whether this is a fuzzy match
      Entity     : GNATCOLL.Xref.Entity_Information := No_Entity;

      Loc        : General_Location := No_Location;
      --  The location which was used to query the entity. This is used to
      --  fall back on the Constructs database if Entity and Old_Entity are
      --  null.
   end record;
   No_General_Entity : aliased constant General_Entity :=
     (Old_Entity => null,
      Is_Fuzzy   => False,
      Loc        => No_Location,
      Entity     => No_Entity);
   --  We no longer store the Entity_Persistent_Access node in General_Entity,
   --  since we would need to also control the assignments to make sure its
   --  reference counting is properly incremented.

   type General_Entity_Reference is record
      Old_Ref : Old_Entities.Entity_Reference :=
        Old_Entities.No_Entity_Reference;
      Ref : Entity_Reference := No_Entity_Reference;
   end record;
   No_General_Entity_Reference : aliased constant General_Entity_Reference :=
     (Old_Ref => Old_Entities.No_Entity_Reference,
      Ref     => No_Entity_Reference);

   No_General_Entity_Declaration : constant General_Entity_Declaration :=
     (Loc  => No_Location,
      Name => Ada.Strings.Unbounded.Null_Unbounded_String,
      Body_Is_Full_Declaration => True);

   type GPS_Recursive_References_Cursor
      is new GNATCOLL.Xref.Recursive_References_Cursor
   with record
      Include_Implicit : Boolean := False;
      Include_All      : Boolean := False;
      Kind             : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Entity_Reference_Iterator is record
      Old_Iter : Old_Entities.Queries.Entity_Reference_Iterator;

      Iter     : GPS_Recursive_References_Cursor;
      In_File  : GNATCOLL.VFS.Virtual_File;
      In_Scope : General_Entity := No_General_Entity;
   end record;

   type Base_Entities_Cursor is tagged record
      Iter     : Entities_Cursor;
   end record;

   type Calls_Iterator is new Base_Entities_Cursor with record
      Old_Iter : Old_Entities.Queries.Calls_Iterator;
   end record;

   type Entities_In_File_Cursor is new Base_Entities_Cursor with record
      Old_Iter : Old_Entities.Queries.Entity_Iterator;
   end record;

   type Entities_In_Project_Cursor is new Base_Entities_Cursor with record
      Old_Iter : Old_Entities.Entities_Search_Tries.Vector_Trie_Iterator;
   end record;

   type File_Iterator is tagged record
      --  Old LI database
      Old_Iter : Old_Entities.Queries.File_Dependency_Iterator;
      Old_Ancestor_Iter : Old_Entities.Queries.Dependency_Iterator;
      Is_Ancestor : Boolean;

      --  New sqlite database
      Iter         : Files_Cursor;
   end record;

   type Database_Lock is record
      Constructs : Old_Entities.Construct_Heuristics_Lock;
   end record;

   No_Lock : constant Database_Lock :=
     (Constructs => Old_Entities.No_Lock);

end Xref;
