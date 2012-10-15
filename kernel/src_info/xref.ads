------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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
with GNATCOLL.Symbols;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.Xref;          use GNATCOLL.Xref;
with GNAT.Strings;
with Old_Entities;
with Old_Entities.Queries;   use Old_Entities.Queries;
with Language_Handlers;
with Language.Tree.Database;

package Xref is

   SQLITE : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("Entities.SQLITE", GNATCOLL.Traces.Off);
   --  Whether to use the sqlite-based cross-reference system

   ---------------
   --  Entities
   ---------------

   type General_Entity is private;
   No_General_Entity : constant General_Entity;

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

   type General_Xref_Database_Record is abstract tagged record
      Entities   : Old_Entities.Entities_Database;
      --  The "legacy" LI database

      Xref       : Extended_Xref_Database_Access;
      --  The "new" LI database

      Constructs : Language.Tree.Database.Construct_Database_Access;
      --  The constructs database

      Lang_Handler : Language_Handlers.Language_Handler;
      --  The type used to convert from file names to languages. This is used
      --  by the constructs database.

   end record;
   type General_Xref_Database is access all General_Xref_Database_Record'Class;

   procedure Destroy (Self : in out General_Xref_Database);
   --  Destroy the xref database (in memory)

   procedure Initialize_Constructs
     (Self         : access General_Xref_Database_Record;
      Lang_Handler : Language_Handlers.Language_Handler;
      Symbols      : GNATCOLL.Symbols.Symbol_Table_Access);
   --  Initialize various internal fields for the constructs. It is assumed
   --  that the xref and LI databases have already been initialized.

   function Select_Entity_Declaration
     (Self   : access General_Xref_Database_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Entity : General_Entity) return General_Entity is abstract;
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
      File   : Virtual_File;
      Line   : Integer := 0;
      Column : Visible_Column_Type := 0;
   end record;
   No_Location : constant General_Location := (No_File, 0, 0);
   --  ??? Should we also cache the Old_Entities.Source_File ?

   type General_Entity_Declaration is record
      Loc  : General_Location;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Body_Is_Full_Declaration : Boolean;
   end record;
   No_General_Entity_Declaration : constant General_Entity_Declaration;
   --  Various pieces of information computed for the entity, including the
   --  location of its declaration.

   --  Entity references

   type General_Entity_Reference is private;
   No_General_Entity_Reference : constant General_Entity_Reference;

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

   procedure Find_Next_Body
     (Dbase                : access General_Xref_Database_Record;
      Entity               : General_Entity;
      Current_Location     : General_Location := No_Location;
      Location             : out General_Location;
      No_Location_If_First : Boolean := False);
   --  Find the location for one of the bodies of the entities. If the
   --  current location is not a body, the first body found is returned.
   --  Otherwise, the first one different from Current_Location is returned.
   --  Calling this subprogram multiple times will eventually return all the
   --  bodies.
   --  This also returns completion for incomplete types.
   --  If No_Location_If_First is True, then this iterator will not loop
   --  to the first body on reaching the last.

   type Reference_Kind_Filter is access function
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean;

   function Entity_Has_Body
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
   --  Retrieve the entity referenced at the given location

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

   function Get_Entity (Ref : General_Entity_Reference) return General_Entity;
   --  Return the entity the reference is pointing to

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

   function Renaming_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the entity that Entity renames (or No_General_Entity)

   function Is_Primitive_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Returns the entity for which Entity is a method/primitive operation

   function Is_Access
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean;
   --  True if E is a type or a variable, and it points to some other type.
   --  This is an Ada access type, an Ada access variable, a C pointer,...

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

   function Pointed_Type
     (Dbase  : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity;
   --  Return the type pointed to by entity

   function Documentation
     (Self             : access General_Xref_Database_Record;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : General_Entity;
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String;
   --  Return the documentation (tooltips,...) for the entity.
   --  If Raw_Format is False, the documentation is formated in HTML.
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
      Include_Overridden    : Boolean := False);
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

private
   type Extended_Xref_Database is new GNATCOLL.Xref.Xref_Database with
      null record;

   type General_Entity is record
      Old_Entity : Old_Entities.Entity_Information := null;
      Entity     : GNATCOLL.Xref.Entity_Information := No_Entity;

      Node       : Language.Tree.Database.Entity_Access :=
        Language.Tree.Database.Null_Entity_Access;
      --  The corresponding node in the constructs database. This can be
      --  computed from the other two fields.
   end record;
   No_General_Entity : constant General_Entity :=
     (Old_Entity => null,
      Entity     => No_Entity,
      Node       => Language.Tree.Database.Null_Entity_Access);

   type General_Entity_Reference is record
      Old_Ref : Old_Entities.Entity_Reference :=
        Old_Entities.No_Entity_Reference;
      Ref : Entity_Reference := No_Entity_Reference;
   end record;
   No_General_Entity_Reference : constant General_Entity_Reference :=
     (Old_Ref => Old_Entities.No_Entity_Reference,
      Ref     => No_Entity_Reference);

   No_General_Entity_Declaration : constant General_Entity_Declaration :=
     (Loc  => No_Location,
      Name => Ada.Strings.Unbounded.Null_Unbounded_String,
      Body_Is_Full_Declaration => True);

   type Entity_Reference_Iterator is record
      Old_Iter : Old_Entities.Queries.Entity_Reference_Iterator;

      Iter     : GNATCOLL.Xref.Recursive_References_Cursor;
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
