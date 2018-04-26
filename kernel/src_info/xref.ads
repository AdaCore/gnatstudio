------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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
with GNATCOLL.SQL.Exec;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.Xref;          use GNATCOLL.Xref;
with GNAT.Strings;
with Projects;
with Language_Handlers;
with Language.Tree.Database;
with Language.Profile_Formaters; use Language.Profile_Formaters;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Containers.Indefinite_Holders;
with Array_Utils;

----------
-- Xref --
----------

package Xref is

   type Root_Reference_Iterator is abstract tagged null record;
   No_Reference_Iterator : constant Root_Reference_Iterator'Class;

   type Root_Entity is abstract tagged null record;
   type Root_Entity_Access is access all Root_Entity'Class;
   No_Root_Entity : aliased constant Root_Entity'Class;
   --  aliased is added to let AJIS make it accessible to GNATbench

   type General_Parameter is record
      Parameter : Root_Entity_Access;
      Kind      : GNATCOLL.Xref.Parameter_Kind;
   end record;
   type Parameter_Array is array (Natural range <>) of General_Parameter;
   No_Parameters : constant Parameter_Array;

   procedure Free (Self : in out Parameter_Array);
   --  Free memory used by the array

   function Parameters
     (Entity : Root_Entity) return Parameter_Array is abstract;
   --  Return the list of parameters for a given subprogram.
   --  Caller must free the returned value

   function Find_All_References
     (Entity                : Root_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class is abstract;
   --  Find all the references to the entity. This also return the location
   --  for the declaration of the entity.
   --  If In_File is specified, then only the references in that file will be
   --  returned. This is also more efficient. Alternatively, In_Scope can be
   --  specified to limit the list of references to the ones that appear
   --  in the scope of In_Scope.
   --  If Include_Overriding or Include_Overridden are True, then all
   --  references to an overriding or Overridden subprogram will also be
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
   --
   --  If Include_Renames is true, then this subprogram follows the "renames"
   --  statement and returns the references to the renamed entities as well.

   function Id_Eq (L, R : Root_Entity_Access) return Boolean is
     (L.all = R.all);

   package Entity_Arrays is new Array_Utils (Root_Entity_Access, Id_Eq);
   subtype Entity_Array is Entity_Arrays.Array_Type;

   No_Entity_Array : Entity_Array (1 .. 0) := (others => <>);

   procedure Free (X : in out Entity_Array);
   --  Free memory associated with X

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

   -------------------------------
   --  The set of all databases --
   -------------------------------

   -----------------------
   --  File location
   -----------------------

   type General_Location is record
      File   : aliased Virtual_File := No_File;
      --  aliased is added to let AJIS make this field accessible to GNATbench

      Project_Path : Virtual_File := No_File;
      --  Used to disambiguate which project is handling the file, for instance
      --  in the case of aggregate projects. This might be left to No_File
      --  when unknown.

      Line   : Integer := 0;
      Column : Visible_Column_Type := 0;
   end record;
   No_Location : aliased constant General_Location :=
     (No_File, No_File, 0, 0);
   --  ??? Should we also cache the Old_Entities.Source_File ?
   --  aliased is added to let AJIS make it accessible to GNATbench

   function Get_Project
     (G : General_Location) return GNATCOLL.Projects.Project_Type;
   --  Convenience function to return the project from a General_Location

   type General_Xref_Database_Record;
   type General_Xref_Database is access all General_Xref_Database_Record'Class;

   type Lang_Specific_Database is interface;
   function Get_Entity
     (Db : Lang_Specific_Database;
      General_Db : General_Xref_Database;
      Name : String;
      Loc : General_Location) return Root_Entity'Class is abstract;

   package Lang_Specific_Databases_Maps
   is new Ada.Containers.Indefinite_Hashed_Maps
     (String,
      Lang_Specific_Database'Class,
      Hash => Ada.Strings.Hash_Case_Insensitive, Equivalent_Keys => "=");

   type General_Xref_Database_Record is tagged record
      Xref       : Extended_Xref_Database_Access;
      --  The "new" LI database

      Constructs : Language.Tree.Database.Construct_Database_Access;
      --  The constructs database

      Lang_Handler : Language.Tree.Database.Abstract_Language_Handler;
      --  The type used to convert from file names to languages. This is used
      --  by the constructs database.

      Registry     : Projects.Project_Registry_Access;
      --  Project information

      Symbols : GNATCOLL.Symbols.Symbol_Table_Access;

      Freeze_Count : Integer := 0;
      --  Used to implement freeze of the DB

      DB : GNATCOLL.SQL.Exec.Database_Description;
      --  The description of the database we are currently connected to.
      --  This must not be freed while where exists connections to this
      --  database, since the connections have a pointer to this descr.

      Working_Xref_Db : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      --  Location of the sqlite database on which GPS is currently working
      --  Set to No_File if GPS is not working on a database at the moment.

      Disable_SQL_Queries : Boolean := False;
      --  Whether the database is writable. When True, no update or query is
      --  performed.

      Xref_Db_Is_Temporary : Boolean := False;
      --  Whether we should remove the database from the disk when we close it

      Errors : access GNATCOLL.SQL.Exec.Error_Reporter'Class;

      Lang_Specific_Databases : Lang_Specific_Databases_Maps.Map;
   end record;

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
      Errors       : access GNATCOLL.SQL.Exec.Error_Reporter'Class := null);
   --  Initialize various internal fields for the constructs. It is assumed
   --  that the xref and LI databases have already been initialized.
   --  It is possible to pre-allocate Xref and/or Entities database if you want
   --  specific instances to be used, instead of the default ones.
   --
   --  Errors is never freed, and is used to report errors when executing
   --  SQL queries.

   function Select_Entity_Declaration
     (Self   : access General_Xref_Database_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Entity : Root_Entity'Class) return Root_Entity'Class;
   --  The user has requested a xref, but the information is not up-to-date and
   --  there is an ambiguity. This function is responsible for asking the user
   --  to chose among all the homonym entities in File.
   --  Entity is set initially to the best choice, and its name is the one
   --  that should be looked for in the file.
   --  Should return No_Root_Entity if the user has cancelled the action.

   function Xref_Database_Location
     (Self    : not null access General_Xref_Database_Record)
      return GNATCOLL.VFS.Virtual_File;
   --  Location of the sqlite file that contains the xref database on which
   --  GPS is currently working.

   function Allow_Queries
     (Self : not null access General_Xref_Database_Record) return Boolean;
   --  Whether SQL queries can be performed in the database

   type General_Entity_Declaration is record
      Loc  : aliased General_Location;
      --  aliased is added to let AJIS make this field accessible to GNATbench
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Body_Is_Full_Declaration : Boolean;
   end record;
   No_General_Entity_Declaration : constant General_Entity_Declaration;
   --  Various pieces of information computed for the entity, including the
   --  location of its declaration.

   ---------------------------
   -- Root Entity Reference --
   ---------------------------

   type Root_Entity_Reference is abstract tagged null record;
   type Root_Entity_Ref_Access is access all Root_Entity_Reference'Class;

   type Reference_Kind_Filter is access function
     (Ref : Root_Entity_Reference'Class) return Boolean;

   function From_Instances
     (Ref : Root_Entity_Reference) return Entity_Array is abstract;
   function Reference_Is_Body
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Reference_Is_Declaration
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Real_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Implicit_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Real_Or_Implicit_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Read_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Write_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Read_Or_Write_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Read_Or_Implicit_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Read_Or_Write_Or_Implicit_Reference
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Get_Entity
     (Ref : Root_Entity_Reference) return Root_Entity'Class is abstract;
   function Get_Entity_Name
     (Ref : Root_Entity_Reference) return String is abstract;
   function Get_Location
     (Ref : Root_Entity_Reference) return General_Location is abstract;
   function Show_In_Callgraph
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Is_Dispatching_Call
     (Ref : Root_Entity_Reference) return Boolean is abstract;
   function Get_Display_Kind
     (Ref  : Root_Entity_Reference) return String is abstract;
   procedure For_Each_Dispatching_Call
     (Ref       : Root_Entity_Reference;
      On_Callee : access function (Callee : Root_Entity'Class) return Boolean;
      Filter    : Reference_Kind_Filter := null) is abstract;
   function Get_Caller
     (Ref : Root_Entity_Reference) return Root_Entity'Class is abstract;

   -----------------------------
   -- Root Reference Iterator --
   -----------------------------

   function At_End (Iter : Root_Reference_Iterator) return Boolean is abstract;
   --  Whether there are no more reference to return

   procedure Next (Iter : in out Root_Reference_Iterator) is abstract;
   --  Move to the next reference to the entity

   function Get
     (Iter : Root_Reference_Iterator) return Root_Entity_Reference'Class
      is abstract;

   function Get_Entity
     (Iter : Root_Reference_Iterator) return Root_Entity'Class is abstract;

   procedure Destroy (Iter : in out Root_Reference_Iterator) is abstract;

   function Get_Current_Progress
     (Iter : Root_Reference_Iterator) return Integer is abstract;

   function Get_Total_Progress
     (Iter : Root_Reference_Iterator) return Integer is abstract;

   --  Entity references

   type General_Entity_Reference is new Root_Entity_Reference with private;

   No_Root_Entity_Reference : aliased constant Root_Entity_Reference'Class;
   --  aliased is added to let AJIS make it accessible to GNATbench

   overriding function "="
     (Ref1, Ref2 : General_Entity_Reference) return Boolean;
   --  Whether the two references point to the same location.

   -----------------
   -- Root_Entity --
   -----------------

   function Is_Fuzzy (Entity : Root_Entity) return Boolean is abstract;
   --  Whether the entity is just a guess (because the xref info generated by
   --  the compiler was not up-to-date).

   function Get_Name
     (Entity : Root_Entity) return String is abstract;
   --  Return the name of the entity

   function Get_Display_Kind
     (Entity : Root_Entity) return String is abstract;
   --  Return a general kind for the entity. This is not its type, more like a
   --  metaclass. Its exact value will vary depending on what was inserted in
   --  the database, and new language can insert new kinds at any time. So this
   --  string should only be used for display.

   function Qualified_Name
     (Entity : Root_Entity) return String is abstract;
   --  Return the fully qualified name of the entity

   function Hash
     (Entity : Root_Entity) return Integer is abstract;
   --  Return a hash code for the entity

   function Cmp
     (Entity1, Entity2 : Root_Entity'Class) return Integer;
   --  Return -1, 0 or 1 to sort the two entities

   function Get_Declaration
     (Entity : Root_Entity) return General_Entity_Declaration is abstract;
   --  Return the location of the entity declaration

   function Caller_At_Declaration
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the entity whose scope contains the declaration of Entity

   function Get_Body
     (Entity : Root_Entity;
      After  : General_Location := No_Location)
      return General_Location is abstract;
   --  Return the location of the first body for this entity

   function Get_Type_Of
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the type of the entity

   function Returned_Type
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the type returned by a function.

   function Parent_Package
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the parent package (if Entity is a package itself)

   function Pointed_Type
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  The type pointed to by an access type. Returns null for a variable.

   function Renaming_Of
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the entity that Entity renames (or No_General_Entity)

   function Is_Primitive_Of
     (Entity : Root_Entity) return Entity_Array is abstract;
   --  Returns the entities for which Entity is a method/primitive operation
   --  (including the entities for which it is an inherited method)
   --  Caller must call Free on the result.

   function Has_Methods (E : Root_Entity) return Boolean is abstract;
   --  True if the entity might have methods

   function Is_Access (E : Root_Entity) return Boolean is abstract;
   --  True if E is a type or a variable, and it points to some other type.
   --  This is an Ada access type, an Ada access variable, a C pointer,...

   function Is_Abstract
     (E  : Root_Entity) return Boolean is abstract;
   --  Whether the entity is abstract (ie cannot be instantiated

   function Is_Array
     (E  : Root_Entity) return Boolean is abstract;
   --  Whether E is an array type or variable. This is mostly used in the
   --  debugger to find whether the user should be able to dereference the
   --  variable.

   function Is_Printable_In_Debugger
     (E  : Root_Entity) return Boolean is abstract;
   --  Whether we can execute a "print" on E in a debugger, and get a value
   --  that can be shown to the user.
   --  ??? We could perhaps try the command in gdb directly and guess from
   --  there

   function Is_Type
     (E  : Root_Entity) return Boolean is abstract;
   --  True f E is a type (not a variable or a package for instance)

   function Is_Subprogram
     (E  : Root_Entity) return Boolean is abstract;
   --  True if E is a subprogram

   function Is_Container
     (E  : Root_Entity) return Boolean is abstract;
   --  True if E can contain other entities (a record, struct,...)

   function Is_Generic
     (E  : Root_Entity) return Boolean is abstract;
   --  Whether the entity is a 'generic' or 'template'

   function Is_Global
     (E  : Root_Entity) return Boolean is abstract;
   --  Whether the entity is a global entity (library-level in Ada)

   function Is_Static_Local
     (E  : Root_Entity) return Boolean is abstract;
   --  Whether the entity is a static, in the C/C++ sense.

   function Is_Predefined_Entity
     (E  : Root_Entity) return Boolean is abstract;
   --  True if E is a predefined entity

   procedure Documentation
     (Handler           : Language_Handlers.Language_Handler;
      Entity            : Root_Entity;
      Formater          : access Profile_Formater'Class;
      Check_Constructs  : Boolean := True;
      Look_Before_First : Boolean := True) is abstract;
   --  Return the documentation (tooltips,...) for the entity.
   --  Formater is responsible for formating and keep resulting text.
   --  Check_Constructs should be False to disable the use of the constructs
   --  database.
   --
   --  If Look_Before_First is True, the comments are first searched before
   --  the entity, and if not found after the entity. Otherwise the search
   --  order is reversed.
   --
   --  ??? Do we need to pass Handler?

   function End_Of_Scope
     (Entity : Root_Entity) return General_Location is abstract;
   --  For type declaration return the location of their syntax scope; for
   --  Ada packages and subprograms return the location of the end of scope
   --  of their body.

   overriding function Get_Caller
     (Ref : General_Entity_Reference) return Root_Entity'Class;
   --  Return the enclosing scope at the given location

   function Is_Parameter_Of
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the subprogram for which entity is a parameter

   function Overrides
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  The entity that Entity overrides.

   function Instance_Of
     (Entity : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the generic entity instantiated by Entity

   function Methods
     (Entity            : Root_Entity;
      Include_Inherited : Boolean) return Entity_Array is abstract;
   --  The list of methods of an entity.

   function Fields
      (Entity            : Root_Entity) return Entity_Array is abstract;
   --  The fields of an Ada record or C struct

   function Literals
      (Entity            : Root_Entity) return Entity_Array is abstract;
   --  Return the literals of an enumeration

   function Formal_Parameters
      (Entity            : Root_Entity) return Entity_Array is abstract;
   --  The formal parameters for a generic entity.

   function Discriminant_Of
      (Entity            : Root_Entity) return Root_Entity'Class is abstract;
   --  Return the Ada record for which Entity is a discriminant

   function Discriminants
      (Entity            : Root_Entity) return Entity_Array is abstract;
   --  Return the list of discriminants for the entity

   function Component_Type
      (Entity : Root_Entity) return Root_Entity'Class is abstract;
   function Index_Types
      (Entity : Root_Entity) return Entity_Array is abstract;
   --  Index and components types for an array

   function Child_Types
      (Entity    : Root_Entity;
       Recursive : Boolean) return Entity_Array is abstract;
   --  Return the list of types derived from Entity (in the type-extension
   --  sense).

   function Parent_Types
      (Entity    : Root_Entity;
       Recursive : Boolean) return Entity_Array is abstract;
   --  Return the list of types that Entity extends.

   overriding function From_Instances
     (Ref : General_Entity_Reference) return Entity_Array;
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

   type Abstract_Entities_Cursor is interface;
   function At_End
     (Iter : Abstract_Entities_Cursor) return Boolean is abstract;
   function Get
     (Iter : Abstract_Entities_Cursor) return Root_Entity'Class is abstract;
   procedure Next (Iter : in out Abstract_Entities_Cursor) is abstract;
   procedure Destroy (Iter : in out Abstract_Entities_Cursor) is abstract;

   No_Entities_Cursor : constant Abstract_Entities_Cursor'Class;

   function Get_All_Called_Entities
     (Entity : Root_Entity) return Abstract_Entities_Cursor'Class is abstract;
   --  Return all the entities that are found in the scope of Entity. This is
   --  not necessarily a subprogram call, but can be many things.
   --  All entities returned are unique. If you need to find the specific
   --  reference(s) to that entity, you'll need to search for the references in
   --  the right scope through the iterators above.

   ---------------
   --  Entities
   ---------------

   type General_Entity is new Root_Entity with private;
   No_General_Entity : aliased constant General_Entity;
   --  aliased is added to let AJIS make it accessible to GNATbench

   overriding function "=" (E1, E2 : General_Entity) return Boolean;
   --  Whether the two entities are the same

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

   function Project
     (Iter : File_Iterator;
      Tree : GNATCOLL.Projects.Project_Tree'Class)
      return GNATCOLL.Projects.Project_Type;
   --  The project to which the current element belongs, or No_Project if
   --  unknown (using the old xref engine)

   procedure Destroy (Iter : in out File_Iterator_Access);
   procedure Destroy (Iter : in out File_Iterator);
   --  ??? Only because of the old LI database

   function Find_Dependencies
     (Self    : access General_Xref_Database_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type) return File_Iterator;
   --  Return the list of files that File depends on.

   function Find_Ancestor_Dependencies
     (Self    : access General_Xref_Database_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type) return File_Iterator;
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

   overriding function Reference_Is_Body
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Reference_Is_Declaration
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Real_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Real_Or_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Read_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Write_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Read_Or_Write_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Read_Or_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean;
   overriding function Is_Read_Or_Write_Or_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean;
   --  Various filters for references.
   --  Some references kinds are special, and mark for instance the end of
   --  scope. They should not in general be visible to users. One special kind
   --  are implicit references.

   overriding procedure For_Each_Dispatching_Call
     (Ref       : General_Entity_Reference;
      On_Callee : access function (Callee : Root_Entity'Class) return Boolean;
      Filter    : Reference_Kind_Filter := null);
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
   --  The Primitive_Of parameter
   --
   --  Nothing is done if Ref does not point to a dispatching call.
   --  This procedure does not propagate any exception.

   package Root_Entity_Reference_Refs
   is new Ada.Containers.Indefinite_Holders (Root_Entity_Reference'Class);

   subtype Root_Entity_Reference_Ref is Root_Entity_Reference_Refs.Holder;

   function Get_Entity
     (Db           : access General_Xref_Database_Record;
      Name         : String;
      Loc          : General_Location;
      Approximate_Search_Fallback : Boolean := True;
      Closest_Ref  : out Root_Entity_Reference_Ref)
      return Root_Entity'Class;
   function Get_Entity
     (Db           : access General_Xref_Database_Record;
      Name         : String;
      Loc          : General_Location;
      Approximate_Search_Fallback : Boolean := True) return Root_Entity'Class;
   --  Retrieve the entity referenced at the given location.
   --  This also works for operators, whether they are quoted ("=") or
   --  not (=).

   overriding function Get_Entity
     (Ref : General_Entity_Reference) return Root_Entity'Class;
   --  Return the entity the reference is pointing to

   overriding function Get_Entity_Name
     (Ref : General_Entity_Reference) return String
   is (Ref.Get_Entity.Get_Name);

   overriding function Get_Location
     (Ref : General_Entity_Reference) return General_Location;
   --  Return the location of this reference

   --------------------------------
   -- Entity and reference kinds --
   --------------------------------
   --  In the sqlite-backend, entity kinds are extensible by the user, and as
   --  such cannot be represented as an enumeration type on the Ada side. So
   --  instead we provide a number of subprograms to perform the expected
   --  tests on the kind.

   overriding function Show_In_Callgraph
     (Ref : General_Entity_Reference) return Boolean;
   --  Whether to show the corresponding entity in a callgraph.

   overriding function Is_Dispatching_Call
     (Ref : General_Entity_Reference) return Boolean;
   --  Whether ref is dispatching call for a subprogram

   overriding function Get_Display_Kind
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

   type Base_Entities_Cursor is new Abstract_Entities_Cursor with private;

   overriding function At_End
     (Iter : Base_Entities_Cursor) return Boolean;
   overriding function Get
     (Iter : Base_Entities_Cursor) return Root_Entity'Class;
   overriding procedure Next (Iter : in out Base_Entities_Cursor);
   --  Iterate and retrieve an entity at each iteration

   overriding procedure Destroy
     (Iter : in out Base_Entities_Cursor) is null;

   type Entities_In_File_Cursor is new Base_Entities_Cursor with private;

   function Entities_In_File
     (Self    : access General_Xref_Database_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Name    : String := "") return Entities_In_File_Cursor;
   --  Return all entities referenced or declared in file.

   overriding function At_End (Iter : Entities_In_File_Cursor) return Boolean;
   overriding function Get
     (Iter : Entities_In_File_Cursor) return Root_Entity'Class;
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
     (Iter : Entities_In_Project_Cursor) return Root_Entity'Class;
   overriding procedure Next (Iter : in out Entities_In_Project_Cursor);

   overriding procedure Destroy (Iter : in out Entities_In_Project_Cursor);

   ---------------
   -- Callgraph --
   ---------------

   type Calls_Iterator is new Base_Entities_Cursor with private;

   overriding function At_End (Iter : Calls_Iterator) return Boolean;
   overriding function Get
     (Iter : Calls_Iterator) return Root_Entity'Class;
   overriding procedure Next (Iter : in out Calls_Iterator);
   --  Move to the next entity

   overriding procedure Destroy (Iter : in out Calls_Iterator);
   --  Free the memory used by the iterator

   ----------------
   -- References --
   ----------------
   --  While we have the old LI engine, we keep an asynchronous iterator to
   --  find all references. This will no longer be needed when we only use the
   --  sqlite backend.

   type Entity_Reference_Iterator is new Root_Reference_Iterator with private;

   subtype References_Sort is GNATCOLL.Xref.References_Sort;
   function Find_All_References
      (Self     : access General_Xref_Database_Record;
       File     : GNATCOLL.VFS.Virtual_File;
       Kind     : String := "";
       Sort     : References_Sort := GNATCOLL.Xref.By_Location)
     return Root_Reference_Iterator'Class;
   --  Return references to all entities in the file, possibly filtering by
   --  entity kind.
   --  ??? This will always return an empty list when using the old xref
   --  engine.

   overriding function At_End
     (Iter : Entity_Reference_Iterator) return Boolean;

   overriding procedure Next (Iter : in out Entity_Reference_Iterator);
   --  Move to the next reference to the entity

   overriding function Get
     (Iter : Entity_Reference_Iterator) return Root_Entity_Reference'Class;
   --  Return the current reference. This might be No_Entity_Reference if the
   --  iterator needs to parse more source files to get that information.
   --  The search is done with small steps, so that this can be easily put in
   --  the background, including the parsing of the source files.

   overriding function Get_Entity
     (Iter : Entity_Reference_Iterator) return Root_Entity'Class;
   --  Return the entity referenced at the current location. Most of the time,
   --  it will be the entity passed in argument to Find_All_Reference. However,
   --  if Is_Real_Reference is false, it might be a different one, such as
   --  the name of a discriminant or a subprogram parameter for instance

   overriding procedure Destroy (Iter : in out Entity_Reference_Iterator);
   --  Free the memory used by Iter

   overriding function Get_Current_Progress
     (Iter : Entity_Reference_Iterator) return Integer;
   overriding function Get_Total_Progress
     (Iter : Entity_Reference_Iterator) return Integer;
   --  Return the progress indicators for the iterator

   -------------
   -- private --
   -------------
   --  The following subprograms should only be used from GPS.Kernel.Xref.

   function From_New
     (Db     : General_Xref_Database;
      Entity : GNATCOLL.Xref.Entity_Information) return General_Entity;

   function From_Constructs
     (Db     : General_Xref_Database;
      Entity : Language.Tree.Database.Entity_Access) return General_Entity;

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

   function Find_Declaration_Or_Overloaded
     (Self              : access General_Xref_Database_Record;
      Loc               : General_Location;
      Entity_Name       : String;
      Ask_If_Overloaded : Boolean := False;
      Closest_Ref       : out Root_Entity_Reference_Ref;
      Approximate_Search_Fallback : Boolean := True) return Root_Entity'Class;
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

   package Root_Entity_Refs
   is new Ada.Containers.Indefinite_Holders (Root_Entity'Class);

   package Root_Reference_Iterator_Refs
   is new Ada.Containers.Indefinite_Holders (Root_Reference_Iterator'Class);

   subtype Root_Entity_Ref is Root_Entity_Refs.Holder;
   subtype Root_Reference_Iterator_Ref
     is Root_Reference_Iterator_Refs.Holder;

private

   type Extended_Xref_Database is new GNATCOLL.Xref.Xref_Database with
      null record;

   type General_Entity is new Root_Entity with record
      Is_Fuzzy   : Boolean := False;  --  Whether this is a fuzzy match
      Entity     : GNATCOLL.Xref.Entity_Information := No_Entity;

      Loc        : General_Location := No_Location;
      --  The location which was used to query the entity. This is used to
      --  fall back on the Constructs database if Entity is null.

      Db         : General_Xref_Database;
   end record;

   overriding function Find_All_References
     (Entity                : General_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Include_Renames       : Boolean := True;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class;

   type General_Entity_Reference is new Root_Entity_Reference with record
      Ref : Entity_Reference := No_Entity_Reference;
      Db  : General_Xref_Database;
   end record;

   overriding function Is_Fuzzy (Entity : General_Entity) return Boolean;
   overriding function Get_Name
     (Entity : General_Entity) return String;
   overriding function Get_Display_Kind
     (Entity : General_Entity) return String;
   overriding function Qualified_Name
     (Entity : General_Entity) return String;
   overriding function Hash
     (Entity : General_Entity) return Integer;

   overriding function Caller_At_Declaration
     (Entity : General_Entity) return Root_Entity'Class;
   overriding function Get_Body
     (Entity : General_Entity;
      After  : General_Location := No_Location)
      return General_Location;
   overriding function Get_Type_Of
     (Entity : General_Entity) return Root_Entity'Class;

   overriding function Returned_Type
     (Entity : General_Entity) return Root_Entity'Class;

   overriding function Parent_Package
     (Entity : General_Entity) return Root_Entity'Class;
   overriding function Pointed_Type
     (Entity : General_Entity) return Root_Entity'Class;
   overriding function Renaming_Of
     (Entity : General_Entity) return Root_Entity'Class;
   overriding function Is_Primitive_Of
     (Entity : General_Entity) return Entity_Array;

   overriding function Has_Methods (E : General_Entity) return Boolean;

   overriding function Is_Access (E : General_Entity) return Boolean;
   overriding function Is_Abstract
     (E  : General_Entity) return Boolean;
   overriding function Is_Array
     (E  : General_Entity) return Boolean;

   overriding function Is_Printable_In_Debugger
     (E  : General_Entity) return Boolean;

   overriding function Is_Type
     (E  : General_Entity) return Boolean;
   overriding function Is_Subprogram
     (E  : General_Entity) return Boolean;
   --  True if E is a subprogram

   overriding function Is_Container
     (E  : General_Entity) return Boolean;
   overriding function Is_Generic
     (E  : General_Entity) return Boolean;

   overriding function Is_Global
     (E  : General_Entity) return Boolean;
   overriding function Is_Static_Local
     (E  : General_Entity) return Boolean;
   overriding function Is_Predefined_Entity
     (E  : General_Entity) return Boolean;

   overriding function Get_Declaration
     (Entity : General_Entity) return General_Entity_Declaration;

   overriding procedure Documentation
     (Handler           : Language_Handlers.Language_Handler;
      Entity            : General_Entity;
      Formater          : access Profile_Formater'Class;
      Check_Constructs  : Boolean := True;
      Look_Before_First : Boolean := True);

   overriding function End_Of_Scope
     (Entity : General_Entity) return General_Location;

   overriding function Is_Parameter_Of
     (Entity : General_Entity) return Root_Entity'Class;

   overriding function Overrides
     (Entity : General_Entity) return Root_Entity'Class;

   overriding function Instance_Of
     (Entity : General_Entity) return Root_Entity'Class;

   overriding function Methods
     (Entity            : General_Entity;
      Include_Inherited : Boolean) return Entity_Array;

   overriding function Fields
     (Entity            : General_Entity) return Entity_Array;

   overriding function Literals
     (Entity            : General_Entity) return Entity_Array;

   overriding function Formal_Parameters
     (Entity            : General_Entity) return Entity_Array;
   overriding function Discriminant_Of
     (Entity            : General_Entity) return Root_Entity'Class;
   overriding function Discriminants
     (Entity            : General_Entity) return Entity_Array;

   overriding function Component_Type
     (Entity : General_Entity) return Root_Entity'Class;
   overriding function Index_Types
     (Entity : General_Entity) return Entity_Array;

   overriding function Child_Types
     (Entity    : General_Entity;
      Recursive : Boolean) return Entity_Array;

   overriding function Parent_Types
     (Entity    : General_Entity;
      Recursive : Boolean) return Entity_Array;
   overriding function Get_All_Called_Entities
     (Entity : General_Entity) return Abstract_Entities_Cursor'Class;

   overriding function Parameters
     (Entity : General_Entity) return Parameter_Array;

   No_Root_Entity_Reference : aliased constant Root_Entity_Reference'Class
     := General_Entity_Reference'(Db      => null,
                                  Ref  => GNATCOLL.Xref.No_Entity_Reference);

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

   type Entity_Reference_Iterator is new Root_Reference_Iterator with record
      Iter     : GPS_Recursive_References_Cursor;
      In_File  : GNATCOLL.VFS.Virtual_File;
      In_Scope : General_Entity := No_General_Entity;

      Db       : General_Xref_Database;
   end record;

   type Base_Entities_Cursor is new Abstract_Entities_Cursor with record
      Iter     : Entities_Cursor;
      Db       : General_Xref_Database;
   end record;

   type Calls_Iterator is new Base_Entities_Cursor with null record;
   type Entities_In_File_Cursor is new Base_Entities_Cursor with null record;
   type Entities_In_Project_Cursor is new Base_Entities_Cursor
      with null record;

   type File_Iterator is tagged record
      Tree : GNATCOLL.Projects.Project_Tree_Access;

      --  New sqlite database
      Iter         : Files_Cursor;
   end record;

   No_General_Entity : aliased constant General_Entity :=
     (Is_Fuzzy   => False,
      Loc        => No_Location,
      Entity     => No_Entity,
      Db         => null);
   --  We no longer store the Entity_Persistent_Access node in General_Entity,
   --  since we would need to also control the assignments to make sure its
   --  reference counting is properly incremented.

   No_Root_Entity : aliased constant Root_Entity'Class :=
     No_General_Entity;

   No_Parameters : constant Parameter_Array (1 .. 0) := (others => <>);

   type Dummy_Entities_Cursor is new Abstract_Entities_Cursor with null record;

   overriding function At_End
     (Iter : Dummy_Entities_Cursor) return Boolean is (True);

   overriding function Get
     (Iter : Dummy_Entities_Cursor) return Root_Entity'Class
   is
     (No_Root_Entity);

   overriding procedure Next (Iter : in out Dummy_Entities_Cursor) is null;

   overriding procedure Destroy
     (Iter : in out Dummy_Entities_Cursor) is null;

   No_Entities_Cursor : constant Abstract_Entities_Cursor'Class :=
     Dummy_Entities_Cursor'(null record);

   type Dummy_Reference_Iterator is new Root_Reference_Iterator
   with null record;

   -----------------------------
   -- Root Reference Iterator --
   -----------------------------

   overriding function At_End (Iter : Dummy_Reference_Iterator) return Boolean
   is (True);
   --  Whether there are no more reference to return

   overriding procedure Next (Iter : in out Dummy_Reference_Iterator) is null;
   --  Move to the next reference to the entity

   overriding function Get
     (Iter : Dummy_Reference_Iterator) return Root_Entity_Reference'Class
      is (No_Root_Entity_Reference);

   overriding function Get_Entity
        (Iter : Dummy_Reference_Iterator) return Root_Entity'Class
      is (No_Root_Entity);

   overriding procedure Destroy (Iter : in out Dummy_Reference_Iterator)
   is null;

   overriding function Get_Current_Progress
     (Iter : Dummy_Reference_Iterator) return Integer is (0);

   overriding function Get_Total_Progress
     (Iter : Dummy_Reference_Iterator) return Integer is (0);

   No_Reference_Iterator : constant Root_Reference_Iterator'Class :=
     Dummy_Reference_Iterator'(null record);

end Xref;
