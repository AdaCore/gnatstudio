------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with GNATCOLL.VFS;
with Interfaces.C;
with Ada.Containers.Ordered_Sets;
with Language_Handlers;
with GNATCOLL.Projects;

package Entities.Queries is

   -----------------------------
   --  Parsing LI information --
   -----------------------------

   type Recursive_LI_Information_Iterator
     is new Entities.LI_Information_Iterator with private;
   --  Will recursively load all xref information from projects. This is more
   --  efficient than iterating over source files and updating their xref info.

   type Language_Filter is access function (Lang : String) return Boolean;
   --  Callback used by Recursive_LI_Information_Iterator to choose which
   --  language to iterate on.

   procedure Start
     (Iter      : out Recursive_LI_Information_Iterator;
      Handler   : access Language_Handlers.Language_Handler_Record'Class;
      Project   : GNATCOLL.Projects.Project_Iterator;
      Filter    : Language_Filter := null);
   --  Start parsing all LI information, for all projects returned by Project.
   --  The parsing can be split into small chunks so that the interface can be
   --  refreshed during the processing.

   overriding procedure Next
     (Iter  : in out Recursive_LI_Information_Iterator;
      Steps : Natural := Natural'Last;
      Count : out Natural;
      Total : out Natural);
   overriding procedure Free (Iter : in out Recursive_LI_Information_Iterator);
   --  See inherited documentation

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
   --  Status of the cross-reference operation
   --  Fuzzy_Match is used if the exact location wasn't found (e.g the LI
   --  file wasn't up-to-date), and the returned location is the closest that
   --  matched. This is assuming there is a single entity with that name
   --  visible. If there are at least two entities with this name,
   --  Overloaded_Entity_Found is used instead, and no Entity is returned

   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : GNATCOLL.VFS.Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False);
   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : GNATCOLL.VFS.Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out Entity_Information;
      Closest_Ref     : out Entity_Reference;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False);
   procedure Find_Declaration
     (Db              : Entities_Database;
      Source          : Source_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out Entity_Information;
      Closest_Ref     : out Entity_Reference;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False;
      Handler         : LI_Handler := null;
      Fuzzy_Expected  : Boolean := False);
   --  Find the entity that is referenced at the given location.
   --  If Entity_Name is unspecified, GPS will no take this into account
   --  If Check_Decl_Only is True, then only declarations are checked, not
   --  any other kind of reference.
   --  The Handler is computed automatically if not passed as an argument.
   --  Closest_Ref is the reference to the entity that was the closest to the
   --  given location.
   --  If Fuzzy_Expected is true, then the search won't try to fallback to
   --  the constructs in case of a fuzzy result.

   procedure Find_Next_Body
     (Entity               : Entity_Information;
      Current_Location     : File_Location := No_File_Location;
      Location             : out File_Location;
      No_Location_If_First : Boolean := False);
   --  Find the location for one of the bodies of the entities. If the
   --  current location is not a body, the first body found is returned.
   --  Otherwise, the first one different from Current_Location is returned.
   --  Calling this subprogram multiple times will eventually return all the
   --  bodies.
   --  This also returns completion for incomplete types.
   --  If No_Location_If_First is True, then this iterator will not loop
   --  to the first body on reaching the last.

   --------------
   -- Entities --
   --------------

   type Entity_Iterator is private;

   procedure Find_All_Entities_In_File
     (Iter                  : out Entity_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Name                  : String := "");
   --  Return all the entities referenced in File which have a name equal to
   --  Name (or all entities if Name is the empty string).
   --  Each entity returned is unique. You can get the list of references for
   --  them by using Find_All_References below

   function At_End (Iter : Entity_Iterator) return Boolean;
   --  Whether there remains any entity to return

   function Get (Iter : Entity_Iterator) return Entity_Information;
   --  Return the current entity

   procedure Next (Iter : in out Entity_Iterator);
   --  Move to the next entity

   ----------------
   -- References --
   ----------------

   type Entity_Reference_Iterator is private;
   type Entity_Reference_Iterator_Access is
     access all Entity_Reference_Iterator;

   procedure Find_All_References
     (Iter                  : out Entity_Reference_Iterator;
      Entity                : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_File               : Source_File := null;
      In_Scope              : Entity_Information := null;
      Filter                : Reference_Kind_Filter := Real_References_Filter;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False);
   --  Find all the references to the entity. This also return the location
   --  for the declaration of the entity.
   --  If In_File is specified, then only the references in that file will be
   --  returned. This is also more efficient. Alternatively, In_Scope can be
   --  specified to limit the list of references to the ones that appear
   --  in the scope of In_Scope.
   --  If Filter is specified, only references whose kind is not filtered will
   --  be returned.
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

   function Get (Iter : Entity_Reference_Iterator) return Entity_Reference;
   --  Return the current reference. This might be No_Entity_Reference if the
   --  iterator needs to parse more source files to get that information.
   --  The search is done with small steps, so that this can be easily put in
   --  the background, including the parsing of the source files.

   function Get_Entity
     (Iter : Entity_Reference_Iterator) return Entity_Information;
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

   --------------
   -- Renaming --
   --------------

   function Renaming_Of
     (Entity : Entity_Information) return Entity_Information;
   --  If Entity is a renaming of another entity (or a typedef for another
   --  type), return the entity that is renamed. Return null if there is no
   --  renaming.

   --------------
   -- Generics --
   --------------

   function Is_Instantiation_Of
     (Entity : Entity_Information) return Entity_Information;
   --  Return the generic entity that Entity instantiates. It is assumed

   ----------------------------
   -- Subprograms parameters --
   ----------------------------

   type Subprogram_Iterator is private;

   function Get_Subprogram_Parameters
     (Subprogram            : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null)
      return Subprogram_Iterator;
   --  Return an iterator that will get all the parameters associated with the
   --  subprogram.
   --  If Subprogram doesn't have any, or isn't a subprogram, the iterator will
   --  not return any value.

   procedure Next (Iterator : in out Subprogram_Iterator);
   --  Move to the next parameter

   procedure Get
     (Iterator  : in out Subprogram_Iterator;
      Parameter : out Entity_Information);
   --  Return the current parameter.
   --  null is returned if there are no more parameters.

   type Parameter_Type is
     (In_Parameter,
      Out_Parameter,
      In_Out_Parameter,
      Access_Parameter);

   function Get_Type (Iterator : Subprogram_Iterator) return Parameter_Type;
   --  Return information on how the parameter is passed to the subprogram.

   function Image (Kind : Parameter_Type) return String;
   --  Return a string suitable for display

   function Is_Parameter_Of
     (Entity : Entity_Information) return Entity_Information;
   --  Return the subprogram for which Entity is a parameter (or null)

   -------------------------------
   -- Formal generic parameters --
   -------------------------------

   type Generic_Iterator is private;

   function Get_Generic_Parameters
     (Generic_Entity        : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null)
      return Generic_Iterator;
   --  Return an iterator that will get all the formal parameters associated
   --  with the Generic_Entity.
   --  If Generic_Entity doesn't have any, or isn't a generic, the iterator
   --  will not return any value.

   procedure Next (Iterator : in out Generic_Iterator);
   --  Move to the next parameter

   procedure Get
     (Iterator  : in out Generic_Iterator;
      Parameter : out Entity_Information);
   --  Return the current parameter.
   --  null is returned if there are no more parameters.

   ------------------
   -- Dependencies --
   ------------------

   type File_Dependency_Iterator is private;

   procedure Find_Dependencies
     (Iter                  : out File_Dependency_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null);
   --  Return the list of files that File depends on.

   function At_End (Iter : File_Dependency_Iterator) return Boolean;
   --  Whether there are no dependency remaining. Calling Get when this returns
   --  True will raise an exception.

   function Is_Explicit (Iter : File_Dependency_Iterator) return Boolean;
   --  Return true if the dependency was explicitely specified by the user.

   procedure Next (Iter : in out File_Dependency_Iterator);
   --  Fetch the next dependency

   function Get (Iter : File_Dependency_Iterator) return Source_File;
   --  Return the current dependency. This is null if there are no remaining
   --  dependencies.

   ---------------------------
   -- Ancestor dependencies --
   ---------------------------

   type Dependency_Iterator is private;
   type Dependency_Iterator_Access is access all Dependency_Iterator;

   procedure Find_Ancestor_Dependencies
     (Iter                  : out Dependency_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Include_Self          : Boolean := False;
      Single_Source_File    : Boolean := False);
   --  Return the list of files that depend on File. The rule is the following:
   --    - bodies, specs and separates always depend on each other
   --
   --  If Include_Self is True, the File itself is part of the returned list.
   --  Otherwise, only the other source files are returned, even if they belong
   --  to the same LI file.
   --
   --  If Single_Source_File is True, then the iterator will only return File
   --  itself. This might be used in special contexts to either work on
   --  multiple LI files or a single source file.
   --
   --  Source files with no xref information are reported through
   --  File_Has_No_LI_Report if set.

   function At_End (Iter : Dependency_Iterator) return Boolean;
   --  Whether there are no dependency remaining

   procedure Next (Iter : in out Dependency_Iterator);
   --  Fetch the next dependency

   function Get (Iter : Dependency_Iterator) return Source_File;
   --  Return the current dependency. This will be null until GPS is done
   --  parsing files to compute the information. You should always
   --  check At_End to know whether there are dependencies remaining

   function Is_Explicit (Iter : Dependency_Iterator) return Boolean;
   --  Return true if the dependency was explicitely specified by the user.

   procedure Destroy (Iter : in out Dependency_Iterator);
   procedure Destroy (Iter : in out Dependency_Iterator_Access);
   --  Free the memory occupied by Iter

   function Get_Current_Progress (Iter : Dependency_Iterator) return Integer;
   function Get_Total_Progress (Iter : Dependency_Iterator) return Integer;
   --  Return the progress indicators for the iterator

   ------------
   -- Scopes --
   ------------

   type Lines_To_Scope (<>) is private;

   function Compute_Scopes (File : Source_File) return Lines_To_Scope;
   --  Compute the entities to which each line in the file belongs to. By
   --  retrieving the entity associated with a line, you will know the
   --  inner-most enclosing subprogram.

   function Get_Scope
     (Scopes : Lines_To_Scope; Line : Integer) return Entity_Information;
   --  Return the inner-most enclosing subprogram for a line in the file

   -------------
   -- Callers --
   -------------

   procedure Compute_All_Call_Graphs (Db : Entities_Database);
   --  Compute the callgraphs for all the files already parsed

   function Get_Caller (Ref : Entity_Reference) return Entity_Information;
   --  Return the entity that encloses the reference

   type Calls_Iterator is private;

   function Get_All_Called_Entities
     (Entity : Entity_Information)
      return Calls_Iterator;
   --  Return all the entities that are found in the scope of Entity. This is
   --  not necessarily a subprogram call, but can be many things.
   --  All entities returned are unique. If you need to find the specific
   --  reference(s) to that entity, you'll need to search for the references in
   --  the right scope through the iterators above.

   function At_End (Iter : Calls_Iterator) return Boolean;
   --  True if there are no more called entities

   function Get (Iter : Calls_Iterator) return Entity_Information;
   --  Return the current entity

   procedure Next (Iter : in out Calls_Iterator);
   --  Move to the next entity

   procedure Destroy (Iter : in out Calls_Iterator);
   --  Free the memory used by the iterator

   function In_Range
     (Loc : File_Location; Entity : Entity_Information) return Boolean;
   --  True if Loc is in the scope of Entity.
   --  If Entity is a class, its scope extends for the whole range of {}, if
   --  it is a subprogram, it extends until the end of the subprogram,...

   function Is_Discriminant
     (Discr, Entity : Entity_Information) return Boolean;
   --  Return True if Discr is a discriminant of Entity

   type Dispatching_Menu_Policy is (Never, From_Memory, Accurate);
   for Dispatching_Menu_Policy'Size use Interfaces.C.int'Size;
   pragma Convention (C, Dispatching_Menu_Policy);
   --  The list of possible behaviours for the contextual menu on dispatching
   --  calls. From_Memory relies on information already parsed in memory, and
   --  might not be accurate. Accurate will possibly load other LI files, but
   --  might be much slower as a result

   Entity_Has_Declaration : constant Reference_Kind_Filter :=
     (Declaration => True, others => False);
   Entity_Has_Body        : constant Reference_Kind_Filter :=
     (Body_Entity => True, others => False);

   procedure For_Each_Dispatching_Call
     (Entity    : Entity_Information;
      Ref       : Entity_Reference;
      On_Callee : access function
        (Callee, Primitive_Of : Entity_Information) return Boolean;
      Filter    : Reference_Kind_Filter := Entity_Has_Declaration;
      Policy    : Dispatching_Menu_Policy);
   --  If Ref is for a subprogram, this will call On_Callee with all the
   --  subprograms that might possibly be called instead of Entity.
   --  This is intended for use on dispatching calls (ie you have an
   --  Entity_Reference for which Get_Kind returns Dispatching_Call).
   --     For instance, if you have
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
   --
   --  Warning: Although there is a new version of this routine in xref
   --    it cannot be removed yet since there is still dependency on it
   --    from package Entities.Commands.

   ---------------
   -- Full_Name --
   ---------------

   function Get_Full_Name (Entity : Entity_Information) return String;
   --  Return the fully qualified name for the entity

   -----------------------
   -- Parents and types --
   -----------------------

   function Get_Variable_Type
     (Entity : Entity_Information) return Entity_Information;
   --  Return the type of a variable. This is not suitable for a class type,
   --  since there can be multiple parents

   function Array_Contents_Type
     (Entity : Entity_Information) return Entity_Information;
   --  Return the type of entities contained in an array

   function Array_Index_Types
     (Entity : Entity_Information) return Entity_Information_Array;
   --  Return the list of types for the indexes of a one-dimensional or
   --  multi-dimensional array.

   function Pointed_Type
     (Entity : Entity_Information) return Entity_Information;
   --  Return the type pointed to by entity

   function Overriden_Entity
     (Entity : Entity_Information) return Entity_Information;
   --  Return the overriden entity

   function Returned_Type
     (Entity : Entity_Information) return Entity_Information;
   --  Return the type returned by the entity

   function Is_Subtype (Entity : Entity_Information) return Boolean;
   --  Whether the entity is a subtype of another (Ada sense)

   ------------------
   -- Parent types --
   ------------------

   function Get_Parent_Types
     (Entity    : Entity_Information;
      Recursive : Boolean := False) return Entity_Information_Array;
   --  Return the list of parent entities for Entity. In
   --  Object-oriented languages, this would be the classes Entity derives
   --  from. In Ada, this includes the parent type of a type or subtype

   function Get_Parent_Package
     (Pkg : Entity_Information; Force_Load_Xrefs : Boolean := True)
      return Entity_Information;
   --  Return the parent package for the package Pkg.
   --  This is not intended to be use for nested packages.

   type Children_Iterator is private;
   function  At_End  (Iter : Children_Iterator) return Boolean;
   function  Get     (Iter : Children_Iterator) return Entity_Information;
   procedure Next    (Iter : in out Children_Iterator);
   procedure Destroy (Iter : in out Children_Iterator);
   --  An iterator for the types derived from a given entity. These are the
   --  usual iterator subprograms, that return one entity at a time, since
   --  finding all the entities might require parsing and loading several
   --  source files. Get might return null even though the search is not
   --  finished, when it needs to parse more files.

   function Get_Child_Types
     (Entity      : Entity_Information;
      Recursive   : Boolean := False;
      Update_Xref : Boolean := True) return Children_Iterator;
   --  Return the list of types derived from Entity. If Recursive is False,
   --  then only the entities that derive directly from Entity are returned,
   --  otherwise all the types that derive even indirectly from Entity are
   --  returned.
   --  If Update_Xref is False, then the entities database is not updated in
   --  memory. If it isn't already up-to-date, some child types might not be
   --  returned.

   --------------------------
   -- Primitive operations --
   --------------------------

   type Primitive_Operations_Iterator is private;

   procedure Find_All_Primitive_Operations
     (Iter               : out Primitive_Operations_Iterator;
      Entity             : Entity_Information;
      Include_Inherited  : Boolean;
      Only_If_Overriding : Boolean := False);
   --  Get all the primitive operations of the entity, including the ones
   --  inherited from its various parents.
   --  If Only_If_Overriding is true, then only those primitives that override
   --  one inherited from the parent are returned.

   function At_End (Iter : Primitive_Operations_Iterator) return Boolean;
   function Get
     (Iter : Primitive_Operations_Iterator) return Entity_Information;
   procedure Next    (Iter : in out Primitive_Operations_Iterator);
   procedure Destroy (Iter : in out Primitive_Operations_Iterator);
   --  Usual subprograms for iterators

private

   type Entity_Information_Array_Access is access Entity_Information_Array;

   type Primitive_Operations_Iterator is record
      Parents           : Entity_Information_Array_Access;
      Current_Parent    : Integer;

      Current_Primitive : Entity_Information_Arrays.Index_Type;
      Overriding_Only   : Boolean;
   end record;

   type Entity_Iterator is record
      Name                : GNATCOLL.Symbols.Symbol;
      SIter               : Entities_Hash.Cursor;
      Iter                : Entities_Hash.Cursor;
      File                : Source_File;
      EL                  : Entity_Information_List_Access;
      Index_In_EL         : Entity_Information_Arrays.Index_Type;
      Case_Sensitive      : Boolean;
      Processing_Entities : Boolean;
      --  Whether we are processing File.Entities or File.All_Entities
   end record;

   type Dependency_Iterator is record
      LI_Iter               : Recursive_LI_Information_Iterator;
      --  Iterator used while parsing the LI info from the disk

      Db                    : Entities_Database;

      Handler               : LI_Handler;
      --  The handler used to parse all LI information.
      --  Set to null when we have finished parsing all projects' LI files, ie
      --  we are now traversing File.Depended_On

      Total_Progress        : Natural;
      Current_Progress      : Natural;

      Include_Self          : Boolean;
      File_Has_No_LI_Report : File_Error_Reporter := null;

      Single_Source_File    : Boolean;
      --  If True, we only return File itself

      Source_File_Index     : Natural;
      --  Index of current source in the current project. This is only used if
      --  each step in the iterator corresponds to a file, as opposed to a
      --  project.
      --  (See constant Find_Deps_File_Granularity in the body)

      File                  : Source_File;
      --  Current source file

      Dep_Index             : Dependency_Arrays.Index_Type;
      --  Index in File.Depended_On.
   end record;

   type File_Dependency_Iterator is record
      File      : Source_File;
      Dep_Index : Dependency_Arrays.Index_Type;
   end record;

   type Subprogram_Iterator is record
      It            : Entity_Reference_Cursor;
      Entity        : Entity_Information;
      Cache_Current : Entity_Information;
   end record;

   type Generic_Iterator is record
      It            : Entity_Reference_Cursor;
      Entity        : Entity_Information;
      Cache_Current : Entity_Information;
   end record;

   package File_Analyzed_Set is new Ada.Containers.Ordered_Sets
     (Virtual_File_Indexes.VF_Key,
      "<" => Virtual_File_Indexes."<",
      "=" => Virtual_File_Indexes."=");

   type File_Analyzed_Set_Access is access all File_Analyzed_Set.Set;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Analyzed_Set.Set, File_Analyzed_Set_Access);

   type Entity_Reference_Iterator is record
      Decl_Returned : Boolean;
      --  Whether the declaration has already been returned or not

      Entity_It : Entities_In_File_Sets.Cursor;
      Files_It  : Entity_File_Maps.Cursor;

      Entity  : Entity_Information;
      In_File : Source_File;
      Start_Line, Last_Line  : Integer;
      Filter  : Reference_Kind_Filter;

      Include_Overriding   : Boolean;
      Include_Overridden   : Boolean;
      --  Whether overridding and overridden entities should also be part of
      --  the search. Their references are returned when they match the filter.

      Extra_Entities       : Entity_Information_Arrays.Instance :=
        Entity_Information_Arrays.Empty_Instance;
      Extra_Entities_Index : Entity_Information_Arrays.Index_Type :=
        Entity_Information_Arrays.First;

      Deps    : Dependency_Iterator;

      Files_Analyzed : File_Analyzed_Set_Access;
   end record;

   type Children_Iterator is record
      Entity      : Entity_Information;
      Recursive   : Boolean;
      Deps        : Dependency_Iterator;
      Update_Xref : Boolean;

      Results     : Entity_Information_List;
      Current     : Entity_Information_Arrays.Index_Type;
   end record;

   type Calls_Iterator is record
      Entity  : Entity_Information;
      Index   : Entity_Information_Arrays.Index_Type;
   end record;

   type LI_Information_Iterator_Access
     is access all Entities.LI_Information_Iterator'Class;

   type Recursive_LI_Information_Iterator
     is new Entities.LI_Information_Iterator with
      record
         Handler      : Language_Handlers.Language_Handler;

         Project      : GNATCOLL.Projects.Project_Iterator; --  Current project
         Filter       : Language_Filter;

         Current_Lang : Natural;  --  Current lang in current project
         LI           : LI_Information_Iterator_Access;
         Lang_Count   : Natural;

         Count        : Natural; --  total processed so far, not including LI
         Total        : Natural; --  total to process, not including LI

         LI_Count     : Natural; --  total processed in LI
         LI_Total     : Natural; --  total to process in LI

         Start        : Ada.Calendar.Time;
      end record;

   type Entity_Info_Array is array (Natural range <>) of Entity_Information;
   type Lines_To_Scope (Line_Max : Natural) is record
      --  We need two tables to memorize the entity enclosing a given
      --  line: in the case of "procedure A (B : Integer)", the caller
      --  of A is the package, whereas the caller of B is A itself.

      Line_Info     : Entity_Info_Array (1 .. Line_Max);
      Info_For_Decl : Entity_Info_Array (1 .. Line_Max);
   end record;

end Entities.Queries;
