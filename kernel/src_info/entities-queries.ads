with VFS;

package Entities.Queries is

   ---------------------------------------
   -- Goto Declaration<-> Body requests --
   ---------------------------------------

   type Find_Decl_Or_Body_Query_Status is
     (Entity_Not_Found,
      Internal_Error,
      No_Body_Entity_Found,
      Overloaded_Entity_Found,
      Fuzzy_Match,
      Success);
   --  Status of the cross-reference operation
   --  Fuzzy_Match is returned if the exact location wasn't found (e.g the LI
   --  file wasn't up-to-date), and the returned location is the closest that
   --  matched.
   --  Whenever the status is Overloaded_Entity_Found, no Entity is returned
   --  at the saem time, since GPS doesn't know which exact one should be used.

   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : VFS.Virtual_File;
      Entity_Name     : String := "";
      Line            : Positive;
      Column          : Positive;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False);
   --  Find the entity that is referenced at the given location.
   --  If Entity_Name is unspecified, GPS will no take this into account
   --  If Check_Decl_Only is True, then only declarations are checked, not
   --  any other kind of reference.

   procedure Find_Next_Body
     (Entity           : Entity_Information;
      Current_Location : File_Location := No_File_Location;
      Location         : out File_Location);
   --  Find the location for one of the bodies of the entities. If the
   --  current location is not a body, the first body found is returned.
   --  Otherwise, the first one different from Current_Location is returned.
   --  Calling this subprogram multiple times will eventually return all the
   --  bodies.
   --  This also returns completion for incomplete types.

   ----------------
   -- References --
   ----------------

   type Entity_Reference_Iterator is private;

   procedure Find_All_References
     (Iter                  : out Entity_Reference_Iterator;
      Entity                : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_File               : Source_File := null;
      In_Scope              : Entity_Information := null);
   --  Find all the references to the entity. This also return the location
   --  for the declaration of the entity.
   --  if In_File is specified, then only the references in that file will be
   --  returned. This is also more efficient. Alternatively, In_Scope can be
   --  specified to limit the list of references to the ones that appear
   --  in the scope of In_Scope.
   --  End_Line can specify a range of lines
   --  Source files with no LI file are reported through File_Has_No_LI_Report.
   --  You must destroy the iterator when you are done with it, to avoid
   --  memory leaks.

   function At_End (Iter : Entity_Reference_Iterator) return Boolean;
   --  Whether there are no more reference to return

   procedure Next (Iter : in out Entity_Reference_Iterator);
   --  Move to the next reference to the entity

   function Get (Iter : Entity_Reference_Iterator) return Entity_Reference;
   --  Return the current reference. This might be No_Entity_Reference if the
   --  iterator needs to parse more source files to get that information.
   --  The search is done with small steps, so that this can be easily put in
   --  the background, including the parsing of the source files.

   procedure Destroy (Iter : in out Entity_Reference_Iterator);
   --  Free the memory used by Iter

   --------------
   -- Renaming --
   --------------

   function Renaming_Of
     (Entity : Entity_Information) return Entity_Information;
   --  If Entity is a renaming of another entity (or a typedef for another
   --  type), return the entity that is renamed. Return null if there is no
   --  renaming.

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
   --  Free the memory occupied by Iter

   -------------
   -- Callers --
   -------------

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

   -------------
   -- Parents --
   -------------

--     type Parent_Iterator is private;

--     function Get_Parent_Types
--       (Entity    : Entity_Information;
--        Recursive : Boolean := False) return Parent_Iterator;
   --  Return the first parent of the entity. In object-oriented languages,
   --  this would be the classes Entities derives from. In Ada, this includes
   --  the parent type of a type or subtype.
--
--     procedure Next (Iter : in out Parent_Iterator);
--     --  Move to the next parent of the entity
--
--     function At_End (Iter : Parent_Iterator) return Boolean;
--     --  Return True if there are no more remaining parent for this entity
--
--     function Get (Iter : Parent_Iterator) return Entity_Information;
   --  Return the current parent of the entity. null might be returned if GPS
   --  hasn't finished parsing all the files.
--
--     procedure Destroy (Iter : in out Parent_Iterator);
   --  Free the memory occupied by the iterator


private

   type Dependency_Iterator is record
      Importing             : Projects.Imported_Project_Iterator;
      --  List of projects to check

      Source_Files : VFS.File_Array_Access;
      Current_File : Natural;
      --  The list of source files in the current project

      Total_Progress        : Natural;
      Current_Progress      : Natural;

      Include_Self          : Boolean;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Single_Source_File    : Boolean;

      File                  : Source_File;
      Dep_Index             : Dependency_Arrays.Index_Type;
   end record;

   type File_Dependency_Iterator is record
      File                  : Source_File;
      Dep_Index             : Dependency_Arrays.Index_Type;
   end record;

   type Subprogram_Iterator is record
      Index  : Entity_Reference_Arrays.Index_Type;
      Entity : Entity_Information;
      Cache_Current : Entity_Information;
   end record;

   type Entity_Reference_Iterator is record
      Need_To_Update_Files : Boolean;
      --  True if we are in the process of parsing all the required files to
      --  get the full xref information

      Decl_Returned : Boolean;
      --  Whether the declaration has already been returned or not

      Index   : Entity_Reference_Arrays.Index_Type;
      Entity  : Entity_Information;
      In_File : Source_File;
      Start_Line, Last_Line  : Integer;

      Deps    : Dependency_Iterator;
   end record;

   type Calls_Iterator is record
      Entity  : Entity_Information;
      Iter    : Entities_Tries.Iterator;
      EL      : Entity_Information_List_Access;
      Index   : Entity_Information_Arrays.Index_Type;
   end record;

end Entities.Queries;
