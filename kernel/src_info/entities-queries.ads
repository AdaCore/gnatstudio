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
      In_File               : Source_File := null);
   --  Find all the references to the entity. This doesn't return the location
   --  for the declaration of the entity, use Get_Declaration_Of in addition.
   --  if In_File is specified, then only the references in that file will be
   --  returned. This is also more efficient.
   --  Source files with no LI file are reported through File_Has_No_LI_Report.
   --  You must destroy the iterator when you are done with it, to avoid
   --  memory leaks.

   function At_End (Iter : Entity_Reference_Iterator) return Boolean;
   --  Whether there are no more reference to return

   procedure Next (Iter : in out Entity_Reference_Iterator);
   --  Move to the next reference to the entity

   function Get (Iter : Entity_Reference_Iterator) return File_Location;
   --  Return the current reference. This might be No_File_Location if the
   --  iterator needs to parse more source files to get that information.
   --  The search is done with small steps, so that this can be easily put in
   --  the background, including the parsing of the source files.

   procedure Destroy (Iter : in out Entity_Reference_Iterator);
   --  Free the memory used by Iter

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

private
   type Scope_Tree_Iterator is record
      File   : Source_File;
      Entity : Entity_Information;
   end record;

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

      Index   : Entity_Reference_Arrays.Index_Type;
      Entity  : Entity_Information;
      In_File : Source_File;

      Deps    : Dependency_Iterator;
   end record;

end Entities.Queries;
