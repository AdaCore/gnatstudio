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
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status);
   --  Find the entity that is referenced at the given location.

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

end Entities.Queries;
