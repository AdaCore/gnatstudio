with VFS;
with Projects;

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
      Project_Of_File : Projects.Project_Type;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status);
   --  Find the entity that is referenced at the given location.

--     procedure Find_Next_Body
--       (Db                     : Entities_Database;
--        File_Name              : VFS.Virtual_File;
--        Project_Of_File        : Projects.Project_Type;
--        Entity_Name            : String;
--        Line                   : Positive;
--        Column                 : Positive;
--        Location               : out File_Location;
--        Status                 : out Find_Decl_Or_Body_Query_Status);
   --  Find the location of the body for the entity. If the entity has multiple
   --  bodies (as is the case for instance for separates in Ada), and
   --  (Line,Column) is already the location of one of the bodies, then this
   --  procedure returns the location of the next body.

end Entities.Queries;
