package Entities.Debug is

   procedure Dump (Db : Entities_Database);
   --  Dump the contents of the Db database to standard_output

   procedure Dump (Entity : Entity_Information; Full : Boolean; Name : String);
   procedure Dump (File   : Source_File);
   procedure Dump (Loc    : File_Location);


   procedure Set_Show_Timestamp (Show : Boolean := True);
   --  Whether timestamps should be displayed by the various dump subprograms.
   --  This should be set to False for the testsuites to get consistent
   --  results.

end Entities.Debug;
