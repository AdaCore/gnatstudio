package Entities.Debug is

   procedure Dump (Db : Entities_Database);
   --  Dump the contents of the Db database to standard_output

   procedure Dump (Entity : Entity_Information; Full : Boolean; Name : String);
   procedure Dump (File   : Source_File);
   procedure Dump (Loc    : File_Location);

end Entities.Debug;
