package SN.Browse is
   Unlink_Failure   : exception;
   Spawn_Failure    : exception;

   DB_File_Name     : constant String := "data";

   procedure Browse (File_Name, DB_Directory : in String;
         Browser_Name : in String := "cbrowser";
         DBUtils_Path : in String := "../sn/bin");
   --  Executes given browser on the file so that all database files
   --  should be placed in the specified directory.
   --  A number of exceptions may be thrown to signal error during
   --  process spawning, file unlinking...

   procedure Generate_Xrefs (DB_Directory : in String);
   --  Removes all .by and .to tables in the DB_Directory and
   --  executes "cat *.xref | dbimp" so that generated cross
   --  reference tables should lie in the DB_Directory.
end SN.Browse;

