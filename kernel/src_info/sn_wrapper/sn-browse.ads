with SN.Xref_Pools; use SN.Xref_Pools;

package SN.Browse is
   Unlink_Failure    : exception;
   Spawn_Failure     : exception;
   Temp_File_Failure : exception;

   DB_Dir_Name          : constant String := ".gpssnprj";
   --  Name of directory where all SN files reside

   DB_File_Name         : constant String := "data";
   --  Name of the SN database files

   Xref_Pool_Filename   : constant String := "xrefs";
   --  Name of file for persistent xref pool

   procedure Browse
     (File_Name, DB_Directory, Browser_Name : in String;
      Xrefs : in out Xref_Pool);
   --  Executes given browser on the file so that all database files
   --  should be placed in the specified directory.
   --  A number of exceptions may be thrown to signal error during
   --  process spawning, file unlinking...

   procedure Generate_Xrefs (DB_Directory : in String);
   --  Removes .by and .to tables in the DB_Directory and
   --  does the same as  "cat *.xref | dbimp" so that generated cross
   --  reference tables should lie in the DB_Directory.

end SN.Browse;

