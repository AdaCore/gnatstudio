with SN.Xref_Pools; use SN.Xref_Pools;
with GNAT.Expect;   use GNAT.Expect;

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
     (File_Name, DB_Directory : in String;
      Xrefs : in out Xref_Pool; PD : out GNAT.Expect.Process_Descriptor);
   --  Starts language browser on the file so that all database files
   --  should be placed in the specified directory.
   --  A number of exceptions may be thrown to signal error during
   --  process spawning, file unlinking...

   procedure Generate_Xrefs
     (DB_Directory : in String;
      Temp_Name    : out GNAT.OS_Lib.Temp_File_Name;
      PD           : out GNAT.Expect.Process_Descriptor);
   --  Removes .by and .to tables in the DB_Directory and
   --  does the same as  "cat *.xref | dbimp" so that generated cross
   --  reference tables should lie in the DB_Directory.
   --  on error an exception is thrown

   procedure Delete_Database (DB_Directory : in String);
   --  Removes all files from SN DB directory except xref pool

   procedure Is_Alive
     (PD : in out GNAT.Expect.Process_Descriptor;
      Status : out Boolean);
   --  checks if the process is still running. If the process exited, its
   --  descriptor is closed and False returned in Status.

end SN.Browse;

