with GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.Expect,
     OS_Utils,
     System,
     Ada.Strings.Fixed,
     Ada.Exceptions;
with Basic_Types;
with SN.Xref_Pools; use SN.Xref_Pools;

with String_Utils; use String_Utils;

use  GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.Expect,
     Ada.Strings.Fixed,
     Ada.Exceptions;

with GNAT.OS_Lib; use GNAT.OS_Lib;
--  with Ada.Text_IO; use Ada.Text_IO;

package body SN.Browse is

   DBIMP    : constant String := "dbimp";
   --  SN database engine

   CBrowser : constant String := "cbrowser";
   --  SN C and C++ parser

   DBIMP_Path     : String_Access := null;
   --  full path to DBIMP (found in PATH) or null,
   --  if DBIMP is not in PATH
   CBrowser_Path  : String_Access := null;
   --  full path to CBrowser (found in PATH) or null,
   --  if CBrowser is not in PATH

   procedure Output_Filter
     (PD        : in GNAT.Expect.Process_Descriptor'Class;
      Str       : in String;
      User_Data : in System.Address := System.Null_Address);
   --  Output filter

   -------------------
   -- Output_Filter --
   -------------------
   procedure Output_Filter
     (PD        : in GNAT.Expect.Process_Descriptor'Class;
      Str       : in String;
      User_Data : in System.Address := System.Null_Address) is
      pragma Unreferenced (PD);
      pragma Unreferenced (Str);
      pragma Unreferenced (User_Data);
   begin
      --  Put ("dbimp: " & Str);
      null;
   end Output_Filter;

   ------------
   -- Browse --
   ------------

   procedure Browse
     (File_Name    : String;
      DB_Directory : String;
      PD           : out GNAT.Expect.Process_Descriptor)
   is
      Args : Argument_List (1 .. 6);
   begin
      --  ??? This should be detected much earlier, so that we can report the
      --  errors to GPS.
      if null = DBIMP_Path then
         Raise_Exception (Spawn_Failure'Identity,
           DBIMP & ": not found in PATH");
      end if;

      if null = CBrowser_Path then
         Raise_Exception (Spawn_Failure'Identity,
           CBrowser & ": not found in PATH");
      end if;

      --  Execute browser
      Args := (1 => new String' ("-n"),
               2 => new String' (DB_Directory & DB_File_Name),
               3 => new String' ("-p"),
               4 => new String' (DBIMP_Path.all),
               5 => new String' ("-y"),
               6 => new String' (File_Name));

      GNAT.Expect.Non_Blocking_Spawn (PD, CBrowser_Path.all, Args,
         Err_To_Out => True);
      GNAT.Expect.Add_Filter (PD, Output_Filter'Access, GNAT.Expect.Output);
      Basic_Types.Free (Args);
   end Browse;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs
     (DB_Directory : in String;
      Temp_Name    : out GNAT.OS_Lib.Temp_File_Name;
      PD           : out GNAT.Expect.Process_Descriptor)
   is
      BY_File_Name : String := Name_As_Directory (DB_Directory)
                      & DB_File_Name & ".by" & ASCII.Nul;
      TO_File_Name : String := Name_As_Directory (DB_Directory)
                      & DB_File_Name & ".to" & ASCII.Nul;
      Dir          : Dir_Type;
      Last         : Natural;
      Dir_Entry    : String (1 .. 1024);
      --  1024 is the value of FILENAME_MAX in stdio.h (see
      --  GNAT.Directory_Operations)
      Success      : Boolean;
      Args         : Argument_List (1 .. 3);
      Content      : String_Access;
      Temp_File    : File_Descriptor;
   begin

      --  remove .to and .by tables
      if File_Exists (BY_File_Name) then
         Delete_File (BY_File_Name'Address, Success);
         if not Success then
            raise Unlink_Failure;
         end if;
      end if;

      if File_Exists (TO_File_Name) then
         Delete_File (TO_File_Name'Address, Success);
         if not Success then
            raise Unlink_Failure;
         end if;
      end if;

      --  start dbimp
      Create_Temp_File (Temp_File, Temp_Name);
      if Temp_File = Invalid_FD then
         raise Temp_File_Failure;
      end if;

      --  enumerate all .xref files in the target directory
      --  and copy them into the temp file
      Open (Dir, DB_Directory);
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Read (Dir, Dir_Entry, Last); -- read first directory entry
      while Last /= 0 loop
         if Tail (Dir_Entry (1 .. Last), Xref_Suffix'Length) = Xref_Suffix then
            Content := OS_Utils.Read_File (Name_As_Directory (DB_Directory)
               & Dir_Entry (1 .. Last));
            if null /= Content then
               if Content'Length
                  /= Write (Temp_File, Content.all'Address, Content'Length)
               then
                  raise Temp_File_Failure;
               end if;
               Free (Content);
            end if;
         end if;
         Read (Dir, Dir_Entry, Last); -- read next directory entry
      end loop;
      Close (Dir);

      if null = DBIMP_Path then
         raise Spawn_Failure;
      end if;

      Args := (1 => new String' (DB_Directory & DB_File_Name),
               2 => new String' ("-f"),
               3 => new String' (Temp_Name));

      GNAT.Expect.Non_Blocking_Spawn
        (PD, DBIMP_Path.all, Args, Err_To_Out => True);
      GNAT.Expect.Add_Filter (PD, Output_Filter'Access, GNAT.Expect.Output);
      Basic_Types.Free (Args);
   end Generate_Xrefs;

   procedure Is_Alive
     (PD : in out GNAT.Expect.Process_Descriptor;
      Status : out Boolean)
   is
      Result       : GNAT.Expect.Expect_Match;
   begin
      Status := False;
      GNAT.Expect.Expect (PD, Result, "", 1);
      if Result = GNAT.Expect.Expect_Timeout then
         Status := True;
         return;
      end if;
      GNAT.Expect.Close (PD);
      return;
   exception
      when GNAT.Expect.Process_Died =>
         GNAT.Expect.Close (PD);
   end Is_Alive;

   ---------------------
   -- Delete_Database --
   ---------------------

   procedure Delete_Database (DB_Directory : in String) is
      Dir          : Dir_Type;
      Last         : Natural;
      Dir_Entry    : String (1 .. 1024);
      --  1024 is the value of FILENAME_MAX in stdio.h (see
      --  GNAT.Directory_Operations)
   begin
      if not Is_Directory (DB_Directory) then
         --  ignore if dir does not exist
         return;
      end if;

      --  enumerate all files in the target directory
      Open (Dir, DB_Directory);
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Read (Dir, Dir_Entry, Last); -- read first directory entry
      while Last /= 0 loop --  delete all files in directory
         declare
            F : String := Name_As_Directory (DB_Directory)
              & Dir_Entry (1 .. Last) & ASCII.NUL;
            Success : Boolean;
         begin
            Delete_File (F'Address, Success);
         end;
         Read (Dir, Dir_Entry, Last); -- read next directory entry
      end loop;
      Close (Dir);
   end Delete_Database;

begin
   --  locate utilities in PATH
   DBIMP_Path     := Locate_Exec_On_Path (DBIMP);
   CBrowser_Path  := Locate_Exec_On_Path (CBrowser);
end SN.Browse;
