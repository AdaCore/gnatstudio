with GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.Expect,
     OS_Utils,
     System,
     Ada.Strings.Fixed,
     Ada.Unchecked_Deallocation,
     Ada.Exceptions;

use  GNAT.Directory_Operations,
     GNAT.IO_Aux,
     Ada.Strings.Fixed,
     Ada.Exceptions;

with GNAT.OS_Lib; use GNAT.OS_Lib;
--  with Ada.Text_IO; use Ada.Text_IO;

package body SN.Browse is

   DBIMP : constant String := "dbimp";
   --  SN database engine

   DBIMP_Path : String_Access := null;
   --  full path to DBIMP (found in PATH) or null, if DBIMP is not in PATH

   procedure Free is new Ada.Unchecked_Deallocation
                              (String, String_Access);

   procedure Delete (Args : in out Argument_List_Access);

   procedure Delete (Args : in out Argument_List_Access) is
      procedure Delete_Array is new Ada.Unchecked_Deallocation
                              (Argument_List, Argument_List_Access);
   begin
      for I in Args.all'Range loop
         Free (Args.all (I));
      end loop;
      Delete_Array (Args);
   end Delete;

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
     (File_Name, DB_Directory, Browser_Name : in String;
      Xrefs : in out Xref_Pool)
   is
      Xref_File_Name      : String_Access;
      Success             : Boolean;
      Args                : Argument_List_Access;
      DBUtil_Path         : String_Access;
      PD                  : GNAT.Expect.Process_Descriptor;
      Result              : GNAT.Expect.Expect_Match;
--    Pid                 : GNAT.OS_Lib.Process_Id;
   begin
      --  check DB_Directory exists
      if not Is_Directory (DB_Directory) then
         --  the target directory does not exists, create it
         Make_Dir (DB_Directory);
      end if;

      Xref_File_Name := Xref_Filename_For (File_Name, DB_Directory, Xrefs);

      --  unlink cross reference file, if any
      if File_Exists (DB_Directory & Directory_Separator
                      & Xref_File_Name.all) then
         declare
            Xref_File_Name_Nul  : String
               := DB_Directory & Directory_Separator
                  & Xref_File_Name.all & ASCII.NUL;
         begin
            Delete_File (Xref_File_Name_Nul'Address, Success);
         end;
         if not Success then
            Raise_Exception (Unlink_Failure'Identity,
               Xref_File_Name.all);
         end if;
      end if;

      if null = DBIMP_Path then
         Raise_Exception (Spawn_Failure'Identity,
           DBIMP & ": not found in PATH");
      end if;
      --  Execute browser
      Args := Argument_String_To_List (
          "-n " & DB_Directory & Directory_Separator & DB_File_Name
          & " -p " & DBIMP_Path.all
          & " -x " & DB_Directory & Directory_Separator & Xref_File_Name.all
          & " " & File_Name);

      DBUtil_Path := Locate_Exec_On_Path (Browser_Name);
      if null = DBUtil_Path then
         Delete (Args);
         Raise_Exception (Spawn_Failure'Identity,
           Browser_Name & ": not found in PATH");
      end if;

      GNAT.Expect.Non_Blocking_Spawn (PD, DBUtil_Path.all, Args.all,
         Err_To_Out => True);
      GNAT.Expect.Add_Filter (PD, Output_Filter'Access, GNAT.Expect.Output);
      loop
         begin
            GNAT.Expect.Expect (PD, Result, "", -1);
         exception
            when GNAT.Expect.Process_Died => exit;
         end;
      end loop;
      GNAT.Expect.Close (PD);
      Delete (Args);
      Free (DBUtil_Path);
   end Browse;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs (DB_Directory : in String) is
      BY_File_Name : String := DB_Directory & Directory_Separator
                      & DB_File_Name & ".by" & ASCII.Nul;
      TO_File_Name : String := DB_Directory & Directory_Separator
                      & DB_File_Name & ".to" & ASCII.Nul;
      Dir          : Dir_Type;
      Last         : Natural;
      Dir_Entry    : String (1 .. 1024);
      --  1024 is the value of FILENAME_MAX in stdio.h (see
      --  GNAT.Directory_Operations)
      Success      : Boolean;
      Args         : Argument_List_Access;
      Content      : String_Access;
      Temp_File    : File_Descriptor;
      Temp_Name    : Temp_File_Name;
      PD           : GNAT.Expect.Process_Descriptor;
      Result       : GNAT.Expect.Expect_Match;
--      Pid          : GNAT.OS_Lib.Process_Id;
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
            Content := OS_Utils.Read_File (DB_Directory
               & Directory_Separator & Dir_Entry (1 .. Last));
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
      Close (Temp_File);

      Args := Argument_String_To_List (
          DB_Directory & Directory_Separator & DB_File_Name
          & " -f " & Temp_Name
      );

      if null = DBIMP_Path then
         Delete (Args);
         raise Spawn_Failure;
      end if;

      GNAT.Expect.Non_Blocking_Spawn (PD, DBIMP_Path.all, Args.all,
         Err_To_Out => True);
      GNAT.Expect.Add_Filter (PD, Output_Filter'Access, GNAT.Expect.Output);
--      Wait_Process (Pid, Success);
      loop
         begin
            GNAT.Expect.Expect (PD, Result, "", -1);
         exception
            when GNAT.Expect.Process_Died => exit;
         end;
      end loop;
      Delete (Args);
      GNAT.Expect.Close (PD);

      Delete_File (Temp_Name'Address, Success);
      if not Success then
         raise Unlink_Failure;
      end if;
   end Generate_Xrefs;

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
            F : String := DB_Directory
              & Directory_Separator & Dir_Entry (1 .. Last) & ASCII.NUL;
            Success : Boolean;
         begin
            Delete_File (F'Address, Success);
         end;
         Read (Dir, Dir_Entry, Last); -- read next directory entry
      end loop;
      Close (Dir);
   end Delete_Database;

begin
   --  locate dbimp utility in PATH
   DBIMP_Path := Locate_Exec_On_Path (DBIMP);
end SN.Browse;
