-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO_Aux;               use GNAT.IO_Aux;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Mmap;                 use GNAT.Mmap;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Basic_Types;
with SN.Xref_Pools; use SN.Xref_Pools;
with File_Utils;    use File_Utils;
with String_Utils;  use String_Utils;
with Traces;        use Traces;

package body SN.Browse is

   Me : constant Debug_Handle := Create ("SN.Browse");

   ------------
   -- Browse --
   ------------

   procedure Browse
     (File_Name     : String;
      DB_Directory  : String;
      DBIMP_Path    : String;
      Cbrowser_Path : String;
      PD            : out GNAT.Expect.TTY.TTY_Process_Descriptor)
   is
      Args : Argument_List (1 .. 6);
   begin
      --  Execute browser
      Args :=
        (1 => new String'("-n"),
         2 => new String'(DB_Directory & DB_File_Name),
         3 => new String'("-p"),
         4 => new String'(DBIMP_Path),
         5 => new String'("-y"),
         6 => new String'(File_Name));

      Trace (Me, "Spawn: " & Cbrowser_Path
             & " " & Argument_List_To_String (Args));
      GNAT.Expect.Non_Blocking_Spawn
        (PD, Cbrowser_Path, Args, Err_To_Out => True);
      Basic_Types.Free (Args);
   end Browse;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs
     (DB_Directories : GNAT.Strings.String_List_Access;
      DBIMP_Path     : String;
      Started        : out Boolean;
      Temp_Name      : out GNAT.OS_Lib.Temp_File_Name;
      PD             : out GNAT.Expect.TTY.TTY_Process_Descriptor)
   is
      DB_Directory : String renames DB_Directories (1).all;
      LV_File_Name : constant String := DB_Directory & DB_File_Name & ".lv";
      TO_File_Name : constant String := DB_Directory & DB_File_Name & ".to";
      F_File_Name  : constant String := DB_Directory & DB_File_Name & ".f";
      Dir          : Dir_Type;
      Last         : Natural;
      Dir_Entry    : String (1 .. 8192);
      Success      : Boolean;
      Args         : Argument_List_Access;
      Content      : GNAT.Strings.String_Access;
      Temp_File    : File_Descriptor;

   begin
      --  Check whether we actually need to run dbimp or not. Since that can
      --  take a while, no need to do it if the files are already up-to-date

      if Active (Me) then
         if File_Exists (F_File_Name) then
            Trace
              (Me, ".f timestamp: "
               & Image (File_Time_Stamp (F_File_Name), "%Y-%m-%d %H:%M:%S"));
         else
            Trace (Me, "No .f file");
         end if;
         if File_Exists (TO_File_Name) then
            Trace
              (Me, ".to timestamp: "
               & Image (File_Time_Stamp (TO_File_Name), "%Y-%m-%d %H:%M:%S"));
         else
            Trace (Me, "No .to file");
         end if;
      end if;

      if File_Exists (TO_File_Name)
        and then File_Exists (F_File_Name)
        and then
          File_Time_Stamp (TO_File_Name) >= File_Time_Stamp (F_File_Name)
      then
         Trace (Me, "No need to start dbimp, since db is up to date");
         Started := False;
         return;
      end if;

      --  remove .to and .lv tables
      if File_Exists (LV_File_Name) then
         Delete_File (LV_File_Name, Success);

         if not Success then
            raise Unlink_Failure;
         end if;
      end if;

      if File_Exists (TO_File_Name) then
         Delete_File (TO_File_Name, Success);

         if not Success then
            raise Unlink_Failure;
         end if;
      end if;

      --  At this point, the .xref files contain various commands for dbimp,
      --  so we collect them all in a single temporary file.
      --  These commands look like:
      --     COMMAND;KEY;DATA
      --  where COMMAND is between PAF_FILE .. PAF_COMMENT_DEF (see sn.h)
      --  COMMANDS 0, -1 and -2 are used for deletion of entries in the
      --  database (for instance "-2;0;x.c" deletes all xref for x.c)

      Create_Temp_File (Temp_File, Temp_Name);
      if Temp_File = Invalid_FD then
         raise Temp_File_Failure;
      end if;

      if Is_Directory (DB_Directory) then
         Open (Dir, DB_Directory);

         loop
            Read (Dir, Dir_Entry, Last);
            exit when Last = 0;

            if Tail (Dir_Entry (1 .. Last), Xref_Suffix'Length) =
              Xref_Suffix
            then
               Content := Read_Whole_File
                 (Name_As_Directory (DB_Directory) & Dir_Entry (1 .. Last));

               if Content /= null then
                  if Content'Length /=
                    Write (Temp_File, Content.all'Address, Content'Length)
                  then
                     Free (Content);
                     raise Temp_File_Failure;
                  end if;

                  Free (Content);
               end if;
            end if;
         end loop;

         Close (Dir);
      end if;

      Close (Temp_File);

      Args     := new Argument_List (1 .. 3 + DB_Directories'Length);
      Args (1) := new String'("-f");
      Args (2) := new String'(Temp_Name (1 .. Temp_Name'Last - 1));
      Args (3) := new String'("-l");

      for D in DB_Directories'Range loop
         Args (4 + D - DB_Directories'First) :=
           new String'(DB_Directories (D).all & DB_File_Name);
      end loop;

      Trace (Me, "Spawn: " & DBIMP_Path
             & " " & Argument_List_To_String (Args.all));
      Non_Blocking_Spawn (PD, DBIMP_Path, Args.all, Err_To_Out => True);
      GNAT.OS_Lib.Free (Args);

      Started := True;
   end Generate_Xrefs;

   --------------
   -- Is_Alive --
   --------------

   procedure Is_Alive
     (PD     : in out GNAT.Expect.TTY.TTY_Process_Descriptor;
      Status : out Boolean)
   is
      Result : Expect_Match;
   begin
      Expect (Process_Descriptor (PD), Result, "", 0);
      Status := True;

   exception
      when Process_Died =>
         Close (Process_Descriptor (PD));
         Status := False;
   end Is_Alive;

   ---------------------
   -- Delete_Database --
   ---------------------

   procedure Delete_Database (DB_Directory : String) is
      Dir       : Dir_Type;
      Last      : Natural;
      Dir_Entry : String (1 .. 1024);
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
            pragma Unreferenced (Success);
         begin
            Delete_File (F'Address, Success);
         end;
         Read (Dir, Dir_Entry, Last); -- read next directory entry
      end loop;
      Close (Dir);
   end Delete_Database;

end SN.Browse;
