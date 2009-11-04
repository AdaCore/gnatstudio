-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2009, AdaCore             --
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

with Ada.Calendar;              use Ada.Calendar;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with SN.Xref_Pools;             use SN.Xref_Pools;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;

package body SN.Browse is

   Me : constant Debug_Handle := Create ("SN.Browse");

   ------------
   -- Browse --
   ------------

   procedure Browse
     (File_Name     : Virtual_File;
      DB_Directory  : Virtual_File;
      DBIMP_Path    : Virtual_File;
      Cbrowser_Path : Virtual_File;
      PD            : out GNAT.Expect.TTY.TTY_Process_Descriptor)
   is
      Args : Argument_List (1 .. 6);
   begin
      --  Execute browser
      Args :=
        (1 => new String'("-n"),
         2 => new String'
           (+(Create_From_Dir (DB_Directory, DB_File_Name).Full_Name (True))),
         3 => new String'("-p"),
         4 => new String'(+DBIMP_Path.Full_Name (True)),
         5 => new String'("-y"),
         6 => new String'(+File_Name.Full_Name (True)));

      Trace (Me, "Spawn: " & Cbrowser_Path.Display_Full_Name
             & " " & Argument_List_To_String (Args));
      GNAT.Expect.Non_Blocking_Spawn
        (PD, +Cbrowser_Path.Full_Name (True), Args, Err_To_Out => True);
      Free (Args);
   end Browse;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs
     (DB_Directories : GNATCOLL.VFS.File_Array;
      DBIMP_Path     : Virtual_File;
      Started        : out Boolean;
      Temp_Name      : out GNAT.OS_Lib.Temp_File_Name;
      PD             : out GNAT.Expect.TTY.TTY_Process_Descriptor)
   is
      DB_Directory : constant Virtual_File := DB_Directories (1);
      LV_File_Name : constant Virtual_File :=
                       Create_From_Dir
                         (DB_Directory, DB_File_Name & ".lv");
      TO_File_Name : constant Virtual_File :=
                       Create_From_Dir
                         (DB_Directory, DB_File_Name & ".to");
      F_File_Name  : constant Virtual_File :=
                       Create_From_Dir
                         (DB_Directory, DB_File_Name & ".f");
      Success      : Boolean;
      Args         : Argument_List_Access;
      Content      : GNAT.Strings.String_Access;
      Temp_File    : File_Descriptor;

   begin
      --  Check whether we actually need to run dbimp or not. Since that can
      --  take a while, no need to do it if the files are already up-to-date

      if Active (Me) then
         if F_File_Name.Is_Regular_File then
            Trace
              (Me, ".f timestamp: "
               & Image (File_Time_Stamp (F_File_Name),
                        "%Y-%m-%d %H:%M:%S"));
         else
            Trace (Me, "No .f file");
         end if;

         if TO_File_Name.Is_Regular_File then
            Trace
              (Me, ".to timestamp: "
               & Image (File_Time_Stamp (TO_File_Name),
                        "%Y-%m-%d %H:%M:%S"));
         else
            Trace (Me, "No .to file");
         end if;
      end if;

      if TO_File_Name.Is_Regular_File
        and then F_File_Name.Is_Regular_File
        and then
          File_Time_Stamp (TO_File_Name) >= File_Time_Stamp (F_File_Name)
      then
         Trace (Me, "No need to start dbimp, since db is up to date");
         Started := False;
         return;
      end if;

      --  remove .to and .lv tables
      if LV_File_Name.Is_Regular_File then
         LV_File_Name.Delete (Success);

         if not Success then
            raise Unlink_Failure;
         end if;
      end if;

      if TO_File_Name.Is_Regular_File then
         TO_File_Name.Delete (Success);

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

      if DB_Directory.Is_Directory then
         declare
            Files : File_Array_Access;
         begin
            Files := DB_Directory.Read_Dir;

            for J in Files'Range loop
               if Has_Suffix (Files (J), Xref_Suffix) then
                  Content := Read_File (Files (J));

                  if Content /= null then
                     if Content'Length /=
                       Write
                         (Temp_File, Content.all'Address, Content'Length)
                     then
                        Free (Content);
                        raise Temp_File_Failure;
                     end if;

                     Free (Content);
                  end if;
               end if;
            end loop;

            Unchecked_Free (Files);
         end;
      end if;

      Close (Temp_File);

      Args     := new Argument_List (1 .. 3 + DB_Directories'Length);
      Args (1) := new String'("-f");
      Args (2) := new String'(Temp_Name (1 .. Temp_Name'Last - 1));
      Args (3) := new String'("-l");

      for D in DB_Directories'Range loop
         Ensure_Directory (DB_Directories (D));

         Args (4 + D - DB_Directories'First) :=
           new String'(+DB_Directories (D).Full_Name (True) & (+DB_File_Name));
      end loop;

      Trace (Me, "Spawn: " & DBIMP_Path.Display_Full_Name
             & " " & Argument_List_To_String (Args.all));
      Non_Blocking_Spawn
        (PD, +DBIMP_Path.Full_Name (True), Args.all, Err_To_Out => True);
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

   procedure Delete_Database (DB_Directory : Virtual_File) is
      Files   : File_Array_Access;
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      if not Is_Directory (DB_Directory) then
         --  ignore if dir does not exist
         return;
      end if;

      --  enumerate all files in the target directory
      Files := Read_Dir (DB_Directory, Files_Only);

      for J in Files'Range loop
         Files (J).Delete (Success);
      end loop;

      Unchecked_Free (Files);
   end Delete_Database;

end SN.Browse;
