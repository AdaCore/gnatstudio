-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.Expect,
     OS_Utils,
     System,
     Ada.Strings.Fixed;
with Basic_Types;
with SN.Xref_Pools; use SN.Xref_Pools;

with String_Utils; use String_Utils;

use  GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.Expect,
     Ada.Strings.Fixed;

with GNAT.OS_Lib; use GNAT.OS_Lib;
--  with Ada.Text_IO; use Ada.Text_IO;

package body SN.Browse is

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
      User_Data : in System.Address := System.Null_Address)
   is
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
     (File_Name     : String;
      DB_Directory  : String;
      DBIMP_Path    : String;
      Cbrowser_Path : String;
      PD            : out GNAT.Expect.Process_Descriptor)
   is
      Args : Argument_List (1 .. 6);
   begin
      --  Execute browser
      Args := (1 => new String' ("-n"),
               2 => new String' (DB_Directory & DB_File_Name),
               3 => new String' ("-p"),
               4 => new String' (DBIMP_Path),
               5 => new String' ("-y"),
               6 => new String' (File_Name));

      GNAT.Expect.Non_Blocking_Spawn
        (PD, Cbrowser_Path, Args, Err_To_Out => True);
      GNAT.Expect.Add_Filter (PD, Output_Filter'Access, GNAT.Expect.Output);
      Basic_Types.Free (Args);
   end Browse;

   --------------------
   -- Generate_Xrefs --
   --------------------

   procedure Generate_Xrefs
     (DB_Directories : GNAT.OS_Lib.String_List_Access;
      DBIMP_Path     : String;
      Temp_Name      : out GNAT.OS_Lib.Temp_File_Name;
      PD             : out GNAT.Expect.Process_Descriptor)
   is
      LV_File_Name : constant String
         := DB_Directories (1).all & DB_File_Name & ".lv";
      TO_File_Name : constant String
         := DB_Directories (1).all & DB_File_Name & ".to";
      Dir          : Dir_Type;
      Last         : Natural;
      Dir_Entry    : String (1 .. 1024);
      --  1024 is the value of FILENAME_MAX in stdio.h (see
      --  GNAT.Directory_Operations)
      Success      : Boolean;
      Args         : Argument_List_Access;
      Content      : String_Access;
      Temp_File    : File_Descriptor;

   begin
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

      --  start dbimp
      Create_Temp_File (Temp_File, Temp_Name);

      if Temp_File = Invalid_FD then
         raise Temp_File_Failure;
      end if;

      --  enumerate all .xref files in the target directory
      --  and copy them into the temp file
      Open (Dir, DB_Directories (1).all);

      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Read (Dir, Dir_Entry, Last); -- read first directory entry
      while Last /= 0 loop
         if Tail (Dir_Entry (1 .. Last), Xref_Suffix'Length) = Xref_Suffix then
            Content := OS_Utils.Read_File
               (Name_As_Directory (DB_Directories (1).all)
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

      Args := new Argument_List (1 .. DB_Directories'Length + 3);
      Args (1) := new String' ("-f");
      Args (2) := new String' (Temp_Name);
      Args (3) := new String' ("-l");

      for J in DB_Directories.all'Range loop
         Args (4 + J - DB_Directories.all'First)
            := new String' (DB_Directories (J).all & DB_File_Name);
      end loop;

      GNAT.Expect.Non_Blocking_Spawn
        (PD, DBIMP_Path, Args.all, Err_To_Out => True);
      GNAT.Expect.Add_Filter (PD, Output_Filter'Access, GNAT.Expect.Output);
      GNAT.OS_Lib.Free (Args);
   end Generate_Xrefs;

   --------------
   -- Is_Alive --
   --------------

   procedure Is_Alive
     (PD : in out GNAT.Expect.Process_Descriptor;
      Status : out Boolean)
   is
      Result : GNAT.Expect.Expect_Match;
   begin
      Status := False;
      GNAT.Expect.Expect (PD, Result, "", 1);

      if Result = GNAT.Expect.Expect_Timeout then
         Status := True;
         return;
      end if;

      GNAT.Expect.Close (PD);

   exception
      when GNAT.Expect.Process_Died =>
         GNAT.Expect.Close (PD);
   end Is_Alive;

   ---------------------
   -- Delete_Database --
   ---------------------

   procedure Delete_Database (DB_Directory : String) is
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

end SN.Browse;
