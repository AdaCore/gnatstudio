-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GVD.Types;             use GVD.Types;
with GVD.Preferences;       use GVD.Preferences;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GVD.Strings;           use GVD.Strings;
with Ada.Text_IO;           use Ada.Text_IO;
with Gtkada.Intl;           use Gtkada.Intl;
pragma Warnings (Off);
with GNAT.Expect;           use GNAT.Expect;
pragma Warnings (On);

package body GVD.Files is

   -------------------
   -- Find_In_Cache --
   -------------------

   function Find_In_Cache
     (Window    : access Main_Debug_Window_Record'Class;
      File_Name : String) return File_Cache_List
   is
      Tmp : File_Cache_List := Window.File_Caches;
   begin
      while Tmp /= null loop
         if Tmp.File_Name /= null
           and then Tmp.File_Name.all = File_Name
         then
            return Tmp;
         end if;

         Tmp := Tmp.Next;
      end loop;

      Tmp := Window.File_Caches;
      Window.File_Caches := new File_Cache;
      Window.File_Caches.File_Name := new String' (File_Name);
      Window.File_Caches.Next := Tmp;
      return Window.File_Caches;
   end Find_In_Cache;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Contents    : out GVD.Types.String_Access;
      Error_Msg   : out GVD.Types.String_Access;
      Cache       : GVD.Types.File_Cache_List;
      Remote_Host : GVD.Types.String_Access := null)
   is
      F             : File_Descriptor;
      Length        : Positive;
      Name          : aliased constant String :=
        Cache.File_Name.all & ASCII.NUL;
      Tmp_File      : GNAT.OS_Lib.Temp_File_Name;
      Args          : Argument_List (1 .. 2);
      Result        : Expect_Match;
      Descriptor    : GNAT.Expect.Process_Descriptor;
      Should_Delete : Boolean := False;
      Num_Tries     : constant Integer := 5;

   begin
      Error_Msg := null;
      Contents  := null;

      --  Do we already know the file contents ?

      if Cache.File_Contents /= null then
         Contents := new String' (Cache.File_Contents.all);
         return;
      end if;

      --  Read the size of the file, we first try locally.
      F := Open_Read (Name'Address, Binary);

      --  If not found, and we are in remote mode, try to download the file
      if F = Invalid_FD
        and then Remote_Host /= null
        and then Remote_Host.all /= ""
      then
         --  Create a temporary file and get its name
         Create_Temp_File (F, Tmp_File);
         Close (F);
         Should_Delete := True;

         --  Download the file

         Args (1) := new String'
           (Remote_Host.all & ":" & Cache.File_Name.all);
         Args (2) := new String'
           (Tmp_File (Tmp_File'First .. Tmp_File'Last - 1));

         GNAT.Expect.Non_Blocking_Spawn
           (Descriptor,
            Command     => Current_Preferences.Remote_Copy.all,
            Args        => Args,
            Buffer_Size => 0,
            Err_To_Out  => True);

         begin
            Expect (Descriptor, Result, ".+", Timeout => -1);

            if Result /= Expect_Timeout then
               --  File was not found
               Error_Msg := new String' (Expect_Out (Descriptor));
               Delete_File (Tmp_File'Address, Should_Delete);
               return;
            end if;

         exception
            when Process_Died =>
               null;
         end;

         Free (Args (1));
         Free (Args (2));

         --  Work around a weird behavior of some rcp that do not make
         --  the file available immediately.

         for J in 1 .. Num_Tries loop
            F := Open_Read (Tmp_File'Address, Binary);
            exit when File_Length (F) > 0;
            Close (F);
            F := Invalid_FD;
            delay 0.1;
         end loop;
      end if;


      if F /= Invalid_FD then
         Length := Positive (File_Length (F));

         --  Allocate the buffer
         --  and strip the ^Ms from the string
         declare
            S : String (1 .. Length);
         begin
            Length := Read (F, S'Address, Length);

            if Need_To_Strip_Control_M then
               Contents := new String' (Strip_Control_M (S));
            else
               Contents := new String' (S);
            end if;

            --  Only save the contents in the cache for remote files
            if Remote_Host /= null
              and then Remote_Host.all /= ""
            then
               Cache.File_Contents := new String' (Contents.all);
            end if;
         end;

         if Should_Delete then
            Delete_File (Tmp_File'Address, Should_Delete);
         end if;

         Close (F);
         return;
      end if;

      if Should_Delete then
         Delete_File (Tmp_File'Address, Should_Delete);
      end if;

      Error_Msg := new String'(-"File not found: " & Cache.File_Name.all);
   end Load_File;

end GVD.Files;
