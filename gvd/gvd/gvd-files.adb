-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Basic_Types;           use Basic_Types;
with GVD.Preferences;       use GVD.Preferences;
with GVD.Main_Window;       use GVD.Main_Window;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with String_Utils;          use String_Utils;
with Gtkada.Intl;           use Gtkada.Intl;
pragma Warnings (Off);
with GNAT.Expect;           use GNAT.Expect;
with GNAT.Expect.TTY;       use GNAT.Expect.TTY;
pragma Warnings (On);
with Ada.Unchecked_Deallocation;

package body GVD.Files is

   procedure Free is new Ada.Unchecked_Deallocation
     (Process_Descriptor'Class, Process_Descriptor_Access);

   -------------------
   -- Find_In_Cache --
   -------------------

   function Find_In_Cache
     (Window    : access GVD_Main_Window_Record'Class;
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
     (Contents    : out Basic_Types.String_Access;
      Error_Msg   : out Basic_Types.String_Access;
      Cache       : Basic_Types.File_Cache_List;
      Remote_Host : Basic_Types.String_Access := null)
   is
      F             : File_Descriptor;
      Length        : Positive;
      Name          : aliased constant String :=
        To_Host_Pathname (Cache.File_Name.all) & ASCII.NUL;
      Tmp_File      : GNAT.OS_Lib.Temp_File_Name;
      Args          : Argument_List (1 .. 2);
      Result        : Expect_Match;
      Descriptor    : Process_Descriptor_Access;
      Should_Delete : Boolean := False;
      Num_Tries     : constant Integer := 5;

   begin
      Error_Msg := null;
      Contents  := null;

      --  No need to do anything when File_Name is null.

      if Cache.File_Name.all = "" then
         Error_Msg := new String'(-"No file to edit");
         return;
      end if;

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

         if Use_Ptys then
            Descriptor := new TTY_Process_Descriptor;
         else
            Descriptor := new Process_Descriptor;
         end if;

         Non_Blocking_Spawn
           (Descriptor.all,
            Command     => Get_Pref (Remote_Copy),
            Args        => Args,
            Buffer_Size => 0,
            Err_To_Out  => True);

         begin
            Expect (Descriptor.all, Result, ".+", Timeout => -1);

            if Result /= Expect_Timeout then
               --  File was not found
               Error_Msg := new String' (Expect_Out (Descriptor.all));
               Delete_File (Tmp_File'Address, Should_Delete);
               Close (Descriptor.all);
               Free (Descriptor);
               return;
            end if;

         exception
            when Process_Died =>
               null;
         end;

         Close (Descriptor.all);
         Free (Descriptor);
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
            S   : String (1 .. Length);
            Pos : Natural;
         begin
            Length := Read (F, S'Address, Length);

            Cache.CR_Stripped := Get_Pref (Should_Strip_CR);

            --  Check whether we should do an automatic strip (since GtkText
            --  is somewhat unreliable and will sometimes put LF, sometimes CR
            --  & LF)
            if not Cache.CR_Stripped then
               Pos := S'First;

               while Pos <= S'Last
                 and then S (Pos) /= ASCII.CR
                 and then S (Pos) /= ASCII.LF
               loop
                  Pos := Pos + 1;
               end loop;

               Cache.CR_Stripped := Pos <= S'Last and then S (Pos) = ASCII.CR;
            end if;

            if Cache.CR_Stripped then
               Contents := new String' (Strip_CR (S));
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

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache
     (Window : access GVD_Main_Window_Record'Class;
      Force  : Boolean := True)
   is
      Tmp : File_Cache_List := Window.File_Caches;
      Next : File_Cache_List;
      Last_Kept : File_Cache_List := null;
   begin
      Window.File_Caches := null;
      while Tmp /= null loop
         Next := Tmp.Next;

         --  If the CR was already stripped, no need to free the file
         if Force
           or else
           (not Tmp.CR_Stripped and then Get_Pref (Should_Strip_CR))
         then
            Free (Tmp.File_Name);
            Free (Tmp.Line_Has_Code);
            Free (Tmp.Line_Parsed);
            Free (Tmp.File_Contents);
            Free (Tmp);
         else
            if Window.File_Caches = null then
               Window.File_Caches := Tmp;
            else
               Last_Kept.Next := Tmp;
            end if;
            Last_Kept := Tmp;
            Last_Kept.Next := null;
         end if;
         Tmp := Next;
      end loop;
   end Clear_Cache;

end GVD.Files;
