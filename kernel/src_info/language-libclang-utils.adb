------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNAT.Expect;     use GNAT.Expect;
with GNAT.Expect.TTY; use GNAT.Expect.TTY;
with GNAT.OS_Lib;
with GNAT.Regpat;     use GNAT.Regpat;

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNAT.Strings; use GNAT.Strings;

package body Language.Libclang.Utils is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("COMPLETION_LIBCLANG.UTILS", On);

   -------------------------------
   -- Get_Compiler_Search_Paths --
   -------------------------------

   Last_Project : Project_Type;
   Last_Lang    : GNAT.Strings.String_Access;
   Last_Result  : Unbounded_String_Array (1 .. 512);
   Last_Result_Index : Natural;
   --  This acts as a very basic cache
   --  ??? This is horrible
   --  ??? We should cache this seriously, in a hash table indexed by
   --  project/language, cleaned on project/scenario switch.

   function Get_Compiler_Search_Paths
     (Kernel   : Core_Kernel;
      Project  : Project_Type;
      Language : String) return Unbounded_String_Array
   is
      The_Exec         : GNAT.Strings.String_Access;
      The_Exec_On_Path : GNAT.Strings.String_Access;
      Result           : Unbounded_String_Array (1 .. 512);
      --  Unlikely to go over 512 search dirs
      First_Free       : Natural := 1;
   begin
      if Last_Lang /= null
        and then Last_Lang.all = Language
        and then Last_Project = Project
      then
         --  Return the cached result
         return Last_Result (1 .. Last_Result_Index);
      end if;

      --  ??? This won't work on remote hosts

      --  Heuristic for finding the default search path:
      --   - we find the compiler driver for this language
      --   - we create a dummy "t.h"
      --   - we invoke <driver> -v t.h
      --   - we expect an output containing:
      --        (something)
      --        #include <...> search starts here:
      --         dir1
      --         dir2
      --         ...
      --         dirN
      --        End of search list.
      --        (something)
      --   - we get dir1 .. dirN from the above.

      --  First find the driver.
      --  Heuristic for finding the driver:
      --     A) we look for an explicitly set Compiler'Driver setting
      --     B) if this is not explicitely set, we use
      --          <target>gcc for C
      --          <target>cpp for C++

      The_Exec := new String'
        (Attribute_Value (Project      => Project,
                          Attribute    => Compiler_Driver_Attribute,
                          Index        => Language,
                          Default      => "",
                          Use_Extended => False));

      if The_Exec.all /= "" then
         Trace (Me, "driver specified in Compiler'Driver: " & The_Exec.all);
      else
         Trace (Me, "no driver specified in Compiler'Driver");

         --  A) has failed, we fall back on B)
         GNAT.Strings.Free (The_Exec);

         declare
            Target        : constant String := Get_Target (Kernel);
            Target_Prefix : GNAT.Strings.String_Access;
         begin
            if Target = "" then
               Target_Prefix := new String'("");
            else
               Target_Prefix := new String'(Target & "-");
            end if;

            if To_Lower (Language) = "c" then
               The_Exec := new String'(Target_Prefix.all & "gcc");
            else
               --  Assume libclang is only for C and C++
               The_Exec := new String'(Target_Prefix.all & "cpp");
            end if;

            Trace (Me, "using driver " & The_Exec.all);

            GNAT.Strings.Free (Target_Prefix);
         end;
      end if;

      --  At this stage we have a driver, now let's run it

      The_Exec_On_Path := GNAT.OS_Lib.Locate_Exec_On_Path (The_Exec.all);
      GNAT.Strings.Free (The_Exec);

      if The_Exec_On_Path = null then
         Trace (Me, "driver not found on PATH");
         return (1 .. 0 => <>);
      end if;

      declare
         Fd      : TTY_Process_Descriptor;
         All_Match : constant Pattern_Matcher := Compile
           (".+", Multiple_Lines);
         Dir     : Virtual_File;
         Tmp_H   : Virtual_File;
         Tmp_H_W : Writable_File;
         Args    : GNAT.OS_Lib.Argument_List (1 .. 2);
         Match   : Expect_Match;
         Ignored : Boolean;
         Listing : Boolean := False;
      begin
         Dir := Project.Object_Dir;

         if Dir = No_File then
            Dir := GNATCOLL.VFS.Get_Current_Dir;
         end if;

         Tmp_H := Create_From_Dir (Dir, "gps_libclang_tmp.h");
         Tmp_H_W := Write_File (Tmp_H);
         Write (Tmp_H_W, "");
         Close (Tmp_H_W);

         Args := (1 => new String'("-v"),
                  2 => new String'(+Tmp_H.Full_Name));

         Non_Blocking_Spawn
           (Fd, The_Exec_On_Path.all, Args,
            Err_To_Out => True);

         declare
         begin
            while True loop
               GNAT.Expect.TTY.Expect (Fd, Match, All_Match, Timeout => 1000);

               declare
                  S : constant String :=
                    Strip_CR (Strip_Character (Expect_Out (Fd), ASCII.LF));
                  F : Natural;
               begin
                  if S = "#include <...> search starts here:" then
                     Listing := True;
                  elsif S = "End of search list." then
                     Listing := False;
                  else
                     if Listing then
                        F := S'First;
                        while S (F) = ' ' loop
                           F := F + 1;
                        end loop;

                        Result (First_Free) := To_Unbounded_String
                          ("-I" & S (F .. S'Last));

                        First_Free := First_Free + 1;
                     end if;
                  end if;
               end;

               --  ??? Add a loop condition termination
            end loop;
         exception
            when Process_Died =>
               --  Expected at some point
               null;
         end;

         Tmp_H.Delete (Ignored);
      end;

      GNAT.Strings.Free (The_Exec_On_Path);

      --  We are about to return results: cache this
      if Last_Lang /= null then
         GNAT.Strings.Free (Last_Lang);
      end if;

      Last_Project := Project;
      Last_Lang := new String'(Language);
      Last_Result := Result;
      Last_Result_Index := First_Free - 1;

      return Result (1 .. First_Free - 1);
   end Get_Compiler_Search_Paths;

   -----------------------------
   -- Get_Project_Source_Dirs --
   -----------------------------

   Source_Dirs_Root   : Project_Type;
   Source_Dirs_Lang   : GNAT.Strings.String_Access;
   Source_Dirs_Result : Unbounded_String_Array (1 .. 2048);
   Source_Dirs_Index  : Natural;
   --  ??? Another horrible cache, this should be reworked

   function Get_Project_Source_Dirs
     (Kernel   : Core_Kernel;
      Project  : Project_Type;
      Language : String) return Unbounded_String_Array
   is
      P    : Project_Type;
      It   : Project_Iterator;

      Result     : Unbounded_String_Array (1 .. 2048);
      First_Free : Natural := 1;

      --  ??? We do not support more than 2048 source directories
   begin
      if Source_Dirs_Lang /= null
        and then Source_Dirs_Lang.all = Language
        and then Source_Dirs_Root = Project
      then
         --  Return the cached result
         return Source_Dirs_Result (1 .. Source_Dirs_Index);
      end if;

      if Project = No_Project then
         --  We are opening a source file which is not in the project
         --  hierarchy. This can be the case when the user has navigated
         --  to the system includes, or if the user has opened a file from
         --  the disk directly.
         --  We do not know here which case this is, so we try to be helpful
         --  and add all -I's corresponding to the loaded hierarchy.
         It := Start (Kernel.Registry.Tree.Root_Project,
                      Recursive        => True,
                      Direct_Only      => False,
                      Include_Extended => True);
      else
         It := Start (Project,
                      Recursive        => True,
                      Direct_Only      => False,
                      Include_Extended => True);
      end if;

      P := Current (It);

      while P /= No_Project loop
         if Has_Language (P, Language) then
            declare
               Dirs : constant File_Array :=
                 Source_Dirs (P, Recursive => False);
            begin
               for D in Dirs'Range loop
                  Result (First_Free) := To_Unbounded_String
                    ("-I" & (+Dirs (D).Full_Name));
                  First_Free := First_Free + 1;
               end loop;
            end;
         end if;

         Next (It);
         P := Current (It);
      end loop;

      --  Cache the results

      Source_Dirs_Root := Project;
      if Source_Dirs_Lang /= null then
         Free (Source_Dirs_Lang);
      end if;

      Source_Dirs_Lang := new String'(Language);
      Source_Dirs_Result := Result;
      Source_Dirs_Index := First_Free - 1;

      return Result;
   end Get_Project_Source_Dirs;

end Language.Libclang.Utils;
