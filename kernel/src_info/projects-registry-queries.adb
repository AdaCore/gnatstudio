-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2009, AdaCore                 --
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
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect, GNAT.Expect.TTY;
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with GPS.Intl;                  use GPS.Intl;
with Prj.Ext;                   use Prj.Ext;
with Prj;                       use Prj;
with Remote;                    use Remote;
with String_Utils;              use String_Utils;
with Toolchains;                use Toolchains;
with Traces;                    use Traces;

package body Projects.Registry.Queries is

   Me : constant Debug_Handle := Create ("Projects.Registry.Queries");

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   Gnatls_Called : Boolean := False;
   --  Flag used to avoid generating an error message the first time
   --  GPS is launched, and only a cross-gnatls is available.
   --  The second time, assuming the project properly defined the Gnatlist
   --  attribute, everything will work properly. Otherwise, we will generate
   --  an error message at this point.
   --  ??? If no Gnatlist attribute is defined, Compute_Predefined_Paths
   --  won't be called a second time, but it's better to hide messages about
   --  gnatls being not found to users rather than confuse them for the case
   --  above (cross-gnatls only).

   procedure Compute_Predefined_Paths
     (Registry     : Project_Registry_Access;
      GNAT_Version : out GNAT.Strings.String_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      E_Handler    : Error_Handler := Null_E_Handler)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Process_Descriptor'Class, Process_Descriptor_Access);
      Current         : GNATCOLL.VFS.File_Array_Access :=
                          new File_Array'(1 .. 0 => <>);
      Object_Path_Set : Boolean := False;

      procedure Add_Directory (S : String);
      --  Add S to the search path.
      --  If Source_Path is True, the source path is modified.
      --  Otherwise, the object path is modified.

      -------------------
      -- Add_Directory --
      -------------------

      procedure Add_Directory (S : String) is
         Dir : Virtual_File;
      begin
         if S = "" then
            return;

         elsif S = "<Current_Directory>" then
            if not Object_Path_Set then
               --  Do not include "." in the default source/object paths: when
               --  the user is compiling, it would represent the object
               --  directory, when the user is searching file it would
               --  represent whatever the current directory is at that point,
               --  ...
               return;
            else
               Dir := Create_From_Base (".");
               Ensure_Directory (Dir);
               Append (Current, Dir);
            end if;

         else
            Dir := To_Local (Create (+S, Get_Nickname (Build_Server)));
            Append (Current, Dir);
         end if;
      end Add_Directory;

      Result  : Expect_Match;
      Success : Boolean;
      Fd      : Process_Descriptor_Access;

   begin
      Trace (Me, "Executing " & Argument_List_To_String (Gnatls_Args.all));
      --  ??? Place a error handler here
      begin
         Success := True;

         if Is_Local (Build_Server) then
            declare
               Gnatls_Path : constant Virtual_File :=
                               Locate_Compiler_Executable
                                 (+Gnatls_Args (Gnatls_Args'First).all);
            begin
               if Gnatls_Path = No_File then
                  Success := False;

                  Trace (Me, "Could not locate exec " &
                         Gnatls_Args (Gnatls_Args'First).all);

                  if Gnatls_Called then
                     Report (E_Handler,
                             -"Could not locate exec " &
                             Gnatls_Args (Gnatls_Args'First).all);
                  end if;
               else
                  Fd := new TTY_Process_Descriptor;
                  Non_Blocking_Spawn
                    (Fd.all,
                     +Gnatls_Path.Full_Name,
                     Gnatls_Args (2 .. Gnatls_Args'Last),
                     Buffer_Size => 0, Err_To_Out => True);
               end if;
            end;
         else
            Remote_Spawn
              (Fd, Get_Nickname (Build_Server), Gnatls_Args.all,
               Err_To_Out => True);
         end if;

      exception
         when others =>
            Report (E_Handler,
                    -"Could not execute " & Gnatls_Args (1).all);
            Success := False;
      end;

      if not Success then
         if Gnatls_Called then
            Report (E_Handler,
                    -"Could not compute predefined paths for this project.");
            Report (E_Handler,
              -("Subprojects might be incorrectly loaded, please make " &
                "sure they are in your ADA_PROJECT_PATH"));
         end if;

         Gnatls_Called := True;
         return;
      end if;

      Gnatls_Called := True;
      Expect (Fd.all, Result, "GNATLS .+(\n| )Copyright", Timeout => 10000);

      declare
         S : constant String := Strip_CR (Expect_Out_Match (Fd.all));
      begin
         GNAT_Version := new String'(S (S'First + 7 .. S'Last - 10));
      end;

      Expect (Fd.all, Result, "Source Search Path:", Timeout => 10000);

      loop
         Expect (Fd.all, Result, "\n", Timeout => 10000);

         declare
            S : constant String :=
                  Trim (Strip_CR (Expect_Out (Fd.all)), Ada.Strings.Left);
         begin
            if S = "Object Search Path:" & ASCII.LF then
               if Active (Me) then
                  Trace (Me, "Set source path from gnatls to :");
                  for J in Current'Range loop
                     Trace (Me, "  " & Current (J).Display_Full_Name);
                  end loop;
               end if;

               Set_Predefined_Source_Path (Registry.all, Current.all);
               Unchecked_Free (Current);
               Current := new File_Array'(1 .. 0 => <>);

            elsif S = "Project Search Path:" & ASCII.LF then
               if Active (Me) then
                  Trace (Me, "Set object path from gnatls to :");
                  for J in Current'Range loop
                     Trace (Me, "  " & Current (J).Display_Full_Name);
                  end loop;
               end if;

               Object_Path_Set := True;
               Set_Predefined_Object_Path (Registry.all, Current.all);
               Unchecked_Free (Current);
               Current := new File_Array'(1 .. 0 => <>);

            else
               Add_Directory (S (S'First .. S'Last - 1));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         if Object_Path_Set then
            if Active (Me) then
               Trace (Me, "Set project path from gnatls to :");
               for J in Current'Range loop
                  Trace (Me, "  " & Current (J).Display_Full_Name);
               end loop;
            end if;
            Prj.Ext.Set_Project_Path (+To_Path (Current.all));

         else
            if Active (Me) then
               Trace (Me, "Set object path (2) from gnatls to :");
               for J in Current'Range loop
                  Trace (Me, "  " & Current (J).Display_Full_Name);
               end loop;
            end if;
            Set_Predefined_Object_Path (Registry.all, Current.all);
         end if;

         Unchecked_Free (Current);
         Close (Fd.all);
         Unchecked_Free (Fd);
   end Compute_Predefined_Paths;

end Projects.Registry.Queries;
