-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2010, AdaCore                 --
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

with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Opt;                        use Opt;
with Namet;                      use Namet;
with Scans;                      use Scans;
with Snames;                     use Snames;
with GPS.Intl;                   use GPS.Intl;
with Remote;                     use Remote;
with String_Utils;               use String_Utils;
with Toolchains;                 use Toolchains;

pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect, GNAT.Expect.TTY;
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

package body Projects is

   Me    : constant Trace_Handle := Create ("Projects");

   Keywords_Initialized : Boolean := False;
   --  Whether we have already initialized the Ada keywords list. This is only
   --  used by Is_Valid_Project_Name

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

   -----------------------
   -- Project_Name_Hash --
   -----------------------

   function Project_Name_Hash
     (Project : Project_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash_Case_Insensitive (Project.Name);
   end Project_Name_Hash;

   -----------------------
   -- Project_Directory --
   -----------------------

   function Project_Directory
     (Project : Project_Type;
      Host    : String := Local_Host) return GNATCOLL.VFS.Virtual_File is
   begin
      return Dir (Project_Path (Project, Host));
   end Project_Directory;

   --------------------
   -- Set_Paths_Type --
   --------------------

   procedure Set_Paths_Type
     (Project : Project_Type; Paths : Paths_Type_Information) is
   begin
      GPS_Project_Data_Access (Project.Data).Paths_Type := Paths;
   end Set_Paths_Type;

   --------------------
   -- Get_Paths_Type --
   --------------------

   function Get_Paths_Type
     (Project : Project_Type) return Paths_Type_Information is
   begin
      return GPS_Project_Data_Access (Project.Data).Paths_Type;
   end Get_Paths_Type;

   --------------------------
   -- Source_Dirs_With_VCS --
   --------------------------

   function Source_Dirs_With_VCS
     (Project   : Project_Type;
      Recursive : Boolean) return GNATCOLL.VFS.File_Array
   is
   begin
      --  ??? We could optimize and only take into account projects with a
      --  VCS attribute. This used to be the case before we moved the projects
      --  API into GNATCOLL

      return Project.Source_Dirs (Recursive => Recursive);
   end Source_Dirs_With_VCS;

   ---------------------------
   -- Is_Valid_Project_Name --
   ---------------------------

   function Is_Valid_Project_Name (Name : String) return Boolean is
   begin
      if Name'Length = 0
        or else (Name (Name'First) not in 'a' .. 'z'
                 and then Name (Name'First) not in 'A' .. 'Z')
      then
         return False;
      end if;

      for N in Name'First + 1 .. Name'Last loop
         if Name (N) not in 'a' .. 'z'
           and then Name (N) not in 'A' .. 'Z'
           and then Name (N) not in '0' .. '9'
           and then Name (N) /= '_'
           and then Name (N) /= '.'
         then
            return False;
         end if;
      end loop;

      if not Keywords_Initialized then
         Scans.Initialize_Ada_Keywords;
         Keywords_Initialized := True;
      end if;

      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;

      if Is_Keyword_Name (Name_Find) then
         return False;
      end if;

      return True;
   end Is_Valid_Project_Name;

   -----------------
   -- Environment --
   -----------------

   function Environment
     (Self : Project_Registry) return Project_Environment_Access
   is
   begin
      return Self.Env;
   end Environment;

   ----------
   -- Tree --
   ----------

   function Tree (Self : Project_Registry) return Project_Tree_Access is
   begin
      return Self.Tree;
   end Tree;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Registry : in out Project_Registry_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Tree'Class, Project_Tree_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Environment'Class, Project_Environment_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Registry'Class, Project_Registry_Access);
   begin
      Registry.Tree.Unload;
      Unchecked_Free (Registry.Tree);
      Unchecked_Free (Registry.Env);
      Unchecked_Free (Registry);

      GNATCOLL.Projects.Finalize;
   end Destroy;

   -----------------------
   -- Set_Object_Subdir --
   -----------------------

   overriding procedure Set_Object_Subdir
     (Self   : in out GPS_Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String)
   is
   begin
      --  ??? Do we need to clean up the subdirs, or is this always followed
      --  by call to Recompute_View in any case ?
      --  Do_Subdirs_Cleanup ();

      Set_Object_Subdir (Project_Environment (Self), Subdir);
   end Set_Object_Subdir;

   ------------
   -- Create --
   ------------

   function Create
     (Tree : not null access Project_Tree'Class)
      return Project_Registry_Access
   is
      Reg : constant Project_Registry_Access := new Project_Registry;
   begin
      Reg.Tree := Project_Tree_Access (Tree);
      Reg.Env  := new GPS_Project_Environment;
      return Reg;
   end Create;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (Registry     : Project_Registry_Access;
      GNAT_Version : out GNAT.Strings.String_Access;
      Gnatls_Args  : GNAT.Strings.String_List_Access;
      Errors       : GNATCOLL.Projects.Error_Report := null)
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
               if Gnatls_Path = GNATCOLL.VFS.No_File then
                  Success := False;

                  Trace (Me, "Could not locate exec " &
                         Gnatls_Args (Gnatls_Args'First).all);

                  if Gnatls_Called and then Errors /= null then
                     Errors (-"Could not locate exec " &
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
            if Errors /= null then
               Errors (-"Could not execute " & Gnatls_Args (1).all);
            end if;
            Success := False;
      end;

      if not Success then
         if Gnatls_Called and then Errors /= null then
            Errors
              (-"Could not compute predefined paths for this project.");
            Errors
              (-("Subprojects might be incorrectly loaded, please make " &
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
                  Trace (Me, "Set source path from gnatls to:");
                  for J in Current'Range loop
                     Trace (Me, "  " & Current (J).Display_Full_Name);
                  end loop;
               end if;

               Registry.Environment.Set_Predefined_Source_Path (Current.all);
               Unchecked_Free (Current);
               Current := new File_Array'(1 .. 0 => <>);

            elsif S = "Project Search Path:" & ASCII.LF then
               if Active (Me) then
                  Trace (Me, "Set object path from gnatls to:");
                  for J in Current'Range loop
                     Trace (Me, "  " & Current (J).Display_Full_Name);
                  end loop;
               end if;

               Object_Path_Set := True;
               Registry.Environment.Set_Predefined_Object_Path (Current.all);
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
               Trace (Me, "Set project path from gnatls to:");
               for J in Current'Range loop
                  Trace (Me, "  " & Current (J).Display_Full_Name);
               end loop;
            end if;
            Registry.Environment.Set_Predefined_Project_Path (Current.all);

         else
            if Active (Me) then
               Trace (Me, "Set object path (2) from gnatls to:");
               for J in Current'Range loop
                  Trace (Me, "  " & Current (J).Display_Full_Name);
               end loop;
            end if;
            Registry.Environment.Set_Predefined_Object_Path (Current.all);
         end if;

         Unchecked_Free (Current);
         Close (Fd.all);
         Unchecked_Free (Fd);
   end Compute_Predefined_Paths;

begin
   --  Use full path name so that the messages are sent to Locations view
   Opt.Full_Path_Name_For_Brief_Errors := True;
end Projects;
