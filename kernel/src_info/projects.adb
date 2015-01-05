------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

with Ada.Strings.Hash_Case_Insensitive;
with Ada.Unchecked_Deallocation;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Opt;                        use Opt;
with Namet;                      use Namet;
with Scans;                      use Scans;
with Snames;                     use Snames;

pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect, GNAT.Expect.TTY;
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

package body Projects is

   Keywords_Initialized : Boolean := False;
   --  Whether we have already initialized the Ada keywords list. This is only
   --  used by Is_Valid_Project_Name

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
      Start  : Natural;
      Finish : Natural;

      function Is_Ada_Identifier (S : String) return Boolean;
      --  Returns True iff S has the syntax of an Ada identifier and is not an
      --  Ada95 reserved word.

      -----------------------
      -- Is_Ada_Identifier --
      -----------------------

      function Is_Ada_Identifier (S : String) return Boolean is
         Underscore : Boolean := False;
      begin
         --  An Ada identifier cannot be empty and must start with a letter

         if S'Length = 0 or else
            (S (S'First) not in 'a' .. 'z' and then
             S (S'First) not in 'A' .. 'Z')
         then
            return False;
         end if;

         for J in S'First + 1 .. S'Last loop
            if S (J) = '_' then
               --  An Ada identifier cannot have two consecutive underscores

               if Underscore then
                  return False;
               end if;

               Underscore := True;

            else
               Underscore := False;

               --  An Ada identifier is made only of letters, digits and
               --  underscores (already treated).

               if S (J) not in 'a' .. 'z' and then
                  S (J) not in 'A' .. 'Z' and then
                  S (J) not in '0' .. '9'
               then
                  return False;
               end if;
            end if;
         end loop;

         --  An Ada identifier cannot ends with an underscore

         if Underscore then
            return False;
         end if;

         Name_Len := S'Length;
         Name_Buffer (1 .. Name_Len) := S;

         --  A project name cannot be an Ada95 reserved word

         if Is_Keyword_Name (Name_Find) then
            return False;
         end if;

         --  All checks have succeeded

         return True;
      end Is_Ada_Identifier;

   begin
      --  A project name cannot be empty of ends with a dot

      if Name'Length = 0 or else Name (Name'Last) = '.' then
         return False;
      end if;

      if not Keywords_Initialized then
         Scans.Initialize_Ada_Keywords;
         Keywords_Initialized := True;
      end if;

      Start := Name'First;

      loop
         Finish := Start - 1;
         while Finish < Name'Last and then
               Name (Finish + 1) /= '.'
         loop
            Finish := Finish + 1;
         end loop;

         declare
            OK : constant Boolean :=
                    Is_Ada_Identifier (Name (Start .. Finish));
         begin
            --  A project name needs to be an Ada identifier and cannot be an
            --  Ada95 reserved word.

            if not OK then
               return False;
            end if;
         end;

         Start := Finish + 2;
         exit when Start > Name'Last;
      end loop;

      --  All checks have succeeded

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
        (Project_Registry'Class, Project_Registry_Access);
   begin
      Registry.Tree.Unload;
      Free (Registry.Tree);
      Free (Registry.Env);
      Unchecked_Free (Registry);

      GNATCOLL.Projects.Finalize;
   end Destroy;

   ------------
   -- Create --
   ------------

   function Create
     (Tree : not null access GNATCOLL.Projects.Project_Tree'Class;
      Env  : access GNATCOLL.Projects.Project_Environment'Class := null)
      return Project_Registry_Access
   is
      Reg : constant Project_Registry_Access := new Project_Registry;
   begin
      Reg.Tree := Project_Tree_Access (Tree);
      Reg.Env  := Project_Environment_Access (Env);
      Initialize (Reg.Env);
      return Reg;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Project_Type_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Type_Array, Project_Type_Array_Access);
   begin
      Unchecked_Free (Self);
   end Free;

   --------------------------------
   -- Source_Files_Non_Recursive --
   --------------------------------

   function Source_Files_Non_Recursive
     (Projects : Project_Type_Array) return GNATCOLL.VFS.File_Array_Access
   is
      Result : File_Array_Access;
      Tmp    : File_Array_Access;
   begin
      for P in Projects'Range loop
         Tmp := Projects (P).Source_Files (Recursive => False);
         if Tmp /= null then
            Append (Result, Tmp.all);
            Unchecked_Free (Tmp);
         end if;
      end loop;
      return Result;
   end Source_Files_Non_Recursive;

begin
   --  Use full path name so that the messages are sent to Locations view
   Opt.Full_Path_Name_For_Brief_Errors := True;
end Projects;
