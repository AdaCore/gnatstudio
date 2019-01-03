------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GNAT.Strings;       use GNAT.Strings;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

package body Language.Libclang.Utils is

   ----------------------------------------
   -- Get_Compiler_Search_Paths_Switches --
   ----------------------------------------

   function Get_Compiler_Search_Paths_Switches
     (Kernel   : Core_Kernel;
      Project  : Project_Type;
      Language : String) return Unbounded_String_Array
   is
      Python           : constant Scripting_Language :=
        Lookup_Scripting_Language (Kernel.Scripts, "python");
      C                : Callback_Data'Class := Python.Create (2);
   begin

      --  This function is implemented in python, because the spawning logic
      --  *and* the string massaging were both easier to express.

      C.Set_Nth_Arg (1, Project.Name);
      C.Set_Nth_Arg (2, Language);
      C.Execute_Command ("GPS.__get_compiler_search_paths");

      declare
         L : constant List_Instance'Class := C.Return_Value;
      begin
         return A : Unbounded_String_Array (1 .. L.Number_Of_Arguments) do
            for J in A'Range loop
               A (J) := To_Unbounded_String ("-I" & String'(L.Nth_Arg (J)));
            end loop;
         end return;
      end;

   end Get_Compiler_Search_Paths_Switches;

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

      return Result (Result'First .. First_Free - 1);
   end Get_Project_Source_Dirs;

end Language.Libclang.Utils;
