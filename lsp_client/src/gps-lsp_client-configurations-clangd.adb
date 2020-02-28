------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;          use GNAT.Strings;

with GNATCOLL.JSON;         use GNATCOLL.JSON;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with Toolchains;            use Toolchains;

package body GPS.LSP_Client.Configurations.Clangd is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.CLANGD_SUPPORT.DIAGNOSTICS", Off);

   package String_String_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   ----------------------------
   -- On_Server_Capabilities --
   ----------------------------

   procedure On_Server_Capabilities
     (Capabilities : in out LSP.Messages.ServerCapabilities) is
   begin
      --  clangd 9.0.0 does renaming only in one (current) file that is
      --  why we cant use it. So we set this capability to False and this
      --  will force using old renaming engine.
      Capabilities.renameProvider := (Is_Set => False);
   end On_Server_Capabilities;

   ------------------------------------
   -- Prepare_Configuration_Settings --
   ------------------------------------

   overriding procedure Prepare_Configuration_Settings
     (Self : in out Clangd_Configuration)
   is
      Tree     : constant Project_Tree_Access := Self.Kernel.Get_Project_Tree;
      Iter     : Project_Iterator;
      P        : Project_Type;
      Dir      : constant Virtual_File := Tree.Root_Project.Project_Path.Dir;
      Dir_Name : constant String := Display_Dir_Name (Dir);

      Compilers  : String_String_Maps.Map;
      List       : File_Array_Access;
      Includes   : Unbounded_String;
      DB         : JSON_Array;

      procedure Process_Files (P : Project_Type);
      --  Process project's files and prepare a database for clangd

      -------------------
      -- Process_Files --
      -------------------

      procedure Process_Files (P : Project_Type)
      is
         Dirs       : constant GNATCOLL.VFS.File_Array := P.Source_Dirs;
         Files      : GNATCOLL.VFS.File_Array_Access   := P.Source_Files;
         Object     : JSON_Value;
         Switches   : GNAT.Strings.String_List_Access;
         Is_Default : Boolean;
         Command    : Unbounded_String;
      begin
         --  Prepare switches for including source directories
         for Index in Dirs'Range loop
            declare
               --  Use relative path because file name is used as a key in
               --  clangd cache, so this will guarantee that we have unique
               --  key-names
               Path : constant String :=
                 (+(Relative_Path (Dirs (Index), Dir)));
            begin
               --  Remove last path separator (e.g. '/') because example in
               --  clangd documentation looks like:
               --  "command": "/usr/bin/clang++ -Irelative -c file.cc",
               --  so without it
               if Path (Path'First .. Path'Last - 1) /= "" then
                  Append
                    (Includes,
                     "-I" & Path (Path'First .. Path'Last - 1) & " ");
               end if;
            end;
         end loop;

         --  Process files and prepare information for clangd
         --  compilation database
         for File of Files.all loop
            declare
               Language : constant String := Tree.Info (File).Language;
            begin
               if Language in "c" | "cpp" | "c++" then
                  --  Retrieve compiler name for certain language if
                  --  did'nt done yet. For C and CPP we can have
                  --  different ones.
                  if not Compilers.Contains (Language) then
                     Compilers.Insert
                       (Language,
                        Get_Exe
                          (Get_Compiler
                               (Self.Kernel.Get_Toolchains_Manager.
                                    Get_Toolchain (P),
                                Tree.Info (File).Language)));
                  end if;

                  Append (List, File);

                  Object := Create_Object;
                  declare
                     Path : constant String := (+(Relative_Path (File, Dir)));
                  begin
                     Object.Set_Field ("directory", Dir_Name);

                     P.Switches
                       ("Compiler", File, Language, Switches, Is_Default);

                     Command := Compilers.Element (Language) & " " & Includes;

                     for Index in 1 .. Switches'Length loop
                        if Switches (Index) /= null
                          and then Switches (Index).all /= ""
                        then
                           Append (Command, Switches (Index).all & " ");
                        end if;
                     end loop;
                     Free (Switches);

                     Object.Set_Field ("command", Command & Path);
                     Object.Set_Field ("file", Path);
                  end;

                  Append (DB, Object);
               end if;
            end;
         end loop;

         GNATCOLL.VFS.Unchecked_Free (Files);
      end Process_Files;

   begin

      Iter := Tree.Root_Project.Start;
      loop
         P := Current (Iter);
         exit when P = No_Project;
         Process_Files (P);
         Next (Iter);
      end loop;

      --  Store clangd compilation database in the project file directory
      declare
         File : constant Virtual_File := Create_From_Dir
           (Dir, "compile_commands.json");
         W_File : Writable_File;
      begin
         Self.Server_Arguments.Append
           ("--compile-commands-dir=" & Display_Dir_Name (Dir));

         W_File := Write_File (File);
         Write (W_File, Write (Create (DB)));
         Close (W_File);
      end;

      Self.Server_Arguments.Append ("--offset-encoding=utf-8");
      Self.Server_Arguments.Append ("--pretty");
      Self.Server_Arguments.Append ("--log=verbose");

      GNATCOLL.VFS.Unchecked_Free (List);

   exception
      when E : others =>
         Me.Trace (E);
   end Prepare_Configuration_Settings;

end GPS.LSP_Client.Configurations.Clangd;
