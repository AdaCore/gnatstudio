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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;         use GNATCOLL.JSON;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with Toolchains;            use Toolchains;

package body GPS.LSP_Client.Configurations.Clangd is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.CLANGD_SUPPORT.DIAGNOSTICS", Off);

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
      Tree : constant Project_Tree_Access := Self.Kernel.Get_Project_Tree;
      Iter : Project_Iterator;
      P    : Project_Type;

      List : File_Array_Access;
      DB   : JSON_Array;

      procedure Process_Files (P : Project_Type);
      --  Process project's files and prepare a database for clangd

      -------------------
      -- Process_Files --
      -------------------

      procedure Process_Files (P : Project_Type) is
         Files    : GNATCOLL.VFS.File_Array_Access := P.Source_Files;
         Object   : JSON_Value;
         Compiler : Unbounded_String;
      begin
         for File of Files.all loop
            if Tree.Info (File).Language in "c" | "cpp" | "c++" then
               if Compiler = Null_Unbounded_String then
                  Compiler := To_Unbounded_String
                    (Get_Exe
                       (Get_Compiler
                            (Self.Kernel.Get_Toolchains_Manager.
                                 Get_Toolchain (P),
                             Tree.Info (File).Language)));
               end if;

               Append (List, File);

               Object := Create_Object;
               declare
                  Dir  : constant String := Display_Dir_Name (File);
                  Name : constant String := (+Base_Name (File));
               begin
                  Object.Set_Field
                    ("directory", Dir (Dir'First .. Dir'Last - 1));

                  Object.Set_Field
                    ("command", Compiler & " -c -gnats " & Name);

                  Object.Set_Field ("file", Name);
               end;

               Append (DB, Object);
            end if;
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

      declare
         Dir  : constant Virtual_File := Greatest_Common_Path (List.all);
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
