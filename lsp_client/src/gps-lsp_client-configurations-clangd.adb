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

with Ada.Calendar;          use Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;
with GNAT.Strings;          use GNAT.Strings;

with GNATCOLL.JSON;         use GNATCOLL.JSON;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with VSS.JSON.Streams.Writers;
with VSS.Stream_Element_Buffers;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory;

with Config;                use Config;
with Toolchains;            use Toolchains;
with Remote;

package body GPS.LSP_Client.Configurations.Clangd is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.CLANGD_SUPPORT.DIAGNOSTICS", Off);

   package String_String_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   ----------------------------
   -- On_Server_Capabilities --
   ----------------------------

   procedure On_Server_Capabilities
     (Capabilities : in out LSP.Messages.ServerCapabilities) is null;

   ------------------------------------
   -- Prepare_Configuration_Settings --
   ------------------------------------

   overriding procedure Prepare_Configuration_Settings
     (Self : in out Clangd_Configuration)
   is
      Tree         : constant Project_Tree_Access :=
        Self.Kernel.Get_Project_Tree;
      Iter             : Project_Iterator;
      P                : Project_Type;
      Project_Dir      : constant Virtual_File :=
        Tree.Root_Project.Project_Path.Dir;
      Project_Dir_Name : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Project_Dir.Display_Dir_Name);
      Database_Dir     : constant Virtual_File :=
        Tree.Root_Project.Artifacts_Dir / (+".clangd");

      Compilers  : String_String_Maps.Map;
      Drivers    : Unbounded_String;
      Includes   : Unbounded_String;

      Writer     : VSS.JSON.Streams.Writers.JSON_Simple_Writer;
      Stream     : aliased VSS.Text_Streams.Memory.Memory_UTF8_Output_Stream;

      procedure Process_Files (P : Project_Type);
      --  Process project's files and prepare a database for clangd

      -------------------
      -- Process_Files --
      -------------------

      procedure Process_Files (P : Project_Type) is
         Target     : constant String := P.Get_Target;
         Dirs       : constant GNATCOLL.VFS.File_Array := P.Source_Dirs;
         Files      : GNATCOLL.VFS.File_Array_Access   := P.Source_Files;
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
                 (+(Relative_Path (Dirs (Index), Project_Dir)));
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
               use Remote;
               Language : constant String :=
                 File_Info'Class (Tree.Info_Set (File).First_Element).Language;

               Toolchain : Toolchains.Toolchain;
               Full_Name : Unbounded_String;
            begin
               if Language in "c" | "cpp" | "c++" then
                  --  Retrieve compiler name for certain language if
                  --  did'nt done yet. For C and CPP we can have
                  --  different ones.
                  if not Compilers.Contains (Language) then
                     Toolchain := Self.Kernel.Get_Toolchains_Manager.
                       Get_Toolchain (P);

                     if Is_Base_Name (Toolchain, Language) then
                        Full_Name := To_Unbounded_String
                          (+GNATCOLL.VFS.Locate_On_Path
                             (+Get_Exe (Get_Compiler (Toolchain, Language)),
                              Get_Nickname (Build_Server)).Full_Name);
                     else
                        Full_Name := To_Unbounded_String
                          (Get_Exe (Get_Compiler (Toolchain, Language)));
                     end if;

                     Compilers.Insert (Language, To_String (Full_Name));

                     if Length (Drivers) /= 0 then
                        Append (Drivers, ",");
                     end if;
                     Append (Drivers, To_String (Full_Name));
                  end if;

                  Writer.Start_Object;

                  declare
                     Path : constant String :=
                       (+(Relative_Path (File, Project_Dir)));

                  begin
                     Writer.Key_Name
                       (VSS.Strings.To_Virtual_String ("directory"));
                     Writer.String_Value (Project_Dir_Name);

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

                     --  If we are on a native Windows host, specify the target
                     --  to clang: otherwise it will try to use MSVC by
                     --  default.
                     if Target = "" and then Config.Host = Windows then
                        Append (Command, "--target=x86_64-pc-windows-gnu ");
                     end if;

                     Writer.Key_Name
                       (VSS.Strings.To_Virtual_String ("command"));
                     Writer.String_Value
                       (VSS.Strings.Conversions.To_Virtual_String
                          (Ada.Strings.Unbounded.To_String (Command & Path)));

                     Writer.Key_Name
                       (VSS.Strings.To_Virtual_String ("file"));
                     Writer.String_Value
                       (VSS.Strings.Conversions.To_Virtual_String (Path));
                  end;

                  Writer.End_Object;
               end if;
            end;
         end loop;

         GNATCOLL.VFS.Unchecked_Free (Files);
      end Process_Files;

   begin
      if not Database_Dir.Is_Directory then
         Make_Dir (Database_Dir);
      end if;

      Writer.Set_Stream (Stream'Unchecked_Access);

      Writer.Start_Document;
      Writer.Start_Array;

      Iter := Tree.Root_Project.Start;
      loop
         P := Current (Iter);
         exit when P = No_Project;
         Process_Files (P);
         Next (Iter);
      end loop;

      Writer.End_Array;
      Writer.End_Document;

      --  Store clangd compilation database in the project file directory
      declare
         File_Name : constant Virtual_File := Create_From_Dir
           (Database_Dir, "compile_commands.json");
         File      : Ada.Streams.Stream_IO.File_Type;

      begin
         Ada.Streams.Stream_IO.Create
           (File, Ada.Streams.Stream_IO.Out_File, +File_Name.Full_Name.all);

         for Position in Stream.Buffer.Each_Stream_Element loop
            Ada.Streams.Stream_IO.Write
              (File, (1 => VSS.Stream_Element_Buffers.Element (Position)));
         end loop;

         Ada.Streams.Stream_IO.Close (File);
      end;

      Self.Server_Arguments.Append
        ("--compile-commands-dir=" & Display_Dir_Name (Database_Dir));
      Self.Server_Arguments.Append ("--offset-encoding=utf-8");
      Self.Server_Arguments.Append ("--pretty");
      Self.Server_Arguments.Append ("-cross-file-rename");
      Self.Server_Arguments.Append ("--log=verbose");
      Self.Server_Arguments.Append ("--query-driver=" & To_String (Drivers));

   exception
      when E : others =>
         Me.Trace (E);
   end Prepare_Configuration_Settings;

   ------------------------------
   -- Set_Standard_Errors_File --
   ------------------------------

   procedure Set_Standard_Errors_File
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Client : in out GPS.LSP_Clients.LSP_Client)
   is
      Now  : constant Ada.Calendar.Time := Clock;
      File : constant Virtual_File := Create_From_Dir
        (Kernel.Get_Log_Dir,
         +("clangd." &
             Image (Now, ISO_Date) & Image (Now, "T%H%M%S") &
             ".txt"));
   begin
      Client.Set_Standard_Errors_File (File);
   end Set_Standard_Errors_File;

end GPS.LSP_Client.Configurations.Clangd;
