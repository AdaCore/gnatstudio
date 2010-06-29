-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNATCOLL.Projects;  use GNATCOLL.Projects;
with Projects;           use Projects;
with GPS.Kernel.Project; use GPS.Kernel.Project;

package body Extending_Environments is

   --------------
   -- Get_File --
   --------------

   function Get_File (Env : Extending_Environment) return Virtual_File is
   begin
      return Env.File;
   end Get_File;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (Env : Extending_Environment) return Virtual_File is
   begin
      return Env.Project_File;
   end Get_Project;

   ----------------------------------
   -- Create_Extending_Environment --
   ----------------------------------

   function Create_Extending_Environment
     (Kernel : Kernel_Handle;
      Source : Virtual_File;
      Server : Server_Type) return Extending_Environment
   is
      Env : Extending_Environment;

      procedure Write_Extending_Project
        (File   : in out Virtual_File;
         P      : Project_Type;
         E_All  : String;
         Body_S : String;
         With_S : String);
      --  Write in File a project extending P.
      --  E_All can contain "all" if the project extends all.
      --  With_S is the string of 'with' statements.

      -----------------------------
      -- Write_Extending_Project --
      -----------------------------

      procedure Write_Extending_Project
        (File   : in out Virtual_File;
         P      : Project_Type;
         E_All  : String;
         Body_S : String;
         With_S : String)
      is
         W : Writable_File;
      begin
         File := Create_From_Dir
           (Env.Temporary_Dir, "extends_" & Base_Name (Project_Path (P)));

         W := Write_File (File);
         Write (W, With_S & ASCII.LF & "project Extends_" & P.Name
                & " extends " & E_All & " """
                & (+Project_Path (P).Full_Name.all) & """ is"
                & ASCII.LF
                & Body_S & ASCII.LF);

         --  If this is a library project, add a "Library_Dir" attribute
         if P.Attribute_Value (Library_Name_Attribute) /= "" then
            declare
               Lib_Directory : Virtual_File;
            begin
               Lib_Directory :=
                 Create_From_Dir (Env.Temporary_Dir, "lib");
               if not Is_Directory (Lib_Directory) then
                  Make_Dir (Lib_Directory);
               end if;
               Write (W, "   for Library_Dir use ""lib"";" & ASCII.LF);
            end;
         end if;

         Write (W, "end Extends_" & P.Name & ";" & ASCII.LF);
         Close (W);
      end Write_Extending_Project;

      P, Root : Project_Type;

      Project_File : Virtual_File;

   begin
      --  Create the temporary directory
      Env.Temporary_Dir := Create_From_Dir
        (Get_Tmp_Directory (Get_Nickname (Server)),
         Base_Name => Source.Base_Name);

      if not Is_Directory (Env.Temporary_Dir) then
         Make_Dir (Env.Temporary_Dir);
      end if;

      --  Create the project file

      P := Get_Registry (Kernel).Tree.Info (Source).Project;
      Root := Get_Registry (Kernel).Tree.Root_Project;

      Write_Extending_Project (File   => Project_File,
                               P      => P,
                               E_All  => "",
                               Body_S => "",
                               With_S => "");

      Write_Extending_Project (File   => Env.Project_File,
                               P      => Root,
                               E_All  => "all",
                               Body_S => "for Source_Dirs use ();",
                               With_S => "with """ &
                               (+Project_File.Full_Name.all) & """;");

      --  Create the file
      Env.File := Create_From_Dir (Env.Temporary_Dir, Base_Name (Source));

      Get_Buffer_Factory (Kernel).Get
        (Source).Save (Interactive => False,
                       File        => Env.File,
                       Internal    => True);

      return Env;
   end Create_Extending_Environment;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : Extending_Environment) is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if Env.Temporary_Dir /= No_File then
         Remove_Dir (Env.Temporary_Dir, Recursive => True, Success => Dummy);
      end if;
   end Destroy;

end Extending_Environments;
