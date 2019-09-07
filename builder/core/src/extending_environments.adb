------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with GNATCOLL.Utils; use GNATCOLL.Utils;

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
     (Kernel  : access Core_Kernel_Record'Class;
      Source  : Virtual_File;
      Project : Project_Type) return Extending_Environment
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

   begin
      --  Create the temporary directory in the object dir of the project

      declare
         BN           : constant Filesystem_String := Source.Base_Name;
         Tmp_Dir_Name : constant Filesystem_String :=
           BN (BN'First .. Find_Char (String (BN), '.') - 1) & "_Tmp";
         Object_Dir   : constant Virtual_File := Project.Object_Dir;

      begin
         Env.Temporary_Dir := Create_From_Dir
           ((if Object_Dir /= No_File
            then Object_Dir
            else Project.Project_Path.Dir),
            Base_Name => Tmp_Dir_Name);
      end;

      if not Is_Directory (Env.Temporary_Dir) then
         Make_Dir (Env.Temporary_Dir);
      end if;

      Env.Project := Project;

      --  Create the extending project file. This project is a simple extension
      --  of the source's project file which source dir is the current
      --  temporary dir, in which the current version of the source file
      --  will be written.

      Write_Extending_Project
        (File   => Env.Project_File,
         P      => Project,
         E_All  => "",
         Body_S => "for Source_Dirs use (""."");",
         With_S => "");

      --  Create the temporary source file

      Env.File := Create_From_Dir (Env.Temporary_Dir, Base_Name (Source));

      --  Write the current content of the source editor into the file and
      --  close it

      declare
         Dest : Writable_File :=
           Create (Filesystem_String (Env.File.Display_Full_Name)).Write_File;
      begin
         Write
           (Dest,
            Get_Buffer_Factory (Kernel).Get
            (Source, Open_View => False).Get_Chars);
         Close (Dest);
      end;

      return Env;
   end Create_Extending_Environment;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : Extending_Environment) is
      Dummy : Boolean;
      BN : constant Filesystem_String := Env.File.Base_Name;
      BN_Stripped : constant Filesystem_String :=
        (if Env.File.Has_Suffix (".ads") or else Env.File.Has_Suffix (".adb")
         then BN (BN'First .. BN'Last - 4)
         else "");
   begin

      if BN_Stripped /= "" then
         declare
            ALI_File : constant Virtual_File :=
              Create_From_Dir (Env.Temporary_Dir, BN_Stripped & ".ali");
         begin
            if ALI_File /= No_File then
               ALI_File.Copy
                 (Env.Project.Object_Dir.Full_Name.all & ALI_File.Base_Name,
                  Dummy);
            end if;
         end;
      end if;

      if Env.Temporary_Dir /= No_File then
         Remove_Dir (Env.Temporary_Dir, Recursive => True, Success => Dummy);
      end if;
   end Destroy;

end Extending_Environments;
