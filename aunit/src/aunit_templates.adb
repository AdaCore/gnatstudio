-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2006                            --
--                             AdaCore                               --
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

with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Gtkada.Dialogs;          use Gtkada.Dialogs;

with Projects;                  use Projects;

with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

package body AUnit_Templates is

   ----------------------------
   -- Get_Template_File_Name --
   ----------------------------

   function Get_Template_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      Base   : String) return String
   is
      Prefix   : constant String := "share/gps/aunit/";
      --  System prefix
   begin
      declare
         Filename : constant String := Base & ".tmpl";
      begin
         if Is_Regular_File (Get_System_Dir (Kernel) & Prefix & Filename) then
            return Get_System_Dir (Kernel) & Prefix & Filename;
         end if;
      end;

      return "";
   end Get_Template_File_Name;

   ------------------
   -- Create_Files --
   ------------------

   procedure Create_Files
     (Kernel         : access Kernel_Handle_Record'Class;
      Base_Template  : String;
      Translations   : Translate_Set;
      Directory_Name : String;
      Name           : String;
      Success        : out Boolean)
   is
      File          : File_Type;
      Spec_Template : constant String :=
                        Get_Template_File_Name
                          (Kernel, Base_Template & ".ads");
      Body_Template : constant String :=
                        Get_Template_File_Name
                          (Kernel, Base_Template & ".adb");
      Spec_Filename : constant String :=
                        Projects.Get_Filename_From_Unit
                          (Get_Project (Kernel),
                           Unit_Name       => Name,
                           Part            => Unit_Spec,
                           File_Must_Exist => False,
                           Language        => "Ada");
      Body_Filename : constant String :=
                        Projects.Get_Filename_From_Unit
                          (Get_Project (Kernel),
                           Unit_Name       => Name,
                           Part            => Unit_Body,
                           File_Must_Exist => False,
                           Language        => "Ada");
      Dead          : Message_Dialog_Buttons;
      pragma Unreferenced (Dead);

   begin
      Success := False;

      if Directory_Name /= ""
        and then Is_Directory (Directory_Name)
        and then Name /= ""
      then
         if Spec_Template /= "" then
            if Is_Regular_File (Directory_Name & Spec_Filename) then
               if Message_Dialog
                 ("File " & Spec_Filename & " exists. Overwrite?",
                  Warning,
                  Button_Yes or Button_No,
                  Button_No,
                  "",
                  "Warning !") = Button_No
               then
                  Success := False;
                  return;
               end if;
            end if;

            --  At least one file created. Set success
            Success := True;
            Ada.Text_IO.Create (File, Out_File,
                                Directory_Name & Spec_Filename);
            Put (File, Parse (Spec_Template, Translations));
            Close (File);
            Open_File_Editor
              (Kernel, Create (Directory_Name & Spec_Filename, Kernel));
         end if;

         if Body_Template /= "" then
            if Is_Regular_File (Directory_Name & Body_Filename) then
               if Message_Dialog
                 ("File " & Body_Filename & " exists. Overwrite?",
                  Warning,
                  Button_Yes or Button_No,
                  Button_No,
                  "",
                  "Warning !") = Button_No
               then
                  Success := False;
                  return;
               end if;
            end if;

            --  At least one file created. Set success
            Success := True;
            Ada.Text_IO.Create (File, Out_File,
                                Directory_Name & Body_Filename);
            Put (File, Parse (Body_Template, Translations));
            Close (File);
            Open_File_Editor
              (Kernel, Create (Directory_Name & Body_Filename, Kernel));
         end if;
      end if;

      if not Success then
         Dead := Message_Dialog
           ("No template with base name " & Base_Template &
            " could be found: please verify your GPS installation",
            Warning,
            Button_Yes,
            Button_Yes,
            "",
            "Warning !");
      end if;
   end Create_Files;

end AUnit_Templates;
