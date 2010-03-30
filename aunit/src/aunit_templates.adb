-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2006-2010, AdaCore                 --
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

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

package body AUnit_Templates is

   ----------------------------
   -- Get_Template_File_Name --
   ----------------------------

   function Get_Template_File_Name
     (Kernel : access Kernel_Handle_Record'Class;
      Base   : Filesystem_String) return Virtual_File
   is
      Prefix : constant Virtual_File :=
                 Create_From_Dir (Get_System_Dir (Kernel), "share/gps/aunit/");
      File   : constant Virtual_File :=
                 Create_From_Dir (Prefix, Base & ".tmpl");
      --  System prefix
   begin
      if Is_Regular_File (File) then
         return File;
      end if;

      return No_File;
   end Get_Template_File_Name;

   ------------------
   -- Create_Files --
   ------------------

   procedure Create_Files
     (Kernel         : access Kernel_Handle_Record'Class;
      Base_Template  : Filesystem_String;
      Translations   : Translate_Set;
      Directory_Name : Virtual_File;
      Name           : String;
      Success        : out Boolean)
   is
      Spec_Template : constant Virtual_File :=
                        Get_Template_File_Name
                          (Kernel, Base_Template & ".ads");
      Body_Template : constant Virtual_File :=
                        Get_Template_File_Name
                          (Kernel, Base_Template & ".adb");
      Spec_Filename : constant Filesystem_String :=
                        Get_Project (Kernel).File_From_Unit
                          (Unit_Name       => Name,
                           Part            => Unit_Spec,
                           File_Must_Exist => False,
                           Language        => "ada");
      Body_Filename : constant Filesystem_String :=
                        Get_Project (Kernel).File_From_Unit
                          (Unit_Name       => Name,
                           Part            => Unit_Body,
                           File_Must_Exist => False,
                           Language        => "ada");
      Spec_File     : constant Virtual_File :=
                        Create_From_Dir (Directory_Name, Spec_Filename);
      Body_File     : constant Virtual_File :=
                        Create_From_Dir (Directory_Name, Body_Filename);
      WF            : Writable_File;
      Dead          : Message_Dialog_Buttons;
      pragma Unreferenced (Dead);

   begin
      Success := False;

      if Is_Directory (Directory_Name)
        and then Name /= ""
      then
         if Spec_Template /= No_File then
            if Is_Regular_File (Spec_File) then
               if Message_Dialog
                 ("File " & Spec_File.Display_Full_Name
                  & " exists. Overwrite?",
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

            WF := Spec_File.Write_File;
            GNATCOLL.VFS.Write
              (WF, Parse (+Spec_Template.Full_Name, Translations));
            Close (WF);

            Open_File_Editor (Kernel, Spec_File);
         end if;

         if Body_Template /= No_File then
            if Is_Regular_File (Body_File) then
               if Message_Dialog
                 ("File " & Body_File.Display_Full_Name
                  & " exists. Overwrite?",
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
            WF := Body_File.Write_File;
            GNATCOLL.VFS.Write
              (WF, Parse (+Body_Template.Full_Name, Translations));
            Close (WF);

            Open_File_Editor (Kernel, Body_File);
         end if;
      end if;

      if not Success then
         Dead := Message_Dialog
           ("No template with base name " & (+Base_Template) &
            " could be found: please verify your GPS installation",
            Warning,
            Button_Yes,
            Button_Yes,
            "",
            "Warning !");
      end if;
   end Create_Files;

end AUnit_Templates;
