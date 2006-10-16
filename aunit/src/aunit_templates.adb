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

with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

with File_Utils;              use File_Utils;

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
      Filename      : constant String :=
                        Name_As_Directory
                          (Directory_Name) & To_File_Name (Name);
      Dead          : Message_Dialog_Buttons;
      pragma Unreferenced (Dead);

   begin
      Success := False;

      if Directory_Name /= ""
        and then Is_Directory (Directory_Name)
        and then Name /= ""
      then
         if Spec_Template /= "" then
            if Is_Regular_File (Filename & ".ads") then
               if Message_Dialog
                 ("File " & Filename & ".ads exists. Overwrite?",
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
            Ada.Text_IO.Create (File, Out_File, Filename & ".ads");
            Put (File, Parse (Spec_Template, Translations));
            Close (File);
            Open_File_Editor
              (Kernel, Create (Filename & ".ads", Kernel));
         end if;

         if Body_Template /= "" then
            if Is_Regular_File (Filename & ".adb") then
               if Message_Dialog
                 ("File " & Filename & ".adb exists. Overwrite?",
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
            Ada.Text_IO.Create (File, Out_File, Filename & ".adb");
            Put (File, Parse (Body_Template, Translations));
            Close (File);
            Open_File_Editor
              (Kernel, Create (Filename & ".ads", Kernel));
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
