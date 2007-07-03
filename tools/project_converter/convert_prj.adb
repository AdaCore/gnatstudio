-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2000-2007, AdaCore              --
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

with GNAT.IO;                   use GNAT.IO;
with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.Command_Line;         use GNAT.Command_Line;
with Convert.Adp;               use Convert.Adp;
with Convert.Gpr;               use Convert.Gpr;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Traces;               use GNAT.Traces;

procedure Convert_Prj is

   procedure Help;
   --  Display help

   procedure Help is
   begin
      Put_Line ("Usage: " & Command_Name & " project_name.gpr");
      Put_Line ("  convert to .adp project file");
      Put_Line
        ("Usage: " & Command_Name & " [-b ext] [-s ext] project_name.adp");
      Put_Line ("  convert to .gpr project file");
      Put_Line ("     -b ext:  Specifies the extension of body files"
                & " (default is "".adb""");
      Put_Line ("     -s ext:  Specifies the extension of spec files"
                & " (default is "".ads""");
   end Help;

   Spec_Extension : String_Access := new String'(".ads");
   Body_Extension : String_Access := new String'(".adb");
   Project_Name : String_Access;

begin
   Parse_Config_File (".gnatdebug");

   loop
      case Getopt ("s: b:") is
         when 's' =>
            Free (Spec_Extension);
            Spec_Extension := new String'(Parameter);
         when 'b' =>
            Free (Body_Extension);
            Body_Extension := new String'(Parameter);
         when others =>
            exit;
      end case;
   end loop;

   loop
      declare
         S : constant String := Get_Argument (Do_Expansion => True);
      begin
         exit when S'Length = 0;

         if Project_Name /= null then
            Help;
            Set_Exit_Status (Failure);
            return;
         end if;

         Free (Project_Name);
         Project_Name := new String'(S);
      end;
   end loop;

   if Project_Name = null then
      Help;
      Set_Exit_Status (Failure);
      return;
   end if;

   if not Is_Regular_File (Project_Name.all) then
      Put_Line ("File not found: " & Project_Name.all);
   else
      if File_Extension (Project_Name.all) = ".gpr" then
         Convert_From_Gpr_To_Adp (Project_Name.all);

      elsif File_Extension (Project_Name.all) = ".adp" then
         Convert_From_Adp_To_Gpr
           (Project_Name.all, Spec_Extension, Body_Extension);

      else
         Put_Line ("Unsupported file extension "
                   & File_Extension (Project_Name.all));
         Set_Exit_Status (Failure);
      end if;
   end if;

exception
   when Invalid_Switch | Invalid_Parameter =>
      Help;
      Set_Exit_Status (Failure);
end Convert_Prj;
