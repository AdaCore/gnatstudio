-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Code_Analysis; use Code_Analysis;
with Code_Analysis_Dump; use Code_Analysis_Dump;
with Code_Coverage; use Code_Coverage;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Main is

   function Read_File (File : String) return String_Access;
   --  Read a file from the given long possible name and return its content via
   --  a string access

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : String) return String_Access is
      FD           : File_Descriptor := Invalid_FD;
      Buffer       : String_Access;
      Length       : Integer;
      Dummy_Length : Integer;
      pragma Unreferenced (Dummy_Length);

   begin
      FD := Open_Read (File, Fmode => Binary);

      if FD = Invalid_FD then
         Put_Line ("Couldn't open " & File);
         return null;
      end if;

      Length := Integer (File_Length (FD));
      Buffer := new String (1 .. Length);
      Dummy_Length := Read (FD, Buffer.all'Address, Length);
      Close (FD);
      return Buffer;
   end Read_File;

   Source_File_Name : constant File_Id := new String'("main.adb");
   File_Contents : String_Access;
   Project_Name  : Project_Id;
   Project_Node  : Project_Access;
   File_Node     : Code_Analysis.File_Access;
begin

   Project_Name  := Get_Project_From_File (Source_File_Name);
   Project_Node  := Get_Or_Create (Project_Name);
   File_Contents := Read_File (Source_File_Name.all & ".gcov");
   declare
      Dummy  : Code_Analysis.File_Access;
      pragma Unreferenced (Dummy);
   begin
      Dummy  := Get_Or_Create (Project_Node, Source_File_Name);
   end;
   File_Node := Get_Or_Create (Get_Or_Create (Project_Name), Source_File_Name);
   Add_Subprograms (File_Node, File_Contents);
   --   Add_Lines (File_Node, File_Contents);
   --  Will return to working state in the next version
   Dump_Text;
   Free (File_Contents);
   Free_Project (Project_Node);
end Main;
