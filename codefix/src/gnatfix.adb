-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Text_IO;            use Ada.Text_IO;

with Codefix; use Codefix;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Errors_Parser;  use Codefix.Errors_Parser;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
with Codefix.File_Io;        use Codefix.File_Io;
use Codefix.Formal_Errors.Extract_List;
with Test_Lib;               use Test_Lib;
use Test_Lib.Navigator;

procedure Gnatfix is

   procedure Print_Help;
   --  Print the use manual of codefix.

   procedure Free_Objects;
   --  Free memory associated to object used in Gnatfix

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Put_Line ("Gnatfix, version 0.1");
      Put_Line ("Use : ");
      Put_Line ("gnatfix <error file>");
   end Print_Help;

   Current_Text       : Text_Navigator;
   Errors_List        : Correction_Manager;
   Errors_Found       : Errors_File;

   ------------------
   -- Free_Objects --
   ------------------

   procedure Free_Objects is
   begin
      Free (Errors_List);
      Free (Current_Text);
      Free (Errors_Found);
      Free_Parsers;
   end Free_Objects;

begin

   if Argument_Count < 1 then
      Print_Help;
      Free_Objects;
      return;
   end if;

   if Argument_Count >= 2 and then Argument (2) = "-r" then
      if Argument_Count >= 3 then
         Create (Capture_File, Out_File, Argument (3));
      else
         Visible := False;
      end if;
   end if;

   Open (Errors_Found, Argument (1));

   Analyze
     (Errors_List,
      Current_Text,
      Errors_Found,
      Corrections_Proposed'Access);

   Commit (Errors_List, Current_Text);

   if Is_Open (Capture_File) then Close (Capture_File); end if;

   Free_Objects;
end Gnatfix;
