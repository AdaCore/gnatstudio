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

package body Codefix.Text_Navigators is

   function New_Text_Interface (This : Text_Navigator) return Ptr_Text is
      pragma Unreferenced (This);
   begin
      return new Unique_File;
   end New_Text_Interface;

   function Get_Body_Or_Spec (This : Text_Navigator; File_Name : String)
     return String is
      pragma Unreferenced (This);
   begin
      --  ??? Should ask the project for the body file instead
      case File_Name (File_Name'Last) is
         when 'b' =>
            return File_Name (File_Name'First .. File_Name'Last - 1) & 's';
         when 's' =>
            return File_Name (File_Name'First .. File_Name'Last - 1) & 'b';
         when others =>
            raise Codefix_Panic;
      end case;
   end Get_Body_Or_Spec;

end Codefix.Text_Navigators;
