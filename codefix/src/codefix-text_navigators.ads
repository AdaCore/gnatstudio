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

--  This unit is the standard derivation of Text_Navigator_Abstr.

with Codefix.Text_Manager; use Codefix.Text_Manager;

generic

   type Unique_File is new Text_Interface with private;
   --  Type of Text_Interfaces that the Text_Navigator contains.

package Codefix.Text_Navigators is

   type Text_Navigator is new Text_Navigator_Abstr with null record;

   function New_Text_Interface (This : Text_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   function Get_Body_Or_Spec (This : Text_Navigator; File_Name : String)
     return String;
   --  Change the .ads to .adb, or .adb to .ads

end Codefix.Text_Navigators;
