------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

with Ada.Directories; use Ada.Directories;

package body BT.Xml is

   function Xml_Directory
     (Output_Dir     : String) return String
   is
   begin
      return Output_Dir & "/bts/";
   end Xml_Directory;

   function Xml_File_Name
     (Output_Dir     : String;
      File_Name      : String;
      For_Backtraces : Boolean) return String
   is
      Xml_File_Name : constant String :=
        Xml_Directory (Output_Dir) & Simple_Name (File_Name);

   begin
      if For_Backtraces then
         return Xml_File_Name & "_bts.xml";
      else
         return Xml_File_Name & "_vals.xml";
      end if;
   end Xml_File_Name;

end BT.Xml;
