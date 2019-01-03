------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

package Codefix.Text_Manager.Spark_Commands is

   -------------------------------
   -- Move_Tilde_Or_Percent_Cmd --
   -------------------------------

   type Move_Tilde_Or_Percent_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Move_Tilde_Or_Percent_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Set the mark that points to the offending symbol

   overriding
   procedure Execute
     (This         : Move_Tilde_Or_Percent_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the tilde or percent symbol correctly placed

   overriding
   procedure Free (This : in out Move_Tilde_Or_Percent_Cmd);
   --  Free the memory associated to a Move_Tilde_Or_Percent_Cmd

   overriding
   function Is_Writable (This : Move_Tilde_Or_Percent_Cmd) return Boolean;
   --  See inherited documentation

private

   type Move_Tilde_Or_Percent_Cmd is new Text_Command (Simple) with record
      Cursor : Ptr_Mark;
   end record;

end Codefix.Text_Manager.Spark_Commands;
