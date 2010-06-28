-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009-2010, AdaCore              --
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

private

   package Mark_List is new Generic_List (Word_Mark);
   use Mark_List;

   type Move_Tilde_Or_Percent_Cmd is new Text_Command (Simple) with record
      Cursor : Ptr_Mark;
   end record;

end Codefix.Text_Manager.Spark_Commands;
