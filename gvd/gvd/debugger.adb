-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.Expect;  use GNAT.Expect;
with GNAT.OS_Lib;  use GNAT.OS_Lib;

package body Debugger is

   Remote_Protocol : constant String := "rsh";
   --  How to run a process on a remote machine ?


   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Debugger     : out Debugger_Root;
      The_Language : Language.Language_Access) is
   begin
      Debugger.The_Language := The_Language;
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Debugger : Debugger_Root) return Language.Language_Access is
   begin
      return Debugger.The_Language;
   end Get_Language;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Debugger : Debugger_Root) return GNAT.Expect.Pipes_Id_Access is
   begin
      return Debugger.Process;
   end Get_Process;

   -------------------
   -- General_Spawn --
   -------------------

   procedure General_Spawn (Debugger       : access Debugger_Root'Class;
                            Arguments      : GNAT.OS_Lib.Argument_List;
                            Debugger_Name  : String;
                            Remote_Machine : String := "")
   is
   begin
      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      if Remote_Machine = "" then
         Debugger.Process := new Pipes_Id'(Non_Blocking_Spawn
                                           (Debugger_Name, Arguments,
                                            Buffer_Size => 0,
                                            Err_To_Out => True));
      else
         declare
            Real_Arguments : Argument_List (1 .. Arguments'Length + 2);
         begin
            Real_Arguments (1) := new String'(Remote_Machine);
            Real_Arguments (2) := new String'(Debugger_Name);
            Real_Arguments (3 .. Real_Arguments'Last) := Arguments;

            Debugger.Process := new Pipes_Id'(Non_Blocking_Spawn
                                              (Remote_Protocol,
                                               Real_Arguments,
                                               Buffer_Size => 0,
                                               Err_To_Out => True));
            Free (Real_Arguments (1));
            Free (Real_Arguments (2));
         end;
      end if;
   end General_Spawn;

end Debugger;
