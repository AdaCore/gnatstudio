-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

package body Language.Debugger is

   ------------------
   -- Set_Debugger --
   ------------------

   procedure Set_Debugger
     (The_Language : access Language_Debugger;
      The_Debugger : Debugger_Access) is
   begin
      The_Language.The_Debugger := The_Debugger;
   end Set_Debugger;

   ------------------
   -- Get_Debugger --
   ------------------

   function Get_Debugger
     (The_Language : access Language_Debugger) return Debugger_Access is
   begin
      return The_Language.The_Debugger;
   end Get_Debugger;

   ---------------------
   -- Break_Exception --
   ---------------------

   function Break_Exception
     (Debugger  : access Language_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String
   is
      pragma Unreferenced (Debugger, Name, Temporary, Unhandled);
   begin
      return "";
   end Break_Exception;

end Language.Debugger;
