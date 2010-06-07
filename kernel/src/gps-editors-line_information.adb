-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

package body GPS.Editors.Line_Information is

   use Commands;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Information_Record) is
   begin
      Free (X.Text);
      Free (X.Tooltip_Text);

      if X.Associated_Command /= null then
         Unref (X.Associated_Command);
      end if;
   end Free;

   ----------------------
   -- Add_Special_Line --
   ----------------------

   procedure Add_Special_Line
     (This       : GPS_Editor_Buffer'Class;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null)
   is
      Mark : constant Editor_Mark'Class :=
        This.Add_Special_Line
          (Start_Line, Text, Category, Name, Column_Id, Info);
      pragma Unreferenced (Mark);

   begin
      null;
   end Add_Special_Line;

end GPS.Editors.Line_Information;
