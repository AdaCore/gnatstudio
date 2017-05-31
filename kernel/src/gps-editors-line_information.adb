------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

package body GPS.Editors.Line_Information is

   use Commands;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Information_Record) is
   begin
      if X.Associated_Command /= null then
         Unref (X.Associated_Command);
      end if;
      X.Text         := Ada.Strings.Unbounded.Null_Unbounded_String;
      X.Tooltip_Text := Ada.Strings.Unbounded.Null_Unbounded_String;
      X.Image        := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Free;

   ----------------------
   -- Add_Special_Line --
   ----------------------

   procedure Add_Special_Line
     (This       : GPS_Editor_Buffer'Class;
      Start_Line : Integer;
      Text       : String;
      Style      : Style_Access := null;
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null)
   is
      Mark     : constant Editor_Mark'Class :=
        This.Add_Special_Line
          (Start_Line, Text, Style, Name, Column_Id, Info);
      pragma Unreferenced (Mark);
   begin
      null;
   end Add_Special_Line;

end GPS.Editors.Line_Information;
