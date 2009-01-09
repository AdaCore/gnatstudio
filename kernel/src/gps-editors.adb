-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008-2009, AdaCore               --
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

package body GPS.Editors is

   --  Dummy bodies for implementation of Nil values.

   procedure Add_Special_Line
     (This       : Editor_Buffer'Class;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "")
   is
      Mark : constant Editor_Mark'Class :=
               This.Add_Special_Line (Start_Line, Text, Category, Name);
      pragma Unreferenced (Mark);

   begin
      null;
   end Add_Special_Line;

   overriding function Beginning_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Beginning_Of_Line;

   overriding function End_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end End_Of_Line;

   overriding function Block_Start
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Block_Start;

   overriding function Block_End
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Block_End;

   overriding function Block_Type
     (This : Dummy_Editor_Location) return Language_Category
   is
      pragma Unreferenced (This);
   begin
      return Cat_Unknown;
   end Block_Type;

   overriding function Line (This : Dummy_Editor_Location) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Line;

   overriding function Column (This : Dummy_Editor_Location) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Column;

   overriding function Create_Mark
     (This : Dummy_Editor_Location; Name : String := "")
      return Editor_Mark'Class
   is
      pragma Unreferenced (This, Name);
   begin
      return Nil_Editor_Mark;
   end Create_Mark;

   overriding function Forward_Char
     (This : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      pragma Unreferenced (This, Count);
   begin
      return Nil_Editor_Location;
   end Forward_Char;

   overriding function Location
     (This : Dummy_Editor_Mark) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Location;

   overriding function New_Location
     (This   : Dummy_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class
   is
      pragma Unreferenced (This, Line, Column);
   begin
      return Nil_Editor_Location;
   end New_Location;

   overriding function New_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_View;
   end New_View;

   overriding function Add_Special_Line
     (This       : Dummy_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "") return Editor_Mark'Class
   is
      pragma Unreferenced (This, Start_Line, Text, Category, Name);
   begin
      return Nil_Editor_Mark;
   end Add_Special_Line;

   overriding function Current_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_View;
   end Current_View;

   overriding function Lines_Count
     (This : Dummy_Editor_Buffer) return Integer
   is
      pragma Unreferenced (This);
   begin
      return 0;
   end Lines_Count;

   overriding function Get_Chars
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) return String
   is
      pragma Unreferenced (This, From, To);
   begin
      return "";
   end Get_Chars;

   overriding function Beginning_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Beginning_Of_Buffer;

   overriding function End_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end End_Of_Buffer;

   overriding function Get_Mark
     (This : Dummy_Editor_Buffer;
      Name : String) return Editor_Mark'Class
   is
      pragma Unreferenced (This, Name);
   begin
      return Nil_Editor_Mark;
   end Get_Mark;

   overriding function Is_Present (This : Dummy_Editor_Mark) return Boolean is
      pragma Unreferenced (This);

   begin
      return False;
   end Is_Present;

   overriding function Cursor
     (This : Dummy_Editor_View) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Cursor;

end GPS.Editors;
