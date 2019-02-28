------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

with Ada.Text_IO;

package body GPS.CLI_Messages_Windows is

   -----------------
   -- Insert_Text --
   -----------------

   overriding procedure Insert_Text
     (Console : access CLI_Virtual_Console_Record; Text : String)
   is
      pragma Unreferenced (Console);
   begin
      Ada.Text_IO.Put (Text);
   end Insert_Text;

   overriding procedure Insert_Error
     (Console : access CLI_Virtual_Console_Record; Text : String)
      renames Insert_Text;

   overriding procedure Insert_Prompt
     (Console : access CLI_Virtual_Console_Record; Text : String)
      renames Insert_Text;

   ------------------
   -- Get_Instance --
   ------------------

   overriding function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access CLI_Virtual_Console_Record)
      return Class_Instance is
   begin
      return Get (Console.Instances, Script);
   end Get_Instance;

   ------------------------
   -- Set_Data_Primitive --
   ------------------------

   overriding procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access CLI_Virtual_Console_Record) is
   begin
      Set (Console.Instances, Instance);
   end Set_Data_Primitive;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Console    : access CLI_Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String
   is
      pragma Unreferenced (Console);
      Result : String (1 .. Size);
   begin
      for J in Result'Range loop
         Ada.Text_IO.Get_Immediate (Result (J));

         if Result (J) = Character'Val (10) and Whole_Line then
            return Result (1 .. J);
         end if;
      end loop;

      return Result;
   end Read;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : not null access Messages_Window;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info)
   is
      pragma Unreferenced (Mode);
   begin
      if Add_LF then
         Self.Console.Insert_Text (Text & Character'Val (10));
      else
         Self.Console.Insert_Text (Text);
      end if;
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   overriding procedure Insert_UTF8
     (Self   : not null access Messages_Window;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info)
   is
   begin
      Self.Insert (UTF8, Add_LF, Mode);
   end Insert_UTF8;

   -------------------------
   -- Get_Virtual_Console --
   -------------------------

   overriding function Get_Virtual_Console
     (Self : not null access Messages_Window)
      return GNATCOLL.Scripts.Virtual_Console is
   begin
      return Self.Console'Access;
   end Get_Virtual_Console;

end GPS.CLI_Messages_Windows;
