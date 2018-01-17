------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

--  This package defines type for message console for command line interface.

with GPS.Messages_Windows;             use GPS.Messages_Windows;
with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;

package GPS.CLI_Messages_Windows is

   type Messages_Window is new Abstract_Messages_Window with private;

   overriding procedure Insert
     (Self   : not null access Messages_Window;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);

   overriding procedure Insert_UTF8
     (Self   : not null access Messages_Window;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);

   overriding procedure Clear (Self : not null access Messages_Window) is null;

   overriding procedure Raise_Console
     (Self       : not null access Messages_Window;
      Give_Focus : Boolean) is null;

   overriding function Get_Virtual_Console
     (Self : not null access Messages_Window)
      return GNATCOLL.Scripts.Virtual_Console;

private

   type CLI_Virtual_Console_Record is new Virtual_Console_Record with record
      Instances : Instance_List;
   end record;

   overriding procedure Insert_Text
     (Console : access CLI_Virtual_Console_Record; Text : String);

   overriding procedure Insert_Error
     (Console : access CLI_Virtual_Console_Record; Text : String);

   overriding procedure Insert_Prompt
     (Console : access CLI_Virtual_Console_Record; Text : String);

   overriding procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access CLI_Virtual_Console_Record);

   overriding function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access CLI_Virtual_Console_Record)
      return Class_Instance;

   overriding function Read
     (Console    : access CLI_Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String;

   type Messages_Window is new Abstract_Messages_Window with record
      Console : aliased CLI_Virtual_Console_Record;
   end record;

end GPS.CLI_Messages_Windows;
