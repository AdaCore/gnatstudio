------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Interactive_Consoles;         use Interactive_Consoles;
with GPS.Kernel.Messages.Legacy;

package body Build_Command_Manager.Console_Writers is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access is
   begin
      if Self.Data.Console =  null then
         return Child;
      else
         return new Console_Writer'(Child => Child, Data => Self.Data);
      end if;
   end Create;

   overriding procedure End_Of_Stream
     (Self    : not null access Console_Writer;
      Status  : Integer;
      Command : Command_Access)
   is
      use GPS.Kernel.Messages.Legacy;
   begin
      --  Raise the messages window is compilation was unsuccessful
      --  and no error was parsed. See D914-005

      if Self.Data.Raise_On_Error and then Status /= 0 and then
        Category_Count (Self.Data.Kernel, To_String (Self.Data.Category)) = 0
      then
         Self.Data.Kernel.Raise_Console;
      end if;

      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);
   end End_Of_Stream;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Console_Writer;
      Item    : String;
      Command : Command_Access) is
   begin
      Self.Data.Console.Insert (Item, Add_LF => False);
      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

   ----------------------------
   -- Raise_Console_On_Error --
   ----------------------------

   procedure Raise_Console_On_Error
     (Self     : access Output_Parser_Fabric;
      Kernel   : Kernel_Handle;
      Category : Unbounded_String) is
   begin
      Self.Data.Kernel := Kernel;
      Self.Data.Category := Category;
      Self.Data.Raise_On_Error := True;
   end Raise_Console_On_Error;

   -----------------
   -- Set_Console --
   -----------------

   procedure Set_Console
     (Self    : access Output_Parser_Fabric;
      Console : Interactive_Consoles.Interactive_Console) is
   begin
      Self.Data.Console := Console;
      Self.Data.Raise_On_Error := True;
   end Set_Console;

end Build_Command_Manager.Console_Writers;
