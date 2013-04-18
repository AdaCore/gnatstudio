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
with String_Utils;                 use String_Utils;
with Time_Utils;                   use Time_Utils;
with GPS.Intl;                     use GPS.Intl;
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

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Console_Writer;
      Status  : Integer;
      Command : Command_Access)
   is
      use GPS.Kernel.Messages.Legacy;
   begin
      if Self.Data.Show_Status then
         declare
            End_Time   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Time_Stamp : constant String := Timestamp (End_Time);
         begin
            if Status = 0 then
               Self.Data.Console.Insert
                 (Time_Stamp &
                  (-"process terminated successfully (elapsed time: ")
                  & Elapsed (Self.Data.Start_Time, End_Time) & "s)");
            else
               Self.Data.Console.Insert
                 (Time_Stamp
                  & (-"process exited with status ")
                  & Image (Status) & " (elapsed time: "
                  & Elapsed (Self.Data.Start_Time, End_Time) & "s)");
            end if;
         end;
      end if;

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
      Self.Data :=
        (Kernel         => Kernel,
         Console        => Self.Data.Console,
         Show_Status    => Self.Data.Show_Status,
         Start_Time     => Self.Data.Start_Time,
         Category       => Category,
         Raise_On_Error => True);
   end Raise_Console_On_Error;

   -----------------
   -- Set_Console --
   -----------------

   procedure Set_Console
     (Self    : access Output_Parser_Fabric;
      Console : Interactive_Consoles.Interactive_Console) is
   begin
      Self.Data.Console := Console;
      Self.Data.Raise_On_Error := False;
      Self.Data.Show_Status := False;
      Self.Data.Start_Time := Ada.Calendar.Clock;
   end Set_Console;

   -------------------------
   -- Show_Status_On_Exit --
   -------------------------

   procedure Show_Status_On_Exit (Self : access Output_Parser_Fabric) is
   begin
      Self.Data.Show_Status := True;
   end Show_Status_On_Exit;

end Build_Command_Manager.Console_Writers;
