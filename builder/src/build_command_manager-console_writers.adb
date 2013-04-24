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
with Build_Configurations;         use Build_Configurations;
with String_Utils;                 use String_Utils;
with Time_Utils;                   use Time_Utils;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Messages.Legacy;

package body Build_Command_Manager.Console_Writers is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      Build          : Build_Information := Self.Builder.Get_Last_Build;
      Show_Status    : constant Boolean :=
        not (Build.Shadow or else Build.Background or else Build.Quiet);
      Raise_On_Error : Boolean := False;
      Cmd_Console    : Interactive_Console;
      Console        : Interactive_Console;
   begin
      if Is_Run (Build.Target) then
         if not Build.Quiet then
            Cmd_Console := Get_Build_Console
              (Kernel_Handle (Self.Builder.Kernel),
               Build.Shadow, Build.Background, False,
               "Run: " & Build.Main.Display_Base_Name);

            --  Update console in Builder.Last_Build
            Build.Console := Cmd_Console.Get_Console_Messages_Window;
            Self.Builder.Set_Last_Build (Build);

            if Show_Status then
               Build.Console.Raise_Console (Give_Focus => True);
               Build.Console.Clear;
            end if;

            if not Build.Background then
               Console := Cmd_Console;
            end if;
         end if;
      else
         Cmd_Console := Get_Build_Console
           (Kernel_Handle (Self.Builder.Kernel),
            Build.Shadow, Build.Background, False);

         --  Update console in Builder.Last_Build
         Build.Console := Cmd_Console.Get_Console_Messages_Window;
         Self.Builder.Set_Last_Build (Build);

         if Show_Status then
            Build.Console.Raise_Console (Give_Focus => False);
         end if;

         Console := Cmd_Console;

         if not Build.Background then
            Raise_On_Error := True;
         end if;
      end if;

      if Console =  null then
         return Child;
      else
         return new Console_Writer'(Child          => Child,
                                    Builder        => Self.Builder,
                                    Build          => Build,
                                    Console        => Console,
                                    Raise_On_Error => Raise_On_Error,
                                    Show_Status    => Show_Status,
                                    Start_Time     => Ada.Calendar.Clock);
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
      Kernel : constant Kernel_Handle := Kernel_Handle (Self.Builder.Kernel);
   begin
      if Self.Show_Status then
         declare
            End_Time   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Time_Stamp : constant String := Timestamp (End_Time);
         begin
            if Status = 0 then
               Self.Console.Insert
                 (Time_Stamp &
                  (-"process terminated successfully (elapsed time: ")
                  & Elapsed (Self.Start_Time, End_Time) & "s)");
            else
               Self.Console.Insert
                 (Time_Stamp
                  & (-"process exited with status ")
                  & Image (Status) & " (elapsed time: "
                  & Elapsed (Self.Start_Time, End_Time) & "s)");
            end if;
         end;
      end if;

      --  Raise the messages window is compilation was unsuccessful
      --  and no error was parsed. See D914-005

      if Self.Raise_On_Error and then Status /= 0 and then
        Category_Count (Kernel, To_String (Self.Build.Category)) = 0
      then
         Kernel.Raise_Console;
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
      Self.Console.Insert (Item, Add_LF => False);
      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self    : access Output_Parser_Fabric;
      Builder : Builder_Context) is
   begin
      Self.Builder := Builder;
   end Set;

end Build_Command_Manager.Console_Writers;
