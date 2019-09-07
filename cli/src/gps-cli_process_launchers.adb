------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with GNAT.Expect;     use GNAT.Expect;
with GNAT.OS_Lib;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;    use GNATCOLL.VFS;
with Ada.Exceptions; use Ada.Exceptions;

package body GPS.CLI_Process_Launchers is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("Process Launchers", On);

   --------------------
   -- Launch_Process --
   --------------------

   overriding procedure Launch_Process
     (Launcher             : access CLI_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Abstract_Messages_Window_Access := null;
      Success              : out Boolean)
   is
      pragma Unreferenced (Launcher);
      Old_Dir     : Virtual_File;
      Status      : aliased Integer := -1;
      Arg_List    : GNAT.OS_Lib.Argument_List :=
        To_List (CL, Include_Command => False);

      procedure Finally;
      --  Gather instruction to do at the end of the procedure
      --  in any case

      procedure Finally is
      begin
         if Directory /= No_File then
            Change_Dir (Old_Dir);
         end if;

         for J in Arg_List'Range loop
            GNAT.OS_Lib.Free (Arg_List (J));
         end loop;
      end Finally;

   begin
      if not Is_Local (Server) then
         --  Remote server not implemented
         Success := False;
         return;
      elsif Show_Command_To /= null then
         Show_Command_To.Insert (To_Display_String (CL), Add_LF => True);
      end if;

      if Directory /= No_File then
         Old_Dir := Get_Current_Dir;
         Change_Dir (Directory);
      end if;

      Success := True;

      declare
         Output : constant String := Get_Command_Output
           (Command    => Get_Command (CL),
            Arguments  => Arg_List,
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         Output_Parser.Parse_Standard_Output (Output, Command => null);
         Output_Parser.End_Of_Stream (Status, Command => null);
      end;

      Finally;

   exception
      when E : Invalid_Process =>
         Trace (Me, "Exception " & Exception_Name (E)
                & ": Could not launch process: " & Get_Command (CL));
         Success := False;
         Finally;
   end Launch_Process;

   ----------------------------------
   -- Launch_Process_In_Background --
   ----------------------------------

   overriding procedure Launch_Process_In_Background
     (Launcher             : access CLI_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Abstract_Messages_Window_Access := null;
      Success              : out Boolean;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Block_Exit           : Boolean := True;
      Created_Command      : out Command_Access)
   is
      pragma Unreferenced (Show_In_Task_Manager);
      pragma Unreferenced (Name_In_Task_Manager);
      pragma Unreferenced (Block_Exit);
   begin
      Launcher.Launch_Process
        (CL              => CL,
         Server          => Server,
         Directory       => Directory,
         Output_Parser   => Output_Parser,
         Show_Command_To => Show_Command_To,
         Success         => Success);

      Created_Command := null;
   end Launch_Process_In_Background;

end GPS.CLI_Process_Launchers;
