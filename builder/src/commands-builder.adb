------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

with Ada.Strings;                      use Ada.Strings;

with GNAT.OS_Lib;                      use GNAT.OS_Lib;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Task_Manager;          use GPS.Kernel.Task_Manager;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;

with Gtk.Text_View;                    use Gtk.Text_View;
with Gtkada.MDI;                       use Gtkada.MDI;

with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Console;               use GPS.Kernel.Console;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Legacy;       use GPS.Kernel.Messages.Legacy;
with GPS.Intl;                         use GPS.Intl;

with Builder_Facility_Module;          use Builder_Facility_Module;
with Commands.Builder.Progress_Parsers;

package body Commands.Builder is

   Shell_Env : constant String := Getenv ("SHELL").all;

   Progress_Parser   : aliased Progress_Parsers.Output_Parser_Fabric;
   Parser_Registered : Boolean := False;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Build_Callback (Data : Process_Data; Output : String);
   --  Callback for the build output

   procedure End_Build_Callback (Data : Process_Data; Status : Integer);
   --  Called at the end of the build

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Data : in out Build_Callback_Data) is
   begin
      if Data.Output_Parser /= null then
         Free (Data.Output_Parser);
      end if;
   end Destroy;

   -----------------------
   -- Get_Build_Console --
   -----------------------

   function Get_Build_Console
     (Kernel              : GPS.Kernel.Kernel_Handle;
      Shadow              : Boolean;
      Background          : Boolean;
      Create_If_Not_Exist : Boolean;
      New_Console_Name    : String := "") return Interactive_Console
   is
      Console : Interactive_Console;
   begin
      if New_Console_Name /= "" then
         Console := Create_Interactive_Console
           (Kernel              => Kernel,
            Title               => New_Console_Name,
            History             => "interactive",
            Create_If_Not_Exist => True,
            Module              => null,
            Force_Create        => False,
            ANSI_Support        => True,
            Accept_Input        => True);

         Modify_Font (Get_View (Console), View_Fixed_Font.Get_Pref);

         return Console;
      end if;

      if Background then
         return Create_Interactive_Console
           (Kernel              => Kernel,
            Title               => -"Background Builds",
            History             => "interactive",
            Create_If_Not_Exist => Create_If_Not_Exist,
            Module              => null,
            Force_Create        => False,
            Accept_Input        => False);

      elsif Shadow then
         return Create_Interactive_Console
           (Kernel              => Kernel,
            Title               => -"Auxiliary Builds",
            History             => "interactive",
            Create_If_Not_Exist => Create_If_Not_Exist,
            Module              => null,
            Force_Create        => False,
            Accept_Input        => False);
      else
         return Get_Console (Kernel);
      end if;
   end Get_Build_Console;

   ------------------------
   -- End_Build_Callback --
   ------------------------

   procedure End_Build_Callback (Data : Process_Data; Status : Integer) is
      Build_Data : Build_Callback_Data
                     renames Build_Callback_Data (Data.Callback_Data.all);

   begin
      if Build_Data.Is_A_Run then
         --  Nothing to do for runs.
         return;
      end if;

      --  Raise the messages window is compilation was unsuccessful
      --  and no error was parsed. See D914-005

      if Category_Count (Data.Kernel, To_String (Build_Data.Category_Name)) = 0
        and then Status /= 0
        and then not Build_Data.Background
      then
         Console.Raise_Console (Data.Kernel);
      end if;

      Destroy (Build_Data.Background_Env);

      if Build_Data.Background then
         --  We remove the previous background build data messages only when
         --  the new background build is completed.

         Get_Messages_Container (Data.Kernel).Remove_Category
           (Previous_Background_Build_Id, Background_Message_Flags);

         Background_Build_Finished;
      end if;

      --  ??? should also pass the Status value to Compilation_Finished
      --  and to the corresponding hook

      Compilation_Finished
        (Data.Kernel,
         To_String (Build_Data.Category_Name),
         To_String (Build_Data.Target_Name),
         To_String (Build_Data.Mode_Name),
         Build_Data.Shadow,
         Build_Data.Background,
         Status);
   end End_Build_Callback;

   --------------------
   -- Build_Callback --
   --------------------

   procedure Build_Callback (Data : Process_Data; Output : String) is
      Build_Data : Build_Callback_Data
        renames Build_Callback_Data (Data.Callback_Data.all);
   begin
      if not Build_Data.Is_A_Run and Build_Data.Output_Parser = null then
         --  Initialize progress parser with command
         Progress_Parser.Set_Command (Data.Command);
            --  Create new chain of output parsers
         Build_Data.Output_Parser := New_Parser_Chain;
      end if;

      if Build_Data.Output_Parser /= null then
         Build_Data.Output_Parser.Parse_Standard_Output (Output);
      end if;
   end Build_Callback;

   --------------------------
   -- Launch_Build_Command --
   --------------------------

   procedure Launch_Build_Command
     (Kernel           : GPS.Kernel.Kernel_Handle;
      CL               : Arg_List;
      Data             : Build_Callback_Data_Access;
      Server           : Server_Type;
      Synchronous      : Boolean;
      Use_Shell        : Boolean;
      Console          : Interactive_Console;
      Directory        : Virtual_File)
   is
      CL2      : Arg_List;
      Success  : Boolean := False;
      Cmd_Name : Unbounded_String;
      Show_Output  : Boolean;
      Show_Command : Boolean;
      Created_Command : Scheduled_Command_Access;
      Background : constant Boolean := Data.Background;
   begin
      --  Register progress output parser once
      if not Parser_Registered then
         declare
            Progress_Pattern : constant String :=
              "completed ([0-9]+) out of ([0-9]+) \(([^\n]*)%\)\.\.\.\n";
            --  ??? This is configurable in some cases (from XML for instance),
            --  so we should not have a hard coded regexp here.
         begin
            Register_Output_Parser (Progress_Parser'Access, Reserved + 10);
            Progress_Parser.Set_Pattern (Progress_Pattern);
            Parser_Registered := True;
         end;
      end if;

      Show_Output  := Data.Is_A_Run and not Data.Background and not Data.Quiet;
      Show_Command := not Data.Background and not Data.Quiet;

      if not Data.Is_A_Run
        and then not Data.Background
      then
         --  If we are starting a "real" build, remove messages from the
         --  current background build
         Get_Messages_Container (Kernel).Remove_Category
           (Previous_Background_Build_Id, Background_Message_Flags);
      end if;

      if not Data.Shadow and Show_Command then
         if Data.Is_A_Run then
            Clear (Console);
            Raise_Child (Find_MDI_Child (Get_MDI (Kernel), Console),
                         Give_Focus => True);
         else
            Raise_Console (Kernel);
         end if;
      end if;

      if Data.Is_A_Run
        or else Compilation_Starting
          (Handle     => Kernel,
           Category   => To_String (Data.Category_Name),
           Quiet      => Data.Quiet,
           Shadow     => Data.Shadow,
           Background => Data.Background)
      then
         Append_To_Build_Output
           (Kernel, To_Display_String (CL), To_String (Data.Target_Name),
            Data.Shadow, Data.Background);

         if Data.Mode_Name /= "default" then
            Cmd_Name := Data.Target_Name & " (" & Data.Mode_Name & ")";
         else
            Cmd_Name := Data.Target_Name;
         end if;

         if Use_Shell
           and then Shell_Env /= ""
           and then Is_Local (Server)
         then
            Append_Argument (CL2, Shell_Env, One_Arg);
            Append_Argument (CL2, "-c", One_Arg);
            Append_Argument (CL2, To_Display_String (CL), One_Arg);
         else
            CL2 := CL;
         end if;

         Launch_Process
           (Kernel,
            CL                   => CL2,
            Server               => Server,
            Console              => Console,
            Show_Command         => Show_Command,
            Show_Output          => Show_Output,
            Callback_Data        => Data.all'Access,
            Success              => Success,
            Line_By_Line         => False,
            Directory            => Directory,
            Callback             => Build_Callback'Access,
            Exit_Cb              => End_Build_Callback'Access,
            Show_In_Task_Manager => not Data.Background,
            Name_In_Task_Manager => To_String (Cmd_Name),
            Synchronous          => Synchronous,
            Show_Exit_Status     => not (Data.Shadow
              or else Data.Background
              or else Data.Quiet),
            Created_Command      => Created_Command);

         --  ??? check value of Success

         if Success and then Background then
            Background_Build_Started (Created_Command);
         end if;
      end if;

   end Launch_Build_Command;

end Commands.Builder;
