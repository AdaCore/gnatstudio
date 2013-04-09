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
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;

with Gtk.Text_View;                    use Gtk.Text_View;
with Gtkada.MDI;                       use Gtkada.MDI;

with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Console;               use GPS.Kernel.Console;
with GPS.Kernel.Interactive;           use GPS.Kernel.Interactive;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Intl;                         use GPS.Intl;

with GPS.Tools_Output;                 use GPS.Tools_Output;

package body Commands.Builder is

   Shell_Env : constant String := Getenv ("SHELL").all;

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

   --------------------------
   -- Launch_Build_Command --
   --------------------------

   procedure Launch_Build_Command
     (Kernel           : GPS.Kernel.Kernel_Handle;
      CL               : Arg_List;
      Server           : Server_Type;
      Synchronous      : Boolean;
      Use_Shell        : Boolean;
      Console          : Interactive_Console;
      Directory        : Virtual_File;
      Builder          : Builder_Context;
      Target_Name      : String;
      Mode             : String;
      Category_Name    : Unbounded_String;
      Quiet            : Boolean;
      Shadow           : Boolean;
      Background       : Boolean;
      Is_Run           : Boolean)
   is
      CL2      : Arg_List;
      Success  : Boolean := False;
      Cmd_Name : Unbounded_String;
      Show_Command : Boolean;
      Created_Command : Command_Access;
      Output_Parser  : Tools_Output_Parser_Access;
   begin
      Output_Parser  := New_Parser_Chain (Target_Name);

      Show_Command := not Background and not Quiet;

      if not Is_Run and then not Background then
         --  If we are starting a "real" build, remove messages from the
         --  current background build
         Get_Messages_Container (Kernel).Remove_Category
           (Builder.Previous_Background_Build_Id,
            Background_Message_Flags);
      end if;

      if not Shadow and Show_Command then
         if Is_Run then
            Clear (Console);
            Raise_Child (Find_MDI_Child (Get_MDI (Kernel), Console),
                         Give_Focus => True);
         else
            Raise_Console (Kernel);
         end if;
      end if;

      if Is_Run
        or else Compilation_Starting
          (Handle     => Kernel,
           Category   => To_String (Category_Name),
           Quiet      => Quiet,
           Shadow     => Shadow,
           Background => Background)
      then
         if not Quiet then
            Append_To_Build_Output
              (Builder,
               To_Display_String (CL), Target_Name,
               Shadow, Background);
         end if;

         Cmd_Name := To_Unbounded_String (Target_Name);

         if Mode /= "default" then
            Cmd_Name := Cmd_Name & " (" & Mode & ")";
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

         if Synchronous then
            Kernel.Process_Launcher.Launch_Process
              (CL              => CL2,
               Server          => Server,
               Directory       => Directory,
               Output_Parser   => Output_Parser,
               Show_Command_To => Console.Get_Console_Messages_Window,
               Success         => Success,
               Created_Command => Created_Command);
         else
            Kernel.Process_Launcher.Launch_Process_In_Background
              (CL              => CL2,
               Server          => Server,
               Directory       => Directory,
               Output_Parser   => Output_Parser,
               Show_Command_To => Console.Get_Console_Messages_Window,
               Success         => Success,
               Show_In_Task_Manager => not Background,
               Name_In_Task_Manager => To_String (Cmd_Name),
               Block_Exit           => not (Shadow
                                            or else Background
                                            or else Quiet),
               Created_Command      => Created_Command);
         end if;

         --  ??? check value of Success

         if Success and then Background then
            Background_Build_Started (Builder, Created_Command);
         end if;
      end if;

   end Launch_Build_Command;

end Commands.Builder;
