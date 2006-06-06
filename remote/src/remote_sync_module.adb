-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Ada.Exceptions;         use Ada.Exceptions;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.Regpat;            use GNAT.Regpat;
with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with Glib;               use Glib;
with Glib.Xml_Int;       use Glib.Xml_Int;
with Gtk.Box;            use Gtk.Box;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Label;          use Gtk.Label;
with Gtk.Progress_Bar;   use Gtk.Progress_Bar;
with Gtk.Main;

with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Console; use GPS.Kernel.Console;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Remote;  use GPS.Kernel.Remote;
with GPS.Kernel.Timeout; use GPS.Kernel.Timeout;

with Commands;           use Commands;
with Filesystem;         use Filesystem;
with Filesystem.Windows; use Filesystem.Windows;
with Password_Manager;   use Password_Manager;
with String_Utils;       use String_Utils;
with Traces;             use Traces;
with VFS;                use VFS;

package body Remote_Sync_Module is

   Me : constant Debug_Handle := Create ("remote_sync_module");

   type Rsync_Module_Record is new Module_ID_Record with record
      Kernel     : Kernel_Handle;
      Rsync_Args : String_List_Access;
   end record;
   type Rsync_Module_ID is access all Rsync_Module_Record'Class;

   procedure Customize
     (Module : access Rsync_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  See doc for inherited subprogram

   Rsync_Module : Rsync_Module_ID := null;

   type Rsync_Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Progress      : Gtk.Progress_Bar.Gtk_Progress_Bar;
      File_Progress : Gtk.Progress_Bar.Gtk_Progress_Bar;
   end record;
   type Rsync_Dialog is access all Rsync_Dialog_Record'Class;

   procedure Gtk_New (Dialog : out Rsync_Dialog;
                      Kernel : access Kernel_Handle_Record'Class;
                      Src_Path, Dest_Path : String);
   --  Creates a new Rsync_Dialog

   type Return_Data is record
      Status : Integer;
   end record;
   type Return_Data_Access is access all Return_Data;

   type Rsync_Callback_Data is new Callback_Data_Record with record
      Network_Name      : String_Access;
      User_Name         : String_Access;
      Nb_Password_Tries : Natural;
      Synchronous       : Boolean;
      Dialog            : Rsync_Dialog;
      Dialog_Running    : Boolean;
      Ret_Data          : Return_Data_Access;
      Buffer            : String_Access;
   end record;

   function On_Rsync_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  run RSync hook

   procedure Parse_Rsync_Output
     (Data : Process_Data; Output : String);
   --  Called whenever new output from rsync is available

   procedure Rsync_Terminated
     (Data : Process_Data; Status : Integer);
   --  Called when rsync exits.

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      --  Register the module
      Rsync_Module := new Rsync_Module_Record;
      Rsync_Module.Kernel := Kernel;
      Register_Module
        (Rsync_Module, Kernel, "rsync");

      Add_Hook (Kernel, Rsync_Action_Hook,
                Wrapper (On_Rsync_Hook'Access),
                "rsync");
   end Register_Module;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Rsync_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Child   : Node_Ptr;
   begin
      if Node.Tag.all = "rsync_configuration" then
         Trace (Me, "Customize: 'rsync_configuration'");
         Child := Find_Tag (Node.Child, "arguments");
         if Child /= null then
            Module.Rsync_Args := Argument_String_To_List (Child.Value.all);
         end if;
      end if;
   end Customize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Dialog : out Rsync_Dialog;
                      Kernel : access Kernel_Handle_Record'Class;
                      Src_Path, Dest_Path : String) is
      Label : Gtk_Label;
   begin
      Dialog := new Rsync_Dialog_Record;
      Initialize (Dialog, -"Synchronisation in progress",
                  Get_Main_Window (Kernel), 0);
      Set_Has_Separator (Dialog, False);
      Gtk_New (Label, -"Synchronisation with remote host in progress.");
      Pack_Start (Get_Vbox (Dialog), Label);
      Gtk_New (Label);
      Set_Markup (Label, (-"From: ") & "<span foreground=""blue"">" &
                  Src_Path & "</span>");
      Pack_Start (Get_Vbox (Dialog), Label);
      Gtk_New (Label);
      Set_Markup (Label, (-"To: ") & "<span foreground=""blue"">" &
                  Dest_Path & "</span>");
      Pack_Start (Get_Vbox (Dialog), Label);
      Gtk_New (Dialog.Progress);
      Pack_Start (Get_Vbox (Dialog), Dialog.Progress);
      Gtk_New (Dialog.File_Progress);
      Pack_Start (Get_Vbox (Dialog), Dialog.File_Progress);
      Show_All (Dialog);
   end Gtk_New;

   -------------------
   -- On_Rsync_Hook --
   -------------------

   function On_Rsync_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      Rsync_Data    : Rsync_Hooks_Args renames Rsync_Hooks_Args (Data.all);
      Src_Path      : String_Access;
      Dest_Path     : String_Access;
      Src_FS        : Filesystem_Access;
      Dest_FS       : Filesystem_Access;
      Machine       : Machine_Descriptor;
      Success       : Boolean;
      Cb_Data       : Rsync_Callback_Data;
      Ret_Data      : aliased Return_Data;

      function  Build_Arg return String_List;
      --  Build rsync arguments

      ---------------
      -- Build_Arg --
      ---------------

      function Build_Arg return String_List
      is
         Rsync_Args    : constant String_List
           := Clone (Rsync_Module.Rsync_Args.all);

         function Transport_Arg return String_List;
         --  Argument for transport

         function Use_Links_Arg return String_List;
         --  Argument for link transfer

         -------------------
         -- Transport_Arg --
         -------------------

         function Transport_Arg return String_List is
         begin
            if Machine.Access_Name.all = "ssh" then
               return (1 => new String'("--rsh=ssh"));
            else
               return (1 .. 0 => null);
            end if;
         end Transport_Arg;

         -------------------
         -- Use_Links_Arg --
         -------------------

         function Use_Links_Arg return String_List is
         begin
            --  No support for symbolic links under windows...
            if Dest_FS.all in Windows_Filesystem_Record'Class then
               return (1 => new String'("-L"));
            else
               return (1 .. 0 => null);
            end if;
         end Use_Links_Arg;

      begin
         return Rsync_Args & Use_Links_Arg & Transport_Arg &
           Src_Path & Dest_Path;
      end Build_Arg;

   begin
      --  Check that we want to use rsync
      if Rsync_Data.Tool_Name /= "rsync" then
         return False;
      end if;

      --  Check the module configuration
      if Rsync_Module = null or else Rsync_Module.Rsync_Args = null then
         Insert (Kernel, "Invalid rsync configuration. Cannot use rsync.",
                 Mode => Error);
         return False;
      end if;

      if Rsync_Data.Src_Name = "" then
         --  Local src machine, remote dest machine
         Machine := Get_Machine_Descriptor (Rsync_Data.Dest_Name);
         Src_FS    := new Filesystem_Record'Class'(Get_Local_Filesystem);
         Src_Path  := new String'
           (To_Unix (Src_FS.all, Rsync_Data.Src_Path, True));
         Dest_FS   := new Filesystem_Record'Class'
           (Get_Filesystem (Rsync_Data.Dest_Name));
         if Machine.User_Name.all /= "" then
            Dest_Path := new String'
              (Machine.User_Name.all & "@" &
               Machine.Network_Name.all & ":" &
               To_Unix (Dest_FS.all, Rsync_Data.Dest_Path, True));
         else
            Dest_Path := new String'
              (Machine.Network_Name.all & ":" &
               To_Unix (Dest_FS.all, Rsync_Data.Dest_Path, True));
         end if;
      else
         --  Remote src machine, local dest machine
         Machine := Get_Machine_Descriptor (Rsync_Data.Src_Name);
         Src_FS    := new Filesystem_Record'Class'
           (Get_Filesystem (Rsync_Data.Src_Name));
         if Machine.User_Name.all /= "" then
            Src_Path  := new String'
              (Machine.User_Name.all & "@" &
               Machine.Network_Name.all & ":" &
               To_Unix (Src_FS.all, Rsync_Data.Src_Path, True));
         else
            Src_Path  := new String'
              (Machine.Network_Name.all & ":" &
               To_Unix (Src_FS.all, Rsync_Data.Src_Path, True));
         end if;
         Dest_FS   := new Filesystem_Record'Class'(Get_Local_Filesystem);
         Dest_Path := new String'
           (To_Unix (Dest_FS.all, Rsync_Data.Dest_Path, True));
      end if;

      if Machine = null then
         return False;
      end if;

      Ret_Data.Status := 0;
      Cb_Data := (Network_Name      => Machine.Network_Name,
                  User_Name         => Machine.User_Name,
                  Nb_Password_Tries => 0,
                  Synchronous       => Rsync_Data.Synchronous,
                  Dialog            => null,
                  Dialog_Running    => False,
                  Ret_Data          => Ret_Data'Unchecked_Access,
                  Buffer            => null);

      if Rsync_Data.Synchronous then
         Gtk_New (Cb_Data.Dialog, Kernel,
                  Rsync_Data.Src_Path, Rsync_Data.Dest_Path);
         Gtk.Main.Grab_Add (Cb_Data.Dialog);
      end if;

      --  Do not set Line_By_Line as this will prevent the password prompt
      --  catch.
      --  Do not strip CR's as file progression is output by rsync using CR
      --  characters.
      --  Do not use pipes in order to be able to retrieve password prompts on
      --  Windows
      Launch_Process
        (Kernel_Handle (Kernel),
         Command       => "rsync",
         Arguments     => Build_Arg,
         Console       => Get_Console (Kernel),
         Show_Command  => Rsync_Data.Print_Output,
         Show_Output   => Rsync_Data.Print_Output,
         Success       => Success,
         Line_By_Line  => False,
         Callback      => Parse_Rsync_Output'Access,
         Exit_Cb       => Rsync_Terminated'Access,
         Callback_Data => new Rsync_Callback_Data'(Cb_Data),
         Queue_Id      => Rsync_Data.Queue_Id,
         Synchronous   => Rsync_Data.Synchronous,
         Timeout       => Machine.Timeout,
         Strip_CR      => False,
         Use_Pipes     => False);

      Free (Src_Path);
      Free (Dest_Path);

      return Ret_Data.Status = 0 and then Success;
   end On_Rsync_Hook;

   ------------------------
   -- Parse_Rsync_Output --
   ------------------------

   procedure Parse_Rsync_Output
     (Data : Process_Data; Output : String)
   is
      Progress_Regexp : constant Pattern_Matcher := Compile
        ("^.*\(([0-9]*), [0-9.%]* of ([0-9]*)", Multiple_Lines);
      File_Regexp     : constant Pattern_Matcher := Compile
        ("^([^ ][^\n\r]*[^\n\r/])$", Multiple_Lines or Single_Line);
      File_Progress_Regexp : constant Pattern_Matcher := Compile
        ("^ *[0-9]* *([0-9]*)%", Multiple_Lines);
      Matched         : Match_Array (0 .. 2);
      File_Nb         : Natural;
      Total_Files     : Natural;
      Force           : Boolean;
      Cb_Data         : Rsync_Callback_Data renames
        Rsync_Callback_Data (Data.Callback_Data.all);
      Dead            : Boolean;
      Old_Buff        : String_Access;
      LF_Index        : Integer;
      pragma Unreferenced (Dead);

      function Cat (S1 : String_Access; S2 : String) return String;
      --  Cat a string access and a string

      function Cat (S1 : String; S2 : String_Access) return String;
      --  Cat a string access and a string

      ---------
      -- Cat --
      ---------

      function Cat (S1 : String_Access; S2 : String) return String is
      begin
         if S1 = null then
            return S2;
         else
            return S1.all & S2;
         end if;
      end Cat;

      ---------
      -- Cat --
      ---------

      function Cat (S1 : String; S2 : String_Access) return String is
      begin
         if S2 = null then
            return S1;
         else
            return S1 & S2.all;
         end if;
      end Cat;

   begin
      Old_Buff := Cb_Data.Buffer;
      Cb_Data.Buffer := null;

      LF_Index := Output'First - 1;

      for J in reverse Output'Range loop
         if Output (J) = ASCII.LF
           or else Output (J) = ASCII.CR
         then
            if J /= Output'Last then
               Cb_Data.Buffer := new String'(Output (J + 1 .. Output'Last));
            end if;

            LF_Index := J;
            exit;
         end if;
      end loop;

      if LF_Index = Output'First - 1 then
         --  no LF found for now. Place everything in buffer.
         Cb_Data.Buffer := new String'(Cat (Old_Buff, Output));
         Free (Old_Buff);
      end if;

      if not Data.Process_Died then

         declare
            Stripped_Buffer : constant String :=
                                Cat (Old_Buff,
                                     Output (Output'First .. LF_Index));
            Buffer          : constant String :=
                                Cat (Stripped_Buffer, Cb_Data.Buffer);
         begin
            Trace (Me, "Parse_Rsync_Output: " & ASCII.LF & Buffer);

            --  Retrieve password prompt if any
            Match (Get_Default_Password_Regexp,
                   Buffer,
                   Matched);

            if Matched (0) /= No_Match then
               Force := Cb_Data.Nb_Password_Tries > 0;
               Cb_Data.Nb_Password_Tries := Cb_Data.Nb_Password_Tries + 1;

               declare
                  Password : constant String :=
                               Get_Password (Get_Main_Window (Data.Kernel),
                                             Cb_Data.Network_Name.all,
                                             Cb_Data.User_Name.all,
                                             Force);
               begin
                  if Password = "" then
                     Interrupt (Data.Descriptor.all);
                  else
                     Send (Data.Descriptor.all, Password);
                  end if;
               end;

               --  Do not preserve the password prompt
               Free (Cb_Data.Buffer);
               return;
            end if;

            --  Retrieve passphrase prompt if any
            Match (Get_Default_Passphrase_Regexp,
                   Buffer,
                   Matched);

            if Matched (0) /= No_Match then
               Force := Cb_Data.Nb_Password_Tries > 0;
               Cb_Data.Nb_Password_Tries := Cb_Data.Nb_Password_Tries + 1;

               declare
                  Password : constant String :=
                               Get_Passphrase
                                 (Get_Main_Window (Data.Kernel),
                                  Buffer
                                    (Matched (1).First .. Matched (1).Last),
                                  Force);
               begin
                  if Password = "" then
                     Interrupt (Data.Descriptor.all);
                  else
                     Send (Data.Descriptor.all, Password);
                  end if;
               end;

               --  Do not preserve the password prompt
               Free (Cb_Data.Buffer);
               return;
            end if;

            --  Retrieve progression.
            Match (Progress_Regexp,
                   Stripped_Buffer,
                   Matched);

            if Matched (0) /= No_Match then
               File_Nb := Natural'Value
                 (Stripped_Buffer (Matched (1).First .. Matched (1).Last));
               Total_Files := Natural'Value
                 (Stripped_Buffer (Matched (2).First .. Matched (2).Last));

               if Cb_Data.Synchronous then
                  Cb_Data.Dialog_Running := True;
                  Set_Fraction (Cb_Data.Dialog.Progress,
                                Gdouble (File_Nb) / Gdouble (Total_Files));
                  Set_Text (Cb_Data.Dialog.Progress,
                            Natural'Image (File_Nb) & "/" &
                            Natural'Image (Total_Files));

               else
                  Set_Progress (Data.Command,
                                Progress => (Activity => Running,
                                             Current  => File_Nb,
                                             Total    => Total_Files));
               end if;
            end if;

            if Cb_Data.Synchronous and then not Cb_Data.Dialog_Running then
               Pulse (Cb_Data.Dialog.Progress);

            elsif Cb_Data.Synchronous and then Cb_Data.Dialog_Running then

               --  Get file transfered
               Match (File_Regexp,
                      Stripped_Buffer,
                      Matched);

               if Matched (0) /= No_Match then
                  Set_Text (Cb_Data.Dialog.File_Progress,
                            Stripped_Buffer
                              (Matched (1).First .. Matched (1).Last));
               end if;

               --  Get file transfered progression
               Match (File_Progress_Regexp,
                      Stripped_Buffer,
                      Matched);

               if Matched (0) /= No_Match then
                  declare
                     Percent : constant Natural := Natural'Value
                       (Stripped_Buffer
                          (Matched (1).First .. Matched (1).Last));
                  begin
                     Set_Fraction (Cb_Data.Dialog.File_Progress,
                                   Gdouble (Percent) / 100.0);
                  end;
               end if;
            end if;

            if Cb_Data.Synchronous then
               while Gtk.Main.Events_Pending loop
                  Dead := Gtk.Main.Main_Iteration;
               end loop;
            end if;
         end;
      end if;

      Free (Old_Buff);
   end Parse_Rsync_Output;

   ----------------------
   -- Rsync_Terminated --
   ----------------------

   procedure Rsync_Terminated
     (Data : Process_Data; Status : Integer)
   is
      Cb_Data : Rsync_Callback_Data renames
        Rsync_Callback_Data (Data.Callback_Data.all);
   begin
      Trace (Me, "Rsync_Terminated");

      if Cb_Data.Synchronous and then Cb_Data.Dialog /= null then
         Hide_All (Cb_Data.Dialog);
         Gtk.Main.Grab_Remove (Cb_Data.Dialog);
         Cb_Data.Dialog := null;
      end if;

      Cb_Data.Ret_Data.Status := Status;
      Trace (Me, "rsync status is" & Integer'Image (Status));

   exception
      when E : others =>
         Trace (Exception_Handle, Exception_Information (E));
   end Rsync_Terminated;

end Remote_Sync_Module;
