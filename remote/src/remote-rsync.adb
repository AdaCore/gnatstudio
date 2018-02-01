------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.Expect;           use GNAT.Expect;
with GNAT.Strings;

with Glib;                  use Glib;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Label;             use Gtk.Label;
with Gtk.Progress_Bar;      use Gtk.Progress_Bar;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Handlers;       use Gtkada.Handlers;

with GPS.Intl;              use GPS.Intl;
with GPS.Kernel.Console;    use GPS.Kernel.Console;
with GPS.Kernel.Hooks;      use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Kernel.Timeout;    use GPS.Kernel.Timeout;
with GPS.Scripts.Commands;  use GPS.Scripts.Commands;

with Commands;              use Commands;
with Password_Manager;      use Password_Manager;
with Remote.Db;             use Remote.Db;
with Remote_Module;         use Remote_Module;
with String_Utils;          use String_Utils;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.VFS_Types;    use GNATCOLL.VFS_Types;

with Gexpect.Db;            use Gexpect, Gexpect.Db;

with GNATCOLL.Arg_Lists;    use GNATCOLL.Arg_Lists;

package body Remote.Rsync is

   Me : constant Trace_Handle := Create ("GPS.REMOTE.remote_sync_module");

   Progress_Regexp         : constant Pattern_Matcher := Compile
     ("^.*\(([0-9]*), [0-9.%]* of ([0-9]*)", Multiple_Lines);
   File_Regexp             : constant Pattern_Matcher := Compile
     ("^([^ ][^\n\r]*[^\n\r/])$", Multiple_Lines or Single_Line);
   File_Progress_Regexp    : constant Pattern_Matcher := Compile
     ("^ *[0-9]* *([0-9]*)%", Multiple_Lines);
   Files_Considered_Regexp : constant Pattern_Matcher := Compile
     ("^ *[0-9]+ *files[.]*", Multiple_Lines or Single_Line);

   type Return_Data is record
      Status : Integer;
   end record;
   type Return_Data_Access is access all Return_Data;

   type Rsync_Module_Record is new Module_ID_Record with record
      Kernel   : Kernel_Handle;
      Ret_Data : Return_Data_Access;
   end record;
   type Rsync_Module_ID is access all Rsync_Module_Record'Class;

   overriding procedure Destroy (Module : in out Rsync_Module_Record);

   Rsync_Module : Rsync_Module_ID := null;

   type Rsync_Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Progress      : Gtk.Progress_Bar.Gtk_Progress_Bar;
      File_Progress : Gtk.Progress_Bar.Gtk_Progress_Bar;
      Abort_Button  : Gtk_Button;
      Aborted       : Boolean;
   end record;
   type Rsync_Dialog is access all Rsync_Dialog_Record'Class;

   procedure Gtk_New (Dialog : out Rsync_Dialog;
                      Kernel : access Kernel_Handle_Record'Class;
                      Src_Path, Dest_Path : Virtual_File);
   --  Creates a new Rsync_Dialog

   type Rsync_Callback_Data is new External_Process_Data with record
      Network_Name      : GNAT.Strings.String_Access;
      User_Name         : GNAT.Strings.String_Access;
      Nb_Password_Tries : Natural;
      Synchronous       : Boolean;
      Dialog            : Rsync_Dialog;
      Dialog_Running    : Boolean;
      Ret_Data          : Return_Data_Access;
      Buffer            : GNAT.Strings.String_Access;
   end record;
   type Rsync_Callback_Data_Access is access all Rsync_Callback_Data'Class;
   overriding procedure On_Output
     (Self    : not null access Rsync_Callback_Data;
      Command : not null access Root_Command'Class;
      Output  : String);
   overriding procedure On_Exit
     (Self    : not null access Rsync_Callback_Data;
      Command : not null access Root_Command'Class);

   type On_Rsync is new Rsync_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Rsync;
      Kernel : not null access Kernel_Handle_Record'Class;
      Synchronous, Force, To_Remote, Print_Output, Print_Cmd : Boolean;
      Tool_Name, Host_Name, Queue_Id : String;
      File : Virtual_File) return Boolean;
   --  run RSync hook

   procedure On_Abort_Clicked (Dialog : access Gtk_Widget_Record'Class);
   --  Abort button pressed

   function Cat (S1 : GNAT.OS_Lib.String_Access; S2 : String) return String;
   --  Concatenate a string access and a string

   function Cat (S1 : String; S2 : GNAT.OS_Lib.String_Access) return String;
   --  Concatenate a string access and a string

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      --  Register the module
      Rsync_Module := new Rsync_Module_Record;
      Rsync_Module.Kernel := Kernel;
      Rsync_Module.Ret_Data := new Return_Data'(Status => 0);
      Register_Module (Rsync_Module, Kernel, "rsync");

      Rsync_Action_Hook.Add (new On_Rsync);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Rsync_Module_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Return_Data, Return_Data_Access);
   begin
      Unchecked_Free (Module.Ret_Data);
   end Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog              : out Rsync_Dialog;
      Kernel              : access Kernel_Handle_Record'Class;
      Src_Path, Dest_Path : Virtual_File)
   is
      Label : Gtk_Label;
   begin
      Dialog := new Rsync_Dialog_Record;
      Initialize (Dialog, -"Synchronisation in progress",
                  Get_Main_Window (Kernel),
                  Destroy_With_Parent
                  or Use_Header_Bar_From_Settings (Get_Main_Window (Kernel)));
      Gtk_New (Label, -"Synchronisation with remote host in progress.");
      Pack_Start (Get_Content_Area (Dialog), Label);
      Gtk_New (Label);
      Set_Markup (Label, (-"From: ") & "<span foreground=""blue"">" &
                  Src_Path.Display_Full_Name & "</span>");
      Pack_Start (Get_Content_Area (Dialog), Label);
      Gtk_New (Label);
      Set_Markup (Label, (-"To: ") & "<span foreground=""blue"">" &
                  Dest_Path.Display_Full_Name & "</span>");
      Pack_Start (Get_Content_Area (Dialog), Label);
      Gtk_New (Dialog.Progress);
      Pack_Start (Get_Content_Area (Dialog), Dialog.Progress);
      Gtk_New (Dialog.File_Progress);
      Pack_Start (Get_Content_Area (Dialog), Dialog.File_Progress);
      Gtk_New_From_Stock (Dialog.Abort_Button, Stock_Cancel);
      Add_Action_Widget (Dialog, Dialog.Abort_Button, Gtk_Response_Cancel);
      Dialog.Aborted := False;
      Show_All (Dialog);
   end Gtk_New;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Rsync;
      Kernel : not null access Kernel_Handle_Record'Class;
      Synchronous, Force, To_Remote, Print_Output, Print_Cmd : Boolean;
      Tool_Name, Host_Name, Queue_Id : String;
      File : Virtual_File) return Boolean
   is
      pragma Unreferenced (Self);
      Remote_Path         : GNAT.OS_Lib.String_Access;
      Src_Path, Dest_Path : GNAT.OS_Lib.String_Access;
      Machine             : Gexpect.Machine_Access;
      All_Success         : Boolean := True;
      Success             : Boolean;
      Data                : Rsync_Callback_Data_Access;
      Real_Print_Output   : Boolean;

      function Build_Arg return GNAT.Strings.String_List;
      --  Build rsync arguments

      ---------------
      -- Build_Arg --
      ---------------

      function Build_Arg return GNAT.Strings.String_List is

         Rsync_Args : constant GNAT.Strings.String_List :=
                        Clone (Machine.Sync_Tool_Args);

         function Transport_Arg return GNAT.Strings.String_List;
         --  Argument for transport

         function Use_Links_Arg return GNAT.Strings.String_List;
         --  Argument for link transfer

         procedure Protect (S : in out GNAT.OS_Lib.String_Access);
         --  Return F's full name, protecting spaces and quotes

         -------------
         -- Protect --
         -------------

         procedure Protect (S : in out GNAT.OS_Lib.String_Access)
         is
            Out_Str : Unbounded_String;
            Ignore  : Boolean;

         begin
            Ignore := False;

            for J in S'Range loop
               if S (J) = '\' then
                  Ignore := True;
                  Out_Str := Out_Str & S (J);

               elsif not Ignore and then (S (J) = ' ' or else S (J) = '"') then
                  Ignore := False;
                  Out_Str := Out_Str & '\' & S (J);

               else
                  Ignore := False;
                  Out_Str := Out_Str & S (J);
               end if;
            end loop;

            Free (S);
            S := new String'(To_String (Out_Str));
         end Protect;

         -------------------
         -- Transport_Arg --
         -------------------

         function Transport_Arg return GNAT.Strings.String_List is
         begin
            if Machine.Access_Tool = "ssh" then
               return (1 => new String'("--rsh=ssh"));
            else
               return (1 .. 0 => null);
            end if;
         end Transport_Arg;

         -------------------
         -- Use_Links_Arg --
         -------------------

         function Use_Links_Arg return GNAT.Strings.String_List is
         begin
            --  No support for symbolic links under windows...
            if Machine.Shell_FS = FS_Windows then
               return (1 => new String'("-L"));
            else
               return (1 .. 0 => null);
            end if;
         end Use_Links_Arg;

      begin
         Protect (Src_Path);
         Protect (Dest_Path);
         return Rsync_Args & Use_Links_Arg & Transport_Arg &
           Src_Path & Dest_Path;
      end Build_Arg;

      Src : Ada.Strings.Unbounded.Unbounded_String;
      Dst : Ada.Strings.Unbounded.Unbounded_String;

   begin
      --  Check that we want to use rsync
      if Tool_Name /= "rsync" then
         return False;
      end if;

      --  Check the module configuration
      if Rsync_Module = null then
         Insert (Kernel, "Invalid rsync configuration. Cannot use rsync.",
                 Mode => Error);
         return False;
      end if;

      if not Is_Configured (Host_Name) then
         Insert (Kernel, "Invalid configuration : " & Host_Name &
                 " is not configured",
                 Mode => Error);
         return False;
      end if;

      Machine := Get_Server (Host_Name);

      if Machine.Sync_Tool_Args'Length = 0 then
         Insert (Kernel, "Invalid rsync configuration. Cannot use rsync.",
                 Mode => Error);
         return False;
      end if;

      declare
         M_Points : constant Mount_Point_Array :=
                      Get_Mount_Points (Get_Database.all, Host_Name);
         Do_Sync  : Boolean;
      begin
         for J in M_Points'Range loop

            Do_Sync := False;
            case M_Points (J).Sync is
               when Always =>
                  Do_Sync := True;
               when On_Request =>
                  Do_Sync := Force;
               when Never =>
                  Do_Sync := False;
               when To_Local =>
                  Do_Sync := not To_Remote;
               when Remote.Db.To_Remote =>
                  Do_Sync := To_Remote;
            end case;

            if Do_Sync then
               if File /= No_File then
                  if M_Points (J).Local_Root.Is_Parent (File) then
                     Src := To_Unbounded_String
                       (+File.Unix_Style_Full_Name (True));
                     Dst := To_Unbounded_String
                       (+File.To_Remote
                          (Host_Name).Unix_Style_Full_Name (True));
                  else
                     Do_Sync := False;
                  end if;
               else
                  Src := To_Unbounded_String
                    (+M_Points (J).Local_Root.Unix_Style_Full_Name (True));
                  Dst := To_Unbounded_String
                    (+M_Points (J).Remote_Root.Unix_Style_Full_Name (True));
               end if;
            end if;

            if Do_Sync then
               if Machine.User_Name /= "" then
                  Remote_Path := new String'
                    (Machine.User_Name & "@" &
                     Machine.Network_Name & ":" &
                     To_String (Dst));

               else
                  Remote_Path := new String'
                    (Machine.Network_Name & ":" &
                     To_String (Dst));
               end if;

               if To_Remote then
                  Src_Path := new String'(To_String (Src));
                  Dest_Path := Remote_Path;
               else
                  Src_Path := Remote_Path;
                  Dest_Path := new String'(To_String (Src));
               end if;

               Rsync_Module.Ret_Data.Status := 0;

               Data := new Rsync_Callback_Data'
                 (External_Process_Data with
                  Network_Name      => new String'(Machine.Network_Name),
                  User_Name         => new String'(Machine.User_Name),
                  Nb_Password_Tries => 0,
                  Synchronous       => Synchronous,
                  Dialog            => null,
                  Dialog_Running    => False,
                  Ret_Data          => Rsync_Module.Ret_Data,
                  Buffer            => null);

               if Synchronous then
                  --  We create the dialog that will be updated as rsync runs.
                  if To_Remote then
                     Gtk_New
                       (Data.Dialog, Kernel,
                        M_Points (J).Local_Root, M_Points (J).Remote_Root);
                  else
                     Gtk_New
                       (Data.Dialog, Kernel,
                        M_Points (J).Remote_Root, M_Points (J).Local_Root);
                  end if;

                  Gtkada.Handlers.Widget_Callback.Object_Connect
                    (Data.Dialog.Abort_Button, Signal_Clicked,
                     On_Abort_Clicked'Access,
                     Data.Dialog);
                  Data.Dialog.Grab_Add;
               end if;

               Real_Print_Output := not Synchronous and then Print_Output;

               if Real_Print_Output then
                  Raise_Console (Kernel);
               end if;

               --  Do not set Line_By_Line as this will prevent the password
               --  prompt catch.
               --  Do not strip CR's as file progression is output by rsync
               --  using CR characters.
               --  Do not use pipes in order to be able to retrieve password
               --  prompts on Windows
               declare
                  Arguments : Argument_List := Build_Arg;
                  CL        : Arg_List;
                  Scheduled : Scheduled_Command_Access;
               begin
                  CL := Create ("rsync");
                  for A in Arguments'Range loop
                     Append_Argument (CL, Arguments (A).all, One_Arg);
                  end loop;

                  Launch_Process
                    (Scheduled         => Scheduled,
                     Success           => Success,
                     Kernel            => Kernel,
                     CL                => CL,
                     Console           => Get_Console (Kernel),
                     Show_Command      => Print_Cmd,
                     Show_Output       => Real_Print_Output,
                     Line_By_Line      => False,
                     Data              => Data,
                     Queue_Id          => Queue_Id,
                     Synchronous       => Synchronous,
                     Timeout           => Machine.Timeout,
                     Strip_CR          => False,
                     Use_Pipes         => False);

                  All_Success := All_Success and Success;

                  for J in Arguments'Range loop
                     Free (Arguments (J));
                  end loop;
               end;
            end if;
         end loop;
      end;

      return Rsync_Module.Ret_Data.Status = 0 and then All_Success;
   end Execute;

   ----------------------
   -- On_Abort_Clicked --
   ----------------------

   procedure On_Abort_Clicked
     (Dialog : access Gtk_Widget_Record'Class) is
   begin
      Rsync_Dialog (Dialog).Aborted := True;
   end On_Abort_Clicked;

   ---------
   -- Cat --
   ---------

   function Cat (S1 : GNAT.OS_Lib.String_Access; S2 : String) return String is
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

   function Cat (S1 : String; S2 : GNAT.OS_Lib.String_Access) return String is
   begin
      if S2 = null then
         return S1;
      else
         return S1 & S2.all;
      end if;
   end Cat;

   ---------------
   -- On_Output --
   ---------------

   overriding procedure On_Output
     (Self    : not null access Rsync_Callback_Data;
      Command : not null access Root_Command'Class;
      Output  : String)
   is
      Matched              : Match_Array (0 .. 2);
      Last_Matched         : Match_Array (0 .. 2);
      File_Nb              : Natural;
      Total_Files          : Natural;
      Force                : Boolean;
      Old_Buff             : GNAT.Strings.String_Access;
      LF_Index             : Integer;
      Last                 : Natural;

   begin
      Old_Buff := Self.Buffer;
      Self.Buffer := null;

      LF_Index := Output'First - 1;

      for J in reverse Output'Range loop
         if Output (J) = ASCII.LF
           or else Output (J) = ASCII.CR
         then
            if J /= Output'Last then
               Self.Buffer := new String'(Output (J + 1 .. Output'Last));
            end if;

            LF_Index := J;
            exit;
         end if;
      end loop;

      if LF_Index = Output'First - 1 then
         --  no LF found for now. Place everything in buffer.
         Self.Buffer := new String'(Cat (Old_Buff, Output));
         Free (Old_Buff);
      end if;

      if Self.Dialog /= null and then Self.Dialog.Aborted then
         Interrupt (Self.Descriptor.all);

      elsif not Self.Process_Died then

         declare
            Stripped_Buffer : constant String :=
                                Cat (Old_Buff,
                                     Output (Output'First .. LF_Index));
            Buffer          : constant String :=
                                Cat (Stripped_Buffer, Self.Buffer);
         begin
            Trace (Me, "Parse_Rsync_Output: " & ASCII.LF & Buffer);

            --  Retrieve password prompt if any
            Match (Get_Default_Password_Regexp,
                   Buffer,
                   Matched);

            if Matched (0) /= No_Match then
               Force := Self.Nb_Password_Tries > 0;
               Self.Nb_Password_Tries := Self.Nb_Password_Tries + 1;

               declare
                  Password : constant String :=
                               Get_Password (Self.Network_Name.all,
                                             Self.User_Name.all,
                                             Force);
               begin
                  if Password = "" then
                     Interrupt (Self.Descriptor.all);
                  else
                     Send (Self.Descriptor.all, Password);
                  end if;
               end;

               --  Do not preserve the password prompt
               GNAT.Strings.Free (Self.Buffer);
               return;
            end if;

            --  Retrieve passphrase prompt if any
            Match (Get_Default_Passphrase_Regexp,
                   Buffer,
                   Matched);

            if Matched (0) /= No_Match then
               Force := Self.Nb_Password_Tries > 0;
               Self.Nb_Password_Tries := Self.Nb_Password_Tries + 1;

               declare
                  Password : constant String :=
                               Get_Passphrase
                                 (Buffer
                                    (Matched (1).First .. Matched (1).Last),
                                  Force);
               begin
                  if Password = "" then
                     Interrupt (Self.Descriptor.all);
                  else
                     Send (Self.Descriptor.all, Password);
                  end if;
               end;

               --  Do not preserve the password prompt
               GNAT.Strings.Free (Self.Buffer);
               return;
            end if;

            --  Retrieve progression
            Last_Matched (0) := No_Match;
            Last := Stripped_Buffer'First;

            loop
               Match (Progress_Regexp,
                      Stripped_Buffer (Last .. Stripped_Buffer'Last),
                      Matched);

               if Matched (0) = No_Match then
                  --  Set back last match
                  Matched := Last_Matched;
                  exit;
               end if;

               Last := Matched (0).Last + 1;
               Last_Matched := Matched;
            end loop;

            if Matched (0) /= No_Match then
               Self.Dialog_Running := True;
               File_Nb := Natural'Value
                 (Stripped_Buffer (Matched (1).First .. Matched (1).Last));
               Total_Files := Natural'Value
                 (Stripped_Buffer (Matched (2).First .. Matched (2).Last));

               if Self.Synchronous then
                  Set_Fraction (Self.Dialog.Progress,
                                Gdouble (File_Nb) / Gdouble (Total_Files));
                  Set_Text (Self.Dialog.Progress,
                            Natural'Image (File_Nb) & "/" &
                            Natural'Image (Total_Files));

               else
                  Command.Set_Progress
                    (Progress => (Activity => Running,
                                  Current  => File_Nb,
                                  Total    => Total_Files));
               end if;
            end if;

            if Self.Synchronous then
               if not Self.Dialog_Running then
                  --  Get number of files to consider
                  Pulse (Self.Dialog.Progress);
                  Last := Stripped_Buffer'First;

                  loop
                     Match (Files_Considered_Regexp,
                            Stripped_Buffer (Last .. Stripped_Buffer'Last),
                            Matched);

                     if Matched (0) = No_Match then
                        --  Set back last match
                        Matched := Last_Matched;
                        exit;
                     end if;

                     Last := Matched (0).Last + 1;
                     Last_Matched := Matched;
                  end loop;

                  if Matched (0) /= No_Match then
                     Set_Fraction (Self.Dialog.Progress, 0.0);
                     Set_Text
                       (Self.Dialog.File_Progress,
                        -"Files to consider: " &
                        Stripped_Buffer
                          (Matched (0).First .. Matched (0).Last));
                  end if;

               else

                  --  Get file transfered
                  Last_Matched (0) := No_Match;
                  Last := Stripped_Buffer'First;

                  loop
                     Match (File_Regexp,
                            Stripped_Buffer (Last .. Stripped_Buffer'Last),
                            Matched);

                     if Matched (0) = No_Match then
                        --  Set back last match
                        Matched := Last_Matched;
                        exit;
                     end if;

                     Last := Matched (0).Last;
                     Last_Matched := Matched;
                  end loop;

                  if Matched (0) /= No_Match then
                     Set_Text (Self.Dialog.File_Progress,
                               Stripped_Buffer
                                 (Matched (1).First .. Matched (1).Last));
                  end if;

                  --  Get file transfered progression
                  Match (File_Progress_Regexp, Stripped_Buffer, Matched);

                  if Matched (0) /= No_Match then
                     declare
                        Percent : constant Natural := Natural'Value
                          (Stripped_Buffer
                             (Matched (1).First .. Matched (1).Last));
                     begin
                        Set_Fraction
                          (Self.Dialog.File_Progress,
                           Gdouble (Percent) / 100.0);
                     end;
                  end if;
               end if;
            end if;
         end;
      end if;

      Free (Old_Buff);
   end On_Output;

   -------------
   -- On_Exit --
   -------------

   overriding procedure On_Exit
     (Self    : not null access Rsync_Callback_Data;
      Command : not null access Root_Command'Class)
   is
      pragma Unreferenced (Command);
   begin
      Trace (Me, "Rsync_Terminated");

      if Self.Synchronous and then Self.Dialog /= null then
         Hide (Self.Dialog);
         Self.Dialog.Grab_Remove;
         Self.Dialog := null;
      end if;

      Self.Ret_Data.Status := Self.Exit_Status;
      Trace (Me, "rsync status is" & Integer'Image (Self.Exit_Status));

      Rsync_Finished_Hook.Run (Self.Kernel);
   end On_Exit;

end Remote.Rsync;
