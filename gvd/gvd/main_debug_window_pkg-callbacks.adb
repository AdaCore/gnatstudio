-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Widget; use Gtk.Widget;
with Gtk.List_Item;       use Gtk.List_Item;
with Gtk.Main;            use Gtk.Main;
with Gtk.List;            use Gtk.List;
with Gtk.Notebook;        use Gtk.Notebook;
with Odd_Preferences_Pkg; use Odd_Preferences_Pkg;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Odd_Intl;            use Odd_Intl;
with Odd.Process;         use Odd.Process;
with Odd.Dialogs.Callbacks; use Odd.Dialogs.Callbacks;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Glib;                use Glib;
with Debugger;            use Debugger;
with Process_Proxies;     use Process_Proxies;
with Language;            use Language;
with Breakpoints_Pkg;     use Breakpoints_Pkg;
with Odd.Process;         use Odd.Process;
with GNAT.Expect;         use GNAT.Expect;
with Ada.Text_IO;         use Ada.Text_IO;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Odd.Types;           use Odd.Types;

package body Main_Debug_Window_Pkg.Callbacks is

   use Odd;
   use Gtk.Arguments;

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access);
   --  Close all the debuggers associated with a given main debug window
   --  by looking at all the pages of the main notebook.

   -----------------------
   -- Cleanup_Debuggers --
   -----------------------

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access) is
      Tab      : Debugger_Process_Tab;
      Page_Num : Gint := 0;
      Page     : Gtk_Widget;

   begin
      Free (Top.Command_History);
      loop
         Page := Get_Nth_Page (Top.Process_Notebook, Page_Num);

         exit when Page = null;

         Page_Num := Page_Num + 1;
         Tab := Process_User_Data.Get (Page);
         Close (Tab.Debugger);
      end loop;
   end Cleanup_Debuggers;

   ---------------------------------------
   -- On_Main_Debug_Window_Delete_Event --
   ---------------------------------------

   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      --  Ref the object since we will destroy it in the main procedure.

      Ref (Object);
      Cleanup_Debuggers (Main_Debug_Window_Access (Object));
      Main_Quit;
      return False;
   end On_Main_Debug_Window_Delete_Event;

   ----------------------
   -- Session Handling --
   ----------------------

   --  The format for session files is as follows:
   --
   --  [Session_File Header]
   --  <number_of_processes>
   --  ---------------------
   --      <program_file_name_1>
   --      <debugger_type_1>
   --      <remote_host_1>
   --      <remote_target_1>
   --      <protocol_1>
   --      <debugger_name_1>
   --  ---------------------
   --      <program_file_name_2>
   --      <debugger_type_2>
   --      <remote_host_2>
   --      <remote_target_2>
   --      <protocol_2>
   --      <debugger_name_2>
   --  (etc)
   --  [History]
   --    <debugger_number>
   --    <command_type>
   --    <command>
   --    <debugger_number>
   --    <command_type>
   --    <command>
   --  (etc)
   --  ---------------------

   -------------------------------
   -- On_Open_Program1_Activate --
   -------------------------------

   procedure On_Open_Program1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      S   : constant String := File_Selection_Dialog;
      Tab : Debugger_Process_Tab;

   begin
      if S /= "" then
         Tab := Get_Current_Process (Object);
         Set_Executable (Tab.Debugger, S, Mode => Hidden);
         Free (Tab.Descriptor.Program);
         Tab.Descriptor.Program := new String' (S);
      end if;
   end On_Open_Program1_Activate;

   --------------------------------
   -- On_Open_Debugger1_Activate --
   --------------------------------

   procedure On_Open_Debugger1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Program : Program_Descriptor;
      List    : Argument_List (1 .. 0);
      Process : Debugger_Process_Tab;
      Top     : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);

   begin
      Open_Program (Top.Open_Program, Program);

      if Program.Launch = New_Debugger then
         Process :=
           Create_Debugger
             (Main_Debug_Window_Access (Object),
              Program.Debugger,
              Program.Program.all,
              List,
              Program.Remote_Host.all,
              Program.Remote_Target.all,
              Program.Protocol.all);
      end if;
   end On_Open_Debugger1_Activate;

   ---------------------------------
   -- On_Open_Core_Dump1_Activate --
   ---------------------------------

   procedure On_Open_Core_Dump1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Open_Core_Dump1_Activate;

   -------------------------------
   -- On_Open_Session1_Activate --
   -------------------------------

   procedure On_Open_Session1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      File         : File_Type;
      S            : Types.String_Access;
      Program      : Program_Descriptor;
      Buffer       : String (1 .. 256);
      Last         : Natural;
      Tab          : Debugger_Process_Tab := Get_Current_Process (Object);
      List         : Argument_List (1 .. 0);
      Process      : Debugger_Process_Tab;
      Top          : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Num_Pages    : Gint;
      Page         : Gtk_Widget;
      Page_Num     : Gint;
      Mode         : Command_Type;

   begin
      Open_Session (Top.Open_Session, Top.Sessions_Dir.all, S);

      if S /= null
        and then S.all /= ""
      then
         Open (File, In_File, Top.Sessions_Dir.all
               & Directory_Separator & S.all);

         Get_Line (File, Buffer, Last);

         if Buffer (1 .. Last) /= "[Session_File Header]" then
            Close (File);
            return;
         end if;

         --  Remove all the pages in the notebook before opening session.

         Page_Num :=
           Gint (Page_List.Length (Get_Children (Top.Process_Notebook)));
         Free (Top.Command_History);
         while Page_Num /= -1 loop
            Page := Get_Nth_Page
              (Top.Process_Notebook, Page_Num);

            if Page /= null then
               Tab := Process_User_Data.Get (Page);
               Close (Tab.Debugger);
            end if;

            Remove_Page (Top.Process_Notebook, Page_Num);
            Page_Num := Page_Num - 1;
         end loop;

         --  Get the number of processes.

         Get_Line (File, Buffer, Last);
         Num_Pages := Gint'Value (Buffer (1 ..  Last));
         Get_Line (File, Buffer, Last);

         --  Read the descriptors and create the debuggers.

         for J in 1 .. Num_Pages loop
            Get_Line (File, Buffer, Last);
            Program.Program := new String' (Buffer (1 .. Last));
            Get_Line (File, Buffer, Last);
            Program.Debugger := Debugger_Type'Value (Buffer (1 .. Last));
            Get_Line (File, Buffer, Last);
            Program.Remote_Host := new String' (Buffer (1 .. Last));
            Get_Line (File, Buffer, Last);
            Program.Remote_Target := new String' (Buffer (1 .. Last));
            Get_Line (File, Buffer, Last);
            Program.Protocol := new String' (Buffer (1 .. Last));
            Get_Line (File, Buffer, Last);
            Program.Debugger_Name := new String' (Buffer (1 .. Last));
            Get_Line (File, Buffer, Last);

            Process :=
              Create_Debugger
                (Main_Debug_Window_Access (Object),
                 Program.Debugger,
                 Program.Program.all,
                 List,
                 Program.Remote_Host.all,
                 Program.Remote_Target.all,
                 Program.Protocol.all);

            Tab := Get_Current_Process (Object);
         end loop;

         --  Read and compute the commands history.

         Get_Line (File, Buffer, Last);

         loop
            Get_Line (File, Buffer, Last);
            exit when Last > 4 and then Buffer (1 .. 4) = "----";
            Page := Get_Nth_Page
              (Top.Process_Notebook, Gint'Value (Buffer (1 .. Last)));
            Tab := Process_User_Data.Get (Page);

            Get_Line (File, Buffer, Last);
            Mode := Command_Type'Value (Buffer (1 .. Last));

            Get_Line (File, Buffer, Last);

            Set_Busy_Cursor (Tab, True);

            if Mode = Hidden then

               Send (Tab.Debugger,
                     Buffer (1 .. Last),
                     Wait_For_Prompt => True,
                     Mode => Hidden);

            elsif Mode = User then
               Process_User_Command (Tab, Buffer (1 .. Last));
            end if;

            Set_Busy_Cursor (Tab, False);
         end loop;
         Close (File);
      end if;

      if S /= null then
         Free (S);
      end if;

   exception
      when Name_Error =>
         null;
      when Device_Error =>
         null;
   end On_Open_Session1_Activate;

   ----------------------------------
   -- On_Save_Session_As1_Activate --
   ----------------------------------

   procedure On_Save_Session_As1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      File        : File_Type;
      S           : Types.String_Access;
      Page        : Gtk_Widget;
      Top         : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Num_Pages   : Gint;
      Tab         : Debugger_Process_Tab;
      Hist_Length : Integer;
   begin

      Num_Pages := Gint
        (Page_List.Length (Get_Children (Top.Process_Notebook)));

      Open_Session (Top.Open_Session, Top.Sessions_Dir.all, S);

      if S /= null
        and then S.all /= ""
      then
         Create (File, Out_File,
                 Top.Sessions_Dir.all
                 & Directory_Separator & S.all);
         Put_Line (File, "[Session_File Header]");
         Put_Line (File, Gint'Image (Num_Pages));
         Put_Line (File, "---------------------");

         for Page_Num in 0 ..
           Gint ((Page_List.Length (Get_Children (Top.Process_Notebook))) - 1)
         loop
            Page := Get_Nth_Page
              (Top.Process_Notebook, Page_Num);

            if Page /= null then
               Tab := Process_User_Data.Get (Page);
               Hist_Length := Length (Top.Command_History);
               Put_Line (File, Tab.Descriptor.Program.all);
               Put_Line (File, Debugger_Type'Image (Tab.Descriptor.Debugger));
               Put_Line (File, Tab.Descriptor.Remote_Host.all);
               Put_Line (File, Tab.Descriptor.Remote_Target.all);
               Put_Line (File, Tab.Descriptor.Protocol.all);
               Put_Line (File, Tab.Descriptor.Debugger_Name.all);
               Put_Line (File, "------------------------");
            end if;

         end loop;
         Put_Line (File, "[History]");
         Wind (Top.Command_History, Backward);

         for J in reverse 1 .. Length (Top.Command_History) loop
            for Count in 1 ..
              Get_Current_Repeat_Num (Top.Command_History)
            loop
               declare Data : History_Data :=
                 Get_Current (Top.Command_History);
               begin
                  Put_Line (File,
                            Natural'Image (Data.Debugger_Num));

                  Put_Line (File,
                            Command_Type'Image (Data.Mode));

                  Put_Line (File, Data.Command.all);
               end;
            end loop;
            if J /= 1 then
               Move_To_Next (Top.Command_History);
            end if;
         end loop;
         Put_Line (File, "---------------------");
         Close (File);
      end if;

      if S /= null then
         Free (S);
      end if;

   exception
      when No_Such_Item =>
         Put_Line (File, "---------------------");
         Close (File);
      when Use_Error =>
         null;
   end On_Save_Session_As1_Activate;

   ------------------------------------
   -- On_Attach_To_Process1_Activate --
   ------------------------------------

   procedure On_Attach_To_Process1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Attach_To_Process1_Activate;

   ---------------------------------
   -- On_Detach_Process1_Activate --
   ---------------------------------

   procedure On_Detach_Process1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Detach_Process1_Activate;

   -----------------------------------
   -- On_Change_Directory1_Activate --
   -----------------------------------

   procedure On_Change_Directory1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Change_Directory1_Activate;

   --------------------------
   -- On_Restart1_Activate --
   --------------------------

   procedure On_Restart1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Restart1_Activate;

   -----------------------
   -- On_Exit1_Activate --
   -----------------------

   procedure On_Exit1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Cleanup_Debuggers (Main_Debug_Window_Access (Object));
      Main_Quit;
   end On_Exit1_Activate;

   -----------------------
   -- On_Undo3_Activate --
   -----------------------

   procedure On_Undo3_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Undo3_Activate;

   -----------------------
   -- On_Redo1_Activate --
   -----------------------

   procedure On_Redo1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Redo1_Activate;

   ----------------------
   -- On_Cut1_Activate --
   ----------------------

   procedure On_Cut1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Cut1_Activate;

   -----------------------
   -- On_Copy1_Activate --
   -----------------------

   procedure On_Copy1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Copy1_Activate;

   ------------------------
   -- On_Paste1_Activate --
   ------------------------

   procedure On_Paste1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Paste1_Activate;

   ------------------------
   -- On_Clear1_Activate --
   ------------------------

   procedure On_Clear1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Clear1_Activate;

   -------------------------
   -- On_Delete1_Activate --
   -------------------------

   procedure On_Delete1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Delete1_Activate;

   -----------------------------
   -- On_Select_All1_Activate --
   -----------------------------

   procedure On_Select_All1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Select_All1_Activate;

   ------------------------------
   -- On_Edit_Source1_Activate --
   ------------------------------

   procedure On_Edit_Source1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Edit_Source1_Activate;

   --------------------------------
   -- On_Reload_Source1_Activate --
   --------------------------------

   procedure On_Reload_Source1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Reload_Source1_Activate;

   ------------------------------
   -- On_Preferences1_Activate --
   ------------------------------

   procedure On_Preferences1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
   begin
      if Top.Odd_Preferences = null then
         Gtk_New (Top.Odd_Preferences);
      end if;

      Show_All (Top.Odd_Preferences);
   end On_Preferences1_Activate;

   -------------------------------
   -- On_Gdb_Settings1_Activate --
   -------------------------------

   procedure On_Gdb_Settings1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Gdb_Settings1_Activate;

   --------------------------
   -- On_Lookup_1_Activate --
   --------------------------

   procedure On_Lookup_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Lookup_1_Activate;

   ------------------------
   -- On_Find_1_Activate --
   ------------------------

   procedure On_Find_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_1_Activate;

   ------------------------
   -- On_Find_2_Activate --
   ------------------------

   procedure On_Find_2_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_2_Activate;

   ----------------------------------
   -- On_Find_Words_Only1_Activate --
   ----------------------------------

   procedure On_Find_Words_Only1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Words_Only1_Activate;

   --------------------------------------
   -- On_Find_Case_Sensitive1_Activate --
   --------------------------------------

   procedure On_Find_Case_Sensitive1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Case_Sensitive1_Activate;

   -----------------------------------
   -- On_Execution_Window1_Activate --
   -----------------------------------

   procedure On_Execution_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Execution_Window1_Activate;

   ------------------------------
   -- On_Gdb_Console1_Activate --
   ------------------------------

   procedure On_Gdb_Console1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Gdb_Console1_Activate;

   --------------------------------
   -- On_Source_Window1_Activate --
   --------------------------------

   procedure On_Source_Window1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Source_Window1_Activate;

   ------------------------------
   -- On_Data_Window1_Activate --
   ------------------------------

   procedure On_Data_Window1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Data_Window1_Activate;

   --------------------------------------
   -- On_Machine_Code_Window1_Activate --
   --------------------------------------

   procedure On_Machine_Code_Window1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Machine_Code_Window1_Activate;

   ----------------------
   -- On_Run1_Activate --
   ----------------------

   procedure On_Run1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Command_In_Process (Get_Process (Tab.Debugger)) then
         return;
      end if;

      declare
         Arguments : constant String := Simple_Entry_Dialog
           (Parent  => Tab.Window,
            Title   => -"Arguments Selection",
            Message => -"Enter the arguments to your application:",
            Key     => -"odd_run_arguments");
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.Nul
         then
            Set_Busy_Cursor (Tab, True);
            Run (Tab.Debugger, Arguments, Mode => User);
            Set_Busy_Cursor (Tab, False);
         end if;
      end;
   end On_Run1_Activate;

   ----------------------------
   -- On_Run_Again1_Activate --
   ----------------------------

   procedure On_Run_Again1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Run_Again1_Activate;

   ------------------------
   -- On_Start1_Activate --
   ------------------------

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Command_In_Process (Get_Process (Tab.Debugger)) then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Start (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Start1_Activate;

   ------------------------------------------
   -- On_Run_In_Execution_Window1_Activate --
   ------------------------------------------

   procedure On_Run_In_Execution_Window1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Run_In_Execution_Window1_Activate;

   -----------------------
   -- On_Step1_Activate --
   -----------------------

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Step_Into (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Step1_Activate;

   -----------------------------------
   -- On_Step_Instruction1_Activate --
   -----------------------------------

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Step_Into_Instruction (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Step_Instruction1_Activate;

   -----------------------
   -- On_Next1_Activate --
   -----------------------

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Step_Over (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Next1_Activate;

   -----------------------------------
   -- On_Next_Instruction1_Activate --
   -----------------------------------

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Step_Over_Instruction (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Next_Instruction1_Activate;

   ------------------------
   -- On_Until1_Activate --
   ------------------------

   procedure On_Until1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      null;
      Set_Busy_Cursor (Tab, False);
   end On_Until1_Activate;

   -------------------------
   -- On_Finish1_Activate --
   -------------------------

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Finish (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Finish1_Activate;

   ---------------------------
   -- On_Continue1_Activate --
   ---------------------------

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Continue (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Continue1_Activate;

   ------------------------------------------
   -- On_Continue_Without_Signal1_Activate --
   ------------------------------------------

   procedure On_Continue_Without_Signal1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Continue_Without_Signal1_Activate;

   -----------------------
   -- On_Kill1_Activate --
   -----------------------

   procedure On_Kill1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Kill1_Activate;

   ----------------------------
   -- On_Interrupt1_Activate --
   ----------------------------

   procedure On_Interrupt1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Tmp : Boolean;

   begin
      --  Give some visual feedback to the user
      Text_Output_Handler (Tab, "<^C>" & ASCII.LF, Is_Command => True);

      --  Process the events so as to show the text.
      while Gtk.Main.Events_Pending loop
         Tmp := Gtk.Main.Main_Iteration;
      end loop;

      --  Empty all the buffers to avoid waiting for a long time that all
      --  the output is processed.
      Interrupt (Tab.Debugger);
      Flush (Get_Descriptor (Get_Process (Tab.Debugger)).all, Timeout => 500);

      --  Make sure a final prompt is displayed for the user.
      --  If we are already processing a command (such as "run"), that command
      --  is already waiting for the prompt, and we don't need to do it.
      --  Otherwise, we need to do it ourselves, so that the new prompt
      --  appears as well.
      Display_Prompt (Tab.Debugger,
         Wait_For_Prompt =>
           not Command_In_Process (Get_Process (Tab.Debugger)));
   end On_Interrupt1_Activate;

   ------------------------
   -- On_Abort1_Activate --
   ------------------------

   procedure On_Abort1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Abort1_Activate;

   ----------------------------------
   -- On_Command_History1_Activate --
   ----------------------------------

   procedure On_Command_History1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Command_History1_Activate;

   --------------------------------
   -- On_Find_Backward1_Activate --
   --------------------------------

   procedure On_Find_Backward1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Backward1_Activate;

   -------------------------------
   -- On_Find_Forward1_Activate --
   -------------------------------

   procedure On_Find_Forward1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Forward1_Activate;

   -----------------------------
   -- On_Clear_Line1_Activate --
   -----------------------------

   procedure On_Clear_Line1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Clear_Line1_Activate;

   -------------------------------
   -- On_Clear_Window1_Activate --
   -------------------------------

   procedure On_Clear_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Clear_Window1_Activate;

   ---------------------------------
   -- On_Define_Command1_Activate --
   ---------------------------------

   procedure On_Define_Command1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Define_Command1_Activate;

   -------------------------------
   -- On_Edit_Buttons1_Activate --
   -------------------------------

   procedure On_Edit_Buttons1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Edit_Buttons1_Activate;

   ----------------------------
   -- On_Backtrace1_Activate --
   ----------------------------

   procedure On_Backtrace1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top      : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Tab      : constant Debugger_Process_Tab := Get_Current_Process (Top);
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Internal : Boolean;

   begin
      Internal := Is_Internal_Command (Get_Process (Tab.Debugger));
      Push_Internal_Command_Status (Get_Process (Tab.Debugger), True);
      Backtrace (Tab.Debugger, Bt, Len);
      Pop_Internal_Command_Status (Get_Process (Tab.Debugger));

      if Top.Backtrace_Dialog = null then
         Gtk_New (Top.Backtrace_Dialog, Gtk_Window (Object), Bt (1 .. Len));
         Widget_Callback.Connect
           (Gtk_Widget (Tab), "context_changed",
            On_Backtrace_Process_Stopped'Access);
      else
         Update (Top.Backtrace_Dialog, Bt (1 .. Len));
      end if;

      Free (Bt (1 .. Len));
      Show_All (Top.Backtrace_Dialog);
   end On_Backtrace1_Activate;

   --------------------------
   -- On_Threads1_Activate --
   --------------------------

   procedure On_Threads1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top      : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Tab      : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Internal : Boolean;

   begin
      Internal := Is_Internal_Command (Get_Process (Tab.Debugger));
      Push_Internal_Command_Status (Get_Process (Tab.Debugger), True);

      declare
         Info : Thread_Information_Array := Info_Threads (Tab.Debugger);
      begin
         if Top.Task_Dialog = null then
            Gtk_New (Top.Task_Dialog, Gtk_Window (Object), Info);
            Widget_Callback.Connect
              (Gtk_Widget (Tab), "process_stopped",
               On_Task_Process_Stopped'Access);
         else
            Update (Top.Task_Dialog, Info);
         end if;

         Free (Info);
      end;

      Pop_Internal_Command_Status (Get_Process (Tab.Debugger));
      Show_All (Top.Task_Dialog);
   end On_Threads1_Activate;

   ----------------------------
   -- On_Processes1_Activate --
   ----------------------------

   procedure On_Processes1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Processes1_Activate;

   --------------------------
   -- On_Signals1_Activate --
   --------------------------

   procedure On_Signals1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Signals1_Activate;

   -----------------------------------
   -- On_Edit_Breakpoints1_Activate --
   -----------------------------------

   procedure On_Edit_Breakpoints1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
   begin
      Breakpoint_Editor
        (Breakpoints_Access (Top.Breakpoints_Editor),
         Get_Current_Process (Top));
   end On_Edit_Breakpoints1_Activate;

   --------------------------------
   -- On_Edit_Displays1_Activate --
   --------------------------------

   procedure On_Edit_Displays1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Edit_Displays1_Activate;

   ---------------------------------
   -- On_Examine_Memory1_Activate --
   ---------------------------------

   procedure On_Examine_Memory1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Examine_Memory1_Activate;

   ------------------------------------------
   -- On_Display_Local_Variables1_Activate --
   ------------------------------------------

   procedure On_Display_Local_Variables1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Process_User_Command
        (Process,
         "graph display `" & Info_Locals (Process.Debugger) & '`',
         Output_Command => True);
   end On_Display_Local_Variables1_Activate;

   ------------------------------------
   -- On_Display_Arguments1_Activate --
   ------------------------------------

   procedure On_Display_Arguments1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Process_User_Command
        (Process,
         "graph display `" & Info_Args (Process.Debugger) & '`',
         Output_Command => True);
   end On_Display_Arguments1_Activate;

   ------------------------------------
   -- On_Display_Registers1_Activate --
   ------------------------------------

   procedure On_Display_Registers1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Command_In_Process (Get_Process (Process.Debugger)) then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Registers (Process.Debugger) & '`',
         Output_Command => True);
   end On_Display_Registers1_Activate;

   -------------------------------------
   -- On_Display_Expression1_Activate --
   -------------------------------------

   procedure On_Display_Expression1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Command_In_Process (Get_Process (Tab.Debugger)) then
         return;
      end if;

      declare
         Is_Func : aliased Boolean;
         Expression : constant String := Display_Entry_Dialog
           (Parent  => Tab.Window,
            Title   => -"Expression Selection",
            Message => -"Enter an expression to display:",
            Key     => -"odd_display_expression_dialog",
            Is_Func => Is_Func'Access);
      begin
         if Expression /= ""
           and then Expression (Expression'First) /= ASCII.Nul
         then
            if Is_Func then
               Process_User_Command
                 (Tab,
                  "graph display `" & Expression & '`',
                  Output_Command => True);
            else
               Process_User_Command
                 (Tab,
                  "graph display " & Expression,
                  Output_Command => True);
            end if;
         end if;
      end;
   end On_Display_Expression1_Activate;

   --------------------------------------
   -- On_More_Status_Display1_Activate --
   --------------------------------------

   procedure On_More_Status_Display1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_More_Status_Display1_Activate;

   --------------------------
   -- On_Refresh1_Activate --
   --------------------------

   procedure On_Refresh1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Refresh1_Activate;

   ---------------------------
   -- On_Overview1_Activate --
   ---------------------------

   procedure On_Overview1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Overview1_Activate;

   --------------------------
   -- On_On_Item1_Activate --
   --------------------------

   procedure On_On_Item1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_On_Item1_Activate;

   ----------------------------
   -- On_On_Window1_Activate --
   ----------------------------

   procedure On_On_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_On_Window1_Activate;

   ----------------------------
   -- On_What_Now_1_Activate --
   ----------------------------

   procedure On_What_Now_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_What_Now_1_Activate;

   ---------------------------------
   -- On_Tip_Of_The_Day1_Activate --
   ---------------------------------

   procedure On_Tip_Of_The_Day1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Tip_Of_The_Day1_Activate;

   --------------------------------
   -- On_Odd_Reference1_Activate --
   --------------------------------

   procedure On_Odd_Reference1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_Reference1_Activate;

   ---------------------------
   -- On_Odd_News1_Activate --
   ---------------------------

   procedure On_Odd_News1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_News1_Activate;

   --------------------------------
   -- On_Gdb_Reference1_Activate --
   --------------------------------

   procedure On_Gdb_Reference1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Gdb_Reference1_Activate;

   ------------------------------
   -- On_Odd_License1_Activate --
   ------------------------------

   procedure On_Odd_License1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_License1_Activate;

   -------------------------------
   -- On_Odd_Www_Page1_Activate --
   -------------------------------

   procedure On_Odd_Www_Page1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_Www_Page1_Activate;

   ----------------------------
   -- On_About_Odd1_Activate --
   ----------------------------

   procedure On_About_Odd1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        ("GVD " & Version &
         (-(": The GNU Visual Debugger" & ASCII.LF & ASCII.LF &
            "by Emmanuel Briot & Arnaud Charlet")),
         Help_Msg =>
           -("This is the About information box." & ASCII.LF & ASCII.LF &
             "Click on the OK button to close this window."),
         Title => -"About...");
   end On_About_Odd1_Activate;

   ------------------------
   -- On_Print1_Activate --
   ------------------------

   procedure On_Print1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Top       : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Process   : constant Debugger_Process_Tab := Get_Current_Process (Top);
      Selection : constant String := Get_Chars (Top.Toolbar_Entry);
      Label     : Gtk_List_Item;

   begin
      if Selection'Length /= 0 then
         Gtk_New (Label, Selection);
         Show (Label);
         Add (Get_List (Top.Toolbar_Combo), Label);
         Process_User_Command
           (Process, "graph print " & Selection, Output_Command => True);
      end if;
   end On_Print1_Activate;

   --------------------------
   -- On_Display1_Activate --
   --------------------------

   procedure On_Display1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Top       : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Process   : constant Debugger_Process_Tab := Get_Current_Process (Top);
      Selection : constant String := Get_Chars (Top.Toolbar_Entry);
      Label     : Gtk_List_Item;

   begin
      if Selection'Length /= 0 then
         Gtk_New (Label, Selection);
         Show (Label);
         Add (Get_List (Top.Toolbar_Combo), Label);
         Process_User_Command
           (Process, "graph display " & Selection, Output_Command => True);
      end if;
   end On_Display1_Activate;

   ---------------------
   -- On_Up1_Activate --
   ---------------------

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Stack_Up (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Up1_Activate;

   -----------------------
   -- On_Down1_Activate --
   -----------------------

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Set_Busy_Cursor (Tab, True);
      Stack_Down (Tab.Debugger, Mode => User);
      Set_Busy_Cursor (Tab, False);
   end On_Down1_Activate;

end Main_Debug_Window_Pkg.Callbacks;
