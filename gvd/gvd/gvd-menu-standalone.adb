-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                    use Glib;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Main;                use Gtk.Main;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;   use Gtkada.File_Selection;

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Odd_Intl;                use Odd_Intl;
with GVD;                     use GVD;
with GVD.Process;             use GVD.Process;
with GVD.Process.Standalone;  use GVD.Process.Standalone;
with GVD.Trace;               use GVD.Trace;
with GVD.Types;               use GVD.Types;
with String_Utils;            use String_Utils;
with GVD.Code_Editors;        use GVD.Code_Editors;
with GVD.Files;               use GVD.Files;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Window_Settings;     use GVD.Window_Settings;
with GVD.Main_Window;         use GVD.Main_Window;
with GVD.Preferences_Dialog;  use GVD.Preferences_Dialog;
with GVD.Open_Program_Dialog; use GVD.Open_Program_Dialog;
with GVD.Session_Dialog;      use GVD.Session_Dialog;

package body GVD.Menu.Standalone is

   use GVD;

   procedure Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   function Idle_Exit (Window : GVD_Main_Window) return Boolean;
   --  Idle function called to finish handling of exiting.

   ----------------------
   -- On_Open_Debugger --
   ----------------------

   procedure On_Open_Debugger
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Program : Program_Descriptor;
      List    : Argument_List (1 .. 0);
      Process : Debugger_Process_Tab;
      Top     : constant GVD_Main_Window := GVD_Main_Window (Object);
      Tab     : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Max_Args : constant := 4;
      Args     : Argument_List (1 .. Max_Args);
      Num_Args : Natural := 0;
      Pid      : GNAT.OS_Lib.Process_Id;
      Prog     : GNAT.OS_Lib.String_Access;

   begin
      Open_Program (Top.Open_Program, Program);

      case Program.Launch is
         when None =>
            return;

         when Current_Debugger =>
            if Tab /= null then
               Close_Debugger (Tab);
            end if;

         when New_Debugger =>
            --  If async commands aren't supported, the only reliable
            --  way to get a new debugger is to launch a new gvd.

            if not GVD.Async_Commands then
               Output_Info (Top, "launching another instance of gvd...");

               Prog := Locate_Exec_On_Path ("gvd");

               if Prog /= null then
                  Num_Args := Num_Args + 1;
                  Args (Num_Args) := new String' (Program.Program.all);

                  if Program.Remote_Host.all /= "" then
                     Num_Args := Num_Args + 1;
                     Args (Num_Args) :=
                       new String' ("--host=" & Program.Remote_Host.all);
                  end if;

                  if Program.Remote_Target.all /= "" then
                     Num_Args := Num_Args + 1;
                     Args (Num_Args) :=
                       new String' ("--target=" & Program.Remote_Target.all &
                                    ":" & Program.Protocol.all);
                  end if;

                  if Program.Debugger_Name.all /= "" then
                     Num_Args := Num_Args + 1;
                     Args (Num_Args) :=
                       new String' ("--debugger=" & Program.Debugger_Name.all);
                  end if;

                  Pid := GNAT.OS_Lib.Non_Blocking_Spawn
                    (Prog.all, Args (1 .. Num_Args));
                  Free (Prog);
               end if;

               for J in Args'Range loop
                  Free (Args (J));
               end loop;

               return;
            end if;
      end case;

      Process :=
        Create_Debugger
          (Top,
           Program.Debugger,
           Program.Program.all,
           List, "",
           Program.Remote_Host.all,
           Program.Remote_Target.all,
           Program.Protocol.all,
           Program.Debugger_Name.all);
   end On_Open_Debugger;

   --------------------
   -- On_Edit_Source --
   --------------------

   procedure On_Edit_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      function Substitute
        (Name : String; File : String; Line : Natural) return String;
      --  Substitute %f and %l in Name by the file name and the line number.

      ----------------
      -- Substitute --
      ----------------

      function Substitute
        (Name : String; File : String; Line : Natural) return String
      is
         Index : Natural := Name'First;
      begin
         while Index < Name'Last loop
            if Name (Index) = '%' and then Name (Index + 1) = 'f' then
               return Name (Name'First .. Index - 1) &
                 File & Substitute (Name (Index + 2 .. Name'Last), File, Line);

            elsif Name (Index) = '%' and then Name (Index + 1) = 'l' then
               declare
                  Img : constant String := Natural'Image (Line);
               begin
                  return Name (Name'First .. Index - 1) &
                    Img (Img'First + 1 .. Img'Last) &
                    Substitute (Name (Index + 2 .. Name'Last), File, Line);
               end;
            end if;

            Index := Index + 1;
         end loop;

         return Name;
      end Substitute;

      Tab       : constant Debugger_Process_Tab :=
        Get_Current_Process (Object);
      Host_File : constant String :=
        To_Host_Pathname (Get_Current_File (Tab.Editor_Text));
      External_Editor : GNAT.OS_Lib.String_Access;
      Args   : Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;

   begin
      External_Editor := Getenv ("GVD_EDITOR");
      if External_Editor.all = "" then
         Free (External_Editor);
         External_Editor := new String' (Get_Pref (Default_External_Editor));
      end if;

      declare
         Editor : constant String := Substitute
           (External_Editor.all, Host_File, Get_Line (Tab.Editor_Text));
      begin
         Output_Info (GVD_Main_Window (Tab.Window), Editor);
         Args := Argument_String_To_List (Editor);
         Prog := Locate_Exec_On_Path (Args (Args'First).all);

         if Prog /= null then
            Pid := GNAT.OS_Lib.Non_Blocking_Spawn
              (Prog.all, Args (Args'First + 1 .. Args'Last));
            Free (Prog);
         end if;

         if Args /= null then
            for J in Args'Range loop
               Free (Args (J));
            end loop;

            Free (Args);
         end if;
      end;

      Free (External_Editor);
   end On_Edit_Source;

   --------------------
   -- On_Open_Source --
   --------------------

   procedure On_Open_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      File_Name : constant String :=
        File_Selection_Dialog (Title => -"Source name", Must_Exist => True);
      Tab       : constant Debugger_Process_Tab :=
        Get_Current_Process (Object);

   begin
      Load_File (Tab.Editor_Text, File_Name, Set_Current => False);
      Set_Line (Tab.Editor_Text, 1, Set_Current => False);
   end On_Open_Source;

   -----------------------
   -- On_Reload_Sources --
   -----------------------

   procedure On_Reload_Sources
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Top    : constant GVD_Main_Window := GVD_Main_Window (Object);
      Editor : constant Code_Editor :=
        Get_Current_Process (Object).Editor_Text;

   begin
      GVD.Files.Clear_Cache (Top, Force => True);
      Load_File (Editor, Get_Current_File (Editor), Force => True);
   end On_Reload_Sources;

   ---------------------
   -- On_Open_Session --
   ---------------------

   procedure On_Open_Session
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Top     : constant GVD_Main_Window := GVD_Main_Window (Object);
      Session : GVD_Session_Dialog;
   begin
      if Top.Open_Session = null then
         Open_Session (Top, Session, Top.Sessions_Dir.all);
         Top.Open_Session := Gtk_Window (Session);
      else
         Session := GVD_Session_Dialog (Top.Open_Session);
         Open_Session (Top, Session, Top.Sessions_Dir.all);
      end if;
   end On_Open_Session;

   ------------------------
   -- On_Save_Session_As --
   ------------------------

   procedure On_Save_Session_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Top     : constant GVD_Main_Window := GVD_Main_Window (Object);
      Session : GVD_Session_Dialog;
   begin
      if Top.Open_Session = null then
         Save_Session (Top, Session, Top.Sessions_Dir.all);
         Top.Open_Session := Gtk_Window (Session);
      else
         Session := GVD_Session_Dialog (Top.Open_Session);
         Save_Session (Top, Session, Top.Sessions_Dir.all);
      end if;
   end On_Save_Session_As;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Close_Debugger (Tab);
      end if;
   end On_Close;

   ---------------
   -- Idle_Exit --
   ---------------

   function Idle_Exit (Window : GVD_Main_Window) return Boolean is
   begin
      Cleanup_Debuggers (Window);
      Main_Quit;
      return False;
   end Idle_Exit;

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Id : Idle_Handler_Id;
   begin
      --  Ref the object since we will destroy it in the main procedure.

      Ref (Object);
      Save_Window_Settings
        (GVD_Main_Window (Object).Home_Dir.all
         & Directory_Separator & "window_settings",
         Gtk_Widget (Object));
      Prepare_Cleanup_Debuggers (GVD_Main_Window (Object));
      Id := Main_Window_Idle.Add (Idle_Exit'Access, GVD_Main_Window (Object));
   end On_Exit;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
   begin
      null;
   end On_Undo;

   -------------
   -- On_Redo --
   -------------

   procedure On_Redo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
   begin
      null;
   end On_Redo;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
   begin
      null;
   end On_Cut;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
   begin
      null;
   end On_Copy;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
   begin
      null;
   end On_Paste;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
   begin
      null;
   end On_Select_All;

   --------------------
   -- On_Preferences --
   --------------------

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
   begin
      if Top.GVD_Preferences = null then
         Gtk_New (Top.GVD_Preferences, Top);
      end if;

      --  First do a show_all, so that Fill_Dialog can choose to
      --  hide or deactivate widgets.
      Show_All (Top.GVD_Preferences);
      GVD.Preferences.Fill_Dialog (Top.GVD_Preferences);
   end On_Preferences;

   ---------------
   -- On_Manual --
   ---------------

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Browse : constant String :=
        Get_Pref (HTML_Browser) & " " &
          GVD_Main_Window (Object).Prefix_Directory.all &
          Directory_Separator & "doc" & Directory_Separator & "gvd" &
          Directory_Separator & "gvd.html";
      Args   : Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;

   begin
      Output_Info (GVD_Main_Window (Object), Browse);

      Args := Argument_String_To_List (Browse);
      Prog := Locate_Exec_On_Path (Args (Args'First).all);

      if Prog /= null then
         Pid := GNAT.OS_Lib.Non_Blocking_Spawn
           (Prog.all, Args (Args'First + 1 .. Args'Last));
         Free (Prog);
      end if;

      if Args /= null then
         for J in Args'Range loop
            Free (Args (J));
         end loop;

         Free (Args);
      end if;
   end On_Manual;

   ------------------
   -- On_About_GVD --
   ------------------

   procedure On_About_GVD
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);

      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        ("GVD " & Version & " (" & Source_Date & ")" &
         (-" hosted on ") & GVD.Target & ASCII.LF &
         (-"The GNU Visual Debugger") & ASCII.LF & ASCII.LF &
         (-"by Emmanuel Briot, Arnaud Charlet & Nicolas Setton") &
           ASCII.LF & ASCII.LF & "(c) 2000, 2001 ACT-Europe",
         Help_Msg =>
           (-"This is the About information box.") & ASCII.LF & ASCII.LF &
           (-"Click on the OK button to close this window."),
         Title => -"About...");
   end On_About_GVD;

   --------------------
   -- GVD_Menu_Items --
   --------------------

   function GVD_Menu_Items return Gtk_Item_Factory_Entry_Access is
   begin
      return new Gtk_Item_Factory_Entry_Array'
        (Gtk_New (-"/_File", Item_Type => Branch),
         Gtk_New (-"/_File/Open Program...", "F3", On_Open_Program'Access),
         Gtk_New (-"/_File/New Debugger...", "", On_Open_Debugger'Access),
         Gtk_New (-"/_File/Open Core Dump...", "", On_Open_Core_Dump'Access),
         Gtk_New (-"/_File/Add Symbols...", "", On_Add_Symbols'Access),
         Gtk_New (-"/_File/sep1", Item_Type => Separator),
         Gtk_New (-"/_File/Edit Current Source", "<control>E",
                  On_Edit_Source'Access),
         Gtk_New (-"/_File/Open Source...", "", On_Open_Source'Access),
         Gtk_New (-"/_File/Reload Sources", "", On_Reload_Sources'Access),
         Gtk_New (-"/_File/sep2", Item_Type => Separator),
         Gtk_New (-"/_File/Open Session...", "<control>N",
                  On_Open_Session'Access),
         Gtk_New (-"/_File/Save Session As...", "<control>S",
                  On_Save_Session_As'Access),
         Gtk_New (-"/_File/sep3", Item_Type => Separator),
         Gtk_New (-"/_File/Attach...", "",
                  On_Attach_To_Process'Access),
         Gtk_New (-"/_File/Detach", "", On_Detach_Process'Access),
         Gtk_New (-"/_File/sep4", Item_Type => Separator),
         Gtk_New (-"/_File/Change Directory...", "",
                  On_Change_Directory'Access),
         Gtk_New (-"/_File/sep5", Item_Type => Separator),
         Gtk_New (-"/_File/Close", "", On_Close'Access),
         Gtk_New (-"/_File/Exit", "<control>Q", On_Exit'Access),

         Gtk_New (-"/_Edit", Item_Type => Branch),
         Gtk_New (-"/_Edit/Undo", "", On_Undo'Access),
         Gtk_New (-"/_Edit/Redo", "", On_Redo'Access),
         Gtk_New (-"/_Edit/sep1", Item_Type => Separator),
         Gtk_New (-"/_Edit/Cut", "<shift>DEL", On_Cut'Access),
         Gtk_New (-"/_Edit/Copy", "<control>INS", On_Copy'Access),
         Gtk_New (-"/_Edit/Paste", "<shift>INS", On_Paste'Access),
         Gtk_New (-"/_Edit/Select All", "<control>A", On_Select_All'Access),
         Gtk_New (-"/_Edit/sep2", Item_Type => Separator),
         Gtk_New (-"/_Edit/Preferences...", "", On_Preferences'Access),

         Gtk_New (-"/_Program", Item_Type => Branch),
         Gtk_New (-"/_Program/Run-Start...", "F2", On_Run'Access),
         Gtk_New (-"/_Program/sep1", Item_Type => Separator),
         Gtk_New (-"/_Program/Step", "F5", On_Step'Access),
         Gtk_New (-"/_Program/Step Instruction", "<shift>F5",
                  On_Step_Instruction'Access),
         Gtk_New (-"/_Program/Next", "F6", On_Next'Access),
         Gtk_New (-"/_Program/Next Instruction", "<shift>F6",
                  On_Next_Instruction'Access),
         Gtk_New (-"/_Program/Finish", "F7", On_Finish'Access),
         Gtk_New (-"/_Program/sep2", Item_Type => Separator),
         Gtk_New (-"/_Program/Continue", "F8", On_Continue'Access),
         Gtk_New (-"/_Program/sep3", Item_Type => Separator),
         Gtk_New (-"/_Program/Kill", "", On_Kill'Access),
         Gtk_New (-"/_Program/Interrupt", "ESC", On_Interrupt'Access),

         Gtk_New (-"/_Command", Item_Type => Branch),
         Gtk_New (-"/_Command/Command History...", "",
                  On_Command_History'Access),
         Gtk_New (-"/_Command/Clear Window", "", On_Clear_Window'Access),

         Gtk_New (-"/_Data", Item_Type => Branch),
         Gtk_New (-"/_Data/Call Stack", "", On_Call_Stack'Access, Check_Item),
         Gtk_New (-"/_Data/Threads", "", On_Threads'Access),
         Gtk_New (-"/_Data/Tasks", "", On_Tasks'Access),
         Gtk_New (-"/_Data/sep1", Item_Type => Separator),
         Gtk_New (-"/_Data/Edit Breakpoints...", "",
                  On_Edit_Breakpoints'Access),
         Gtk_New (-"/_Data/Examine Memory...", "", On_Examine_Memory'Access),
         Gtk_New (-"/_Data/sep2", Item_Type => Separator),
         Gtk_New (-"/_Data/Display Local Variables", "<alt>L",
                  On_Display_Local_Variables'Access),
         Gtk_New (-"/_Data/Display Arguments", "<alt>U",
                  On_Display_Arguments'Access),
         Gtk_New (-"/_Data/Display Registers", "",
                  On_Display_Registers'Access),
         Gtk_New (-"/_Data/Display Any Expression...", "",
                  On_Display_Expression'Access),
         Gtk_New (-"/_Data/sep3", Item_Type => Separator),
         Gtk_New (-"/_Data/Refresh", "<control>L", On_Refresh'Access),
         Gtk_New (-"/_Data/Show", "", On_Show'Access),

         Gtk_New (-"/_Window"),

         Gtk_New (-"/_Help", Item_Type => Branch),
         Gtk_New (-"/_Help/GVD Manual...", "F1", On_Manual'Access),
         Gtk_New (-"/_Help/About GVD...", "", On_About_GVD'Access));
   end GVD_Menu_Items;

end GVD.Menu.Standalone;
