-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
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

with Ada.Exceptions;            use Ada.Exceptions;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.File_Selector;      use Gtkada.File_Selector;

with Config;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with Logo_Boxes;                use Logo_Boxes;
with Histories;                 use Histories;
with Creation_Wizard.Selector;  use Creation_Wizard.Selector;
with VFS;                       use VFS;
with Projects;                  use Projects;
with Traces;                    use Traces;

package body Welcome is

   function On_New_Project
     (Screen : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user wants to create a new widget
   --  Return True if a new project has been created

   procedure On_Default_Project (Screen : access Gtk_Widget_Record'Class);
   --  Create and load a default project

   procedure On_Browse_Default (Screen : access Gtk_Widget_Record'Class);
   --  Browse a new directory for the default project

   function On_Load_Project (Screen : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Load an existing project

   procedure On_Browse_Load (Screen : access Gtk_Widget_Record'Class);
   --  Browse a new project to open

   procedure On_Default_Project_Clicked
     (Screen : access Gtk_Widget_Record'Class);
   --  Callback for the default project radio button

   procedure On_Create_Project_Clicked
     (Screen : access Gtk_Widget_Record'Class);
   --  Callback for the create project radio button

   procedure On_Open_Project_Clicked (Screen : access Gtk_Widget_Record'Class);
   --  Callback for the open project radio button

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Screen       : out Welcome_Screen;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project_Name : VFS.Virtual_File := VFS.No_File)
   is
      Box, Hbox    : Gtk_Box;
      Sep          : Gtk_Separator;
      Size         : Gtk_Size_Group;
      Stock_Button : Gtk_Widget;
      pragma Warnings (Off, Stock_Button);

   begin
      Screen := new Welcome_Screen_Record;

      Logo_Boxes.Initialize
        (Win        => Screen,
         Title      => -"Welcome to GPS " & Config.Version &
                       " (" & Config.Source_Date & ")",
         Parent     => null,
         Title_Font => Get_Pref (Wizard_Title_Font));

      Set_Default_Size (Screen, 600, 350);

      Screen.Kernel := Kernel_Handle (Kernel);

      Set_Has_Separator (Screen, False);
      Set_Position (Screen, Win_Pos_Center);

      Gtk_New (Size);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_Start (Get_Contents (Screen), Box, Expand => True, Fill => True);
      Set_Border_Width (Box, 10);

      --  Default project

      Gtk_New
        (Screen.Default_Project,
         Label => -"Start with default project in directory:");
      Pack_Start (Box, Screen.Default_Project, Expand => False);
      Set_Tip
        (Get_Tooltips (Kernel), Screen.Default_Project,
         (-"Create a default project in memory, and use the following ") &
         (-"directory as the source and object directory. ") &
         (-"Click on the browse button to modify the directory. ") &
         (-"You can later modify any property and save the project ") &
         (-"on disk if needed."));
      Widget_Callback.Object_Connect
        (Screen.Default_Project, "clicked", On_Default_Project_Clicked'Access,
         Screen);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Screen.Default_Dir);
      Pack_Start (Hbox, Screen.Default_Dir, Expand => True, Fill => True);
      Set_Text (Screen.Default_Dir, Normalize_Pathname (Get_Current_Dir));

      Gtk_New (Screen.Default_Browse, -"Browse");
      Add_Widget (Size, Screen.Default_Browse);
      Pack_Start (Hbox, Screen.Default_Browse, Expand => False);
      Widget_Callback.Object_Connect
        (Screen.Default_Browse, "clicked", On_Browse_Default'Access, Screen);

      --  Creating a new project

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False, Padding => 5);

      Gtk_New
        (Screen.Create_Project,
         Screen.Default_Project,
         -"Create new project with wizard");
      Pack_Start (Box, Screen.Create_Project, Expand => False);
      Set_Tip
        (Get_Tooltips (Kernel), Screen.Create_Project,
         (-"Launch a wizard to create a new project on disk which will ") &
         (-"be loaded automatically. ") &
         (-"After the wizard, you can still modify any project's property."));
      Widget_Callback.Object_Connect
        (Screen.Create_Project, "clicked", On_Create_Project_Clicked'Access,
         Screen);

      --  Open project

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False, Padding => 5);

      Gtk_New
        (Screen.Open_Project_Button,
         Screen.Create_Project,
         -"Open existing project:");
      Pack_Start (Box, Screen.Open_Project_Button, Expand => False);
      Set_Tip
        (Get_Tooltips (Kernel), Screen.Open_Project_Button,
         (-"Open a project from disk, either from a list of recent ") &
         (-"projects or by browsing the file system."));
      Widget_Callback.Object_Connect
        (Screen.Open_Project_Button, "clicked", On_Open_Project_Clicked'Access,
         Screen);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Screen.Open_Project);
      Get_History (Get_History (Kernel).all,
                   "project_files", Screen.Open_Project);
      Pack_Start (Hbox, Screen.Open_Project, Expand => True, Fill => True);
      --  Synchronize the name of the key with gps-menu.adb

      if Project_Name /= No_File then
         Set_Text
           (Get_Entry (Screen.Open_Project),
            Full_Name (Project_Name, Normalize => False).all);
      end if;

      Gtk_New (Screen.Open_Browse, -"Browse");
      Add_Widget (Size, Screen.Open_Browse);
      Pack_Start (Hbox, Screen.Open_Browse, Expand => False);
      Widget_Callback.Object_Connect
        (Screen.Open_Browse, "clicked", On_Browse_Load'Access, Screen);

      if Get_Text (Get_Entry (Screen.Open_Project)) = "" then
         Clicked (Screen.Default_Project);
      else
         Clicked (Screen.Open_Project_Button);
      end if;

      --  Always displaying the welcome dialog

      Gtk_New (Screen.Always_Show,
               -"Always show this dialog when GPS starts");
      Pack_End (Box, Screen.Always_Show, Expand => False);
      Set_Active (Screen.Always_Show,
                  Get_Pref (Display_Welcome));

      Gtk_New_Hseparator (Sep);
      Pack_End (Box, Sep, Expand => False, Padding => 5);

      Stock_Button := Add_Button (Screen, Stock_Ok, Gtk_Response_OK);
      Grab_Default (Stock_Button);
      Stock_Button := Add_Button (Screen, Stock_Quit, Gtk_Response_Close);
   end Gtk_New;

   -----------------
   -- Run_Welcome --
   -----------------

   function Run_Welcome
     (Screen : access Welcome_Screen_Record) return Welcome_Result
   is
      Response : Gtk_Response_Type;
   begin
      if not Get_Pref (Display_Welcome) then
         On_Default_Project (Screen);
         return Project_Loaded;
      end if;

      Show_All (Screen);
      Display_Message (Screen, "");

      --  While the user hasn't selected a valid project...

      loop
         --  Prevent deleting of the dialog through the title bar's X button
         --  (which is shown on some window managers)
         loop
            Response := Run (Screen);

            exit when Response = Gtk_Response_Close
              or else (Response = Gtk_Response_OK
                       and then (not Get_Active (Screen.Create_Project)
                                 or else On_New_Project (Screen)));
         end loop;

         if not Get_Active (Screen.Always_Show) then
            Set_Pref (Screen.Kernel, Display_Welcome, False);
         end if;

         if Response = Gtk_Response_OK then
            Set_Focus (Screen, null);

            if Get_Active (Screen.Default_Project) then
               On_Default_Project (Screen);
               return Project_Loaded;

            elsif Get_Active (Screen.Open_Project_Button) then
               if On_Load_Project (Screen) then
                  return Project_Loaded;
               end if;

            else
               --  A new project was loaded.
               return Project_Loaded;
            end if;
         else
            return Quit_GPS;
         end if;
      end loop;

      return Project_Loaded;
   end Run_Welcome;

   --------------------
   -- On_New_Project --
   --------------------

   function On_New_Project
     (Screen : access Gtk_Widget_Record'Class) return Boolean
   is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
   begin
      return Create_New_Project (S.Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_New_Project;

   ------------------------
   -- On_Default_Project --
   ------------------------

   procedure On_Default_Project (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
   begin
      Load_Default_Project (S.Kernel, Create (Get_Text (S.Default_Dir)));
      Response (S, Gtk_Response_OK);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Default_Project;

   ---------------------
   -- On_Load_Project --
   ---------------------

   function On_Load_Project
     (Screen : access Gtk_Widget_Record'Class) return Boolean
   is
      S            : constant Welcome_Screen := Welcome_Screen (Screen);
      Project_Name : Virtual_File := Create
        (Get_Text (Get_Entry (S.Open_Project)));
      Button       : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      Response (S, Gtk_Response_OK);

      if File_Extension (Project_Name) /= Project_File_Extension then
         Project_Name := Create
           (Full_Name (Project_Name).all & Project_File_Extension);
      end if;

      if not Is_Regular_File (Project_Name) then
         Button := Message_Dialog
           ((-"Project file ")
            & Full_Name (Project_Name).all & (-" doesn't exist"),
            Error, Button_OK, Parent => Gtk_Window (S));
         return False;
      end if;

      Load_Project (S.Kernel, Project_Name, Clear => False);

      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Button := Message_Dialog
           ((-"Project file ")
            & Full_Name (Project_Name).all & (-" couldn't be loaded"),
            Error, Button_OK, Parent => Gtk_Window (S));
         return False;
   end On_Load_Project;

   -----------------------
   -- On_Browse_Default --
   -----------------------

   procedure On_Browse_Default (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
      Dir : constant Virtual_File := Select_Directory
        (Title             => -"Select a directory",
         Base_Directory    => Create (Get_Text (S.Default_Dir)),
         Parent            => Gtk_Window (S),
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
         History           => Get_History (S.Kernel));
   begin
      if Dir /= No_File then
         Set_Text (S.Default_Dir, Full_Name (Dir).all);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Browse_Default;

   --------------------
   -- On_Browse_Load --
   --------------------

   procedure On_Browse_Load (Screen : access Gtk_Widget_Record'Class) is
      S            : constant Welcome_Screen := Welcome_Screen (Screen);
      Project_Name : constant String := Get_Text (Get_Entry (S.Open_Project));
      Dir          : Virtual_File;
   begin
      Push_State (S.Kernel, Busy);
      if Project_Name = "" then
         Dir := Create (Get_Text (S.Default_Dir));
      else
         Dir := Create (Dir_Name (Project_Name));
      end if;

      declare
         File : constant Virtual_File := Select_File
           (Title             => -"Select project file",
            Base_Directory    => Dir,
            File_Pattern      => "*.gpr",
            Pattern_Name      => -"Project Files",
            Parent            => Gtk_Window (Get_Toplevel (Screen)),
            Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
            Kind              => Open_File,
            History           => Get_History (S.Kernel));

      begin
         if File /= VFS.No_File then
            Set_Text (Get_Entry (S.Open_Project), Full_Name (File).all);
         end if;
      end;

      Pop_State (S.Kernel);

   exception
      when E : others =>
         Pop_State (S.Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Browse_Load;

   --------------------------------
   -- On_Default_Project_Clicked --
   --------------------------------

   procedure On_Default_Project_Clicked
     (Screen : access Gtk_Widget_Record'Class)
   is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
   begin
      Set_Sensitive (S.Open_Project, False);
      Set_Sensitive (S.Open_Browse, False);
      Set_Sensitive (S.Default_Dir, True);
      Set_Sensitive (S.Default_Browse, True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Default_Project_Clicked;

   -------------------------------
   -- On_Create_Project_Clicked --
   -------------------------------

   procedure On_Create_Project_Clicked
     (Screen : access Gtk_Widget_Record'Class)
   is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
   begin
      Set_Sensitive (S.Open_Project, False);
      Set_Sensitive (S.Open_Browse, False);
      Set_Sensitive (S.Default_Dir, False);
      Set_Sensitive (S.Default_Browse, False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Create_Project_Clicked;

   -----------------------------
   -- On_Open_Project_Clicked --
   -----------------------------

   procedure On_Open_Project_Clicked
     (Screen : access Gtk_Widget_Record'Class)
   is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
   begin
      Set_Sensitive (S.Open_Project, True);
      Set_Sensitive (S.Open_Browse, True);
      Set_Sensitive (S.Default_Dir, False);
      Set_Sensitive (S.Default_Browse, False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_Project_Clicked;

end Welcome;
