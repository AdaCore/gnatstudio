-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Check_Button;     use Gtk.Check_Button;
with Gtk.Combo;            use Gtk.Combo;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Label;            use Gtk.Label;
with Gtk.Separator;        use Gtk.Separator;
with Gtk.Size_Group;       use Gtk.Size_Group;
with Gtk.Stock;            use Gtk.Stock;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.File_Selector; use Gtkada.File_Selector;

with GVD;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Intl;                use Glide_Intl;
with Logo_Boxes;                use Logo_Boxes;
with Histories;                 use Histories;
with Project_Viewers;           use Project_Viewers;
with Creation_Wizard;           use Creation_Wizard;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Prj;           use Prj;

package body Welcome is

   procedure On_New_Project (Screen : access Gtk_Widget_Record'Class);
   --  Called when the user wants to create a new widget

   procedure On_Default_Project (Screen : access Gtk_Widget_Record'Class);
   --  Create and load a default project

   procedure On_Browse_Default (Screen : access Gtk_Widget_Record'Class);
   --  Browse a new directory for the default project

   procedure On_Load_Project (Screen : access Gtk_Widget_Record'Class);
   --  Load an existing project

   procedure On_Browse_Load (Screen : access Gtk_Widget_Record'Class);
   --  Browse a new project to open

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Screen       : out Welcome_Screen;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project_Name : String := "")
   is
      Box, Hbox, Vbox : Gtk_Box;
      Sep       : Gtk_Separator;
      Label     : Gtk_Label;
      Button    : Gtk_Button;
      Size      : Gtk_Size_Group;
      Quit      : Gtk_Widget;
      pragma Unreferenced (Quit);

   begin
      Screen := new Welcome_Screen_Record;

      Logo_Boxes.Initialize
        (Win        => Screen,
         Title      => -"Welcome to GPS " & GVD.Version &
                       " (" & GVD.Source_Date & ")",
         Parent     => null,
         Title_Font => Get_Pref (Kernel, Wizard_Title_Font));

      Set_Default_Size (Screen, 600, 300);

      Screen.Kernel := Kernel_Handle (Kernel);

      Set_Has_Separator (Screen, False);
      Set_Position (Screen, Win_Pos_Center);

      Gtk_New (Size);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Get_Contents (Screen), Box);
      Set_Border_Width (Box, 10);

      --  Default project

      Gtk_New (Label, "Start with default project in directory:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Screen.Default_Dir);
      Pack_Start (Hbox, Screen.Default_Dir, Expand => True, Fill => True);
      Set_Text (Screen.Default_Dir, Get_Current_Dir);

      Gtk_New_Vbox (Vbox, Homogeneous => True);
      Pack_Start (Hbox, Vbox, Expand => False);

      Gtk_New (Button, -"Browse");
      Add_Widget (Size, Button);
      Pack_Start (Vbox, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Browse_Default'Access), Screen);

      Gtk_New (Button, -"Start");
      Add_Widget (Size, Button);
      Pack_Start (Vbox, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Default_Project'Access),
         Screen);

      --  Creating a new project

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False, Padding => 5);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Label, -"Create new project with wizard");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Hbox, Label, Expand => True, Fill => True);

      Gtk_New (Button, -"Create");
      Add_Widget (Size, Button);
      Pack_Start (Hbox, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_New_Project'Access),
         Screen);

      --  Open project

      Gtk_New_Hseparator (Sep);
      Pack_Start (Box, Sep, Expand => False, Padding => 5);

      Gtk_New (Label, "Open existing project:");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => False);

      Gtk_New (Screen.Open_Project);
      Get_History (Get_History (Kernel).all,
                   Project_Viewers.Project_History_Key, Screen.Open_Project);
      Pack_Start (Hbox, Screen.Open_Project, Expand => True, Fill => True);
      Set_Text (Get_Entry (Screen.Open_Project),
                Normalize_Pathname (Project_Name, Resolve_Links => False));

      Gtk_New_Vbox (Vbox, Homogeneous => True);
      Pack_Start (Hbox, Vbox, Expand => False);

      Gtk_New (Button, -"Browse");
      Add_Widget (Size, Button);
      Pack_Start (Vbox, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Browse_Load'Access), Screen);

      Gtk_New (Button, -"Open");
      Add_Widget (Size, Button);
      Pack_Start (Vbox, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Load_Project'Access),
         Screen);

      --  Always displaying the welcome dialog

      Gtk_New (Screen.Always_Show,
               -"Always show this dialog when GPS starts");
      Pack_End (Box, Screen.Always_Show, Expand => False);
      Set_Active (Screen.Always_Show,
                  Get_Pref (Kernel, Display_Welcome));

      Gtk_New_Hseparator (Sep);
      Pack_End (Box, Sep, Expand => False, Padding => 5);

      Quit := Add_Button (Screen, Stock_Quit, Gtk_Response_Close);
   end Gtk_New;

   ---------
   -- Run --
   ---------

   function Run (Screen : access Welcome_Screen_Record) return Boolean is
      Response : Gtk_Response_Type;
   begin
      if Get_Pref (Screen.Kernel, Display_Welcome) then
         Show_All (Screen);

         --  Prevent deleting of the dialog through the title bar's X button
         --  (which is shown on some window managers)
         loop
            Response := Run (Screen);

            exit when Response = Gtk_Response_OK
              or else Response = Gtk_Response_Close;
         end loop;

         if not Get_Active (Screen.Always_Show) then
            Set_Pref (Screen.Kernel, Display_Welcome, False);
         end if;

         return Response /= Gtk_Response_Close;

      elsif Get_Text (Get_Entry (Screen.Open_Project)) /= "" then
         On_Load_Project (Screen);

      else
         On_Default_Project (Screen);
      end if;

      return True;
   end Run;

   --------------------
   -- On_New_Project --
   --------------------

   procedure On_New_Project (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
      Wiz : Creation_Wizard.Prj_Wizard;
   begin
      Gtk_New (Wiz, S.Kernel);

      declare
         Name : constant String := Run (Wiz);
      begin
         if Name /= "" then
            Load_Project (S.Kernel, Name);
            Project_Viewers.Add_To_Reopen (S.Kernel, Name);
            Response (Welcome_Screen (Screen), Gtk_Response_OK);
         end if;
      end;

      Destroy (Wiz);
   end On_New_Project;

   ------------------------
   -- On_Default_Project --
   ------------------------

   procedure On_Default_Project (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
   begin
      Load_Default_Project (S.Kernel, Get_Text (S.Default_Dir));
      Response (Welcome_Screen (Screen), Gtk_Response_OK);
   end On_Default_Project;

   ---------------------
   -- On_Load_Project --
   ---------------------

   procedure On_Load_Project (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
      Project_Name : constant String := Normalize_Pathname
        (Get_Text (Get_Entry (S.Open_Project)), Resolve_Links => False);
   begin
      if File_Extension (Project_Name) = Project_File_Extension then
         Load_Project (S.Kernel, Project_Name);
         Project_Viewers.Add_To_Reopen (S.Kernel, Project_Name);
      else
         Load_Project
           (S.Kernel, Project_Name & Project_File_Extension);
         Project_Viewers.Add_To_Reopen
           (S.Kernel, Project_Name & Project_File_Extension);
      end if;

      Response (Welcome_Screen (Screen), Gtk_Response_OK);
   end On_Load_Project;

   -----------------------
   -- On_Browse_Default --
   -----------------------

   procedure On_Browse_Default (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
      Dir : constant String := Select_Directory
        (Title             => -"Select a directory",
         Base_Directory    => Get_Text (S.Default_Dir),
         Use_Native_Dialog => Get_Pref (S.Kernel, Use_Native_Dialogs),
         History           => Get_History (S.Kernel));
   begin
      if Dir /= "" then
         Set_Text (S.Default_Dir, Dir);
      end if;
   end On_Browse_Default;

   --------------------
   -- On_Browse_Load --
   --------------------

   procedure On_Browse_Load (Screen : access Gtk_Widget_Record'Class) is
      S : constant Welcome_Screen := Welcome_Screen (Screen);
      Project_Name : constant String := Get_Text (Get_Entry (S.Open_Project));
      File : constant String := Select_File
        (Title             => -"Select project file",
         Base_Directory    => Dir_Name (Project_Name),
         Use_Native_Dialog => Get_Pref (S.Kernel, Use_Native_Dialogs),
         History           => Get_History (S.Kernel));
   begin
      if File /= "" then
         Set_Text (Get_Entry (S.Open_Project), File);
      end if;
   end On_Browse_Load;

end Welcome;
