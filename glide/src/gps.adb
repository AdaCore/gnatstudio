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

with Gtk;                       use Gtk;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Main;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Rc;
with Glide_Page;
with Glide_Menu;
with Glide_Main_Window;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with String_Utils;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Gtkada.Intl;               use Gtkada.Intl;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with GVD.Types;
with OS_Utils;                  use OS_Utils;
with GNAT.Command_Line;         use GNAT.Command_Line;
with Ada.Text_IO;               use Ada.Text_IO;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Language.Ada;              use Language.Ada;
with Language.C;                use Language.C;
with Language.Cpp;              use Language.Cpp;
with Prj;                       use Prj;
with Src_Info;                  use Src_Info;
with Traces;                    use Traces;

--  Modules registered by GPS.
with Aunit_Module;
with Browsers.Dependency_Items;
with Browsers.Projects;
with Browsers.Call_Graph;
with External_Editor_Module;
with GVD_Module;
with Metrics_Module;
with Project_Explorers;
with Project_Viewers;
with Src_Editor_Module;
with VCS_Module;
with VCS.CVS;
with Glide_Kernel.Help;
with Vdiff_Module;
with Builder_Module;
with Glide_Kernel.Console;
with Navigation_Module;

--  The LI parsers
with Src_Info.ALI;
with Src_Info.CPP;

procedure GPS is
   use Glide_Main_Window;

   Me : Debug_Handle := Create ("GPS");

   subtype String_Access is GNAT.OS_Lib.String_Access;

   GPS            : Glide_Window;
   Page           : Glide_Page.Glide_Page;
   Directory      : Dir_Type;
   Str            : String (1 .. 1024);
   Last           : Natural;
   Project_Loaded : Boolean := False;
   Button         : Message_Dialog_Buttons;
   Home           : String_Access;
   Prefix         : String_Access;
   Dir            : String_Access;
   File_Opened    : Boolean := False;
   Handler        : Language_Handlers.Glide.Glide_Language_Handler;

   procedure Init_Settings;
   --  Set up environment for GPS.

   procedure Help;
   --  Display help on the standard output.

   ----------
   -- Init --
   ----------

   procedure Init_Settings is
      Dir_Created : Boolean := False;
      File : File_Type;
   begin
      Home := Getenv ("GPS_HOME");

      if Home.all = "" then
         Free (Home);
         Home := Getenv ("HOME");
      end if;

      Prefix := Getenv ("GPS_ROOT");

      if Prefix.all = "" then
         Free (Prefix);
         Prefix := new String' (Executable_Location);

         if Prefix.all = "" then
            Free (Prefix);
            Prefix := new String' (GVD.Prefix);
         end if;
      end if;

      Bind_Text_Domain
        ("gps", Format_Pathname (Prefix.all & "/share/locale"));

      if Home.all /= "" then
         Dir := new String'
           (String_Utils.Name_As_Directory (Home.all) & ".gps");
      else
         --  Default to /
         Dir := new String' (Format_Pathname ("/.gps"));
      end if;

      begin
         if not Is_Directory (Dir.all) then
            Make_Dir (Dir.all);
            Button := Message_Dialog
              ((-"Created config directory ") & Dir.all,
               Information, Button_OK, Justification => Justify_Left);
            Dir_Created := True;

            --  Create a default configuration file for the traces.
            --  This should be left while GPS is considered as not fully
            --  stable.
            Create
              (File,
               Name => String_Utils.Name_As_Directory (Dir.all)
                 & "traces.cfg");
            Put_Line (File, ">log");
            Put_Line (File, "+");
            Put_Line (File, "DEBUG.COLORS=no");
            Put_Line (File, "DEBUG.ABSOLUTE_TIME=yes");
            Put_Line (File, "DEBUG.ELAPSED_TIME=no");
            Put_Line (File, "DEBUG.STACK_TRACE=no");
            Put_Line (File, "DEBUG.LOCATION=no");
            Put_Line (File, "DEBUG.ENCLOSING_ENTITY=no");
            Close (File);
         end if;

         if not
           Is_Directory (String_Utils.Name_As_Directory (Dir.all) & "sessions")
         then
            Make_Dir (String_Utils.Name_As_Directory (Dir.all) & "sessions");
            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ")
                  & String_Utils.Name_As_Directory (Dir.all) & "sessions",
                  Information, Button_OK, Justification => Justify_Left);
            end if;
         end if;

      exception
         when Directory_Error =>
            Button := Message_Dialog
              ((-"Cannot create config directory ") & Dir.all & ASCII.LF &
               (-"Exiting..."),
               Error, Button_OK,
               Justification => Justify_Left);
            OS_Exit (1);
      end;
   end Init_Settings;

   ----------
   -- Help --
   ----------

   procedure Help is
      use ASCII;
   begin
      if GVD.Can_Output then
         Put_Line ("GPS " & GVD.Version &
                   (-", A complete application development system."));
         Put_Line (-"Usage:");
         Put_Line (-"   gps [project-file] [source1] [source2] ...");
         Put_Line (-"Options:");
         Put_Line (-"   --help              Show this help message and exit.");
         Put_Line (-"   --version           Show the GPS version and exit.");

      else
         Button := Message_Dialog
           ("GPS " & GVD.Version &
            (-", A complete application development system.") & LF &
            (-"Usage:") & LF &
            (-"   gps [project-file] [source1] [source2] ...") & LF &
            (-"Options:") & LF &
            (-"   --help              Show this help message and exit.") & LF &
            (-"   --version           Show the GPS version and exit."),
            Information, Button_OK,
            Title => -"Help",
            Justification => Justify_Left);
      end if;
   end Help;

begin
   --  Initialize GtkAda

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Init_Settings;

   --  Initialize the traces
   Traces.Parse_Config_File
     (Default => String_Utils.Name_As_Directory (Dir.all) & "traces.cfg");

   Gtk_New
     (GPS, "<gps>", Glide_Menu.Glide_Menu_Items.all, Dir.all, Prefix.all);
   Set_Title (GPS, "GPS - Next Generation");
   Maximize (GPS);

   declare
      System_Rc : constant String :=
        Format_Pathname (Prefix.all & "/etc/gps/gtkrc");

      Rc : constant String :=
        String_Utils.Name_As_Directory (Dir.all) & "gtkrc";
   begin
      --  Parse the system's RC file
      Trace (Me, "Parsing System RC file " & System_Rc);
      if Is_Regular_File (System_Rc) then
         Gtk.Rc.Parse (System_Rc);
      end if;

      --  Parse the user's RC file
      Trace (Me, "Parsing RC file " & Rc);
      if Is_Regular_File (Rc) then
         Gtk.Rc.Parse (Rc);
      end if;
   end;

   --  ??? Should have a cleaner way of initializing Log_File

   declare
      Log : constant String :=
        String_Utils.Name_As_Directory (GPS.Home_Dir.all) & "debugger.log";
   begin
      GPS.Debug_Mode := True;
      GPS.Log_Level  := GVD.Types.Internal;
      GPS.Log_File   := Create_File (Log, Fmode => Text);
   end;

   Glide_Page.Gtk_New (Page, GPS);
   Initialize_Console (GPS.Kernel);

   --  Register all modules

   Navigation_Module.Register_Module (GPS.Kernel);
   Metrics_Module.Register_Module (GPS.Kernel);
   Browsers.Call_Graph.Register_Module (GPS.Kernel);
   Browsers.Dependency_Items.Register_Module (GPS.Kernel);
   Browsers.Projects.Register_Module (GPS.Kernel);
   Project_Viewers.Register_Module (GPS.Kernel);
   Project_Explorers.Register_Module (GPS.Kernel);
   Src_Editor_Module.Register_Module (GPS.Kernel);
   External_Editor_Module.Register_Module (GPS.Kernel);
   Glide_Kernel.Help.Register_Module (GPS.Kernel);
   GVD_Module.Register_Module (GPS.Kernel);
   Builder_Module.Register_Module (GPS.Kernel);
   Vdiff_Module.Register_Module (GPS.Kernel);
   VCS_Module.Register_Module (GPS.Kernel);
   VCS.CVS.Register_Module (GPS.Kernel);
   Aunit_Module.Register_Module (GPS.Kernel);
   Glide_Kernel.Console.Register_Module (GPS.Kernel);

   --  Register the supported languages and their associated LI handlers.

   Handler := Glide_Language_Handler (Get_Language_Handler (GPS.Kernel));

   Register_LI_Handler
     (Handler, "Ada", new Src_Info.ALI.ALI_Handler_Record);
   Register_LI_Handler
     (Handler, "c/c++", new Src_Info.CPP.CPP_LI_Handler_Record);

   Register_Language (Handler, "Ada", Ada_Lang);
   Add_Language_Info
     (Handler, "Ada",
      LI                  => Get_LI_Handler_By_Name (Handler, "Ada"),
      Default_Spec_Suffix => ".ads",
      Default_Body_Suffix => ".adb");

   Register_Language (Handler, "c", C_Lang);
   Add_Language_Info
     (Handler, "c",
      LI                  => Get_LI_Handler_By_Name (Handler, "c/c++"),
      Default_Spec_Suffix => ".h",
      Default_Body_Suffix => ".c");

   Register_Language (Handler, "c++", Cpp_Lang);
   Add_Language_Info
     (Handler, "c++",
      LI                  => Get_LI_Handler_By_Name (Handler, "c/c++"),
      Default_Spec_Suffix => ".h",
      Default_Body_Suffix => ".cc");

   --  Temporarily disable unimplemented menu items

   declare
      File     : constant String := '/' & (-"File") & '/';
      Edit     : constant String := '/' & (-"Edit") & '/';
      Navigate : constant String := '/' & (-"Navigate") & '/';
      Tools    : constant String := '/' & (-"Tools") & '/';

   begin
      Set_Sensitive (Find_Menu_Item (GPS.Kernel, File & (-"Print")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, File & (-"Close All")), False);

      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Edit & (-"Preferences")), False);

      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Navigate & (-"Goto Parent Unit")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Navigate & (-"Start Of Statement")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Navigate & (-"End Of Statement")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Navigate & (-"Next Procedure")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Navigate & (-"Previous Procedure")), False);

      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Tools & (-"Code Fixing")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Tools & (-"Profile")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Tools & (-"Memory Analyzer")), False);
      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, Tools & (-"Generate API doc")), False);

      Set_Sensitive (Find_Menu_Item
        (GPS.Kernel, File & (-"New View")), False);
   end;

   loop
      case Getopt ("-version -help") is
         -- long option names --
         when '-' =>
            case Full_Switch (Full_Switch'First + 1) is
               -- --version --
               when 'v' =>
                  if GVD.Can_Output then
                     Put_Line ("GPS version " & GVD.Version &
                       " hosted on " & GVD.Target);
                  else
                     Button := Message_Dialog
                       ("GPS version " & GVD.Version &
                        " hosted on " & GVD.Target,
                        Information, Button_OK,
                        Title => -"Version",
                        Justification => Justify_Left);
                  end if;

                  OS_Exit (0);

               when 'h' =>
                  -- --help --
                  if Full_Switch = "-help" then
                     Help;
                     OS_Exit (0);
                  end if;

               when others =>
                  null;
            end case;

         when ASCII.NUL =>
            exit;

         when others =>
            null;
      end case;
   end loop;

   Glide_Page.Load_Desktop (Page, GPS);

   loop
      declare
         S : constant String := Get_Argument (Do_Expansion => True);
      begin
         exit when S = "";

         if File_Extension (S) = Project_File_Extension then
            Load_Project (GPS.Kernel, Normalize_Pathname (S));
            Project_Loaded := True;
         else
            Open_File_Editor (GPS.Kernel, S);
            File_Opened := True;
         end if;
      end;
   end loop;

   --  If no project has been specified on the command line, try to open
   --  the first one in the current directory (if any).

   if not Project_Loaded then
      Open (Directory, Get_Current_Dir);

      loop
         Read (Directory, Str, Last);

         exit when Last = 0;

         if File_Extension (Str (1 .. Last)) = Project_File_Extension then
            Load_Project (GPS.Kernel, Str (1 .. Last));
            exit;
         end if;
      end loop;
   end if;

   --  Call Show_All before displaying the help so that the help window will
   --  have the focus.

   Show_All (GPS);

   if not File_Opened
     and then not Has_Saved_Desktop (GPS.Kernel)
   then
      Open_Html
        (GPS.Kernel,
         Format_Pathname
           (GPS.Prefix_Directory.all &
            "/doc/gps/html/gps-welcome.html"));
      Maximize_Children (Get_MDI (GPS.Kernel));
   end if;

   Gtk.Main.Main;

   Trace (Me, "Saving preferences in "
          & String_Utils.Name_As_Directory (Dir.all) & "preferences");
   Save_Preferences
     (GPS.Kernel,
      String_Utils.Name_As_Directory (Dir.all) & "preferences");

   Free (Home);
   Free (Dir);
   Free (Prefix);

exception
   when Invalid_Switch | Invalid_Parameter =>
      if GVD.Can_Output then
         Put_Line ("Invalid command line");
      end if;

      Help;
end GPS;
