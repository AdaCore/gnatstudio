-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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

with Gtk; use Gtk;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main;
with Gtk.Rc;
with Glide_Page;
with Glide_Menu;
with Glide_Main_Window;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Editor;  use Glide_Kernel.Editor;
with Gtkada.Intl;          use Gtkada.Intl;
with Gtkada.Dialogs;       use Gtkada.Dialogs;
with GVD.Types;
with OS_Utils;             use OS_Utils;
with Ada.Command_Line; use Ada.Command_Line;

--  Just force the loading of the modules
--  Removing any of the line below will not load the module in Glide, and thus
--  the associated functionalities will not be available
pragma Warnings (Off);
with Project_Viewers;
with Project_Explorers;
with Aunit_Module;
with Browsers.Module;
pragma Warnings (On);

procedure Glide2 is
   use Glide_Main_Window;

   subtype String_Access is GNAT.OS_Lib.String_Access;

   Directory_Separator : constant Character := GNAT.OS_Lib.Directory_Separator;
   Glide          : Glide_Window;
   Page           : Glide_Page.Glide_Page;
   Directory      : Dir_Type;
   Str            : String (1 .. 1024);
   Last           : Natural;
   Project_Loaded : Boolean := False;
   Button         : Message_Dialog_Buttons;
   Home           : String_Access;
   Prefix         : String_Access;
   Dir            : String_Access;

   procedure Init_Settings;
   --  Set up environment for Glide.

   ----------
   -- Init --
   ----------

   procedure Init_Settings is
      Dir_Created : Boolean := False;
   begin
      Home := Getenv ("GLIDE_HOME");

      if Home.all = "" then
         Free (Home);
         Home := Getenv ("HOME");
      end if;

      Prefix := Getenv ("GLIDE_ROOT");

      if Prefix.all = "" then
         Free (Prefix);
         Prefix := new String' (Executable_Location);

         if Prefix.all = "" then
            Free (Prefix);
            Prefix := new String' (GVD.Prefix);
         end if;
      end if;

      Bind_Text_Domain
        ("glide", Prefix.all & GNAT.OS_Lib.Directory_Separator & "share" &
         Directory_Separator & "locale");

      if Home.all /= "" then
         if Is_Directory_Separator (Home (Home'Last)) then
            Dir := new String' (Home (Home'First .. Home'Last - 1) &
              Directory_Separator & ".glide");
         else
            Dir := new String' (Home.all & Directory_Separator & ".glide");
         end if;

      else
         --  Default to /
         Dir := new String'(Directory_Separator & ".glide");
      end if;

      begin
         if not Is_Directory (Dir.all) then
            Make_Dir (Dir.all);
            Button := Message_Dialog
              ((-"Created config directory ") & Dir.all,
               Information, Button_OK, Justification => Justify_Left);
            Dir_Created := True;
         end if;

         if not
           Is_Directory (Dir.all & Directory_Separator & "sessions")
         then
            Make_Dir (Dir.all & Directory_Separator & "sessions");
            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ")
                  & Dir.all & Directory_Separator & "sessions",
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

      --  ??? Load the preferences, or set the default values

      --  if Is_Regular_File
      --    (Dir.all & Directory_Separator & "preferences")
      --  then
      --     Load_Preferences (Dir.all & Directory_Separator & "preferences");
      --  else
      --     Set_Default_Preferences;
      --  end if;
   end Init_Settings;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Glide, "<glide>", Glide_Menu.Glide_Menu_Items.all);
   Set_Title (Glide, "Glide - Next Generation");
   Maximize (Glide);

   Init_Settings;
   Glide.Home_Dir := Dir;
   Glide.Prefix_Directory := Prefix;

   declare
      Rc : constant String := Prefix.all & Directory_Separator & "bin" &
        Directory_Separator & "gtkrc";
   begin
      if Is_Regular_File (Rc) then
         Gtk.Rc.Parse (Rc);
      end if;
   end;

   --  ??? Should have a cleaner way of initializing Log_File

   declare
      Log : aliased constant String :=
        Dir.all & Directory_Separator & "log" & ASCII.NUL;
   begin
      Glide.Debug_Mode := True;
      Glide.Log_Level  := GVD.Types.Hidden;
      Glide.Log_File   := Create_File (Log'Address, Fmode => Text);
   end;

   Glide_Page.Gtk_New (Page, Glide);
   Initialize_All_Modules (Glide.Kernel);

   for J in 1 .. Argument_Count loop
      if File_Extension (Argument (J)) = ".gpr" then
         Load_Project (Glide.Kernel, Argument (J));
         Project_Loaded := True;
      else
         Open_Or_Create (Glide.Kernel, Argument (J));
      end if;
   end loop;

   --  If no project has been specified on the command line, try to open
   --  the first one in the current directory (if any).

   if not Project_Loaded then
      Open (Directory, Get_Current_Dir);

      loop
         Read (Directory, Str, Last);

         exit when Last = 0;

         if File_Extension (Str (1 .. Last)) = ".gpr" then
            Load_Project (Glide.Kernel, Str (1 .. Last));
            exit;
         end if;
      end loop;
   end if;

   Show_All (Glide);
   Gtk.Main.Main;
end Glide2;
