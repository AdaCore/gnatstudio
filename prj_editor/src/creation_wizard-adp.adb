-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
--                             AdaCore                               --
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

with Adp_Converter;             use Adp_Converter;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Projects;                  use Projects;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Label;                 use Gtk.Label;
with Glide_Intl;                use Glide_Intl;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with VFS;                       use VFS;
with Ada.Exceptions;            use Ada.Exceptions;
with Traces;                    use Traces;

package body Creation_Wizard.Adp is

   procedure On_Browse (Wiz : access Gtk_Widget_Record'Class);
   --  Called when the browse button is pressed.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz    : out Adp_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Wiz := new Adp_Wizard_Record;
      Creation_Wizard.Adp.Initialize (Wiz, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz    : access Adp_Wizard_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Box2   : Gtk_Box;
      Button : Gtk_Button;
   begin
      Creation_Wizard.Initialize
        (Wiz, Kernel,
         Force_Relative_Dirs       => True,
         Ask_About_Loading         => False,
         Activate_Finish_From_Page => -1);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add_Page (Wiz, Box, -".adp file selection", -"Select .adp file name");

      Gtk_New (Label, -"Enter the name of the .adp file:");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New_Hbox (Box2, Homogeneous => False);
      Pack_Start (Box, Box2, Expand => False);

      Gtk_New (Wiz.Adp_File_Name);
      Pack_Start (Box2, Wiz.Adp_File_Name, Expand => True);

      Gtk_New (Button, -"Browse");
      Pack_Start (Box2, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Browse'Access), Wiz);

      Show_All (Box);
   end Initialize;

   ---------------
   -- On_Browse --
   ---------------

   procedure On_Browse (Wiz : access Gtk_Widget_Record'Class) is
      W    : constant Adp_Wizard := Adp_Wizard (Wiz);
      Name : constant VFS.Virtual_File := Select_File
        (Use_Native_Dialog  => Get_Pref (W.Kernel, Use_Native_Dialogs),
         File_Pattern       => "*.adp",
         Parent             => Get_Main_Window (W.Kernel),
         Kind               => Open_File);
   begin
      if Name /= VFS.No_File then
         Set_Text (W.Adp_File_Name, Full_Name (Name).all);
      end if;
   end On_Browse;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Wiz     : access Adp_Wizard_Record;
      Project : in out Projects.Project_Type)
   is
      Adp_File : constant String := Get_Text (Wiz.Adp_File_Name);
   begin
      if Adp_File /= "" then
         Convert_Adp_File
           (Adp_Filename   => Adp_File,
            Registry       => Get_Registry (Wiz.Kernel).all,
            Project        => Project,
            Spec_Extension => ".ads",
            Body_Extension => ".adb");
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Information (E));
   end Generate_Project;

end Creation_Wizard.Adp;
