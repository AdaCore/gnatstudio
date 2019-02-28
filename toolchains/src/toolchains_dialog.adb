------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Glib;                      use Glib;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Editable;              use Gtk.Editable;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;

with Toolchains_Old;            use Toolchains_Old;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GUI_Utils;                 use GUI_Utils;

package body Toolchains_Dialog is

   package Dialog_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Dialog);

   type Entry_Callback_Data is record
      E : Gtk_Entry;
      D : Dialog;
   end record;

   package Entry_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Entry_Callback_Data);

   procedure Activate_Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      D      : Dialog);
   --  Called when the 'Activate' check button is toggled

   procedure Xrefs_Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      D      : Dialog);
   --  Called when the 'Activate' check button is toggled

   procedure On_Changed
     (GEntry : access Gtk_Widget_Record'Class;
      D      : Dialog);
   --  Called when an entry changed.

   procedure On_Browse
     (Button : access Gtk_Widget_Record'Class;
      Data   : Entry_Callback_Data);
   --  Browse for a directory, then fill the GEntry

   ----------------------
   -- Activate_Toggled --
   ----------------------

   procedure Activate_Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      D      : Dialog)
   is
   begin
      D.Active := Get_Active (Gtk_Check_Button (Toggle));
      Set_Sensitive (D.Frame, D.Active);
      On_Changed (Toggle, D);
   end Activate_Toggled;

   -------------------
   -- Xrefs_Toggled --
   -------------------

   procedure Xrefs_Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      D      : Dialog)
   is
   begin
      D.Xrefs_Subdir := Get_Active (Gtk_Check_Button (Toggle));
   end Xrefs_Toggled;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
     (GEntry : access Gtk_Widget_Record'Class;
      D      : Dialog)
   is
      pragma Unreferenced (GEntry);
   begin
      --  Allow OK if the tools path and compiler path are different
      if D.Active
        and then Get_Text (D.Tools_Entry) = Get_Text (D.Compiler_Entry)
      then
         Set_Sensitive (D.OK_Button, False);
      else
         Set_Sensitive (D.OK_Button, True);
      end if;
   end On_Changed;

   ---------------
   -- On_Browse --
   ---------------

   procedure On_Browse
     (Button : access Gtk_Widget_Record'Class;
      Data   : Entry_Callback_Data)
   is
      Current_Dir : constant String := Get_Text (Data.E);
      Start_Dir   : Virtual_File;

   begin
      if Current_Dir /= "" then
         Start_Dir := Create_From_UTF8 (Current_Dir);

         if not Is_Directory (Start_Dir) then
            Start_Dir := GNATCOLL.VFS.Get_Current_Dir;
         end if;
      else
         Start_Dir := GNATCOLL.VFS.Get_Current_Dir;
      end if;

      declare
         Dir      : constant GNATCOLL.VFS.Virtual_File :=
                      Select_Directory
                        (Base_Directory => Start_Dir,
                         Parent         => Gtk_Window (Get_Toplevel (Button)));
         Compiler : constant String :=
                      Get_Project (Data.D.Kernel).Attribute_Value
                        (Compiler_Command_Attribute,
                         Default => "gnatmake",
                         Index   => "Ada");
         Exec     : Virtual_File;
      begin
         if Dir /= No_File then
            Exec := Locate_Exec (+Compiler, (1 => Dir));

            if Exec /= No_File then
               --  OK, we could locate a valid compiler.
               Set_Text (Data.E, Display_Full_Name (Dir));
            else
               --  No compiler found: let's display an error.
               declare
                  Resp : Gtkada.Dialogs.Message_Dialog_Buttons;
               begin
                  Resp := GPS_Message_Dialog
                    (-("The selected path does not contain a compiler." &
                       ASCII.LF &
                       "Are you sure you want to use this path ?"),
                     Dialog_Type => Gtkada.Dialogs.Error,
                     Buttons     => Button_OK + Button_Cancel,
                     Title       => -"Invalid compiler path",
                     Parent      => Gtk_Window (Data.D));

                  if Resp = Button_OK then
                     Set_Text (Data.E, Display_Full_Name (Dir));
                  end if;
               end;
            end if;
         end if;
      end;
   end On_Browse;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget          : out Dialog;
      Kernel          : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Active          : Boolean;
      Tools_Path      : Virtual_File;
      Use_Xrefs_Subdirs : Boolean;
      Compiler_Path     : Virtual_File)
   is
      Check  : Gtk_Check_Button;
      Dead   : Gtk_Widget;
      Table  : Gtk_Table;
      Label  : Gtk_Label;
      Browse : Gtk_Button;
      Pix    : Gtk_Image;
      pragma Unreferenced (Dead);

   begin
      Widget := new Dialog_Record;
      Widget.Active       := Active;
      Widget.Xrefs_Subdir := Use_Xrefs_Subdirs;

      GPS.Dialogs.Initialize
        (Widget,
         Title  => -"Toolchains Configuration",
         Kernel => Kernel,
         Flags  => Gtk.Dialog.Modal or Destroy_With_Parent);

      Widget.OK_Button :=
        Gtk_Button (Widget.Add_Button (Gtk.Stock.Stock_Ok, Gtk_Response_OK));
      Dead :=
        Widget.Add_Button (Gtk.Stock.Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Check, -"Activate multiple toolchains setup");
      Show_All (Check);
      Set_Active (Check, Widget.Active);
      Widget.Get_Content_Area.Add (Check);
      Dialog_Callback.Connect
        (Check, Signal_Toggled, Activate_Toggled'Access, Widget);

      Gtk_New (Widget.Frame, -"Paths");
      Set_Sensitive (Widget.Frame, Widget.Active);
      Show_All (Widget.Frame);
      Widget.Get_Content_Area.Add (Widget.Frame);

      Gtk_New (Table, Rows => 3, Columns => 3, Homogeneous => False);
      Show_All (Table);
      Add (Widget.Frame, Table);

      Gtk_New (Label, -"Compiler path");
      Set_Alignment (Label, 1.0, 0.5);
      Show_All (Label);
      Attach (Table, Label, 0, 1, 0, 1);

      Gtk_New (Widget.Compiler_Entry);
      Set_Text (Widget.Compiler_Entry, Compiler_Path.Display_Full_Name);
      Show_All (Widget.Compiler_Entry);
      Attach (Table, Widget.Compiler_Entry, 1, 2, 0, 1);
      Set_Tooltip_Text
        (Widget.Compiler_Entry,
         -("This path will be used to spawn all code generation actions." &
           ASCII.LF &
           "In particular gnatmake, gprbuild, gcc, gdb, gcov" &
           " will be searched for in this path." &
           ASCII.LF &
           "To compile your project with a specific version of a compiler," &
           " please choose its bin directory here." &
           ASCII.LF &
           "This path should be different from the tools path." & ASCII.LF &
           ASCII.LF &
           "Note concerning the interaction with the remote mode:" &
           ASCII.LF &
           "In case you have defined a build server for your project, then " &
           "this path will be ignored, and the regular server's search path " &
           "will be used."));

      Gtk_New (Label, -"Tools path");
      Set_Alignment (Label, 1.0, 0.5);
      Show_All (Label);
      Attach (Table, Label, 0, 1, 1, 2);

      Gtk_New (Widget.Tools_Entry);
      Set_Text (Widget.Tools_Entry, Tools_Path.Display_Full_Name);
      Show_All (Widget.Tools_Entry);
      Attach (Table, Widget.Tools_Entry, 1, 2, 1, 2);
      Set_Tooltip_Text
        (Widget.Tools_Entry,
         -("This path will be used to spawn all actions not related to code" &
           " generation. These actions are (the list is not exclusive)" &
           " gnatcheck, gnatmetric, cross-reference generation." &
           ASCII.LF &
           "This path should be different from the compiler path." & ASCII.LF &
           ASCII.LF &
           "Note concerning the interaction with the remote mode:" &
           ASCII.LF &
           "In case you have defined a build server for your project, then " &
           "defining a tools path will make all actions enumerated above " &
           "execute locally using the tools path."));

      Dialog_Callback.Connect
        (Widget.Compiler_Entry, Signal_Changed, On_Changed'Access, Widget);
      Dialog_Callback.Connect
        (Widget.Tools_Entry, Signal_Changed, On_Changed'Access, Widget);

      for J in 1 .. 2 loop
         Gtk_New (Browse);
         Gtk_New_From_Icon_Name
           (Pix, "gps-open-file-symbolic", Icon_Size_Menu);
         Add (Browse, Pix);
         Set_Relief (Browse, Relief_None);
         Set_Border_Width (Browse, 0);
         Set_Can_Focus (Browse, False);
         Set_Can_Default (Browse, False);
         Show_All (Browse);
         Attach (Table, Browse, 2, 3, Guint (J - 1), Guint (J));
         Set_Tooltip_Text
           (Browse,
            -"Use this button to select the folder with a file explorer");

         if J = 1 then
            Entry_Callback.Connect
              (Browse, Signal_Clicked,
               On_Browse'Access,
               (E => Widget.Compiler_Entry,
                D => Widget));
         else
            Entry_Callback.Connect
              (Browse, Signal_Clicked,
               On_Browse'Access,
               (E => Widget.Tools_Entry,
                D => Widget));
         end if;
      end loop;

      Gtk_New
        (Check,
         -"Use the compiler in tools path to generate cross-reference files");
      Show_All (Check);
      Set_Active (Check, Widget.Xrefs_Subdir);
      Attach (Table, Check, 0, 2, 2, 3);
      Set_Tooltip_Text
        (Check,
         -("If checked, then GPS will automatically generate cross reference" &
           " files (.ali files) upon compilations. It will use the compiler" &
           " found in the tools path to generate those cross reference files" &
           " and will place them in a specific subdirectory, so as not to" &
           " interract with objects and cross reference files generated by" &
           " the regular compiler used for actually building the project." &
           ASCII.LF & ASCII.LF &
           "This functionnality is used to allow full GPS " &
           "functionalities with old compilers. If you need to use an old " &
           "compiler with your project, then you might consider using this " &
           "feature."));
      Dialog_Callback.Connect
        (Check, Signal_Toggled, Xrefs_Toggled'Access, Widget);

   end Gtk_New;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Widget : access Dialog_Record'Class) return Boolean
   is
   begin
      return Widget.Active;
   end Get_Active;

   --------------------------
   -- Get_Use_Xrefs_Subdir --
   --------------------------

   function Get_Use_Xrefs_Subdir
     (Widget : access Dialog_Record'Class) return Boolean
   is
   begin
      return Widget.Xrefs_Subdir;
   end Get_Use_Xrefs_Subdir;

   --------------------
   -- Get_Tools_Path --
   --------------------

   function Get_Tools_Path
     (Widget : access Dialog_Record'Class) return Virtual_File is
   begin
      return Create_From_UTF8 (Get_Text (Widget.Tools_Entry));
   end Get_Tools_Path;

   -----------------------
   -- Get_Compiler_Path --
   -----------------------

   function Get_Compiler_Path
     (Widget : access Dialog_Record'Class) return Virtual_File is
   begin
      return Create_From_UTF8 (Get_Text (Widget.Compiler_Entry));
   end Get_Compiler_Path;

end Toolchains_Dialog;
