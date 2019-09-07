------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Glib;                           use Glib;
with Gtk;                            use Gtk;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Image;                      use Gtk.Image;
with Gtkada.Handlers;                use Gtkada.Handlers;

with Files_Extra_Info_Pkg.Callbacks; use Files_Extra_Info_Pkg.Callbacks;

with GPS.Intl;                       use GPS.Intl;

package body Files_Extra_Info_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Files_Extra_Info : out Files_Extra_Info_Access;
      Handle           : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Files_Extra_Info := new Files_Extra_Info_Record;
      Files_Extra_Info_Pkg.Initialize (Files_Extra_Info, Handle, 0);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Files_Extra_Info : access Files_Extra_Info_Record'Class;
      Handle           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Start_Row_Number : Glib.Guint)
   is
      pragma Unreferenced (Handle);

      Icon     : Gtk_Image;
      Hbox     : Gtk_Hbox;
      Start    : Guint renames Start_Row_Number;

   begin
      Gtk.Box.Initialize_Vbox (Files_Extra_Info);

      Gtk_New (Files_Extra_Info.Files_Table, 3 + Start, 2, False);
      Set_Border_Width (Files_Extra_Info.Files_Table, 5);
      Set_Row_Spacings (Files_Extra_Info.Files_Table, 5);
      Set_Col_Spacings (Files_Extra_Info.Files_Table, 5);
      --  Add (Files_Extra_Info.Files_Frame, Files_Extra_Info.Files_Table);
      Pack_Start (Files_Extra_Info, Files_Extra_Info.Files_Table);

      Gtk_New (Files_Extra_Info.Files_Label, -("Files:"));
      Set_Alignment (Files_Extra_Info.Files_Label, 0.0, 0.5);
      Set_Padding (Files_Extra_Info.Files_Label, 0, 0);
      Set_Justify (Files_Extra_Info.Files_Label, Justify_Center);
      Set_Line_Wrap (Files_Extra_Info.Files_Label, False);
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Files_Label,
              0, 1, Start, Start + 1, Fill, 0, 0, 0);

      Gtk_New (Files_Extra_Info.Files_Entry);
      Set_Editable (Files_Extra_Info.Files_Entry, True);
      Set_Max_Length (Files_Extra_Info.Files_Entry, 0);
      Set_Text (Files_Extra_Info.Files_Entry, "*");
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Files_Entry,
              1, 2, Start, Start + 1, Fill, 0, 0, 0);
      Set_Tooltip_Text (Files_Extra_Info.Files_Entry, -"File(s) to scan");

      Gtk_New (Files_Extra_Info.Directory_Label, -("Directory:"));
      Set_Alignment (Files_Extra_Info.Directory_Label, 0.0, 0.5);
      Set_Padding (Files_Extra_Info.Directory_Label, 0, 0);
      Set_Justify (Files_Extra_Info.Directory_Label, Justify_Center);
      Set_Line_Wrap (Files_Extra_Info.Directory_Label, False);
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Directory_Label,
              0, 1, Start + 1, Start + 2, Fill, 0, 0, 0);

      Gtk_New_Hbox (Hbox);
      Attach (Files_Extra_Info.Files_Table, Hbox,
              1, 2, Start + 1, Start + 2, Fill, 0, 0, 0);

      Gtk_New (Files_Extra_Info.Directory_Entry);
      Set_Editable (Files_Extra_Info.Directory_Entry, True);
      Set_Max_Length (Files_Extra_Info.Directory_Entry, 0);
      Set_Text (Files_Extra_Info.Directory_Entry, -"");
      Set_Tooltip_Text
        (Files_Extra_Info.Directory_Entry, -"Directory to scan");
      Pack_Start (Hbox, Files_Extra_Info.Directory_Entry, True, True);

      Gtk_New_From_Icon_Name
        (Icon, "gps-emblem-directory-symbolic", Icon_Size_Button);
      Gtk.Button.Gtk_New (Files_Extra_Info.Browse_Button);
      Files_Extra_Info.Browse_Button.Add (Icon);
      Pack_Start (Hbox, Files_Extra_Info.Browse_Button, False, False);
      Set_Tooltip_Text
        (Files_Extra_Info.Browse_Button, -"Select a directory");
      Widget_Callback.Object_Connect
        (Files_Extra_Info.Browse_Button, Signal_Clicked,
         On_Browse_Button_Clicked'Access, Files_Extra_Info);

      Gtk_New (Files_Extra_Info.Subdirs_Check, -"Recursive Search");
      Set_Active (Files_Extra_Info.Subdirs_Check, False);
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Subdirs_Check,
              1, 2, Start + 2, Start + 3, Expand or Fill, 0, 0, 0);
   end Initialize;

end Files_Extra_Info_Pkg;
