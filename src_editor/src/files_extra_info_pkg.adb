-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2011, AdaCore              --
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

with Gtk;             use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Tooltips;    use Gtk.Tooltips;
with Gtkada.Handlers; use Gtkada.Handlers;

with Files_Extra_Info_Pkg.Callbacks; use Files_Extra_Info_Pkg.Callbacks;

with GPS.Intl;        use GPS.Intl;
with GPS.Kernel;      use GPS.Kernel;
with GPS.Kernel.MDI;  use GPS.Kernel.MDI;

package body Files_Extra_Info_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Files_Extra_Info : out Files_Extra_Info_Access;
      Handle           : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Files_Extra_Info := new Files_Extra_Info_Record;
      Files_Extra_Info_Pkg.Initialize (Files_Extra_Info, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Files_Extra_Info : access Files_Extra_Info_Record'Class;
      Handle           : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tooltips : Gtk_Tooltips;

   begin
      --  Gtk.Window.Initialize (Files_Extra_Info, Window_Toplevel);
      --  Set_Title (Files_Extra_Info, -"window1");
      --  Set_Policy (Files_Extra_Info, False, True, False);
      --  Set_Position (Files_Extra_Info, Win_Pos_None);
      --  Set_Modal (Files_Extra_Info, False);

      --  Gtk_New (Files_Extra_Info.Files_Frame, -"Files");
      --  Set_Shadow_Type (Files_Extra_Info.Files_Frame, Shadow_Etched_In);
      --  Add (Files_Extra_Info, Files_Extra_Info.Files_Frame);
      Gtk.Box.Initialize_Vbox (Files_Extra_Info);

      Gtk_New (Files_Extra_Info.Files_Table, 4, 3, False);
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
              0, 1, 0, 1, Fill, 0, 0, 0);

      Gtk_New (Files_Extra_Info.Directory_Label, -("Directory:"));
      Set_Alignment (Files_Extra_Info.Directory_Label, 0.0, 0.5);
      Set_Padding (Files_Extra_Info.Directory_Label, 0, 0);
      Set_Justify (Files_Extra_Info.Directory_Label, Justify_Center);
      Set_Line_Wrap (Files_Extra_Info.Directory_Label, False);
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Directory_Label,
              0, 1, 1, 2, Fill, 0, 0, 0);

      Gtk_New (Files_Extra_Info.Files_Entry);
      Set_Editable (Files_Extra_Info.Files_Entry, True);
      Set_Max_Length (Files_Extra_Info.Files_Entry, 0);
      Set_Text (Files_Extra_Info.Files_Entry, -"");
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Files_Entry,
              1, 3, 0, 1, Fill, 0, 0, 0);
      Tooltips := Get_Tooltips (Handle);
      Set_Tip (Tooltips, Files_Extra_Info.Files_Entry, -"File(s) to scan");

      Gtk_New (Files_Extra_Info.Directory_Entry);
      Set_Editable (Files_Extra_Info.Directory_Entry, True);
      Set_Max_Length (Files_Extra_Info.Directory_Entry, 0);
      Set_Text (Files_Extra_Info.Directory_Entry, -"");
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Directory_Entry,
              1, 3, 1, 2, Fill, 0, 0, 0);
      Set_Tip
        (Tooltips, Files_Extra_Info.Directory_Entry, -"Directory to scan");

      Gtk_New (Files_Extra_Info.Browse_Button, -"Browse");
      Set_Relief (Files_Extra_Info.Browse_Button, Relief_Normal);
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Browse_Button,
              1, 2, 3, 4, Fill, 0, 0, 0);
      Set_Tip
        (Tooltips, Files_Extra_Info.Browse_Button, -"Select a directory");
      Widget_Callback.Object_Connect
        (Files_Extra_Info.Browse_Button, Signal_Clicked,
         On_Browse_Button_Clicked'Access, Files_Extra_Info);

      Gtk_New (Files_Extra_Info.Subdirs_Check, -"Recursive Search");
      Set_Active (Files_Extra_Info.Subdirs_Check, False);
      Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Subdirs_Check,
              2, 3, 3, 4, Expand or Fill, 0, 0, 0);
   end Initialize;

end Files_Extra_Info_Pkg;
