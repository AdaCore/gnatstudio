-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;                 use Glib;
with Gtk;                  use Gtk;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Hbutton_Box;      use Gtk.Hbutton_Box;

with Glide_Intl;           use Glide_Intl;

package body Log_Editor_Window_Pkg is

   -------------------
   -- Add_File_Name --
   -------------------

   procedure Add_File_Name
     (Log_Editor_Window : access Log_Editor_Window_Record'Class;
      File_Name         : String)
   is
      Label : Gtk_Label;
   begin
      if not Is_Empty (Log_Editor_Window.Files) then
         Set_Text (Log_Editor_Window.Files_Label, -"Edit log for files:");
      end if;

      Append (Log_Editor_Window.Files, File_Name);

      Gtk_New (Label, File_Name);
      Set_Alignment (Log_Editor_Window.Files_Label, 0.0, 0.5);
      Set_Padding (Label, 3, 3);
      Set_Justify (Label, Justify_Left);
      Set_Line_Wrap (Label, False);
      Pack_Start (Log_Editor_Window.Labels_Vbox, Label, False, False, 0);
   end Add_File_Name;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Log_Editor_Window : access Log_Editor_Window_Record'Class;
      Text              : String)
   is
      Position : Gint := 0;
   begin
      Delete_Text (Log_Editor_Window.Log_Text);
      Insert_Text (Log_Editor_Window.Log_Text, Text, Position);
   end Set_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Log_Editor_Window : access Log_Editor_Window_Record'Class)
     return String
   is
   begin
      return Get_Chars (Log_Editor_Window.Log_Text);
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Log_Editor_Window : out Log_Editor_Window_Access) is
   begin
      Log_Editor_Window := new Log_Editor_Window_Record;
      Log_Editor_Window_Pkg.Initialize (Log_Editor_Window);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Log_Editor_Window : access Log_Editor_Window_Record'Class)
   is
      Vbox1 : Gtk_Vbox;
      Vbox2 : Gtk_Vbox;
      Hbox1 : Gtk_Hbox;
      Hbox2 : Gtk_Hbox;

      Hbuttonbox1     : Gtk_Hbutton_Box;
      Scrolledwindow1 : Gtk_Scrolled_Window;
   begin
      Gtk.Window.Initialize (Log_Editor_Window, Window_Toplevel);
      Set_Title (Log_Editor_Window, -"Log Editor");
      Set_Policy (Log_Editor_Window, False, True, False);
      Set_Position (Log_Editor_Window, Win_Pos_None);
      Set_Modal (Log_Editor_Window, False);
      Set_Default_Size (Log_Editor_Window, 400, 300);

      Gtk_New_Vbox (Vbox1, False, 0);
      Add (Log_Editor_Window, Vbox1);

      Gtk_New_Hbox (Hbox2, False, 0);
      Pack_Start (Vbox1, Hbox2, False, False, 3);

      Gtk_New_Vbox (Vbox2, False, 0);
      Pack_Start (Hbox2, Vbox2, False, False, 3);

      Gtk_New (Log_Editor_Window.Files_Label, -"Edit log for file:");
      Set_Alignment (Log_Editor_Window.Files_Label, 0.5, 0.5);
      Set_Padding (Log_Editor_Window.Files_Label, 3, 3);
      Set_Justify (Log_Editor_Window.Files_Label, Justify_Center);
      Set_Line_Wrap (Log_Editor_Window.Files_Label, False);
      Pack_Start (Vbox2, Log_Editor_Window.Files_Label, False, False, 0);

      Gtk_New_Vbox (Log_Editor_Window.Labels_Vbox, False, 0);
      Pack_Start (Hbox2, Log_Editor_Window.Labels_Vbox, False, False, 0);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Vbox1, Hbox1, True, True, 3);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1, Policy_Never, Policy_Automatic);
      Pack_Start (Hbox1, Scrolledwindow1, True, True, 3);

      Gtk_New (Log_Editor_Window.Log_Text);
      Set_Editable (Log_Editor_Window.Log_Text, True);
      Add (Scrolledwindow1, Log_Editor_Window.Log_Text);

      Gtk_New (Hbuttonbox1);
      Set_Spacing (Hbuttonbox1, 30);
      Set_Layout (Hbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Hbuttonbox1, 7, 0);
      Pack_Start (Vbox1, Hbuttonbox1, False, False, 3);

      Gtk_New (Log_Editor_Window.Ok_Button, -"Commit");
      Set_Relief (Log_Editor_Window.Ok_Button, Relief_Normal);
      Set_Flags (Log_Editor_Window.Ok_Button, Can_Default);

      Add (Hbuttonbox1, Log_Editor_Window.Ok_Button);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Log_Editor_Window : access Log_Editor_Window_Record'Class)
   is
   begin
      Free (Log_Editor_Window.Files);
      Destroy (Log_Editor_Window);
   end Close;

end Log_Editor_Window_Pkg;
