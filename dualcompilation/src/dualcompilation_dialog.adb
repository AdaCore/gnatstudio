-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2008, AdaCore                  --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.File_Selector;      use Gtkada.File_Selector;

with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Intl;                  use GPS.Intl;
with Traces;                    use Traces;

package body Dualcompilation_Dialog is

   package Dualc_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Dualc_Dialog);

   package Entry_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Gtk_Entry);

   procedure Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      Dialog : Dualc_Dialog);
   --  Called when the check button is toggled

   procedure On_Browse
     (Button : access Gtk_Widget_Record'Class;
      GEntry : Gtk_Entry);
   --  Browse for a directory, then fill the GEntry

   -------------
   -- Toggled --
   -------------

   procedure Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      Dialog : Dualc_Dialog)
   is
   begin
      Dialog.Active := Get_Active (Gtk_Check_Button (Toggle));
      Set_Sensitive (Dialog.Frame, Dialog.Active);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Toggled;

   ---------------
   -- On_Browse --
   ---------------

   procedure On_Browse
     (Button : access Gtk_Widget_Record'Class;
      GEntry : Gtk_Entry)
   is
      Current_Dir : constant String :=
                      Get_Text (GEntry);
      Start_Dir   : Virtual_File;
   begin
      if Current_Dir /= "" then
         Start_Dir := Create (Current_Dir);

         if not Is_Directory (Start_Dir) then
            Start_Dir := GNATCOLL.VFS.Get_Current_Dir;
         end if;
      else
         Start_Dir := GNATCOLL.VFS.Get_Current_Dir;
      end if;

      declare
         Dir : constant GNATCOLL.VFS.Virtual_File :=
                 Select_Directory
                   (Base_Directory => Start_Dir,
                    Parent         => Gtk_Window (Get_Toplevel (Button)));
      begin
         if Dir /= No_File then
            Set_Text (GEntry, Full_Name (Dir).all);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Browse;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget        : out Dualc_Dialog;
      Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Active        : Boolean;
      Tools_Path    : String;
      Compiler_Path : String)
   is
      Check  : Gtk_Check_Button;
      Dead   : Gtk_Widget;
      Table  : Gtk_Table;
      Label  : Gtk_Label;
      Browse : Gtk_Button;
      Pix    : Gtk_Image;
      Tips   : constant Gtk_Tooltips := GPS.Kernel.Get_Tooltips (Kernel);
      pragma Unreferenced (Dead);

   begin
      Widget := new Dualc_Dialog_Record;
      Widget.Active := Active;

      Initialize
        (Widget,
         Title  => -"Dual compilation setup",
         Parent => GPS.Kernel.Get_Main_Window (Kernel),
         Flags  => Modal);

      Dead := Widget.Add_Button (Gtk.Stock.Stock_Ok, Gtk_Response_OK);
      Dead := Widget.Add_Button (Gtk.Stock.Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Check, -"Activate the dual compilation mode");
      Show_All (Check);
      Set_Active (Check, Widget.Active);
      Widget.Get_Vbox.Add (Check);
      Dualc_Callback.Connect
        (Check, Signal_Toggled, Toggled'Access, Widget);

      Gtk_New (Widget.Frame, -"Paths");
      Set_Sensitive (Widget.Frame, Widget.Active);
      Show_All (Widget.Frame);
      Widget.Get_Vbox.Add (Widget.Frame);

      Gtk_New (Table, Rows => 2, Columns => 4, Homogeneous => False);
      Show_All (Table);
      Add (Widget.Frame, Table);

      Gtk_New (Label, -"Tools path");
      Show_All (Label);
      Attach (Table, Label, 0, 1, 0, 1);

      Gtk_New (Label, -"Compiler path");
      Show_All (Label);
      Attach (Table, Label, 0, 1, 1, 2);

      Gtk_New (Widget.Tools_Entry);
      Set_Text (Widget.Tools_Entry, Tools_Path);
      Show_All (Widget.Tools_Entry);
      Attach (Table, Widget.Tools_Entry, 1, 2, 0, 1);
      Set_Tip
        (Tips, Widget.Tools_Entry,
         -("This path will be used to spawn all actions not related to code" &
           " generation. These utilities are (the list is not exclusive)" &
           " gnatcheck, gnatmetrics, cross-reference generation." &
           ASCII.LF & ASCII.LF &
           "Note that if both the remote mode and the dual compilation mode" &
           " are activated, then those actions will always be performed" &
           " locally."));

      Gtk_New (Widget.Compiler_Entry);
      Set_Text (Widget.Compiler_Entry, Compiler_Path);
      Show_All (Widget.Compiler_Entry);
      Attach (Table, Widget.Compiler_Entry, 1, 2, 1, 2);
      Set_Tip
        (Tips, Widget.Compiler_Entry,
         -("This path will be used to spawn all code generation actions." &
           ASCII.LF &
           "In particular gnatmake, gprbuild, gcc, gdb, gcov" &
           " will be searched for in this path." &
           ASCII.LF & ASCII.LF &
           "Note that if both the remote mode and the dual compilation mode" &
           " are activated, those actions will then be the only ones" &
           " executed on the remote host."));

      for J in 1 .. 2 loop
         Gtk_New (Browse);
         Gtk_New (Pix, Stock_Open, Icon_Size_Menu);
         Add (Browse, Pix);
         Set_Relief (Browse, Relief_None);
         Set_Border_Width (Browse, 0);
         Unset_Flags (Browse, Can_Focus or Can_Default);
         Show_All (Browse);
         Attach (Table, Browse, 2, 3, Guint (J - 1), Guint (J));
         Set_Tip
           (Tips, Browse,
            -"Use this button to select the folder with a file explorer");

         case J is
            when 1 =>
               Entry_Callback.Connect
                 (Browse, Signal_Clicked,
                  On_Browse'Access, Widget.Tools_Entry);
            when others =>
               Entry_Callback.Connect
                 (Browse, Signal_Clicked,
                  On_Browse'Access, Widget.Compiler_Entry);
         end case;
      end loop;
   end Gtk_New;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Widget : access Dualc_Dialog_Record'Class) return Boolean
   is
   begin
      return Widget.Active;
   end Get_Active;

   --------------------
   -- Get_Tools_Path --
   --------------------

   function Get_Tools_Path
     (Widget : access Dualc_Dialog_Record'Class) return String is
   begin
      if Widget.Active then
         return Get_Text (Widget.Tools_Entry);
      else
         return "";
      end if;
   end Get_Tools_Path;

   -----------------------
   -- Get_Compiler_Path --
   -----------------------

   function Get_Compiler_Path
     (Widget : access Dualc_Dialog_Record'Class) return String is
   begin
      if Widget.Active then
         return Get_Text (Widget.Compiler_Entry);
      else
         return "";
      end if;
   end Get_Compiler_Path;

end Dualcompilation_Dialog;
