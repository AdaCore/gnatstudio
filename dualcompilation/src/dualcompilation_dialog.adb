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

with System.OS_Lib;             use System.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with GPS.Intl;                  use GPS.Intl;

package body Dualcompilation_Dialog is

   package My_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Dualc_Dialog);

   procedure Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      Dialog : Dualc_Dialog);
   --  Called when the check button is toggled

   procedure Toggled
     (Toggle : access Gtk_Widget_Record'Class;
      Dialog : Dualc_Dialog)
   is
   begin
      Dialog.Active := Get_Active (Gtk_Check_Button (Toggle));
      Set_Sensitive (Dialog.Frame, Dialog.Active);
   end Toggled;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget        : out Dualc_Dialog;
      Parent        : access GObject_Record'Class;
      Active        : Boolean;
      Tools_Path    : String;
      Compiler_Path : String)
   is
      Check : Gtk_Check_Button;
      Dead  : Gtk_Widget;
      Table : Gtk_Table;
      Label : Gtk_Label;
      pragma Unreferenced (Dead);
   begin
      Widget := new Dualc_Dialog_Record;
      Widget.Active := Active;

      Initialize (Widget,
                  Title => -"Dual compilation setup",
                  Parent => Gtk_Window (Get_Toplevel (Gtk_Widget (Parent))),
                  Flags  => Modal);

      Dead := Widget.Add_Button (Gtk.Stock.Stock_Ok, Gtk_Response_OK);
      Dead := Widget.Add_Button (Gtk.Stock.Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Check, -"Activate the dual compilation mode");
      Show_All (Check);
      Set_Active (Check, Widget.Active);
      Widget.Get_Vbox.Add (Check);
      My_Callback.Connect
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

      Gtk_New (Widget.Tools_Entry);

      if Tools_Path = "" then
         declare
            Path : String_Access := Locate_Exec_On_Path ("gnatmake");
         begin
            Set_Text (Widget.Tools_Entry, Dir_Name (Path.all));
            Free (Path);
         end;
      else
         Set_Text (Widget.Tools_Entry, Tools_Path);
      end if;

      Show_All (Widget.Tools_Entry);
      Attach (Table, Widget.Tools_Entry, 1, 2, 0, 1);

      Gtk_New (Label, -"Compiler path");
      Show_All (Label);
      Attach (Table, Label, 0, 1, 1, 2);

      Gtk_New (Widget.Compiler_Entry);
      Set_Text (Widget.Compiler_Entry, Compiler_Path);
      Show_All (Widget.Compiler_Entry);
      Attach (Table, Widget.Compiler_Entry, 1, 2, 1, 2);
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
