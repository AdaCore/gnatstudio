-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2007                       --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;

with Gtkada.Dialogs;        use Gtkada.Dialogs;

with GPS.Intl;              use GPS.Intl;
with GVD.Process;           use GVD.Process;
with GVD.Types;             use GVD.Types;
with Traces;                use Traces;

package body GVD.Dialogs.Callbacks is

   use GVD;
   use Gtk.Arguments;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      Hide (Get_Toplevel (Object));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Close_Button_Clicked;

   -----------------------------
   -- On_Question_Yes_Clicked --
   -----------------------------

   procedure On_Question_Yes_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
      use type Gint_List.Glist;
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger :=
           Convert (Dialog.Main_Window, Debugger);

      begin
         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false

         Unregister_Dialog (Process);
         Set_Busy (Process, False);

         Send (Debugger,
               "y",
               Mode => GVD.Types.Visible,
               Empty_Buffer => False,
               Wait_For_Prompt => False);
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Question_Yes_Clicked;

   -----------------------------
   -- On_Question_No_Clicked --
   -----------------------------

   procedure On_Question_No_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
      use type Gint_List.Glist;
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger :=
           Convert (Dialog.Main_Window, Debugger);

      begin
         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false
         Unregister_Dialog (Process);
         Set_Busy (Process, False);

         Send (Debugger,
               "n",
               Mode => GVD.Types.Visible,
               Empty_Buffer => False,
               Wait_For_Prompt => False);
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Question_No_Clicked;

   ----------------------------
   -- On_Question_OK_Clicked --
   ----------------------------

   procedure On_Question_OK_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
      use type Gint_List.Glist;
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));

         Selection : constant Gint_List.Glist := Get_Selection (Dialog.List);
         S         : Unbounded_String;
         Tmp       : Gint_List.Glist := Gint_List.First (Selection);
         Button    : Message_Dialog_Buttons;
         pragma Unreferenced (Button);

         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger :=
           Convert (Dialog.Main_Window, Debugger);

      begin
         while Tmp /= Gint_List.Null_List loop
            Append (S, Get_Text (Dialog.List, Gint_List.Get_Data (Tmp), 0));
            Tmp := Gint_List.Next (Tmp);
         end loop;

         if Length (S) = 0 then
            Button :=
              Message_Dialog
                (-"You must select at least one of the choices",
                 Error, Button_OK);
            Emit_Stop_By_Name (Object, "clicked");
            return;
         end if;

         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false

         Unregister_Dialog (Process);
         Set_Busy (Process, False);

         Send (Debugger,
               To_String (S),
               Mode => GVD.Types.Visible,
               Empty_Buffer => False,
               Wait_For_Prompt => False);
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Question_OK_Clicked;

   -------------------------------
   -- On_Question_Close_Clicked --
   -------------------------------

   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog : constant Question_Dialog_Access :=
        Question_Dialog_Access (Get_Toplevel (Object));
   begin
      --  Send the interrupt signal to the debugger, so that it does not keep
      --  waiting for user input.
      Interrupt (Dialog.Debugger);

      --  Destroy the dialog, since we will have to recreate it anyway.
      Unregister_Dialog (Convert (Dialog.Main_Window, Dialog.Debugger));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Question_Close_Clicked;

end GVD.Dialogs.Callbacks;
