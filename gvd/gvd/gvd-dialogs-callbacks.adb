-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
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

with Glib;                  use Glib;
with Gtk.Clist;             use Gtk.Clist;
with Gtk.Container;         use Gtk.Container;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.List;              use Gtk.List;
with Gtk.List_Item;         use Gtk.List_Item;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Dialogs;        use Gtkada.Dialogs;

with Odd_Intl;              use Odd_Intl;
with GVD.Process;           use GVD.Process;
with Debugger;              use Debugger;
with GVD.Main_Window;       use GVD.Main_Window;
with GVD.Types;             use GVD.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Regpat;           use GNAT.Regpat;
with Ada.Exceptions;        use Ada.Exceptions;
with Traces;                use Traces;
with Config;                use Config;

package body GVD.Dialogs.Callbacks is

   use GVD;
   use Gtk.Arguments;

   -----------------------------
   -- On_Task_List_Select_Row --
   -----------------------------

   procedure On_Task_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Index       : constant Gint := To_Gint (Params, 1);
      Str         : constant String :=
        Get_Text (Gtk_Clist (Object), Index, 0);
      Top         : constant GVD_Dialog := GVD_Dialog (Get_Toplevel (Object));
      Main_Window : constant GVD_Main_Window :=
        GVD_Main_Window (Top.Main_Window);
      Process     : constant Visual_Debugger :=
        Get_Current_Process (Main_Window);
      Matched     : Match_Array (0 .. 0);
      Info        : PD_Information_Array (1 .. Max_PD);
      Len         : Natural;
      PD_Dialog   : PD_Dialog_Access;

   begin
      if Process.Debugger = null then
         Hide (Top);
      end if;

      if Main_Window.Thread_Dialog /= null
        and then Top = GVD_Dialog (Main_Window.Thread_Dialog)
      then
         Match ("[0-9]+", Str, Matched);
         Thread_Switch
           (Process.Debugger,
            Natural'Value (Str (Matched (0).First .. Matched (0).Last)),
            Mode => GVD.Types.Visible);

      elsif Main_Window.Task_Dialog /= null
        and then Top = GVD_Dialog (Main_Window.Task_Dialog)
      then
         Task_Switch
           (Process.Debugger, Natural (Index) + 1, Mode => GVD.Types.Visible);

      elsif Main_Window.PD_Dialog /= null
        and then Top = GVD_Dialog (Main_Window.PD_Dialog)
      then
         Match ("(0x)?[0-9a-fA-F]+", Str, Matched);

         --  ??? The Command_Type was changed from Visible to Hidden
         --  (revision 1.62) because the debugger is still
         --  processing the previous command (Info_PD), and there is
         --  an assertion failure in Debugger.Send_Full. This does
         --  not happen for Task_Switch or Thread_Switch (above)

         PD_Switch
           (Process.Debugger,
            Str (Matched (0).First .. Matched (0).Last),
            Mode => GVD.Types.Hidden);

         --  After switching to a new protection domain, we want the
         --  PD dialog to reflect that change immediately

         Info_PD (Process.Debugger, Info, Len);
         Freeze (Gtk_Clist (Object));

         PD_Dialog := PD_Dialog_Access (Main_Window.PD_Dialog);
         Update_PD (PD_Dialog, Info (1 .. Len));
         Handler_Block (Object, PD_Dialog.Select_Row_Id);
         Select_Row (Gtk_Clist (Object), Index, 0);
         Handler_Unblock (Object, PD_Dialog.Select_Row_Id);
         Thaw (Gtk_Clist (Object));
      else
         raise Program_Error;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Task_List_Select_Row;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      Hide (Get_Toplevel (Object));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Question_Close_Clicked;

   ---------------------------------
   -- On_Replay_Selection_Clicked --
   ---------------------------------

   procedure On_Replay_Selection_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      History_Dialog : constant History_Dialog_Access :=
        History_Dialog_Access (Get_Toplevel (Object));
      Top  : constant GVD_Main_Window :=
        GVD_Main_Window (History_Dialog.Window);
      Tab  : constant Visual_Debugger := Get_Current_Process (Top);
      List : constant Gtk_List :=
        History_Dialog_Access (Get_Toplevel (Object)).List;
      Item : Gtk_List_Item;

      use Widget_List;
      Selected   : Widget_List.Glist := First (Get_Children (List));
      Current    : constant Widget_List.Glist := Get_Selection (List);

   begin
      Freeze (History_Dialog);

      while Selected /= Null_List loop
         if Index (Current, Get_Data (Selected)) /= -1 then
            declare
               Command : constant String :=
                 Get (Gtk_Label
                       (Get_Data
                         (Children
                           (Gtk_Container (Get_Data (Selected))))));
            begin
               Process_User_Command
                 (Tab,
                  Command,
                  Output_Command => True,
                  Mode           => GVD.Types.User);
               Wait_User_Command (Tab.Debugger);
               Gtk_New (Item, Label => Command);
               Show (Item);
               Add (History_Dialog.List, Item);
            end;
         end if;

         Selected := Next (Selected);
      end loop;

      Thaw (History_Dialog);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Replay_Selection_Clicked;

   -------------------------------
   -- On_History_Cancel_Clicked --
   -------------------------------

   procedure On_History_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      Hide (Get_Toplevel (Object));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_History_Cancel_Clicked;

end GVD.Dialogs.Callbacks;
