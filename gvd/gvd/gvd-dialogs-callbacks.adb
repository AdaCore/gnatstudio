-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
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

with Glib;                  use Glib;
with Gtk.Clist;             use Gtk.Clist;
with Gtk.Container;         use Gtk.Container;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Notebook;          use Gtk.Notebook;
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

package body GVD.Dialogs.Callbacks is

   use GVD;
   use Gtk.Arguments;

   ------------------------------
   -- On_Stack_Process_Stopped --
   ------------------------------

   procedure On_Stack_Process_Stopped
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      Update_Call_Stack (Object);
   end On_Stack_Process_Stopped;

   -----------------------------
   -- On_Task_List_Select_Row --
   -----------------------------

   procedure On_Task_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Since lists start at 0, increment the value.
      --  ??? This will only work with info tasks where there are no
      --  holes, but info threads can have holes when e.g threads are
      --  terminated, see 9019-008.

      Thread        : constant Gint := To_Gint (Params, 1) + 1;

      Top           : constant GVD_Dialog :=
        GVD_Dialog (Get_Toplevel (Object));

      Main_Window   : constant GVD_Main_Window :=
        GVD_Main_Window (Top.Main_Window);

      --  Get the process notebook from the main window which is associated
      --  with the task dialog (toplevel (object)).

      Notebook      : constant Gtk_Notebook := Main_Window.Process_Notebook;

      --  Get the current page in the process notebook.

      Process       : Debugger_Process_Tab :=
        Process_User_Data.Get (Get_Nth_Page
          (Notebook, Get_Current_Page (Notebook)));

   begin
      if Top = GVD_Dialog (Main_Window.Thread_Dialog) then
         Thread_Switch
           (Process.Debugger, Natural (Thread), Mode => GVD.Types.Visible);
      elsif Top = GVD_Dialog (Main_Window.Task_Dialog) then
         Task_Switch
           (Process.Debugger, Natural (Thread), Mode => GVD.Types.Visible);
      else
         raise Program_Error;
      end if;
   end On_Task_List_Select_Row;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      Hide (Get_Toplevel (Object));
   end On_Close_Button_Clicked;

   ----------------------------
   -- On_Question_OK_Clicked --
   ----------------------------

   procedure On_Question_OK_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      use type Gint_List.Glist;

      Dialog    : constant Question_Dialog_Access :=
        Question_Dialog_Access (Get_Toplevel (Object));

      Selection : constant Gint_List.Glist := Get_Selection (Dialog.List);
      S         : Unbounded_String;
      Tmp       : Gint_List.Glist := Gint_List.First (Selection);
      Button    : Message_Dialog_Buttons;

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

      --  The dialog will be unregistered by Send.

      Send (Dialog.Debugger,
            To_String (S),
            Mode => GVD.Types.Visible,
            Empty_Buffer => False,
            Wait_For_Prompt => False);
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
   end On_Question_Close_Clicked;

   ---------------------------------
   -- On_Replay_Selection_Clicked --
   ---------------------------------

   procedure On_Replay_Selection_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      History_Dialog : History_Dialog_Access :=
        History_Dialog_Access (Get_Toplevel (Object));
      Top  : constant GVD_Main_Window :=
        GVD_Main_Window (History_Dialog.Window);
      Page : constant Gtk_Widget :=
        Get_Nth_Page
          (Top.Process_Notebook, Get_Current_Page (Top.Process_Notebook));
      Tab  : constant Debugger_Process_Tab := Process_User_Data.Get (Page);
      List : constant Gtk_List :=
        History_Dialog_Access (Get_Toplevel (Object)).List;
      Item : Gtk_List_Item;

      use Widget_List;
      Selected   : Widget_List.Glist := First (Get_Children (List));
      Current    : Widget_List.Glist := Get_Selection (List);

   begin
      Freeze (History_Dialog);

      while Selected /= Null_List loop
         if Index (Current, Get_Data (Selected)) /= -1 then
            declare
               Command : String :=
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
   end On_Replay_Selection_Clicked;

   -------------------------------
   -- On_History_Cancel_Clicked --
   -------------------------------

   procedure On_History_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      Hide (Get_Toplevel (Object));
   end On_History_Cancel_Clicked;

   -----------------------------
   -- On_History_Help_Clicked --
   -----------------------------

   procedure On_History_Help_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      null;
   end On_History_Help_Clicked;

end GVD.Dialogs.Callbacks;
