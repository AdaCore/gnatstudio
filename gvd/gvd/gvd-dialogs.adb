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

with Interfaces.C.Strings;
with Interfaces.C;          use Interfaces.C;

with Glib;                  use Glib;

with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Widget;            use Gtk.Widget;
with Gtk;                   use Gtk;

with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtkada.Types;          use Gtkada.Types;

with Basic_Types;           use Basic_Types;
with Config;                use Config;
with GPS.Intl;              use GPS.Intl;
pragma Elaborate_All (GPS.Intl);
with GVD.Call_Stack;        use GVD.Call_Stack;
with GVD.Callbacks;         use GVD.Callbacks;
with GVD.Dialogs.Callbacks; use GVD.Dialogs.Callbacks;
with GVD.Process;           use GVD.Process;
with GVD;                   use GVD;
with GVD_Module;            use GVD_Module;
with Process_Proxies;       use Process_Proxies;

package body GVD.Dialogs is

   Question_Titles : constant Chars_Ptr_Array := "" + (-"Choice");

   procedure Initialize
     (Dialog      : access GVD_Dialog_Record'Class;
      Title       : String;
      Main_Window : Gtk_Window);
   --  Create a standard dialog.

   procedure Initialize_Task_Thread
     (Dialog : access GVD_Dialog_Record'Class);
   --  Common initializations between task and thread dialogs.

   function Delete_Dialog
     (Dialog : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user deletes a dialog by clicking on the small
   --  button in the title bar of the window.

   procedure Update_Task_Thread
     (Dialog   : access GVD_Dialog_Record'Class;
      Info     : in out Thread_Information_Array);
   --  Common operations between task and thread dialogs.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Gtk_Window) is
   begin
      Task_Dialog := new Task_Dialog_Record;
      Initialize (Task_Dialog, Main_Window);
   end Gtk_New;

   procedure Gtk_New
     (Thread_Dialog : out Thread_Dialog_Access;
      Main_Window   : Gtk_Window) is
   begin
      Thread_Dialog := new Thread_Dialog_Record;
      Initialize (Thread_Dialog, Main_Window);
   end Gtk_New;

   procedure Gtk_New
     (PD_Dialog  : out PD_Dialog_Access;
      Main_Window : Gtk_Window) is
   begin
      PD_Dialog := new PD_Dialog_Record;
      Initialize (PD_Dialog, Main_Window);
   end Gtk_New;

   procedure Gtk_New
     (Question_Dialog            : out Question_Dialog_Access;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "") is
   begin
      Question_Dialog := new Question_Dialog_Record;

      Initialize
        (Question_Dialog, Main_Window, Debugger,
         Multiple_Selection_Allowed, Questions,
         Question_Description);
   end Gtk_New;

   ------------------------
   -- Update_Task_Thread --
   ------------------------

   procedure Update_Task_Thread
     (Dialog : access GVD_Dialog_Record'Class;
      Info   : in out Thread_Information_Array)
   is
      Num_Columns : Thread_Fields;
      Row         : Gint;
      pragma Unreferenced (Row);

   begin
      if Dialog.List = null and then Info'Length > 0 then
         Num_Columns := Info (Info'First).Num_Fields;
         Gtk_New
           (Dialog.List, Gint (Num_Columns), Info (Info'First).Information);
         Show_All (Dialog.List);
         Dialog.Select_Row_Id := Widget_Callback.Connect
           (Dialog.List,
            "select_row",
            On_Task_List_Select_Row'Access);
         Add (Dialog.Scrolledwindow1, Dialog.List);
      end if;

      if Info'Length > 0 then
         Freeze (Dialog.List);
         Clear (Dialog.List);

         for J in Info'First + 1 .. Info'Last loop
            Row := Append (Dialog.List, Info (J).Information);
         end loop;

         Row := Columns_Autosize (Dialog.List);
         Thaw (Dialog.List);
      end if;

      Free (Info);
   end Update_Task_Thread;

   -----------------
   --  Update_PD  --
   -----------------

   procedure Update_PD
     (Dialog   : access GVD_Dialog_Record'Class;
      Info     : in out PD_Information_Array) is
   begin
      Update_Task_Thread (Dialog, Info);
   end Update_PD;

   ------------
   -- Update --
   ------------

   procedure Update
     (Task_Dialog : access Task_Dialog_Record;
      Debugger    : access Glib.Object.GObject_Record'Class)
   is
      Process : constant Process_Proxy_Access :=
        Get_Process (Visual_Debugger (Debugger).Debugger);
      Info    : Thread_Information_Array (1 .. Max_Tasks);
      Len     : Natural;

   begin
      if not Visible_Is_Set (Task_Dialog) then
         return;
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         if Task_Dialog.List /= null then
            Thaw (Task_Dialog.List);
         end if;

         return;
      end if;

      --  Read the information from the debugger
      Info_Tasks (Visual_Debugger (Debugger).Debugger, Info, Len);
      Update_Task_Thread (Task_Dialog, Info (1 .. Len));
   end Update;

   procedure Update
     (Thread_Dialog : access Thread_Dialog_Record;
      Debugger      : access Glib.Object.GObject_Record'Class)
   is
      Process : constant Process_Proxy_Access :=
        Get_Process (Visual_Debugger (Debugger).Debugger);
      Info    : Thread_Information_Array (1 .. Max_Tasks);
      Len     : Natural;

   begin
      if not Visible_Is_Set (Thread_Dialog) then
         return;
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         if Thread_Dialog.List /= null then
            Thaw (Thread_Dialog.List);
         end if;

         return;
      end if;

      --  Read the information from the debugger
      Info_Threads (Visual_Debugger (Debugger).Debugger, Info, Len);
      Update_Task_Thread (Thread_Dialog, Info (1 .. Len));
   end Update;

   procedure Update
     (PD_Dialog : access PD_Dialog_Record;
      Debugger   : access Glib.Object.GObject_Record'Class)
   is
      Process : constant Process_Proxy_Access :=
        Get_Process (Visual_Debugger (Debugger).Debugger);
      Info    : PD_Information_Array (1 .. Max_PD);
      Len     : Natural;

   begin
      if not Visible_Is_Set (PD_Dialog) then
         return;
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         if PD_Dialog.List /= null then
            Thaw (PD_Dialog.List);
         end if;

         return;
      end if;

      --  Read the information from the debugger
      Info_PD (Visual_Debugger (Debugger).Debugger, Info, Len);
      Update_PD (PD_Dialog, Info (1 .. Len));
   end Update;

   -----------------------
   -- Update_Call_Stack --
   -----------------------

   procedure Update_Call_Stack
     (Debugger : access Glib.Object.GObject_Record'Class)
   is
      Tab : constant Visual_Debugger := Visual_Debugger (Debugger);
   begin
      if Tab.Stack = null or else Tab.Debugger = null then
         return;
      end if;

      Update (Tab.Stack, Tab.Debugger);
   end Update_Call_Stack;

   -----------------------------
   -- On_Task_Process_Stopped --
   -----------------------------

   procedure On_Task_Process_Stopped
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      Tab         : constant Visual_Debugger := Visual_Debugger (Widget);
      Task_Dialog : constant Task_Dialog_Access :=
        Task_Dialog_Access (Get_Task_Dialog (Tab.Window.Kernel));

   begin
      if Task_Dialog /= null then
         Update (Task_Dialog, Tab);
      end if;
   end On_Task_Process_Stopped;

   -------------------------------
   -- On_Thread_Process_Stopped --
   -------------------------------

   procedure On_Thread_Process_Stopped
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      Tab           : constant Visual_Debugger := Visual_Debugger (Widget);
      Thread_Dialog : constant Thread_Dialog_Access :=
        Thread_Dialog_Access (Get_Thread_Dialog (Tab.Window.Kernel));

   begin
      if Thread_Dialog /= null then
         Update (Thread_Dialog, Tab);
      end if;
   end On_Thread_Process_Stopped;

   ----------------------------
   -- On_PD_Process_Stopped --
   ----------------------------

   procedure On_PD_Process_Stopped
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      Tab       : constant Visual_Debugger := Visual_Debugger (Widget);
      PD_Dialog : constant PD_Dialog_Access :=
        PD_Dialog_Access (Get_PD_Dialog (Tab.Window.Kernel));

   begin
      if PD_Dialog /= null then
         Update (PD_Dialog, Tab);
      end if;
   end On_PD_Process_Stopped;

   ------------------------------
   -- On_Stack_Process_Stopped --
   ------------------------------

   procedure On_Stack_Process_Stopped
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      Tab : constant Visual_Debugger := Visual_Debugger (Widget);
   begin
      Update_Call_Stack (Tab);
   end On_Stack_Process_Stopped;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog      : access GVD_Dialog_Record'Class;
      Title       : String;
      Main_Window : Gtk_Window) is
   begin
      Gtk.Dialog.Initialize (Dialog, Title, Main_Window, 0);
      Dialog.Main_Window := Main_Window;

      Set_Policy (Dialog, False, True, False);
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, -1, 200);

      Dialog.Vbox1 := Get_Vbox (Dialog);
      Set_Homogeneous (Dialog.Vbox1, False);
      Set_Spacing (Dialog.Vbox1, 0);

      Dialog.Hbox1 := Get_Action_Area (Dialog);
      Set_Border_Width (Dialog.Hbox1, 5);
      Set_Homogeneous (Dialog.Hbox1, True);
      Set_Spacing (Dialog.Hbox1, 5);

      Gtk_New (Dialog.Hbuttonbox1);
      Pack_Start (Dialog.Hbox1, Dialog.Hbuttonbox1, True, True, 0);
      Set_Spacing (Dialog.Hbuttonbox1, 10);
      Set_Child_Size (Dialog.Hbuttonbox1, 85, 27);
      Set_Layout (Dialog.Hbuttonbox1, Buttonbox_Spread);

      Gtk_New_From_Stock (Dialog.Close_Button, Stock_Close);
      Add (Dialog.Hbuttonbox1, Dialog.Close_Button);

      Return_Callback.Connect
        (Dialog, "delete_event",
         Return_Callback.To_Marshaller (Delete_Dialog'Access));
   end Initialize;

   procedure Initialize_Task_Thread
     (Dialog : access GVD_Dialog_Record'Class) is
   begin
      Button_Callback.Connect
        (Dialog.Close_Button, "clicked",
         Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));

      Set_Default_Size (Dialog, 400, 200);
      Gtk_New (Dialog.Scrolledwindow1);
      Pack_Start
        (Dialog.Vbox1, Dialog.Scrolledwindow1, True, True, 0);
      Set_Policy
        (Dialog.Scrolledwindow1,
         Policy_Automatic,
         Policy_Automatic);
      --  We can't create the clist here, since we don't know yet how many
      --  columns there will be. This will be done on the first call to update
   end Initialize_Task_Thread;

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Gtk_Window) is
   begin
      Initialize (Task_Dialog, -"Task Status", Main_Window);
      Initialize_Task_Thread (Task_Dialog);
   end Initialize;

   procedure Initialize
     (Thread_Dialog : access Thread_Dialog_Record'Class;
      Main_Window   : Gtk_Window) is
   begin
      Initialize (Thread_Dialog, -"Thread Status", Main_Window);
      Initialize_Task_Thread (Thread_Dialog);
   end Initialize;

   procedure Initialize
     (PD_Dialog  : access PD_Dialog_Record'Class;
      Main_Window : Gtk_Window) is
   begin
      Initialize (PD_Dialog, -"Protection Domains", Main_Window);
      Initialize_Task_Thread (PD_Dialog);
   end Initialize;

   procedure Initialize
     (Question_Dialog            : access Question_Dialog_Record'Class;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "")
   is
      Temp      : Chars_Ptr_Array (0 .. 1);
      Row       : Gint;
      pragma Unreferenced (Row);

      Width     : Gint;
      OK_Button : Gtk_Button;
      Label     : Gtk_Label;

   begin
      Initialize (Question_Dialog, "Question", Main_Window);
      Question_Dialog.Debugger := Debugger;

      Widget_Callback.Connect
        (Question_Dialog.Close_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Question_Close_Clicked'Access));

      if Question_Description /= "" then
         Gtk_New (Label, Question_Description);
         Pack_Start (Question_Dialog.Vbox1, Label, False, False, 5);
      end if;

      --  Detect if only choices are "Yes" and "No"
      if Questions'Length = 2
        and then Questions (Questions'First).Choice /= null
        and then Questions (Questions'Last).Choice /= null
        and then
          ((Questions (Questions'Last).Choice.all = "y"
            and then Questions (Questions'First).Choice.all = "n")
          or else
           (Questions (Questions'Last).Choice.all = "n"
            and then Questions (Questions'First).Choice.all = "y"))
      then
         Set_Default_Size (Question_Dialog, 100, 50);
         Gtk_New_From_Stock (OK_Button, Stock_Yes);
         Add (Question_Dialog.Hbuttonbox1, OK_Button);
         Widget_Callback.Connect
           (OK_Button,
            "clicked",
            On_Question_Yes_Clicked'Access);
         Grab_Focus (OK_Button);

         Gtk_New_From_Stock (OK_Button, Stock_No);
         Add (Question_Dialog.Hbuttonbox1, OK_Button);
         Widget_Callback.Connect
           (OK_Button,
            "clicked",
            On_Question_No_Clicked'Access);

         Ref (Question_Dialog.Close_Button);
         Remove (Question_Dialog.Hbuttonbox1, Question_Dialog.Close_Button);

      else
         Gtk_New (Question_Dialog.Scrolledwindow1);
         Pack_Start
           (Question_Dialog.Vbox1, Question_Dialog.Scrolledwindow1,
            True, True, 0);
         Set_Policy
           (Question_Dialog.Scrolledwindow1, Policy_Automatic,
            Policy_Automatic);

         --  Make sure the Cancel button is on the right, for homogeneity
         Ref (Question_Dialog.Close_Button);
         Remove (Question_Dialog.Hbuttonbox1, Question_Dialog.Close_Button);
         Gtk_New_From_Stock (OK_Button, Stock_Ok);
         Add (Question_Dialog.Hbuttonbox1, OK_Button);
         Widget_Callback.Connect
           (OK_Button,
            "clicked",
            On_Question_OK_Clicked'Access);
         Add (Question_Dialog.Hbuttonbox1, Question_Dialog.Close_Button);
         Unref (Question_Dialog.Close_Button);

         Gtk_New (Question_Dialog.List, 2, Question_Titles);
         Add (Question_Dialog.Scrolledwindow1, Question_Dialog.List);

         if Multiple_Selection_Allowed then
            Set_Selection_Mode (Question_Dialog.List, Selection_Multiple);
         else
            Set_Selection_Mode (Question_Dialog.List, Selection_Single);
         end if;

         for J in Questions'Range loop
            Temp (0) := C.Strings.New_String (Questions (J).Choice.all);
            Temp (1) := C.Strings.New_String (Questions (J).Description.all);
            Row := Append (Question_Dialog.List, Temp);
            Free (Temp);
         end loop;

         Set_Column_Width
           (Question_Dialog.List, 0,
            Optimal_Column_Width (Question_Dialog.List, 0));
         Set_Column_Width
           (Question_Dialog.List, 1,
            Gint'Min (Optimal_Column_Width (Question_Dialog.List, 1),
                      Max_Column_Width));
         Set_Column_Auto_Resize (Question_Dialog.List, 0, True);
         Set_Column_Auto_Resize (Question_Dialog.List, 1, True);

         Width := Optimal_Column_Width (Question_Dialog.List, 0)
           + Optimal_Column_Width (Question_Dialog.List, 1)
           + 20;
         Set_Default_Size (Question_Dialog, Gint'Min (Width, 500), 200);
      end if;

      Register_Dialog (Convert (Main_Window, Debugger), Question_Dialog);
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Questions : in out Question_Array) is
   begin
      for Q in Questions'Range loop
         Free (Questions (Q).Choice);
         Free (Questions (Q).Description);
      end loop;
   end Free;

   -------------------
   -- Delete_Dialog --
   -------------------

   function Delete_Dialog
     (Dialog : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Hide (Dialog);
      return True;
   end Delete_Dialog;

end GVD.Dialogs;
