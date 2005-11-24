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

with Ada.Exceptions;        use Ada.Exceptions;
with Basic_Types;           use Basic_Types;
with Config;                use Config;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNAT.Regpat;           use GNAT.Regpat;
with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Main_Window;       use GPS.Main_Window;
with GPS.Intl;              use GPS.Intl;
pragma Elaborate_All (GPS.Intl);
with GVD.Callbacks;         use GVD.Callbacks;
with GVD.Dialogs.Callbacks; use GVD.Dialogs.Callbacks;
with GVD.Process;           use GVD.Process;
with GVD.Types;             use GVD.Types;
with GVD.Views;             use GVD.Views;
with GVD;                   use GVD;
with GVD_Module;            use GVD_Module;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;
with Gtk.Tree_Store;        use Gtk.Tree_Store;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Widget;            use Gtk.Widget;
with Gtk;                   use Gtk;
with Gtkada.Dialogs;        use Gtkada.Dialogs;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtkada.Types;          use Gtkada.Types;
with GUI_Utils;             use GUI_Utils;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C;          use Interfaces.C;
with Process_Proxies;       use Process_Proxies;
with Traces;                use Traces;

package body GVD.Dialogs is
   Me : constant Debug_Handle := Create ("GVD.Dialogs");

   Question_Titles : constant Gtkada.Types.Chars_Ptr_Array :=
     "" + (-"Choice");

   -----------------
   -- Thread View --
   -----------------

   type Thread_View_Record is new Scrolled_Views.Process_View_Record with
      record
         Tree : Gtk.Tree_View.Gtk_Tree_View;
      end record;
   type Thread_View is access all Thread_View_Record'Class;

   procedure Initialize
     (Thread : access Thread_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   function Get_Thread_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window;
   procedure Set_Thread_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window);
   procedure On_Attach
     (Thread  : access Thread_View_Record;
      Process : access Visual_Debugger_Record'Class);
   --  See description in GVD.Generic_View

   package Thread_Views is new Scrolled_Views.Simple_Views
     (Module_Name        => "Thread_View",
      View_Name          => -"Threads",
      Formal_View_Record => Thread_View_Record,
      Get_View           => Get_Thread_View,
      Set_View           => Set_Thread_View,
      Initialize         => Initialize);

   procedure Update_Threads (Thread : access Gtk_Widget_Record'Class);
   --  Update the contents of the thread dialog.

   procedure On_Threads
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Threads

   procedure On_Thread_Selection (Thread : access Gtk_Widget_Record'Class);
   --  Called when a thread was selected in the view

   ----------
   -- Misc --
   ----------

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

   procedure Attach_To_Thread_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
      renames Thread_Views.Attach_To_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug    : constant String := '/' & (-"_Debug") & '/';
      Data_Sub : constant String := Debug & (-"D_ata") & '/';
   begin
      Register_Menu
        (Kernel, Data_Sub, -"_Threads", "",
         On_Threads'Access, Ref_Item => -"Protection Domains");
      Thread_Views.Register_Desktop_Functions;
   end Register_Module;

   ----------------
   -- On_Threads --
   ----------------

   procedure On_Threads
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Top     : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot display threads list while the debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      Attach_To_Thread_Dialog (Process, Create_If_Necessary => True);
      Update_Threads (Get_Thread_View (Process));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Threads;

   -------------------------
   -- On_Thread_Selection --
   -------------------------

   procedure On_Thread_Selection (Thread : access Gtk_Widget_Record'Class) is
      T     : constant Thread_View := Thread_View (Thread);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (T.Tree));
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Model);
      Matched : Match_Array (0 .. 0);
   begin
      while Iter /= Null_Iter
        and then not Iter_Is_Selected (Get_Selection (T.Tree), Iter)
      loop
         Next (Model, Iter);
      end loop;

      declare
         Str : constant String := Get_String (Model, Iter, 0);
      begin
         Match ("[0-9]+", Str, Matched);

         if Matched (0) /= No_Match then
            Thread_Switch
              (Get_Process (T).Debugger,
               Natural'Value (Str (Matched (0).First .. Matched (0).Last)),
               Mode => GVD.Types.Visible);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception: "
                & Exception_Information (E));
   end On_Thread_Selection;

   ---------------------
   -- Get_Thread_View --
   ---------------------

   function Get_Thread_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window is
   begin
      return Gtk_Scrolled_Window (Process.Threads);
   end Get_Thread_View;

   ---------------------
   -- Set_Thread_View --
   ---------------------

   procedure Set_Thread_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window) is
   begin
      Process.Threads := Gtk_Widget (View);
   end Set_Thread_View;

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

   ---------------
   -- On_Attach --
   ---------------

   procedure On_Attach
     (Thread  : access Thread_View_Record;
      Process : access Visual_Debugger_Record'Class) is
   begin
      Widget_Callback.Object_Connect
        (Process, "process_stopped", Update_Threads'Access, Thread);
   end On_Attach;

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

   --------------------
   -- Update_Threads --
   --------------------

   procedure Update_Threads (Thread : access Gtk_Widget_Record'Class) is
      View        : constant Thread_View := Thread_View (Thread);
      Info        : Thread_Information_Array (1 .. Max_Tasks);
      Len         : Natural;
      Num_Columns : Thread_Fields;
      Iter        : Gtk_Tree_Iter;
   begin
      if Get_Process (View) /= null
        and then Visible_Is_Set (View)
        and then Get_Process (Get_Process (View).Debugger) /= null
      then
         Info_Threads (Get_Process (View).Debugger, Info, Len);
         Num_Columns := Info (Info'First).Num_Fields;

         if View.Tree /= null
           and then Get_N_Columns (Get_Model (View.Tree)) /=
           Gint (Num_Columns)
         then
            Trace (Me, "Threads: Number of columns has changed");
            Destroy (View.Tree);
            View.Tree := null;
         end if;

         if View.Tree = null and then Len > Info'First then
            declare
               Titles : GNAT.OS_Lib.String_List (1 .. Integer (Num_Columns));
            begin
               Trace (Me, "Threads: Creating tree, num_columns="
                      & Num_Columns'Img);
               for T in Titles'Range loop
                  Titles (T) := new String'
                    (Value
                       (Info (Info'First).Information (Thread_Fields (T))));
               end loop;

               View.Tree := Create_Tree_View
                 (Column_Types       =>
                    (0 .. Guint (Num_Columns) - 1 => GType_String),
                  Column_Names       => Titles,
                  Show_Column_Titles => False);
               Free (Titles);

               Add (View, View.Tree);
               Show_All (View.Tree);
               Widget_Callback.Object_Connect
                 (Get_Selection (View.Tree), "changed",
                  On_Thread_Selection'Access, Thread);
            end;
         end if;

         Clear (Gtk_Tree_Store (Get_Model (View.Tree)));

         for J in Info'First + 1 .. Len loop
            Append (Gtk_Tree_Store (Get_Model (View.Tree)), Iter, Null_Iter);
            for Col in Info (J).Information'Range loop
               Set (Gtk_Tree_Store (Get_Model (View.Tree)),
                    Iter,
                    Gint (Col - Info (J).Information'First),
                    Value (Info (J).Information (Col)));
            end loop;
         end loop;

         Free (Info);
      end if;
   end Update_Threads;

   ------------
   -- Update --
   ------------

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Thread : access Thread_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Gtk.Scrolled_Window.Initialize (Thread);
      Set_Policy (Thread, Policy_Automatic, Policy_Automatic);

      --  The tree will be created on the first call to Update, since we do not
      --  know yet how many columns are needed
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

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
      Temp      : Gtkada.Types.Chars_Ptr_Array (0 .. 1);
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
