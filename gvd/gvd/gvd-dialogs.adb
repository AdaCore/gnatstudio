-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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
with Gdk.Color;             use Gdk.Color;
with Gtk;                   use Gtk;
with Gtk.Enums;             use Gtk.Enums;
with Gtkada.Types;          use Gtkada.Types;
with GVD;                   use GVD;
with GVD.Dialogs.Callbacks; use GVD.Dialogs.Callbacks;
with Callbacks_Odd;         use Callbacks_Odd;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;
with GVD.Types;             use GVD.Types;
with GVD.Process;           use GVD.Process;
with Odd_Intl;              use Odd_Intl;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Main;              use Gtk.Main;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Label;             use Gtk.Label;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Combo;             use Gtk.Combo;
with Gtk.List_Item;         use Gtk.List_Item;
with Gtk.Object;            use Gtk.Object;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Process_Proxies;       use Process_Proxies;
with Language;              use Language;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with GVD.Utils;             use GVD.Utils;
with GVD.Preferences;       use GVD.Preferences;

package body GVD.Dialogs is

   type GVD_Dialog_Access is access all GVD_Dialog;

   Question_Titles : constant Chars_Ptr_Array := "" + "Choice";
   --  ??? Should be translatable.

   type Simple_Entry_Dialog_Record is new Gtk_Dialog_Record with record
      Entry_Field  : Gtk_Combo;
      Was_Canceled : Boolean;
      Label        : Gtk_Label;
   end record;
   type Simple_Entry_Dialog_Access is access
     all Simple_Entry_Dialog_Record'Class;

   type Display_Dialog_Record is new Simple_Entry_Dialog_Record with record
      Check : Gtk_Check_Button;
   end record;
   type Display_Dialog_Access is access all Display_Dialog_Record'Class;

   package Dialog_User_Data is new Gtk.Object.User_Data
     (Simple_Entry_Dialog_Access);

   procedure Initialize
     (Dialog      : access GVD_Dialog_Record'Class;
      Title       : String;
      Main_Window : Gtk_Window);
   --  Create a standard dialog.

   procedure Cancel_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class);
   --  "Cancel" was pressed in a simple entry dialog

   function Delete_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class)
     return Boolean;
   --  A simple entry dialog was deleted

   procedure Ok_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class);
   --  "Ok" was pressed in a simple entry dialog

   function Internal_Simple_Entry_Dialog
     (Dialog   : access Simple_Entry_Dialog_Record'Class;
      Must_Initialize : Boolean;
      Parent   : access Gtk.Window.Gtk_Window_Record'Class;
      Extra_Box : Gtk.Check_Button.Gtk_Check_Button := null;
      Title    : String;
      Message  : String;
      Position : Gtk_Window_Position := Win_Pos_Center;
      Key      : String := "") return String;
   --  Internal version of Simple_Entry_Dialog, where Dialog is already
   --  created.

   function Delete_Dialog
     (Dialog : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user deletes a dialog by clicking on the small
   --  button in the title bar of the window.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (History_Dialog : out History_Dialog_Access;
      Main_Window    : Gtk_Window) is
   begin
      History_Dialog := new History_Dialog_Record;
      GVD.Dialogs.Initialize (History_Dialog);
      History_Dialog.Window := Main_Window;
   end Gtk_New;

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Gtk_Window) is
   begin
      Task_Dialog := new Task_Dialog_Record;
      Initialize (Task_Dialog, Main_Window);
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

   ------------
   -- Update --
   ------------

   procedure Update
     (Task_Dialog : access Task_Dialog_Record;
      Debugger    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Process     : constant Process_Proxy_Access :=
        Get_Process (Debugger_Process_Tab (Debugger).Debugger);
      Num_Columns : Thread_Fields;
      Row         : Gint;

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
      declare
         Info : Thread_Information_Array :=
           Info_Threads (Debugger_Process_Tab (Debugger).Debugger);
      begin
         if Task_Dialog.List = null and then Info'Length > 0 then
            Num_Columns := Info (Info'First).Num_Fields;
            Gtk_New
              (Task_Dialog.List,
               Gint (Num_Columns),
               Info (Info'First).Information);
            Show_All (Task_Dialog.List);
            Widget_Callback.Connect
              (Task_Dialog.List,
               "select_row",
               On_Task_List_Select_Row'Access);
            Add (Task_Dialog.Scrolledwindow1, Task_Dialog.List);
         end if;

         if Info'Length > 0 then
            Freeze (Task_Dialog.List);
            Clear (Task_Dialog.List);

            for J in Info'First + 1 .. Info'Last loop
               Row := Append (Task_Dialog.List, Info (J).Information);
            end loop;

            Row := Columns_Autosize (Task_Dialog.List);
            Thaw (Task_Dialog.List);
         end if;

         Free (Info);
      end;
   end Update;

   -----------------------------
   -- Show_Call_Stack_Columns --
   -----------------------------

   procedure Show_Call_Stack_Columns
     (Debugger : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Process  : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Debugger);
      Filter   : constant Stack_List_Filter :=
        Process.Backtrace_Filter;
      List     : Gtk_Clist := Process.Stack_List;
   begin
      Set_Column_Visibility (List, 0, (Filter and Frame_Num) /= 0);
      Set_Column_Visibility (List, 1, (Filter and Subprog_Name) /= 0);
      Set_Column_Visibility (List, 2, (Filter and Params) /= 0);
      Set_Column_Visibility (List, 3, (Filter and File_Location) /= 0);
      Set_Column_Visibility (List, 4, (Filter and Program_Counter) /= 0);
   end Show_Call_Stack_Columns;

   -----------------------
   -- Update_Call_Stack --
   -----------------------

   procedure Update_Call_Stack
     (Debugger : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Temp     : Chars_Ptr_Array (0 .. 4);
      Row      : Gint;
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Process  : constant Process_Proxy_Access :=
        Get_Process (Debugger_Process_Tab (Debugger).Debugger);
      List     : Gtk_Clist := Debugger_Process_Tab (Debugger).Stack_List;
      Columns  : Gint;
      Index    : Integer;
      Subp     : String_Access;

   begin
      --  Do nothing if the stack list has been hidden
      if not Visible_Is_Set (List) then
         return;
      end if;

      Columns := Get_Columns (List);

      Freeze (List);
      Clear (List);

      --  If the debugger was killed, no need to refresh

      if Process = null then
         Thaw (List);
         return;
      end if;

      --  Parse the information from the debugger

      Backtrace (Debugger_Process_Tab (Debugger).Debugger, Bt, Len);

      --  Update the contents of the window

      if Len > 0 then
         for J in 1 .. Len loop
            --  ??? We currently consider that the list of parameters always
            --  starts at the first '(' character encountered
            Subp := Bt (J).Subprogram;
            Index := Subp'First;
            while Index <= Subp'Last and then Subp (Index) /= '(' loop
               Index := Index + 1;
            end loop;

            Temp (0) := Strings.New_String (Natural'Image (Bt (J).Frame_Id));
            Temp (1) := Strings.New_String (Subp (Subp'First .. Index - 1));
            Temp (2) := Strings.New_String (Subp (Index .. Subp'Last));
            Temp (3) := Strings.New_String (Bt (J).Source_Location.all);
            Temp (4) := Strings.New_String (Bt (J).Program_Counter.all);
            Row := Append (List, Temp);
            Free (Temp);
         end loop;

         Row := Columns_Autosize (List);
      end if;

      Free (Bt (1 .. Len));

      Highlight_Stack_Frame (Debugger, 0);
      Thaw (List);
   end Update_Call_Stack;

   procedure Update
     (History_Dialog : History_Dialog_Access;
      Debugger       : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use String_History;

      Tab     : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Debugger);
      History : History_List := Tab.Window.Command_History;
      Data    : History_Data;
      Item    : Gtk_List_Item;

   begin
      if not Visible_Is_Set (History_Dialog)
        or else History_Dialog.Freeze_Count /= 0
      then
         return;
      end if;

      Remove_Items
        (History_Dialog.List, Get_Children (History_Dialog.List));
      Wind (History, Backward);

      for J in 1 .. Length (History) loop
         Data := Get_Current (History);

         if Data.Debugger_Num = Natural (Get_Num (Tab))
           and then Data.Mode /= Hidden
         then
            Gtk_New (Item, Label => Data.Command.all);
            Show (Item);
            Add (History_Dialog.List, Item);
         end if;

         Move_To_Next (History);
      end loop;

   exception
      when No_Such_Item => null;
   end Update;

   -----------------------------
   -- On_Task_Process_Stopped --
   -----------------------------

   procedure On_Task_Process_Stopped
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Debugger_Process_Tab (Widget);
   begin
      Update (Tab.Window.Task_Dialog, Tab);
   end On_Task_Process_Stopped;

   ------------------------------
   -- On_Stack_Process_Stopped --
   ------------------------------

   procedure On_Stack_Process_Stopped
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Debugger_Process_Tab (Widget);
   begin
      Update_Call_Stack (Tab);
   end On_Stack_Process_Stopped;

   ---------------------------
   -- Highlight_Stack_Frame --
   ---------------------------

   procedure Highlight_Stack_Frame
     (Debugger : access Gtk.Widget.Gtk_Widget_Record'Class;
      Frame    : Natural)
   is
      Tab : constant Debugger_Process_Tab := Debugger_Process_Tab (Debugger);
      Rows : constant Gint := Get_Rows (Tab.Stack_List);
      Bg : Gdk_Color;
   begin
      Freeze (Tab.Stack_List);

      --  Restore the default background for all the lines
      for R in 0 .. Rows - 1 loop
         Set_Background (Tab.Stack_List, R, Null_Color);
      end loop;

      --  Highlight the current frame
      Bg := Parse (File_Name_Bg_Color);
      Alloc (Get_System, Bg);
      Set_Background (Tab.Stack_List, Gint (Frame), Bg);
      Free_Colors (Get_System, (1 => Bg));

      Thaw (Tab.Stack_List);
   end Highlight_Stack_Frame;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog      : access GVD_Dialog_Record'Class;
      Title       : String;
      Main_Window : Gtk_Window) is
   begin
      Gtk.Dialog.Initialize (Dialog);
      Dialog.Main_Window := Main_Window;

      Set_Title (Dialog, Title);
      Set_Policy (Dialog, False, True, False);
      Set_Position (Dialog, Win_Pos_Center);
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
      Set_Child_Ipadding (Dialog.Hbuttonbox1, 7, 0);
      Set_Layout (Dialog.Hbuttonbox1, Buttonbox_Spread);

      Gtk_New (Dialog.Close_Button, -"Close");
      Add (Dialog.Hbuttonbox1, Dialog.Close_Button);

      Return_Callback.Connect
        (Dialog, "delete_event",
         Return_Callback.To_Marshaller (Delete_Dialog'Access));
   end Initialize;

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Gtk_Window) is
   begin
      Initialize (Task_Dialog, -"Task Status", Main_Window);
      Button_Callback.Connect
        (Task_Dialog.Close_Button, "clicked",
         Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));

      Set_Default_Size (Task_Dialog, 400, 200);
      Gtk_New (Task_Dialog.Scrolledwindow1);
      Pack_Start
        (Task_Dialog.Vbox1, Task_Dialog.Scrolledwindow1, True, True, 0);
      Set_Policy
        (Task_Dialog.Scrolledwindow1,
         Policy_Automatic,
         Policy_Automatic);

      --  We can't create the clist here, since we don't know yet how many
      --  columns there will be. This will be done on the first call to update
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
      Width     : Gint;
      OK_Button : Gtk_Button;
      Label     : Gtk_Label;

   begin
      Initialize (Question_Dialog, "Question", Main_Window);
      Widget_Callback.Connect
        (Question_Dialog.Close_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Question_Close_Clicked'Access));

      Question_Dialog.Debugger := Debugger;

      if Question_Description /= "" then
         Gtk_New (Label, Question_Description);
         Pack_Start (Question_Dialog.Vbox1, Label, False, False, 5);
      end if;

      Gtk_New (Question_Dialog.Scrolledwindow1);
      Pack_Start
        (Question_Dialog.Vbox1, Question_Dialog.Scrolledwindow1,
         True, True, 0);
      Set_Policy
        (Question_Dialog.Scrolledwindow1, Policy_Automatic, Policy_Automatic);

      --  Make sure the Cancel button is on the right, for homogeneity
      Ref (Question_Dialog.Close_Button);
      Remove (Question_Dialog.Hbuttonbox1, Question_Dialog.Close_Button);
      Gtk_New (OK_Button, -"OK");
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
         Temp (0) := Strings.New_String (Questions (J).Choice.all);
         Temp (1) := Strings.New_String (Questions (J).Description.all);
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

      Register_Dialog (Convert (Main_Window, Debugger), Question_Dialog);
   end Initialize;

   procedure Initialize
     (History_Dialog : access History_Dialog_Record'Class) is
   begin
      Gtk.Window.Initialize (History_Dialog, Window_Toplevel);
      Set_Title (History_Dialog, -"Command History");
      Set_Policy (History_Dialog, False, True, False);
      Set_Position (History_Dialog, Win_Pos_Center);
      Set_Default_Size (History_Dialog, 0, 200);
      Set_Modal (History_Dialog, False);

      Gtk_New_Vbox (History_Dialog.Vbox1, False, 0);
      Add (History_Dialog, History_Dialog.Vbox1);

      Gtk_New (History_Dialog.Scrolledwindow1);
      Pack_Start
        (History_Dialog.Vbox1,
         History_Dialog.Scrolledwindow1, True, True, 0);
      Set_Policy
        (History_Dialog.Scrolledwindow1, Policy_Automatic, Policy_Automatic);

      Gtk_New (History_Dialog.List);
      Add_With_Viewport (History_Dialog.Scrolledwindow1, History_Dialog.List);
      Set_Selection_Mode (History_Dialog.List, Selection_Extended);

      Gtk_New (History_Dialog.Hbuttonbox1);
      Pack_Start
        (History_Dialog.Vbox1, History_Dialog.Hbuttonbox1, False, False, 0);
      Set_Spacing (History_Dialog.Hbuttonbox1, 30);
      Set_Layout (History_Dialog.Hbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (History_Dialog.Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (History_Dialog.Hbuttonbox1, 7, 0);

      Gtk_New (History_Dialog.Replay_Selection, -"Replay selection");
      Set_Flags (History_Dialog.Replay_Selection, Can_Default);
      Button_Callback.Connect
        (History_Dialog.Replay_Selection, "clicked",
         Button_Callback.To_Marshaller (On_Replay_Selection_Clicked'Access));
      Add (History_Dialog.Hbuttonbox1, History_Dialog.Replay_Selection);

      Gtk_New (History_Dialog.Cancel, -"Close");
      Set_Flags (History_Dialog.Cancel, Can_Default);
      Button_Callback.Connect
        (History_Dialog.Cancel, "clicked",
         Button_Callback.To_Marshaller (On_History_Cancel_Clicked'Access));
      Add (History_Dialog.Hbuttonbox1, History_Dialog.Cancel);

      Gtk_New (History_Dialog.Help, -"Help");
      Set_Flags (History_Dialog.Help, Can_Default);
      Button_Callback.Connect
        (History_Dialog.Help, "clicked",
         Button_Callback.To_Marshaller (On_History_Help_Clicked'Access));
      Add (History_Dialog.Hbuttonbox1, History_Dialog.Help);

      Return_Callback.Connect
        (History_Dialog, "delete_event",
         Return_Callback.To_Marshaller (Delete_Dialog'Access));
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

   ----------------------------------
   -- Internal_Simple_Entry_Dialog --
   ----------------------------------

   function Internal_Simple_Entry_Dialog
     (Dialog   : access Simple_Entry_Dialog_Record'Class;
      Must_Initialize : Boolean;
      Parent   : access Gtk.Window.Gtk_Window_Record'Class;
      Extra_Box : Gtk.Check_Button.Gtk_Check_Button := null;
      Title    : String;
      Message  : String;
      Position : Gtk_Window_Position := Win_Pos_Center;
      Key      : String := "") return String
   is
      Button : Gtk_Button;
      Box    : Gtk_Box;
      Vbox   : Gtk_Box;
   begin
      if Must_Initialize then
         Set_Transient_For (Dialog, Parent);
         Set_Modal (Dialog);
         Set_Position (Dialog, Position);
         Gtkada.Handlers.Return_Callback.Connect
           (Dialog, "delete_event",
            Gtkada.Handlers.Return_Callback.To_Marshaller
            (Delete_Simple_Entry'Access));

         Gtk_New_Vbox (Vbox);
         Pack_Start (Get_Vbox (Dialog), Vbox);

         Gtk_New_Hbox (Box);
         Pack_Start (Vbox, Box, Padding => 10);

         Gtk_New (Dialog.Label, Message);
         Set_Justify (Dialog.Label, Justify_Center);
         Pack_Start
           (Box, Dialog.Label, Fill => True, Expand => True, Padding => 10);

         Gtk_New (Dialog.Entry_Field);
         Pack_Start (Box, Dialog.Entry_Field, Padding => 10);
         Disable_Activate (Dialog.Entry_Field);
         Widget_Callback.Object_Connect
           (Get_Entry (Dialog.Entry_Field), "activate",
            Widget_Callback.To_Marshaller (Ok_Simple_Entry'Access),
            Dialog);

         if Extra_Box /= null then
            Gtk_New_Hbox (Box);
            Pack_Start (Vbox, Box);
            Pack_Start (Box, Extra_Box, Padding => 10);
         end if;

         Gtk_New (Button, -"OK");
         Set_USize (Button, 80, -1);
         Pack_Start (Get_Action_Area (Dialog), Button, False, False, 14);
         Set_Flags (Button, Can_Default);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Ok_Simple_Entry'Access),
            Dialog);

         Gtk_New (Button, -"Cancel");
         Set_USize (Button, 80, -1);
         Pack_Start (Get_Action_Area (Dialog), Button, False, False, 14);
         Set_Flags (Button, Can_Default);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Cancel_Simple_Entry'Access),
            Dialog);

         if Key /= "" then
            Dialog_User_Data.Set
              (Parent, Simple_Entry_Dialog_Access (Dialog), Key);
         end if;
      else
         Set_Text (Dialog.Label, Message);
      end if;

      Set_Title (Dialog, Title);
      Set_Text (Get_Entry (Dialog.Entry_Field), "");
      Dialog.Was_Canceled := False;
      Show_All (Dialog);
      Gtk.Main.Main;

      if Dialog.Was_Canceled then
         if Key = "" then
            Destroy (Dialog);
         else
            Hide (Dialog);
         end if;

         return ASCII.Nul & "";

      else
         declare
            S : constant String := Get_Text (Get_Entry (Dialog.Entry_Field));
         begin
            if S /= "" then
               Add_Unique_Combo_Entry (Dialog.Entry_Field, S);
            end if;

            if Key = "" then
               Destroy (Dialog);
            else
               Hide (Dialog);
            end if;

            return S;
         end;
      end if;
   end Internal_Simple_Entry_Dialog;

   -------------------------
   -- Simple_Entry_Dialog --
   -------------------------

   function Simple_Entry_Dialog
     (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
      Title    : String;
      Message  : String;
      Position : Gtk_Window_Position := Win_Pos_Center;
      Key      : String := "") return String
   is
      Dialog      : Simple_Entry_Dialog_Access;
      Must_Initialize : Boolean := False;
   begin
      if Key /= "" then
         begin
            Dialog := Dialog_User_Data.Get (Parent, Key);
         exception
            when Gtkada.Types.Data_Error => null;
         end;
      end if;

      if Dialog = null then
         Dialog := new Simple_Entry_Dialog_Record;
         Initialize (Dialog);
         Must_Initialize := True;
      end if;

      return Internal_Simple_Entry_Dialog
        (Dialog, Must_Initialize, Parent, null, Title, Message, Position, Key);
   end Simple_Entry_Dialog;

   -------------------------
   -- Cancel_Simple_Entry --
   -------------------------

   procedure Cancel_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class) is
   begin
      Simple_Entry_Dialog_Access (Simple_Dialog).Was_Canceled := True;
      Gtk.Main.Main_Quit;
   end Cancel_Simple_Entry;

   ---------------------
   -- Ok_Simple_Entry --
   ---------------------

   procedure Ok_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end Ok_Simple_Entry;

   -------------------------
   -- Delete_Simple_Entry --
   -------------------------

   function Delete_Simple_Entry
     (Simple_Dialog : access Gtk_Widget_Record'Class)
     return Boolean is
   begin
      Simple_Entry_Dialog_Access (Simple_Dialog).Was_Canceled := True;
      Gtk.Main.Main_Quit;
      return False;
   end Delete_Simple_Entry;

   --------------------------
   -- Display_Entry_Dialog --
   --------------------------

   function Display_Entry_Dialog
     (Parent        : access Gtk.Window.Gtk_Window_Record'Class;
      Title         : String;
      Message       : String;
      Position      : Gtk_Window_Position := Win_Pos_Center;
      Check_Msg     : String;
      Key           : String := "";
      Button_Active : access Boolean) return String
   is
      Dialog      : Display_Dialog_Access;
      Must_Initialize : Boolean := False;
   begin
      if Key /= "" then
         begin
            Dialog := Display_Dialog_Access
              (Dialog_User_Data.Get (Parent, Key));
         exception
            when Gtkada.Types.Data_Error => null;
         end;
      end if;

      if Dialog = null then
         Dialog := new Display_Dialog_Record;
         Initialize (Dialog);
         Must_Initialize := True;
         Gtk_New (Dialog.Check, Check_Msg);
      end if;

      declare
         S : constant String := Internal_Simple_Entry_Dialog
           (Dialog, Must_Initialize, Parent, Dialog.Check, Title, Message,
            Position, Key);
         R : Boolean;
      begin
         R := Get_Active (Dialog.Check);
         Button_Active.all := R;
         return S;
      end;
   end Display_Entry_Dialog;

   -------------------
   -- Delete_Dialog --
   -------------------

   function Delete_Dialog
     (Dialog : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Hide (Dialog);
      return True;
   end Delete_Dialog;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Dialog : History_Dialog_Access) is
   begin
      Dialog.Freeze_Count := Dialog.Freeze_Count + 1;
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Dialog : History_Dialog_Access) is
   begin
      Dialog.Freeze_Count := Dialog.Freeze_Count - 1;
   end Thaw;

end GVD.Dialogs;
