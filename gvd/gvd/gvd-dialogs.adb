with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Types;    use Gtkada.Types;
with Odd.Dialogs.Callbacks; use Odd.Dialogs.Callbacks;
with Callbacks_Odd;   use Callbacks_Odd;
with Gtkada.Handlers; use Gtkada.Handlers;
with Interfaces.C;    use Interfaces.C;
with Interfaces.C.Strings;

package body Odd.Dialogs is

   pragma Suppress (All_Checks);
   --  Checks are expensive (in code size) and not needed in this package.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Gtk_Window;
      Information : Thread_Information_Array) is
   begin
      Task_Dialog := new Task_Dialog_Record;
      Initialize (Task_Dialog, Main_Window, Information);
   end Gtk_New;

   procedure Gtk_New
     (Backtrace_Dialog : out Backtrace_Dialog_Access;
      Main_Window      : Gtk_Window;
      Backtrace        : Backtrace_Array) is
   begin
      Backtrace_Dialog := new Backtrace_Dialog_Record;
      Initialize (Backtrace_Dialog, Main_Window, Backtrace);
   end Gtk_New;

   ------------
   -- Update --
   ------------

   procedure Update
     (Task_Dialog : access Task_Dialog_Record;
      Information : Thread_Information_Array)
   is
      Num_Columns : Thread_Fields;
      Row         : Gint;

   begin
      if Task_Dialog.Scrolledwindow1 /= null then
         Destroy (Task_Dialog.Scrolledwindow1);
         Task_Dialog.Scrolledwindow1 := null;
      end if;

      if Information'Length > 0 then
         Gtk_New (Task_Dialog.Scrolledwindow1);
         Pack_Start
           (Task_Dialog.Vbox1, Task_Dialog.Scrolledwindow1, True, True, 0);
         Set_Policy
           (Task_Dialog.Scrolledwindow1, Policy_Never, Policy_Automatic);

         Num_Columns := Information (Information'First).Num_Fields;
         Gtk_New
           (Task_Dialog.List,
            Gint (Num_Columns),
            Information (Information'First).Information);
         Widget_Callback.Connect
           (Task_Dialog.List,
            "select_row",
            On_Task_List_Select_Row'Access);
         Add (Task_Dialog.Scrolledwindow1, Task_Dialog.List);

         for J in Information'First + 1 .. Information'Last loop
            Row := Append (Task_Dialog.List, Information (J).Information);
         end loop;

         Row := Columns_Autosize (Task_Dialog.List);
      end if;

      Show_All (Task_Dialog);
   end Update;

   Backtrace_Titles : constant Chars_Ptr_Array :=
      "PC" + "Subprogram" + "Source";

   procedure Update
     (Backtrace_Dialog : access Backtrace_Dialog_Record;
      Backtrace        : Backtrace_Array)
   is
      Temp : Chars_Ptr_Array (0 .. 2);
      Row  : Gint;

   begin
      if Backtrace_Dialog.Scrolledwindow1 /= null then
         Destroy (Backtrace_Dialog.Scrolledwindow1);
         Backtrace_Dialog.Scrolledwindow1 := null;
      end if;

      if Backtrace'Length > 0 then
         Gtk_New (Backtrace_Dialog.Scrolledwindow1);
         Pack_Start
           (Backtrace_Dialog.Vbox1, Backtrace_Dialog.Scrolledwindow1,
            True, True, 0);
         Set_Policy
           (Backtrace_Dialog.Scrolledwindow1, Policy_Never, Policy_Automatic);

         Gtk_New (Backtrace_Dialog.List, 3, Backtrace_Titles);
         Widget_Callback.Connect
           (Backtrace_Dialog.List,
            "select_row",
            On_Backtrace_List_Select_Row'Access);
         Add (Backtrace_Dialog.Scrolledwindow1, Backtrace_Dialog.List);

         for J in Backtrace'Range loop
            Temp (0) := Strings.New_String (Backtrace (J).Program_Counter.all);
            Temp (1) := Strings.New_String (Backtrace (J).Subprogram.all);
            Temp (2) := Strings.New_String (Backtrace (J).Source_Location.all);
            Row := Append (Backtrace_Dialog.List, Temp);
            Free (Temp);
         end loop;

         Row := Columns_Autosize (Backtrace_Dialog.List);

         --  Prevent huge windows

         for J in Gint range 0 .. 2 loop
            if Optimal_Column_Width (Backtrace_Dialog.List, J) >
              Max_Column_Width
            then
               Set_Column_Width (Backtrace_Dialog.List, J, Max_Column_Width);
            end if;
         end loop;
      end if;

      Show_All (Backtrace_Dialog);
   end Update;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Gtk_Window;
      Information : Thread_Information_Array) is
   begin
      Gtk.Dialog.Initialize (Task_Dialog);
      Task_Dialog.Main_Window := Main_Window;

      Set_Title (Task_Dialog, "Task Status");
      Set_Policy (Task_Dialog, False, True, False);
      Set_Position (Task_Dialog, Win_Pos_Center);
      Set_Default_Size (Task_Dialog, -1, 200);

      Task_Dialog.Vbox1 := Get_Vbox (Task_Dialog);
      Set_Homogeneous (Task_Dialog.Vbox1, False);
      Set_Spacing (Task_Dialog.Vbox1, 0);

      Update (Task_Dialog, Information);

      Task_Dialog.Hbox1 := Get_Action_Area (Task_Dialog);
      Set_Border_Width (Task_Dialog.Hbox1, 5);
      Set_Homogeneous (Task_Dialog.Hbox1, True);
      Set_Spacing (Task_Dialog.Hbox1, 5);

      Gtk_New (Task_Dialog.Hbuttonbox1);
      Pack_Start (Task_Dialog.Hbox1, Task_Dialog.Hbuttonbox1, True, True, 0);
      Set_Spacing (Task_Dialog.Hbuttonbox1, 10);
      Set_Child_Size (Task_Dialog.Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Task_Dialog.Hbuttonbox1, 7, 0);

      Gtk_New (Task_Dialog.Close_Button, "Close");
      Set_Flags (Task_Dialog.Close_Button, Can_Default);
      Button_Callback.Connect
        (Task_Dialog.Close_Button, "clicked",
         Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
      Add (Task_Dialog.Hbuttonbox1, Task_Dialog.Close_Button);
   end Initialize;

   procedure Initialize
     (Backtrace_Dialog : access Backtrace_Dialog_Record'Class;
      Main_Window      : Gtk_Window;
      Backtrace        : Backtrace_Array) is
   begin
      Gtk.Dialog.Initialize (Backtrace_Dialog);
      Backtrace_Dialog.Main_Window := Main_Window;

      Set_Title (Backtrace_Dialog, "Backtrace");
      Set_Policy (Backtrace_Dialog, False, True, False);
      Set_Position (Backtrace_Dialog, Win_Pos_Center);
      Set_Default_Size (Backtrace_Dialog, -1, 200);

      Backtrace_Dialog.Vbox1 := Get_Vbox (Backtrace_Dialog);
      Set_Homogeneous (Backtrace_Dialog.Vbox1, False);
      Set_Spacing (Backtrace_Dialog.Vbox1, 0);

      Update (Backtrace_Dialog, Backtrace);

      Backtrace_Dialog.Hbox1 := Get_Action_Area (Backtrace_Dialog);
      Set_Border_Width (Backtrace_Dialog.Hbox1, 5);
      Set_Homogeneous (Backtrace_Dialog.Hbox1, True);
      Set_Spacing (Backtrace_Dialog.Hbox1, 5);

      Gtk_New (Backtrace_Dialog.Hbuttonbox1);
      Pack_Start
        (Backtrace_Dialog.Hbox1, Backtrace_Dialog.Hbuttonbox1, True, True, 0);
      Set_Spacing (Backtrace_Dialog.Hbuttonbox1, 10);
      Set_Child_Size (Backtrace_Dialog.Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Backtrace_Dialog.Hbuttonbox1, 7, 0);

      Gtk_New (Backtrace_Dialog.Close_Button, "Close");
      Set_Flags (Backtrace_Dialog.Close_Button, Can_Default);
      Button_Callback.Connect
        (Backtrace_Dialog.Close_Button, "clicked",
         Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
      Add (Backtrace_Dialog.Hbuttonbox1, Backtrace_Dialog.Close_Button);
   end Initialize;

end Odd.Dialogs;
