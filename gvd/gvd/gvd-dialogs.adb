with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Types;    use Gtkada.Types;
with Task_Dialog_Pkg.Callbacks; use Task_Dialog_Pkg.Callbacks;
with Callbacks_Odd;   use Callbacks_Odd;
with Gtkada.Handlers; use Gtkada.Handlers;
with Interfaces.C;    use Interfaces.C;

package body Task_Dialog_Pkg is

   pragma Suppress (All_Checks);
   --  Checks are expensive (in code size) and not needed in this package.

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Gtk_Window;
      Information : Thread_Information_Array) is
   begin
      Task_Dialog := new Task_Dialog_Record;
      Task_Dialog_Pkg.Initialize (Task_Dialog, Main_Window, Information);
   end Gtk_New;

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Gtk_Window;
      Information : Thread_Information_Array)
   is
      Num_Columns : Thread_Fields;
      Row         : Gint;

   begin
      Gtk.Dialog.Initialize (Task_Dialog);
      Task_Dialog.Main_Window := Main_Window;

      Set_Title (Task_Dialog, "Task Status");
      Set_Policy (Task_Dialog, False, True, False);
      Set_Position (Task_Dialog, Win_Pos_Center);
      Set_Modal (Task_Dialog, False);
      Set_Default_Size (Task_Dialog, -1, 200);

      Task_Dialog.Vbox1 := Get_Vbox (Task_Dialog);
      Set_Homogeneous (Task_Dialog.Vbox1, False);
      Set_Spacing (Task_Dialog.Vbox1, 0);

      if Information'Length > 0 then
         Gtk_New (Task_Dialog.Scrolledwindow1);
         Pack_Start
           (Task_Dialog.Vbox1, Task_Dialog.Scrolledwindow1, True, True, 0);
         Set_Policy
           (Task_Dialog.Scrolledwindow1, Policy_Never, Policy_Automatic);

         Num_Columns := Information (Information'First).Num_Fields;
         Gtk_New
           (Task_Dialog.Task_List,
            Gint (Num_Columns),
            Information (Information'First).Information);
         Widget_Callback.Connect
           (Task_Dialog.Task_List,
            "select_row",
            On_Task_List_Select_Row'Access);
         Add (Task_Dialog.Scrolledwindow1, Task_Dialog.Task_List);
         Set_Selection_Mode (Task_Dialog.Task_List, Selection_Single);
         Set_Shadow_Type (Task_Dialog.Task_List, Shadow_In);
         Set_Show_Titles (Task_Dialog.Task_List, True);

         for J in Gint range 0 .. Gint (Num_Columns) - 1 loop
            Set_Column_Auto_Resize (Task_Dialog.Task_List, J, True);
         end loop;

         for J in Information'First + 1 .. Information'Last loop
            Row := Append (Task_Dialog.Task_List, Information (J).Information);
         end loop;
      end if;

      Task_Dialog.Hbox1 := Get_Action_Area (Task_Dialog);
      Set_Border_Width (Task_Dialog.Hbox1, 10);
      Set_Homogeneous (Task_Dialog.Hbox1, True);
      Set_Spacing (Task_Dialog.Hbox1, 5);

      Gtk_New (Task_Dialog.Hbuttonbox1);
      Pack_Start (Task_Dialog.Hbox1, Task_Dialog.Hbuttonbox1, True, True, 0);
      Set_Spacing (Task_Dialog.Hbuttonbox1, 30);
      Set_Layout (Task_Dialog.Hbuttonbox1, Buttonbox_Default_Style);
      Set_Child_Size (Task_Dialog.Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Task_Dialog.Hbuttonbox1, 7, 0);

      Gtk_New (Task_Dialog.Close_Button, "Close");
      Set_Flags (Task_Dialog.Close_Button, Can_Default);
      Button_Callback.Connect
        (Task_Dialog.Close_Button, "clicked",
         Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
      Add (Task_Dialog.Hbuttonbox1, Task_Dialog.Close_Button);
   end Initialize;

end Task_Dialog_Pkg;
