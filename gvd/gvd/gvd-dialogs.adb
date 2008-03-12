-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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

with Basic_Types;           use Basic_Types;
with Config;                use Config;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNAT.Regpat;           use GNAT.Regpat;
with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Main_Window;       use GPS.Main_Window;
with GPS.Intl;              use GPS.Intl;
pragma Elaborate_All (GPS.Intl);
with GVD.Dialogs.Callbacks; use GVD.Dialogs.Callbacks;
with GVD.Process;           use GVD.Process;
with GVD.Types;             use GVD.Types;
with GVD.Views;             use GVD.Views;
with GVD;                   use GVD;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Gdk.Event;             use Gdk.Event;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;
with Gtk.Tree_Store;        use Gtk.Tree_Store;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Widget;            use Gtk.Widget;
with Gtk;                   use Gtk;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtkada.MDI;            use Gtkada.MDI;
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

   generic
      with procedure Attach
        (Process : access Visual_Debugger_Record'Class;
         Create_If_Necessary : Boolean);
   procedure Open_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open one of the views, and update it immediately

   -----------------
   -- Thread View --
   -----------------

   type Get_Info_Subprogram is access procedure
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural);
   procedure Info_Threads_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural);
   procedure Info_Tasks_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural);
   procedure Info_PD_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   type Thread_View_Record;
   type Switch_Subprogram is access procedure
     (View : access Thread_View_Record'Class; Line : String);
   procedure Task_Switch_Dispatch
     (View : access Thread_View_Record'Class; Line : String);
   procedure Thread_Switch_Dispatch
     (View : access Thread_View_Record'Class; Line : String);
   procedure PD_Switch_Dispatch
     (View : access Thread_View_Record'Class; Line : String);
   --  Would be nice to use a primitive operation, but that would require
   --  declaring the types in the spec, not nice...

   type Thread_View_Record is new Scrolled_Views.Process_View_Record with
      record
         Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Get_Info : Get_Info_Subprogram := Info_Threads_Dispatch'Access;
         Switch   : Switch_Subprogram := Thread_Switch_Dispatch'Access;
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
   procedure Update (Thread : access Thread_View_Record);
   --  See description in GVD.Generic_View

   package Thread_Views is new Scrolled_Views.Simple_Views
     (Module_Name        => "Thread_View",
      View_Name          => -"Threads",
      Formal_View_Record => Thread_View_Record,
      Get_View           => Get_Thread_View,
      Set_View           => Set_Thread_View,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);

   function On_Thread_Button_Release
     (Thread : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when a thread was selected in the view

   ----------------
   -- Tasks view --
   ----------------

   type Task_View_Record is new Thread_View_Record with null record;
   function Get_Task_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window;
   procedure Set_Task_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window);
   procedure Initialize
     (Tasks  : access Task_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  See inherited documentation

   package Tasks_Views is new Scrolled_Views.Simple_Views
     (Module_Name        => "Tasks_View",
      View_Name          => -"Tasks",
      Formal_View_Record => Task_View_Record,
      Get_View           => Get_Task_View,
      Set_View           => Set_Task_View,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);

   -----------------------------
   -- Protection domains view --
   -----------------------------

   type PD_View_Record is new Thread_View_Record with null record;
   function Get_PD_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window;
   procedure Set_PD_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window);
   procedure Initialize
     (PDs    : access PD_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  See inherited documentation

   package PD_Views is new Scrolled_Views.Simple_Views
     (Module_Name        => "PD_View",
      View_Name          => -"Protection Domains",
      Formal_View_Record => PD_View_Record,
      Get_View           => Get_PD_View,
      Set_View           => Set_PD_View,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);

   ----------
   -- Misc --
   ----------

   function Delete_Dialog
     (Dialog : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user deletes a dialog by clicking on the small
   --  button in the title bar of the window.

   procedure Attach_To_Thread_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
      renames Thread_Views.Attach_To_View;
   procedure Attach_To_Tasks_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
      renames Tasks_Views.Attach_To_View;
   procedure Attach_To_PD_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
      renames PD_Views.Attach_To_View;

   ---------------
   -- Open_View --
   ---------------

   procedure Open_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Top     : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
   begin
      Attach (Process, Create_If_Necessary => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Open_View;

   ----------------------------
   -- Generics instantiation --
   ----------------------------

   procedure On_Threads is new Open_View (Attach_To_Thread_Dialog);
   procedure On_Tasks is new Open_View   (Attach_To_Tasks_Dialog);
   procedure On_PD is new Open_View      (Attach_To_PD_Dialog);
   --  The various menus to open views

   ---------------------------
   -- Info_Threads_Dispatch --
   ---------------------------

   procedure Info_Threads_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Info_Threads (Debugger, Info, Len);
   end Info_Threads_Dispatch;

   -------------------------
   -- Info_Tasks_Dispatch --
   -------------------------

   procedure Info_Tasks_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Info_Tasks (Debugger, Info, Len);
   end Info_Tasks_Dispatch;

   ----------------------
   -- Info_PD_Dispatch --
   ----------------------

   procedure Info_PD_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Info_PD (Debugger, Info, Len);
   end Info_PD_Dispatch;

   --------------------------
   -- Task_Switch_Dispatch --
   --------------------------

   procedure Task_Switch_Dispatch
     (View : access Thread_View_Record'Class; Line : String)
   is
      Matched : Match_Array (0 .. 0);
   begin
      Match ("[0-9]+", Line, Matched);

      if Matched (0) /= No_Match then
         Task_Switch
           (Get_Process (View).Debugger,
            Natural'Value (Line (Matched (0).First .. Matched (0).Last)),
            Mode => GVD.Types.Visible);
      end if;
   end Task_Switch_Dispatch;

   ----------------------------
   -- Thread_Switch_Dispatch --
   ----------------------------

   procedure Thread_Switch_Dispatch
     (View : access Thread_View_Record'Class; Line : String)
   is
      Matched : Match_Array (0 .. 0);
   begin
      Match ("[0-9]+", Line, Matched);

      if Matched (0) /= No_Match then
         Thread_Switch
           (Get_Process (View).Debugger,
            Natural'Value (Line (Matched (0).First .. Matched (0).Last)),
            Mode => GVD.Types.Visible);
      end if;
   end Thread_Switch_Dispatch;

   ------------------------
   -- PD_Switch_Dispatch --
   ------------------------

   procedure PD_Switch_Dispatch
     (View : access Thread_View_Record'Class; Line : String)
   is
      Matched : Match_Array (0 .. 0);
   begin
      Match ("(0x)?[0-9a-fA-F]+", Line, Matched);

      --  ??? The Command_Type was changed from Visible to Hidden
      --  (revision 1.62) because the debugger is still
      --  processing the previous command (Info_PD), and there is
      --  an assertion failure in Debugger.Send_Full. This does
      --  not happen for Task_Switch or Thread_Switch (above)

      if Matched (0) /= No_Match then
         PD_Switch
           (Get_Process (View).Debugger,
            Line (Matched (0).First .. Matched (0).Last),
            Mode => GVD.Types.Hidden);

         --  After switching to a new protection domain, we want the
         --  PD dialog to reflect that change immediately
         Update (View);
      end if;
   end PD_Switch_Dispatch;

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
        (Kernel, Data_Sub, -"_Protection Domains", "", On_PD'Access);
      Register_Menu
        (Kernel, Data_Sub, -"_Threads", "",
         On_Threads'Access, Ref_Item => -"Protection Domains");
      Register_Menu
        (Kernel, Data_Sub, -"Ta_sks", "",
         On_Tasks'Access, Ref_Item => -"Protection Domains");

      Thread_Views.Register_Desktop_Functions (Kernel);
      Tasks_Views.Register_Desktop_Functions (Kernel);
      PD_Views.Register_Desktop_Functions (Kernel);
   end Register_Module;

   ------------------------------
   -- On_Thread_Button_Release --
   ------------------------------

   function On_Thread_Button_Release
     (Thread : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      T     : constant Thread_View := Thread_View (Thread);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store (Get_Model (T.Tree));
      Iter  : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (T.Tree, Model, Event);

      if Iter /= Null_Iter then
         T.Switch (T, Get_String (Model, Iter, 0));
      end if;

      return False;
   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Thread_Button_Release;

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

   -------------------
   -- Get_Task_View --
   -------------------

   function Get_Task_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window is
   begin
      return Gtk_Scrolled_Window (Process.Tasks);
   end Get_Task_View;

   -------------------
   -- Set_Task_View --
   -------------------

   procedure Set_Task_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window) is
   begin
      Process.Tasks := Gtk_Widget (View);
   end Set_Task_View;

   -----------------
   -- Get_PD_View --
   -----------------

   function Get_PD_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window is
   begin
      return Gtk_Scrolled_Window (Process.PDs);
   end Get_PD_View;

   -----------------
   -- Set_PD_View --
   -----------------

   procedure Set_PD_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window) is
   begin
      Process.PDs := Gtk_Widget (View);
   end Set_PD_View;

   -------------
   -- Gtk_New --
   -------------

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

   procedure Update (Thread : access Thread_View_Record) is
      Info        : Thread_Information_Array (1 .. Max_Tasks);
      Len         : Natural;
      Num_Columns : Thread_Fields;
      Iter        : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;
      Path        : Gtk_Tree_Path;
      Sel         : Gtk_Tree_Selection;
   begin
      if Get_Process (Thread) /= null
        and then Visible_Is_Set (Thread)
        and then Get_Process (Get_Process (Thread).Debugger) /= null
      then
         Thread.Get_Info (Get_Process (Thread).Debugger, Info, Len);
         Num_Columns := Info (Info'First).Num_Fields;

         if Thread.Tree /= null
           and then Get_N_Columns (Get_Model (Thread.Tree)) /=
           Gint (Num_Columns)
         then
            Trace (Me, "Threads: Number of columns has changed");
            Destroy (Thread.Tree);
            Thread.Tree := null;
         end if;

         if Thread.Tree = null and then Len > Info'First then
            declare
               Titles : GNAT.Strings.String_List (1 .. Integer (Num_Columns));
            begin
               Trace (Me, "Threads: Creating tree, num_columns="
                      & Num_Columns'Img);
               for T in Titles'Range loop
                  Titles (T) := new String'
                    (Value
                       (Info (Info'First).Information (Thread_Fields (T))));
               end loop;

               Thread.Tree := Create_Tree_View
                 (Column_Types       =>
                    (0 .. Guint (Num_Columns) - 1 => GType_String),
                  Column_Names       => Titles);
               Free (Titles);

               Add (Thread, Thread.Tree);
               Show_All (Thread.Tree);
               Return_Callback.Object_Connect
                 (Thread.Tree, Signal_Button_Release_Event,
                  Return_Callback.To_Marshaller
                    (On_Thread_Button_Release'Access),
                  Thread, After => False);
            end;
         end if;

         --  Before clearing the tree, save the position of the selection
         if Thread.Tree /= null then
            Sel := Get_Selection (Thread.Tree);

            if Sel /= null then
               Get_Selected (Sel, Model, Iter);

               if Iter /= Null_Iter then
                  Path := Get_Path (Model, Iter);
               end if;
            end if;

            Clear (Gtk_Tree_Store (Get_Model (Thread.Tree)));
         end if;

         for J in Info'First + 1 .. Len loop
            Append (Gtk_Tree_Store (Get_Model (Thread.Tree)), Iter, Null_Iter);
            for Col in Info (J).Information'Range loop
               Set (Gtk_Tree_Store (Get_Model (Thread.Tree)),
                    Iter,
                    Gint (Col - Info (J).Information'First),
                    Value (Info (J).Information (Col)));
            end loop;
         end loop;

         --  If a selection was found before clearing the tree, restore it

         if Path /= null then
            Set_Cursor (Thread.Tree, Path, null, False);
            Path_Free (Path);
         end if;

         Free (Info);
      end if;
   end Update;

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
      --  know yet how many columns are needed.
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Tasks  : access Task_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      Initialize (Thread => Tasks, Kernel => Kernel);
      Tasks.Get_Info := Info_Tasks_Dispatch'Access;
      Tasks.Switch   := Task_Switch_Dispatch'Access;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (PDs    : access PD_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      Initialize (Thread => PDs, Kernel => Kernel);
      PDs.Get_Info := Info_PD_Dispatch'Access;
      PDs.Switch   := PD_Switch_Dispatch'Access;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog                     : access Question_Dialog_Record'Class;
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
      Gtk.Dialog.Initialize (Dialog, -"Question", Main_Window, 0);
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
        (Dialog, Gtk.Widget.Signal_Delete_Event,
         Return_Callback.To_Marshaller (Delete_Dialog'Access));

      Dialog.Debugger := Debugger;

      Widget_Callback.Connect
        (Dialog.Close_Button, Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Question_Close_Clicked'Access));

      if Question_Description /= "" then
         Gtk_New (Label, Question_Description);
         Pack_Start (Dialog.Vbox1, Label, False, False, 5);
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
         Dialog.Kind := Yes_No_Dialog;
         Set_Default_Size (Dialog, 100, 50);
         Gtk_New_From_Stock (OK_Button, Stock_Yes);
         Add (Dialog.Hbuttonbox1, OK_Button);
         Widget_Callback.Connect
           (OK_Button,
            Signal_Clicked,
            On_Question_Yes_Clicked'Access);
         Grab_Focus (OK_Button);

         Gtk_New_From_Stock (OK_Button, Stock_No);
         Add (Dialog.Hbuttonbox1, OK_Button);
         Widget_Callback.Connect
           (OK_Button,
            Signal_Clicked,
            On_Question_No_Clicked'Access);

         Ref (Dialog.Close_Button);
         Remove (Dialog.Hbuttonbox1, Dialog.Close_Button);

      else
         Dialog.Kind := Multiple_Choice_Dialog;
         Gtk_New (Dialog.Scrolledwindow1);
         Pack_Start (Dialog.Vbox1, Dialog.Scrolledwindow1, True, True, 0);
         Set_Policy
           (Dialog.Scrolledwindow1, Policy_Automatic, Policy_Automatic);

         --  Make sure the Cancel button is on the right, for homogeneity
         Ref (Dialog.Close_Button);
         Remove (Dialog.Hbuttonbox1, Dialog.Close_Button);
         Gtk_New_From_Stock (OK_Button, Stock_Ok);
         Add (Dialog.Hbuttonbox1, OK_Button);
         Widget_Callback.Connect
           (OK_Button,
            Signal_Clicked,
            On_Question_OK_Clicked'Access);
         Add (Dialog.Hbuttonbox1, Dialog.Close_Button);
         Unref (Dialog.Close_Button);

         Gtk_New (Dialog.List, 2, Question_Titles);
         Add (Dialog.Scrolledwindow1, Dialog.List);

         if Multiple_Selection_Allowed then
            Set_Selection_Mode (Dialog.List, Selection_Multiple);
         else
            Set_Selection_Mode (Dialog.List, Selection_Single);
         end if;

         for J in Questions'Range loop
            Temp (0) := C.Strings.New_String (Questions (J).Choice.all);
            Temp (1) := C.Strings.New_String (Questions (J).Description.all);
            Row := Append (Dialog.List, Temp);
            Free (Temp);
         end loop;

         Set_Column_Width
           (Dialog.List, 0, Optimal_Column_Width (Dialog.List, 0));
         Set_Column_Width
           (Dialog.List, 1,
            Gint'Min (Optimal_Column_Width (Dialog.List, 1),
                      Max_Column_Width));
         Set_Column_Auto_Resize (Dialog.List, 0, True);
         Set_Column_Auto_Resize (Dialog.List, 1, True);

         Width := Optimal_Column_Width (Dialog.List, 0)
           + Optimal_Column_Width (Dialog.List, 1) + 20;
         Set_Default_Size (Dialog, Gint'Min (Width, 500), 200);
      end if;

      Register_Dialog (Convert (Main_Window, Debugger), Dialog);
   end Initialize;

   ---------------------
   -- Get_Dialog_Kind --
   ---------------------

   function Get_Dialog_Kind
     (Question_Dialog : access Question_Dialog_Record'Class)
      return Dialog_Kind is
   begin
      return Question_Dialog.Kind;
   end Get_Dialog_Kind;

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
      On_Question_Close_Clicked (Dialog);
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return True;
   end Delete_Dialog;

end GVD.Dialogs;
