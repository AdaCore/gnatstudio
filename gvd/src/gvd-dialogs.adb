------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C;          use Interfaces.C;

with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNAT.Regpat;           use GNAT.Regpat;

with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

with Glib.Object;           use Glib.Object;
with Glib.Values;
with Glib;                  use Glib;
with Glib_Values_Utils;     use Glib_Values_Utils;
with Gtk.Button;            use Gtk.Button;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;
with Gtk.Window;            use Gtk.Window;
with Gtk;                   use Gtk;
with Gtkada.Dialogs;        use Gtkada.Dialogs;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtkada.MDI;            use Gtkada.MDI;

with Config;                use Config;
with GPS.Debuggers;         use GPS.Debuggers;
with GPS.Intl;              use GPS.Intl;
pragma Elaborate_All (GPS.Intl);
with GPS.Main_Window;       use GPS.Main_Window;
with GUI_Utils;             use GUI_Utils;
with GVD.Generic_View;      use GVD.Generic_View;
with GVD.Process;           use GVD.Process;
with GVD.Types;             use GVD.Types;
with GVD;                   use GVD;
with GVD_Module;            use GVD_Module;
with Generic_Views;         use Generic_Views;
with Process_Proxies;       use Process_Proxies;

package body GVD.Dialogs is
   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.DIALOGS");

   type Is_Vx653_Debugger is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
      (Self    : access Is_Vx653_Debugger;
       Context : Selection_Context) return Boolean;
   --  Whether the current debugger is for a vx653 target

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

   type Thread_View_Record is new Process_View_Record with
      record
         Scrolled : Gtk_Scrolled_Window;
         Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Get_Info : Get_Info_Subprogram := Info_Threads_Dispatch'Access;
         Switch   : Switch_Subprogram := Thread_Switch_Dispatch'Access;
      end record;
   type Thread_View is access all Thread_View_Record'Class;

   function Initialize
     (Thread : access Thread_View_Record'Class) return Gtk_Widget;
   function Get_Thread_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Thread_View_Record'Class;
   procedure Set_Thread_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Thread_View_Record'Class := null);
   overriding procedure Update (Thread : not null access Thread_View_Record);
   --  See description in GVD.Generic_View

   package Thread_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Thread_View",
      View_Name          => -"Threads",
      Formal_View_Record => Thread_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);
   package Thread_Views is new GVD.Generic_View.Simple_Views
     (Views              => Thread_MDI_Views,
      Formal_View_Record => Thread_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_Thread_View,
      Set_View           => Set_Thread_View);

   procedure On_Selection_Changed (Thread : access Gtk_Widget_Record'Class);
   --  Called when a thread was selected in the view

   ----------------
   -- Tasks view --
   ----------------

   type Task_View_Record is new Thread_View_Record with null record;
   type Task_View is access all Task_View_Record'Class;
   function Get_Task_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Task_View_Record'Class;
   procedure Set_Task_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Task_View_Record'Class := null);
   function Initialize
     (Tasks  : access Task_View_Record'Class) return Gtk_Widget;
   --  See inherited documentation

   package Tasks_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Tasks_View",
      View_Name          => -"Debugger Tasks",
      Formal_View_Record => Task_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);
   package Tasks_Views is new GVD.Generic_View.Simple_Views
     (Views              => Tasks_MDI_Views,
      Formal_View_Record => Task_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_Task_View,
      Set_View           => Set_Task_View);

   -----------------------------
   -- Protection domains view --
   -----------------------------

   type PD_View_Record is new Thread_View_Record with null record;
   type PD_View is access all PD_View_Record'Class;
   function Get_PD_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access PD_View_Record'Class;
   procedure Set_PD_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access PD_View_Record'Class := null);
   function Initialize
     (PDs    : access PD_View_Record'Class) return Gtk_Widget;
   --  See inherited documentation

   package PD_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "PD_View",
      View_Name          => -"Protection Domains",
      Formal_View_Record => PD_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);
   package PD_Views is new GVD.Generic_View.Simple_Views
     (Views              => PD_MDI_Views,
      Formal_View_Record => PD_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_PD_View,
      Set_View           => Set_PD_View);

   ----------
   -- Misc --
   ----------

   function Delete_Dialog
     (Dialog : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user deletes a dialog by clicking on the small
   --  button in the title bar of the window.

   function Get_Dialog_Kind
     (Question_Dialog : access Question_Dialog_Record'Class)
      return Dialog_Kind;
   --  Return the kind of dialog associated with Question_Dialog

   procedure On_Question_No_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   procedure On_Question_Yes_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   procedure On_Question_OK_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  Callbacks for the question dialog

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
      (Self    : access Is_Vx653_Debugger;
       Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Self);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
   begin
      return Process /= null
         and then Process.Debugger /= null
         and then Process.Debugger.VxWorks_Version = Vx653;
   end Filter_Matches_Primitive;

   ---------------------------
   -- Info_Threads_Dispatch --
   ---------------------------

   procedure Info_Threads_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Debugger.Info_Threads (Info, Len);
   end Info_Threads_Dispatch;

   -------------------------
   -- Info_Tasks_Dispatch --
   -------------------------

   procedure Info_Tasks_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Debugger.Info_Tasks (Info, Len);
   end Info_Tasks_Dispatch;

   ----------------------
   -- Info_PD_Dispatch --
   ----------------------

   procedure Info_PD_Dispatch
     (Debugger : access Debugger_Root'Class;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Debugger.Info_PD (Info, Len);
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
         Visual_Debugger (View.Get_Process).Debugger.Task_Switch
           (Natural'Value (Line (Matched (0).First .. Matched (0).Last)),
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
         Visual_Debugger (View.Get_Process).Debugger.Thread_Switch
           (Natural'Value (Line (Matched (0).First .. Matched (0).Last)),
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
         Visual_Debugger (View.Get_Process).Debugger.PD_Switch
           (Line (Matched (0).First .. Matched (0).Last),
            Mode => GVD.Types.Hidden);

         --  After switching to a new protection domain, we want the
         --  PD dialog to reflect that change immediately
         View.Update;
      end if;
   end PD_Switch_Dispatch;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Thread_Views.Register_Module (Kernel);
      Tasks_Views.Register_Module (Kernel);
      PD_Views.Register_Module (Kernel);

      Thread_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open threads debugger window",
         Description => -"Open the 'Threads' window for the debugger");

      Tasks_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open tasks debugger window",
         Description => -"Open the 'Tasks' window for the debugger");

      PD_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open protection domains debugger window",
         Description =>
           -"Open the 'Protection Domains' window for the debugger",
        Filter       => new Is_Vx653_Debugger);
   end Register_Module;

   ------------------------------
   -- On_Thread_Button_Release --
   ------------------------------

   procedure On_Selection_Changed (Thread : access Gtk_Widget_Record'Class)
   is
      T           : constant Thread_View    := Thread_View (Thread);
      Store_Model : constant Gtk_Tree_Store := -Get_Model (T.Tree);
      Model       : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter;
   begin
      T.Tree.Get_Selection.Get_Selected (Model, Iter);

      if Iter /= Null_Iter then
         T.Switch (T, Get_String (Store_Model, Iter, 0));
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Selection_Changed;

   ---------------------
   -- Get_Thread_View --
   ---------------------

   function Get_Thread_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Thread_View_Record'Class is
   begin
      return Thread_View (Visual_Debugger (Process).Threads);
   end Get_Thread_View;

   ---------------------
   -- Set_Thread_View --
   ---------------------

   procedure Set_Thread_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Thread_View_Record'Class := null)
   is
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant Thread_View     := Get_Thread_View (Process);
   begin
      if Old /= null and then Old.Tree /= null then
         Clear (-Get_Model (Old.Tree));
      end if;

      V.Threads := Abstract_View_Access (View);
   end Set_Thread_View;

   -------------------
   -- Get_Task_View --
   -------------------

   function Get_Task_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Task_View_Record'Class is
   begin
      return Task_View (Visual_Debugger (Process).Tasks);
   end Get_Task_View;

   -------------------
   -- Set_Task_View --
   -------------------

   procedure Set_Task_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Task_View_Record'Class := null)
   is
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant Task_View       := Get_Task_View (Process);
   begin
      if Old /= null and then Old.Tree /= null then
         Clear (-Get_Model (Old.Tree));
      end if;

      V.Tasks := Abstract_View_Access (View);
   end Set_Task_View;

   -----------------
   -- Get_PD_View --
   -----------------

   function Get_PD_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access PD_View_Record'Class is
   begin
      return PD_View (Visual_Debugger (Process).PDs);
   end Get_PD_View;

   -----------------
   -- Set_PD_View --
   -----------------

   procedure Set_PD_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access PD_View_Record'Class := null)
   is
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant PD_View         := Get_PD_View (Process);
   begin
      if Old /= null and then Old.Tree /= null then
         Clear (-Get_Model (Old.Tree));
      end if;

      V.PDs := Abstract_View_Access (View);
   end Set_PD_View;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Question_Dialog            : out Question_Dialog_Access;
      Kernel                     : not null access Kernel_Handle_Record'Class;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "") is
   begin
      Question_Dialog := new Question_Dialog_Record;

      Initialize
        (Question_Dialog, Kernel, Debugger,
         Multiple_Selection_Allowed, Questions,
         Question_Description);
   end Gtk_New;

   ------------
   -- Update --
   ------------

   overriding procedure Update (Thread : not null access Thread_View_Record) is
      Info        : Thread_Information_Array (1 .. Max_Tasks);
      Len         : Natural;
      Num_Columns : Thread_Fields;
      Iter        : Gtk_Tree_Iter;
      V   : constant Visual_Debugger := Visual_Debugger (Get_Process (Thread));
   begin
      if V /= null
        and then Thread.Get_Visible
        and then V.Debugger.Get_Process /= null
      then
         Thread.Get_Info (V.Debugger, Info, Len);
         Num_Columns := Info (Info'First).Num_Fields;

         if Thread.Tree /= null
           and then Get_N_Columns (Thread.Tree.Get_Model) /=
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
               Trace (Me, "Threads: Creating tree, num_columns=" &
                        Num_Columns'Img);

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

               Thread.Scrolled.Add (Thread.Tree);
               Thread.Tree.Show_All;

               Widget_Callback.Object_Connect
                 (Thread.Tree.Get_Selection, Signal_Changed,
                  Widget_Callback.To_Marshaller (On_Selection_Changed'Access),
                  Slot_Object => Thread);
            end;
         end if;

         if Thread.Tree /= null then
            --  Disable the selection before the Clear procedure:
            --  this procedure will send the "changed" signal multiple times
            --  when deleting the rows
            Thread.Tree.Get_Selection.Set_Mode (Selection_None);
            Clear (-Get_Model (Thread.Tree));
            Thread.Tree.Get_Selection.Set_Mode (Selection_Single);
         end if;

         for J in Info'First + 1 .. Len loop
            Append (-Get_Model (Thread.Tree), Iter, Null_Iter);
            declare
               Values  : Glib.Values.GValue_Array
                 (Gint (Info (J).Information'First) ..
                    Gint (Info (J).Information'Last));
               Columns : Columns_Array (Values'Range);
            begin
               for Col in Info (J).Information'Range loop
                  Columns (Gint (Col)) :=
                    Gint (Col - Info (J).Information'First);
                  Glib.Values.Init_Set_String
                    (Values (Gint (Col)),
                     (if Info (J).Information (Col) = Null_Ptr then ""
                      else Value (Info (J).Information (Col))));
               end loop;
               Set_And_Clear (-Get_Model (Thread.Tree), Iter, Columns, Values);
            end;
         end loop;

         Free (Info);
      end if;
   end Update;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Thread : access Thread_View_Record'Class) return Gtk_Widget is
   begin
      Initialize_Vbox (Thread, Homogeneous => False);

      Gtk_New (Thread.Scrolled);
      Thread.Pack_Start (Thread.Scrolled, Expand => True, Fill => True);
      Thread.Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      --  The tree will be created on the first call to Update, since we do not
      --  know yet how many columns are needed.

      return Gtk_Widget (Thread);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Tasks  : access Task_View_Record'Class) return Gtk_Widget
   is
      W : Gtk_Widget;
   begin
      W := Initialize (Thread => Tasks);
      Tasks.Get_Info := Info_Tasks_Dispatch'Access;
      Tasks.Switch   := Task_Switch_Dispatch'Access;
      return W;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (PDs    : access PD_View_Record'Class) return Gtk_Widget
   is
      W : Gtk_Widget;
   begin
      W := Initialize (Thread => PDs);
      PDs.Get_Info := Info_PD_Dispatch'Access;
      PDs.Switch   := PD_Switch_Dispatch'Access;
      return W;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog                     : access Question_Dialog_Record'Class;
      Kernel                     : not null access Kernel_Handle_Record'Class;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "")
   is
      Row           : Gint with Unreferenced;
      Width         : Gint with Unreferenced;
      OK_Button     : Gtk_Widget;
      Cancel_Button : Gtk_Widget;
      Label         : Gtk_Label;

   begin
      GPS.Dialogs.Initialize
        (Dialog,
         Title   => -"Question",
         Kernel  => Kernel,
         Flags   =>
           Gtk.Dialog.Destroy_With_Parent and Gtk.Dialog.Use_Header_Bar);

      Set_Default_Size_From_History
         (Dialog, "debug-question", Kernel, -1, 200);

      Dialog.Content_Area := Get_Content_Area (Dialog);
      Dialog.Content_Area.Set_Homogeneous (False);
      Dialog.Content_Area.Set_Spacing (0);

      Gtkada.Handlers.Return_Callback.Connect
        (Dialog, Gtk.Widget.Signal_Delete_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Delete_Dialog'Access));

      Dialog.Debugger := Debugger;

      if Question_Description /= "" then
         Gtk_New (Label, Question_Description);
         Dialog.Content_Area.Pack_Start (Label, False, False, 5);
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
         Set_Default_Size_From_History
           (Dialog, "debug-yes-no", Kernel, 100, 50);

         OK_Button := Dialog.Add_Button ("Yes", Gtk_Response_Yes);
         Widget_Callback.Connect
           (OK_Button,
            Gtk.Button.Signal_Clicked,
            On_Question_Yes_Clicked'Access);
         OK_Button.Grab_Focus;

         Cancel_Button := Dialog.Add_Button ("No", Gtk_Response_No);
         Widget_Callback.Connect
           (Cancel_Button,
            Gtk.Button.Signal_Clicked,
            On_Question_No_Clicked'Access);

      else
         Dialog.Kind := Multiple_Choice_Dialog;

         Gtk_New (Dialog.Scrolledwindow1);
         Dialog.Content_Area.Pack_Start
           (Dialog.Scrolledwindow1, True, True, 0);
         Dialog.Scrolledwindow1.Set_Policy
           (Policy_Automatic, Policy_Automatic);

         --  Make sure the Cancel button is on the right, for homogeneity
         OK_Button := Dialog.Add_Button ("Select", Gtk_Response_Apply);
         OK_Button.Set_Name ("Select Button");
         Widget_Callback.Connect
           (OK_Button,
            Gtk.Button.Signal_Clicked,
            On_Question_OK_Clicked'Access);

         Cancel_Button := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
         Cancel_Button.Set_Name ("Cancel Button");
         Widget_Callback.Connect
           (Cancel_Button, Gtk.Button.Signal_Clicked,
            Widget_Callback.To_Marshaller (On_Question_Close_Clicked'Access));

         Gtk_New (Dialog.Tree_Model, (0 => GType_String, 1 => GType_String));
         Gtk_New (Dialog.Tree_View, Dialog.Tree_Model);
         Dialog.Tree_View.Set_Name ("Question tree");

         declare
            T     : Gtk_Cell_Renderer_Text;
            C     : Gtk_Tree_View_Column;
            Dummy : Gint;
         begin
            Gtk_New (C);
            C.Set_Title ("");
            Dummy := Dialog.Tree_View.Append_Column (C);

            Gtk_New (T);
            C.Pack_Start (T, False);
            C.Add_Attribute (T, "text", 0);

            Gtk_New (C);
            C.Set_Title (-"Choice");
            Dummy := Dialog.Tree_View.Append_Column (C);

            Gtk_New (T);
            C.Pack_Start (T, True);
            C.Add_Attribute (T, "text", 1);
         end;

         Dialog.Scrolledwindow1.Add (Dialog.Tree_View);

         Dialog.Tree_View.Get_Selection.Set_Mode
           ((if Multiple_Selection_Allowed
            then Selection_Multiple
            else Selection_Single));

         declare
            Iter : Gtk_Tree_Iter;
         begin
            for J in Questions'Range loop
               Dialog.Tree_Model.Append (Iter, Null_Iter);

               Set_And_Clear
                 (Dialog.Tree_Model, Iter,
                  (0 => As_String (Questions (J).Choice.all),
                   1 => As_String (Questions (J).Description.all)));
            end loop;
         end;
         Dialog.Tree_View.Columns_Autosize;
         Set_Default_Size_From_History
            (Dialog, "debug-question-multiple", Kernel, 500, 200);
      end if;

      Register_Dialog (Convert (Debugger), Dialog);
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
         Trace (Me, E);
         return True;
   end Delete_Dialog;

   -----------------------------
   -- On_Question_Yes_Clicked --
   -----------------------------

   procedure On_Question_Yes_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger := Convert (Debugger);

      begin
         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false

         Unregister_Dialog (Process);

         Debugger.Send
           ("y" & Gdb_Answer_Suffix,
            Mode => GVD.Types.Visible,
            Empty_Buffer    => False,
            Force_Send      => True,
            Wait_For_Prompt => False);
      end;
   end On_Question_Yes_Clicked;

   -----------------------------
   -- On_Question_No_Clicked --
   -----------------------------

   procedure On_Question_No_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger := Convert (Debugger);

      begin
         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false
         Unregister_Dialog (Process);

         Debugger.Send
           ("n" & Gdb_Answer_Suffix,
            Mode => GVD.Types.Visible,
            Empty_Buffer    => False,
            Force_Send      => True,
            Wait_For_Prompt => False);
      end;
   end On_Question_No_Clicked;

   ----------------------------
   -- On_Question_OK_Clicked --
   ----------------------------

   procedure On_Question_OK_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
   begin
      declare
         Dialog    : constant Question_Dialog_Access :=
           Question_Dialog_Access (Get_Toplevel (Object));

         Selection : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
         S         : Unbounded_String;
         Tmp       : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
         Button    : Message_Dialog_Buttons with Unreferenced;
         Debugger  : constant Debugger_Access := Dialog.Debugger;
         Process   : constant Visual_Debugger := Convert (Debugger);
         M         : Gtk_Tree_Model;

         use type Gtk_Tree_Path_List.Glist;
      begin
         Dialog.Tree_View.Get_Selection.Get_Selected_Rows (M, Selection);
         Tmp := Gtk_Tree_Path_List.First (Selection);
         while Tmp /= Gtk_Tree_Path_List.Null_List loop
            declare
               Path : constant Gtk_Tree_Path :=
                 Gtk_Tree_Path_List.Get_Data (Tmp);
               Iter : Gtk_Tree_Iter;
            begin
               Iter := Get_Iter (M, Path);
               Append (S, Get_String (M, Iter, 0));
            end;
            Tmp := Gtk_Tree_Path_List.Next (Tmp);
         end loop;
         Free_Path_List (Selection);

         if Length (S) = 0 then
            Button :=
              GPS_Message_Dialog
                (-"You must select at least one of the choices",
                 Error, Button_OK,
                 Parent => Gtk_Window (Dialog));
            Emit_Stop_By_Name (Object, "clicked");
            return;
         end if;

         --  Unregister the dialog, since Send will not take care of it when
         --  Wait_For_Prompt is false

         Unregister_Dialog (Process);

         Debugger.Send
           (To_String (S) & Gdb_Answer_Suffix,
            Mode            => GVD.Types.Visible,
            Force_Send      => True,
            Empty_Buffer    => False,
            Wait_For_Prompt => False);
      end;
   end On_Question_OK_Clicked;

   -------------------------------
   -- On_Question_Close_Clicked --
   -------------------------------

   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog   : constant Question_Dialog_Access :=
        Question_Dialog_Access (Get_Toplevel (Object));
      Debugger : constant Debugger_Access := Dialog.Debugger;
      Process  : constant Visual_Debugger := Convert (Debugger);
      Kind     : constant Dialog_Kind     := Get_Dialog_Kind (Dialog);

   begin
      --  We used to call Interrupt (Dialog.Debugger) here, but this proved to
      --  be unreliable in some cases (e.g. gdb mingw under Windows, so instead
      --  we send an answer to gdb, in order to cancel the question.

      --  Destroy the dialog, since we will have to recreate it anyway.
      Unregister_Dialog (Process);

      case Kind is
         when Yes_No_Dialog =>
            Debugger.Send
              ("n" & Gdb_Answer_Suffix,
               Mode            => GVD.Types.Visible,
               Force_Send      => True,
               Empty_Buffer    => False,
               Wait_For_Prompt => False);

         when Multiple_Choice_Dialog =>
            Debugger.Send
              ("0" & Gdb_Answer_Suffix,
               Mode            => GVD.Types.Visible,
               Force_Send      => True,
               Empty_Buffer    => False,
               Wait_For_Prompt => False);
      end case;
   end On_Question_Close_Clicked;

end GVD.Dialogs;
