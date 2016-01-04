------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2016, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Main_Window;           use GPS.Main_Window;
with GPS.Stock_Icons;           use GPS.Stock_Icons;
with Gdk.Event;                 use Gdk.Event;
with Glib.Main;
with Glib.Object;               use Glib.Object;
with Glib;                      use Glib;
with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Progress_Bar;          use Gtk.Progress_Bar;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Pango.Layout;              use Pango.Layout;
with String_Utils;              use String_Utils;

package body Task_Manager.GUI is
   Me : constant Trace_Handle := Create ("TASKS");

   ---------------------
   -- Local constants --
   ---------------------

   Refresh_Timeout     : constant := 200;
   --  The timeout to refresh the GUI, in milliseconds

   -----------------
   -- Local types --
   -----------------

   type Task_Manager_Module_Record is new Module_ID_Record with null record;
   overriding procedure Destroy (Module : in out Task_Manager_Module_Record);
   --  Called when the module is destroyed

   type Task_Manager_Interface_Record is tagged;
   type Task_Manager_Interface is access all
     Task_Manager_Interface_Record'Class;

   type Refresh_Data is record
      GUI : Task_Manager_Interface;
      Index : Integer;
   end record;

   package Task_Manager_Source is new Glib.Main.Generic_Sources
     (Refresh_Data);

   type Task_Manager_UI_Record is new Task_Manager_Record with record
      GUI           : Task_Manager_Interface := null;
      Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
   end record;
   type Task_Manager_UI_Access is access all Task_Manager_UI_Record'Class;

   overriding procedure Queue_Added
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer);
   --  Inform the GUI that a queue has been added

   overriding procedure Queue_Removed
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer);
   --  Inform the GUI that a queue has been removed

   overriding procedure Queue_Changed
     (Manager           : access Task_Manager_UI_Record;
      Index             : Integer;
      Immediate_Refresh : Boolean);
   --  Inform the GUI that the progress or running information of a queue has
   --  been changed. If Immediate_Refresh is True, reflect the changes in the
   --  GUI immediately, otherwise do it in a timeout callback.

   package Integer_List is new Ada.Containers.Doubly_Linked_Lists (Integer);

   type Task_Manager_Interface_Record is new Gtk_Box_Record with record
      Kernel                 : Kernel_Handle;
      Manager                : Task_Manager_UI_Access;

      Main_Progress_Bar      : Gtk_Progress_Bar;
      --  The main progress bar (in the toolbar) that summarizes all current
      --  tasks.

      Progress_Bar_Button    : Gtk_Button;
      --  The pause/play button in the main progress bar

      Task_Label         : Gtk_Label;
      --  What action are we performing ?

      Progress_Label         : Gtk_Label;
      --  The progress to show

      Timeout_Cb             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The registered refresh timeout callback
   end record;

   type Manager_Index_Record is record
      D     : Task_Manager_Interface;
      Index : Integer;
   end record;

   -----------------------
   -- Local subprograms --
   -----------------------

   package Task_Manager_Handler is new Gtk.Handlers.User_Callback
     (GObject_Record, Manager_Index_Record);

   function Create (Kernel : Kernel_Handle) return Task_Manager_Access;
   --  Create the task manager's main progress bar (to be displayed in the GPS
   --  main toolbar).

   procedure On_Progress_Bar_Button_Clicked
     (Object : access GObject_Record'Class;
      Data   : Manager_Index_Record);
   --  Callback for a click on the global progress bar "x" button

   function On_Main_Progress_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for a "button_press_event" on the main progress bar

   procedure On_GUI_Destroy
     (Object  : access GObject_Record'Class;
      Manager : Manager_Index_Record);
   --  Callback for the destruction of the GUI

   procedure Set_Mode
     (View : access Task_Manager_Interface_Record'Class;
      Idle : Boolean);
   --  Hide or show the proper widgets

   procedure Refresh (GUI : Task_Manager_Interface);
   --  Refresh the information in View from the Task_Manager

   type Progress_Data (L, P : Integer) is record
      Fraction        : Gdouble;

      Text            : String (1 .. L);
      --  What action are we performing ?

      Progress_Text   : String (1 .. P);
      --  How far along are we (either as a "n/m" or a "n%")

      Multiple_Queues : Boolean;
   end record;

   Null_Progress_Data : constant Progress_Data := (0, 0, 0.0, "", "", False);

   function Get_Progress_Text
     (Manager                : access Task_Manager_Record'Class;
      Index                  : Natural;
      As_Percent             : Boolean;
      With_Name_And_Fraction : Boolean) return Progress_Data;
   --  Get the text for queue Index

   function Get_Fraction
     (Manager : access Task_Manager_Record'Class;
      Index   : Natural) return Gdouble;
   --  Return the fraction for queue Index

   function Get_Progress_Text
     (Manager    : access Task_Manager_Record'Class;
      As_Percent : Boolean) return Progress_Data;
   --  Get the text for the global progress bar

   procedure Process_Pending_Refreshes (GUI : Task_Manager_Interface);
   --  Process all pending refreshes

   function GUI_Refresh_Cb (Data : Refresh_Data) return Boolean;
   --  Timeout callback that refreshes the GUI

   procedure Unregister_Timeout (GUI : Task_Manager_Interface);
   --  Remove the timeout that refreshes the GUI and clear the list of items
   --  that need to be refreshed.

   -----------------------------------------
   -- On_Main_Progress_Button_Press_Event --
   -----------------------------------------

   function On_Main_Progress_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      GUI : constant Task_Manager_Interface :=
        Task_Manager_Interface (Object);
      Success : Boolean;
      pragma Unreferenced (Event, Success);
   begin
      Success := Execute_Action
         (GUI.Kernel, "open Task Manager", Error_Msg_In_Console => True);
      return False;
   end On_Main_Progress_Button_Press_Event;

   ------------------------------------
   -- On_Progress_Bar_Button_Clicked --
   ------------------------------------

   procedure On_Progress_Bar_Button_Clicked
     (Object : access GObject_Record'Class;
      Data   : Manager_Index_Record)
   is
      Index : Natural := 0;
      Count : Natural := 1;
      pragma Unreferenced (Object);
   begin
      if Data.D.Manager.Queues = null then
         return;
      end if;

      for J in Data.D.Manager.Queues'Range loop
         if Data.D.Manager.Queues (J).Show_Bar then
            if Index = 0 then
               Index := J;
            else
               Count := Count + 1;
               exit;
            end if;
         end if;
      end loop;

      if Count = 1 then
         Interrupt_Command (Data.D.Manager, Index);
      end if;
   end On_Progress_Bar_Button_Clicked;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (View : access Task_Manager_Interface_Record'Class;
      Idle : Boolean)
   is
   begin
      if Idle then
         View.Main_Progress_Bar.Set_Child_Visible (False);
         View.Progress_Bar_Button.Set_Child_Visible (False);
         View.Task_Label.Set_Child_Visible (False);
         View.Progress_Label.Set_Child_Visible (False);
         View.Main_Progress_Bar.Hide;
         View.Progress_Bar_Button.Hide;
         View.Task_Label.Hide;
         View.Progress_Label.Hide;

      else
         View.Main_Progress_Bar.Set_Child_Visible (True);
         View.Progress_Bar_Button.Set_Child_Visible (True);
         View.Task_Label.Set_Child_Visible (True);
         View.Progress_Label.Set_Child_Visible (True);
         View.Main_Progress_Bar.Show_All;
         View.Progress_Bar_Button.Show_All;
         View.Task_Label.Show_All;
         View.Progress_Label.Show_All;
      end if;
   end Set_Mode;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (GUI : Task_Manager_Interface) is
   begin
      if GUI = null then
         --  No GUI yet, or already exited
         return;

      elsif GUI.Manager.Queues = null then
         GUI.Set_Mode (Idle => True);
      else
         declare
            Pd : constant Progress_Data :=
                   Get_Progress_Text (GUI.Manager, False);
         begin
            if Pd = Null_Progress_Data then
               GUI.Set_Mode (Idle => True);

            else
               GUI.Set_Mode (Idle => False);
               GUI.Main_Progress_Bar.Set_Fraction (Pd.Fraction);
               GUI.Task_Label.Set_Text (Pd.Text);
               GUI.Progress_Label.Set_Text (" " & Pd.Progress_Text);
               GUI.Progress_Bar_Button.Set_Sensitive (not Pd.Multiple_Queues);

               GUI.Main_Progress_Bar.Set_Tooltip_Markup
                 (Pd.Text & " (" & Pd.Progress_Text & ")" & ASCII.LF
                  & (-"<i>Double-click to open the Task Manager</i>"));
            end if;
         end;
      end if;
   end Refresh;

   --------------------
   -- On_GUI_Destroy --
   --------------------

   procedure On_GUI_Destroy
     (Object  : access GObject_Record'Class;
      Manager : Manager_Index_Record)
   is
      pragma Unreferenced (Object);
      use Gdk;
      use type Glib.Main.G_Source_Id;
      GUI : Task_Manager_Interface renames Manager.D;

   begin
      --  If the graphics have been initialized, free them now

      Unregister_Timeout (GUI);
      GUI.Manager.GUI := null;
   end On_GUI_Destroy;

   ------------------------
   -- Unregister_Timeout --
   ------------------------

   procedure Unregister_Timeout (GUI : Task_Manager_Interface) is
      use type Glib.Main.G_Source_Id;
   begin
      if GUI.Timeout_Cb /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (GUI.Timeout_Cb);
         GUI.Timeout_Cb := Glib.Main.No_Source_Id;
      end if;

      if GUI.Manager.Queues /= null then
         for J in GUI.Manager.Queues'Range loop
            GUI.Manager.Queues (J).To_Refresh := False;
         end loop;
      end if;
   end Unregister_Timeout;

   -----------------
   -- Queue_Added --
   -----------------

   overriding procedure Queue_Added
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer)
   is
      GUI  : constant Task_Manager_Interface := Manager.GUI;
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      if GUI /= null then
         Refresh (GUI);
         Unregister_Timeout (GUI);
      end if;
      Task_Started_Hook.Run (GUI.Kernel, Queue_Id => Index);
   end Queue_Added;

   -------------------
   -- Queue_Removed --
   -------------------

   overriding procedure Queue_Removed
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer)
   is
      GUI  : constant Task_Manager_Interface := Manager.GUI;
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      if GUI /= null then
         Task_Terminated_Hook.Run (GUI.Kernel, Queue_Id => Index);
         Refresh (GUI);
         Unregister_Timeout (GUI);
      end if;
   end Queue_Removed;

   -------------------------------
   -- Process_Pending_Refreshes --
   -------------------------------

   procedure Process_Pending_Refreshes (GUI : Task_Manager_Interface) is
      To_Refresh : Integer_List.List;
   begin
      if GUI.Manager.Queues = null then
         return;
      end if;

      --  Store items to refresh in a temporary variable, to avoid
      --  looping on GUI.To_Refresh while potentially modifying it.

      for J in GUI.Manager.Queues'Range loop
         if GUI.Manager.Queues (J).To_Refresh then
            To_Refresh.Append (J);
            GUI.Manager.Queues (J).To_Refresh := False;
         end if;
      end loop;

      for Index of To_Refresh loop
         Task_Changed_Hook.Run (GUI.Kernel, Queue_Id => Index);
      end loop;
   end Process_Pending_Refreshes;

   --------------------
   -- GUI_Refresh_Cb --
   --------------------

   function GUI_Refresh_Cb (Data : Refresh_Data) return Boolean is
   begin
      Process_Pending_Refreshes (Data.GUI);
      Refresh (Data.GUI);

      Data.GUI.Timeout_Cb := Glib.Main.No_Source_Id;
      return False;
   exception
      when E : others =>
         Trace (Me, E);
         Data.GUI.Timeout_Cb := Glib.Main.No_Source_Id;
         return False;
   end GUI_Refresh_Cb;

   -------------------
   -- Queue_Changed --
   -------------------

   overriding procedure Queue_Changed
     (Manager           : access Task_Manager_UI_Record;
      Index             : Integer;
      Immediate_Refresh : Boolean)
   is
      use type Glib.Main.G_Source_Id;
      GUI : constant Task_Manager_Interface := Manager.GUI;
   begin
      if Immediate_Refresh then
         Task_Changed_Hook.Run (GUI.Kernel, Queue_Id => Index);
         Refresh (GUI);
      else
         --  Add the index to the list of indexes to be refreshed

         if GUI.Manager.Queues /= null
           and then Index in GUI.Manager.Queues'Range
         then
            GUI.Manager.Queues (Index).To_Refresh := True;

            --  Register the timeout callback

            if GUI.Timeout_Cb = Glib.Main.No_Source_Id then
               GUI.Timeout_Cb := Task_Manager_Source.Timeout_Add
                 (Interval => Refresh_Timeout,
                  Func     => GUI_Refresh_Cb'Access,
                  Data     => (GUI => GUI, Index => Index));
            end if;
         end if;
      end if;
   end Queue_Changed;

   ------------------
   -- Get_Fraction --
   ------------------

   function Get_Fraction
     (Manager : access Task_Manager_Record'Class;
      Index   : Natural) return Gdouble
   is
      Fraction   : Gdouble;
      Task_Queue : Task_Queue_Access;
      Length     : Integer;
      Command    : Command_Access;
      Progress   : Progress_Record;
   begin
      if Manager.Queues = null
        or else Index = 0
        or else Index > Manager.Queues'Length
      then
         return 0.0;
      end if;

      Task_Queue := Manager.Queues (Manager.Queues'First - 1 + Index);
      Length := Integer (Task_Queue.Queue.Length);

      if Length /= 0 then
         Command := Task_Queue.Queue.First_Element;
         Progress := Commands.Progress (Command);

         if Progress.Total = 0 then
            Fraction := 0.0;
         else
            Fraction := Gdouble (Progress.Current) /
              Gdouble (Progress.Total);
         end if;

         if Manager.Queues (Index).Total > 1 then
            Fraction := (Fraction + Gdouble (Manager.Queues (Index).Done))
              / Gdouble (Manager.Queues (Index).Total);
         end if;

         return Fraction;
      end if;

      return 0.0;
   end Get_Fraction;

   -----------------------
   -- Get_Progress_Text --
   -----------------------

   function Get_Progress_Text
     (Manager                : access Task_Manager_Record'Class;
      Index                  : Natural;
      As_Percent             : Boolean;
      With_Name_And_Fraction : Boolean) return Progress_Data
   is
      Fraction   : constant Gdouble := Get_Fraction (Manager, Index);
      Task_Queue : Task_Queue_Access;
      Length     : Integer;
      Command    : Command_Access;
      Progress   : Progress_Record;

      function Progress_Indicator return String;
      pragma Inline (Progress_Indicator);
      --  Return the progress indicator in xxx/yyy or in xx% format, depending
      --  on the value of As_Percent.

      function Descr return String;
      pragma Inline (Descr);
      --  Return the description of the task

      ------------------------
      -- Progress_Indicator --
      ------------------------

      function Progress_Indicator return String is
      begin
         if As_Percent then
            return Image (Integer (Fraction * 100.0)) & "%";

         else
            if Progress.Total <= 1 then
               return "";
            else
               return
                 (GNATCOLL.Utils.Image (Progress.Current, Min_Width => 0)
                  & "/"
                  & GNATCOLL.Utils.Image (Progress.Total, Min_Width => 0));
            end if;
         end if;
      end Progress_Indicator;

      -----------
      -- Descr --
      -----------

      function Descr return String is
      begin
         if With_Name_And_Fraction then
            return Commands.Name (Command);
         elsif Length > 1 then
            return Image (Length) & (-" queued");
         else
            return "";
         end if;
      end Descr;

   begin
      if Manager.Queues = null
        or else Index = 0
        or else Index > Manager.Queues'Length
      then
         return Null_Progress_Data;
      end if;

      Task_Queue := Manager.Queues (Manager.Queues'First - 1 + Index);
      Length := Integer (Task_Queue.Queue.Length);

      if Length /= 0 then
         Command := Task_Queue.Queue.First_Element;
         Progress := Commands.Progress (Command);

         --  Assemble output

         declare
            D : constant String := Descr;
            Progress : constant String := Progress_Indicator;
         begin
            return (D'Length, Progress'Length,
                    Fraction, D, Progress, Multiple_Queues => False);
         end;

      else
         return Null_Progress_Data;
      end if;
   end Get_Progress_Text;

   -----------------------
   -- Get_Progress_Text --
   -----------------------

   function Get_Progress_Text
     (Manager    : access Task_Manager_Record'Class;
      As_Percent : Boolean) return Progress_Data
   is
      Index    : Natural := 0;
      Count    : Natural := 0;
      Fraction : Gdouble := 0.0;
   begin
      if Manager.Queues = null then
         return Null_Progress_Data;

      else
         for J in Manager.Queues'Range loop
            if Manager.Queues (J).Show_Bar then
               Count := Count + 1;

               if Index = 0 then
                  Index := J;
                  Fraction := Get_Fraction (Manager, J);
               else
                  Fraction := Fraction + Get_Fraction (Manager, J);
               end if;
            end if;
         end loop;

         if Count = 0 then
            return Null_Progress_Data;

         elsif Count = 1 then
            return Get_Progress_Text (Manager, Index, As_Percent, True);

         else
            declare
               F : constant Gdouble := Fraction / Gdouble (Count);
               S : constant String :=
                 GNATCOLL.Utils.Image (Count, Min_Width => 0) & " tasks";
               P : constant String := Image (Integer (F * 100.0)) & "%";
            begin
               return (S'Length, P'Length, F, S, P, Multiple_Queues => True);
            end;
         end if;
      end if;
   end Get_Progress_Text;

   ------------
   -- Create --
   ------------

   function Create (Kernel : Kernel_Handle) return Task_Manager_Access is
      Manager : Task_Manager_Access;
      View : Task_Manager_Interface;
      Image   : Gtk_Image;
      VBox     : Gtk_Box;
      HBox    : Gtk_Box;
      Event   : Gtk_Event_Box;
      Label_Box : Gtk_Box;

   begin
      Manager := new Task_Manager_UI_Record;
      View := new Task_Manager_Interface_Record;
      Task_Manager_UI_Access (Manager).GUI := View;
      Task_Manager_UI_Access (Manager).Kernel := Kernel;

      Initialize_Hbox (View, Homogeneous => False);
      Get_Style_Context (View).Add_Class ("gps-task-manager");

      --  The progress bar area

      Gtk_New_Hbox (HBox);
      View.Pack_Start (HBox, Expand => True);

      Gtk_New (Event);
      Event.Set_Has_Window (False);
      HBox.Pack_Start (Event, Expand => True);

      Gtk_New_Vbox (VBox);
      Event.Add (VBox);

      Gtk_New_Hbox (Label_Box);
      VBox.Pack_Start (Label_Box);

      Gtk_New (View.Task_Label, "");
      View.Task_Label.Set_Alignment (0.0, 0.5);
      View.Task_Label.Override_Font (Small_Font.Get_Pref);
      View.Task_Label.Set_Ellipsize (Ellipsize_End);
      Label_Box.Pack_Start (View.Task_Label, Expand => False);

      Gtk_New (View.Progress_Label, "");
      View.Progress_Label.Set_Alignment (1.0, 0.5);
      View.Progress_Label.Set_Ellipsize (Ellipsize_End);
      View.Progress_Label.Override_Font (Small_Font.Get_Pref);
      Label_Box.Pack_End (View.Progress_Label, Expand => False);

      Gtk_New (View.Main_Progress_Bar);
      View.Main_Progress_Bar.Override_Font (Small_Font.Get_Pref);
      View.Main_Progress_Bar.Set_Show_Text (False);
      VBox.Pack_Start (View.Main_Progress_Bar, Padding => 2);

      Gtk_New (View.Progress_Bar_Button);
      Gtk_New_From_Icon_Name
         (Image, "gps-stop-symbolic", Icon_Size_Action_Button);
      View.Progress_Bar_Button.Add (Image);
      View.Progress_Bar_Button.Set_Relief (Relief_None);
      HBox.Pack_Start (View.Progress_Bar_Button, Expand => False);

      Task_Manager_Handler.Connect
        (View.Progress_Bar_Button, Gtk.Button.Signal_Clicked,
         On_Progress_Bar_Button_Clicked'Access,
         User_Data => (View, 0));

      Set_Events (Event, Get_Events (Event) or Button_Press_Mask);
      Return_Callback.Object_Connect
        (Event,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller
           (On_Main_Progress_Button_Press_Event'Access),
         View);

      View.Kernel  := Kernel;
      View.Manager := Task_Manager_UI_Access (Manager);

      Task_Manager_Handler.Connect
        (View,
         "destroy",
         Task_Manager_Handler.To_Marshaller (On_GUI_Destroy'Access),
         User_Data => (View, 0),
         After => False);

      return Manager;
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Task_Manager_Module_Record) is
   begin
      Destroy (Get_Task_Manager (Get_Kernel (Module)));
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Align   : Gtk_Alignment;
      Manager : Task_Manager_Access;
   begin
      --  Create the main progress bar in the main toolbar
      Manager := Create (Kernel_Handle (Kernel));
      Set_Task_Manager (Kernel, Manager);

      --  Display the main progress bar in the GPS main toolbar

      Gtk_New (Align, 0.0, 1.0, 0.0, 0.0);
      Align.Set_Padding (0, 0, 0, 10);  --  to the right
      Align.Add (Task_Manager_UI_Access (Manager).GUI);
      GPS_Window (Get_Main_Window (Kernel)).Toolbar_Box.Pack_End
        (Align, Expand => False);
   end Register_Module;

end Task_Manager.GUI;
