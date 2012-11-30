------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with System;                     use System;
with System.Storage_Elements;    use System.Storage_Elements;

with Gdk.Event;                  use Gdk.Event;

with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;

with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Progress; use Gtk.Cell_Renderer_Progress;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Event_Box;              use Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Icon_Factory;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Settings;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Stock;                  use Gtk.Stock;

with Gtkada.Abstract_List_Model; use Gtkada.Abstract_List_Model;
with Gtk.Tree_Model.Utils;       use Gtk.Tree_Model.Utils;

with Gtkada.Handlers;            use Gtkada.Handlers;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GUI_Utils;                  use GUI_Utils;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;
with GNAT.IO; use GNAT.IO;

package body Task_Manager.GUI is

   ---------------------
   -- Local constants --
   ---------------------

   Refresh_Timeout     : constant := 200;
   --  The timeout to refresh the GUI, in milliseconds

   Progress_Bar_Length : constant := 200;
   --  The length of the progress bar, in number of pixels
   --  ??? This hard-codes the size of the tree column.

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column             : constant := 0;
   Command_Name_Column     : constant := 1;
   Command_Progress_Column : constant := 2;
   Command_Text_Column     : constant := 3;
   Command_Button_Column   : constant := 4;
   Pause_Button_Column     : constant := 5;

   -----------------
   -- Local types --
   -----------------

   type Manager_Index_Record is record
      D     : Task_Manager_Interface;
      Index : Integer;
   end record;

   -----------------------
   -- Local subprograms --
   -----------------------

   package Task_Manager_Handler is new Gtk.Handlers.User_Callback
     (GObject_Record, Manager_Index_Record);

   procedure Set_Column_Types
     (View : access Task_Manager_Widget_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   procedure On_Progress_Bar_Button_Clicked
     (Object : access GObject_Record'Class;
      Data   : Manager_Index_Record);
   --  Callback for a click on the global progress bar "x" button

   function On_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for a "button_press_event" on a tree view

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

   procedure On_Progress_Bar_Destroy
     (Object  : access GObject_Record'Class;
      Manager : Manager_Index_Record);
   pragma Unreferenced (On_Progress_Bar_Destroy);
   --  Called when a progress bar is destroyed

   procedure Refresh (GUI : Task_Manager_Interface);
   --  Refresh the information in View from the Task_Manager

   procedure Init_Graphics
     (GUI : access Task_Manager_Interface_Record'Class);
   --  Initialize the graphic elements needed to render the tree view

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

   procedure Refresh_One_Index
     (GUI   : Task_Manager_Interface;
      Index : Integer);
   --  Perform graphical refresh of one command in the GUI

   procedure Process_Pending_Refreshes (GUI : Task_Manager_Interface);
   --  Process all pending refreshes

   function GUI_Refresh_Cb (GUI : Task_Manager_Interface) return Boolean;
   --  Timeout callback that refreshes the GUI

   procedure Unregister_Timeout (GUI : Task_Manager_Interface);
   --  Remove the timeout that refreshes the GUI and clear the list of items
   --  that need to be refreshed.

   -----------------------------
   -- On_Progress_Bar_Destroy --
   -----------------------------

   procedure On_Progress_Bar_Destroy
     (Object  : access GObject_Record'Class;
      Manager : Manager_Index_Record)
   is
      pragma Unreferenced (Object);
   begin
      Pop_State (Manager.D.Manager);
   end On_Progress_Bar_Destroy;

   -----------------------
   -- Interrupt_Command --
   -----------------------

   procedure Interrupt_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         Interrupt (Manager.Queues (Index).Queue.First_Element.all);

         Manager.Queues (Index).Status := Completed;
         Queue_Changed (Manager, Index, True);
         Run (Task_Manager_Access (Manager),
              Active => Index < Manager.Passive_Index);
      end if;
   end Interrupt_Command;

   -------------------
   -- Pause_Command --
   -------------------

   procedure Pause_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         if Manager.Queues (Index).Status = Running then
            Manager.Queues (Index).Status := Paused;
         end if;

         Queue_Changed (Manager, Index, True);
      end if;
   end Pause_Command;

   --------------------
   -- Resume_Command --
   --------------------

   procedure Resume_Command
     (Manager : access Task_Manager_Record'Class;
      Index   : Integer) is
   begin
      if Manager.Queues = null then
         return;
      end if;

      if Index in Manager.Queues'Range then
         if Manager.Queues (Index).Status = Paused then
            Manager.Queues (Index).Status := Running;
            Run (Task_Manager_Access (Manager),
                 Active => Index < Manager.Passive_Index);
         end if;

         Queue_Changed (Manager, Index, True);
      end if;
   end Resume_Command;

   -----------------------------------------
   -- On_Main_Progress_Button_Press_Event --
   -----------------------------------------

   function On_Main_Progress_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      GUI : constant Task_Manager_Interface :=
        Task_Manager_Interface (Object);
   begin
      Put_Line ("MANU On_Main_Progress_Button_Press_Event");
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Show_Task_Manager (GUI.Kernel);
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Main_Progress_Button_Press_Event;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   function On_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Iface : constant Task_Manager_Widget_Access :=
        Task_Manager_Widget_Access (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Col   : Gtk_Tree_View_Column;

   begin
      Model := Get_Model (Iface.Tree);
      Coordinates_For_Event (Iface.Tree, Event, Iter, Col);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);

         declare
            A     : constant Gint_Array := Get_Indices (Path);
            Index : constant Natural := Natural (A (A'First)) + 1;
         begin
            if Get_Button (Event) = 1 then
               if Col = Iface.Quit_Button_Col then
                  Interrupt_Command (Iface.Model.Manager, Index);
               elsif Col = Iface.Pause_Button_Col then
                  if Iface.Model.Manager.Queues (Index).Status = Running then
                     Pause_Command (Iface.Model.Manager, Index);
                  else
                     Resume_Command (Iface.Model.Manager, Index);
                  end if;
               end if;
            end if;
         end;

         Path_Free (Path);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Button_Press_Event;

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
   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
         View.Logo.Set_Child_Visible (True);
         View.Logo.Show_All;

         View.Main_Progress_Bar.Set_Child_Visible (False);
         View.Progress_Bar_Button.Set_Child_Visible (False);
         View.Task_Label.Set_Child_Visible (False);
         View.Main_Progress_Bar.Hide;
         View.Progress_Bar_Button.Hide;
         View.Task_Label.Hide;

      else
         View.Logo.Set_Child_Visible (False);
         View.Logo.Hide;

         View.Main_Progress_Bar.Set_Child_Visible (True);
         View.Progress_Bar_Button.Set_Child_Visible (True);
         View.Task_Label.Set_Child_Visible (True);
         View.Main_Progress_Bar.Show_All;
         View.Progress_Bar_Button.Show_All;
         View.Task_Label.Show_All;
      end if;
   end Set_Mode;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (GUI : Task_Manager_Interface) is
   begin
      if GUI.Manager.Queues = null then
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
               GUI.Main_Progress_Bar.Set_Text (Pd.Progress_Text);
               GUI.Progress_Bar_Button.Set_Sensitive (not Pd.Multiple_Queues);

               GUI.Main_Progress_Bar.Set_Tooltip_Markup
                 (Pd.Text & " (" & Pd.Progress_Text & ")" & ASCII.LF
                  & (-"<i>Double-click to open the Task Manager</i>"));
            end if;
         end;
      end if;
   end Refresh;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (View : access Task_Manager_Widget_Record'Class)
   is
      Tree          : constant Gtk_Tree_View := View.Tree;
      Col           : Gtk_Tree_View_Column;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Progress_Rend : Gtk_Cell_Renderer_Progress;
      Dummy         : Gint;
      W, H          : Gint;
      Result        : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Set_Rules_Hint (Tree, False);

      Gtk.Icon_Factory.Icon_Size_Lookup_For_Settings
        (Gtk.Settings.Get_Default, Icon_Size_Menu,
         W, H, Result);

      Gtk_New (View.Quit_Button_Col);
      Gtk_New (Pixbuf_Rend);
      Pack_End (View.Quit_Button_Col, Pixbuf_Rend, False);
      Add_Attribute
        (View.Quit_Button_Col, Pixbuf_Rend, "pixbuf", Command_Button_Column);
      Dummy := Append_Column (Tree, View.Quit_Button_Col);

      Gtk_New (Col);
      Set_Expand (Col, False);
      Gtk_New (Progress_Rend);
      Progress_Rend.Set_Fixed_Size (Progress_Bar_Length, H);
      Pack_Start (Col, Progress_Rend, False);
      Add_Attribute (Col, Progress_Rend, "value", Command_Progress_Column);
      Add_Attribute (Col, Progress_Rend, "text", Command_Text_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (View.Pause_Button_Col);
      Gtk_New (Pixbuf_Rend);
      Pack_End (View.Pause_Button_Col, Pixbuf_Rend, False);
      Add_Attribute
        (View.Pause_Button_Col, Pixbuf_Rend, "pixbuf", Pause_Button_Column);
      Dummy := Append_Column (Tree, View.Pause_Button_Col);
   end Set_Column_Types;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column             => Gdk.Pixbuf.Get_Type,
         Command_Name_Column     => GType_String,
         Command_Progress_Column => GType_Int,
         Command_Text_Column     => GType_String,
         Command_Button_Column   => Gdk.Pixbuf.Get_Type,
         Pause_Button_Column     => Gdk.Pixbuf.Get_Type);
   end Columns_Types;

   -------------------------
   -- Task_Manager_Dialog --
   -------------------------

   function Task_Manager_Dialog
     (Manager : Task_Manager_Access;
      Dialog  : Gtk_Widget := null) return Task_Manager_Widget_Access
   is
      GUI      : constant Task_Manager_Interface :=
                   Task_Manager_UI_Access (Manager).GUI;
      View     : Task_Manager_Widget_Access;
      Scrolled : Gtk_Scrolled_Window;
   begin
      View := new Task_Manager_Widget_Record;
      Initialize_Hbox (View);

      --  Initialize the tree

      View.Dialog := Dialog;
      View.Model := GUI;

      Gtk_New (View.Tree, GUI.Model);
      Set_Enable_Search (View.Tree, False);

      Set_Name (View.Tree, "Task Manager Tree");  --  For testsuite

      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, False);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      Set_Font_And_Colors (View.Tree, Fixed_Font => True);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Button_Press_Event'Access),
         View,
         After => False);

      return View;
   end Task_Manager_Dialog;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View    : out Task_Manager_Interface;
      Kernel  : Kernel_Handle;
      Manager : Task_Manager_Access) is
   begin
      View := new Task_Manager_Interface_Record;
      Initialize (View, Kernel, Manager);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Kernel  : Kernel_Handle;
      Manager : Task_Manager_Access)
   is
      Model : constant Task_Manager_Model := new Task_Manager_Model_Record;
      GPS_Dir  : constant Virtual_File := Create_From_Dir
        (Kernel.Get_System_Dir, "/share/gps/icons");
      Image_File : constant Virtual_File :=
        Create_From_Dir (GPS_Dir, "24px/gps_24.png");
      Image_Close : constant Virtual_File :=
        Create_From_Dir (GPS_Dir, "9px/close_8.png");
      Image   : Gtk_Image;
      Box     : Gtk_Box;
      VBox    : Gtk_Box;
      Event   : Gtk_Event_Box;
   begin
      Initialize_Hbox (View, Homogeneous => False);
      Get_Style_Context (View).Add_Class ("gps-task-manager");

      --  The logo goes to the left of everything, so that when it is hidden
      --  it doesn't matter if it is part of the size computation for the
      --  task manager

      Gtk_New (View.Logo, Filename => +Image_File.Full_Name.all);
      View.Pack_Start (View.Logo, Expand => False);

      --  The progress bar area

      Gtk_New_Vbox (VBox);
      View.Pack_Start (VBox, Expand => True);

      Gtk_New (View.Task_Label, "");
      View.Task_Label.Set_Alignment (0.0, 0.5);
      View.Task_Label.Override_Font (Small_Font.Get_Pref);
      VBox.Pack_Start (View.Task_Label, Expand => False);

      Gtk_New_Hbox (Box);
      VBox.Pack_Start (Box, Expand => False);

      Gtk_New (Event);
      Event.Set_Has_Window (False);
      Box.Pack_Start (Event, Expand => True);

      Gtk_New (View.Main_Progress_Bar);
      View.Main_Progress_Bar.Override_Font (Small_Font.Get_Pref);
      View.Main_Progress_Bar.Set_Show_Text (True);
      Event.Add (View.Main_Progress_Bar);

      Gtk_New (View.Progress_Bar_Button);
      Gtk_New (Image, +Image_Close.Full_Name.all);
      View.Progress_Bar_Button.Add (Image);
      View.Progress_Bar_Button.Set_Relief (Relief_None);
      Box.Pack_Start (View.Progress_Bar_Button, Expand => False);

      Task_Manager_Handler.Connect
        (View.Progress_Bar_Button, Gtk.Button.Signal_Clicked,
         On_Progress_Bar_Button_Clicked'Access,
         User_Data => (Task_Manager_Interface (View), 0));

      Set_Events (Event, Get_Events (Event) or Button_Press_Mask);
      Return_Callback.Object_Connect
        (Event,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller
           (On_Main_Progress_Button_Press_Event'Access),
         View);

      Gtkada.Abstract_List_Model.Initialize (Model);
      Model.GUI := Task_Manager_Interface (View);

      View.Kernel  := Kernel;
      View.Manager := Task_Manager_UI_Access (Manager);
      View.Model   := Model;
      View.Manager.GUI := Task_Manager_Interface (View);

      Task_Manager_Handler.Connect
        (View,
         "destroy",
         Task_Manager_Handler.To_Marshaller (On_GUI_Destroy'Access),
         User_Data => (Task_Manager_Interface (View), 0),
         After => False);
   end Initialize;

   ---------------
   -- Pop_State --
   ---------------

   procedure Pop_State (Manager : access Task_Manager_Record'Class) is
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      if Manager.Pop_Command /= null then
         Dummy := Execute (Manager.Pop_Command);
      end if;
   end Pop_State;

   ----------------
   -- Push_State --
   ----------------

   procedure Push_State (Manager : access Task_Manager_Record'Class) is
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      if Manager.Push_Command /= null then
         Dummy := Execute (Manager.Push_Command);
      end if;
   end Push_State;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Task_Manager_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      if Path = Null_Gtk_Tree_Path then
         return Null_Iter;
      end if;

      declare
         Indices : constant Glib.Gint_Array :=
            Gtk.Tree_Model.Get_Indices (Path);
            Index_1 : Integer_Address;
      begin
         if Indices'Length = 0 then
            return Null_Iter;
         end if;

         Index_1 := Integer_Address (Indices (Indices'First));

         if Self.GUI.Manager.Queues = null
           or else Index_1 >= Self.GUI.Manager.Queues'Length
         then
            return Null_Iter;

         else
            return Init_Tree_Iter
              (Stamp       => 1,
               User_Data_1 => To_Address (Index_1 + 1),
               User_Data_2 => System.Null_Address,
               User_Data_3 => System.Null_Address);
         end if;
      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return Null_Iter;
      end;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Task_Manager_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      pragma Unreferenced (Self);
      Result : Gtk_Tree_Path;

   begin
      if Iter = Null_Iter then
         raise Program_Error with "Get_Path with null iter";
      end if;

      Gtk_New (Result);
      Append_Index (Result, Gint (To_Integer (Get_User_Data_1 (Iter)) - 1));
      return Result;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Path_Free (Result);
         Gtk_New (Result, "");
         return Result;
   end Get_Path;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Task_Manager_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Old_Index : constant Integer_Address :=
                    To_Integer (Get_User_Data_1 (Iter));
   begin
      if Self.GUI.Manager.Queues = null
        or else Old_Index >= Self.GUI.Manager.Queues'Length
      then
         Iter := Null_Iter;

      else
         Iter := Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address (Old_Index + 1),
            User_Data_2 => System.Null_Address,
            User_Data_3 => System.Null_Address);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Next;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Task_Manager_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is
   begin
      if Iter = Null_Iter
        and then Self.GUI.Manager.Queues /= null
      then
         return Self.GUI.Manager.Queues'Length;
      end if;

      return 0;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return 0;
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Task_Manager_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter;
   begin
      if Parent = Null_Iter
        and then Self.GUI.Manager.Queues /= null
        and then N <= Self.GUI.Manager.Queues'Length
      then
         Iter := Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address (Integer_Address (N) + 1),
            User_Data_2 => System.Null_Address,
            User_Data_3 => System.Null_Address);

         return Iter;
      end if;

      return Null_Iter;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Null_Iter;
   end Nth_Child;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Task_Manager_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);
   begin
      return Columns_Types'Length;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return 0;
   end Get_N_Columns;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Task_Manager_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);
      T : constant GType_Array := Columns_Types;
   begin
      return T (Guint (Index));

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return T (0);
   end Get_Column_Type;

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

      if GUI.Close_Button_Pixbuf /= null then
         Unref (GUI.Close_Button_Pixbuf);
         Unref (GUI.Pause_Button_Pixbuf);
         Unref (GUI.Play_Button_Pixbuf);
      end if;

      Unregister_Timeout (GUI);

      if GUI.Model /= null then
         Unref (GUI.Model);
         GUI.Model := null;
      end if;
   end On_GUI_Destroy;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics
     (GUI : access Task_Manager_Interface_Record'Class)
   is
   begin
      if GUI.Close_Button_Pixbuf /= null
        or else GUI.Main_Progress_Bar = null
      then
         --  Already initialized or cannot initialize now
         return;
      end if;

      GUI.Close_Button_Pixbuf := Render_Icon
        (GUI.Main_Progress_Bar, Stock_Close, Icon_Size_Menu);
      GUI.Pause_Button_Pixbuf := Render_Icon
        (GUI.Main_Progress_Bar, Stock_Media_Pause, Icon_Size_Menu);
      GUI.Play_Button_Pixbuf := Render_Icon
        (GUI.Main_Progress_Bar, Stock_Media_Play, Icon_Size_Menu);
   end Init_Graphics;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Task_Manager_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Index      : constant Integer_Address :=
                     To_Integer (Get_User_Data_1 (Iter));
      Task_Queue : Task_Queue_Access;
      Length     : Integer;
      Command    : Command_Access;
   begin
      if Iter = Null_Iter then
         raise Program_Error with "Null iter";
      end if;

      if Index = 0 then
         raise Program_Error with "Null index";
      end if;

      if Self.GUI.Manager.Queues = null
        or else Index > Self.GUI.Manager.Queues'Length
      then
         return;
      end if;

      Task_Queue := Self.GUI.Manager.Queues
        (Self.GUI.Manager.Queues'First - 1 + Integer (Index));

      Length := Integer (Task_Queue.Queue.Length);

      if Length /= 0 then
         Command := Task_Queue.Queue.First_Element;

         case Column is
            when Command_Name_Column =>
               Init (Value, GType_String);
               Set_String (Value, Commands.Name (Command));

            when Command_Progress_Column =>
               Init (Value, GType_Int);
               Set_Int
                 (Value,
                  Gint
                    (Get_Fraction (Self.GUI.Manager, Integer (Index)) *
                       100.0));

            when Command_Text_Column =>
               Init (Value, GType_String);
               Set_String
                 (Value,
                  Get_Progress_Text
                    (Task_Manager_Access (Self.GUI.Manager),
                     Integer (Index), False, True).Text);

            when Command_Button_Column =>
               Init_Graphics (Self.GUI);
               Init (Value, Gdk.Pixbuf.Get_Type);
               Set_Object (Value, GObject (Self.GUI.Close_Button_Pixbuf));

            when Pause_Button_Column =>
               Init_Graphics (Self.GUI);
               Init (Value, Gdk.Pixbuf.Get_Type);

               if Task_Queue.Status = Paused then
                  Set_Object (Value, GObject (Self.GUI.Play_Button_Pixbuf));
               else
                  Set_Object (Value, GObject (Self.GUI.Pause_Button_Pixbuf));
               end if;

            when others =>
               Init (Value, Gdk.Pixbuf.Get_Type);
               Set_Object (Value, null);
         end case;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Init (Value, GType_String);
         Set_String (Value, "");
   end Get_Value;

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
      Integer_Stack.Clear (GUI.To_Refresh);
   end Unregister_Timeout;

   -----------------
   -- Queue_Added --
   -----------------

   overriding procedure Queue_Added
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer)
   is
      GUI  : constant Task_Manager_Interface := Manager.GUI;
      Iter : constant Gtk_Tree_Iter :=
               Nth_Child (GUI.Model, Null_Iter, Gint (Index - 1));
      Path : constant Gtk_Tree_Path := Get_Path (GUI.Model, Iter);
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      Row_Inserted (+GUI.Model, Path, Iter);
      Path_Free (Path);
      Refresh (GUI);

      if GUI.Manager.Queues (Index).Show_Bar then
         Dummy := Execute (GUI.Manager.Push_Command);
      end if;

      Unregister_Timeout (GUI);
   end Queue_Added;

   -------------------
   -- Queue_Removed --
   -------------------

   overriding procedure Queue_Removed
     (Manager : access Task_Manager_UI_Record;
      Index   : Integer)
   is
      GUI  : constant Task_Manager_Interface := Manager.GUI;
      Path : Gtk_Tree_Path;
      Dummy : Command_Return_Type;
      pragma Unreferenced (Dummy);
   begin
      Gtk_New_First (Path);
      Row_Deleted (+GUI.Model, Path);
      Path_Free (Path);
      Refresh (GUI);

      if GUI.Manager.Queues (Index).Show_Bar then
         Dummy := Execute (GUI.Manager.Pop_Command);
      end if;

      Unregister_Timeout (GUI);
   end Queue_Removed;

   -----------------------
   -- Refresh_One_Index --
   -----------------------

   procedure Refresh_One_Index
     (GUI   : Task_Manager_Interface;
      Index : Integer)
   is
      Iter : constant Gtk_Tree_Iter :=
               Nth_Child (GUI.Model, Null_Iter, Gint (Index - 1));
      Path : constant Gtk_Tree_Path := Get_Path (GUI.Model, Iter);
   begin
      Row_Changed (+GUI.Model, Path, Iter);
      Path_Free (Path);
   end Refresh_One_Index;

   -------------------------------
   -- Process_Pending_Refreshes --
   -------------------------------

   procedure Process_Pending_Refreshes (GUI : Task_Manager_Interface) is
      Index : Integer;
      use Integer_Stack;
   begin
      while not Is_Empty (GUI.To_Refresh) loop
         Pop (GUI.To_Refresh, Index);
         Refresh_One_Index (GUI, Index);
      end loop;
   end Process_Pending_Refreshes;

   --------------------
   -- GUI_Refresh_Cb --
   --------------------

   function GUI_Refresh_Cb (GUI : Task_Manager_Interface) return Boolean is
   begin
      Process_Pending_Refreshes (GUI);
      Refresh (GUI);

      GUI.Timeout_Cb := Glib.Main.No_Source_Id;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         GUI.Timeout_Cb := Glib.Main.No_Source_Id;
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
         Refresh_One_Index (GUI, Index);
         Refresh (GUI);

      else
         --  Add the index to the list of indexes to be refreshed

         Integer_Stack.Push (GUI.To_Refresh, Index);

         --  Register the timeout callback

         if GUI.Timeout_Cb = Glib.Main.No_Source_Id then
            GUI.Timeout_Cb := Task_Manager_Source.Timeout_Add
              (Interval => Refresh_Timeout,
               Func     => GUI_Refresh_Cb'Access,
               Data     => GUI);
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
      R : Task_Manager_Access;
   begin
      R := new Task_Manager_UI_Record;
      Gtk_New (Task_Manager_UI_Access (R).GUI, Kernel, R);
      return R;
   end Create;

   ----------------------
   -- Get_Focus_Widget --
   ----------------------

   function Get_Focus_Widget
     (GUI : Task_Manager_Widget_Access) return Gtk_Widget is
   begin
      return Gtk_Widget (GUI.Tree);
   end Get_Focus_Widget;

   -------------
   -- Get_GUI --
   -------------

   function Get_GUI (Manager : Task_Manager_Access) return Gtk_Widget is
   begin
      return Gtk_Widget (Task_Manager_UI_Access (Manager).GUI);
   end Get_GUI;

   -------------
   -- Set_GUI --
   -------------

   procedure Set_GUI (Manager : Task_Manager_Access; GUI : Gtk_Widget) is
   begin
      Task_Manager_UI_Access (Manager).GUI := Task_Manager_Interface (GUI);
   end Set_GUI;

   -----------------------
   -- Set_Progress_Area --
   -----------------------

   procedure Set_Progress_Area
     (Manager : Task_Manager_Access;
      Area    : Gtk.Box.Gtk_Hbox) is
   begin
      Area.Pack_End (Task_Manager_UI_Access (Manager).GUI, Expand => False);
   end Set_Progress_Area;

end Task_Manager.GUI;
