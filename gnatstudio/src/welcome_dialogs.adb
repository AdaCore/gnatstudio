------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GNAT.Strings;           use GNAT.Strings;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with Glib.Object;
with Gdk.Event;              use Gdk.Event;
with Gtk.Box;                use Gtk.Box;
with Gtk.Button;             use Gtk.Button;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Image;              use Gtk.Image;
with Gtk.Label;              use Gtk.Label;
with Gtk.List_Box;           use Gtk.List_Box;
with Gtk.List_Box_Row;       use Gtk.List_Box_Row;
with Gtk.Main;
with Gtk.Paned;              use Gtk.Paned;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Size_Group;         use Gtk.Size_Group;
with Gtk.Style_Context;      use Gtk.Style_Context;
with Gtk.Widget;             use Gtk.Widget;

with Dialog_Utils;           use Dialog_Utils;
with Histories;              use Histories;
with GPS.Dialogs;            use GPS.Dialogs;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Main_Window;        use GPS.Main_Window;
with GUI_Utils;              use GUI_Utils;

package body Welcome_Dialogs is

   Me : constant Trace_Handle := Create ("GPS.MAIN.WELCOME_DIALOG");

   type Welcome_Dialog_Record is new GPS_Dialog_Record with record
      Response : Welcome_Dialog_Response := Quit_GPS;
   end record;
   type Welcome_Dialog is access all Welcome_Dialog_Record'Class;
   --  Type representing a welcome dialog

   type Recent_Projects_List_Box_Record is new Gtk_List_Box_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Recent_Projects_List_Box is
     access all Recent_Projects_List_Box_Record'Class;
   --  Type representing the recent projects list displayed on the left side
   --  of the welcome dialog.

   type Recent_Project_Item_Box_Record is new Gtk_Vbox_Record with record
      Project_File : Virtual_File;
   end record;
   type Recent_Project_Item_Box is
     access all Recent_Project_Item_Box_Record'Class;
   --  Type representing the recent project items displayed in the recent
   --  projects list view.

   type Welcome_Dialog_Action_Button_Record is new Gtk_Button_Record
   with record
      Kernel   : Kernel_Handle;
      Dialog   : Welcome_Dialog;
      Callback : Welcome_Dialog_Action_Callback;
   end record;
   type Welcome_Dialog_Action_Button is
     access all Welcome_Dialog_Action_Button_Record'Class;
   --  Type representing welcome dialog actions' buttons

   function On_Delete
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the welcome dialog is closed. Set the dialog's response
   --  accordingly (i.e: Quit_GPS).

   procedure On_Row_Activated
     (Self : access Glib.Object.GObject_Record'Class;
      Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class);
   --  Called when a recent project item is double-clicked. Open the associated
   --  project.

   procedure On_Clicked (Self : access Gtk_Button_Record'Class);
   --  Called when a welcome dialog action's button is clicked. Call the
   --  associated callback and set the dialog's response accordingly.

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Dialog : constant Welcome_Dialog := Welcome_Dialog (Self);
   begin
      Dialog.Response := Quit_GPS;
      Gtk.Main.Main_Quit;

      return True;
   end On_Delete;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
     (Self : access Glib.Object.GObject_Record'Class;
      Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   is
      Dialog : constant Welcome_Dialog := Welcome_Dialog (Self);
      Item   : constant Recent_Project_Item_Box :=
                 Recent_Project_Item_Box (Row.Get_Child);
   begin
      Gtk.Main.Main_Quit;
      Load_Project (Dialog.Kernel, Item.Project_File);
      Dialog.Response := Project_Loaded;
   end On_Row_Activated;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked (Self : access Gtk_Button_Record'Class) is
      Action_Button : constant Welcome_Dialog_Action_Button :=
                        Welcome_Dialog_Action_Button (Self);
      Success       : Boolean;
   begin
      Success := Action_Button.Callback
        (Kernel => Action_Button.Kernel,
         Parent => Action_Button.Dialog);

      if Success then
         Action_Button.Dialog.Response := Project_Loaded;
         Gtk.Main.Main_Quit;
      end if;
   end On_Clicked;

   ------------
   -- Create --
   ------------

   function Create
     (Callback  : not null Welcome_Dialog_Action_Callback;
      Label     : String;
      Icon_Name : String) return Welcome_Dialog_Action
   is
     (Welcome_Dialog_Action'
        (Callback  => Callback,
         Label     => To_Unbounded_String (Label),
         Icon_Name => To_Unbounded_String (Icon_Name)));

   -------------------------------
   -- Display_Welcome_Dialog --
   -------------------------------

   function Display_Welcome_Dialog
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Actions : Welcome_Dialog_Action_Array)
      return Welcome_Dialog_Response
   is
      Dialog               : Welcome_Dialog;
      Pane                 : Gtk_Paned;
      Main_View            : Dialog_View;
      Response             : Welcome_Dialog_Response;
      Scrolled             : Gtk_Scrolled_Window;
      Recent_Projects_View : Recent_Projects_List_Box;
      Recent_Projects      : constant String_List_Access :=
                             Get_History
                               (Kernel.Get_History.all, "project_files");

      procedure Fill_Recent_Projects_View;

      procedure Create_Welcome_Dialog_Options;

      -------------------------------
      -- Fill_Recent_Projects_View --
      -------------------------------

      procedure Fill_Recent_Projects_View is
         Project_File    : Virtual_File;

         Label           : Gtk_Label;
         Item            : Recent_Project_Item_Box;
      begin
         for Project_Name of Recent_Projects.all loop
            if Create_From_UTF8 (Project_Name.all).Is_Regular_File then
               Project_File := Create (+Project_Name.all);

               Item := new Recent_Project_Item_Box_Record;
               Initialize_Vbox (Item);
               Item.Project_File := Project_File;

               Gtk_New (Label, Project_File.Display_Base_Name);
               Label.Set_Alignment (0.0, 0.5);
               Item.Pack_Start (Label, Expand => False, Padding => 5);

               Gtk_New (Label, Project_File.Display_Full_Name);
               Label.Set_Alignment (0.0, 0.5);
               Apply_Doc_Style (Label);
               Label.Set_Justify (Justify_Left);
               Item.Pack_Start (Label, Expand => False);

               Recent_Projects_View.Add (Item);
            end if;
         end loop;
      end Fill_Recent_Projects_View;

      -----------------------------------
      -- Create_Welcome_Dialog_Options --
      -----------------------------------

      procedure Create_Welcome_Dialog_Options is
         Action_Box    : Gtk_Box;
         Action_Button : Welcome_Dialog_Action_Button;
         Action_Image  : Gtk_Image;
         Size_Group    : Gtk_Size_Group;
      begin
         Gtk_New (Size_Group);

         for Action of Actions loop
            Gtk_New_Hbox (Action_Box);
            Main_View.Append
              (Action_Box,
               Expand        => False,
               Add_Separator => False);

            Action_Button := new Welcome_Dialog_Action_Button_Record;
            Initialize (Action_Button, To_String (Action.Label));
            Size_Group.Add_Widget (Action_Button);
            Action_Button.Set_Relief (Relief_None);
            Action_Button.Set_Alignment (0.0, 0.5);
            Action_Button.Set_Always_Show_Image (True);
            Action_Button.Set_Image_Position (Pos_Left);

            Gtk_New_From_Icon_Name
              (Action_Image,
               Icon_Name => To_String (Action.Icon_Name),
               Size      => Icon_Size_Button);
            Action_Button.Set_Image (Action_Image);

            Action_Button.Kernel := Kernel_Handle (Kernel);
            Action_Button.Dialog := Dialog;
            Action_Button.Callback := Action.Callback;

            Action_Button.On_Clicked (On_Clicked'Access);

            Action_Box.Pack_Start
              (Action_Button,
               Expand => True,
               Fill   => False);
         end loop;
      end Create_Welcome_Dialog_Options;

   begin
      Dialog := new Welcome_Dialog_Record;
      GPS.Dialogs.Initialize
        (Dialog,
         Title  => "Welcome to GNAT Studio",
         Kernel => Kernel);
      Dialog.Set_Position (Win_Pos_Center);
      Set_Default_Size_From_History
        (Win    => Dialog,
         Name   => "welcome-dialog",
         Kernel => Kernel,
         Width  => 760,
         Height => 350);
      Dialog.On_Delete_Event (On_Delete'Access);
      Get_Style_Context (Dialog).Add_Class ("gps-welcome-dialog");

      --  Create and fill the view containing all the welcome dialog options

      Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Main_View);
      Get_Style_Context (Main_View).Add_Class ("gps-welcome-dialog-main-view");

      Main_View.Append
        (Create_Logo_And_Title_Area (Gtk_Theme.Get_Pref.Dark),
         Expand => False);
      Create_Welcome_Dialog_Options;

      --  If there is no recent project in the history, don't create the
      --  recent projects view on the left side of the dialog.

      if Recent_Projects /= null then
         Gtk_New (Scrolled);
         Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

         Recent_Projects_View := new Recent_Projects_List_Box_Record;
         Recent_Projects_View.Kernel := Kernel_Handle (Kernel);
         Initialize (Recent_Projects_View);
         Scrolled.Add (Recent_Projects_View);
         Recent_Projects_View.Set_Selection_Mode (Selection_Single);
         Recent_Projects_View.Set_Activate_On_Single_Click (False);
         Recent_Projects_View.On_Row_Activated
           (On_Row_Activated'Access,
            Slot => Dialog);

         Fill_Recent_Projects_View;

         --  Create a paned view for both the recent projects view and the
         --  welcome dialog options.

         Gtk_New_Hpaned (Pane);
         Dialog.Get_Content_Area.Pack_Start (Pane);

         Pane.Pack2
           (Main_View,
            Resize => True,
            Shrink => True);
         Pane.Pack1
           (Scrolled,
            Resize => False,
            Shrink => False);

         Pane.Set_Position (300);
      else
         Dialog.Get_Content_Area.Pack_Start (Main_View);
      end if;

      --  Show the dialog and block the execution of the procedure while it's
      --  running.

      Dialog.Show_All;
      Gtk.Main.Main;

      --  Return the dialog's response

      Response := Dialog.Response;

      if Response = Project_Loaded then
         Trace (Me, "A project has been loaded.");
      else
         Trace (Me, "No project has been loaded. Quitting GPS");
      end if;

      Dialog.Destroy;

      return Response;
   end Display_Welcome_Dialog;

end Welcome_Dialogs;
