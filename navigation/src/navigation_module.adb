-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
--                            ACT-Europe                             --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Button;                use Gtk.Button;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;

with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Result_View;         use Glide_Result_View;
with Glide_Intl;                use Glide_Intl;

with Commands;                  use Commands;
with Commands.Locations;        use Commands.Locations;
with Traces;                    use Traces;
with Basic_Types;               use Basic_Types;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Exceptions;            use Ada.Exceptions;

package body Navigation_Module is

   Navigation_Module_Name : constant String := "Navigation";

   Me : constant Debug_Handle := Create ("Navigation");

   use Command_Queues;

   type Navigation_Module_Record is new Module_ID_Record with record
      --  Fields related to back/forward navigation.

      Moving_Back : Boolean := False;
      --  This boolean indicates whether we are going backwards in the
      --  location history.

      Back    : List;
      Forward : List;
      --  The past and "future" locations.

      Current_Location : Command_Access;
      --  The currently visited location.

      Back_Button : Gtk.Widget.Gtk_Widget;
      Forward_Button : Gtk.Widget.Gtk_Widget;
      --  Back and forward buttons on the toolbar.
      --  ??? This might be put elsewhere.
   end record;
   type Navigation_Module is access all Navigation_Module_Record'Class;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Destroy (Id : in out Navigation_Module_Record);
   --  Free memory associated to Id.

   procedure Refresh_Location_Buttons
     (Handle : access Kernel_Handle_Record'Class);
   --  Refresh the active/inactive state of the location buttons.

   procedure On_Back
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   procedure On_Forward
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callbacks for the back/forward buttons.

   procedure On_Other_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open the spec if a body or separate is currently selected, and the spec
   --  otherwise.

   procedure On_Next_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Next Result menu.

   procedure On_Previous_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Navigate->Previous Result menu.

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the navigation module.

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      The_Command : Generic_Location_Command;
      N_Data : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Args : Argument_List (1 .. Number_Of_Arguments (Data));

   begin
      for Index in Args'Range loop
         Args (Index) := new String'(Nth_Arg (Data, Index));
      end loop;

      Create (The_Command, Get_Kernel (Data), Args);

      Free (Args);

      if N_Data.Current_Location /= null then
         Prepend (N_Data.Back, N_Data.Current_Location);
      end if;

      N_Data.Current_Location := Command_Access (The_Command);
      Free (N_Data.Forward);

      Refresh_Location_Buttons (Get_Kernel (Data));
   end Command_Handler;

   -------------
   -- On_Back --
   -------------

   procedure On_Back
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Data    : Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Success : Boolean;
      pragma Unreferenced (Widget, Success);

   begin
      if not Is_Empty (Data.Back) then

         if Data.Current_Location /= null then
            Prepend (Data.Forward, Data.Current_Location);
         end if;

         Data.Current_Location := Head (Data.Back);
         Success := Execute (Data.Current_Location);
         Next (Data.Back, Free_Data => False);
      end if;

      Refresh_Location_Buttons (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Back;

   ----------------
   -- On_Forward --
   ----------------

   procedure On_Forward
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Data    : Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Success : Boolean;
      pragma Unreferenced (Widget, Success);

   begin
      if not Is_Empty (Data.Forward) then
         if Data.Current_Location /= null then
            Prepend (Data.Back, Data.Current_Location);
         end if;

         Data.Current_Location := Head (Data.Forward);
         Success := Execute (Data.Current_Location);
         Next (Data.Forward, Free_Data => False);
      end if;

      Refresh_Location_Buttons (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Forward;

   --------------------
   -- On_Next_Result --
   --------------------

   procedure On_Next_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Results : constant Result_View :=
        Get_Or_Create_Result_View (Kernel, False);
   begin
      if Results /= null then
         Next_Item (Results);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Next_Result;

   ------------------------
   -- On_Previous_Result --
   ------------------------

   procedure On_Previous_Result
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Results : constant Result_View :=
        Get_Or_Create_Result_View (Kernel, False);
   begin
      if Results /= null then
         Next_Item (Results, Backwards => True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Previous_Result;

   -------------------
   -- On_Other_File --
   -------------------

   procedure On_Other_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      File : File_Selection_Context_Access;
   begin
      Push_State (Kernel, Busy);

      if Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_File_Information
          (File_Selection_Context_Access (Context))
      then
         File := File_Selection_Context_Access (Context);
         declare
            Other_File : constant String := Other_File_Name
              (Kernel, File_Information (File));
         begin
            if Other_File /= "" then
               Open_File_Editor (Kernel, Other_File, Line => 0,
                                 From_Path => True);
            else
               Trace (Me, "Other file not found for "
                      & File_Information (File));
            end if;
         end;
      else
         Insert (Kernel, -"There is no selected file", Mode => Error);
      end if;

      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Other_File;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar       : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Button        : Gtk_Button;
      Navigate      : constant String := "/_" & (-"Navigate");
      Menu_Item     : Gtk_Menu_Item;
   begin
      Navigation_Module_ID := new Navigation_Module_Record;

      Register_Module
        (Module                  => Navigation_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Navigation_Module_Name,
         Priority                => High_Priority);

      Register_Menu
        (Kernel,
         Navigate,
         Ref_Item => -"Edit",
         Add_Before => False);

      Register_Command
        (Kernel,
         Command      => "add_location_command",
         Params       => "(command, arg1, [arg2...])",
         Description  => -("Register a command to be associated with"
                           &" navigation buttons."),
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Command_Handler'Access);

      Register_Menu (Kernel, Navigate, -"Goto _File Spec<->Body",
                     Stock_Convert, On_Other_File'Access);
      Register_Menu (Kernel, Navigate, -"Goto _Parent Unit", "", null);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Navigate, Menu_Item);
      Register_Menu (Kernel, Navigate, -"_Start Of Statement",
                     Stock_Go_Up, null);
      Register_Menu (Kernel, Navigate, -"_End Of Statement",
                     Stock_Go_Down, null);
      Register_Menu (Kernel, Navigate, -"Next Procedure", "", null);
      Register_Menu (Kernel, Navigate, -"Previous Procedure", "", null);

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Navigate, Menu_Item);
      Register_Menu
        (Kernel, Navigate, -"Previous Tag", "", On_Previous_Result'Access,
         Accel_Key  => GDK_less,
         Accel_Mods => Control_Mask);
      Register_Menu
        (Kernel, Navigate, -"Next Tag", "", On_Next_Result'Access,
         Accel_Key  => GDK_greater,
         Accel_Mods => Control_Mask);

      Append_Space (Toolbar);

      Button := Insert_Stock
        (Toolbar, Stock_Go_Back, -"Goto Previous Location");
      Navigation_Module (Navigation_Module_ID).Back_Button :=
        Gtk_Widget (Button);

      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Back'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Go_Forward, -"Goto Next Location");
      Navigation_Module (Navigation_Module_ID).Forward_Button :=
        Gtk_Widget (Button);

      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Forward'Access),
         Kernel_Handle (Kernel));

      Refresh_Location_Buttons (Kernel);
   end Register_Module;

   ------------------------------
   -- Refresh_Location_Buttons --
   ------------------------------

   procedure Refresh_Location_Buttons
     (Handle : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Handle);
      Data : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
   begin
      Set_Sensitive
        (Data.Back_Button,
         not Is_Empty (Data.Back));
      Set_Sensitive
        (Data.Forward_Button,
         not Is_Empty (Data.Forward));
   end Refresh_Location_Buttons;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Navigation_Module_Record) is
   begin
      Free (Id.Back);
      Destroy (Id.Current_Location);
      Free (Id.Forward);
   end Destroy;

end Navigation_Module;
