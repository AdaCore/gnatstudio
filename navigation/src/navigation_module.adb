-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;

with Gtk.Button;                use Gtk.Button;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;

with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Intl;                use Glide_Intl;

with Commands;                  use Commands;
with Commands.Locations;        use Commands.Locations;
with Traces;                    use Traces;

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

   procedure Navigation_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure On_Other_File
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open the spec if a body or separate is currently selected, and the spec
   --  otherwise.

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      pragma Unreferenced (Mode);
      Location_Command : Source_Location_Command;

      N_Data : constant Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);

   begin
      if Mime_Type = Mime_Source_File then
         --  ??? The decapsulating and re-encapsulating of Values is
         --  an unnecessary step and could be avoided.

         declare
            File      : constant String  := Get_String (Data (Data'First));
            Line      : constant Gint    := Get_Int (Data (Data'First + 1));
            Column    : constant Gint    := Get_Int (Data (Data'First + 2));
            Highlight : constant Boolean :=
              Get_Boolean (Data (Data'First + 3));
            Navigate  : constant Boolean :=
              Get_Boolean (Data (Data'First + 4));
            Success   : Boolean;
         begin
            if not Navigate then
               return False;
            end if;

            Create (Location_Command,
                    Kernel_Handle (Kernel),
                    File,
                    Integer (Line),
                    Integer (Column),
                    Highlight);

            if N_Data.Current_Location /= null then
               Prepend (N_Data.Back, N_Data.Current_Location);
            end if;

            N_Data.Current_Location := Command_Access (Location_Command);
            Success := Execute (N_Data.Current_Location);
            Free (N_Data.Forward);

            Refresh_Location_Buttons (Kernel);

            return True;
         end;
      end if;

      return False;
   end Mime_Action;

   -------------
   -- On_Back --
   -------------

   procedure On_Back
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Data    : Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Success : Boolean;
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
      pragma Unreferenced (Widget);
      Data    : Navigation_Module :=
        Navigation_Module (Navigation_Module_ID);
      Success : Boolean;
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
            Other_File : constant String := Get_Other_File_Of
              (Kernel, File_Information (File));
         begin
            if Other_File /= "" then
               Open_File_Editor (Kernel, Other_File);
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

   --------------------------------
   -- Navigation_Contextual_Menu --
   --------------------------------

   procedure Navigation_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      pragma Unreferenced (Context);
      pragma Unreferenced (Menu);
   begin
      null;
   end Navigation_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar       : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Button        : Gtk_Button;
      Navigate_Root : constant String := -"Navigate";
      Navigate      : constant String := "/" & Navigate_Root;
      Menu_Item     : Gtk_Menu_Item;
   begin
      Navigation_Module_ID := new Navigation_Module_Record;

      Register_Module
        (Module                  => Navigation_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Navigation_Module_Name,
         Priority                => High_Priority,
         Contextual_Menu_Handler => Navigation_Contextual_Menu'Access,
         Mime_Handler            => Mime_Action'Access);

      Register_Menu
        (Kernel,
         "/_" & Navigate_Root,
         Ref_Item => -"Edit",
         Add_Before => False);

      Register_Menu (Kernel, Navigate, -"Goto File Spec<->Body",
                     Stock_Convert, On_Other_File'Access);
      Register_Menu (Kernel, Navigate, -"Goto Parent Unit", "", null);
      Register_Menu (Kernel, Navigate, -"Find All References", "", null);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Navigate, Menu_Item);
      Register_Menu (Kernel, Navigate, -"Start Of Statement",
                     Stock_Go_Up, null);
      Register_Menu (Kernel, Navigate, -"End Of Statement",
                     Stock_Go_Down, null);
      Register_Menu (Kernel, Navigate, -"Next Procedure", "", null);
      Register_Menu (Kernel, Navigate, -"Previous Procedure", "", null);

      Append_Space (Toolbar);

      Button := Insert_Stock
        (Toolbar, Stock_Go_Back, -"Goto Previous Location");
      Navigation_Module (Navigation_Module_ID).Back_Button
        := Gtk_Widget (Button);

      Kernel_Callback.Connect
        (Button, "clicked",
         Kernel_Callback.To_Marshaller (On_Back'Access),
         Kernel_Handle (Kernel));

      Button := Insert_Stock
        (Toolbar, Stock_Go_Forward, -"Goto Next Location");
      Navigation_Module (Navigation_Module_ID).Forward_Button
        := Gtk_Widget (Button);

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
      Set_Sensitive (Data.Back_Button,
                     not Is_Empty (Data.Back));
      Set_Sensitive (Data.Forward_Button,
                     not Is_Empty (Data.Forward));
   end Refresh_Location_Buttons;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Navigation_Module_Record) is
   begin
      Free (Id.Back);
      Free (Id.Current_Location);
      Free (Id.Forward);
   end Destroy;

end Navigation_Module;
