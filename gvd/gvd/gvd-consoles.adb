-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2005                        --
--                             AdaCore                               --
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

with Ada.Unchecked_Conversion;
with Ada.Exceptions;         use Ada.Exceptions;
with Basic_Types;            use Basic_Types;
with Debugger;               use Debugger;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Glib.Xml_Int;           use Glib.Xml_Int;
with Gdk.Event;              use Gdk.Event;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Dialogs;         use Gtkada.Dialogs;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Intl;               use GPS.Intl;
with GPS.Main_Window.Debug;  use GPS.Main_Window.Debug;
with GVD.Preferences;        use GVD.Preferences;
with GVD.Process;            use GVD.Process;
with GVD.Types;              use GVD.Types;
with GVD_Module;             use GVD_Module;
with Histories;              use Histories;
with Interactive_Consoles;   use Interactive_Consoles;
with Pango.Font;             use Pango.Font;
with Process_Proxies;        use Process_Proxies;
with String_List_Utils;      use String_List_Utils;
with String_Utils;           use String_Utils;
with System;                 use System;
with Traces;                 use Traces;

package body GVD.Consoles is

   type Debugger_Console_Record is new Interactive_Console_Record with
      record
         Process         : Visual_Debugger;
         Contextual_Menu : Gtk_Menu;
      end record;
   type Debugger_Console is access all Debugger_Console_Record'Class;

   function Create_Console
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Create a new debugger console

   function Complete_Command
     (Input  : String; Console : System.Address)
      return String_List_Utils.String_List.List;
   --  Return the list of completions for Input.

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal on the console.

   function Interpret_Command_Handler
     (Console            : access Interactive_Console_Record'Class;
      Input              : String;
      Console_User_Data  : System.Address) return String;
   --  Launch the command interpreter for Input and return the output.

   procedure On_Grab_Focus (Console : access Gtk_Widget_Record'Class);
   --  Callback for the "grab_focus" signal on the console.

   function On_Button_Press
     (Console : access Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback for all the button press events in the console.
   --  This is used to display the contexual menu.

   function Load_Desktop
     (MDI    : MDI_Window;
      Node   : Glib.Xml_Int.Node_Ptr;
      Kernel : GPS.Kernel.Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle) return Glib.Xml_Int.Node_Ptr;
   --  Desktop-related functions (see Gtkada.MDI)

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Console : access Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Boolean
   is
      C : constant Debugger_Console := Debugger_Console (Console);
      Mitem : Gtk_Menu_Item;
   begin
      if C.Process /= null then
         if C.Contextual_Menu = null then
            Gtk_New (C.Contextual_Menu);
            Gtk_New (Mitem, Label => -"Info");
            Set_State (Mitem, State_Insensitive);
            Append (C.Contextual_Menu, Mitem);
            Show_All (C.Contextual_Menu);
         end if;

         if Get_Button (Event) = 3 then
            Popup (C.Contextual_Menu,
                   Button        => Get_Button (Event),
                   Activate_Time => Get_Time (Event));
            Emit_Stop_By_Name (C, "button_press_event");

            return True;
         end if;
      end if;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception: "
                & Exception_Information (E));
         return False;
   end On_Button_Press;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class) is
      C : constant Debugger_Console := Debugger_Console (Console);
   begin
      if C.Process /= null then
         C.Process.Debugger_Text := null;
      end if;
   end On_Destroy;

   -------------------
   -- On_Grab_Focus --
   -------------------

   procedure On_Grab_Focus (Console : access Gtk_Widget_Record'Class)
   is
      C : constant Debugger_Console := Debugger_Console (Console);
   begin
      if C.Process /= null then
         Switch_Debugger (C.Process.Window, GObject (C.Process));
         String_History.Wind
           (C.Process.Command_History, String_History.Forward);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception: "
                & Exception_Information (E));
   end On_Grab_Focus;

   ----------------------
   -- Complete_Command --
   ----------------------

   function Complete_Command
     (Input  : String; Console : System.Address)
     return String_List_Utils.String_List.List
   is
      use String_List_Utils.String_List;
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Debugger_Console);
      C      : constant Debugger_Console := Convert (Console);
      Result : List;
   begin
      if C.Process = null then
         return String_List_Utils.String_List.Null_List;

      elsif not Command_In_Process (Get_Process (C.Process.Debugger)) then
         --  Do not launch completion if the last character is ' ', as that
         --  might result in an output of several thousand entries, which
         --  will take a long time to parse.

         if Input /= ""
           and then Input (Input'Last) /= ' '
         then
            declare
               S : String_Array := Complete (C.Process.Debugger, Input);
            begin
               for J in S'Range loop
                  Append (Result, S (J).all);
               end loop;

               Free (S);
            end;
         end if;
      end if;

      if Is_Empty (Result) then
         Append (Result, Input);
      end if;

      return Result;
   end Complete_Command;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Console           : access Interactive_Console_Record'Class;
      Input             : String;
      Console_User_Data : System.Address) return String
   is
      pragma Unreferenced (Console_User_Data);
      C   : constant Debugger_Console := Debugger_Console (Console);
   begin
      if C.Process /= null then
         C.Process.Interactive_Command := True;
         Process_User_Command (C.Process, Input, Mode => User);
      end if;
      return "";
   end Interpret_Command_Handler;

   --------------------
   -- Create_Console --
   --------------------

   function Create_Console
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child
   is
      Console : Debugger_Console;
      Child   : MDI_Child;
   begin
      Console := new Debugger_Console_Record;
      Initialize
        (Console,
         Handler             => Interpret_Command_Handler'Access,
         User_Data           => Console.all'Address,
         Prompt              => "",
         Font                => Get_Pref_Font (Default_Style),
         History_List        => Get_History (Kernel),
         Key                 => "gvd_console",
         Wrap_Mode           => Wrap_Char,
         Empty_Equals_Repeat => True);

      Set_Max_Length (Get_History (Kernel).all, 100, "gvd_console");
      Allow_Duplicates
        (Get_History (Kernel).all, "gvd_console", True, True);

      Set_Highlight_Color    (Console, Get_Pref (Debugger_Highlight_Color));
      Set_Completion_Handler (Console, Complete_Command'Access);
      Widget_Callback.Connect (Console, "destroy", On_Destroy'Access);
      Widget_Callback.Object_Connect
        (Get_View (Console), "grab_focus", On_Grab_Focus'Access, Console);

      --  Set up the command window for the contextual menus

      Add_Events (Console, Button_Press_Mask);
      Gtkada.Handlers.Return_Callback.Connect
        (Console, "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (On_Button_Press'Access));

      --  Add debugger console in the MDI

      Gtk_New (Child, Console,
               Flags        => 0,
               Group        => Group_Consoles,
               Focus_Widget => Gtk_Widget (Get_View (Console)));
      Set_Title (Child, -"Debugger Console");
      Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);
      Set_Focus_Child (Child);

      return Child;
   end Create_Console;

   --------------------------------
   -- Attach_To_Debugger_Console --
   --------------------------------

   procedure Attach_To_Debugger_Console
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
   is
      Kernel  : constant Kernel_Handle :=
        Get_Kernel (Debugger_Module_ID.all);
      Button  : Message_Dialog_Buttons;
      Child   : MDI_Child;
      Iter    : Child_Iterator;
      Console : Debugger_Console;
      pragma Unreferenced (Button);
   begin
      if Debugger.Debugger_Text = null then
         --  Do we have an existing unattached console ?
         Iter := First_Child (Get_MDI (Kernel));
         loop
            Child := Get (Iter);
            exit when Child = null;

            if Get_Widget (Child).all in Debugger_Console_Record'Class then
               Console := Debugger_Console (Get_Widget (Child));
               if Console.Process = null then
                  exit;
               end if;
               Console := null;
            end if;

            Next (Iter);
         end loop;

         --  If no exsting call stack was found, create one

         if Child = null and then Create_If_Necessary then
            Child := Create_Console (Kernel);
            Console := Debugger_Console (Get_Widget (Child));
         end if;

         Console.Process        := Visual_Debugger (Debugger);
         Debugger.Debugger_Text := Interactive_Console (Console);

         if Child /= null then
            if Debugger.Debugger_Num = 1 then
               Set_Title (Child, -"Debugger Console");
            else
               Set_Title
                 (Child, (-"Debugger Console") & " <"
                  & Image (Debugger.Debugger_Num) & ">");
            end if;
         end if;

      else
         Child := Find_MDI_Child (Get_MDI (Kernel), Debugger.Debugger_Text);
         if Child /= null then
            Raise_Child (Child);
         else
            --  Something really bad happened: the stack window is not
            --  part of the MDI, reset it.
            Destroy (Debugger.Debugger_Text);
            Debugger.Debugger_Text := null;
         end if;
      end if;
   end Attach_To_Debugger_Console;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI    : MDI_Window;
      Node   : Glib.Xml_Int.Node_Ptr;
      Kernel : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Debugger_Console" then
         return Create_Console (Kernel);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel : Kernel_Handle) return Glib.Xml_Int.Node_Ptr
   is
      pragma Unreferenced (Kernel);
      N : Glib.Xml_Int.Node_Ptr;
   begin
      if Widget.all in Debugger_Console_Record'Class then
         N     := new Glib.Xml_Int.Node;
         N.Tag := new String'("Debugger_Console");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

end GVD.Consoles;
