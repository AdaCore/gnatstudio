------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                         Copyright (C) 2022, AdaCore                      --
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

with VSS.Strings;             use VSS.Strings;
with VSS.Strings.Conversions; use VSS.Strings.Conversions;
with VSS.String_Vectors;      use VSS.String_Vectors;

with Glib.Object; use Glib.Object;
with Glib.Main;   use Glib.Main;

with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

with Gtk.Window;  use Gtk.Window;
with Gtk.Box;     use Gtk.Box;
with Gtk.Enums;
with Gtk.Label;  use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Style_Context;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with LSP.Types;     use LSP.Types;
with LSP.Messages;  use LSP.Messages;

with GPS.Kernel; use GPS.Kernel;
with GUI_Utils; use GUI_Utils;

with GPS.LSP_Client.Requests.Check_Syntax;
use  GPS.LSP_Client.Requests.Check_Syntax;
with Gdk.Window; use Gdk.Window;
with Glib; use Glib;
with GPS.Editors; use GPS.Editors;
with Src_Editor_Box; use Src_Editor_Box;
with Src_Editor_Module; use Src_Editor_Module;

package body GPS.LSP_Client.Editors.Code_Actions.Dialog is

   type Code_Action_Window_Record is new Gtk_Window_Record with record
      Kernel  : Kernel_Handle;
      Lang    : Language_Access;

      Request : Execute_Command_Request_Access;
      --  The request to execute, if the user validates the input dialog

      Input          : Gtk_Entry;
      --  The user-editable text

      Response_Label : Gtk_Label;
      --  Label containing the validation response sent by the language server

      Closing : Boolean := False;
      --  Whether we're closing the window

      Handler : G_Source_Id := No_Source_Id;
      --  Handler of a timeout source to ask the language server to validate
      --  the input.

      Rules   : VSS.String_Vectors.Virtual_String_Vector;
      --  The rules used to validate the input.
   end record;
   type Code_Action_Window is access all Code_Action_Window_Record'Class;

   The_Win : Code_Action_Window := null;
   --  Maintain a global variable: there can be only one such window open
   --  at a time.

   function On_Entry_Focus_Out
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean;
   --  React to a focus out event on Entry

   function On_Entry_Focus_In
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean;
   --  React to a focus in event on Entry

   function On_Entry_Key_Press
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  React to a key press event on Entry

   procedure On_Win_Destroy
     (Self  : access Glib.Object.GObject_Record'Class);
   --  Called when the window is about to be destroyed

   procedure Dialog
      (Kernel    : Kernel_Handle;
       Lang      : Language_Access;
       Request   : Execute_Command_Request_Access;
       Rules     : VSS.String_Vectors.Virtual_String_Vector;
       Help_Text : String);
   --  Create a dialog for the given request.
   --  Lang is the language/language server to use.
   --  Request is the request to execute if the user validates the dialog.
   --  Command is the ALS refactoring command to execute.
   --  Rules is the list of rules to pass to the language server in the
   --  $/alsCheckSyntax request used to validate the input.
   --  Help_Text indicates what to enter in the dialog.

   function On_Win_Timeout return Boolean;
   --  Called on a timeout after a key press on the entry

   --------------------
   -- On_Win_Timeout --
   --------------------

   function On_Win_Timeout return Boolean is
   begin
      if The_Win = null then
         return False;
      end if;

      The_Win.Handler := No_Source_Id;

      declare
         Request : Check_Syntax_Request_Access;
      begin
         Request := new Check_Syntax_Request'
           (LSP_Request with
            Kernel => The_Win.Kernel,
            Input  => Conversions.To_Virtual_String (The_Win.Input.Get_Text),
            Rules  => The_Win.Rules);

         GPS.LSP_Client.Requests.Execute
            (The_Win.Lang, Request_Access (Request));
      end;
      return False;
   end On_Win_Timeout;

   --------------------
   -- On_Win_Destroy --
   --------------------

   procedure On_Win_Destroy
     (Self  : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      The_Win := null;
   end On_Win_Destroy;

   ------------------------
   -- On_Entry_Focus_Out --
   ------------------------

   function On_Entry_Focus_Out
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      Win : constant Code_Action_Window := Code_Action_Window (Self);
   begin
      if Win.Closing then
         return True;
      end if;

      Win.Closing := True;
      Win.Close;
      return True;
   end On_Entry_Focus_Out;

   -----------------------
   -- On_Entry_Focus_In --
   -----------------------

   function On_Entry_Focus_In
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      Win : constant Code_Action_Window := Code_Action_Window (Self);
   begin
      --  When the window gets the focus, refresh the context -
      --  this will be a null context. This prevents some keys
      --  (such as backspace) to have effect on editors.

      Win.Kernel.Refresh_Context;
      return False;
   end On_Entry_Focus_In;

   ------------------------
   -- On_Entry_Key_Press --
   ------------------------

   function On_Entry_Key_Press
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      Win : constant Code_Action_Window := Code_Action_Window (Self);
      Key : constant Gdk_Key_Type := Event.Keyval;
   begin
      if Win.Closing then
         return True;
      end if;

      case Key is
         when GDK_Escape =>
            --  Escape: close the window
            Win.Closing := True;
            Win.Close;
            return True;

         when GDK_Return =>
            --  Enter: validate and send the request...
            if Win.Request.Params.command = "als-refactor-add-parameters" then
               declare
                  Obj : LSP_Any;
               begin
                  Obj := Win.Request.Params.arguments.Value.First_Element;
                  Obj.Set_Field ("newParameter", Create (Win.Input.Get_Text));
                  Win.Request.Params.arguments.Value.Replace_Element (1, Obj);
                  GPS.LSP_Client.Requests.Execute
                    (Win.Lang, Request_Access (Win.Request));
               end;
            end if;

            --  ... and close the window
            Win.Closing := True;
            Win.Close;
            return True;
         when others =>
            --  Another key has been pressed: schedule a validation of the
            --  input. Don't spam the language server: do this in a timeout;
            --  if the timeout is already scheduled, don't schedule another
            --  one.
            if Win.Handler = No_Source_Id then
               Win.Handler := Timeout_Add (150, On_Win_Timeout'Access);
            end if;
      end case;

      return False;
   end On_Entry_Key_Press;

   ------------
   -- Dialog --
   ------------

   procedure Dialog
      (Kernel    : Kernel_Handle;
       Lang      : Language_Access;
       Request   : Execute_Command_Request_Access;
       Rules     : VSS.String_Vectors.Virtual_String_Vector;
       Help_Text : String)
   is
      Win  : Code_Action_Window;
      Vbox : Gtk_Vbox;
      Hbox : Gtk_Hbox;
      Help : Gtk_Label;
      Total_Height, Total_Width, Dummy : Gint;
   begin
      Win := new Code_Action_Window_Record;
      Win.Kernel  := Kernel;
      Win.Lang    := Lang;
      Win.Request := Request;
      Win.Rules   := Rules;

      Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);

      Gtk.Style_Context.Get_Style_Context
        (Win).Add_Class ("gnatstudio-refactor-input");

      Win.Set_Title ("GNAT Studio refactoring");

      --  We want the window to be transient, so it appears in front
      --  of the main window. We don't want it to be an actual dialog,
      --  because we don't want a nested main loop.
      Win.Set_Transient_For (Kernel.Get_Main_Window);
      Win.Set_Type_Hint (Window_Type_Hint_Menu);
      Win.Set_Decorated (False);
      Win.Set_Skip_Taskbar_Hint (True);
      Win.Set_Skip_Pager_Hint (True);
      Win.Set_Resizable (False);
      Gtk_New_Vbox (Vbox);

      Gtk_New (Win.Input);
      Win.Input.Set_Name ("refactoring_input");
      Gtk_New_Hbox (Hbox);
      Hbox.Pack_Start (Child => Win.Input);
      Vbox.Pack_Start (Child => Hbox, Expand => False, Fill => False);

      Gtk_New (Win.Response_Label);
      Win.Response_Label.Set_Name ("refactoring_response_label");
      Gtk_New_Hbox (Hbox);
      Hbox.Pack_Start
        (Child => Win.Response_Label, Expand => False, Fill => False);
      Vbox.Pack_Start (Child => Hbox, Expand => False, Fill => False);

      Gtk_New (Help);
      Help.Set_Text (Help_Text);
      Gtk_New_Hbox (Hbox);
      Hbox.Pack_Start (Child => Help, Expand => False, Fill => False);
      Vbox.Pack_Start (Child => Hbox, Expand => False, Fill => False);

      Gtk_New (Help);
      Help.Set_Markup ("<b>Enter</b> to confirm or <b>Escape</b> to cancel.");
      Gtk_New_Hbox (Hbox);
      Hbox.Pack_Start (Child => Help, Expand => False, Fill => False);
      Vbox.Pack_Start (Child => Hbox, Expand => False, Fill => False);

      Win.Add (Vbox);

      --  Connect signals
      Win.On_Destroy
        (On_Win_Destroy'Access, Slot => Win, After => False);

      Win.Input.On_Focus_Out_Event
        (On_Entry_Focus_Out'Access, Slot => Win, After => True);

      --  When the dialog appears, we need to generate an empty
      --  context, so that "Backspace" (or other keys) won't act
      --  on the open editor that is currently stored in the context.
      --  We'll do this by calling Refresh_Context. However, that
      --  procedure looks at who has the focus, so we need to make
      --  sure to call it once this entry has the focus.
      Win.Input.On_Focus_In_Event
        (On_Entry_Focus_In'Access, Slot => Win, After => True);

      Win.Input.On_Key_Press_Event
        (On_Entry_Key_Press'Access, Slot => Win, After => False);

      --  Show the window
      Win.Show_All;

      --  Grab the focus. The window will disappear when the entry
      --  loses the focus.
      Win.Input.Grab_Focus;

      Win.Get_Preferred_Height (Dummy, Total_Height);
      Win.Get_Preferred_Width (Dummy, Total_Width);

      Place_Window_On_Cursor
        (Editor       => Get_Source_Box_From_MDI
           (Find_Current_Editor
                (Kernel,
                 Only_If_Focused => True)),
         Win          => Gtk_Window (Win),
         Total_Height => Total_Height,
         Total_Width  => Total_Width);

      --  Set the global variable
      The_Win := Win;
   end Dialog;

   --------------------------------
   -- Execute_Request_Via_Dialog --
   --------------------------------

   procedure Execute_Request_Via_Dialog
      (Kernel  : Kernel_Handle;
       Lang    : Language_Access;
       Request : in out Execute_Command_Request_Access)
   is
      Rules : VSS.String_Vectors.Virtual_String_Vector;
   begin
      if Request.Params.command = "als-refactor-add-parameters" then
         --  Support for the "Add Parameter" refactoring

         Rules.Append (Conversions.To_Virtual_String ("Defining_Id_Rule"));
         Rules.Append (Conversions.To_Virtual_String
            ("Defining_Id_List_Rule"));
         Rules.Append (Conversions.To_Virtual_String ("Param_Spec_Rule"));

         Dialog (Kernel, Lang, Request,
                 Rules,
                 "Insert one or more comma-separated parameter names" &
                 ASCII.LF & "or a full parameter specification.");
      else
         GPS.LSP_Client.Requests.Execute (Lang, Request_Access (Request));
      end if;
   end Execute_Request_Via_Dialog;

   ------------------------
   -- Set_Result_Message --
   ------------------------

   procedure Set_Result_Message
     (Response : LSP.Messages.ALS_Check_Syntax_Result) is
   begin
      if The_Win = null then
         return;
      end if;

      if Response.Is_Set then
         The_Win.Response_Label.Set_Text (To_UTF_8_String (Response.Value));
      else
         The_Win.Response_Label.Set_Text ("");
      end if;
   end Set_Result_Message;

end GPS.LSP_Client.Editors.Code_Actions.Dialog;
