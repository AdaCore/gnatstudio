------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2025, AdaCore                  --
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

with Ada.Exceptions;                  use Ada.Exceptions;
with GNAT.Regpat;                     use GNAT.Regpat;

with GNATCOLL.JSON;
with GNATCOLL.Projects;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with VSS.Characters;
with VSS.Strings.Conversions;

with Completion_Module;               use Completion_Module;
with Dialog_Utils;                    use Dialog_Utils;
with Gdk.Event;                       use Gdk.Event;
with Gdk.Types.Keysyms;               use Gdk.Types.Keysyms;
with Gdk.Types;                       use Gdk.Types;
with Gdk.Window;                      use Gdk.Window;
with Glib.Convert;                    use Glib.Convert;
with Glib.Convert.VSS_Utils;          use Glib.Convert.VSS_Utils;
with Glib.Main;
with Glib.Object;

with GPS.Editors;                     use GPS.Editors;
with GPS.Kernel.Contexts;             use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;          use GPS.Kernel.Preferences;
with GPS.LSP_Client.Utilities;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests.Signature_Help;
use GPS.LSP_Client.Requests.Signature_Help;
with GPS.LSP_Client.Requests;         use GPS.LSP_Client.Requests;
with GPS.LSP_Module;                  use GPS.LSP_Module;
with Gtk.Adjustment;                  use Gtk.Adjustment;
with Gtk.Arrow;                       use Gtk.Arrow;
with Gtk.Button;                      use Gtk.Button;
with Gtk.Enums;                       use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;                       use Gtk.Label;
with Gtk.Separator;                   use Gtk.Separator;
with Gtk.Widget;                      use Gtk.Widget;
with Gtk.Window;                      use Gtk.Window;
with Gtkada.Handlers;                 use Gtkada.Handlers;
with Gtkada.MDI;                      use Gtkada.MDI;
with GUI_Utils;                       use GUI_Utils;
with Language;                        use Language;
with LSP.Messages;                    use LSP.Messages;
with LSP.Types;                       use LSP.Types;
with Pango.Enums;                     use Pango.Enums;
with Src_Editor_Box;                  use Src_Editor_Box;
with Src_Editor_Module;               use Src_Editor_Module;
with Src_Editor_View;                 use Src_Editor_View;
with Glib; use Glib;

package body GPS.LSP_Client.Editors.Signature_Help is

   Me          : constant Trace_Handle :=
     Create ("GPS.LSP.SIGNATURE_HELP", On);
   Me_Use_Toplevel : constant Trace_Handle :=
     Create ("GPS.LSP.SIGNATURE_HELP.USE_TOPLEVEL", Off);

   Max_Signature_Help_Window_Height : constant := 200;

   type Signature_Help_Request is new Abstract_Signature_Help_Request with
   null record;
   type Signature_Help_Request_Access is
     access all Signature_Help_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Signature_Help_Request;
      Result :        LSP.Messages.SignatureHelp);

   overriding procedure On_Error_Message
     (Self    : in out Signature_Help_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is null;

   overriding procedure On_Rejected
     (Self : in out Signature_Help_Request; Reason : Reject_Reason) is null;

   type On_Character_Added is new Character_Hooks_Function with null record;
   overriding procedure Execute
     (Self        : On_Character_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Char        : Glib.Gunichar;
      Interactive : Boolean);
   --  Called when a character is added. Used to trigger the signature help
   --  request when needed.

   type On_Location_Changed is new File_Location_Hooks_Function
     with null record;
   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type);
   --  Recompute the signature if the cursor was moved when the window
   --  was visible.

   type On_MDI_Child_Selected is new Mdi_Child_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_MDI_Child_Selected;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : Gtkada.MDI.MDI_Child);
   --  Called when the focused MDI child changes. Used to close the
   --  signature help window.

   type Signature_Help_Window_Record is new Gtk_Window_Record with record
      Kernel                          : Kernel_Handle;
      View                            : Dialog_View_With_Button_Box;
      Signatures                      : SignatureInformation_Vector;
      Up_Arrow                        : Gtk_Button;
      Down_Arrow                      : Gtk_Button;
      Active_Signature_Label          : Gtk_Label;
      Active_Signature_Selector_Label : Gtk_Label;
      Documentation_Label             : Gtk_Label;
      Sep                             : Gtk_Hseparator;
      Active_Signature_Nb             : Natural := 0;
      Active_Parameter_Nb             : Natural := 0;
      In_DnD                          : Boolean := False;
   end record;
   type Signature_Help_Window is access all Signature_Help_Window_Record'Class;

   type Signature_Help_Provider_Record is record
      Global_Window : Signature_Help_Window := null;
      --  The signature help global window.

      Was_Opened    : Boolean := False;
      --  True if Global_Window was opened when Current_Request was created

      Current_Request : GPS.LSP_Client.Requests.Reference;
      --  The current signature help request
   end record;

   Signature_Help_Provider : Signature_Help_Provider_Record;

   procedure Create_Signature_Help_If_Needed
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Response : LSP.Messages.SignatureHelp);
   --  Create the signature help window if it's not the case yet.

   function Should_Send_Signature_Help_Request
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Char    : Glib.Gunichar;
      Context : in out LSP.Messages.Optional_SignatureHelpContext;
      Lang    : out Language.Language_Access)
      return Boolean;
   --  Return True if signatureHelp should be triggered and fill the Context

   function Create_Signature_Help_Request
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Context : LSP.Messages.Optional_SignatureHelpContext)
      return Signature_Help_Request_Access;

   procedure Refresh
     (Self         : not null Signature_Help_Window;
      Content_Only : Boolean := False);
   --  Refresh the signature help window.
   --  This procedure will update its contents, its size and its position
   --  depending on the currently active signature.
   --  If Content_Only then the size and position will not be updated to
   --  simplify the user interaction (clicking on arrow for example).

   procedure On_Scroll (Self : access Glib.Object.GObject_Record'Class);
   --  Called when the editor is scrolled.
   --  Destroy the signature help window, if any.

   procedure On_Arrow_Up_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when the up arrow button is clicked.
   --  Display the previous signature if there is one.

   procedure On_Arrow_Down_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when the bottom arrow button is clicked.
   --  Display the next signature if there is one.

   procedure Change_Signature
     (Self     : not null Signature_Help_Window;
      Backward : Boolean;
      Clicked  : Boolean);
   --  Select the next/previous signature depending of Backward
   --  Clicked should be True if triggered by a mouse click.

   function On_Button_Pressed_Window
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  Called when the user clicks on the window.
   --  Used to support drag'n'drop to move the signature help window around.

   package Signature_Help_Sources is new Glib.Main.Generic_Sources
     (Signature_Help_Window);

   function On_Idle (Self : Signature_Help_Window) return Boolean;
   --  Called to set the signature help window in a 'focused' state, disabling
   --  the 'graying' that might appear on windows that don't accept/have the
   --  focus.

   function On_Button_Pressed
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Called when a button is pressed in the editor => close the windowS

   function On_Key_Pressed
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event)
      return Boolean;
   --  Called when the user presses a key while the signature help window is
   --  actived.
   --  ESC to close the window
   --  Up to previous signature
   --  Down to next signature

   function In_Signature_Help return Boolean;
   --  Return True if the Signature Help window is displayed

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self         : not null Signature_Help_Window;
      Content_Only : Boolean := False)
   is
      Signature : constant LSP.Messages.SignatureInformation :=
        Self.Signatures (Self.Active_Signature_Nb);
      Signature_Label : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Signature.label);
      Total_Height    : Gint;
      Total_Width     : Gint;

      procedure Refresh_Size;
      --  Refresh the signature help window's size depending on its contents.

      ------------------
      -- Refresh_Size --
      ------------------

      procedure Refresh_Size is
         Active_Signature_Label_Height : Gint;
         Sep_Height                    : Gint;
         Doc_Label_Height              : Gint;
         Button_Box_Height             : Gint;
         Dummy                         : Gint;
      begin
         Signature_Help_Provider.Global_Window.Realize;

         Self.Active_Signature_Label.Get_Preferred_Height
           (Dummy, Active_Signature_Label_Height);
         Self.Sep.Get_Preferred_Height
           (Dummy, Sep_Height);
         Self.Documentation_Label.Get_Preferred_Height
           (Dummy, Doc_Label_Height);

         Total_Height :=
           Active_Signature_Label_Height + Sep_Height + Doc_Label_Height;

         Self.View.Get_Button_Box_Preferred_Height (Dummy, Button_Box_Height);

         Total_Height := Gint'Max (Total_Height, Button_Box_Height) + 20;

         if Total_Height > Max_Signature_Help_Window_Height then
            Total_Height := Max_Signature_Help_Window_Height;
            Self.Set_Size_Request
              (Width => -1, Height => Max_Signature_Help_Window_Height);
         else
            Self.Set_Size_Request (Width => -1, Height => Total_Height);
         end if;

         Self.Get_Preferred_Width (Dummy, Total_Width);
      end Refresh_Size;

      Active_Param_Num : Natural;

   begin
      Signature_Help_Provider.Global_Window.View.Show_All;

      --  Display the selected signature number (e.g: "1/2" if the first
      --  signature is selected on a total of two signatures).

      Self.Active_Signature_Selector_Label.Set_Text
        (GNATCOLL.Utils.Image (Self.Active_Signature_Nb, 1)
         & "/"
         & GNATCOLL.Utils.Image (Natural (Self.Signatures.Length), 1));

      --  Show/hide the button box containing the arrow buttons depending
      --  on the total number of signatures: we don't want to display them
      --  if we only have one signature.

      Self.View.Set_Button_Box_Visibility
        (Natural (Self.Signatures.Length) > 1);

      --  Show/hide the documentation label, depending on whether there is doc
      --  associated to the active signature or not.

      if Signature.documentation.Is_Set
        and then Signature.documentation.Value.Is_String
      then
         declare
            Doc : constant VSS.Strings.Virtual_String :=
              Escape_Text (Signature.documentation.Value.String);
         begin
            if not Doc.Is_Empty then
               Self.Documentation_Label.Set_Text
                 (VSS.Strings.Conversions.To_UTF_8_String (Doc));
            else
               Self.Documentation_Label.Hide;
               Self.Sep.Hide;
            end if;
         end;
      else
         Self.Documentation_Label.Hide;
         Self.Sep.Hide;
      end if;

      --  Display the active signature and highlight the active parameter when
      --  specified.

      Active_Param_Num := (if Signature.activeParameter.Is_Set then
                              Natural (Signature.activeParameter.Value + 1)
                           else Self.Active_Parameter_Nb);

      if Active_Param_Num <= Signature.parameters.Last_Index then
         declare
            Param : constant LSP.Messages.ParameterInformation :=
              Signature.parameters (Active_Param_Num);
         begin
            if not Param.label.Is_String then
               declare
                  From : constant Integer := Integer (Param.label.From);
                  Till : constant Integer := Integer (Param.label.Till);
               begin
                  Self.Active_Signature_Label.Set_Markup
                    (Escape_Text
                       (Signature_Label (Signature_Label'First .. From - 1))
                     & "<b>"
                     & Escape_Text (Signature_Label (From .. Till))
                     & "</b>"
                     & Escape_Text
                       (Signature_Label (Till + 1 .. Signature_Label'Last)));
               end;
            else
               declare
                  Escaped_Param     : constant String :=
                    Escape_Text
                      (VSS.Strings.Conversions.To_UTF_8_String
                         (Param.label.String));
                  Escaped_Signature : constant String :=
                    Escape_Text (Signature_Label);
                  Param_Pattern     : constant Pattern_Matcher :=
                    Compile ("[^\w]" & Escaped_Param & "[^\w]");
                  Match_Res         : Match_Array (0 .. 0);
               begin
                  Match
                    (Self    => Param_Pattern,
                     Data    => Escaped_Signature,
                     Matches => Match_Res);
                  if Match_Res (0) /= No_Match then
                     Self.Active_Signature_Label.Set_Markup
                       (Escaped_Signature
                          (Escaped_Signature'First
                           .. Match_Res (0).First)
                        & "<b>" & Escaped_Param & "</b>"
                        & Escaped_Signature
                          (Match_Res (0).Last
                           .. Escaped_Signature'Last)
                       );
                  else
                     Self.Active_Signature_Label.Set_Markup
                       (Escaped_Signature);
                  end if;
               end;
            end if;

         exception
            when E : others =>
               Trace (Me, Exception_Message (E));
               Self.Destroy;
               return;
         end;
      else
         Self.Active_Signature_Label.Set_Markup
           (Escape_Text (Signature_Label));
      end if;

      --  Don't recompute the size and position if we just want to change the
      --  contents or if the window is in a DnD operation.
      if not Content_Only and then
        not Signature_Help_Provider.Global_Window.In_DnD
      then
         Refresh_Size;
         Place_Window_On_Cursor
           (Editor       => Get_Source_Box_From_MDI
              (Find_Current_Editor
                   (Self.Kernel,
                    Only_If_Focused => True)),
            Win          => Gtk_Window (Self),
            Total_Height => Total_Height,
            Total_Width  => Total_Width);
      end if;

      Signature_Help_Provider.Global_Window.Show;
   end Refresh;

   -----------------------
   -- On_Button_Pressed --
   -----------------------

   function On_Button_Pressed
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Self, Event);
   begin
      if Signature_Help_Provider.Global_Window /= null then
         Signature_Help_Provider.Global_Window.Destroy;
         Signature_Help_Provider.Global_Window := null;
      end if;

      return False;
   end On_Button_Pressed;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : Signature_Help_Window) return Boolean is
      pragma Unreferenced (Self);
   begin
      if Signature_Help_Provider.Global_Window /= null then
         Signature_Help_Provider.Global_Window.Set_State_Flags
           (Gtk_State_Flag_Focused, Clear => True);
      end if;

      return False;
   end On_Idle;

   ------------------------------
   -- On_Button_Pressed_Window --
   ------------------------------

   function On_Button_Pressed_Window
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean is
   begin
      if Event.The_Type = Button_Press and then Event.Button = 1 then
         Gtk.Window.Begin_Move_Drag
           (Window    => Gtk_Window (Self),
            Button    => Gint (Event.Button),
            Root_X    => Gint (Event.X_Root),
            Root_Y    => Gint (Event.Y_Root),
            Timestamp => Event.Time);
         Signature_Help_Provider.Global_Window.In_DnD := True;

         return True;
      end if;

      return False;
   end On_Button_Pressed_Window;

   --------------------
   -- On_Key_Pressed --
   --------------------

   function On_Key_Pressed
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event)
      return Boolean
   is
      pragma Unreferenced (Self);
      Key : constant Gdk_Key_Type := Get_Key_Val (Event);
   begin
      if Signature_Help_Provider.Global_Window = null then
         return False;
      end if;

      --  If the completion window is present let the event through
      if Completion_Module.In_Smart_Completion then
         return False;
      end if;

      case Key is
         when GDK_Escape =>
            Signature_Help_Provider.Global_Window.Destroy;
            Signature_Help_Provider.Global_Window := null;

         when GDK_KP_Up | GDK_Up =>
            Change_Signature
              (Self     => Signature_Help_Provider.Global_Window,
               Backward => True,
               Clicked  => False);

         when GDK_KP_Down | GDK_Down =>
            Change_Signature
              (Self     => Signature_Help_Provider.Global_Window,
               Backward => False,
               Clicked  => False);

         when others =>
            --  Let the event through
            return False;
      end case;

      --  The event has been consumed
      return True;
   end On_Key_Pressed;

   -------------------------
   -- On_Arrow_Up_Clicked --
   -------------------------

   procedure On_Arrow_Up_Clicked
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Window : constant Signature_Help_Window := Signature_Help_Window (Self);
   begin
      Change_Signature (Window, Backward => True, Clicked => True);
   end On_Arrow_Up_Clicked;

   ---------------------------
   -- On_Arrow_Down_Clicked --
   ---------------------------

   procedure On_Arrow_Down_Clicked
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Window : constant Signature_Help_Window := Signature_Help_Window (Self);
   begin
      Change_Signature (Window, Backward => False, Clicked => True);
   end On_Arrow_Down_Clicked;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll (Self : access Glib.Object.GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Signature_Help_Provider.Global_Window /= null then
         Signature_Help_Provider.Global_Window.Destroy;
         Signature_Help_Provider.Global_Window := null;
      end if;
   end On_Scroll;

   ----------------------
   -- Change_Signature --
   ----------------------

   procedure Change_Signature
     (Self     : not null Signature_Help_Window;
      Backward : Boolean;
      Clicked  : Boolean) is
   begin
      if Backward
        and then Self.Active_Signature_Nb > 1
      then
         Self.Active_Signature_Nb := Self.Active_Signature_Nb - 1;
         Refresh (Self, Content_Only => Clicked);
      elsif not Backward
        and then Self.Active_Signature_Nb < Natural (Self.Signatures.Length)
      then
         Self.Active_Signature_Nb := Self.Active_Signature_Nb + 1;
         Refresh (Self, Content_Only => Clicked);
      else
         Destroy (Self);
         Signature_Help_Provider.Global_Window := null;
      end if;
   end Change_Signature;

   -------------------------------------
   -- Create_Signature_Help_If_Needed --
   -------------------------------------

   procedure Create_Signature_Help_If_Needed
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Response : LSP.Messages.SignatureHelp)
   is
      Arrow                : Gtk_Arrow;
      Label_Max_With_Chars : constant := 80;
      Editor               : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI
          (Find_Current_Editor (Kernel, Only_If_Focused => True));
      Source : Glib.Main.G_Source_Id;
      Global_Window : Signature_Help_Window renames
        Signature_Help_Provider.Global_Window;
      pragma Unreferenced (Source);
   begin
      if Editor = null then
         --  Signature Help must be attached to an editor
         return;
      end if;

      if Global_Window = null then
         Global_Window := new Signature_Help_Window_Record;

         Global_Window.Kernel := Kernel_Handle (Kernel);

         --  Use a popup window by default, since most of window managers
         --  support DnD on popup windows.
         --  When it's not the case, the user can activate a dedicated trace
         --  to make the window toplevel instead.
         Gtk.Window.Initialize
           (Global_Window,
            (if Me_Use_Toplevel.Is_Active then
                Window_Toplevel
             else
                Window_Popup));

         Global_Window.Set_Type_Hint (Window_Type_Hint_Menu);
         Global_Window.Set_Skip_Taskbar_Hint (True);
         Global_Window.Set_Skip_Pager_Hint (True);
         Global_Window.Set_Decorated (False);
         Global_Window.Set_Resizable (False);
         Global_Window.Set_Name ("signature-help-window");
         Global_Window.Set_Accept_Focus (False);

         Global_Window.Add_Events (Button_Press_Mask);
         Global_Window.On_Button_Press_Event
           (On_Button_Pressed_Window'Access);

         Global_Window.View := new Dialog_View_With_Button_Box_Record;
         Global_Window.View.Initialize (Pos_Left);
         Global_Window.View.Set_Scrolled_Policy
           (Policy_Never, Policy_Automatic);
         Global_Window.View.Set_Propagate_Natural_Height (False);
         Global_Window.Add (Global_Window.View);

         Gtk_New (Global_Window.Up_Arrow);
         Global_Window.Up_Arrow.Set_Relief (Relief_None);
         Global_Window.View.Append_Button (Global_Window.Up_Arrow);
         Global_Window.Up_Arrow.Set_Can_Focus (False);
         Gtk_New (Arrow, Arrow_Up, Shadow_None);
         Global_Window.Up_Arrow.On_Clicked
           (On_Arrow_Up_Clicked'Access, Global_Window);
         Add (Global_Window.Up_Arrow, Arrow);

         Gtk_New (Global_Window.Down_Arrow);
         Global_Window.Down_Arrow.Set_Relief (Relief_None);
         Global_Window.View.Append_Button (Global_Window.Down_Arrow);
         Gtk_New (Arrow, Arrow_Down, Shadow_None);
         Global_Window.Down_Arrow.On_Clicked
           (On_Arrow_Down_Clicked'Access, Global_Window);
         Add (Global_Window.Down_Arrow, Arrow);

         Gtk_New (Global_Window.Active_Signature_Selector_Label);
         Global_Window.View.Append_Button
           (Global_Window.Active_Signature_Selector_Label);

         Gtk_New (Global_Window.Active_Signature_Label);
         Global_Window.Active_Signature_Label.Set_Use_Markup (True);
         Global_Window.Active_Signature_Label.Set_Max_Width_Chars
           (Label_Max_With_Chars);
         Global_Window.Active_Signature_Label.Set_Line_Wrap (True);
         Global_Window.Active_Signature_Label.Set_Line_Wrap_Mode
           (Pango_Wrap_Word);
         Global_Window.View.Append
           (Widget => Global_Window.Active_Signature_Label);
         Set_Font_And_Colors
           (Widget     => Global_Window.Active_Signature_Label,
            Fixed_Font => True);
         Global_Window.Active_Signature_Selector_Label.Set_Alignment
           (0.0, 0.5);

         Gtk_New (Global_Window.Documentation_Label);
         Global_Window.Documentation_Label.Set_Use_Markup (False);
         Global_Window.Documentation_Label.Set_Max_Width_Chars
           (Label_Max_With_Chars);
         Global_Window.Documentation_Label.Set_Line_Wrap (True);
         Global_Window.Documentation_Label.Set_Line_Wrap_Mode
           (Pango_Wrap_Word);
         Global_Window.Documentation_Label.Set_Alignment
           (0.0, 0.5);

         Gtk_New_Hseparator (Global_Window.Sep);
         Global_Window.View.Append
           (Global_Window.Sep,
            Expand        => False,
            Fill          => False,
            Add_Separator => False);
         Global_Window.View.Append
           (Widget       => Global_Window.Documentation_Label,
            Add_Separator => False);

         --  Create temporary callbacks attached to the editor which spawned
         --  the window and its toplevel
         Gtk.Handlers.Add_Watch
           (Return_Callback.Connect
              (Widget => Editor,
               Name   => Signal_Button_Press_Event,
               Marsh  =>
                 Return_Callback.To_Marshaller (On_Button_Pressed'Access)),
            Global_Window);
         Gtk.Handlers.Add_Watch
           (Return_Callback.Connect
              (Widget => Editor.Get_Toplevel,
               Name   => Signal_Key_Press_Event,
               Marsh  =>
                 Return_Callback.To_Marshaller (On_Key_Pressed'Access)),
            Global_Window);
         Gtk.Handlers.Add_Watch
           (Object_Callback.Connect
              (Get_Vadjustment (Editor.Get_View),
               Name  => Signal_Value_Changed,
               Marsh => Object_Callback.To_Marshaller (On_Scroll'Access)),
            Global_Window);

         Mdi_Child_Selected_Hook.Add
           (new On_MDI_Child_Selected, Watch => Global_Window);
      end if;

      Global_Window.Active_Signature_Nb :=
        (if Response.activeSignature.Is_Set then
           Natural (Response.activeSignature.Value) + 1
         else 1);
      Global_Window.Active_Parameter_Nb :=
        (if Response.activeParameter.Is_Set then
           Natural (Response.activeParameter.Value) + 1
         else 1);

      Global_Window.Signatures := Response.signatures;

      Refresh (Global_Window);

      --  Add a timeout that sets the 'focused' state flag to avoid graying out
      --  the signature help window (since it can't accept the focus).
      Source :=
        Signature_Help_Sources.Timeout_Add
          (50, On_Idle'Access, Global_Window);
   end Create_Signature_Help_If_Needed;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Signature_Help_Request;
      Result :        LSP.Messages.SignatureHelp)
   is
   begin
      if Signature_Help_Provider.Was_Opened
        and then Signature_Help_Provider.Global_Window = null
      then
         --  The signature help window has been closed since then => do nothing
         null;
      elsif not Result.signatures.Is_Empty then
         Create_Signature_Help_If_Needed (Self.Kernel, Result);
      elsif Signature_Help_Provider.Global_Window /= null then
         Signature_Help_Provider.Global_Window.Destroy;
         Signature_Help_Provider.Global_Window := null;
      end if;

      Signature_Help_Provider.Was_Opened := False;
   end On_Result_Message;

   ----------------------------------------
   -- Should_Send_Signature_Help_Request --
   ----------------------------------------

   function Should_Send_Signature_Help_Request
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Char    : Glib.Gunichar;
      Context : in out LSP.Messages.Optional_SignatureHelpContext;
      Lang    : out Language.Language_Access)
      return Boolean
   is
      Editor      : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File => File, Open_View => False);
      Editor_Lang : constant Language.Language_Access := Editor.Get_Language;
      Server      : constant Language_Server_Access :=
        (if Editor_Lang /= null
         then Get_Language_Server (Editor_Lang)
         else null);
      Res    : Boolean := False;
   begin
      Lang := Editor_Lang;

      if Lang = null or else Server = null then
         return False;
      end if;

      declare
         Capabilities : constant LSP.Messages.ServerCapabilities :=
           Server.Get_Client.Capabilities;
      begin
         if not Capabilities.signatureHelpProvider.Is_Set then
            return False;
         end if;

         --  Check if the typed character is among the server's trigger
         --  characters for signature help.

         declare
            Signature_Options : LSP.Messages.SignatureHelpOptions renames
              Capabilities.signatureHelpProvider.Value;
            Virtual_Char      : VSS.Strings.Virtual_String;

         begin
            Virtual_Char.Append (VSS.Characters.Virtual_Character'Val (Char));

            if Signature_Options.triggerCharacters.Is_Set
              and then Signature_Options.triggerCharacters.Value.Contains
                (Virtual_Char)
            then
               Context.Value.triggerKind := TriggerCharacter;
               Context.Value.triggerCharacter := (True, Virtual_Char);
               --  Trigger characters will always send a signatureHelp request
               Res := True;
            elsif Signature_Options.retriggerCharacters.Is_Set
              and then Signature_Options.retriggerCharacters.Value.Contains
                (Virtual_Char)
            then
               Context.Value.triggerCharacter := (True, Virtual_Char);
               Context.Value.triggerKind := TriggerCharacter;
            else
               Context.Value.triggerKind := ContentChange;
            end if;

            --  If the signature help window is opened: returns True and reuse
            --  the current context.
            if Signature_Help_Provider.Global_Window /= null then
               Context.Value.isRetrigger := True;
               Context.Value.activeSignatureHelp :=
                 (True,
                  (signatures      =>
                       Signature_Help_Provider.Global_Window.Signatures,
                   activeSignature =>
                     (True,
                      LSP_Number
                        (Signature_Help_Provider.Global_Window.
                             Active_Signature_Nb) - 1),
                   activeParameter =>
                     (True,
                      LSP_Number
                        (Signature_Help_Provider.Global_Window.
                             Active_Parameter_Nb) - 1)));
               Res := True;
            else
               Context.Value.isRetrigger := False;
            end if;
         end;
      end;

      return Res;
   end Should_Send_Signature_Help_Request;

   -----------------------------------
   -- Create_Signature_Help_Request --
   -----------------------------------

   function Create_Signature_Help_Request
     (Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Context : LSP.Messages.Optional_SignatureHelpContext)
      return Signature_Help_Request_Access
   is
      Editor_Context : constant Selection_Context :=
        Kernel.Get_Current_Context;
      Holder   : constant Controlled_Editor_Buffer_Holder :=
        Kernel.Get_Buffer_Factory.Get_Holder (File);
      Location : constant GPS.Editors.Editor_Location'Class :=
        Holder.Editor.New_Location
          (Line_Information (Editor_Context),
           Column_Information (Editor_Context));

      Request : constant Signature_Help_Request_Access :=
        new Signature_Help_Request'
          (LSP_Request with Kernel => Kernel_Handle (Kernel), File => File,
           Position => GPS.LSP_Client.Utilities.Location_To_LSP_Position
             (Location),
           Context  => Context);
   begin
      return Request;
   end Create_Signature_Help_Request;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self        : On_Character_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Char        : Glib.Gunichar;
      Interactive : Boolean)
   is
      Lang    : Language.Language_Access;
      Context : LSP.Messages.Optional_SignatureHelpContext :=
        (Is_Set => True, others => <>);
   begin
      if not GPS.Kernel.Preferences.LSP_Use_Signatures.Get_Pref
        or else not Interactive
        or else not Should_Send_Signature_Help_Request
          (Kernel  => Kernel,
           File    => File,
           Char    => Char,
           Context => Context,
           Lang    => Lang)
      then
         return;
      end if;

      if Context.Value.triggerKind = ContentChange then
         --  The text has been modified without affecting the signature
         --  handle it later (in the debounce callback)
         return;
      else
         --  trigger or retrigger => the previous request is obsolete
         Signature_Help_Provider.Current_Request.Cancel;
      end if;

      --  Only allow one request at the time while typing
      if not Signature_Help_Provider.Current_Request.Has_Request then
         declare
            Request : Signature_Help_Request_Access :=
              Create_Signature_Help_Request (Kernel, File, Context);
         begin
            Signature_Help_Provider.Current_Request :=
              GPS.LSP_Client.Requests.Execute
                (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
            Signature_Help_Provider.Was_Opened :=
              Signature_Help_Provider.Global_Window /= null;
         end;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type)
   is
      pragma Unreferenced (Project);
      Context : LSP.Messages.Optional_SignatureHelpContext :=
           (Is_Set => True, others => <>);
   begin
      if not GPS.Kernel.Preferences.LSP_Use_Signatures.Get_Pref
        or else Signature_Help_Provider.Global_Window = null
      then
         --  Location_Changed can refresh the window but not create it
         return;
      end if;

      --  See LSP documentation for ContentChange:
      --  Signature help was triggered by the cursor moving or
      --  by the document content changing.
      Context.Value.triggerKind := ContentChange;

      --  Reuse the previous context
      Context.Value.isRetrigger := True;
      Context.Value.activeSignatureHelp :=
        (True,
         (signatures      =>
              Signature_Help_Provider.Global_Window.Signatures,
          activeSignature =>
            (True,
             LSP_Number
               (Signature_Help_Provider.Global_Window.
                    Active_Signature_Nb) - 1),
          activeParameter =>
            (True,
             LSP_Number
               (Signature_Help_Provider.Global_Window.
                    Active_Parameter_Nb) - 1)));

      declare
         Request : Signature_Help_Request_Access :=
           Create_Signature_Help_Request (Kernel, File, Context);
         Editor  : constant Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get (File => File, Open_View => False);
         Lang    : constant Language.Language_Access :=
           Editor.Get_Language;
      begin
         --  The cursor has stopped => cancel the previous location
         Signature_Help_Provider.Current_Request.Cancel;

         Signature_Help_Provider.Current_Request :=
           GPS.LSP_Client.Requests.Execute
             (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
         Signature_Help_Provider.Was_Opened :=
           Signature_Help_Provider.Global_Window /= null;
      end;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_MDI_Child_Selected;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : Gtkada.MDI.MDI_Child) is
   begin
      Signature_Help_Provider.Global_Window.Destroy;
      Signature_Help_Provider.Global_Window := null;
   end Execute;

   -----------------------
   -- In_Signature_Help --
   -----------------------

   function In_Signature_Help return Boolean is
   begin
      return Signature_Help_Provider.Global_Window /= null;
   end In_Signature_Help;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      Character_Added_Hook.Add (new On_Character_Added, Last => True);
      Location_Changed_Hook.Add_Debounce (new On_Location_Changed);
      Set_In_Signature_Help_Provider (Kernel, In_Signature_Help'Access);
   end Register_Module;

end GPS.LSP_Client.Editors.Signature_Help;
