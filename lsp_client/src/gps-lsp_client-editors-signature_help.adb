------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2021, AdaCore                  --
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
with Ada.Strings.Fixed;
with Basic_Types;                     use Basic_Types;
with Completion_Module;               use Completion_Module;
with Dialog_Utils;                    use Dialog_Utils;
with Gdk.Event;                       use Gdk.Event;
with Gdk.Rectangle;                   use Gdk.Rectangle;
with Gdk.Screen;                      use Gdk.Screen;
with Gdk.Types.Keysyms;               use Gdk.Types.Keysyms;
with Gdk.Types;                       use Gdk.Types;
with Gdk.Window;                      use Gdk.Window;
with Glib.Convert;                    use Glib.Convert;
with Glib.Object;
with Glib;                            use Glib;
with GNATCOLL.JSON;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

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
with VSS.Characters;

package body GPS.LSP_Client.Editors.Signature_Help is

   Me          : constant Trace_Handle :=
     Create ("GPS.LSP.SIGNATURE_HELP", Off);
   Me_Advanced : constant Trace_Handle :=
     Create ("GPS.LSP.SIGNATURE_HELP.ADVANCED", Off);

   Max_Signature_Help_Window_Height : constant := 200;

   type Signature_Help_Request is new Abstract_Signature_Help_Request with
   null record;
   type Signature_Help_Request_Access is
     access all Signature_Help_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Signature_Help_Request;
      Result :        LSP.Messages.SignatureHelp);

   overriding procedure On_Error_Message
     (Self    : in out Signature_Help_Request; Code : LSP.Messages.ErrorCodes;
      Message :        String; Data : GNATCOLL.JSON.JSON_Value) is null;

   overriding procedure On_Rejected
     (Self : in out Signature_Help_Request) is null;

   type On_Character_Added is new Character_Hooks_Function with null record;
   overriding procedure Execute
     (Self        : On_Character_Added;
      Kernel      : not null access Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Char        : Glib.Gunichar;
      Interactive : Boolean);
   --  Called when a character is added. Used to trigger the signature help
   --  request when needed.

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
   end record;
   type Signature_Help_Window is access all Signature_Help_Window_Record'Class;

   Global_Window : Signature_Help_Window;
   --  The signature help global window.

   procedure Create_Signature_Help_If_Needed
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Response : LSP.Messages.SignatureHelp);
   --  Create the signature help window if it's not the case yet.

   procedure Refresh
     (Self         : not null Signature_Help_Window;
      Content_Only : Boolean := False);
   --  Refresh the signature help window.
   --  This procedure will update its contents, its size and its position
   --  depending on the currently active signature.
   --  If Content_Only then the size and position will not be updated to
   --  simplify the user interaction (clicking on arrow for example).

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

   function On_Button_Pressed
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Called when a button is pressed in the editor => close the windowS

   function On_Focus_Out
     (Self : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the focus quits the editor => close the window

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
      Signature_Label : constant String := To_UTF_8_String (Signature.label);
      Total_Height    : Gint;
      Total_Width     : Gint;

      procedure Refresh_Size;
      --  Refresh the signature help window's size depending on its contents.

      procedure Refresh_Position;
      --  Refresh the signature help window's position, depending on its size,
      --  the screen boundaries, and the presence of the completion window.

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
         Global_Window.Realize;

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
            Self.Set_Size_Request
              (Width => -1, Height => Max_Signature_Help_Window_Height);
         else
            Self.Set_Size_Request (Width => -1, Height => Total_Height);
         end if;

         Self.Get_Preferred_Width (Dummy, Total_Width);
      end Refresh_Size;

      ----------------------
      -- Refresh_Position --
      ----------------------

      procedure Refresh_Position is
         Completion_Window : constant Gtk_Window :=
           Gtk_Window (Completion_Module.Get_Completion_Display);
         Context           : constant Selection_Context :=
           Get_Current_Context (Self.Kernel);
         Editor            : constant Source_Editor_Box :=
           Get_Source_Box_From_MDI
             (Find_Current_Editor (Self.Kernel, Only_If_Focused => True));
         Toplevel          : constant Gtk_Window :=
           Gtk_Window (Editor.Get_Toplevel);
         Screen            : constant Gdk_Screen := Toplevel.Get_Screen;
         Line              : constant Integer := Line_Information (Context);
         Column            : constant Visible_Column_Type :=
           Column_Information (Context);
         Geom              : Gdk_Rectangle;
         Monitor           : Gint;
         Root_X            : Gint;
         Root_Y            : Gint;
         Completion_X      : Gint;
         Completion_Y      : Gint;
         Completion_Height : Gint;
         Screen_Width      : Gint;
         Screen_Height     : Gint;
         Dummy             : Gint;
      begin
         if Editor = null then
            return;
         end if;

         --  Get the root coordinates of the editor's cursor

         Editor.Get_View.Get_Root_Coords_For_Location
           (Line   => Editable_Line_Type (Line), Column => Column,
            Root_X => Root_X, Root_Y => Root_Y);

         Trace (Me_Advanced, "Cursor X: " & Root_X'Img);
         Trace (Me_Advanced, "Cursor Y: " & Root_Y'Img);

         Trace (Me_Advanced, "Signature help width: " & Total_Width'Img);
         Trace (Me_Advanced, "Signature help height: " & Total_Height'Img);

         --  Get the screen size

         Monitor := Screen.Get_Monitor_At_Point (Root_X, Root_Y);
         Screen.Get_Monitor_Geometry (Monitor, Geom);

         Screen_Width  := Geom.Width;
         Screen_Height := Geom.Height;

         Trace (Me_Advanced, "Screen width: " & Screen_Width'Img);
         Trace (Me_Advanced, "Screen height: " & Screen_Height'Img);

         --  If there is no completion window, display the signature help
         --  window above the cursor or just under if there is not enough place
         --  above.
         --  Otherwise, make sure to not overlap the completion window, by
         --  moving it when it's needed.

         if Completion_Window = null
           or else not Completion_Window.Is_Visible
         then

            --  Check if the window does not go outside of the screen
            --  on the x-axis. Move it if necessary.

            if Root_X - Geom.X + Total_Width > Screen_Width then
               Root_X := Screen_Width + Geom.X - Total_Width;
            end if;

            --  Check if the window does not go outside of the screen
            --  on the y-axis when placed above the cursor (default).
            --  Otherwise, place it right under the cursor.

            if Root_Y - Geom.Y - Total_Height >= 0 then
               Root_Y := Root_Y - Total_Height;
            else
               Editor.Get_View.Get_Root_Coords_For_Location
                 (Line   => Editable_Line_Type (Line + 1), Column => Column,
                  Root_X => Dummy, Root_Y => Root_Y);
            end if;
         else
            --  Get the coordinates and the height of the completion window.

            Get_Origin
              (Self => Completion_Window.Get_Window,
               X    => Completion_X,
               Y    => Completion_Y);
            Completion_Height := Completion_Window.Get_Allocated_Height;

            --   If the completion window is present, two cases:
            --
            --     . The completion window is under the cursor:
            --        - we should place the signature help window above the
            --          cursor
            --        - if not possible, place the signature help window under
            --          the cursor, moving the completion window if necessary
            --          to avoid overlapping.
            --
            --     . The completion window is above the cursor:
            --        - we should place the signature help window under the
            --          cursor
            --        - if not possible, place the signature help window above
            --          the cursor, moving the completion window if necessary
            --          to avoid overlapping.

            if Completion_Y > Root_Y then
               if Root_Y - Total_Height >= 0 then
                  Root_Y := Root_Y - Total_Height;
               else
                  Editor.Get_View.Get_Root_Coords_For_Location
                    (Line   => Editable_Line_Type (Line + 1), Column => Column,
                     Root_X => Dummy, Root_Y => Root_Y);

                  if Completion_Y in Root_Y .. Root_Y + Total_Height then
                     Completion_Window.Move
                       (Completion_X, Completion_Y + Total_Height);
                  end if;
               end if;
            else
               if Root_Y + Total_Height > Screen_Height then
                  Root_Y := Root_Y - Total_Height;

                  if Completion_Y + Completion_Height in
                    Root_Y .. Root_Y + Total_Height
                  then
                     Completion_Window.Move
                       (Completion_X, Completion_Y - Total_Height);
                  end if;
               else
                  Editor.Get_View.Get_Root_Coords_For_Location
                    (Line   => Editable_Line_Type (Line + 1), Column => Column,
                     Root_X => Dummy, Root_Y => Root_Y);
               end if;
            end if;
         end if;

         Trace (Me_Advanced, "Window X: " & Root_X'Img);
         Trace (Me_Advanced, "Window Y: " & Root_Y'Img);

         Global_Window.Move (Root_X, Root_Y);
      end Refresh_Position;

   begin
      Global_Window.View.Show_All;

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
            Doc : constant String := Escape_Text
              (LSP.Types.To_UTF_8_String
                 (Signature.documentation.Value.String));
         begin
            if Doc /= "" then
               Self.Documentation_Label.Set_Text (Doc);
            else
               Self.Documentation_Label.Hide;
               Self.Sep.Hide;
            end if;
         end;
      else
         Self.Documentation_Label.Hide;
         Self.Sep.Hide;
      end if;

      --  Display the active siagnature and highlight the active parameter when
      --  specified.

      if Self.Active_Parameter_Nb > 0 then
         declare
            Param : constant LSP.Messages.ParameterInformation :=
              Signature.parameters
                (if Signature.activeParameter.Is_Set
                 then Natural (Signature.activeParameter.Value + 1)
                 else Self.Active_Parameter_Nb);
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
                  Escaped_Param     : constant String  :=
                    Escape_Text (To_UTF_8_String (Param.label.String));
                  Escaped_Signature : constant String  :=
                    Escape_Text (Signature_Label);
                  Index             : constant Integer :=
                    Ada.Strings.Fixed.Index (Escaped_Signature, Escaped_Param);
               begin
                  if Index > 0 then
                     Self.Active_Signature_Label.Set_Markup
                       (Escaped_Signature
                          (Escaped_Signature'First
                           .. Index - 1)
                        & "<b>" & Escaped_Param & "</b>"
                        & Escaped_Signature
                          (Index + Escaped_Param'Length
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

      if not Content_Only then
         --  Refresh the size and the position of the signature help window.

         Refresh_Size;
         Refresh_Position;
      end if;

      Global_Window.Show;
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
      if Global_Window /= null then
         Global_Window.Destroy;
         Global_Window := null;
      end if;

      return False;
   end On_Button_Pressed;

   ------------------
   -- On_Focus_Out --
   ------------------

   function On_Focus_Out
     (Self : access Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      if Global_Window /= null then
         Global_Window.Destroy;
         Global_Window := null;
      end if;

      return False;
   end On_Focus_Out;

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
      if Global_Window = null then
         return False;
      end if;

      --  If the completion window is present let the event through
      if Completion_Module.In_Smart_Completion then
         return False;
      end if;

      case Key is
         when GDK_Escape =>
            Global_Window.Destroy;
            Global_Window := null;

         when GDK_KP_Up | GDK_Up =>
            Change_Signature
              (Self     => Global_Window,
               Backward => True,
               Clicked  => False);

         when GDK_KP_Down | GDK_Down =>
            Change_Signature
              (Self     => Global_Window,
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
         Global_Window := null;
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
   begin
      if Editor = null then
         --  Signature Help must be attached to an editor
         return;
      end if;

      if Global_Window = null then
         Global_Window := new Signature_Help_Window_Record;

         Global_Window.Kernel := Kernel_Handle (Kernel);
         Gtk.Window.Initialize (Global_Window, Window_Popup);
         Global_Window.Set_Type_Hint (Window_Type_Hint_Tooltip);
         Global_Window.Set_Decorated (False);
         Global_Window.Set_Resizable (False);
         Global_Window.Set_Name ("signature-help-window");
         Global_Window.Set_Accept_Focus (False);

         Global_Window.View := new Dialog_View_With_Button_Box_Record;
         Global_Window.View.Initialize (Pos_Left);
         Global_Window.View.Set_Policy (Policy_Never, Policy_Never);
         Global_Window.View.Set_Scrolled_Policy
           (Policy_Never, Policy_Automatic);
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
         Global_Window.Documentation_Label.Set_Use_Markup (True);
         Global_Window.Documentation_Label.Set_Max_Width_Chars
           (Label_Max_With_Chars);
         Global_Window.Documentation_Label.Set_Line_Wrap (True);
         Global_Window.Documentation_Label.Set_Line_Wrap_Mode
           (Pango_Wrap_Word);
         Global_Window.Documentation_Label.Set_Alignment
           (0.0, 0.5);

         Gtk_New_Hseparator (Global_Window.Sep);
         Global_Window.View.Append
           (Global_Window.Sep, Add_Separator => False);
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
               Name   => Signal_Focus_Out_Event,
               Marsh  =>
                 Return_Callback.To_Marshaller (On_Focus_Out'Access)),
            Global_Window);
         Gtk.Handlers.Add_Watch
           (Return_Callback.Connect
              (Widget => Editor.Get_Toplevel,
               Name   => Signal_Key_Press_Event,
               Marsh  =>
                 Return_Callback.To_Marshaller (On_Key_Pressed'Access)),
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
   end Create_Signature_Help_If_Needed;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Signature_Help_Request;
      Result :        LSP.Messages.SignatureHelp)
   is
   begin
      if not Result.signatures.Is_Empty then
         Create_Signature_Help_If_Needed (Self.Kernel, Result);
      elsif Global_Window /= null then
         Global_Window.Destroy;
         Global_Window := null;
      end if;
   end On_Result_Message;

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
      Editor : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File => File, Open_View => False);
      Lang   : constant Language.Language_Access := Editor.Get_Language;
      Server : constant Language_Server_Access   := Get_Language_Server (Lang);

      function Should_Send_Signature_Help_Request return Boolean;
      --  Return True if the entered character should trigger a signature help
      --  request.

      ----------------------------------------
      -- Should_Send_Signature_Help_Request --
      ----------------------------------------

      function Should_Send_Signature_Help_Request return Boolean is
         Capabilities : constant LSP.Messages.ServerCapabilities :=
           Server.Get_Client.Capabilities;
      begin
         if not Interactive then
            return False;
         end if;

         if not Capabilities.signatureHelpProvider.Is_Set then
            return False;
         end if;

         --  If the signature help window is already displayed, return True:
         --  the signature help request should be sent to update the active
         --  signature (in case of overloading) and/or the active parameter.

         if Global_Window /= null then
            return True;
         end if;

         --  Check if the typed character is among the server's trigger
         --  characters for signature help.

         declare
            Completion_Options : LSP.Messages.SignatureHelpOptions renames
              Capabilities.signatureHelpProvider.Value;
            Virtual_Char       : VSS.Strings.Virtual_String;
         begin
            Virtual_Char.Append (VSS.Characters.Virtual_Character'Val (Char));

            if Completion_Options.triggerCharacters.Is_Set
              and then Completion_Options.triggerCharacters.Value.Contains
                (To_LSP_String (Virtual_Char))
            then
               return True;
            end if;
         end;

         return False;
      end Should_Send_Signature_Help_Request;

   begin
      if Server = null or else not Should_Send_Signature_Help_Request then
         return;
      end if;

      declare
         Editor_Context : constant Selection_Context :=
           Kernel.Get_Current_Context;
         Holder   : constant Controlled_Editor_Buffer_Holder :=
           Kernel.Get_Buffer_Factory.Get_Holder (File);
         Location : constant GPS.Editors.Editor_Location'Class :=
           Holder.Editor.New_Location
             (Line_Information (Editor_Context),
              Column_Information (Editor_Context));

         Request : Signature_Help_Request_Access :=
           new Signature_Help_Request'
             (LSP_Request with Kernel => Kernel_Handle (Kernel), File => File,
              Position => GPS.LSP_Client.Utilities.Location_To_LSP_Position
                (Location));
      begin
         GPS.LSP_Client.Requests.Execute
           (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
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
      Global_Window.Destroy;
      Global_Window := null;
   end Execute;

   -----------------------
   -- In_Signature_Help --
   -----------------------

   function In_Signature_Help return Boolean is
   begin
      return Global_Window /= null;
   end In_Signature_Help;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      if Me.Is_Active then
         Character_Added_Hook.Add (new On_Character_Added);
         Set_In_Signature_Help_Provider (Kernel, In_Signature_Help'Access);
      end if;
   end Register_Module;

end GPS.LSP_Client.Editors.Signature_Help;
