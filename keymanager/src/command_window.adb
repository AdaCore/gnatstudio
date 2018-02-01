------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;

with Gdk.Event;               use Gdk.Event;
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gdk.Window;              use Gdk.Window;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;
with Gtk.Accel_Group;         use Gtk.Accel_Group;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Label;               use Gtk.Label;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Pango.Enums;             use Pango.Enums;
with Pango.Font;              use Pango.Font;

with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel;              use GPS.Kernel;
with GUI_Utils;               use GUI_Utils;
with KeyManager_Module;       use KeyManager_Module;

package body Command_Window is
   Me : constant Trace_Handle := Create ("GPS.KEY_MANAGER.COMMAND");

   type Rectangle is record
      X, Y, Width, Height : Gint;
   end record;

   type Command_Window_Record is new Gtk_Window_Record with record
      Kernel            : Kernel_Handle;
      Box               : Gtk_Box;
      Prompt            : Gtk_Label;
      Line              : Gtk_Text_View;
      Inst              : Class_Instance := No_Class_Instance;
      On_Changed        : Subprogram_Type;
      On_Key            : Subprogram_Type;
      On_Activate       : Subprogram_Type;
      On_Cancel         : Subprogram_Type;
      Close_On_Activate : Boolean;

      Parent            : Gtk_Window;
      Parent_Geometry   : Rectangle;
   end record;
   type Command_Window is access all Command_Window_Record'Class;

   function Get_Geometry (Window : Gtk_Window) return Rectangle;
   --  Return the coordinates of Window

   procedure Gtk_New
     (Window            : out Command_Window;
      Kernel            : access Kernel_Handle_Record'Class;
      Prompt            : String := "";
      Applies_To_Global : Boolean := True);
   --  Creates a new command window. The window is immediately displayed on
   --  the screen.
   --  If Applies_To_Global is True, then the window is shown at the bottom of
   --  the GPS window, and occupies its whole width. Otherwise, it is shown
   --  at the bottom of the current child.

   type CW_Module_Record is new Module_ID_Record with record
      Window : Command_Window;
   end record;
   type CW_Module_ID is access all CW_Module_Record'Class;

   CW_Module : CW_Module_ID;

   procedure Set_Background_Color
     (Window : not null access Command_Window_Record'Class;
      Color  : Gdk.RGBA.Gdk_RGBA);
   --  Set the bakcground color of the command window

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands for the CommandWindow class

   function On_Key_Press
     (Window : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   function On_Key_Press_After
     (Window : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Handle key presses inside a Command_Window

   procedure On_Destroy (Window : access Gtk_Widget_Record'Class);
   --  The Command_Window is being destroyed

   procedure On_Changed (Window : access Gtk_Widget_Record'Class);
   --  Called when the contents of the command line has changed

   function Get_Text
     (Window : access Command_Window_Record'Class) return String;
   --  Return the current text in the window

   function On_Focus_Out
     (Window : access Gtk_Widget_Record'Class) return Boolean;
   --  Close command window when focus leaves it.

   function On_Parent_Configure
     (Window : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the toplevel displaying Window is reconfigured

   function Command_Window_Event_Handler
     (Event : Gdk_Event; Kernel : access Kernel_Handle_Record'Class)
      return Boolean;
   --  Called when any even is processed by GPS. This is used to cancel the
   --  command window when appropriate.

   Prompt_Cst            : aliased constant String := "prompt";
   Global_Cst            : aliased constant String := "global_window";
   On_Changed_Cst        : aliased constant String := "on_changed";
   On_Activate_Cst       : aliased constant String := "on_activate";
   On_Cancel_Cst         : aliased constant String := "on_cancel";
   On_Key_Cst            : aliased constant String := "on_key";
   Text_Cst              : aliased constant String := "text";
   Cursor_Cst            : aliased constant String := "cursor";
   Color_Cst             : aliased constant String := "color";
   Close_On_Activate_Cst : aliased constant String := "close_on_activate";

   --------------------------
   -- Set_Background_Color --
   --------------------------

   procedure Set_Background_Color
     (Window : not null access Command_Window_Record'Class;
      Color  : Gdk.RGBA.Gdk_RGBA) is
   begin
      Window.Line.Override_Background_Color
        (Gtk_State_Flag_Normal, Color);
      Window.Line.Override_Background_Color
        (Gtk_State_Flag_Active, Color);
      Window.Line.Override_Background_Color
        (Gtk_State_Flag_Selected, Color);
   end Set_Background_Color;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Window : access Command_Window_Record'Class) return String
   is
      Buffer   : constant Gtk_Text_Buffer := Get_Buffer (Window.Line);
      From, To : Gtk_Text_Iter;
   begin
      Get_Start_Iter (Buffer, From);
      Get_End_Iter (Buffer, To);
      return Get_Text (Buffer, From, To);
   end Get_Text;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Window : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean
   is
      Win   : constant Command_Window := Command_Window (Window);
      Key   : constant Gdk_Key_Type := Get_Key_Val (Event);
      Modif : constant Gdk_Modifier_Type :=
                Get_State (Event) and Get_Default_Mod_Mask;
   begin
      --  Ignore when the key is just one of the modifier. No binding can
      --  be associated to them anyway, so this is slightly more efficient,
      --  and this also avoids resetting the last command.
      --  Note that Modif might be different of 0 on Windows in this case.
      if (Key >= GDK_Shift_L and then Key <= GDK_Hyper_R)
        or else Key = GDK_Num_Lock
        or else Key = GDK_ISO_Level3_Shift  --  alt-gr
        or else Key = GDK_Mode_Switch       --  alt-gr on some keyboards
      then
         return True;
      end if;

      --  If the user has pressed Enter, we activate the Command_Window and
      --  close it automatically

      if Modif = 0
        and then (Key = GDK_Return or Key = GDK_ISO_Enter)
      then
         declare
            Str         : constant String := Get_Text (Win);
            On_Activate : constant Subprogram_Type := Win.On_Activate;
            On_Cancel   : constant Subprogram_Type := Win.On_Cancel;
         begin
            if Win.Close_On_Activate then
               --  Prevent callback when window is destroyed
               Win.On_Cancel := null;
               Destroy (Win);
               Win.On_Cancel := On_Cancel;
            end if;

            if On_Activate /= null then
               declare
                  C : Callback_Data'Class :=
                    Create (Get_Script (On_Activate.all), 1);
                  Tmp : Boolean;
                  pragma Unreferenced (Tmp);
               begin
                  Set_Nth_Arg (C, 1, Str);
                  Tmp := Execute (On_Activate, C);
                  Free (C);
               end;
            end if;
         end;

         return True;
      end if;

      if Win.On_Key /= null then
         declare
            C : Callback_Data'Class := Create (Get_Script (Win.On_Key.all), 3);
            Tmp : Boolean;
            Cursor : Gtk_Text_Iter;
         begin
            Get_Iter_At_Mark (Get_Buffer (Win.Line), Cursor,
                              Get_Insert (Get_Buffer (Win.Line)));

            Set_Nth_Arg (C, 1, Get_Text (Win));
            Set_Nth_Arg (C, 2, Image (Key, Modif));
            Set_Nth_Arg (C, 3, Integer (Get_Offset (Cursor)));
            Tmp := Execute (Win.On_Key, C);
            Free (C);

            --  If the window is destroyed, we should probably forward the
            --  key event to the new focus widget (at least it would be the
            --  expected behavior for isearch.py
            --  ??? Not sure how to implement that, nor whether this is the
            --  most appropriate choice in all cases
            --              if not Is_Created (GObject (Win).all)
            --                or else Destroyed_Is_Set (Win)
            --              then
            --                 null;
            --              end if;

            return Tmp;
         end;
      else
         return False;
      end if;
   end On_Key_Press;

   ------------------------
   -- On_Key_Press_After --
   ------------------------

   function On_Key_Press_After
     (Window : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      --  This function is called when the key has not been handled by gtk+.
      --  This is a special key, and we just cancel

      Destroy (Window);
      return False;
   end On_Key_Press_After;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Window : access Gtk_Widget_Record'Class) is
      Win : constant Command_Window := Command_Window (Window);
   begin
      if Win.On_Changed /= null then
         declare
            C : Callback_Data'Class :=
              Create (Get_Script (Win.On_Changed.all), 2);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
            Cursor : Gtk_Text_Iter;
         begin
            Get_Iter_At_Mark (Get_Buffer (Win.Line), Cursor,
                              Get_Insert (Get_Buffer (Win.Line)));
            Set_Nth_Arg (C, 1, Get_Text (Win));
            Set_Nth_Arg (C, 2, Integer (Get_Offset (Cursor)));
            Tmp := Execute (Win.On_Changed, C);
            Free (C);
         end;
      end if;
   end On_Changed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : access Gtk_Widget_Record'Class) is
      Win : constant Command_Window := Command_Window (Window);
   begin
      --  A key that has not been handled by the Command_Window, nor by the
      --  keymanager in fact => this is a special key, destroy the window

      if Win.On_Cancel /= null then
         declare
            C : Callback_Data'Class :=
              Create (Get_Script (Win.On_Cancel.all), 1);
            Tmp : Boolean;
            pragma Unreferenced (Tmp);
         begin
            Set_Nth_Arg (C, 1, Get_Text (Win));
            Tmp := Execute (Win.On_Cancel, C);
            Free (C);
         end;
      end if;

      if CW_Module.Window = Win then
         CW_Module.Window := null;
      end if;

      Remove_Event_Handler (Win.Kernel, Command_Window_Event_Handler'Access);
      KeyManager_Module.Unblock_Key_Shortcuts (Win.Kernel);

   exception
      when E : others => Trace (Me, E);
   end On_Destroy;

   ------------------
   -- On_Focus_Out --
   ------------------

   function On_Focus_Out
     (Window : access Gtk_Widget_Record'Class) return Boolean
   is
   begin
      --  Cancel the search. This will happen mostly when the window manager
      --  has switched the current desktop, and has thus cancelled the key
      --  grab.
      Destroy (Window);
      return True;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Focus_Out;

   -------------------------
   -- On_Parent_Configure --
   -------------------------

   function On_Parent_Configure
     (Window : access Gtk_Widget_Record'Class) return Boolean
   is
      Win : constant Command_Window := Command_Window (Window);
   begin
      if Get_Geometry (Win.Parent) /= Win.Parent_Geometry then
         --  The geometry of the parent window has actually changed: destroy
         --  the command window.
         Destroy (Window);
      end if;

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Parent_Configure;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Window            : out Command_Window;
      Kernel            : access Kernel_Handle_Record'Class;
      Prompt            : String := "";
      Applies_To_Global : Boolean := True)
   is
      X, Y             : Gint;
      Applies_To       : Gtk_Widget;
      Min_H, Natural_H : Gint;
      Current_Editor   : constant MDI_Child := Get_File_Editor
        (Kernel,
         File => GNATCOLL.VFS.No_File);
   begin
      --  Do not make the window modal, although that is much more precise to
      --  be sure we always get all key events on the application. This has the
      --  big drawback that mouse events (in particular scrolling in editors)
      --  is no longer handled properly. Instead, we try to manage with the
      --  focus_in_event on the window itself, which does a server-wide grab.
      --  This one also has the drawback that other applications can no longer
      --  be driven by the keyboard.
      Window := new Command_Window_Record;

      --  Not Window_Popup, since otherwise it never gains the focus
      Gtk.Window.Initialize   (Window, Window_Toplevel);
      Set_Decorated           (Window, False);
      Set_Destroy_With_Parent (Window, True);

      Window.Kernel := Kernel_Handle (Kernel);

      Gtk_New_Hbox (Window.Box, Homogeneous => False);
      Window.Add (Window.Box);

      Gtk_New (Window.Prompt, Prompt);
      Pack_Start
        (Window.Box,
         Window.Prompt,
         Expand  => False,
         Padding => 10);
      Set_Alignment (Window.Prompt, 0.0, 0.5);
      Modify_Font (Window.Prompt, Default_Font.Get_Pref);

      if Prompt = "" then
         Set_Child_Visible (Window.Prompt, False);
      end if;

      Gtk_New (Window.Line);
      Set_Wrap_Mode (Window.Line, Wrap_Word);
      Pack_Start (Window.Box, Window.Line, Expand => True, Fill => True);
      Modify_Font (Window.Line, View_Fixed_Font.Get_Pref);

      Return_Callback.Object_Connect
        (Window.Line, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press'Access), Window);
      Return_Callback.Object_Connect
        (Window, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press_After'Access), Window,
         After => True);
      Widget_Callback.Connect
        (Window, Gtk.Widget.Signal_Destroy, On_Destroy'Access);
      Widget_Callback.Object_Connect
        (Get_Buffer (Window.Line), Signal_Changed,
         On_Changed'Access, Window, After => True);
      Return_Callback.Object_Connect
        (Window.Line, Signal_Focus_Out_Event,
         On_Focus_Out'Access, Window);

      --  Compute size and placement of the window
      if Applies_To_Global
        or else Current_Editor = null
      then
         Applies_To := Gtk_Widget (Get_Main_Window (Kernel));
         Set_Transient_For (Window, Gtk_Window (Applies_To));
      else
         Applies_To := Get_Widget (Current_Editor);
         Set_Transient_For (Window, Gtk_Window (Get_Toplevel (Applies_To)));
      end if;

      --  We want to put the window at the bottom of the editor. For this, we
      --  need to know its size. Unfortunately, until it is realized, a
      --  GtK_Text_View will always request a height of 0, and thus our
      --  window would start at the bottom of the editor, and extend below the
      --  editor.
      --  So instead we do the following: compute an approximate height by
      --  using the font properties. Move the window to that position, and
      --  show it (we move it first so that the window does not show briefly
      --  at coordinates 0,0). After this, we request the actual size now that
      --  the Gtk_Text_View has been realized, and slightly adjust the
      --  position of the window.

      Min_H := Get_Size (View_Fixed_Font.Get_Pref) / Pango_Scale + 4;

      Get_Origin (Get_Window (Applies_To), X, Y);
      Window.Set_Size_Request
        (Width  => Get_Allocated_Width (Applies_To), Height => Min_H);
      Move (Window,
            X => X, Y => Y + Get_Allocated_Height (Applies_To) - Min_H);

      Window.Show_All;  --  Now realize Window.Line so that we know its size

      Get_Preferred_Height_For_Width
        (Window.Line, Applies_To.Get_Allocated_Width, Min_H, Natural_H);

      Window.Set_Size_Request
        (Width  => Get_Allocated_Width (Applies_To), Height => Min_H);
      Move (Window,
            X => X, Y => Y + Get_Allocated_Height (Applies_To) - Min_H);

      Window.Parent := Gtk_Window (Get_Toplevel (Applies_To));
      Window.Parent_Geometry := Get_Geometry (Window.Parent);

      Return_Callback.Object_Connect
        (Window.Parent, Signal_Configure_Event,
         On_Parent_Configure'Access, Window);

      Add_Event_Handler (Kernel, Command_Window_Event_Handler'Access);

      Grab_Toplevel_Focus (Get_MDI (Kernel), Window.Line);

      KeyManager_Module.Block_Key_Shortcuts (Kernel);

      --  Set the default background color of the command window from the
      --  associated preference.
      Set_Background_Color
        (Window,
         Color => Command_Windows_Bg_Color.Get_Pref);

   exception
      when E : others =>
         Trace (Testsuite_Handle, E);
   end Gtk_New;

   ----------------------------------
   -- Command_Window_Event_Handler --
   ----------------------------------

   function Command_Window_Event_Handler
     (Event : Gdk_Event; Kernel : access Kernel_Handle_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Kernel);
   begin
      if CW_Module.Window /= null then
         case Get_Event_Type (Event) is
            when Button_Press =>
               Destroy (CW_Module.Window);
               return True;
            when others =>
               return False;
         end case;
      end if;
      return False;
   end Command_Window_Event_Handler;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Class   : constant Class_Type :=
                  New_Class (Get_Kernel (Data), "CommandWindow");
      Inst    : constant Class_Instance := Nth_Arg (Data, 1, Class);
      Window  : Command_Window;
      Color   : Gdk.RGBA.Gdk_RGBA;
      Success : Boolean;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, ( --  1 => Self,
                                 2 => Prompt_Cst'Access,
                                 3 => Global_Cst'Access,
                                 4 => On_Changed_Cst'Access,
                                 5 => On_Activate_Cst'Access,
                                 6 => On_Cancel_Cst'Access,
                                 7 => On_Key_Cst'Access,
                                 8 => Close_On_Activate_Cst'Access));
         if CW_Module.Window /= null then
            Set_Error_Msg (Data, "A command window is already in use");
         else
            Gtk_New
              (CW_Module.Window, Get_Kernel (Data),
               Prompt            => Nth_Arg (Data, 2, ""),
               Applies_To_Global => Nth_Arg (Data, 3, False));

            Set_Data (Inst, Widget => GObject (CW_Module.Window));
            CW_Module.Window.Inst := Inst;
            CW_Module.Window.On_Changed  := Nth_Arg (Data, 4, null);
            CW_Module.Window.On_Activate := Nth_Arg (Data, 5, null);
            CW_Module.Window.On_Cancel   := Nth_Arg (Data, 6, null);
            CW_Module.Window.On_Key      := Nth_Arg (Data, 7, null);
            CW_Module.Window.Close_On_Activate := Nth_Arg (Data, 8, True);
         end if;

      elsif Command = "write" then
         Name_Parameters (Data, ( --  1 => Self,
                                 2 => Text_Cst'Access,
                                 3 => Cursor_Cst'Access));
         Window := Command_Window (GObject'(Get_Data (Inst)));
         if Window /= null then
            declare
               Buffer : constant Gtk_Text_Buffer := Get_Buffer (Window.Line);
               Loc    : constant Gint := Gint (Nth_Arg (Data, 3, -1));
               From, To : Gtk_Text_Iter;
            begin
               Get_Start_Iter (Buffer, From);
               Get_End_Iter (Buffer, To);
               Delete (Buffer, From, To);
               Insert_At_Cursor (Buffer, Nth_Arg (Data, 2));

               if Loc /= -1 then
                  Get_Iter_At_Offset (Buffer, From, Loc);
                  Place_Cursor (Buffer, From);
               end if;
            end;
         end if;

      elsif Command = "read" then
         Window := Command_Window (GObject'(Get_Data (Inst)));
         if Window /= null then
            Set_Return_Value (Data, Get_Text (Window));
         else
            Set_Return_Value (Data, String'(""));
         end if;

      elsif Command = "set_prompt" then
         Name_Parameters (Data, ( --  1 => Self,
                                 2 => Prompt_Cst'Access));
         Window := Command_Window (GObject'(Get_Data (Inst)));
         if Window /= null then
            declare
               P : constant String := Nth_Arg (Data, 2);
            begin
               Set_Text (Window.Prompt, P);
               if P /= "" then
                  Set_Child_Visible (Window.Prompt, True);
                  Show_All (Window.Prompt);
               else
                  Set_Child_Visible (Window.Prompt, False);
                  Hide (Window.Prompt);
               end if;
            end;
         end if;

      elsif Command = "set_background" then
         Name_Parameters (Data, ( --  1 => Self,
                                 2 => Color_Cst'Access));
         Window := Command_Window (GObject'(Get_Data (Inst)));
         if Window /= null then
            if Nth_Arg (Data, 2, "") = "" then
               Color := Gdk.RGBA.Null_RGBA;
            else
               Parse (Color, Nth_Arg (Data, 2), Success);
            end if;
            Set_Background_Color (Window, Color);
         end if;
      end if;
   end Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class : constant Class_Type := New_Class
        (Kernel, "CommandWindow", Get_GUI_Class (Kernel));
   begin
      CW_Module := new CW_Module_Record;
      Register_Module (CW_Module, Kernel, "Command_Window");

      Register_Command
        (Kernel, Constructor_Method, 0, 7, Class => Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel, "write", 1, 2, Class => Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel, "read", 0, 0, Class => Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel, "set_background", 0, 1, Class => Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel, "set_prompt", 1, 1, Class => Class,
         Handler => Command_Handler'Access);
   end Register_Module;

   ------------------
   -- Get_Geometry --
   ------------------

   function Get_Geometry (Window : Gtk_Window) return Rectangle is
      R : Rectangle;
   begin
      Window.Get_Position (R.X, R.Y);
      Window.Get_Size (R.Width, R.Height);
      return R;
   end Get_Geometry;

end Command_Window;
