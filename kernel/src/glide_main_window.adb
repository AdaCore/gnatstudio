-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Glib.Values;
with Pango.Font;                use Pango.Font;
with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.Event;                 use Gdk.Event;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Rc;                    use Gtk.Rc;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with Projects;                  use Projects;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glib.Values;               use Glib.Values;
with Commands.Interactive;      use Commands, Commands.Interactive;

package body Glide_Main_Window is

   Me : constant Debug_Handle := Create ("Glide_Main_Window");

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the delete event.

   procedure Preferences_Changed
     (Main_Window : access GObject_Record'Class;
      Kernel      : Kernel_Handle);
   --  Called when the preferences have changed.

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class);
   --  Called when the the main window is destroyed

   type Navigation_Mode is (All_Windows, Notebook_Windows);
   type MDI_Child_Selection_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
      Move_To_Next : Boolean;
      Mode   : Navigation_Mode;
   end record;
   type MDI_Child_Selection_Command_Access is access all
     MDI_Child_Selection_Command'Class;
   function Execute
     (Command : access MDI_Child_Selection_Command;
      Event   : Gdk_Event)
      return Command_Return_Type;
   --  Check whether Event should activate the selection dialog for MDI
   --  children.

   type Window_Mode is
     (Split_H, Split_V, Tile_H, Tile_V, Cascade, Maximize, Unmaximize, Single);
   type MDI_Window_Actions_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
      Mode   : Window_Mode;
   end record;
   type MDI_Window_Actions_Command_Access is access all
     MDI_Window_Actions_Command'Class;
   function Execute
     (Command : access MDI_Window_Actions_Command;
      Event   : Gdk_Event)
      return Command_Return_Type;
   --  Act on the layout of windows

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access MDI_Window_Actions_Command;
      Event   : Gdk_Event)
      return Command_Return_Type
   is
      pragma Unreferenced (Event);
   begin
      case Command.Mode is
         when Split_H =>
            Split (Get_MDI (Command.Kernel), Orientation_Horizontal);
         when Split_V =>
            Split (Get_MDI (Command.Kernel), Orientation_Vertical);
         when Tile_H =>
            Tile_Horizontally (Get_MDI (Command.Kernel));
         when Tile_V =>
            Tile_Vertically (Get_MDI (Command.Kernel));
         when Cascade =>
            Cascade_Children (Get_MDI (Command.Kernel));
         when Maximize =>
            Maximize_Children (Get_MDI (Command.Kernel), True);
         when Unmaximize =>
            Maximize_Children (Get_MDI (Command.Kernel), False);
         when Single =>
            Single_Window (Get_MDI (Command.Kernel));
      end case;
      return Success;
   end Execute;

   -------------
   -- Anim_Cb --
   -------------

   function Anim_Cb (Kernel : Kernel_Handle) return Boolean is
      Window : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
   begin
      if Advance (Window.Animation_Iter) then
         Set (Window.Animation_Image, Get_Pixbuf (Window.Animation_Iter));
      end if;

      return True;
   end Anim_Cb;

   ---------------------------
   -- Display_Default_Image --
   ---------------------------

   procedure Display_Default_Image (Kernel : Glide_Kernel.Kernel_Handle) is
      Window : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
   begin
      if Window.Animation /= null and then Window.Animation_Image /= null then
         Set (Window.Animation_Image, Get_Static_Image (Window.Animation));
      end if;
   end Display_Default_Image;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window      : out Glide_Window;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String) is
   begin
      Main_Window := new Glide_Window_Record;
      Glide_Main_Window.Initialize
        (Main_Window, Key, Menu_Items, Home_Dir, Prefix_Directory);
   end Gtk_New;

   ----------------------
   -- Confirm_And_Quit --
   ----------------------

   procedure Quit
     (Main_Window : access Glide_Window_Record'Class;
      Force       : Boolean := False) is
   begin
      if Force or else Save_MDI_Children (Main_Window.Kernel) then
         Main_Quit;
      end if;
   end Quit;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);
   begin
      Quit (Glide_Window (Widget));

      return True;
   end Delete_Callback;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Main_Window : access GObject_Record'Class;
      Kernel      : Kernel_Handle)
   is
      use Glib;

      Main       : constant Glide_Window := Glide_Window (Main_Window);
      Key_Theme  : String := Key_Themes'Image
        (Key_Themes'Val (Get_Pref (Kernel, Key_Theme_Name)));

   begin
      Mixed_Case (Key_Theme);
      Gtk.Rc.Parse_String
        ("gtk-font-name=""" &
         To_String (Get_Pref (Kernel, Default_Font)) &
         '"' & ASCII.LF &
         "gtk-can-change-accels=" &
         Integer'Image
           (Boolean'Pos
              (Get_Pref (Kernel, Can_Change_Accels))) & ASCII.LF &
         "gtk-key-theme-name=""" & Key_Theme & '"');

      if Get_Pref (Kernel, Toolbar_Show_Text) then
         Set_Style (Main.Toolbar, Toolbar_Both);
      else
         Set_Style (Main.Toolbar, Toolbar_Icons);
      end if;

      Configure
        (Get_MDI (Kernel),
         Opaque_Resize     => Get_Pref (Kernel, MDI_Opaque),
         Opaque_Move       => Get_Pref (Kernel, MDI_Opaque),
         Close_Floating_Is_Unfloat =>
           not Get_Pref (Kernel, MDI_Destroy_Floats),
         Title_Font        => Get_Pref (Kernel, Default_Font),
         Background_Color  => Get_Pref (Kernel, MDI_Background_Color),
         Title_Bar_Color   => Get_Pref (Kernel, MDI_Title_Bar_Color),
         Focus_Title_Color => Get_Pref (Kernel, MDI_Focus_Title_Color));

      Set_All_Floating_Mode
        (Get_MDI (Kernel), Get_Pref (Kernel, MDI_All_Floating));
   end Preferences_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window      : access Glide_Window_Record'Class;
      Key              : String;
      Menu_Items       : Gtk_Item_Factory_Entry_Array;
      Home_Dir         : String;
      Prefix_Directory : String)
   is
      Box1   : Gtk_Hbox;
      Error  : GError;
      Pixbuf : Gdk_Pixbuf;

   begin
      Gtk_New (Main_Window.Kernel, Gtk_Window (Main_Window), Home_Dir);
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
      Set_Priorities (Main_Window.Process_Mdi, (Left, Top, Bottom, Right));
      Setup_Toplevel_Window (Main_Window.Process_Mdi, Main_Window);
      Main_Window.Home_Dir := new String'(Home_Dir);
      Main_Window.Prefix_Directory := new String'(Prefix_Directory);
      Main_Window.Standalone := False;

      Gtk_New_Hbox (Box1);
      Pack_Start (Main_Window.Toolbar_Box, Box1);
      Gtk_New (Main_Window.Toolbar, Orientation_Horizontal, Toolbar_Icons);
      Set_Tooltips (Main_Window.Toolbar, True);
      Pack_Start (Box1, Main_Window.Toolbar, True, True);

      declare
         File : constant String := Format_Pathname
           (Prefix_Directory & "/share/gps/" &
            Get_Pref (Main_Window.Kernel, Animated_Image));
      begin
         if Is_Regular_File (File) then
            Trace (Me, "Loading animation " & File);
            Gtk_New (Main_Window.Animation_Frame);
            Set_Shadow_Type (Main_Window.Animation_Frame, Shadow_In);
            Pack_End (Box1, Main_Window.Animation_Frame, False, False);

            Gdk_New_From_File (Main_Window.Animation, File, Error);
            Gtk_New (Main_Window.Animation_Image, Main_Window.Animation);
            Main_Window.Animation_Iter := Get_Iter (Main_Window.Animation);
            Pixbuf := Get_Pixbuf (Main_Window.Animation_Iter);
            Set (Main_Window.Animation_Image, Pixbuf);
            Add (Main_Window.Animation_Frame, Main_Window.Animation_Image);
         end if;
      end;

      Widget_Callback.Connect
        (Main_Window, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));

      Kernel_Callback.Connect
        (Main_Window, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         Kernel_Handle (Main_Window.Kernel));

      Preferences_Changed (Main_Window, Main_Window.Kernel);

      Return_Callback.Object_Connect
        (Main_Window, "delete_event",
         Delete_Callback'Access,
         Gtk_Widget (Main_Window),
         After => False);

      --  Support for Win32 WM_DROPFILES drag'n'drop

      Gtk.Dnd.Dest_Set
        (Main_Window, Dest_Default_All, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Main_Window, "drag_data_received",
         Drag_Data_Received'Access, Kernel_Handle (Main_Window.Kernel));
   end Initialize;

   -------------------
   -- Register_Keys --
   -------------------

   procedure Register_Keys (Main_Window : access Glide_Window_Record'Class) is
      Command : MDI_Child_Selection_Command_Access;
      Command2 : MDI_Window_Actions_Command_Access;
   begin
      Command              := new MDI_Child_Selection_Command;
      Command.Kernel       := Main_Window.Kernel;
      Command.Move_To_Next := True;
      Command.Mode         := All_Windows;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to next window",
         Command     => Command,
         Description =>
           -("Select the next window in GPS. Any key binding should use a"
             & " modifier such as control for best usage of this function."));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move to next window",
         Default_Key => "alt-Tab");

      Command              := new MDI_Child_Selection_Command;
      Command.Kernel       := Main_Window.Kernel;
      Command.Move_To_Next := False;
      Command.Mode         := All_Windows;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to previous window",
         Command     => Command,
         Description =>
           -("Select the previous window in GPS. Any key binding should use a"
             & " modifier such as control for best usage of this function."));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move to previous window",
         Default_Key => "alt-shift-ISO_Left_Tab");

      Command              := new MDI_Child_Selection_Command;
      Command.Kernel       := Main_Window.Kernel;
      Command.Mode         := Notebook_Windows;
      Command.Move_To_Next := True;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Select other window",
         Command     => Command,
         Description =>
           -("Select the next splitted window in the central area of GPS."));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Split_H;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Split horizontally",
         Command     => Command2,
         Description => -("Split the current window in two horizontally"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Split_V;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Split vertically",
         Command     => Command2,
         Description => -("Split the current window in two vertically"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Tile_H;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Tile horizontally",
         Command     => Command2,
         Description =>
           -("Tile the windows in the central area horizontally"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Tile_V;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Tile vertically",
         Command     => Command2,
         Description =>
           -("Tile the windows in the central area vertically"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Maximize;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Maximize windows",
         Command     => Command2,
         Description => -("Maximize all windows in the central area"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Unmaximize;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Unmaximize windows",
         Command     => Command2,
         Description => -("Unmaximize all windows in the central area"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Single;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Single window",
         Command     => Command2,
         Description => -("Unsplit the central area of GPS, so that only one"
                          & " window is visible"));
   end Register_Keys;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access MDI_Child_Selection_Command;
      Event   : Gdk_Event) return Command_Return_Type is
   begin
      if Command.Mode = Notebook_Windows then
         Check_Interactive_Selection_Dialog
           (Get_MDI (Command.Kernel), null,
            Move_To_Next => Command.Move_To_Next,
            Visible_In_Central_Only => Command.Mode = Notebook_Windows);
      else
         Check_Interactive_Selection_Dialog
           (Get_MDI (Command.Kernel), Event,
            Move_To_Next => Command.Move_To_Next,
            Visible_In_Central_Only => Command.Mode = Notebook_Windows);
      end if;
      return Success;
   end Execute;

   --------------
   -- GPS_Name --
   --------------

   function GPS_Name (Window : access Glide_Window_Record) return String is
   begin
      if Window.Public_Version then
         return "GPS";
      else
         return "GPS Pro";
      end if;
   end GPS_Name;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class) is
      Win : constant Glide_Window := Glide_Window (Main_Window);

      use Glib;
   begin
      Free (Win.Home_Dir);
      Free (Win.Prefix_Directory);
      Unref (Win.Animation);
      Unref (Win.Animation_Iter);

      if Main_Level > 0 then
         Main_Quit;
      end if;
   end On_Destroy;

   ------------------
   -- Load_Desktop --
   ------------------

   procedure Load_Desktop (Window : access Glide_Window_Record'Class) is
      Was_Loaded : Boolean;
      pragma Unreferenced (Was_Loaded);
   begin
      Was_Loaded := Load_Desktop (Window.Kernel);
   end Load_Desktop;

   -----------------
   -- Reset_Title --
   -----------------

   procedure Reset_Title
     (Window : access Glide_Window_Record;
      Info   : String := "") is
   begin
      if Info = "" then
         Set_Title (Window, GPS_Name (Window) &
                    (-" - GNAT Programming System (project: ") &
                    Project_Name (Get_Project (Window.Kernel)) & ')');
      else
         Set_Title (Window, GPS_Name (Window) &
                    (-" - GNAT Programming System (project: ") &
                    Project_Name (Get_Project (Window.Kernel)) &
                    ") - " & Info);
      end if;
   end Reset_Title;

end Glide_Main_Window;
