-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
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

with Glib;                use Glib;
with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Ctree;           use Gtk.Ctree;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.MDI;          use Gtkada.MDI;

with GVD.Explorer;        use GVD.Explorer;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Main_Window;     use GVD.Main_Window;
with Basic_Types;         use Basic_Types;
with GVD.Types;           use GVD.Types;
with GVD.Text_Box.Asm_Editor; use GVD.Text_Box.Asm_Editor;
with GVD.Text_Box.Source_Editor; use GVD.Text_Box.Source_Editor;
with GVD.Text_Box.Source_Editor.Builtin;
with GVD.Text_Box.Source_Editor.Socket;
with Odd_Intl;            use Odd_Intl;

with GVD.Process;         use GVD.Process;

package body GVD.Code_Editors is

   use GVD;

   ---------------------
   -- Local constants --
   ---------------------

   Explorer_Width : constant := 200;
   --  Width of the area reserved for the explorer.

   --------------------
   -- Local packages --
   --------------------

   type Editor_Mode_Data is record
      Editor : Code_Editor;
      Mode   : View_Mode;
   end record;

   package Editor_Mode_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Radio_Menu_Item_Record, Editor_Mode_Data);

   procedure Change_Mode
     (Item : access Gtk_Radio_Menu_Item_Record'Class;
      Data : Editor_Mode_Data);
   --  Change the display mode for the editor

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Update_Editor_Frame (Editor : access Gtk_Widget_Record'Class);
   --  Updates the label on top of the source editor.

   -------------------------
   -- Update_Editor_Frame --
   -------------------------

   procedure Update_Editor_Frame (Editor : access Gtk_Widget_Record'Class) is
   begin
      Update_Editor_Frame
        (Process => Debugger_Process_Tab (Code_Editor (Editor).Process));
   end Update_Editor_Frame;

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox
     (Editor      : out Code_Editor;
      Process     : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Editor := new Code_Editor_Record;

      Initialize (Editor, Process);
   end Gtk_New_Hbox;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Code_Editor_Record'Class;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Tab   : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Process);
      Top   : constant GVD_Main_Window := Tab.Window;
      Child : MDI_Child;

   begin
      Initialize_Hbox (Editor);

      Editor.Process := Gtk_Widget (Process);
      Gtk_New (Editor.Asm, Process);
      Gtk_New_Vpaned (Editor.Source_Asm_Pane);

      if Top.Standalone then
         Gtk_New (Editor.Explorer_Scroll);
         Set_Policy
           (Editor.Explorer_Scroll, Policy_Automatic, Policy_Automatic);
         Set_USize (Editor.Explorer_Scroll, Explorer_Width, -1);
         Child := Put (Tab.Process_Mdi, Editor.Explorer_Scroll);
         Set_Title (Child, "Explorer");
         Set_Dock_Side (Child, Left);
         Dock_Child (Child);

         Gtk_New (Editor.Explorer, Editor);
         Add (Editor.Explorer_Scroll, Editor.Explorer);
      end if;

      --  Since we are sometimes unparenting these widgets, We need to
      --  make sure they are not automatically destroyed by reference
      --  counting.
      --  ??? Should add a "destroy" callback to the editor to free the
      --  memory.
      Ref (Editor.Asm);
      Ref (Editor.Source_Asm_Pane);
      Show_All (Editor);
   end Initialize;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor      : access Code_Editor_Record;
      Line        : Natural;
      Set_Current : Boolean := True)
   is
      Top : constant GVD_Main_Window :=
        GVD_Main_Window (Debugger_Process_Tab (Editor.Process).Window);

   begin
      Editor.Source_Line := Line;

      Set_Line (Editor.Source, Line, Set_Current);

      if Set_Current then
         --  If the assembly code is displayed, highlight the code for the
         --  current line

         if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
            Highlight_Address_Range (Editor.Asm, Line);
         end if;

         if Top.Standalone then
            Set_Current_Line (Editor.Explorer, Line);
         end if;

         --  Highlight the background of the current source line
         Highlight_Current_Line (Editor.Source);
      end if;

   end Set_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Editor : access Code_Editor_Record) return Natural is
   begin
      return Get_Line (Editor.Source);
   end Get_Line;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode (Editor : access Code_Editor_Record) return View_Mode is
   begin
      return Editor.Mode;
   end Get_Mode;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Editor : access Code_Editor_Record'Class) return Gtk.Widget.Gtk_Widget is
   begin
      return Editor.Process;
   end Get_Process;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
     (Editor : access Code_Editor_Record'Class)
      return GVD.Text_Box.Source_Editor.Source_Editor is
   begin
      return Editor.Source;
   end Get_Source;

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer
     (Editor : access Code_Editor_Record'Class)
      return GVD.Explorer.Explorer_Access is
   begin
      return Editor.Explorer;
   end Get_Explorer;

   -------------------------
   -- Get_Explorer_Scroll --
   -------------------------

   function Get_Explorer_Scroll
     (Editor : access Code_Editor_Record'Class)
      return Gtk.Scrolled_Window.Gtk_Scrolled_Window is
   begin
      return Editor.Explorer_Scroll;
   end Get_Explorer_Scroll;

   -------------
   -- Get_Asm --
   -------------

   function Get_Asm
     (Editor : access Code_Editor_Record'Class)
     return GVD.Text_Box.Asm_Editor.Asm_Editor is
   begin
      return Editor.Asm;
   end Get_Asm;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access Code_Editor_Record;
      File_Name   : String;
      Set_Current : Boolean := True;
      Force       : Boolean := False)
   is
      Top : constant GVD_Main_Window :=
        GVD_Main_Window (Debugger_Process_Tab (Editor.Process).Window);

   begin
      Load_File (Editor.Source, File_Name, Set_Current, Force);

      --  Create the explorer tree

      if Top.Standalone and then Set_Current then
         Set_Current_File (Editor.Explorer, File_Name);

         if not Get_Pref (Display_Explorer) then
            Hide (Editor.Explorer_Scroll);
         end if;
      end if;

      --  Update the name of the source file in the frame.

      Update_Editor_Frame (Process => Debugger_Process_Tab (Editor.Process));
   end Load_File;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor    : access Code_Editor_Record;
      Br        : GVD.Types.Breakpoint_Array) is
   begin
      if Editor.Mode = Source or else Editor.Mode = Source_Asm then
         Update_Breakpoints (Editor.Source, Br);
      end if;

      if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
         Update_Breakpoints (Editor.Asm, Br);
      end if;
   end Update_Breakpoints;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Editor            : access Code_Editor_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Default_Icon      : Gtkada.Types.Chars_Ptr_Array;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Comments_Color    : Gdk.Color.Gdk_Color;
      Strings_Color     : Gdk.Color.Gdk_Color;
      Keywords_Color    : Gdk.Color.Gdk_Color;
      TTY_Mode          : Boolean;
      External_XID      : Guint32)
   is
      Builtin_Source  : Builtin.Builtin;
      External_Source : Socket.Socket;

   begin
      Configure (Editor.Asm, Ps_Font_Name, Font_Size, Current_Line_Icon,
                 Stop_Icon, Strings_Color, Keywords_Color);

      if External_XID = 0 then
         pragma Assert (Editor.Source = null);
         Builtin.Gtk_New
           (Builtin_Source, Editor.Process, TTY_Mode, Ps_Font_Name,
            Font_Size, Default_Icon, Current_Line_Icon, Stop_Icon,
            Comments_Color, Strings_Color, Keywords_Color);
         Editor.Source := Source_Editor (Builtin_Source);
      else
         pragma Assert (Editor.Source = null);
         Socket.Gtk_New (External_Source, External_XID, TTY_Mode);
         Editor.Source := Source_Editor (External_Source);
      end if;

      Attach (Editor.Source, Editor);
   end Configure;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Code_Editor_Record) return String is
   begin
      return Get_Current_File (Editor.Source);
   end Get_Current_File;

   -----------------------
   -- Display_Selection --
   -----------------------

   procedure Display_Selection (Editor : access Code_Editor_Record)
   is
      Node : Gtk_Ctree_Node := Node_List.Get_Data
        (Node_List.First (Get_Selection (Editor.Explorer)));
   begin
      if Node_Is_Visible (Editor.Explorer, Node) /= Visibility_Full then
         Node_Moveto (Editor.Explorer, Node, 0, 0.1, 0.1);
      end if;
   end Display_Selection;

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language
     (Editor : access Code_Editor_Record;
      Lang   : Language.Language_Access) is
   begin
      Set_Current_Language (Editor.Source, Lang);
   end Set_Current_Language;

   -------------------------------
   -- Append_To_Contextual_Menu --
   -------------------------------

   procedure Append_To_Contextual_Menu
     (Editor : access Code_Editor_Record;
      Menu   : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Mitem : Gtk_Menu_Item;
      Show_Submenu : Gtk_Menu;
      Radio : Gtk_Radio_Menu_Item;
   begin
      --  Create the submenu

      Gtk_New (Show_Submenu);

      Gtk_New (Radio, Widget_SList.Null_List, -"Source Code");
      Set_Active (Radio, Editor.Mode = Source);
      Editor_Mode_Cb.Connect
        (Radio, "activate",
         Editor_Mode_Cb.To_Marshaller (Change_Mode'Access),
         Editor_Mode_Data'(Editor => Code_Editor (Editor),
                           Mode   => Source));
      Append (Show_Submenu, Radio);

      Gtk_New (Radio, Group (Radio), -"Asm Code");
      Set_Active (Radio, Editor.Mode = Asm);
      Editor_Mode_Cb.Connect
        (Radio, "activate",
         Editor_Mode_Cb.To_Marshaller (Change_Mode'Access),
         Editor_Mode_Data'(Editor => Code_Editor (Editor),
                           Mode   => Asm));
      Append (Show_Submenu, Radio);

      Gtk_New (Radio, Group (Radio), -"Asm and Source");
      Set_Active (Radio, Editor.Mode = Source_Asm);
      Editor_Mode_Cb.Connect
        (Radio, "activate",
         Editor_Mode_Cb.To_Marshaller (Change_Mode'Access),
         Editor_Mode_Data'(Editor => Code_Editor (Editor),
                           Mode   => Source_Asm));
      Append (Show_Submenu, Radio);

      --  Insert a separator followed by the submenu at the end
      --  of the contextual menu

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show...");
      Append (Menu, Mitem);
      Set_Submenu (Mitem, Show_Submenu);

   end Append_To_Contextual_Menu;

   ----------------
   -- Apply_Mode --
   ----------------

   procedure Apply_Mode
     (Editor : access Code_Editor_Record; Mode : View_Mode)
   is
      Process : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Editor.Process);
   begin
      if Mode = Editor.Mode then
         return;
      end if;

      case Editor.Mode is
         when Source =>
            Detach (Editor.Source);
         when Asm =>
            Remove (Editor, Editor.Asm);
         when Source_Asm =>
            Detach (Editor.Source);
            Remove (Editor.Source_Asm_Pane, Editor.Asm);
            Remove (Editor, Editor.Source_Asm_Pane);
      end case;

      Editor.Mode := Mode;

      case Editor.Mode is
         when Source =>
            Attach (Editor.Source, Editor);
            Set_Line (Editor.Source, Editor.Source_Line, Set_Current => True);

            if Process.Breakpoints /= null then
               Update_Breakpoints (Editor.Source, Process.Breakpoints.all);
            end if;

         when Asm =>
            Add (Editor, Editor.Asm);
            Show_All (Editor.Asm);

            if Editor.Asm_Address /= null then
               Set_Address (Editor.Asm, Editor.Asm_Address.all);
            end if;

            Highlight_Address_Range (Editor.Asm, Editor.Source_Line);

            if Process.Breakpoints /= null then
               Update_Breakpoints (Editor.Asm, Process.Breakpoints.all);
            end if;

         when Source_Asm =>
            Add (Editor, Editor.Source_Asm_Pane);
            Attach (Editor.Source, Editor.Source_Asm_Pane);
            Add2 (Editor.Source_Asm_Pane, Editor.Asm);
            Show_All (Editor.Source_Asm_Pane);

            if Editor.Asm_Address /= null then
               Set_Address (Editor.Asm, Editor.Asm_Address.all);
            end if;

            Highlight_Address_Range (Editor.Asm, Editor.Source_Line);
            Set_Line (Editor.Source, Editor.Source_Line, Set_Current => True);

            if Process.Breakpoints /= null then
               Update_Breakpoints (Editor.Source, Process.Breakpoints.all);
               Update_Breakpoints (Editor.Asm, Process.Breakpoints.all);
            end if;
      end case;
   end Apply_Mode;

   -----------------
   -- Change_Mode --
   -----------------

   procedure Change_Mode
     (Item : access Gtk_Radio_Menu_Item_Record'Class;
      Data : Editor_Mode_Data) is
   begin
      if Get_Active (Item) and then Data.Editor.Mode /= Data.Mode then
         Apply_Mode (Data.Editor, Data.Mode);
      end if;
   end Change_Mode;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Editor : access Code_Editor_Record;
      Pc     : String) is
   begin
      Free (Editor.Asm_Address);
      Editor.Asm_Address := new String'(Pc);

      if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
         Set_Address (Editor.Asm, Pc);
      end if;
   end Set_Address;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed
     (Editor : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Edit : constant Code_Editor := Code_Editor (Editor);
      Top  : constant GVD_Main_Window :=
        GVD_Main_Window (Debugger_Process_Tab (Edit.Process).Window);

   begin
      if Top.Standalone and then Get_Pref (Display_Explorer) then
         GVD.Explorer.On_Executable_Changed (Edit.Explorer);
      end if;

      --  Always clear the cache for the assembly editor, even if it is not
      --  displayed.

      if Edit.Asm /= null then
         GVD.Text_Box.Asm_Editor.On_Executable_Changed (Edit.Asm);
      end if;
   end On_Executable_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Editor : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Edit : constant Code_Editor := Code_Editor (Editor);
   begin
      if Edit.Mode = Source or else Edit.Mode = Source_Asm then
         GVD.Text_Box.Source_Editor.Preferences_Changed (Edit.Source);
      end if;

      if Edit.Mode = Asm or else Edit.Mode = Source_Asm then
         GVD.Text_Box.Asm_Editor.Preferences_Changed (Edit.Asm);
         Highlight_Address_Range (Edit.Asm, Edit.Source_Line);
      end if;
   end Preferences_Changed;

   ---------------------
   -- Get_Window_Size --
   ---------------------

   function Get_Window_Size
     (Editor : access Code_Editor_Record'Class) return Gint is
   begin
      if Editor.Mode = Asm then
         return Gint (Get_Allocation_Width (Editor.Asm)) - Layout_Width;
      else
         return Gint (Get_Allocation_Width
           (Get_Widget (Editor.Source))) - Layout_Width;
      end if;
   end Get_Window_Size;

end GVD.Code_Editors;
