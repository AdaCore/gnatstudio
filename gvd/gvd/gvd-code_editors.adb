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
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;

with GVD.Asm_Editors;     use GVD.Asm_Editors;
with GVD.Explorer;        use GVD.Explorer;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Source_Editors;  use GVD.Source_Editors;
with GVD.Types;           use GVD.Types;
with Odd_Intl;            use Odd_Intl;

with Process_Proxies;     use Process_Proxies;
with GVD.Process;         use GVD.Process;
with Debugger;            use Debugger;

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

   package Editor_Register is new Register_Generic
     (Editor_Mode_Data, Gtk_Radio_Menu_Item_Record);

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

   procedure Apply_Mode
     (Data : Editor_Mode_Data);
   --  Actually apply mode changes to the editor.

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
     (Editor      : access Code_Editor_Record'Class;
      Process     : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Initialize_Hbox (Editor);

      Gtk_New_Hpaned (Editor.Explorer_Editor_Pane);
      Gtk_New_Hbox (Editor.Editor_Container);

      Editor.Process := Gtk_Widget (Process);

      Gtk_New (Editor.Source, Process);
      Add (Editor.Editor_Container, Editor.Source);

      Gtk_New (Editor.Asm, Process);
      Gtk_New_Vpaned (Editor.Source_Asm_Pane);

      Gtk_New (Editor.Explorer_Scroll);
      Set_Policy (Editor.Explorer_Scroll, Policy_Automatic, Policy_Automatic);
      Set_USize (Editor.Explorer_Scroll, Explorer_Width, -1);

      Gtk_New (Editor.Explorer, Editor);
      Add (Editor.Explorer_Scroll, Editor.Explorer);

      if Get_Pref (Display_Explorer) then
         Add (Editor, Editor.Explorer_Editor_Pane);
         Add1 (Editor.Explorer_Editor_Pane, Editor.Explorer_Scroll);
         Add2 (Editor.Explorer_Editor_Pane, Editor.Editor_Container);

      else
         Ref (Editor.Explorer_Editor_Pane);
         Add (Editor, Editor.Editor_Container);
      end if;

      --  Since we are sometimes unparenting these widgets, We need to
      --  make sure they are not automatically destroyed by reference
      --  counting.
      --  ??? Should add a "destroy" callback to the editor to free the
      --  memory.
      Ref (Editor.Editor_Container);
      Ref (Editor.Source);
      Ref (Editor.Asm);
      Ref (Editor.Source_Asm_Pane);

      Widget_Callback.Object_Connect
        (Editor.Source, "size_allocate",
         Widget_Callback.To_Marshaller (Update_Editor_Frame'Access),
         Editor, After => True);

      Widget_Callback.Object_Connect
        (Editor.Asm, "size_allocate",
         Widget_Callback.To_Marshaller (Update_Editor_Frame'Access),
         Editor, After => True);

      Show_All (Editor);
   end Initialize;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor      : access Code_Editor_Record;
      Line        : Natural;
      Set_Current : Boolean := True) is
   begin
      Editor.Source_Line := Line;

      Set_Line (Editor.Source, Line, Set_Current);

      if Set_Current then
         --  If the assembly code is displayed, highlight the code for the
         --  current line

         if Editor.Mode = Asm_Only or else Editor.Mode = Source_Asm then
            Highlight_Address_Range (Editor.Asm, Line);
         end if;

         Set_Current_Line (Editor.Explorer, Line);

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
      return GVD.Source_Editors.Source_Editor is
   begin
      return Editor.Source;
   end Get_Source;

   -------------------------
   -- Get_Explorer_Scroll --
   -------------------------

   function Get_Explorer_Scroll
     (Editor : access Code_Editor_Record'Class)
      return Gtk.Scrolled_Window.Gtk_Scrolled_Window is
   begin
      return Editor.Explorer_Scroll;
   end Get_Explorer_Scroll;

   ------------------------------
   -- Get_Explorer_Editor_Pane --
   ------------------------------

   function Get_Explorer_Editor_Pane
     (Editor : access Code_Editor_Record'Class)
     return Gtk.Paned.Gtk_Paned is
   begin
      return Editor.Explorer_Editor_Pane;
   end Get_Explorer_Editor_Pane;

   -------------
   -- Get_Asm --
   -------------

   function Get_Asm
     (Editor : access Code_Editor_Record'Class)
     return GVD.Asm_Editors.Asm_Editor is
   begin
      return Editor.Asm;
   end Get_Asm;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access Code_Editor_Record;
      File_Name   : String;
      Set_Current : Boolean := True) is
   begin
      Load_File (Editor.Source, File_Name, Set_Current);

      --  Create the explorer tree.
      if Set_Current then
         Set_Current_File (Editor.Explorer, File_Name);
      end if;

      if not Get_Pref (Display_Explorer) then
         Hide (Editor.Explorer_Scroll);
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
      if Editor.Mode = Source_Only or else Editor.Mode = Source_Asm then
         Update_Breakpoints (Editor.Source, Br);
      end if;

      if Editor.Mode = Asm_Only or else Editor.Mode = Source_Asm then
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
      Keywords_Color    : Gdk.Color.Gdk_Color) is
   begin
      Configure (Editor.Source, Ps_Font_Name, Font_Size, Default_Icon,
                 Current_Line_Icon, Stop_Icon, Comments_Color, Strings_Color,
                 Keywords_Color);
      Configure (Editor.Asm, Ps_Font_Name, Font_Size, Current_Line_Icon,
                 Stop_Icon, Strings_Color, Keywords_Color);
   end Configure;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Code_Editor_Record) return String is
   begin
      return Get_Current_File (Editor.Source);
   end Get_Current_File;

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
      Radio : Gtk_Radio_Menu_Item;
   begin
      Gtk_New (Mitem);
      Append (Menu, Mitem);

      Gtk_New (Radio, Widget_SList.Null_List, -"Show Source Code");
      Set_Active (Radio, Editor.Mode = Source_Only);
      Editor_Mode_Cb.Connect
        (Radio, "activate",
         Editor_Mode_Cb.To_Marshaller (Change_Mode'Access),
         Editor_Mode_Data'(Editor => Code_Editor (Editor),
                           Mode   => Source_Only));
      Append (Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Gtk_New (Radio, Group (Radio), -"Show Asm Code");
      Set_Active (Radio, Editor.Mode = Asm_Only);
      Editor_Mode_Cb.Connect
        (Radio, "activate",
         Editor_Mode_Cb.To_Marshaller (Change_Mode'Access),
         Editor_Mode_Data'(Editor => Code_Editor (Editor),
                           Mode   => Asm_Only));
      Append (Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Gtk_New (Radio, Group (Radio), -"Show Asm and Source");
      Set_Active (Radio, Editor.Mode = Source_Asm);
      Editor_Mode_Cb.Connect
        (Radio, "activate",
         Editor_Mode_Cb.To_Marshaller (Change_Mode'Access),
         Editor_Mode_Data'(Editor => Code_Editor (Editor),
                           Mode   => Source_Asm));
      Append (Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);
   end Append_To_Contextual_Menu;

   ----------------
   -- Apply_Mode --
   ----------------

   procedure Apply_Mode
     (Data : Editor_Mode_Data)
   is
      Process : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Data.Editor.Process);
   begin
      case Data.Editor.Mode is
         when Source_Only =>
            Remove (Data.Editor.Editor_Container, Data.Editor.Source);
         when Asm_Only =>
            Remove (Data.Editor.Editor_Container, Data.Editor.Asm);
         when Source_Asm =>
            Remove (Data.Editor.Source_Asm_Pane, Data.Editor.Source);
            Remove (Data.Editor.Source_Asm_Pane, Data.Editor.Asm);
            Remove (Data.Editor.Editor_Container,
                    Data.Editor.Source_Asm_Pane);
      end case;

      Data.Editor.Mode := Data.Mode;

      case Data.Editor.Mode is
         when Source_Only =>
            Add (Data.Editor.Editor_Container, Data.Editor.Source);
            Show_All (Data.Editor.Source);
            Set_Line (Data.Editor.Source, Data.Editor.Source_Line,
                      Set_Current => True);
            if Process.Breakpoints /= null then
               Update_Breakpoints
                 (Data.Editor.Source, Process.Breakpoints.all);
            end if;

         when Asm_Only =>
            Add (Data.Editor.Editor_Container, Data.Editor.Asm);
            Show_All (Data.Editor.Asm);

            if Data.Editor.Asm_Address /= null then
               Set_Address (Data.Editor.Asm, Data.Editor.Asm_Address.all);
            end if;

            Highlight_Address_Range
              (Data.Editor.Asm, Data.Editor.Source_Line);

            if Process.Breakpoints /= null then
               Update_Breakpoints
                 (Data.Editor.Asm, Process.Breakpoints.all);
            end if;

         when Source_Asm =>
            Add (Data.Editor.Editor_Container, Data.Editor.Source_Asm_Pane);
            Add1 (Data.Editor.Source_Asm_Pane, Data.Editor.Source);
            Add2 (Data.Editor.Source_Asm_Pane, Data.Editor.Asm);
            Show_All (Data.Editor.Source_Asm_Pane);

            if Data.Editor.Asm_Address /= null then
               Set_Address (Data.Editor.Asm, Data.Editor.Asm_Address.all);
            end if;

            Highlight_Address_Range
              (Data.Editor.Asm, Data.Editor.Source_Line);
            Set_Line (Data.Editor.Source, Data.Editor.Source_Line,
                      Set_Current => True);

            if Process.Breakpoints /= null then
               Update_Breakpoints
                 (Data.Editor.Source, Process.Breakpoints.all);
               Update_Breakpoints
                 (Data.Editor.Asm, Process.Breakpoints.all);
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
         --  If we are currently processing a command, wait till the current
         --  one is finished, and then change the mode

         if Editor_Register.Register_Post_Cmd_If_Needed
           (Get_Process (Debugger_Process_Tab (Data.Editor.Process).Debugger),
            Item, Change_Mode'Access, Data)
         then
            return;
         end if;

         Apply_Mode (Data);
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

      if Editor.Mode = Asm_Only or else Editor.Mode = Source_Asm then
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
   begin
      if Get_Pref (Display_Explorer) then
         GVD.Explorer.On_Executable_Changed (Edit.Explorer);
      end if;

      --  Always clear the cache for the assembly editor, even if it is not
      --  displayed.

      if Edit.Asm /= null then
         GVD.Asm_Editors.On_Executable_Changed (Edit.Asm);
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
      if Edit.Mode = Source_Only or else Edit.Mode = Source_Asm then
         GVD.Source_Editors.Preferences_Changed (Edit.Source);
      end if;

      if Edit.Mode = Asm_Only or else Edit.Mode = Source_Asm then
         GVD.Asm_Editors.Preferences_Changed (Edit.Asm);
         Highlight_Address_Range (Edit.Asm, Edit.Source_Line);
      end if;

      if Get_Pref (Display_Explorer) then
         if Get_Parent (Edit.Explorer_Editor_Pane) = null then
            Ref (Edit.Editor_Container);
            Remove (Edit, Edit.Editor_Container);

            Add (Edit, Edit.Explorer_Editor_Pane);
            Add1 (Edit.Explorer_Editor_Pane, Edit.Explorer_Scroll);
            Add2 (Edit.Explorer_Editor_Pane, Edit.Editor_Container);

            Show_All (Edit.Explorer_Editor_Pane);
         end if;

         GVD.Explorer.Preferences_Changed (Edit.Explorer);
         Set_Current_File (Edit.Explorer, Get_Current_File (Edit.Source));

      else
         if Get_Parent (Edit.Explorer_Editor_Pane) /= null then
            Ref (Edit.Explorer_Editor_Pane);
            Remove (Edit, Edit.Explorer_Editor_Pane);

            Ref (Edit.Editor_Container);
            Remove (Edit.Explorer_Editor_Pane, Edit.Editor_Container);

            Add (Edit, Edit.Editor_Container);
         end if;
      end if;

      Apply_Mode (Editor_Mode_Data'
                  (Editor => Edit,
                   Mode   => View_Mode'Value (Get_Pref (Editor_Mode))));

   end Preferences_Changed;

   ---------------------
   -- Get_Window_Size --
   ---------------------

   function Get_Window_Size
     (Editor : access Code_Editor_Record'Class) return Gint
   is
   begin
      if Editor.Mode = Asm_Only then
         return Gint (Get_Allocation_Width (Editor.Asm)) - Layout_Width;
      else
         return Gint (Get_Allocation_Width (Editor.Source)) - Layout_Width;
      end if;
   end Get_Window_Size;

end GVD.Code_Editors;
