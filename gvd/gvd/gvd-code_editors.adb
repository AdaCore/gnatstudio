-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Text_IO;         use Ada.Text_IO;
with Glib;                use Glib;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Notebook;        use Gtk.Notebook;
with Odd.Asm_Editors;     use Odd.Asm_Editors;
with Odd.Explorer;        use Odd.Explorer;
with Odd.Source_Editors;  use Odd.Source_Editors;
with Odd.Types;           use Odd.Types;
with Odd_Intl;            use Odd_Intl;
with Odd.Process;         use Odd.Process;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Gtk.Label;           use Gtk.Label;
with Odd.Strings;         use Odd.Strings;

package body Odd.Code_Editors is

   use Odd;

   ---------------------
   -- Local constants --
   ---------------------

   Display_Explorer : constant Boolean := True;
   --  True if we should associate an explorer tree to each editor.

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
      Initialize_Hpaned (Editor);
      Editor.Process := Gtk_Widget (Process);

      Gtk_New (Editor.Source, Process);
      Gtk_New (Editor.Asm, Process);
      Gtk_New_Vpaned (Editor.Pane);
      Gtk_New (Editor.Explorer_Scroll);
      Set_Policy (Editor.Explorer_Scroll, Policy_Automatic, Policy_Automatic);
      Set_USize (Editor.Explorer_Scroll, Explorer_Width, -1);

      Gtk_New (Editor.Explorer, Editor);
      Add (Editor.Explorer_Scroll, Editor.Explorer);

      Add1 (Editor, Editor.Explorer_Scroll);

      Add2 (Editor, Editor.Source);

      --  Since we are sometimes unparenting these widgets, We need to
      --  make sure they are not automatically destroyed by reference
      --  counting.
      --  ??? Should add a "destroy" callback to the editor to free the
      --  memory.
      Ref (Editor.Source);
      Ref (Editor.Asm);
      Ref (Editor.Pane);

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
         Set_Current_Line (Editor.Explorer, Line);

         --  If the assembly code is displayed, highlight the code for the
         --  current line

         if Editor.Mode = Asm_Only or else Editor.Mode = Source_Asm then
            Highlight_Address_Range (Editor.Asm, Line);
         end if;
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
      return Odd.Source_Editors.Source_Editor is
   begin
      return Editor.Source;
   end Get_Source;

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

      if not Display_Explorer then
         Hide (Editor.Explorer_Scroll);
      end if;
   end Load_File;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor    : access Code_Editor_Record;
      Br        : Odd.Types.Breakpoint_Array) is
   begin
      Update_Breakpoints (Editor.Source, Br);
      Update_Breakpoints (Editor.Asm, Br);
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
      Comments_Color    : String;
      Strings_Color     : String;
      Keywords_Color    : String) is
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

   -----------------
   -- Change_Mode --
   -----------------

   procedure Change_Mode
     (Item : access Gtk_Radio_Menu_Item_Record'Class;
      Data : Editor_Mode_Data) is
   begin
      if Get_Active (Item)
        and then Data.Editor.Mode /= Data.Mode
      then
         case Data.Editor.Mode is
            when Source_Only =>
               Remove (Data.Editor, Data.Editor.Source);
            when Asm_Only =>
               Remove (Data.Editor, Data.Editor.Asm);
            when Source_Asm =>
               Remove (Data.Editor.Pane, Data.Editor.Source);
               Remove (Data.Editor.Pane, Data.Editor.Asm);
               Remove (Data.Editor, Data.Editor.Pane);
         end case;

         Data.Editor.Mode := Data.Mode;

         case Data.Editor.Mode is
            when Source_Only =>
               Add2 (Data.Editor, Data.Editor.Source);
               Show_All (Data.Editor.Source);
               Set_Line (Data.Editor.Source, Data.Editor.Source_Line,
                         Set_Current => True);

            when Asm_Only =>
               Add2 (Data.Editor, Data.Editor.Asm);
               Show_All (Data.Editor.Asm);
               if Data.Editor.Asm_Address /= null then
                  Set_Address (Data.Editor.Asm, Data.Editor.Asm_Address.all);
               end if;
               Highlight_Address_Range
                 (Data.Editor.Asm, Data.Editor.Source_Line);

            when Source_Asm =>
               Add2 (Data.Editor,      Data.Editor.Pane);
               Add1 (Data.Editor.Pane, Data.Editor.Source);
               Add2 (Data.Editor.Pane, Data.Editor.Asm);
               Show_All (Data.Editor.Pane);

               Set_Line (Data.Editor.Source, Data.Editor.Source_Line,
                         Set_Current => True);
               if Data.Editor.Asm_Address /= null then
                  Set_Address (Data.Editor.Asm, Data.Editor.Asm_Address.all);
               end if;
               Highlight_Address_Range
                 (Data.Editor.Asm, Data.Editor.Source_Line);
         end case;
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
      Edit : Code_Editor := Code_Editor (Editor);
      Label : Gtk_Label := Gtk_Label
        (Get_Tab_Label
         (Get_Cur_Page
          (Main_Debug_Window_Access
           (Get_Toplevel (Editor)).Process_Notebook)));
      Tab : GNAT.OS_Lib.String_Access := Debugger_Process_Tab
        (Edit.Process).Descriptor.Program;
   begin
      Odd.Explorer.On_Executable_Changed (Edit.Explorer);

      if Tab /= null then
         Set_Text (Label, Get (Label) (1 .. 6) & Base_File_Name (Tab.all));
      end if;

      --  Always clear the cache for the assembly editor, even if it is not
      --  displayed.
      if Edit.Asm /= null then
         Odd.Asm_Editors.On_Executable_Changed (Edit.Asm);
      end if;
   end On_Executable_Changed;

end Odd.Code_Editors;
