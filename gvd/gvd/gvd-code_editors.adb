-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;            use Glib;
with Gtk.Box;         use Gtk.Box;
with Gtk.Object;      use Gtk.Object;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.Handlers; use Gtkada.Handlers;

with Pango.Font;      use Pango.Font;
with GVD.Types;       use GVD.Types;
with VFS;             use VFS;

package body GVD.Code_Editors is

   use GVD.Assembly_View;
   use GVD.Source_Editor;

   --------------------
   -- Local packages --
   --------------------

   procedure On_Destroy (Editor : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox
     (Editor  : out Code_Editor;
      Process : access Glib.Object.GObject_Record'Class) is
   begin
      Editor := new Code_Editor_Record;
      Initialize (Editor, Process);
   end Gtk_New_Hbox;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Code_Editor_Record'Class;
      Process : access Glib.Object.GObject_Record'Class) is
   begin
      Initialize_Hbox (Editor);
      Editor.Process := Glib.Object.GObject (Process);
      Gtk_New (Editor.Asm, Process);
      Ref (Editor.Asm);
      Widget_Callback.Connect
        (Editor, Signal_Destroy,
         Widget_Callback.To_Marshaller (On_Destroy'Access));
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Code_Editor := Code_Editor (Editor);
   begin
      Destroy (Ed.Asm);
   end On_Destroy;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor  : access Code_Editor_Record;
      Line    : Natural;
      Process : Glib.Object.GObject) is
   begin
      Editor.Source_Line := Line;
      Set_Line (Editor.Source, Line, Process);

      --  Highlight the background of the current source line
      Highlight_Current_Line (Editor.Source);

      --  If the assembly code is displayed, highlight the code for the
      --  current line

      if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
         Set_Source_Line (Editor.Asm, Line);
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
   -- Set_Mode --
   --------------

   procedure Set_Mode (Editor : access Code_Editor_Record; Mode : View_Mode) is
   begin
      Editor.Mode := Mode;
   end Set_Mode;

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
     (Editor : access Code_Editor_Record'Class) return Glib.Object.GObject is
   begin
      return Editor.Process;
   end Get_Process;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
     (Editor : access Code_Editor_Record'Class)
      return GVD.Source_Editor.Source_Editor is
   begin
      return Editor.Source;
   end Get_Source;

   -------------
   -- Get_Asm --
   -------------

   function Get_Asm
     (Editor : access Code_Editor_Record'Class)
      return GVD.Assembly_View.GVD_Assembly_View is
   begin
      return Editor.Asm;
   end Get_Asm;

   ---------------------
   -- Get_Asm_Address --
   ---------------------

   function Get_Asm_Address
     (Editor : access Code_Editor_Record'Class)
      return GVD.Types.Address_Type is
   begin
      return Editor.Asm_Address;
   end Get_Asm_Address;

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Message
     (Editor      : access Code_Editor_Record;
      Message     : String) is
   begin
      Show_Message (Editor.Source, Message);
   end Show_Message;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access Code_Editor_Record;
      File_Name   : VFS.Virtual_File) is
   begin
      Load_File (Editor.Source, File_Name);
   end Load_File;

   --------------------------
   -- Update_Assembly_View --
   --------------------------

   procedure Update_Assembly_View (Editor : access Code_Editor_Record) is
   begin
      if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
         Update_Display (Editor.Asm);
      end if;
   end Update_Assembly_View;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor    : access Code_Editor_Record;
      Br        : GVD.Types.Breakpoint_Array) is
   begin
      if Editor.Mode = Source or else Editor.Mode = Source_Asm then
         Update_Breakpoints (Editor.Source, Br, Editor.Process);
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
      Source            : GVD.Source_Editor.Source_Editor;
      Font              : Pango_Font_Description;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array)
   is
      pragma Unreferenced (Current_Line_Icon, Stop_Icon);
   begin
      Configure (Editor.Asm, Font);
      pragma Assert (Editor.Source = null);
      Editor.Source := Source;
   end Configure;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Code_Editor_Record) return Virtual_File is
   begin
      return Get_Current_File (Editor.Source);
   end Get_Current_File;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Editor : access Code_Editor_Record;
      Pc     : GVD.Types.Address_Type) is
   begin
      Editor.Asm_Address := Pc;

      if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
         Set_Address (Editor.Asm, Pc);
      end if;
   end Set_Address;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Editor : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Edit : constant Code_Editor := Code_Editor (Editor);
   begin
      if Edit.Mode = Source or else Edit.Mode = Source_Asm then
         GVD.Source_Editor.Preferences_Changed (Edit.Source);
      end if;

      if Edit.Mode = Asm or else Edit.Mode = Source_Asm then
         GVD.Assembly_View.Preferences_Changed (Edit.Asm);
      end if;
   end Preferences_Changed;

end GVD.Code_Editors;
