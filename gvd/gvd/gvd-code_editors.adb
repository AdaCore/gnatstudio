-----------------------------------------------------------------------
--                                GPS                                --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
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

with Glib;                use Glib;
with Gtk.Box;             use Gtk.Box;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;

with Pango.Font;          use Pango.Font;
with GVD.Types;           use GVD.Types;
with Basic_Types;         use Basic_Types;
with VFS;                 use VFS;
with Config;              use Config;

package body GVD.Code_Editors is

   use GVD.Text_Box.Asm_Editor;
   use GVD.Text_Box.Source_Editor;

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
        (Editor, "destroy",
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
     (Editor      : access Code_Editor_Record;
      Line        : Natural;
      Set_Current : Boolean := True;
      Process     : Glib.Object.GObject) is
   begin
      Editor.Source_Line := Line;
      Set_Line (Editor.Source, Line, Set_Current, Process);

      if Set_Current then
         --  Highlight the background of the current source line
         Highlight_Current_Line (Editor.Source);

         --  If the assembly code is displayed, highlight the code for the
         --  current line

         if Editor.Mode = Asm or else Editor.Mode = Source_Asm then
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
      return GVD.Text_Box.Source_Editor.Source_Editor is
   begin
      return Editor.Source;
   end Get_Source;

   -------------
   -- Get_Asm --
   -------------

   function Get_Asm
     (Editor : access Code_Editor_Record'Class)
      return GVD.Text_Box.Asm_Editor.Asm_Editor is
   begin
      return Editor.Asm;
   end Get_Asm;

   ---------------------
   -- Get_Asm_Address --
   ---------------------

   function Get_Asm_Address
     (Editor : access Code_Editor_Record'Class) return String is
   begin
      if Editor.Asm_Address = null then
         return "";
      else
         return Editor.Asm_Address.all;
      end if;
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
      File_Name   : VFS.Virtual_File;
      Set_Current : Boolean := True;
      Force       : Boolean := False) is
   begin
      Load_File (Editor.Source, File_Name, Set_Current, Force);
   end Load_File;

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
      Source            : GVD.Text_Box.Source_Editor.Source_Editor;
      Font              : Pango_Font_Description;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Strings_Color     : Gdk.Color.Gdk_Color;
      Keywords_Color    : Gdk.Color.Gdk_Color) is
   begin
      Configure
        (Editor.Asm, Font, Current_Line_Icon,
         Stop_Icon, Strings_Color, Keywords_Color);

      pragma Assert (Editor.Source = null);
      Editor.Source := Source;
      Attach (Editor.Source, Editor);
   end Configure;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Code_Editor_Record) return Virtual_File is
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
   begin
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
