-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gtk.Container;        use Gtk.Container;
with Basic_Types;          use Basic_Types;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Main_Window;    use Glide_Main_Window;

package body GVD.Text_Box.Source_Editor.Glide is

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Editor : access GEdit_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class)
   is
      pragma Unreferenced (Editor, Parent);
   begin
      --  Nothing needed within Glide
      null;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Editor : access GEdit_Record) is
      pragma Unreferenced (Editor);
   begin
      --  Nothing needed within Glide
      null;
   end Detach;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Editor : access GEdit_Record) return Natural is
   begin
      return Editor.Line;
   end Get_Line;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out GEdit;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class) is
   begin
      Editor := new GEdit_Record;
      Initialize (Editor, Window);
   end Gtk_New;

   ----------------------------
   -- Highlight_Current_Line --
   ----------------------------

   procedure Highlight_Current_Line (Editor : access GEdit_Record) is
      Kernel  : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
   begin
      pragma Assert (Editor.Current_File /= null);
      Open_File_Editor (Kernel, Editor.Current_File.all,
                        Editor.Line, 1);
   end Highlight_Current_Line;

   --------------------
   -- Highlight_Word --
   --------------------

   procedure Highlight_Word
     (Editor   : access GEdit_Record;
      Line     : Natural;
      Column   : Natural;
      Position : Position_Type)
   is
      pragma Unreferenced (Editor, Line, Column, Position);
   begin
      --  Only needed by the GVD explorer, which is disabled within Glide
      null;
   end Highlight_Word;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor : access GEdit_Record'Class;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class) is
   begin
      Editor.Window := GVD.Main_Window.GVD_Main_Window (Window);
   end Initialize;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access GEdit_Record;
      File_Name   : String;
      Set_Current : Boolean := True;
      Force       : Boolean := False)
   is
      pragma Unreferenced (Set_Current, Force);
      Kernel  : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;

   begin
      Free (Editor.Current_File);
      Editor.Current_File := new String' (File_Name);
      Open_File_Editor (Kernel, Editor.Current_File.all);
   end Load_File;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Editor : access GEdit_Record) is
      pragma Unreferenced (Editor);
   begin
      null;
   end Preferences_Changed;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor      : access GEdit_Record;
      Line        : Natural;
      Set_Current : Boolean := True)
   is
      pragma Unreferenced (Set_Current);
      Kernel  : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;

   begin
      Editor.Line := Line;

      if Editor.Current_File = null then
         return;
      end if;

      Open_File_Editor (Kernel, Editor.Current_File.all, Editor.Line, 1);
   end Set_Line;

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Message
     (Editor  : access GEdit_Record;
      Message : String)
   is
      Kernel  : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
   begin
      Free (Editor.Current_File);
      Editor.Current_File := new String' ("");
      Console.Insert (Kernel, Message);
   end Show_Message;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor : access GEdit_Record;
      Br     : GVD.Types.Breakpoint_Array)
   is
      pragma Unreferenced (Editor, Br);
   begin
      --  ???
      null;
   end Update_Breakpoints;

end GVD.Text_Box.Source_Editor.Glide;
