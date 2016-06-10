------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Debugger_Pixmaps;             use Debugger_Pixmaps;
with GNATCOLL.Projects;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GVD.Preferences;              use GVD.Preferences;

package body GVD.Code_Editors is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor  : out Code_Editor;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : not null access Base_Visual_Debugger'Class) is
   begin
      Editor := new Code_Editor_Record;
      Editor.Process := Process;
      Editor.Kernel  := Kernel;

      Editor.Current_Line_Style :=
        Get_Style_Manager (Kernel_Handle (Kernel)).Create_From_Preferences
        ("debugger current line",
         Fg_Pref => null,
         Bg_Pref => Editor_Current_Line_Color);
   end Gtk_New;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Editor : access Code_Editor_Record) return Natural is
   begin
      return Editor.Current_Line;
   end Get_Line;

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Message
     (Editor      : access Code_Editor_Record;
      Message     : String) is
   begin
      Editor.Current_File := GNATCOLL.VFS.No_File;
      Editor.Kernel.Insert (Message);
      --  ??? Do not insert an error, to avoid loosing the focus from the
      --  debugger console. Consider putting a message in the status bar
      --  instead
   end Show_Message;

   -------------------------------
   -- Set_Current_File_And_Line --
   -------------------------------

   procedure Set_Current_File_And_Line
     (Editor : not null access Code_Editor_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural)
   is
      Prev_Current_Line : constant Integer := Editor.Current_Line;
      Prev_File : constant Virtual_File := Editor.Current_File;
   begin
      if File = GNATCOLL.VFS.No_File then
         Editor.Current_File := GNATCOLL.VFS.No_File;
         return;
      end if;

      if Editor.Current_File /= File then
         Editor.Current_File := File;
         Open_File_Action_Hook.Run
           (Kernel            => Editor.Kernel,
            File              => File,
            Line              => 0,
            Project           => GNATCOLL.Projects.No_Project, --   ??? unknown
            New_File          => False,
            Enable_Navigation => False,
            Focus             => False);
      end if;

      Editor.Current_Line := Line;

      if Prev_Current_Line /= 0 then
         declare
            Info : constant Line_Information_Array :=
              (Prev_Current_Line => Empty_Line_Information);
         begin
            Add_Line_Information
              (Editor.Kernel,
               File       => Prev_File,
               Identifier => "Current Line",
               Info       => Info);
         end;
      end if;

      declare
         Info : constant Line_Information_Array :=
           (Line => Line_Information_Record'
              (Text               => Null_Unbounded_String,
               Tooltip_Text       => Null_Unbounded_String,
               Image              => Current_Line_Pixbuf,
               Message            => <>,
               Associated_Command => null));
      begin
         Add_Line_Information
           (Editor.Kernel,
            File       => Editor.Current_File,
            Identifier => "Current Line",
            Info       => Info);
      end;

      Highlight_Current_Line (Editor);

      Debugger_Location_Changed_Hook.Run (Editor.Kernel, Editor.Process);
   end Set_Current_File_And_Line;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Code_Editor_Record) return Virtual_File is
   begin
      return Editor.Current_File;
   end Get_Current_File;

   ---------------------
   -- Free_Debug_Info --
   ---------------------

   procedure Free_Debug_Info (Editor : access Code_Editor_Record) is
   begin
      Editor.Unhighlight_Current_Line;
   end Free_Debug_Info;

   ----------------------------
   -- Highlight_Current_Line --
   ----------------------------

   procedure Highlight_Current_Line
     (Editor  : not null access Code_Editor_Record) is
   begin
      if Editor.Current_File /= GNATCOLL.VFS.No_File then
         declare
            Buffer : constant Editor_Buffer'Class :=
              Editor.Kernel.Get_Buffer_Factory.Get (Editor.Current_File);
         begin
            Buffer.Current_View.Cursor_Goto
              (Location   => Buffer.New_Location_At_Line (Editor.Current_Line),
               Raise_View => True);

            Buffer.Remove_Style
              (Style      => Get_Name (Editor.Current_Line_Style),
               Line       => 0);  --  whole buffer

            Buffer.Apply_Style
              (Style      => Get_Name (Editor.Current_Line_Style),
               Line       => Editor.Current_Line);
         end;
      end if;
   end Highlight_Current_Line;

   ------------------------------
   -- Unhighlight_Current_Line --
   ------------------------------

   procedure Unhighlight_Current_Line
     (Editor  : not null access Code_Editor_Record) is
   begin
      if Editor.Current_File = GNATCOLL.VFS.No_File then
         return;
      end if;

      if Editor.Current_Line /= 0 then
         declare
            Info : constant Line_Information_Array :=
              (Editor.Current_Line => Empty_Line_Information);
         begin
            Add_Line_Information
              (Editor.Kernel,
               File       => Editor.Current_File,
               Identifier => "Current Line",
               --  ??? we should get that from elsewhere.
               Info       => Info);
         end;
      end if;

      declare
         Buffer : constant Editor_Buffer'Class :=
           Editor.Kernel.Get_Buffer_Factory.Get
             (Editor.Current_File, Open_Buffer => False, Open_View => False);
      begin
         Buffer.Remove_Style
           (Style => Get_Name (Editor.Current_Line_Style), Line => 0);
      end;
   end Unhighlight_Current_Line;

end GVD.Code_Editors;
