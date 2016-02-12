------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with GPS.Editors;               use GPS.Editors;
with GPS.Kernel;                use GPS.Kernel;

with GVD.Process;               use GVD.Process;
with GVD.Types;                 use GVD.Types;
with Debugger_Pixmaps;          use Debugger_Pixmaps;
with String_List_Utils;         use String_List_Utils;

with GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Commands;                  use Commands;
with Commands.Debugger;         use Commands.Debugger;

with Ada.Unchecked_Deallocation;
with GNAT.Strings;              use GNAT.Strings;

with GVD.Preferences;           use GVD.Preferences;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body GVD.Source_Editor.GPS is

   use String_List_Utils.String_List;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (Editor : access GEdit_Record) return Natural is
   begin
      return Editor.Line;
   end Get_Line;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out GEdit;
      Window : access GPS_Window_Record'Class) is
   begin
      Editor := new GEdit_Record;
      Initialize (Editor, Window);
   end Gtk_New;

   ----------------------------
   -- Highlight_Current_Line --
   ----------------------------

   overriding procedure Highlight_Current_Line
     (Editor : access GEdit_Record)
   is
      Kernel : constant Kernel_Handle := Editor.Window.Kernel;
   begin
      if Editor.Current_File /= GNATCOLL.VFS.No_File then
         declare
            Buffer : constant Editor_Buffer'Class :=
              Kernel.Get_Buffer_Factory.Get (Editor.Current_File);
            View   : constant Editor_View'Class   := Buffer.Current_View;
         begin
            View.Cursor_Goto
              (Location   => Buffer.New_Location_At_Line (Editor.Line),
               Raise_View => True);

            Buffer.Remove_Style
              (Style      => Get_Name (Editor.Current_Line_Style),
               Line       => 0);  --  whole buffer

            Buffer.Apply_Style
              (Style      => Get_Name (Editor.Current_Line_Style),
               Line       => Editor.Line);
         end;

         Add_Unique_Sorted
           (Editor.Highlighted_Files,
            +Full_Name (Editor.Current_File));
      end if;
   end Highlight_Current_Line;

   ------------------------------
   -- Unhighlight_Current_Line --
   ------------------------------

   overriding procedure Unhighlight_Current_Line
     (Editor  : access GEdit_Record;
      Process : Glib.Object.GObject)
   is
      Kernel : constant Kernel_Handle := Editor.Window.Kernel;
      Line   : constant Integer :=
                 Get_Current_Source_Line (Visual_Debugger (Process));

   begin
      if Editor.Current_File = GNATCOLL.VFS.No_File then
         return;
      end if;

      if Line /= 0 then
         declare
            Info : aliased Line_Information_Array :=
               (Line => Empty_Line_Information);
         begin
            Add_Line_Information
              (Kernel,
               File       => Editor.Current_File,
               Identifier => "Current Line",
               --  ??? we should get that from elsewhere.
               Info       => Info'Access);
         end;
      end if;

      Kernel.Get_Buffer_Factory.Get (Editor.Current_File).Remove_Style
        (Style => Get_Name (Editor.Current_Line_Style), Line => 0);
   end Unhighlight_Current_Line;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor : access GEdit_Record'Class;
      Window : access GPS_Window_Record'Class)
   is
   begin
      Editor.Window := GPS_Window (Window);

      Editor.Current_Line_Style :=
        Get_Style_Manager (Window.Kernel).Create_From_Preferences
        ("debugger current line",
         Fg_Pref => null,
         Bg_Pref => Editor_Current_Line_Color);

      Preferences_Changed (Editor);
   end Initialize;

   ---------------
   -- Load_File --
   ---------------

   overriding procedure Load_File
     (Editor    : access GEdit_Record;
      File_Name : GNATCOLL.VFS.Virtual_File)
   is
      Kernel : constant Kernel_Handle := Editor.Window.Kernel;
      File   : Virtual_File;
   begin
      if File_Name = GNATCOLL.VFS.No_File then
         Editor.Current_File := GNATCOLL.VFS.No_File;
         return;
      end if;

      if Is_Regular_File (File_Name) then
         File := File_Name;
      else
         File := Kernel.Create_From_Base (Full_Name (File_Name));
      end if;

      if Editor.Current_File /= File then
         Editor.Current_File := File;

         Open_File_Action_Hook.Run
           (Kernel, File,
            Project           => GNATCOLL.Projects.No_Project, --   ??? unknown
            New_File          => False,
            Enable_Navigation => False,
            Focus             => False);
      end if;
   end Load_File;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   overriding procedure Preferences_Changed (Editor : access GEdit_Record) is
   null;

   --------------
   -- Set_Line --
   --------------

   overriding procedure Set_Line
     (Editor  : access GEdit_Record;
      Line    : Natural;
      Process : Glib.Object.GObject)
   is
      Kernel  : constant Kernel_Handle := Editor.Window.Kernel;
      Tab     : constant Visual_Debugger := Visual_Debugger (Process);

      Prev_Current_Line : constant Integer := Get_Current_Source_Line (Tab);
      Prev_File         : constant Virtual_File :=
        Get_Current_Source_File (Tab);

   begin
      if Prev_Current_Line /= 0 then
         declare
            Info : aliased Line_Information_Array :=
               (Prev_Current_Line => Empty_Line_Information);
         begin
            Add_Line_Information
              (Kernel,
               File => Prev_File,
               Identifier => "Current Line",
               --  ??? we should get that from elsewhere.
               Info => Info'Access);
         end;
      end if;

      if Editor.Current_File = GNATCOLL.VFS.No_File then
         return;
      end if;

      Editor.Line := Line;
      Open_File_Action_Hook.Run
        (Kernel,
         Editor.Current_File,
         Project  => GNATCOLL.Projects.No_Project,  --   ??? unknown
         Line     => Editor.Line,
         Column   => 1,
         New_File => False,
         Focus    => False);
      Append (Editor.Highlighted_Files,
              +Full_Name (Editor.Current_File));

      declare
         Info : aliased Line_Information_Array :=
            (Line => Line_Information_Record'
               (Text               => null,
                Tooltip_Text       => null,
                Image              => new String'(Current_Line_Pixbuf),
                Associated_Command => null));
      begin
         Add_Line_Information
           (Kernel,
            File => Editor.Current_File,
            Identifier => "Current Line",
            --  ??? we should get that from elsewhere.
            Info => Info'Access);
         Set_Current_Source_Location (Tab, Editor.Current_File, Line);
      end;
   end Set_Line;

   ------------------
   -- Show_Message --
   ------------------

   overriding procedure Show_Message
     (Editor  : access GEdit_Record;
      Message : String)
   is
      Kernel  : constant Kernel_Handle := Editor.Window.Kernel;
   begin
      Editor.Current_File := GNATCOLL.VFS.No_File;
      Kernel.Insert (Message);
      --  ??? Do not insert an error, to avoid loosing the focus from the
      --  debugger console. Consider putting a message in the status bar
      --  instead
   end Show_Message;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   overriding procedure Update_Breakpoints
     (Editor  : access GEdit_Record;
      Br      : GVD.Types.Breakpoint_Array;
      Process : Glib.Object.GObject)
   is
      Kernel  : constant Kernel_Handle := Editor.Window.Kernel;
      Tab     : constant Visual_Debugger := Visual_Debugger (Process);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Breakpoint_Array, Breakpoint_Array_Ptr);

      function Copy_Bp (D : Breakpoint_Data) return Breakpoint_Data;
      --  Deep copy of a breakpoint.

      procedure Add_Unique_Info
        (A     : in out Breakpoint_Array_Ptr;
         N     : Breakpoint_Data;
         Added : out Boolean);
      --  Add an entry N in A, if A does not already contain such an entry.
      --  N is set to an entry that contains no valid breakpoint..
      --  Perform a resize on A if necessary.
      --  If the entry existed in A before calling Add_Unique_Info,
      --  Added is set to False, True otherwise.

      -------------
      -- Copy_Bp --
      -------------

      function Copy_Bp (D : Breakpoint_Data) return Breakpoint_Data is
         Result : Breakpoint_Data;
      begin
         Result.Num := D.Num;
         Result.The_Type := D.The_Type;
         Result.Disposition := D.Disposition;
         Result.Enabled := D.Enabled;

         if D.Address /= Invalid_Address then
            Result.Address := D.Address;
         end if;

         if D.Expression /= null then
            Result.Expression := new String'(D.Expression.all);
         end if;

         Result.File := D.File;

         if D.Except /= null then
            Result.Except := new String'(D.Except.all);
         end if;

         if D.Subprogram /= null then
            Result.Subprogram := new String'(D.Subprogram.all);
         end if;

         Result.Line := D.Line;

         Result.Ignore := D.Ignore;

         if D.Condition /= null then
            Result.Condition := new String'(D.Condition.all);
         end if;

         if D.Commands /= null then
            Result.Commands := new String'(D.Commands.all);
         end if;

         Result.Scope := D.Scope;
         Result.Action := D.Action;

         return Result;
      end Copy_Bp;

      ---------------------
      -- Add_Unique_Info --
      ---------------------

      procedure Add_Unique_Info
        (A     : in out Breakpoint_Array_Ptr;
         N     : Breakpoint_Data;
         Added : out Boolean)
      is
         First_Zero : Integer := -1;
      begin
         if N.File = GNATCOLL.VFS.No_File then
            Added := False;
            return;
         end if;

         if A = null then
            A := new Breakpoint_Array (1 .. 16);
         end if;

         for J in A'Range loop
            if A (J).Line = N.Line
              and then A (J).File = N.File
              and then A (J).Num  = N.Num
            then
               Added := False;
               return;
            end if;

            if First_Zero = -1
              and then A (J).File = GNATCOLL.VFS.No_File
            then
               First_Zero := J;
            end if;
         end loop;

         Added := True;

         if First_Zero = -1 then
            --  We need to resize A.

            declare
               B : constant Breakpoint_Array_Ptr := new Breakpoint_Array
                 (A'First .. A'Last + 16);
            begin
               B (A'First .. A'Last) := A.all;
               B (A'Last + 1) := Copy_Bp (N);
               Unchecked_Free (A);
               A := B;
            end;

         else
            A (First_Zero) := Copy_Bp (N);
         end if;
      end Add_Unique_Info;

      Result : Boolean := False;
      Found  : Boolean := False;
      Index  : Natural;
      File   : Virtual_File;

   begin
      --  Add new breakpoints to the column information.
      for J in Br'Range loop
         Add_Unique_Info (Editor.Current_Breakpoints, Br (J), Result);

         if Result then
            declare
               Other_Command : Set_Breakpoint_Command_Access;
               L             : constant Integer := Br (J).Line;
               A             : Line_Information_Data :=
                  new Line_Information_Array (L .. L);
            begin
               Create
                 (Other_Command,
                  Kernel,
                  Tab,
                  Unset,
                  Br (J).File,
                  Br (J).Line);
               A (L).Image := new String'(Line_Has_Breakpoint_Pixbuf);
               A (L).Associated_Command := Command_Access (Other_Command);

               --  Try to resolve file when it is not present to handle
               --  relocation of project.

               File := Br (J).File;

               if not File.Is_Regular_File then
                  File := Kernel.Create_From_Base (File.Full_Name);
               end if;

               Add_Line_Information
                 (Kernel,
                  File  => File,
                  Identifier => Breakpoints_Column_Id,
                  Info => A);
               Unchecked_Free (A);
            end;
         end if;
      end loop;

      --  Remove old breakpoints from the column information.
      if Editor.Current_Breakpoints = null then
         return;
      end if;

      for J in Editor.Current_Breakpoints'Range loop
         if Editor.Current_Breakpoints (J).File /= GNATCOLL.VFS.No_File then
            Found := False;
            Index := Br'First;

            while (not Found)
              and then Index <= Br'Last
            loop
               if Editor.Current_Breakpoints (J).Line = Br (Index).Line
                 and then Editor.Current_Breakpoints (J).File =
                 Br (Index).File
               then
                  Found := True;
               end if;

               Index := Index + 1;
            end loop;

            if not Found then
               declare
                  Other_Command : Set_Breakpoint_Command_Access;
                  L : constant Integer := Editor.Current_Breakpoints (J).Line;
                  A : Line_Information_Data :=
                     new Line_Information_Array (L .. L);
               begin
                  Create
                    (Other_Command,
                     Kernel,
                     Tab,
                     Set,
                     Editor.Current_Breakpoints (J).File,
                     Editor.Current_Breakpoints (J).Line);

                  if Editor_Show_Line_With_Code.Get_Pref then
                     A (L).Image := new String'(Line_Has_Code_Pixbuf);
                  else
                     A (L).Image := new String'(Line_Might_Have_Code_Pixbuf);
                  end if;

                  A (L).Associated_Command := Command_Access (Other_Command);

                  --  Try to resolve file when it is not present to handle
                  --  relocation of project.

                  File := Editor.Current_Breakpoints (J).File;

                  if not File.Is_Regular_File then
                     File := Kernel.Create_From_Base (File.Full_Name);
                  end if;

                  Add_Line_Information
                    (Kernel,
                     File       => File,
                     Identifier => Breakpoints_Column_Id,
                     Info       => A);
                  Unchecked_Free (A);
               end;

               Free (Editor.Current_Breakpoints (J));
            end if;
         end if;
      end loop;
   end Update_Breakpoints;

   ---------------------
   -- Free_Debug_Info --
   ---------------------

   procedure Free_Debug_Info (Editor : access GEdit_Record) is
      Kernel : constant Kernel_Handle := Editor.Window.Kernel;
   begin
      Free (Editor.Current_Breakpoints);

      --  Clear the line highlight for files that had an highlight.

      while not Is_Empty (Editor.Highlighted_Files) loop
         Kernel.Get_Buffer_Factory.Get
           (Create (+Head (Editor.Highlighted_Files))).Remove_Style
           (Style => Get_Name (Editor.Current_Line_Style), Line => 0);
         Next (Editor.Highlighted_Files);
      end loop;
   end Free_Debug_Info;

end GVD.Source_Editor.GPS;
