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
with Ada.Unchecked_Deallocation;
with Commands.Debugger;            use Commands.Debugger;
with Commands;                     use Commands;
with Debugger_Pixmaps;             use Debugger_Pixmaps;
with GNAT.Strings;                 use GNAT.Strings;
with GNATCOLL.Projects;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GVD.Preferences;              use GVD.Preferences;
with GVD.Process;                  use GVD.Process;
with GVD.Types;                    use GVD.Types;

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
            Info : aliased Line_Information_Array :=
              (Prev_Current_Line => Empty_Line_Information);
         begin
            Add_Line_Information
              (Editor.Kernel,
               File       => Prev_File,
               Identifier => "Current Line",
               Info       => Info'Access);
         end;
      end if;

      declare
         Info : aliased Line_Information_Array :=
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
            Info       => Info'Access);
      end;

      Highlight_Current_Line (Editor);

      Debugger_Location_Changed_Hook.Run (Editor.Kernel, Editor.Process);
   end Set_Current_File_And_Line;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor    : access Code_Editor_Record;
      Br        : GVD.Types.Breakpoint_Array)
   is
      Tab     : constant Visual_Debugger := Visual_Debugger (Editor.Process);

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
                  Kernel_Handle (Editor.Kernel),
                  Tab,
                  Unset,
                  Br (J).File,
                  Br (J).Line);

               if not Br (J).Enabled then
                  A (L).Image := Line_Has_Disabled_Breakpoint_Pixbuf;
                  A (L).Tooltip_Text := To_Unbounded_String
                    ("A disabled breakpoint has been set on this line");

               elsif Br (J).Condition /= null then
                  A (L).Image := Line_Has_Conditional_Breakpoint_Pixbuf;
                  A (L).Tooltip_Text := To_Unbounded_String
                    ("A conditional breakpoint has been set on this line");

               else
                  A (L).Image := Line_Has_Breakpoint_Pixbuf;
                  A (L).Tooltip_Text := To_Unbounded_String
                    ("An active breakpoint has been set on this line");
               end if;

               A (L).Associated_Command := Command_Access (Other_Command);

               --  Try to resolve file when it is not present to handle
               --  relocation of project.

               File := Br (J).File;

               if not File.Is_Regular_File then
                  File := Editor.Kernel.Create_From_Base (File.Full_Name);
               end if;

               Add_Line_Information
                 (Editor.Kernel,
                  File       => File,
                  Identifier => Breakpoints_Column_Id,
                  Info       => A);
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
                  L             : constant Integer :=
                    Editor.Current_Breakpoints (J).Line;
                  A             : Line_Information_Data :=
                    new Line_Information_Array (L .. L);
               begin
                  Create
                    (Other_Command,
                     Kernel_Handle (Editor.Kernel),
                     Tab,
                     Set,
                     Editor.Current_Breakpoints (J).File,
                     Editor.Current_Breakpoints (J).Line);

                  if Editor_Show_Line_With_Code.Get_Pref then
                     A (L).Image := Line_Has_Code_Pixbuf;
                  else
                     A (L).Image := Line_Might_Have_Code_Pixbuf;
                  end if;

                  A (L).Associated_Command :=
                    Command_Access (Other_Command);

                  --  Try to resolve file when it is not present to handle
                  --  relocation of project.

                  File := Editor.Current_Breakpoints (J).File;

                  if not File.Is_Regular_File then
                     File :=
                       Editor.Kernel.Create_From_Base (File.Full_Name);
                  end if;

                  Add_Line_Information
                    (Editor.Kernel,
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
      Free (Editor.Current_Breakpoints);
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
            Info : aliased Line_Information_Array :=
              (Editor.Current_Line => Empty_Line_Information);
         begin
            Add_Line_Information
              (Editor.Kernel,
               File       => Editor.Current_File,
               Identifier => "Current Line",
               --  ??? we should get that from elsewhere.
               Info       => Info'Access);
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
