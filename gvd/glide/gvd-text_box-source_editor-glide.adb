-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Gdk.Pixbuf;
with Gdk.Color;                use Gdk.Color;
with Gtk.Container;            use Gtk.Container;
with Gtkada.MDI;               use Gtkada.MDI;
with Basic_Types;              use Basic_Types;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Main_Window;        use Glide_Main_Window;

with GVD.Process;          use GVD.Process;
with GVD.Code_Editors;     use GVD.Code_Editors;
with GVD.Types;            use GVD.Types;
with GVD_Module;           use GVD_Module;
with Debugger_Pixmaps;     use Debugger_Pixmaps;
with String_List_Utils;    use String_List_Utils;
with VFS;                  use VFS;

with GVD.Text_Box.Asm_Editor; use GVD.Text_Box;

with Commands;                use Commands;
with Commands.Debugger;       use Commands.Debugger;

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;

with GVD.Preferences;         use GVD.Preferences;

package body GVD.Text_Box.Source_Editor.Glide is

   Highlight_Category : constant String := "Debugger Highlight";

   use String_List_Utils.String_List;

   ----------------
   -- Apply_Mode --
   ----------------

   procedure Apply_Mode (Editor : access GEdit_Record; Mode : View_Mode) is
      Kernel : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
      Top      : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process  : constant Visual_Debugger := Get_Current_Process (Top);
      Edit     : constant Code_Editor  := Process.Editor_Text;
      Assembly : constant Asm_Editor.Asm_Editor := Get_Asm (Edit);

   begin
      if Mode = Get_Mode (Edit) then
         return;
      end if;

      case Mode is
         when Source =>
            Gtkada.MDI.Close (Get_MDI (Kernel), Assembly);

         when Asm | Source_Asm =>
            On_Assembly (Kernel, Kernel);
      end case;
   end Apply_Mode;

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
      Kernel : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;

   begin
      if Editor.Debugger_Current_File = VFS.No_File then
         return;
      end if;

      Open_File_Editor
        (Kernel, Editor.Debugger_Current_File,
         Editor.Line, 1, Enable_Navigation => False, New_File => False);

      declare
         Args : GNAT.OS_Lib.Argument_List (1 .. 2) :=
           (1 => new String'(Full_Name (Editor.Debugger_Current_File)),
            2 => new String'(Highlight_Category));
      begin
         Execute_GPS_Shell_Command (Kernel, "unhighlight", Args);

         for A in Args'Range loop
            GNAT.OS_Lib.Free (Args (A));
         end loop;
      end;

      if Editor.Line /= 0 then
         declare
            Args : GNAT.OS_Lib.Argument_List (1 .. 3) :=
              (1 => new String'(Full_Name (Editor.Debugger_Current_File)),
               2 => new String'(Highlight_Category),
               3 => new String'(Editor.Line'Img));
         begin
            Execute_GPS_Shell_Command (Kernel, "highlight", Args);

            for A in Args'Range loop
               GNAT.OS_Lib.Free (Args (A));
            end loop;
         end;
      end if;

      Add_Unique_Sorted
        (Editor.Highlighted_Files, Full_Name (Editor.Debugger_Current_File));
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
      --  Only needed by the GVD explorer, which is disabled within GPS
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

      --  Initialize the color for line highlighting.

      Preferences_Changed (Editor);
   end Initialize;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access GEdit_Record;
      File_Name   : VFS.Virtual_File;
      Set_Current : Boolean := True;
      Force       : Boolean := False)
   is
      pragma Unreferenced (Force);
      Kernel : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;

   begin
      if Editor.Current_File /= File_Name then
         Editor.Current_File := File_Name;

         if Set_Current then
            Editor.Debugger_Current_File := File_Name;
         end if;

         Open_File_Editor
           (Kernel, File_Name, New_File => False,
            Enable_Navigation => False);
      end if;
   end Load_File;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Editor : access GEdit_Record) is
      Kernel : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
      Args   : GNAT.OS_Lib.Argument_List :=
        (1 => new String'(Highlight_Category),
         2 => new String'
           (To_String (Get_Pref (Kernel, Editor_Highlight_Color))));
   begin
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);

      for A in Args'Range loop
         GNAT.OS_Lib.Free (Args (A));
      end loop;
   end Preferences_Changed;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor      : access GEdit_Record;
      Line        : Natural;
      Set_Current : Boolean := True;
      Process     : Glib.Object.GObject)
   is
      Kernel  : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
      Tab     : constant Visual_Debugger := Visual_Debugger (Process);

      Prev_Current_Line : constant Integer := Get_Current_Source_Line (Tab);
      Prev_File       : constant Virtual_File := Get_Current_Source_File (Tab);
   begin
      if Editor.Current_File = VFS.No_File then
         return;
      end if;

      if Set_Current
        and then Prev_Current_Line /= 0
      then
         Add_Line_Information
           (Kernel,
            Prev_File,
            "Current Line",
            --  ??? we should get that from elsewhere.
            new Line_Information_Array'
              (Prev_Current_Line => Line_Information_Record'
                 (Text  => null,
                  Image => Gdk.Pixbuf.Null_Pixbuf,
                  Associated_Command => null)));
      end if;

      Editor.Line := Line;
      Open_File_Editor
        (Kernel, Editor.Current_File, Editor.Line, 1, New_File => False);
      Append (Editor.Highlighted_Files,
              Full_Name (Editor.Debugger_Current_File));

      if Set_Current then
         Add_Line_Information
           (Kernel,
            Editor.Current_File,
            "Current Line",
            --  ??? we should get that from elsewhere.
            new Line_Information_Array'
              (Line => Line_Information_Record'
                (Text  => null,
                 Image => Current_Line_Pixbuf,
                 Associated_Command => null)));

         Set_Current_Source_Location (Tab, Editor.Current_File, Line);
      end if;
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
      Editor.Current_File := VFS.No_File;
      Console.Insert (Kernel, Message);
      --  ??? Do not insert an error, to avoid loosing the focus from the
      --  debugger console. Consider putting a message in the status bar
      --  instead
   end Show_Message;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor  : access GEdit_Record;
      Br      : GVD.Types.Breakpoint_Array;
      Process : Glib.Object.GObject)
   is
      Kernel  : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
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

         if D.Address /= null then
            Result.Address := new String'(D.Address.all);
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

         if D.Info /= null then
            Result.Info := new String'(D.Info.all);
         end if;

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
         if N.File = VFS.No_File then
            Added := False;
            return;
         end if;

         if A = null then
            A := new Breakpoint_Array (1 .. 16);
         end if;

         for J in A'Range loop
            if A (J).Line = N.Line
              and then A (J).File = N.File
            then
               Added := False;
               return;
            end if;

            if First_Zero = -1
              and then A (J).File = VFS.No_File
            then
               First_Zero := J;
            end if;
         end loop;

         Added := True;

         if First_Zero = -1 then
            --  We need to resize A.

            declare
               B : Breakpoint_Array_Ptr := new Breakpoint_Array
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

   begin
      --  Add new breakpoints to the column information.
      for J in Br'Range loop
         Add_Unique_Info (Editor.Current_Breakpoints, Br (J), Result);

         if Result then
            declare
               Other_Command : Set_Breakpoint_Command_Access;
               L             : constant Integer := Br (J).Line;
               A             : Line_Information_Array (L .. L);

            begin
               Create
                 (Other_Command,
                  Kernel,
                  Tab,
                  Unset,
                  Br (J).File,
                  Br (J).Line);
               A (L).Image := Line_Has_Breakpoint_Pixbuf;
               A (L).Associated_Command := Command_Access (Other_Command);
               Add_Line_Information
                 (Kernel,
                  Br (J).File,
                  Breakpoints_Column_Id,
                  new Line_Information_Array'(A));

            end;
         end if;
      end loop;

      --  Remove old breakpoints from the column information.
      if Editor.Current_Breakpoints = null then
         return;
      end if;

      for J in Editor.Current_Breakpoints'Range loop
         if Editor.Current_Breakpoints (J).File /= VFS.No_File then
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
                  A : Line_Information_Array (L .. L);
               begin
                  Create
                    (Other_Command,
                     Kernel,
                     Tab,
                     Set,
                     Editor.Current_Breakpoints (J).File,
                     Editor.Current_Breakpoints (J).Line);

                  if Get_Pref (GVD_Prefs, Editor_Show_Line_With_Code) then
                     A (L).Image := Line_Has_Code_Pixbuf;
                  else
                     A (L).Image := Line_Might_Have_Code_Pixbuf;
                  end if;

                  A (L).Associated_Command := Command_Access (Other_Command);
                  Add_Line_Information
                    (Kernel,
                     Editor.Current_Breakpoints (J).File,
                     Breakpoints_Column_Id,
                     new Line_Information_Array'(A));
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
      Kernel : constant Kernel_Handle := Glide_Window (Editor.Window).Kernel;
   begin
      Free (Editor.Current_Breakpoints);

      --  Clear the line highlight for files that had an highlight.

      while not Is_Empty (Editor.Highlighted_Files) loop
         declare
            Args : GNAT.OS_Lib.Argument_List (1 .. 2) :=
              (1 => new String'(Head (Editor.Highlighted_Files)),
               2 => new String'(Highlight_Category));
         begin
            Execute_GPS_Shell_Command (Kernel, "unhighlight", Args);

            for A in Args'Range loop
               GNAT.OS_Lib.Free (Args (A));
            end loop;
         end;

         Next (Editor.Highlighted_Files);
      end loop;
   end Free_Debug_Info;

end GVD.Text_Box.Source_Editor.Glide;
