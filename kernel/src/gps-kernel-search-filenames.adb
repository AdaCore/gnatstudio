------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Interfaces.C;              use Interfaces.C;

with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Mmap;             use GNATCOLL.Mmap;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib.Convert;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Label;                 use Gtk.Label;
with Gtkada.Entry_Completion;   use Gtkada.Entry_Completion;
with Gtkada.Types;              use Gtkada.Types;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Charsets;       use GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Search;                use GPS.Search;
with Histories;                 use Histories;
with String_Utils;              use String_Utils;

package body GPS.Kernel.Search.Filenames is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.SEARCH_FILENAMES");

   Key_Search_Other_Files : constant History_Key :=
     "omni-search-include-all-from-source-dirs";
   --  whether to include files found in the source dirs and that are not
   --  sources of the project.

   Bytes_To_Preview : constant := 10000;
   --  Number of bytes to load in a preview

   type On_Project_View_Changed is new Simple_Hooks_Function with record
      Provider : access Filenames_Search_Provider;
      --  The provider to refresh (do not free)
   end record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   procedure Check_Pattern
     (Self     : not null access Filenames_Search_Provider'Class;
      Has_Next : out Boolean;
      Callback : not null access function
        (Text    : String;
         Context : Search_Context;
         File    : Virtual_File;
         Project : Project_Type) return Boolean);
   --  Search for the next possible match. When a match is found, calls
   --  Callback. Stops iterating when the callback returns False.

   procedure Set_Step
     (Self : not null access Filenames_Search_Provider'Class;
      Step : Search_Step);
   --  Set and initialize the current search step

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Free (Self.Provider.Files);
      Unchecked_Free (Self.Provider.Runtime);
      Unchecked_Free (Self.Provider.Source_Dirs);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Filenames_Search_Provider) is
   begin
      Free (Self.Files);
      Unchecked_Free (Self.Runtime);
      Unchecked_Free (Self.Source_Dirs);

      if Self.Data.Step = Other_Files then
         Unchecked_Free (Self.Data.Files_In_Dir);
      end if;

      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access Filenames_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return -("Search amongst the source files of the project or the run time"
         & " files of the compiler." & ASCII.LF
         & "The following syntax is supported to open a file at a specific"
         & " location:" & ASCII.LF
         & " <b>filename:line:column</b>" & ASCII.LF
         & "where the line and column are optional." & ASCII.LF
         & "Possible completions are found by testing the filename pattern"
         & " with the base names of the source files, unless filename"
         & " contains a '/' or '\', in which case the full name of the"
         & " source file is used.");
   end Documentation;

   --------------
   -- Set_Step --
   --------------

   procedure Set_Step
     (Self : not null access Filenames_Search_Provider'Class;
      Step : Search_Step)
   is
   begin
      if Self.Data.Step = Other_Files then
         Unchecked_Free (Self.Data.Files_In_Dir);
      end if;

      case Step is
         when User_File =>
            Self.Data :=
              (Step => User_File);
         when Project_Sources =>
            Trace (Me, "Will parse name of project sources");
            if Self.Files = null then
               Self.Files :=
                 Get_Project (Self.Kernel).Source_Files (Recursive => True);
            end if;
            Self.Data :=
              (Step   => Project_Sources,
               Index  => Self.Files'First - 1);
         when Runtime_Sources =>
            Trace (Me, "Will parse name of runtime sources");
            if Self.Runtime = null then
               Self.Runtime := new File_Array'
                 (Get_Registry (Self.Kernel).Environment
                  .Predefined_Source_Files);
            end if;
            Self.Data :=
              (Step          => Runtime_Sources,
               Runtime_Index => Self.Runtime'First - 1);
         when Project_Files =>
            Trace (Me, "Will parse name of project files");
            Self.Data :=
              (Step => Project_Files,
               Iter => Get_Project (Self.Kernel).Start (Recursive => True));
         when Other_Files =>
            Trace (Me, "Will parse name of files in source_dirs");

            Create_New_Boolean_Key_If_Necessary
              (Get_History (Self.Kernel).all,
               Key_Search_Other_Files,
               Default_Value => True);

            if Get_History
              (Get_History (Self.Kernel).all, Key_Search_Other_Files)
            then
               if Self.Source_Dirs = null then
                  Self.Source_Dirs       := new File_Array'
                    (Get_Project (Self.Kernel).Source_Dirs
                     (Recursive => True));
               end if;

               Self.Data :=
                 (Step         => Other_Files,
                  Files_In_Dir => null,
                  Dirs_Index   => Self.Source_Dirs'First,
                  File_Index   => 0);

               if Self.Source_Dirs'Length /= 0 then
                  Self.Data.Files_In_Dir :=
                    Self.Source_Dirs (Self.Source_Dirs'First).Read_Dir;
                  Self.Data.File_Index   := Self.Data.Files_In_Dir'First - 1;
               end if;

            else
               Self.Data :=
                 (Step         => Other_Files,
                  Files_In_Dir => null,
                  Dirs_Index   => Natural'Last,
                  File_Index   => Natural'Last);
            end if;
      end case;
   end Set_Step;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Filenames_Search_Provider;
      Pattern : not null access Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
      Text : constant String := Pattern.Get_Text;
      P    : constant Pattern_Matcher := Compile (":(\d+)?(:(\d+))?$");
      M    : Match_Array (0 .. 3);
      Hook : Simple_Hooks_Function_Access;

   begin
      if Self.Files = null then
         --  The first time the provider is used, we connect to the
         --  appropriate hooks so that we refresh the cached list of
         --  source and runtime files whenever the project is recomputed.

         Hook :=
           new On_Project_View_Changed'(Hook_Function with Provider => Self);
         Project_View_Changed_Hook.Add (Hook);
         Hook.Execute (Self.Kernel);
      end if;

      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Pattern_Needs_Free := False;
      Self.Match_Directory :=
         Ada.Strings.Fixed.Index (Text, "/") >= Text'First or else
         Ada.Strings.Fixed.Index (Text, "\") >= Text'First;

      Self.Searched_Count := 0;
      Self.Seen.Clear;
      Set_Step (Self, User_File);

      --  Search for "filename:line:column" pattern
      Match (P, Text, M);

      if M (1) /= GNAT.Regpat.No_Match then
         Self.Line := Natural'Value (Text (M (1).First .. M (1).Last));
      end if;

      if M (3) /= GNAT.Regpat.No_Match then
         Self.Column := Natural'Value (Text (M (3).First .. M (3).Last));
      end if;

      if M (0) /= GNAT.Regpat.No_Match and then M (0).First /= Text'First then
         Self.Pattern := Build
           (Self.Pattern, Text (Text'First .. M (0).First - 1));
         Self.Pattern_Needs_Free := True;
      end if;
   end Set_Pattern;

   -------------------
   -- Check_Pattern --
   -------------------

   procedure Check_Pattern
     (Self     : not null access Filenames_Search_Provider'Class;
      Has_Next : out Boolean;
      Callback : not null access function
        (Text    : String;
         Context : Search_Context;
         File    : Virtual_File;
         Project : Project_Type) return Boolean)
   is
      Continue : Boolean := True;

      procedure Check (F : Virtual_File; Project : Project_Type);
      --  Sets Result to non-null if F matches

      -----------
      -- Check --
      -----------

      procedure Check (F : Virtual_File; Project : Project_Type) is
         Text : constant String :=
            (if Self.Match_Directory then +F.Full_Name.all else +F.Base_Name);
         C : Search_Context;
      begin
         --  As a special case, we systematically omit .o files, in case the
         --  object_dir is part of the source_dirs. Such files will make the
         --  overview very slow, and are of little interest in editors...
         --
         --  We do not want to return the same file twice, unless we are using
         --  aggregate projects and this is a duplication in the project itself

         if F.File_Extension /= ".o"

           --  We don't want to show directories as entries in the results
           and then not F.Is_Directory

           and then
             (Self.Data.Step = Project_Sources
              or else not Self.Seen.Contains (F))
         then
            C := Self.Pattern.Start (Text);
            if C /= GPS.Search.No_Match then
               Continue := Callback (Text, C, F, Project);
               Self.Seen.Include (F);
            end if;
         end if;
      end Check;

      F : Virtual_File;
      Prj : Project_Type;
      Start : constant Ada.Calendar.Time := Clock;
   begin
      Has_Next := True;

      For_Each_Step :
      loop
         case Self.Data.Step is
         when User_File =>
            Set_Step (Self, Search_Step'Succ (Self.Data.Step));

            --  Does the text entered by the user match an existing file ?
            F := GNATCOLL.VFS.Create_From_Base (+Self.Pattern.Get_Text);
            if F.Is_Regular_File then
               Self.Seen.Include (F);  --  avoid duplicates
               Continue := Callback
                 (Text    => +F.Base_Name,
                  Context => GPS.Search.No_Match,
                  File    => F,
                  Project => No_Project);

               exit For_Each_Step when not Continue;
            end if;

         when Project_Sources =>
            while Self.Data.Index < Self.Files'Last loop
               exit For_Each_Step when Clock - Start >
                 Gtkada.Entry_Completion.Max_Idle_Duration;
               Self.Data.Index := Self.Data.Index + 1;
               Self.Searched_Count := Self.Searched_Count + 1;
               Check (Self.Files (Self.Data.Index).File,
                      Self.Files (Self.Data.Index).Project);
               exit For_Each_Step when not Continue;
            end loop;
            Set_Step (Self, Search_Step'Succ (Self.Data.Step));

         when Runtime_Sources =>
            while Self.Data.Runtime_Index < Self.Runtime'Last loop
               exit For_Each_Step when Clock - Start >
                 Gtkada.Entry_Completion.Max_Idle_Duration;
               Self.Data.Runtime_Index := Self.Data.Runtime_Index + 1;
               Self.Searched_Count := Self.Searched_Count + 1;
               Check (Self.Runtime (Self.Data.Runtime_Index), No_Project);
               exit For_Each_Step when not Continue;
            end loop;
            Set_Step (Self, Search_Step'Succ (Self.Data.Step));

         when Project_Files =>
            loop
               Prj := Current (Self.Data.Iter);
               exit when Prj = No_Project;
               Check (Prj.Project_Path, Prj);
               Next (Self.Data.Iter);
               Self.Searched_Count := Self.Searched_Count + 1;
               exit For_Each_Step when not Continue;
            end loop;
            Set_Step (Self, Search_Step'Succ (Self.Data.Step));

         when Other_Files =>
            --  Test all files in the current directory
            while Self.Data.Files_In_Dir /= null
              and then Self.Data.File_Index < Self.Data.Files_In_Dir'Last
            loop
               exit For_Each_Step when Clock - Start >
                 Gtkada.Entry_Completion.Max_Idle_Duration;
               Self.Data.File_Index := Self.Data.File_Index + 1;
               Self.Searched_Count := Self.Searched_Count + 1;
               Check (Self.Data.Files_In_Dir (Self.Data.File_Index),
                      No_Project);
               exit For_Each_Step when not Continue;
            end loop;

            --  Move on to the next diretory

            if Self.Source_Dirs /= null
              and then Self.Data.Dirs_Index < Self.Source_Dirs'Last
            then
               Self.Data.Dirs_Index := Self.Data.Dirs_Index + 1;
               Unchecked_Free (Self.Data.Files_In_Dir);

               begin
                  Self.Data.Files_In_Dir :=
                    Self.Source_Dirs (Self.Data.Dirs_Index).Read_Dir;
                  Self.Data.File_Index := Self.Data.Files_In_Dir'First - 1;
               exception
                  when GNATCOLL.VFS.VFS_Directory_Error =>
                     Trace (Me, "Skipping invalid source dir: "
                        & Self.Source_Dirs
                           (Self.Data.Dirs_Index).Display_Full_Name);
               end;

               --  Will do the actual testing at the next iteration
            else
               Has_Next := False;
               exit For_Each_Step;
            end if;
         end case;
      end loop For_Each_Step;
   end Check_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Filenames_Search_Provider;
      Result   : out Search_Result_Access;
      Has_Next : out Boolean)
   is
      Is_Aggregate : constant Boolean :=
        Get_Registry (Self.Kernel).Tree.Root_Project.Is_Aggregate_Project;

      function Highlight_Runtime (Str : String) return String;
      --  For runtime files, highlight them specially

      function Callback
        (Text    : String;
         Context : Search_Context;
         File    : Virtual_File;
         Project : Project_Type) return Boolean;

      -----------------------
      -- Highlight_Runtime --
      -----------------------

      function Highlight_Runtime (Str : String) return String is
      begin
         if Self.Pattern.Get_Allow_Highlights then
            return (if Self.Data.Step = Runtime_Sources
                    then "<i>" & Str & "</i>" else Str);
         else
            return Str;
         end if;
      end Highlight_Runtime;

      --------------
      -- Callback --
      --------------

      function Callback
        (Text    : String;
         Context : Search_Context;
         File    : Virtual_File;
         Project : Project_Type) return Boolean
      is
         L : GNAT.Strings.String_Access;
         P_Name : constant String :=
           (if Project = No_Project or else not Is_Aggregate
            then ""
            else ASCII.LF
            & "(" & Project.Project_Path.Display_Base_Name & " -- "
            & (+Project.Project_Path.Dir_Name) & ')');
      begin
         L := new String'
           (Path_And_Name (Self.Kernel, File, Project) & P_Name);

         if Context = GPS.Search.No_Match then
            Result := new Filenames_Search_Result'
              (Kernel   => Self.Kernel,
               Provider => Self,
               Score    => 100 * 100,
               Short    => new String'(+File.Base_Name),
               Long     => L,
               Id       => L,
               Line     => Self.Line,
               Column   => Self.Column,
               Project  => Project,
               File     => File);

         elsif Self.Match_Directory then
            Result := new Filenames_Search_Result'
              (Kernel   => Self.Kernel,
               Provider => Self,
               Score    => Context.Score,
               Short    => new String'
                 (Highlight_Runtime (+File.Base_Name)),
               Long     => new String'
                 (Self.Pattern.Highlight_Match
                      (Buffer => Text, Context => Context) & P_Name),
               Id       => L,
               Line     => Self.Line,
               Column   => Self.Column,
               Project  => Project,
               File     => File);
         else
            Result := new Filenames_Search_Result'
              (Kernel   => Self.Kernel,
               Provider => Self,
               Score    => Context.Score,
               Short    => new String'
                 (Highlight_Runtime
                      (Self.Pattern.Highlight_Match
                           (Buffer => Text, Context => Context))),
               Long     => L,
               Id       => L,
               Line     => Self.Line,
               Column   => Self.Column,
               Project  => Project,
               File     => File);
         end if;

         --  Lower the score for runtime files, so that the source files
         --  always appear first. "10" is so that in fuzzy matching this
         --  corresponds to having characters separated by 9 others.

         if Self.Data.Step = Runtime_Sources then
            Result.Score := Result.Score - 10;
         end if;

         --  Give priority to shorter items (which means the pattern matched
         --  a bigger portion of it). This way, "buffer" matches
         --  "src_editor_buffer.adb" before "src_editor_buffer-hooks.adb".

         Result.Score := 100 * Result.Score - File.Base_Name'Length;
         Self.Adjust_Score (Result);

         return False;  --  return that result and wait till next call to Next
                        --  to keep looking
      end Callback;

   begin
      Result := null;

      --  We do not want this search provider to be active when there is no
      --  pattern to search for.
      if Self.Pattern.Get_Text = "" then
         Has_Next := False;
         return;
      end if;

      Check_Pattern (Self, Has_Next, Callback'Access);
   end Next;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Filenames_Search_Result;
      Give_Focus : Boolean) is
   begin
      Open_File_Action_Hook.Run
        (Self.Kernel, Self.File,
         Project           => Self.Project,
         Enable_Navigation => True,
         New_File          => False,
         Focus             => Give_Focus,
         Line              => Self.Line,
         Column            =>
           Basic_Types.Visible_Column_Type (Self.Column));
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self : not null access Filenames_Search_Result)
     return Gtk.Widget.Gtk_Widget
   is
      Tmp   : GNAT.Strings.String_Access;
      Label : Gtk_Label;
      UTF8   : Gtkada.Types.Chars_Ptr;
      Count  : Natural;
      Props  : File_Props;
      pragma Unreferenced (Props);
   begin
      --  Only display a preview when the file has a known language. This
      --  should filter out binary files (executables, images,...) which will
      --  either be very slow to load and display, or will simply crash gtk
      --  and GPS

      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Self.Kernel).Tree.Info_Set (Self.File)
              .First_Element);
      begin
         if F_Info.Language = "" then
            return null;
         end if;
      end;

      --  The call to this function is blocking, we do not want to stay
      --  here too long, so do not try to preview a non-local file.

      if not Self.File.Is_Local then
         Gtk_New (Label, -"File not available locally.");
         Label.Modify_Font (View_Fixed_Font.Get_Pref);
         return Gtk.Widget.Gtk_Widget (Label);
      end if;

      --  Similarly, we do not want to do the work of reading, converting,
      --  entering in a label, a file which is too big: preview the first N
      --  bytes only.

      declare
         File   : Mapped_File := Open_Read (+Self.File.Full_Name.all);
         Region : Mapped_Region;
         R      : GNAT.Strings.String_Access;
         L      : Integer;
         Ignored_1, Ignored_2, Ignored_3 : Boolean;
         Ignore        : aliased Natural;
         Length        : aliased Natural;
      begin
         Read (File, Region, Offset => 0, Length => Bytes_To_Preview);
         Close (File);

         if Region /= Invalid_Mapped_Region then
            L := Last (Region);
            R := new String (1 .. L);
            R.all := String (Data (Region).all (1 .. L));
            Free (Region);

            Strip_CR_And_NUL (R.all, L, Ignored_1, Ignored_2, Ignored_3);

            UTF8 := Glib.Convert.Convert
              (R (R'First .. L), "UTF-8", Get_File_Charset (Self.File),
               Ignore'Unchecked_Access, Length'Unchecked_Access);

            Free (R);
         end if;

         if UTF8 /= Gtkada.Types.Null_Ptr then
            Tmp := new String (1 .. Length);
            To_Ada (Gtkada.Types.Value (UTF8, size_t (Length)),
                    Tmp.all, Count, False);
            Gtkada.Types.g_free (UTF8);
         end if;
      end;

      if Tmp = null then
         return null;

      else
         Gtk_New (Label, Tmp.all);
         Label.Modify_Font (View_Fixed_Font.Get_Pref);
         GNAT.Strings.Free (Tmp);
         return Gtk.Widget.Gtk_Widget (Label);
      end if;
   end Full;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Filenames_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      Suffix : Unbounded_String;
      Suffix_Last : Natural := 0;

      function Callback
        (Text    : String;
         Context : Search_Context;
         File    : Virtual_File;
         Project : Project_Type) return Boolean;

      function Callback
        (Text    : String;
         Context : Search_Context;
         File    : Virtual_File;
         Project : Project_Type) return Boolean
      is
         pragma Unreferenced (File, Project);
      begin
         Self.Pattern.Compute_Suffix (Context, Text, Suffix, Suffix_Last);
         return True;  --  keep looking
      end Callback;

      Has_Next : Boolean;
   begin
      Self.Set_Pattern (Pattern);

      loop
         Check_Pattern (Self, Has_Next, Callback'Access);
         exit when not Has_Next or else Suffix_Last = 0;
      end loop;

      return Slice (Suffix, 1, Suffix_Last);
   end Complete_Suffix;

   -------------------
   -- Edit_Settings --
   -------------------

   overriding procedure Edit_Settings
     (Self : not null access Filenames_Search_Provider;
      Box  : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback)
   is
      Include : Gtk_Check_Button;
   begin
      Gtk_New (Include, -"Include all files from source dirs");
      Include.Set_Tooltip_Text
        (-("Whether to check the file names for all files in source"
         & " directories and not just actual sources of the project."));
      Box.Pack_Start (Include, Expand => False);
      Associate (Get_History (Self.Kernel).all,
                 Key_Search_Other_Files,
                 Include,
                 Default => True);
      Include.On_Toggled (Gtk.Toggle_Button.Cb_GObject_Void (On_Change), Data);
   end Edit_Settings;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Self : not null access Filenames_Search_Provider) return Integer
   is
      function Count_Source_Files return Integer;

      function Count_Project_Files return Integer;

      ------------------------
      -- Count_Source_Files --
      ------------------------

      function Count_Source_Files return Integer is
         Count : Integer := 0;
      begin
         if not Get_History
           (Get_History (Self.Kernel).all, Key_Search_Other_Files)
         then
            return 0;
         end if;

         if Self.Source_Dirs /= null then
            for Source_Dir of Self.Source_Dirs.all loop
               declare
                  Files_In_Dir : File_Array_Access := Source_Dir.Read_Dir;
               begin
                  Count := Count + Files_In_Dir'Length;
                  Unchecked_Free (Files_In_Dir);
               end;
            end loop;
         end if;

         return Count;
      end Count_Source_Files;

      -------------------------
      -- Count_Project_Files --
      -------------------------

      function Count_Project_Files return Integer is
         Iter  : GNATCOLL.Projects.Project_Iterator;
         Count : Integer := 0;
      begin
         Iter := Get_Project (Self.Kernel).Start (Recursive => True);

         while Current (Iter) /= No_Project loop
            Count := Count + 1;
            Next (Iter);
         end loop;

         return Count;
      end Count_Project_Files;

   begin
      if Self.Total_Count > -1 then
         return Self.Total_Count;
      end if;

      --  Retrieve all the filenames available for the currently loaded project

      if Self.Files = null then
         Self.Files :=
           Get_Project (Self.Kernel).Source_Files (Recursive => True);
      end if;

      if Self.Runtime = null then
         Self.Runtime := new File_Array'
           (Get_Registry (Self.Kernel).Environment
            .Predefined_Source_Files);
      end if;

      if Self.Source_Dirs = null then
         Self.Source_Dirs := new File_Array'
           (Get_Project (Self.Kernel).Source_Dirs
            (Recursive => True));
      end if;

      Self.Total_Count := Self.Files'Length
        + Self.Runtime'Length
        + Count_Project_Files
        + Count_Source_Files;

      return Self.Total_Count;
   end Get_Total_Progress;

end GPS.Kernel.Search.Filenames;
