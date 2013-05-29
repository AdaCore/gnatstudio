------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Gtk.Label;                 use Gtk.Label;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with GPS.Search;                use GPS.Search;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;

package body GPS.Kernel.Search.Filenames is

   type Hook_Project_View_Changed is new Function_No_Args with record
      Provider : access Filenames_Search_Provider;
      --  The provider to refresh (do not free).
   end record;
   overriding procedure Execute
      (Hook : Hook_Project_View_Changed;
       Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Hook : Hook_Project_View_Changed;
       Kernel : access Kernel_Handle_Record'Class) is
   begin
      Unchecked_Free (Hook.Provider.Files);
      Unchecked_Free (Hook.Provider.Runtime);
      Hook.Provider.Files :=
         Get_Project (Kernel).Source_Files (Recursive => True);
      Hook.Provider.Runtime := new File_Array'
         (Get_Registry (Kernel).Environment.Predefined_Source_Files);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Filenames_Search_Provider) is
   begin
      Unchecked_Free (Self.Files);
      Unchecked_Free (Self.Runtime);

      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;
   end Free;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self    : not null access Filenames_Search_Provider) return String
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
      Hook : access Hook_Project_View_Changed;
   begin
      if Self.Files = null then
         --  The first time the provider is used, we connect to the
         --  appropriate hooks so that we refresh the cached list of
         --  source and runtime files whenever the project is recomputed.

         Hook := new Hook_Project_View_Changed'
            (Function_No_Args with Provider => Self);
         Add_Hook
            (Self.Kernel, Project_View_Changed_Hook, Hook,
             "gps-kernel-search-filenames.on_project_view_changed");
         Hook.Execute (Self.Kernel);
      end if;

      Self.Index := Self.Files'First - 2;
      Self.Runtime_Index := Self.Runtime'First - 1;
      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Pattern_Needs_Free := False;
      Self.Match_Directory :=
         Ada.Strings.Fixed.Index (Text, "/") >= Text'First or else
         Ada.Strings.Fixed.Index (Text, "\") >= Text'First;

      Self.Seen.Clear;

      --  Search for "filename:line:column" pattern
      Match (P, Text, M);

      if M (1) /= GNAT.Regpat.No_Match then
         Self.Line := Natural'Value (Text (M (1).First .. M (1).Last));
      end if;

      if M (3) /= GNAT.Regpat.No_Match then
         Self.Column := Natural'Value (Text (M (3).First .. M (3).Last));
      end if;

      if M (0) /= GNAT.Regpat.No_Match then
         Self.Pattern := Build
            (Self.Pattern, Text (Text'First .. M (0).First - 1));
         Self.Pattern_Needs_Free := True;
      end if;
   end Set_Pattern;

   ----------------------------
   -- Build_Filenames_Result --
   ----------------------------

   function Build_Filenames_Result
      (Provider : not null access Filenames_Search_Provider'Class;
       File   : GNATCOLL.VFS.Virtual_File;
       Line, Column : Natural := 0;
       Score  : Natural := 100;
       Short  : String := "";
       Long   : String := "")
      return GPS.Search.Search_Result_Access
   is
      L : constant GNAT.Strings.String_Access := new String'
         ((if Long = "" then File.Display_Full_Name else Long));
      S : GNAT.Strings.String_Access;
   begin
      if Short = "" then
         S := new String'(+File.Base_Name);
      else
         S := new String'(Short);
      end if;

      return new Filenames_Search_Result'
        (Kernel   => Provider.Kernel,
         Provider => Provider,
         Score  => Score,
         Short  => S,
         Long   => L,
         Id     => L,
         Line   => Line,
         Column => Column,
         File   => File);
   end Build_Filenames_Result;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Filenames_Search_Provider;
      Result   : out Search_Result_Access;
      Has_Next : out Boolean)
   is
      procedure Check (F : Virtual_File; Runtime : Boolean);
      --  Sets Result to non-null if F matches

      procedure Check (F : Virtual_File; Runtime : Boolean) is
         Text : constant String :=
            (if Self.Match_Directory then +F.Full_Name.all else +F.Base_Name);
         C : constant Search_Context := Self.Pattern.Start (Text);
      begin
         if C /= GPS.Search.No_Match then
            Has_Next := Self.Runtime_Index < Self.Runtime'Last;

            if Self.Seen.Contains (F) then
               Result := null;
               return;
            end if;

            if Self.Match_Directory then
               Result := Build_Filenames_Result
                  (Self, F, Line => Self.Line,
                   Column => Self.Column, Score => C.Score,
                   Long => Self.Pattern.Highlight_Match
                      (Buffer => Text, Context => C));
            else
               Result := Build_Filenames_Result
                  (Self, F, Line => Self.Line,
                   Column => Self.Column, Score => C.Score,
                   Short => Self.Pattern.Highlight_Match
                      (Buffer => Text, Context => C));
            end if;

            if Result /= null then
               --  Lower the score for runtime files, so that the source files
               --  always appear first.

               if Runtime then
                  Result.Score := Result.Score - 1;
               end if;

               Self.Seen.Include (F);
            end if;
         end if;
      end Check;

      F : Virtual_File;
   begin
      Result := null;

      if Self.Index = Self.Files'First - 2 then
         Self.Index := Self.Index + 1;

         --  Does the text entered by the user match an existing file ?
         F := GNATCOLL.VFS.Create_From_Base (+Self.Pattern.Get_Text);
         if F.Is_Regular_File then
            Self.Seen.Include (F);  --  avoid duplicates
            Result := Build_Filenames_Result
               (Self, F, Line => Self.Line,
                Column => Self.Column, Score => 200);
            Has_Next := True;
            return;
         end if;
      end if;

      while Self.Index < Self.Files'Last loop
         Self.Index := Self.Index + 1;
         Check (Self.Files (Self.Index), Runtime => False);
         if Result /= null then
            return;
         end if;
      end loop;

      while Self.Runtime_Index < Self.Runtime'Last loop
         Self.Runtime_Index := Self.Runtime_Index + 1;
         Check (Self.Runtime (Self.Runtime_Index), Runtime => True);
         if Result /= null then
            return;
         end if;
      end loop;

      Has_Next := False;
   end Next;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Filenames_Search_Result;
      Give_Focus : Boolean) is
   begin
      Open_File_Editor
        (Self.Kernel, Self.File,
         Enable_Navigation => True,
         New_File          => False,
         Focus             => Give_Focus,
         Line              => Self.Line,
         Column            => Visible_Column (Self.Column));
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self : not null access Filenames_Search_Result)
     return Gtk.Widget.Gtk_Widget
   is
      Tmp : GNAT.Strings.String_Access;
      Label : Gtk_Label;
   begin
      Tmp := Self.File.Read_File;
      if Tmp = null then
         return null;
      else
         Gtk_New (Label, Tmp.all);
         Label.Modify_Font (View_Fixed_Font.Get_Pref);
         GNAT.Strings.Free (Tmp);
         return Gtk.Widget.Gtk_Widget (Label);
      end if;
   end Full;

end GPS.Kernel.Search.Filenames;
