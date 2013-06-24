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

with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Cairo.Region;              use Cairo.Region;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Window;                use Gdk.Window;
with Glib.Object;               use Glib.Object;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Tag;              use Gtk.Text_Tag;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Enums;               use Pango.Enums;
with Pango.Font;                use Pango.Font;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Search;                use GPS.Search;

package body GPS.Kernel.Search.Sources is

   type Source_Search_Result is new Kernel_Search_Result with record
      File : GNATCOLL.VFS.Virtual_File;
      Line, Column : Natural;
      Line_End, Column_End : Natural;
   end record;
   type Source_Search_Result_Access is access all Source_Search_Result'Class;
   overriding procedure Execute
      (Self       : not null access Source_Search_Result;
       Give_Focus : Boolean);
   overriding function Full
     (Self : not null access Source_Search_Result)
      return Gtk.Widget.Gtk_Widget;

   type Result_View is new Gtk_Text_View_Record with record
      Result : Source_Search_Result_Access;
   end record;
   type Result_View_Access is access all Result_View'Class;

   type Hook_Project_View_Changed is new Function_No_Args with record
      Provider : access Sources_Search_Provider;
      --  The provider to refresh (do not free)
   end record;
   overriding procedure Execute
     (Hook : Hook_Project_View_Changed;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   procedure On_Size_Allocate
     (View       : access Gtk_Widget_Record'Class;
      Allocation : Cairo_Rectangle_Int);
   --  Called when the preview widget is resized.

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook : Hook_Project_View_Changed;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      Unchecked_Free (Hook.Provider.Files);
      Hook.Provider.Files :=
         Get_Project (Kernel).Source_Files (Recursive => True);
      Hook.Provider.Index := Hook.Provider.Files'First;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Sources_Search_Provider) is
   begin
      Unchecked_Free (Self.Files);
      Free (Self.Current);

      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Single_Source_Search_Provider) is
   begin
      Free (Self.Text);

      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access Sources_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return -("Search in the contents of all source files of the projects");
   end Documentation;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self    : not null access Single_Source_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return -("Search for references in a specific file");
   end Documentation;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Self : in out Single_Source_Search_Provider;
      File : GNATCOLL.VFS.Virtual_File)
   is
   begin
      if File /= Self.File then
         Free (Self.Text);
         Self.File    := File;
         Self.Text    := Self.File.Read_File;
      end if;

      Self.Restart := True;
      Self.Context := GPS.Search.No_Match;
   end Set_File;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Single_Source_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      case Pattern.Get_Kind is
         when Full_Text | Regexp =>
            Self.Pattern := Search_Pattern_Access (Pattern);
            Self.Pattern_Needs_Free := False;
         when Fuzzy =>
            --  Fuzzy does not make sense in sources, would match too much
            Self.Pattern := Pattern.Build (Kind => Full_Text);
            Self.Pattern_Needs_Free := True;
      end case;

      Self.Set_File (Self.File);  --  reset search
   end Set_Pattern;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Sources_Search_Provider;
      Pattern : not null access Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
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
             "gps-kernel-search-sources.on_project_view_changed");
         Hook.Execute (Self.Kernel);
      end if;

      case Pattern.Get_Kind is
         when Full_Text | Regexp =>
            Self.Pattern := Search_Pattern_Access (Pattern);
            Self.Pattern_Needs_Free := False;
         when Fuzzy =>
            --  Fuzzy does not make sense in sources, would match too much
            Self.Pattern := Pattern.Build (Kind => Full_Text);
            Self.Pattern_Needs_Free := True;
      end case;

      Self.Index := Self.Files'First;
      if Self.Files'Length > 0 then
         Self.Current.Set_File (Self.Files (Self.Index));
      end if;

      Self.Current.Kernel := Self.Kernel;
      Self.Current.Set_Pattern (Self.Pattern, Limit);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Single_Source_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Start, Finish : Integer;
      L : String_Access;
   begin
      Result := null;

      if Self.Text = null
        or else (not Self.Restart
                 and then Self.Context = GPS.Search.No_Match)
      then
         Has_Next := True;
         return;
      end if;

      if Self.Restart then
         Self.Restart := False;
         Self.Context := Self.Pattern.Start (Self.Text.all);
      else
         Self.Pattern.Next (Self.Text.all, Self.Context);
      end if;

      if Self.Context = GPS.Search.No_Match then
         Has_Next := False;
      else
         --  Find beginning of the line, but ignore leading spaces
         Start := Self.Context.Start;
         while Start >= Self.Text'First
           and then Self.Text (Start) /= ASCII.LF
         loop
            Start := Start - 1;
         end loop;

         Start := Start + 1;
         while Start <= Self.Context.Start
           and then (Self.Text (Start) = ' '
                     or else Self.Text (Start) = ASCII.HT)
         loop
            Start := Start + 1;
         end loop;

         Finish := Self.Context.Finish;
         while Finish <= Self.Text'Last
           and then Self.Text (Finish) /= ASCII.LF
         loop
            Finish := Finish + 1;
         end loop;

         L := new String'
              (Self.File.Display_Full_Name
               & ":" & Image (Self.Context.Line_Start, Min_Width => 0)
               & ":"
               & Image (Integer (Self.Context.Col_Start), Min_Width => 0));

         Result := new Source_Search_Result'
           (Kernel     => Self.Kernel,
            Provider   => Self,
            Score      => Self.Context.Score,
            Short      => new String'
              (Self.Pattern.Highlight_Match
                 (Self.Text (Start .. Finish - 1), Self.Context)),
            Long       => L,
            Id         => L,
            File       => Self.File,
            Line       => Self.Context.Line_Start,
            Column     => Integer (Self.Context.Col_Start),
            Line_End   => Self.Context.Line_End,
            Column_End => Integer (Self.Context.Col_End));
         Self.Adjust_Score (Result);
         Has_Next := True;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Sources_Search_Provider;
      Result   : out Search_Result_Access;
      Has_Next : out Boolean)
   is
      Current_Has_Next : Boolean;
   begin
      Result := null;

      if Self.Index > Self.Files'Last then
         Has_Next := False;
         return;
      else
         Has_Next := True;
      end if;

      Self.Current.Next (Result, Current_Has_Next);
      if Result /= null then
         Result.Provider := Self;

         --  ??? Could update the score with Self.Index, to group all entries
         --  from the same file

         return;
      end if;

      --  No more in current file, let's move to next file

      Self.Index := Self.Index + 1;
      if Self.Index > Self.Files'Last then
         Has_Next := False;
         return;
      end if;

      Self.Current.Set_File (Self.Files (Self.Index));
      Has_Next := True;
   end Next;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self       : not null access Source_Search_Result;
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

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
     (View       : access Gtk_Widget_Record'Class;
      Allocation : Cairo_Rectangle_Int)
   is
      pragma Unreferenced (Allocation);
      V : constant Result_View_Access := Result_View_Access (View);
      Buffer : constant Gtk_Text_Buffer := V.Get_Buffer;
      First : Gtk_Text_Iter;
   begin
      Buffer.Get_Iter_At_Line_Offset
        (First, Gint (V.Result.Line - 1), Gint (V.Result.Column - 1));

      V.Scroll_To_Mark
        (Buffer.Create_Mark (Where => First),
         Within_Margin => 0.0,
         Use_Align     => True,
         Xalign        => 1.0,
         Yalign        => 0.5);
   end On_Size_Allocate;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self : not null access Source_Search_Result)
     return Gtk.Widget.Gtk_Widget
   is
      Tmp    : GNAT.Strings.String_Access;
      View   : Result_View_Access;
      Buffer : Gtk_Text_Buffer;
      Tag    : Gtk_Text_Tag;
      First, Last : Gtk_Text_Iter;
   begin
      Tmp := Self.File.Read_File;

      if Tmp = null then
         return null;
      else
         Gtk_New (Buffer);
         View := new Result_View;
         View.Result := Source_Search_Result_Access (Self);
         Initialize (View, Buffer);

         Unref (Buffer);

         View.Set_Editable (False);
         View.Set_Wrap_Mode (Wrap_None);
         View.Modify_Font (Default_Style.Get_Pref_Font);

         --  ??? Need to convert to UTF8
         Buffer.Get_End_Iter (First);
         Buffer.Insert (First, Tmp.all);
         GNAT.Strings.Free (Tmp);

         Buffer.Get_Iter_At_Line_Offset
           (First, Gint (Self.Line - 1), Gint (Self.Column - 1));
         Buffer.Get_Iter_At_Line_Offset
           (Last, Gint (Self.Line_End - 1), Gint (Self.Column_End - 1));

         Tag := Buffer.Create_Tag;
         Set_Property
           (Tag, Gtk.Text_Tag.Font_Desc_Property,
            Keywords_Style.Get_Pref_Font);
         Set_Property
           (Tag, Gtk.Text_Tag.Foreground_Rgba_Property,
            Keywords_Style.Get_Pref_Fg);
         Set_Property
           (Tag, Gtk.Text_Tag.Background_Rgba_Property,
            Keywords_Style.Get_Pref_Bg);
         Set_Property
           (Tag, Gtk.Text_Tag.Underline_Property, Pango_Underline_Single);

         Buffer.Apply_Tag (Tag, First, Last);

         View.On_Size_Allocate (On_Size_Allocate'Access, After => False);

         return Gtk.Widget.Gtk_Widget (View);
      end if;
   end Full;

end GPS.Kernel.Search.Sources;
