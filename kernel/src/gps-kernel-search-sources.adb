------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with Interfaces.C.Strings;       use Interfaces.C, Interfaces.C.Strings;
with GNAT.Strings;               use GNAT.Strings;
with GNAT.Heap_Sort;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with String_Utils;               use String_Utils;

with Cairo.Region;               use Cairo.Region;
with Gdk.RGBA;                   use Gdk.RGBA;
with Gdk.Window;                 use Gdk.Window;
with Glib.Object;                use Glib.Object;
with Gtkada.Types;               use Gtkada.Types;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Text_Tag;               use Gtk.Text_Tag;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Widget;                 use Gtk.Widget;
with Pango.Enums;                use Pango.Enums;
with Pango.Font;                 use Pango.Font;

with Basic_Types;                use Basic_Types;
with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Charsets;        use GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup; use GPS.Kernel.Messages.Markup;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Search;                 use GPS.Search;

package body GPS.Kernel.Search.Sources is

   Me : constant Trace_Handle := Create ("SEARCH.SOURCES", Off);

   type Source_Search_Result is new Kernel_Search_Result with record
      File                 : GNATCOLL.VFS.Virtual_File;
      Project              : GNATCOLL.Projects.Project_Type;
      Line, Column         : Natural;
      Line_End, Column_End : Natural;
   end record;
   type Source_Search_Result_Access is access all Source_Search_Result'Class;
   overriding procedure Execute
      (Self       : not null access Source_Search_Result;
       Give_Focus : Boolean);
   overriding function Full
     (Self : not null access Source_Search_Result)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure To_Message
     (Self : not null access Source_Search_Result);
   overriding function Can_Display_In_Locations
     (Self : not null access Source_Search_Result) return Boolean is (True);

   type Result_View is new Gtk_Text_View_Record with record
      Result : Source_Search_Result_Access;
   end record;
   type Result_View_Access is access all Result_View'Class;

   type On_Project_View_Changed is new Simple_Hooks_Function with record
      Provider : access Sources_Search_Provider;
      --  The provider to refresh (do not free)
   end record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   procedure On_Size_Allocate
     (View       : access Gtk_Widget_Record'Class;
      Allocation : Cairo_Rectangle_Int);
   --  Called when the preview widget is resized.

   procedure Sort (X : in out File_And_Project_Array);
   --  Utility function

   ----------
   -- Sort --
   ----------

   procedure Sort (X : in out File_And_Project_Array) is

      procedure Xchg_Procedure (Op1, Op2 : Natural);
      --  Exchange procedure

      function Lt_Function (Op1, Op2 : Natural) return Boolean;
      --  Comparison function

      --------------------
      -- Xchg_Procedure --
      --------------------

      procedure Xchg_Procedure (Op1, Op2 : Natural) is
         D : constant File_And_Project := X (Op1 + X'First - 1);
      begin
         X (Op1 + X'First - 1) := X (Op2 + X'First - 1);
         X (Op2 + X'First - 1) := D;
      end Xchg_Procedure;

      -----------------
      -- Lt_Function --
      -----------------

      function Lt_Function (Op1, Op2 : Natural) return Boolean is

         function "<" (Left, Right : Project_Type) return Boolean;

         function "<" (Left, Right : Project_Type) return Boolean is
         begin
            return Left.Name < Right.Name;
         end "<";

      begin
         if X (Op1 + X'First - 1).Project < X (Op2 + X'First - 1).Project then
            return True;
         end if;

         if X (Op2 + X'First - 1).Project < X (Op1 + X'First - 1).Project then
            return False;
         end if;

         if X (Op1 + X'First - 1).File < X (Op2 + X'First - 1).File then
            return True;
         end if;

         return False;
      end Lt_Function;

   begin
      GNAT.Heap_Sort.Sort
        (X'Last,
         Xchg_Procedure'Unrestricted_Access,
         Lt_Function'Unrestricted_Access);
   end Sort;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
   begin
      Free (Self.Provider.Files);
      Self.Provider.Files :=
        Get_Project (Kernel).Source_Files
           (Recursive => True, Include_Project_Files => True);

      --  In testsuite mode, we want to sort the results so that the matches
      --  do not depend on the filesystem order.
      if Active (Testsuite_Handle) then
         Sort (Self.Provider.Files.all);
      end if;

      Self.Provider.Index := Self.Provider.Files'First;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Sources_Search_Provider) is
   begin
      Free (Self.Files);
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
     (Self    : in out Single_Source_Search_Provider;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type)
   is
      UTF8   : Gtkada.Types.Chars_Ptr;
      Length : Natural;
      Count  : Natural;
      Props  : File_Props;
      pragma Unreferenced (Props);
   begin
      if File /= Self.File
        or else Project /= Self.Project
      then
         Trace (Me, "Examining " & (+File.Full_Name.all));
         Free (Self.Text);
         Self.File    := File;
         Self.Project := Project;

         if File /= No_File then

            --  ??? This requires a lot of copies of the file text, but
            --  unfortunately the conversion-to-utf8 routines are written in C.

            Read_File_With_Charset
              (Self.File,
               UTF8     => UTF8,
               UTF8_Len => Length,
               Props    => Props);

            Self.Text := new String (1 .. Length);

            if Length > 0 then
               To_Ada
                 (Value (UTF8, size_t (Length)), Self.Text.all, Count, False);
            end if;

            Free (UTF8);
         end if;
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
      --  If Self.Pattern has been allocated by the provider itself, free
      --  it before allocating a new pattern.
      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      --  Set Self.Pattern to Approximate if Pattern.Kind = Fuzzy
      Self.Pattern := Pattern.Build_If_Needed
        (Kind     => Fuzzy,
         New_Kind => Approximate,
         Built    => Self.Pattern_Needs_Free);

      Self.Set_File (Self.File, Self.Project);  --  reset search
   end Set_Pattern;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Sources_Search_Provider;
      Pattern : not null access Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      Hook : access On_Project_View_Changed;
   begin
      if Self.Files = null then
         --  The first time the provider is used, we connect to the
         --  appropriate hooks so that we refresh the cached list of
         --  source and runtime files whenever the project is recomputed.

         Hook := new On_Project_View_Changed;
         Hook.Provider := Self;
         Project_View_Changed_Hook.Add (Hook);
         Hook.Execute (Self.Kernel);
      end if;

      case Pattern.Get_Kind is
         when Full_Text | Regexp =>
            Self.Pattern := Search_Pattern_Access (Pattern);
            Self.Pattern_Needs_Free := False;
         when Fuzzy | Approximate =>
            Self.Pattern := Pattern.Build (Kind => Approximate);
            Self.Pattern_Needs_Free := True;
      end case;

      Self.Index := Self.Files'First;
      if Self.Files'Length > 0 then
         Self.Current.Set_File
           (Self.Files (Self.Index).File,
            Self.Files (Self.Index).Project);
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
      L : GNAT.Strings.String_Access;
   begin
      Result   := null;
      Has_Next := False;

      if Self.Text = null
        or else (not Self.Restart and then Self.Context = GPS.Search.No_Match)
      then
         return;
      end if;

      if Self.Restart then
         Self.Restart := False;
         Self.Context := Self.Pattern.Start (Self.Text.all);
      else
         Self.Pattern.Next (Self.Text.all, Self.Context);
      end if;

      if Self.Context = GPS.Search.No_Match then
         return;
      end if;

      declare
         Matched_Line : constant String :=
           (if not Is_Empty_Match (Self.Context) then
                 Get_Surrounding_Line (Self.Text.all,
              Self.Context.Start.Index,
              Self.Context.Finish.Index)
            else
               Get_Surrounding_Line (Self.Text.all,
              Self.Context.Start.Index,
              Self.Context.Start.Index));

         P_Name       : constant String :=
           (if Self.Project = No_Project
            or else not Get_Registry
              (Self.Kernel).Tree.Root_Project.Is_Aggregate_Project
            then ""
            else ASCII.LF
            & "(" & Self.Project.Project_Path.Display_Base_Name & " -- "
            & (+Self.Project.Project_Path.Dir_Name) & ')');
      begin
         L := new String'
           (Path_And_Name (Self.Kernel, Self.File, Self.Project)
            & ":" & Image (Self.Context.Start.Line, Min_Width => 0)
            & ":"
            & Image (Integer (Self.Context.Start.Column), Min_Width => 0)
            & P_Name);

         Result   := new Source_Search_Result'
           (Kernel     => Self.Kernel,
            Provider   => Self,
            Score      => Self.Context.Score,
            Short      => new String'
              (Self.Pattern.Highlight_Match
                   (Matched_Line, Self.Context)),
            Long       => L,
            Id         => L,
            File       => Self.File,
            Project    => Self.Project,
            Line       => Self.Context.Start.Line,
            Column     => Integer (Self.Context.Start.Column),
            Line_End   => Self.Context.Finish.Line,
            Column_End => Integer (Self.Context.Finish.Column));
         Self.Adjust_Score (Result);
         Has_Next := True;
      end;
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

      if Self.Pattern.Get_Text = ""
        or else Self.Index > Self.Files'Last
      then
         Has_Next := False;
         return;
      end if;

      Has_Next := True;

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

      Self.Current.Set_File
        (Self.Files (Self.Index).File,
         Self.Files (Self.Index).Project);
   end Next;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self       : not null access Source_Search_Result;
       Give_Focus : Boolean) is
   begin
      if Self.File /= No_File then
         Open_File_Action_Hook.Run
           (Self.Kernel,
            File              => Self.File,
            Project           => Self.Project,
            Enable_Navigation => True,
            New_File          => False,
            Focus             => Give_Focus,
            Line              => Self.Line,
            Column            => Visible_Column (Self.Column));
      end if;
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

      UTF8   : Gtkada.Types.Chars_Ptr;
      Length : Natural;
      Count  : Natural;
      Props  : File_Props;
      pragma Unreferenced (Props);
   begin
      Read_File_With_Charset
        (Self.File,
         UTF8     => UTF8,
         UTF8_Len => Length,
         Props    => Props);

      Tmp := new String (1 .. Length);
      To_Ada (Value (UTF8, size_t (Length)), Tmp.all, Count, False);
      Free (UTF8);

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

         --  If match is empty string we have nothing to tag
         if Self.Line_End /= 0 then

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
         end if;

         View.On_Size_Allocate (On_Size_Allocate'Access, After => False);

         return Gtk.Widget.Gtk_Widget (View);
      end if;
   end Full;

   ----------------
   -- To_Message --
   ----------------

   overriding procedure To_Message
     (Self : not null access Source_Search_Result)
   is
      Msg : Markup_Message_Access;
      pragma Unreferenced (Msg);
   begin
      Msg := GPS.Kernel.Messages.Markup.Create_Markup_Message
        (Container                => Get_Messages_Container (Self.Kernel),
         Category                 => Self.Provider.Display_Name,
         File                     => Self.File,
         Line                     => Self.Line,
         Column                   => Visible_Column_Type (Self.Column),
         Text                     => Self.Short.all,
         Weight                   => 1,
         Flags                    => Side_And_Locations,
         Allow_Auto_Jump_To_First => True);
   end To_Message;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Current_File_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      --  Get the current editor
      Editor : constant Editor_Buffer'Class :=
         Self.Kernel.Get_Buffer_Factory.Get
            (File => No_File, Open_View => False);
   begin
      Single_Source_Search_Provider (Self.all).Set_Pattern
         (Pattern, Limit);  --  inherited
      Self.Set_File
        (Editor.File,
         Project => Get_Registry
           (Self.Kernel).Tree.Info (Editor.File).Project);
   end Set_Pattern;

end GPS.Kernel.Search.Sources;
