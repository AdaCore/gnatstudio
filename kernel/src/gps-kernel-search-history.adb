------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2013-2021, AdaCore                     --
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

with Interfaces.C;               use Interfaces.C;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Utils;

with Cairo.Region;               use Cairo.Region;
with Gdk.Window;                 use Gdk.Window;
with Glib.Object;                use Glib.Object;
with Gtkada.Types;               use Gtkada.Types;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Text_Tag;               use Gtk.Text_Tag;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Widget;                 use Gtk.Widget;

with Basic_Types;                use Basic_Types;
with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Charsets;        use GPS.Kernel.Charsets;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Search;                 use GPS.Search;

package body GPS.Kernel.Search.History is

   type History_Search_Result_Access is access all History_Search_Result'Class;

   type Result_View is new Gtk_Text_View_Record with record
      Result : History_Search_Result_Access;
   end record;
   type Result_View_Access is access all Result_View'Class;

   procedure On_Size_Allocate
     (View       : access Gtk_Widget_Record'Class;
      Allocation : Cairo_Rectangle_Int);

   Vector : History_Vectors.Vector;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out History_Search_Provider) is
   begin
      Free (Self.Pattern);
      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access History_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return -("Shows files that were already opened for a given pattern or" &
                 " whose names match the pattern");
   end Documentation;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access History_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      Free (Self.Pattern);
      Self.Pattern := Pattern.Build (Kind => Pattern.Get_Kind);
      Self.Current := Vector.First;
      Self.Searched_Count := 0;
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access History_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Context : Search_Context;
      H       : History;
      L       : GNAT.Strings.String_Access;
      Similar : Boolean := False;
   begin
      Result   := null;
      Has_Next := False;

      if Self.Pattern = null
        or else Self.Pattern.Get_Text = ""
        or else not Has_Element (Self.Current)
      then
         return;
      end if;

      H := Element (Self.Current);

      --  Show a file if its name is matched by the pattern
      Context := Self.Pattern.Start (+H.File.Base_Name);

      if Context = GPS.Search.No_Match then
         --  Show a file if the pattern that was present when the file was
         --  selected is similar to the current pattern
         Similar := GNATCOLL.Utils.Starts_With
           (To_String (H.Pattern), Self.Pattern.Get_Text);
      end if;

      if Context /= GPS.Search.No_Match
        or else Similar
      then
         declare
            P_Name : constant String :=
              (if H.Project = No_Project
               then ""
               else ASCII.LF
               & "(" & H.Project.Project_Path.Display_Base_Name & " -- "
               & (+H.Project.Project_Path.Dir_Name) & ')');
         begin
            L := new String'
              (Path_And_Name (Self.Kernel, H.File, H.Project) & P_Name);

            Result := new History_Search_Result'
              (Kernel   => Self.Kernel,
               Provider => Self,
               Score    => (if Similar then 100 else Context.Score),
               Short    => new String'
                 (if Similar
                  then To_String (H.Pattern)
                     else Self.Pattern.Highlight_Match
                    (Buffer => (+H.File.Base_Name),
                     Context => Context)),
               Long     => L,
               Id       => L,
               Line     => H.Line,
               Column   => H.Column,
               Project  => H.Project,
               File     => H.File);
         end;
      end if;

      Self.Searched_Count := Self.Searched_Count + 1;
      Next (Self.Current);
      Has_Next := Has_Element (Self.Current);
   end Next;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Self : not null access History_Search_Provider) return Integer is
   begin
      if Is_Empty (Vector) then
         return -1;
      else
         return Integer (Length (Vector));
      end if;
   end Get_Total_Progress;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self       : not null access History_Search_Result;
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
            Column            =>
              Basic_Types.Visible_Column_Type (Self.Column));
      end if;
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self : not null access History_Search_Result)
     return Gtk.Widget.Gtk_Widget
   is
      Tmp    : GNAT.Strings.String_Access;
      View   : Result_View_Access;
      Buffer : Gtk_Text_Buffer;
      First  : Gtk_Text_Iter;

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
      g_free (UTF8);

      if Count <= 0 then
         GNAT.Strings.Free (Tmp);
         return null;
      else
         Gtk_New (Buffer);
         View := new Result_View;
         View.Result := History_Search_Result_Access (Self);
         Initialize (View, Buffer);

         Unref (Buffer);

         View.Set_Editable (False);
         View.Set_Wrap_Mode (Wrap_None);
         View.Modify_Font (Default_Style.Get_Pref_Font);

         Buffer.Get_End_Iter (First);
         Buffer.Insert (First, Tmp (1 .. Count));
         GNAT.Strings.Free (Tmp);

         View.On_Size_Allocate (On_Size_Allocate'Access, After => False);

         return Gtk.Widget.Gtk_Widget (View);
      end if;
   end Full;

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

   -------------------------
   -- Add_File_To_History --
   -------------------------

   procedure Add_File_To_History
     (Pattern : String;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Natural := 0;
      Column  : Natural := 0)
   is
      H : History;
      C : History_Vectors.Cursor;
   begin
      if Pattern /= ""
        and then File /= No_File
      then
         --  Delete history for the file, if any
         C := Vector.First;
         while Has_Element (C) loop
            if Element (C).File = File then
               Vector.Delete (C);
               exit;
            end if;
            C := Next (C);
         end loop;

         --  Insert the file into history
         H := (To_Unbounded_String (Pattern),
               File, Project, Line, Column);
         Vector.Prepend (H);
      end if;
   end Add_File_To_History;

end GPS.Kernel.Search.History;
