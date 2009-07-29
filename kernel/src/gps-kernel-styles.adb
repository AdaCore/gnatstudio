-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2005-2009, AdaCore                 --
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

with Ada.Unchecked_Deallocation;

with Gdk;                    use Gdk;
with Gtk.Widget;             use Gtk.Widget;
with Gtk.Window;             use Gtk.Window;

with GPS.Editors;            use GPS.Editors;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Traces;                 use Traces;

with XML_Utils;              use XML_Utils;
with XML_Parsers;

package body GPS.Kernel.Styles is

   Me : constant Debug_Handle := Create ("Styles");

   use GNAT.Strings;
   use Style_Htable.String_Hash_Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Allocate_Color
     (Name : String; Color : out Gdk_Color; GC : out Gdk.Gdk_GC);
   --  Allocates the low-level structures for Color.

   procedure Initialize_Predefined_Styles (Kernel : Kernel_Handle);
   --  Initialize the GPS predefined styles.

   ----------------------------------
   -- Initialize_Predefined_Styles --
   ----------------------------------

   procedure Initialize_Predefined_Styles (Kernel : Kernel_Handle) is
      procedure Init
        (Style : in out Style_Access;
         Name  : String;
         Desc  : String;
         Fg    : String := "";
         Bg    : String := "";
         Speedbar : Boolean := False);
      --  Initialize one style, if it has never been initialized before.
      --  Otherwise, do nothing.

      procedure Init
        (Style : in out Style_Access;
         Name  : String;
         Desc  : String;
         Fg    : String := "";
         Bg    : String := "";
         Speedbar : Boolean := False) is
      begin
         Style := Get_Or_Create_Style (Kernel, Name, False);

         if Style = null then
            Style := Get_Or_Create_Style (Kernel, Name, True);
            Style.Description := new String'(Desc);

            if Fg /= "" then
               Set_Foreground (Style, Fg);
            end if;

            if Bg /= "" then
               Set_Background (Style, Bg);
            end if;

            Set_In_Speedbar (Style, Speedbar);
         end if;
      end Init;
   begin
      --  ??? Should we use the old preferences as reference ?
      Init (Search_Results_Style,
            -"Search results",
            -"Color used to highlight the search results",
            Bg => "light blue", Speedbar => True);

      Init (Builder_Errors_Style,
            -"Builder results",
            -"Color used to highlight the build errors",
            Bg => "red", Speedbar => True);

      Init (Builder_Warnings_Style,
            -"Builder warnings",
            -"Color used to highlight the build warnings",
            Bg => "orange", Speedbar => True);

      Init (Builder_Style_Style,
            -"Style errors",
            -"Color used to highlight the style errors",
            Bg => "yellow", Speedbar => True);

      Init (Builder_Shadow_Style,
            -"Syntax check",
            -"Color used to highlight the build errors in background builds",
            Bg => "light grey", Speedbar => True);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "styles.preferences_changed");
   end Initialize_Predefined_Styles;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Set_Background (Search_Results_Style,   Search_Results_Color.Get_Pref);
      Set_Background (Builder_Errors_Style,   Error_Src_Highlight.Get_Pref);
      Set_Background (Builder_Warnings_Style, Warning_Src_Highlight.Get_Pref);
      Set_Background (Builder_Style_Style,    Style_Src_Highlight.Get_Pref);
   end Preferences_Changed;

   --------------------
   -- Allocate_Color --
   --------------------

   procedure Allocate_Color
     (Name : String; Color : out Gdk_Color; GC : out Gdk.Gdk_GC)
   is
      Success : Boolean;
      Widget  : Gtk_Widget;
      Tops    : Gtk.Widget.Widget_List.Glist;
   begin
      Color := Null_Color;
      GC    := Null_GC;

      if Name = "" then
         Trace (Me, "Color field not filled");
         return;
      end if;

      begin
         Color := Parse (Name);
      exception
         when Wrong_Color =>
            Trace (Me, "Could not parse color " & Name);
            return;
      end;

      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if not Success then
         Trace (Me, "Could not allocate color " & Name);
         return;
      end if;

      Tops := List_Toplevels;
      Widget := Widget_List.Get_Data (Tops);
      Widget_List.Free (Tops);

      if Widget = null
        or else not Realized_Is_Set (Widget)
      then
         Trace (Me, "Cannot create GC: toplevel window not realized");
         return;
      end if;

      Gdk_New (GC, Get_Window (Widget));
      Set_Foreground (GC, Color);
   end Allocate_Color;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (X : access Style_Htable_Record) is
   begin
      Reset (X.Table);
   end Reset;

   -----------------------
   -- Get_Background_GC --
   -----------------------

   function Get_Background_GC
     (Style : not null access Style_Record) return Gdk.GC.Gdk_GC is
   begin
      if Style.Bg_GC = null then
         Allocate_Color (Get_Background (Style), Style.Bg_Color, Style.Bg_GC);
      end if;

      return Style.Bg_GC;
   end Get_Background_GC;

   --------------------------
   -- Get_Background_Color --
   --------------------------

   function Get_Background_Color
     (Style : not null access Style_Record) return Gdk_Color is
   begin
      if Style.Bg_GC = null then
         Allocate_Color (Get_Background (Style), Style.Bg_Color, Style.Bg_GC);
      end if;

      return Style.Bg_Color;
   end Get_Background_Color;

   -----------------
   -- Save_Styles --
   -----------------

   procedure Save_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File)
   is
      Main, Node, Child : Node_Ptr;
      Iter    : Style_Htable.String_Hash_Table.Cursor;
      Info    : Style_Access;
      Success : Boolean;

   begin
      Main := new XML_Utils.Node;
      Main.Tag := new String'("Styles");

      Get_First (Style_Htable_Access (Kernel.Styles).Table, Iter);

      loop
         Info := Get_Element (Iter);
         exit when Info = null;

         Node := new XML_Utils.Node;
         Node.Tag := new String'("style");
         Child := new XML_Utils.Node;
         Child.Tag := new String'("name");
         Child.Value := new String'(Get_Key (Iter));
         Add_Child (Node, Child, True);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("desc");
         if Info.Description /= null then
            Child.Value := new String'(Info.Description.all);
         end if;
         Add_Child (Node, Child, True);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("fg");

         if Get_Foreground (Info) /= "" then
            Child.Value := new String'(Get_Foreground (Info));
         end if;
         Add_Child (Node, Child, True);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("bg");
         if Get_Background (Info) /= "" then
            Child.Value := new String'(Get_Background (Info));
         end if;
         Add_Child (Node, Child, True);

         Add_Child (Main, Node, True);

         Get_Next (Style_Htable_Access (Kernel.Styles).Table, Iter);
      end loop;

      Print (Main, File, Success);

      Free (Main);

      if not Success then
         Report_Preference_File_Error (Kernel, File);
         GPS.Kernel.Console.Insert
           (Kernel,
            "Could not save the configuration file " &
            File.Display_Full_Name &
            ASCII.LF &
            "Please verify that you have write access to this file.",
            Mode => GPS.Kernel.Console.Error);
         Raise_Console (Kernel);
      end if;
   end Save_Styles;

   -----------------
   -- Load_Styles --
   -----------------

   procedure Load_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File)
   is
      F, Node : Node_Ptr;
      Err     : GNAT.Strings.String_Access;

      procedure Read_Style (N : Node_Ptr);
      --  Read one Style from N.

      procedure Read_Style (N : Node_Ptr) is
         Name  : constant String := Get_Field (N, "name").all;
         Style : constant Style_Access :=
           Get_Or_Create_Style (Kernel, Name, True);
      begin
         Free (Style.Description);
         Style.Description := new String'(Get_Field (N, "desc").all);
         Set_Foreground (Style, Get_Field (N, "fg").all);
         Set_Background (Style, Get_Field (N, "bg").all);

      exception
         when E : others =>
            Trace (Exception_Handle, E);
      end Read_Style;

   begin
      if Kernel.Styles = null then
         Kernel.Styles := new Style_Htable_Record;
      end if;

      if Is_Regular_File (File) then
         XML_Parsers.Parse (File, F, Err);

         if F /= null then
            Node := F.Child;

            if F.Tag.all = "Styles" then
               Trace (Me, "Loading styles");

               while Node /= null loop
                  if Node.Tag.all = "style" then
                     Read_Style (Node);
                  end if;

                  Node := Node.Next;
               end loop;
            end if;

         else
            Trace (Me, "Error while parsing styles file " & Err.all);
            GNAT.Strings.Free (Err);
         end if;

         Free (F);
      end if;

      Initialize_Predefined_Styles (Kernel);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Load_Styles;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Style : in out Style_Record) is
   begin
      Free (Style.Name);
      Free (Style.Description);
      Free (Simple_Style_Record (Style));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Style : in out Style_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Style_Record'Class, Style_Access);
   begin
      if Style /= null then
         Free (Style.all);
         Unchecked_Free (Style);
      end if;
   end Free;

   -------------------------
   -- Get_Or_Create_Style --
   -------------------------

   function Get_Or_Create_Style
     (Kernel : Kernel_Handle;
      Name   : String;
      Create : Boolean := True) return Style_Access
   is
      Style : Style_Access;
   begin
      Style := Get (Style_Htable_Access (Kernel.Styles).Table, Name);

      if Style = null
        and then Create
      then
         Style := new Style_Record;
         Style.Name := new String'(Name);
         Set (Style_Htable_Access (Kernel.Styles).Table, Name, Style);
      end if;

      return Style;
   end Get_Or_Create_Style;

   --------------------
   -- Set_Foreground --
   --------------------

   overriding procedure Set_Foreground
     (Style : not null access Style_Record; Color : String) is
   begin
      Set_Foreground (Simple_Style_Record (Style.all)'Access, Color);
      Style.Fg_Color := Null_Color;
      Style.Fg_GC    := Null_GC;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Style : not null access Style_Record; Color : String) is
   begin
      Set_Background (Simple_Style_Record (Style.all)'Access, Color);
      Style.Bg_Color := Null_Color;
      Style.Bg_GC    := Null_GC;
   end Set_Background;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Style : Style_Access) return String is
   begin
      if Style /= null
        and then Style.Name /= null
      then
         return Style.Name.all;
      end if;

      return "";
   end Get_Name;

end GPS.Kernel.Styles;
