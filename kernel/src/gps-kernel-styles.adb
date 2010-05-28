-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2005-2010, AdaCore                 --
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

with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

with Gtk.Widget;             use Gtk.Widget;

with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;

package body GPS.Kernel.Styles is

   use GNAT.Strings;
   use Style_Htable.String_Hash_Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

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
            Bg => Search_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Errors_Style,
            -"Builder results",
            -"Color used to highlight the build errors",
            Bg => Error_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Warnings_Style,
            -"Builder warnings",
            -"Color used to highlight the build warnings",
            Bg => Warning_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Style_Style,
            -"Style errors",
            -"Color used to highlight the style errors",
            Bg => Style_Src_Highlight.Get_Pref, Speedbar => True);

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

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (X : access Style_Htable_Record) is
   begin
      Reset (X.Table);
   end Reset;

   -------------------------
   -- Get_Or_Create_Style --
   -------------------------

   function Get_Or_Create_Style
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Create : Boolean := True) return Style_Access
   is
      Separator : constant Natural := Index (Name, "/");
      Style     : Style_Access;

   begin
      Style := Get (Style_Htable_Access (Kernel.Styles).Table, Name);

      if Style = null
        and then Create
      then
         if Separator /= 0 then
            --  Looking for base style first

            Style :=
              Get_Or_Create_Style
                (Kernel, Name (Name'First .. Separator - 1), False);

            if Style /= null then
               return Get_Or_Create_Style_Copy
                 (Kernel_Handle (Kernel), Name, Style);
            end if;
         end if;

         Style := new Style_Record;
         Style.Name := new String'(Name);
         Set (Style_Htable_Access (Kernel.Styles).Table, Name, Style);
      end if;

      return Style;
   end Get_Or_Create_Style;

   ------------------------------
   -- Get_Or_Create_Style_Copy --
   ------------------------------

   function Get_Or_Create_Style_Copy
     (Kernel     : Kernel_Handle;
      Name       : String;
      From_Style : Style_Access) return Style_Access
   is
      Style : Style_Access :=
        Get (Style_Htable_Access (Kernel.Styles).Table, Name);

   begin
      if Style = null then
         Style := new Style_Record;
         Style.Name := new String'(Name);
         Set_Background (Style, Get_Background (From_Style));
         Set_Foreground (Style, Get_Foreground (From_Style));
         Set_In_Speedbar (Style, In_Speedbar (From_Style));
         Set (Style_Htable_Access (Kernel.Styles).Table, Name, Style);
      end if;

      return Style;
   end Get_Or_Create_Style_Copy;

   ----------
   -- Init --
   ----------

   procedure Init (Kernel : Kernel_Handle) is
   begin
      if Kernel.Styles = null then
         Kernel.Styles := new Style_Htable_Record;
      end if;

      Initialize_Predefined_Styles (Kernel);
   end Init;

end GPS.Kernel.Styles;
