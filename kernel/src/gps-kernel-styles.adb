------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

with Gtk.Widget;             use Gtk.Widget;

with Default_Preferences;    use Default_Preferences;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

package body GPS.Kernel.Styles is

   use GNAT.Strings;
   use Style_Htable.String_Hash_Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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
         Icon_Name : String;
         Fg    : String := "";
         Bg    : String := "";
         Speedbar : Boolean := False);
      --  Initialize one style, if it has never been initialized before.
      --  Otherwise, do nothing.

      procedure Init
        (Style : in out Style_Access;
         Name  : String;
         Desc  : String;
         Icon_Name : String;
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

         if Icon_Name /= "" then
            Set_Editor_Icon_Name (Style, Icon_Name);
         end if;
      end Init;
   begin
      --  ??? Should we use the old preferences as reference ?
      Init (Search_Results_Style,
            -"Search results",
            -"Color used to highlight the search results",
            "",
            Bg => Search_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Styles (Errors),
            -"Builder results",
            -"Color used to highlight the build errors",
            "gps-emblem-build-error",
            Bg => Error_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Styles (Warnings),
            -"Builder warnings",
            -"Color used to highlight the build warnings",
            "gps-emblem-build-warning",
            Bg => Warning_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Styles (Style),
            -"Style errors",
            -"Color used to highlight the style errors",
            "gps-emblem-build-style",
            Bg => Style_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Styles (Info),
            -"Compiler info",
            -"Color used to highlight the compiler information",
            "gps-emblem-build-info-symbolic",
            Bg => Info_Src_Highlight.Get_Pref, Speedbar => True);

      Init (Builder_Background_Style,
            -"Background compilation",
            -"Color used to highlight the build errors in background builds",
            "",
            Bg => "", Speedbar => True);

      Add_Hook (Kernel, Preference_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "styles.preferences_changed");
   end Initialize_Predefined_Styles;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      P : constant Preference := Get_Pref (Data);
   begin
      if P = Preference (Search_Results_Color) then
         Set_Background (Search_Results_Style, Search_Results_Color.Get_Pref);
      elsif P = Preference (Error_Src_Highlight) then
         Set_Background
           (Builder_Styles (Errors),
            Error_Src_Highlight.Get_Pref);
      elsif P = Preference (Warning_Src_Highlight) then
         Set_Background
           (Builder_Styles (Warnings),
            Warning_Src_Highlight.Get_Pref);
      elsif P = Preference (Style_Src_Highlight) then
         Set_Background
           (Builder_Styles (Style),
            Style_Src_Highlight.Get_Pref);
      elsif P = Preference (Info_Src_Highlight) then
         Set_Background
           (Builder_Styles (Info),
            Info_Src_Highlight.Get_Pref);
      end if;
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
