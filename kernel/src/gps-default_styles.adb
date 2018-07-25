------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2018, AdaCore                   --
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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GPS.Intl; use GPS.Intl;
with Default_Preferences; use Default_Preferences;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

package body GPS.Default_Styles is

   type Entity_To_Pref_Array is array
     (Standout_Language_Entity) of Variant_Preference;

   -------------------------------
   -- Initialize_Default_Styles --
   -------------------------------

   procedure Initialize_Default_Styles (Kernel : Kernel_Handle) is
      M       : constant Style_Manager_Access := Get_Style_Manager (Kernel);

      procedure Init
        (Style     : out Style_Access;
         Name      : String;
         Icon_Name : String := "";
         Fg        : Color_Preference := null;
         Bg        : Color_Preference := null;
         Speedbar  : Boolean := False);
      --  Factorization function for preference-based styles

      function To_Name (E : Standout_Language_Entity) return String;
      --  Get the name of a style name from a language element

      -------------
      -- To_Name --
      -------------

      function To_Name (E : Standout_Language_Entity) return String is
         S : constant String := To_Lower (E'Img);
      begin
         return S (S'First .. S'Last - 5);
      end To_Name;

      ----------
      -- Init --
      ----------

      procedure Init
        (Style     : out Style_Access;
         Name      : String;
         Icon_Name : String := "";
         Fg        : Color_Preference := null;
         Bg        : Color_Preference := null;
         Speedbar  : Boolean := False) is
      begin
         Style := M.Create_From_Preferences
           (Key => Name, Fg_Pref => Fg, Bg_Pref => Bg);

         if Speedbar then
            Set_In_Speedbar (Style, True);
         end if;

         if Icon_Name /= "" then
            Set_Icon (Style, Icon_Name);
         end if;
      end Init;

      Entity_To_Pref : constant Entity_To_Pref_Array :=
        (Block_Text             => Blocks_Style,
         Type_Text              => Types_Style,
         Number_Text            => Numbers_Style,
         Keyword_Text           => Keywords_Style,
         Comment_Text           => Comments_Style,
         Annotated_Keyword_Text => Keywords_Style,
         Annotated_Comment_Text => Annotated_Comments_Style,
         Aspect_Keyword_Text    => Aspects_Keywords_Style,
         Aspect_Comment_Text    => Aspects_Comments_Style,
         Aspect_Text            => Aspects_Style,
         Character_Text         => Strings_Style,
         String_Text            => Strings_Style);

      Aspect_Styles : array (1 .. 4) of Style_Access;
      pragma Unreferenced (Aspect_Styles);

   begin
      Aspect_Styles (1) := M.Create_From_Preferences
        (Key     => "aspect_block",
         Style   => Default_Style,
         Variant => Aspects_Blocks_Style);

      Aspect_Styles (2) := M.Create_From_Preferences
        (Key     => "aspect_type",
         Style   => Default_Style,
         Variant => Aspects_Types_Style);

      Aspect_Styles (3) := M.Create_From_Preferences
        (Key     => "aspect_string",
         Style   => Default_Style,
         Variant => Aspects_Strings_Style);

      Aspect_Styles (4) := M.Create_From_Preferences
        (Key     => "aspect_number",
         Style   => Default_Style,
         Variant => Aspects_Numbers_Style);

      ------------
      -- Editor --
      ------------

      for E in Standout_Language_Entity'Range loop
         Language_Styles (E) := M.Create_From_Preferences
           (Key     => To_Name (E),
            Style   => Default_Style,
            Variant => Entity_To_Pref (E));
      end loop;

      Editor_Code_Annotations_Style := M.Create_From_Preferences
        (Key     => "Editor code annotations",
         Style   => Default_Style,
         Variant => Code_Annotations_Style);

      Editor_Default_Style := M.Create_From_Preferences
        (Key   => "Editor default",
         Style => Default_Style);

      Editor_Ephemeral_Highlighting_Smart := M.Create_From_Preferences
        (Key     => "Editor ephemeral highlighting smart",
         Style   => Default_Style,
         Variant => Ephemeral_Highlighting_Smart);

      Editor_Ephemeral_Highlighting_Simple := M.Create_From_Preferences
        (Key     => "Editor ephemeral highlighting simple",
         Style   => Default_Style,
         Variant => Ephemeral_Highlighting_Simple);

      Hyper_Links_Default_Style := M.Create_From_Preferences
        (Key     => "Hyper links default style",
         Style   => Default_Style,
         Variant => Hyper_Links_Style);

      Init (Bookmark_Default_Style,
            -"Editor bookmarks",
            Icon_Name => "gps-goto-symbolic",
            Bg        => Bookmark_Color,
            Speedbar  => True);

      ------------
      -- Search --
      ------------

      Init (Search_Results_Style,
            -"Search results",
            "",
            Bg => Search_Src_Highlight, Speedbar => True);

      --------------
      -- Debugger --
      --------------

      Init (Debugger_Breakpoint_Style,
            -"Lines with breakpoints",
            Icon_Name => "gps-emblem-debugger-breakpoint",
            Bg        => Breakpoint_Color,
            Speedbar  => True);
      Init (Debugger_Conditional_Breakpoint_Style,
            -"Lines with conditional breakpoints",
            Icon_Name => "gps-emblem-debugger-conditional-breakpoint",
            Bg        => Conditional_Breakpoint_Color,
            Speedbar  => True);
      Init (Debugger_Disabled_Breakpoint_Style,
            -"Lines with disabled breakpoints",
            Icon_Name => "gps-emblem-debugger-disabled-breakpoint",
            Bg        => Disabled_Breakpoint_Color,
            Speedbar  => True);

      --------------------
      -- Analysis Tools --
      --------------------

      --  Use a transparent style for messages that have their importance
      --  set to Unspecified since it's the default.
      Messages_Styles (Unspecified) := No_Style;

      Init (Messages_Styles (Annotation),
            Name      => -"Annotation messages",
            Bg        => Annotation_Messages_Highlight,
            Speedbar  => True);

      Init (Messages_Styles (Informational),
            Name      => -"Compiler info",
            Bg        => Info_Messages_Highlight,
            Speedbar  => True);

      Init (Messages_Styles (High_Importance),
            Name      => -"Builder results",
            Bg        => High_Messages_Highlight,
            Speedbar  => True);

      Init (Messages_Styles (Medium_Importance),
            Name      => -"Builder warnings",
            Bg        => Medium_Messages_Highlight,
            Speedbar  => True);

      Init (Messages_Styles (Low_Importance),
            Name      => -"Style errors",
            Bg        => Low_Messages_Highlight,
            Speedbar  => True);

   end Initialize_Default_Styles;

end GPS.Default_Styles;
