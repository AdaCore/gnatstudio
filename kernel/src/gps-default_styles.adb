------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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
         Icon_Name : String;
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
         Icon_Name : String;
         Fg        : Color_Preference := null;
         Bg        : Color_Preference := null;
         Speedbar  : Boolean := False) is
      begin
         Style := M.Create_From_Preferences
           (Key => Name, Fg_Pref => Fg, Bg_Pref => Bg);
         if Speedbar then
            Set_In_Speedbar (Style, True);
         end if;
         Set_Icon (Style, Icon_Name);
      end Init;

      Entity_To_Pref : constant Entity_To_Pref_Array :=
        (Block_Text             => Blocks_Style,
         Type_Text              => Types_Style,
         Number_Text            => Numbers_Style,
         Keyword_Text           => Keywords_Style,
         Comment_Text           => Comments_Style,
         Annotated_Keyword_Text => Keywords_Style,
         Annotated_Comment_Text => Annotated_Comments_Style,
         Aspect_Keyword_Text    => Keywords_Style,
         Aspect_Comment_Text    => Comments_Style,
         Aspect_Text            => Aspects_Style,
         Character_Text         => Strings_Style,
         String_Text            => Strings_Style);

   begin

      --  Language

      for E in Standout_Language_Entity'Range loop
         Language_Styles (E) := M.Create_From_Preferences
           (Key     => To_Name (E),
            Style   => Default_Style,
            Variant => Entity_To_Pref (E));
      end loop;

      --  Search

      Init (Search_Results_Style,
            -"Search results",
            "",
            Bg => Search_Src_Highlight, Speedbar => True);

      --  Builder

      Init (Builder_Styles (Errors),
            -"Builder results",
            "gps-emblem-build-error",
            Bg => Error_Src_Highlight, Speedbar => True);

      Init (Builder_Styles (Warnings),
            -"Builder warnings",
            "gps-emblem-build-warning",
            Bg => Warning_Src_Highlight, Speedbar => True);

      Init (Builder_Styles (Style),
            -"Style errors",
            "gps-emblem-build-style",
            Bg => Style_Src_Highlight, Speedbar => True);

      Init (Builder_Styles (Info),
            -"Compiler info",
            "gps-emblem-build-info-symbolic",
            Bg => Info_Src_Highlight, Speedbar => True);

      Init (Builder_Background_Style,
            -"Background compilation",
            "",
            Bg => null, Speedbar => True);

   end Initialize_Default_Styles;

end GPS.Default_Styles;
