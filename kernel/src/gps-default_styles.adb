------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2015-2023, AdaCore                   --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GPS.Intl;                  use GPS.Intl;
with Default_Preferences;       use Default_Preferences;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;

package body GPS.Default_Styles is

   type Entity_To_Pref_Array is array
     (Standout_Language_Entity) of Variant_Preference;

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed to update "synthetic"
   --  preferences that based on another

   type LSP_Styles_Data is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Pref : Variant_Preference;
   end record;

   All_Styles : array (1 .. 18) of LSP_Styles_Data;

   LSP_Namespace_Style_Pos     : constant := 1;
   Types_Style_Pos             : constant := 2;
   LSP_Class_Style_Pos         : constant := 3;
   LSP_Enum_Style_Pos          : constant := 4;
   LSP_Interface_Style_Pos     : constant := 5;
   LSP_Struct_Style_Pos        : constant := 6;
   LSP_TypeParameter_Style_Pos : constant := 7;
   LSP_Parameter_Style_Pos     : constant := 8;
   LSP_Variable_Style_Pos      : constant := 9;
   LSP_Property_Style_Pos      : constant := 10;
   LSP_EnumMember_Style_Pos    : constant := 11;
   LSP_Function_Style_Pos      : constant := 12;
   Keywords_Style_Pos          : constant := 13;
   LSP_Modifier_Style_Pos      : constant := 14;
   Comments_Style_Pos          : constant := 15;
   Strings_Style_Pos           : constant := 16;
   Numbers_Style_Pos           : constant := 17;
   LSP_Operator_Style_Pos      : constant := 18;

   LSP_Styles : array (1 .. 14) of LSP_Styles_Data;

   type LSP_Synthetic_Pref_Array is array (All_Styles'Range) of Style_Access;

   LSP_Semantic_Readonly_Styles       : LSP_Synthetic_Pref_Array;
   LSP_Semantic_Abstract_Styles       : LSP_Synthetic_Pref_Array;
   LSP_Semantic_Declaration_Styles    : LSP_Synthetic_Pref_Array;
   LSP_Semantic_Definition_Styles     : LSP_Synthetic_Pref_Array;
   LSP_Semantic_Static_Styles         : LSP_Synthetic_Pref_Array;
   LSP_Semantic_Documentation_Styles  : LSP_Synthetic_Pref_Array;
   LSP_Semantic_Defaultlibrary_Styles : LSP_Synthetic_Pref_Array;

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

      LSP_Semantic_Styles : array (LSP_Styles'Range) of Style_Access;
      pragma Unreferenced (LSP_Semantic_Styles);

   begin
      All_Styles :=
        ((To_Unbounded_String ("namespace"),     LSP_Namespace_Style),
         (To_Unbounded_String ("type"),          Types_Style),
         (To_Unbounded_String ("class"),         LSP_Class_Style),
         (To_Unbounded_String ("enum"),          LSP_Enum_Style),
         (To_Unbounded_String ("interface"),     LSP_Interface_Style),
         (To_Unbounded_String ("struct"),        LSP_Struct_Style),
         (To_Unbounded_String ("typeparameter"), LSP_TypeParameter_Style),
         (To_Unbounded_String ("parameter"),     LSP_Parameter_Style),
         (To_Unbounded_String ("variable"),      LSP_Variable_Style),
         (To_Unbounded_String ("property"),      LSP_Property_Style),
         (To_Unbounded_String ("enummember"),    LSP_EnumMember_Style),
         (To_Unbounded_String ("function"),      LSP_Function_Style),
         (To_Unbounded_String ("keyword"),       Keywords_Style),
         (To_Unbounded_String ("modifier"),      LSP_Modifier_Style),
         (To_Unbounded_String ("comment"),       Comments_Style),
         (To_Unbounded_String ("string"),        Strings_Style),
         (To_Unbounded_String ("number"),        Numbers_Style),
         (To_Unbounded_String ("operator"),      LSP_Operator_Style));

      LSP_Styles :=
        ((To_Unbounded_String ("namespace"),     LSP_Namespace_Style),
         (To_Unbounded_String ("class"),         LSP_Class_Style),
         (To_Unbounded_String ("enum"),          LSP_Enum_Style),
         (To_Unbounded_String ("interface"),     LSP_Interface_Style),
         (To_Unbounded_String ("struct"),        LSP_Struct_Style),
         (To_Unbounded_String ("typeparameter"), LSP_TypeParameter_Style),
         (To_Unbounded_String ("parameter"),     LSP_Parameter_Style),
         (To_Unbounded_String ("variable"),      LSP_Variable_Style),
         (To_Unbounded_String ("property"),      LSP_Property_Style),
         (To_Unbounded_String ("enummember"),    LSP_EnumMember_Style),
         (To_Unbounded_String ("function"),      LSP_Function_Style),
         (To_Unbounded_String ("modifier"),      LSP_Modifier_Style),
         (To_Unbounded_String ("operator"),      LSP_Operator_Style),
         (To_Unbounded_String ("deprecated"),    LSP_Deprecated_Style));

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
      Set_In_Speedbar (Editor_Ephemeral_Highlighting_Smart, True);

      Editor_Ephemeral_Highlighting_Simple := M.Create_From_Preferences
        (Key     => "Editor ephemeral highlighting simple",
         Style   => Default_Style,
         Variant => Ephemeral_Highlighting_Simple);
      Set_In_Speedbar (Editor_Ephemeral_Highlighting_Simple, True);

      Hyper_Links_Default_Style := M.Create_From_Preferences
        (Key     => "Hyper links default style",
         Style   => Default_Style,
         Variant => Hyper_Links_Style);

      Init (Bookmark_Default_Style,
            -"Editor bookmarks",
            Icon_Name => "gps-goto-symbolic",
            Bg        => Bookmark_Color,
            Speedbar  => True);

      ---------
      -- LSP --
      ---------

      for Index in LSP_Styles'Range loop
         LSP_Semantic_Styles (Index) := M.Create_From_Preferences
           (Key     => To_String (LSP_Styles (Index).Name),
            Style   => Default_Style,
            Variant => LSP_Styles (Index).Pref);
      end loop;

      --  *-declaration
      for Index in All_Styles'Range loop
         LSP_Semantic_Declaration_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-declaration",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Declaration_Styles (Index).Set_Variant (Italic);
      end loop;

      --  *-definition
      for Index in All_Styles'Range loop
         LSP_Semantic_Definition_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-definition",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Definition_Styles (Index).Set_Variant (Bold);
      end loop;

      --  *-readonly
      for Index in All_Styles'Range loop
         LSP_Semantic_Readonly_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-readonly",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Readonly_Styles (Index).Set_Background
           (LSP_Readonly_Bg.Get_Pref);
      end loop;

      --  *-abstract
      for Index in All_Styles'Range loop
         LSP_Semantic_Abstract_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-abstract",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Abstract_Styles (Index).Set_Variant (Italic);
      end loop;

      --  *-static
      for Index in All_Styles'Range loop
         LSP_Semantic_Static_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-static",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Static_Styles (Index).Set_Variant (Bold_Italic);
      end loop;

      --  *-documentation
      for Index in All_Styles'Range loop
         LSP_Semantic_Documentation_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-documentation",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Documentation_Styles (Index).Set_Variant (Bold);
      end loop;

      --  *-defaultlibrary
      for Index in All_Styles'Range loop
         LSP_Semantic_Defaultlibrary_Styles (Index) := M.Create_From_Style
           (Key     => To_String (All_Styles (Index).Name) & "-defaultlibrary",
            Style   => To_String (All_Styles (Index).Name),
            Shade_Or_Lighten_Amount => 0.0);
         LSP_Semantic_Defaultlibrary_Styles (Index).Set_Variant (Bold_Italic);
      end loop;

      ------------
      -- Search --
      ------------

      Init (Search_Results_Style,
            -"Search results",
            "",
            Bg => Search_Src_Highlight, Speedbar => True);

      --------------------
      -- Error Messages --
      --------------------

      Init (Error_Msg_Style,
            -"Error messages",
            "",
            Fg       => Message_Highlight,
            Speedbar => True);

      --------------
      -- Debugger --
      --------------

      Init (Debugger_Current_Line_Style,
            -"Debugger current line",
            Bg       => Debugger_Current_Line_Color,
            Speedbar => True);
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

      Init (Messages_Styles (High),
            Name      => -"Builder results",
            Bg        => High_Messages_Highlight,
            Speedbar  => True);

      Init (Messages_Styles (Medium),
            Name      => -"Builder warnings",
            Bg        => Medium_Messages_Highlight,
            Speedbar  => True);

      Init (Messages_Styles (Low),
            Name      => -"Style errors",
            Bg        => Low_Messages_Highlight,
            Speedbar  => True);

      Preferences_Changed_Hook.Add
        (Obj => new On_Pref_Changed);
   end Initialize_Default_Styles;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      M     : constant Style_Manager_Access := Get_Style_Manager
        (Kernel_Handle (Kernel));
      Style : Style_Access;

      procedure Update_Prefs (Arr : LSP_Synthetic_Pref_Array);
      procedure Update_Prefs (Index : Positive);
      --  Update preferences based on the changed one

      ------------------
      -- Update_Prefs --
      ------------------

      procedure Update_Prefs (Index : Positive) is
         procedure Update (Arr : LSP_Synthetic_Pref_Array);

         ------------
         -- Update --
         ------------

         procedure Update (Arr : LSP_Synthetic_Pref_Array) is
         begin
            Arr (Index).Set_Foreground (Style.Foreground);
            Arr (Index).Set_Background (Style.Background);
         end Update;

      begin
         Style := M.Get (Style_Key (To_String (All_Styles (Index).Name)));

         --  *-declaration
         Update (LSP_Semantic_Declaration_Styles);

         --  *-definition
         Update (LSP_Semantic_Definition_Styles);

         --  *-abstract
         Update (LSP_Semantic_Abstract_Styles);

         --  *-static
         Update (LSP_Semantic_Static_Styles);

         --  *-documentation
         Update (LSP_Semantic_Documentation_Styles);

         --  *-defaultlibrary
         Update (LSP_Semantic_Defaultlibrary_Styles);

         --  *-readonly
         LSP_Semantic_Readonly_Styles (Index).Set_Foreground
           (Style.Foreground);
         LSP_Semantic_Readonly_Styles (Index).Set_Background
           (LSP_Readonly_Bg.Get_Pref);
      end Update_Prefs;

      ------------------
      -- Update_Prefs --
      ------------------

      procedure Update_Prefs (Arr : LSP_Synthetic_Pref_Array) is
      begin
         for Index in All_Styles'Range loop
            Style := M.Get (Style_Key (To_String (All_Styles (Index).Name)));
            Arr (Index).Set_Foreground (Style.Foreground);
            Arr (Index).Set_Background (Style.Background);
         end loop;
      end Update_Prefs;

   begin
      if Pref = null then
         --  *-declaration
         Update_Prefs (LSP_Semantic_Declaration_Styles);

         --  *-definition
         Update_Prefs (LSP_Semantic_Definition_Styles);

         --  *-abstract
         Update_Prefs (LSP_Semantic_Abstract_Styles);

         --  *-static
         Update_Prefs (LSP_Semantic_Static_Styles);

         --  *-documentation
         Update_Prefs (LSP_Semantic_Documentation_Styles);

         --  *-defaultlibrary
         Update_Prefs (LSP_Semantic_Defaultlibrary_Styles);

         --  *-readonly
         for Index in All_Styles'Range loop
            Style := M.Get (Style_Key (To_String (All_Styles (Index).Name)));
            LSP_Semantic_Readonly_Styles (Index).Set_Foreground
              (Style.Foreground);
            LSP_Semantic_Readonly_Styles (Index).Set_Background
              (LSP_Readonly_Bg.Get_Pref);
         end loop;

      else
         if Pref = Preference (LSP_Namespace_Style) then
            Update_Prefs (LSP_Namespace_Style_Pos);
         end if;

         if Pref = Preference (Types_Style) then
            Update_Prefs (Types_Style_Pos);
         end if;

         if Pref = Preference (LSP_Class_Style) then
            Update_Prefs (LSP_Class_Style_Pos);
         end if;

         if Pref = Preference (LSP_Enum_Style) then
            Update_Prefs (LSP_Enum_Style_Pos);
         end if;

         if Pref = Preference (LSP_Interface_Style) then
            Update_Prefs (LSP_Interface_Style_Pos);
         end if;

         if Pref = Preference (LSP_Struct_Style) then
            Update_Prefs (LSP_Struct_Style_Pos);
         end if;

         if Pref = Preference (LSP_TypeParameter_Style) then
            Update_Prefs (LSP_TypeParameter_Style_Pos);
         end if;

         if Pref = Preference (LSP_Parameter_Style) then
            Update_Prefs (LSP_Parameter_Style_Pos);
         end if;

         if Pref = Preference (LSP_Variable_Style) then
            Update_Prefs (LSP_Variable_Style_Pos);
         end if;

         if Pref = Preference (LSP_Property_Style) then
            Update_Prefs (LSP_Property_Style_Pos);
         end if;

         if Pref = Preference (LSP_EnumMember_Style) then
            Update_Prefs (LSP_EnumMember_Style_Pos);
         end if;

         if Pref = Preference (LSP_Function_Style) then
            Update_Prefs (LSP_Function_Style_Pos);
         end if;

         if Pref = Preference (Keywords_Style) then
            Update_Prefs (Keywords_Style_Pos);
         end if;

         if Pref = Preference (LSP_Modifier_Style) then
            Update_Prefs (LSP_Modifier_Style_Pos);
         end if;

         if Pref = Preference (Comments_Style) then
            Update_Prefs (Comments_Style_Pos);
         end if;

         if Pref = Preference (Strings_Style) then
            Update_Prefs (Strings_Style_Pos);
         end if;

         if Pref = Preference (Numbers_Style) then
            Update_Prefs (Numbers_Style_Pos);
         end if;

         if Pref = Preference (LSP_Operator_Style) then
            Update_Prefs (LSP_Operator_Style_Pos);
         end if;

         if Pref = Preference (LSP_Readonly_Bg) then
            --  *-readonly
            for Index in All_Styles'Range loop
               LSP_Semantic_Readonly_Styles (Index).Set_Background
                 (LSP_Readonly_Bg.Get_Pref);
            end loop;
         end if;
      end if;
   end Execute;

end GPS.Default_Styles;
