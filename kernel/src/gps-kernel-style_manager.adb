------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

with Ada.Unchecked_Conversion;
with Glib.Object; use Glib.Object;
with Pango.Enums;               use Pango.Enums;
with Pango.Font;                use Pango.Font;
with Gtkada.Style; use Gtkada.Style;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body GPS.Kernel.Style_Manager is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.Style_Manager");

   use Style_Map;

   function To_Style_Manager_Access is new Ada.Unchecked_Conversion
     (System.Address, Style_Manager_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Style_Record, Style_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Source'Class, Source_Access);

   procedure Refresh_Values (V : Style_Access);
   --  Refresh the values of V from its source.

   procedure Free (X : in out Style_Record);
   --  Free memory associated to X

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Manager : Style_Manager_Access;
   end record;
   overriding procedure Execute
     (Hook   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   type Tag_And_Variant is record
      Style   : Style_Access;
      Tag     : Gtk_Text_Tag;
   end record;

   package Tag_Data is new Glib.Object.User_Data (Tag_And_Variant);

   procedure On_Tag_Destroyed (Data : Tag_And_Variant);
   --  Called when a tag is destroyed

   ------------------------
   -- Predefined sources --
   ------------------------

   --  Styles following preference values

   type Source_From_Style_And_Variant_Prefs is new Root_Source with record
      Style   : Style_Preference;
      --  The root style pref from which this style is created

      Variant : Variant_Preference;
      --  The font variant applying to the root font
   end record;
   overriding function Depends_On_Pref
     (Source : Source_From_Style_And_Variant_Prefs;
      Pref   : access Preference_Record'Class) return Boolean;
   overriding procedure Apply
     (Source : Source_From_Style_And_Variant_Prefs;
      Style  : in out Style_Record);

   type Source_From_Fg_Bg_Prefs is new Root_Source with record
      Fg_Pref, Bg_Pref : Color_Preference;
   end record;
   overriding function Depends_On_Pref
     (Source : Source_From_Fg_Bg_Prefs;
      Pref   : access Preference_Record'Class) return Boolean;
   overriding procedure Apply
     (Source : Source_From_Fg_Bg_Prefs;
      Style  : in out Style_Record);

   --  Styles based on another style

   type Source_Shade_Or_Lighten is new Root_Source with record
      Source_Style : Style_Access;
      --  The original style

      Shade_Amount : Gdouble := 0.0;
      --  The amount of shading to do
   end record;
   overriding function Depends_On_Pref
     (Source     : Source_Shade_Or_Lighten;
      Dummy_Pref : access Preference_Record'Class) return Boolean is (False);
   overriding procedure Apply
     (Source : Source_Shade_Or_Lighten;
      Style  : in out Style_Record);

   --  Styles overridden by code

   type Source_Override is new Root_Source with null record;
   overriding function Depends_On_Pref
     (Source     : Source_Override;
      Dummy_Pref : access Preference_Record'Class) return Boolean is (False);
   overriding procedure Apply
     (Source : Source_Override;
      Style  : in out Style_Record) is null;

   -----------
   -- Apply --
   -----------

   overriding procedure Apply
     (Source : Source_Shade_Or_Lighten;
      Style  : in out Style_Record) is
   begin
      Style.Foreground := Shade_Or_Lighten
        (Source.Source_Style.Foreground, Source.Shade_Amount);
      Style.Background := Shade_Or_Lighten
        (Source.Source_Style.Background, Source.Shade_Amount);
      Style.Variant := Source.Source_Style.Variant;
   end Apply;

   ---------------------
   -- Depends_On_Pref --
   ---------------------

   overriding function Depends_On_Pref
     (Source : Source_From_Style_And_Variant_Prefs;
      Pref   : access Preference_Record'Class)
      return Boolean
   is
   begin
      if Pref = null then
         return True;
      end if;

      return Pref = Source.Style
        or else Pref = Source.Variant;
   end Depends_On_Pref;

   -----------
   -- Apply --
   -----------

   overriding procedure Apply
     (Source : Source_From_Style_And_Variant_Prefs;
      Style  : in out Style_Record)
   is
      Variant : Variant_Enum;
      Source_Style_Bg   : constant Gdk_RGBA := Source.Style.Get_Pref_Bg;
      Source_Variant_Bg : Gdk_RGBA;

      function Variant_In_Font
        (Font : Pango_Font_Description) return Variant_Enum;
      --  Return the variant set in Font

      ---------------------
      -- Variant_In_Font --
      ---------------------

      function Variant_In_Font
        (Font : Pango_Font_Description) return Variant_Enum is
      begin
         if Get_Weight (Font) = Pango_Weight_Bold then
            if Get_Style (Font) = Pango_Style_Italic then
               return Bold_Italic;
            else
               return Bold;
            end if;
         else
            if Get_Style (Font) = Pango_Style_Italic then
               return Italic;
            else
               return Normal;
            end if;
         end if;
      end Variant_In_Font;

   begin
      --  If the variant has the same background as the style, assume
      --  the variant wants a transparent background.
      if Source.Variant = null then
         Style.Background := Source.Style.Get_Pref_Bg;
         Style.Foreground := Source.Style.Get_Pref_Fg;
         Style.Variant := Variant_In_Font (Source.Style.Get_Pref_Font);
      else
         Style.Foreground := Source.Variant.Get_Pref_Fg;
         Source_Variant_Bg := Source.Variant.Get_Pref_Bg;

         if Source_Style_Bg = Source_Variant_Bg then
            Style.Background := Null_RGBA;
         else
            Style.Background := Source_Variant_Bg;
         end if;

         Variant := Source.Variant.Get_Pref_Variant;

         if Variant = Default then
            Style.Variant := Variant_In_Font (Source.Style.Get_Pref_Font);
         else
            Style.Variant := Variant;
         end if;
      end if;
   end Apply;

   ---------------------
   -- Depends_On_Pref --
   ---------------------

   overriding function Depends_On_Pref
     (Source : Source_From_Fg_Bg_Prefs;
      Pref   : access Preference_Record'Class) return Boolean is
   begin
      if Pref = null then
         return True;
      end if;

      return Pref = Source.Fg_Pref
        or else Pref = Source.Bg_Pref;
   end Depends_On_Pref;

   -----------
   -- Apply --
   -----------

   overriding procedure Apply
     (Source : Source_From_Fg_Bg_Prefs;
      Style  : in out Style_Record) is
   begin
      if Source.Fg_Pref /= null then
         Style.Foreground := Source.Fg_Pref.Get_Pref;
      end if;

      if Source.Bg_Pref /= null then
         Style.Background := Source.Bg_Pref.Get_Pref;
      end if;
   end Apply;

   -----------------------------
   -- Create_From_Preferences --
   -----------------------------

   function Create_From_Preferences
     (Self    : Style_Manager_Record;
      Key     : Style_Key;
      Style   : Style_Preference;
      Variant : Variant_Preference := null) return Style_Access
   is
      V : Style_Access;
   begin
      V := Self.Get_Or_Create (Key);

      Unchecked_Free (V.Source);
      V.Source := new Source_From_Style_And_Variant_Prefs'
        (Style   => Style,
         Variant => Variant);

      Refresh_Values (V);

      return V;
   end Create_From_Preferences;

   -----------------------------
   -- Create_From_Preferences --
   -----------------------------

   function Create_From_Preferences
     (Self    : Style_Manager_Record;
      Key     : Style_Key;
      Fg_Pref : Color_Preference;
      Bg_Pref : Color_Preference) return Style_Access
   is
      V : Style_Access;
   begin
      V := Self.Get_Or_Create (Key);

      Unchecked_Free (V.Source);
      V.Source := new Source_From_Fg_Bg_Prefs'(Fg_Pref, Bg_Pref);

      Refresh_Values (V);
      return V;
   end Create_From_Preferences;

   -----------------------
   -- Create_From_Style --
   -----------------------

   function Create_From_Style
     (Self   : Style_Manager_Record;
      Key    : Style_Key;
      Style  : Style_Key;
      Shade_Or_Lighten_Amount : Gdouble) return Style_Access
   is
      V : Style_Access;
      O : Style_Access;
   begin
      O := Self.Get (Style);
      V := Self.Get_Or_Create (Key);

      --  Inform about the existing dependency.
      O.Children.Append (V);

      Unchecked_Free (V.Source);
      V.Source := new Source_Shade_Or_Lighten'
        (Source_Style => O,
         Shade_Amount => Shade_Or_Lighten_Amount);

      Refresh_Values (V);
      return V;
   end Create_From_Style;

   --------------------
   -- Refresh_Values --
   --------------------

   Refresh_Stack_Count : Natural := 0;
   Circular_Dependency : exception;

   procedure Refresh_Values (V : Style_Access) is
      use Default_Preferences;

      W : Weight := Pango_Weight_Normal;
      S : Pango.Enums.Style := Pango_Style_Normal;
   begin
      Refresh_Stack_Count := Refresh_Stack_Count + 1;

      --  This is a cheapo mechanism for dealing with dependency cycles:
      --  we assume that there will never be a legitimate chain of
      --  dependencies with 32 links in the chain. If this is observed,
      --  this means there is a cycle in the graph of depencencies:
      --  complain and leave.

      if Refresh_Stack_Count > 32 then
         Refresh_Stack_Count := Refresh_Stack_Count - 1;
         raise Circular_Dependency;
      end if;

      --  Still here? Good. First, apply the source to the style

      V.Source.Apply (V.all);

      --  And now apply the change to the tags following this style, if any.

      if not V.Tags.Is_Empty then
         --  Compute the settings to assign to each tag

         case V.Variant is
            when Default =>
               --  ??? Shouldn't happen
               null;
            when Normal =>
               null;
            when Bold =>
               W := Pango_Weight_Bold;
            when Italic =>
               S := Pango_Style_Italic;
            when Bold_Italic =>
               W := Pango_Weight_Bold;
               S := Pango_Style_Italic;
         end case;

         for J in 1 .. Natural (V.Tags.Length) loop
            Set_Property (V.Tags (J),
                          Foreground_Rgba_Property, V.Foreground);
            Set_Property (V.Tags (J),
                          Background_Rgba_Property, V.Background);
            Set_Property (V.Tags (J), Weight_Property, W);
            Set_Property (V.Tags (J), Style_Property, S);
         end loop;
      end if;

      Refresh_Stack_Count := Refresh_Stack_Count - 1;

      --  We have just refreshed a style, now refresh all styles that
      --  depend on this.

      for J in 1 .. V.Children.Last_Index loop
         Refresh_Values (V.Children (J));
      end loop;
   end Refresh_Values;

   ---------
   -- Get --
   ---------

   function Get
     (Self       : Style_Manager_Record;
      Key        : Style_Key;
      Allow_Null : Boolean := False) return Style_Access
   is
      C : Style_Map.Cursor;
   begin
      C := Self.Variants.Find (Key);

      if C = No_Element then
         if Allow_Null then
            return null;
         else
            raise Key_Not_Found with "Style not found: " & String (Key);
         end if;
      end if;

      return Element (C);
   end Get;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Self : Style_Manager_Record; Key : Style_Key) return Style_Access
   is
      C : Style_Map.Cursor;
      V : Style_Access;
   begin
      C := Self.Variants.Find (Key);

      if C = No_Element then
         V := new Style_Record;

         V.Name := new Style_Key'(Key);
         V.Source := new Source_Override;

         Self.Variants.Insert (Key, V);
         return V;
      else
         return Element (C);
      end if;
   end Get_Or_Create;

   -----------------------
   -- Get_Style_Manager --
   -----------------------

   function Get_Style_Manager
     (Handle : Kernel_Handle)
     return not null access Style_Manager_Record'Class
   is
   begin
      return To_Style_Manager_Access (Handle.Style_Manager);
   end Get_Style_Manager;

   -----------------
   -- List_Styles --
   -----------------

   function List_Styles
     (Self : Style_Manager_Record)
      return Style_Vector.Vector
   is
      Result : Style_Vector.Vector;
      C : Style_Map.Cursor;
   begin
      C := Self.Variants.First;

      while Has_Element (C) loop
         Result.Append (Element (C));
         Next (C);
      end loop;

      return Result;
   end List_Styles;

   -----------------------
   -- Set_Style_Manager --
   -----------------------

   procedure Set_Style_Manager
     (Handle  : Kernel_Handle;
      Manager : not null access Style_Manager_Record'Class)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Style_Manager_Access, System.Address);
   begin
      Handle.Style_Manager := To_Address (Manager);
   end Set_Style_Manager;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Style_Record) is
   begin
      Unchecked_Free (X.Name);
      Unchecked_Free (X.Source);
      X.Tags.Clear;
      X.Children.Clear;
      if X.Icon /= null then
         Unchecked_Free (X.Icon);
      end if;
   end Free;

   ------------------------
   -- Free_Style_Manager --
   ------------------------

   procedure Free_Style_Manager (Handle : Kernel_Handle) is
      X : Style_Manager_Access := To_Style_Manager_Access
        (Handle.Style_Manager);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Style_Map.Map, Map_Access);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Style_Manager_Record'Class, Style_Manager_Access);

      procedure U (Position : Style_Map.Cursor);
      --  Utility to walk the container and free memory.

      -------
      -- U --
      -------

      procedure U (Position : Style_Map.Cursor) is
         V : Style_Access;
      begin
         if Position /= No_Element then
            V := Element (Position);
            Free (V.all);
            Unchecked_Free (V);
         end if;
      end U;

   begin
      --  Remove all references to allow text tags to destroy themselves
      X.Variants.Iterate (U'Unrestricted_Access);

      --  ??? need unref all variants
      X.Variants.Clear;
      Unchecked_Free (X.Variants);
      X.Variants := null;

      --  Free self.
      Unchecked_Free (X);
      Handle.Style_Manager := System.Null_Address;
   end Free_Style_Manager;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground (Style : Style_Access) return Gdk_RGBA is
   begin
      return Style.Foreground;
   end Get_Foreground;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background (Style : Style_Access) return Gdk_RGBA is
   begin
      return Style.Background;
   end Get_Background;

   -----------------
   -- Get_Variant --
   -----------------

   function Get_Variant (Style : Style_Access) return Variant_Enum is
   begin
      return Style.Variant;
   end Get_Variant;

   ----------------------
   -- On_Tag_Destroyed --
   ----------------------

   procedure On_Tag_Destroyed (Data : Tag_And_Variant) is
   begin
      for J in 1 .. Integer (Data.Style.Tags.Length) loop
         if Data.Style.Tags (J) = Data.Tag then
            Data.Style.Tags.Delete (J);
            --  We know this tag only appears once in the container, so
            --  we can exit now.
            exit;
         end if;
      end loop;
   end On_Tag_Destroyed;

   -------------
   -- Get_Tag --
   -------------

   function Get_Tag (Style : Style_Access) return Gtk_Text_Tag is
      Tag : Gtk_Text_Tag;
      W   : Weight := Pango_Weight_Normal;
      S   : Pango.Enums.Style := Pango_Style_Normal;
   begin
      Gtk_New (Tag, Style.Name.all);

      Style.Tags.Append (Tag);

      --  Watch tag.

      Tag_Data.Set (Object       => Tag,
                    Data         => (Style, Tag),
                    Id           => "tag_data" & Style.Name.all,
                    On_Destroyed => On_Tag_Destroyed'Access);

      case Style.Variant is
         when Default =>
            null;
         when Normal =>
            null;
         when Bold =>
            W := Pango_Weight_Bold;
         when Italic =>
            S := Pango_Style_Italic;
         when Bold_Italic =>
            W := Pango_Weight_Bold;
            S := Pango_Style_Italic;
      end case;

      Set_Property (Tag, Foreground_Rgba_Property, Style.Foreground);
      Set_Property (Tag, Background_Rgba_Property, Style.Background);
      Set_Property (Tag, Weight_Property, W);
      Set_Property (Tag, Style_Property, S);

      return Tag;
   end Get_Tag;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
      Self : constant Style_Manager_Access := Hook.Manager;

      procedure Process (Position : Style_Map.Cursor);
      --  Process one item in the container

      -------------
      -- Process --
      -------------

      procedure Process (Position : Style_Map.Cursor) is
         V : Style_Access;

      begin
         if Position = No_Element then
            return;
         end if;

         V := Element (Position);

         if V.Source.Depends_On_Pref (Pref) then
            Refresh_Values (V);
         end if;
      end Process;

   begin
      Self.Variants.Iterate (Process'Unrestricted_Access);
      --  ??? need to refresh values of all items depending on the
      --  items that we have just refreshed
   end Execute;

   -----------------
   -- Parse_Color --
   -----------------

   function Parse_Color (Name : String) return Gdk_RGBA is
      Success : Boolean;
      Color   : Gdk_RGBA;
   begin

      if Name = "" then
         Trace (Me, "Color field not filled");
         return Null_RGBA;
      end if;

      Parse (Color, Name, Success);
      if not Success then
         Trace (Me, "Could not parse color " & Name);
         return Null_RGBA;
      end if;

      return Color;
   end Parse_Color;

   ----------------
   -- Foreground --
   ----------------

   function Foreground (Style : Style_Access) return Gdk_RGBA is
   begin
      return Style.Foreground;
   end Foreground;

   ----------------
   -- Background --
   ----------------

   function Background (Style : Style_Access) return Gdk_RGBA is
   begin
      return Style.Background;
   end Background;

   --------------
   -- Priority --
   --------------

   function Priority (Style : Style_Access) return Natural is
   begin
      return Style.Priority;
   end Priority;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Style : Style_Access) return String is
   begin
      return Style.Name.all;
   end Get_Name;

   ---------------------
   -- Get_In_Speedbar --
   ---------------------

   function Get_In_Speedbar (Style : Style_Access) return Boolean is
   begin
      return Style.In_Speedbar;
   end Get_In_Speedbar;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon (Style : Style_Access) return String is
   begin
      if Style.Icon = null then
         return "";
      else
         return Style.Icon.all;
      end if;
   end Get_Icon;

   --------------------------
   -- Get_Color_Preference --
   --------------------------

   function Get_Color_Preference
     (Style      : Style_Access;
      Background : Boolean := True) return Color_Preference is
   begin
      if Style.Source = null
        or else Style.Source.all not in Source_From_Fg_Bg_Prefs'Class
      then
         return null;
      end if;

      if Background then
         return Source_From_Fg_Bg_Prefs (Style.Source.all).Bg_Pref;
      else
         return Source_From_Fg_Bg_Prefs (Style.Source.all).Fg_Pref;
      end if;
   end Get_Color_Preference;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Style : Style_Access; Color : Gdk_RGBA) is
   begin
      Unchecked_Free (Style.Source);
      Style.Source := new Source_Override;
      Style.Foreground := Color;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Style : Style_Access; Color : Gdk_RGBA) is
   begin
      Unchecked_Free (Style.Source);
      Style.Source := new Source_Override;
      Style.Background := Color;
   end Set_Background;

   ---------------------
   -- Set_In_Speedbar --
   ---------------------

   procedure Set_In_Speedbar (Style : Style_Access; In_Speedbar : Boolean) is
   begin
      Style.In_Speedbar := In_Speedbar;
   end Set_In_Speedbar;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon (Style : Style_Access; Icon : String) is
   begin
      if Style.Icon /= null then
         Unchecked_Free (Style.Icon);
      end if;

      Style.Icon := new Style_Key'(Icon);
   end Set_Icon;

   ------------------------------
   -- Initialize_Style_Manager --
   ------------------------------

   procedure Initialize_Style_Manager (Kernel : Kernel_Handle) is
   begin
      Preferences_Changed_Hook.Add
        (new On_Pref_Changed'(Preferences_Hooks_Function with
             Manager => Get_Style_Manager (Kernel)));
   end Initialize_Style_Manager;

end GPS.Kernel.Style_Manager;
