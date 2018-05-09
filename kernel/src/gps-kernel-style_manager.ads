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

--  This package provides a style manager.
--  The style manager is a centralized place for controlling styles in GPS.
--
--  A Style is a combination of graphical elements used to highlight
--  specific information to the users: it contains components such as
--  foreground and background colors, font weight or style variations,
--  an icon, etc. Depending on where the style is applied, a subset of
--  these components may be applied. For instance, for Styles corresponding
--  to syntax highlighting, the foreground, background, and font variant
--  will apply. For Styles attached to a Message, the icon associated to
--  the Style might be displayed in the speedbar, or the location view.
--
--  The Style Manager also provides facilities for inheriting a Style from
--  another Style. For instance we could have a Style for comments, then
--  a style for special "doc comments" which inherits from the first style.
--
--  The Style Manager also handles facilities for factorizing common uses
--  of a Style. For instance, it is possible to obtain a text tag associated
--  to a Style, in which case the Style Manager will take care of updating
--  the tag properties every time the values of the Style change. This way,
--  clients do not have to implement their own "preferences_changed" hook
--  to implement that.
--
--  An invariant:
--     Styles are *never* destroyed while GPS is running.
--     This means that it is safe for clients to store a Style_Access.
--     It is also valid to compare styles by comparing two Style_Access.

with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

with Gdk.RGBA;            use Gdk.RGBA;
with Gtk.Text_Tag;        use Gtk.Text_Tag;

with Default_Preferences; use Default_Preferences;

package GPS.Kernel.Style_Manager is

   type Style_Manager_Record is tagged private;
   type Style_Manager_Access is access all Style_Manager_Record'Class;

   ------------
   -- Styles --
   ------------

   type Style_Record is private;
   type Style_Access is access Style_Record;

   package Style_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Style_Access);

   --  Accessors.
   --  These are guaranteed to be fast.

   function Foreground (Style : Style_Access) return Gdk_RGBA;
   pragma Inline (Foreground);
   --  The foreground

   function Background (Style : Style_Access) return Gdk_RGBA;
   pragma Inline (Background);
   --  The background

   ----------------------
   -- Style priorities --
   ----------------------

   --  The priority is a Natural, 0 meaning the lowest priority, and higher
   --  number meaning higher priorities. This is used, for instance,
   --  if we have the room to display only one priority, which one should
   --  have precedence.

   Priority_None     : constant Natural := 0;
   Priority_Info     : constant Natural := 100;
   Priority_Warning  : constant Natural := 200;
   Priority_Error    : constant Natural := 300;
   Priority_Critical : constant Natural := 400;

   function Priority (Style : Style_Access) return Natural;
   --  The priority of the style.

   ----------------
   -- Style keys --
   ----------------

   subtype Style_Key is String;
   --  A Style_Key is a key for identifying and retrieving styles in the
   --  style manager. There is at most one style associated to each key.
   --  The name of a style (obtained through Get_Name) is equal to its key.

   ---------------------
   -- Kernel handling --
   ---------------------

   function Get_Style_Manager
     (Handle : Kernel_Handle)
     return not null access Style_Manager_Record'Class;
   --  Retrieve the color manager from the kernel.

   procedure Set_Style_Manager
     (Handle  : Kernel_Handle;
      Manager : not null access Style_Manager_Record'Class);
   --  Set the color manager in the kernel.

   procedure Free_Style_Manager (Handle : Kernel_Handle);
   --  Free memory associated to X

   Key_Not_Found : exception;

   -------------
   -- Getters --
   -------------

   function Get_Name (Style : Style_Access) return String;
   function Get_Foreground (Style : Style_Access) return Gdk_RGBA;
   function Get_Background (Style : Style_Access) return Gdk_RGBA;
   function Get_Variant (Style : Style_Access) return Variant_Enum;
   function Get_In_Speedbar (Style : Style_Access) return Boolean;
   function Get_Icon (Style : Style_Access) return String;
   --  Return the Foreground, Background, Variant for the given key.
   --  Raise Key_Not_Found if the key was not found in the manager.

   function Get_Color_Preference
     (Style      : Style_Access;
      Background : Boolean := True) return Color_Preference;
   --  Return the color preference associated with the given style or null
   --  when the style is not associated to a color preference.

   -------------
   -- Setters --
   -------------

   --  Calling any of these subprograms marks the style as being
   --  "overridden": for instance, if this style was marked as following
   --  a preference or another style, this will no longer be the case.

   procedure Set_Foreground (Style : Style_Access; Color : Gdk_RGBA);
   procedure Set_Background (Style : Style_Access; Color : Gdk_RGBA);

   --  These subprograms change the behavior of the style but do not
   --  mark the style as being "overridden".
   procedure Set_In_Speedbar (Style : Style_Access; In_Speedbar : Boolean);
   procedure Set_Icon (Style : Style_Access; Icon : String);

   ------------------------------------------
   -- Entering values in the color manager --
   ------------------------------------------

   --  Keys set from preferences

   function Create_From_Preferences
     (Self    : Style_Manager_Record;
      Key     : Style_Key;
      Style   : Style_Preference;
      Variant : Variant_Preference := null) return Style_Access;
   --  Create a style that follows a given style pref, modified with a
   --  variant pref.

   function Create_From_Preferences
     (Self    : Style_Manager_Record;
      Key     : Style_Key;
      Fg_Pref : Color_Preference;
      Bg_Pref : Color_Preference) return Style_Access;
   --  Create an entry for key in the manager.
   --  This style follows a pair for preferences for its foreground
   --  and background; any of these can be null.
   --  If this key was already entered, set it to follow the preference.

   --  Keys set from Formulas

   function Create_From_Style
     (Self   : Style_Manager_Record;
      Key    : Style_Key;
      Style  : Style_Key;
      Shade_Or_Lighten_Amount : Gdouble) return Style_Access;
   --  Create an entry for Key in the manager.
   --  Key will get its value from Source and a Shade_Or_Lighten amount
   --  (specified between 0 and 1)
   --  If this key was already entered, set it to follow the formula.

   function Get
     (Self       : Style_Manager_Record;
      Key        : Style_Key;
      Allow_Null : Boolean := False) return Style_Access;
   --  Returns the key contained in Self.
   --  If the key is not found: if Allow_Null, then return null, otherwise
   --  raise Key_Not_Found.

   function Get_Or_Create
     (Self : Style_Manager_Record; Key : Style_Key) return Style_Access;
   --  Returns the key contained in Self, and create it if it is not found.

   function List_Styles
     (Self : Style_Manager_Record)
      return Style_Vector.Vector;
   --  Return the list of registered styles

   ------------------------------
   -- Associating tags to keys --
   ------------------------------

   function Get_Tag (Style : Style_Access) return Gtk_Text_Tag;
   --  Return a text tag which follows the value of the key.

   ---------------
   -- Utilities --
   ---------------

   function Parse_Color (Name : String) return Gdk_RGBA;
   --  Sooner or later, someone needs to use this

   --------------------
   -- Initialization --
   --------------------

   procedure Initialize_Style_Manager (Kernel : Kernel_Handle);
   --  Initializes the style manager

private

   type Source_Type is (From_Preference, From_Formula, From_Override);

   type Key_Access is access Style_Key;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Style_Key, Key_Access);

   package Tag_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Gtk_Text_Tag);

   -------------
   -- Sources --
   -------------

   --  The source indicates where the style is getting its source from.

   type Root_Source is abstract tagged null record;
   type Source_Access is access all Root_Source'Class;

   function Depends_On_Pref
     (Source : Root_Source;
      Pref   : access Preference_Record'Class)
      return Boolean is abstract;
   --  Return True IFF the source is a derivative of Pref.

   procedure Apply
     (Source  : Root_Source;
      Style   : in out Style_Record) is abstract;
   --  Apply the effects of Source to Style.

   ------------
   -- Styles --
   ------------

   --  The definition of styles.

   type Style_Record is record
      Name   : Key_Access;
      --  This cannot be null

      Source : Source_Access;
      --  The source from which the style is derived. This cannot be null.

      Tags : Tag_Vector.Vector;
      --  The list of tags currently following this style

      Children : Style_Vector.Vector;
      --  The styles which depend on this style (ie, which must be refreshed
      --  when this style is refreshed).
      --  No cycle detection is done on this

      --  Values cached
      Foreground : Gdk_RGBA := Null_RGBA;
      Background : Gdk_RGBA := Null_RGBA;
      Variant    : Variant_Enum := Default;

      Priority   : Natural := Priority_None;

      Icon        : Key_Access := null;
      In_Speedbar : Boolean := False;
   end record;

   package Style_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Style_Key,
      Element_Type    => Style_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   --  ??? We could probably find a more efficient container for this
   --  if we need.

   type Map_Access is access Style_Map.Map;

   type Style_Manager_Record is tagged record
      Variants : Map_Access := new Style_Map.Map;
   end record;

end GPS.Kernel.Style_Manager;
