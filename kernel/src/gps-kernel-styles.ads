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

--  Styles are a GPS-wide resource used to represent color and/or fonts.
--  Styles are used across modules, in all places that require color and
--  fonts that are customizable by the user: Vdiff colors, Search results
--  highlighting, syntax highlighting, Locations view, and so on.

--  This package also implements the shell interface for Styles.

with Gdk.GC;     use Gdk.GC;
with Gdk.Color; use Gdk.Color;

with GPS.Editors;
with String_Hash;

with GNAT.Strings;

package GPS.Kernel.Styles is

   type Style_Record is new GPS.Editors.Simple_Style_Record with private;
   type Style_Access is access all Style_Record'Class;
   pragma No_Strict_Aliasing (Style_Access);

   ---------------------------
   -- GPS predefined styles --
   ---------------------------

   --  These are the styles defined by default in GPS.

   Search_Results_Style   : Style_Access;
   Builder_Errors_Style   : Style_Access;
   Builder_Warnings_Style : Style_Access;
   Builder_Style_Style    : Style_Access;
   Builder_Shadow_Style   : Style_Access;

   overriding procedure Set_Foreground
     (Style : not null access Style_Record; Color : String);
   overriding procedure Set_Background
     (Style : not null access Style_Record; Color : String);

   --  Note: when adding default styles, do not forget to update
   --  Initialize_Predefined_Styles and Preferences_Changed.

   function Get_Background_GC
     (Style : not null access Style_Record) return Gdk_GC;
   function Get_Background_Color
     (Style : not null access Style_Record) return Gdk_Color;
   --  Return the background GC stored in Style. Return Null_GC if there is
   --  none.

   function Get_Name (Style : Style_Access) return String;
   --  Return the name of Style.

   procedure Save_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Save the currently registered Styles to File.

   procedure Load_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Load the currently registered Styles from File.

   function Get_Or_Create_Style
     (Kernel : Kernel_Handle;
      Name   : String;
      Create : Boolean := True) return Style_Access;
   --  Lookup the style Name.
   --  If it doesn't exist and Create = True, then create it.

   function Get_Or_Create_Style_Copy
     (Kernel     : Kernel_Handle;
      Name       : String;
      From_Style : Style_Access) return Style_Access;
   --  Lookups the style Name. Creates new style as copy of Style when it
   --  doesn't exists.

private

   type Style_Record is new GPS.Editors.Simple_Style_Record with record
      Name        : GNAT.Strings.String_Access;
      Description : GNAT.Strings.String_Access;
      --  A short description of the style

      Fg_Color   : Gdk_Color := Null_Color;
      Fg_GC      : Gdk_GC    := Null_GC;
      Bg_Color   : Gdk_Color := Null_Color;
      Bg_GC      : Gdk_GC    := Null_GC;
   end record;

   overriding procedure Free (Style : in out Style_Record);
   procedure Free (Style : in out Style_Access);
   --  Free memory occupied by Style.

   package Style_Htable is new String_Hash (Style_Access, Free, null);

   type Style_Htable_Record is new Root_Table with record
      Table : Style_Htable.String_Hash_Table.Instance;
   end record;
   type Style_Htable_Access is access all Style_Htable_Record'Class;

   overriding procedure Reset (X : access Style_Htable_Record);
   --  Reset the table.

end GPS.Kernel.Styles;
