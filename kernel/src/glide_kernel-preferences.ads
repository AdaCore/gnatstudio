-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gdk.Color;
with Gdk.Font;
with Glib.Properties;
with Glib;
with Pango.Font;
with Default_Preferences; use Default_Preferences;

package Glide_Kernel.Preferences is

   procedure Load_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String);
   --  Load the preferences from the specified file.
   --  No query is allowed before loading the preferences

   procedure Save_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String);
   --  Save the preferences in the specified file.
   --  Note that only the preferences that have been modified by the user are
   --  saved.

   procedure Set_Default_Preferences
     (Kernel    : access Kernel_Handle_Record'Class);
   --  Reset the preferences to their default value.

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Uint) return Glib.Guint;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Boolean) return Boolean;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_String) return String;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Property_Color) return Gdk.Color.Gdk_Color;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Property_Font) return Pango.Font.Pango_Font_Description;
   --  Return the value for a specific property.
   --  The colors and fonts have already been allocated when they are returned.

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   --------------
   -- Explorer --
   --------------

   Absolute_Directories : constant Glib.Properties.Property_Boolean :=
     Register_Property ("Explorer:Absolute_Directories", False);
   --  True if directories should be displayed as absolute names,
   --  False if they should be relative to the current directory set by the
   --  user.

   Show_Directories : constant Glib.Properties.Property_Boolean :=
     Register_Property ("Explorer:Show_Directories", True);
   --  Whether directories should be displayed in the tree.  If False, only the
   --  projects are shown.

   --------------------
   -- Project editor --
   --------------------

   Timestamp_Picture : constant Glib.Properties.Property_String :=
     Register_Property ("Prj_Editor:Timestamp_Picture", "%Y/%m/%d %H:%M:%S");
   --  Format used to display timestamps in the project editor

   Default_Switches_Color : constant Property_Color :=
     Register_Property ("Prj_Editor:Default_Switches_Color", "#777777");
   --  Color to use when displaying switches that are not file specific, but
   --  set at the project or package level.

   Switches_Editor_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property ("Prj_Editor:Title_Font", "helvetica bold oblique 14");
   --  Font to use for the switches editor dialog

   Variable_Ref_Background : constant Property_Color :=
     Register_Property ("Prj_Editor:Var_Ref_Bg", "#AAAAAA");
   --  Color to use for the background of variable references in the value
   --  editor

   Invalid_Variable_Ref_Background : constant Property_Color :=
     Register_Property ("Prj_Editor:Invalid_Var_Ref_Bg", "#AA0000");
   --  Color to use for the foreground of invalid variable references.

   -------------
   -- Wizards --
   -------------

   Wizard_Toc_Highlight_Color : constant Property_Color :=
     Register_Property ("Wizard:Toc_Highlight_Color", "yellow");
   --  Color to use to highlight strings in the TOC.

   Wizard_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property ("Wizard:Title_Font", "helvetica bold oblique 14");
   --  Font to use for the title of the pages in the wizard

   --------------
   -- Browsers --
   --------------

   Browsers_Link_Font : constant Property_Font :=
     Register_Property ("Browsers:Link_Font", "Helvetica 12");
   --  Font used to draw the links in the items

   Browsers_Link_Color : constant Property_Color :=
     Register_Property ("Browsers:Link_Color", "#0000FF");
   --  Color used to draw the links in the items

end Glide_Kernel.Preferences;
