-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Color;         use Gdk.Color;
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Gint_Xml;          use Gint_Xml;
with Gtk.Widget;        use Gtk.Widget;
with Pango.Font;        use Pango.Font;

with GNAT.OS_Lib;       use GNAT.OS_Lib;

package body Glide_Kernel.Preferences is

   function Get_Node
     (Kernel : access Kernel_Handle_Record'Class; Pref : Property)
      return Node_Ptr;
   --  Return the XML node that contains the value for a specific preference.
   --  Note that the returned node can either be part of the preferences tree
   --  in the kernel, or be a node in the default_preferences tree.

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) is
   begin
      if Kernel.Preferences /= null then
         Free (Kernel.Preferences);
      end if;

      if Is_Regular_File (File_Name) then
         Kernel.Preferences := Parse (File_Name);
      end if;
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) is
   begin
      Print (Kernel.Preferences, File_Name => File_Name);
   end Save_Preferences;

   -----------------------------
   -- Set_Default_Preferences --
   -----------------------------

   procedure Set_Default_Preferences
     (Kernel    : access Kernel_Handle_Record'Class) is
   begin
      if Kernel.Preferences /= null then
         Free (Kernel.Preferences);
      end if;
   end Set_Default_Preferences;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Kernel : access Kernel_Handle_Record'Class; Pref : Property)
      return Node_Ptr
   is
      Node : Node_Ptr;

      --  Get read of the trailing \0
      C_Name : constant String := Property_Name (Pref);
      N : constant String := C_Name (C_Name'First .. C_Name'Last - 1);
   begin
      if Kernel.Preferences /= null then
         Node := Find_Tag (Kernel.Preferences.Child, N);
      end if;

      if Node = null then
         Node := Find_Default_Pref (N);
      end if;

      pragma Assert (Node.Value /= null);
      return Node;
   end Get_Node;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Uint) return Glib.Guint is
   begin
      return Guint'Value (Get_Node (Kernel, Property (Pref)).Value.all);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Boolean) return Boolean is
   begin
      return Boolean'Value (Get_Node (Kernel, Property (Pref)).Value.all);
   exception
      when Constraint_Error =>
         return False;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_String) return String is
   begin
      return Get_Node (Kernel, Property (Pref)).Value.all;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Property_Color) return Gdk.Color.Gdk_Color
   is
      Color : Gdk_Color;
   begin
      Color := Parse (Get_Node (Kernel, Property (Pref)).Value.all);
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      return Color;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Property_Font) return Pango.Font.Pango_Font_Description
   is
      Name : constant String := Get_Node (Kernel, Property (Pref)).Value.all;
   begin
      return From_String (Name);
   end Get_Pref;

end Glide_Kernel.Preferences;
