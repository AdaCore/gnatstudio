-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005                         --
--                              AdaCore                              --
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

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Glib.Object;                use Glib.Object;
with Glib.Properties.Creation;   use Glib.Properties.Creation;

with Gtk.Combo;                  use Gtk.Combo;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.List;                   use Gtk.List;
with Gtk.List_Item;              use Gtk.List_Item;
with Gtk.Tooltips;               use Gtk.Tooltips;
with Gtk.Widget;                 use Gtk.Widget;

with Default_Preferences;        use Default_Preferences;

package body GPS.Kernel.Charsets is
   function Edit_Charset
     (Manager            : access Preferences_Manager_Record;
      Preferences_Editor : access Gtk.Widget.Gtk_Widget_Record'Class;
      Param              : Glib.Param_Spec;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget;
   --  Create a widget suitable for editing a charset

   type Charset_Description is record
      Name        : String_Access;
      Description : String_Access;
   end record;
   type Charset_Description_Array
     is array (Natural range <>) of Charset_Description;

   Charsets : constant Charset_Description_Array :=
     ((Name        => new String'("ISO-8859-1"),
       Description => new String'("Western European (ISO-8859-1)")),
      (Name        => new String'("ISO-8859-2"),
       Description => new String'("Central European (ISO-8859-2)")),
      (Name        => new String'("ISO-8859-5"),
       Description => new String'("Cyrillic (ISO-8859-5)")),
      (Name        => new String'("ISO-8859-6"),
       Description => new String'("Arabic (ISO-8859-6)")),
      (Name        => new String'("ISO-8859-7"),
       Description => new String'("Greek (ISO-8859-7)")),
      (Name        => new String'("ISO-8859-8"),
       Description => new String'("Hebrew (ISO-8859-8)")),
      (Name        => new String'("KOI8-R"),
       Description => new String'("Cyrillic (KOI8-R)")),
      (Name        => new String'("SHIFT-JIS"),
       Description => new String'("Japanese (SHIFT-JIS)")),
      (Name        => new String'("GB2312"),
       Description => new String'("Chinese (GB2312)")),
      (Name        => new String'("UNICODE"),
       Description => new String'("Unicode")));
   --  The list comes from "man charsets", or "`iconv -l`".
   --  See also the list in "gedit  File->Open  Available Encodings"

   procedure Charset_Changed
     (Combo : access GObject_Record'Class; Data : Manager_Param_Spec);
   --  Called when the contents of the Combo has changed, to set the pref

   procedure Update_Charset
     (Combo : access GObject_Record'Class; Data : Manager_Param_Spec);
   --  Called when the pref has changed, to set the combo

   ---------------------
   -- Charset_Changed --
   ---------------------

   procedure Charset_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Param_Spec)
   is
      Value : constant String := Get_Text (Get_Entry (Gtk_Combo (Combo)));
   begin
      for C in Charsets'Range loop
         if Charsets (C).Description.all = Value then
            Set_Pref (Data.Manager, Param_Spec_String (Data.Param),
                      Charsets (C).Name.all);
            return;
         end if;
      end loop;

      Set_Pref (Data.Manager, Param_Spec_String (Data.Param), Value);
   end Charset_Changed;

   --------------------
   -- Update_Charset --
   --------------------

   procedure Update_Charset
     (Combo : access GObject_Record'Class;
      Data  : Manager_Param_Spec) is
   begin
      Set_Text (Get_Entry (Gtk_Combo (Combo)),
                Get_Pref (Param_Spec_Charset (Data.Param)));
   end Update_Charset;

   ------------------
   -- Edit_Charset --
   ------------------

   function Edit_Charset
     (Manager            : access Preferences_Manager_Record;
      Preferences_Editor : access Gtk.Widget.Gtk_Widget_Record'Class;
      Param              : Glib.Param_Spec;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Value    : constant String := Get_Pref (Param_Spec_Charset (Param));
      Combo   : Gtk_Combo;
      Item    : Gtk_List_Item;
      Selected : Integer := -1;
   begin
      Gtk_New (Combo);
      Set_Value_In_List (Combo, False, Ok_If_Empty => False);
      Set_Editable (Get_Entry (Combo), True);

      for C in Charsets'Range loop
         if Charsets (C).Name.all = Value then
            Selected := C;
         end if;
         Gtk_New (Item, Charsets (C).Description.all);
         Add (Get_List (Combo), Item);
         Show_All (Item);
      end loop;

      if Selected /= -1 then
         Set_Text (Get_Entry (Combo), Charsets (Selected).Description.all);
      else
         Set_Text (Get_Entry (Combo), Value);
      end if;

      Param_Spec_Handlers.Object_Connect
        (Get_Entry (Combo), "changed",
         Charset_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Param),
         Slot_Object => Combo,
         After       => True);
      Param_Spec_Handlers.Object_Connect
        (Preferences_Editor, "preferences_changed",
         Update_Charset'Access,
         Slot_Object => Combo,
         User_Data => (Preferences_Manager (Manager), Param));

      return Gtk_Widget (Combo);
   end Edit_Charset;

   ------------------
   -- Gnew_Charset --
   ------------------

   function Gnew_Charset
     (Name, Nick, Blurb   : String;
      Default             : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Charset
   is
      function Internal
        (Name, Nick, Blurb : String;
         Default           : String;
         Flags             : Param_Flags) return Param_Spec_Charset;
      pragma Import (C, Internal, "g_param_spec_string");
      Param : Param_Spec_Charset;
   begin
      Param := Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Default => Default & ASCII.Nul,
         Flags   => Flags);
      Set_Param_Spec_Editor (Param_Spec (Param), Edit_Charset'Access);
      return Param;
   end Gnew_Charset;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Pref : Param_Spec_Charset) return String is
   begin
      return Get_Pref (Param_Spec_String (Pref));
   end Get_Pref;

end GPS.Kernel.Charsets;
