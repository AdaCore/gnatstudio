-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2011, AdaCore             --
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

with Gtk.Combo_Box;              use Gtk.Combo_Box;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Tooltips;               use Gtk.Tooltips;
with Gtk.Widget;                 use Gtk.Widget;

with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GPS.Properties;             use GPS.Properties;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Intl;                   use GPS.Intl;
with GUI_Utils;                  use GUI_Utils;

package body GPS.Kernel.Charsets is

   CHARSET : constant String_Access := Getenv ("CHARSET");

   Default_Charset : Charset_Preference;
   --  Preference that defines the default charset to use when opening files

   type Charset_Description is record
      Name        : String_Access;
      Description : String_Access;
   end record;
   type Charset_Description_Array
     is array (Natural range <>) of Charset_Description;

   Charsets : constant Charset_Description_Array :=
     ((Name        => new String'("ISO-8859-1"),
       Description => new String'("Western/Latin-1 (ISO-8859-1)")),
      (Name        => new String'("ISO-8859-15"),
       Description => new String'("Western/Latin-9 (ISO-8859-15)")),
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
      (Name        => new String'("ISO-8859-9"),
       Description => new String'("Turkish (ISO-8859-9)")),
      (Name        => new String'("KOI8-R"),
       Description => new String'("Cyrillic (KOI8-R)")),
      (Name        => new String'("SHIFT-JIS"),
       Description => new String'("Japanese (SHIFT-JIS)")),
      (Name        => new String'("GB2312"),
       Description => new String'("Chinese (GB2312)")),
      (Name        => new String'("UTF-8"),
       Description => new String'("Unicode UTF-8")),
      (Name        => new String'("UTF-16"),
       Description => new String'("Unicode UTF-16")),
      (Name        => new String'("UTF-32"),
       Description => new String'("Unicode UTF-32")));
   --  The list comes from "man charsets", or "`iconv -l`".
   --  See also the list in "gedit  File->Open  Available Encodings"

   procedure Charset_Changed
     (Combo : access GObject_Record'Class; Data : Manager_Preference);
   --  Called when the contents of the Combo has changed, to set the pref

   procedure Update_Charset
     (Combo : access GObject_Record'Class; Data : Manager_Preference);
   --  Called when the pref has changed, to set the combo

   ---------------------
   -- Charset_Changed --
   ---------------------

   procedure Charset_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      Value : constant String := Get_Active_Text (Gtk_Combo_Box (Combo));
   begin
      for C in Charsets'Range loop
         if Charsets (C).Description.all = Value then
            Set_Pref (Data.Pref, Data.Manager, Charsets (C).Name.all);
            return;
         end if;
      end loop;

      Set_Pref (Data.Pref, Data.Manager, Value);
   end Charset_Changed;

   --------------------
   -- Update_Charset --
   --------------------

   procedure Update_Charset
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference) is
   begin
      Set_Text
        (Gtk_Entry (Gtk_Combo_Box (Combo).Get_Child), Data.Pref.Get_Pref);
   end Update_Charset;

   --------------------------
   -- Create_Charset_Combo --
   --------------------------

   function Create_Charset_Combo
     (File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo_Box.Gtk_Combo_Box
   is
      Combo : Gtk_Combo_Box;
      Found : Boolean := False;
      Prop  : String_Property;

   begin
      Gtk_New_Combo_Text_With_Entry (Combo);

      for C in Charsets'Range loop
         Combo.Append_Text (Charsets (C).Description.all);
      end loop;

      if File /= GNATCOLL.VFS.No_File then
         Get_Property (Prop, File, "charset", Found);
      end if;

      if Found then
         Set_Text (Gtk_Entry (Get_Child (Combo)), Prop.Value.all);
      elsif Default /= "" then
         Set_Text (Gtk_Entry (Get_Child (Combo)), Default);
      else
         Set_Text (Gtk_Entry (Get_Child (Combo)), Get_Pref (Default_Charset));
      end if;

      return Combo;
   end Create_Charset_Combo;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Charset_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class;
      Tips               : Gtk.Tooltips.Gtk_Tooltips)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tips);
      Value    : constant String := Pref.Get_Pref;
      Combo    : Gtk_Combo_Box;
      Selected : Integer := -1;
   begin
      Gtk_New_Combo_Text_With_Entry (Combo);

      for C in Charsets'Range loop
         Combo.Append_Text (Charsets (C).Description.all);

         if Charsets (C).Name.all = Value then
            Selected := C;
         end if;
      end loop;

      if Selected /= -1 then
         Combo.Set_Active (Gint (Selected));
      else
         Set_Text (Gtk_Entry (Combo.Get_Child), Value);
      end if;

      Preference_Handlers.Object_Connect
        (Combo, Gtk.Combo_Box.Signal_Changed,
         Charset_Changed'Access,
         User_Data   => (Preferences_Manager (Manager), Preference (Pref)),
         Slot_Object => Combo,
         After       => True);
      Preference_Handlers.Object_Connect
        (Get_Editor (Manager), Signal_Preferences_Changed,
         Update_Charset'Access,
         Slot_Object => Combo,
         User_Data => (Preferences_Manager (Manager), Preference (Pref)));

      return Gtk_Widget (Combo);
   end Edit;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Name, Label, Page, Doc    : String;
      Default                   : String)
      return Charset_Preference
   is
      Result : constant Charset_Preference := new Charset_Preference_Record;
   begin
      Set_Pref (Result, null, Default);
      Register (Manager, Name, Label, Page, Doc, Result);
      return Result;
   end Create;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Default_Charset := Create
        (Get_Preferences (Kernel),
         Name    => "General-Charset",
         Label   => -"Character set",
         Page    => -"General",
         Doc     => -("Name of character set to use when reading or saving"
                      & " files. GPS uses unicode internally, but need to"
                      & " convert the files from and to your system's"
                      & " own encoding. Use ""UTF-8"" if your system supports"
                      & " unicode"),
         Default => "ISO-8859-1");
   end Register_Preferences;

   ----------------------
   -- Get_File_Charset --
   ----------------------

   function Get_File_Charset (File : Virtual_File) return String is
      Found : Boolean;
      Prop  : String_Property;
   begin
      if File = GNATCOLL.VFS.No_File then
         if Default_Charset = null then
            --  Cannot happen in GPS itself, but could in the test suites,
            --  e.g. when no kernel/preferences are available.

            return CHARSET.all;

         else
            return Get_Pref (Default_Charset);
         end if;

      else
         Get_Property (Prop, File, "charset", Found);
         if Found then
            return Prop.Value.all;
         else
            if Default_Charset = null then
               return CHARSET.all;
            else
               return Get_Pref (Default_Charset);
            end if;
         end if;
      end if;
   end Get_File_Charset;

   ----------------------
   -- Set_File_Charset --
   ----------------------

   procedure Set_File_Charset
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File : GNATCOLL.VFS.Virtual_File; Charset : String := "")
   is
      Prop : String_Property_Access;
   begin
      if Charset = "" then
         Remove_Property (Kernel, File, "charset");
      else
         Prop := new String_Property'(Value => new String'(Charset));
         Set_Property (Kernel, File, "charset", Prop, Persistent => True);
      end if;
   end Set_File_Charset;

end GPS.Kernel.Charsets;
