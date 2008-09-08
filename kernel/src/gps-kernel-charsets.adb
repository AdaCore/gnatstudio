-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2008, AdaCore             --
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

with Gtk.Combo;                  use Gtk.Combo;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Item;                   use Gtk.Item;
with Gtk.List;                   use Gtk.List;
with Gtk.List_Item;              use Gtk.List_Item;
with Gtk.Tooltips;               use Gtk.Tooltips;
with Gtk.Widget;                 use Gtk.Widget;

with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Intl;                   use GPS.Intl;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;

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
      Value : constant String := Get_Text (Get_Entry (Gtk_Combo (Combo)));
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
      Set_Text (Get_Entry (Gtk_Combo (Combo)), Data.Pref.Get_Pref);
   end Update_Charset;

   --------------------------
   -- Create_Charset_Combo --
   --------------------------

   function Create_Charset_Combo
     (File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo.Gtk_Combo
   is
      Combo : Gtk_Combo;
      Found : Boolean := False;
      Prop  : String_Property;
      Item    : Gtk_List_Item;
   begin
      Gtk_New (Combo);
      Set_Value_In_List (Combo, False, Ok_If_Empty => False);
      Set_Editable (Get_Entry (Combo), True);

      for C in Charsets'Range loop
         Gtk_New (Item, Charsets (C).Description.all);
         Set_Item_String (Combo, Gtk_Item (Item), Charsets (C).Name.all);
         Add (Get_List (Combo), Item);
         Show_All (Item);
      end loop;

      if File /= GNATCOLL.VFS.No_File then
         Get_Property (Prop, File, "charset", Found);
      end if;

      if Found then
         Set_Text (Get_Entry (Combo), Prop.Value.all);
      elsif Default /= "" then
         Set_Text (Get_Entry (Combo), Default);
      else
         Set_Text (Get_Entry (Combo), Get_Pref (Default_Charset));
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

      Preference_Handlers.Object_Connect
        (Get_Entry (Combo), Signal_Changed,
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
