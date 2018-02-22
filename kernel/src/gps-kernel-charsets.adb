------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Glib.Convert;               use Glib.Convert;
with Glib.Unicode;               use Glib.Unicode;

with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Widget;                 use Gtk.Widget;

with Basic_Types;                use Basic_Types;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GPS.Properties;             use GPS.Properties;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Intl;                   use GPS.Intl;
with String_Utils;               use String_Utils;
with Gtkada.Types;               use Gtkada.Types;

package body GPS.Kernel.Charsets is
   CHARSET : constant GNAT.Strings.String_Access := Getenv ("CHARSET");

   Default_Charset : Charset_Preference;
   --  Preference that defines the default charset to use when opening files

   type Charset_Description is record
      Name        : GNAT.Strings.String_Access;
      Description : GNAT.Strings.String_Access;
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
      (Name        => new String'("EUC-JP"),
       Description => new String'("Japanese (EUC-JP)")),
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

   ---------------------
   -- Charset_Changed --
   ---------------------

   procedure Charset_Changed
     (Combo : access GObject_Record'Class;
      Data  : Manager_Preference)
   is
      Value : constant String := Get_Active_Text (Gtk_Combo_Box_Text (Combo));
   begin
      for C in Charsets'Range loop
         if Charsets (C).Description.all = Value then
            Set_Pref (Data.Pref, Data.Manager, Charsets (C).Name.all);
            return;
         end if;
      end loop;

      Set_Pref (Data.Pref, Data.Manager, Value);
   end Charset_Changed;

   --------------------------
   -- Create_Charset_Combo --
   --------------------------

   function Create_Charset_Combo
     (File    : GNATCOLL.VFS.Virtual_File;
      Default : String := "") return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text
   is
      function Get_Default_Charset_Name return String;
      Combo : Gtk_Combo_Box_Text;

      function Get_Default_Charset_Name return String is
         Found : Boolean := False;
         Prop  : String_Property;
      begin
         if File /= GNATCOLL.VFS.No_File then
            Get_Property (Prop, File, "charset", Found);
         end if;

         if Found then
            return Prop.Value.all;
         elsif Default /= "" then
            return Default;
         else
            return Get_Pref (Default_Charset);
         end if;
      end Get_Default_Charset_Name;

      Default_Name  : constant String := Get_Default_Charset_Name;
      Default_Index : Integer := -1;
   begin
      Gtk_New_With_Entry (Combo);

      for C in Charsets'Range loop
         Combo.Append_Text (Charsets (C).Description.all);

         if Charsets (C).Name.all = Default_Name then
            Default_Index := C;
         end if;
      end loop;

      Set_Active (Combo, Gint (Default_Index));

      return Combo;
   end Create_Charset_Combo;

   ----------------------
   -- Selected_Charset --
   ----------------------

   function Selected_Charset
     (Combo : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text) return String
   is
      Index : constant Integer := Integer (Combo.Get_Active);
   begin
      if Index in Charsets'Range then
         return Charsets (Index).Name.all;
      else
         return "";
      end if;
   end Selected_Charset;

   ----------
   -- Edit --
   ----------

   overriding function Edit
     (Pref               : access Charset_Preference_Record;
      Manager            : access Preferences_Manager_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Value    : constant String := Pref.Get_Pref;
      Combo    : Gtk_Combo_Box_Text;
      Selected : Integer := -1;
   begin
      Gtk_New_With_Entry (Combo);

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

      Set_GObject_To_Update (Pref, GObject (Combo));

      return Gtk_Widget (Combo);
   end Edit;

   ------------
   -- Create --
   ------------

   function Create
     (Manager                   : access Preferences_Manager_Record'Class;
      Path                      : Preference_Path;
      Name, Label, Doc          : String;
      Default                   : String)
      return Charset_Preference
   is
      Result : constant Charset_Preference := new Charset_Preference_Record;
   begin
      Set_Pref (Result, Manager, Default);
      Manager.Register (Path, Name, Label, Doc, Result);

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
         Path    => -"General:Charsets",
         Doc     =>
           -("Character set to load and save files. GPS uses unicode"
             & " internally and needs to convert appropriately."),
         Default => "ISO-8859-1");
   end Register_Preferences;

   ----------------------------
   -- Update_On_Pref_Changed --
   ----------------------------

   overriding procedure Update_On_Pref_Changed
     (Pref   : access Charset_Preference_Record;
      Widget : access GObject_Record'Class) is
   begin
      Set_Text
        (Gtk_Entry
           (Gtk_Combo_Box_Text (Widget).Get_Child), Pref.Get_Pref);
   end Update_On_Pref_Changed;

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

   ----------------------------
   -- Read_File_With_Charset --
   ----------------------------

   procedure Read_File_With_Charset
     (File     : GNATCOLL.VFS.Virtual_File;
      UTF8     : out Gtkada.Types.Chars_Ptr;
      UTF8_Len : out Natural;
      Props    : out File_Props)
   is
      Contents      : GNAT.Strings.String_Access;
      Last          : Natural;
      Ignore        : aliased Natural;
      Length        : aliased Natural;
      Valid         : Boolean;
      First_Invalid : Natural;
      Charset : constant String := Get_File_Charset (File);
      C             : Integer := Charsets'First;

      function To_Unchecked_String is new Ada.Unchecked_Conversion
        (Chars_Ptr, Unchecked_String_Access);

   begin
      Props := (Invalid_UTF8          => False,
                CR_Found              => False,
                NUL_Found             => False,
                Trailing_Spaces_Found => False,
                Trailing_Lines_Found  => False);

      Contents := File.Read_File;
      if Contents = null then
         UTF8 := Null_Ptr;
         UTF8_Len := 0;
         return;
      end if;

      Strip_CR_And_NUL
        (Contents.all, Last,
         Props.CR_Found, Props.NUL_Found, Props.Trailing_Spaces_Found);

      UTF8 := Glib.Convert.Convert
        (Contents (Contents'First .. Last), "UTF-8", Charset,
         Ignore'Unchecked_Access, Length'Unchecked_Access);

      --  In case conversion failed, use a default encoding so that we
      --  can at least show something in the editor

      while UTF8 = Gtkada.Types.Null_Ptr
        and then C <= Charsets'Last
      loop
         UTF8 := Glib.Convert.Convert
           (Contents (Contents'First .. Last), "UTF-8",
            Charsets (C).Name.all,
            Ignore'Unchecked_Access, Length'Unchecked_Access);
         C := C + 1;
      end loop;

      for J in reverse Contents'Range loop
         if Contents (J) = ASCII.LF then
            if J /= Length - 1 then
               Props.Trailing_Lines_Found := True;
               exit;
            end if;
         elsif Contents (J) /= ' ' and then Contents (J) /= ASCII.HT then
            exit;
         end if;
      end loop;

      GNAT.Strings.Free (Contents);

      if UTF8 /= Null_Ptr then
         UTF8_Validate
           (To_Unchecked_String (UTF8) (1 .. Length), Valid, First_Invalid);
      else
         Valid := False;
         First_Invalid := 1;
      end if;

      if not Valid then
         UTF8_Len := First_Invalid - 1;
         Props.Invalid_UTF8 := True;
      else
         UTF8_Len := Length;
      end if;
   end Read_File_With_Charset;

end GPS.Kernel.Charsets;
