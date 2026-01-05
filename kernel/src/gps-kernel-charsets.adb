------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2026, AdaCore                     --
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

with Ada.Streams;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Widget;                 use Gtk.Widget;

with VSS.Characters.Latin;
with VSS.Characters.Punctuations;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Converters.Decoders;
with VSS.Strings.Conversions;

with GPS.Properties;             use GPS.Properties;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Intl;                   use GPS.Intl;
with String_Utils;               use String_Utils;

package body GPS.Kernel.Charsets is

   use all type VSS.Strings.Converters.Converter_Flag;

   CHARSET : constant GNAT.Strings.String_Access := Getenv ("CHARSET");

   Default_Charset : Charset_Preference;
   --  Preference that defines the default charset to use when opening files

   Me              : constant Trace_Handle := Create
     ("GPS.INTERNAL.CHARSETS", Default => Off);

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

   Decoder_Flags : constant VSS.Strings.Converters.Converter_Flags :=
     (Stateless     => True,
      --  Data is decoded as single chunk, don't save state but report error
      --  for incomplete byte sequences at the end of data
      Stop_On_Error => False,
      --  Errors should be reported but not to stop decoding of the following
      --  data
      Process_BOM   => True);
      --  Byte-Order-Mark at the beginning of the data should be ignored if
      --  present
   --  Default flags for the text decoder.

   procedure Charset_Changed
     (Combo : access GObject_Record'Class; Data : Manager_Preference);
   --  Called when the contents of the Combo has changed, to set the pref

   function Is_Ada_Separator
     (Item : VSS.Characters.Virtual_Character) return Boolean;
   --  Return True when given character belongs to 'separator' category,
   --  defined by Ada 2012 Reference Manual.

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
           -("Character set to load and save files. GNAT Studio uses unicode"
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

   -------------------------
   -- Get_Default_Charset --
   -------------------------

   function Get_Default_Charset return String is
   begin
      if Default_Charset = null then
         --  Cannot happen in GNAT Studio itself, but could in the test
         --  suites, e.g. when no kernel/preferences are available.

         return CHARSET.all;

      else
         return Get_Pref (Default_Charset);
      end if;
   end Get_Default_Charset;

   ----------------------
   -- Get_File_Charset --
   ----------------------

   function Get_File_Charset (File : Virtual_File) return String is
      Found : Boolean;
      Prop  : String_Property;
   begin
      if File = GNATCOLL.VFS.No_File then
         return Get_Default_Charset;

      else
         Get_Property (Prop, File, "charset", Found);

         if Found then
            return Prop.Value.all;

         else
            return Get_Default_Charset;
         end if;
      end if;
   end Get_File_Charset;

   ----------------------
   -- Set_File_Charset --
   ----------------------

   procedure Set_File_Charset
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File : GNATCOLL.VFS.Virtual_File; Charset : String := "") is
   begin
      if Charset = "" then
         Remove_Property (Kernel, File, "charset");

      else
         Set_Property
           (Kernel     => Kernel,
            File       => File,
            Name       => "charset",
            Property   => new String_Property'(Value => new String'(Charset)),
            Persistent => True);
      end if;
   end Set_File_Charset;

   ----------------------------
   -- Read_File_With_Charset --
   ----------------------------

   procedure Read_File_With_Charset
     (File  : GNATCOLL.VFS.Virtual_File;
      Text  : out VSS.Strings.Virtual_String;
      Props : out File_Props)
   is
      Contents : GNAT.Strings.String_Access;
      Last     : Natural;
      Charset  : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Get_File_Charset (File));

   begin
      Trace (Me, "Reading file: " & File.Display_Full_Name);

      Props := (Invalid_UTF8          => False,
                CR_Found              => False,
                NUL_Found             => False,
                Trailing_Spaces_Found => False,
                Trailing_Lines_Found  => False,
                Bidirectional_Unicode => False);

      Contents := File.Read_File;

      if Contents = null then
         --  Reset Text to null state to report "file" error.
         Trace (Me, "Unable to read file: " & File.Display_Full_Name);
         Text.Clear;

         return;
      end if;

      Strip_CR_And_NUL
        (Contents.all, Last,
         Props.CR_Found, Props.NUL_Found, Props.Trailing_Spaces_Found);

      declare
         Buffer  : constant Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Offset (Last - Contents'First + 1))
             with Import, Address => Contents.all'Address;
         Decoder : VSS.Strings.Converters.Decoders.Virtual_String_Decoder;
         Aux     : VSS.Strings.Virtual_String;

      begin
         --  Attempt to decode text from specified encoding.
         Trace (Me, "Decoding file: " & File.Display_Full_Name);
         Decoder.Initialize (Charset, Decoder_Flags);

         if Decoder.Is_Valid then
            Text               := Decoder.Decode (Buffer);
            Props.Invalid_UTF8 := Decoder.Has_Error;
         end if;

         if not Decoder.Is_Valid or else Decoder.Has_Error then
            --  If there is an error, try to fallback to "utf-8" encoding.

            Decoder.Initialize ("utf-8", Decoder_Flags);

            Aux := Decoder.Decode (Buffer);

            if not Props.Invalid_UTF8 or else not Decoder.Has_Error then
               --  "not Props.Invalid_UTF8" means that specified encoding is
               --  not supported, thus Text is empty, so use text decoded by
               --  "utf-8" decoder even if there are errors.

               Text               := Aux;
               Props.Invalid_UTF8 := Decoder.Has_Error;
            end if;
         end if;

         if Text.Is_Null then
            --  Decoder doesn't allocate memory when there is no data to
            --  convert, but "null" state of the string is used to report
            --  file errors; thus, assign an empty string to the result.

            Text := "";
         end if;

         --  Detect multiple empty lines at the end of the file.

         declare
            use type VSS.Characters.Virtual_Character;

            Iterator   : VSS.Strings.Character_Iterators.Character_Iterator :=
              Text.After_Last_Character;
            Line_Found : Boolean := False;

         begin
            while Iterator.Backward loop
               if Iterator.Element = VSS.Characters.Latin.Line_Feed then
                  if Line_Found then
                     Props.Trailing_Lines_Found := True;

                     exit;

                  else
                     Line_Found := True;
                  end if;

               elsif not Is_Ada_Separator (Iterator.Element) then
                  exit;
               end if;
            end loop;
         end;

         --  Scan text for some special characters.

         declare
            use VSS.Characters.Punctuations;

            Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
              Text.Before_First_Character;

         begin
            while Iterator.Forward loop
               case Iterator.Element is
                  when Left_To_Right_Embedding
                     | Right_To_Left_Embedding
                     | Pop_Directional_Formatting
                     | Left_To_Right_Override
                     | Right_To_Left_Override
                     | Left_To_Right_Isolate
                     | Right_To_Left_Isolate
                     | First_Strong_Isolate
                     | Pop_Directional_Isolate
                     =>
                     --  Some of Unicode bidirectional override characters
                     --  is present in the text.

                     Props.Bidirectional_Unicode := True;

                  when others =>
                     null;
               end case;
            end loop;
         end;
      end;

      GNAT.Strings.Free (Contents);
   end Read_File_With_Charset;

   ----------------------
   -- Is_Ada_Separator --
   ----------------------

   function Is_Ada_Separator
     (Item : VSS.Characters.Virtual_Character) return Boolean is
   begin
      --  Ada 2012's RM defines separator as 'separator_space',
      --  'format_efector' or end of a line, with some exceptions inside
      --  comments.
      --
      --  'separator_space' is defined as a set of characters with
      --  'General Category' defined as 'Separator, Space'.
      --
      --  'format_effector' is set of characters:
      --    - CHARACTER TABULATION
      --    - LINE FEED
      --    - LINE TABULATION
      --    - FORM FEED
      --    - CARRIAGE RETURN
      --    - NEXT LINE
      --    - characters with General Category defined as
      --      'Separator, Line'
      --    - characters with General Category defined as
      --      'Separator, Paragraph'

      return
        Item in
            VSS.Characters.Virtual_Character'Val (16#09#)
          | VSS.Characters.Virtual_Character'Val (16#0A#)
          | VSS.Characters.Virtual_Character'Val (16#0B#)
          | VSS.Characters.Virtual_Character'Val (16#0C#)
          | VSS.Characters.Virtual_Character'Val (16#0D#)
          | VSS.Characters.Virtual_Character'Val (16#85#)
        or VSS.Characters.Get_General_Category (Item) in
            VSS.Characters.Space_Separator
          | VSS.Characters.Line_Separator
          | VSS.Characters.Paragraph_Separator;
   end Is_Ada_Separator;

end GPS.Kernel.Charsets;
