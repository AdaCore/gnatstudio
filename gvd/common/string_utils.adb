-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with OS_Utils;                use OS_Utils;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with System;                    use System;
with Unchecked_Conversion;

package body String_Utils is

   function My_Normalize_Pathname
     (Name          : String;
      Directory     : String := "";
      Resolve_Links : Boolean := True)
      return      String;
   --  Same as GNAT.OS_Lib.My_Normalize_Pathname, but don't resolve the
   --  symbolic links.
   --  Temporary, until B425-004 is resolved.

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1) is
   begin
      while (Index <= Type_Str'Last and then Index >= Type_Str'First)
        and then (Type_Str (Index) = ' '
                  or else Type_Str (Index) = ASCII.HT
                  or else Type_Str (Index) = ASCII.LF
                  or else Type_Str (Index) = ASCII.CR)
      loop
         Index := Index + Step;
      end loop;
   end Skip_Blanks;

   -------------------
   -- Skip_To_Blank --
   -------------------

   procedure Skip_To_Blank
     (Type_Str : String;
      Index    : in out Natural) is
   begin
      while Index <= Type_Str'Last
        and then Type_Str (Index) /= ' '
        and then Type_Str (Index) /= ASCII.HT
        and then Type_Str (Index) /= ASCII.LF
        and then Type_Str (Index) /= ASCII.CR
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_Blank;

   ---------------------
   -- Skip_Hexa_Digit --
   ---------------------

   procedure Skip_Hexa_Digit
     (Type_Str : String;
      Index    : in out Natural) is
   begin
      --  skips initial 0x if present

      if Index + 1 <= Type_Str'Last
        and then Type_Str (Index) = '0'
        and then Type_Str (Index + 1) = 'x'
      then
         Index := Index + 2;
      end if;

      while Index <= Type_Str'Last
        and then Is_Hexadecimal_Digit (Type_Str (Index))
      loop
         Index := Index + 1;
      end loop;
   end Skip_Hexa_Digit;

   ------------------
   -- Skip_To_Char --
   ------------------

   procedure Skip_To_Char
     (Type_Str : String;
      Index    : in out Natural;
      Char     : Character;
      Step     : Integer := 1) is
   begin
      while Index <= Type_Str'Last
        and then Index >= Type_Str'First
        and then Type_Str (Index) /= Char
      loop
         Index := Index + Step;
      end loop;
   end Skip_To_Char;

   --------------------
   -- Skip_To_String --
   --------------------

   procedure Skip_To_String
     (Type_Str  : String;
      Index     : in out Natural;
      Substring : String)
   is
      L : constant Natural := Substring'Length - 1;
   begin
      while Index + L <= Type_Str'Last
        and then Type_Str (Index .. Index + L) /= Substring
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_String;

   ---------------
   -- Parse_Num --
   ---------------

   procedure Parse_Num
     (Type_Str : String;
      Index    : in out Natural;
      Result   : out Long_Integer)
   is
      Tmp_Index : constant Natural := Index;
   begin

      --  Recognize negative numbers as well

      if Type_Str (Index) = '-' then
         Index := Index + 1;
      end if;

      while Index <= Type_Str'Last
        and then Type_Str (Index) in '0' .. '9'
      loop
         Index := Index + 1;
      end loop;

      --  If at least one valid character was found, we have a number.

      if Index > Tmp_Index then
         Result := Long_Integer'Value (Type_Str (Tmp_Index .. Index - 1));
      else
         Result := 0;
      end if;
   end Parse_Num;

   ----------------
   -- Looking_At --
   ----------------

   function Looking_At
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Boolean is
   begin
      return Index + Substring'Length - 1 <= Type_Str'Last
        and then Type_Str (Index .. Index + Substring'Length - 1) = Substring;
   end Looking_At;

   ----------------------
   -- Parse_Cst_String --
   ----------------------

   procedure Parse_Cst_String
     (Type_Str          : String;
      Index             : in out Natural;
      Str               : out String;
      Backslash_Special : Boolean := True)
   is
      procedure Parse_Next_Char
        (Index : in out Natural;
         Char  : out Character);
      --  Parse the character pointed to by Index, including special characters

      ---------------------
      -- Parse_Next_Char --
      ---------------------

      procedure Parse_Next_Char
        (Index : in out Natural;
         Char  : out Character)
      is
         Int : Natural;
      begin
         --  Special characters are represented as ["00"] or ["""]
         --  Note that we can have '[" ' that represents the character
         --  '[' followed by the end of the string

         if Index + 4 <= Type_Str'Last
           and then Type_Str (Index) = '['
           and then Type_Str (Index + 1) = '"'
           and then (Type_Str (Index + 2) = '"'
                     or else Type_Str (Index + 2) in '0' .. '9'
                     or else Type_Str (Index + 2) in 'a' .. 'f')
         then
            if Type_Str (Index + 2) = '"' then
               Index := Index + 5;
               Char := '"';

            else
               if Type_Str (Index + 2) in 'a' .. 'f' then
                  Int := 16 * (Character'Pos (Type_Str (Index + 2))
                               - Character'Pos ('a') + 10);
               else
                  Int := 16 * (Character'Pos (Type_Str (Index + 2))
                               - Character'Pos ('0'));
               end if;

               if Type_Str (Index + 3) in 'a' .. 'f' then
                  Int := Int + Character'Pos (Type_Str (Index + 3))
                    - Character'Pos ('a') + 10;
               else
                  Int := Int + Character'Pos (Type_Str (Index + 3))
                    - Character'Pos ('0');
               end if;

               Char  := Character'Val (Int);
               Index := Index + 6;
            end if;

         --  Else, a standard character.

         else
            Char := Type_Str (Index);
            Index := Index + 1;
         end if;
      end Parse_Next_Char;


      S_Index   : Natural := Str'First;
      Char      : Character;
      Num       : Long_Integer;
      In_String : Boolean;
      Last      : Natural := Str'Last;

   begin  --  Parse_Cst_String
      if Str'Length = 0 then
         Last := Natural'Last;
      end if;

      In_String := Type_Str (Index) = '"';
      Index := Index + 1;

      --  Note: this is a slightly complex loop, since a string might not
      --  appear as a single string in gdb, but can be made of multiple
      --  elements, including characters repeated a number of times, as in:
      --  "["af"]["c7"]", '["00"]' <repeats 12 times>, "BA"

      while S_Index <= Last
        and then Index <= Type_Str'Last
        and then Type_Str (Index) /= ASCII.LF
      loop
         case Type_Str (Index) is
            when '"' =>
               In_String := not In_String;
               Index := Index + 1;

               --  In cases like {field = 0x8048f88 "bar"}, we need to consider
               --  the string finished.
               if not In_String
                 and then Index <= Type_Str'Last
                 and then Type_Str (Index) /= ' '
               then
                  Index := Index + 1;
                  return;
               end if;

            when ''' =>
               if In_String then
                  if Str'Length /= 0 then
                     Str (S_Index) := ''';
                  end if;

                  S_Index := S_Index + 1;
                  Index := Index + 1;
               else
                  Index := Index + 1;  --  skips initial '''
                  Parse_Next_Char (Index, Char);

                  if Str'Length /= 0 then
                     Str (S_Index) := Char;
                  end if;

                  Index := Index + 2;     --  skips "' " at the end

                  if Looking_At (Type_Str, Index, "<repeats ") then
                     Index := Index + 9;
                     Parse_Num (Type_Str, Index, Num);

                     if Str'Length /= 0 then
                        Str (S_Index .. S_Index + Integer (Num) - 1) :=
                          (others => Char);
                     end if;

                     S_Index := S_Index + Integer (Num);
                     Index := Index + 7; --  skips " times>"

                  else
                     S_Index := S_Index + 1;
                  end if;
               end if;

            when '\' =>
               if Backslash_Special then
                  if Str'Length /= 0 then
                     Str (S_Index) := Type_Str (Index + 1);
                     S_Index := S_Index + 1;
                  end if;

                  Index := Index + 2;

               else
                  Str (S_Index) := Type_Str (Index);
                  S_Index := S_Index + 1;
                  Index := Index + 1;
               end if;

            when ' ' | ',' =>
               if In_String then
                  if Str'Length /= 0 then
                     Str (S_Index) := ' ';
                  end if;

                  S_Index := S_Index + 1;

               --  ',' is still part of the string output only if it is
               --  followed by a constant string or character (repeats).
               --  Otherwise, ',' simply denotes the end of a struct field,
               --  as in "field3 = "ab", field4 = 1"

               elsif Type_Str (Index) = ','
                 and then
                 (Index >= Type_Str'Last - 1
                  or else (Type_Str (Index + 2) /= '''
                           and then Type_Str (Index + 2) /= '"'))
               then
                  Index := Index + 1;
                  return;
               end if;

               Index := Index + 1;

            when others =>
               Parse_Next_Char (Index, Char);

               if Str'Length /= 0 then
                  Str (S_Index) := Char;
               end if;

               S_Index := S_Index + 1;
         end case;
      end loop;

      Index := Index + 1;
   end Parse_Cst_String;

   -----------------------
   -- Skip_Simple_Value --
   -----------------------

   procedure Skip_Simple_Value
     (Type_Str             : in String;
      Index                : in out Natural;
      Array_Item_Separator : in Character := ',';
      End_Of_Array         : in Character := ')';
      Repeat_Item_Start    : in Character := '<') is
   begin
      while Index <= Type_Str'Last
        and then Type_Str (Index) /= Array_Item_Separator
        and then Type_Str (Index) /= End_Of_Array
        and then Type_Str (Index) /= ASCII.LF --  always the end of a field
        and then Type_Str (Index) /= Repeat_Item_Start
      loop
         Index := Index + 1;
      end loop;
   end Skip_Simple_Value;

   --------------------
   -- Base_File_Name --
   --------------------

   function Base_File_Name (File_Name : String) return String is
      Last : Natural := File_Name'Last;
   begin
      --  Maybe we should also ignore drive letters

      while Last >= File_Name'First loop
         exit when Is_Directory_Separator (File_Name (Last));

         Last := Last - 1;
      end loop;

      return File_Name (Last + 1 .. File_Name'Last);
   end Base_File_Name;

   ---------------
   -- Skip_Word --
   ---------------

   procedure Skip_Word
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1)
   is
      Initial  : constant Natural := Index;
   begin
      while Index <= Type_Str'Last
        and then Index >= Type_Str'First
        and then (Is_Alphanumeric (Type_Str (Index))
                  or else
                  Type_Str (Index) = '_')
      loop
         Index := Index + Step;
      end loop;

      --  Move at least one character

      if Index = Initial then
         Index := Index + Step;
      end if;
   end Skip_Word;

   ------------
   -- Reduce --
   ------------

   function Reduce (S : String) return String is
      Result : String (S'Range);
      Len    : Positive := Result'First;
      Blank  : Boolean  := False;

   begin
      for J in S'Range loop
         if S (J) = ASCII.LF or else S (J) = ASCII.HT or else S (J) = ' ' then
            if not Blank then
               Result (Len) := ' ';
               Len := Len + 1;
               Blank := True;
            end if;

         else
            Blank := False;
            Result (Len) := S (J);
            Len := Len + 1;
         end if;
      end loop;

      return Result (Result'First .. Len - 1);
   end Reduce;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Text : String) return String is
      To       : String (1 .. Text'Length);
      Index_To : Positive := 1;

   begin
      for Index in Text'Range loop
         if Text (Index) /= ASCII.CR then
            To (Index_To) := Text (Index);
            Index_To := Index_To + 1;
         end if;
      end loop;

      return To (1 .. Index_To - 1);
   end Strip_CR;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (File_Name : String) return String is
      Base : constant String := Base_File_Name (File_Name);
   begin
      for J in reverse Base'Range loop
         if Base (J) = '.' then
            return Base (J + 1 .. Base'Last);
         end if;
      end loop;

      return "";
   end File_Extension;

   ----------------------
   -- To_Host_Pathname --
   ----------------------

   function To_Host_Pathname (Path : String) return String is
      Result : String (Path'Range);
      Cygdrv : constant String := "/cygdrive/";
   begin
      if GNAT.OS_Lib.Directory_Separator = '/' then
         return Path;
      end if;

      --  Replace /cygdrive/x/ by x:\

      if Path'Length > Cygdrv'Length + 1
        and then Path (Path'First .. Path'First + Cygdrv'Length - 1) = Cygdrv
        and then Path (Path'First + Cygdrv'Length + 1) = '/'
      then
         return
           To_Host_Pathname
             (Path (Path'First + Cygdrv'Length) & ":\" &
              Path (Path'First + Cygdrv'Length + 2 .. Path'Last));

      --  Replace //x/ by x:\
      elsif Path'Length >= 4
        and then Path (Path'First) = '/'
        and then Path (Path'First + 1) = '/'
        and then Path (Path'First + 3) = '/'
      then
         return
           To_Host_Pathname
             (Path (Path'First + 2) & ":\" &
              Path (Path'First + 4 .. Path'Last));
      end if;

      for J in Result'Range loop
         if Path (J) = '/' then
            Result (J) := GNAT.OS_Lib.Directory_Separator;
         else
            Result (J) := Path (J);
         end if;
      end loop;

      return Result;
   end To_Host_Pathname;

   ----------------------
   -- To_Unix_Pathname --
   ----------------------

   function To_Unix_Pathname (Path : String) return String is
      Result : String (Path'Range);
   begin
      if GNAT.OS_Lib.Directory_Separator = '/' then
         return Path;
      end if;

      for J in Result'Range loop
         if Path (J) = GNAT.OS_Lib.Directory_Separator then
            Result (J) := '/';
         else
            Result (J) := Path (J);
         end if;
      end loop;

      return Result;
   end To_Unix_Pathname;

   ----------------------
   -- Do_Tab_Expansion --
   ----------------------

   function Do_Tab_Expansion
     (Text     : String;
      Tab_Size : Integer) return String
   is
      Num_Tabs : Natural := 0;
      Col      : Integer := 1;

   begin
      --  Count the number of tabs in the string

      for K in Text'Range loop
         if Text (K) = ASCII.HT then
            Num_Tabs := Num_Tabs + 1;
         end if;
      end loop;

      if Num_Tabs = 0 then
         return Text;
      else
         declare
            S       : String (1 .. Num_Tabs * Tab_Size + Text'Length);
            S_Index : Integer := 1;
            Bound   : Integer;

         begin
            for K in Text'Range loop
               case Text (K) is
                  when ASCII.LF =>
                     S (S_Index) := Text (K);
                     S_Index := S_Index + 1;
                     Col := 1;

                  when ASCII.HT =>
                     if Col mod Tab_Size /= 0 then
                        Bound := (1 + Col / Tab_Size) * Tab_Size - Col + 1;
                        S (S_Index .. S_Index + Bound - 1) := (others => ' ');
                        S_Index := S_Index + Bound;
                        Col := Col + Bound;

                     else
                        S (S_Index) := ' ';
                        S_Index := S_Index + 1;
                        Col := Col + 1;
                     end if;

                  when others =>
                     S (S_Index) := Text (K);
                     S_Index := S_Index + 1;
                     Col := Col + 1;
               end case;
            end loop;

            return S (S'First .. S_Index - 1);
         end;
      end if;
   end Do_Tab_Expansion;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Name : String) return String is
      Result : String (1 .. Name'Length) := To_Lower (Name);
   begin
      for J in Result'First .. Result'Last loop
         if Result (J) = '.' then
               Result (J) := '-';
         end if;
      end loop;

      return Result;
   end To_File_Name;

   ----------------
   -- Mixed_Case --
   ----------------

   procedure Mixed_Case (S : in out String) is
      Dot : Boolean := False;
   begin
      S (S'First) := To_Upper (S (S'First));

      for J in S'First + 1 .. S'Last loop
         if Dot or else S (J - 1) = '_' then
            S (J) := To_Upper (S (J));
         else
            S (J) := To_Lower (S (J));
         end if;

         if S (J) = '.' then
            Dot := True;
         elsif S (J) /= ' '
           and then S (J) /= ASCII.HT
           and then S (J) /= ASCII.LF
           and then S (J) /= ASCII.CR
         then
            Dot := False;
         end if;
      end loop;
   end Mixed_Case;

   ----------------
   -- Lower_Case --
   ----------------

   procedure Lower_Case (S : in out String) is
   begin
      for J in S'Range loop
         S (J) := To_Lower (S (J));
      end loop;
   end Lower_Case;

   ------------------
   -- Strip_Quotes --
   ------------------

   function Strip_Quotes (S : in String) return String is
      S_First : Integer := S'First;
      S_Last  : Integer := S'Last;

   begin
      if S = "" then
         return "";
      end if;

      while S (S_First) = ' ' or else S (S_First) = '"' loop
         S_First := S_First + 1;
      end loop;

      while S (S_Last) = ' ' or else S (S_Last) = '"' loop
         S_Last := S_Last - 1;
      end loop;

      return S (S_First .. S_Last);
   end Strip_Quotes;

   -----------
   -- Image --
   -----------

   function Image (N : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (N), Ada.Strings.Left);
   end Image;

   function Image (N : Integer; Length : Positive) return String is
      Pad         : constant Character := ' ';
      Small_Image : constant String := Image (N);

   begin
      if Small_Image'Length >= Length then
         return Small_Image;
      else
         declare
            Padded_Image : String (1 .. Length);
         begin
            for Index in 1 .. Length - Small_Image'Length loop
               Padded_Image (Index) := Pad;
            end loop;

            Padded_Image
              (Length - Small_Image'Length + 1 ..  Length) :=
              Small_Image;

            return Padded_Image;
         end;
      end if;
   end Image;

   ----------------------
   -- Number_Of_Digits --
   ----------------------

   function Number_Of_Digits (N : Integer) return Natural is
   begin
      case N is
         when 0 .. 9 =>
            return 1;
         when 10 .. 99 =>
            return 2;
         when 100 .. 999 =>
            return 3;
         when 1_000 .. 9_999 =>
            return 4;
         when 10_000 .. 99_999 =>
            return 5;
         when others =>
            return Image (N)'Length;
      end case;
   end Number_Of_Digits;

   --------------------
   -- Suffix_Matches --
   --------------------

   function Suffix_Matches
     (File_Name : String; Suffix : String) return Boolean is
   begin
      return Tail (File_Name, Suffix'Length) = Suffix;
   end Suffix_Matches;

   -----------------------
   -- Name_As_Directory --
   -----------------------

   function Name_As_Directory
     (Name  : String;
      Style : Path_Style := System_Default) return String
   is
      Dir : constant String := Format_Pathname (Name, Style);

   begin
      if Dir = "" then
         return "";

      elsif Style = UNIX
        and then Dir (Dir'Last) /= '/'
      then
         return Dir & '/';

      elsif Style = DOS
        and then Dir (Dir'Last) /= '\'
      then
         return Dir & '\';

      elsif Style = System_Default
        and then Dir (Dir'Last) /= Dir_Separator
      then
         return Dir & Dir_Separator;

      else
         return Dir;
      end if;
   end Name_As_Directory;

   ------------------
   -- Is_Word_Char --
   ------------------

   function Is_Word_Char (C : Character) return Boolean is
   begin
      return C = '_' or else Is_Alphanumeric (C);
   end Is_Word_Char;

   ---------------------------
   -- My_Normalize_Pathname --
   ---------------------------

   function My_Normalize_Pathname
     (Name          : String;
      Directory     : String := "";
      Resolve_Links : Boolean := True)
      return      String
   is
      Max_Path : Integer;
      pragma Import (C, Max_Path, "max_path_len");
      --  Maximum length of a path name

      procedure Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Get_Current_Dir, "__gnat_get_current_dir");

      Path_Buffer : String (1 .. Max_Path + Max_Path + 2);
      End_Path    : Natural := 0;
      Link_Buffer : String (1 .. Max_Path + 2);
      Status      : Integer;
      Last        : Positive;
      Start       : Natural;
      Finish      : Positive;

      Max_Iterations : constant := 500;

      function Readlink
        (Path   : System.Address;
         Buf    : System.Address;
         Bufsiz : Integer)
         return   Integer;
      pragma Import (C, Readlink, "__gnat_readlink");

      function To_Canonical_File_Spec
        (Host_File : System.Address)
         return      System.Address;
      pragma Import
        (C, To_Canonical_File_Spec, "__gnat_to_canonical_file_spec");

      The_Name : String (1 .. Name'Length + 1);
      Canonical_File_Addr : System.Address;
      Canonical_File_Len  : Integer;

      Need_To_Check_Drive_Letter : Boolean := False;
      --  Set to true if Name is an absolute path that starts with "//"

      function Strlen (S : System.Address) return Integer;
      pragma Import (C, Strlen, "strlen");

      function Get_Directory return String;
      --  If Directory is not empty, return it, adding a directory separator
      --  if not already present, otherwise return current working directory
      --  with terminating directory separator.

      function Final_Value (S : String) return String;
      --  Make final adjustment to the returned string.
      --  To compensate for non standard path name in Interix,
      --  if S is "/x" or starts with "/x", where x is a capital
      --  letter 'A' to 'Z', add an additional '/' at the beginning
      --  so that the returned value starts with "//x".

      -------------------
      -- Get_Directory --
      -------------------

      function Get_Directory return String is
      begin
         --  Directory given, add directory separator if needed

         if Directory'Length > 0 then
            if Directory (Directory'Length) = Directory_Separator then
               return Directory;
            else
               declare
                  Result : String (1 .. Directory'Length + 1);

               begin
                  Result (1 .. Directory'Length) := Directory;
                  Result (Result'Length) := Directory_Separator;
                  return Result;
               end;
            end if;

         --  Directory name not given, get current directory

         else
            declare
               Buffer   : String (1 .. Max_Path + 2);
               Path_Len : Natural := Max_Path;

            begin
               Get_Current_Dir (Buffer'Address, Path_Len'Address);

               if Buffer (Path_Len) /= Directory_Separator then
                  Path_Len := Path_Len + 1;
                  Buffer (Path_Len) := Directory_Separator;
               end if;

               return Buffer (1 .. Path_Len);
            end;
         end if;
      end Get_Directory;

      Reference_Dir : constant String := Get_Directory;
      --  Current directory name specified

      -----------------
      -- Final_Value --
      -----------------

      function Final_Value (S : String) return String is
      begin
         --  Interix has the non standard notion of disk drive
         --  indicated by two '/' followed by a capital letter
         --  'A' .. 'Z'. One of the two '/' may have been removed
         --  by Normalize_Pathname. It has to be added again.
         --  For other OSes, this should not make no difference.

         if Need_To_Check_Drive_Letter
           and then S'Length >= 2
           and then S (S'First) = '/'
           and then S (S'First + 1) in 'A' .. 'Z'
           and then (S'Length = 2 or else S (S'First + 2) = '/')
         then
            declare
               Result : String (1 .. S'Length + 1);

            begin
               Result (1) := '/';
               Result (2 .. Result'Last) := S;
               return Result;
            end;

         else
            return S;
         end if;

      end Final_Value;

   --  Start of processing for Normalize_Pathname

   begin
      --  Special case, if name is null, then return null

      if Name'Length = 0 then
         return "";
      end if;

      --  First, convert VMS file spec to Unix file spec.
      --  If Name is not in VMS syntax, then this is equivalent
      --  to put Name at the begining of Path_Buffer.

      VMS_Conversion : begin
         The_Name (1 .. Name'Length) := Name;
         The_Name (The_Name'Last) := ASCII.NUL;

         Canonical_File_Addr := To_Canonical_File_Spec (The_Name'Address);
         Canonical_File_Len  := Strlen (Canonical_File_Addr);

         --  If VMS syntax conversion has failed, return an empty string
         --  to indicate the failure.

         if Canonical_File_Len = 0 then
            return "";
         end if;

         declare
            subtype Path_String is String (1 .. Canonical_File_Len);
            type    Path_String_Access is access Path_String;

            function Address_To_Access is new
               Unchecked_Conversion (Source => Address,
                                     Target => Path_String_Access);

            Path_Access : constant Path_String_Access :=
                            Address_To_Access (Canonical_File_Addr);

         begin
            Path_Buffer (1 .. Canonical_File_Len) := Path_Access.all;
            End_Path := Canonical_File_Len;
            Last := 1;
         end;
      end VMS_Conversion;

      --  Replace all '/' by Directory Separators (this is for Windows)

      if Directory_Separator /= '/' then
         for Index in 1 .. End_Path loop
            if Path_Buffer (Index) = '/' then
               Path_Buffer (Index) := Directory_Separator;
            end if;
         end loop;
      end if;

      --  Start the conversions

      --  If this is not finished after Max_Iterations, give up and
      --  return an empty string.

      for J in 1 .. Max_Iterations loop

         --  If we don't have an absolute pathname, prepend
         --  the directory Reference_Dir.

         if Last = 1
           and then not Is_Absolute_Path (Path_Buffer (1 .. End_Path))
         then
            Path_Buffer
              (Reference_Dir'Last + 1 .. Reference_Dir'Length + End_Path) :=
                 Path_Buffer (1 .. End_Path);
            End_Path := Reference_Dir'Length + End_Path;
            Path_Buffer (1 .. Reference_Dir'Length) := Reference_Dir;
            Last := Reference_Dir'Length;
         end if;

         --  If name starts with "//", we may have a drive letter on Interix

         if Last = 1 and then End_Path >= 3 then
            Need_To_Check_Drive_Letter := (Path_Buffer (1 .. 2)) = "//";
         end if;

         Start  := Last + 1;
         Finish := Last;

         --  If we have traversed the full pathname, return it

         if Start > End_Path then
            return Final_Value (Path_Buffer (1 .. End_Path));
         end if;

         --  Remove duplicate directory separators

         while Path_Buffer (Start) = Directory_Separator loop
            if Start = End_Path then
               return Final_Value (Path_Buffer (1 .. End_Path - 1));

            else
               Path_Buffer (Start .. End_Path - 1) :=
                 Path_Buffer (Start + 1 .. End_Path);
               End_Path := End_Path - 1;
            end if;
         end loop;

         --  Find the end of the current field: last character
         --  or the one preceding the next directory separator.

         while Finish < End_Path
           and then Path_Buffer (Finish + 1) /= Directory_Separator
         loop
            Finish := Finish + 1;
         end loop;

         --  Remove "." field

         if Start = Finish and then Path_Buffer (Start) = '.' then
            if Start = End_Path then
               if Last = 1 then
                  return (1 => Directory_Separator);
               else
                  return Path_Buffer (1 .. Last - 1);
               end if;

            else
               Path_Buffer (Last + 1 .. End_Path - 2) :=
                 Path_Buffer (Last + 3 .. End_Path);
               End_Path := End_Path - 2;
            end if;

         --  Remove ".." fields

         elsif Finish = Start + 1
           and then Path_Buffer (Start .. Finish) = ".."
         then
            Start := Last;
            loop
               Start := Start - 1;
               exit when Start < 1 or else
                 Path_Buffer (Start) = Directory_Separator;
            end loop;

            if Start <= 1 then
               if Finish = End_Path then
                  return (1 => Directory_Separator);

               else
                  Path_Buffer (1 .. End_Path - Finish) :=
                    Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - Finish;
                  Last := 1;
               end if;

            else
               if Finish = End_Path then
                  return Final_Value (Path_Buffer (1 .. Start - 1));

               else
                  Path_Buffer (Start + 1 .. Start + End_Path - Finish - 1) :=
                    Path_Buffer (Finish + 2 .. End_Path);
                  End_Path := Start + End_Path - Finish - 1;
                  Last := Start;
               end if;
            end if;

         --  Check if current field is a symbolic link

         elsif Resolve_Links then
            declare
               Saved : constant Character := Path_Buffer (Finish + 1);

            begin
               Path_Buffer (Finish + 1) := ASCII.NUL;
               Status := Readlink (Path_Buffer'Address,
                                   Link_Buffer'Address,
                                   Link_Buffer'Length);
               Path_Buffer (Finish + 1) := Saved;
            end;

            --  Not a symbolic link, move to the next field, if any

            if Status <= 0 then
               Last := Finish + 1;

            --  Replace symbolic link with its value.

            else
               if Is_Absolute_Path (Link_Buffer (1 .. Status)) then
                  Path_Buffer (Status + 1 .. End_Path - (Finish - Status)) :=
                  Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - (Finish - Status);
                  Path_Buffer (1 .. Status) := Link_Buffer (1 .. Status);
                  Last := 1;

               else
                  Path_Buffer
                    (Last + Status + 1 .. End_Path - Finish + Last + Status) :=
                    Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - Finish + Last + Status;
                  Path_Buffer (Last + 1 .. Last + Status) :=
                    Link_Buffer (1 .. Status);
               end if;
            end if;

         else
            Last := Finish + 1;
         end if;
      end loop;

      --  Too many iterations: give up

      --  This can happen when there is a circularity in the symbolic links:
      --  A is a symbolic link for B, which itself is a symbolic link, and
      --  the target of B or of another symbolic link target of B is A.
      --  In this case, we return an empty string to indicate failure to
      --  resolve.

      return "";
   end My_Normalize_Pathname;

   ------------------------
   -- Relative_Path_Name --
   ------------------------

   function Relative_Path_Name
     (File_Name : String; Base_Name : String) return String
   is
      Base       : constant String := Name_As_Directory
        (My_Normalize_Pathname (Base_Name, Resolve_Links => False));
      File       : constant String :=
        My_Normalize_Pathname (File_Name, Resolve_Links => False);
      Level      : Natural := 0;
      Base_End   : Natural := Base'Last;
      Length     : Natural;
      Parent_Dir : constant String := ".." & Directory_Separator;

   begin
      if File = Base then
         return ".";
      end if;

      while Base_End >= Base'First loop
         Length := Base_End - Base'First + 1;

         if File'Length >= Length
           and then File
           (File'First .. File'First + Length - 1) = Base
           (Base'First .. Base_End)
         then
            return (Level * Parent_Dir) & File
              (File'First + Length .. File'Last);

         --  Else try without the last directory separator
         elsif File'Length = Length - 1
           and then File = Base (Base'First .. Base_End - 1)
         then
            return (Level * Parent_Dir) & File
              (File'First + Length .. File'Last);
         end if;

         --  Look for the parent directory.
         Level := Level + 1;
         loop
            Base_End := Base_End - 1;
            exit when Base_End < Base'First
              or else Base (Base_End) = Directory_Separator;
         end loop;
      end loop;

      return File;
   end Relative_Path_Name;

end String_Utils;
