-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body String_Utils is

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

   exception
      when Constraint_Error =>
         Result := Long_Integer'Last;
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
      if In_String then
         Index := Index + 1;
      end if;

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
               --  the string finished, but not for
               --     "bar", 'cd' <repeats 12 times>
               if not In_String
                 and then Index <= Type_Str'Last
                 and then Type_Str (Index) /= ' '
                 and then Type_Str (Index) /= ','
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
         if S (J) = ASCII.LF or else S (J) = ASCII.CR
           or else S (J) = ASCII.HT or else S (J) = ' '
         then
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

   --------------
   --  Shorten --
   --------------

   function Shorten
     (Path    : String;
      Max_Len : Natural := 40) return String
   is
      Len : constant Natural := Path'Length;
   begin
      if Len <= Max_Len then
         return Path;
      else
         declare
            Prefix       : constant String  := "[...]";
            Search_Start : constant Natural
              := Path'Last - Max_Len + Prefix'Length;
            New_Start    : Natural;
         begin
            if Search_Start > Path'Last then
               --  Max_Len < Prefix'Length
               --  Shorten anyway, but might give a strange result
               return Path (Path'Last - Max_Len .. Path'Last);
            end if;

            New_Start := Index (Path (Search_Start .. Path'Last), "/");

            if New_Start = 0 and New_Start not in Path'Range then
               --  Shorten anyway (but it might not make sense)
               New_Start := Search_Start;
            end if;

            return (Prefix & Path (New_Start .. Path'Last));
         end;
      end if;
   end Shorten;

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

      while S_First <= S'Last
        and then (S (S_First) = ' ' or else S (S_First) = '"')
      loop
         S_First := S_First + 1;
      end loop;

      while S_Last >= S'First
        and then (S (S_Last) = ' ' or else S (S_Last) = '"')
      loop
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

   ----------------------
   -- Is_Entity_Letter --
   ----------------------

   function Is_Entity_Letter (Char : Character) return Boolean is
   begin
      return Char = '_' or else Is_Alphanumeric (Char);
   end Is_Entity_Letter;

   ------------------------
   -- Is_Operator_Letter --
   ------------------------

   function Is_Operator_Letter (Char : Character) return Boolean is
   begin
      case Char is
         when '<' | '=' | '>' | '+' | '-' | '*' | '/' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Operator_Letter;

   ------------------------
   -- Relative_Path_Name --
   ------------------------

   function Relative_Path_Name
     (File_Name : String; Base_Name : String) return String
   is
      Base       : constant String := Name_As_Directory
        (Normalize_Pathname (Base_Name, Resolve_Links => False));
      File       : constant String :=
        Normalize_Pathname (File_Name, Resolve_Links => False);
      Level      : Natural := 0;
      Base_End   : Natural := Base'Last;
      Length     : Natural;
      Parent_Dir : constant String := ".." & Directory_Separator;

   begin
      if File = Base or else File = Base (Base'First .. Base'Last - 1) then
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

   -----------------
   -- Copy_String --
   -----------------

   procedure Copy_String
     (Item : Interfaces.C.Strings.chars_ptr;
      Str  : out String;
      Len  : Natural)
   is
      procedure Strncpy
        (Dest : out String;
         Src  : Interfaces.C.Strings.chars_ptr;
         Len  : Interfaces.C.size_t);
      pragma Import (C, Strncpy, "strncpy");

   begin
      Strncpy (Str, Item, Interfaces.C.size_t (Len));
   end Copy_String;

   ----------------------------
   -- Case_Insensitive_Equal --
   ----------------------------

   function Case_Insensitive_Equal (S1, S2 : String) return Boolean is
      J1 : Natural;
      J2 : Natural;
   begin
      if S1'Length /= S2'Length then
         return False;
      end if;

      J1 := S1'First;
      J2 := S2'First;

      while J1 <= S1'Last loop
         if To_Lower (S1 (J1)) /= To_Lower (S2 (J2)) then
            return False;
         end if;

         J1 := J1 + 1;
         J2 := J2 + 1;
      end loop;

      return True;
   end Case_Insensitive_Equal;

   -----------------------------
   -- Argument_List_To_String --
   -----------------------------

   function Argument_List_To_String
     (List : GNAT.OS_Lib.Argument_List) return String
   is
      Length : Natural := 0;
   begin
      for L in List'Range loop
         Length := Length + List (L)'Length + 1;
         for S in List (L)'Range loop
            if List (L)(S) = '"'
              or else List (L)(S) = ' '
            then
               Length := Length + 1;
            end if;
         end loop;
      end loop;

      declare
         S : String (1 .. Length);
         Index : Positive := S'First;
      begin
         for L in List'Range loop
            for J in List (L)'Range loop
               if List (L)(J) = '"' or else List (L)(J) = ' ' then
                  S (Index) := '\';
                  Index := Index + 1;
               end if;
               S (Index) := List (L)(J);
               Index := Index + 1;
            end loop;
            S (Index) := ' ';
            Index := Index + 1;
         end loop;
         return S;
      end;
   end Argument_List_To_String;

   -----------
   -- Clone --
   -----------

   function Clone (List : GNAT.OS_Lib.Argument_List)
      return GNAT.OS_Lib.Argument_List
   is
      L : Argument_List (List'Range);
   begin
      for J in List'Range loop
         L (J) := new String'(List (J).all);
      end loop;
      return L;
   end Clone;

   ------------
   -- Append --
   ------------

   procedure Append (List  : in out GNAT.OS_Lib.Argument_List_Access;
                     List2 : GNAT.OS_Lib.Argument_List)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);
      L : Argument_List_Access := List;
   begin
      if List = null then
         List := new Argument_List (1 .. List2'Length);
      else
         List := new Argument_List (L'First .. L'Last + List2'Length);
         List (L'Range) := L.all;
         Unchecked_Free (L);
      end if;

      List (List'Last - List2'Length + 1 .. List'Last) := List2;
   end Append;

   -----------
   -- Start --
   -----------

   function Start (Path : String) return Path_Iterator is
   begin
      return Next (Path, (First => 0, Last => Path'First - 1));
   end Start;

   ----------
   -- Next --
   ----------

   function Next (Path : String; Iter : Path_Iterator) return Path_Iterator is
      Pos : Natural := Iter.Last + 1;
   begin
      while Pos <= Path'Last
        and then Path (Pos) /= Path_Separator
      loop
         Pos := Pos + 1;
      end loop;

      return (First => Iter.Last + 1, Last => Pos);
   end Next;

   -------------
   -- Current --
   -------------

   function Current (Path : String; Iter : Path_Iterator) return String is
   begin
      if Iter.First <= Path'Last then
         return Path (Iter.First .. Iter.Last - 1);
      else
         return "";
      end if;
   end Current;

   ----------
   -- Free --
   ----------

   procedure Free (Substrings : in out Substitution_Array) is
   begin
      for S in Substrings'Range loop
         Free (Substrings (S).Name);
         Free (Substrings (S).Value);
      end loop;
   end Free;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Str               : String;
      Substitution_Char : Character;
      Substrings        : Substitution_Array;
      Recursive         : Boolean := False) return String
   is
      Result : Unbounded_String;
      First, Last : Natural := Str'First;
      Found : Boolean;
   begin
      while First <= Str'Last loop
         Last := First;
         while Last <= Str'Last
           and then Str (Last) /= Substitution_Char
         loop
            Last := Last + 1;
         end loop;

         Result := Result & Str (First .. Last - 1);

         exit when Last > Str'Last;

         Found := False;

         for S in Substrings'Range loop
            if Last + Substrings (S).Name'Length <= Str'Last
              and then Substrings (S).Name.all =
              Str (Last + 1 .. Last + Substrings (S).Name'Length)
            then
               if Recursive then
                  Result := Result & Substitute
                    (Substrings (S).Value.all,
                     Substitution_Char, Substrings, Recursive);
               else
                  Result := Result & Substrings (S).Value.all;
               end if;

               Found := True;
               First := Last + Substrings (S).Name'Length + 1;
               exit;
            end if;
         end loop;

         if not Found then
            Result := Result & Str (Last);
            First := Last + 1;
         end if;

      end loop;

      return To_String (Result);
   end Substitute;

end String_Utils;
