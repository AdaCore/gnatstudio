-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Ada.Unchecked_Deallocation;
with Basic_Types; use Basic_Types;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Language is

   type Language_Detection_Record is record
      Pattern  : String_Access;
      Language : Language_Access;
   end record;
   type Language_Detection_Array is array (Positive range <>)
     of Language_Detection_Record;
   type Language_Detection_Access is access Language_Detection_Array;

   Language_Detection : Language_Detection_Access;
   --  Global table that indicates which languages to associate with file
   --  names, so that one can find the best language associated with a
   --  given file name.
   --  Note that this variable is not thread-safe.

   ------------------------
   -- Add_File_Extension --
   ------------------------

   procedure Add_File_Extension
     (Language : Language_Access;
      Pattern  : String)
   is
      Table_Index : Positive := Positive'Last;
      Tmp         : Language_Detection_Access;
      T           : String_Access;

      procedure Free is new Ada.Unchecked_Deallocation
        (Language_Detection_Array, Language_Detection_Access);

   begin
      --  Do we already have this language in the table

      if Language_Detection /= null then
         for Index in Language_Detection'Range loop
            if Language_Detection (Index).Language.all = Language.all then
               Table_Index := Index;
               exit;
            end if;
         end loop;
      end if;

      --  If not, extend the table

      if Table_Index = Positive'Last then
         if Language_Detection = null then
            Tmp := new Language_Detection_Array (1 .. 1);
            Table_Index := 1;
         else
            Tmp := new Language_Detection_Array
              (Language_Detection'First .. Language_Detection'Last + 1);
            Tmp (Language_Detection'Range) := Language_Detection.all;
            Table_Index := Tmp'Last;
         end if;

         Free (Language_Detection);
         Language_Detection := Tmp;
      end if;

      --  Add the new item in the table
      Language_Detection (Table_Index).Language := Language;

      if Language_Detection (Table_Index).Pattern = null then
         Language_Detection (Table_Index).Pattern :=
           new String'("(" & Pattern & ")");

      else
         T := Language_Detection (Table_Index).Pattern;
         Language_Detection (Table_Index).Pattern :=
           new String'(T.all & "|(" & Pattern & ")");
         Free (T);
      end if;
   end Add_File_Extension;

   -------------------------
   -- Add_File_Extensions --
   -------------------------

   procedure Add_File_Extensions
     (Lang : Language_Access; Extensions : String)
   is
      First : Natural;
      Ind   : Natural;

      function To_Regexp (S : String) return String;
      --  Transform S into a regexp, by applying the following transformations:
      --    - "." is transformed into "\."
      --    - a "$" is appended to S.

      function To_Regexp (S : String) return String is
         Res : String (1 .. S'Length * 2 + 1);
         Ind : Positive := 1;
      begin
         for J in S'Range loop
            if S (J) /= '.' then
               Res (Ind) := S (J);
            else
               Res (Ind) := '\';
               Ind := Ind + 1;
               Res (Ind) := '.';
            end if;

            Ind := Ind + 1;
         end loop;

         Res (Ind) := '$';
         return Res (1 .. Ind);
      end To_Regexp;

   begin
      First := Extensions'First;

      loop
         Ind := Ada.Strings.Fixed.Index
           (Extensions (First .. Extensions'Last), ";");

         if Ind = 0 then
            Ind := Extensions'Last + 1;
         end if;

         Add_File_Extension (Lang, To_Regexp (Extensions (First .. Ind - 1)));

         exit when Ind > Extensions'Last;

         First := Ind + 1;
      end loop;
   end Add_File_Extensions;

   ---------------------------
   -- Can_Tooltip_On_Entity --
   ---------------------------

   function Can_Tooltip_On_Entity
     (Lang   : access Language_Root;
      Entity : String) return Boolean is
   begin
      return True;
   end Can_Tooltip_On_Entity;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Language_Root) return Explorer_Categories
   is
      E : Explorer_Categories (1 .. 0);
   begin
      return E;
   end Explorer_Regexps;

   ----------
   -- Free --
   ----------

   procedure Free (Lang : in out Language_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Language_Root'Class, Language_Access);
   begin
      Internal (Lang);
   end Free;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (File_Name : String) return Language_Access is
   begin
      if Language_Detection /= null then
         for Index in Language_Detection'Range loop
            if Match (Language_Detection (Index).Pattern.all, File_Name) then
               return Language_Detection (Index).Language;
            end if;
         end loop;
      end if;

      return null;
   end Get_Language_From_File;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang      : access Language_Root;
      File_Name : String) return Boolean is
   begin
      return False;
   end Is_System_File;

   ----------------
   -- Looking_At --
   ----------------

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      Entity    : out Language_Entity;
      Next_Char : out Positive)
   is
      Matched : Match_Array (0 .. 1);
      Context : constant Language_Context :=
        Get_Language_Context (Language_Access (Lang));
      Keys : constant Pattern_Matcher := Keywords (Language_Access (Lang));
      Comm1, Comm2 : Character;
   begin

      --  Do we have a comment ?

      if Context.Comment_Start_Length /= 0
        and then Buffer'Length > Context.Comment_Start_Length
        and then Buffer
        (Buffer'First .. Buffer'First + Context.Comment_Start_Length - 1)
        = Context.Comment_Start
      then
         Entity := Comment_Text;
         Next_Char := Buffer'First + Context.Comment_Start_Length;
         while Next_Char + Context.Comment_End_Length - 1 <= Buffer'Last
           and then Buffer
           (Next_Char .. Next_Char + Context.Comment_End_Length - 1)
           /= Context.Comment_End
         loop
            Next_Char := Next_Char + 1;
         end loop;
         Next_Char := Next_Char + Context.Comment_End_Length;
         return;
      end if;

      --  Do we have a comment that end on newline ?

      if Context.New_Line_Comment_Start_Length /= 0
        and then Buffer'Length > Context.New_Line_Comment_Start_Length
        and then Buffer
        (Buffer'First .. Buffer'First
         + Context.New_Line_Comment_Start_Length - 1)
        = Context.New_Line_Comment_Start
      then
         Entity := Comment_Text;
         Next_Char := Buffer'First + Context.New_Line_Comment_Start_Length;

         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= ASCII.LF
         loop
            Next_Char := Next_Char + 1;
         end loop;

         return;
      end if;

      --  Do we have a string ?

      if Buffer (Buffer'First) = Context.String_Delimiter then
         Entity := String_Text;
         Next_Char := Buffer'First;

         loop
            Next_Char := Next_Char + 1;
            exit when Next_Char >= Buffer'Last
              or else
                (Buffer (Next_Char) = Context.String_Delimiter
                   and then
                     (Context.Quote_Character = ASCII.NUL
                        or else
                          Buffer (Next_Char - 1) /= Context.Quote_Character));
         end loop;

         Next_Char := Next_Char + 1;
         return;
      end if;

      --  A constant character

      if Buffer'Length > 3
        and then Buffer (Buffer'First) = Context.Constant_Character
        and then Buffer (Buffer'First + 2) = Context.Constant_Character
      then
         Entity := String_Text;
         Next_Char := Buffer'First + 3;
         return;
      end if;

      --  Another special character, not part of a word: just skip it, before
      --  doing some regexp matching
      --  It is better to return a pointer to the newline, so that the icons
      --  on the side might be displayed properly.

      if not Is_Letter (Buffer (Buffer'First)) then
         Entity := Normal_Text;
         Next_Char := Buffer'First + 1;

         Comm1 := ASCII.LF;
         Comm2 := ASCII.LF;
         if Context.Comment_Start_Length /= 0 then
            Comm1 := Context.Comment_Start (Context.Comment_Start'First);
         end if;
         if Context.New_Line_Comment_Start_Length /= 0 then
            Comm2 :=
              Context.New_Line_Comment_Start (Context.Comment_Start'First);
         end if;

         while Next_Char <= Buffer'Last
           and then Buffer (Next_Char) /= ' '
           and then Buffer (Next_Char) /= ASCII.LF
           and then Buffer (Next_Char) /= ASCII.HT
           and then Buffer (Next_Char) /= Context.String_Delimiter
           and then Buffer (Next_Char) /= Comm1
           and then Buffer (Next_Char) /= Comm2
           and then Buffer (Next_Char) /= Context.Constant_Character
           and then not Is_Letter (Buffer (Next_Char))
         loop
            Next_Char := Next_Char + 1;
         end loop;

         return;
      end if;

      --  Do we have a keyword ?

      Match (Keys, Buffer, Matched);

      if Matched (0) /= No_Match then
         Next_Char := Matched (0).Last + 1;
         Entity := Keyword_Text;
         return;
      end if;

      --  If no, skip to the next meaningful character. we know we are
      --  starting with a letter

      Next_Char := Buffer'First + 1;
      Entity := Normal_Text;

      --  Skip the current word

      while Next_Char <= Buffer'Last
        and then (Is_Letter (Buffer (Next_Char))
                  or else Buffer (Next_Char) = '_')
      loop
         Next_Char := Next_Char + 1;
      end loop;
   end Looking_At;

   ---------------------------
   -- Reset_File_Extensions --
   ---------------------------

   procedure Reset_File_Extensions is
   begin
      if Language_Detection /= null then
         for J in Language_Detection'Range loop
            Free (Language_Detection (J).Pattern);
         end loop;
      end if;
   end Reset_File_Extensions;

end Language;
