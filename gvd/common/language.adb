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

with Ada.Unchecked_Deallocation;
with Basic_Types; use Basic_Types;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.IO; use GNAT.IO;

package body Language is

   ---------------------------
   -- Can_Tooltip_On_Entity --
   ---------------------------

   function Can_Tooltip_On_Entity
     (Lang   : access Language_Root;
      Entity : String) return Boolean
   is
      pragma Unreferenced (Lang, Entity);
   begin
      return True;
   end Can_Tooltip_On_Entity;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Language_Root) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
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

   procedure Free (List : in out Construct_List) is
      Info, Tmp : Construct_Access;

      procedure Free is new
        Ada.Unchecked_Deallocation (Construct_Information, Construct_Access);

   begin
      Info := List.First;

      loop
         exit when Info = null;

         Free (Info.Name);
         Free (Info.Profile);
         Tmp := Info;
         Info := Info.Next;
         Free (Tmp);
      end loop;

      List.First   := null;
      List.Current := null;
      List.Last    := null;
   end Free;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang      : access Language_Root;
      File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang, File_Name);
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
      --  Note that we consider that strings never span over multiple lines...

      if Buffer (Buffer'First) = Context.String_Delimiter then
         Entity := String_Text;
         Next_Char := Buffer'First;

         loop
            Next_Char := Next_Char + 1;
            exit when Next_Char >= Buffer'Last
              or else Buffer (Next_Char) = ASCII.LF
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

      --  A protected constant character
      --  ??? The following test still does not handle cases such as
      --  '\012' for instance, or multi-byte character constants.

      if Buffer'Length > 4
        and then Buffer (Buffer'First) = Context.Constant_Character
        and then Buffer (Buffer'First + 1) = Context.Quote_Character
        and then Buffer (Buffer'First + 3) = Context.Constant_Character
      then
         Entity := Character_Text;
         Next_Char := Buffer'First + 4;
         return;
      end if;

      --  A constant character

      if Buffer'Length > 3
        and then Buffer (Buffer'First) = Context.Constant_Character
        and then Buffer (Buffer'First + 2) = Context.Constant_Character
      then
         Entity := Character_Text;
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

      --  If not, skip to the next meaningful character. we know we are
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

   -------------------
   -- Format_Source --
   -------------------

   procedure Format_Source
     (Lang             : access Language_Root;
      Buffer           : String;
      Indent_Params    : Indent_Parameters := Default_Indent_Parameters;
      Reserved_Casing  : Casing_Type       := Lower;
      Ident_Casing     : Casing_Type       := Mixed;
      Format_Operators : Boolean           := True)
   is
      pragma Unreferenced (Lang, Indent_Params, Reserved_Casing,
                           Ident_Casing, Format_Operators);
   begin
      Put (Buffer);
   end Format_Source;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   procedure Parse_Constructs
     (Lang          : access Language_Root;
      Buffer        : String;
      Result        : out Construct_List)
   is
      Matches        : Match_Array (0 .. 10);
      Categories     : constant Explorer_Categories :=
        Explorer_Regexps (Language_Access (Lang));
      First          : Natural;
      Line           : Natural;
      Line_Pos       : Natural;
      Sloc_Entity    : Source_Location;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Info           : Construct_Access;
      Match_Index    : Natural;

      procedure Forward
        (Index : Natural;
         Sloc  : in out Source_Location);
      --  Compute Line and Column fields in Sloc and update Line and Line_Pos

      procedure Forward
        (Index : Natural;
         Sloc  : in out Source_Location) is
      begin
         for J in Index .. Sloc.Index loop
            if Buffer (J) = ASCII.LF then
               Line     := Line + 1;
               Line_Pos := J;
            end if;
         end loop;

         Sloc.Line   := Line;
         Sloc.Column := Sloc.Index - Line_Pos;
      end Forward;

   begin
      Result := (null, null, null);

      --  For each category, parse the buffer

      for C in Categories'Range loop
         First    := Buffer'First;
         Line     := 1;
         Line_Pos := 0;

         loop
            Match (Categories (C).Regexp.all,
                   Buffer (First .. Buffer'Last),
                   Matches);

            exit when Matches (0) = No_Match;

            Match_Index       := Categories (C).Position_Index;
            Sloc_Start.Index  := Matches (0).First;
            Sloc_Entity.Index := Matches (Match_Index).First;
            Sloc_End.Index    := Matches (0).Last;

            Forward (First, Sloc_Start);
            Forward (Sloc_Start.Index + 1, Sloc_Entity);
            Forward (Sloc_Entity.Index + 1, Sloc_End);

            Info           := Result.Current;
            Result.Current := new Construct_Information;

            if Result.First = null then
               Result.First := Result.Current;
            else
               Result.Current.Prev := Info;
               Result.Current.Next := Info.Next;
               Info.Next           := Result.Current;
            end if;

            Result.Last := Result.Current;
            Result.Current.Category := Categories (C).Category;

            if Categories (C).Make_Entry /= null then
               Result.Current.Name := new String'
                 (Categories (C).Make_Entry (Buffer, Matches));
            else
               Result.Current.Name := new String'
                 (Buffer (Matches (Match_Index).First ..
                          Matches (Match_Index).Last));
            end if;

            --  Result.Current.Profile := ???

            Result.Current.Sloc_Entity    := Sloc_Entity;
            Result.Current.Sloc_Start     := Sloc_Start;
            Result.Current.Sloc_End       := Sloc_End;
            Result.Current.Is_Declaration := False;

            First := Matches (0).Last + 1;
         end loop;
      end loop;
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang          : access Language_Root;
      Buffer        : String;
      Callback      : Entity_Callback)
   is
      Index     : Natural := Buffer'First;
      Next_Char : Natural;
      End_Char  : Natural;
      Entity    : Language_Entity;

   begin
      loop
         exit when Index >= Buffer'Last;

         Looking_At (Lang, Buffer (Index .. Buffer'Last), Entity, Next_Char);

         if Next_Char = Buffer'Last then
            End_Char := Buffer'Last;
         else
            End_Char := Next_Char - 1;
         end if;

         exit when Callback
           (Entity,
            (0, 0, Index),
            (0, 0, End_Char),
            Entity = Comment_Text and then Next_Char > Buffer'Last);

         Index := Next_Char;
      end loop;
   end Parse_Entities;

   ----------------------
   -- Next_Indentation --
   ----------------------

   procedure Next_Indentation
     (Lang          : access Language_Root;
      Buffer        : String;
      Success       : out Boolean;
      Indent        : out Natural;
      Next_Indent   : out Natural;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters)
   is
      pragma Unreferenced (Lang, Indent_Params);

      Index  : Natural;
      Blanks : Natural;

   begin
      Success := True;

      if Buffer'Length = 0 then
         Indent := 0;
         Next_Indent := 0;
         return;
      end if;

      Index := Buffer'Last - 1;

      while Index > 1 and then Buffer (Index - 1) /= ASCII.LF loop
         Index := Index - 1;
      end loop;

      Blanks := Index;

      while Blanks < Buffer'Last
        and then (Buffer (Blanks) = ' ' or else Buffer (Blanks) = ASCII.HT)
      loop
         Blanks := Blanks + 1;
      end loop;

      Indent      := Blanks - Index;
      Next_Indent := Indent;
   end Next_Indentation;

end Language;
