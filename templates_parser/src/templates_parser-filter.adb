------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;

with Templates_Parser.Configuration;
with Templates_Parser.Utils;

separate (Templates_Parser)
package body Filter is

   --  User's defined filter

   package Filter_Map is
     new Containers.Indefinite_Hashed_Maps
       (String, User_CB, Strings.Hash, "=", "=");

   User_Filters : Filter_Map.Map;

   --  Filter tokens

   Multiply_Token      : aliased constant String := """*""";
   Plus_Token          : aliased constant String := """+""";
   Minus_Token         : aliased constant String := """-""";
   Divide_Token        : aliased constant String := """/""";
   Abs_Token           : aliased constant String := "ABS";
   Add_Token           : aliased constant String := "ADD";
   Add_Param_Token     : aliased constant String := "ADD_PARAM";
   BR_2_EOL_Token      : aliased constant String := "BR_2_EOL";
   BR_2_LF_Token       : aliased constant String := "BR_2_LF";
   Capitalize_Token    : aliased constant String := "CAPITALIZE";
   Clean_Text_Token    : aliased constant String := "CLEAN_TEXT";
   Coma_2_Point_Token  : aliased constant String := "COMA_2_POINT";
   Contract_Token      : aliased constant String := "CONTRACT";
   Del_Param_Token     : aliased constant String := "DEL_PARAM";
   Div_Token           : aliased constant String := "DIV";
   Exist_Token         : aliased constant String := "EXIST";
   File_Exists_Token   : aliased constant String := "FILE_EXISTS";
   Format_Date_Token   : aliased constant String := "FORMAT_DATE";
   Format_Number_Token : aliased constant String := "FORMAT_NUMBER";
   Is_Empty_Token      : aliased constant String := "IS_EMPTY";
   LF_2_BR_Token       : aliased constant String := "LF_2_BR";
   Lower_Token         : aliased constant String := "LOWER";
   Match_Token         : aliased constant String := "MATCH";
   Max_Token           : aliased constant String := "MAX";
   Min_Token           : aliased constant String := "MIN";
   Modulo_Token        : aliased constant String := "MOD";
   Mult_Token          : aliased constant String := "MULT";
   Neg_Token           : aliased constant String := "NEG";
   No_Digit_Token      : aliased constant String := "NO_DIGIT";
   No_Dynamic_Token    : aliased constant String := "NO_DYNAMIC";
   No_Letter_Token     : aliased constant String := "NO_LETTER";
   No_Space_Token      : aliased constant String := "NO_SPACE";
   Oui_Non_Token       : aliased constant String := "OUI_NON";
   Point_2_Coma_Token  : aliased constant String := "POINT_2_COMA";
   Repeat_Token        : aliased constant String := "REPEAT";
   Replace_Token       : aliased constant String := "REPLACE";
   Replace_All_Token   : aliased constant String := "REPLACE_ALL";
   Replace_Param_Token : aliased constant String := "REPLACE_PARAM";
   Reverse_Token       : aliased constant String := "REVERSE";
   Size_Token          : aliased constant String := "SIZE";
   Slice_Token         : aliased constant String := "SLICE";
   Sub_Token           : aliased constant String := "SUB";
   Trim_Token          : aliased constant String := "TRIM";
   Upper_Token         : aliased constant String := "UPPER";
   User_Defined_Token  : aliased constant String := "USER_DEFINED";
   Web_Encode_Token    : aliased constant String := "WEB_ENCODE";
   Web_Escape_Token    : aliased constant String := "WEB_ESCAPE";
   Web_NBSP_Token      : aliased constant String := "WEB_NBSP";
   Wrap_Token          : aliased constant String := "WRAP";
   Yes_No_Token        : aliased constant String := "YES_NO";

   --  Filters Table

   Table : constant array (Mode) of Filter_Record
     := (Multiply       =>
           (Multiply_Token'Access,       Multiply'Access),

         Plus           =>
           (Plus_Token'Access,           Plus'Access),

         Minus          =>
           (Minus_Token'Access,          Minus'Access),

         Divide         =>
           (Divide_Token'Access,         Divide'Access),

         Absolute       =>
           (Abs_Token'Access,            Absolute'Access),

         Add            =>
           (Add_Token'Access,            Plus'Access),

         Add_Param      =>
           (Add_Param_Token'Access,      Add_Param'Access),

         BR_2_EOL       =>
           (BR_2_EOL_Token'Access,       BR_2_EOL'Access),

         BR_2_LF        =>
           (BR_2_LF_Token'Access,        BR_2_LF'Access),

         Capitalize     =>
           (Capitalize_Token'Access,     Capitalize'Access),

         Clean_Text     =>
           (Clean_Text_Token'Access,     Clean_Text'Access),

         Coma_2_Point   =>
           (Coma_2_Point_Token'Access,   Coma_2_Point'Access),

         Contract       =>
           (Contract_Token'Access,       Contract'Access),

         Del_Param      =>
           (Del_Param_Token'Access,      Del_Param'Access),

         Div            =>
           (Div_Token'Access,            Divide'Access),

         Exist          =>
           (Exist_Token'Access,          Exist'Access),

         File_Exists    =>
           (File_Exists_Token'Access,    File_Exists'Access),

         Format_Date    =>
           (Format_Date_Token'Access,    Format_Date'Access),

         Format_Number  =>
           (Format_Number_Token'Access,  Format_Number'Access),

         Is_Empty       =>
           (Is_Empty_Token'Access,       Is_Empty'Access),

         LF_2_BR        =>
           (LF_2_BR_Token'Access,        LF_2_BR'Access),

         Lower          =>
           (Lower_Token'Access,          Lower'Access),

         Match          =>
           (Match_Token'Access,          Match'Access),

         Max            =>
           (Max_Token'Access,            Max'Access),

         Min            =>
           (Min_Token'Access,            Min'Access),

         Modulo         =>
           (Modulo_Token'Access,         Modulo'Access),

         Mult           =>
           (Mult_Token'Access,           Multiply'Access),

         Neg            =>
           (Neg_Token'Access,            Neg'Access),

         No_Digit       =>
           (No_Digit_Token'Access,       No_Digit'Access),

         No_Dynamic     =>
           (No_Dynamic_Token'Access,     No_Dynamic'Access),

         No_Letter      =>
           (No_Letter_Token'Access,      No_Letter'Access),

         No_Space       =>
           (No_Space_Token'Access,       No_Space'Access),

         Oui_Non        =>
           (Oui_Non_Token'Access,        Oui_Non'Access),

         Point_2_Coma   =>
           (Point_2_Coma_Token'Access,   Point_2_Coma'Access),

         Repeat         =>
           (Repeat_Token'Access,         Repeat'Access),

         Replace        =>
           (Replace_Token'Access,        Replace'Access),

         Replace_All    =>
           (Replace_All_Token'Access,    Replace_All'Access),

         Replace_Param  =>
           (Replace_Param_Token'Access,  Replace_Param'Access),

         Invert         =>
           (Reverse_Token'Access,        Reverse_Data'Access),

         Size           =>
           (Size_Token'Access,           Size'Access),

         Slice          =>
           (Slice_Token'Access,          Slice'Access),

         Sub            =>
           (Sub_Token'Access,            Minus'Access),

         Trim           =>
           (Trim_Token'Access,           Trim'Access),

         Upper          =>
           (Upper_Token'Access,          Upper'Access),

         User_Defined   =>
           (User_Defined_Token'Access,   User_Defined'Access),

         Web_Encode     =>
           (Web_Encode_Token'Access,     Web_Encode'Access),

         Web_Escape     =>
           (Web_Escape_Token'Access,     Web_Escape'Access),

         Web_NBSP       =>
           (Web_NBSP_Token'Access,       Web_NBSP'Access),

         Wrap           =>
           (Wrap_Token'Access,           Wrap'Access),

         Yes_No         =>
           (Yes_No_Token'Access,         Yes_No'Access)
         );

   function Replace_One_Or_All
     (S   : String;
      P   : Parameter_Data;
      T   : Translate_Set;
      I   : Parameter_Set;
      One : Boolean) return String;
   --  Routine used to implement the REPLACE (One set to True) and REPLACE_ALL
   --  filters.

   function Value
     (Str          : String;
      Translations : Translate_Set;
      I_Params     : Parameter_Set) return String;
   --  Returns the value for Str, or if Str is a tag, returns it's value

   function BR_2_EOL (S : String; EOL : String) return String;
   --  Returns a string where all occurences of <BR> HTML tag have been
   --  replaced by EOL, assuming EOL is "LF", "CR", "LFCR" or "CRLF".

   --------------
   -- Absolute --
   --------------

   function Absolute
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if S = "" or else not Is_Number (S) then
         return "";
      else
         return Utils.Image (abs Integer'Value (S));
      end if;
   end Absolute;

   ---------------
   -- Add_Param --
   ---------------

   function Add_Param
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      function Get (Str : String) return String;
      pragma Inline (Get);
      --  Returns the parameter key=value to be added

      ---------
      -- Get --
      ---------

      function Get (Str : String) return String is
         P : constant Natural := Strings.Fixed.Index (Str, "=");
      begin
         if P = 0 then
            return Str;
         else
            return Str (Str'First .. P)
              & Value (Str (P + 1 .. Str'Last),
                       C.Translations, C.I_Parameters);
         end if;
      end Get;

      Param : constant String := Get (To_String (P.S));

   begin
      if Strings.Fixed.Index (S, "?") = 0 then
         --  No parameter yet
         return S & '?' & Param;

      elsif S (S'Last) = '?' or else S (S'Last) = '&' then
         return S & Param;

      else
         return S & '&' & Param;
      end if;
   end Add_Param;

   --------------
   -- BR_2_EOL --
   --------------

   function BR_2_EOL (S : String; EOL : String) return String is
      Result : String (S'Range);
      K      : Positive := Result'First;
      J      : Positive := S'First;
   begin
      if S = "" then
         return "";
      end if;

      loop
         if S (J) = '<'
           and then J + 3 <= S'Last
           and then Characters.Handling.To_Lower (S (J .. J + 2)) = "<br"
           and then
             (S (J + 3) = '>'
              or else (J + 4 <= S'Last and then S (J + 3 .. J + 4) = "/>"))
         then
            Result (K .. K + EOL'Length - 1) := EOL;
            K := K + EOL'Length;
            if S (J + 3) = '>' then
               J := J + 4;
            else
               J := J + 5;
            end if;
         else
            Result (K) := S (J);
            K := K + 1;
            J := J + 1;
         end if;

         exit when J > S'Last;
      end loop;

      return Result (Result'First .. K - 1);
   end BR_2_EOL;

   function BR_2_EOL
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      V_Str : constant String := To_String (P.S);
      EOL   : String (1 .. V_Str'Length / 2);
   begin
      if V_Str = "LF" then
         EOL (EOL'First) := ASCII.LF;
      elsif V_Str = "CRLF" then
         EOL := ASCII.CR & ASCII.LF;
      elsif V_Str = "CR" then
         EOL (EOL'First) := ASCII.CR;
      elsif V_Str = "LFCR" then
         EOL := ASCII.LF & ASCII.CR;
      else
         raise Template_Error with "unknown parameter for BR_2_EOL filter";
      end if;

      return BR_2_EOL (S, EOL);
   end BR_2_EOL;

   -------------
   -- BR_2_LF --
   -------------

   function BR_2_LF
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      return BR_2_EOL (S, String'(1 => ASCII.LF));
   end BR_2_LF;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String (S'Range);
      Upper  : Boolean := True;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Upper then
            Result (K) := Characters.Handling.To_Upper (S (K));
            Upper := False;
         else
            Result (K) := Characters.Handling.To_Lower (S (K));
            if Result (K) = ' ' or else Result (K) = '_' then
               Upper := True;
            end if;
         end if;
      end loop;
      return Result;
   end Capitalize;

   --------------------------
   -- Check_Null_Parameter --
   --------------------------

   procedure Check_Null_Parameter (P : Parameter_Data) is
   begin
      if P /= No_Parameter then
         raise Template_Error with "no parameter allowed in this filter";
      end if;
   end Check_Null_Parameter;

   ----------------
   -- Clean_Text --
   ----------------

   function Clean_Text
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      use type Strings.Maps.Character_Set;

      Result : String (S'Range);

      Clean_Set : constant Strings.Maps.Character_Set
        := Strings.Maps.Constants.Letter_Set
             or Strings.Maps.Constants.Decimal_Digit_Set
             or Strings.Maps.To_Set (" йикопафз");

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Clean_Set) then
            Result (K) := S (K);
         else
            Result (K) := ' ';
         end if;
      end loop;
      return Result;
   end Clean_Text;

   ------------------
   -- Coma_2_Point --
   ------------------

   function Coma_2_Point
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Result (K) = ',' then
            Result (K) := '.';
         end if;
      end loop;

      return Result;
   end Coma_2_Point;

   --------------
   -- Contract --
   --------------

   function Contract
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      use type Strings.Maps.Character_Set;

      Result : String (S'Range);
      R      : Natural := 0;
      Space  : Boolean := False;

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop

         if S (K) = ' ' then

            if Space = False then
               Space := True;

               R := R + 1;
               Result (R) := ' ';
            end if;

         else
            Space := False;

            R := R + 1;
            Result (R) := S (K);
         end if;

      end loop;

      if R = 0 then
         return "";
      else
         return Result (Result'First .. R);
      end if;
   end Contract;

   ---------------
   -- Del_Param --
   ---------------

   function Del_Param
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Param : constant String  := To_String (P.S);
      E     : constant Natural := Strings.Fixed.Index (S, "?");
      Len   : constant Natural := Param'Length;

   begin
      if E = 0 then
         --  No parameter, return original string
         return S;

      else
         declare
            Pos : constant Natural := Strings.Fixed.Index (S, Param);
            First, Last : Natural;
         begin
            if Pos < E
              or else
                (Pos + Len <= S'Last
                 and then S (Pos + Len) /= '='
                 and then S (Pos + Len) /= '&')
            then
               --  The parameter is not present, return original string
               return S;

            else
               First := Pos;
               Last  := Pos;

               while Last < S'Last and then S (Last) /= '&' loop
                  Last := Last + 1;
               end loop;

               if Last = S'Last then
                  --  This is the last parameter, remove the parameter with
                  --  leading parameter separator (? or &)
                  First := Pos - 1;
               end if;

               return S (S'First .. First - 1) & S (Last + 1 .. S'Last);
            end if;
         end;
      end if;
   end Del_Param;

   ------------
   -- Divide --
   ------------

   function Divide
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      N, V : Integer;
   begin
      declare
         V_Str : constant String := To_String (P.S);
      begin
         if Is_Number (V_Str) then
            N := Integer'Value (V_Str);
         else
            N := Integer'Value (Value (V_Str, C.Translations, C.I_Parameters));
         end if;
      exception
         when Constraint_Error =>
            raise Template_Error with """/"" filter parameter error";
      end;

      begin
         V := Integer'Value (S);
         return Utils.Image (V / N);
      exception
         when others =>
            return "";
      end;
   end Divide;

   -----------
   -- Exist --
   -----------

   function Exist
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if S /= "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Exist;

   -----------------
   -- File_Exists --
   -----------------

   function File_Exists
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if Configuration.Is_Regular_File (S) then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end File_Exists;

   -----------------
   -- Format_Date --
   -----------------

   function Format_Date
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      Date_Only : constant := 10;
      Date_Time : constant := 19;
      Param     : constant GNAT.Calendar.Time_IO.Picture_String :=
                    GNAT.Calendar.Time_IO.Picture_String
                      (Value (To_String (P.S),
                       C.Translations, C.I_Parameters));
      F         : constant Positive := S'First;

      Year   : Calendar.Year_Number;
      Month  : Calendar.Month_Number;
      Day    : Calendar.Day_Number;
      Hour   : GNAT.Calendar.Hour_Number   := 0;
      Minute : GNAT.Calendar.Minute_Number := 0;
      Second : GNAT.Calendar.Second_Number := 0;
      Time   : Calendar.Time;
   begin
      if S'Length >= Date_Only then
         Year  := Calendar.Year_Number'Value  (S (F     .. F + 3));
         Month := Calendar.Month_Number'Value (S (F + 5 .. F + 6));
         Day   := Calendar.Day_Number'Value   (S (F + 8 .. F + 9));

         if S (F + 4) /= '-' or else S (F + 7) /= '-' then
            return S;
         end if;

      else
         return S;
      end if;

      if S'Length = Date_Time then
         Hour   := GNAT.Calendar.Hour_Number'Value   (S (F + 11 .. F + 12));
         Minute := GNAT.Calendar.Minute_Number'Value (S (F + 14 .. F + 15));
         Second := GNAT.Calendar.Second_Number'Value (S (F + 17 .. F + 18));

         if S (F + 13) /= ':' or else S (F + 16) /= ':' then
            return S;
         end if;
      end if;

      Time := GNAT.Calendar.Time_Of (Year, Month, Day, Hour, Minute, Second);

      return GNAT.Calendar.Time_IO.Image (Time, Param);
   end Format_Date;

   -------------------
   -- Format_Number --
   -------------------

   function Format_Number
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      TS        : constant String := Strings.Fixed.Trim (S, Both);
      Separator : Character := ' ';

      function Is_Number return Boolean;
      --  Returns true if S is a number

      Point : Natural := 0;

      ---------------
      -- Is_Number --
      ---------------

      function Is_Number return Boolean is
      begin
         for K in TS'Range loop
            if TS (K) = '.' then
               Point := K;

            elsif not Characters.Handling.Is_Digit (TS (K)) then
               return False;
            end if;
         end loop;

         return True;
      end Is_Number;

      Result : String (1 .. TS'Length * 2);
      K      : Natural := Result'Last;

      N      : Natural;
      Count  : Natural := 0;

   begin
      if P.Mode = Str then
         declare
            Param : constant String :=
                      Value (To_String (P.S), C.Translations, C.I_Parameters);
         begin
            Separator := Param (Param'First);
         end;
      end if;

      if Is_Number then

         if Point = 0 then
            N := TS'Last;
         else
            N := Point - 1;
         end if;

         for P in reverse TS'First .. N loop
            Result (K) := TS (P);
            K := K - 1;
            Count := Count + 1;

            if Count mod 3 = 0 and then P /= TS'First then
               Result (K) := Separator;
               K := K - 1;
            end if;
         end loop;

         if Point = 0 then
            return Result (K + 1 .. Result'Last);

         else
            return Result (K + 1 .. Result'Last) & TS (Point .. TS'Last);
         end if;

      else
         return S;
      end if;
   end Format_Number;

   ------------------
   -- Free_Filters --
   ------------------

   procedure Free_Filters is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (User_Filter'Class, User_Filter_Access);
      C : Filter_Map.Cursor := Filter_Map.First (User_Filters);
      U : User_CB;
   begin
      while Filter_Map.Has_Element (C) loop
         if Filter_Map.Element (C).Typ = As_Tagged then
            U := Filter_Map.Element (C);
            Unchecked_Free (U.CBT);
         end if;
         Filter_Map.Next (C);
      end loop;
      Filter_Map.Clear (User_Filters);
   end Free_Filters;

   ------------
   -- Handle --
   ------------

   function Handle (Name : String) return Callback is
      Mode : constant Filter.Mode := Mode_Value (Name);
   begin
      return Table (Mode).Handle;
   end Handle;

   function Handle (Mode : Filter.Mode) return Callback is
   begin
      return Table (Mode).Handle;
   end Handle;

   -----------
   -- Image --
   -----------

   function Image (P : Parameter_Data) return String is
   begin
      if P = No_Parameter then
         return "";

      else
         case P.Mode is
            when Str          => return '(' & To_String (P.S) & ')';
            when Regexp       => return '(' & To_String (P.R_Str) & ')';
            when Regpat       => return
                 '(' & To_String (P.P_Str) & '/' & To_String (P.Param) & ')';
            when Slice        =>
               return '(' & Utils.Image (P.First)
                 & " .. " & Utils.Image (P.Last) & ')';
            when User_Callback =>
               return '(' & To_String (P.P) & ')';
         end case;
      end if;
   end Image;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if S = "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Is_Empty;

   -------------------
   -- Is_No_Dynamic --
   -------------------

   function Is_No_Dynamic (Filters : Set_Access) return Boolean is
   begin
      return Filters /= null
        and then Filters (Filters'First).Handle = No_Dynamic'Access;
   end Is_No_Dynamic;

   -------------
   -- LF_2_BR --
   -------------

   function LF_2_BR
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      N : constant Natural
        := Fixed.Count (S, Strings.Maps.To_Set (ASCII.LF));
   begin
      Check_Null_Parameter (P);

      if N = 0 then
         --  No LF, return the original string
         return S;
      end if;

      declare
         Result : String (1 .. S'Length + N * 4);
         K      : Positive := S'First;
      begin
         for J in S'Range loop
            if S (J) = ASCII.LF then
               Result (K .. K + 4) := "<br/>";
               K := K + 5;
            else
               Result (K) := S (J);
               K := K + 1;
            end if;
         end loop;

         return Result (1 .. K - 1);
      end;
   end LF_2_BR;

   -----------
   -- Lower --
   -----------

   function Lower
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      return Characters.Handling.To_Lower (S);
   end Lower;

   -----------
   -- Match --
   ------------

   function Match
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      use type GNAT.Regpat.Match_Location;

      Matches : GNAT.Regpat.Match_Array (0 .. 0);
   begin
      if P = No_Parameter then
         raise Template_Error with "missing parameter for MATCH filter";
      end if;

      GNAT.Regpat.Match (P.Regexp.all, S, Matches);

      if Matches (0) = GNAT.Regpat.No_Match then
         return "FALSE";
      else
         return "TRUE";
      end if;
   end Match;

   ---------
   -- Max --
   ---------

   function Max
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      V_Str : constant String := To_String (P.S);
   begin
      if Is_Number (V_Str) and then Is_Number (S) then
         return Utils.Image
           (Integer'Max (Integer'Value (V_Str), Integer'Value (S)));
      else
         return "";
      end if;
   end Max;

   ---------
   -- Min --
   ---------

   function Min
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      V_Str : constant String := To_String (P.S);
   begin
      if Is_Number (V_Str) and then Is_Number (S) then
         return Utils.Image
           (Integer'Min (Integer'Value (V_Str), Integer'Value (S)));
      else
         return "";
      end if;
   end Min;

   -----------
   -- Minus --
   -----------

   function Minus
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      N, V : Integer;
   begin
      declare
         V_Str : constant String := To_String (P.S);
      begin
         if Is_Number (V_Str) then
            N := Integer'Value (V_Str);
         else
            N := Integer'Value (Value (V_Str, C.Translations, C.I_Parameters));
         end if;
      exception
         when Constraint_Error =>
            raise Template_Error with """-"" filter parameter error";
      end;

      begin
         V := Integer'Value (S);
         return Utils.Image (V - N);
      exception
         when others =>
            return "";
      end;
   end Minus;

   ----------------
   -- Mode_Value --
   ----------------

   function Mode_Value (Name : String) return Mode is
      F, L, K : Mode;
   begin
      F := Mode'First;
      L := Mode'Last;

      loop
         K := Mode'Val ((Mode'Pos (F) + Mode'Pos (L)) / 2);

         if Table (K).Name.all = Name then
            return K;

         else
            exit when F = K and then L = K;

            if Table (K).Name.all < Name then
               F := K;
               if F /= Mode'Last then
                  F := Mode'Succ (F);
               end if;

               exit when Table (F).Name.all > Name;

            else
               L := K;
               if L /= Mode'First then
                  L := Mode'Pred (L);
               end if;

               exit when Table (L).Name.all < Name;
            end if;
         end if;
      end loop;

      --  Not found in the table of built-in filters, look for a user's one

      if User_Filters.Contains (Name) then
         return User_Defined;
      end if;

      raise Internal_Error with "Unknown filter " & Name;
   end Mode_Value;

   ------------
   -- Modulo --
   ------------

   function Modulo
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      N, V : Integer;
   begin
      declare
         V_Str : constant String := To_String (P.S);
      begin
         if Is_Number (V_Str) then
            N := Integer'Value (V_Str);
         else
            N := Integer'Value (Value (V_Str, C.Translations, C.I_Parameters));
         end if;
      exception
         when Constraint_Error =>
            raise Template_Error with "modulo filter parameter error";
      end;

      begin
         V := Integer'Value (S);
         return Utils.Image (V mod N);
      exception
         when others =>
            return "";
      end;
   end Modulo;

   --------------
   -- Multiply --
   --------------

   function Multiply
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      N, V : Integer;
   begin
      declare
         V_Str : constant String := To_String (P.S);
      begin
         if Is_Number (V_Str) then
            N := Integer'Value (V_Str);
         else
            N := Integer'Value (Value (V_Str, C.Translations, C.I_Parameters));
         end if;
      exception
         when Constraint_Error =>
            raise Template_Error with """*"" filter parameter error";
      end;

      begin
         V := Integer'Value (S);
         return Utils.Image (V * N);
      exception
         when others =>
            return "";
      end;
   end Multiply;

   ----------
   -- Name --
   ----------

   function Name (Handle : Callback) return String is
   begin
      for K in Table'Range loop
         if Table (K).Handle = Handle then
            return Table (K).Name.all;
         end if;
      end loop;

      raise Internal_Error with "Unknown filter handle";
   end Name;

   ---------
   -- Neg --
   ---------

   function Neg
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if S = "" or else not Is_Number (S) then
         return "";
      else
         return Utils.Image (Integer'Value (S) * (-1));
      end if;
   end Neg;

   --------------
   -- No_Digit --
   --------------

   function No_Digit
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In
           (S (K), Strings.Maps.Constants.Decimal_Digit_Set)
         then
            Result (K) := ' ';
         end if;
      end loop;

      return Result;
   end No_Digit;

   ----------------
   -- No_Dynamic --
   ----------------

   function No_Dynamic
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);
      return S;
   end No_Dynamic;

   ---------------
   -- No_Letter --
   ---------------

   function No_Letter
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Strings.Maps.Constants.Letter_Set) then
            Result (K) := ' ';
         end if;
      end loop;

      return Result;
   end No_Letter;

   --------------
   -- No_Space --
   --------------

   function No_Space
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String (S'Range);
      L      : Natural := Result'First - 1;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if not (S (K) = ' ') then
            L := L + 1;
            Result (L) := S (K);
         end if;
      end loop;

      return Result (Result'First .. L);
   end No_Space;

   -------------
   -- Oui_Non --
   -------------

   function Oui_Non
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if S = "TRUE" then
         return "OUI";

      elsif S = "true" then
         return "oui";

      elsif S = "True" then
         return "Oui";

      elsif S = "FALSE" then
         return "NON";

      elsif S = "false" then
         return "non";

      elsif S = "False" then
         return "Non";

      else
         return S;
      end if;
   end Oui_Non;

   ---------------
   -- Parameter --
   ---------------

   function Parameter (Mode : Filter.Mode) return Parameter_Mode is
   begin
      case Mode is
         when Match                 => return Regexp;
         when Replace | Replace_All => return Regpat;
         when Slice                 => return Slice;
         when User_Defined          => return User_Callback;
         when others                => return Str;
      end case;
   end Parameter;

   ----------
   -- Plus --
   ----------

   function Plus
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      N, V : Integer;
   begin
      declare
         V_Str : constant String := To_String (P.S);
      begin
         if Is_Number (V_Str) then
            N := Integer'Value (V_Str);
         else
            N := Integer'Value (Value (V_Str, C.Translations, C.I_Parameters));
         end if;
      exception
         when Constraint_Error =>
            raise Template_Error with """+"" filter parameter error";
      end;

      begin
         V := Integer'Value (S);
         return Utils.Image (V + N);
      exception
         when others =>
            return "";
      end;
   end Plus;

   ------------------
   -- Point_2_Coma --
   ------------------

   function Point_2_Coma
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Result (K) = '.' then
            Result (K) := ',';
         end if;
      end loop;

      return Result;
   end Point_2_Coma;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name    : String;
      Handler : Templates_Parser.Callback)
   is
      Position : Filter_Map.Cursor;
      Success  : Boolean;
   begin
      User_Filters.Insert (Name, (With_Param, Handler), Position, Success);
   end Register;

   procedure Register
     (Name    : String;
      Handler : Callback_No_Param)
   is
      Position : Filter_Map.Cursor;
      Success  : Boolean;
   begin
      User_Filters.Insert (Name, (No_Param, Handler), Position, Success);
   end Register;

   procedure Register
     (Name    : String;
      Handler : not null access User_Filter'Class)
   is
      Position : Filter_Map.Cursor;
      Success  : Boolean;
   begin
      User_Filters.Insert
        (Name, (As_Tagged, User_Filter_Access (Handler)), Position, Success);
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out Parameter_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Access);
   begin
      if P.Mode = Regpat then
         Unchecked_Free (P.Regpat);
      elsif P.Mode = Regexp then
         Unchecked_Free (P.Regexp);
      end if;
   end Release;

   procedure Release (S : in out Set) is
   begin
      for K in S'Range loop
         Release (S (K).Parameters);
      end loop;
   end Release;

   ------------
   -- Repeat --
   ------------

   function Repeat
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      N       : Natural;
      Pattern : Unbounded_String;
   begin
      declare
         V_Str : constant String := To_String (P.S);
      begin
         if Is_Number (V_Str) then
            --  REPEAT(N):STR
            N       := Natural'Value (V_Str);
            Pattern := To_Unbounded_String (S);

         else
            declare
               N_Str : constant String :=
                         Value (V_Str, C.Translations, C.I_Parameters);
            begin
               if Is_Number (N_Str) then
                  --  REPEAT(N_VAR):STR
                  N       := Natural'Value (N_Str);
                  Pattern := To_Unbounded_String (S);
               else
                  --  REPEAT(STR):N
                  N       := Natural'Value (S);
                  Pattern := P.S;
               end if;
            end;
         end if;

         declare
            S : constant String := To_String (Pattern);
            R : String (1 .. N * S'Length);
         begin
            for K in 1 .. N loop
               R (1 + (K - 1) * S'Length .. S'Length * K) := S;
            end loop;

            return R;
         end;
      end;
   exception
      when Constraint_Error =>
         raise Template_Error with "repeat filter parameter error";
   end Repeat;

   -------------
   -- Replace --
   -------------

   function Replace
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
   begin
      return Replace_One_Or_All
        (S, P, C.Translations, C.I_Parameters, One => True);
   end Replace;

   -----------------
   -- Replace_All --
   -----------------

   function Replace_All
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
   begin
      return Replace_One_Or_All
        (S, P, C.Translations, C.I_Parameters, One => False);
   end Replace_All;

   ------------------------
   -- Replace_One_Or_All --
   ------------------------

   function Replace_One_Or_All
     (S   : String;
      P   : Parameter_Data;
      T   : Translate_Set;
      I   : Parameter_Set;
      One : Boolean) return String
   is
      use type GNAT.Regpat.Match_Location;
      use Ada.Strings.Fixed;

      Param   : constant String  := Value (To_String (P.Param), T, I);

      Matches : GNAT.Regpat.Match_Array
        (0 .. GNAT.Regpat.Paren_Count (P.Regpat.all));

      Result  : Unbounded_String;
      Temp    : Unbounded_String;
      N       : Natural;
      Current : Natural := S'First;
      Matched : Boolean := False;
   begin

      loop
         GNAT.Regpat.Match (P.Regpat.all, S (Current .. S'Last), Matches);
         exit when Matches (0) = GNAT.Regpat.No_Match;

         Matched := True;
         Temp    := To_Unbounded_String (Param);

         --  Replace each occurrence of \n in Temp by the corresponding match

         for K in 1 .. Matches'Last loop
            --  We only accept \1 ... \9 because we want to be able to write
            --  such a replacement string "\10123456789\2"
            exit when K = 10 or else Matches (K) = GNAT.Regpat.No_Match;

            N := 1;

            loop
               N := Index
                 (Slice (Temp, N, Length (Temp)), '\' & Utils.Image (K));

               exit when N = 0;

               Replace_Slice
                 (Temp, N, N + 1,
                  By => S (Matches (K).First .. Matches (K).Last));

               --  Position N just after the inserted replacement text
               N := N + Matches (K).Last - Matches (K).First + 1;
            end loop;
         end loop;

         --  Prepend the beginning of string before the match
         Result := Result
           & To_Unbounded_String (S (Current .. Matches (0).First - 1))
           & Temp;

         --  Position the cursor just after the current match
         Current := Matches (0).Last + 1;

         exit when One;
      end loop;

      if Matched then
         return To_String (Result) & S (Current .. S'Last);
      else
         --  No match, returns the initial string
         return S;
      end if;
   exception
      when Constraint_Error =>
         raise Template_Error with "replace filter parameter error";
   end Replace_One_Or_All;

   -------------------
   -- Replace_Param --
   -------------------

   function Replace_Param
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      Param : constant String  := To_String (P.S);
      Pos   : constant Natural := Strings.Fixed.Index (Param, "=");

   begin
      if Pos = 0 then
         raise Template_Error with "Replace_Param error";

      else
         declare
            Key : constant String := Param (Param'First .. Pos - 1);
         begin
            return Add_Param
              (Del_Param (S, C, (Str, To_Unbounded_String (Key))),
               C, P);
         end;
      end if;
   end Replace_Param;

   ------------------
   -- Reverse_Data --
   ------------------

   function Reverse_Data
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Result : String (S'Range);
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         Result (Result'Last - K + Result'First) := S (K);
      end loop;
      return Result;
   end Reverse_Data;

   ----------
   -- Size --
   ----------

   function Size
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      return Utils.Image (S'Length);
   end Size;

   -----------
   -- Slice --
   -----------

   function Slice
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      First, Last : Integer;
   begin
      if S'Length = 0 then
         return "";
      else
         if P.First <= 0 then
            First := Integer'Max (S'First, S'Last + P.First);
         else
            First := S'First + P.First - 1;
         end if;

         if P.Last <= 0 then
            Last := S'Last + P.Last;
         else
            Last := Integer'Min (S'Last, S'First + P.Last - 1);
         end if;

         if First > S'Last then
            return "";
         end if;

         return S (First .. Last);
      end if;
   end Slice;

   ----------
   -- Trim --
   ----------

   function Trim
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Trim;

   -----------
   -- Upper --
   -----------

   function Upper
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      return Characters.Handling.To_Upper (S);
   end Upper;

   ------------------
   -- User_Defined --
   ------------------

   function User_Defined
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String is
   begin
      case P.Handler.Typ is
         when With_Param =>
            return P.Handler.CBP
              (S, To_String (P.P), (C.Translations, C.Lazy_Tag));

         when No_Param =>
            if P.P /= Null_Unbounded_String then
               raise Template_Error with "no parameter allowed in this filter";
            else
               return P.Handler.CB (S, (C.Translations, C.Lazy_Tag));
            end if;

         when As_Tagged =>
            if P.Handler.CBT /= null then
               return Execute
                 (P.Handler.CBT,
                  Value      => S,
                  Parameters => To_String (P.P),
                  Context    => (C.Translations, C.Lazy_Tag));
            else
               return "";
            end if;
      end case;
   end User_Defined;

   -----------------
   -- User_Handle --
   -----------------

   function User_Handle (Name : String) return User_CB is
   begin
      return User_Filters.Element (Name);
   end User_Handle;

   -----------
   -- Value --
   -----------

   function Value
     (Str          : String;
      Translations : Translate_Set;
      I_Params     : Parameter_Set) return String
   is
      Pos : Association_Map.Cursor;
   begin
      if Str'Length > 0
        and then Str (Str'First) = '$'
        and then Is_Number (Str (Str'First + 1 .. Str'Last))
      then
         --  This is an include parameter

         declare
            N : constant Natural :=
                  Natural'Value (Str (Str'First + 1 .. Str'Last));
         begin
            return To_String (I_Params (N + 1));
         end;

      elsif Translations = Null_Set then
         return Str;

      else
         Pos := Translations.Set.Find (Str);

         if Association_Map.Has_Element (Pos) then
            declare
               Tk : constant Association := Association_Map.Element (Pos);
            begin
               if Tk.Kind = Std then
                  return To_String (Tk.Value);
               end if;
            end;
         end if;

         return Str;
      end if;
   end Value;

   ----------------
   -- Web_Encode --
   ----------------

   function Web_Encode
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      C_Inf  : constant Natural := Character'Pos ('<');
      C_Sup  : constant Natural := Character'Pos ('>');
      C_And  : constant Natural := Character'Pos ('&');
      C_Quo  : constant Natural := Character'Pos ('"');

      Result : Unbounded_String;
      Last   : Integer := S'First;
      Code   : Natural;

      procedure Append_To_Result
        (Str  : String;
         From : Integer;
         To   : Integer);
      --  Append S (From .. To) to Result if not empty concatenated with Str
      --  and update Last.

      ----------------------
      -- Append_To_Result --
      ----------------------

      procedure Append_To_Result
        (Str  : String;
         From : Integer;
         To   : Integer) is
      begin
         if From <= To then
            Append (Result, S (From .. To) & Str);
         else
            Append (Result, Str);
         end if;

         Last := To + 2;
      end Append_To_Result;

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         Code := Character'Pos (S (K));

         if Code not in 32 .. 127
           or else Code = C_Inf or else Code = C_Sup
           or else Code = C_And or else Code = C_Quo
         then
            declare
               I_Code : constant String := Utils.Image (Code);
            begin
               Append_To_Result ("&#" & I_Code & ";", Last, K - 1);
            end;
         end if;
      end loop;

      if Last <= S'Last then
         Append (Result, S (Last .. S'Last));
      end if;

      return To_String (Result);
   end Web_Encode;

   ----------------
   -- Web_Escape --
   ----------------

   function Web_Escape
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);
      return Utils.Web_Escape (S);
   end Web_Escape;

   --------------
   -- Web_NBSP --
   --------------

   function Web_NBSP
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Nbsp_Token          : constant String := "&nbsp;";
      Max_Escape_Sequence : constant Positive := Nbsp_Token'Length;
      Result              : String (1 .. S'Length * Max_Escape_Sequence);
      Last                : Natural := 0;
   begin
      Check_Null_Parameter (P);

      for I in S'Range loop
         Last := Last + 1;

         if S (I) = ' ' then
            Result (Last .. Last + Nbsp_Token'Length - 1) := Nbsp_Token;
            Last := Last + Nbsp_Token'Length - 1;
         else
            Result (Last) := S (I);
         end if;

      end loop;

      return Result (1 .. Last);
   end Web_NBSP;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
      Max_Line_Length : constant Positive := Positive'Value (To_String (P.S));
      Last            : Natural := S'First;
      First           : Natural := S'First;
      Last_Space_Init : constant Integer := S'First - 1;
      Last_Space      : Integer := Last_Space_Init;
      Result          : Unbounded_String;
   begin
      while Last <= S'Last loop
         if S (Last) = ' ' then
            Last_Space := Last;
         end if;

         if S (Last) = ASCII.LF then
            --  End of the line

            Append (Result, S (First .. Last));
            First := Last + 1;
            Last  := First;
            Last_Space := Last_Space_Init;

         elsif Last - First >= Max_Line_Length then
            --  The line must be wrapped

            if Last_Space in First .. Last then
               --  Split the line before the last word

               Append (Result, S (First .. Last_Space - 1) & ASCII.LF);
               First := Last_Space + 1;
               Last := First;
            else
               --  There is only one word on the line: cut it

                  Append (Result, S (First .. Last - 1) & ASCII.LF);
                  First := Last;
            end if;

            Last_Space := Last_Space_Init;

         else
            --  Go to the next character

            Last := Last + 1;
         end if;
      end loop;

      Append (Result, S (First .. S'Last));

      return To_String (Result);
   exception
      when Constraint_Error =>
         raise Template_Error with "wrap filter parameter error";
   end Wrap;

   ------------
   -- Yes_No --
   ------------

   function Yes_No
     (S : String;
      C : not null access Filter_Context;
      P : Parameter_Data := No_Parameter) return String
   is
      pragma Unreferenced (C);
   begin
      Check_Null_Parameter (P);

      if S = "TRUE" then
         return "YES";

      elsif S = "true" then
         return "yes";

      elsif S = "True" then
         return "Yes";

      elsif S = "FALSE" then
         return "NO";

      elsif S = "false" then
         return "no";

      elsif S = "False" then
         return "No";

      else
         return S;
      end if;
   end Yes_No;

end Filter;
