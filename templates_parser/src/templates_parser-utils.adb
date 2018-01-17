------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

package body Templates_Parser.Utils is

   --------------------------
   -- Executable_Extension --
   --------------------------

   function Executable_Extension return String is
   begin
      if Is_Windows then
         return "exe";
      else
         return "";
      end if;
   end Executable_Extension;

   ---------------------------
   -- Get_Program_Directory --
   ---------------------------

   function Get_Program_Directory return String is

      function Locate_On_Path (Filename : String) return String;
      --  Returns the full pathname for filename or the empty string if not
      --  found.

      function Is_Full_Pathname (Filename : String) return Boolean;
      --  Returns True is Filename is a full pathname

      function Get_Command_Name return String;
      --  Returns the normalized command string

      function Containing_Directory (Filename : String) return String;
      --  Containing directory without directory separator, this can happen
      --  with GNAT when returning the current working directory?

      --------------------------
      -- Containing_Directory --
      --------------------------

      function Containing_Directory (Filename : String) return String is
         CD : constant String := Directories.Containing_Directory (Filename);
      begin
         if CD (CD'Last) = Directory_Separator then
            return CD (CD'First .. CD'Last - 1);
         else
            return CD;
         end if;
      end Containing_Directory;

      ----------------------
      -- Get_Command_Name --
      ----------------------

      function Get_Command_Name return String is
         N : constant String := Command_Line.Command_Name;
         E : constant String := Executable_Extension;
      begin
         if N'Length > E'Length
           and then N (N'Last - E'Length + 1 .. N'Last) = E
         then
            return N;
         else
            return N & E;
         end if;
      end Get_Command_Name;

      ----------------------
      -- Is_Full_Pathname --
      ----------------------

      function Is_Full_Pathname (Filename : String) return Boolean is
         F : String renames Filename;
      begin
         return F (F'First) = Directory_Separator
           or else
             (F'Length > 2
              and then (F (F'First) in 'a' .. 'z'
                        or else F (F'First) in 'A' .. 'Z')
              and then F (F'First + 1) = ':'
              and then F (F'First + 2) = Directory_Separator);
      end Is_Full_Pathname;

      --------------------
      -- Locate_On_Path --
      --------------------

      function Locate_On_Path (Filename : String) return String is
         PATH        : constant String := Environment_Variables.Value ("PATH");
         First, Last : Natural;
         Idx         : Natural;
      begin
         First := PATH'First;

         loop
            Last := Strings.Fixed.Index
              (PATH, String'(1 => Path_Separator), From => First);

            if Last = 0 then
               Idx := PATH'Last;
            else
               Idx := Last - 1;
            end if;

            declare
               Full_Pathname : constant String := Directories.Compose
                 (PATH (First .. Idx) & Directory_Separator, Filename);
            begin
               if Directories.Exists (Full_Pathname) then
                  return Full_Pathname;
               end if;
            end;

            First := Last + 1;

            exit when Last = 0 or else First > PATH'Last;
         end loop;

         return "";
      end Locate_On_Path;

      Command_Name : constant String := Get_Command_Name;

   begin
      if Command_Name = "" then
         --  Command name can be empty on OS not supporting command line
         --  options like VxWorks.
         return ".";

      else
         declare
            Dir : constant String := Containing_Directory (Command_Name);
         begin
            --  On UNIX command_name doesn't include the directory name
            --  when the command was found on the PATH. On Windows using
            --  the standard shell, the command is never passed using a
            --  full pathname. In such a case, which check on the PATH
            --  ourselves to find it.

            if Directories.Exists (Command_Name) then
               --  Command is found
               if Is_Full_Pathname (Command_Name)
                 or else Is_Full_Pathname (Dir)
               then
                  --  And we have a full pathname use it
                  return Dir & Directory_Separator;
               else
                  --  A relative pathname, catenate the current directory
                  return Directories.Current_Directory
                    & Directory_Separator & Dir & Directory_Separator;
               end if;

            else
               --  Command does not exists, try checkin it on the PATH
               declare
                  Full_Pathname : constant String :=
                                    Locate_On_Path
                                      (Directories.Simple_Name (Command_Name));
               begin
                  if Full_Pathname = "" then
                     --  Not found on the PATH, nothing we can do
                     return Dir;

                  else
                     return Directories.Containing_Directory (Full_Pathname)
                       & Directory_Separator;
                  end if;
               end;
            end if;
         end;
      end if;
   end Get_Program_Directory;

   -----------
   -- Image --
   -----------

   function Image (T : Tag) return String is

      function Quote (Str : String) return String;
      pragma Inline (Quote);
      --  Quote Str and double quote inside Str if needed

      -----------
      -- Quote --
      -----------

      function Quote (Str : String) return String is
         Result : Unbounded_String;
      begin
         Append (Result, """");
         for K in Str'Range loop
            if Str (K) = '"' then
               Append (Result, """""");
            else
               Append (Result, Str (K));
            end if;
         end loop;
         Append (Result, """");
         return To_String (Result);
      end Quote;

      Result : Unbounded_String;
      N      : Tag_Node_Access := T.Data.Head;
   begin
      while N /= null loop
         if N.Kind = Value then
            if Result /= Null_Unbounded_String then
               Append (Result, ",");
            end if;
            Append (Result, Quote (To_String (N.V)));
         else
            Append (Result, Image (N.VS.all));
         end if;
         N := N.Next;
      end loop;

      return "(" & To_String (Result) & ")";
   end Image;

   function Image (N : Integer) return String is
      N_Img : constant String := Integer'Image (N);
   begin
      if N_Img (N_Img'First) = '-' then
         return N_Img;
      else
         return N_Img (N_Img'First + 1 .. N_Img'Last);
      end if;
   end Image;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (S : String) return Boolean is
   begin
      return S'Length /= 0
        and then
          Strings.Fixed.Index
            (S,
             Strings.Maps.Constants.Decimal_Digit_Set,
             Test => Strings.Outside) = 0;
   end Is_Number;

   -----------
   -- Value --
   -----------

   function Value (T : String) return Tag is

      function Value (T : String) return Tag;
      --  Returns the Tag for T string

      function Clear_Quote (Str : String) return String;
      pragma Inline (Clear_Quote);
      --  Removes double quote in Str

      -----------------
      -- Clear_Quote --
      -----------------

      function Clear_Quote (Str : String) return String is
         Result : Unbounded_String;
      begin
         for K in Str'Range loop
            if Str (K) /= '"'
              or else (K < Str'Last and then Str (K + 1) /= '"')
            then
               Append (Result, Str (K));
            end if;
         end loop;
         return To_String (Result);
      end Clear_Quote;

      -----------
      -- Value --
      -----------

      function Value (T : String) return Tag is
         Result : Tag;
         K      : Natural := T'First;
         N      : Natural;
         Last   : Natural;
      begin
         while K < T'Last loop

            if T (K) = '(' then
               --  This is a nested Tag, Look for corresponding closing parent
               Last := K + 1;
               N    := 0;

               Nested_Tag : loop
                  if T (Last) = ')' then
                     if N = 0 then
                        --  Matching parent found, add it to the result
                        Result := Result & Value (T (K + 1 .. Last));
                        K := Last;
                        --  and leave this loop
                        exit Nested_Tag;
                     else
                        N := N - 1;
                     end if;

                  elsif T (Last) = '(' then
                     N := N + 1;
                  end if;

                  if Last = T'Last then
                     --  Matching parent not found
                     raise Constraint_Error;
                  else
                     Last := Last + 1;
                  end if;
               end loop Nested_Tag;

            elsif T (K) = '"' then
               --  This is a value, Look for corresponding closing quote
               Last := K + 1;

               Quoted_Value : loop
                  if T (Last) = '"' then
                     if Last < T'Last
                       and then T (Last + 1) = '"'
                     then
                        --  Skip this quote
                        Last := Last + 1;

                     else
                        --  Found matching quote, add this value
                        Result := Result & Clear_Quote (T (K + 1 .. Last - 1));
                        K := Last;
                        --  and leave loop
                        exit Quoted_Value;
                     end if;

                  elsif Last = T'Last then
                     --  No matching quote
                     raise Constraint_Error;
                  end if;
                  Last := Last + 1;
               end loop Quoted_Value;

               --  Here we must have either a ',' or ")"
               if Last /= T'Last
                 and then T (Last + 1) /= ','
                 and then T (Last + 1) /= ')'
               then
                  raise Constraint_Error;
               end if;
            end if;

            K := K + 1;
         end loop;

         return Result;
      end Value;

   begin
      if T'Length > 1 and then T (T'First) = '(' and then T (T'Last) = ')' then
         return Value (T (T'First + 1 .. T'Last - 1));
      else
         raise Constraint_Error;
      end if;
   end Value;

   ----------------
   -- Web_Escape --
   ----------------

   function Web_Escape (S : String) return String is

      Result : Unbounded_String;
      Last   : Integer := S'First;

      procedure Append_To_Result (Str : String; From, To : Integer);
      --  Append S (From .. To) to Result if not empty concatenated with Str
      --  and update Last.

      ----------------------
      -- Append_To_Result --
      ----------------------

      procedure Append_To_Result (Str : String; From, To : Integer) is
      begin
         if From <= To then
            Append (Result, S (From .. To) & Str);
         else
            Append (Result, Str);
         end if;

         Last := To + 2;
      end Append_To_Result;

   begin
      for I in S'Range loop
         case S (I) is
            when '&' =>
               Append_To_Result ("&amp;", Last, I - 1);

            when '>' =>
               Append_To_Result ("&gt;", Last, I - 1);

            when '<' =>
               Append_To_Result ("&lt;", Last, I - 1);

            when '"' =>
               Append_To_Result ("&quot;", Last, I - 1);

            when others =>
               null;
         end case;
      end loop;

      if Last <= S'Last then
         Append (Result, S (Last .. S'Last));
      end if;

      return To_String (Result);
   end Web_Escape;

end Templates_Parser.Utils;
