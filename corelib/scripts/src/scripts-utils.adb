with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with GNAT.Strings;      use GNAT.Strings;

package body Scripts.Utils is

   ------------------------------------
   -- Argument_List_To_Quoted_String --
   ------------------------------------

   function Argument_List_To_Quoted_String
     (Args            : GNAT.Strings.String_List;
      Quote           : Character := '"';
      Quote_Backslash : Boolean := True) return String
   is
      Len : Natural := 1;
   begin
      --  Compute the maximum length of the output.

      for J in Args'Range loop
         --  For each argument we append at most 3 characters, two quotes
         --  plus an ending space.

         if Args (J) /= null then
            Len := Len + Args (J)'Length + 3;

            for T in Args (J)'Range loop
               if Args (J)(T) = Quote or else Args (J)(T) = '\' then
                  Len := Len + 1;
               end if;
            end loop;
         end if;
      end loop;

      declare
         Result : String (1 .. Len + 1);
         Ind    : Natural := Result'First;

         procedure Append (Str : String);
         --  Append the contents of Str to Result, protecting quote characters

         ------------
         -- Append --
         ------------

         procedure Append (Str : String) is
         begin
            for J in Str'Range loop
               if Str (J) = Quote
                 or else (Quote_Backslash and then Str (J) = '\')
               then
                  Result (Ind)     := '\';
                  Result (Ind + 1) := Str (J);
                  Ind := Ind + 2;
               else
                  Result (Ind) := Str (J);
                  Ind := Ind + 1;
               end if;
            end loop;
         end Append;

      begin
         for J in Args'Range loop
            if Args (J) /= null then
               if Index (Args (J).all, " ") > 0 then
                  Result (Ind) := Quote;
                  Ind := Ind + 1;
                  Append (Args (J).all);
                  Result (Ind) := Quote;
                  Result (Ind + 1) := ' ';
                  Ind := Ind + 2;

               else
                  Append (Args (J).all);
                  Result (Ind) := ' ';
                  Ind := Ind + 1;
               end if;
            end if;
         end loop;

         return Result (1 .. Ind - 1);
      end;
   end Argument_List_To_Quoted_String;

      ------------------------------------------------
   -- Argument_String_To_List_With_Triple_Quotes --
   ------------------------------------------------

   function Argument_String_To_List_With_Triple_Quotes
     (Arg_String : String) return String_List_Access
   is
      Max_Args : Integer := 128;
      New_Argv : String_List_Access := new String_List (1 .. Max_Args);
      New_Argc : Natural := 0;
      Idx      : Integer;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String_List, String_List_Access);
      Backslashed       : Boolean;
      Quoted            : Boolean;
      Triple_Quoted     : Boolean;
      Has_Triple        : Boolean;
      Start_Idx         : Integer;
      Start_With_Triple : Boolean;
      End_With_Triple   : Boolean;

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         Backslashed   := False;
         Quoted        := False;
         Triple_Quoted := False;
         Start_Idx     := Idx;
         Start_With_Triple := False;
         End_With_Triple   := False;

         while Idx <= Arg_String'Last
           and then (Backslashed
                     or else Quoted
                     or else Triple_Quoted
                     or else Arg_String (Idx) /= ' ')
         loop
            End_With_Triple := False;

            if Backslashed then
               Backslashed := False;
            else

               case Arg_String (Idx) is
                  when '\' =>
                     Backslashed := True;

                  when '"' =>
                     if Quoted then
                        Quoted := False;
                     else
                        Has_Triple := Idx + 2 <= Arg_String'Last
                          and then Arg_String (Idx) = '"'
                          and then Arg_String (Idx + 1) = '"'
                          and then Arg_String (Idx + 2) = '"';
                        if Has_Triple then
                           Triple_Quoted := not Triple_Quoted;
                           if Idx = Start_Idx then
                              Start_With_Triple := Triple_Quoted;
                           end if;
                           End_With_Triple := True;
                           Idx := Idx + 2;
                        else
                           Quoted := True;
                        end if;
                     end if;

                  when others =>
                     null;
               end case;
            end if;

            Idx := Idx + 1;
         end loop;

         New_Argc := New_Argc + 1;

         --  Resize the table if needed
         if New_Argc > Max_Args then
            declare
               New_New_Argv : String_List (1 .. Max_Args * 2);
            begin
               New_New_Argv (1 .. Max_Args) := New_Argv.all;
               Unchecked_Free (New_Argv);
               New_Argv := new String_List'(New_New_Argv);
            end;

            Max_Args := Max_Args * 2;
         end if;

         if Start_With_Triple and End_With_Triple then
            New_Argv (New_Argc) :=
              new String'(Arg_String (Start_Idx + 3 .. Idx - 4));
         else
            New_Argv (New_Argc) :=
              new String'(Arg_String (Start_Idx .. Idx - 1));
         end if;

         --  Skip extraneous spaces

         while Idx <= Arg_String'Last and then Arg_String (Idx) = ' ' loop
            Idx := Idx + 1;
         end loop;
      end loop;

      declare
         Result : constant String_List := New_Argv (1 .. New_Argc);
      begin
         Unchecked_Free (New_Argv);
         return new String_List'(Result);
      end;
   end Argument_String_To_List_With_Triple_Quotes;

   ---------------
   -- Unprotect --
   ---------------

   function Unprotect (Str : String) return String is
      Result : String (Str'Range);
      Index  : Natural := Result'First;
      S      : Natural := Str'First;
   begin
      while S <= Str'Last loop
         if Str (S) = '\' then
            if S < Str'Last then
               Result (Index) := Str (S + 1);
            end if;

            S := S + 2;
         else
            Result (Index) := Str (S);
            S := S + 1;
         end if;

         Index := Index + 1;
      end loop;

      return Result (Result'First .. Index - 1);
   end Unprotect;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : String) return String_Access is
      use GNAT.OS_Lib;
      FD           : File_Descriptor := Invalid_FD;
      Buffer       : GNAT.Strings.String_Access;
      Length       : Integer;
      Dummy_Length : Integer;
      pragma Unreferenced (Dummy_Length);

   begin
      FD := Open_Read (File, Fmode => Binary);

      if FD = Invalid_FD then
         return null;
      end if;

      Length := Integer (File_Length (FD));
      Buffer := new String (1 .. Length);
      Dummy_Length := Read (FD, Buffer.all'Address, Length);
      Close (FD);
      return Buffer;
   end Read_File;

end Scripts.Utils;
