
with Generic_Values; use Generic_Values;
with System;         use System;
with GNAT.IO;        use GNAT.IO;
with GNAT.Regpat;    use GNAT.Regpat;
with GNAT.Expect;    use GNAT.Expect;
with GNAT.OS_Lib;    use GNAT.OS_Lib;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package body Debugger.Gdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher := Compile ("\(gdb\)");
   --  Regular expressions used to recognize the prompt.



   function Parse_Type_Gdb_Ada (Debugger : Gdb_Debugger;
                                Entity   : String)
                               return Generic_Type_Access;
   --  Parse the type definition for Entity.

   procedure Skip_Blanks (Type_Str : String;
                          Index    : in out Natural);
   --  Skip all the blanks character starting from Index, and return the
   --  position of the first non-blank character.

   function Find_Char (Type_Str : String;
                       Index    : Natural;
                       Char     : Character)
                      return Natural;
   --  Find the first occurence of Char in Type_Str, starting at position
   --  Index.

   procedure Parse_Num (Type_Str : String;
                        Index    : in out Natural;
                        Result   : out Integer);
   --  Parse the integer found at position Index in Type_Str.
   --  Index is set to the position of the first character that does not
   --  belong to the integer.

   Unexpected_Type : exception;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks (Type_Str : String;
                          Index    : in out Natural)
   is
   begin
      while Index <= Type_Str'Last
        and then (Type_Str (Index) = ' '
                  or else Type_Str (Index) = ASCII.HT
                  or else Type_Str (Index) = ASCII.LF
                  or else Type_Str (Index) = ASCII.CR)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Blanks;

   ---------------
   -- Find_Char --
   ---------------

   function Find_Char (Type_Str : String;
                       Index    : Natural;
                       Char     : Character)
                      return Natural
   is
      I : Natural := Index;
   begin
      while I <= Type_Str'Last and then Type_Str (I) /= Char loop
         I := I + 1;
      end loop;
      return I;
   end Find_Char;

   ---------------
   -- Parse_Num --
   ---------------

   procedure Parse_Num (Type_Str : String;
                        Index    : in out Natural;
                        Result   : out Integer)
   is
      I : constant Natural := Index;
   begin
      if Type_Str (Index) = '-' then
         Index := Index + 1;
      end if;

      while Index <= Type_Str'Last
        and then Type_Str (Index) in '0' .. '9'
      loop
         Index := Index + 1;
      end loop;
      Result := Integer'Value (Type_Str (I .. Index - 1));
   end Parse_Num;

   ------------------------
   -- Parse_Type_Gdb_Ada --
   ------------------------

   function Parse_Type_Gdb_Ada (Debugger : Gdb_Debugger;
                                Entity   : String)
                               return Generic_Type_Access
   is
      Type_Str : String := Type_Of (Debugger, Entity);


      procedure Parse_Internal
        (Index     : in out Natural;
         Result    : out Generic_Values.Generic_Type_Access);
      --  Index is the initial index in the string, and is set on exit to the
      --  last character parsed.
      --  Result contains the tree of values matching Type_Str.

      procedure Parse_Array
        (Index     : in out Natural;
         Result    : out Generic_Type_Access);
      --  Parse the type describing an array.

      procedure Parse_Record
        (Index     : in out Natural;
         Result    : out Generic_Type_Access;
         End_On    : in String);
      --  Parse the type describing a record.
      --  Index should pointer after the initial "record ", and the record is
      --  assumed to end on a string like End_On.
      --  This function is also used to parse the variant part of a record.

      Last_Value : Generic_Type_Access := null;

      -----------------
      -- Parse_Array --
      -----------------

      procedure Parse_Array
        (Index     : in out Natural;
         Result    : out Generic_Type_Access)
      is
         Num_Dim : Integer := 1;
         I       : Natural := Index;
         R       : Array_Type_Access;
      begin

         --  First, find the number of dimensions

         while I <= Type_Str'Last
           and then Type_Str (I) /= ')'
         loop
            if Type_Str (I) = ',' then
               Num_Dim := Num_Dim + 1;
            end if;
            I := I + 1;
         end loop;

         --  Create the type

         R := new Array_Type (Num_Dimensions => Num_Dim);

         --  Then parse the dimensions

         Num_Dim := 1;
         Index := Index + 7;

         while Num_Dim <= R.Num_Dimensions loop
            Parse_Num (Type_Str, Index, R.Dimensions (Num_Dim).First);
            Index := Index + 4;  --  skips ' .. '
            Parse_Num (Type_Str, Index, R.Dimensions (Num_Dim).Last);
            Num_Dim := Num_Dim + 1;
            Index := Index + 2;  --  skips ', '
         end loop;

         --  Skip the type of the items
         --  ??? Should parse the type, in case we have a complex one.
         --  This means to emit a second query.

         while Index <= Type_Str'Last
           and then Type_Str (Index) /= ' '
           and then Type_Str (Index) /= ASCII.LF
         loop
            Index := Index + 1;
         end loop;

         --  Get the type of the items.
         --  Note that we can not simply do a "ptype" on the string we read
         --  after "of ", since we might not be in the right context, for
         --  instance if Entity is something like "Foo::entity".
         --  Thus, we have to do a "ptype" directly on the first item of the
         --  array.

         declare
            Index_Str : Unbounded_String;
         begin
            for J in 1 .. R.Num_Dimensions loop
               Append (Index_Str,
                       Integer'Image (R.Dimensions (J).First));
               if J /= R.Num_Dimensions then
                  Append (Index_Str, ",");
               end if;
            end loop;

            R.Item_Type := Parse_Type_Gdb_Ada
              (Debugger, Entity & "(" & To_String (Index_Str) & ")");
         end;

         Result := Generic_Type_Access (R);
      end Parse_Array;

      ------------------
      -- Parse_Record --
      ------------------

      procedure Parse_Record
        (Index     : in out Natural;
         Result    : out Generic_Type_Access;
         End_On    : in String)
      is
         I : Natural;
         Num_Fields : Natural := 0;
         R : Record_Type_Access;
         Num_Parts  : Natural := 0;
      begin
         Skip_Blanks (Type_Str, Index);
         I := Index;

         --  Count the number of fields

         while Type_Str (I .. I + End_On'Length - 1) /= End_On loop

            --  A null field ? Do no increment the count
            if Type_Str (I .. I + 4) = "null;" then
               I := I + 5;

               --  A record with a variant part ? This counts as
               --  only one field

            elsif Type_Str (I .. I + 4) = "case " then
               I := I + 5;
               while Type_Str (I .. I + 8) /= "end case;" loop
                  I := I + 1;
               end loop;
               Num_Fields := Num_Fields + 1;
               I := I + 9;

               --  Else a standard field
            else
               while Type_Str (I) /= ';' loop
                  I := I + 1;
               end loop;
               I := I + 1;
               Num_Fields := Num_Fields + 1;
            end if;

            Skip_Blanks (Type_Str, I);
         end loop;

         R := new Record_Type (Num_Fields);

         --  Now parse all the fields

         Num_Fields := 1;

         while Num_Fields <= R.Num_Fields loop

            if Type_Str (Index .. Index + 4) = "null;" then
               Index := Index + 5;

            elsif Type_Str (Index .. Index + 4) = "case " then
               Index := Index + 5;
               I := Index;

               while Type_Str (Index) /= ' ' loop
                  Index := Index + 1;
               end loop;

               R.Fields (Num_Fields).Name
                 := new String'(Type_Str (I .. Index - 1));
               R.Fields (Num_Fields).Value := null;

               --  Count the number of alternatives in the variant part.

               I := Index;
               while Type_Str (I .. I + 7) /= "end case" loop
                  if Type_Str (I .. I + 1) = "=>" then
                     Num_Parts := Num_Parts + 1;
                  end if;
                  I := I + 1;
               end loop;

               R.Fields (Num_Fields).Variant_Part
                 := new  Record_Type_Array (1 .. Num_Parts);

               --  Parses the parts, and create a record for each

               Num_Parts := 0;
               while Type_Str (Index .. Index + 3) /= "end " loop
                  while Type_Str (Index .. Index + 1) /= "=>" loop
                     Index := Index + 1;
                  end loop;

                  Index := Index + 2;
                  Num_Parts := Num_Parts + 1;

                  declare
                     Part : Generic_Type_Access;
                  begin
                     if Num_Parts = R.Fields (Num_Fields).Variant_Part'Last
                     then
                        Parse_Record (Index, Part, "end case");
                     else
                        Parse_Record (Index, Part, "when ");
                     end if;
                     R.Fields (Num_Fields).Variant_Part (Num_Parts)
                       := Record_Type_Access (Part);
                  end;

                  Skip_Blanks (Type_Str, Index);
               end loop;

               Index := Index + 9;
               Put_Line ("@@Finished parsing variant part");

               Num_Fields := Num_Fields + 1;

            --  Else a standard field
            else

               --  Get the name of the field

               I := Index;
               while Type_Str (Index) /= ':' loop
                  Index := Index + 1;
               end loop;

               R.Fields (Num_Fields).Name
                 := new String'(Type_Str (I .. Index - 1));

               --  Get the type of the field
               Index := Index + 2;
               I := Index;
               while Type_Str (Index) /= ';' loop
                  Index := Index + 1;
               end loop;

               R.Fields (Num_Fields).Value
                 := Parse_Type_Gdb_Ada
                 (Debugger,
                  Entity & "." & R.Fields (Num_Fields).Name.all);

               Index := Index + 1;
               Num_Fields := Num_Fields + 1;
            end if;

            Skip_Blanks (Type_Str, Index);
         end loop;

         Result := Generic_Type_Access (R);
      end Parse_Record;

      --------------------
      -- Parse_Internal --
      --------------------

      procedure Parse_Internal
        (Index     : in out Natural;
         Result    : out Generic_Values.Generic_Type_Access)
      is
      begin
         Skip_Blanks (Type_Str, Index);

         case Type_Str (Index) is

            when '<' =>

               --  Simple types, like <4-byte integer> and <4-byte float>
               Index := Find_Char (Type_Str, Index, '>') + 1;
               Result := new Simple_Type'(Address  => System.Null_Address,
                                          Value    => new String'(""));

            when 'a' =>

               --  Arrays, as in "array (1 .. 4, 3 .. 5) of character"

               if Index + 4 <= Type_Str'Last
                 and then Type_Str (Index .. Index + 4) = "array"
               then
                  Parse_Array (Index, Result);

               elsif Index + 5 <= Type_Str'Last
                 and then Type_Str (Index .. Index + 5) = "access"
               then
                  Result := new Access_Type;
               else
                  raise Unexpected_Type;
               end if;

            when 'r' =>

               -- A record type, as in 'record field1: integer; end record'

               if Type_Str (Index .. Index + 5) = "record" then
                  Index := Index + 7;
                  Parse_Record (Index, Result, "end record");
               else
                  raise Unexpected_Type;
               end if;

            when '(' =>

               --  Enumeration type
               while Type_Str (Index) /= ')' loop
                  Index := Index + 1;
               end loop;
               Index := Index + 1;
               Result := new Enum_Type;

            --  A type we do not expect.

            when others =>
               raise Unexpected_Type;

         end case;
      end Parse_Internal;

      Result : Generic_Type_Access;
      Index  : Natural := Type_Str'First;
   begin
      Parse_Internal (Index, Result);
      return Result;
   end Parse_Type_Gdb_Ada;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type (Debugger : Gdb_Debugger;
                        Entity   : String)
                       return Generic_Values.Generic_Type_Access
   is
   begin
      return Parse_Type_Gdb_Ada (Debugger, Entity);
   end Parse_Type;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Debugger : Gdb_Debugger;
                     Entity : String)
                    return String
   is
      Result : Expect_Match;
   begin
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);
      Send (Debugger.Process.all, "ptype " & Entity);
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Debugger.Process.all);
      begin
         if S'Length > 14 then
            return S (S'First + 7 .. S'Last - 6);
         else
            return "";
         end if;
      end;
   end Type_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : in out Gdb_Debugger) is
      Null_List : Gnat.OS_Lib.Argument_List (1 .. 0);
   begin
      Debugger.Process := new Pipes_Id'(Non_Blocking_Spawn ("gdb", Null_List));
      Add_Output_Filter (Debugger.Process.all, Trace_Filter'Access);
      Add_Input_Filter (Debugger.Process.all, Trace_Filter'Access);
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set prompt (gdb)");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set width 0");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set height 0");
      Wait_Prompt (Debugger);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : in out Gdb_Debugger) is
   begin
      Close (Debugger.Process.all);
      Free (Debugger.Process);
   end Close;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Debugger : Gdb_Debugger;
                             Executable : String)
   is
   begin
      Send (Debugger.Process.all, "file " & Executable);
      Wait_Prompt (Debugger);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : Gdb_Debugger) is
      Num : Expect_Match;
   begin
      Expect (Debugger.Process.all, Num, Prompt_Regexp);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : Gdb_Debugger) is
   begin
      Send (Debugger.Process.all, "run");
   end Run;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception (Debugger  : Gdb_Debugger;
                              Name      : String  := "";
                              Unhandled : Boolean := False)
   is
   begin
      --  ??? If language = "Ada"
      if Unhandled then
         Send (Debugger.Process.all, "break exception unhandled");
      elsif Name /= "" then
         Send (Debugger.Process.all, "break exception " & Name);
      else
         raise Unknown_Command;
      end if;
      Wait_Prompt (Debugger);
   end Break_Exception;

   ---------------
   -- Backtrace --
   ---------------

   function Backtrace (Debugger : Gdb_Debugger) return String is
      Result : Expect_Match;
   begin
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);
      Send (Debugger.Process.all, "bt");
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Debugger.Process.all);
      begin
         return S (S'First .. S'Last - 6);
      end;
   end Backtrace;

end Debugger.Gdb;
