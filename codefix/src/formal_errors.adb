with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Regpat; use GNAT.Regpat;

package body Formal_Errors is

   -----------------
   -- Get_Extract --
   -----------------

   function Get_Extract
     (This     : Solution_List;
      Position : Positive)
     return Extract is

      Current_Node : Extract_List.List_Node;

   begin

      Current_Node := First (This);

      for J in 1 .. Position - 1 loop
         Current_Node := Next (Current_Node);
      end loop;

      return Data (Current_Node);

   end Get_Extract;

   --------------------
   -- Add_Correction --
   --------------------

   procedure Add_Correction
     (This           : in out Correction_Manager;
      New_Correction : Extract) is

   begin
      Append (This.Corrections, New_Correction);
   end Add_Correction;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Correction_Manager) is
      Current_Node : Line_List.List_Node;
      Modifs_List  : Line_List.List;
      Offset_Line  : Natural := 0;

   begin

      Modifs_List := Sort (This.Corrections);

      Current_Node := First (Modifs_List);

      while Current_Node /= Line_List.Null_Node loop
         Update (Data (Current_Node),
                 This.Current_Text.all,
                 Offset_Line);
         Current_Node := Next (Current_Node);
      end loop;

      Free (Modifs_List);
   end Update;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Correction_Manager) is
   begin
      Free (This.Corrections);
   end Free;

   ----------
   -- Sort --
   ----------

   function Sort (List : Solution_List) return Line_List.List is
      Node_Solution : Extract_List.List_Node;
      Node_Line     : Line_List.List_Node;
      Line_Temp     : Extract_Line;
      Result_List   : Line_List.List;

   begin
      Node_Solution := First (List);
      while Node_Solution /= Extract_List.Null_Node loop
         for J in 1 .. Get_Number_Lines (Data (Node_Solution)) loop
            Node_Line := First (Result_List);
            Line_Temp := Clone (Get_Record (Data (Node_Solution), J).all);

            while Node_Line /= Line_List.Null_Node loop
               if Get_Cursor (Data (Node_Line)).Line >
                 Get_Cursor (Line_Temp).Line
               then
                  Prepend (Result_List, Node_Line, Line_Temp);
               end if;
               Node_Line := Next (Node_Line);
            end loop;

            if Node_Line = Line_List.Null_Node then
               Append (Result_List, Line_Temp);
            end if;

         end loop;
         Node_Solution := Next (Node_Solution);
      end loop;

      return Result_List;
   end Sort;

   ---------------
   -- Should_Be --
   ---------------

   function Should_Be
     (Current_Text : Text_Interface'Class;
      Message      : Error_Message;
      Str_Expected : String;
      Str_Red      : String := "")
      return Extract is

      New_Extract : Extract;
      Line_Cursor : File_Cursor := File_Cursor (Message);

   begin
      Line_Cursor.Col := 1;
      Get_Line (Current_Text,
                Line_Cursor,
                New_Extract);
      if Str_Red = "" then
         Replace_Word
           (New_Extract,
            Message,
            Str_Expected);
      else
         Replace_Word
           (New_Extract,
            Message,
            Str_Expected, "^(" & Str_Red & ")");
      end if;

      return New_Extract;
   end Should_Be;

   -----------------
   -- Wrong_Order --
   -----------------

   function Wrong_Order
     (Current_Text                 : Text_Interface'Class;
      Message                      : Error_Message;
      First_String, Second_String  : String)
      return Extract is

      New_Extract               : Extract;
      Matches                   : Match_Array (1 .. 1);
      Matcher                   : constant Pattern_Matcher :=
      Compile ("(" & Second_String & ") ", Case_Insensitive);

      Second_Cursor : File_Cursor := File_Cursor (Message);
      Line_Cursor   : File_Cursor := File_Cursor (Message);

   begin
      Second_Cursor.Col := 1;
      loop
         Match (Matcher, Get_Line (Current_Text, Second_Cursor), Matches);
         exit when Matches (1) /= No_Match;
         Second_Cursor.Line := Second_Cursor.Line - 1;
      end loop;

      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      if Message.Line /= Second_Cursor.Line then
         Get_Line (Current_Text, Second_Cursor, New_Extract);
      end if;

      Second_Cursor.Col := Matches (1).First;

      Replace_Word
        (New_Extract,
         Message,
         Second_String,
         "^(" & First_String & ")");

      Replace_Word
        (New_Extract,
         Second_Cursor,
         First_String,
         "^(" & Second_String & ")");

      return New_Extract;

   end Wrong_Order;

   -----------------
   -- Expected --
   -----------------

   function Expected
     (Current_Text    : Text_Interface'Class;
      Message         : Error_Message;
      String_Expected : String;
      Add_Spaces      : Boolean := True)
     return Extract is

      New_Extract  : Extract;
      New_Str      : Dynamic_String;
      Line_Cursor  : File_Cursor := File_Cursor (Message);
      Space_Cursor : File_Cursor := File_Cursor (Message);

   begin

      Affect (New_Str, String_Expected);

      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      Space_Cursor.Col := Space_Cursor.Col - 1;
      if Add_Spaces and then
         Message.Col > 1 and then
         Get
          (Current_Text,
           Space_Cursor,
           1) /= " "
      then
         Affect (New_Str, " " & New_Str.all);
      end if;

      Space_Cursor.Col := Space_Cursor.Col + 1;
      if Add_Spaces
        and then Message.Col < Line_Length
          (Current_Text, Line_Cursor)
        and then Get
                   (Current_Text,
                    Space_Cursor,
                    1) /= " "
      then
         Affect (New_Str, New_Str.all & " ");
      end if;

      Add_Word
        (New_Extract,
         Message,
         New_Str.all);

      Free (New_Str);

      return New_Extract;

   end Expected;

   ----------------
   -- Unexpected --
   ----------------

   function Unexpected
     (Current_Text      : Text_Interface'Class;
      Message           : Error_Message;
      String_Unexpected : String;
      Mode              : String_Mode := Text_Ascii)
     return Extract is

      New_Extract : Extract;
      New_Str     : Dynamic_String;
      Line_Cursor : File_Cursor := File_Cursor (Message);

   begin
      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      New_Str := new String'(Get_String (New_Extract));

      case Mode is
         when Text_Ascii =>
            Set_String (New_Extract, New_Str.all (1 .. Message.Col - 1) &
                        New_Str.all (Message.Col +
                           String_Unexpected'Length
                              .. New_Str.all'Length));
         when Regular_Expression =>
            Set_String (New_Extract, New_Str.all (1 .. Message.Col - 1) &
                        New_Str.all (Message.Col +
                                     Get_Word_Length (New_Extract,
                                                      Message,
                                                      String_Unexpected)
                                     ..  New_Str.all'Length));
      end case;

      Set_String (New_Extract, New_Str.all (1 .. Message.Col - 1) &
                  New_Str.all (Message.Col + String_Unexpected'Length ..
                     New_Str.all'Length));
      Free (New_Str);

      return New_Extract;
   end Unexpected;

   ------------------
   -- Wrong_Column --
   ------------------

   function Wrong_Column
     (Current_Text    : Text_Interface'Class;
      Message         : Error_Message;
      Column_Expected : Natural := 0)
      return Extract is

      function Most_Close (Size_Red : Positive) return Positive;

      function Most_Close (Size_Red : Positive) return Positive is
      begin
         Put_Line (Integer'Image (Size_Red));

         case Size_Red - 1 mod 3 is
            when 0 =>
               return Size_Red + 3;
               --  not - 3 because of the case where Size_Red = 1
            when 1 =>
               return Size_Red - 1;
            when 2 =>
               return Size_Red + 1;
            when others =>
               return 1;
         end case;
      end Most_Close;

      New_Extract  : Extract;
      Str_Red      : Dynamic_String;
      White_String : constant String (1 .. 256) := (others => ' ');
      Line_Cursor  : File_Cursor := File_Cursor (Message);

   begin
      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);
      Str_Red := new String'(Get_String (New_Extract));

      if Column_Expected = 0 then
         Set_String (New_Extract,
                     White_String (1 .. Most_Close (Message.Col)) &
                     Str_Red (Message.Col .. Str_Red'Length));
      else
         Set_String (New_Extract,
                     White_String (1 .. Column_Expected - 1) &
                     Str_Red (Message.Col .. Str_Red'Length));
      end if;

      Free (Str_Red);

      return New_Extract;

   end Wrong_Column;

   -------------------------
   -- With_Clause_Missing --
   -------------------------

   function With_Clause_Missing
     (Current_Text   : Text_Interface'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String)
     return Extract is

      New_Cursor  : File_Cursor := (0, 1, Cursor.File_Name);
      New_Extract : Extract;

   begin
      New_Line
        (New_Extract,
         New_Cursor,
         "with " & Missing_Clause & "; use " & Missing_Clause & ";");
      return New_Extract;
   end With_Clause_Missing;

   ----------------
   -- Bad_Casing --
   ----------------

   function Bad_Casing
     (Current_Text : Text_Interface'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed)
     return Extract is

      function To_Correct_Case (Str : String) return String;

      function To_Correct_Case (Str : String) return String is
         Maj        : Boolean := True;
         New_String : String (Str'Range);
      begin
         case Word_Case is
            when Mixed =>
               for J in Str'Range loop
                  if Maj then
                     New_String (J) := To_Upper (Str (J));
                  else
                     New_String (J) := To_Lower (Str (J));
                  end if;
                  if Str (J) = '_' or else Str (J) = '.' then
                     Maj := True;
                  else
                     Maj := False;
                  end if;
               end loop;
            when Upper =>
               for J in Str'Range loop
                  New_String (J) := To_Upper (Str (J));
               end loop;
            when Lower =>
               for J in Str'Range loop
                  New_String (J) := To_Lower (Str (J));
               end loop;
         end case;
         return New_String;
      end To_Correct_Case;

      New_Extract : Extract;
      Cursor_Line : File_Cursor := File_Cursor (Cursor);
      Word        : Pattern_Matcher := Compile ("([\w]+)");
      Matches     : Match_Array (0 .. 1);
      Size        : Integer;
      Line        : Dynamic_String;

   begin
      Cursor_Line.Col := 1;
      Get_Line (Current_Text, Cursor_Line, New_Extract);
      Affect (Line, Get_String (New_Extract));
      Match (Word, Line.all (Cursor.Col .. Line.all'Length), Matches);

      Size := Matches (1).Last - Matches (1).First + 1;

      if Correct_Word /= "" then
         Replace_Word
           (New_Extract,
            Cursor,
            Correct_Word (Correct_Word'Last - Size + 1 .. Correct_Word'Last));
      else
         Replace_Word
           (New_Extract,
            Cursor,
            To_Correct_Case (Line.all
                             (Matches (1).First .. Matches (1).Last)));
      end if;

      return New_Extract;
   end Bad_Casing;

end Formal_Errors;
