with GNAT.Regpat; use GNAT.Regpat;

package body Codefix.Error_Manager is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Error_Message; Message : String) is
   begin
      Affect (This.Message, Message);
      Parse_Head (Message, This);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Error_Message; Line, Col : Positive) is
   begin
      Affect (This.Message, "");
      This.Line := Line;
      This.Col := Col;
   end Initialize;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (This : Error_Message) return String is
   begin
      return This.Message.all;
   end Get_Message;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Message) is
   begin
      Free (File_Cursor (This));
      Free (This.Message);
   end Free;

   ----------------
   -- Parse_Head --
   ----------------

   procedure Parse_Head (Message : String; This : out Error_Message) is
      Matches : Match_Array (0 .. 3);
      Matcher : constant Pattern_Matcher :=
         Compile ("([^:]*):([0-9]*):([0-9]*)");

   begin
      Match (Matcher, Message, Matches);

      begin
         Affect (This.File_Name,
                 Message (Matches (1).First .. Matches (1).Last));
         This.Line := Positive'Value
            (Message (Matches (2).First .. Matches (2).Last));
         This.Col := Positive'Value
            (Message (Matches (3).First .. Matches (3).Last));
      exception
         when Constraint_Error => -- et tester No_Match
            null; -- Lever une exception due au 'Value
      end;
   end Parse_Head;

   -----------
   -- Clone --
   -----------

   function Clone (This : Error_Message) return Error_Message is
      New_Message : Error_Message;
   begin
      New_Message := (Clone (File_Cursor (This)) with
                         new String'(This.Message.all));
      return New_Message;
   end Clone;

   -------------
   -- Analyse --
   -------------

   procedure Analyze
     (This        : in out Correction_Manager;
      Source_Text : Text_Interface'Class;
      Errors_List : Errors_Interface'Class;
      Callback    : Error_Callback := null) is

      Current_Message : Error_Message;
      Solutions       : Solution_List;
      New_Error       : Error_Id;

   begin
      Set_Parse_Mode (Source_Text);
      while not No_More_Messages (Errors_List) loop
         Current_Message := Get_Message (Errors_List);
         Solutions := Get_Corrections (This.Current_Text, Current_Message);
         if Length (Solutions) > 0 then
            Add_Error (This, Solutions, New_Error);
            Callback (Current_Message, New_Error, Solutions);
         end if;
      end loop;
      Set_Update_Mode (Source_Text);
   end Analyze;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (This         : in out Correction_Manager;
      Error        : Error_Id;
      Choice       : Natural;
      Later_Update : Boolean := True) is

   begin
      if Choice /= 0 then
         Append
           (This.Valid_Corrections,
            Get_Extract (Data (Error.Ptr_Solutions), Choice));
      end if;

      Remove_Nodes (This.Potential_Corrections, Error.Ptr_Solutions);
   end Validate;

   ------------
   -- Update --
   ------------

   procedure Update
     (This     : in out Correction_Manager;
      Success  : out Boolean;
      Callback : Ambiguous_Callback := null) is

      Current_Node     : Line_List.List_Node;
      Modifs_List      : Line_List.List;
      Offset_Line      : Natural := 0;
      No_More_Problems : Boolean;

   begin

      Success := False;

      Check_Ambiguities (This.Valid_Corrections, Callback, No_More_Problems);

      if not No_More_Problems then return; end if;

      Modifs_List := Sort (This.Valid_Corrections);

      Current_Node := First (Modifs_List);

      while Current_Node /= Line_List.Null_Node loop
         Update (Data (Current_Node),
                 This.Current_Text.all,
                 Offset_Line);
         Current_Node := Next (Current_Node);
      end loop;

      Free (Modifs_List);
      Success := True;
   end Update;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Correction_Manager) is
   begin
      Free (This.Potential_Corrections);
      Free (This.Valid_Corrections);
   end Free;

   -----------------------
   -- Check_Ambiguities --
   -----------------------

   procedure Check_Ambiguities
     (Solutions        : in out Solution_List;
      Callback         : Ambiguous_Callback;
      No_More_Problems : out Boolean) is

      function Delete_And_Next (Node : Extract_List.List_Node)
         return Extract_List.List_Node;

      Node_I, Node_J     : Extract_List.List_Node;
      Delete_I, Delete_J : Boolean;
      Choice             : Alternative_Choice;

      function Delete_And_Next (Node : Extract_List.List_Node)
        return Extract_List.List_Node is
         Garbage, Next_Node : Extract_List.List_Node;
      begin
         Garbage := Node;
         Next_Node := Next (Node);
         Remove_Nodes (Solutions, Prev (Garbage), Garbage);
         return Next_Node
      end Delete_And_Next;

   begin
      No_More_Problems := True;
      Node_I := First (Solutions);
      while Node_I /= Extract_List.Null_Node loop
         Node_J := Node_I;
         while Node_J /= Extract_List.Null_Node loop
            if Callback = null then
               No_More_Problems := False;
               return;
            else
               Callback (Data (Node_I), Data (Node_J), Choice);
               case Choice is
                  when 0 =>
                     No_More_Problems := False;
                  when 1 =>
                     Delete_I := True;
                     exit;
                  when 2 =>
                     Delete_J := True;
               end case;
               if Delete_J then
                  Node_J := Delete_And_Next (Node_J);
               else
                  Node_J := Next (Node_J);
               end if;
            end if;
         end loop
         if Delete_I then
            Node_I := Delete_And_Next (Node_I);
         else
            Node_I := Next (Node_I);
         end if;
      end loop;
   end Check_Ambiguities;

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
   -- Add_Error --
   ---------------

   procedure Add_Error
     (This      : in out Correction_Manager;
      Solutions : Solution_List
      New_Error : out Error_Id) is
   begin
      Append (This.Potential_Corrections, Solutions);
      New_Error.Ptr_Solutions := Last (This.Potential_Corrections);
   end Add_Error;

end Codefix.Error_Manager;
