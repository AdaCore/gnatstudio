-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Regpat; use GNAT.Regpat;

package body Codefix.Errors_Manager is

   ----------------------------------------------------------------------------
   --  type Errors_Interface
   ----------------------------------------------------------------------------

   procedure Get_Message
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Current      : out Error_Message)
   is
      Current_Line        : Dynamic_String;
      Cursor_Line         : File_Cursor;
      File_Pos, Logic_Pos : Natural;

   begin
      Get_Direct_Message (This, Current);

      if Current.File_Name = null or else Current.File_Name.all = "" then
         Free (Current);
         Current := Invalid_Error_Message;
         return;
      end if;

      Cursor_Line := File_Cursor (Current);
      Cursor_Line.Col := 1;

      Assign (Current_Line, Get_Line (Current_Text, Cursor_Line));
      Logic_Pos := 1;
      File_Pos := 1;

      loop
         exit when Logic_Pos = Current.Col;

         if Current_Line.all (File_Pos) = ASCII.HT then
            Logic_Pos := Logic_Pos + ((-Logic_Pos) mod Tab_Width);
         end if;

         Logic_Pos := Logic_Pos + 1;
         File_Pos := File_Pos + 1;
      end loop;

      Current.Col := File_Pos;
      Free (Current_Line);
   end Get_Message;

   ----------------------------------------------------------------------------
   --  type Correction_Manager
   ----------------------------------------------------------------------------

   ----------
   -- Next --
   ----------

   function Next (This : Error_Id) return Error_Id is
   begin
      return Error_Id (Next (Memorized_Corrections.List_Node (This)));
   end Next;

   -------------------
   -- Get_Solutions --
   -------------------

   function Get_Solutions (This : Error_Id) return Solution_List is
   begin
      return Data (Memorized_Corrections.List_Node (This)).Solutions;
   end Get_Solutions;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (This : Error_Id) return Error_Message is
   begin
      return Data (Memorized_Corrections.List_Node (This)).Message;
   end Get_Error_Message;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Id_Record) is
   begin
      Free (This.Message);
      Codefix.Formal_Errors.Free (This.Solutions);
   end Free;

   -------------
   -- Analyse --
   -------------

   procedure Analyze
     (This        : in out Correction_Manager;
      Source_Text : Text_Navigator_Abstr'Class;
      Errors_List : in out Errors_Interface'Class;
      Callback    : Error_Callback := null)
   is
      Current_Message : Error_Message;
      Solutions       : Solution_List;
      New_Error       : Error_Id;

   begin
      while not No_More_Messages (Errors_List) loop
         Get_Message (Errors_List, Source_Text, Current_Message);

         if Current_Message /= Invalid_Error_Message then
            Solutions := Get_Solutions (Source_Text, Current_Message);

            if Length (Solutions) > 0 then
               Add_Error (This, Current_Message, Solutions, New_Error);

               if Callback /= null then
                  Callback
                    (Current_Message,
                     New_Error,
                     Solutions,
                     Source_Text,
                     This);
               end if;
            end if;
         end if;

         Free (Current_Message);
      end loop;
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
            Clone (Get_Extract (Get_Solutions (Error), Choice)));
      end if;
   end Validate;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (This         : in out Correction_Manager;
      Error        : Error_Id;
      Choice       : Extract;
      Later_Update : Boolean := True) is
   begin
      Append (This.Valid_Corrections, Clone (Choice));
   end Validate;

   ------------
   -- Update --
   ------------

   procedure Update
     (This         : in out Correction_Manager;
      Success      : out Boolean;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Callback     : Ambiguous_Callback := null)
   is
      Current_Node     : Line_List.List_Node;
      Modifs_List      : Line_List.List;
      Offset_Line      : Integer := 0;
      No_More_Problems : Boolean;

   begin
      Success := False;
      Check_Ambiguities
        (This.Valid_Corrections, Callback, Current_Text, No_More_Problems);

      if not No_More_Problems then
         return;
      end if;

      Modifs_List := Sort (This.Valid_Corrections);
      Current_Node := First (Modifs_List);

      while Current_Node /= Line_List.Null_Node loop
         Update (Data (Current_Node), Current_Text, Offset_Line);
         Current_Node := Next (Current_Node);
      end loop;

      Free (Modifs_List);
      Update (Current_Text);
      Success := True;
   end Update;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Correction_Manager) is
   begin
      Free (This.Potential_Corrections);
      Free (This.Valid_Corrections, True);
   end Free;

   -----------------------
   -- Check_Ambiguities --
   -----------------------

   procedure Check_Ambiguities
     (Solutions        : in out Solution_List;
      Callback         : Ambiguous_Callback;
      Current_Text     : Text_Navigator_Abstr'Class;
      No_More_Problems : out Boolean)
   is
      function Delete_And_Next
        (Node : Extract_List.List_Node) return Extract_List.List_Node;
      --  Delete the current node after having got the next one.

      function Conflict (Extract_1, Extract_2 : Extract) return Boolean;
      --  Is True when Extract_1 and Extract_2 have at least one line in
      --  common.

      Node_I, Node_J     : Extract_List.List_Node;
      Delete_I, Delete_J : Boolean;
      Choice             : Alternative_Choice;

      ---------------------
      -- Delete_And_Next --
      ---------------------

      function Delete_And_Next
        (Node : Extract_List.List_Node) return Extract_List.List_Node
      is
         Garbage, Next_Node : Extract_List.List_Node;
      begin
         Garbage := Node;
         Next_Node := Next (Node);
         Remove_Nodes (Solutions, Prev (Solutions, Garbage), Garbage);
         return Next_Node;
      end Delete_And_Next;

      -------------
      -- Conflit --
      -------------

      function Conflict (Extract_1, Extract_2 : Extract) return Boolean is
         Num_1, Num_2   : Natural;
         Line_1, Line_2 : Extract_Line;

      begin
         Num_1 := Get_Number_Lines (Extract_1);
         Num_2 := Get_Number_Lines (Extract_2);

         for I_1 in 1 .. Num_1 loop
            Line_1 := Get_Record (Extract_1, I_1).all;

            for I_2 in 1 .. Num_2 loop
               Line_2 := Get_Record (Extract_2, I_2).all;

               if Get_Cursor (Line_1).Line = Get_Cursor (Line_2).Line
                 and then Get_Context (Line_1) = Line_Modified
                 and then Get_Context (Line_2) = Line_Modified
               then
                  return True;
               end if;
            end loop;
         end loop;

         return False;
      end Conflict;

   begin
      No_More_Problems := True;
      Node_I := First (Solutions);

      while Node_I /= Extract_List.Null_Node loop
         Node_J := Next (Node_I);
         Delete_I := False;

         while Node_J /= Extract_List.Null_Node loop
            Delete_J := False;

            if Conflict (Data (Node_I), Data (Node_J))then
               if Callback = null then
                  No_More_Problems := False;
                  return;
               else
                  Callback
                    (Data (Node_I), Data (Node_J), Current_Text, Choice);

                  case Choice is
                     when 0 =>
                        No_More_Problems := False;
                     when 1 =>
                        Delete_I := True;
                        exit;
                     when 2 =>
                        Delete_J := True;
                  end case;
               end if;
            end if;

            if Delete_J then
               Node_J := Delete_And_Next (Node_J);
            else
               Node_J := Next (Node_J);
            end if;
         end loop;

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
      Recorded      : Boolean;

   begin
      Node_Solution := First (List);

      while Node_Solution /= Extract_List.Null_Node loop

         for J in 1 .. Get_Number_Lines (Data (Node_Solution)) loop
            Node_Line := First (Result_List);
            Line_Temp := Clone
              (Get_Record (Data (Node_Solution), J).all,
               False);

            Recorded := False;

            while not Recorded and then Node_Line /= Line_List.Null_Node loop
               if Get_Cursor (Data (Node_Line)).Line >
                 Get_Cursor (Line_Temp).Line
               then
                  Prepend (Result_List, Node_Line, Line_Temp);
                  Recorded := True;
               end if;

               Node_Line := Next (Node_Line);
            end loop;

            if not Recorded then
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
      Message   : Error_Message;
      Solutions : Solution_List;
      New_Error : out Error_Id) is

      New_Error_Record : Error_Id_Record;

   begin
      New_Error_Record.Solutions := Solutions;
      New_Error_Record.Message := Clone (Message);
      Append (This.Potential_Corrections, New_Error_Record);
      New_Error := Last (This.Potential_Corrections);
   end Add_Error;

   ---------------------
   -- Get_First_Error --
   ---------------------

   function Get_First_Error (This : Correction_Manager) return Error_Id is
   begin
      return First (This.Potential_Corrections);
   end Get_First_Error;

   ------------------
   -- Search_Error --
   ------------------

   function Search_Error (This : Correction_Manager; Message : String)
     return Error_Id is

      Current_Id       : Error_Id := Get_First_Error (This);
      Message_Expected : Error_Message;

   begin
      while Current_Id /= Null_Error_Id loop
         exit when Get_Message (Get_Error_Message (Current_Id)) = Message;
         Current_Id := Next (Current_Id);
      end loop;

      return Current_Id;
   end Search_Error;

end Codefix.Errors_Manager;
