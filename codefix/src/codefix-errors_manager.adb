-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002 - 2003                     --
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

with Codefix.Errors_Parser; use Codefix.Errors_Parser;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with VFS;                   use VFS;

package body Codefix.Errors_Manager is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Errors_Interface) is
      procedure Free_Pool is new Ada.Unchecked_Deallocation
        (Errors_Interface'Class, Ptr_Errors_Interface);
   begin
      if This /= null then
         Free (This.all);
         Free_Pool (This);
      end if;
   end Free;

   -----------------
   -- Get_Message --
   -----------------

   procedure Get_Message
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Current      : out Error_Message)
   is
      pragma Unreferenced (Current_Text);
--        Current_Line        : String_Access;
--        Cursor_Line         : File_Cursor;
--        File_Pos, Logic_Pos : Natural;

   begin
      if This.Preview /= Invalid_Error_Message then
         Current := This.Preview;
         This.Preview := Invalid_Error_Message;
         return;
      end if;

      Get_Direct_Message (This, Current);

--        if Current = Invalid_Error_Message then
--           return;
--        end if;

--        Cursor_Line := File_Cursor (Current);
--        Cursor_Line.Col := 1;
--
--        Assign (Current_Line, Get_Line (Current_Text, Cursor_Line));
--        Logic_Pos := 1;
--        File_Pos  := 1;

      --  ??? It seems the following loop is not needed, since GNAT correctly
      --  expands the ASCII.HTs when displaying an error message. I am not
      --  removing the loop, however, since I'm not sure that the field
      --  File_Cursor.Col should indeed be taking into accound TAB expansion.
      --  I'm simply commenting out the active of the loop below for now.

--        loop
--           exit when Logic_Pos = Current.Col
--             or else File_Pos > Current_Line'Last;
--
--           --  if Current_Line (File_Pos) = ASCII.HT then
--           --     Logic_Pos := Logic_Pos + ((-Logic_Pos) mod Tab_Width);
--           --  end if;
--
--           Logic_Pos := Logic_Pos + 1;
--           File_Pos := File_Pos + 1;
--        end loop;
--
--        Current.Col := File_Pos;
--        Free (Current_Line);
   end Get_Message;

   -----------------
   -- Get_Preview --
   -----------------

   procedure Get_Preview
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Preview      : out Error_Message) is
   begin
      if This.Preview = Invalid_Error_Message
        and then not No_More_Messages (This)
      then
         Get_Message (This, Current_Text, This.Preview);
      end if;

      Free (Preview);
      Preview := This.Preview;
   end Get_Preview;

   ------------------
   -- Skip_Message --
   ------------------

   procedure Skip_Message (This : in out Errors_Interface'Class) is
      Garbage : Error_Message;
   begin
      if This.Preview /= Invalid_Error_Message then
         This.Preview := Invalid_Error_Message;
         return;
      end if;

      Get_Direct_Message (This, Garbage);
      Free (Garbage);
   end Skip_Message;

   ----------------------------------------------------------------------------
   --  type Correction_Manager
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Correction_Manager) is
      procedure Free_Pool is new Ada.Unchecked_Deallocation
        (Correction_Manager, Ptr_Correction_Manager);
   begin
      if This /= null then
         Free (This.all);
         Free_Pool (This);
      end if;
   end Free;

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

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (This : Error_Id) return String is
   begin
      return Data (Memorized_Corrections.List_Node (This)).Category.all;
   end Get_Category;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Id_Record) is
   begin
      Free (This.Message);
      Codefix.Formal_Errors.Free (This.Solutions);
      Free (This.Category);
      Free (This.Fixed);
      Free (This.Solution_Chosen);
   end Free;

   --------------
   -- Is_Fixed --
   --------------

   function Is_Fixed (This : Error_Id) return Boolean is
   begin
      return Data (This).Fixed.all;
   end Is_Fixed;

   -------------------------
   -- Get_Number_Of_Fixes --
   -------------------------

   function Get_Number_Of_Fixes (This : Error_Id) return Natural is
   begin
      return Length (Data (This).Solutions);
   end Get_Number_Of_Fixes;

   ----------
   -- Undo --
   ----------

   procedure Undo
     (This : Error_Id; Current_Text : Text_Navigator_Abstr'Class) is
   begin
      Undo (Data (This).Solution_Chosen.all, Current_Text);
      Data (This).Fixed.all := False;
      Free (Data (This).Solution_Chosen.all);
   end Undo;

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
      Category        : String_Access;
   begin
      while not No_More_Messages (Errors_List) loop
         Get_Message (Errors_List, Source_Text, Current_Message);

         if Current_Message /= Invalid_Error_Message then

            Solutions := Solution_List (Command_List.Null_List);
            Get_Solutions
              (Source_Text,
               Errors_List,
               Current_Message,
               Category,
               Solutions);

            if Length (Solutions) > 0 then
               Add_Error
                 (This, Current_Message, Solutions, Category.all, New_Error);

               if Callback /= null then
                  Callback
                    (New_Error,
                     Source_Text,
                     This);
               end if;
            end if;

            Free (Category);
         end if;

         Free (Current_Message);
      end loop;
   end Analyze;

   -------------------------
   -- Validate_And_Commit --
   -------------------------

   procedure Validate_And_Commit
     (This         : in out Correction_Manager;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Error        : Error_Id;
      Choice       : Natural)
   is
      New_Extract : Extract;
   begin
      Secured_Execute
        (Get_Command (Get_Solutions (Error), Choice),
         Current_Text,
         New_Extract,
         This.Error_Cb);
      Commit (New_Extract, Current_Text);

      Data (Error).Fixed.all := True;
      Extract (Data (Error).Solution_Chosen.all) := New_Extract;
   end Validate_And_Commit;

   -------------------------
   -- Validate_And_Commit --
   -------------------------

   procedure Validate_And_Commit
     (This         : in out Correction_Manager;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Error        : Error_Id;
      Choice       : Text_Command'Class)
   is
      New_Extract : Extract;
   begin
      Secured_Execute
        (Choice,
         Current_Text,
         New_Extract,
         This.Error_Cb);

      Commit (New_Extract, Current_Text);

      Data (Error).Fixed.all := True;
      Extract (Data (Error).Solution_Chosen.all) := New_Extract;
   end Validate_And_Commit;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Correction_Manager) is
   begin
      Free (This.Potential_Corrections);
   end Free;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (This      : in out Correction_Manager;
      Message   : Error_Message;
      Solutions : Solution_List;
      Category  : String;
      New_Error : out Error_Id) is

      New_Error_Record : Error_Id_Record;

   begin
      New_Error_Record.Solutions := Solutions;
      New_Error_Record.Message := Clone (Message);
      Assign (New_Error_Record.Category, Category);
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

   function Search_Error
     (This         : Correction_Manager;
      File         : VFS.Virtual_File;
      Line, Column : Integer;
      Message      : String := "")
      return Error_Id
   is
      Current_Id : Error_Id := Get_First_Error (This);
      Error      : Error_Message;
   begin
      while Current_Id /= Null_Error_Id loop
         Error := Get_Error_Message (Current_Id);
         exit when Get_Line (Error) = Line
           and then Get_Column (Error) = Column
           and then Get_File (Error) = File
           and then (Message = "" or else Get_Message (Error) = Message);
         Current_Id := Next (Current_Id);
      end loop;

      return Current_Id;
   end Search_Error;

   ------------------
   -- Set_Error_Cb --
   ------------------

   procedure Set_Error_Cb
     (This     : in out Correction_Manager; Error_Cb : Execute_Corrupted) is
   begin
      This.Error_Cb := Error_Cb;
   end Set_Error_Cb;

   ------------------------
   -- Get_Previous_Error --
   ------------------------

   function Get_Previous_Error
     (This : Correction_Manager; Error : Error_Id) return Error_Id is
   begin
      return Prev (This.Potential_Corrections, Error);
   end Get_Previous_Error;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out State_Node) is
   begin
      Free (This.Error);
   end Free;

   ---------------------
   -- Set_Error_State --
   ---------------------

   procedure Set_Error_State
     (List : in out State_List; Error : String; State : Error_State)
   is
      Node : State_Lists.List_Node := First (List);
   begin
      while Node /= State_Lists.Null_Node loop
         if Data (Node).Error.all = Error then
            Set_Data (Node, (new String'(Error), State));
            return;
         end if;
         Node := Next (Node);
      end loop;

      Append (List, (new String'(Error), State));
   end Set_Error_State;

   ---------------------
   -- Get_Error_State --
   ---------------------

   function Get_Error_State
     (List : State_List; Error : String) return Error_State
   is
      Node : State_Lists.List_Node := First (List);
   begin
      while Node /= State_Lists.Null_Node loop
         if Data (Node).Error.all = Error then
            return Data (Node).State;
         end if;
         Node := Next (Node);
      end loop;

      return Unknown;
   end Get_Error_State;

end Codefix.Errors_Manager;
