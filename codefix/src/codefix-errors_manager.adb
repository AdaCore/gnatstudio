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

with Codefix.Errors_Parser; use Codefix.Errors_Parser;

package body Codefix.Errors_Manager is

   ----------------------------------------------------------------------------
   --  type Errors_Interface
   ----------------------------------------------------------------------------

   -----------------
   -- Get_Message --
   -----------------

   procedure Get_Message
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Current      : out Error_Message)
   is
      Current_Line        : Dynamic_String;
      Cursor_Line         : File_Cursor;
      File_Pos, Logic_Pos : Natural;

   begin

      if This.Preview /= Invalid_Error_Message then
         Current := This.Preview;
         This.Preview := Invalid_Error_Message;
         return;
      end if;

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

   -----------------
   -- Get_Preview --
   -----------------

   procedure Get_Preview
     (This         : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Preview      : out Error_Message) is
   begin
      if This.Preview = Invalid_Error_Message then
         Get_Message (This, Current_Text, This.Preview);
      end if;

      Preview := This.Preview;
   end Get_Preview;

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
            Get_Solutions
              (Source_Text, Errors_List, Current_Message, Solutions);

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
     (This   : in out Correction_Manager;
      Error  : Error_Id;
      Choice : Natural)
   is
   begin
      if Choice /= 0 then
         Validate
           (This,
            Error,
            Get_Extract (Get_Solutions (Error), Choice));
      end if;
   end Validate;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (This   : in out Correction_Manager;
      Error  : Error_Id;
      Choice : Extract'Class)
   is
      pragma Unreferenced (Error);

      Temp_Fix_List          : Extract;
      Success                : Boolean;

   begin
      Unchecked_Assign (Temp_Fix_List, This.Fix_List);
      Unchecked_Free (This.Fix_List);
      Merge (This.Fix_List, Temp_Fix_List, Choice, Success);
      Free (Temp_Fix_List);
   end Validate;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This         : in out Correction_Manager;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Offset_Line : Integer := 0;
   begin
      Commit (This.Fix_List, Current_Text, Offset_Line);
      Commit (Current_Text);
      Free (This.Fix_List);
   end Commit;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Correction_Manager) is
   begin
      Free (This.Potential_Corrections);
      Free (This.Fix_List);
   end Free;

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

   function Search_Error
     (This : Correction_Manager; Message : String) return Error_Id
   is
      Current_Id : Error_Id := Get_First_Error (This);
   begin
      while Current_Id /= Null_Error_Id loop
         exit when Get_Message (Get_Error_Message (Current_Id)) = Message;
         Current_Id := Next (Current_Id);
      end loop;

      return Current_Id;
   end Search_Error;

   --------------------
   -- Update_Changes --
   --------------------

   procedure Update_Changes
     (This          : Correction_Manager;
      Current_Text  : Text_Navigator_Abstr'Class;
      Object        : in out Extract'Class;
      Success       : out Boolean;
      Already_Fixed : out Boolean)
   is
      Line_Object, Line_This : Ptr_Extract_Line;
      Merged_Extract         : Extract;
      Little_Fix_List        : Extract;
      Old_Cursor             : File_Cursor;
   begin
      Success := True;

      Line_This := Get_First_Line (This.Fix_List);
      Line_Object := Get_First_Line (Object);

      while Line_This /= null and then Line_Object /= null loop
         if Get_Cursor (Line_This.all) > Get_Cursor (Line_Object.all) then
            Line_Object := Next (Line_Object.all);
         elsif Get_Cursor (Line_Object.all) > Get_Cursor (Line_This.all) then
            Line_This := Next (Line_This.all);
         elsif Get_Cursor (Line_This.all) = Get_Cursor (Line_Object.all) then
            Add_Element
              (Little_Fix_List,
               new Extract_Line'(Clone (Line_This.all, False)));
            Old_Cursor := File_Cursor (Get_Cursor (Line_This.all));
            Line_This := Next (Line_This.all);

            if Line_This /= null
              and then File_Cursor (Get_Cursor (Line_This.all)) /= Old_Cursor
            then
               Old_Cursor := File_Cursor (Get_Cursor (Line_Object.all));

               while Line_Object /= null
                 and then Old_Cursor =
                 File_Cursor (Get_Cursor (Line_Object.all))
               loop
                  Line_Object := Next (Line_Object.all);
               end loop;
            end if;
         end if;
      end loop;

      Merge
        (Merged_Extract,
         Little_Fix_List,
         Object,
         Current_Text,
         Success);

      if not Success then
         Free (Little_Fix_List);
         Free (Merged_Extract);
         return;
      end if;

      Set_Caption (Merged_Extract, Get_Caption (Object));
      Free (Object);
      Unchecked_Assign (Object, Merged_Extract);

      Line_Object := Get_First_Line (Object);
      Line_This := Get_First_Line (This.Fix_List);

      Already_Fixed := True;

      while Line_Object /= null loop
         if Get_Context (Line_Object.all) /= Line_Created then
            Line_This := Get_Line (Line_This, Get_Cursor (Line_Object.all));

            if Line_This = null then
               Line_This := Get_First_Line (This.Fix_List);
               Already_Fixed := False;
            else
               if Get_Context (Line_Object.all) = Original_Line
                 or else Line_Object.all = Line_This.all
               then
                  Set_Coloration (Line_Object.all, False);
               else
                  Already_Fixed := False;
               end if;
            end if;
         else
            Line_This := Get_Line (Line_This, Get_Cursor (Line_Object.all));

            while Line_This /= null
              and then Get_Cursor (Line_This.all) =
              Get_Cursor (Line_Object.all)
            loop
               if Line_Object.all = Line_This.all then
                  Set_Coloration (Line_Object.all, False);
                  exit;
               end if;
               Line_This := Next (Line_This.all);
            end loop;

            if Get_Coloration (Line_Object.all) then
               Already_Fixed := False;
            end if;
         end if;

         Line_Object := Next (Line_Object.all);
      end loop;

      Free (Little_Fix_List);

   end Update_Changes;

end Codefix.Errors_Manager;
