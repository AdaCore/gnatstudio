------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with String_Utils; use String_Utils;

package body Codefix.Error_Lists is

   use type Basic_Types.Visible_Column_Type;

   procedure Internal_Add_Error
     (List : Error_Message_List; Error : Error_Message);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : out Error_Message_List) is
   begin
      This := new Error_Message_List_Record;
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Message_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Error_Message_List_Record, Error_Message_List);
   begin
      Clear_Messages (This);
      Free (This.File_Regexp);
      Unchecked_Free (This);
   end Free;

   ---------------------
   -- Add_Errors_From --
   ---------------------

   procedure Add_Errors_From
     (List     : Error_Message_List;
      Registry : Project_Registry_Access;
      Messages : String)
   is
      Last_Index    : Integer := Messages'First;
      Current_Index : Integer := Messages'First;
      Order_Id      : Long_Long_Integer := 0;
   begin
      while Current_Index < Messages'Last loop
         Order_Id := Order_Id + 1;
         Last_Index := Current_Index;

         Skip_To_Char (Messages, Current_Index, ASCII.LF);

         declare
            Error : Error_Message;
         begin
            Initialize
              (Error,
               Registry      => Registry,
               Error_Line    =>
                 To_Unbounded_String
                   (Messages (Last_Index .. Current_Index - 1)),
               Regexp        => List.File_Regexp.all,
               File_Index    => List.File_Index,
               Line_Index    => List.Line_Index,
               Col_Index     => List.Col_Index,
               Msg_Index     => List.Msg_Index,
               Style_Index   => List.Style_Index,
               Warning_Index => List.Warning_Index,
               Order         => Order_Id);

            Internal_Add_Error (List, Error);
         end;

         Current_Index := Current_Index + 1;
      end loop;
   end Add_Errors_From;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (List    : Error_Message_List;
      File    : Virtual_File;
      Line    : Integer;
      Column  : Visible_Column_Type;
      Message : Unbounded_String;
      Order   : Long_Long_Integer)
   is
      Error : Error_Message;
   begin
      Initialize
        (This    => Error,
         File    => File,
         Line    => Line,
         Col     => Column,
         Message => Message,
         Order   => Order);

      Internal_Add_Error (List, Error);
   end Add_Error;

   ------------------------
   -- Internal_Add_Error --
   ------------------------

   procedure Internal_Add_Error
     (List : Error_Message_List; Error : Error_Message)
   is
      Loc      : Messages_Loc;
      Loc_List : Internal_List_Access;
   begin
      if Error /= Invalid_Error_Message then
         Loc.File := Error.Get_File;
         Loc.Line := Error.Get_Line;
         Loc.Column := Error.Get_Column;

         if Contains (List.Messages, Loc) then
            Loc_List := Element (List.Messages, Loc);
         else
            Loc_List := new Internal_Message_List_Pckg.Set;
            Insert (List.Messages, Loc, Loc_List);
         end if;

         Insert (Loc_List.all, Error);
      end if;
   end Internal_Add_Error;

   --------------------
   -- Clear_Messages --
   --------------------

   procedure Clear_Messages (List : Error_Message_List) is
      Cur : Error_Message_Container.Cursor := First (List.Messages);
      Loc_List : Internal_List_Access;
   begin
      while Cur /= Error_Message_Container.No_Element loop
         Loc_List := Element (Cur);
         Free (Loc_List);

         Cur := Next (Cur);
      end loop;

      Clear (List.Messages);
   end Clear_Messages;

   --------------------------------
   -- Clear_Messages_At_Location --
   --------------------------------

   procedure Clear_Messages_At_Location
     (List   : Error_Message_List;
      File   : Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type)
   is
      Loc : Messages_Loc;
      Loc_List : Internal_List_Access;
   begin
      Loc.File := File;
      Loc.Line := Line;
      Loc.Column := Column;

      if Contains (List.Messages, Loc) then
         Loc_List := Element (List.Messages, Loc);
         Free (Loc_List);
         Delete (List.Messages, Loc);
      end if;
   end Clear_Messages_At_Location;

   -----------
   -- First --
   -----------

   function First (List : Error_Message_List) return Error_Message_Iterator
   is
      It : Error_Message_Iterator;
   begin
      It.Map_Cur := First (List.Messages);

      if It.Map_Cur /= Error_Message_Container.No_Element then
         It.Message_List := Element (First (List.Messages));
         It.List_Cur := First (It.Message_List.all);
      else
         It.List_Cur := Internal_Message_List_Pckg.No_Element;
      end if;

      return It;
   end First;

   -----------------------
   -- First_At_Location --
   -----------------------

   function First_At_Location
     (List   : Error_Message_List;
      File   : Virtual_File;
      Line   : Integer;
      Column : Visible_Column_Type;
      Order  : Long_Long_Integer)
      return Error_Message_Iterator
   is
      Loc : Messages_Loc;
      It : Error_Message_Iterator;
   begin
      Loc.File := File;
      Loc.Line := Line;
      Loc.Column := Column;

      if not Contains (List.Messages, Loc) then
         return
           (Map_Cur      => Error_Message_Container.No_Element,
            List_Cur     => Internal_Message_List_Pckg.No_Element,
            Message_List => null);
      else
         It :=
           (Map_Cur      => Error_Message_Container.No_Element,
            List_Cur     => First (Element (List.Messages, Loc).all),
            Message_List => Element (List.Messages, Loc));

         while It.List_Cur /= Internal_Message_List_Pckg.No_Element loop
            declare
               Message : constant Error_Message := Get_Message (It);
            begin
               exit when Message.Get_Order = Order
                 and then not Message.Is_Cancelled;
            end;

            It.List_Cur := Next (It.List_Cur);
         end loop;

         return It;
      end if;
   end First_At_Location;

   ----------
   -- Next --
   ----------

   function Next (It : Error_Message_Iterator) return Error_Message_Iterator is
      Result : Error_Message_Iterator := It;
   begin
      Result.List_Cur := Next (Result.List_Cur);

      if Result.List_Cur = Internal_Message_List_Pckg.No_Element
        and then Result.Map_Cur /= Error_Message_Container.No_Element
      then
         Result.Map_Cur := Next (Result.Map_Cur);

         if Result.Map_Cur /= Error_Message_Container.No_Element then
            Result.Message_List := Element (Result.Map_Cur);
            Result.List_Cur := First (Result.Message_List.all);
         end if;
      end if;

      if not At_End (Result) and then Get_Message (Result).Is_Cancelled then
         return Next (Result);
      else
         return Result;
      end if;
   end Next;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (It : Error_Message_Iterator) return Error_Message is
   begin
      return Element (It.List_Cur);
   end Get_Message;

   --------------------
   -- Cancel_Message --
   --------------------

   procedure Cancel_Message (It : Error_Message_Iterator) is
      Message : Error_Message := Get_Message (It);
   begin
      Message.Cancel;
      Internal_Message_List_Pckg.Replace_Element
        (It.Message_List.all, It.List_Cur, Message);
   end Cancel_Message;

   ------------
   -- At_End --
   ------------

   function At_End (It : Error_Message_Iterator) return Boolean is
   begin
      return It.List_Cur = Internal_Message_List_Pckg.No_Element;
   end At_End;

   ----------------
   -- Set_Regexp --
   ----------------

   procedure Set_Regexp
     (This                    : in out Error_Message_List;
      File_Location_Regexp    : GNAT.Regpat.Pattern_Matcher;
      File_Index_In_Regexp    : Integer;
      Line_Index_In_Regexp    : Integer;
      Col_Index_In_Regexp     : Integer;
      Msg_Index_In_Regexp     : Integer;
      Style_Index_In_Regexp   : Integer;
      Warning_Index_In_Regexp : Integer) is
   begin
      Free (This.File_Regexp);
      This.File_Regexp   := new Pattern_Matcher'(File_Location_Regexp);
      This.File_Index    := File_Index_In_Regexp;
      This.Line_Index    := Line_Index_In_Regexp;
      This.Col_Index     := Col_Index_In_Regexp;
      This.Msg_Index     := Msg_Index_In_Regexp;
      This.Style_Index   := Style_Index_In_Regexp;
      This.Warning_Index := Warning_Index_In_Regexp;
   end Set_Regexp;

   --------
   -- Lt --
   --------

   function Lt (Left, Right : Error_Message) return Boolean is
   begin
      return Left.Get_Order < Right.Get_Order;
   end Lt;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Messages_Loc) return Boolean is
   begin
      return Left.Line < Right.Line
        or else
          (Left.Line = Right.Line
           and then
             (Left.Column < Right.Column
              or else
                (Left.Column = Right.Column
                 and then Left.File < Right.File)));
   end "<";

end Codefix.Error_Lists;
