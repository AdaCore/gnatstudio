------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with GNATCOLL.VFS;                   use GNATCOLL.VFS;
with GNATCOLL.Xref;

package body Codefix.Errors_Manager is
   use type GNATCOLL.Xref.Visible_Column;

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

   overriding function Next (This : Error_Id) return Error_Id is
   begin
      return Error_Id (Std_Vectors.Next (Std_Vectors.Cursor (This)));
   end Next;

   -------------------
   -- Get_Solutions --
   -------------------

   function Get_Solutions (This : Error_Id) return Solution_List is
   begin
      return Element (This).Solutions;
   end Get_Solutions;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (This : Error_Id) return Error_Message is
   begin
      return Element (This).Message;
   end Get_Error_Message;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Id_Record) is
   begin
      Free (This.Message);
      Codefix.Formal_Errors.Free_List (This.Solutions);
      Free (This.Fixed);
   end Free;

   --------------
   -- Is_Fixed --
   --------------

   function Is_Fixed (This : Error_Id) return Boolean is
   begin
      return Element (This).Fixed.all;
   end Is_Fixed;

   -------------------------
   -- Get_Number_Of_Fixes --
   -------------------------

   function Get_Number_Of_Fixes (This : Error_Id) return Natural is
   begin
      return Length (Element (This).Solutions);
   end Get_Number_Of_Fixes;

   ----------
   -- Undo --
   ----------

   procedure Undo
     (This : Error_Id; Current_Text : Text_Navigator_Abstr'Class) is
   begin
      null;
   end Undo;

   -------------
   -- Analyze --
   -------------

   procedure Analyze
     (This        : in out Correction_Manager;
      Processor   : Fix_Processor;
      Source_Text : Text_Navigator_Abstr'Class;
      Errors_List : in out Error_Message_List;
      Options     : Fix_Options;
      Callback    : Error_Callback := null)
   is
      Current_Message  : Error_Message;
      Solutions        : Solution_List;
      New_Error        : Error_Id;
      Previous_Message : Error_Message := Invalid_Error_Message;
      It               : Error_Message_Iterator := First (Errors_List);
   begin
      while not At_End (It) loop
         Current_Message := Get_Message (It);

         if Current_Message /= Invalid_Error_Message then
            --  Ignore style and warning errors if there are already standard
            --  errors on the same line. The latter needs to be fixed first,
            --  since their correction will often make the fix for the
            --  standard error irrelevant.
            --
            --  We are assuming that style errors and/or warnings are grouped
            --  together.

            if not Is_Style_Or_Warning (Current_Message) then
               while Previous_Message /= Invalid_Error_Message
                 and then Is_Style_Or_Warning (Previous_Message)
                 and then not Is_Style_Or_Warning (Current_Message)
                 and then Get_Line (Previous_Message)
                 = Get_Line (Current_Message)
                 and then Get_File (Previous_Message)
                 = Get_File (Current_Message)
               loop
                  --  Remove previous from list

                  This.Potential_Corrections.Delete_Last;

                  if not This.Potential_Corrections.Is_Empty then
                     Previous_Message :=
                       Std_Vectors.Element
                         (This.Potential_Corrections.Last).Message;
                  else
                     Previous_Message := Invalid_Error_Message;
                  end if;
               end loop;
            end if;

            if Previous_Message /= Invalid_Error_Message
              and then Is_Style_Or_Warning (Current_Message)
              and then not Is_Style_Or_Warning (Previous_Message)
              and then Get_Line (Previous_Message) = Get_Line (Current_Message)
            then
               --  Ignore this error if it's a warning and there has already
               --  been a real error on this line.
               null;

            else
               Solutions := Null_Solution_List;
               Get_Solutions
                 (Processor,
                  Source_Text,
                  It,
                  Options,
                  Solutions);

               if Length (Solutions) > 0 then
                  Add_Error
                    (This, Current_Message, Solutions, New_Error);

                  if Callback /= null then
                     Callback (New_Error, Source_Text, This);
                  end if;

                  Previous_Message := Current_Message;
               end if;
            end if;
         end if;

         Free (Current_Message);

         It := Next (It);
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
   begin
      Secured_Execute
        (Get_Command (Get_Solutions (Error), Choice).all,
         Current_Text,
         This.Error_Cb);

      Element (Error).Fixed.all := True;
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
   begin
      Secured_Execute
        (Choice,
         Current_Text,
         This.Error_Cb);

      Element (Error).Fixed.all := True;
   end Validate_And_Commit;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Correction_Manager) is
   begin
      This.Potential_Corrections.Clear;
      Free (This.Error_Cb);
   end Free;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (This      : in out Correction_Manager;
      Message   : Error_Message;
      Solutions : Solution_List;
      New_Error : out Error_Id)
   is
      New_Error_Record : Error_Id_Record;
   begin
      New_Error_Record.Solutions := Solutions;
      New_Error_Record.Message := Clone (Message);
      Append (This.Potential_Corrections, New_Error_Record);
      New_Error := Error_Id (This.Potential_Corrections.Last);
   end Add_Error;

   ---------------------
   -- Get_First_Error --
   ---------------------

   function Get_First_Error (This : Correction_Manager) return Error_Id is
   begin
      return Error_Id (This.Potential_Corrections.First);
   end Get_First_Error;

   --------------------------
   -- Get_Number_Of_Errors --
   --------------------------

   function Get_Number_Of_Errors (This : Correction_Manager) return Natural is
   begin
      return Natural (This.Potential_Corrections.Length);
   end Get_Number_Of_Errors;

   ------------------
   -- Search_Error --
   ------------------

   function Search_Error
     (This         : Correction_Manager;
      File         : GNATCOLL.VFS.Virtual_File;
      Line         : Integer;
      Column       : Visible_Column_Type;
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

   function Get_Previous_Error (Error : Error_Id) return Error_Id is
   begin
      return Previous (Error);
   end Get_Previous_Error;

end Codefix.Errors_Manager;
