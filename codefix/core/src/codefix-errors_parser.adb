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

with GNATCOLL.Traces;  use GNATCOLL.Traces;

package body Codefix.Errors_Parser is

   Me : constant Trace_Handle := Create ("GPS.CODEFIX.ERROR_PARSER");

   -------------------
   -- Get_Solutions --
   -------------------

   procedure Get_Solutions
     (Processor    : Fix_Processor;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List)
   is
      Success : Boolean := False;

   begin
      for Item of Processor.Parse_List loop
         begin
            Fix
              (Item.all,
               Current_Text,
               Message_It,
               Options,
               Solutions,
               Success);

            if Length (Solutions) /= 0 then
               --  We've found a solution. Set the parser that managed to
               --  find it and quit.
               declare
                  It : Solution_List_Iterator := First (Solutions);
               begin
                  while not At_End (It) loop
                     Set_Parser (It, Error_Parser_Access (Item));
                     It := Next (It);
                  end loop;
               end;

               exit;
            end if;
         exception
            when E : Codefix_Panic | Obsolescent_Fix =>
               Trace
                 (Handle => Me,
                  E      => E,
                  Msg    => "Cannot propose a fix.");
         end;

         exit when Success;
      end loop;

   end Get_Solutions;

   ----------------
   -- Add_Parser --
   ----------------

   procedure Add_Parser
     (Processor : in out Fix_Processor; New_Parser : Ptr_Parser) is
   begin
      Processor.Parse_List.Append (New_Parser);
   end Add_Parser;

   ----------
   -- Free --
   ----------

   procedure Free (Processor : in out Fix_Processor) is
   begin
      Processor.Parse_List.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Ptr_Parser) is
      procedure Delete is new
         Ada.Unchecked_Deallocation (Error_Parser'Class, Ptr_Parser);
   begin
      Free (Data.all);
      Delete (Data);
   end Free;

   ------------------------
   -- Initialize_Parsers --
   ------------------------

   procedure Initialize_Parsers (Processor : in out Fix_Processor) is
   begin
      for Item of Processor.Parse_List loop
         Initialize (Item.all);
      end loop;
   end Initialize_Parsers;

   ----------------------------------------------------------------------------
   --  Error_Parser
   ----------------------------------------------------------------------------

   ---------
   -- Fix --
   ---------

   procedure Fix
     (This         : Error_Parser'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Success      : out Boolean)
   is
      Message : constant Error_Message := Get_Message (Message_It);
   begin
      for J in This.Matcher'Range loop
         declare
            Matches : Match_Array (0 .. Paren_Count (This.Matcher (J).all));
         begin
            Match (This.Matcher (J).all, Get_Message (Message), Matches);
            if Matches (0) /= No_Match then
               Fix
                 (This,
                  Current_Text,
                  Message_It,
                  Options,
                  Solutions,
                  Matches);

               Success := True;
               return;
            end if;
         exception
            when Uncorrectable_Message =>
               Trace (Me, "Error cannot be corrected automatically");
               null;
         end;
      end loop;

      Success := False;
   end Fix;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Parser) is
   begin
      for J in This.Matcher'Range loop
         Free (This.Matcher (J));
      end loop;
   end Free;

end Codefix.Errors_Parser;
