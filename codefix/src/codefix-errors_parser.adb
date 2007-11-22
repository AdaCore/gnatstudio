-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2007, AdaCore                 --
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

with Traces;  use Traces;

package body Codefix.Errors_Parser is

   Me : constant Debug_Handle := Create ("Codefix");

   -------------------
   -- Get_Solutions --
   -------------------

   procedure Get_Solutions
     (Processor    : Fix_Processor;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Category     : out String_Access;
      Solutions    : out Solution_List)
   is
      Current_Node : Parser_List.List_Node;
      Success      : Boolean := False;

   begin
      Current_Node := First (Processor.Parse_List);

      while Current_Node /= Parser_List.Null_Node loop
         if Get_Error_State
           (Processor.Preferences,
            Data (Current_Node).Category.all) /= Disabled
         then
            begin
               Fix
                 (Data (Current_Node).all,
                  Current_Text,
                  Message_It,
                  Options,
                  Solutions,
                  Success);

            exception
               when E : Codefix_Panic =>
                  Traces.Trace
                    (Handle => Me,
                     E      => E,
                     Msg    => "Cannot propose a fix.");
            end;
         end if;

         if Success then
            Assign (Category, Data (Current_Node).Category);
            exit;
         end if;

         Current_Node := Next (Current_Node);
      end loop;

   end Get_Solutions;

   ----------------
   -- Add_Parser --
   ----------------

   procedure Add_Parser
     (Processor : in out Fix_Processor; New_Parser : Ptr_Parser) is
   begin
      Append (Processor.Parse_List, New_Parser);
   end Add_Parser;

   ------------------
   -- Free_Parsers --
   ------------------

   procedure Free (Processor : in out Fix_Processor) is
   begin
      Free (Processor.Parse_List);
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
      Current_Node : Parser_List.List_Node;
   begin
      Current_Node := First (Processor.Parse_List);

      while Current_Node /= Parser_List.Null_Node loop
         Initialize (Data (Current_Node).all);
         Current_Node := Next (Current_Node);
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

   --  The following functions are not commented after their name, but after
   --  the first parameter.

end Codefix.Errors_Parser;
