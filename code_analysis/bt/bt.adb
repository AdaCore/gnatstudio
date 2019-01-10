------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded.Hash;

package body BT is

   use Ada.Containers;

   -------------------------
   -- Readable_Event_Kind --
   -------------------------

   function Readable_Event_Kind (Event : Event_Enum) return String is
   begin
      case Event is
         when Other_From_Assume_Event =>
            return "other_assume";
         when Precond_Assume_Event =>
            return "precond_assume";
         when Non_Invalid_Input_Assume_Event =>
            return "non_invalid_input_values";
         when Check_Event =>
            return "check";
         when Postcondition_Assume_Event =>
            return "postcond_assume";
         when Induction_Var_Assume_Event =>
            return "induction_var";
         when Precondition_Event =>
            return "precondition check";
         when Jump_Event =>
            return "jump";
         when Conditional_Precond_Checks_Event =>
            return "conditional precondition checks";
      end case;
   end Readable_Event_Kind;

   --------
   -- EQ --
   --------

   function EQ (B1, B2 : BT_Info) return Boolean is
      use Message_Kinds;
   begin
      return B1.Event = B2.Event
        and then B1.Kind = B2.Kind
        and then B1.Sloc.Line = B2.Sloc.Line
        and then B1.Sloc.Column = B2.Sloc.Column;
   end EQ;

   -----------------
   -- Same_Srcpos --
   -----------------

   function Same_Srcpos (S1, S2 : Src_Pos_Record) return Boolean is
   begin
      return S1.File_Name = S2.File_Name
        and then S1.Sloc.Line = S2.Sloc.Line
        and then S1.Sloc.Column = S2.Sloc.Column;
   end Same_Srcpos;

   -----------------
   -- Srcpos_Hash --
   -----------------

   function Srcpos_Hash (S : Src_Pos_Record) return Hash_Type is
   begin
      return Hash (S.File_Name) +
        Hash_Type (S.Sloc.Line) + Hash_Type (S.Sloc.Column);
   end Srcpos_Hash;

end BT;
