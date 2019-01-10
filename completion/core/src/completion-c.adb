------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with GNATCOLL.Traces; use GNATCOLL.Traces;

package body Completion.C is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
     (Manager : access C_Completion_Manager;
      Context : Completion_Context)
      return Completion_List
   is
      function In_String_Literal (Offset : String_Index_Type) return Boolean;
      --  Returns true if Offset references a location inside of an string
      --  literal

      function In_String_Literal (Offset : String_Index_Type) return Boolean is
         In_String : Boolean := False;
         J         : Natural := Natural (Context.Offset);

      begin
         --  Locate the beginning of the current line

         while J > Context.Buffer'First
            and then Context.Buffer (J) /= ASCII.LF
         loop
            J := J - 1;
         end loop;

         while J < Natural (Offset) loop
            if Context.Buffer (J) = '"' then
               In_String := not In_String;
            end if;

            J := J + 1;
         end loop;

         return In_String;
      end In_String_Literal;

      New_Context : constant Completion_Context :=
        new C_Completion_Context'(Buffer => Context.Buffer,
                                  Offset => Context.Offset,
                                  Lang   => Context.Lang,
                                  File   => Context.File,
                                  Expression => Null_Parsed_Expression);
      New_Context_All : C_Completion_Context renames
                          C_Completion_Context (New_Context.all);

      Result   : Completion_List;
      Prev_Tok : Token_Record;

   begin
      if Active (Clang_Support) then
         for Item of Manager.Ordered_Resolvers loop
            Get_Completion_Root
              (Resolver => Item,
               Offset   => Context.Offset,
               Context  => New_Context,
               Result   => Result);
         end loop;
      else
         --  TODO ??? This branch of the code is obsolete and kept for legacy
         --  reasons. We probably want to remove it at some point in the future
         if In_String_Literal (Context.Offset) then
            return Null_Completion_List;
         end if;

         Append (Manager.Contexts, New_Context);

         New_Context_All.Expression :=
           Parse_Expression_Backward (Context.Buffer, Context.Offset);

         if New_Context_All.Expression /= Null_Parsed_Expression then
            Prev_Tok := New_Context_All.Expression.Tokens.Last_Element;

            for Item of Manager.Ordered_Resolvers loop
               Get_Completion_Root
                 (Resolver => Item,
                  Offset   => Prev_Tok.Token_First - 1,
                  Context  => New_Context,
                  Result   => Result);
            end loop;
         end if;

      end if;
      return Result;
   end Get_Initial_Completion_List;

end Completion.C;
