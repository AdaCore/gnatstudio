-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2011, AdaCore                   --
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

package body Completion.C is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
     (Manager : access C_Completion_Manager;
      Context : Completion_Context)
      return Completion_List
   is
      Prev_Tok : Token_Record;

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean);
      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean) is
      begin
         Prev_Tok := Token;
         Stop := True;
      end Handle_Token;

      --  Local variables

      New_Context : constant Completion_Context :=
        new C_Completion_Context'(Buffer => Context.Buffer,
                                  Offset => Context.Offset,
                                  Lang   => Context.Lang,
                                  File   => Context.File);

      It     : Completion_Resolver_List_Pckg.Cursor;
      Result : Completion_List;

   begin
      Append (Manager.Contexts, New_Context);

      --  Scan backward to locate the beginning of the current word

      Context.Lang.Parse_Tokens_Backwards
         (Buffer       => Context.Buffer.all,
          Start_Offset => Context.Offset,
          End_Offset   => 0,
          Callback     => Handle_Token'Access);

      It := First (Manager.Ordered_Resolvers);
      while It /= Completion_Resolver_List_Pckg.No_Element loop
         Get_Completion_Root
           (Resolver => Element (It),
            Offset   => Prev_Tok.Token_First - 1,
            Context  => New_Context,
            Result   => Result);

         It := Next (It);
      end loop;

      return Result;
   end Get_Initial_Completion_List;

end Completion.C;
