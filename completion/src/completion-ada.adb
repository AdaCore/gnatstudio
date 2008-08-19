-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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

with Language.Ada;  use Language.Ada;

package body Completion.Ada is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   function Get_Initial_Completion_List
     (Manager : access Ada_Completion_Manager; Context : Completion_Context)
      return Completion_List
   is
      It     : Completion_Resolver_List_Pckg.Cursor;
      Result : Completion_List;

      Ada_Context : constant Completion_Context := new Ada_Completion_Context;

      Ada_Context_All : Ada_Completion_Context renames
        Ada_Completion_Context (Ada_Context.all);
   begin
      Append (Manager.Contexts, Ada_Context);

      It := First (Manager.Ordered_Resolvers);

      Completion_Context_Record (Ada_Context.all) :=
        Completion_Context_Record (Context.all);
      Ada_Context_All.Expression := Parse_Expression_Backward
        (Ada_Lang, Context.Buffer, Context.Offset);

      if Token_List.Length (Ada_Context_All.Expression.Tokens) > 0 then
         while It /= Completion_Resolver_List_Pckg.No_Element loop
            Get_Completion_Root
              (Resolver => Element (It),
               Offset   => Token_List.Data
                 (Token_List.First
                    (Ada_Context_All.Expression.Tokens)).Token_First - 1,
               Context  => Ada_Context,
               Result   => Result);

            It := Next (It);
         end loop;
      end if;

      return Result;
   end Get_Initial_Completion_List;

end Completion.Ada;
