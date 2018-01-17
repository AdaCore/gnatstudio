------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

package body Completion.Ada is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
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
        (Context.Buffer, Context.Offset);

      if not Ada_Context_All.Expression.Tokens.Is_Empty then
         while It /= Completion_Resolver_List_Pckg.No_Element loop
            Get_Completion_Root
              (Resolver => Element (It),
               Offset   => Ada_Context_All.Expression.Tokens.
                 First_Element.Token_First - 1,
               Context  => Ada_Context,
               Result   => Result);
            It := Next (It);
         end loop;
      end if;

      return Result;
   end Get_Initial_Completion_List;

end Completion.Ada;
