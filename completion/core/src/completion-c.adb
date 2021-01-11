------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2021, AdaCore                     --
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

with Language.Libclang;

package body Completion.C is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
     (Manager : access C_Completion_Manager;
      Context : Completion_Context) return Completion_List
   is
      New_Context : constant Completion_Context :=
        new C_Completion_Context'(Buffer => Context.Buffer,
                                  Offset => Context.Offset,
                                  Lang   => Context.Lang,
                                  File   => Context.File,
                                  Expression => Null_Parsed_Expression);

      Result   : Completion_List;

   begin
      if Language.Libclang.Is_Module_Active then
         for Item of Manager.Ordered_Resolvers loop
            Get_Completion_Root
              (Resolver => Item,
               Offset   => Context.Offset,
               Context  => New_Context,
               Result   => Result);
         end loop;
      end if;

      return Result;
   end Get_Initial_Completion_List;

end Completion.C;
