------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with Language;            use Language;
with Language_Handlers;   use Language_Handlers;

package body GVD.Contexts is

   -----------------------
   -- Get_Variable_Name --
   -----------------------

   function Get_Variable_Name
     (Context     : Selection_Context;
      Dereference : Boolean) return String
   is
      Lang  : Language_Access;
   begin
      if Context = No_Context then
         return "";
      end if;

      if Has_File_Information (Context) then
         Lang := Get_Language_From_File
           (Get_Language_Handler (Get_Kernel (Context)),
            File_Information (Context));
      end if;

      if Has_Area_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name (Lang, Text_Information (Context));
         end if;

         return Text_Information (Context);
      end if;

      if Has_Expression_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name (Lang, Expression_Information (Context));
         end if;

         return Expression_Information (Context);
      end if;

      if Has_Entity_Name_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name
              (Lang, Entity_Name_Information (Context));
         end if;

         return Entity_Name_Information (Context);
      end if;

      return "";
   end Get_Variable_Name;

end GVD.Contexts;
