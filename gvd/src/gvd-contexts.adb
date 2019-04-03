------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Xref;                      use Xref;

package body GVD.Contexts is

   ------------------
   -- Get_Variable --
   ------------------

   function Get_Variable
     (Context : GPS.Kernel.Selection_Context)
      return Item_Info is
   begin
      if Has_Debugging_Variable (Context) then
         return Context_Item_Info_Access (Debugging_Variable (Context)).Info;
      else
         return No_Item_Info;
      end if;
   end Get_Variable;

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

      if Has_Debugging_Variable (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name
              (Lang, To_String
                 (Context_Item_Info_Access
                      (Debugging_Variable (Context)).Text));
         end if;

         return To_String
           (Context_Item_Info_Access (Debugging_Variable (Context)).Text);
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
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            if Dereference
              and then Lang /= null
              and then (Is_Fuzzy (Entity)
                        or else (not Is_Type (Entity)
                                 and then Is_Access (Entity)))
            then
               return Dereference_Name
                 (Lang, Entity_Name_Information (Context));

            elsif Is_Fuzzy (Entity)
              or else Is_Printable_In_Debugger (Entity)
            then
               return Entity_Name_Information (Context);
            end if;
         end;
      end if;

      return "";
   end Get_Variable_Name;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Context   : in out GPS.Kernel.Selection_Context;
      Full_Name : String;
      Info      : Item_Info)
   is
      Item : constant Context_Item_Info_Access := new Context_Item_Info'
        (To_Unbounded_String (Full_Name), Info);
   begin
      Set_Debugging_Variable (Context, Context_Item_Access (Item));
   end Set_Variable;

end GVD.Contexts;
