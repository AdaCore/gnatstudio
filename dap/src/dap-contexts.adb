------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2026, AdaCore                  --
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

with Ada.Strings.Unbounded;

with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;

with VSS.Strings.Conversions;
with VSS.Transformers.Casing;    use VSS.Transformers.Casing;

with Xref;                       use Xref;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;

package body DAP.Contexts is

   type Context_Item_Info is new GPS.Kernel.Context_Item with record
      Holder : Item_Holder;
   end record;
   type Context_Item_Info_Access is access all Context_Item_Info'Class;

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
              (Lang, Ada.Strings.Unbounded.To_String
                 (Context_Item_Info_Access
                      (Debugging_Variable (Context)).Text));
         end if;

         return Ada.Strings.Unbounded.To_String
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
   -- Get_Variable --
   ------------------

   function Get_Variable
     (Context : GPS.Kernel.Selection_Context)
      return Item_Info'Class is
   begin
      if Has_Debugging_Variable (Context) then
         return Context_Item_Info_Access
           (Debugging_Variable (Context)).Holder.Info.all;
      else
         return DAP.Modules.Variables.Items.No_Item;
      end if;
   end Get_Variable;

   --------------------
   -- Store_Variable --
   --------------------

   procedure Store_Variable
     (Context   : in out GPS.Kernel.Selection_Context;
      Full_Name : VSS.Strings.Virtual_String;
      Info      : Item_Info'Class)
   is
      Holder : Item_Holder;
      Item   : Context_Item_Info_Access;
   begin
      Set (Holder, Info);
      Item := new Context_Item_Info'
        (VSS.Strings.Conversions.To_Unbounded_UTF_8_String
           (To_Lowercase.Transform (Full_Name)),
         Holder);
      Set_Debugging_Variable (Context, Context_Item_Access (Item));
   end Store_Variable;

end DAP.Contexts;
