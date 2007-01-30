-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2007                         --
--                              AdaCore                              --
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

with Ada.Unchecked_Conversion;
with Glib.Values;                use Glib, Glib.Values;
with System;                     use System;

package body Entities.Values is

   Entity_Information_Type : Glib.GType := Glib.GType_None;

   function To_EI is new Ada.Unchecked_Conversion
     (System.Address, Entity_Information);
   function Entity_Information_Boxed_Copy
     (Boxed : System.Address) return System.Address;
   procedure Entity_Information_Boxed_Free (Boxed : System.Address);
   pragma Convention (C, Entity_Information_Boxed_Copy);
   pragma Convention (C, Entity_Information_Boxed_Free);
   --  Internal subprograms used in the conversion of Entity_Information to
   --  GValues.

   -----------------------------------
   -- Entity_Information_Boxed_Copy --
   -----------------------------------

   function Entity_Information_Boxed_Copy
     (Boxed : System.Address) return System.Address
   is
      Value : constant Entity_Information := To_EI (Boxed);
   begin
      if Value /= null then
         Ref (Value);
      end if;
      return Boxed;
   end Entity_Information_Boxed_Copy;

   -----------------------------------
   -- Entity_Information_Boxed_Free --
   -----------------------------------

   procedure Entity_Information_Boxed_Free (Boxed : System.Address) is
      Entity : Entity_Information := To_EI (Boxed);
   begin
      Unref (Entity);
   end Entity_Information_Boxed_Free;

   ---------------------------------
   -- Get_Entity_Information_Type --
   ---------------------------------

   function Get_Entity_Information_Type return Glib.GType is
   begin
      if Entity_Information_Type = GType_None then
         Entity_Information_Type := Boxed_Type_Register_Static
           ("Entity_Information", Entity_Information_Boxed_Copy'Access,
            Entity_Information_Boxed_Free'Access);
      end if;

      return Entity_Information_Type;
   end Get_Entity_Information_Type;

   ---------------
   -- To_GValue --
   ---------------

   function To_GValue
     (Entity : Entity_Information) return Glib.Values.GValue
   is
      Value : GValue;
   begin
      Init (Value, Get_Entity_Information_Type);
      if Entity = null then
         Set_Boxed (Value, System.Null_Address);
      else
         Ref (Entity);
         Set_Boxed (Value, Entity.all'Address);
      end if;
      return Value;
   end To_GValue;

   -----------------
   -- From_GValue --
   -----------------

   function From_GValue
     (Value : Glib.Values.GValue) return Entity_Information
   is
      Entity : Entity_Information;
   begin
      Entity := To_EI (Get_Boxed (Value));
      return Entity;
   end From_GValue;

end Entities.Values;
