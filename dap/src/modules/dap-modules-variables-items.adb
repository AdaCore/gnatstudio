------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with Ada.Unchecked_Deallocation;

with VSS.Strings.Conversions;

with DAP.Modules.Variables.Items.Variables;
with DAP.Modules.Variables.Items.Commands;
with DAP.Modules.Variables.Items.Arguments;
with DAP.Modules.Variables.Items.Locals;

package body DAP.Modules.Variables.Items is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Object : in out Item_Holder) is
      I : constant Item_Info_Access := new Item_Info'Class'(Object.Info.all);
   begin
      Object.Info := I;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Item_Info'Class, Item_Info_Access);

   overriding procedure Finalize (Object : in out Item_Holder) is
   begin
      Free (Object.Info);
   end Finalize;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Item_Holder; Item : Item_Info'Class) is
   begin
      Finalize (Self);
      Self.Info := new Item_Info'Class'(Item);
   end Set;

   --------------
   --  Convert --
   --------------

   function Convert (Format : DAP.Tools.ValueFormat) return Value_Format is
   begin
      if Format = Default_Format then
         return Default;
      else
         return Hexadecimal;
      end if;
   end Convert;

   --------------
   --  Convert --
   --------------

   function Convert (Format : Value_Format) return DAP.Tools.ValueFormat is
   begin
      if Format = Default then
         return Default_Format;
      else
         return DAP.Tools.ValueFormat'(hex => True);
      end if;
   end Convert;

   ------------
   -- Create --
   ------------

   function Create
     (Variable    : VSS.Strings.Virtual_String := "";
      Command     : VSS.Strings.Virtual_String := "";
      Split_Lines : Boolean := False;
      Arguments   : Boolean := False;
      Locals      : Boolean := False;
      Format      : DAP.Tools.ValueFormat := Default_Format)
      return Item_Info'Class is
   begin
      if not Variable.Is_Empty then
         return DAP.Modules.Variables.Items.Variables.Create
           (Variable, Format);

      elsif not Command.Is_Empty then
         return DAP.Modules.Variables.Items.Commands.Create
           (Command, Split_Lines, Format);

      elsif Arguments then
         return DAP.Modules.Variables.Items.Arguments.Create (Format);

      elsif Locals then
         return DAP.Modules.Variables.Items.Locals.Create (Format);

      else
         return No_Item;
      end if;
   end Create;

   ----------------------
   -- Get_Special_Kind --
   ----------------------

   function Get_Special_Kind (Info : Item_Info) return Variable_Kind is
   begin
      return Non_Specified;
   end Get_Special_Kind;

   ----------------
   -- Is_Command --
   ----------------

   function Is_Command (Info : Item_Info) return Boolean is
   begin
      return False;
   end Is_Command;

   ----------------
   -- Is_No_Item --
   ----------------

   function Is_No_Item (Info : Item_Info) return Boolean is
   begin
      return False;
   end Is_No_Item;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Item_Info'Class) return String is
   begin
      return VSS.Strings.Conversions.To_UTF_8_String (Get_Name (Self));
   end Get_Name;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format (Self : Item_Info) return String is
   begin
      return Image (Self.Format);
   end Get_Format;

   ------------------
   -- Is_Same_Name --
   ------------------

   function Is_Same_Name
     (Info : Item_Info'Class; Name : Virtual_String) return Boolean is
   begin
      return Info.Get_Full_Name = Name;
   end Is_Same_Name;

   -----------
   -- Image --
   -----------

   function Image (Format : DAP.Tools.ValueFormat) return String is
   begin
      if Format = Default_Format then
         return "";
      else
         return " (Hexadecimal)";
      end if;
   end Image;

   -------------------
   -- Find_DAP_Item --
   -------------------

   overriding procedure Find_DAP_Item
     (Info  : No_Item_Info;
      C     : in out DAP.Types.Variables_References_Trees.Cursor;
      Found : out Boolean) is
   begin
      C     := DAP.Types.Variables_References_Trees.No_Element;
      Found := False;
   end Find_DAP_Item;

   ----------------
   -- Get_Format --
   ----------------

   overriding function Get_Format (Self : No_Item_Info) return String is
   begin
      return "";
   end Get_Format;

   ----------------
   -- Is_No_Item --
   ----------------

   overriding function Is_No_Item (Info : No_Item_Info) return Boolean is
   begin
      return True;
   end Is_No_Item;

   -------------
   -- Restore --
   -------------

   function Restore
     (Value : GNATCOLL.JSON.JSON_Value) return Item_Info'Class is
   begin
      if String'(Value.Get ("tag")) = "cmd" then
         return DAP.Modules.Variables.Items.Commands.Load (Value);

      elsif String'(Value.Get ("tag")) = "variable" then
         return DAP.Modules.Variables.Items.Variables.Load (Value);

      elsif String'(Value.Get ("tag")) = "arguments" then
         return DAP.Modules.Variables.Items.Arguments.Load (Value);

      else
         return No_Item;
      end if;
   end Restore;

end DAP.Modules.Variables.Items;
