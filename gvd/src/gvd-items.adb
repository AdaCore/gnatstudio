------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with GNATCOLL.Traces;    use GNATCOLL.Traces;
with GVD.Types;          use GVD.Types;
with Language;           use Language;
with Items.Simples;      use Items.Simples;

package body GVD.Items is

   Me : constant Trace_Handle := Create ("ITEMS");

   ---------------------------
   -- Wrap_Debugger_Command --
   ---------------------------

   function Wrap_Debugger_Command
     (Cmd         : String;
      Split_Lines : Boolean := False) return Item_Info
   is
      Result : Item_Info;
   begin
      Result.Cmd := To_Unbounded_String (Cmd);
      Result.Entity := New_Debugger_Type (Cmd, Split_Lines => Split_Lines);
      Result.Split_Lines := Split_Lines;
      return Result;
   end Wrap_Debugger_Command;

   -------------------
   -- Wrap_Variable --
   -------------------

   function Wrap_Variable
     (Varname  : String;
      Format   : Debugger.Value_Format := Default_Format)
      return Item_Info
   is
      Result : Item_Info;
   begin
      Result.Varname := To_Unbounded_String (Varname);
      Result.Format  := Format;
      return Result;
   end Wrap_Variable;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self    : in out Item_Info;
      Process : not null access Visual_Debugger_Record'Class)
   is
      Value_Found : Boolean;
   begin
      if Self.Varname /= "" and then Self.Entity = null then
         Self.Entity := Process.Debugger.Parse_Type (To_String (Self.Varname));
      end if;

      if Self.Entity /= null then
         --  Parse the value

         if Self.Entity.all in Debugger_Output_Type'Class then
            Set_Value
              (Debugger_Output_Type (Self.Entity.all),
               Process.Debugger.Send_And_Get_Clean_Output
                 (Refresh_Command (Debugger_Output_Type (Self.Entity.all)),
                  Mode => GVD.Types.Internal));

         elsif Self.Varname /= "" then
            begin
               Process.Debugger.Parse_Value
                 (To_String (Self.Varname),
                  Self.Entity,
                  Format      => Self.Format,
                  Value_Found => Value_Found);
               Self.Entity.Set_Valid (Value_Found);
            exception
               when Language.Unexpected_Type | Constraint_Error =>
                  Self.Entity.Set_Valid (False);
            end;

         else
            Self.Entity.Set_Valid (True);
         end if;
      end if;

   exception
         --  Could be a parse_type error
      when E : others =>
         Trace (Me, E);
   end Update;

   ------------------------
   -- Mark_As_Up_To_Date --
   ------------------------

   procedure Mark_As_Up_To_Date (Self : in out Item_Info) is
   begin
      if Self.Entity /= null then
         Reset_Recursive (Self.Entity);
      end if;
   end Mark_As_Up_To_Date;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Item_Info) is
   begin
      if Self.Entity /= null then
         Free (Self.Entity);
      end if;
   end Free;

   -------------
   -- Is_Same --
   -------------

   function Is_Same (Info1, Info2 : Item_Info) return Boolean is
   begin
      return Info1.Cmd = Info2.Cmd and then Info1.Varname = Info2.Varname;
   end Is_Same;

end GVD.Items;
