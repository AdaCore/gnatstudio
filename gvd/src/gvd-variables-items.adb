------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GVD.Types;                   use GVD.Types;
with Language;                    use Language;
with GVD.Variables.Types.Simples; use GVD.Variables.Types.Simples;

package body GVD.Variables.Items is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.GVD_VARIABLES_ITEMS");

   ----------
   -- Name --
   ----------

   function Name (Self : Item_Info) return String is
   begin
      if Self.Varname /= "" then
         return To_String (Self.Varname);

      elsif Self.Cmd_Name /= ""
        and then Self.Cmd_Name /= "<>"
      then
         return To_String (Self.Cmd_Name);

      else
         return To_String (Self.Cmd);
      end if;
   end Name;

   ---------------------------
   -- Wrap_Debugger_Command --
   ---------------------------

   function Wrap_Debugger_Command
     (Cmd         : String;
      Split_Lines : Boolean := False) return Item_Info
   is
      Result : Item_Info;
   begin
      Result.Cmd_Name    := To_Unbounded_String ("<>");
      Result.Cmd         := To_Unbounded_String (Cmd);
      Result.Entity      := New_Debugger_Type (Cmd, Split_Lines);
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
      Result      : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Self.Varname /= ""
        and then Self.Entity = Empty_GVD_Type_Holder
      then
         Self.Entity := Process.Debugger.Parse_Type (To_String (Self.Varname));
      end if;

      if Self.Entity /= Empty_GVD_Type_Holder then
         --  Parse the value

         if Self.Entity.Get_Type.all in GVD_Debugger_Output_Type'Class then
            Process.Debugger.Filter_Output
              (Visible,
               Process.Debugger.Send_And_Get_Clean_Output
                 (GVD_Debugger_Output_Type_Access
                      (Self.Entity.Get_Type).Refresh_Command,
                  Mode => GVD.Types.Internal),
               Result);
            GVD_Debugger_Output_Type_Access (Self.Entity.Get_Type).Set_Value
              (Ada.Strings.Unbounded.To_String (Result));

         elsif Self.Varname /= "" then
            begin
               Process.Debugger.Parse_Value
                 (To_String (Self.Varname),
                  Self.Entity,
                  Format      => Self.Format,
                  Value_Found => Value_Found);
               Self.Entity.Get_Type.Set_Valid (Value_Found);
            exception
               when Language.Unexpected_Type | Constraint_Error =>
                  Self.Entity.Get_Type.Set_Valid (False);
            end;

         else
            Self.Entity.Get_Type.Set_Valid (True);
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
      if Self.Entity /= Empty_GVD_Type_Holder then
         Self.Entity.Get_Type.Reset_Recursive;
      end if;
   end Mark_As_Up_To_Date;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Item_Info) is
   begin
      if Self.Entity /= Empty_GVD_Type_Holder then
         Self.Entity := Empty_GVD_Type_Holder;
      end if;
   end Free;

   -------------
   -- Is_Same --
   -------------

   function Is_Same (Info1, Info2 : Item_Info) return Boolean is
   begin
      return Info1.Cmd = Info2.Cmd and then Info1.Varname = Info2.Varname;
   end Is_Same;

end GVD.Variables.Items;
