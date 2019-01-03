------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

with GNATCOLL.Traces;  use GNATCOLL.Traces;
with GNAThub.Messages; use GNAThub.Messages;

package body GNAThub.Filters is

   Me : constant Trace_Handle := Create ("GNATHUB.FILTERS");

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Self    : in out Message_Filter;
      Message : GPS.Kernel.Messages.Abstract_Message'Class)
      return GPS.Kernel.Messages.Filter_Result
   is
      use all type GPS.Kernel.Messages.Message_Visibility_Kind;

   begin
      if Message not in GNAThub_Message'Class then
         return (Non_Applicable => True);
      end if;

      declare
         M : GNAThub_Message'Class
           renames GNAThub_Message'Class (Message);

      begin
         if Self.Tools.Contains (M.Get_Tool)
           and then Self.Severities.Contains (M.Get_Severity)
           and then Self.Rules.Contains (M.Get_Rule)
         then
            return
              (Non_Applicable => False,
               Flags          =>
                 (Locations   => True,
                  Editor_Line => False,
                  Editor_Side => True));

         else
            return (Non_Applicable => False,
                    Flags          => GPS.Kernel.Messages.Empty_Message_Flags);
         end if;
      end;
   end Apply;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Message_Filter) is
   begin
      Self.Tools.Clear;
      Self.Severities.Clear;
      Self.Rules.Clear;
   end Clear;

   --------------
   -- Add_Tool --
   --------------

   procedure Add_Tool
     (Self : in out Message_Filter;
      Tool : Tool_Access) is
   begin
      Self.Tools.Include (Tool);
   end Add_Tool;

   ------------------
   -- Add_Severity --
   ------------------

   procedure Add_Severity
     (Self     : in out Message_Filter;
      Severity : Severity_Access) is
   begin
      Self.Severities.Include (Severity);
   end Add_Severity;

   --------------
   -- Add_Rule --
   --------------

   procedure Add_Rule
     (Self : in out Message_Filter;
      Rule : Rule_Access) is
   begin
      Self.Rules.Include (Rule);
   end Add_Rule;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Self       : in out Message_Filter;
      Tools      : Tools_Ordered_Sets.Set;
      Severities : Severities_Ordered_Sets.Set;
      Rules      : Rule_Sets.Set) is
   begin
      Self.Tools      := Tools;
      Self.Severities := Severities;
      Self.Rules      := Rules;

      --  Trace the filters being applied

      Trace (Me, "Applying filters...");

      Increase_Indent (Me, "Selected tools:");
      for Tool of Tools loop
         Trace (Me, To_String (Tool.Name));
      end loop;
      Decrease_Indent (Me);

      Increase_Indent (Me, "Selected severities:");
      for Sev of Severities loop
         Trace (Me, Message_Importance_Type'Image (Sev.Ranking));
      end loop;
      Decrease_Indent (Me);

      Increase_Indent (Me, "Selected rules:");
      for Rule of Rules loop
         Trace (Me, To_String (Rule.Name));
      end loop;
      Decrease_Indent (Me);

      --  Emit the 'criteria-changed' signal to react to the filter's changes

      Self.Criteria_Changed;
   end Fill;

end GNAThub.Filters;
