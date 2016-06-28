------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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
with GNAThub.Messages;

package body GNAThub.Filters is

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
      if Message not in GNAThub.Messages.Message'Class then
         return (Non_Applicable => True);
      end if;

      declare
         M : GNAThub.Messages.Message'Class
           renames GNAThub.Messages.Message'Class (Message);

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
                  Editor_Side => M.Get_Severity.On_Sidebar));

         else
            return (Non_Applicable => False,
                    Flags          => GPS.Kernel.Messages.Empty_Message_Flags);
         end if;
      end;
   end Apply;

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
   end Fill;

end GNAThub.Filters;
