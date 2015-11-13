------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2015, AdaCore                     --
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

with GNAT.Strings;         use GNAT.Strings;

with GPS.Kernel.Actions;   use GPS.Kernel.Actions;
with GPS.Search.GUI;       use GPS.Search, GPS.Search.GUI;
with Histories;            use Histories;

package body GPS.Kernel.Search is

   Max_History_Items : constant := 5;
   --  Maximum number of items stored in the history

   ------------------
   -- Adjust_Score --
   ------------------

   procedure Adjust_Score
      (Self   : not null access Kernel_Search_Provider;
       Result : not null access Search_Result'Class)
   is
      Key : constant History_Key :=
         "search-recent-"
         & History_Key (Search_Provider'Class (Self.all).Display_Name);
      Hist : constant GNAT.Strings.String_List_Access := Get_History
         (Self.Kernel.Get_History.all, Key);
      M : Integer;
   begin
      if Hist /= null then
         --  in case too many items are stored in the history
         M := Integer'Min (Hist'First + Max_History_Items, Hist'Last);
         for H in Hist'First .. M loop
            if Hist (H) /= null
               and then Result.Id.all = Hist (H).all
            then
               Result.Score := Result.Score + (M + 1 - H) * 20;
               exit;
            end if;
         end loop;
      end if;
   end Adjust_Score;

   ------------------------
   -- On_Result_Executed --
   ------------------------

   overriding procedure On_Result_Executed
      (Self   : not null access Kernel_Search_Provider;
       Result : not null access Search_Result'Class)
   is
      Key : constant History_Key :=
         "search-recent-"
         & History_Key (Search_Provider'Class (Self.all).Display_Name);
   begin
      Set_Max_Length (Self.Kernel.Get_History.all, Max_History_Items, Key);
      Self.Kernel.Add_To_History (Key, Result.Id.all);
   end On_Result_Executed;

   ----------------------------------
   -- Register_Provider_And_Action --
   ----------------------------------

   procedure Register_Provider_And_Action
      (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Provider   : not null access Kernel_Search_Provider'Class;
       Icon_Name  : String := "")
   is
      Command : Global_Search_Command_Access;
   begin
      Provider.Kernel := Kernel_Handle (Kernel);

      GPS.Kernel.Search.Registry.Register (Provider);

      Command := new Global_Search_Command;
      Command.Provider := Search_Provider_Access (Provider);
      Command.History := new History_Key'
         ("global-search-entry-" & History_Key (Provider.Display_Name));
      Register_Action
         (Kernel, Action_Name_Prefix & Provider.Display_Name, Command,
          Description => Command.Provider.Documentation,
          Category    => "Search",
          Icon_Name   => Icon_Name);
   end Register_Provider_And_Action;

end GPS.Kernel.Search;
