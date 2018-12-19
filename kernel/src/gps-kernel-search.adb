------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with GPS.Search;           use GPS.Search;
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

   -------------------
   -- Path_And_Name --
   -------------------

   function Path_And_Name
     (Kernel       : Kernel_Handle;
      File         : Virtual_File;
      Project_View : Projects.Views.Project_View_Reference) return String
   is
      use GNATCOLL.Projects;

   begin
      if Project_View.Get_Project_Type /= No_Project
        and then Get_History
          (Get_History (Kernel).all, Key_Search_Displays_Relative_Paths)
      then
         return +(Relative_Path (File, Project_View.Get_Project_Path.Dir));
      else
         return +(File.Full_Name);
      end if;
   end Path_And_Name;

end GPS.Kernel.Search;
