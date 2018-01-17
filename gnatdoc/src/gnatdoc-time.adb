------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body GNATdoc.Time is

   ----------------
   -- Print_Time --
   ----------------

   procedure Print_Time
     (Context : access constant Docgen_Context)
   is
      Printout : aliased Unbounded_String;

      procedure Append_Line (Text : String);
      --  Append Text to Printout plus ASCII.LF

      function To_Percent (Partial_Time, Total_Time : Duration) return String;
      --  Compute Partial_Time/Total time and return such value as text
      --  ended with suffix '%'

      function To_String (D : Duration) return String;
      --  Return a text containing D ended with suffix 's'

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Text & ASCII.LF;
      end Append_Line;

      ----------------
      -- To_Percent --
      ----------------

      function To_Percent (Partial_Time, Total_Time : Duration) return String
      is
         Value : Duration;
      begin
         if Total_Time /= 0.0 then
            Value := Duration (Partial_Time / Total_Time);

            declare
               Point_Loc : constant Natural := 3;
               S : constant String := Value'Img;
            begin
               return S (Point_Loc + 1 .. Point_Loc + 2) & "%";
            end;
         else
            return "";
         end if;
      end To_Percent;

      ---------------
      -- To_String --
      ---------------

      function To_String (D : Duration) return String is
         S         : constant String := D'Img;
         Point_Loc : constant Natural := Index (S, ".");
      begin
         return S (S'First .. Point_Loc + 2) & "s";
      end To_String;

      --  Return a text containing D ended with suffix 's'

   begin
      Append_Line ("--- Frontend");
      Append_Line
        ("Frontend_Time .................. "
         & To_String (Frontend_Time));

      Append_Line
        (" - Build_Tree_Time ............. "
         & To_String (Build_Tree_Time)
         & " "
         & To_Percent (Build_Tree_Time, Frontend_Time));

      Append_Line
        (" - Get_Documentation_Time ...... "
         & To_String (GetDoc_Time)
         & " "
         & To_Percent (GetDoc_Time, Frontend_Time));

      Append_Line
        (" - Build_Comments_Time ......... "
         & To_String (Build_Comments_Time)
         & " "
         & To_Percent (Build_Comments_Time, Frontend_Time));

      Append_Line
        ("--- Backend");

      Append_Line
        ("Generate_Doc_Time ............ "
         & To_String (Generate_Doc_Time));

      Append_Line
        ("Generate_Global_Index_Time ... "
         & To_String (Generate_Global_Index_Time));

      Write_To_File
        (Context   => Context,
         Directory => Get_Doc_Directory (Context.Kernel),
         Filename  => +"time_summary.txt",
         Text      => Printout'Access);

   end Print_Time;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      --  Frontend time
      Build_Tree_Time            := 0.0;
      GetDoc_Time                := 0.0;
      Build_Comments_Time        := 0.0;
      Frontend_Time              := 0.0;

      --  Backend time
      Generate_Doc_Time          := 0.0;
      Generate_Global_Index_Time := 0.0;
   end Reset;

   -----------
   -- Start --
   -----------

   procedure Start (D : in out Delay_Time) is
   begin
      D.T1 := Ada.Calendar.Clock;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (D : in out Delay_Time; Accum : in out Duration) is
      use type Ada.Calendar.Time;
   begin
      D.T2  := Ada.Calendar.Clock;
      Accum := Accum + D.T2 - D.T1;
   end Stop;

end GNATdoc.Time;
