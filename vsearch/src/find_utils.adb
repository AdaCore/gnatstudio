------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
with GPS.Kernel;                use GPS.Kernel;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with Gtk.Widget;                use Gtk.Widget;
with Vsearch;                   use Vsearch;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Intl;                  use GPS.Intl;
with UTF8_Utils;                use UTF8_Utils;

package body Find_Utils is
   use type Basic_Types.Visible_Column_Type;

   -----------
   -- Match --
   -----------

   function Match
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Positive := Positive'Last)
      return GPS.Search.Search_Context
   is
   begin
      return Context.Pattern.Start
        (Buffer      => Buffer,
         Start_Index => Start_Index,
         End_Index   => End_Index);
   end Match;

   ---------------------------
   -- Matched_Subexpression --
   ---------------------------

   procedure Matched_Subexpression
     (Result      : GPS.Search.Search_Context;
      Index       : Natural;
      First       : out Natural;
      Last        : out Natural) is
   begin
      if Index in Result.Groups'Range then
         First := Result.Groups (Index).First;
         Last := Result.Groups (Index).Last;
      else
         First := 1;
         Last := 0;
      end if;
   end Matched_Subexpression;

   ---------------------------
   -- Get_Terminate_Message --
   ---------------------------

   function Get_Terminate_Message
     (Context : access Root_Search_Context;
      Kind    : Operation_Kind) return String
   is
      pragma Unreferenced (Context, Kind);
   begin
      return "";
   end Get_Terminate_Message;

   --------------------------
   -- Scan_Buffer_No_Scope --
   --------------------------

   procedure Scan_Buffer_No_Scope
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Natural;
      End_Index   : Natural;
      Callback    : Scan_Callback;
      Ref_Index   : in out Integer;
      Ref_Line    : in out Natural;
      Ref_Column  : in out Character_Offset_Type;
      Ref_Visible_Column : in out Visible_Column_Type;
      Was_Partial : out Boolean)
   is
      Result : GPS.Search.Search_Context;
   begin
      Was_Partial := False;

      --  Special case here: If we have an empty section, do nothing. In
      --  fact, End_Index might be 0 in the following case: we search in
      --  one of the GPS source files for "all but comments". The first
      --  section is empty, and End_Index is 0. However, it is
      --  legitimate, if inefficient, to have an empty section

      if End_Index = 0 then
         return;
      end if;

      Result := Context.Pattern.Start
        (Buffer      => Buffer,
         Start_Index => Start_Index,
         End_Index   => End_Index,
         Ref_Index   => Ref_Index,
         Ref_Line    => Ref_Line,
         Ref_Column  => Ref_Column,
         Ref_Visible_Column => Ref_Visible_Column);

      while Result /= GPS.Search.No_Match loop
         if not Callback
           (Result,
            Context.Pattern.Highlight_Match
              (Buffer
                 (Line_Start (Buffer, Result.Start)
                  .. Line_End (Buffer, Result.Finish)),
               Result))
         then
            Was_Partial := True;
            exit;
         end if;

         Context.Pattern.Next (Buffer, Result);
      end loop;

      Ref_Index  := Result.Ref_Index;
      Ref_Line   := Result.Ref_Line;
      Ref_Column := Result.Ref_Column;
      Ref_Visible_Column := Result.Ref_Visible_Column;

   exception
      when Invalid_Context =>
         null;
   end Scan_Buffer_No_Scope;

   --------------------
   -- To_Line_Column --
   --------------------

   procedure To_Line_Column
     (Buffer         : Glib.UTF8_String;
      Pos            : Natural;
      Line           : in out Natural;
      Column         : in out Character_Offset_Type;
      Visible_Column : in out Visible_Column_Type;
      Line_Start     : in out Natural)
   is
      J          : Natural := Buffer'First;
      Tab_Width  : constant Visible_Column_Type :=
        Visible_Column_Type (Get_Tab_Width);

      function At_Line_End return Boolean;
      pragma Inline (At_Line_End);
      --  Return True if J points to an end-of-line character

      function At_Line_End return Boolean is
      begin
         return Buffer (J) = ASCII.LF
           or else (Buffer (J) = ASCII.CR
                    and then Buffer (J + 1) /= ASCII.LF);
      end At_Line_End;

   begin
      loop
         exit when J > Pos - 1;

         if At_Line_End then
            Line           := Line + 1;
            Column         := 1;
            Visible_Column := 1;
            Line_Start := J + 1;
         else
            Column := Column + 1;

            if Buffer (J) = ASCII.HT then
               Visible_Column := Visible_Column
                 + Tab_Width - (Visible_Column mod Tab_Width) + 1;
            else
               Visible_Column := Visible_Column + 1;
            end if;
         end if;

         J := UTF8_Utils.UTF8_Next_Char (Buffer, J);
      end loop;
   end To_Line_Column;

   ----------------------
   -- Context_Look_For --
   ----------------------

   function Context_Look_For
     (Context : access Root_Search_Context) return String is
   begin
      return Context.Pattern.Get_Text;
   end Context_Look_For;

   ------------------------
   -- Set_End_Notif_Done --
   ------------------------

   procedure Set_End_Notif_Done
     (Context : in out Root_Search_Context; Value : Boolean) is
   begin
      Context.End_Notif_Done := Value;
   end Set_End_Notif_Done;

   ------------------------
   -- Get_End_Notif_Done --
   ------------------------

   function Get_End_Notif_Done
     (Context : Root_Search_Context) return Boolean is
   begin
      return Context.End_Notif_Done;
   end Get_End_Notif_Done;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context  : access Root_Search_Context'Class;
      Look_For : String;
      Options  : Search_Options) is
   begin
      Free (Root_Search_Context (Context.all));

      Context.Pattern := Build
        (Pattern        => Look_For,
         Case_Sensitive => Options.Case_Sensitive,
         Whole_Word     => Options.Whole_Word,
         Negate         => False,
         Kind           =>
           (if Options.Regexp then GPS.Search.Regexp
            else GPS.Search.Full_Text),
         Allow_Highlight => True);
   end Set_Context;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Root_Search_Context) is
   begin
      Free (Context.Pattern);
   end Free;

   procedure Free (Context : in out Root_Search_Context_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Root_Search_Context'Class, Root_Search_Context_Access);
   begin
      if Context /= null then
         Free (Context.all);
         Unchecked_Free (Context);
      end if;
   end Free;

   -------------
   -- Replace --
   -------------

   function Replace
     (Context         : access Root_Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean
   is
      pragma Unreferenced
        (Context, Kernel, Replace_String, Search_Backward, Give_Focus,
         Case_Preserving);
   begin
      return False;
   end Replace;

   -----------------
   -- Get_Options --
   -----------------

   function Get_Options
     (Context : access Root_Search_Context) return Search_Options is
   begin
      return (Regexp         => Context.Pattern.Get_Kind = GPS.Search.Regexp,
              Case_Sensitive => Context.Pattern.Get_Case_Sensitive,
              Whole_Word     => Context.Pattern.Get_Whole_Word);
   end Get_Options;

   ------------------------
   -- Find_Closest_Match --
   ------------------------

   procedure Find_Closest_Match
     (Buffer         : String;
      Line           : in out Natural;
      Column         : in out Character_Offset_Type;
      Found          : out Boolean;
      Str            : String;
      Case_Sensitive : Boolean)
   is
      Best_Line   : Integer := 0;
      Best_Column : Character_Offset_Type := 0;
      Pattern     : Search_Pattern_Access;
      Index       : Integer;
      Result      : GPS.Search.Search_Context;
      Line_Diff, Col_Diff : Integer;

   begin
      Index := Buffer'First;
      Pattern := Build
        (Pattern       => Str,
         Case_Sensitive => Case_Sensitive,
         Whole_Word     => True,
         Kind           => GPS.Search.Full_Text);

      Result := Pattern.Start
        (Buffer      => Buffer,
         Start_Index => Buffer'First,
         End_Index   => Buffer'Last,
         Ref_Index   => Index,
         Ref_Line    => 1,
         Ref_Column  => 1,
         Ref_Visible_Column => 1);
      while Result /= GPS.Search.No_Match loop

         Line_Diff := Integer
           (abs (Result.Line_Start - Line) - abs (Best_Line - Line));
         Col_Diff := Integer
           (abs (Result.Col_Start - Column) - abs (Best_Column - Column));

         if Line_Diff < 0
           or else (Line_Diff = 0 and then Col_Diff < 0)
         then
            Best_Line := Result.Line_Start;
            Best_Column := Result.Col_Start;
         end if;

         Pattern.Next (Buffer, Result);
      end loop;

      Free (Pattern);

      Line   := Best_Line;
      Column := Best_Column;
      Found  := Best_Line /= 0;
   end Find_Closest_Match;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Context : access Root_Search_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (-"Search for: " & Context.Context_Look_For,
         (Editor_Side => True, Locations => True));
   end Reset;

   ---------------------
   -- Context_Look_In --
   ---------------------

   function Context_Look_In
     (Self : Root_Search_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      --  Only used in Find_Closest_Match
      return -"file";
   end Context_Look_In;

end Find_Utils;
