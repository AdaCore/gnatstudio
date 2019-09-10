------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
with Glib.Convert;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Intl;                  use GPS.Intl;

package body Find_Utils is
   use type Basic_Types.Visible_Column_Type;

   -----------
   -- Match --
   -----------

   function Match
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1)
      return GPS.Search.Search_Context
   is
   begin
      return Context.Pattern.Start
        (Buffer      => Buffer,
         Start_Index => Start_Index,
         End_Index   => End_Index);
   end Match;

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
     (Context              : access Root_Search_Context;
      Buffer               : String;
      Start_Index          : Natural;
      End_Index            : Natural;
      Callback             : Scan_Callback;
      Ref                  : in out Buffer_Position;
      Was_Partial          : out Boolean;
      Display_Matched_Only : Boolean := False)
   is
      Result : GPS.Search.Search_Context;
      BOL, EOL : Integer;
      After : Positive;
      Matched_Text : Unbounded_String;
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
         Ref         => Ref);

      while Result /= GPS.Search.No_Match loop
         Ref  := Result.Ref;

         After := Index_After_Match (Result);
         BOL := Line_Start (Buffer, Result.Start.Index);
         EOL := Line_End (Buffer, After);

         --  Don't use GPS.Search.Highlight_Match, since that would only show
         --  the part of the buffer that was tested, whereas we want the full
         --  line (including comments if we were only searching in code for
         --  instance)
         Matched_Text :=
           To_Unbounded_String ("<b>"
                                & Glib.Convert.Escape_Text
                                  (Buffer (Result.Start.Index .. After - 1))
                                & "</b>");
         if not Display_Matched_Only then
            Matched_Text :=
              Glib.Convert.Escape_Text (Buffer (BOL .. Result.Start.Index - 1))
              & Matched_Text
              & Glib.Convert.Escape_Text (Buffer (After .. EOL));
         end if;
         if not Callback (Result, To_String (Matched_Text)) then
            Was_Partial := True;
            exit;
         end if;

         Context.Pattern.Next (Buffer, Result);
      end loop;

   exception
      when Invalid_Context =>
         null;
   end Scan_Buffer_No_Scope;

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
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
     (Context        : access Root_Search_Context'Class;
      Pattern        : String;
      Whole_Word     : Boolean;
      Case_Sensitive : Boolean;
      Kind           : GPS.Search.Search_Kind)
   is
   begin
      Free (Context.Pattern);
      Context.Pattern := Build
        (Pattern,
         Whole_Word      => Whole_Word,
         Case_Sensitive  => Case_Sensitive,
         Kind            => Kind,
         Allow_Highlight => True);
   end Set_Pattern;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Occurrence : not null access Search_Occurrence_Record'Class;
      Pattern    : String) is
   begin
      Occurrence.Pattern := To_Unbounded_String (Pattern);
   end Initialize;

   -----------------
   -- Get_Pattern --
   -----------------

   function Get_Pattern
     (Occurrence : not null access Search_Occurrence_Record'Class)
     return String
   is
      (To_String (Occurrence.Pattern));

   ----------
   -- Free --
   ----------

   procedure Free (Occurrence : in out Search_Occurrence) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Search_Occurrence_Record'Class, Search_Occurrence);
   begin
      if Occurrence /= null then
         Unchecked_Free (Occurrence);
      end if;
   end Free;

   ------------
   -- Search --
   ------------

   procedure Search
     (Context              : access Root_Search_Context'Class;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean;
      Display_Matched_Only : Boolean := False)
   is
      Occurrence : Search_Occurrence;
   begin
      Occurrence := Context.Search
        (Kernel               => Kernel,
         Search_Backward      => Search_Backward,
         From_Selection_Start => From_Selection_Start,
         Give_Focus           => Give_Focus,
         Found                => Found,
         Continue             => Continue,
         Display_Matched_Only => Display_Matched_Only);
      Free (Occurrence);
   end Search;

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

   ---------------
   -- Is_Regexp --
   ---------------

   function Is_Regexp (Context : access Root_Search_Context) return Boolean is
   begin
      return Context.Pattern /= null
        and then Context.Pattern.Get_Kind = GPS.Search.Regexp;
   end Is_Regexp;

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
      Result      : GPS.Search.Search_Context;
      Line_Diff, Col_Diff : Integer;
      Ref         : constant Buffer_Position := (Buffer'First, 1, 1, 1);

   begin
      Pattern := Build
        (Pattern       => Str,
         Case_Sensitive => Case_Sensitive,
         Whole_Word     => True,
         Kind           => GPS.Search.Full_Text);
      Result := Pattern.Start
        (Buffer      => Buffer,
         Start_Index => Buffer'First,
         End_Index   => Buffer'Last,
         Ref         => Ref);

      while Result /= GPS.Search.No_Match loop

         Line_Diff := Integer
           (abs (Result.Start.Line - Line) - abs (Best_Line - Line));
         Col_Diff := Integer
           (abs (Result.Start.Column - Column) - abs (Best_Column - Column));

         if Line_Diff < 0
           or else (Line_Diff = 0 and then Col_Diff < 0)
         then
            Best_Line := Result.Start.Line;
            Best_Column := Result.Start.Column;
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
        (Get_Search_Category_Name
           (Look_For    => Context.Context_Look_For,
            Interactive => not Context.All_Occurrences),
         Side_And_Locations);
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Module       : not null access Search_Module_Type;
      Label        : String;
      Selector     : access Scope_Selector_Interface'Class := null;
      Id           : access GPS.Kernel.Abstract_Module_ID_Record'Class := null;
      Mask         : Search_Options_Mask := All_Options;
      In_Selection : Boolean := False) is
   begin
      Module.Label := To_Unbounded_String (Label);
      Module.Selector := Selector;
      Module.Id := Module_ID (Id);
      Module.Mask := Mask;
      Module.In_Selection := In_Selection;
   end Initialize;

   ---------------------
   -- Push_Occurrence --
   ---------------------

   procedure Push_Occurrence
     (Module     : not null access Search_Module_Type;
      Occurrence : not null access Search_Occurrence_Record'Class) is
   begin
      Module.Search_Occurrences_Stack.Prepend (Occurrence);
   end Push_Occurrence;

   --------------------
   -- Pop_Occurrence --
   --------------------

   function Pop_Occurrence
     (Module : not null access Search_Module_Type)
      return Search_Occurrence
   is
      Occurrence : Search_Occurrence;
   begin
      if not Module.Search_Occurrences_Stack.Is_Empty then
         Occurrence := Module.Search_Occurrences_Stack.First_Element;
         Module.Search_Occurrences_Stack.Delete_First;
      end if;

      return Occurrence;
   end Pop_Occurrence;

   -------------------------
   -- Get_Last_Occurrence --
   -------------------------

   function Get_Last_Occurrence
     (Module : not null access Search_Module_Type)
      return Search_Occurrence
   is
     (if Module.Search_Occurrences_Stack.Is_Empty then
         null
      else
         Module.Search_Occurrences_Stack.First_Element);

   -----------------------
   -- Clear_Occurrences --
   -----------------------

   procedure Clear_Occurrences (Module : not null access Search_Module_Type) is
   begin
      for Occurrence of Module.Search_Occurrences_Stack loop
         Free (Occurrence);
      end loop;

      Module.Search_Occurrences_Stack.Clear;
   end Clear_Occurrences;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Module : not null access Search_Module_Type) return String
   is
      (To_String (Module.Label));

   ------------------------
   -- Get_Scope_Selector --
   ------------------------

   function Get_Scope_Selector
     (Module : not null access Search_Module_Type) return Scope_Selector
   is
     (Module.Selector);

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Module : not null access Search_Module_Type) return Module_ID
   is
      (Module.Id);

   ----------------------
   -- Get_In_Selection --
   ----------------------

   function Get_In_Selection
     (Module : not null access Search_Module_Type) return Boolean
   is
     (Module.In_Selection);

   -------------------------
   -- Is_Option_Supported --
   -------------------------

   function Is_Option_Supported
     (Module : not null access Search_Module_Type;
      Option : Search_Options_Mask) return Boolean
   is
     ((Module.Mask and Option) /= 0);

   ------------------------------
   -- Get_Search_Category_Name --
   ------------------------------

   function Get_Search_Category_Name
     (Look_For    : String;
      Interactive : Boolean) return String is
   begin
      return
        (if Interactive then
            -"interactive search"
         else -"Search for: ")
        & Glib.Convert.Escape_Text (Look_For);
   end Get_Search_Category_Name;

end Find_Utils;
