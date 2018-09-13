------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with GNAT.Regpat;
with GNAT.Strings;                    use GNAT.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Glib.Convert;
with GPS.Default_Styles;              use GPS.Default_Styles;
with GPS.Kernel.Messages.Hyperlink;   use GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.Legacy;
with GPS.Kernel.Messages.Simple;      use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Preferences;          use GPS.Kernel.Preferences;
with String_Utils;                    use String_Utils;
with GPS.Editors;                     use GPS.Editors;
with GPS.Editors.Line_Information;    use GPS.Editors.Line_Information;
with GPS.Intl;                        use GPS.Intl;
with UTF8_Utils;                      use UTF8_Utils;

package body GPS.Kernel.Messages.Tools_Output is

   use Basic_Types;
   use Category_Maps;
   use File_Maps;
   use GNAT.Regpat;
   use Node_Vectors;
   use GPS.Kernel.Style_Manager;

   type Location is record
      File   : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line   : Positive := 1;
      Column : Visible_Column_Type := 1;
      First  : Positive := 1;
      Last   : Natural := 0;
   end record;

   package Locations_List is new Ada.Containers.Vectors (Positive, Location);
   use Locations_List;

   function Extract_Locations
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Message : String) return Locations_List.Vector;

   ----------------------
   -- Add_Tool_Message --
   ----------------------

   function Add_Tool_Message
     (Container          : not null access Messages_Container'Class;
      Category           : String;
      File               : GNATCOLL.VFS.Virtual_File;
      Line               : Positive;
      Column             : Basic_Types.Visible_Column_Type;
      Text               : String;
      Importance         : Message_Importance_Type;
      Highlight_Category : GPS.Kernel.Style_Manager.Style_Access;
      Length             : Highlight_Length;
      Look_For_Secondary : Boolean;
      Show_In_Locations  : Boolean;
      Allow_Auto_Jump_To_First : Boolean := True) return Message_Access
   is
      Locs                   : Locations_List.Vector;
      Primary_Locs           : Locations_List.Vector;
      Has_Secondary_Location : Boolean := False;
      Returned               : Message_Access;
   begin
      --  Looking for existent message

      declare
         Category_Position : constant Category_Maps.Cursor :=
                               Container.Category_Map.Find
                                 (To_Unbounded_String (Category));
         Category_Node     : Node_Access;
         File_Position     : File_Maps.Cursor;
         File_Node         : Node_Access;
         Message_Position  : Node_Vectors.Cursor;
         Message           : Message_Access;

      begin
         if Has_Element (Category_Position) then
            Category_Node := Element (Category_Position);

            File_Position := Category_Node.File_Map.Find (File);

            if Has_Element (File_Position) then
               File_Node := Element (File_Position);

               Message_Position := File_Node.Children.First;

               while Has_Element (Message_Position) loop
                  Message := Message_Access (Element (Message_Position));

                  if Message.Line = Line
                    and then Message.Column = Column
                    and then Message.Get_Text = Text
                  then
                     return null;
                  end if;

                  Next (Message_Position);
               end loop;
            end if;
         end if;
      end;

      --  Look for secondary file information and loop on information found

      if Look_For_Secondary then
         Locs := Extract_Locations (Container.Kernel, Text);
         Has_Secondary_Location := not Is_Empty (Locs);
      else
         Has_Secondary_Location := False;
      end if;

      --  Create messages

      declare
         Primary : Message_Access;
      begin
         if Has_Secondary_Location then
            Primary :=
              GPS.Kernel.Messages.Legacy.Get_Message_At
                (Container, Category, File, Line, Column);

            --  Check if the primary message's text contains locations
            --  information.
            --  If it's the case, don't consider it as a primary message. This
            --  is needed in order to avoid creating secondary messages
            --  (i.e: children of primary messages in the Locations tree view)
            --  for messages that point to the same line in a generic unit,
            --  since these messages can have different weights.
            --
            --  e.g : avoid having a SPARK 2014 "check proved" message on a
            --  generic unit line as a parent and having a "check fail" message
            --  on the same line as a secondary message.

            if Primary /= null then
               Primary_Locs := Extract_Locations
                 (Container.Kernel, To_String (Primary.Get_Text));

               if not Primary_Locs.Is_Empty then
                  Primary := null;
               end if;
            end if;
         end if;

         if Primary = null then
            Primary :=
              Message_Access
                (Create_Simple_Message
                     (Messages_Container_Access (Container),
                      Category,
                      File,
                      Line,
                      Column,
                      Text,
                      Importance,
                      (Editor_Side => True,
                       Editor_Line => False,
                       Locations   => Show_In_Locations),
                      Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First));
            Returned := Primary;

            if Highlight_Category /= null then
               Primary.Set_Highlighting (Highlight_Category, Length);
            end if;
         end if;

         for Loc of Locs loop
            if Loc.File = No_File then
               --  Secondary locations extraction subprogram can set File to
               --  No_File when reference to the same file as primary message
               --  was found.

               Loc.File := Primary.Get_File;
            end if;

            Create_Hyperlink_Message
              (Primary,
               Loc.File,
               Loc.Line,
               Loc.Column,
               Text,
               Loc.First,
               Loc.Last,
               (Editor_Side => True, Locations => Show_In_Locations,
                Editor_Line => False));
         end loop;

         Locs.Clear;
      end;

      return Returned;
   end Add_Tool_Message;

   -----------------------
   -- Extract_Locations --
   -----------------------

   function Extract_Locations
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Message : String) return Locations_List.Vector
   is
      SFP     : constant Pattern_Matcher :=
                  Compile (Secondary_File_Pattern.Get_Pref);
      SFF     : constant Natural := Secondary_File_Pattern_Index.Get_Pref;
      SFL     : constant Natural := Secondary_Line_Pattern_Index.Get_Pref;
      SFC     : constant Natural := Secondary_Column_Pattern_Index.Get_Pref;
      ASFP    : constant Pattern_Matcher :=
        Compile (Alternate_Secondary_Pattern.Get_Pref);
      ASFL    : constant Natural := Alternate_Secondary_Line_Index.Get_Pref;
      Result  : Locations_List.Vector;
      Matched : Match_Array (0 .. 9);
      Loc     : Location;
      Start   : Natural;

   begin
      --  Lookup for primary format of secondary locations.

      Start := Message'First;

      while Start <= Message'Last loop
         Match (SFP, Message (Start .. Message'Last), Matched);

         exit when Matched (0) = No_Match;

         Loc.File := Create
           (+Message (Matched (SFF).First .. Matched (SFF).Last),
            Kernel);
         Loc.First := Matched (1).First;
         Loc.Last  := Matched (1).Last;

         if Matched (SFL) /= No_Match then
            declare
               Val : constant Integer := Safe_Value
                (Message (Matched (SFL).First .. Matched (SFL).Last), 1);

            begin
               if Val >= 1 then
                  Loc.Line := Val;
               else
                  Loc.Line := 1;
               end if;
            end;
         end if;

         if Matched (SFC) /= No_Match then
            declare
               Val : constant Integer := Safe_Value
                 (Message (Matched (SFF).First .. Matched (SFF).Last), 1);
            begin
               if Val >= 1 then
                  Loc.Column :=  Visible_Column_Type (Val);
               else
                  Loc.Column := 1;
               end if;
            end;
         end if;

         Append (Result, Loc);
         Loc := (No_File, 1, 1, 1, 0);
         Start := Matched (1).Last + 1;
      end loop;

      --  Lookup for secondary messages in alternate format

      if Is_Empty (Result) then
         Start := Message'First;

         while Start <= Message'Last loop
            Match (ASFP, Message (Start .. Message'Last), Matched);

            exit when Matched (0) = No_Match;

            Loc.First := Matched (1).First;
            Loc.Last  := Matched (1).Last;

            declare
               Val : constant Integer := Safe_Value
                 (Message (Matched (ASFL).First .. Matched (ASFL).Last), 1);

            begin
               if Val >= 1 then
                  Loc.Line := Val;
               else
                  Loc.Line := 1;
               end if;
            end;

            Append (Result, Loc);
            Loc := (No_File, 1, 1, 1, 0);
            Start := Matched (1).Last + 1;
         end loop;
      end if;

      return Result;
   end Extract_Locations;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel            : access Kernel_Handle_Record'Class;
      Text              : Basic_Types.UTF8_String;
      Category          : Basic_Types.UTF8_String;
      Highlight         : Boolean := False;
      Styles            : Message_Styles_Array :=
        (others => null);
      Show_In_Locations : Boolean := True;
      Allow_Auto_Jump_To_First : Boolean := True)
   is
   begin
      GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
        (Kernel,
         Text,
         Category,
         Highlight,
         Styles,
         "",
         -1,
         -1,
         -1,
         -1,
         -1,
         -1,
         -1,
         Show_In_Locations => Show_In_Locations,
         Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
   end Parse_File_Locations;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel                   : access Kernel_Handle_Record'Class;
      Text                     : Basic_Types.UTF8_String;
      Category                 : String;
      Highlight                : Boolean := False;
      Styles                   : Message_Styles_Array := (others => null);
      File_Location_Regexp     : String;
      File_Index_In_Regexp     : Integer;
      Line_Index_In_Regexp     : Integer;
      Col_Index_In_Regexp      : Integer;
      Msg_Index_In_Regexp      : Integer;
      Style_Index_In_Regexp    : Integer;
      Warning_Index_In_Regexp  : Integer;
      Info_Index_In_Regexp     : Integer;
      Show_In_Locations        : Boolean;
      Allow_Auto_Jump_To_First : Boolean := True)
   is
      function Get_File_Location return GNAT.Regpat.Pattern_Matcher;
      --  Return the pattern matcher for the file location

      function Get_Index
        (Pref  : access Default_Preferences.Integer_Preference_Record'Class;
         Value : Integer) return Integer;
      --  If Value is -1, return Pref, otherwise return Value

      function Get_Message (Last : Natural) return Glib.UTF8_String;
      --  Return the error message. For backward compatibility with existing
      --  preferences file, we check that the message Index is still good.
      --  Otherwise, we return the last part of the regexp

      -----------------------
      -- Get_File_Location --
      -----------------------

      function Get_File_Location return GNAT.Regpat.Pattern_Matcher is
      begin
         if File_Location_Regexp = "" then
            return
              GNAT.Regpat.Compile
                (GPS.Kernel.Preferences.File_Pattern.Get_Pref);

         else
            return GNAT.Regpat.Compile (File_Location_Regexp);
         end if;
      end Get_File_Location;

      Max : Integer := 0;
      --  Maximal value for the indexes

      ---------------
      -- Get_Index --
      ---------------

      function Get_Index
        (Pref  : access Default_Preferences.Integer_Preference_Record'Class;
         Value : Integer) return Integer
      is
         Location : Integer;
      begin
         if Value = -1 then
            Location := Pref.Get_Pref;
         else
            Location := Value;
         end if;

         Max := Integer'Max (Max, Location);
         return Location;
      end Get_Index;

      File_Location : constant GNAT.Regpat.Pattern_Matcher :=
                        Get_File_Location;
      File_Index    : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.File_Pattern_Index,
                           File_Index_In_Regexp);
      Line_Index    : constant Integer :=
                        Get_Index
                         (GPS.Kernel.Preferences.Line_Pattern_Index,
                          Line_Index_In_Regexp);
      Col_Index     : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Column_Pattern_Index,
                           Col_Index_In_Regexp);
      Msg_Index     : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Message_Pattern_Index,
                           Msg_Index_In_Regexp);
      Style_Index   : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Style_Pattern_Index,
                           Style_Index_In_Regexp);
      Info_Index   : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Info_Pattern_Index,
                           Info_Index_In_Regexp);
      Warning_Index : constant Integer :=
                        Get_Index
                          (GPS.Kernel.Preferences.Warning_Pattern_Index,
                           Warning_Index_In_Regexp);
      Matched       : GNAT.Regpat.Match_Array (0 .. Max);
      Start         : Natural := Text'First;
      Last          : Natural;
      Real_Last     : Natural;
      Line          : Natural := 1;
      Column        : Basic_Types.Visible_Column_Type := 1;
      Importance    : Message_Importance_Type := Unspecified;
      Style         : Style_Access;
      Length        : Highlight_Length;

      -----------------
      -- Get_Message --
      -----------------

      function Get_Message (Last : Natural) return Glib.UTF8_String is
      begin
         if Matched (Msg_Index) /= GNAT.Regpat.No_Match then
            return Text
              (Matched (Msg_Index).First .. Matched (Msg_Index).Last);
         else
            return Text (Last + 1 .. Real_Last);
         end if;
      end Get_Message;

      Message : Message_Access;
      Action  : Action_Item;

   begin
      if Show_In_Locations then
         Length := Highlight_Whole_Line;
      else
         Length := 2;
      end if;

      while Start <= Text'Last loop
         --  Parse Text line by line and look for file locations

         while Start < Text'Last
           and then (Text (Start) = ASCII.CR
                     or else Text (Start) = ASCII.LF)
         loop
            Start := Start + 1;
         end loop;

         Real_Last := Start;

         while Real_Last < Text'Last
           and then Text (Real_Last + 1) /= ASCII.CR
           and then Text (Real_Last + 1) /= ASCII.LF
         loop
            Real_Last := Real_Last + 1;
         end loop;

         GNAT.Regpat.Match (File_Location, Text (Start .. Real_Last), Matched);

         if Matched (0) /= GNAT.Regpat.No_Match then
            if Matched (Line_Index) /= GNAT.Regpat.No_Match then
               Line := Integer'Value
                 (Text
                    (Matched (Line_Index).First .. Matched (Line_Index).Last));

               if Line <= 0 then
                  Line := 1;
               end if;
            end if;

            if Matched (Col_Index) = GNAT.Regpat.No_Match then
               Last := Matched (Line_Index).Last;

            else
               Last := Matched (Col_Index).Last;
               Column := Basic_Types.Visible_Column_Type
                  (Safe_Value (Text (Matched (Col_Index).First .. Last)));

               if Column <= 0 then
                  Column := 1;
               end if;
            end if;

            if Highlight then
               if Matched (Warning_Index) /= GNAT.Regpat.No_Match then
                  Importance := Medium;
               elsif  Matched (Style_Index) /= GNAT.Regpat.No_Match then
                  Importance := Low;
               elsif  Matched (Info_Index) /= GNAT.Regpat.No_Match then
                  Importance := Informational;
               else
                  Importance := High;
               end if;
            end if;

            declare
               Msg : constant String := Get_Message (Last);
            begin
               Action := new Line_Information_Record;

               Style := Styles (Importance);

               --  TODO: what is that?
               if Style /= null and then Get_Icon (Style) /= "" then
                  Action.Image := To_Unbounded_String (Get_Icon (Style));
               end if;

               Message := Add_Tool_Message
                 (Get_Messages_Container (Kernel),
                  Glib.Convert.Escape_Text (Category),
                  Create
                    (+Text (Matched
                     (File_Index).First .. Matched (File_Index).Last),
                     Kernel),
                  Positive (Line),
                  Column,
                  Get_Message (Last),
                  Importance,
                  Style,
                  Length,
                  True,
                  Show_In_Locations,
                  Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);

               if Message /= null then
                  Action.Tooltip_Text := To_Unbounded_String (Msg);
                  Message.Set_Action (Action);
               else
                  Free (Action);
               end if;
            end;
         end if;

         Start := Real_Last + 1;
      end loop;
   end Parse_File_Locations;

   -------------------------------------------
   -- Parse_File_Locations_Unknown_Encoding --
   -------------------------------------------

   procedure Parse_File_Locations_Unknown_Encoding
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : Glib.UTF8_String;
      Highlight               : Boolean := False;
      Highlight_Category      : String := "Builder results";
      Style_Category          : String := "Style errors";
      Warning_Category        : String := "Builder warnings";
      Info_Category           : String := "Compiler info";
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1;
      Info_Index_In_Regexp    : Integer := -1;
      Quiet                   : Boolean := False;
      Allow_Auto_Jump_To_First : Boolean := True)
   is
      pragma Unreferenced (Quiet);

      Output : GNAT.Strings.String_Access;
      Valid  : Boolean;
      Styles : Message_Styles_Array;
   begin
      Unknown_To_UTF8 (Text, Output, Valid);
      if not Valid then
         Kernel.Insert
           (-"Locations.parse: could not convert input to UTF8",
            Mode => GPS.Kernel.Error);

      else
         --   ??? reuse existing styles defined in Style_Manager?
         Styles (High) :=
           Get_Style_Manager
             (Kernel_Handle (Kernel)).Get (Highlight_Category);

         Styles (Medium) :=
           Get_Style_Manager
             (Kernel_Handle (Kernel)).Get (Warning_Category);

         Styles (Low) :=
           Get_Style_Manager
             (Kernel_Handle (Kernel)).Get (Style_Category);

         Styles (Informational) :=
           Get_Style_Manager
             (Kernel_Handle (Kernel)).Get (Info_Category);

         if Output = null then
            Parse_File_Locations
              (Kernel                  => Kernel,
               Text                    => Text,
               Category                => Category,
               Highlight               => Highlight,
               Styles                  => Styles,
               File_Location_Regexp    => File_Location_Regexp,
               File_Index_In_Regexp    => File_Index_In_Regexp,
               Line_Index_In_Regexp    => Line_Index_In_Regexp,
               Col_Index_In_Regexp     => Col_Index_In_Regexp,
               Msg_Index_In_Regexp     => Msg_Index_In_Regexp,
               Style_Index_In_Regexp   => Style_Index_In_Regexp,
               Warning_Index_In_Regexp => Warning_Index_In_Regexp,
               Info_Index_In_Regexp    => Info_Index_In_Regexp,
               Show_In_Locations       => True,
               Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
         else
            Parse_File_Locations
              (Kernel                  => Kernel,
               Text                    => Output.all,
               Category                => Category,
               Highlight               => Highlight,
               Styles                  => Styles,
               File_Location_Regexp    => File_Location_Regexp,
               File_Index_In_Regexp    => File_Index_In_Regexp,
               Line_Index_In_Regexp    => Line_Index_In_Regexp,
               Col_Index_In_Regexp     => Col_Index_In_Regexp,
               Msg_Index_In_Regexp     => Msg_Index_In_Regexp,
               Style_Index_In_Regexp   => Style_Index_In_Regexp,
               Warning_Index_In_Regexp => Warning_Index_In_Regexp,
               Info_Index_In_Regexp    => Info_Index_In_Regexp,
               Show_In_Locations       => True,
               Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
            Free (Output);
         end if;
      end if;
   end Parse_File_Locations_Unknown_Encoding;

end GPS.Kernel.Messages.Tools_Output;
