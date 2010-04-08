-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Regpat;

with Glib.Convert;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.Legacy;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Preferences;
with GPS.Kernel.Styles;
with String_Utils;

package body GPS.Kernel.Messages.Tools_Output is

   use Ada.Strings.Unbounded;
   use Basic_Types;
   use Category_Maps;
   use File_Maps;
   use GNAT.Regpat;
   use GPS.Kernel.Messages.Hyperlink;
   use GPS.Kernel.Messages.Simple;
   use GPS.Kernel.Preferences;
   use GPS.Kernel.Styles;
   use GPS.Styles;
   use Node_Vectors;
   use String_Utils;

   type Location is record
      File   : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line   : Positive := 1;
      Column : Visible_Column_Type := 1;
      First  : Positive := 1;
      Last   : Natural := 0;
   end record;

   procedure Free (X : in out Location) is null;
   --  Free memory associated to X

   package Locations_List is new Generic_List (Location, Free);
   use Locations_List;

   function Extract_Locations
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Message : String) return Locations_List.List;

   ----------------------
   -- Add_Tool_Message --
   ----------------------

   procedure Add_Tool_Message
     (Container          : not null access Messages_Container'Class;
      Category           : String;
      File               : GNATCOLL.VFS.Virtual_File;
      Line               : Positive;
      Column             : Basic_Types.Visible_Column_Type;
      Text               : String;
      Weight             : Natural;
      Highlight_Category : GPS.Styles.Style_Access;
      Length             : Natural;
      Look_For_Secondary : Boolean)
   is
      Locs                   : Locations_List.List;
      Loc                    : Location;
      Node                   : Locations_List.List_Node;
      Has_Secondary_Location : Boolean := False;
      Highlight_Style        : Style_Access;

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
                     return;
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
                      Weight,
                      (Editor_Side => True, Locations => True)));

            if Highlight_Category /= null then
               Highlight_Style :=
                 Get_Or_Create_Style_Copy
                   (Kernel_Handle (Container.Kernel),
                    Get_Name (Highlight_Category) & '/' & Category,
                    Highlight_Category);

               if Length = 0 then
                  Primary.Set_Highlighting (Highlight_Style);

               else
                  Primary.Set_Highlighting (Highlight_Style, Length);
               end if;
            end if;
         end if;

         Node := First (Locs);

         while Node /= Locations_List.Null_Node loop
            Loc := Data (Node);
            Create_Hyperlink_Message
              (Primary,
               Loc.File,
               Loc.Line,
               Loc.Column,
               Text,
               Loc.First,
               Loc.Last,
               (Editor_Side => True, Locations => True));
            Node := Next (Node);
         end loop;

         Free (Locs);
      end;
   end Add_Tool_Message;

   -----------------------
   -- Extract_Locations --
   -----------------------

   function Extract_Locations
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Message : String) return Locations_List.List
   is
      SFP     : constant Pattern_Matcher :=
                  Compile (Secondary_File_Pattern.Get_Pref);
      SFF     : constant Natural := Secondary_File_Pattern_Index.Get_Pref;
      SFL     : constant Natural := Secondary_Line_Pattern_Index.Get_Pref;
      SFC     : constant Natural := Secondary_Column_Pattern_Index.Get_Pref;
      Result  : Locations_List.List;
      Matched : Match_Array (0 .. 9);
      Loc     : Location;
      Start   : Natural := Message'First;

   begin
      while Start <= Message'Last loop
         Match (SFP, Message (Start .. Message'Last), Matched);

         exit when Matched (0) = No_Match;

         Loc.File := Create
           (+Message (Matched (SFF).First .. Matched (SFF).Last),
            Kernel);
         Loc.First := Matched (0).First;
         Loc.Last  := Matched (0).Last;

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

      return Result;
   end Extract_Locations;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel             : access Kernel_Handle_Record'Class;
      Text               : UTF8_String;
      Category           : Glib.UTF8_String;
      Highlight          : Boolean := False;
      Highlight_Category : GPS.Styles.Style_Access := null;
      Style_Category     : GPS.Styles.Style_Access := null;
      Warning_Category   : GPS.Styles.Style_Access := null)
   is
   begin
      GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
        (Kernel,
         Text,
         Category,
         Highlight,
         Highlight_Category,
         Style_Category,
         Warning_Category,
         "",
         -1,
         -1,
         -1,
         -1,
         -1,
         -1);
   end Parse_File_Locations;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : UTF8_String;
      Category                : String;
      Highlight               : Boolean := False;
      Highlight_Category      : GPS.Styles.Style_Access := null;
      Style_Category          : GPS.Styles.Style_Access := null;
      Warning_Category        : GPS.Styles.Style_Access := null;
      File_Location_Regexp    : String;
      File_Index_In_Regexp    : Integer;
      Line_Index_In_Regexp    : Integer;
      Col_Index_In_Regexp     : Integer;
      Msg_Index_In_Regexp     : Integer;
      Style_Index_In_Regexp   : Integer;
      Warning_Index_In_Regexp : Integer)
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
      Weight        : Natural;
      C             : GPS.Styles.Style_Access;

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

   begin
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
               Column := Basic_Types.Visible_Column_Type'Value
                 (Text (Matched (Col_Index).First ..
                            Matched (Col_Index).Last));

               if Column <= 0 then
                  Column := 1;
               end if;
            end if;

            if Highlight then
               if Matched (Warning_Index) /= GNAT.Regpat.No_Match then
                  Weight := 1;
                  C := Warning_Category;
               elsif  Matched (Style_Index) /= GNAT.Regpat.No_Match then
                  Weight := 0;
                  C := Style_Category;
               else
                  Weight := 2;
                  C := Highlight_Category;
               end if;
            else
               Weight := 0;
            end if;

            Add_Tool_Message
              (Get_Messages_Container (Kernel),
               Glib.Convert.Escape_Text (Category),
               Create
                 (+Text (Matched
                  (File_Index).First .. Matched (File_Index).Last),
                  Kernel),
               Positive (Line),
               Column,
               Get_Message (Last),
               Weight,
               C,
               0,
               True);
         end if;

         Start := Real_Last + 1;
      end loop;
   end Parse_File_Locations;

end GPS.Kernel.Messages.Tools_Output;
