------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with GNATdoc.Markup_Streams;      use GNATdoc.Markup_Streams;
with GNATdoc.Backend.Text_Parser; use GNATdoc.Backend.Text_Parser;
with GNATdoc.Customization.Tag_Handlers.Images;
with GNATdoc.Utils;

procedure GNATdoc.Backend.Parser_Test is
   Text     : Unbounded_String;
   Events   : Event_Vectors.Vector;
   Position : Name_Value_Maps.Cursor;

begin
   --  Register tag handler for 'image' tag.

   GNATdoc.Customization.Tag_Handlers.Register
     (new GNATdoc.Customization.Tag_Handlers.Images.Image_Tag_Handler);

   --  Load input file.

   while not End_Of_File loop
      Append (Text, Get_Line & ASCII.LF);
   end loop;

   Events := Parse_Text (GNATdoc.Utils.Split_Lines (To_String (Text)));

   for Event of Events loop
      case Event.Kind is
         when Start_Tag =>
            Put_Line ("START_TAG '" & To_String (Event.Name) & ''');

            Position := Event.Attributes.First;

            while Name_Value_Maps.Has_Element (Position) loop
               Put_Line
                 ("  '" & Name_Value_Maps.Key (Position) & "' => '"
                      & Name_Value_Maps.Element (Position) & ''');
               Name_Value_Maps.Next (Position);
            end loop;
--            if not Event.Arguments.Is_Empty then
--
--            else
--               Put_Line
--                 ("START_TAG '"
--                    & To_String (Event.Name) & "' '"
--                    & To_String (Event.Parameter) & ''');
--            end if;

         when End_Tag =>
            Put_Line ("END_TAG   '" & To_String (Event.Name) & ''');

         when Markup_Streams.Text =>
            Put_Line ("TEXT      '" & To_String (Event.Text) & ''');
      end case;
   end loop;
end GNATdoc.Backend.Parser_Test;
