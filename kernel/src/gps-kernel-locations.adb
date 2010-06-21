-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009-2010, AdaCore                 --
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

with Gtk.Tree_Model;            use Gtk.Tree_Model;

with Basic_Types;               use Basic_Types;
with GPS.Kernel.Console;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Legacy;
with GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Styles;         use GPS.Kernel.Styles;
with GPS.Intl;                  use GPS.Intl;
with GPS.Location_View;         use GPS.Location_View;
with UTF8_Utils;                use UTF8_Utils;

package body GPS.Kernel.Locations is

   ---------------
   -- Next_Item --
   ---------------

   procedure Next_Item
     (Kernel    : access Kernel_Handle_Record'Class;
      Backwards : Boolean := False)
   is
      View : constant GPS.Location_View.Location_View :=
               GPS.Location_View.Get_Or_Create_Location_View (Kernel, False);

   begin
      if View /= null then
         GPS.Location_View.Next_Item (View, Backwards);
      end if;
   end Next_Item;

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
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1;
      Quiet                   : Boolean := False)
   is
      pragma Unreferenced (Quiet);

      Output             : Unchecked_String_Access;
      Len                : Natural;
      Valid              : Boolean;
   begin
      Unknown_To_UTF8 (Text, Output, Len, Valid);
      if not Valid then
         GPS.Kernel.Console.Insert
           (Kernel,
            -"Locations.parse: could not convert input to UTF8",
            Mode => Console.Error);

      else
         if Output = null then
            GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
              (Kernel                  => Kernel,
               Text                    => Text,
               Category                => Category,
               Highlight               => Highlight,
               Highlight_Category      =>
                 Get_Or_Create_Style (Kernel, Highlight_Category, False),
               Style_Category          =>
                 Get_Or_Create_Style (Kernel, Style_Category, False),
               Warning_Category        =>
                 Get_Or_Create_Style (Kernel, Warning_Category, False),
               File_Location_Regexp    => File_Location_Regexp,
               File_Index_In_Regexp    => File_Index_In_Regexp,
               Line_Index_In_Regexp    => Line_Index_In_Regexp,
               Col_Index_In_Regexp     => Col_Index_In_Regexp,
               Msg_Index_In_Regexp     => Msg_Index_In_Regexp,
               Style_Index_In_Regexp   => Style_Index_In_Regexp,
               Warning_Index_In_Regexp => Warning_Index_In_Regexp,
               Show_In_Locations       => True);
         else
            GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
              (Kernel                  => Kernel,
               Text                    => Output (1 .. Len),
               Category                => Category,
               Highlight               => Highlight,
               Highlight_Category      =>
                 Get_Or_Create_Style (Kernel, Highlight_Category, False),
               Style_Category          =>
                 Get_Or_Create_Style (Kernel, Style_Category, False),
               Warning_Category        =>
                 Get_Or_Create_Style (Kernel, Warning_Category, False),
               File_Location_Regexp    => File_Location_Regexp,
               File_Index_In_Regexp    => File_Index_In_Regexp,
               Line_Index_In_Regexp    => Line_Index_In_Regexp,
               Col_Index_In_Regexp     => Col_Index_In_Regexp,
               Msg_Index_In_Regexp     => Msg_Index_In_Regexp,
               Style_Index_In_Regexp   => Style_Index_In_Regexp,
               Warning_Index_In_Regexp => Warning_Index_In_Regexp,
               Show_In_Locations       => True);
            Free (Output);
         end if;
      end if;
   end Parse_File_Locations_Unknown_Encoding;

   ------------------------------
   -- Remove_Location_Category --
   ------------------------------

   procedure Remove_Location_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Positive)
   is
      pragma Assert (Category /= "");
      pragma Assert (File /= No_File);

   begin
      GPS.Kernel.Messages.Legacy.Get_Message_At
        (Get_Messages_Container (Kernel), Category, File, Line, 0).Remove;
   end Remove_Location_Category;

end GPS.Kernel.Locations;
