------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with GNAT.OS_Lib;              use GNAT.OS_Lib;

with Gdk;                      use Gdk;
with Gdk.Cairo;                use Gdk.Cairo;
with Gdk.Color;                use Gdk.Color;

with Glib.Object;              use Glib.Object;

with Gtk;                      use Gtk;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;       use Gtk.Text_Tag_Table;
with Gtk.Text_Mark;            use Gtk.Text_Mark;

with Pango.Cairo;              use Pango.Cairo;
with Pango.Enums;              use Pango.Enums;

with Commands.Editor;          use Commands.Editor;
with GPS.Editors;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel;               use GPS.Kernel;

with Src_Editor_Buffer.Blocks; use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer;        use Src_Editor_Buffer;
with Src_Editor_Module;        use Src_Editor_Module;
with Traces;                   use Traces;

with Language.Ada;             use Language.Ada;

package body Src_Editor_Buffer.Line_Information is

   Me : constant Debug_Handle := Create ("Src_Editor_Buffer.Line_Information");

   type Line_Info_Note_Record is new Abstract_Note with record
      Data : Line_Info_Width_Array_Access;
      --  The data which contains Message in the source editor side info.

      Style : Style_Access := null;
      --  The style used to highlight this message

      End_Mark : Gtk_Text_Mark;
      --  The mark indicating the end of the message highlighting, if relevant

      At_End_Of_Line : Boolean := False;
      --  True if the message was put at the end of the line and the
      --  highlighting was set one character back.
   end record;
   type Line_Info_Note is access all Line_Info_Note_Record'Class;

   procedure Remove_Line_Information_Column
     (Buffer : access Source_Buffer_Record'Class;
      Column : Integer);
   --  Remove the column from the side window information in Buffer

   procedure Remove_Blank_Lines
     (Buffer                : access Source_Buffer_Record'Class;
      Buffer_Line_At_Blanks : Buffer_Line_Type;
      Number                : Natural;
      Shift                 : Boolean := True);
   --  Remove Number blank lines at Buffer_Line_At_Blanks. If Number = 0,
   --  remove all continuous blank lines at Buffer_Lines_At_Blanks.

   procedure Get_Column_For_Identifier
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Width      : Integer;
      Every_Line : Boolean);
   --  Return the index of the column corresponding to the identifier.
   --  Create such a column if necessary.

   type Line_Processor is access procedure
     (Buffer : access Source_Buffer_Record'Class;
      Data   : in out Line_Data_Record);

   procedure Foreach_Hidden_Line
     (Buffer : access Source_Buffer_Record'Class;
      Fun    : Line_Processor);
   --  Apply Fun once to all hidden lines, in order.

   procedure Add_Side_Information
     (Buffer         : access Source_Buffer_Record'Class;
      Identifier     : String;
      Messages       : Message_Array;
      At_Buffer_Line : Buffer_Line_Type);
   --  Factor code between Add_File_Information and Add_Special_Lines.
   --  If At_Buffer_Line is 0, insert the information at the editable lines
   --  indicated by the indexes of Info. Otherwise, add them at the buffer
   --  lines starting at At_Buffer_Line.

   function Column_For_Identifier
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String) return Integer;
   --  Return the index of the column corresponding to Identifier in the side
   --  information column.

   procedure Put_Message
     (Buffer        : access Source_Buffer_Record'Class;
      Editable_Line : Editable_Line_Type;
      Buffer_Line   : Buffer_Line_Type;
      Column        : Integer;
      Message       : Message_Access);
   --  Add Message in the column in Buffer.
   --  Either Editable_Line or Buffer_Line must be null: if Editable_Line is
   --  null, put the message at Buffer_Line, and vice versa.
   --  Message can be null.
   --  Remove other messages.
   --  Width is the computed width of the message.
   --  If Remove_Old is True, old messages at the pointed line/column will be
   --  removed.
   --  If Override_Old is True, the new message will replace the old message,
   --  otherwise the old message takes precedence and will not be removed

   procedure Highlight_Range
     (Buffer     : access Source_Buffer_Record'Class;
      Style      : Style_Access;
      Line       : Editable_Line_Type;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Remove     : Boolean := False);

   function Message_Is_On_Line
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access;
      Line    : Editable_Line_Type) return Boolean;
   --  Return True if Message is present on the side information for Line

   function Find_Line_With_Message
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access) return Editable_Line_Type;
   --  Return the Line which contains Message, 0 if it wasn't found.

   ------------------------
   -- Message_Is_On_Line --
   ------------------------

   function Message_Is_On_Line
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access;
      Line    : Editable_Line_Type) return Boolean
   is
      Data : constant Line_Info_Width_Array_Access :=
        Get_Side_Information (Buffer, Line);

      C : Message_List.Cursor;
      use Message_List;
   begin
      for K in Data'Range loop
         C := Data (K).Messages.First;

         while Has_Element (C) loop
            if Element (C) = Message then
               return True;
            end if;
            Next (C);
         end loop;
      end loop;
      return False;
   end Message_Is_On_Line;

   ----------------------------
   -- Find_Line_With_Message --
   ----------------------------

   function Find_Line_With_Message
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access) return Editable_Line_Type is
   begin
      for Line in 1 .. Buffer.Last_Editable_Line loop
         if Message_Is_On_Line (Buffer, Message, Line) then
            return Line;
         end if;
      end loop;
      return 0;
   end Find_Line_With_Message;

   -------------------------
   -- Foreach_Hidden_Line --
   -------------------------

   procedure Foreach_Hidden_Line
     (Buffer : access Source_Buffer_Record'Class;
      Fun    : Line_Processor)
   is
      use Lines_List;

      function Recurse_Explore
        (L : Editable_Line_Type) return Editable_Line_Type;
      --  Return the last line that has actually been explored.

      ---------------------
      -- Recurse_Explore --
      ---------------------

      function Recurse_Explore
        (L : Editable_Line_Type) return Editable_Line_Type
      is
         C        : Lines_List.Cursor;
         U        : Universal_Line;
         Prev     : Editable_Line_Type;
      begin
         if Buffer.Editable_Lines (L).Where = In_Mark
           and then Buffer.Editable_Lines (L).UL /= null
         then
            Fun.all (Buffer, Buffer.Editable_Lines (L).UL.Data);
         end if;

         Prev := L;
         C := Buffer.Editable_Lines (L).Stored_Lines.First;

         while Has_Element (C) loop
            U := Element (C);

            if U.Nature = Editable then
               Prev := Recurse_Explore (Prev + 1);
            else
               Fun.all (Buffer, U.Data);
            end if;

            Next (C);
         end loop;

         return Prev;
      end Recurse_Explore;

      Line : Editable_Line_Type;
   begin
      if Fun = null then
         return;
      end if;

      Line := 1;

      while Line < Buffer.Editable_Lines'Last loop
         Line := Recurse_Explore (Line) + 1;
      end loop;
   end Foreach_Hidden_Line;

   -----------------------------------
   -- Recalculate_Side_Column_Width --
   -----------------------------------

   procedure Recalculate_Side_Column_Width
     (Buffer : access Source_Buffer_Record'Class)
   is
      BL : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;
      Line_Char_Width : constant Gint := Line_Number_Character_Width;
      Dummy           : Editable_Line_Type := 1;
   begin
      if Line_Char_Width > 0 then
         Buffer.Line_Numbers_Width := 2;

         if BL.all = null then
            Create_Line_Information_Column
              (Buffer, Default_Column, True, Empty_Line_Information);
         end if;

         loop
            Buffer.Line_Numbers_Width :=
              Buffer.Line_Numbers_Width + Natural (Line_Char_Width);

            Dummy := Dummy * 10;
            exit when Dummy > Buffer.Last_Editable_Line;
         end loop;
      else
         Buffer.Line_Numbers_Width := 0;
      end if;

      Buffer.Total_Column_Width := Buffer.Line_Numbers_Width;

      if BL.all /= null then
         Buffer.Total_Column_Width :=
           Buffer.Total_Column_Width
             + BL.all (BL.all'Last).Starting_X + BL.all (BL.all'Last).Width;
      end if;
   end Recalculate_Side_Column_Width;

   -------------------------------
   -- Get_Column_For_Identifier --
   -------------------------------

   procedure Get_Column_For_Identifier
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Width      : Integer;
      Every_Line : Boolean)
   is
      Column         : Integer;
      Columns_Config : Columns_Config_Access;

      procedure Process_Data
        (Buffer : access Source_Buffer_Record'Class;
         D      : in out Line_Data_Record);
      --  Process data for one line

      procedure Process_Data
        (Buffer : access Source_Buffer_Record'Class;
         D      : in out Line_Data_Record)
      is
         M    : Message_Access;
         Note : Line_Info_Note;
         pragma Unreferenced (Buffer);
      begin
         if D.Side_Info_Data = null then
            D.Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config.all'Range);

            for K in Columns_Config.all'Range loop
               D.Side_Info_Data (K) :=
                 (Message_List.Empty_List,
                  Action => null,
                  Set   => not Columns_Config.all (K).Every_Line);
            end loop;

         else
            declare
               A : Line_Info_Width_Array
                 (D.Side_Info_Data'First .. D.Side_Info_Data'Last + 1);
               C : Message_List.Cursor;
            begin
               A (A'First .. A'Last - 1) := D.Side_Info_Data.all;

               Unchecked_Free (D.Side_Info_Data);
               D.Side_Info_Data := new Line_Info_Width_Array'(A);

               D.Side_Info_Data
                 (D.Side_Info_Data'Last) :=
                 (Messages => Message_List.Empty_List,
                  Action   => null,
                  Set      => not Every_Line);

               --  Regenerate notes for each message in this side info data
               --  There is no need to check for messages at
               --  D.Side_Info_Data'Last since we have just initialized it to
               --  null.
               for J in D.Side_Info_Data'First
                 .. D.Side_Info_Data'Last - 1
               loop
                  C := D.Side_Info_Data (J).Messages.First;

                  while Message_List.Has_Element (C) loop
                     M := Message_List.Element (C);

                     if M /= null
                       and then M.Has_Note (Line_Info_Note_Record'Tag)
                     then
                        Note := Line_Info_Note
                          (M.Get_Note (Line_Info_Note_Record'Tag));
                        Note.Data := D.Side_Info_Data;
                     end if;

                     Message_List.Next (C);
                  end loop;
               end loop;
            end;
         end if;
      end Process_Data;

   begin
      Columns_Config := Buffer.Editable_Line_Info_Columns;

      --  Browse through existing columns and try to match Identifier

      if Columns_Config.all /= null then
         for J in Columns_Config.all'Range loop
            if Columns_Config.all (J).Identifier.all = Identifier then
               Column := J;

               --  Set the new width of the column

               if Columns_Config.all (J).Width < Width then
                  Columns_Config.all (J).Width := Width;

                  for K in (J + 1) .. Columns_Config.all'Last loop
                     Columns_Config.all (K).Starting_X :=
                       Columns_Config.all (K - 1).Starting_X
                       + Columns_Config.all (K - 1).Width + 1;
                  end loop;

                  Recalculate_Side_Column_Width (Buffer);
               end if;

               Columns_Config.all (Column).Every_Line := Every_Line;

               return;
            end if;
         end loop;
      end if;

      --  If we reach this point, that means no column was found that
      --  corresponds to Identifier. Therefore we create one.

      if Columns_Config.all = null then
         Column := 1;
         Columns_Config.all := new Line_Info_Display_Array (1 .. 1);
         Columns_Config.all (1) := new Line_Info_Display_Record'
           (Identifier  => new String'(Identifier),
            Starting_X  => 2,
            Width       => Width,
            Every_Line  => Every_Line);

      else
         declare
            A : Line_Info_Display_Array
              (Columns_Config.all'First .. Columns_Config.all'Last + 1);
         begin
            A (Columns_Config.all'Range) := Columns_Config.all.all;

            A (A'Last) := new Line_Info_Display_Record'
              (Identifier  => new String'(Identifier),
               Starting_X  => Columns_Config.all (A'Last - 1).Starting_X
                 + Columns_Config.all (A'Last - 1).Width + 1,
               Width       => Width,
               Every_Line  => Every_Line);
            Unchecked_Free (Columns_Config.all);
            Columns_Config.all := new Line_Info_Display_Array'(A);

            Column := Columns_Config.all'Last;

            Recalculate_Side_Column_Width (Buffer);
         end;
      end if;

      --  Create a corresponding column in all lines.

      --  First create it in all buffer lines ...
      for J in Buffer.Line_Data'Range loop
         Process_Data (Buffer, Buffer.Line_Data (J));
      end loop;

      --  ... then create it in all hidden lines
      Foreach_Hidden_Line (Buffer, Process_Data'Unrestricted_Access);

   end Get_Column_For_Identifier;

   --------------------------
   -- Get_Side_Information --
   --------------------------

   function Get_Side_Information
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type)
      return Line_Info_Width_Array_Access is
   begin
      return Buffer.Line_Data (Line).Side_Info_Data;
   end Get_Side_Information;

   --------------------------
   -- Get_Side_Information --
   --------------------------

   function Get_Side_Information
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type)
      return Line_Info_Width_Array_Access is
   begin
      if Buffer.Editable_Lines (Line).Where = In_Buffer then
         return Get_Side_Information
           (Buffer, Buffer.Editable_Lines (Line).Buffer_Line);
      else
         return Buffer.Editable_Lines (Line).UL.Data.Side_Info_Data;
      end if;
   end Get_Side_Information;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Every_Line : Boolean;
      Data       : Line_Information_Record)
   is
      Width  : Gint := 10;
      Height : Gint;
      Layout : Pango_Layout;

   begin
      if Data.Text /= null then
         Layout := Create_Pango_Layout
           (Gtk_Widget (Get_Main_Window (Buffer.Kernel)));
         Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
         Set_Markup (Layout, String'(Data.Text.all));
         Get_Pixel_Size (Layout, Width, Height);

      elsif Data.Image /= Null_Pixbuf then
         Width := Get_Width (Data.Image);
      end if;

      Get_Column_For_Identifier
        (Buffer, Identifier, Integer (Width), Every_Line);
      Side_Column_Configuration_Changed (Buffer);
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Buffer : access Source_Buffer_Record'Class;
      Column : Integer)
   is
      Columns_Config : Columns_Config_Access;
      Width          : Integer;

      procedure Process_Data
        (Buffer : access Source_Buffer_Record'Class;
         D      : in out Line_Data_Record);
      --  Process data for one line

      ------------------
      -- Process_Data --
      ------------------

      procedure Process_Data
        (Buffer : access Source_Buffer_Record'Class;
         D      : in out Line_Data_Record)
      is
         M    : Message_Access;
         Note : Line_Info_Note;
         C    : Message_List.Cursor;
         pragma Unreferenced (Buffer);
      begin
         if D.Side_Info_Data /= null then
            if D.Side_Info_Data'First /= Columns_Config.all'First
              or else D.Side_Info_Data'Last /= Columns_Config.all'Last
            then
               Trace (Me, "Inconsistent line data");
               return;
            end if;

            --  If there are messages in the column being removed, remove the
            --  messages

            C := D.Side_Info_Data (Column).Messages.First;

            while Message_List.Has_Element (C) loop
               M := Message_List.Element (C);

               M.Remove;
               C := D.Side_Info_Data (Column).Messages.First;
            end loop;

            declare
               A : Line_Info_Width_Array
                 (Columns_Config.all'First .. Columns_Config.all'Last - 1);
            begin
               A (Columns_Config.all'First .. Column - 1) :=
                 D.Side_Info_Data (Columns_Config.all'First .. Column - 1);
               A (Column .. Columns_Config.all'Last - 1) :=
                 D.Side_Info_Data (Column + 1 .. Columns_Config.all'Last);
               Unchecked_Free (D.Side_Info_Data);

               D.Side_Info_Data := new Line_Info_Width_Array'(A);
            end;

            --  Regenerate notes for each message in this side info data
            for J in D.Side_Info_Data'Range loop
               C := D.Side_Info_Data (J).Messages.First;

               while Message_List.Has_Element (C) loop
                  M := Message_List.Element (C);

                  if M /= null
                    and then M.Has_Note (Line_Info_Note_Record'Tag)
                  then
                     Note := Line_Info_Note
                       (M.Get_Note (Line_Info_Note_Record'Tag));
                     Note.Data := D.Side_Info_Data;
                  end if;

                  Message_List.Next (C);
               end loop;
            end loop;
         end if;
      end Process_Data;

   begin
      Columns_Config := Buffer.Editable_Line_Info_Columns;

      Width := Columns_Config.all (Column).Width;

      --  Free the column for all data

      --  First in all buffer lines...

      for J in Buffer.Line_Data'Range loop
         Process_Data (Buffer, Buffer.Line_Data (J));
      end loop;

      --  ... then in all hidden lines

      Foreach_Hidden_Line (Buffer, Process_Data'Unrestricted_Access);

      declare
         A : Line_Info_Display_Array
           (Columns_Config.all'First .. Columns_Config.all'Last - 1);
      begin
         A (Columns_Config.all'First .. Column - 1) :=
           Columns_Config.all (Columns_Config.all'First .. Column - 1);
         A (Column .. Columns_Config.all'Last - 1) :=
           Columns_Config.all (Column + 1 .. Columns_Config.all'Last);
         Unchecked_Free (Columns_Config.all);

         if A'Length /= 0 then
            Columns_Config.all := new Line_Info_Display_Array'(A);
         end if;
      end;

      if Columns_Config.all /= null then
         for J in Column .. Columns_Config.all'Last loop
            Columns_Config.all (J).Starting_X :=
              Columns_Config.all (J).Starting_X - Width - 1;
         end loop;
      end if;

      Recalculate_Side_Column_Width (Buffer);
   end Remove_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String)
   is
      Columns_Config : Columns_Config_Access;
      Column         : Integer := -1;
   begin
      if Buffer.In_Destruction then
         return;
      end if;

      --  Browse through existing columns and try to match Identifier

      Columns_Config := Buffer.Editable_Line_Info_Columns;

      if Columns_Config /= null and then Columns_Config.all /= null then
         for J in Columns_Config.all'Range loop
            if Columns_Config.all (J).Identifier.all = Identifier then
               Column := J;
               exit;
            end if;
         end loop;
      end if;

      if Column = -1 then
         return;
      end if;

      if Column < Buffer.Block_Highlighting_Column then
         Buffer.Block_Highlighting_Column :=
           Buffer.Block_Highlighting_Column - 1;
      end if;

      Remove_Line_Information_Column (Buffer, Column);
      Side_Column_Configuration_Changed (Buffer);
   end Remove_Line_Information_Column;

   ---------------------------
   -- Free_File_Information --
   ---------------------------

   procedure Free_File_Information
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Extra_Information /= null then
         for J in Buffer.Extra_Information'Range loop
            Free (Buffer.Extra_Information (J).Info);
            GNAT.Strings.Free (Buffer.Extra_Information (J).Identifier);
            Unchecked_Free (Buffer.Extra_Information (J));
         end loop;
         Unchecked_Free (Buffer.Extra_Information);
      end if;
   end Free_File_Information;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Messages   : Message_Array)
   is
   begin
      Add_Side_Information (Buffer, Identifier, Messages, 0);
   end Add_File_Information;

   ---------------------
   -- Remove_Messages --
   ---------------------

   procedure Remove_Messages
     (Buffer     : access Source_Buffer_Record'Class;
      Messages   : Message_Array)
   is
      Column        : Integer;
      Note          : Line_Info_Note;
      Pos           : Message_List.Cursor;
   begin
      for K in Messages'Range loop
         if Has_Note (Messages (K), Line_Info_Note_Record'Tag) then
            Note := Line_Info_Note
              (Messages (K).Get_Note (Line_Info_Note_Record'Tag));

            Column := Column_For_Identifier
              (Buffer, Messages (K).Get_Category);

            Pos := Note.Data (Column).Messages.Find (Messages (K));

            Remove_Message_Highlighting
              (Buffer, Messages (K), Messages (K).Get_Highlighting_Style);

            if Message_List.Has_Element (Pos) then
               Note.Data (Column).Messages.Delete (Pos);
            end if;

            Messages (K).Remove_Note (Line_Info_Note_Record'Tag);
         else
            Remove_Message_Highlighting
              (Buffer, Messages (K), Messages (K).Get_Highlighting_Style);
         end if;
      end loop;

      Side_Column_Configuration_Changed (Buffer);
   end Remove_Messages;

   ---------------------------
   -- Column_For_Identifier --
   ---------------------------

   function Column_For_Identifier
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String) return Integer
   is
      Columns_Config : Columns_Config_Access;
      Column : Integer := -1;
   begin
      Columns_Config := Buffer.Editable_Line_Info_Columns;

      if Columns_Config /= null and then Columns_Config.all /= null then
         for J in Columns_Config.all'Range loop
            if Columns_Config.all (J).Identifier.all = Identifier then
               return J;

            elsif Columns_Config.all (J).Identifier.all = Default_Column then
               --  This branch will make sure that we have a column
               --  corresponding to the default column if there is no column
               --  for Identifier.
               Column := J;
            end if;
         end loop;
      end if;

      return Column;
   end Column_For_Identifier;

   ---------------------------
   -- Add_Extra_Information --
   ---------------------------

   procedure Add_Extra_Information
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Info       : Line_Information_Data)
   is
      Found  : Boolean := False;
   begin

      --  Look for an existing entry

      if Buffer.Extra_Information = null then
         Buffer.Extra_Information := new Extra_Information_Array'
           (1 => new Extra_Information_Record'
              (Identifier => new String'(Identifier),
               Info    => new Line_Information_Record'(Info (Info'First))));

      else
         for J in Buffer.Extra_Information'Range loop
            if Buffer.Extra_Information (J).Identifier.all = Identifier then
               Free (Buffer.Extra_Information (J).Info);
               Buffer.Extra_Information (J).Info :=
                 new Line_Information_Record'(Info (Info'First));
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            declare
               A : Extra_Information_Array
                 (1 .. Buffer.Extra_Information'Last + 1);
            begin
               A (1 .. Buffer.Extra_Information'Last) :=
                 Buffer.Extra_Information.all;
               A (Buffer.Extra_Information'Last + 1) :=
                 new Extra_Information_Record'
                   (Info       => new Line_Information_Record'
                        (Info (Info'First)),
                    Identifier => new String'(Identifier));
            end;
         end if;
      end if;

      Buffer_Information_Changed (Buffer);
   end Add_Extra_Information;

   --------------------------
   -- Add_Side_Information --
   --------------------------

   procedure Add_Side_Information
     (Buffer         : access Source_Buffer_Record'Class;
      Identifier     : String;
      Data           : Line_Information_Array;
      At_Buffer_Line : Buffer_Line_Type)
   is
      Editable_Line : Editable_Line_Type;
      BL            : Buffer_Line_Type;
      The_Data      : Line_Info_Width_Array_Access;
      Column : Integer := -1;
   begin
      Column := Column_For_Identifier (Buffer, Identifier);

      for J in Data'Range loop
         if At_Buffer_Line = 0 then
            Editable_Line := Editable_Line_Type (J);

            case Buffer.Editable_Lines (Editable_Line).Where is
            when In_Buffer =>
               BL := Buffer.Editable_Lines (Editable_Line).Buffer_Line;
               if BL in Buffer.Line_Data'Range
                 and then Buffer.Line_Data (BL).Side_Info_Data /= null
               then
                  The_Data := Buffer.Line_Data (BL).Side_Info_Data;
               end if;

            when In_Mark =>
               if Buffer.Editable_Lines (Editable_Line).UL.Data.Side_Info_Data
                 /= null
               then
                  The_Data := Buffer.Editable_Lines
                    (Editable_Line).UL.Data.Side_Info_Data;
                  BL := 0;
               end if;
            end case;
         else
            The_Data := Buffer.Line_Data
              (At_Buffer_Line + Buffer_Line_Type (J - Data'First))
              .Side_Info_Data;
         end if;

         if The_Data /= null then
            if The_Data (Column).Action /= null then
               Free (The_Data (Column).Action);
            end if;

            The_Data (Column).Action := new Line_Information_Record'(Data (J));
         end if;
      end loop;

      Side_Column_Configuration_Changed (Buffer);
   end Add_Side_Information;

   --------------------------
   -- Add_Side_Information --
   --------------------------

   procedure Add_Side_Information
     (Buffer         : access Source_Buffer_Record'Class;
      Identifier     : String;
      Messages       : Message_Array;
      At_Buffer_Line : Buffer_Line_Type)
   is
      Column : Integer := -1;

      Editable_Line : Editable_Line_Type;
      BL            : Buffer_Line_Type;
   begin

      --  Get the column that corresponds to Identifier

      Column := Column_For_Identifier (Buffer, Identifier);

      --  The column has been found: update the stored data

      if At_Buffer_Line = 0 then
         for K in Messages'Range loop
            Editable_Line := Editable_Line_Type (Get_Line (Messages (K)));
            Put_Message
              (Buffer        => Buffer,
               Editable_Line => Editable_Line,
               Buffer_Line   => 0,
               Column        => Column,
               Message       => Messages (K));
         end loop;
      else
         for K in Messages'Range loop
            BL := At_Buffer_Line + Buffer_Line_Type
              (Messages (K).Get_Line - Messages (Messages'First).Get_Line);
            Put_Message
              (Buffer        => Buffer,
               Editable_Line => 0,
               Buffer_Line   => BL,
               Column        => Column,
               Message       => Messages (K));
         end loop;
      end if;

      Side_Column_Configuration_Changed (Buffer);
   end Add_Side_Information;

   -------------------------
   -- Side_Column_Changed --
   -------------------------

   procedure Side_Column_Changed
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_By_Name (Get_Object (Buffer), "side_column_changed" & ASCII.NUL);
   end Side_Column_Changed;

   ---------------------------------------
   -- Side_Column_Configuration_Changed --
   ---------------------------------------

   procedure Side_Column_Configuration_Changed
     (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_By_Name
        (Get_Object (Buffer),
         "side_column_configuration_changed" & ASCII.NUL);
   end Side_Column_Configuration_Changed;

   --------------------
   -- Draw_Line_Info --
   --------------------

   procedure Draw_Line_Info
     (Buffer      : access Source_Buffer_Record'Class;
      Top_Line    : Buffer_Line_Type;
      Bottom_Line : Buffer_Line_Type;
      View        : Gtk_Text_View;
      Color       : Gdk_Color;
      Layout      : Pango_Layout;
      Drawable    : Cairo.Cairo_Surface)
   is
      Current_Line    : Buffer_Line_Type;
      Editable_Line   : Editable_Line_Type;
      Iter            : Gtk_Text_Iter;
      Y_In_Buffer     : Gint;
      Y_Pix_In_Window : Gint;
      Line_Height     : Gint;
      Dummy_Gint      : Gint;
      Dummy_Boolean   : Boolean;
      Line_Info       : Line_Info_Width;
      Cr              : Cairo_Context;

      Line_Char_Width : constant Gint := Line_Number_Character_Width;

      BL : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;

      procedure Draw_Info (Starting_X : Gint);
      pragma Inline (Draw_Info);
      --  Draw the info contained in Line_Info, at offset Starting_X

      procedure Draw_Line_Info;
      --  Draw the line number for Editable_Line

      procedure Draw_Blank_Line_Info;
      --  Draw information for an area where there is no editable line

      --------------------------
      -- Draw_Blank_Line_Info --
      --------------------------

      procedure Draw_Blank_Line_Info is
      begin
         --  ??? Should we draw something on the side of blank lines ?
         null;
      end Draw_Blank_Line_Info;

      --------------------
      -- Draw_Line_Info --
      --------------------

      procedure Draw_Line_Info is
         Height, Width : Gint;
      begin
         if Editable_Line = 0 then
            return;
         end if;

         Set_Markup (Layout, Editable_Line'Img);
         Get_Pixel_Size (Layout, Width, Height);

         Move_To (Cr,
                  Gdouble (Gint (Buffer.Line_Numbers_Width) - Width),
                  Gdouble (Y_Pix_In_Window));
         Show_Layout (Cr, Layout);
      end Draw_Line_Info;

      ---------------
      -- Draw_Info --
      ---------------

      procedure Draw_Info (Starting_X : Gint) is
         Action : Line_Information_Record;
      begin
         Action := Get_Relevant_Action (Line_Info);

         if Action.Text /= null then
            Set_Markup (Layout, Action.Text.all);

            Move_To (Cr,
                     Gdouble (Starting_X),
                     Gdouble (Y_Pix_In_Window));
            Show_Layout (Cr, Layout);
         end if;

         if Action.Image /= Null_Pixbuf then
            Save (Cr);
            Translate
              (Cr,
               Gdouble (Starting_X),
               Gdouble (Y_Pix_In_Window + (Line_Height -
                   Get_Height (Action.Image)) / 2));
            Set_Source_Pixbuf (Cr, Action.Image, 0.0, 0.0);
            Paint (Cr);
            Restore (Cr);
         end if;
      end Draw_Info;

   begin
      Current_Line := Top_Line;
      Get_Iter_At_Line (Buffer, Iter, Gint (Current_Line - 1));

      Cr := Create (Drawable);
      Set_Source_Color (Cr, Color);

      Drawing_Loop :
      while Current_Line <= Bottom_Line loop

         Get_Line_Yrange (View, Iter, Y_In_Buffer, Line_Height);

         --  Convert the buffer coords back to window coords

         Buffer_To_Window_Coords
           (View, Text_Window_Left,
            Buffer_X => 0, Buffer_Y => Y_In_Buffer,
            Window_X => Dummy_Gint, Window_Y => Y_Pix_In_Window);

         Editable_Line := Get_Editable_Line (Buffer, Current_Line);

         if BL.all /= null
           and then Buffer.Line_Data
             (Current_Line).Side_Info_Data /= null
         then
            for Col in BL.all'Range loop
               Line_Info :=
                 Buffer.Line_Data
                   (Current_Line).Side_Info_Data (Col);

               Draw_Info
                 (Gint (Buffer.Line_Numbers_Width
                  + BL.all (Col).Starting_X));
            end loop;

            if Line_Char_Width > 0 then
               Draw_Line_Info;
            end if;
         else
            Draw_Blank_Line_Info;
         end if;

         Forward_Line (Iter, Dummy_Boolean);

         exit Drawing_Loop when Dummy_Boolean = False;

         Current_Line := Current_Line + 1;
      end loop Drawing_Loop;

      Destroy (Cr);
   end Draw_Line_Info;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Relevant_Action
     (Data : Line_Info_Width) return Line_Information_Record
   is
      C : Message_List.Cursor;
      Action : GPS.Kernel.Messages.Action_Item;
   begin

      --  First look for an action in the messages
      C := Data.Messages.First;

      while Message_List.Has_Element (C) loop
         Action := Message_List.Element (C).Get_Action;
         if Action /= null then
            return Action.all;
         end if;

         Message_List.Next (C);
      end loop;

      --  Next look in the action itself
      if Data.Action /= null then
         return Data.Action.all;
      end if;

      --  Finally return an empty line information
      return Empty_Line_Information;
   end Get_Relevant_Action;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Offset : Gint)
   is
      BL     : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;
      Ignore : Command_Return_Type;
      pragma Unreferenced (Ignore);

      Command : Command_Access;
   begin
      Set_Cursor_Position
        (Buffer, Gint (Line - 1), 0, GPS.Editors.Minimal, False);

      if BL.all /= null then
         for Col in BL.all'Range loop
            if Offset < Gint
              (BL.all (Col).Width + BL.all (Col).Starting_X +
                 Buffer.Line_Numbers_Width)
            then
               Command := Get_Relevant_Action
                 (Buffer.Line_Data
                    (Line).Side_Info_Data (Col)).Associated_Command;

               if Command /= null then
                  if Command.all in
                    Base_Editor_Command_Type'Class
                  then
                     Base_Editor_Command_Type
                       (Command.all).Base_Line := Line;
                  end if;

                  Ignore := Execute (Command);
               end if;

               return;
            end if;
         end loop;
      end if;
   end On_Click;

   ---------------------
   -- Add_Blank_Lines --
   ---------------------

   function Add_Blank_Lines
     (Buffer             : access Source_Buffer_Record'Class;
      Line               : Buffer_Line_Type;
      EL                 : Editable_Line_Type;
      Highlight_Category : Integer;
      Text               : String;
      Name               : String;
      Column_Id          : String;
      Info               : Line_Information_Data)
      return Gtk.Text_Mark.Gtk_Text_Mark
   is
      Iter   : Gtk_Text_Iter;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Number : Positive := 1;
   begin
      if Line = 0 then
         return null;
      end if;

      Buffer.Modifying_Real_Lines := True;
      End_Action (Buffer);

      --  Compute the number of lines we are adding
      for J in Text'Range loop
         if Text (J) = ASCII.LF then
            Number := Number + 1;
         end if;
      end loop;

      Get_Iter_At_Line (Buffer, Iter, Gint (Line - 1));

      Buffer.Modifying_Editable_Lines := False;
      Buffer.Start_Inserting;
      Insert (Buffer, Iter, Text & ASCII.LF);
      Buffer.End_Inserting;
      Buffer.Modifying_Editable_Lines := True;

      --  Shift down editable lines

      for J in EL .. Buffer.Editable_Lines'Last loop
         if Buffer.Editable_Lines (J).Where = In_Buffer then
            Buffer.Editable_Lines (J).Buffer_Line :=
              Buffer.Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Number);
         end if;
      end loop;

      --  Reset information for newly inserted buffer lines

      for J in Line .. Line + Buffer_Line_Type (Number) - 1 loop
         Buffer.Line_Data (J).Editable_Line := 0;
         Buffer.Line_Data (J).Highlight_Category := Highlight_Category;
         Buffer.Line_Data (J).Highlight_Category_Speedbar :=
           Highlight_Category;
         Buffer.Line_Data (J).Highlight_In := (True, True);

         Create_Side_Info (Buffer, J);
      end loop;

      Get_Iter_At_Line_Offset (Buffer, Iter, Gint (Line - 1), 0);

      Side_Column_Changed (Buffer);

      Buffer.Blank_Lines := Buffer.Blank_Lines + Number;

      Mark := Create_Mark (Buffer, Name, Iter);

      --  Store a reference to the mark in the Line_Data.
      Buffer.Line_Data (Line).Line_Mark := Mark;

      Buffer.Modifying_Real_Lines := False;

      if Info /= null
        and then Info'Length /= 0
      then
         Add_Side_Information
           (Buffer         => Buffer,
            Identifier     => Column_Id,
            Data           => Info.all,
            At_Buffer_Line => Line);
      end if;

      return Mark;
   end Add_Blank_Lines;

   -----------------------------
   -- Add_Special_Blank_Lines --
   -----------------------------

   function Add_Special_Blank_Lines
     (Buffer             : access Source_Buffer_Record'Class;
      Line               : Editable_Line_Type;
      Highlight_Category : Integer;
      Number             : Natural;
      Name               : String;
      Column_Id          : String;
      Info               : Line_Information_Data)
      return Gtk.Text_Mark.Gtk_Text_Mark
   is
      LFs : constant String (1 .. Number - 1) := (others => ASCII.LF);
   begin
      return Add_Special_Lines
        (Buffer, Line, Highlight_Category, LFs, Name, Column_Id, Info);
   end Add_Special_Blank_Lines;

   -----------------------
   -- Add_Special_Lines --
   -----------------------

   function Add_Special_Lines
     (Buffer             : access Source_Buffer_Record'Class;
      Line               : Editable_Line_Type;
      Highlight_Category : Integer;
      Text               : String;
      Name               : String;
      Column_Id          : String;
      Info               : Line_Information_Data) return Gtk_Text_Mark
   is
      M : Gtk_Text_Mark;
      B : Buffer_Line_Type;
   begin
      Unfold_Line (Buffer, Line);

      if Line not in Buffer.Editable_Lines'Range then
         return null;
      end if;

      B := Buffer.Editable_Lines (Line).Buffer_Line;
      M := Add_Blank_Lines
        (Buffer, B, Line, Highlight_Category, Text, Name, Column_Id, Info);

      return M;
   end Add_Special_Lines;

   ---------------------------------
   -- Remove_Message_Highlighting --
   ---------------------------------

   procedure Remove_Message_Highlighting
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access;
      Style   : Style_Access)
   is

      From_Column, To_Column : Visible_Column_Type := 0;
      Line, New_Line         : Editable_Line_Type;
      End_Iter               : Gtk_Text_Iter;
      Start_Iter             : Gtk_Text_Iter;
      Note                   : Line_Info_Note;

      Result    : Boolean;
      Has_Iters : Boolean := False;

      Mark : constant Editor_Mark'Class := Message.Get_Editor_Mark;
   begin
      Line := Editable_Line_Type (Mark.Line);

      --  Sanity check: verify that Line does contain Message

      if not Message_Is_On_Line (Buffer, Message, Line) then
         --  This call is slow, only here for extra safety - we should never
         --  go through this
         New_Line := Find_Line_With_Message (Buffer, Message);

         if New_Line = 0 then
            --  This should not happen
            Trace (Me, "Could not find editor line associated with a message");
            return;
         else
            Line := New_Line;
         end if;
      end if;

      From_Column := Mark.Column;

      --  Determine the To_Column:
      --  If there is an End_Mark associated with the message, remove the
      --  highlighting all the way to the End_Mark then remove the End_Mark

      if Message.Has_Note (Line_Info_Note_Record'Tag) then
         Note := Line_Info_Note (Message.Get_Note (Line_Info_Note_Record'Tag));

         if Note.End_Mark /= null then
            Get_Iter_At_Mark (Buffer, End_Iter, Note.End_Mark);

            Get_Iter_At_Screen_Position
              (Buffer, Start_Iter, Line, From_Column);

            if Note.At_End_Of_Line
              or else Ends_Line (Start_Iter)
            then
               Backward_Char (Start_Iter, Result);
            end if;

            Delete_Mark (Buffer, Note.End_Mark);

            Has_Iters := True;
            Note.End_Mark := null;
         end if;
      end if;

      To_Column   := From_Column +
        Visible_Column_Type (Message.Get_Highlighting_Length);

      if From_Column = To_Column then
         Remove_Line_Highlighting (Buffer, Line, Style);
      else
         if Has_Iters then
            Highlight_Range (Buffer, Style, Line, Start_Iter, End_Iter, True);
         else
            Highlight_Range
              (Buffer, Style, Line, From_Column, To_Column, True);
         end if;
      end if;
   end Remove_Message_Highlighting;

   -----------------------
   -- Highlight_Message --
   -----------------------

   procedure Highlight_Message
     (Buffer        : access Source_Buffer_Record'Class;
      Editable_Line : Editable_Line_Type;
      Buffer_Line   : Buffer_Line_Type;
      Message       : Message_Access)
   is
      Style    : Style_Access;
      Length   : Natural;
      End_Col  : Visible_Column_Type;
      EL       : Editable_Line_Type := 0; --  The actual buffer line
      BL       : Buffer_Line_Type := 0;   --  The actual editable line

      Mark     : constant Editor_Mark'Class := Message.Get_Editor_Mark;

      procedure Compute_EL;
      --  Compute EL

      procedure Compute_BL;
      --  Compute BL

      ----------------
      -- Compute_BL --
      ----------------

      procedure Compute_BL is
      begin
         if BL = 0 then
            if Buffer_Line /= 0 then
               BL := Buffer_Line;
            else
               Compute_EL;
               BL := Buffer.Editable_Lines (EL).Buffer_Line;
            end if;
         end if;
      end Compute_BL;

      ----------------
      -- Compute_EL --
      ----------------

      procedure Compute_EL is
      begin
         if EL = 0 then
            if Editable_Line /= 0 then
               EL := Editable_Line;
            else
               EL := Editable_Line_Type (Mark.Line);
            end if;
         end if;
      end Compute_EL;

      Note : Line_Info_Note;
      Iter : Gtk_Text_Iter;

   begin
      Style := Message.Get_Highlighting_Style;

      --  Remove previous style if needed
      if Message.Has_Note (Line_Info_Note_Record'Tag) then
         Note := Line_Info_Note (Message.Get_Note (Line_Info_Note_Record'Tag));
         if Note.Style /= null then
            Remove_Message_Highlighting (Buffer, Message, Note.Style);
         end if;

         --  Record new style in message note
         Note.Style := Style;
      end if;

      --  Highlight if needed
      if Style /= null then
         Length := Message.Get_Highlighting_Length;

         if Length = 0 then
            Compute_BL;

            Set_Line_Highlighting
              (Editor       => Buffer,
               Line         => BL,
               Style        => Style,
               Set          => True,
               Highlight_In =>
                 (Highlight_Speedbar => Style.In_Speedbar,
                  Highlight_Editor   => True));
         else
            if Style.In_Speedbar then
               Compute_BL;

               Set_Line_Highlighting
                 (Editor       => Buffer,
                  Line         => BL,
                  Style        => Style,
                  Set          => True,
                  Highlight_In =>
                    (Highlight_Speedbar => Style.In_Speedbar,
                     Highlight_Editor   => False));
            end if;

            Compute_EL;

            End_Col := Message.Get_Column + Visible_Column_Type (Length);
            --  We are going to highlight a given range: store in the message
            --  note.

            if Note /= null then
               --  It should never happen that note=null here, that would mean
               --  that the message is not properly entered in the database

               Get_Iter_At_Screen_Position (Buffer, Iter, EL, End_Col);
               Note.End_Mark := Create_Mark (Buffer => Buffer, Where => Iter);

               if Ends_Line (Iter) then
                  Note.At_End_Of_Line := True;
               end if;
            end if;

            Highlight_Range
              (Buffer    => Buffer,
               Style     => Style,
               Line      => EL,
               Start_Col => Mark.Column,
               End_Col   => End_Col,
               Remove    => False);
         end if;
      end if;
   end Highlight_Message;

   -----------------
   -- Put_Message --
   -----------------

   procedure Put_Message
     (Buffer        : access Source_Buffer_Record'Class;
      Editable_Line : Editable_Line_Type;
      Buffer_Line   : Buffer_Line_Type;
      Column        : Integer;
      Message       : Message_Access)
   is
      Note     : Line_Info_Note;
      The_Data : Line_Info_Width_Array_Access;
      BL       : Buffer_Line_Type := 0;

      EL       : Editable_Line_Type := Editable_Line;
      Mark     : constant Editor_Mark'Class := Message.Get_Editor_Mark;

   begin
      --  If the message has a mark, we get the line information from this
      --  mark.
      if Mark /= Nil_Editor_Mark then
         EL := Editable_Line_Type (Mark.Line);
      end if;

      if Buffer_Line = 0 then
         if EL not in Buffer.Editable_Lines'Range then
            return;
         end if;

         case Buffer.Editable_Lines (EL).Where is
            when In_Buffer =>
               BL := Buffer.Editable_Lines (EL).Buffer_Line;
               if BL in Buffer.Line_Data'Range
                 and then Buffer.Line_Data (BL).Side_Info_Data /= null
               then
                  The_Data := Buffer.Line_Data (BL).Side_Info_Data;
               end if;

            when In_Mark =>
               if Buffer.Editable_Lines (EL).UL.Data.Side_Info_Data
                 /= null
               then
                  The_Data := Buffer.Editable_Lines
                    (EL).UL.Data.Side_Info_Data;
                  BL := 0;
               end if;
         end case;
      else
         The_Data := Buffer.Line_Data (Buffer_Line).Side_Info_Data;
         BL := Buffer_Line;
      end if;

      if The_Data /= null then
         if Message /= null then
            Note := new Line_Info_Note_Record;
            Note.Data := The_Data;
            Note.Style := Message.Get_Highlighting_Style;
            Message.Set_Note (Note_Access (Note));

            The_Data (Column).Messages.Prepend (Message);
            The_Data (Column).Set := True;

            Highlight_Message (Buffer        => Buffer,
                               Editable_Line => EL,
                               Buffer_Line   => BL,
                               Message       => Message);

            Side_Column_Changed (Buffer);
         end if;
      end if;
   end Put_Message;

   -----------------------
   -- Add_Block_Command --
   -----------------------

   procedure Add_Block_Command
     (Buffer        : access Source_Buffer_Record'Class;
      Editable_Line : Editable_Line_Type;
      Command       : Command_Access;
      Image         : Gdk_Pixbuf)
   is
   begin
      Add_Side_Information
        (Buffer,
         Default_Column,
         --  ??? optimization: should use the integer value for column here:
         --  buffer.block_highlighting_column
         Line_Information_Array'(Integer (Editable_Line)
           => (Text => null,
               Tooltip_Text => null,
               Image        => Image,
               Associated_Command => Command)),
         0);
   end Add_Block_Command;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      Iter : Gtk_Text_Iter;
   begin
      Unfold_Line (Buffer, Line);
      Get_Iter_At_Screen_Position (Buffer, Iter, Line, Column);
      return Create_Mark (Buffer, "", Iter);
   end Create_Mark;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type)
   is
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      EN : constant Editable_Line_Type := Editable_Line_Type (Number);
      Bottom_Line : Buffer_Line_Type;
      Ref_Editable_Line : Editable_Line_Type;

      procedure Expand_Lines
        (N : Buffer_Line_Type);
      --  Expand the line-indexed arrays to contain N lines in size

      ------------------
      -- Expand_Lines --
      ------------------

      procedure Expand_Lines (N : Buffer_Line_Type) is
         H : constant Line_Data_Array := Buffer_Lines.all;
         K : constant Editable_Line_Array := Buffer.Editable_Lines.all;
      begin
         Unchecked_Free (Buffer_Lines);
         Buffer_Lines := new Line_Data_Array (1 .. N * 2);

         Buffer_Lines (H'Range) := H;

         for J in H'Last + 1 .. Buffer_Lines'Last loop
            Buffer_Lines (J) := New_Line_Data;
         end loop;

         Unchecked_Free (Buffer.Editable_Lines);
         Buffer.Editable_Lines := new Editable_Line_Array
           (1 .. Editable_Line_Type (N * 2));

         Buffer.Editable_Lines (K'Range) := K;

         for J in K'Last + 1 .. Buffer.Editable_Lines'Last loop
            Buffer.Editable_Lines (J) :=
              (Where       => In_Buffer,
               Buffer_Line => 0,
               Stored_Lines   => Lines_List.Empty_List,
               Block          => null,
               Stored_Editable_Lines => 0);
         end loop;
      end Expand_Lines;

   begin
      if Number <= 0 then
         return;
      end if;

      --  All block information becomes obsolete anyway. Do the reset before
      --  we change the Editable_Lines, or some of them might not be freed
      --  correctly

      Reset_Blocks_Info (Buffer);

      --  ??? What if inserting in non editable area ?
      Ref_Editable_Line := Buffer_Lines (Start).Editable_Line;

      if not Buffer.Original_Text_Inserted then
         Buffer.Original_Lines_Number := Number;

         if Buffer.Original_Lines_Number >= Buffer_Lines'Last then
            Expand_Lines (Number);
         end if;

         for J in 1 .. Number loop
            Buffer_Lines (Start + J) := New_Line_Data;
            Buffer_Lines (Start + J).Editable_Line := Editable_Line_Type
              (Start + J);
            Buffer_Lines (Start + J).File_Line := File_Line_Type (Start + J);

            if Buffer.Modifying_Editable_Lines then
               Buffer.Editable_Lines (Ref_Editable_Line
                                        + Editable_Line_Type (J)) :=
                 (Where        => In_Buffer,
                  Buffer_Line  => Start + J,
                  Stored_Lines   => Lines_List.Empty_List,
                  Block          => null,
                  Stored_Editable_Lines => 0);
               Create_Side_Info (Buffer, Start + J);
            end if;
         end loop;

         Buffer.Last_Editable_Line := Buffer.Last_Editable_Line +
           Editable_Line_Type (Number);
         Buffer.Original_Text_Inserted := True;

      else
         --  Figure out whether we need to expand the line arrays

         Bottom_Line := Buffer_Line_Type'Max
           (Buffer_Line_Type (Get_Line_Count (Buffer)),
            Buffer_Line_Type (Buffer.Last_Editable_Line)) + Number;

         if Buffer_Lines'Last < Bottom_Line then
            Expand_Lines (Bottom_Line);
         end if;

         for J in reverse Start + Number .. Buffer_Lines'Last loop
            Buffer_Lines (J) := Buffer_Lines (J - Number);

            if Buffer.Modifying_Editable_Lines
              and then Buffer_Lines (J - Number).Editable_Line /= 0
            then
               Buffer_Lines (J).Editable_Line
                 := Buffer_Lines (J - Number).Editable_Line + EN;
            end if;
         end loop;

         if Buffer.Modifying_Editable_Lines then
            for Line in reverse
              Ref_Editable_Line + EN .. Buffer.Last_Editable_Line + EN
            loop
               Editable_Lines (Line) := Editable_Lines (Line - EN);

               if Editable_Lines (Line).Where = In_Buffer then
                  Editable_Lines (Line).Buffer_Line
                    := Editable_Lines (Line - EN).Buffer_Line + Number;
               end if;
            end loop;
         end if;

         --  Remove highlighting for the topmost of moved lines

         Buffer_Lines (Start + Number).Highlight_Category := 0;
         Buffer_Lines (Start + Number).Highlight_Category_Speedbar := 0;

         --  Reset the newly inserted lines

         for J in 0 .. Number - 1 loop
            Buffer_Lines (Start + J) := New_Line_Data;
            Buffer_Lines (Start + J).Editable_Line := Ref_Editable_Line
              + Editable_Line_Type (J);
         end loop;

         if Buffer.Modifying_Editable_Lines then
            for J in 0 .. EN - 1 loop
               Editable_Lines (Ref_Editable_Line + J) :=
                 (Where       => In_Buffer,
                  Buffer_Line => Start + Buffer_Line_Type (J),
                  Stored_Lines   => Lines_List.Empty_List,
                  Block          => null,
                  Stored_Editable_Lines => 0);
               Create_Side_Info (Buffer, Start + Buffer_Line_Type (J));
            end loop;

            Buffer.Last_Editable_Line := Buffer.Last_Editable_Line + EN;
         end if;
      end if;

      Recalculate_Side_Column_Width (Buffer);
      Side_Column_Configuration_Changed (Buffer);
   end Add_Lines;

   ------------------
   -- Remove_Lines --
   ------------------

   procedure Remove_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type)
   is
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Number : constant Buffer_Line_Type := End_Line - Start_Line;
      EN     : Editable_Line_Type := Editable_Line_Type (Number);

   begin
      if End_Line <= Start_Line then
         return;
      end if;

      --  If the lines are not real, then the number of editable lines
      --  actually removed is dependent on folded or hidden lines.

      if not Lines_Are_Real (Buffer) then
         EN := 0;
         for J in Start_Line + 1 .. End_Line loop
            if Buffer_Lines (J).Editable_Line /= 0 then
               EN := EN + 1;
            end if;
         end loop;
      end if;

      if Buffer.Modifying_Editable_Lines then
         for J in Start_Line .. Start_Line + Number - 1 loop
            if Buffer_Lines (J).Side_Info_Data /= null then
               for Col in Buffer_Lines (J).Side_Info_Data'Range loop
                  Free (Buffer, Buffer_Lines (J).Side_Info_Data (Col), True);
               end loop;
            end if;
         end loop;
      end if;

      for J in Start_Line .. Buffer_Lines'Last - Number - 1 loop
         Buffer_Lines (J) := Buffer_Lines (J + Number);

         if Buffer.Modifying_Editable_Lines then
            if Buffer_Lines (J + Number).Editable_Line /= 0 then
               Buffer_Lines (J).Editable_Line :=
                 Buffer_Lines (J + Number).Editable_Line - EN;
            end if;
         end if;
      end loop;

      Reset_Blocks_Info (Buffer);

      if Buffer.Modifying_Editable_Lines then
         declare
            --  This is a trick: we are removing EN lines in the middle of the
            --  array, and moving the soft boundary of the array up EN lines.
            --  Instead of freeing memory here and allocating some at the
            --  bottom of this subprogram, we simply move the allocated
            --  structures down directly.
            Lines_To_Report : Editable_Line_Array (1 .. EN);
         begin
            Lines_To_Report := Editable_Lines
              (Buffer_Lines (Start_Line).Editable_Line
               .. Buffer_Lines (Start_Line).Editable_Line + EN - 1);

            for J in Buffer_Lines (Start_Line).Editable_Line
              .. Buffer.Last_Editable_Line - EN
            loop
               Editable_Lines (J) := Editable_Lines (J + EN);

               if Editable_Lines (J).Where = In_Buffer then
                  Editable_Lines (J).Buffer_Line :=
                    Editable_Lines (J + EN).Buffer_Line - Number;
               end if;
            end loop;

            Editable_Lines
              (Buffer.Last_Editable_Line - EN + 1 ..
                 Buffer.Last_Editable_Line) := Lines_To_Report;
         end;

         Buffer.Last_Editable_Line := Buffer.Last_Editable_Line - EN;
      end if;

      --  Reset bottom lines
      --  ??? Should this be made a simple allocation ?

      for J in Buffer_Line_Type'Max
        (Start_Line + 1, Buffer_Lines'Last - Number) .. Buffer_Lines'Last
      loop
         Buffer_Lines (J) := New_Line_Data;
      end loop;

      Recalculate_Side_Column_Width (Buffer);
      Side_Column_Configuration_Changed (Buffer);
   end Remove_Lines;

   ------------------------
   -- Remove_Blank_Lines --
   ------------------------

   procedure Remove_Blank_Lines
     (Buffer                : access Source_Buffer_Record'Class;
      Buffer_Line_At_Blanks : Buffer_Line_Type;
      Number                : Natural;
      Shift                 : Boolean := True)
   is
      Iter     : Gtk_Text_Iter;
      End_Iter : Gtk_Text_Iter;

      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      Real_Number : Buffer_Line_Type := 0;
      Result      : Boolean := True;
      Buffer_Line : Buffer_Line_Type;

      Begin_Tag   : Gtk_Text_Iter;
      Success     : Boolean;

   begin
      if Buffer.In_Destruction then
         return;
      end if;

      Buffer.Modifying_Real_Lines := True;

      Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line_At_Blanks) - 1);
      Copy (Iter, End_Iter);

      --  Compute the real number of blank lines

      Buffer_Line := Buffer_Line_At_Blanks;

      while Result loop
         exit when Buffer_Lines (Buffer_Line).Editable_Line /= 0;

         Real_Number := Real_Number + 1;
         Forward_Line (End_Iter, Result);
         Buffer_Line := Buffer_Line + 1;

         exit when Real_Number = Buffer_Line_Type (Number);
      end loop;

      --  Remove the non-editable tag
      Copy (Iter, Begin_Tag);
      Backward_Char (Begin_Tag, Success);

      if not Success then
         Copy (Iter, Begin_Tag);
      end if;

      Remove_Tag (Buffer, Buffer.Non_Editable_Tag, Begin_Tag, End_Iter);

      --  Remove the lines

      Buffer.Blank_Lines := Buffer.Blank_Lines - Natural (Real_Number);

      Buffer.Modifying_Editable_Lines := False;
      Buffer.Start_Inserting;
      Delete (Buffer, Iter, End_Iter);
      Buffer.End_Inserting;
      Buffer.Modifying_Editable_Lines := True;

      if Shift then
         for J in Buffer_Line_At_Blanks .. Buffer_Lines'Last loop
            if Buffer_Lines (J).Editable_Line /= 0
              and then Editable_Lines
                (Buffer_Lines (J).Editable_Line).Where = In_Buffer
              and then Editable_Lines
                (Buffer_Lines (J).Editable_Line).Buffer_Line /= 0
            then
               Editable_Lines (Buffer_Lines (J).Editable_Line).Buffer_Line :=
                 Editable_Lines (Buffer_Lines (J).Editable_Line).Buffer_Line
                 - Real_Number;
            end if;
         end loop;
      end if;

      --  Redraw the side column

      Side_Column_Configuration_Changed (Buffer);
      Buffer.Modifying_Real_Lines := False;
   end Remove_Blank_Lines;

   procedure Remove_Blank_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Number : Natural)
   is
      Iter     : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Mark);
      Remove_Blank_Lines
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1), Number);
   end Remove_Blank_Lines;

   ----------------
   -- Hide_Lines --
   ----------------

   procedure Hide_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Number : Editable_Line_Type)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Result               : Boolean;

      Line_Start           : Editable_Line_Type;
      Line_End             : Editable_Line_Type;

      EL                   : Editable_Line_Type;
      Buffer_Line          : Buffer_Line_Type;

      Iter                 : Gtk_Text_Iter;

      Command    : Unhide_Editable_Lines_Command;
      Number_Of_Lines_Folded  : Natural := 0;
      Number_Of_Lines_Removed : Natural := 0;

      Cursor_Move    : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Blocks_Timeout : constant Boolean := Buffer.Blocks_Timeout_Registered;
   begin
      Buffer.Modifying_Real_Lines := True;

      Line_Start := Get_Editable_Line (Buffer, Line);
      Line_End := Line_Start + Number;

      --  If there is no ASCII.LF at the end of the last line, add one since
      --  otherwise moving the cursor to the end of the buffer crashes GPS.

      Get_Iter_At_Line
       (Buffer, Start_Iter, Gint (Get_Buffer_Line (Buffer, Line_End) - 1));

      if not Ends_Line (Start_Iter) then
         Forward_To_Line_End (Start_Iter, Result);
      end if;

      Get_Iter_At_Line (Buffer, Iter, Gint (Line - 1));

      if Is_End (Start_Iter) and then Get_Char (Iter) /= ASCII.LF then
         Insert (Buffer, Start_Iter, "" & ASCII.LF);
      end if;

      --  Disable emitting new cursor positions while we hide lines

      Buffer.Do_Not_Move_Cursor := True;

      EL := 0;

      Buffer_Line := Line + 1;

      Command := new Unhide_Editable_Lines_Type;

      while Number_Of_Lines_Folded <= Natural (Number) loop
         declare
            The_Line : Universal_Line;
            The_Text : String_Access;
         begin
            Get_Iter_At_Line (Buffer, Start_Iter, Gint (Buffer_Line - 1));
            Copy (Start_Iter, End_Iter);

            if not Ends_Line (End_Iter) then
               Forward_To_Line_End (End_Iter, Result);
            end if;

            The_Text := new String'
              (Get_Text (Buffer, Start_Iter, End_Iter, True));

            if Buffer.Line_Data (Buffer_Line).Editable_Line = 0 then
               --  We are in a special line
               The_Line.Nature := Special;
               The_Line.Text := The_Text;
               The_Line.Line_Mark := Buffer.Line_Data (Buffer_Line).Line_Mark;
               The_Line.Data := Buffer.Line_Data (Buffer_Line);

               --  Remove the line from the screen

               Remove_Blank_Lines (Buffer, Buffer_Line, 1, Shift => False);
            else
               --  We are in an editable line
               The_Line.Nature := Editable;
               The_Line.Data := Buffer.Line_Data (Buffer_Line);

               EL := Buffer.Line_Data (Buffer_Line).Editable_Line;

               exit when EL > Line_End;

               --  Mark the Editable line as being In_Mark
               declare
                  Line_Data : Editable_Line_Data :=
                    (Where              => In_Mark,
                     UL                 => new Universal_Line'(The_Line),
                     Text               => null,
                     Block              => null,
                     Stored_Lines       => Editable_Lines
                       (EL).Stored_Lines,
                     Stored_Editable_Lines => Editable_Lines
                       (EL).Stored_Editable_Lines);
               begin
                  --  ??? Could we simplify this?
                  Line_Data.Text := The_Text;
                  Editable_Lines (EL) := Line_Data;

                  Number_Of_Lines_Folded := Number_Of_Lines_Folded +
                    Editable_Lines (EL).Stored_Editable_Lines;
               end;

               Number_Of_Lines_Folded := Number_Of_Lines_Folded + 1;

               --  Remove the line from the screen

               Forward_Char (End_Iter, Result);

               Buffer.Modifying_Editable_Lines := False;
               Buffer.Start_Inserting;
               Buffer.Blocks_Timeout_Registered := True;
               Delete (Buffer, Start_Iter, End_Iter);
               Buffer.Blocks_Timeout_Registered := Blocks_Timeout;
               Buffer.End_Inserting;
               Buffer.Modifying_Editable_Lines := True;
            end if;

            --  Add the line to the store

            Buffer.Editable_Lines (Line_Start).Stored_Lines.Append (The_Line);

            Number_Of_Lines_Removed := Number_Of_Lines_Removed + 1;
         end;
      end loop;

      Buffer.Editable_Lines (Line_Start).Stored_Editable_Lines :=
        Number_Of_Lines_Folded;
      Buffer.Hidden_Lines := Buffer.Hidden_Lines + Number_Of_Lines_Folded;

      --  Shift up editable lines

      for J in Line_End + 1 .. Buffer.Last_Editable_Line loop
         if Editable_Lines (J).Where = In_Buffer then
            Editable_Lines (J).Buffer_Line := Editable_Lines (J).Buffer_Line
              - Buffer_Line_Type (Number_Of_Lines_Removed);
         end if;
      end loop;

      --  Add an icon to unhide the lines

      Command.Buffer := Source_Buffer (Buffer);

      Add_Block_Command
        (Buffer, Line_Start, Command_Access (Command),
         Unhide_Block_Pixbuf);

      Buffer.Modifying_Real_Lines := False;
      Register_Edit_Timeout (Buffer);

      --  Re-enable moving the cursor, and reemit the cursor position

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);
   end Hide_Lines;

   ------------------
   -- Unhide_Lines --
   ------------------

   procedure Unhide_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Editable_Line_Type)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      Iter           : Gtk_Text_Iter;
      Number_Of_Lines_Unfolded : Natural := 0;

      Command        : Hide_Editable_Lines_Command;
      Cursor_Move    : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Blocks_Timeout : constant Boolean := Buffer.Blocks_Timeout_Registered;

      use Lines_List;
      C              : Lines_List.Cursor;
      U              : Universal_Line;

      Current        : Editable_Line_Type;
      --  The number of the line we are currently adding
      Current_B      : Buffer_Line_Type;

      Start_Buffer_Line    : Buffer_Line_Type;
      Start_Iter, End_Iter : Gtk_Text_Iter;

   begin
      --  Initializations

      Buffer.Modifying_Real_Lines := True;

      Current := Start_Line;
      Current_B := Buffer.Editable_Lines (Start_Line).Buffer_Line;
      Start_Buffer_Line := Current_B;

      if Current = 0 then
         --  This should never happen
         Buffer.Modifying_Real_Lines := False;
         return;
      end if;

      --  Disable emitting new cursor positions while we hide lines

      Buffer.Do_Not_Move_Cursor := True;

      --  Do the actual line creation

      C := Buffer.Editable_Lines (Current).Stored_Lines.First;

      while Has_Element (C) loop
         U := Element (C);

         Current_B := Current_B + 1;
         Get_Iter_At_Line (Buffer, Iter, Gint (Current_B - 1));

         case U.Nature is
            when Editable =>
               --  This is an editable line, add it to the text.
               Current := Current + 1;

               Buffer.Modifying_Editable_Lines := False;
               Buffer.Start_Inserting;
               Buffer.Blocks_Timeout_Registered := True;

               Insert (Buffer, Iter,
                       Editable_Lines (Current).Text.all & ASCII.LF);

               Buffer.Blocks_Timeout_Registered := Blocks_Timeout;
               Buffer.End_Inserting;
               Buffer.Modifying_Editable_Lines := True;

               --  Modify editable line structure

               declare
                  Line_Data : constant Editable_Line_Data :=
                    (Where          => In_Buffer,
                     Block          => null,
                     Buffer_Line    => Current_B,
                     Stored_Lines   => Editable_Lines (Current).Stored_Lines,
                     Stored_Editable_Lines =>
                       Editable_Lines (Current).Stored_Editable_Lines);
               begin
                  Buffer.Line_Data (Current_B) :=
                    Editable_Lines (Current).UL.Data;
                  Unchecked_Free (Editable_Lines (Current).UL);
                  GNAT.Strings.Free (Editable_Lines (Current).Text);
                  Editable_Lines (Current) := Line_Data;
               end;

               --  Set the editable line information
               Buffer.Line_Data (Current_B).Editable_Line := Current;

               --  If the line we just inserted contained folded lines,
               --  this changes the number of the line we are currently adding

               Current := Current +
                 Editable_Line_Type
                   (Buffer.Editable_Lines (Current).Stored_Editable_Lines);

               Number_Of_Lines_Unfolded := Number_Of_Lines_Unfolded + 1;

            when Special =>
               declare
                  M    : Gtk_Text_Mark;
                  Iter : Gtk_Text_Iter;
               begin
                  M := Add_Blank_Lines
                    (Buffer             => Buffer,
                     Line               => Current_B,
                     EL                 => Current + 1,
                     Highlight_Category => 0,
                     Text               => U.Text.all,
                     Name               => "",
                     Column_Id          => "",
                     Info               => null);

                  --  If the line previously contained a mark, move it to the
                  --  location of the new mark.

                  if U.Line_Mark /= null then
                     Get_Iter_At_Mark (Buffer, Iter, M);
                     Move_Mark (Buffer => Buffer,
                                Mark   => U.Line_Mark,
                                Where  => Iter);
                  end if;

                  Delete_Mark (Buffer, M);
               end;
         end case;

         Next (C);
      end loop;

      Buffer.Editable_Lines (Start_Line).Stored_Lines.Clear;
      Buffer.Editable_Lines (Start_Line).Stored_Editable_Lines := 0;
      --  ??? NICO free text strings!

      --  Propagate results

      Buffer.Hidden_Lines := Buffer.Hidden_Lines - Number_Of_Lines_Unfolded;

      for J in Current + 1 .. Editable_Lines'Last loop
         if Editable_Lines (J).Where = In_Buffer then
            Editable_Lines (J).Buffer_Line := Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Number_Of_Lines_Unfolded);
         end if;
      end loop;

      --  Highlight the inserted text

      Get_Iter_At_Line (Buffer, Start_Iter, Gint (Start_Buffer_Line - 1));
      Get_Iter_At_Line (Buffer, End_Iter, Gint (Current_B - 1));

      Highlight_Slice (Buffer, Start_Iter, End_Iter);

      --  Redraw the side column

      Side_Column_Configuration_Changed (Buffer);

      --  Add an icon to hide the lines

      Command := new Hide_Editable_Lines_Type;
      Command.Buffer := Source_Buffer (Buffer);
      Command.Number := Editable_Line_Type (Number_Of_Lines_Unfolded);

      Add_Block_Command
        (Buffer, Start_Line,
         Command_Access (Command),
         Hide_Block_Pixbuf);
      Buffer.Modifying_Real_Lines := False;
      Register_Edit_Timeout (Buffer);

      --  Re-enable moving the cursor, and reemit the cursor position

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);
   end Unhide_Lines;

   --------------
   -- Fold_All --
   --------------

   procedure Fold_All (Buffer : access Source_Buffer_Record'Class) is
      Command      : Command_Access;
      Ignore       : Command_Return_Type;
      pragma Unreferenced (Ignore);

      Cursor_Move : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Line        : Editable_Line_Type;

      use Language;

      First_Line_Found : Boolean := Buffer.Lang /= Ada_Lang;
      --  ??? This is a kludge to implement the following feature:
      --  "When folding all blocks, we do not want to fold the main enclosing
      --   block".
      --  This is valid only for languages that do have an enclosing block,
      --  we cannot simply rely on whether a block is top-level or not, since,
      --  for C files, we do want to fold the top-level blocks.
      --  The correct implementation would be for example to add a
      --  "Has_Globally_Enclosing_Construct" function in the Languages, or,
      --  better, to wait until we implement "fold nth sublevel" capacity, and
      --  rely on it.

      Prev_State : constant Constructs_State_Type := Buffer.Constructs_State;

   begin
      if Buffer.Block_Highlighting_Column = -1 then
         return;
      end if;

      Buffer.Do_Not_Move_Cursor := True;

      Line := 1;

      loop
         exit when Line > Buffer.Last_Editable_Line;

         if Buffer.Editable_Lines (Line).Where = In_Buffer then
            declare
               BL : constant Buffer_Line_Type :=
                 Buffer.Editable_Lines (Line).Buffer_Line;
            begin
               if Buffer.Line_Data (BL).Side_Info_Data /= null then
                  if Buffer.Line_Data (BL).Side_Info_Data
                    (Buffer.Block_Highlighting_Column).Action /= null
                  then
                     Command := Buffer.Line_Data (BL).Side_Info_Data
                       (Buffer.Block_Highlighting_Column)
                       .Action.Associated_Command;

                     if Command /= null
                       and then Command.all in
                         Hide_Editable_Lines_Type'Class
                     then
                        if First_Line_Found then
                           Base_Editor_Command (Command).Base_Line :=
                             Buffer.Editable_Lines (Line).Buffer_Line;

                           Line := Line +
                             Hide_Editable_Lines_Type
                               (Command.all).Number - 1;

                           Ignore := Execute (Command);

                           --  even though lines have been deleted,
                           --  constructs info hasn't changed.
                           Buffer.Constructs_State := Prev_State;

                        else
                           First_Line_Found := True;
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end if;

         Line := Line + 1;
      end loop;

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);
   end Fold_All;

   ----------------
   -- Unfold_All --
   ----------------

   procedure Unfold_All (Buffer : access Source_Buffer_Record'Class) is
      Result       : Command_Return_Type;
      pragma Unreferenced (Result);

      Cursor_Move : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Line        : Editable_Line_Type;

      Prev_State  : constant Constructs_State_Type := Buffer.Constructs_State;
   begin
      if Buffer.Block_Highlighting_Column = -1 then
         return;
      end if;

      Buffer.Do_Not_Move_Cursor := True;

      Line := 1;

      loop
         exit when Line > Buffer.Last_Editable_Line;

         if not Buffer.Editable_Lines (Line).Stored_Lines.Is_Empty then
            Unhide_Lines (Buffer, Line);
         end if;

         Line := Line + 1;
      end loop;

      --  Even though lines have been deleted, the constructs state
      --  information hasn't changed.
      Buffer.Constructs_State := Prev_State;

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);
   end Unfold_All;

   ----------------------
   -- Fold_Unfold_Line --
   ----------------------

   function Fold_Unfold_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      Fold   : Boolean) return Boolean
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      Command        : Command_Access;
      BL             : Buffer_Line_Type;
      Action         : Line_Information_Access;
      Ignore         : Command_Return_Type;
      pragma Unreferenced (Ignore);

   begin
      if Buffer.Block_Highlighting_Column = -1 then
         return False;
      end if;

      for L in reverse Editable_Lines'First .. Line loop
         if Buffer.Editable_Lines (L).Where = In_Buffer then
            BL := Buffer.Editable_Lines (L).Buffer_Line;

            if Buffer.Line_Data (BL).Side_Info_Data /= null then
               Action := Buffer.Line_Data (BL).Side_Info_Data
                 (Buffer.Block_Highlighting_Column).Action;

               if Action /= null then
                  Command := Action.Associated_Command;

                  if Command /= null
                    and then
                      ((Fold and then
                          Command.all in Hide_Editable_Lines_Type'Class)
                       or else
                         (not Fold and then
                            Command.all in
                              Unhide_Editable_Lines_Type'Class))
                  then
                     Base_Editor_Command (Command).Base_Line :=
                       Buffer.Editable_Lines (L).Buffer_Line;
                     Ignore := Execute (Command);
                     return True;
                  end if;

               end if;
            end if;

            exit;
         end if;
      end loop;

      return False;
   end Fold_Unfold_Line;

   -----------------
   -- Unfold_Line --
   -----------------

   procedure Unfold_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
   begin
      if Lines_Are_Real (Buffer)
        or else Editable_Lines = null
        or else Line not in Editable_Lines'Range
      then
         return;
      end if;

      while Editable_Lines (Line).Where /= In_Buffer loop
         exit when not Fold_Unfold_Line (Buffer, Line, Fold => False);
      end loop;
   end Unfold_Line;

   ----------------
   -- Fold_Block --
   ----------------

   procedure Fold_Block
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type)
   is
      Ignore : Boolean;
      pragma Unreferenced (Ignore);
   begin
      if Get_Constructs_State (Buffer) /= Exact then
         Compute_Blocks (Buffer);
      end if;

      Ignore := Fold_Unfold_Line (Buffer, Line, Fold => True);
   end Fold_Block;

   -----------------------------------
   -- Remove_Block_Folding_Commands --
   -----------------------------------

   procedure Remove_Block_Folding_Commands
     (Buffer                 : access Source_Buffer_Record'Class;
      Remove_Unfold_Commands : Boolean := True)
   is
      Action  : Line_Information_Access;

   begin
      if Buffer.Block_Highlighting_Column = -1 then
         return;
      end if;

      for Line in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (Line).Side_Info_Data /= null then
            Action := Buffer.Line_Data (Line).Side_Info_Data
              (Buffer.Block_Highlighting_Column).Action;

            if Action /= null then
               if Action.Associated_Command /= null then
                  if Action.Associated_Command.all in
                    Hide_Editable_Lines_Type'Class
                    or else
                      (Remove_Unfold_Commands
                       and then Action.Associated_Command.all in
                         Unhide_Editable_Lines_Type'Class)
                  then
                     Free (Buffer.Line_Data (Line).Side_Info_Data
                           (Buffer.Block_Highlighting_Column).Action);
                     Buffer.Line_Data (Line).Side_Info_Data
                           (Buffer.Block_Highlighting_Column).Action := null;
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Remove_Block_Folding_Commands;

   --------------------
   -- Lines_Are_Real --
   --------------------

   function Lines_Are_Real
     (Buffer : access Source_Buffer_Record'Class) return Boolean is
   begin
      return (not Buffer.Modifying_Real_Lines)
        and then Buffer.Hidden_Lines = 0
        and then Buffer.Blank_Lines = 0;
   end Lines_Are_Real;

   ---------------------
   -- Highlight_Range --
   ---------------------

   procedure Highlight_Range
     (Buffer     : access Source_Buffer_Record'Class;
      Style      : Style_Access;
      Line       : Editable_Line_Type;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Remove     : Boolean := False)
   is
      Tag                  : Gtk_Text_Tag;
      Color                : Gdk_Color;
      New_Tag              : Boolean := False;
   begin
      --  Get the text tag, create it if necessary

      Tag := Lookup (Get_Tag_Table (Buffer), Get_Name (Style));

      if Tag = null then
         if Remove then
            return;
         else
            Gtk_New (Tag, Get_Name (Style));
            New_Tag := True;
         end if;
      end if;

      Color := Get_Background_Color (Style);

      --  ??? Should we do the following even if not New_Tag ?

      if Color /= Null_Color then
         Set_Property (Tag, Background_Gdk_Property, Color);
         Set_Property (Tag, Underline_Property, Pango_Underline_None);
      else
         Set_Property (Tag, Underline_Property, Pango_Underline_Error);
      end if;

      if New_Tag then
         Add (Get_Tag_Table (Buffer), Tag);
      end if;

      --  Highlight/Unhighlight the text

      if Remove then
         Remove_Tag (Buffer, Tag, Start_Iter, End_Iter);
      else
         Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
      end if;

      if Line /= 0 then
         if Style.In_Speedbar then
            if Remove then
               Remove_Line_Highlighting (Buffer, Line, Style);
            else
               Add_Line_Highlighting
                 (Buffer, Line, Style,
                  Highlight_In => (Highlight_Speedbar => True,
                                   others             => False));
            end if;
         end if;
      end if;
   end Highlight_Range;

   ---------------------
   -- Highlight_Range --
   ---------------------

   procedure Highlight_Range
     (Buffer    : access Source_Buffer_Record'Class;
      Style     : Style_Access;
      Line      : Editable_Line_Type;
      Start_Col : Visible_Column_Type;
      End_Col   : Visible_Column_Type;
      Remove    : Boolean := False)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Result               : Boolean;
      The_Line             : Gint;
   begin
      --  Here we test whether the buffer is in destruction. If it is the case
      --  we simply return since it is not worth taking care of unhighlighting
      --  lines. Furthermore this prevents GPS from crashing when we close a
      --  source file used in a visual diff while the reference file is still
      --  being displayed.

      if Buffer.In_Destruction then
         return;
      end if;

      --  Get the boundaries of text to (un)highlight

      if Line = 0 then
         Get_Bounds (Buffer, Start_Iter, End_Iter);

      else
         The_Line :=
           Gint (Get_Buffer_Line (Buffer, Line) - 1);

         if The_Line < 0 then
            return;
         end if;

         if Start_Col <= 0
           or else not Is_Valid_Position (Buffer, Line, Start_Col)
         then
            Get_Iter_At_Line (Buffer, Start_Iter, The_Line);
         else
            Get_Iter_At_Screen_Position
              (Buffer, Start_Iter, Line, Start_Col);

            if Ends_Line (Start_Iter) then
               Backward_Char (Start_Iter, Result);
            end if;
         end if;

         if End_Col <= 0
           or else not Is_Valid_Position (Buffer, Line, End_Col)
         then
            Copy (Start_Iter, End_Iter);
            Forward_To_Line_End (End_Iter, Result);
         else
            Get_Iter_At_Screen_Position
              (Buffer, End_Iter, Line, End_Col);
         end if;
      end if;

      Highlight_Range (Buffer     => Buffer,
                       Style      => Style,
                       Line       => Line,
                       Start_Iter => Start_Iter,
                       End_Iter   => End_Iter,
                       Remove     => Remove);
   end Highlight_Range;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Buffer   : access Source_Buffer_Record'Class;
      Position : Gtk.Text_Mark.Gtk_Text_Mark)
      return Editable_Line_Type
   is
      Iter : Gtk_Text_Iter;
      Line : Editable_Line_Type;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Position);

      Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1));

      if Line = 0 then
         return 1;
      else
         return Line;
      end if;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Buffer   : access Source_Buffer_Record'Class;
      Position : Gtk.Text_Mark.Gtk_Text_Mark)
      return Positive
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Position);

      return Positive (Get_Line_Offset (Iter) + 1);
   end Get_Column;

   ------------------
   -- Flatten_Area --
   ------------------

   function Flatten_Area
     (Buffer            : access Source_Buffer_Record'Class;
      Start_Line        : Editable_Line_Type;
      End_Line          : Editable_Line_Type;
      Start_Buffer_Line : Buffer_Line_Type;
      End_Buffer_Line   : Buffer_Line_Type) return Boolean
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Returned       : Command_Return_Type;
      Result         : Boolean := False;
      pragma Unreferenced (Returned);
   begin
      --  There is nothing to do if the lines are real

      if Lines_Are_Real (Buffer) then
         return False;
      end if;

      --  Unfold all the lines

      for Line in Start_Line .. End_Line loop
         if Editable_Lines (Line).Where /= In_Buffer then
            Result := True;
            Unfold_Line (Buffer, Line);
         end if;
      end loop;

      --  Remove all blank lines

      if Buffer.Blank_Lines /= 0 then
         for BL in reverse Start_Buffer_Line .. End_Buffer_Line loop
            if Get_Editable_Line (Buffer, BL) = 0 then
               Remove_Blank_Lines (Buffer, BL, 0);
            end if;
         end loop;
      end if;

      return Result;
   end Flatten_Area;

   ---------------
   -- Free_Note --
   ---------------

   procedure Free_Note (Message : Message_Access) is
   begin
      if Message = null then
         return;
      end if;

      if Message.Has_Note (Line_Info_Note_Record'Tag) then
         Message.Remove_Note (Line_Info_Note_Record'Tag);
      end if;
   end Free_Note;

   -----------------------
   -- Has_Special_Lines --
   -----------------------

   function Has_Special_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Line_Start : Buffer_Line_Type;
      Line_End   : Buffer_Line_Type)
      return Boolean
   is
      Editable_Line_Start : Editable_Line_Type;
      Editable_Line_End   : Editable_Line_Type;
   begin
      --  Trivial case indicating the absence of special lines
      if Lines_Are_Real (Buffer) then
         return False;
      end if;

      Editable_Line_Start :=  Get_Editable_Line (Buffer, Line_Start);
      Editable_Line_End   := Get_Editable_Line (Buffer, Line_End);

      --  Trivial cases indicating the presence of special lines
      if Editable_Line_Start = 0
        or else Editable_Line_End = 0
        or else Editable_Line_End - Editable_Line_Start
          /= Editable_Line_Type (Line_End - Line_Start)
      then
         return True;
      end if;

      --  Look in all lines
      for J in Line_Start + 1 .. Line_End - 1 loop
         if Get_Editable_Line (Buffer, J) = 0 then
            return True;
         end if;
      end loop;

      return False;
   end Has_Special_Lines;

end Src_Editor_Buffer.Line_Information;
