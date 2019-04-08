------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Tags;
with GNAT.OS_Lib;              use GNAT.OS_Lib;

with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.Xref;

with Gdk;                      use Gdk;
with Gdk.Cairo;                use Gdk.Cairo;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.RGBA;                 use Gdk.RGBA;
with Glib.Error;
with Glib.Object;              use Glib.Object;
with Gtk;                      use Gtk;
with Gtk.Icon_Theme;           use Gtk.Icon_Theme;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_Mark;            use Gtk.Text_Mark;
with Gtkada.Style;             use Gtkada.Style;
with Pango.Cairo;              use Pango.Cairo;

with Commands.Editor;          use Commands.Editor;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Src_Editor_Buffer.Blocks; use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer;        use Src_Editor_Buffer;
with Src_Editor_Module;        use Src_Editor_Module;

package body Src_Editor_Buffer.Line_Information is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Trace_Handle := Create ("GPS.SOURCE_EDITOR.LINE_INFORMATION");

   type Line_Info_Note_Record is new Abstract_Note with record
      Style : Style_Access := null;
      --  The style used to highlight this message

      End_Mark : Gtk_Text_Mark;
      --  The mark indicating the end of the message highlighting, if relevant

      At_End_Of_Line : Boolean := False;
      --  True if the message was put at the end of the line and the
      --  highlighting was set one character back.
   end record;
   type Line_Info_Note is access all Line_Info_Note_Record'Class;

   Default_Icon_Width : constant := 16;
   --  Maximum size to display icons in the sidebar of editors.
   --  The actual size will be smaller if the line height is smaller.

   Default_Info_Col_Starting_X : constant := 4;
   --  The default starting X for line information columns. This corresponds
   --  to the space between the first information column and the line numbers.

   Default_Line_Number_Left_Margin : constant := 6;
   --  The default left margin of line numbers. This corresponds to the
   --  minimal space left between line numbers and the left border of editors.

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
      Message       : not null Message_Access;
      Editable_Line : Editable_Line_Type;
      Buffer_Line   : Buffer_Line_Type;
      Column        : Integer);
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

   function Message_Is_On_Line
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access;
      Line    : Editable_Line_Type) return Boolean;
   --  Return True if Message is present on the side information for Line

   function Find_Line_With_Message
     (Buffer  : access Source_Buffer_Record'Class;
      Message : Message_Access) return Editable_Line_Type;
   --  Return the Line which contains Message, 0 if it wasn't found.

   function Is_Iter_Visible
     (Buffer : not null access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter) return Boolean;
   --  Return True if the iter is visible or False if the enclosing
   --  block is folded.

   function Find_Line_Info_With_Type
     (Line_Infos : Line_Information_Array;
      Info_Type  : Line_Information_Display_Type)
      return Line_Information_Record;
   --  Return the first encountered line information that matches the given
   --  type.
   --  Return Empty_Line_Information if there is no match.

   procedure On_Click_On_Line_Number
     (Buffer : not null access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type);
   --  Called when the user clicks on a line number.
   --  Execute the action associated to the given line number, if any.

   procedure On_Click_On_Side_Column
     (Buffer : not null access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Col    : Natural);
   --  Called when the user clicks on a side column (e.g: to fold a block).
   --  Execute the action associated with the given column, if any.

   package Source_Buffer_Idle_Sources is
     new Glib.Main.Generic_Sources (Source_Buffer);
   --  Typed idle handler for Source_Buffer_Record'Class.

   function On_Rehightlight_Messages (Self : Source_Buffer) return Boolean;
   --  Rehighlights messages on idle.

   function Image (Data : Line_Info_Width_Array_Access) return String;
   function Image (Data : Line_Info_Width) return String;
   function Image (Data : Gtk_Text_Mark) return String;
   function Image (Data : Highlighting_Data_Array) return String;
   function Image (Data : Message_Reference) return String;
   function Image (Data : Line_Information_Access) return String;
   function Image (Data : Boolean_Array_Access) return String;
   function Image (Data : Commands.Command_Access) return String;
   function Image (Data : Lines_List.List) return String;
   function Image (Data : Universal_Line_Access) return String;
   function Image (Data : Universal_Line) return String;

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
   begin
      if Data = null then
         --  Defensive code, this should not happen
         return False;
      end if;

      for K in Data'Range loop
         for M_Ref of Data (K).Messages loop
            if not M_Ref.Is_Empty and then M_Ref.Message = Message then
               return True;
            end if;
         end loop;
      end loop;

      return False;
   end Message_Is_On_Line;

   ------------------------------
   -- On_Rehightlight_Messages --
   ------------------------------

   function On_Rehightlight_Messages (Self : Source_Buffer) return Boolean is
   begin
      --  Iterate over all the shifted lines to re-highlight the messages that
      --  can be associated.

      for J in Self.Line_Data'Range loop
         if Self.Line_Data (J).Side_Info_Data /= null then
            declare
               Position : Message_Reference_List.Cursor :=
                            Self.Line_Data
                              (J).Side_Info_Data (1).Messages.First;
               Ref      : Message_Reference;

            begin
               --  This code doesn't use "for X of" loop to speedup code.

               while Message_Reference_List.Has_Element (Position) loop
                  Ref := Message_Reference_List.Element (Position);

                  if not Ref.Is_Empty then
                     Highlight_Message
                       (Buffer        => Self,
                        Editable_Line => 0,
                        Buffer_Line   => 0,
                        Message       => Ref.Message);
                  end if;

                  Message_Reference_List.Next (Position);
               end loop;
            end;
         end if;
      end loop;

      Self.Hightlight_Messages_Idle := Glib.Main.No_Source_Id;

      return False;
   end On_Rehightlight_Messages;

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

   ---------------------
   -- Is_Line_Visible --
   ---------------------

   function Is_Line_Visible
     (Buffer : not null access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type) return Boolean
   is
      Iter : Gtk_Text_Iter;
   begin
      Buffer.Get_Iter_At_Line (Iter, Gint (Line - 1));
      return Is_Iter_Visible (Buffer, Iter);
   end Is_Line_Visible;

   ---------------------
   -- Is_Iter_Visible --
   ---------------------

   function Is_Iter_Visible
     (Buffer : not null access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter) return Boolean
   is
      Tags : Text_Tag_List.GSlist;
      Tmp  : Text_Tag_List.GSlist;
      use Gtk.Text_Tag.Text_Tag_List;
   begin
      Tags := Get_Tags (Iter);
      Tmp := Tags;

      --  Verify that the Hidden_Text_Tag is not present on Iter to
      --  decide whether this line is visible or not.

      while Tmp /= Null_List loop
         if Get_Data (Tmp) = Buffer.Hidden_Text_Tag then
            Free (Tags);

            return False;
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (Tags);

      return True;
   end Is_Iter_Visible;

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
      Line_Char_Width    : constant Natural := Natural
        (Line_Number_Character_Width);
      Dummy              : Editable_Line_Type := 1;
      Info_Columns_Width : Natural;
   begin
      if Line_Char_Width > 0 then

         --  Add the default left margin to the minimal space needed
         Buffer.Line_Numbers_Width := Default_Line_Number_Left_Margin;

         --  Create the default line information column if needed
         if BL.all = null then
            Create_Line_Information_Column
              (Buffer, Default_Column, True, Empty_Line_Information);
         end if;

         --  Calculate the width needed depending on the number of characters
         --  needed to display all the lines.
         loop
            Buffer.Line_Numbers_Width :=
              Buffer.Line_Numbers_Width + Line_Char_Width;
            Dummy := Dummy * 10;

            exit when Dummy > Buffer.Last_Editable_Line;
         end loop;

         --  Reserve space for 2 line characters if the file contains less
         --  than 10 lines: this way the space needed won't be changed if
         --  the editor grows to get more that 10 lines.
         if Buffer.Last_Editable_Line < 10 then
            Buffer.Line_Numbers_Width :=
              Buffer.Line_Numbers_Width + Line_Char_Width;
         end if;

         if Visualize_Internal_Buffers.Is_Active then
            Buffer.Line_Numbers_Width := Buffer.Line_Numbers_Width * 3 + 4;
         end if;
      else
         Buffer.Line_Numbers_Width := 0;
      end if;

      --  Get the total width of all the information columns (i.e: where
      --  block folding icons are drawn etc.).

      Info_Columns_Width :=
        (if BL /= null then
            BL.all (BL.all'Last).Starting_X + BL.all (BL.all'Last).Width
         else
            0);

      --  Set the total width of the gutter by additioning the width taken
      --  by the line numbers, the info columns and the gutter right margin
      --  preference.

      Buffer.Total_Column_Width := Buffer.Line_Numbers_Width
        + Info_Columns_Width
        + Gutter_Right_Margin.Get_Pref;
   end Recalculate_Side_Column_Width;

   ----------------------------
   -- Has_Information_Column --
   ----------------------------

   function Has_Information_Column
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String)
     return Boolean
   is
      Columns_Config : constant Columns_Config_Access :=
         Buffer.Editable_Line_Info_Columns;
   begin
      if Columns_Config.all = null then
         return False;
      end if;

      for C of Columns_Config.all.all loop
         if C.Identifier.all = Identifier then
            return True;
         end if;
      end loop;

      return False;
   end Has_Information_Column;

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
         pragma Unreferenced (Buffer);
      begin
         if D.Side_Info_Data = null then
            D.Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config.all'Range);

            for K in Columns_Config.all'Range loop
               D.Side_Info_Data (K) :=
                 (Message_Reference_List.Empty_List,
                  Action => null,
                  Set    => not Columns_Config.all (K).Every_Line);
            end loop;

         else
            declare
               A : Line_Info_Width_Array
                 (D.Side_Info_Data'First .. D.Side_Info_Data'Last + 1);
            begin
               A (A'First .. A'Last - 1) := D.Side_Info_Data.all;

               Unchecked_Free (D.Side_Info_Data);
               D.Side_Info_Data := new Line_Info_Width_Array'(A);

               D.Side_Info_Data
                 (D.Side_Info_Data'Last) :=
                 (Messages => Message_Reference_List.Empty_List,
                  Action   => null,
                  Set      => not Every_Line);
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
            Starting_X  => Default_Info_Col_Starting_X,
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
   -- Get_Internal_Tooltip --
   --------------------------

   function Get_Internal_Tooltip
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type) return String
   is
      Result : Unbounded_String;
      BL     : Line_Data_Record renames Buffer.Line_Data (Line);
      EL     : Editable_Line_Data renames
        Buffer.Editable_Lines (Editable_Line_Type (Line));

   begin
      Append
        (Result, "Buffer.Line_Data:" & Buffer.Line_Data'Last'Img & ASCII.LF);
      Append
        (Result, "Buffer.Editable_Lines:" &
           Buffer.Editable_Lines'Last'Img & ASCII.LF);
      Append
        (Result, "Buffer.Last_Editable_Line:" &
           Buffer.Last_Editable_Line'Img & ASCII.LF);

      if Line in Buffer.Line_Data'Range then
         Append
           (Result,
            "Number:" & Line'Img & ASCII.LF &
              "Line_Data:" & ASCII.LF &
              "  Side_Info_Data:" & ASCII.LF & Image (BL.Side_Info_Data) &
              "  Editable_Line:" & BL.Editable_Line'Img & ASCII.LF &
              "  Line_Mark:" & Image (BL.Line_Mark) &
              "  Highlighting:" & Image (BL.Highlighting));
      end if;

      if Editable_Line_Type (Line) <= Buffer.Last_Editable_Line then
         Append (Result, ASCII.LF & "Editable_Line:" & ASCII.LF);
         Append (Result, "  Where:" & EL.Where'Img & ASCII.LF);
         Append (Result, "  Stored_Lines:" & Image (EL.Stored_Lines));
         Append (Result, "  Stored_Editable_Lines:" &
                   EL.Stored_Editable_Lines'Img & ASCII.LF);
         Append (Result, "  Stored_Editable_Lines:" &
                   EL.Stored_Editable_Lines'Img & ASCII.LF);

         case EL.Where is
            when In_Buffer =>
               Append (Result, "  Buffer_Line:" &
                         EL.Buffer_Line'Img & ASCII.LF);

            when In_Mark =>
               Append (Result, "  Text:" &
                       (if EL.Text = null then " null" else EL.Text.all)
                       & ASCII.LF);

               Append (Result, "  UL:" & Image (EL.UL));
         end case;
      end if;

      Buffer.Kernel.Messages_Window.Insert (To_String (Result));

      return To_String (Result);
   end Get_Internal_Tooltip;

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
      Width  : Gint := Default_Icon_Width;
      Height : Gint;
      Layout : Pango_Layout;

   begin
      if Data.Text /= Null_Unbounded_String then
         Layout := Create_Pango_Layout
           (Gtk_Widget (Get_Main_Window (Buffer.Kernel)));
         Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
         Set_Markup (Layout, To_String (Data.Text));
         Get_Pixel_Size (Layout, Width, Height);

      elsif Data.Image /= Null_Unbounded_String then
         --  ??? We could compute the line height in the editor, and use this,
         --  assuming that icons as square.
         Width := Default_Icon_Width;
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

            for M_Ref of D.Side_Info_Data (Column).Messages loop
               if not M_Ref.Is_Empty then
                  M := M_Ref.Message;
                  M.Remove;
               end if;
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
            Free (Buffer.Extra_Information (J));
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

   --------------------
   -- Remove_Message --
   --------------------

   procedure Remove_Message
     (Buffer    : access Source_Buffer_Record'Class;
      Reference : Message_Reference)
   is
      M : constant Message_Access := Reference.Message;
   begin
      if M /= null then
         Remove_Message_Highlighting
           (Buffer, M, M.Get_Highlighting_Style);

         if Has_Note (M, Line_Info_Note_Record'Tag) then
            M.Remove_Note (Line_Info_Note_Record'Tag);
         end if;
      end if;

      Side_Column_Configuration_Changed (Buffer);
   end Remove_Message;

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
      Info       : access Line_Information_Array;
      Tooltip    : String := "";
      Icon       : String := "")
   is
      Found  : Boolean := False;
   begin

      --  Look for an existing entry

      if Buffer.Extra_Information = null then
         Buffer.Extra_Information := new Extra_Information_Array'
           (1 => new Extra_Information_Record'
              (Identifier => new String'(Identifier),
               Tooltip    => new String'(Tooltip),
               Icon       => new String'(Icon),
               Info       => new Line_Information_Record'(Info (Info'First))));

      else
         for J in Buffer.Extra_Information'Range loop
            if Buffer.Extra_Information (J).Identifier.all = Identifier then
               Free (Buffer.Extra_Information (J).Info);
               Free (Buffer.Extra_Information (J).Tooltip);
               Free (Buffer.Extra_Information (J).Icon);

               Buffer.Extra_Information (J).all :=
                 (Info       => new Line_Information_Record'
                    (Info (Info'First)),
                  Tooltip    => new String'(Tooltip),
                  Icon       => new String'(Icon),
                  Identifier => Buffer.Extra_Information (J).Identifier);

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
                    Tooltip    => new String'(Tooltip),
                    Icon       => new String'(Icon),
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
      Column_From_ID : constant Integer := Column_For_Identifier
        (Buffer, Identifier);
      Editable_Line  : Editable_Line_Type;
      BL             : Buffer_Line_Type;
   begin
      for Message of Messages loop
         if At_Buffer_Line = 0 then
            Editable_Line := Editable_Line_Type (Message.Get_Line);
            BL := 0;
         else
            Editable_Line := 0;
            BL := At_Buffer_Line + Buffer_Line_Type
              (Message.Get_Line - Messages (Messages'First).Get_Line);
         end if;

         Put_Message
           (Buffer        => Buffer,
            Editable_Line => Editable_Line,
            Buffer_Line   => BL,
            Column        => Column_From_ID,
            Message       => Message);
      end loop;

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
         Signal_Side_Column_Configuration_Changed & ASCII.NUL);
   end Side_Column_Configuration_Changed;

   --------------------
   -- Draw_Line_Info --
   --------------------

   procedure Draw_Line_Info
     (Buffer       : access Source_Buffer_Record'Class;
      Top_Line     : Buffer_Line_Type;
      Bottom_Line  : Buffer_Line_Type;
      Current_Line : Buffer_Line_Type;
      As_Line      : Boolean;
      View         : Gtk_Text_View;
      Area         : Gtk_Drawing_Area;
      Color        : Gdk_RGBA;
      Line_Color   : Gdk_RGBA;
      Layout       : Pango_Layout;
      Cr           : Cairo.Cairo_Context)
   is
      Line_Nums   : constant Line_Number_Policy :=
                      Display_Line_Numbers.Get_Pref;
      BL          : Columns_Config_Access renames
                      Buffer.Editable_Line_Info_Columns;
      Ctxt        : constant Gtk_Style_Context := Get_Style_Context (Area);
      Max_Width   : constant Gdouble := Gdouble (Buffer.Line_Numbers_Width);
      Num_Start_X : Gdouble;
      Iter        : Gtk_Text_Iter;

      procedure Draw_Line_Info
        (Y             : Gdouble;
         Line          : Buffer_Line_Type;
         Line_Height   : Gint) with Inline;
      --  Draw line numbers and information for messsages.
      --  This is for line number Line, which is displayed at
      --  coordinates (0,Y) with the specified height

      --------------------
      -- Draw_Line_Info --
      --------------------

      procedure Draw_Line_Info
        (Y             : Gdouble;
         Line          : Buffer_Line_Type;
         Line_Height   : Gint)
      is
         Info : constant Line_Info_Width_Array_Access :=
           Buffer.Line_Data (Line).Side_Info_Data;
         Editable_Line : constant Editable_Line_Type :=
           Get_Editable_Line (Buffer, Line);

         --  Size depends on actual line height, but also on the size
         --  we reserved for the column
         Size          : constant Gint :=
           Gint'Min (Default_Icon_Width, Line_Height - 2);

         LH            : constant Gdouble := Gdouble (Line_Height);
         X             : Gdouble;
         Height, Width : Gint;

         procedure Draw_Number
           (Num         : Integer;
            From        : in out Gdouble;
            Indentation : Gdouble);
         --  Draws the number e.g. the line number.
         --  Right side will be "From" minus "Indentation".
         --  Returns left side in "From".

         procedure Draw_Line_Number_Line_Info
           (Line_Info : Line_Information_Record);

         procedure Draw_Side_Area_Line_Info
           (Line_Info : Line_Information_Record);

         -----------------
         -- Draw_Number --
         -----------------

         procedure Draw_Number
           (Num         : Integer;
            From        : in out Gdouble;
            Indentation : Gdouble) is
         begin
            Layout.Set_Markup (Image (Num, Min_Width => 0));
            Get_Pixel_Size (Layout, Width, Height);
            From := From - Gdouble (Width) - Indentation;
            Move_To (Cr, From, Y);
            Show_Layout (Cr, Layout);
         end Draw_Number;

         --------------------------------
         -- Draw_Line_Number_Line_Info --
         --------------------------------

         procedure Draw_Line_Number_Line_Info
           (Line_Info : Line_Information_Record) is
         begin
            Save (Cr);
            Set_Source_RGBA
              (Cr,
               Get_Background
                 (Line_Info.Message.Message.Get_Highlighting_Style));

            New_Path (Cr);
            Move_To (Cr, 0.0, Y);
            Line_To (Cr, Max_Width, Y);

            --  We should make sure that we never draw on top of
            --  the first line's information column: that's why we
            --  draw until where the column starts.
            Line_To
              (Cr,
               Max_Width + Gdouble (Default_Info_Col_Starting_X),
               Y + LH / 2.0);

            Line_To (Cr, Max_Width, Y + LH);
            Line_To (Cr, 0.0, Y + LH);
            Close_Path (Cr);
            Cairo.Fill (Cr);
            Restore (Cr);
         end Draw_Line_Number_Line_Info;

         ------------------------------
         -- Draw_Side_Area_Line_Info --
         ------------------------------

         procedure Draw_Side_Area_Line_Info
           (Line_Info : Line_Information_Record)
         is
            Image : Unbounded_String := Line_Info.Image;
         begin
            --  Draw the associated text first, if any

            if Line_Info.Text /= Null_Unbounded_String then
               Set_Markup (Layout, To_String (Line_Info.Text));
               Move_To (Cr, X, Y);
               Show_Layout (Cr, Layout);
            end if;

            --  If there is no image set directly in the line info, look at the
            --  image set in the associated message, if any.

            if Image = Null_Unbounded_String then
               if not Line_Info.Message.Is_Empty then
                  declare
                     Message : constant Message_Access :=
                                 Line_Info.Message.Message;
                     Action  : Action_Item;
                  begin
                     if Message /= null then
                        Action := Message.Get_Action;
                        if Action /= null then
                           Image := Action.Image;
                        end if;
                     end if;
                  end;
               end if;
            end if;

            --  Draw the image, if any

            if Image /= Null_Unbounded_String then
               declare
                  P            : Gdk_Pixbuf;
                  Info         : Gtk_Icon_Info;
                  Was_Symbolic : aliased Boolean;
                  Error        : aliased Glib.Error.GError;
                  Strs         : GNAT.Strings.String_List :=
                                   (1 => new String'(To_String (Image)));
               begin
                  --   ??? Should have a cache
                  Info := Choose_Icon_For_Scale
                    (Gtk.Icon_Theme.Get_Default, Strs, Size, 1, 0);
                  Free (Strs);

                  if Info /= null then
                     P := Load_Symbolic_For_Context
                       (Icon_Info    => Info,
                        Context      => Ctxt,
                        Was_Symbolic => Was_Symbolic'Access,
                        Error        => Error'Access);
                     Render_Icon
                       (Ctxt, Cr, P,
                        X,
                        Y + Gdouble ((Line_Height - Size) / 2));
                     Unref (P);
                     Unref (Info);
                  end if;
               end;
            end if;
         end Draw_Side_Area_Line_Info;

      begin
         --  Draw messages

         for Col in BL.all'Range loop
            X := Gdouble (Buffer.Line_Numbers_Width + BL.all (Col).Starting_X);

            declare
               Line_Infos            : constant Line_Information_Array :=
                                         Get_Line_Infos (Info (Col));
               Line_Number_Line_Info : constant Line_Information_Record :=
                                         Find_Line_Info_With_Type
                                           (Line_Infos => Line_Infos,
                                            Info_Type  => On_Line_Number);
               Side_Area_Line_Info   : constant Line_Information_Record :=
                                         Find_Line_Info_With_Type
                                           (Line_Infos => Line_Infos,
                                            Info_Type  => On_Side_Area);
            begin
               --  Draw the first line information that should be displayed
               --  on line numbers, if any.
               if Line_Number_Line_Info /= Empty_Line_Information then
                  Draw_Line_Number_Line_Info (Line_Number_Line_Info);
               end if;

               --  Draw the first line information that should be displayed
               --  on the editor's side column, if any.
               if Side_Area_Line_Info /= Empty_Line_Information then
                  Draw_Side_Area_Line_Info (Side_Area_Line_Info);
               end if;
            end;
         end loop;

         --  Check if we should show line numbers

         if Line_Nums = All_Lines
           or else (Line_Nums = Some_Lines and then Editable_Line mod 5 = 0)
         then
            --  Center the number in the gutter
            Num_Start_X := Gdouble (Buffer.Line_Numbers_Width);

            if Visualize_Internal_Buffers.Is_Active
              or else (Editable_Line > 0
                       and then Is_Iter_Visible (Buffer, Iter))
            --  don't draw 0 (codepeer)
            then
               Draw_Number (Integer (Editable_Line), Num_Start_X, 0.0);
            end if;

            if Visualize_Internal_Buffers.Is_Active then
               --  Draw Editable_Lines
               if Editable_Line in Buffer.Editable_Lines'Range
                 and then Buffer.Editable_Lines
                   (Editable_Line).Where = In_Buffer
               then
                  Draw_Number
                    (Integer
                       (Buffer.Editable_Lines (Editable_Line).Buffer_Line),
                     Num_Start_X, 2.0);
               end if;
            end if;
         end if;
      end Draw_Line_Info;

      L               : Buffer_Line_Type;
      Y_In_Buffer     : Gint;
      Line_Height     : Gint;
      Dummy_Gint      : Gint;
      More_Lines      : Boolean;
      Y_Pix_In_Window : Gint;

   begin
      L := Top_Line;
      Get_Iter_At_Line (Buffer, Iter, Gint (L - 1));

      Set_Source_RGBA (Cr, Color);

      if Buffer.Line_Data'Last < Bottom_Line then
         Trace (Me, "Inconsistent state of Buffer's data, last buffer line:" &
                  Buffer_Line_Type'Image (Buffer.Line_Data'Last) &
                  " and bottom:" & Bottom_Line'Img);
      end if;

      Drawing_Loop :
      while L <= Buffer_Line_Type'Min (Bottom_Line, Buffer.Line_Data'Last) loop

         Get_Line_Yrange (View, Iter, Y_In_Buffer, Height => Line_Height);

         --  Convert the buffer coords back to window coords

         Buffer_To_Window_Coords
           (View, Text_Window_Text,
            Buffer_X => 0, Buffer_Y => Y_In_Buffer,
            Window_X => Dummy_Gint, Window_Y => Y_Pix_In_Window);

         if L = Current_Line then
            --  Draw the current line color

            Save (Cr);
            if As_Line then
               Set_Line_Width (Cr, 1.0);
               Draw_Line
                 (Cr, Line_Color,
                  0, Y_Pix_In_Window + Line_Height,
                  Gint (Buffer.Total_Column_Width) + 1,
                  Y_Pix_In_Window + Line_Height);
            else
               Set_Source_RGBA (Cr, Line_Color);
               Cairo.Rectangle
                 (Cr, 0.0, Gdouble (Y_Pix_In_Window),
                  Gdouble (Buffer.Total_Column_Width) + 1.0,
                  Gdouble (Line_Height));
               Cairo.Fill (Cr);
            end if;
            Restore (Cr);
         end if;

         if BL.all /= null
           and then Buffer.Line_Data (L).Side_Info_Data /= null
         then
            Draw_Line_Info
              (Y           => Gdouble (Y_Pix_In_Window),
               Line        => L,
               Line_Height => Line_Height);
         end if;

         Forward_Line (Iter, More_Lines);

         exit Drawing_Loop when not More_Lines;

         L := L + 1;
      end loop Drawing_Loop;
   end Draw_Line_Info;

   --------------------
   -- Get_Line_Infos --
   --------------------

   function Get_Line_Infos
     (Data : Line_Info_Width) return Line_Information_Array
   is
      package Line_Information_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Line_Information_Record,
         "="          => "=");
      M          : Message_Access;
      Line_Info  : GPS.Kernel.Messages.Action_Item;
      Line_Infos : Line_Information_Lists.List;
      J          : Integer := 1;
   begin

      --  First look for line information in the messages
      for Message_Ref of Data.Messages loop
         M := Message_Ref.Message;
         if M /= null then
            Line_Info := M.Get_Action;
            if Line_Info /= null then
               Line_Infos.Append (Line_Info.all);
            end if;
         end if;
      end loop;

      --  Next look in the action itself
      if Data.Action /= null then
         Line_Infos.Append (Data.Action.all);
      end if;

      return Result : Line_Information_Array (J .. Integer (Line_Infos.Length))
      do
         for Line_Info of Line_Infos loop
            Result (J) := Line_Info;
            J := J + 1;
         end loop;
      end return;
   end Get_Line_Infos;

   ------------------------------
   -- Find_Line_Info_With_Type --
   ------------------------------

   function Find_Line_Info_With_Type
     (Line_Infos : Line_Information_Array;
      Info_Type  : Line_Information_Display_Type)
      return Line_Information_Record is
   begin
      for Line_Info of Line_Infos loop
         if Get_Display_Type (Line_Info) = Info_Type then
            return Line_Info;
         end if;
      end loop;

      return Empty_Line_Information;
   end Find_Line_Info_With_Type;

   -----------------------
   -- Execute_Line_Info --
   -----------------------

   procedure Execute_Line_Info
     (Buffer    : access Source_Buffer_Record'Class;
      Line_Info : Line_Information_Record;
      At_Line   : Buffer_Line_Type)
   is
      Ignore  : Command_Return_Type;
      Context : Selection_Context;
   begin
      if Line_Info.Associated_Command /= null then

         --  If the associated command needs line information, set it before
         --  executing it.

         if Line_Info.Associated_Command.all in
           Base_Editor_Command_Type'Class
         then
            Base_Editor_Command_Type
              (Line_Info.Associated_Command.all).Base_Line := At_Line;
         end if;

         if not Line_Info.Message.Is_Empty then
            --  Update selection context when message
            --  information was associated with Line_Info.

            Context := Buffer.Kernel.Get_Current_Context;
            Set_Messages_Information
              (Context, (1 => Line_Info.Message.Message));
            Buffer.Kernel.Context_Changed (Context);
         end if;

         Ignore := Line_Info.Associated_Command.Execute;

         if not Line_Info.Message.Is_Empty then
            --  Refresh selection context to initial state
            Buffer.Kernel.Refresh_Context;
         end if;
      end if;
   end Execute_Line_Info;

   -----------------------------
   -- On_Click_On_Line_Number --
   -----------------------------

   procedure On_Click_On_Line_Number
     (Buffer : not null access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type)
   is
      BL         : Columns_Config_Access renames
                     Buffer.Editable_Line_Info_Columns;
      Info       : constant Line_Info_Width_Array_Access :=
                     Buffer.Line_Data (Line).Side_Info_Data;
      Line_Infos : constant Line_Information_Array :=
                     Get_Line_Infos (Info (BL.all'First));
      Line_Info  : constant Line_Information_Record :=
                     Find_Line_Info_With_Type
                       (Line_Infos => Line_Infos,
                        Info_Type  => On_Line_Number);
   begin
      --  Execute the first line information's action that is related with
      --  editor line numbers.

      if Line_Info /= Empty_Line_Information then
         Trace (Me, "Found one action in editor_line");
         Execute_Line_Info
           (Buffer    => Buffer,
            Line_Info => Line_Info,
            At_Line   => Line);
         return;
      end if;

      --  If there is no line informations' actions related with editor line
      --  numbers, execute the default one.

      Trace (Me, "Execute default action for click on line number");
      Execute_Default_Line_Number_Click
        (Buffer.Kernel, Buffer.Kernel.Get_Current_Context);
   end On_Click_On_Line_Number;

   -----------------------------
   -- On_Click_On_Side_Column --
   -----------------------------

   procedure On_Click_On_Side_Column
     (Buffer : not null access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Col    : Natural)
   is
      Info       : constant Line_Info_Width_Array_Access :=
                     Buffer.Line_Data (Line).Side_Info_Data;
      Line_Infos : constant Line_Information_Array       :=
                     Get_Line_Infos (Info (Col));
      Line_Info  : constant Line_Information_Record :=
                     Find_Line_Info_With_Type
                       (Line_Infos => Line_Infos,
                        Info_Type  => On_Side_Area);
   begin
      if Line_Info.Associated_Command /= null then
         Trace (Me, "Execute command for line" & Line'Img);
         Execute_Line_Info
           (Buffer    => Buffer,
            Line_Info => Line_Info,
            At_Line   => Line);
      end if;
   end On_Click_On_Side_Column;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Offset : Gint)
   is
      BL : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;
   begin
      Set_Cursor_Position (Buffer, Gint (Line - 1), 0, False);

      if BL.all = null then
         null;

      --  Click on line numbers

      elsif Offset <= Gint (Buffer.Line_Numbers_Width) then
         On_Click_On_Line_Number
           (Buffer => Buffer,
            Line   => Line);

      --  Click on other columns

      else
         for Col in BL.all'Range loop
            if Offset < Gint
              (BL.all (Col).Width + BL.all (Col).Starting_X +
                 Buffer.Line_Numbers_Width)
            then
               On_Click_On_Side_Column
                 (Buffer => Buffer,
                  Line   => Line,
                  Col    => Col);
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
      Style              : Style_Access;
      Text               : String;
      Name               : String;
      Column_Id          : String;
      Info               : Line_Information_Data)
      return Gtk.Text_Mark.Gtk_Text_Mark
   is
      use type Glib.Main.G_Source_Id;

      Iter     : Gtk_Text_Iter;
      Mark     : Gtk.Text_Mark.Gtk_Text_Mark;
      Number   : Positive := 1;
      New_Line : Buffer_Line_Type;
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

      --  The marks of the messages of *Line* have left gravity
      --  so the marks at column 1 will be located in the added special lines
      New_Line := Buffer_Line_Type (Positive (Line) + Number);
      if Buffer.Line_Data (New_Line).Side_Info_Data /= null then
         for J in Buffer.Line_Data (New_Line).Side_Info_Data'Range loop
            declare
               Message_List : Message_Reference_List.List renames
                 Buffer.Line_Data (New_Line).Side_Info_Data (J).Messages;
            begin
               if not Message_List.Is_Empty then
                  for Ref of Message_List loop
                     if not Ref.Is_Empty
                       and then Message (Ref).Get_Column = 1
                     then
                        Forward_Chars (This   => Message (Ref).Get_Editor_Mark,
                                       Offset => Text'Length + 1);
                     end if;
                  end loop;
               end if;
            end;
         end loop;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Gint (Line - 1), 0);

      --  Highlight the added lines using the given style, if non-null
      if Style /= null then
         declare
            End_Iter   : Gtk_Text_Iter;
            Success    : Boolean;
         begin
            Copy (Iter, Dest => End_Iter);
            Forward_Chars (End_Iter, Text'Length, Success);

            Buffer.Highlighter.Highlight_Range
              (Style      => Style,
               Line       => EL,
               Start_Iter => Iter,
               End_Iter   => End_Iter);

            --  Save the style for each of the added lines
            for J in 0 .. Number - 1 loop
               Buffer.Line_Data
                 (Buffer_Line_Type (Integer (Line) + J)).Style := Style;
            end loop;
         end;
      end if;

      --  Shift down editable lines

      for J in EL .. Buffer.Editable_Lines'Last loop
         if Buffer.Editable_Lines (J).Where = In_Buffer
           and then Buffer.Editable_Lines (J).Buffer_Line /= 0
         then
            Buffer.Editable_Lines (J).Buffer_Line :=
              Buffer.Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Number);
         end if;
      end loop;

      --  Reset information for newly inserted buffer lines

      for J in Line .. Line + Buffer_Line_Type (Number) - 1 loop
         Buffer.Line_Data (J).Editable_Line := 0;

         Create_Side_Info (Buffer, J);
      end loop;

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

      --  Register idle handler to rehightlight messages after buffer
      --  modification. It allows to do it only once after all modifications
      --  of the source buffer.

      if Buffer.Hightlight_Messages_Idle = Glib.Main.No_Source_Id then
         Buffer.Hightlight_Messages_Idle :=
           Source_Buffer_Idle_Sources.Idle_Add
             (On_Rehightlight_Messages'Access,
              Buffer,
              Glib.Main.Priority_Default);
      end if;

      return Mark;
   end Add_Blank_Lines;

   -----------------------------
   -- Add_Special_Blank_Lines --
   -----------------------------

   function Add_Special_Blank_Lines
     (Buffer             : access Source_Buffer_Record'Class;
      Line               : Editable_Line_Type;
      Style              : Style_Access;
      Number             : Natural;
      Name               : String;
      Column_Id          : String;
      Info               : Line_Information_Data)
      return Gtk.Text_Mark.Gtk_Text_Mark
   is
      LFs : constant String (1 .. Number - 1) := (others => ASCII.LF);
   begin
      return Add_Special_Lines
        (Buffer, Line, Style, LFs, Name, Column_Id, Info);
   end Add_Special_Blank_Lines;

   -----------------------
   -- Add_Special_Lines --
   -----------------------

   function Add_Special_Lines
     (Buffer             : access Source_Buffer_Record'Class;
      Line               : Editable_Line_Type;
      Style              : Style_Access;
      Text               : String;
      Name               : String;
      Column_Id          : String;
      Info               : Line_Information_Data)
      return Gtk.Text_Mark.Gtk_Text_Mark
   is
      M : Gtk_Text_Mark;
      B : Buffer_Line_Type;
   begin
      if Line not in Buffer.Editable_Lines'Range then
         return null;
      end if;

      B := Buffer.Editable_Lines (Line).Buffer_Line;
      M := Add_Blank_Lines
        (Buffer, B, Line, Style, Text, Name, Column_Id, Info);

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
      Length    : Highlight_Length;

      Mark : constant Editor_Mark'Class := Message.Get_Editor_Mark;
   begin
      Line := Editable_Line_Type (Mark.Line);

      --  Sanity check: verify that Line does contain Message

      if Line /= 0 and then not Message_Is_On_Line (Buffer, Message, Line) then
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

      if Style /= null then
         Length := Message.Get_Highlighting_Length;

         case Length is
            when Highlight_None | Highlight_Whole_Line =>
               Buffer.Highlighter.Remove_Line_Highlighting (Line, Style);
            when others =>
               To_Column := From_Column + Visible_Column_Type (Length);
               if Has_Iters then
                  Buffer.Highlighter.Highlight_Range
                    (Style, Line, Start_Iter, End_Iter, True);
               else
                  Buffer.Highlighter.Highlight_Range
                    (Style, Line, From_Column, To_Column, True);
               end if;
         end case;
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
      Length   : Highlight_Length;
      End_Col  : Visible_Column_Type;
      EL       : Editable_Line_Type := 0; --  The actual editable line
      BL       : Buffer_Line_Type := 0;   --  The actual buffer line

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
         if Buffer_Line /= 0 then
            BL := Buffer_Line;
         else
            Compute_EL;
            if EL in Buffer.Editable_Lines'Range then
               BL := Buffer.Editable_Lines (EL).Buffer_Line;
            end if;
         end if;
      end Compute_BL;

      ----------------
      -- Compute_EL --
      ----------------

      procedure Compute_EL is
      begin
         if Editable_Line /= 0 then
            EL := Editable_Line;
         else
            EL := Editable_Line_Type (Mark.Line);
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

      Compute_BL;

      --  Highlight if needed
      if Style /= null and then BL /= 0 then
         Length := Message.Get_Highlighting_Length;

         Buffer.Highlighter.Set_Line_Highlighting
           (Line         => BL,
            Style        => Style,
            Set          => True,
            Highlight_In =>
              (Highlight_Speedbar => Get_In_Speedbar (Style),
               Highlight_Editor   => Length = Highlight_Whole_Line));
         Line_Highlights_Changed (Buffer);

         if Length /= Highlight_Whole_Line
           and then Length /= Highlight_None
         then
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

            Buffer.Highlighter.Highlight_Range
              (Style     => Style,
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
      Message       : not null Message_Access;
      Editable_Line : Editable_Line_Type;
      Buffer_Line   : Buffer_Line_Type;
      Column        : Integer)
   is
      Note     : Line_Info_Note;
      The_Data : Line_Info_Width_Array_Access;
      BL       : Buffer_Line_Type := 0;
      EL       : Editable_Line_Type := Editable_Line;
   begin
      if Message /= null then
         declare
            Mark : constant Editor_Mark'Class := Message.Get_Editor_Mark;
         begin
            --  If the message has a mark, we get the line information
            --  from this mark.
            if Mark /= Nil_Editor_Mark then
               EL := Editable_Line_Type (Mark.Line);
            end if;
         end;
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
            Note.Style := Message.Get_Highlighting_Style;
            Message.Set_Note (Note_Access (Note));

            The_Data (Column).Messages.Prepend (Create (Message));
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
      Icon_Name     : String) is
   begin
      Add_Side_Information
        (Buffer,
         Default_Column,
         --  ??? optimization: should use the integer value for column here:
         --  buffer.block_highlighting_column
         Line_Information_Array'(Integer (Editable_Line)
           => (Text               => Null_Unbounded_String,
               Tooltip_Text       => Null_Unbounded_String,
               Image              => To_Unbounded_String (Icon_Name),
               Message            => <>,
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
      Get_Iter_At_Screen_Position (Buffer, Iter, Line, Column);
      return Create_Mark (Buffer => Buffer, Where => Iter);
   end Create_Mark;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type)
   is
      Position          : Buffer_Line_Type := Start;
      Buffer_Lines      : Line_Data_Array_Access renames Buffer.Line_Data;
      Editable_Lines    : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      EN                : constant Editable_Line_Type :=
        Editable_Line_Type (Number);
      Bottom_Line       : Buffer_Line_Type;
      Ref_Editable_Line : Editable_Line_Type;

      procedure Expand_Lines
        (N : Buffer_Line_Type);
      --  Expand the line-indexed arrays to contain N lines in size

      ------------------
      -- Expand_Lines --
      ------------------

      procedure Expand_Lines (N : Buffer_Line_Type) is
         H : Line_Data_Array_Access;
         K : Editable_Line_Array_Access;
      begin
         H := new Line_Data_Array (Buffer_Lines'Range);
         H.all := Buffer_Lines.all;

         K := new Editable_Line_Array (Editable_Lines'Range);
         K.all := Editable_Lines.all;

         Unchecked_Free (Buffer_Lines);
         Buffer_Lines := new Line_Data_Array (1 .. N * 2);

         Buffer_Lines (H'Range) := H.all;

         for J in H'Last + 1 .. Buffer_Lines'Last loop
            Buffer_Lines (J) := New_Line_Data;
         end loop;

         Unchecked_Free (Editable_Lines);
         Editable_Lines := new Editable_Line_Array
           (1 .. Editable_Line_Type (N * 2));

         Editable_Lines (K'Range) := K.all;

         for J in K'Last + 1 .. Editable_Lines'Last loop
            Editable_Lines (J) :=
              (Where                 => In_Buffer,
               Buffer_Line           => 0,
               Stored_Lines          => Lines_List.Empty_List,
               Stored_Editable_Lines => 0);
         end loop;

         Unchecked_Free (K);
         Unchecked_Free (H);
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
            Expand_Lines (Number + 1);
         end if;

         for J in 1 .. Number + 1 loop
            Buffer_Lines (Start + J) := New_Line_Data;
            Buffer_Lines (Start + J).Editable_Line :=
              Editable_Line_Type (Start + J);

            if Buffer.Modifying_Editable_Lines then
               Editable_Lines (Ref_Editable_Line + Editable_Line_Type (J)) :=
                 (Where                 => In_Buffer,
                  Buffer_Line           => Start + J,
                  Stored_Lines          => Lines_List.Empty_List,
                  Stored_Editable_Lines => 0);
               Create_Side_Info (Buffer, Start + J);
            end if;
         end loop;

         Buffer.Last_Editable_Line := Buffer.Last_Editable_Line +
           Editable_Line_Type (Number);
         Buffer.Original_Text_Inserted := True;

      else
         if Buffer.Inserting_Position = At_End then
            --  We were at end of line before inserting so we have to move
            --  not start line but next one instead.
            Position := Position + 1;

            Ref_Editable_Line := Ref_Editable_Line + 1;
            for Index in Position .. Buffer_Lines'Last loop
               if Buffer_Lines (Index).Editable_Line /= 0 then
                  Ref_Editable_Line := Buffer_Lines (Index).Editable_Line;
                  exit;
               end if;
            end loop;
         end if;

         --  Figure out whether we need to expand the line arrays

         Bottom_Line := Buffer_Line_Type'Max
           (Buffer_Line_Type (Get_Line_Count (Buffer)),
            Buffer_Line_Type (Buffer.Last_Editable_Line)) + Number;

         if Buffer_Lines'Last < Bottom_Line then
            Expand_Lines (Bottom_Line);
         end if;

         Buffer_Lines (Position + Number .. Buffer_Lines'Last) :=
           Buffer_Lines (Position .. Buffer_Lines'Last - Number);

         if Buffer.Modifying_Editable_Lines then
            for J in Position + Number .. Buffer_Lines'Last loop
               if Buffer_Lines (J).Editable_Line /= 0 then
                  Buffer_Lines (J).Editable_Line :=
                    Buffer_Lines (J).Editable_Line + EN;
               end if;
            end loop;

            Editable_Lines
              (Ref_Editable_Line + EN .. Buffer.Last_Editable_Line + EN) :=
              Editable_Lines
                (Ref_Editable_Line .. Buffer.Last_Editable_Line);

            for Line in
              Ref_Editable_Line + EN .. Buffer.Last_Editable_Line + EN
            loop
               if Editable_Lines (Line).Where = In_Buffer then
                  Editable_Lines (Line).Buffer_Line :=
                    Editable_Lines (Line).Buffer_Line + Number;
               end if;
            end loop;
         end if;

         --  Remove highlighting for the topmost of moved lines

         Bottom_Line := Position + Number;

         for K in Highlight_Location loop
            Buffer_Lines (Bottom_Line).Highlighting (K).Active := 0;

            if Buffer_Lines
              (Bottom_Line).Highlighting (K).Enabled /= null
            then
               for J in Buffer_Lines
                 (Bottom_Line).Highlighting (K).Enabled'Range
               loop
                  Buffer_Lines
                    (Bottom_Line).Highlighting (K).Enabled (J) := False;
               end loop;
            end if;
         end loop;

         --  FIXME: A message is associated to a mark, thus adding lines
         --  can move the marks. If a mark is not moved to Position + Number
         --  then GPS will loose track of its message. It's better to delete
         --  these messages than losing them

         if Buffer.Inserting_Position = Other
           and then Buffer_Lines (Start).Side_Info_Data /= null
         then
            declare
               use Message_Reference_List;

               EL        : constant Editable_Line_Type :=
                 Buffer_Lines (Position + Number).Editable_Line;
               Src       : Line_Info_Width_Array_Access renames
                 Buffer_Lines (Position).Side_Info_Data;
               Msg       : Message_Access;
               To_Delete : Message_Reference_List.List;
            begin
               --  Gather the lost messages in a list ...
               for Index in Src'Range loop
                  for M of Src (Index).Messages loop
                     Msg := Message (M);

                     if Msg /= null
                       and then Editable_Line_Type
                         (Msg.Get_Editor_Mark.Line) /= EL
                     then
                        To_Delete.Append (M);
                     end if;
                  end loop;
               end loop;

               --  ... and clean them
               for M of To_Delete loop
                  Remove (Message (M));
               end loop;
               To_Delete.Clear;
            end;
         end if;

         --  Reset the newly inserted lines

         for J in 0 .. Number - 1 loop
            Buffer_Lines (Position + J) := New_Line_Data;
            Buffer_Lines (Position + J).Editable_Line := Ref_Editable_Line
              + Editable_Line_Type (J);
         end loop;

         if Buffer.Modifying_Editable_Lines then
            for J in 0 .. EN - 1 loop
               Editable_Lines (Ref_Editable_Line + J) :=
                 (Where                 => In_Buffer,
                  Buffer_Line           => Position + Buffer_Line_Type (J),
                  Stored_Lines          => Lines_List.Empty_List,
                  Stored_Editable_Lines => 0);
               Create_Side_Info (Buffer, Position + Buffer_Line_Type (J));
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
      Count      : Buffer_Line_Type)
   is
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      ES : constant Buffer_Line_Type := Start_Line + 1;
      EN : Editable_Line_Type := Editable_Line_Type (Count);
      EL : Editable_Line_Type;
   begin
      if Count < 1 then
         return;
      end if;

      --  If the lines are not real, then the number of editable lines
      --  actually removed is dependent on folded or hidden lines.

      if not Lines_Are_Real (Buffer) then
         EN := 0;
         for J in ES .. Start_Line + Count loop
            if Buffer_Lines (J).Editable_Line /= 0 then
               EN := EN + 1;
            end if;
         end loop;
      end if;

      if Buffer.Modifying_Editable_Lines then
         for J in ES .. Start_Line + Count loop
            if Buffer_Lines (J).Side_Info_Data /= null then
               for Col in Buffer_Lines (J).Side_Info_Data'Range loop
                  Free (Buffer, Buffer_Lines (J).Side_Info_Data (Col), True);
               end loop;
            end if;
         end loop;
      end if;

      EL := Buffer_Lines (ES).Editable_Line;

      Buffer_Lines (ES .. Buffer_Lines'Last - Count) :=
        Buffer_Lines (ES + Count .. Buffer_Lines'Last);

      if Buffer.Modifying_Editable_Lines then
         for J in ES .. Buffer_Lines'Last - Count loop
            if Buffer_Lines (J).Editable_Line /= 0 then
               Buffer_Lines (J).Editable_Line :=
                 Buffer_Lines (J).Editable_Line - EN;
            end if;
         end loop;
      end if;

      if Buffer.Modifying_Editable_Lines then
         --  If we are modifying editable lines, mark the block info as
         --  obsolete.
         Reset_Blocks_Info (Buffer);
      end if;

      if Buffer.Modifying_Editable_Lines then
         declare
            --  This is a trick: we are removing EN lines in the middle of the
            --  array, and moving the soft boundary of the array up EN lines.
            --  Instead of freeing memory here and allocating some at the
            --  bottom of this subprogram, we simply move the allocated
            --  structures down directly.
            Lines_To_Report : Editable_Line_Array_Access;
         begin
            Lines_To_Report     := new Editable_Line_Array (1 .. EN);
            Lines_To_Report.all := Editable_Lines (EL .. EL + EN - 1);

            Editable_Lines (EL .. Buffer.Last_Editable_Line - EN) :=
              Editable_Lines (EL + EN .. Buffer.Last_Editable_Line);

            for J in EL .. Buffer.Last_Editable_Line - EN loop
               if Editable_Lines (J).Where = In_Buffer then
                  Editable_Lines (J).Buffer_Line :=
                    Editable_Lines (J).Buffer_Line - Count;
               end if;
            end loop;

            Editable_Lines
              (Buffer.Last_Editable_Line - EN + 1 ..
                 Buffer.Last_Editable_Line) := Lines_To_Report.all;

            Unchecked_Free (Lines_To_Report);
         end;

         Buffer.Last_Editable_Line := Buffer.Last_Editable_Line - EN;
      end if;

      --  Reset bottom lines
      Buffer_Lines
        (Buffer_Line_Type'Max
           (Start_Line + 1, Buffer_Lines'Last - Count) ..
             Buffer_Lines'Last) := (others => New_Line_Data);

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
      if Mark /= null then
         Get_Iter_At_Mark (Buffer, Iter, Mark);
         Remove_Blank_Lines
           (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1), Number);
      end if;
   end Remove_Blank_Lines;

   ----------------
   -- Hide_Lines --
   ----------------

   procedure Hide_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Number : Editable_Line_Type)
   is
      Command              : Unhide_Editable_Lines_Command;
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Line_Start           : constant Editable_Line_Type :=
                               Get_Editable_Line (Buffer, Line);
      Line_End             : constant Editable_Line_Type :=
                               Line_Start + Number;
      Start_Loc            : constant Editor_Location'Class :=
                               Buffer.Get_Editor_Buffer.New_Location_At_Line
                                 (Line => Integer (Line_Start));
      Cursor_Move          : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Folded_Block_Info    : Folded_Block_Info_Type;
   begin
      Buffer.Modifying_Real_Lines := True;

      --  Hide lines

      Get_Iter_At_Line (Buffer, Start_Iter, Gint (Line_Start));
      Get_Iter_At_Line (Buffer, End_Iter, Gint (Line_End));

      Buffer.Apply_Tag (Buffer.Hidden_Text_Tag, Start_Iter, End_Iter);

      --  Redraw the side column

      Side_Column_Configuration_Changed (Buffer);

      --  Save the block being folded

      Folded_Block_Info.Start_Mark.Replace_Element
        (Start_Loc.Create_Mark (Left_Gravity => False));
      Folded_Block_Info.Nb_Lines := Number;
      Buffer.Folded_Blocks.Append (Folded_Block_Info);

      --  Add an icon to unhide the lines

      Command := new Unhide_Editable_Lines_Type;
      Command.Buffer := Source_Buffer (Buffer);
      Command.Number := Number;

      Add_Block_Command
        (Buffer, Line_Start, Command_Access (Command),
         Unhide_Block_Pixbuf);

      Buffer.Modifying_Real_Lines := False;
      Register_Edit_Timeout (Buffer);

      --  Re-enable moving the cursor, and reemit the cursor position

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);

      --  Emit the "source_lines_folded" hook

      Buffer.Source_Lines_Folded (Line_Start, Line_End);
   end Hide_Lines;

   ------------------
   -- Unhide_Lines --
   ------------------

   procedure Unhide_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Editable_Line_Type;
      Number     : Editable_Line_Type)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Line_Start           : Buffer_Line_Type;
      Line_End             : Buffer_Line_Type;
      Command              : Hide_Editable_Lines_Command;
      Cursor_Move          : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Nested_Folded_Blocks : Folded_Block_Info_Vectors.Vector;

      procedure Refold_Nested_Folded_Blocks;
      --  Refold the nested blocks that were folded

      ---------------------------------
      -- Refold_Nested_Folded_Blocks --
      ---------------------------------

      procedure Refold_Nested_Folded_Blocks is
      begin
         for Nested_Folded_Block of Nested_Folded_Blocks loop
            declare
               Nested_Line_Start : constant Gint := Gint
                 (Nested_Folded_Block.Start_Mark.Element.Line);
               Nested_Lined_End  : constant Gint :=
                                     Nested_Line_Start
                                       + Gint (Nested_Folded_Block.Nb_Lines);
            begin
               Get_Iter_At_Line
                 (Buffer, Start_Iter, Nested_Line_Start);
               Get_Iter_At_Line
                 (Buffer, End_Iter, Nested_Lined_End);

               Buffer.Apply_Tag (Buffer.Hidden_Text_Tag, Start_Iter, End_Iter);
            end;
         end loop;
      end Refold_Nested_Folded_Blocks;

   begin
      Buffer.Modifying_Real_Lines := True;

      Line_Start := Get_Buffer_Line (Buffer, Start_Line);
      Line_End   := Line_Start + Buffer_Line_Type (Number);

      --  Disable emitting new cursor positions while we hide lines

      Buffer.Do_Not_Move_Cursor := True;

      --  Show lines

      Get_Iter_At_Line (Buffer, Start_Iter, Gint (Line_Start));
      Get_Iter_At_Line (Buffer, End_Iter, Gint (Line_End));

      Buffer.Remove_Tag (Buffer.Hidden_Text_Tag, Start_Iter, End_Iter);

      --  Iterate over the list of saved folded blocks to check if we have
      --  nested blocks that are folded: we'll need to refold them after.
      --  Also, delete the block that is currently being unfolded from this
      --  list.

      declare
         Idx                        : Positive :=
                                        Buffer.Folded_Blocks.First_Index;
         Folded_Block_To_Delete_Idx : Positive;
         Block_Start_Line           : Natural;
      begin
         for Folded_Block of Buffer.Folded_Blocks loop
            Block_Start_Line := Folded_Block.Start_Mark.Element.Line;

            if Block_Start_Line = Natural (Line_Start) then
               Folded_Block_To_Delete_Idx := Idx;
            elsif Block_Start_Line in
              Integer (Line_Start) + 1 .. Integer (Line_End)
            then
               Nested_Folded_Blocks.Append (Folded_Block);
            end if;

            Idx := Idx + 1;
         end loop;

         Buffer.Folded_Blocks.Delete (Folded_Block_To_Delete_Idx);
      end;

      --  Refold the nested blocks

      Refold_Nested_Folded_Blocks;

      --  Redraw the side column

      Side_Column_Configuration_Changed (Buffer);

      --  Add a command to hide the lines

      Command := new Hide_Editable_Lines_Type;
      Command.Buffer := Source_Buffer (Buffer);
      Command.Number := Number;

      Add_Block_Command
        (Buffer, Start_Line,
         Command_Access (Command),
         Hide_Block_Pixbuf);
      Buffer.Modifying_Real_Lines := False;
      Register_Edit_Timeout (Buffer);

      --  Re-enable moving the cursor, and reemit the cursor position

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);

      --  Emit the "source_lines_unfolded" hook

      Buffer.Source_Lines_Unfolded
        (Start_Line,
         Start_Line + Number);
   end Unhide_Lines;

   --------------
   -- Fold_All --
   --------------

   procedure Fold_All
     (Buffer  : access Source_Buffer_Record'Class;
      Similar : Boolean := False)
   is
      Command      : Command_Access;
      Ignore       : Command_Return_Type;
      pragma Unreferenced (Ignore);

      Cursor_Move : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Line        : Editable_Line_Type;
      Category    : constant Language_Category :=
        Buffer.Get_Current_Block.Block_Type;

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
               Block : constant Block_Record := Buffer.Get_Block (Line, False);
            begin
               if (not Similar or else Block.Block_Type = Category)
                 and then Buffer.Line_Data (BL).Side_Info_Data /= null
                 and then Buffer.Line_Data (BL).Side_Info_Data
                 (Buffer.Block_Highlighting_Column).Action /= null
               then
                  Command := Buffer.Line_Data (BL).Side_Info_Data
                    (Buffer.Block_Highlighting_Column)
                    .Action.Associated_Command;

                  if Command /= null
                    and then Command.all in
                      Hide_Editable_Lines_Type'Class
                  then
                     --  When folding all the similar blocks also fold the
                     --  first block of the matching type
                     if Similar or else First_Line_Found then
                        Base_Editor_Command (Command).Base_Line :=
                          Buffer.Editable_Lines (Line).Buffer_Line;

                        Line := Line +
                          Hide_Editable_Lines_Type
                            (Command.all).Number - 1;

                        Ignore := Execute (Command);

                     else
                        First_Line_Found := True;
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

   procedure Unfold_All
     (Buffer  : access Source_Buffer_Record'Class;
      Similar : Boolean := False)
   is
      Result       : Command_Return_Type;
      pragma Unreferenced (Result);

      Cursor_Move        : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Line               : Editable_Line_Type;
      Folded_Blocks_Copy : Folded_Block_Info_Vectors.Vector :=
        Buffer.Folded_Blocks.Copy;
      Category           : constant Language_Category :=
        Buffer.Get_Current_Block.Block_Type;
   begin
      if Buffer.Block_Highlighting_Column = -1
        or else Buffer.Folded_Blocks.Is_Empty
      then
         return;
      end if;

      Buffer.Do_Not_Move_Cursor := True;

      --  We use a copy here to avoid tampering checks, since unfolding a block
      --  removes it from the buffer's folded blocks' vector.

      for Folded_Block of Folded_Blocks_Copy loop
         Line := Editable_Line_Type (Folded_Block.Start_Mark.Element.Line);
         if not Similar
           or else Buffer.Get_Block (Line, False).Block_Type = Category
         then
            Unhide_Lines (Buffer, Line, Folded_Block.Nb_Lines);
         end if;
      end loop;

      Folded_Blocks_Copy.Clear;

      Buffer.Do_Not_Move_Cursor := Cursor_Move;
      Emit_New_Cursor_Position (Buffer);

      --  Scroll back to the cursor's position after unfolding all blocks

      declare
         View : constant Editor_View'Class :=
                  Buffer.Get_Editor_Buffer.Current_View;
      begin
         View.Cursor_Goto
           (Location         => View.Cursor,
            Centering        => With_Margin,
            Extend_Selection => False);
      end;
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

      --  Try to find the line where the block begins by iterating the lines in
      --  a reverse order and execute the associated fold/unfold command.

      for L in reverse Editable_Lines'First .. Line loop
         if Is_Line_Visible (Buffer, L) then
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
      Dummy : Boolean;
   begin
      if not Is_Line_Visible (Buffer, Line) then
         Dummy := Fold_Unfold_Line (Buffer, Line, Fold => False);
      end if;
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
      if not Blocks_Are_Exact (Buffer) then
         Compute_Blocks (Buffer, Immediate => True);
      end if;

      if Is_Line_Visible (Buffer, Line) then
         Ignore := Fold_Unfold_Line (Buffer, Line, Fold => True);
      end if;
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
      return Buffer.Blank_Lines = 0 and then Buffer.Folded_Blocks.Is_Empty;
   end Lines_Are_Real;

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
      Returned : Command_Return_Type;
      Result   : Boolean := False;
      pragma Unreferenced (Returned);
   begin
      --  There is nothing to do if the lines are real

      if Lines_Are_Real (Buffer) then
         return False;
      end if;

      --  Unfold all the lines

      for Line in Start_Line .. End_Line loop
         Result := Result or Fold_Unfold_Line (Buffer, Line, Fold => False);
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

      Editable_Line_Start := Get_Editable_Line (Buffer, Line_Start);
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
      for J in Editable_Line_Start .. Editable_Line_End  loop
         if not Is_Line_Visible (Buffer, J) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Special_Lines;

   -----------
   -- Image --
   -----------

   function Image (Data : Line_Info_Width_Array_Access) return String is
      Result : Unbounded_String;
   begin
      if Data = null then
         return "    null" & ASCII.LF;
      else
         for Index in Data'Range loop
            Append
              (Result,
               "    #" & Index'Img & ": "  & ASCII.LF & Image (Data (Index)));
         end loop;

         return To_String (Result);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Line_Info_Width) return String is
      Result : Unbounded_String;
      Cnt    : Natural := 0;
   begin
      if Data.Messages.Is_Empty then
         Append (Result, "      Messages: empty" & ASCII.LF);
      else
         for M of Data.Messages loop
            if M.Message = null then
               Cnt := Cnt + 1;
            else
               if Cnt > 0 then
                  Append (Result, "      Message: null");

                  if Cnt = 1 then
                     Append (Result, ASCII.LF);
                  else
                     Append (Result, " <" & Cnt'Img & '>' & ASCII.LF);
                  end if;
               end if;
               Cnt := 0;

               Append (Result, "      Message:" & Image (M) & ASCII.LF);
            end if;
         end loop;

         if Cnt /= 0 then
            Append
              (Result, "      Message: null <" & Cnt'Img & '>' & ASCII.LF);
         end if;
      end if;

      return To_String (Result) &
        "      Action:" & Image (Data.Action) &
        "      Set:" & Data.Set'Img & ASCII.LF;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Gtk_Text_Mark) return String is
   begin
      if Data = null then
         return " null" & ASCII.LF;
      else
         return " Name:" & Data.Get_Name &
           " Visible:" & Data.Get_Visible'Img & ASCII.LF;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Highlighting_Data_Array) return String is
      Result : Unbounded_String;
   begin
      if Data'Length = 0 then
         return "      empty";
      end if;

      for Index in Data'Range loop
         Append (Result, "   " & Index'Img & ":" & ASCII.LF);
         Append (Result, "      Enabled:" & Image (Data (Index).Enabled));
         Append (Result, "      Active:"
                 & Data (Index).Active'Img & ASCII.LF);
      end loop;

      return ASCII.LF & To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Message_Reference) return String is
   begin
      if Data.Message = null then
         return "null";
      else
         return To_String (Data.Message.Get_Category) &
           ":" & To_String (Data.Message.Get_Text);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Line_Information_Access) return String is
      Result : Unbounded_String;
   begin
      if Data = null then
         return "null" & ASCII.LF;
      end if;

      Append (Result, ASCII.LF);
      Append (Result, "        Text:" & Data.Text & ASCII.LF);
      Append (Result, "        Tooltip_Text:" &
                Data.Tooltip_Text & ASCII.LF);
      Append (Result, "        Image:" & Data.Image & ASCII.LF);
      Append (Result, "        Message:" & Image (Data.Message) & ASCII.LF);
      Append (Result, "        Associated_Command:" &
                Image (Data.Associated_Command));

      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Boolean_Array_Access) return String is
      Result : Unbounded_String;
      Prev   : Boolean;
      Cnt    : Natural := 0;
   begin
      if Data = null then
         return " null" & ASCII.LF;
      end if;

      if Data'Length = 0 then
         return " empty" & ASCII.LF;
      end if;

      Append (Result, ASCII.LF);
      Prev := Data (Data'First);

      for Index in Data'Range loop
         if Data (Index) = Prev then
            Cnt := Cnt + 1;

         else
            Append
              (Result, "        " & Prev'Img &
               (if Cnt > 1 then '<' & Cnt'Img & '>' else "") &
                 ASCII.LF);

            Prev := Data (Index);
            Cnt  := 1;
         end if;
      end loop;
      Append
        (Result, "        " & Prev'Img &
         (if Cnt > 1 then '<' & Cnt'Img & '>' else "") & ASCII.LF);

      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Commands.Command_Access) return String is
   begin
      if Data = null then
         return " null" & ASCII.LF;
      else
         return ASCII.LF & "          Tag:" &
           Ada.Tags.External_Tag (Data'Tag) & ASCII.LF
           & "          Name:" & Data.Name & ASCII.LF;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Lines_List.List) return String is
      Result : Unbounded_String;
   begin
      if Data.Is_Empty then
         return " empty" & ASCII.LF;
      end if;

      for Item of Data loop
         Append (Result, Image (Item));
      end loop;

      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Universal_Line_Access) return String is
   begin
      if Data = null then
         return " null" & ASCII.LF;
      else
         return Image (Data.all);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Data : Universal_Line) return String is
   begin
      return ASCII.LF & "    Nature:" & Data.Nature'Img & ASCII.LF &
        "    Data:" & ASCII.LF &
        "      Editable_Line:" & Data.Data.Editable_Line'Img & ASCII.LF &
        "    Text:" &
      (if Data.Text = null
       then " null"
       else Data.Text.all) & ASCII.LF &
        "    Line_Mark:" & Image (Data.Line_Mark);
   end Image;

end Src_Editor_Buffer.Line_Information;
