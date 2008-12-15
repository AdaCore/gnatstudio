-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with GNAT.OS_Lib;

with Gdk;                      use Gdk;
with Gdk.Color;                use Gdk.Color;
with Gdk.Drawable;             use Gdk.Drawable;
with Gdk.Pixmap;               use Gdk.Pixmap;

with Glib.Object;              use Glib.Object;

with Gtk;                      use Gtk;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;       use Gtk.Text_Tag_Table;
with Pango.Enums;              use Pango.Enums;

with Commands.Editor;          use Commands.Editor;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel;               use GPS.Kernel;
with Src_Editor_Buffer.Blocks; use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer;        use Src_Editor_Buffer;
with Src_Editor_Module;        use Src_Editor_Module;
with Traces;                   use Traces;

with Language.Ada;             use Language.Ada;

package body Src_Editor_Buffer.Line_Information is

   Me : constant Debug_Handle := Create ("Src_Editor_Buffer.Line_Information");

   Block_Info_Column : constant String := "Block Information";
   --  Identifier for the block information column

   procedure Free (Info : in out Line_Information_Access);
   --  Free memory associated with Info

   procedure Remove_Line_Information_Column
     (Buffer : access Source_Buffer_Record'Class;
      Column : Integer);
   --  Remove the column from the side window information in Buffer

   procedure Remove_Blank_Lines
     (Buffer                : access Source_Buffer_Record'Class;
      Buffer_Line_At_Blanks : Buffer_Line_Type;
      Number                : Natural);
   --  Remove Number blank lines at Buffer_Line_At_Blanks. If Number = 0,
   --  remove all continuous blank lines at Buffer_Lines_At_Blanks.

   procedure Get_Column_For_Identifier
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Width      : Integer;
      Every_Line : Boolean);
   --  Return the index of the column corresponding to the identifier.
   --  Create such a column if necessary.

   function Fold_Unfold_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      Fold   : Boolean) return Boolean;
   --  Fold or unfold the block containing Line.
   --  Return True when an operation was executed, False otherwise.

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
      --  ??? How costly is this ?

      for J in Buffer.Editable_Lines'Range loop
         if Buffer.Editable_Lines (J).Side_Info_Data = null then
            --  ??? Code duplication with Add_Lines
            Buffer.Editable_Lines (J).Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config.all'Range);

            for K in Columns_Config.all'Range loop
               Buffer.Editable_Lines (J).Side_Info_Data (K) :=
                 (null,
                  Width => -1,
                  Set   => not Columns_Config.all (K).Every_Line);
            end loop;

         else
            declare
               A : Line_Info_Width_Array
                 (Buffer.Editable_Lines (J).Side_Info_Data'First
                  .. Buffer.Editable_Lines (J).Side_Info_Data'Last + 1);
            begin
               A (A'First .. A'Last - 1) :=
                 Buffer.Editable_Lines (J).Side_Info_Data.all;

               Unchecked_Free (Buffer.Editable_Lines (J).Side_Info_Data);
               Buffer.Editable_Lines (J).Side_Info_Data :=
                 new Line_Info_Width_Array'(A);

               Buffer.Editable_Lines (J).Side_Info_Data
                 (Buffer.Editable_Lines (J).Side_Info_Data'Last) :=
                 (Info  => null,
                  Width => Width,
                  Set   => not Every_Line);
            end;
         end if;
      end loop;
   end Get_Column_For_Identifier;

   --------------------------
   -- Get_Side_Information --
   --------------------------

   function Get_Side_Information
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type)
      return Line_Info_Width_Array_Access is
   begin
      return Buffer.Editable_Lines (Line).Side_Info_Data;
   end Get_Side_Information;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Every_Line : Boolean) is
   begin
      Get_Column_For_Identifier (Buffer, Identifier, -1, Every_Line);
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
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Width : Integer;
   begin
      Columns_Config := Buffer.Editable_Line_Info_Columns;

      Width := Columns_Config.all (Column).Width;

      --  Free the column for all data

      for J in Editable_Lines'Range loop
         if Editable_Lines (J).Side_Info_Data /= null then

            if Editable_Lines (J).Side_Info_Data'First
              /= Columns_Config.all'First
              or else  Editable_Lines (J).Side_Info_Data'Last
              /= Columns_Config.all'Last
            then
               Trace (Me, "Inconsistent line data");
               return;
            end if;

            declare
               A : Line_Info_Width_Array
                 (Columns_Config.all'First .. Columns_Config.all'Last - 1);
            begin
               A (Columns_Config.all'First .. Column - 1) :=
                 Editable_Lines (J).Side_Info_Data
                 (Columns_Config.all'First .. Column - 1);
               A (Column .. Columns_Config.all'Last - 1) :=
                 Editable_Lines (J).Side_Info_Data
                 (Column + 1 .. Columns_Config.all'Last);
               Unchecked_Free (Editable_Lines (J).Side_Info_Data);

               Editable_Lines (J).Side_Info_Data :=
                 new Line_Info_Width_Array'(A);
            end;
         end if;
      end loop;

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

   ----------
   -- Free --
   ----------

   procedure Free (Info : in out Line_Information_Access) is
   begin
      GNAT.Strings.Free (Info.Text);
      GNAT.Strings.Free (Info.Tooltip_Text);
      Unchecked_Free (Info);
   end Free;

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
      Box        : Gtk_Widget;
      Info       : Standard_Hooks.Line_Information_Data)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      Column : Integer := -1;
      Num    : Gint := 1;
      Height : Gint;
      Width  : Gint := -1;
      Widths : array (Info'Range) of Gint;
      Layout : Pango_Layout;
      Found  : Boolean := False;

      Editable_Line : Editable_Line_Type;

      Columns_Config : Columns_Config_Access;

      use GNAT.OS_Lib;
   begin
      --  Test if we are adding extra information, or line information

      if Info'First < 0 then
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
         return;
      end if;

      --  If we reach this point, the info corresponds to line information

      Layout := Create_Pango_Layout (Box);
      Set_Font_Description (Layout, Default_Style.Get_Pref_Font);

      --  Compute the maximum width of the items to add.
      --  We compute this width once and for all and in advance,
      --  because is is quite expensive, and we don't want to do it
      --  in Src_Editor_View.Redraw_Columns, since that function is
      --  called a great number of times.

      for J in Info'Range loop
         Widths (J) := -1;

         if Info (J).Text /= null then
            Set_Markup (Layout, String'(Info (J).Text.all));
            Get_Pixel_Size (Layout, Num, Height);

            if Num = 0 then
               Num := 1;
            end if;

            Widths (J) := Num;

            if Num > Width then
               Width := Num;
            end if;
         end if;

         if Info (J).Image /= Null_Pixbuf then
            Num := Get_Width (Info (J).Image);

            Widths (J) := Num;

            if Num > Width then
               Width := Num;
            end if;
         end if;
      end loop;

      --  Get the column that corresponds to Identifier

      Columns_Config := Buffer.Editable_Line_Info_Columns;

      if Columns_Config /= null and then Columns_Config.all /= null then
         for J in Columns_Config.all'Range loop
            if Columns_Config.all (J).Identifier.all = Identifier then
               Column := J;
               exit;
            end if;
         end loop;
      end if;

      --  The column has not been found: exit

      if Column = -1 then
         return;
      end if;

      --  The column has been found: update the stored data

      for K in Info'Range loop
         Editable_Line := Editable_Line_Type (K);

         if Editable_Line /= 0 then
            Editable_Lines (Editable_Line).Side_Info_Data (Column).Info :=
              new Line_Information_Record'(Info (K));
            Editable_Lines (Editable_Line).Side_Info_Data (Column).Width :=
              Integer (Widths (K));
            Editable_Lines (Editable_Line).Side_Info_Data (Column).Set := True;
         end if;
      end loop;

      --  If the line info width is bigger than the column width, resize the
      --  column and all columns.

      if Integer (Width) > Columns_Config.all (Column).Width then
         Columns_Config.all (Column).Width := Integer (Width);

         for J in Column + 1 .. Columns_Config.all'Last loop
            Columns_Config.all (J).Starting_X :=
              Columns_Config.all (J - 1).Starting_X
              + Columns_Config.all (J - 1).Width + 1;
         end loop;

         Recalculate_Side_Column_Width (Buffer);
      end if;

      Side_Column_Changed (Buffer);
      Unref (Layout);
   end Add_File_Information;

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
      GC          : Gdk.GC.Gdk_GC;
      Layout      : Pango_Layout;
      Drawable    : Gdk.Pixmap.Gdk_Pixmap)
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
         Set_Markup (Layout, Editable_Line'Img);
         Get_Pixel_Size (Layout, Width, Height);

         Draw_Layout
           (Drawable => Drawable,
            GC       => GC,
            X        => Gint (Buffer.Line_Numbers_Width) - Width,
            Y        => Y_Pix_In_Window,
            Layout   => Layout);
      end Draw_Line_Info;

      ---------------
      -- Draw_Info --
      ---------------

      procedure Draw_Info (Starting_X : Gint) is
         use GNAT.OS_Lib;
      begin
         if Line_Info.Info /= null then
            if Line_Info.Info.Text /= null then
               Set_Markup (Layout, Line_Info.Info.Text.all);

               Draw_Layout
                 (Drawable => Drawable,
                  GC       => GC,
                  X        => Starting_X,
                  Y        => Y_Pix_In_Window,
                  Layout   => Layout);
            end if;

            if Line_Info.Info.Image /= Null_Pixbuf then
               Render_To_Drawable
                 (Pixbuf   => Line_Info.Info.Image,
                  Drawable => Drawable,
                  GC       => GC,
                  Src_X    => 0,
                  Src_Y    => 0,
                  Dest_X   => Starting_X,
                  Dest_Y   => Y_Pix_In_Window + (Line_Height -
                    Get_Height (Line_Info.Info.Image)) / 2,
                  Width    => -1,
                  Height   => -1);
            end if;
         end if;
      end Draw_Info;

   begin
      Current_Line := Top_Line;
      Get_Iter_At_Line (Buffer, Iter, Gint (Current_Line - 1));

      Drawing_Loop :
      while Current_Line <= Bottom_Line loop

         Get_Line_Yrange (View, Iter, Y_In_Buffer, Line_Height);

         --  Convert the buffer coords back to window coords

         Buffer_To_Window_Coords
           (View, Text_Window_Left,
            Buffer_X => 0, Buffer_Y => Y_In_Buffer,
            Window_X => Dummy_Gint, Window_Y => Y_Pix_In_Window);

         Editable_Line := Get_Editable_Line (Buffer, Current_Line);

         if Editable_Line /= 0 then
            if BL.all /= null
              and then Buffer.Editable_Lines
                (Editable_Line).Side_Info_Data /= null
            then
               for Col in BL.all'Range loop
                  Line_Info :=
                    Buffer.Editable_Lines
                      (Editable_Line).Side_Info_Data (Col);

                  Draw_Info
                    (Gint (Buffer.Line_Numbers_Width
                           + BL.all (Col).Width - Line_Info.Width
                           + BL.all (Col).Starting_X));
               end loop;
            end if;

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
   end Draw_Line_Info;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Offset : Gint)
   is
      BL     : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;
      EL     : constant Editable_Line_Type := Get_Editable_Line (Buffer, Line);
      Result : Command_Return_Type;
      pragma Unreferenced (Result);

   begin
      if EL = 0 then
         return;
      end if;

      Set_Cursor_Position (Buffer, Gint (Line - 1), 0, Minimal, False);

      if BL.all /= null then
         for Col in BL.all'Range loop
            if Offset < Gint
              (BL.all (Col).Width + BL.all (Col).Starting_X +
                 Buffer.Line_Numbers_Width)
            then
               if Buffer.Editable_Lines (EL).Side_Info_Data (Col).Info /= null
                 and then Buffer.Editable_Lines (EL).Side_Info_Data
                 (Col).Info.Associated_Command /= null
               then
                  Result := Execute
                    (Buffer.Editable_Lines
                       (EL).Side_Info_Data (Col).Info.Associated_Command);
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
      Line               : Editable_Line_Type;
      Highlight_Category : Integer;
      Text               : String;
      Number             : Positive;
      Name               : String) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      LFs         : String (1 .. Natural (Number));
      Buffer_Line : Buffer_Line_Type;
      Iter        : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Success     : Boolean;
      Mark        : Gtk.Text_Mark.Gtk_Text_Mark;
   begin
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Buffer_Line = 0 then
         return null;
      end if;

      Buffer.Modifying_Real_Lines := True;
      End_Action (Buffer);

      LFs := (others => ASCII.LF);
      Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line - 1));

      Buffer.Modifying_Editable_Lines := False;
      Buffer.Inserting := True;
      Insert (Buffer, Iter, LFs);
      Buffer.Inserting := False;
      Buffer.Modifying_Editable_Lines := True;

      Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line - 1));
      Backward_Char (Iter, Success);
      Get_Iter_At_Line (Buffer, End_Iter,
                        Gint (Buffer_Line - 1) + Gint (Number));

      Apply_Tag (Buffer, Buffer.Non_Editable_Tag, Iter, End_Iter);

      --  Shift down editable lines

      for J in Line .. Buffer.Editable_Lines'Last loop
         if Buffer.Editable_Lines (J).Where = In_Buffer then
            Buffer.Editable_Lines (J).Buffer_Line :=
              Buffer.Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Number);
         end if;
      end loop;

      --  Reset information for newly inserted buffer lines

      for J in Buffer_Line .. Buffer_Line + Buffer_Line_Type (Number) - 1 loop
         Buffer.Line_Data (J).Editable_Line := 0;
         Buffer.Line_Data (J).Highlight_Category := Highlight_Category;
         Buffer.Line_Data (J).Highlight_In := (True, True);
      end loop;

      Get_Iter_At_Line_Offset (Buffer, Iter, Gint (Buffer_Line - 1), 0);

      Side_Column_Changed (Buffer);

      Buffer.Blank_Lines := Buffer.Blank_Lines + Number;

      Mark := Create_Mark (Buffer, Name, Iter);

      Buffer.Inserting := True;
      Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line - 1));
      Insert (Buffer, Iter, Text);
      Buffer.Inserting := False;

      Buffer.Modifying_Real_Lines := False;

      return Mark;
   end Add_Blank_Lines;

   -----------------------
   -- Add_Block_Command --
   -----------------------

   procedure Add_Block_Command
     (Buffer        : access Source_Buffer_Record'Class;
      Editable_Line : Editable_Line_Type;
      Command       : Command_Access;
      Image         : Gdk_Pixbuf)
   is
      Width : Integer;
      BL    : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;
   begin
      --  Create the line information column.
      --  ??? This should not occur every time.

      if Buffer.Block_Highlighting_Column = -1 then
         Create_Line_Information_Column (Buffer, Block_Info_Column, False);
         Buffer.Block_Highlighting_Column := BL.all'Last;
      end if;

      if Image = null then
         Width := 0;
      else
         Width := Integer (Get_Width (Image));
      end if;

      if Buffer.Editable_Lines (Editable_Line).Side_Info_Data /= null then
         Free (Buffer.Editable_Lines (Editable_Line).Side_Info_Data
                 (Buffer.Block_Highlighting_Column));
         Buffer.Editable_Lines (Editable_Line).Side_Info_Data
           (Buffer.Block_Highlighting_Column) :=
           (Info => new Line_Information_Record'
              (Text               => null,
               Tooltip_Text       => null,
               Image              => Image,
               Associated_Command => Command),
            Width => Width,
            Set   => True);
      end if;

      if Command /= null then
         BL.all (Buffer.Block_Highlighting_Column).Width := Width;

         for J in Buffer.Block_Highlighting_Column + 1 .. BL.all'Last loop
            BL.all (J).Starting_X :=
              BL.all (J - 1).Starting_X
              + BL.all (J - 1).Width + 1;
         end loop;

         Recalculate_Side_Column_Width (Buffer);
         Side_Column_Configuration_Changed (Buffer);
      end if;
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
         R : Buffer_Line_Type;
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

         --  ??? Should we save R somewhere as last editable buffer line ?
         R := 0;

         for J in reverse K'Range loop
            if K (J).Where = In_Buffer then
               R := K (J).Buffer_Line;
               exit;
            end if;
         end loop;

         Buffer.Editable_Lines (K'Range) := K;

         for J in K'Last + 1 .. Buffer.Editable_Lines'Last loop
            Buffer.Editable_Lines (J) :=
              (Where       => In_Buffer,
               Buffer_Line => R + Buffer_Line_Type (J - K'Last),
               Side_Info_Data => null);
            Create_Side_Info (Buffer, J);
         end loop;
      end Expand_Lines;

   begin
      if Number <= 0 then
         return;
      end if;

      --  ??? What if inserting in non editable area ?
      Ref_Editable_Line := Buffer_Lines (Start).Editable_Line;

      if not Buffer.Original_Text_Inserted then
         Buffer.Original_Lines_Number := Number;

         if Buffer.Original_Lines_Number >= Buffer_Lines'Last then
            Expand_Lines (Number);
         end if;

         for J in 0 .. Number loop
            Buffer_Lines (Start + J) := New_Line_Data;
            Buffer_Lines (Start + J).Editable_Line := Editable_Line_Type
              (Start + J);
            Buffer_Lines (Start + J).File_Line := File_Line_Type (Start + J);

            if Buffer.Modifying_Editable_Lines then
               Buffer.Editable_Lines (Ref_Editable_Line
                                        + Editable_Line_Type (J)) :=
                 (Where        => In_Buffer,
                  Buffer_Line  => Start + J,
                  Side_Info_Data => null);
               Create_Side_Info
                 (Buffer, Ref_Editable_Line + Editable_Line_Type (J));
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
                  Side_Info_Data => null);
               Create_Side_Info (Buffer, Ref_Editable_Line + J);
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
         EN := Buffer_Lines (End_Line).Editable_Line -
           Buffer_Lines (Start_Line).Editable_Line;
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

            for J in Buffer.Last_Editable_Line - EN + 1
              .. Buffer.Last_Editable_Line
            loop
               Editable_Lines (J).Side_Info_Data := null;
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
      Number                : Natural)
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
      Buffer.Inserting := True;
      Delete (Buffer, Iter, End_Iter);
      Buffer.Inserting := False;
      Buffer.Modifying_Editable_Lines := True;

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
     (Buffer     : access Source_Buffer_Record'Class;
      Mark       : Gtk.Text_Mark.Gtk_Text_Mark;
      Number     : Editable_Line_Type)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Result               : Boolean;

      Buffer_Line : Buffer_Line_Type;
      Line_Start  : Editable_Line_Type;
      Line_End    : Editable_Line_Type;
      Iter        : Gtk_Text_Iter;

      Command    : Unhide_Editable_Lines_Command;
      Number_Of_Lines_Folded : Natural := 0;

      Cursor_Move    : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Blocks_Timeout : constant Boolean := Buffer.Blocks_Timeout_Registered;
   begin
      Buffer.Modifying_Real_Lines := True;

      Get_Iter_At_Mark (Buffer, Iter, Mark);
      Buffer_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

      Line_Start := Get_Editable_Line (Buffer, Buffer_Line);
      Line_End := Line_Start + Number;

      --  If there is no ASCII.LF at the end of the last line, add one since
      --  otherwise moving the cursor to the end of the buffer crashes GPS.

      Get_Iter_At_Line
       (Buffer, Start_Iter, Gint (Get_Buffer_Line (Buffer, Line_End) - 1));

      if not Ends_Line (Start_Iter) then
         Forward_To_Line_End (Start_Iter, Result);
      end if;

      if Is_End (Start_Iter) and then Get_Char (Iter) /= ASCII.LF then
         Insert (Buffer, Start_Iter, "" & ASCII.LF);
      end if;

      --  Disable emitting new cursor positions while we hide lines

      Buffer.Do_Not_Move_Cursor := True;

      --  Remove all blank lines in the block that is to be folded

      if Buffer.Blank_Lines /= 0 then
         Result := Flatten_Area (Buffer, Line_Start, Line_End);
      end if;

      --  Remove line by line in order to avoid problems when removing lines
      --  that are already removed.

      for L in Line_Start .. Line_End loop
         if L not in Editable_Lines'Range then
            Buffer.Modifying_Real_Lines := False;
            return;
         end if;

         if Editable_Lines (L).Where = In_Buffer then
            --  Get the line of text and store it in the pointer

            Get_Iter_At_Line
              (Buffer,
               Start_Iter,
               Gint (Buffer_Line - 1));

            Copy (Start_Iter, End_Iter);

            if not Ends_Line (End_Iter) then
               Forward_To_Line_End (End_Iter, Result);
            end if;

            declare
               Line_Data : Editable_Line_Data :=
                 (Where          => In_Mark,
                  Side_Info_Data => Editable_Lines (L).Side_Info_Data,
                  Text           => null);
            begin
               Line_Data.Text := new String'
                 (Get_Text (Buffer, Start_Iter, End_Iter, True));

               Editable_Lines (L) := Line_Data;
            end;

            --  Remove the line from the screen

            Forward_Char (End_Iter, Result);

            Buffer.Modifying_Editable_Lines := False;
            Buffer.Inserting := True;
            Buffer.Blocks_Timeout_Registered := True;
            Delete (Buffer, Start_Iter, End_Iter);
            Buffer.Blocks_Timeout_Registered := Blocks_Timeout;
            Buffer.Inserting := False;
            Buffer.Modifying_Editable_Lines := True;

            Number_Of_Lines_Folded := Number_Of_Lines_Folded + 1;
         end if;
      end loop;

      Buffer.Hidden_Lines := Buffer.Hidden_Lines + Number_Of_Lines_Folded;

      --  Shift up editable lines

      for J in Line_End + 1 .. Buffer.Last_Editable_Line loop
         if Editable_Lines (J).Where = In_Buffer then
            Editable_Lines (J).Buffer_Line := Editable_Lines (J).Buffer_Line
              - Buffer_Line_Type (Number_Of_Lines_Folded);
         end if;
      end loop;

      --  Add an icon to unhide the lines

      Command := new Unhide_Editable_Lines_Type;
      Command.Buffer := Source_Buffer (Buffer);
      Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line - 1));
      Command.Mark := Create_Mark (Buffer, "", Iter);
      Command.First_Line := Line_Start;
      Command.Last_Line  := Line_End;

      Add_Block_Command
        (Buffer, Line_Start - 1, Command_Access (Command),
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
      Mark       : Gtk.Text_Mark.Gtk_Text_Mark)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Iter           : Gtk_Text_Iter;
      Buffer_Line    : Buffer_Line_Type;

      First_Line : Editable_Line_Type;
      Last_Line  : Editable_Line_Type;

      Start_Iter, End_Iter : Gtk_Text_Iter;

      Number_Of_Lines_Unfolded : Natural := 0;

      Command        : Hide_Editable_Lines_Command;
      Cursor_Move    : constant Boolean := Buffer.Do_Not_Move_Cursor;
      Blocks_Timeout : constant Boolean := Buffer.Blocks_Timeout_Registered;
   begin
      Buffer.Modifying_Real_Lines := True;
      Get_Iter_At_Mark (Buffer, Iter, Mark);
      Buffer_Line := Buffer_Line_Type (Get_Line (Iter));

      First_Line := Get_Editable_Line (Buffer, Buffer_Line);
      Last_Line  := Get_Editable_Line (Buffer, Buffer_Line + 1);

      --  Disable emitting new cursor positions while we hide lines

      Buffer.Do_Not_Move_Cursor := True;

      for Line in reverse First_Line .. Last_Line loop
         --  If the line is already in the buffer, skip

         if Editable_Lines (Line).Where /= In_Buffer then
            --  Insert the line

            Get_Iter_At_Mark (Buffer, Iter, Mark);

            Buffer_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

            Buffer.Modifying_Editable_Lines := False;
            Buffer.Inserting := True;
            Buffer.Blocks_Timeout_Registered := True;

            Insert (Buffer, Iter, Editable_Lines (Line).Text.all & ASCII.LF);

            Buffer.Blocks_Timeout_Registered := Blocks_Timeout;
            Buffer.Inserting := False;
            Buffer.Modifying_Editable_Lines := True;

            --  Modify editable line structure

            declare
               Line_Data : constant Editable_Line_Data :=
                 (Where          => In_Buffer,
                  Side_Info_Data => Editable_Lines (Line).Side_Info_Data,
                  Buffer_Line    => Buffer_Line_Type (Get_Line (Iter) + 1));
            begin
               GNAT.Strings.Free (Editable_Lines (Line).Text);
               Editable_Lines (Line) := Line_Data;
            end;

            --  Set the editable line information
            Buffer.Line_Data (Buffer_Line).Editable_Line := Line;

            Number_Of_Lines_Unfolded := Number_Of_Lines_Unfolded + 1;
         end if;
      end loop;

      Buffer.Hidden_Lines := Buffer.Hidden_Lines - Number_Of_Lines_Unfolded;

      for Line in First_Line + 1 .. Last_Line - 1 loop
         if Editable_Lines (Line).Where = In_Buffer then
            Editable_Lines (Line).Buffer_Line :=
              Editable_Lines (First_Line).Buffer_Line
              + Buffer_Line_Type (Line - First_Line);
         end if;
      end loop;

      for J in Last_Line .. Editable_Lines'Last loop
         if Editable_Lines (J).Where = In_Buffer then
            Editable_Lines (J).Buffer_Line := Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Number_Of_Lines_Unfolded);
         end if;
      end loop;

      --  Highlight the inserted text

      Get_Iter_At_Line
        (Buffer, Start_Iter, Gint (Get_Buffer_Line (Buffer, First_Line) - 1));
      Get_Iter_At_Line
        (Buffer, End_Iter, Gint (Get_Buffer_Line (Buffer, Last_Line) - 1));

      Highlight_Slice (Buffer, Start_Iter, End_Iter);

      --  Redraw the side column

      Side_Column_Configuration_Changed (Buffer);

      --  Add an icon to hide the lines

      Command := new Hide_Editable_Lines_Type;
      Command.Buffer := Source_Buffer (Buffer);
      Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line - 1));
      Command.Mark := Create_Mark (Buffer, "", Iter);
      Command.Number := Editable_Line_Type (Number_Of_Lines_Unfolded - 1);

      Add_Block_Command
        (Buffer, First_Line,
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
      Result       : Command_Return_Type;
      pragma Unreferenced (Result);

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

         if Buffer.Editable_Lines (Line).Side_Info_Data /= null
           and then Buffer.Editable_Lines (Line).Where = In_Buffer
           and then Buffer.Editable_Lines (Line).Side_Info_Data
           (Buffer.Block_Highlighting_Column).Info /= null
         then
            Command :=
              Buffer.Editable_Lines (Line).Side_Info_Data
              (Buffer.Block_Highlighting_Column).Info.Associated_Command;

            if Command /= null
              and then Command.all in Hide_Editable_Lines_Type'Class
            then
               if First_Line_Found then
                  Line := Line +
                    Hide_Editable_Lines_Type (Command.all).Number;
                  Result := Execute (Command);

                  --  even though lines have been deleted, constructs info
                  --  hasn't changed.
                  Buffer.Constructs_State := Prev_State;
               else
                  First_Line_Found := True;
               end if;
            end if;
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
      Command      : Command_Access;
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
         if Buffer.Editable_Lines (Line).Side_Info_Data /= null
           and then Buffer.Editable_Lines
             (Line).Side_Info_Data
             (Buffer.Block_Highlighting_Column).Info /= null
         then
            Command :=
              Buffer.Editable_Lines (Line).Side_Info_Data
              (Buffer.Block_Highlighting_Column).Info.Associated_Command;

            if Command /= null
              and then Command.all in Unhide_Editable_Lines_Type'Class
            then
               Line := Unhide_Editable_Lines_Type (Command.all).Last_Line;
               Result := Execute (Command);

               --  Even though lines have been deleted, the constructs state
               --  information hasn't changed.
               Buffer.Constructs_State := Prev_State;
            end if;
         end if;

         Line := Line + 1;
      end loop;

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
      Returned       : Command_Return_Type;
      pragma Unreferenced (Returned);
   begin
      if Buffer.Block_Highlighting_Column = -1 then
         return False;
      end if;

      for L in reverse Editable_Lines'First .. Line loop
         if Buffer.Editable_Lines (L).Where = In_Buffer then
            if Buffer.Editable_Lines (L).Side_Info_Data /= null
              and then Buffer.Editable_Lines (L).Side_Info_Data
                         (Buffer.Block_Highlighting_Column).Info /= null
            then
               Command :=
                 Buffer.Editable_Lines (L).Side_Info_Data
                 (Buffer.Block_Highlighting_Column).Info.Associated_Command;

               if Command /= null
                 and then
                   ((Fold and then
                       Command.all in Hide_Editable_Lines_Type'Class)
                    or else
                      (not Fold and then
                         Command.all in Unhide_Editable_Lines_Type'Class))
               then
                  Returned := Execute (Command);
                  return True;
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
      if Lines_Are_Real (Buffer) then
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
      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      if Get_Constructs_State (Buffer) /= Exact then
         Compute_Blocks (Buffer);
      end if;

      Result := Fold_Unfold_Line (Buffer, Line, Fold => True);
   end Fold_Block;

   -----------------------------------
   -- Remove_Block_Folding_Commands --
   -----------------------------------

   procedure Remove_Block_Folding_Commands
     (Buffer                 : access Source_Buffer_Record'Class;
      Remove_Unfold_Commands : Boolean := True)
   is
      Command : Command_Access;
      Other_Command_Found : Boolean := False;

   begin
      if Buffer.Block_Highlighting_Column = -1 then
         return;
      end if;

      for Line in 1 .. Buffer.Last_Editable_Line loop
         if Buffer.Editable_Lines (Line).Side_Info_Data /= null
           and then Buffer.Editable_Lines (Line).Side_Info_Data
           (Buffer.Block_Highlighting_Column).Info /= null
         then
            Command :=
              Buffer.Editable_Lines (Line).Side_Info_Data
              (Buffer.Block_Highlighting_Column).Info.Associated_Command;

            if Command /= null then
               if Command.all in Hide_Editable_Lines_Type'Class
                 or else
                   (Remove_Unfold_Commands
                    and then Command.all in
                      Unhide_Editable_Lines_Type'Class)
               then
                  Add_Block_Command (Buffer, Line, null, null);
               else
                  Other_Command_Found := True;
               end if;
            end if;
         end if;
      end loop;

      if not Other_Command_Found then
         Remove_Line_Information_Column (Buffer, Block_Info_Column);
         Buffer.Block_Highlighting_Column := -1;
      end if;
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
      Color                : Gdk_Color;
      Tag                  : Gtk_Text_Tag;
      New_Tag              : Boolean := False;
   begin
      --  Here we test whether the buffer is in destruction. If it is the case
      --  we simply return since it is not worth taking care of unhighlighting
      --  lines. Furthermore this prevents GPS from crashing when we close a
      --  source file used in a visual diff while the reference file is still
      --  being displayed.

      if Buffer.In_Destruction then
         return;
      end if;

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
      else
         Set_Property (Tag, Underline_Property, Pango_Underline_Error);
      end if;

      if New_Tag then
         Add (Get_Tag_Table (Buffer), Tag);
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

      --  Highlight/Unhighlight the text

      if Remove then
         Remove_Tag (Buffer, Tag, Start_Iter, End_Iter);
      else
         Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
      end if;
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
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type) return Boolean
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Buffer_Line    : Buffer_Line_Type;
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
         Buffer_Line := Get_Buffer_Line (Buffer, Start_Line);

         while Buffer_Line < Get_Buffer_Line (Buffer, End_Line) loop
            if Get_Editable_Line (Buffer, Buffer_Line) = 0 then
               Remove_Blank_Lines (Buffer, Buffer_Line, 0);
            end if;

            Buffer_Line := Buffer_Line + 1;
         end loop;
      end if;

      return Result;
   end Flatten_Area;

end Src_Editor_Buffer.Line_Information;
