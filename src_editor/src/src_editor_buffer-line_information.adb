-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                       Copyright (C) 2003                          --
--                            ACT-Europe                             --
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

with Glib;                        use Glib;
with Glib.Convert;                use Glib.Convert;
with Glib.Object;                 use Glib.Object;
with Gdk;                         use Gdk;
with Gdk.Drawable;                use Gdk.Drawable;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with Gdk.Pixmap;                  use Gdk.Pixmap;
with Gtk;                         use Gtk;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Widget;                  use Gtk.Widget;
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with Src_Editor_Module;           use Src_Editor_Module;
with Pango.Layout;                use Pango.Layout;

with Traces;                      use Traces;
with Basic_Types;                 use Basic_Types;
with Glide_Kernel;                use Glide_Kernel;
with Glide_Kernel.Preferences;    use Glide_Kernel.Preferences;

with Gtkada.Types;              use Gtkada.Types;
with Basic_Types;               use Basic_Types;
with System;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;

with Pango.Layout;              use Pango.Layout;
with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Traces; use Traces;
with Gtk.Enums; use Gtk.Enums;

with Commands;        use Commands;
with Commands.Editor; use Commands.Editor;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

package body Src_Editor_Buffer.Line_Information is

   Me : constant Debug_Handle := Create ("Src_Editor_Buffer.Line_Information");

   Block_Info_Column : constant String := "Block Information";
   --  Identifier for the block information column.

   procedure Remove_Line_Information_Column
     (Buffer        : access Source_Buffer_Record'Class;
      Stick_To_Data : Boolean;
      Column        : Integer);
   --  Remove the column from the side window information in Buffer.

   procedure Get_Column_For_Identifier
     (Buffer        : access Source_Buffer_Record'Class;
      Identifier    : String;
      Width         : Integer;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean);
   --  Return the index of the column corresponding to the identifier.
   --  Create such a column if necessary.

   procedure Side_Column_Configuration_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "side_column_configuration_changed" signal.

   procedure Side_Column_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "side_column_changed" signal.

   procedure Recalculate_Side_Column_Width
     (Buffer : access Source_Buffer_Record'Class);
   --  Recalculate the total width of the left column side.

   -----------------------------------
   -- Recalculate_Side_Column_Width --
   -----------------------------------

   procedure Recalculate_Side_Column_Width
     (Buffer : access Source_Buffer_Record'Class)
   is
      BL : Columns_Config_Access renames Buffer.Buffer_Line_Info_Columns;
      EL : Columns_Config_Access renames Buffer.Editable_Line_Info_Columns;
   begin
      --  ??? This code could be made prettier.

      Buffer.Total_Column_Width := 2;

      if BL.all /= null then
         Buffer.Total_Column_Width :=
           Buffer.Total_Column_Width
             + BL.all (BL.all'Last).Starting_X + BL.all (BL.all'Last).Width;
      end if;

      if EL.all /= null then
         Buffer.Total_Column_Width :=
           Buffer.Total_Column_Width
             + EL.all (EL.all'Last).Starting_X + EL.all (EL.all'Last).Width;
      end if;
   end Recalculate_Side_Column_Width;

   -------------------------------
   -- Get_Column_For_Identifier --
   -------------------------------

   procedure Get_Column_For_Identifier
     (Buffer        : access Source_Buffer_Record'Class;
      Identifier    : String;
      Width         : Integer;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean)
   is
      Column         : Integer;
      Columns_Config : Columns_Config_Access;

   begin
      if Stick_To_Data then
         Columns_Config := Buffer.Buffer_Line_Info_Columns;
      else
         Columns_Config := Buffer.Editable_Line_Info_Columns;
      end if;

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

               Columns_Config.all (Column).Stick_To_Data := Stick_To_Data;
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
            Stick_To_Data => Stick_To_Data,
            Every_Line    => Every_Line);

      else
         declare
            A         : Line_Info_Display_Array
              (Columns_Config.all'First .. Columns_Config.all'Last + 1);
         begin
            A (Columns_Config.all'Range) := Columns_Config.all.all;

            A (A'Last) := new Line_Info_Display_Record'
              (Identifier  => new String'(Identifier),
               Starting_X  => Columns_Config.all (A'Last - 1).Starting_X
                 + Columns_Config.all (A'Last - 1).Width + 1,
               Width       => Width,
               Stick_To_Data => Stick_To_Data,
               Every_Line    => Every_Line);
            Unchecked_Free (Columns_Config.all);
            Columns_Config.all := new Line_Info_Display_Array'(A);

            Column := Columns_Config.all'Last;

            Recalculate_Side_Column_Width (Buffer);
         end;
      end if;

      --  Create a corresponding column in all lines.
      --  ??? How costly is this ?

      if Stick_To_Data then
         for J in Buffer.Line_Data'Range loop
            if Buffer.Line_Data (J).Side_Info_Data = null then
               --  ??? Code duplication with Add_Lines.
               Buffer.Line_Data (J).Side_Info_Data := new
                 Line_Info_Width_Array (Columns_Config.all'Range);

               for K in Columns_Config.all'Range loop
                  Buffer.Line_Data (J).Side_Info_Data (K) :=
                    (null,
                     Width => -1,
                     Set   => not Columns_Config.all (K).Every_Line);
               end loop;

            else
               declare
                  A : Line_Info_Width_Array
                    (Buffer.Line_Data (J).Side_Info_Data'First
                       .. Buffer.Line_Data (J).Side_Info_Data'Last + 1);
               begin
                  A (A'First .. A'Last - 1) :=
                    Buffer.Line_Data (J).Side_Info_Data.all;

                  Unchecked_Free (Buffer.Line_Data (J).Side_Info_Data);
                  Buffer.Line_Data (J).Side_Info_Data :=
                    new Line_Info_Width_Array'(A);

                  Buffer.Line_Data (J).Side_Info_Data
                    (Buffer.Line_Data (J).Side_Info_Data'Last) :=
                    (Info  => null,
                     Width => Width,
                     Set   => not Every_Line);
               end;
            end if;
         end loop;

      else
         for J in Buffer.Editable_Lines'Range loop
            if Buffer.Editable_Lines (J).Side_Info_Data = null then
               --  ??? Code duplication with Add_Lines.
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
      end if;
   end Get_Column_For_Identifier;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Buffer          : access Source_Buffer_Record'Class;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean) is
   begin
      Get_Column_For_Identifier
        (Buffer, Identifier, -1, Stick_To_Data, Every_Line);
      Side_Column_Configuration_Changed (Buffer);
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Buffer        : access Source_Buffer_Record'Class;
      Stick_To_Data : Boolean;
      Column        : Integer)
   is
      Columns_Config : Columns_Config_Access;
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Width : Integer;
   begin
      if Stick_To_Data then
         Columns_Config := Buffer.Buffer_Line_Info_Columns;
      else
         Columns_Config := Buffer.Editable_Line_Info_Columns;
      end if;

      Width := Columns_Config.all (Column).Width;

      --  Free the column for all data.

      if Stick_To_Data then
         for J in Buffer_Lines'Range loop
            if Buffer_Lines (J).Side_Info_Data /= null then

               if Buffer_Lines (J).Side_Info_Data'First
                 /= Columns_Config.all'First
                 or else  Buffer_Lines (J).Side_Info_Data'Last
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
                    Buffer_Lines (J).Side_Info_Data
                    (Columns_Config.all'First .. Column - 1);
                  A (Column .. Columns_Config.all'Last - 1) :=
                    Buffer_Lines (J).Side_Info_Data
                    (Column + 1 .. Columns_Config.all'Last);
                  Unchecked_Free (Buffer_Lines (J).Side_Info_Data);

                  Buffer_Lines (J).Side_Info_Data :=
                    new Line_Info_Width_Array'(A);
               end;
            end if;
         end loop;

      else
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
      end if;

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
      Stick_To_Data  : Boolean := True;
      Column         : Integer := -1;
   begin
      --  Browse through existing columns and try to match Identifier

      Columns_Config := Buffer.Editable_Line_Info_Columns;

      if Columns_Config.all /= null then
         for J in Columns_Config.all'Range loop
            if Columns_Config.all (J).Identifier.all = Identifier then
               Stick_To_Data := False;
               Column := J;
               exit;
            end if;
         end loop;
      end if;

      if Stick_To_Data then
         Columns_Config := Buffer.Buffer_Line_Info_Columns;

         if Columns_Config.all /= null then
            for J in Columns_Config.all'Range loop
               if Columns_Config.all (J).Identifier.all = Identifier then
                  Stick_To_Data := True;
                  Column := J;
                  exit;
               end if;
            end loop;
         end if;
      end if;

      if Column = -1 then
         return;
      end if;

      Remove_Line_Information_Column (Buffer, Stick_To_Data, Column);
      Side_Column_Configuration_Changed (Buffer);
   end Remove_Line_Information_Column;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Box        : Gtk_Widget;
      Info       : Glide_Kernel.Modules.Line_Information_Data)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;

      Column : Integer := -1;
      Num    : Gint := 1;
      Height : Gint;
      Width  : Gint := -1;
      Widths : array (Info'Range) of Gint;
      Layout : Pango_Layout;
      Found  : Boolean := False;

      Line        : Editable_Line_Type;
      Buffer_Line : Buffer_Line_Type;

      Stick_To_Data  : Boolean;
      Columns_Config : Columns_Config_Access;
   begin
      --  Test if we are adding extra information, or line information.

      if Info'First < 0 then
         --  Look for an existing entry.

         if Buffer.Extra_Information = null then
            Buffer.Extra_Information := new Extra_Information_Array'
              (1 => new Extra_Information_Record'
                 (Identifier => new String'(Identifier),
                  Info => new Line_Information_Record'(Info (Info'First))));
         else
            for J in Buffer.Extra_Information'Range loop
               if Buffer.Extra_Information (J).Identifier.all = Identifier then
                  Unchecked_Free (Buffer.Extra_Information (J).Info);
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

      --  If we reach this point, the info corresponds to line information.

      Layout := Create_Pango_Layout (Box);
      Set_Font_Description
        (Layout,
         Get_Pref (Buffer.Kernel, Source_Editor_Font));

      --  Compute the maximum width of the items to add.
      --  We compute this width once and for all and in advance,
      --  because is is quite expensive, and we don't want to do it
      --  in Src_Editor_View.Redraw_Columns, since that function is
      --  called a great number of times.

      for J in Info'Range loop
         Widths (J) := -1;

         if Info (J).Text /= null then
            Set_Text (Layout, String'(Info (J).Text.all));
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

      Columns_Config := Buffer.Buffer_Line_Info_Columns;
      Stick_To_Data  := False;

      if Columns_Config.all /= null then
         for J in Columns_Config.all'Range loop
            if Columns_Config.all (J).Identifier.all = Identifier then
               Column := J;
               Stick_To_Data := True;
               exit;
            end if;
         end loop;
      end if;

      if not Stick_To_Data then
         Columns_Config := Buffer.Editable_Line_Info_Columns;

         if Columns_Config.all = null then
            Unref (Layout);
            return;
         end if;

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

      --  The column has been found: update the stored data.

      if Columns_Config.all (Column).Stick_To_Data then
         for K in Info'Range loop
            Buffer_Line := Get_Buffer_Line (Buffer, File_Line_Type (K));

            if Buffer_Line /= 0 then
               Buffer_Lines (Buffer_Line).Side_Info_Data (Column).Info :=
                 new Line_Information_Record'(Info (K));
               Buffer_Lines
                 (Buffer_Line).Side_Info_Data (Column).Width :=
                 Integer (Widths (K));
               Buffer_Lines
                 (Buffer_Line).Side_Info_Data (Column).Set := True;
            end if;
         end loop;

      else
         for K in Info'Range loop
            Line := Editable_Line_Type (K);

            if Line /= 0 then
               Editable_Lines (Line).Side_Info_Data (Column).Info :=
                 new Line_Information_Record'(Info (K));
               Editable_Lines
                 (Line).Side_Info_Data (Column).Width :=
                 Integer (Widths (K));
               Editable_Lines (Line).Side_Info_Data (Column).Set := True;
            end if;
         end loop;
      end if;

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
         Side_Column_Configuration_Changed (Buffer);
      end if;

      Side_Column_Changed (Buffer);
      Unref (Layout);
   end Add_File_Information;

   -------------------------
   -- Side_Column_Changed --
   -------------------------

   procedure Side_Column_Changed
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");
   begin
      Emit_By_Name (Get_Object (Buffer), "side_column_changed" & ASCII.NUL);
   end Side_Column_Changed;

   ---------------------------------------
   -- Side_Column_Configuration_Changed --
   ---------------------------------------

   procedure Side_Column_Configuration_Changed
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");
   begin
      Emit_By_Name
        (Get_Object (Buffer),
         "side_column_configuration_changed" & ASCII.NUL);
   end Side_Column_Configuration_Changed;

   --------------------
   -- Draw_Line_Info --
   --------------------

   procedure Draw_Line_Info
     (Editor      : access Source_Buffer_Record'Class;
      Top_Line    : Buffer_Line_Type;
      Bottom_Line : Buffer_Line_Type;
      View        : Gtk_Text_View;
      GC          : Gdk.GC.Gdk_GC;
      Layout      : in out Pango_Layout;
      Drawable    : in out Gdk.Pixmap.Gdk_Pixmap)
   is
      Current_Line : Buffer_Line_Type;
      Iter         : Gtk_Text_Iter;
      Y_In_Buffer                : Gint;
      Y_Pix_In_Window            : Gint;
      Line_Height                : Gint;
      Dummy_Gint                 : Gint;
      Dummy_Boolean              : Boolean;
      Line_Info                  : Line_Info_Width;
      Editable_Line : Editable_Line_Type;

      Buffer_Line_Starting_X     : Gint := 0;

      BL : Columns_Config_Access renames Editor.Buffer_Line_Info_Columns;
      EL : Columns_Config_Access renames Editor.Editable_Line_Info_Columns;

      procedure Draw_Info (Starting_X : Gint);
      --  Draw the info contained in Line_Info, at offset Starting_X.

      procedure Draw_Info (Starting_X : Gint) is
      begin
         if Line_Info.Info /= null then
            if Line_Info.Info.Text /= null then
               Set_Text (Layout, Line_Info.Info.Text.all);
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
                  Gc       => GC,
                  Src_X    => 0,
                  Src_Y    => 0,
                  Dest_X   => Starting_X,
                  Dest_Y   => Y_Pix_In_Window,
                  Width    => -1,
                  Height   => -1);
            end if;
         end if;
      end Draw_Info;

   begin
      if EL.all /= null and then BL.all /= null then
         Buffer_Line_Starting_X :=
           Gint (EL.all (EL.all'Last).Starting_X
                   + EL.all (EL.all'Last).Width + 1);
      end if;

      Current_Line := Top_Line;
      Get_Iter_At_Line (Editor, Iter, Gint (Current_Line - 1));

      Drawing_Loop :
      while Current_Line <= Bottom_Line loop

         Get_Line_Yrange (View, Iter, Y_In_Buffer, Line_Height);

         --  Convert the buffer coords back to window coords

         Buffer_To_Window_Coords
           (View, Text_Window_Left,
            Buffer_X => 0, Buffer_Y => Y_In_Buffer,
            Window_X => Dummy_Gint, Window_Y => Y_Pix_In_Window);

         if EL.all /= null then
            for Col in EL.all'Range loop
               Editable_Line := Editor.Line_Data (Current_Line).Editable_Line;

               if Editable_Line /= 0 then
                  Line_Info :=
                    Editor.Editable_Lines
                      (Editable_Line).Side_Info_Data (Col);

                  Draw_Info
                    (Gint (EL.all (Col).Width - Line_Info.Width)
                       + Gint (EL.all (Col).Starting_X));
               end if;
            end loop;
         end if;

         if BL.all /= null then
            for Col in BL.all'Range loop
               Line_Info :=
                 Editor.Line_Data
                   (Current_Line).Side_Info_Data (Col);

               Draw_Info
                 (Buffer_Line_Starting_X
                    + Gint (BL.all (Col).Width - Line_Info.Width)
                    + Gint (BL.all (Col).Starting_X));
            end loop;
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
     (Editor : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Offset : Gint)
   is
      BL : Columns_Config_Access renames Editor.Buffer_Line_Info_Columns;
      EL : Columns_Config_Access renames Editor.Editable_Line_Info_Columns;
      Buffer_Line_Starting_X : Gint := 0;
      Editable_Line          : Editable_Line_Type;
   begin
      if EL.all /= null then
         for Col in EL.all'Range loop
            if Offset < Gint
              (EL.all (Col).Width + EL.all (Col).Starting_X)
            then
               Editable_Line := Editor.Line_Data (Line).Editable_Line;

               if Editable_Line /= 0 then
                  if Editor.Editable_Lines
                    (Editable_Line).Side_Info_Data
                    (Col).Info /= null
                  and then Editor.Editable_Lines
                    (Editable_Line).Side_Info_Data
                    (Col).Info.Associated_Command /= null
                  then
                     Launch_Synchronous
                       (Editor.Editable_Lines
                          (Editable_Line).Side_Info_Data
                          (Col).Info.Associated_Command);
                  end if;
               end if;

               return;
            end if;
         end loop;
      end if;

      if EL.all /= null and then BL.all /= null then
         Buffer_Line_Starting_X :=
           Gint (EL.all (EL.all'Last).Starting_X
                   + EL.all (EL.all'Last).Width + 1);
      end if;

      for Col in BL.all'Range loop
         if Offset < Gint
           (BL.all (Col).Width
              + BL.all (Col).Starting_X) + Buffer_Line_Starting_X
         then
            if Editor.Line_Data
              (Line).Side_Info_Data
              (Col).Info /= null
              and then Editor.Line_Data
                (Line).Side_Info_Data
                (Col).Info.Associated_Command /= null
            then
               Launch_Synchronous
                 (Editor.Line_Data
                    (Line).Side_Info_Data
                    (Col).Info.Associated_Command);
            end if;

            return;
         end if;
      end loop;
   end On_Click;

   ---------------------
   -- Add_Blank_Lines --
   ---------------------

   function Add_Blank_Lines
     (Editor : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      GC     : Gdk.GC.Gdk_GC;
      Text   : String;
      Number : Positive) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      pragma Unreferenced (Text);
      Command     : Remove_Blank_Lines_Command;
      LFs         : String (1 .. Natural (Number));
      Buffer_Line : Buffer_Line_Type;
      Iter        : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Success     : Boolean;
      Mark        : Gtk.Text_Mark.Gtk_Text_Mark;
   begin
      Buffer_Line := Get_Buffer_Line (Editor, Line);

      if Buffer_Line = 0 then
         return null;
      end if;

      End_Action (Editor);

      LFs := (others => ASCII.LF);

      Get_Iter_At_Line (Editor, Iter, Gint (Buffer_Line - 1));

      Editor.Modifying_Editable_Lines := False;
      Editor.Inserting := True;
      Insert (Editor, Iter, LFs);
      Editor.Inserting := False;
      Editor.Modifying_Editable_Lines := True;

      Get_Iter_At_Line (Editor, Iter, Gint (Buffer_Line - 1));
      Backward_Char (Iter, Success);
      Get_Iter_At_Line (Editor, End_Iter,
                        Gint (Buffer_Line - 1) + Gint (Number));

      Apply_Tag (Editor, Editor.Non_Editable_Tag, Iter, End_Iter);

      --  Shift down editable lines.

      for J in Line .. Editor.Editable_Lines'Last loop
         if Editor.Editable_Lines (J).Where = In_Buffer then
            Editor.Editable_Lines (J).Buffer_Line :=
              Editor.Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Number);
         end if;
      end loop;

      --  Reset information for newly inserted buffer lines.

      for J in Buffer_Line .. Buffer_Line + Buffer_Line_Type (Number) - 1 loop
         Editor.Line_Data (J).Editable_Line := 0;
         Editor.Line_Data (J).Current_Highlight := GC;
      end loop;

      Get_Iter_At_Line_Offset (Editor, Iter, Gint (Buffer_Line - 1), 0);

      --  Create a command to remove the line information at the desired
      --  column.

      Mark := Create_Mark (Editor, "", Iter);

      Command := new Remove_Blank_Lines_Command_Type;
      Command.Buffer := Source_Buffer (Editor);
      Command.Mark   := Mark;
      Command.Number := Natural (Number);

      Add_Block_Command (Editor, Buffer_Line, Command_Access (Command),
                         Remove_Blank_Lines_Pixbuf);

      Side_Column_Changed (Editor);

      return Mark;
   end Add_Blank_Lines;

   -----------------------
   -- Add_Block_Command --
   -----------------------

   procedure Add_Block_Command
     (Buffer      : access Source_Buffer_Record'Class;
      Buffer_Line : Buffer_Line_Type;
      Command     : Command_Access;
      Image       : Gdk_Pixbuf)
   is
      Column  : Natural;
      Width   : Integer;
      BL      : Columns_Config_Access renames Buffer.Buffer_Line_Info_Columns;
   begin
      --  Create the line information column.
      --  ??? This should not occur every time.

      Create_Line_Information_Column (Buffer, Block_Info_Column, True, False);

      for J in BL.all'Range loop
         if BL.all (J).Identifier.all = Block_Info_Column then

            if Image = null then
               Width := 0;
            else
               Width := Integer (Get_Width (Image));
            end if;

            Free (Buffer.Line_Data (Buffer_Line).Side_Info_Data (J));
            Buffer.Line_Data (Buffer_Line).Side_Info_Data (J) :=
              (Info => new Line_Information_Record'
                 (Text               => null,
                  Image              => Image,
                  Associated_Command => Command),
               Width => Width,
               Set   => True);

            Column := J;
            exit;
         end if;
      end loop;

      if Column /= -1 and then Command /= null then
         BL.all (Column).Width := Width;

         for J in Column + 1 .. BL.all'Last loop
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
     (Editor : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      Column : Positive) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      Buffer_Line : Buffer_Line_Type;
      Iter        : Gtk_Text_Iter;
   begin
      --  ??? How do we deal with marks in non-present lines ?

      Buffer_Line := Get_Buffer_Line (Editor, Line);

      if Buffer_Line = 0 then
         Get_Iter_At_Line_Offset (Editor, Iter, 0, 0);
         return Create_Mark (Editor, "", Iter);
      end if;

      if Is_Valid_Position
        (Editor, Gint (Buffer_Line - 1), Gint (Column - 1))
      then
         Get_Iter_At_Line_Offset
           (Editor, Iter, Gint (Buffer_Line - 1), Gint (Column - 1));
         return Create_Mark (Editor, "", Iter);

      elsif Is_Valid_Position (Editor, Gint (Buffer_Line - 1), 1) then
         Get_Iter_At_Line_Offset (Editor, Iter, Gint (Buffer_Line - 1), 1);
         return Create_Mark (Editor, "", Iter);
      end if;

      Get_Iter_At_Line_Offset (Editor, Iter, 0, 0);
      return Create_Mark (Editor, "", Iter);
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

      Bottom_Line : Buffer_Line_Type;
      Ref_Editable_Line : Editable_Line_Type;

      procedure Expand_Lines
        (N : Buffer_Line_Type);
      --  Expand the line-indexed arrays to contain N lines in size.

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
            Create_Side_Info (Buffer, J);
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

            Create_Side_Info (Buffer, Start + J);

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
         Bottom_Line := Buffer_Line_Type (Get_Line_Count (Buffer));

         if Buffer_Lines'Last < Bottom_Line then
            Expand_Lines (Bottom_Line);
         end if;

         --  Shift down the existing lines.

         for J in reverse Start + Number .. Buffer_Lines'Last loop
            Buffer_Lines (J) := Buffer_Lines (J - Number);

            if Buffer.Modifying_Editable_Lines
              and then Buffer_Lines (J).Editable_Line /= 0
            then
               Buffer_Lines (J).Editable_Line
                 := Buffer_Lines (J).Editable_Line
                 + Editable_Line_Type (Number);

               if Editable_Lines
                 (Buffer_Lines (J).Editable_Line).Where = In_Buffer
               then
                  Editable_Lines (Buffer_Lines (J).Editable_Line).Buffer_Line
                    := J;
               end if;
            end if;
         end loop;

         --  Reset the newly inserted lines.

         for J in 0 .. Number - 1 loop
            Buffer_Lines (Start + J) := New_Line_Data;
            Buffer_Lines (Start + J).Editable_Line := Ref_Editable_Line
              + Editable_Line_Type (J);

            if Editable_Lines
              (Buffer_Lines (Start + J).Editable_Line).Where = In_Buffer
            then
               Editable_Lines
                 (Buffer_Lines (Start + J).Editable_Line).Buffer_Line :=
                 Start + J;
            end if;

            Create_Side_Info (Buffer, Start + J);
         end loop;

         if Buffer.Modifying_Editable_Lines then
            Buffer.Last_Editable_Line := Buffer.Last_Editable_Line +
              Editable_Line_Type (Number);
         end if;
      end if;
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
      EN     : constant Editable_Line_Type := Editable_Line_Type (Number);

   begin
      if End_Line <= Start_Line then
         return;
      end if;

      for J in Start_Line .. Buffer_Lines'Last - Number - 1 loop
         Buffer_Lines (J) := Buffer_Lines (J + Number);

         if Buffer.Modifying_Editable_Lines then
            if Buffer_Lines (J).Editable_Line /= 0 then
               Buffer_Lines (J).Editable_Line :=
                 Buffer_Lines (J).Editable_Line - EN;

               if Editable_Lines
                 (Buffer_Lines (J).Editable_Line).Where = In_Buffer
               then
                  Editable_Lines
                    (Buffer_Lines (J).Editable_Line).Buffer_Line := J;
               end if;
            end if;
         end if;
      end loop;

      --  Reset bottom lines

      for J in Buffer_Lines'Last - Number
        .. Buffer_Lines'Last
      loop
         if Buffer_Lines (J).Editable_Line /= 0
           and then Editable_Lines
             (Buffer_Lines (J).Editable_Line).Where = In_Buffer
         then
            Editable_Lines
              (Buffer_Lines (J).Editable_Line).Buffer_Line := 0;
         end if;

         Buffer_Lines (J) := New_Line_Data;
      end loop;

      if Buffer.Modifying_Editable_Lines then
         Buffer.Last_Editable_Line := Buffer.Last_Editable_Line -
           Editable_Line_Type (End_Line - Start_Line);
      end if;
   end Remove_Lines;

   ------------------------
   -- Remove_Blank_Lines --
   ------------------------

   procedure Remove_Blank_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Number : Natural)
   is
      Iter     : Gtk_Text_Iter;
      End_Iter : Gtk_Text_Iter;
      Buffer_Line_At_Blanks  : Buffer_Line_Type;
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;

      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      BL : Columns_Config_Access renames Buffer.Buffer_Line_Info_Columns;

      Real_Number : Buffer_Line_Type := 0;
      Result      : Boolean := True;
      Buffer_Line : Buffer_Line_Type;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Mark);

      --  Compute the real number of blank lines.

      Get_Iter_At_Line (Buffer, End_Iter, Get_Line (Iter));
      Buffer_Line_At_Blanks := Buffer_Line_Type (Get_Line (Iter)) + 1;

      Buffer_Line := Buffer_Line_At_Blanks;

      while Result loop
         Forward_Line (End_Iter, Result);
         Buffer_Line := Buffer_Line + 1;
         Real_Number := Real_Number + 1;
         exit when Real_Number = Buffer_Line_Type (Number)
           or else Buffer_Lines (Buffer_Line).Editable_Line /= 0;
      end loop;

      Buffer.Modifying_Editable_Lines := False;
      Buffer.Inserting := True;
      Delete (Buffer, Iter, End_Iter);
      Buffer.Inserting := False;
      Buffer.Modifying_Editable_Lines := True;

      for J in Buffer_Line_At_Blanks .. Buffer_Lines'Last loop
         if Buffer_Lines (J).Editable_Line /= 0
           and then Editable_Lines
             (Buffer_Lines (J).Editable_Line).Where = In_Buffer
         then
            Editable_Lines (Buffer_Lines (J).Editable_Line).Buffer_Line :=
              Editable_Lines (Buffer_Lines (J).Editable_Line).Buffer_Line
              - Real_Number;
         end if;
      end loop;

      --  If it was the last item in the column, delete the column.

      Delete_Columns :
      for Col in BL.all'Range loop
         if BL.all (Col).Identifier.all = Block_Info_Column then
            for J in Buffer_Lines'Range loop
               exit Delete_Columns when
                 Buffer_Lines (J).Side_Info_Data /= null
                 and then Buffer_Lines (J).Side_Info_Data (Col).Info /= null;
            end loop;

            Remove_Line_Information_Column (Buffer, Block_Info_Column);
            exit Delete_Columns;
         end if;
      end loop Delete_Columns;

      --  Redraw the side column.

      Side_Column_Configuration_Changed (Buffer);
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
      Line_Start : Editable_Line_Type;
      Line_End   : Editable_Line_Type;
      Iter       : Gtk_Text_Iter;

      Command    : Unhide_Editable_Lines_Command;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Mark);

      Buffer_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

      Line_Start := Get_Editable_Line (Buffer, Buffer_Line);

      Line_End := Line_Start + Number;

      --  Remove line by line in order to avoid problems when removing lines
      --  that are already removed.

      for L in Line_Start .. Line_End loop
         if L not in Editable_Lines'Range then
            return;
         end if;

         if Editable_Lines (L).Where = In_Buffer then
            --  Get the line of text and store it in the pointer.

            Get_Iter_At_Line
              (Buffer,
               Start_Iter,
               Gint (Buffer_Line - 1));

            Copy (Start_Iter, End_Iter);

            Forward_To_Line_End (End_Iter);

            if Get_Line (Start_Iter) /= Get_Line (End_Iter) then
               Backward_Char (End_Iter, Result);
            end if;

            declare
               Line_Data : Editable_Line_Data :=
                 (Where          => In_Mark,
                  Side_Info_Data => Editable_Lines (L).Side_Info_Data,
                  Mark           => null,
                  Text           => null);
            begin
               declare
                  Ignore   : Natural;
                  UTF8     : constant Gtkada.Types.Chars_Ptr :=
                    Get_Text (Buffer, Start_Iter, End_Iter, True);
                  Length   : constant Natural := Integer (Strlen (UTF8));
                  Bytes    : Natural;

               begin
                  Line_Data.Text := new String (1 .. Length);
                  Glib.Convert.Convert
                    (UTF8, Length,
                     Get_Pref (Buffer.Kernel, Default_Charset), "UTF-8",
                     Ignore, Bytes, Result => Line_Data.Text.all);

                  --  ??? Should not ignore Bytes, otherwise Line_Data.Text
                  --  will contain garbage

                  g_free (UTF8);
               end;

               Editable_Lines (L) := Line_Data;
            end;

            --  Remove the line from the screen.

            Forward_Char (End_Iter, Result);

            Buffer.Modifying_Editable_Lines := False;
            Buffer.Inserting := True;
            Delete (Buffer, Start_Iter, End_Iter);
            Buffer.Inserting := False;
            Buffer.Modifying_Editable_Lines := True;
         end if;
      end loop;

      --  Shift up editable lines.

      for J in Line_End + 1 .. Editable_Lines'Last loop
         if Editable_Lines (J).Where = In_Buffer then
            Editable_Lines (J).Buffer_Line := Editable_Lines (J).Buffer_Line
              - Buffer_Line_Type (Number) - 1;
         end if;
      end loop;

      --  Add an icon to unhide the lines.
      --  ???  Where are the marks freed ?

      Command := new Unhide_Editable_Lines_Type;
      Command.Buffer := Source_Buffer (Buffer);
      Command.Mark := Mark;
      Command.First_Line := Line_Start;
      Command.Last_Line   := Line_End;

      Add_Block_Command
        (Buffer, Buffer_Line - 1, Command_Access (Command),
         Unhide_Block_Pixbuf);
   end Hide_Lines;

   ------------------
   -- Unhide_Lines --
   ------------------

   procedure Unhide_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Mark       : Gtk.Text_Mark.Gtk_Text_Mark;
      First_Line : Editable_Line_Type;
      Last_Line  : Editable_Line_Type)
   is
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;
      Iter           : Gtk_Text_Iter;
      Buffer_Line    : Buffer_Line_Type;

   begin
      for Line in reverse First_Line .. Last_Line loop
         --  If the line is already in the buffer, skip.

         if Editable_Lines (Line).Where /= In_Buffer then
            --  Insert the line.

            Get_Iter_At_Mark (Buffer, Iter, Mark);

            Buffer_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

            Buffer.Modifying_Editable_Lines := False;
            Buffer.Inserting := True;
            Insert (Buffer, Iter, Editable_Lines (Line).Text.all & ASCII.LF);
            Buffer.Inserting := False;
            Buffer.Modifying_Editable_Lines := True;

            --  Modify editable line structure.

            declare
               Line_Data : constant Editable_Line_Data :=
                 (Where          => In_Buffer,
                  Side_Info_Data => Editable_Lines (Line).Side_Info_Data,
                  Buffer_Line    => Buffer_Line_Type (Get_Line (Iter) + 1));
            begin
               Free (Editable_Lines (Line).Text);
               Editable_Lines (Line) := Line_Data;
            end;

            --  Set the editable line information.
            Buffer.Line_Data (Buffer_Line).Editable_Line := Line;
         end if;
      end loop;

      for Line in First_Line .. Last_Line loop
         if Editable_Lines (Line).Where = In_Buffer then
            Editable_Lines (Line).Buffer_Line :=
              Editable_Lines (First_Line - 1).Buffer_Line
              + Buffer_Line_Type (Line - First_Line) + 1;
         end if;
      end loop;

      for J in Last_Line + 1 .. Editable_Lines'Last loop
         if Editable_Lines (J).Where = In_Buffer then
            Editable_Lines (J).Buffer_Line := Editable_Lines (J).Buffer_Line
              + Buffer_Line_Type (Last_Line - First_Line) + 1;
         end if;
      end loop;

      --  Redraw the side column.

      Side_Column_Configuration_Changed (Buffer);

      --  Remove the command that unhides the lines.

      Add_Block_Command
        (Buffer, Get_Buffer_Line (Buffer, First_Line - 1), null,
         Null_Pixbuf);
   end Unhide_Lines;

   --------------
   -- Fold_All --
   --------------

   procedure Fold_All (Buffer : access Source_Buffer_Record'Class) is
      BL : Columns_Config_Access renames Buffer.Buffer_Line_Info_Columns;
      Buffer_Lines : Line_Data_Array_Access renames Buffer.Line_Data;
      Command : Command_Access;

   begin
      if BL.all = null then
         return;
      end if;

      for Col in BL.all'Range loop
         if BL.all (Col).Identifier.all = Block_Info_Column then
            for Line in Buffer_Lines'Range loop
               if Buffer_Lines (Line).Side_Info_Data /= null
                 and then Buffer_Lines (Line).Side_Info_Data (Col).Info /= null
               then
                  Command :=
                    Buffer_Lines (Line).Side_Info_Data
                    (Col).Info.Associated_Command;

                  if Command /= null then
                     if Command.all in Hide_Editable_Lines_Type'Class then
                        if Execute (Command) = Success then
                           Fold_All (Buffer);
                        end if;

                        return;
                     end if;
                  end if;
               end if;
            end loop;

            exit;
         end if;
      end loop;
   end Fold_All;

   ----------------
   -- Unfold_All --
   ----------------

   procedure Unfold_All (Buffer : access Source_Buffer_Record'Class) is
      BL : Columns_Config_Access renames Buffer.Buffer_Line_Info_Columns;
      Buffer_Lines : Line_Data_Array_Access renames Buffer.Line_Data;
      Command : Command_Access;
   begin
      if BL.all = null then
         return;
      end if;

      for Col in BL.all'Range loop
         if BL.all (Col).Identifier.all = Block_Info_Column then
            for Line in Buffer_Lines'Range loop
               if Buffer_Lines (Line).Side_Info_Data /= null
                 and then Buffer_Lines (Line).Side_Info_Data (Col).Info /= null
               then
                  Command :=
                    Buffer_Lines (Line).Side_Info_Data
                    (Col).Info.Associated_Command;

                  if Command /= null then
                     if Command.all in Unhide_Editable_Lines_Type'Class then
                        if Execute (Command) = Success then
                           Unfold_All (Buffer);
                        end if;

                        return;
                     end if;
                  end if;
               end if;
            end loop;

            exit;
         end if;
      end loop;
   end Unfold_All;

   -----------------
   -- Unfold_Line --
   -----------------

   procedure Unfold_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type)
   is
      BL : Columns_Config_Access renames Buffer.Buffer_Line_Info_Columns;
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Editable_Lines : Editable_Line_Array_Access renames
        Buffer.Editable_Lines;

      Command        : Command_Access;
      Column         : Integer := -1;
      Buffer_Line    : Buffer_Line_Type;
   begin
      for Col in BL.all'Range loop
         if BL.all (Col).Identifier.all = Block_Info_Column then

            Column := Col;
            exit;
         end if;
      end loop;

      if Column = -1 then
         return;
      end if;

      while Editable_Lines (Line).Where /= In_Buffer loop
         --  Find the command unfolding the enclosing block.

         for L in reverse Line .. Editable_Lines'First loop
            if Editable_Lines (L).Where = In_Buffer then
               Buffer_Line := Get_Buffer_Line (Buffer, L);

               if Buffer_Lines (Buffer_Line).Side_Info_Data /= null
                 and then Buffer_Lines
                   (Buffer_Line).Side_Info_Data (Column).Info /= null
               then
                  Command :=
                    Buffer_Lines (Buffer_Line).Side_Info_Data
                    (Column).Info.Associated_Command;

                  if Command.all in Unhide_Editable_Lines_Type'Class then
                     if Execute (Command) /= Success then
                        return;
                     end if;
                  end if;
               end if;
               exit;
            end if;
         end loop;
      end loop;
   end Unfold_Line;

end Src_Editor_Buffer.Line_Information;
