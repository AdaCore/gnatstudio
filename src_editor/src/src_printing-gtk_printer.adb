------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Gtkada.Printing;
with Gtk.Print_Context;
with Glib;
with Src_Editor_Buffer.Text_Handling;
with Gtk.Print_Operation;
with Pango.Font;
with GPS.Kernel.Preferences;
with Ada.Text_IO;
with Cairo;
with Pango.Layout;
with Pango.Cairo;
with GNATCOLL.VFS;
with Basic_Types;        use Basic_Types;

package body Src_Printing.Gtk_Printer is

   use type Glib.Gint;
   use Src_Editor_Buffer;

   package Line_Number_IO is new Ada.Text_IO.Integer_IO (Editable_Line_Type);

   type GPS_Print_Operation_Record is
     new Gtkada.Printing.Gtkada_Print_Operation_Record with
      record
         Editor         : Src_Editor_Box.Source_Editor_Box;
         From_Line      : Editable_Line_Type;
         To_Line        : Editable_Line_Type;
         Lines_Per_Page : Editable_Line_Type;
         Font           : Pango.Font.Pango_Font_Description;
         Page_Height    : Glib.Gint;
         Line_Height    : Glib.Gint;
      end record;

   overriding procedure Begin_Print
     (Self        : access GPS_Print_Operation_Record;
      Context     : Gtk.Print_Context.Gtk_Print_Context);

   overriding procedure Draw_Page
     (Self        : access GPS_Print_Operation_Record;
      Context     : Gtk.Print_Context.Gtk_Print_Context;
      Page_Number : Glib.Gint);

   type GPS_Print_Operation is access all GPS_Print_Operation_Record;

   overriding procedure Begin_Print
     (Self        : access GPS_Print_Operation_Record;
      Context     : Gtk.Print_Context.Gtk_Print_Context)
   is
      X : Glib.Gint;
      Layout : constant Pango.Layout.Pango_Layout :=
        Context.Create_Pango_Layout;
   begin
      Self.Page_Height := Glib.Gint (Context.Get_Height);
      Layout.Set_Font_Description (Self.Font);
      Layout.Set_Text ("X");
      Layout.Get_Pixel_Size (X, Self.Line_Height);
      Self.Lines_Per_Page := Editable_Line_Type
        (Self.Page_Height / Self.Line_Height - 5);

      Self.Set_N_Pages
        (Glib.Gint
           ((Self.To_Line - Self.From_Line) / Self.Lines_Per_Page + 1));
      Layout.Unref;
   end Begin_Print;

   overriding procedure Draw_Page
     (Self        : access GPS_Print_Operation_Record;
      Context     : Gtk.Print_Context.Gtk_Print_Context;
      Page_Number : Glib.Gint)
   is

      use type Glib.Gdouble;

      CC     : constant Cairo.Cairo_Context := Context.Get_Cairo_Context;
      Buffer : constant Source_Buffer := Self.Editor.Get_Buffer;
      Offset : constant Editable_Line_Type :=
        Editable_Line_Type (Page_Number) * Self.Lines_Per_Page;
      From   : constant Editable_Line_Type := Self.From_Line + Offset;
      To     : constant Editable_Line_Type :=
        Editable_Line_Type'Min (From + Self.Lines_Per_Page - 1, Self.To_Line);
   begin
      declare
         use type GNATCOLL.VFS.Filesystem_String;
         File       : constant String := +Self.Editor.Get_Filename.Base_Name;
         Layout     : constant Pango.Layout.Pango_Layout :=
           Context.Create_Pango_Layout;
      begin
         Cairo.Move_To (CC, 0.0, Glib.Gdouble (Self.Line_Height));
         Layout.Set_Text
           (File & " (" & Editable_Line_Type'Image (From) &
            " .." & Editable_Line_Type'Image (To) & " )");
         Pango.Cairo.Show_Layout (CC, Layout);
         Layout.Unref;
      end;

      for J in From .. To loop
         declare
            Index   : String (1 .. 5);
            Content : constant String := Text_Handling.Get_Chars (Buffer, J);
            Layout  : constant Pango.Layout.Pango_Layout :=
              Context.Create_Pango_Layout;
         begin
            Line_Number_IO.Put (Index, J);
            Cairo.Move_To
              (CC, 0.0,
               Glib.Gdouble (Self.Line_Height) * Glib.Gdouble (J - From + 4));
            Layout.Set_Font_Description (Self.Font);
            Layout.Set_Text (Index & ": " & Content);
            Pango.Cairo.Show_Layout (CC, Layout);
            Layout.Unref;
         end;
      end loop;
   end Draw_Page;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (This       : Printer;
      Editor     : Src_Editor_Box.Source_Editor_Box;
      From       : Editable_Line_Type := 1;
      To         : Editable_Line_Type := Editable_Line_Type'Last)
   is
      pragma Unreferenced (This);

      Printer : constant GPS_Print_Operation := new GPS_Print_Operation_Record;
      Ignore  : Gtk.Print_Operation.Gtk_Print_Operation_Result;
      pragma Unreferenced (Ignore);

   begin
      Gtkada.Printing.Initialize (Printer);
      Printer.Editor := Editor;
      Printer.From_Line := From;
      Printer.To_Line := Editable_Line_Type'Min
        (To, Editable_Line_Type (Editor.Get_Last_Line));
      Printer.Font := GPS.Kernel.Preferences.View_Fixed_Font.Get_Pref;

      Ignore := Gtkada.Printing.Connect_And_Run
        (Printer,
         Gtk.Print_Operation.Action_Print_Dialog,
         Editor.Get_Kernel.Get_Main_Window);
   end Print;

   ------------
   -- Create --
   ------------

   function Create return Printer is
   begin
      return (Abstract_Printer with null record);
   end Create;

end Src_Printing.Gtk_Printer;
