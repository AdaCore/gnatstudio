-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtk.Widget; use Gtk.Widget;

package body Csc_HTML_Widget is

   ---------------------
   -- Align_Paragraph --
   ---------------------

   procedure Align_Paragraph
     (HTML      : access Csc_HTML_Record;
      Alignment : Paragraph_Alignment)
   is
      procedure Internal
        (HTML      : System.Address;
         Alignment : Paragraph_Alignment);
      pragma Import (C, Internal, "csc_html_align_paragraph");

   begin
      Internal (Get_Object (HTML), Alignment);
   end Align_Paragraph;

   -----------------------
   -- Allow_Font_Switch --
   -----------------------

   procedure Allow_Font_Switch
     (HTML : access Csc_HTML_Record;
      Allow : Boolean := True)
   is
      procedure Internal
        (HTML  : System.Address;
         Allow : Gboolean);
      pragma Import (C, Internal, "csc_html_allow_font_switch");

   begin
      Internal (Get_Object (HTML), To_Gboolean (Allow));
   end Allow_Font_Switch;

   ---------------------
   -- Allow_Selection --
   ---------------------

   procedure Allow_Selection
     (HTML : access Csc_HTML_Record;
      Allow : Boolean := True)
   is
      procedure Internal
        (HTML  : System.Address;
         Allow : Gboolean);
      pragma Import (C, Internal, "csc_html_allow_selection");

   begin
      Internal (Get_Object (HTML), To_Gboolean (Allow));
   end Allow_Selection;

   ----------
   -- Copy --
   ----------

   procedure Copy (HTML : access Csc_HTML_Record) is
      procedure Internal (HTML : System.Address);
      pragma Import (C, Internal, "csc_html_copy");

   begin
      Internal (Get_Object (HTML));
   end Copy;

   ---------
   -- Cut --
   ---------

   procedure Cut (HTML : access Csc_HTML_Record) is
      procedure Internal (HTML : System.Address);
      pragma Import (C, Internal, "csc_html_copy");

   begin
      Internal (Get_Object (HTML));
   end Cut;

   ------------------
   -- Enable_Debug --
   ------------------

   procedure Enable_Debug
     (HTML  : access Csc_HTML_Record;
      Debug : Boolean := True)
   is
      procedure Internal
        (HTML  : System.Address;
         Debug : Gboolean);
      pragma Import (C, Internal, "csc_html_enable_debug");

   begin
      Internal (Get_Object (HTML), To_Gboolean (Debug));
   end Enable_Debug;

   ------------
   -- Frozen --
   ------------

   function Frozen (Engine : HTML_Engine) return Boolean is
      function Internal (Engine : HTML_Engine) return Gboolean;
      pragma Import (C, Internal, "html_engine_frozen");

   begin
      return To_Boolean (Internal (Engine));
   end Frozen;

   ---------------------------
   -- Get_Default_Font_Face --
   ---------------------------

   function Get_Default_Font_Face
     (HTML : access Csc_HTML_Record) return String
   is
      function Internal (HTML : System.Address) return chars_ptr;
      pragma Import (C, Internal, "csc_html_get_default_font_face");

   begin
      return Value (Internal (Get_Object (HTML)));
   end Get_Default_Font_Face;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable (HTML : access Csc_HTML_Record) return Boolean is
      function Internal (HTML : System.Address) return Gboolean;
      pragma Import (C, Internal, "csc_html_get_editable");

   begin
      return To_Boolean (Internal (Get_Object (HTML)));
   end Get_Editable;

   ----------------
   -- Get_Engine --
   ----------------

   function Get_Engine
     (HTML : access Csc_HTML_Record) return HTML_Engine
   is
      function Internal (HTML : System.Address) return HTML_Engine;
      pragma Import (C, Internal, "csc_html_get_engine");

   begin
      return Internal (Get_Object (HTML));
   end Get_Engine;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (HTML : access Csc_HTML_Record) return String is
      function Internal (HTML : System.Address) return chars_ptr;
      pragma Import (C, Internal, "csc_html_get_title");

   begin
      return Value (Internal (Get_Object (HTML)));
   end Get_Title;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (HTML : out Csc_HTML) is
   begin
      HTML := new Csc_HTML_Record;
      Csc_HTML_Widget.Initialize (HTML);
   end Gtk_New;

   ----------------
   -- HTML_Begin --
   ----------------

   function HTML_Begin
     (HTML : access Csc_HTML_Record) return Csc_HTML_Stream
   is
      function Internal (HTML : System.Address) return Csc_HTML_Stream;
      pragma Import (C, Internal, "csc_html_begin");

   begin
      return Internal (Get_Object (HTML));
   end HTML_Begin;

   --------------
   -- HTML_End --
   --------------

   procedure HTML_End
     (HTML   : access Csc_HTML_Record;
      Handle : Csc_HTML_Stream;
      Status : Stream_Status)
   is
      procedure Internal
        (HTML   : System.Address;
         Handle : Csc_HTML_Stream;
         Status : Stream_Status);
      pragma Import (C, Internal, "csc_html_end");

   begin
      Internal (Get_Object (HTML), Handle, Status);
   end HTML_End;

   ----------------
   -- HTML_Write --
   ----------------

   procedure HTML_Write
     (HTML   : access Csc_HTML_Record;
      Handle : Csc_HTML_Stream;
      Buffer : String)
   is
      procedure Internal
        (HTML   : System.Address;
         Handle : Csc_HTML_Stream;
         Buffer : System.Address;
         Size   : size_t);
      pragma Import (C, Internal, "csc_html_write");

   begin
      Internal (Get_Object (HTML), Handle, Buffer'Address, Buffer'Length);
   end HTML_Write;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (HTML        : access Csc_HTML_Record;
      Delta_Value : Glib.Gint)
   is
      procedure Internal
        (HTML        : System.Address;
         Delta_Value : Glib.Gint);
      pragma Import (C, Internal, "csc_html_indent");

   begin
      Internal (Get_Object (HTML), Delta_Value);
   end Indent;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (HTML : access Csc_HTML_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "csc_html_new");

   begin
      Set_Object (HTML, Internal);
      Initialize_User_Data (HTML);
   end Initialize;

   --------------------
   -- Jump_To_Anchor --
   --------------------

   function Jump_To_Anchor
     (HTML   : access Csc_HTML_Record;
      Anchor : String) return Boolean
   is
      function Internal
        (HTML   : System.Address;
         Anchor : String) return Gboolean;
      pragma Import (C, Internal, "csc_html_jump_to_anchor");

   begin
      return To_Boolean (Internal (Get_Object (HTML), Anchor & ASCII.NUL));
   end Jump_To_Anchor;

   ----------------
   -- Load_Empty --
   ----------------

   procedure Load_Empty (HTML : access Csc_HTML_Record) is
      procedure Internal (HTML : System.Address);
      pragma Import (C, Internal, "csc_html_load_empty");

   begin
      Internal (Get_Object (HTML));
   end Load_Empty;

   -----------
   -- Paste --
   -----------

   procedure Paste (HTML : access Csc_HTML_Record) is
      procedure Internal (HTML : System.Address);
      pragma Import (C, Internal, "csc_html_paste");

   begin
      Internal (Get_Object (HTML));
   end Paste;

   ----------
   -- Redo --
   ----------

   procedure Redo (HTML : access Csc_HTML_Record) is
      procedure Internal (HTML : System.Address);
      pragma Import (C, Internal, "csc_html_redo");

   begin
      Internal (Get_Object (HTML));
   end Redo;

   -------------------
   -- Request_Paste --
   -------------------

   function Request_Paste
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Time   : Glib.Gint32) return Glib.Gint
   is
      function Internal
        (Widget : System.Address;
         Time   : Glib.Gint32) return Glib.Gint;
      pragma Import (C, Internal, "csc_html_request_paste");

   begin
      return Internal (Get_Object (Widget), Time);
   end Request_Paste;

   ------------
   -- Search --
   ------------

   function Search
     (Engine         : HTML_Engine;
      Text           : String;
      Case_Sensitive : Boolean := True;
      Forward        : Boolean := True;
      Regular        : Boolean := False) return Boolean
   is
      function Internal
        (Engine         : HTML_Engine;
         Text           : String;
         Case_Sensitive : Gboolean;
         Forward        : Gboolean;
         Regular        : Gboolean) return Gboolean;
      pragma Import (C, Internal, "html_engine_search");

   begin
      return To_Boolean
        (Internal
          (Engine,
           Text & ASCII.NUL,
           To_Gboolean (Case_Sensitive),
           To_Gboolean (Forward),
           To_Gboolean (Regular)));
   end Search;

   -----------------
   -- Search_Next --
   -----------------

   function Search_Next (Engine : HTML_Engine) return Boolean is
      function Internal (Engine : HTML_Engine) return Gboolean;
      pragma Import (C, Internal, "html_engine_search_next");

   begin
      return To_Boolean (Internal (Engine));
   end Search_Next;

   ------------------------
   -- Search_Incremental --
   ------------------------

   function Search_Incremental (Engine : HTML_Engine) return Boolean is
      function Internal (Engine : HTML_Engine) return Gboolean;
      pragma Import (C, Internal, "html_engine_search_incremental");

   begin
      return To_Boolean (Internal (Engine));
   end Search_Incremental;

   ----------------------------------
   -- Set_Default_Background_Color --
   ----------------------------------

   procedure Set_Default_Background_Color
     (HTML : access Csc_HTML_Record;
      C    : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (HTML : System.Address;
         C    : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "csc_html_set_default_background_color");

   begin
      Internal (Get_Object (HTML), C);
   end Set_Default_Background_Color;

   ---------------------------
   -- Set_Default_Font_Face --
   ---------------------------

   procedure Set_Default_Font_Face
     (HTML : access Csc_HTML_Record;
      Face : String)
   is
      procedure Internal
        (HTML : System.Address;
         Face : String);
      pragma Import (C, Internal, "csc_html_set_default_font_face");

   begin
      Internal (Get_Object (HTML), Face & ASCII.NUL);
   end Set_Default_Font_Face;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (HTML     : access Csc_HTML_Record;
      Editable : Boolean := True)
   is
      procedure Internal
        (HTML     : System.Address;
         Editable : Gboolean);
      pragma Import (C, Internal, "csc_html_set_editable");

   begin
      Internal (Get_Object (HTML), To_Gboolean (Editable));
   end Set_Editable;

   --------------------
   -- Set_Font_Style --
   --------------------

   procedure Set_Font_Style
     (HTML     : access Csc_HTML_Record;
      And_Mask : Font_Style;
      Or_Mask  : Font_Style)
   is
      procedure Internal
        (HTML     : System.Address;
         And_Mask : Font_Style;
         Or_Mask  : Font_Style);
      pragma Import (C, Internal, "csc_html_set_font_style");

   begin
      Internal (Get_Object (HTML), And_Mask, Or_Mask);
   end Set_Font_Style;

   -------------------------
   -- Set_Paragraph_Style --
   -------------------------

   procedure Set_Paragraph_Style
     (HTML  : access Csc_HTML_Record;
      Style : Paragraph_Style)
   is
      procedure Internal (HTML : System.Address; Style : Paragraph_Style);
      pragma Import (C, Internal, "csc_html_set_paragraph_style");

   begin
      Internal (Get_Object (HTML), Style);
   end Set_Paragraph_Style;

   ------------------
   -- Stream_Write --
   ------------------

   procedure Stream_Write
     (Stream : Csc_HTML_Stream;
      Buffer : String)
   is
      procedure Internal
        (Stream : Csc_HTML_Stream;
         Buffer : System.Address;
         Len    : size_t);
      pragma Import (C, Internal, "csc_html_stream_write");

   begin
      Internal (Stream, Buffer'Address, Buffer'Length);
   end Stream_Write;

   ----------
   -- Undo --
   ----------

   procedure Undo (HTML : access Csc_HTML_Record) is
      procedure Internal (HTML : System.Address);
      pragma Import (C, Internal, "csc_html_undo");

   begin
      Internal (Get_Object (HTML));
   end Undo;

end Csc_HTML_Widget;
