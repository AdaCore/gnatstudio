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

--  This is a binding of the CscHTML widget, which is a port of the GtkHTML
--  widget with all GNOME dependencies removed.
--
--  This widget provides an HTML renderer, allowing displaying and editing
--  of HTML pages.

with Glib;
with Gdk.Color;
with Gtk.Widget;
with Gtk.Layout;

package Csc_HTML_Widget is

   type Csc_HTML_Record is new Gtk.Layout.Gtk_Layout_Record with private;
   type Csc_HTML is access all Csc_HTML_Record'Class;

   type Csc_HTML_Stream is new Glib.C_Proxy;

   type HTML_Engine is new Glib.C_Proxy;

   type Stream_Status is (Stream_OK, Stream_Error);

   type Paragraph_Style is
     (Paragraph_Style_Normal,
      Paragraph_Style_H1,
      Paragraph_Style_H2,
      Paragraph_Style_H3,
      Paragraph_Style_H4,
      Paragraph_Style_H5,
      Paragraph_Style_H6,
      Paragraph_Style_Address,
      Paragraph_Style_Pre,
      Paragraph_Style_Itemdotted,
      Paragraph_Style_Itemroman,
      Paragraph_Style_Itemdigit);

   type Paragraph_Alignment is
     (Paragraph_Alignment_Left,
      Paragraph_Alignment_Right,
      Paragraph_Alignment_Center);

   type Font_Style is new Glib.Guint;

   Font_Style_Default   : constant Font_Style := 0;
   Font_Style_Size_1    : constant Font_Style := 1;
   Font_Style_Size_2    : constant Font_Style := 2;
   Font_Style_Size_3    : constant Font_Style := 3;
   Font_Style_Size_4    : constant Font_Style := 4;
   Font_Style_Size_5    : constant Font_Style := 5;
   Font_Style_Size_6    : constant Font_Style := 6;
   Font_Style_Size_7    : constant Font_Style := 7;
   Font_Style_Size_Mask : constant Font_Style := 16#7#;
   Font_Style_Bold      : constant Font_Style := 2 ** 3;
   Font_Style_Italic    : constant Font_Style := 2 ** 4;
   Font_Style_Underline : constant Font_Style := 2 ** 5;
   Font_Style_Strikeout : constant Font_Style := 2 ** 6;
   Font_Style_Fixed     : constant Font_Style := 2 ** 7;

   Font_Style_Size_Max  : constant := 7;

   --------------
   -- Creation --
   --------------

   function Get_Type return Glib.GType;
   --  Return the internal type associated with Csc_HTML.

   procedure Gtk_New (HTML : out Csc_HTML);
   --  Create a new HTML renderer widget.

   procedure Initialize (HTML : access Csc_HTML_Record'Class);
   --  Internal initialization function.

   function Get_Engine
     (HTML : access Csc_HTML_Record) return HTML_Engine;
   --  Return the underlying HTML engine associated with HTML.
   --  See routines below to handle searches with the engine.

   ---------------
   -- Debugging --
   ---------------

   procedure Enable_Debug
     (HTML : access Csc_HTML_Record; Debug : Boolean := True);
   --  Enable/disable debugging traces of HTML.

   --------------
   -- Behavior --
   --------------

   procedure Allow_Selection
     (HTML : access Csc_HTML_Record; Allow : Boolean := True);
   --  Allow/Disallow selection on HTML.
   --  HTML allows selection by default.

   function Request_Paste
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Time   : Glib.Gint32) return Glib.Gint;

   ---------------------------------
   -- Font switching and defaults --
   ---------------------------------

   procedure Allow_Font_Switch
     (HTML : access Csc_HTML_Record; Allow : Boolean := True);

   procedure Set_Default_Font_Face
     (HTML : access Csc_HTML_Record; Face : String);

   function Get_Default_Font_Face
     (HTML : access Csc_HTML_Record) return String;

   -------------
   -- Loading --
   -------------

   function HTML_Begin (HTML : access Csc_HTML_Record) return Csc_HTML_Stream;
   --  Start loading of a page.
   --  The stream returned can then be used in the functions below.

   procedure HTML_Write
     (HTML   : access Csc_HTML_Record;
      Handle : Csc_HTML_Stream;
      Buffer : String);
   --  Display buffer contents (an HTML chunk) in HTML, using the given stream.

   procedure HTML_End
     (HTML   : access Csc_HTML_Record;
      Handle : Csc_HTML_Stream;
      Status : Stream_Status);
   --  Notify the end of an HTML stream.

   procedure Load_Empty (HTML : access Csc_HTML_Record);
   --  Load an empty page.
   --  Useful to reset a widget.

   ------------
   -- Saving --
   ------------

   --  function HTML_Save
   --    (HTML     : access Csc_HTML_Record;
   --     Receiver : Csc_HTML_Save_Receiver_Fn;
   --     Data     : System.Address) return Boolean;

   --  function HTML_Export
   --    (HTML     : access Csc_HTML_Record;
   --     The_Type : String;
   --     Receiver : Csc_HTML_Save_Receiver_Fn;
   --     Data     : System.Address) return Boolean;

   ----------------------------------------------------
   -- Streams for feeding the widget with extra data --
   -- (e.g. images) at loading time.                 --
   ----------------------------------------------------

   function Stream_Ref (Handle : Csc_HTML_Stream) return Csc_HTML_Stream;

   procedure Stream_Unref (Handle : Csc_HTML_Stream);

   procedure Stream_Write
     (Stream : Csc_HTML_Stream;
      Buffer : String);
   --  Send contents (e.g an image) to Stream.
   --  This is typically used in a handler for the "url_requested" signal,
   --  see below.

   procedure Stream_Close
     (Stream : Csc_HTML_Stream;
      Status : Stream_Status);
   --  Close the stream. This is usually done automatically.

   ----------------------
   -- Editable support --
   ----------------------

   procedure Set_Editable
     (HTML     : access Csc_HTML_Record;
      Editable : Boolean := True);
   --  Set/Unset the editable property of the HTML widget.
   --  HTML is created non editable by default.

   function Get_Editable (HTML : access Csc_HTML_Record) return Boolean;
   --  Return whether the HTML widget is editable.

   -----------
   -- Title --
   -----------

   function Get_Title (HTML : access Csc_HTML_Record) return String;
   --  Return the current title associated with HTML.

   -------------
   -- Anchors --
   -------------

   function Jump_To_Anchor
     (HTML   : access Csc_HTML_Record;
      Anchor : String) return Boolean;
   --  Jump to a specific anchor in the currently displayed page.
   --  The anchor is the right part of the url, after the '#' character.
   --  For example, for url "file.html#ref1", the anchor would be "ref1".

   -----------------------
   -- Editing functions --
   -----------------------

   procedure Set_Paragraph_Style
     (HTML  : access Csc_HTML_Record;
      Style : Paragraph_Style);

   procedure Indent
     (HTML        : access Csc_HTML_Record;
      Delta_Value : Glib.Gint);

   procedure Set_Font_Style
     (HTML     : access Csc_HTML_Record;
      And_Mask : Font_Style;
      Or_Mask  : Font_Style);

   procedure Align_Paragraph
     (HTML      : access Csc_HTML_Record;
      Alignment : Paragraph_Alignment);

   procedure Cut (HTML : access Csc_HTML_Record);
   --  Cut the current selection in HTML.

   procedure Copy (HTML : access Csc_HTML_Record);
   --  Copy the current selection in HTML.

   procedure Paste (HTML : access Csc_HTML_Record);
   --  Paste the current selection in HTML.

   procedure Undo (HTML : access Csc_HTML_Record);
   --  Undo the last editing.

   procedure Redo (HTML : access Csc_HTML_Record);
   --  Redo the last editing.

   ----------------
   -- Misc Utils --
   ----------------

   procedure Set_Default_Background_Color
     (HTML : access Csc_HTML_Record; C : Gdk.Color.Gdk_Color);
   --  Set the default background color of HTML.
   --  Useful if none is specified in the page rendered.

   -----------------
   -- HTML_Engine --
   -----------------

   function Frozen (Engine : HTML_Engine) return Boolean;
   --  Indicate whether Engine is frozen.

   procedure Freeze (Engine : HTML_Engine);
   --  Freeze engine.

   procedure Thaw (Engine : HTML_Engine);
   --  Thaw engine.

   function Search
     (Engine         : HTML_Engine;
      Text           : String;
      Case_Sensitive : Boolean := True;
      Forward        : Boolean := True;
      Regular        : Boolean := False) return Boolean;

   function Search_Next (Engine : HTML_Engine) return Boolean;
   --  Search next occurence.

   function Search_Incremental (Engine : HTML_Engine) return Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "title_changed"
   --    procedure Handler
   --      (Widget    : access Csc_HTML_Record'Class;
   --       New_Title : String);
   --
   --    The title of the HTML page has been set/changed
   --
   --  - "url_requested"
   --    procedure Handler
   --      (Widget : access Csc_HTML_Record'Class;
   --       Url    : String;
   --       Handle : out Csc_HTML_Stream);
   --
   --    A URL has been requested while loading a page.
   --
   --  - "load_done"
   --    procedure Handler (Widget : access Csc_HTML_Record'Class);
   --
   --    A page has been entirely loaded.
   --
   --  - "link_clicked"
   --    procedure Handler
   --      (Widget : access Csc_HTML_Record'Class;
   --       Url    : String);
   --
   --    A link has been clicked on the page.
   --
   --  - "set_base"
   --    procedure Handler
   --      (Widget   : access Csc_HTML_Record'Class;
   --       Base_Url : String);
   --
   --  - "set_base_target"
   --    procedure Handler
   --      (Widget   : access Csc_HTML_Record'Class;
   --       Base_Url : String);
   --
   --  - "on_url"
   --    procedure Handler
   --      (Widget : access Csc_HTML_Record'Class;
   --       Url    : String);
   --
   --    The mouse is over a link.
   --    If Url is null, it means that the mouse just left a link.
   --
   --  - "redirect"
   --    procedure Handler
   --      (Widget : access Csc_HTML_Record'Class;
   --       Url    : String;
   --       Delay  : Integer);
   --
   --  - "submit"
   --    procedure Handler
   --      (Widget   : access Csc_HTML_Record'Class;
   --       Method   : String;
   --       Url      : String;
   --       Encoding : String);
   --
   --  - "object_requested"
   --    function Handler
   --      (Widget : access Csc_HTML_Record'Class;
   --       Arg_1  : out Csc_HTML_Embedded)
   --       return Boolean;
   --
   --  - "current_paragraph_style_changed"
   --    procedure Handler
   --      (Widget    : access Csc_HTML_Record'Class;
   --       New_Style : Csc_HTML_Paragraph_Style);
   --
   --  - "current_paragraph_alignment_changed"
   --    procedure Handler
   --      (Widget        : access Csc_HTML_Record'Class;
   --       New_Alignment : Csc_HTML_Paragraph_Alignment);
   --
   --  - "current_paragraph_indentation_changed"
   --    procedure Handler
   --      (Widget          : access Csc_HTML_Record'Class;
   --       New_Indentation : Guint);
   --
   --  - "insertion_font_style_changed"
   --    procedure Handler
   --      (Widget : access Csc_HTML_Record'Class;
   --       Style  : Csc_HTML_Font_Style);
   --
   --  </signals>

private
   type Csc_HTML_Record is new Gtk.Layout.Gtk_Layout_Record with null record;

   pragma Import (C, Get_Type, "csc_html_get_type");
   pragma Import (C, Stream_Ref, "csc_html_stream_ref");
   pragma Import (C, Stream_Unref, "csc_html_stream_unref");
   pragma Import (C, Stream_Close, "csc_html_stream_close");
   pragma Import (C, Freeze, "html_engine_freeze");
   pragma Import (C, Thaw, "html_engine_thaw");

   pragma Convention (C, Stream_Status);
   pragma Convention (C, Paragraph_Style);
   pragma Convention (C, Paragraph_Alignment);
end Csc_HTML_Widget;
