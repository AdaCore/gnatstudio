-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Values;               use Glib.Values;
with Csc_HTML_Widget;           use Csc_HTML_Widget;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtkada.MDI;                use Gtkada.MDI;
with Glide_Main_Window;         use Glide_Main_Window;
with Glide_Page;                use Glide_Page;
with GVD.Process;               use GVD.Process;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Glide_Intl;                use Glide_Intl;
with Traces;                    use Traces;
with OS_Utils;                  use OS_Utils;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

package body Glide_Kernel.Help is

   Me : Debug_Handle := Create ("Glide_Kernel.Help");

   --  ??? Preferences
   Default_Width  : constant := 400;
   Default_Height : constant := 400;

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      HTML   : access Csc_HTML_Record'Class;
      File   : String) return Boolean;
   --  Load File in HTML widget, and set File as the Current_Help_File for
   --  Kernel. Return True if the file could be successfully read.

   procedure Url_Requested
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the url_requested signal
   --  Called when loading a url as part of another page display (e.g an
   --  image).

   procedure On_Url
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the on_url signal

   procedure Link_Clicked
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the link_clicked signal

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      HTML   : access Csc_HTML_Record'Class;
      File   : String) return Boolean
   is
      Buffer   : String_Access;
      Stream   : Csc_HTML_Stream;

   begin
      Buffer := Read_File (File);

      if Buffer /= null then
         Trace (Me, "loading file: " & File);
         Free (Kernel.Current_Help_File);
         Kernel.Current_Help_File := new String' (File);
         Stream := HTML_Begin (HTML);
         HTML_Write (HTML, Stream, Buffer.all);
         HTML_End (HTML, Stream, Stream_OK);
         Free (Buffer);
         return True;

      else
         Trace (Me, "link not found");
         return False;
      end if;
   end Load_File;

   -------------------
   -- Url_Requested --
   -------------------

   procedure Url_Requested
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      Url      : constant String := Get_String (Nth (Params, 1));
      Stream   : Csc_HTML_Stream :=
        Csc_HTML_Stream (Get_Proxy (Nth (Params, 2)));
      Base_Dir : constant String := Dir_Name (Kernel.Current_Help_File.all);
      Buffer   : String_Access;

   begin
      Trace (Me, "url requested: " & Url);

      if Is_Absolute_Path (Url) then
         Buffer := Read_File (Url);
      else
         Buffer := Read_File (Base_Dir & Url);
         Trace (Me, "url normalized: " & Base_Dir & Url);
      end if;

      if Buffer /= null then
         Stream_Write (Stream, Buffer.all);
         Free (Buffer);
      end if;
   end Url_Requested;

   ------------
   -- On_Url --
   ------------

   procedure On_Url
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      Url : constant String := Get_String (Nth (Params, 1));
   begin
      Trace (Me, "on url: " & Url);
   end On_Url;

   ------------------
   -- Link_Clicked --
   ------------------

   procedure Link_Clicked
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      Url      : constant String := Get_String (Nth (Params, 1));
      Result   : Boolean := True;
      Anchor   : Natural := Index (Url, "#");
      Base_Dir : constant String := Dir_Name (Kernel.Current_Help_File.all);
      Basename : constant String := Base_Name (Kernel.Current_Help_File.all);

   begin
      if Anchor = 0 then
         Anchor := Url'Last + 1;
      end if;

      if Is_Absolute_Path (Url) then
         if Url /= Kernel.Current_Help_File.all then
            Result := Load_File
              (Kernel, Csc_HTML (Object), Url (Url'First .. Anchor - 1));
         end if;

      elsif Url /= Basename then
         Result := Load_File
           (Kernel, Csc_HTML (Object),
            Base_Dir & Url (Url'First .. Anchor - 1));
      end if;

      if Result and then Anchor < Url'Last then
         Trace (Me, "jumping to anchor " & Url (Anchor .. Url'Last));
         Result := Jump_To_Anchor
           (Csc_HTML (Object), Url (Anchor + 1 .. Url'Last));
      end if;
   end Link_Clicked;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : String)
   is
      HTML     : Csc_HTML;
      Top      : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      MDI      : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
      Scrolled : Gtk_Scrolled_Window;
      Child    : MDI_Child;
      Result   : Boolean;

      use Gtk.Widget;

   begin
      if Kernel.HTML_Widget = null then
         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Always);
         Gtk_New (HTML);
         Add (Scrolled, HTML);
         Kernel.HTML_Widget := Gtk_Widget (HTML);
         Child := Put (MDI, Scrolled);

         Set_Title (Child, -"Help");
         Set_Size_Request (Scrolled, Default_Width, Default_Height);
         Show_All (Scrolled);
         Kernel_Callback.Connect
           (HTML, "url_requested",
            Url_Requested'Access, Kernel_Handle (Kernel));
         Kernel_Callback.Connect
           (HTML, "link_clicked",
            Link_Clicked'Access, Kernel_Handle (Kernel));
         Kernel_Callback.Connect
           (HTML, "on_url",
            On_Url'Access, Kernel_Handle (Kernel));

      else
         HTML := Csc_HTML (Kernel.HTML_Widget);
      end if;

      Result := Load_File (Kernel, HTML, Help_File);
   end Display_Help;

   ------------
   -- Search --
   ------------

   function Search
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Case_Sensitive : Boolean := True;
      Forward        : Boolean := True;
      Regular        : Boolean := False) return Boolean is
   begin
      if Kernel.HTML_Widget = null then
         return False;
      end if;

      return Search
        (Get_Engine (Csc_HTML (Kernel.HTML_Widget)), Text,
         Case_Sensitive, Forward, Regular);
   end Search;

   -----------------
   -- Search_Next --
   -----------------

   function Search_Next
     (Kernel : access Kernel_Handle_Record'Class) return Boolean is
   begin
      if Kernel.HTML_Widget = null then
         return False;
      end if;

      return Search_Next (Get_Engine (Csc_HTML (Kernel.HTML_Widget)));
   end Search_Next;

end Glide_Kernel.Help;
