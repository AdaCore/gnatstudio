------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada.Calendar;           use Ada, Ada.Calendar;

with GNAT.Calendar;          use GNAT.Calendar;
with GNAT.Calendar.Time_IO;  use GNAT.Calendar.Time_IO;
with GNAT.IO;                use GNAT.IO;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with Glib.Object;            use Glib.Object;

with Gtk.Enums;              use Gtk.Enums;
with Gtk.Widget;             use Gtk.Widget;

with Gtkada.File_Selector;   use Gtkada.File_Selector;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

with Config;                 use Config;
with Generic_Views;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Tools_Output;  use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Histories;              use Histories;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;

package body GPS.Kernel.Console is
   Me : constant Debug_Handle := Traces.Create ("CONSOLE");

   type GPS_Message_Record is new Interactive_Console_Record with null record;
   --  Type for the messages window. This is mostly use to have a unique tag
   --  for this console, so that we can save it in the desktop

   function Initialize
     (Console : access GPS_Message_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class) return Gtk_Widget;
   --  Initialize the messages window, and return the focus widget.

   package Messages_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Message_Window",
      View_Name          => -"Messages",
      Formal_View_Record => GPS_Message_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => False,
      Local_Config       => False,
      Group              => Group_Consoles);
   use Messages_Views;
   subtype GPS_Message is Messages_Views.View_Access;

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Prevent the destruction of the console in the MDI

   procedure On_Save_Console_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Messages->Save As... menu

   procedure On_Load_To_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Messages->Load Contents... menu

   procedure On_Clear_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Messages->Clear menu

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   type Kernel_Messages_Window is new Abstract_Messages_Window with record
      Kernel : Kernel_Handle;
   end record;
   overriding procedure Insert
     (Self   : not null access Kernel_Messages_Window;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   overriding procedure Insert_UTF8
     (Self   : not null access Kernel_Messages_Window;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   overriding procedure Raise_Console
     (Self   : not null access Kernel_Messages_Window);
   overriding procedure Clear
     (Self   : not null access Kernel_Messages_Window);
   overriding function Get_Virtual_Console
     (Self : not null access Kernel_Messages_Window)
      return GNATCOLL.Scripts.Virtual_Console;

   procedure Internal_Insert
     (Self   : not null access Kernel_Messages_Window'Class;
      Text   : String;
      UTF8   : Boolean;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   --  Factor code between Insert_Non_UTF8 and Insert_UTF8

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class) return Interactive_Console
   is
   begin
      return Interactive_Console (Messages_Views.Retrieve_View (Kernel));
   end Get_Console;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self   : not null access Kernel_Messages_Window)
   is
      Console : constant Interactive_Console := Get_Console (Self.Kernel);
   begin
      if Console /= null then
         Clear (Console);
      end if;
   end Clear;

   -------------------------
   -- Get_Virtual_Console --
   -------------------------

   overriding function Get_Virtual_Console
     (Self : not null access Kernel_Messages_Window)
      return GNATCOLL.Scripts.Virtual_Console
   is
      Console : constant Interactive_Console := Get_Console (Self.Kernel);
   begin
      if Console /= null then
         return Get_Or_Create_Virtual_Console (Console);
      else
         return null;
      end if;
   end Get_Virtual_Console;

   ---------------------
   -- Internal_Insert --
   ---------------------

   procedure Internal_Insert
     (Self   : not null access Kernel_Messages_Window'Class;
      Text   : String;
      UTF8   : Boolean;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info)
   is
      Console : constant Interactive_Console := Get_Console (Self.Kernel);
      T       : constant Calendar.Time := Calendar.Clock;
   begin
      if Console = null then
         Put_Line (Text);

      elsif Text /= "" then
         if Mode = Error then
            if Active (Me) then
               Trace (Me, Text);
            end if;

            if UTF8 then
               Insert_UTF8
                 (Console, "[" & Image (T, ISO_Date & " %T") & "] " & Text,
                  Add_LF, Mode = Error);
            else
               Insert
                 (Console, "[" & Image (T, ISO_Date & " %T") & "] " & Text,
                  Add_LF, Mode = Error);
            end if;

            Self.Raise_Console;

         else
            if UTF8 then
               Insert_UTF8 (Console, Text, Add_LF, Mode = Error);
            else
               Insert (Console, Text, Add_LF, Mode = Error);
            end if;
            Highlight_Child (Find_MDI_Child (Get_MDI (Self.Kernel), Console));
         end if;
      end if;
   end Internal_Insert;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : not null access Kernel_Messages_Window;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      Internal_Insert (Self, Text, False, Add_LF, Mode);
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   overriding procedure Insert_UTF8
     (Self   : not null access Kernel_Messages_Window;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      Internal_Insert (Self, UTF8, True, Add_LF, Mode);
   end Insert_UTF8;

   -------------------
   -- Raise_Console --
   -------------------

   overriding procedure Raise_Console
     (Self   : not null access Kernel_Messages_Window)
   is
      View : constant GPS_Message :=
        Messages_Views.Retrieve_View (Self.Kernel);
   begin
      if View /= null then
         Messages_Views.Child_From_View (Self.Kernel, View).Raise_Child
           (Give_Focus => False);
      end if;
   end Raise_Console;

   --------------------------
   -- Console_Delete_Event --
   --------------------------

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Console);
   begin
      return True;
   end Console_Delete_Event;

   ------------------------
   -- On_Save_Console_As --
   ------------------------

   procedure On_Save_Console_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Console : constant Interactive_Console := Get_Console (Kernel);
      Len     : Integer;
      pragma Unreferenced (Widget, Len);

   begin
      declare
         File : constant Virtual_File :=
                  Select_File
                    (Title             => -"Save messages window as",
                     Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                     Kind              => Save_File,
                     Parent            => Get_Current_Window (Kernel),
                     History           => Get_History (Kernel));
      begin
         if File = GNATCOLL.VFS.No_File then
            return;
         end if;

         declare
            Contents : constant String := Get_Chars (Console);
            WF       : Writable_File;
         begin
            WF := File.Write_File;
            Write (WF, Contents);
            Close (WF);
         end;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Save_Console_As;

   ------------------------
   -- On_Load_To_Console --
   ------------------------

   procedure On_Load_To_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Contents : String_Access;
      Last     : Natural;
      CR_Found : Boolean;
      File     : Virtual_File;

   begin
      File := Select_File
        (Title             => -"Select file to load in the messages window",
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Kind              => Open_File,
         Parent            => Get_Current_Window (Kernel),
         History           => Get_History (Kernel));

      if File = GNATCOLL.VFS.No_File then
         return;
      end if;

      Contents := Read_File (File);
      Strip_CR (Contents.all, Last, CR_Found);

      if CR_Found then
         Insert (Get_Console (Kernel), Contents (Contents'First .. Last));
         Parse_File_Locations_Unknown_Encoding
           (Kernel    => Kernel,
            Text      => Contents (Contents'First .. Last),
            Category  => -"Loaded contents",
            Highlight => True);
      else
         Insert (Get_Console (Kernel), Contents.all);
         Parse_File_Locations_Unknown_Encoding
           (Kernel    => Kernel,
            Text      => Contents.all,
            Category  => -"Loaded contents",
            Highlight => True);
      end if;

      Free (Contents);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_To_Console;

   ----------------------
   -- On_Clear_Console --
   ----------------------

   procedure On_Clear_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Console : constant Interactive_Console := Get_Console (Kernel);
   begin
      if Console /= null then
         Clear (Console);
      end if;
   end On_Clear_Console;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Console : constant Interactive_Console := Get_Console (Kernel);
   begin
      if Console /= null then
         Set_Font_And_Colors (Get_View (Console), Fixed_Font => True);
      end if;
   end On_Preferences_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Console : access GPS_Message_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
   begin
      Initialize
        (Console,
         "",
         null,
         Kernel.all'Address,
         Highlight    => Message_Highlight.Get_Pref,
         History_List => null,
         ANSI_Support => Host /= Windows, --  ANSI_Support does not work
                                          --  well under Windows ???
         Key          => "",
         Wrap_Mode    => Wrap_Char);
      Console.Enable_Prompt_Display (False);
      Set_Font_And_Colors (Console.Get_View, Fixed_Font => True);

      Add_Hook (Kernel, To_Hook_Name ("preferences_changed"),
                Wrapper (On_Preferences_Changed'Access),
                Name => "console.preferences_changed",
                Watch => GObject (Console));

      Return_Callback.Connect
        (Console, Gtk.Widget.Signal_Delete_Event, Console_Delete_Event'Access);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Get_View (Console),
         Object          => Console,
         ID              => Messages_Views.Get_Module,
         Context_Func    => null);
      return Gtk_Widget (Console.Get_View);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File    : constant String := '/' & (-"File");
      Console : constant String := File & '/' & (-"Messa_ges");
      Msg     : GPS_Message;
      Msg2    : constant access Kernel_Messages_Window :=
        new Kernel_Messages_Window;
      pragma Unreferenced (Msg);
   begin
      Messages_Views.Register_Module
        (Kernel, Menu_Name    => -"_Messages");

      Msg := Messages_Views.Get_Or_Create_View (Kernel);

      Msg2.Kernel := Kernel_Handle (Kernel);
      Kernel.Set_Messages_Window (Msg2);

      Register_Menu
        (Kernel, Console,
         Ref_Item => "Change Directory...", Add_Before => False);

      Register_Menu
        (Kernel, Console, -"_Clear", "", On_Clear_Console'Access);
      Register_Menu
        (Kernel, Console, -"_Save As...", "", On_Save_Console_As'Access);
      Register_Menu
        (Kernel, Console, -"_Load Contents...", "", On_Load_To_Console'Access);
   end Register_Module;

end GPS.Kernel.Console;
