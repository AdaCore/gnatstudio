-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2009, AdaCore                  --
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

with Ada.Calendar;           use Ada, Ada.Calendar;

with GNAT.Calendar;          use GNAT.Calendar;
with GNAT.Calendar.Time_IO;  use GNAT.Calendar.Time_IO;
with GNAT.IO;                use GNAT.IO;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with Glib.Object;            use Glib.Object;
with XML_Utils;              use XML_Utils;

with Gtk.Enums;              use Gtk.Enums;
with Gtk.Object;             use Gtk.Object;
with Gtk.Widget;             use Gtk.Widget;

with Gtkada.File_Selector;   use Gtkada.File_Selector;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

with Config;                 use Config;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Locations;   use GPS.Kernel.Locations;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Histories;              use Histories;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;

package body GPS.Kernel.Console is

   type GPS_Message_Record is new Interactive_Console_Record with null record;
   type GPS_Message is access GPS_Message_Record'Class;
   --  Type for the messages window. This is mostly use to have a unique tag
   --  for this console, so that we can save it in the desktop

   type Console_Module_Id_Record is new Module_ID_Record with record
      Console : GPS_Message;
      Child   : MDI_Child;
   end record;

   type Console_Module_Id_Access is access all Console_Module_Id_Record'Class;

   overriding procedure Destroy (Module : in out Console_Module_Id_Record);
   --  Called when the module is destroyed

   Console_Module_Id   : Console_Module_Id_Access;
   Console_Module_Name : constant String := "GPS.Kernel.Console";

   Me : constant Debug_Handle := Traces.Create (Console_Module_Name);

   type GPS_Console_MDI_Child_Record is
     new GPS_MDI_Child_Record with null record;
   overriding function Interrupt
     (Child : access GPS_Console_MDI_Child_Record) return Boolean;

   procedure Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Called when the console has been destroyed

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

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure Insert
     (Kernel : access Kernel_Handle_Record'Class;
      Text   : String;
      UTF8   : Boolean;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   --  Factor code between Insert_Non_UTF8 and Insert_UTF8

   ---------------
   -- Interrupt --
   ---------------

   overriding function Interrupt
     (Child : access GPS_Console_MDI_Child_Record) return Boolean
   is
      Console : constant Interactive_Console :=
                  Interactive_Console (Get_Widget (Child));
   begin
      return Interrupt (Console);
   end Interrupt;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class) return Interactive_Console
   is
      pragma Unreferenced (Kernel);
   begin
      if Console_Module_Id = null then
         return null;
      else
         return Interactive_Console (Console_Module_Id.Console);
      end if;
   end Get_Console;

   -----------
   -- Clear --
   -----------

   procedure Clear (Kernel : access Kernel_Handle_Record'Class) is
      Console : constant Interactive_Console := Get_Console (Kernel);
   begin
      if Console /= null then
         Clear (Console);
      end if;
   end Clear;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel : access Kernel_Handle_Record'Class;
      Text   : String;
      UTF8   : Boolean;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info)
   is
      Console : constant Interactive_Console := Get_Console (Kernel);
      T       : constant Calendar.Time := Calendar.Clock;
   begin
      if Console = null then
         Put_Line (Text);

      elsif Text /= "" then
         if Mode = Error then
            Trace (Me, Text);
            if UTF8 then
               Insert_UTF8
                 (Console, "[" & Image (T, ISO_Date & " %T") & "] " & Text,
                  Add_LF, Mode = Error);
            else
               Insert
                 (Console, "[" & Image (T, ISO_Date & " %T") & "] " & Text,
                  Add_LF, Mode = Error);
            end if;

            Raise_Console (Kernel);

         else
            if UTF8 then
               Insert_UTF8 (Console, Text, Add_LF, Mode = Error);
            else
               Insert (Console, Text, Add_LF, Mode = Error);
            end if;
            Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), Console));
         end if;
      end if;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel : access Kernel_Handle_Record'Class;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      Insert (Kernel, Text, False, Add_LF, Mode);
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   procedure Insert_UTF8
     (Kernel : access Kernel_Handle_Record'Class;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      Insert (Kernel, UTF8, True, Add_LF, Mode);
   end Insert_UTF8;

   -------------------
   -- Raise_Console --
   -------------------

   procedure Raise_Console (Kernel : access Kernel_Handle_Record'Class) is
      Console : constant Interactive_Console := Get_Console (Kernel);
   begin
      if Console_Module_Id.Child /= null then
         Raise_Child (Console_Module_Id.Child, Give_Focus => False);
         Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), Console), False);
      end if;
   end Raise_Console;

   -----------------------
   -- Console_Destroyed --
   -----------------------

   procedure Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Console, Kernel);
   begin
      Console_Module_Id.Console := null;
      Console_Module_Id.Child := null;
   end Console_Destroyed;

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
           (Kernel            => Kernel,
            Text              => Contents (Contents'First .. Last),
            Category          => -"Loaded contents",
            Highlight         => True,
            Remove_Duplicates => True);
      else
         Insert (Get_Console (Kernel), Contents.all);
         Parse_File_Locations_Unknown_Encoding
           (Kernel            => Kernel,
            Text              => Contents.all,
            Category          => -"Loaded contents",
            Highlight         => True,
            Remove_Duplicates => True);
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
   begin
      Clear (Kernel);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Clear_Console;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Console_Module_Id_Record) is
   begin
      if Module.Console /= null then
         Destroy (Module.Console);
         Module.Console := null;
      end if;

      --  Destroyed has been called as GPS is in the way to quit. We want to
      --  invalidate the module access as it will be freed just after this
      --  call. We want to do that to be sure that every call to Get_Console
      --  will return null at this point.
      Console_Module_Id := null;
   end Destroy;

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

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Console : GPS_Message;
      Child   : GPS_MDI_Child;

   begin
      --  ??? Using an interactive_console seems overkill, since the user
      --  cannot write in the messages window
      Console := new GPS_Message_Record;
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
      Enable_Prompt_Display (Console, False);

      Set_Font_And_Colors (Get_View (Console), Fixed_Font => True);

      Gtk_New (Child, Console,
               Default_Width       => 400,
               Default_Height      => 120,
               Group               => Group_Consoles,
               Focus_Widget        => Gtk_Widget (Get_View (Console)),
               Flags               => 0,
               Module              => Console_Module_Id,
               Desktop_Independent => True);
      Set_Title (Child, -"Messages");
      Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);
      Set_Focus_Child (Child);
      Raise_Child (Child);

      Console_Module_Id.Child   := MDI_Child (Child);
      Console_Module_Id.Console := Console;

      Add_Hook (Kernel, "preferences_changed",
                Wrapper (On_Preferences_Changed'Access),
                Name => "console.preferences_changed",
                Watch => GObject (Console));

      Kernel_Callback.Connect
        (Console, Signal_Destroy,
         Console_Destroyed'Access, Kernel_Handle (Kernel));
      Return_Callback.Connect
        (Console, Gtk.Widget.Signal_Delete_Event, Console_Delete_Event'Access);
   end Initialize_Console;

   --------------------------------
   -- Create_Interactive_Console --
   --------------------------------

   function Create_Interactive_Console
     (Kernel              : access Kernel_Handle_Record'Class;
      Title               : String := "";
      History             : History_Key := "interactive";
      Create_If_Not_Exist : Boolean := True;
      Module              : GPS.Kernel.Abstract_Module_ID := null;
      Force_Create        : Boolean := False;
      Accept_Input        : Boolean := True;
      ANSI_Support        : Boolean := False;
      Manage_Prompt       : Boolean := True) return Interactive_Console
   is
      Console : Interactive_Console;
      Child   : MDI_Child;
      NChild  : GPS_MDI_Child;
      Create  : Boolean;
   begin
      if Title /= "" and then Title /= "Messages" then
         Create := Force_Create;
         if not Create then
            Child := Find_MDI_Child_By_Name (Get_MDI (Kernel), Title);
            Create := (Child = null
                       or else Get_Widget (Child).all not in
                         Interactive_Console_Record'Class)
                and then Create_If_Not_Exist;
         end if;

         if Create then
            Gtk_New
              (Console, "", null,
               System.Null_Address,
               History_List => Get_History (Kernel),
               Key          => History,
               Wrap_Mode    => Wrap_Char,
               Manage_Prompt => Manage_Prompt,
               ANSI_Support => ANSI_Support,
               Highlight    => Message_Highlight.Get_Pref);
            Set_Font_And_Colors (Get_View (Console), Fixed_Font => False);
            Set_Max_Length   (Get_History (Kernel).all, 100, History);
            Allow_Duplicates (Get_History (Kernel).all, History, True, True);

            NChild := new GPS_Console_MDI_Child_Record;

            if Module /= null then
               GPS.Kernel.MDI.Initialize
                 (NChild, Console,
                  Group               => Group_Consoles,
                  Focus_Widget        => Gtk_Widget (Get_View (Console)),
                  Module              => Module_ID (Module),
                  Desktop_Independent => True);

            else
               GPS.Kernel.MDI.Initialize
                 (NChild, Console,
                  Group               => Group_Consoles,
                  Focus_Widget        => Gtk_Widget (Get_View (Console)),
                  Module              => null,
                  Desktop_Independent => False);
            end if;

            Set_Title (NChild, Title, Title);
            Put
              (Get_MDI (Kernel), NChild, Initial_Position => Position_Bottom);
            Raise_Child (NChild);

            if Console /= null then
               Enable_Prompt_Display (Console, Accept_Input);
            end if;

         elsif Child /= null then
            Highlight_Child (Child);
            Console := Interactive_Console (Get_Widget (Child));
            Enable_Prompt_Display (Console, Accept_Input);
         end if;

         return Console;

      else
         Highlight_Child (Console_Module_Id.Child);
         return Get_Console (Kernel);
      end if;
   end Create_Interactive_Console;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Message_Window" then
         if Console_Module_Id.Console = null then
            Initialize_Console (User);
            return Console_Module_Id.Child;
         else
            return Console_Module_Id.Child;
         end if;
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in GPS_Message_Record'Class then
         N := new Node;
         N.Tag := new String'("Message_Window");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File    : constant String := '/' & (-"File");
      Console : constant String := File & '/' & (-"Messa_ges");
   begin
      Console_Module_Id := new Console_Module_Id_Record;
      Register_Module
        (Module       => Module_ID (Console_Module_Id),
         Kernel       => Kernel,
         Module_Name  => Console_Module_Name,
         Priority     => Default_Priority);
      GPS.Kernel.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Initialize_Console (Kernel);

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
