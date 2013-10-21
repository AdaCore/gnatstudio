------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with Glib.Object;            use Glib.Object;

with Gtk.Check_Menu_Item;    use Gtk.Check_Menu_Item;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Toolbar;            use Gtk.Toolbar;
with Gtk.Widget;             use Gtk.Widget;

with Gtkada.File_Selector;   use Gtkada.File_Selector;
with Gtkada.MDI;             use Gtkada.MDI;

with Commands.Interactive;   use Commands, Commands.Interactive;
with Config;                 use Config;
with Generic_Views;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Actions;     use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Tools_Output;  use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Stock_Icons;        use GPS.Stock_Icons;
with Histories;              use Histories;
with String_Utils;           use String_Utils;

package body GPS.Kernel.Console is
   Me : constant Trace_Handle := Create ("CONSOLE");

   Command_Clear_Messages_Name : constant String := "Messages clear";
   Command_Clear_Messages_Tip : constant String :=
     "Clear the contents of the Messages window";
   Command_Save_Name : constant String := "Messages save to file";
   Command_Save_Tip  : constant String :=
     "Save the contents of the messages window to a file";
   Command_Load_Name : constant String := "Messages load from file";
   Command_Load_Tip  : constant String :=
     "Loads the contents of a file into the Messages window, and process"
     & " locations into the Locations window.";

   History_Wrap_Lines : constant History_Key := "messages-wrap-line";

   type GPS_Message_Record is new Interactive_Console_Record with
      null record;
   --  Type for the messages window. This is mostly use to have a unique tag
   --  for this console, so that we can save it in the desktop

   overriding procedure Create_Toolbar
     (View    : not null access GPS_Message_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access GPS_Message_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   function Initialize
     (Console : access GPS_Message_Record'Class) return Gtk_Widget;
   --  Initialize the messages window, and return the focus widget.

   package Messages_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Message_Window",
      View_Name          => -"Messages",
      Formal_View_Record => GPS_Message_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => True,
      Local_Config       => True,
      MDI_Flags          => 0,  --  prevent explicit delete
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Consoles);
   use Messages_Views;
   subtype GPS_Message is Messages_Views.View_Access;

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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
     (Self       : not null access Kernel_Messages_Window;
      Give_Focus : Boolean);
   overriding procedure Clear
     (Self   : not null access Kernel_Messages_Window);
   overriding function Get_Virtual_Console
     (Self : not null access Kernel_Messages_Window)
      return GNATCOLL.Scripts.Virtual_Console;
   overriding function Get_Console_Window
     (Self : not null access Kernel_Messages_Window)
      return Gtk.Widget.Gtk_Widget;

   procedure Internal_Insert
     (Self   : not null access Kernel_Messages_Window'Class;
      Text   : String;
      UTF8   : Boolean;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   --  Factor code between Insert_Non_UTF8 and Insert_UTF8

   type Clear_Messages_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Clear_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Clear the contents of the messages window

   type Save_Messages_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Save_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Saves the contents of the message window in a file

   type Load_Messages_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Load_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Load the contents of a file into the Messages window

   procedure On_Toggle_Line_Wrap (Self : access GObject_Record'Class);
   --  Called when the user toggles line wrapping

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class;
      Create_If_Not_Exist : Boolean := True) return Interactive_Console
   is
   begin
      if Create_If_Not_Exist then
         return Interactive_Console
            (Messages_Views.Get_Or_Create_View (Kernel, Focus => False));
      else
         return Interactive_Console (Messages_Views.Retrieve_View (Kernel));
      end if;
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

   ------------------------
   -- Get_Console_Window --
   ------------------------

   overriding function Get_Console_Window
     (Self : not null access Kernel_Messages_Window)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Get_Console (Self.Kernel));
   end Get_Console_Window;

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
      use type Message_Type;
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

            Self.Raise_Console (Give_Focus => False);

         else
            if UTF8 then
               Insert_UTF8 (Console, Text, Add_LF, Mode = Error);
            else
               Insert (Console, Text, Add_LF, Mode = Error);
            end if;

            Messages_Views.Child_From_View
              (GPS_Message (Console)).Highlight_Child;
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
     (Self       : not null access Kernel_Messages_Window;
      Give_Focus : Boolean)
   is
      View : constant GPS_Message :=
        Messages_Views.Retrieve_View (Self.Kernel);
   begin
      if View /= null then
         Messages_Views.Child_From_View (View).Raise_Child (Give_Focus);
      end if;
   end Raise_Console;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Save_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      View : constant GPS_Message :=
        Messages_Views.Retrieve_View (Get_Kernel (Context.Context));
      Len  : Integer;
      pragma Unreferenced (Len);
   begin
      declare
         File : constant Virtual_File :=
                  Select_File
                    (Title             => -"Save messages window as",
                     Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                     Kind              => Save_File,
                     Parent            => Get_Current_Window (View.Kernel),
                     History           => Get_History (View.Kernel));
      begin
         if File = GNATCOLL.VFS.No_File then
            return Commands.Success;
         end if;

         declare
            Contents : constant String := Get_Chars (View);
            WF       : Writable_File;
         begin
            WF := File.Write_File;
            Write (WF, Contents);
            Close (WF);
         end;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Load_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      View : constant GPS_Message :=
        Messages_Views.Retrieve_View (Get_Kernel (Context.Context));
      Contents : String_Access;
      Last     : Natural;
      CR_Found : Boolean;
      File     : Virtual_File;
   begin
      File := Select_File
        (Title             => -"Select file to load in the messages window",
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Kind              => Open_File,
         Parent            => Get_Current_Window (View.Kernel),
         History           => Get_History (View.Kernel));

      if File = GNATCOLL.VFS.No_File then
         return Commands.Success;
      end if;

      Contents := Read_File (File);
      Strip_CR (Contents.all, Last, CR_Found);

      if CR_Found then
         Insert (View, Contents (Contents'First .. Last));
         Parse_File_Locations_Unknown_Encoding
           (Kernel    => View.Kernel,
            Text      => Contents (Contents'First .. Last),
            Category  => -"Loaded contents",
            Highlight => True);
      else
         Insert (View, Contents.all);
         Parse_File_Locations_Unknown_Encoding
           (Kernel    => View.Kernel,
            Text      => Contents.all,
            Category  => -"Loaded contents",
            Highlight => True);
      end if;

      Free (Contents);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Clear_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      View : constant GPS_Message :=
        Messages_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         Clear (View);
      end if;
      return Commands.Success;
   end Execute;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Console : constant Interactive_Console := Get_Console (Kernel);
   begin
      if Console /= null then
         Set_Font_And_Colors
           (Get_View (Console), Fixed_Font => True, Pref => Get_Pref (Data));
      end if;
   end On_Preferences_Changed;

   -------------------------
   -- On_Toggle_Line_Wrap --
   -------------------------

   procedure On_Toggle_Line_Wrap (Self : access GObject_Record'Class) is
      Console : constant GPS_Message := GPS_Message (Self);
   begin
      if Get_History
        (Get_History (Console.Kernel).all, History_Wrap_Lines)
      then
         Console.Get_View.Set_Wrap_Mode (Wrap_Char);
      else
         Console.Get_View.Set_Wrap_Mode (Wrap_None);
      end if;
   end On_Toggle_Line_Wrap;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Console : access GPS_Message_Record'Class) return Gtk_Widget
   is
   begin
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Console.Kernel).all, History_Wrap_Lines,
         Default_Value => True);

      Initialize
        (Console,
         "",
         null,
         Console.Kernel.all'Address,
         Highlight    => Message_Highlight.Get_Pref,
         History_List => null,
         ANSI_Support => Host /= Windows, --  ANSI_Support does not work
                                          --  well under Windows ???
         Key          => "",
         Wrap_Mode    => Wrap_Char);
      On_Toggle_Line_Wrap (Console);
      Console.Enable_Prompt_Display (False);
      Set_Font_And_Colors (Console.Get_View, Fixed_Font => True);

      Add_Hook (Console.Kernel, Preference_Changed_Hook,
                Wrapper (On_Preferences_Changed'Access),
                Name => "console.preferences_changed",
                Watch => GObject (Console));

      Register_Contextual_Menu
        (Kernel          => Console.Kernel,
         Event_On_Widget => Get_View (Console),
         Object          => Console,
         ID              => Messages_Views.Get_Module,
         Context_Func    => null);
      return Gtk_Widget (Console.Get_View);
   end Initialize;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access GPS_Message_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Check : Gtk_Check_Menu_Item;
   begin
      Gtk_New (Check, -"Wrap lines");
      Check.Set_Tooltip_Text
        (-"Whether to wrap long lines, or require horizontal scrolling");
      Associate (Get_History (View.Kernel).all,
                 History_Wrap_Lines, Check, Default => True);
      Check.On_Toggled (On_Toggle_Line_Wrap'Access, View);
      Menu.Add (Check);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access GPS_Message_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Add_Button
        (Kernel   => View.Kernel,
         Toolbar  => Toolbar,
         Action   => Command_Clear_Messages_Name);
      Add_Button
        (Kernel   => View.Kernel,
         Toolbar  => Toolbar,
         Action   => Command_Save_Name);
      Add_Button
        (Kernel   => View.Kernel,
         Toolbar  => Toolbar,
         Action   => Command_Load_Name);
   end Create_Toolbar;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Msg     : GPS_Message;
      Msg2    : constant access Kernel_Messages_Window :=
        new Kernel_Messages_Window;
      Command : Interactive_Command_Access;
      pragma Unreferenced (Msg);
   begin
      Messages_Views.Register_Module
        (Kernel, Menu_Name  => -"Views/_Messages");

      Msg2.Kernel := Kernel_Handle (Kernel);
      Kernel.Set_Messages_Window (Msg2);

      Command := new Clear_Messages_Command;
      Register_Action
        (Kernel, Command_Clear_Messages_Name,
         Command, Command_Clear_Messages_Tip,
         Stock_Id => Stock_Clear,
         Category => -"Messages");

      Command := new Save_Messages_Command;
      Register_Action
        (Kernel, Command_Save_Name,
         Command, Command_Save_Tip,
         Stock_Id => GPS_Save,
         Category => -"Messages");

      Command := new Load_Messages_Command;
      Register_Action
        (Kernel, Command_Load_Name,
         Command, Command_Load_Tip,
         Stock_Id => Stock_Open,
         Category => -"Messages");

      --  After the actions have been registered, so that the icons are found
      Msg := Messages_Views.Get_Or_Create_View (Kernel);
   end Register_Module;

end GPS.Kernel.Console;
