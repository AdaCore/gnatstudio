-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Ada.Calendar;             use Ada.Calendar;
with GNAT.Calendar;            use GNAT.Calendar;
with GNAT.Calendar.Time_IO;    use GNAT.Calendar.Time_IO;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Interactive_Consoles;     use Interactive_Consoles;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with GNAT.IO;                  use GNAT.IO;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtkada.File_Selector;     use Gtkada.File_Selector;
with Gtkada.MDI;               use Gtkada.MDI;
with String_Utils;             use String_Utils;
with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Histories;                use Histories;
with Gtk.Widget;               use Gtk.Widget;
with VFS;                      use VFS;
with Basic_Types;

package body Glide_Kernel.Console is

   type GPS_Message_Record is new Interactive_Console_Record with null record;
   type GPS_Message is access GPS_Message_Record'Class;
   --  Type for the messages window. This is mostly use to have a unique tag
   --  for this console, so that we can save it in the desktop

   type Console_Module_Id_Record is new Module_ID_Record with record
      Console : GPS_Message;
   end record;

   type Console_Module_Id_Access is access all Console_Module_Id_Record'Class;

   procedure Destroy (Module : in out Console_Module_Id_Record);
   --  Called when the module is destroyed.

   Console_Module_Id   : Console_Module_Id_Access;
   Console_Module_Name : constant String := "Glide_Kernel.Console";

   Me : constant Debug_Handle := Create (Console_Module_Name);

   procedure Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Called when the console has been destroyed.

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Prevent the destruction of the console in the MDI

   procedure On_Save_Console_As
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Messages->Save As... menu.

   procedure On_Load_To_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Messages->Load Contents... menu.

   procedure On_Clear_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for File->Messages->Clear menu.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class)
      return Interactive_Console
   is
      pragma Unreferenced (Kernel);
   begin
      return Interactive_Console (Console_Module_Id.Console);
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
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info)
   is
      Console : constant Interactive_Console := Get_Console (Kernel);
      T       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      if Console = null then
         Put_Line (Text);
      elsif Text /= "" then
         if Mode = Error then
            Trace (Me, "Error: " & Text);
            Insert
              (Console, "[" & Image (T, ISO_Date & " %T") & "] " & Text,
               Add_LF, Mode = Error);
            Raise_Console (Kernel);
         else
            Insert (Console, Text, Add_LF, Mode = Error);
            Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), Console));
         end if;
      end if;
   end Insert;

   -------------------
   -- Raise_Console --
   -------------------

   procedure Raise_Console (Kernel : access Kernel_Handle_Record'Class) is
      MDI   : constant MDI_Window := Get_MDI (Kernel);
      Child : constant MDI_Child :=
        Find_MDI_Child_By_Name (MDI, -"Messages");
   begin
      if Child /= null then
         Raise_Child (Child);
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
      FD      : File_Descriptor;
      Len     : Integer;
      pragma Unreferenced (Widget, Len);

   begin
      declare
         File : constant Virtual_File :=
           Select_File
             (Title             => -"Save messages window as",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Save_File,
              Parent            => Get_Main_Window (Kernel),
              History           => Get_History (Kernel));
      begin
         if File = VFS.No_File then
            return;
         end if;

         declare
            Contents : constant String := Get_Chars (Console);
         begin
            FD := Create_File (Locale_Full_Name (File), Binary);
            Len := Write (FD, Contents'Address, Contents'Length);
            Close (FD);
         end;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_Console_As;

   ------------------------
   -- On_Load_To_Console --
   ------------------------

   procedure On_Load_To_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Console  : constant Interactive_Console := Get_Console (Kernel);
      Contents : String_Access;

   begin
      declare
         File : constant Virtual_File :=
           Select_File
             (Title => -"Select file to load in the messages window",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              Parent            => Get_Main_Window (Kernel),
              History           => Get_History (Kernel));
      begin
         if File = VFS.No_File then
            return;
         end if;

         Contents := Read_File (File);

         declare
            S : constant String := Strip_CR (Contents.all);
            Args : Argument_List :=
              (1 => new String'(S),
               2 => new String'(-"Loaded contents"));
         begin
            Insert (Console, S);
            Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), Console));
            Execute_GPS_Shell_Command (Kernel, "locations_parse", Args);
            Basic_Types.Free (Args);
         end;

         Free (Contents);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Clear_Console;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Console_Module_Id_Record) is
   begin
      if Module.Console /= null then
         Destroy (Module.Console);
      end if;
   end Destroy;

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Console     : GPS_Message;
      Child       : MDI_Child;

   begin
      --  ??? Using an interactive_console seems overkill, since the user
      --  cannot write in the messages window
      Console := new GPS_Message_Record;
      Initialize
        (Console,
         "",
         null,
         GObject (Kernel),
         Get_Pref (Kernel, Source_Editor_Font),
         Highlight    => Get_Pref (Kernel, Message_Highlight),
         History_List => null,
         Key          => "",
         Wrap_Mode    => Wrap_Char);
      Enable_Prompt_Display (Console, False);

      Child := Put
        (Kernel, Console, Iconify_Button or Maximize_Button,
         Default_Width  => 400,
         Default_Height => 120,
         Focus_Widget   => Gtk_Widget (Get_View (Console)),
         Module         => Console_Module_Id,
         Desktop_Independent => True);
      Set_Focus_Child (Child);
      Set_Title (Child, -"Messages");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);
      Raise_Child (Child);

      Console_Module_Id.Console := Console;

      Kernel_Callback.Connect
        (Console, "destroy",
         Kernel_Callback.To_Marshaller (Console_Destroyed'Access),
         Kernel_Handle (Kernel));
      Return_Callback.Connect
        (Console, "delete_event",
         Return_Callback.To_Marshaller (Console_Delete_Event'Access));
   end Initialize_Console;

   --------------------------------
   -- Create_Interactive_Console --
   --------------------------------

   function Create_Interactive_Console
     (Kernel      : access Kernel_Handle_Record'Class;
      Title       : String := "";
      History     : History_Key := "interactive";
      Create_If_Not_Exist : Boolean := True) return Interactive_Console
   is
      Console : Interactive_Console;
      Child   : MDI_Child;
   begin
      if Title /= "" then
         Child := Find_MDI_Child_By_Name (Get_MDI (Kernel), Title);

         if Child = null
           or else Get_Widget (Child).all not in
              Interactive_Console_Record'Class
         then
            if not Create_If_Not_Exist then
               return null;
            end if;

            Gtk_New
              (Console, "", null,
               null, Get_Pref (Kernel, Source_Editor_Font),
               History_List => Get_History (Kernel),
               Key          => History,
               Wrap_Mode    => Wrap_Char,
               Highlight    => Get_Pref (Kernel, Message_Highlight));
            Set_Max_Length   (Get_History (Kernel).all, 100, History);
            Allow_Duplicates (Get_History (Kernel).all, History, True, True);

            Child := Put (Get_MDI (Kernel), Gtk_Widget (Console));
            Set_Dock_Side (Child, Bottom);
            Dock_Child (Child);
            Set_Title (Child, Title, Title);
            Set_Focus_Child (Child);
         else
            Console := Interactive_Console (Get_Widget (Child));
         end if;

         Raise_Child (Child);
         return Console;
      else
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
         end if;
         return Find_MDI_Child (Get_MDI (User), Console_Module_Id.Console);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File     : constant String := '/' & (-"File");
      Console  : constant String := File & '/' & (-"_Messages");
      Mitem    : Gtk_Menu_Item;
      N        : Node_Ptr;
   begin
      Console_Module_Id := new Console_Module_Id_Record;
      Register_Module
        (Module       => Module_ID (Console_Module_Id),
         Kernel       => Kernel,
         Module_Name  => Console_Module_Name,
         Priority     => Default_Priority);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      --  Add Messages to the default desktop, so that we can enforce its
      --  being on top.
      N     := new Node;
      N.Tag := new String'("Message_Window");
      Add_Default_Desktop_Item
        (Kernel, N,
         10, 10,
         400, 120,
         -"Messages", -"Messages",
         Docked, Bottom,
         Focus => True, Raised => True);

      Initialize_Console (Kernel);

      Register_Menu (Kernel, Console, Ref_Item => -"Close");
      Register_Menu
        (Kernel, Console, -"_Clear", "", On_Clear_Console'Access);
      Register_Menu
        (Kernel, Console, -"_Save As...", "", On_Save_Console_As'Access);
      Register_Menu
        (Kernel, Console, -"_Load Contents...", "", On_Load_To_Console'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, File, Mitem, Ref_Item => -"Close");
   end Register_Module;

end Glide_Kernel.Console;
