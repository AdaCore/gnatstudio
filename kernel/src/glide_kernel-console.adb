-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Values;          use Glib.Values;

with Interactive_Consoles; use Interactive_Consoles;
with Glide_Intl;           use Glide_Intl;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with GNAT.IO;              use GNAT.IO;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GNAT.Regpat;          use GNAT.Regpat;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtkada.MDI;           use Gtkada.MDI;
with OS_Utils;             use OS_Utils;
with String_Utils;         use String_Utils;
with Traces;               use Traces;
with Ada.Exceptions;       use Ada.Exceptions;
with Glide_Result_View;    use Glide_Result_View;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Histories;            use Histories;

package body Glide_Kernel.Console is

   type Console_Module_Id_Record is new Module_ID_Record with record
      Console : Interactive_Consoles.Interactive_Console;
      Interactive_Console : Interactive_Consoles.Interactive_Console;
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

   procedure Interactive_Console_Destroyed
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

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String;
   --  Launch the command interpreter for Input and return the output.

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String
   is
      S : constant String := Interpret_Command (Kernel_Handle (Kernel), Input);
   begin
      if S = ""
        or else S (S'Last) = ASCII.LF
        or else S (S'Last) = ASCII.CR
      then
         return S;
      else
         return S & ASCII.LF;
      end if;
   end Interpret_Command_Handler;

   -----------------------------
   -- Get_Interactive_Console --
   -----------------------------

   function Get_Interactive_Console
     (Kernel : access Kernel_Handle_Record'Class)
      return Interactive_Console
   is
      pragma Unreferenced (Kernel);
   begin
      return Console_Module_Id.Interactive_Console;
   end Get_Interactive_Console;

   -------------------------------
   -- Get_Or_Create_Result_View --
   -------------------------------

   function Get_Or_Create_Result_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return Result_View
   is
      Child   : MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Result_View_Record'Tag);
      Results : Result_View;
   begin
      if Child = null then
         if not Allow_Creation then
            return null;
         end if;

         Gtk_New (Results, Kernel_Handle (Kernel),
                  Module_ID (Console_Module_Id));
         Child := Put (Get_MDI (Kernel), Results);
         Set_Title (Child, -"Locations");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
         return Results;
      else
         return Result_View (Get_Widget (Child));
      end if;
   end Get_Or_Create_Result_View;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class)
      return Interactive_Console
   is
      pragma Unreferenced (Kernel);
   begin
      return Console_Module_Id.Console;
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
   begin
      if Console = null then
         Put_Line (Text);
      elsif Text /= "" then
         Insert (Console, Text, Add_LF, Mode = Error);

         if Mode = Error then
            Raise_Console (Kernel);
         else
            Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), Console));
         end if;
      end if;
   end Insert;

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Category       : String)
   is
      File_Location : constant Pattern_Matcher :=
        Compile (Get_Pref (Kernel, File_Pattern));
      File_Index : constant Integer :=
        Integer (Get_Pref (Kernel, File_Pattern_Index));
      Line_Index : constant Integer :=
        Integer (Get_Pref (Kernel, Line_Pattern_Index));
      Col_Index  : constant Integer :=
        Integer (Get_Pref (Kernel, Column_Pattern_Index));
      Matched    : Match_Array (0 .. 9);
      Start      : Natural := Text'First;
      Last       : Natural;
      Real_Last  : Natural;
      Line       : Natural := 1;
      Column     : Natural := 1;
   begin
      while Start <= Text'Last loop
         --  Parse Text line by line and look for file locations

         while Start < Text'Last
           and then (Text (Start) = ASCII.CR
                     or else Text (Start) = ASCII.LF)
         loop
            Start := Start + 1;
         end loop;

         Real_Last := Start;

         while Real_Last < Text'Last
           and then Text (Real_Last + 1) /= ASCII.CR
           and then Text (Real_Last + 1) /= ASCII.LF
         loop
            Real_Last := Real_Last + 1;
         end loop;

         Match (File_Location, Text (Start .. Real_Last), Matched);

         if Matched (0) /= No_Match then
            if Matched (Line_Index) /= No_Match then
               Line := Integer'Value
                 (Text
                    (Matched (Line_Index).First .. Matched (Line_Index).Last));

               if Line <= 0 then
                  Line := 1;
               end if;
            end if;

            if Matched (Col_Index) = No_Match then
               Last := Matched (Line_Index).Last;
            else
               Last := Matched (Col_Index).Last;
               Column := Integer'Value
                 (Text (Matched (Col_Index).First ..
                            Matched (Col_Index).Last));

               if Column <= 0 then
                  Column := 1;
               end if;
            end if;

            Insert_Result
              (Kernel,
               Category,
               Text
                 (Matched (File_Index).First .. Matched (File_Index).Last),
               Text (Last + 1 .. Real_Last),
               Positive (Line), Positive (Column), 0);
         end if;

         Start := Real_Last + 1;
      end loop;
   end Parse_File_Locations;

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

   -------------------
   -- Insert_Result --
   -------------------

   procedure Insert_Result
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String;
      File     : String;
      Text     : String;
      Line     : Positive;
      Column   : Positive;
      Length   : Natural := 0)
   is
      View : constant Result_View := Get_Or_Create_Result_View (Kernel);
   begin
      if View /= null then
         Insert (View, Category, File, Line, Column, Text, Length);
         Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), View));
      end if;
   end Insert_Result;

   ----------------------------
   -- Remove_Result_Category --
   ----------------------------

   procedure Remove_Result_Category
     (Kernel   : access Kernel_Handle_Record'Class;
      Category : String)
   is
      View : constant Result_View :=
        Get_Or_Create_Result_View (Kernel, Allow_Creation => False);
   begin
      if View /= null then
         Remove_Category (View, Category);
      end if;
   end Remove_Result_Category;

   -----------------------------------
   -- Interactive_Console_Destroyed --
   -----------------------------------

   procedure Interactive_Console_Destroyed
     (Console : access Glib.Object.GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Console, Kernel);
   begin
      Console_Module_Id.Interactive_Console := null;
   end Interactive_Console_Destroyed;

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
         File : constant String :=
           Select_File
             (Title             => -"Save messages window as",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Save_File,
              History           => Get_History (Kernel));
      begin
         if File = "" then
            return;
         end if;

         declare
            Contents : constant String := Get_Chars (Console);
         begin
            FD := Create_File (File, Binary);
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
         File : constant String :=
           Select_File
             (Title => -"Select file to load in the messages window",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if File = "" then
            return;
         end if;

         Contents := Read_File (File);

         declare
            S : constant String := Strip_CR (Contents.all);
         begin
            Insert (Console, S);
            Highlight_Child (Find_MDI_Child (Get_MDI (Kernel), Console));
            Parse_File_Locations (Kernel, S, -"Loaded contents");
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

      if Module.Interactive_Console /= null then
         Destroy (Module.Interactive_Console);
      end if;
   end Destroy;

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Console     : Interactive_Console;
      Interactive : Interactive_Console;
      Child       : MDI_Child;

   begin
      --  ??? Using an interactive_console seems overkill, since the user
      --  cannot write in the messages window
      Gtk_New (Console,
               "",
               null,
               GObject (Kernel),
               Get_Pref (Kernel, Source_Editor_Font),
               History_List => null,
               Key          => "",
               Wrap_Mode    => Wrap_Char);
      Enable_Prompt_Display (Console, False);

      Child := Put
        (Get_MDI (Kernel), Console, Iconify_Button or Maximize_Button);
      Set_Title (Child, -"Messages");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);
      Raise_Child (Child);

      Gtk_New (Interactive,
               "GPS> ",
               Interpret_Command_Handler'Access,
               GObject (Kernel),
               Get_Pref (Kernel, Source_Editor_Font),
               History_List => Get_History (Kernel),
               Key          => "shell",
               Wrap_Mode    => Wrap_Char);
      Child := Put
        (Get_MDI (Kernel), Interactive,
         Iconify_Button or Maximize_Button);
      Set_Title (Child, -"Shell");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);

      --  Only remember the last 100 commands.
      Set_Max_Length (Get_History (Kernel).all, 100, "shell");
      Allow_Duplicates (Get_History (Kernel).all, "shell", True, True);

      Console_Module_Id.Console := Console;
      Console_Module_Id.Interactive_Console := Interactive;

      Kernel_Callback.Connect
        (Console, "destroy",
         Kernel_Callback.To_Marshaller (Console_Destroyed'Access),
         Kernel_Handle (Kernel));
      Return_Callback.Connect
        (Console, "delete_event",
         Return_Callback.To_Marshaller (Console_Delete_Event'Access));

      Kernel_Callback.Connect
        (Interactive, "destroy",
            Kernel_Callback.To_Marshaller
         (Interactive_Console_Destroyed'Access),
         Kernel_Handle (Kernel));
      Return_Callback.Connect
        (Interactive, "delete_event",
         Return_Callback.To_Marshaller (Console_Delete_Event'Access));
   end Initialize_Console;

   ------------------
   -- Mime_Handler --
   ------------------

   function Mime_Handler
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;

   function Mime_Handler
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      View : constant Result_View := Get_Or_Create_Result_View (Kernel, False);
      pragma Unreferenced (Mode);
   begin
      if View /= null
        and then Mime_Type = Mime_Location_Action
      then
         declare
            Identifier : constant String := Get_String (Data (Data'First));
            Category   : constant String := Get_String (Data (Data'First + 1));
            File       : constant String := Get_String (Data (Data'First + 2));
            Line       : constant Gint   := Get_Int (Data (Data'First + 3));
            Column     : constant Gint   := Get_Int (Data (Data'First + 4));
            Message    : constant String := Get_String (Data (Data'First + 5));
            Action     : constant Action_Item := To_Action_Item
              (Get_Address (Data (Data'First + 6)));
         begin
            Add_Action_Item
              (View, Identifier, Category, File,
               Integer (Line), Integer (Column),
               Message, Action);
         end;

         return True;
      end if;

      return False;
   end Mime_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File     : constant String := '/' & (-"File");
      Console  : constant String := File & '/' & (-"_Messages");
      Mitem    : Gtk_Menu_Item;

   begin
      Console_Module_Id := new Console_Module_Id_Record;
      Register_Module
        (Module       => Module_ID (Console_Module_Id),
         Kernel       => Kernel,
         Module_Name  => Console_Module_Name,
         Priority     => Default_Priority,
         Mime_Handler => Mime_Handler'Access);

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
