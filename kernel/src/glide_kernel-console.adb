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
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Values;              use Glib.Values;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Interactive_Consoles;     use Interactive_Consoles;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with GNAT.IO;                  use GNAT.IO;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNAT.Regpat;              use GNAT.Regpat;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtkada.File_Selector;     use Gtkada.File_Selector;
with Gtkada.MDI;               use Gtkada.MDI;
with String_Utils;             use String_Utils;
with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;
with Glide_Result_View;        use Glide_Result_View;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Histories;                use Histories;
with Gtk.Widget;               use Gtk.Widget;
with VFS;                      use VFS;

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

   Output_Cst        : aliased constant String := "output";
   Category_Cst      : aliased constant String := "category";
   Regexp_Cst        : aliased constant String := "regexp";
   File_Index_Cst    : aliased constant String := "file_index";
   Line_Index_Cst    : aliased constant String := "line_index";
   Col_Index_Cst     : aliased constant String := "column_index";
   Msg_Index_Cst     : aliased constant String := "msg_index";
   Style_Index_Cst   : aliased constant String := "style_index";
   Warning_Index_Cst : aliased constant String := "warning_index";

   Parse_Location_Parameters : constant Cst_Argument_List :=
     (1 => Output_Cst'Access,
      2 => Category_Cst'Access,
      3 => Regexp_Cst'Access,
      4 => File_Index_Cst'Access,
      5 => Line_Index_Cst'Access,
      6 => Col_Index_Cst'Access,
      7 => Msg_Index_Cst'Access,
      8 => Style_Index_Cst'Access,
      9 => Warning_Index_Cst'Access);
   Remove_Category_Parameters : constant Cst_Argument_List :=
     (1 => Category_Cst'Access);

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive shell command handler.

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

   function Get_Or_Create_Result_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return MDI_Child;
   --  Internal version of Get_Or_Create_Result_View

   -------------------------------
   -- Get_Or_Create_Result_View --
   -------------------------------

   function Get_Or_Create_Result_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return Result_View
   is
      Child : MDI_Child;
   begin
      Child := Get_Or_Create_Result_View_MDI (Kernel, Allow_Creation);
      if Child = null then
         return null;
      else
         return Result_View (Get_Widget (Child));
      end if;
   end Get_Or_Create_Result_View;

   -----------------------------------
   -- Get_Or_Create_Result_View_MDI --
   -----------------------------------

   function Get_Or_Create_Result_View_MDI
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return MDI_Child
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
         Child := Put
           (Kernel, Results,
            Module              => Console_Module_Id,
            Default_Width       => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height      => Get_Pref (Kernel, Default_Widget_Height),
            Desktop_Independent => True);
         Set_Focus_Child (Child);

         Set_Title (Child, -"Locations");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
      end if;

      return Child;
   end Get_Or_Create_Result_View_MDI;

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

   --------------------------
   -- Parse_File_Locations --
   --------------------------

   procedure Parse_File_Locations
     (Kernel                  : access Kernel_Handle_Record'Class;
      Text                    : String;
      Category                : String;
      Highlight               : Boolean := False;
      Style_Category          : String := "";
      Warning_Category        : String := "";
      File_Location_Regexp    : String := "";
      File_Index_In_Regexp    : Integer := -1;
      Line_Index_In_Regexp    : Integer := -1;
      Col_Index_In_Regexp     : Integer := -1;
      Msg_Index_In_Regexp     : Integer := -1;
      Style_Index_In_Regexp   : Integer := -1;
      Warning_Index_In_Regexp : Integer := -1)
   is
      function Get_File_Location return Pattern_Matcher;
      --  Return the pattern matcher for the file location

      function Get_Index
        (Pref : Param_Spec_Int; Value : Integer) return Integer;
      --  If Value is -1, return Pref, otherwise return Value

      function Get_Message (Last : Natural) return String;
      --  Return the error message. For backward compatibility with existing
      --  preferences file, we check that the message Index is still good.
      --  Otherwise, we return the last part of the regexp

      function Get_File_Location return Pattern_Matcher is
      begin
         if File_Location_Regexp = "" then
            return Compile (Get_Pref (Kernel, File_Pattern));
         else
            return Compile (File_Location_Regexp);
         end if;
      end Get_File_Location;

      Max : Integer := 0;
      --  Maximal value for the indexes

      function Get_Index
        (Pref : Param_Spec_Int; Value : Integer) return Integer
      is
         Result : Integer;
      begin
         if Value = -1 then
            Result := Integer (Get_Pref (Kernel, Pref));
         else
            Result := Value;
         end if;

         Max := Integer'Max (Max, Result);
         return Result;
      end Get_Index;

      File_Location : constant Pattern_Matcher := Get_File_Location;
      File_Index    : constant Integer :=
        Get_Index (File_Pattern_Index, File_Index_In_Regexp);
      Line_Index    : constant Integer :=
        Get_Index (Line_Pattern_Index, Line_Index_In_Regexp);
      Col_Index     : constant Integer :=
        Get_Index (Column_Pattern_Index, Col_Index_In_Regexp);
      Msg_Index     : constant Integer :=
        Get_Index (Message_Pattern_Index, Msg_Index_In_Regexp);
      Style_Index  : constant Integer :=
        Get_Index (Style_Pattern_Index, Style_Index_In_Regexp);
      Warning_Index : constant Integer :=
        Get_Index (Warning_Pattern_Index, Warning_Index_In_Regexp);
      Matched    : Match_Array (0 .. Max);
      Start      : Natural := Text'First;
      Last       : Natural;
      Real_Last  : Natural;
      Line       : Natural := 1;
      Column     : Natural := 1;
      C          : String_Access;

      function Get_Message (Last : Natural) return String is
      begin
         if Matched (Msg_Index) /= No_Match then
            return Text
              (Matched (Msg_Index).First .. Matched (Msg_Index).Last);
         else
            return Text (Last + 1 .. Real_Last);
         end if;
      end Get_Message;

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

            if Matched (Warning_Index) /= No_Match then
               C := Warning_Category'Unrestricted_Access;
            elsif  Matched (Style_Index) /= No_Match then
               C := Style_Category'Unrestricted_Access;
            else
               C := Category'Unrestricted_Access;
            end if;

            Insert_Result
              (Kernel,
               Category,
               Create
                 (Text (Matched
                          (File_Index).First .. Matched (File_Index).Last),
                  Kernel),
               Get_Message (Last),
               Positive (Line), Positive (Column), 0,
               Highlight,
               C.all);
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
     (Kernel             : access Kernel_Handle_Record'Class;
      Category           : String;
      File               : VFS.Virtual_File;
      Text               : String;
      Line               : Positive;
      Column             : Positive;
      Length             : Natural := 0;
      Highlight          : Boolean := False;
      Highlight_Category : String := "")
   is
      View : constant Result_View := Get_Or_Create_Result_View (Kernel);
   begin
      if View /= null then
         Insert
           (View, Category, File, Line, Column, Text, Length,
            Highlight, Highlight_Category);
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
      View : Result_View;
      pragma Unreferenced (Mode);
   begin
      if Mime_Type = Mime_Location_Action then
         View := Get_Or_Create_Result_View (Kernel, False);
         declare
            Identifier : constant String := Get_String (Data (Data'First));
            Category   : constant String := Get_String (Data (Data'First + 1));
            File       : constant Virtual_File :=
              Create (Full_Filename => Get_String (Data (Data'First + 2)));
            Line       : constant Gint   := Get_Int (Data (Data'First + 3));
            Column     : constant Gint   := Get_Int (Data (Data'First + 4));
            Message    : constant String := Get_String (Data (Data'First + 5));
            Action     : constant Action_Item := To_Action_Item
              (Get_Address (Data (Data'First + 6)));
         begin
            Add_Action_Item
              (View, Identifier, Category, "", File,
               Integer (Line), Integer (Column),
               Message, Action);
         end;

         return True;
      end if;

      return False;
   end Mime_Handler;

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
      if Node.Tag.all = "Result_View_Record" then
         return Get_Or_Create_Result_View_MDI (User, Allow_Creation => True);
      elsif Node.Tag.all = "Message_Window" then
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
      if Widget.all in Result_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Result_View_Record");
         return N;
      elsif Widget.all in GPS_Message_Record'Class then
         N := new Node;
         N.Tag := new String'("Message_Window");
         return N;
      end if;

      return null;
   end Save_Desktop;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "locations_parse" then
         Name_Parameters (Data, Parse_Location_Parameters);
         Parse_File_Locations
           (Get_Kernel (Data),
            Text                    => Nth_Arg (Data, 1),
            Category                => Nth_Arg (Data, 2),
            File_Location_Regexp    => Nth_Arg (Data, 3, ""),
            File_Index_In_Regexp    => Nth_Arg (Data, 4, -1),
            Line_Index_In_Regexp    => Nth_Arg (Data, 5, -1),
            Col_Index_In_Regexp     => Nth_Arg (Data, 6, -1),
            Style_Index_In_Regexp   => Nth_Arg (Data, 7, -1),
            Warning_Index_In_Regexp => Nth_Arg (Data, 8, -1));

      elsif Command = "locations_remove_category" then
         Name_Parameters (Data, Remove_Category_Parameters);
         Remove_Result_Category
           (Get_Kernel (Data),
            Category => Nth_Arg (Data, 1));
      end if;
   end Default_Command_Handler;

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
         Priority     => Default_Priority,
         Mime_Handler => Mime_Handler'Access);
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

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Register_Command
        (Kernel,
         Command      => "locations_parse",
         Params       =>
           Parameter_Names_To_Usage (Parse_Location_Parameters, 7),
         Description  =>
           -("Parse the contents of the string, which is supposedly the"
             & " output of some tool, and add the errors and warnings to the"
             & " locations window. A new category is created in the locations"
             & " window if it doesn't exist. Preexisting contents for that"
             & " category is not removed, see locations_remove_category."
             & ASCII.LF
             & "The regular expression specifies how locations are recognized."
             & " By default, it matches file:line:column. The various indexes"
             & " indicate the index of the opening parenthesis that contains"
             & " the relevant information in the regular expression. Set it"
             & " to 0 if that information is not available. Style_Index and"
             & " Warning_Index, if they match, force the error message in a"
             & " specific category."),
         Minimum_Args => 2,
         Maximum_Args => 9,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "locations_remove_category",
         Params       => Parameter_Names_To_Usage (Remove_Category_Parameters),
         Description  =>
           -("Remove a category from the location window. This removes all"
             & " associated files"),
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Default_Command_Handler'Access);
   end Register_Commands;

end Glide_Kernel.Console;
