-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib;                         use Glib;
with Glib.Object;                  use Glib.Object;
with Glib.Xml_Int;                 use Glib.Xml_Int;
with Glib.Values;                  use Glib.Values;
with GVD;
with Csc_HTML_Widget;              use Csc_HTML_Widget;
with GNAT.Directory_Operations;    use GNAT.Directory_Operations;
with GNAT.OS_Lib;                  use GNAT.OS_Lib;
with Glide_Main_Window;            use Glide_Main_Window;
with Glide_Kernel;                 use Glide_Kernel;
with Glide_Kernel.Console;         use Glide_Kernel.Console;
with Glide_Kernel.Modules;         use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;     use Glide_Kernel.Preferences;
with Gtkada.Dialogs;               use Gtkada.Dialogs;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gdk.Event;                    use Gdk.Event;
with Gdk.Types.Keysyms;            use Gdk.Types.Keysyms;
with Gtk.Adjustment;               use Gtk.Adjustment;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Widget;                   use Gtk.Widget;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Glide_Intl;                   use Glide_Intl;
with Traces;                       use Traces;
with OS_Utils;                     use OS_Utils;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Exceptions;               use Ada.Exceptions;
with Find_Utils;                   use Find_Utils;

package body Help_Module is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Help");

   type Help_Browser_Record is new Gtk_Scrolled_Window_Record with record
      Kernel : Kernel_Handle;
      Current_Help_File : GNAT.OS_Lib.String_Access;
      --  The current help file displayed. Used to find relative (hyper) links.

      Csc : Csc_HTML;
   end record;
   type Help_Browser is access all Help_Browser_Record'Class;

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      File   : String) return Boolean;
   --  Load File in HTML widget, and set File as the Current_Help_File for
   --  Kernel. Return True if the file could be successfully read.

   procedure Url_Requested
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler for the url_requested signal
   --  Called when loading a url as part of another page display (e.g an
   --  image).

   procedure On_Url
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler for the on_url signal

   procedure Link_Clicked
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the link_clicked signal

   procedure Display_Url
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      Url    : String);
   --  Display Url in Html.

   procedure Title_Changed
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the title_changed signal

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel

   function Create_Html_Editor
     (Kernel : access Kernel_Handle_Record'Class; File : String)
      return Help_Browser;
   --  Create a new html editor that edits File.

   procedure Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : String);
   --  Display HTML Help file

   function Key_Press
     (Html : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Handles the scrolling through the keyboard keys

   procedure On_Destroy (Html : access Gtk_Widget_Record'Class);
   --  Called when an html browser is destroyed.

   procedure On_Zoom_In
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Zoom in

   procedure On_Zoom_Out
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Zoom in

   generic
      HTML_File : String;
   package HTML_Loader is
      procedure On_Load_HTML
        (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
      --  Load HMTL_File in the HTML/Help widget

      function File_Name (Kernel : access Kernel_Handle_Record'Class)
         return String;
      --  Return the full file name for the help file

      procedure Register_Menu
        (Kernel : access Kernel_Handle_Record'Class;
         Name   : String);
      --  Register the menu in the GPS menubar
   end HTML_Loader;

   procedure On_Open_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Open HTML...
   --  Display a file selection dialog, and then open the HTML file in the
   --  help widget.

   -----------------
   -- HTML_Loader --
   -----------------

   package body HTML_Loader is
      ------------------
      -- On_Load_HTML --
      ------------------

      procedure On_Load_HTML
        (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
      is
         pragma Unreferenced (Widget);
      begin
         Open_Html (Kernel, File_Name (Kernel));

      exception
         when E : others =>
            Trace (Me, "Unexpected exception: " & Exception_Information (E));
      end On_Load_HTML;

      ---------------
      -- File_Name --
      ---------------

      function File_Name (Kernel : access Kernel_Handle_Record'Class)
         return String
      is
         Top  : constant Glide_Window := Glide_Window
           (Get_Main_Window (Kernel));
      begin
         return Format_Pathname
           (Top.Prefix_Directory.all & "/doc/gps/html/" & HTML_File);
      end File_Name;

      -------------------
      -- Register_Menu --
      -------------------

      procedure Register_Menu
        (Kernel : access Kernel_Handle_Record'Class;
         Name   : String)
      is
         Help : constant String := "/_" & (-"Help") & '/';
      begin
         Register_Menu
           (Kernel, Help, Name, "", On_Load_HTML'Access,
            Sensitive => Is_Regular_File (File_Name (Kernel)));
      end Register_Menu;
   end HTML_Loader;

   package On_Welcome is new HTML_Loader ("gps-welcome.html");
   --  Menu Help->Welcome

   package On_GPS_Tutorial is new HTML_Loader ("gps-tutorial.html");
   --  Menu Help->GPS Tutorial

   package On_GPS_Help is new HTML_Loader ("gps.html");
   --  Menu Help->Using GPS

   package On_GVD_Help is new HTML_Loader ("gvd.html");
   --  Menu Help->Using the GNU Visual Debugger

   package On_GNAT_UG_Help is new HTML_Loader ("gnat_ug.html");
   --  Menu Help->GNAT User's Guide

   package On_GNAT_RM_Help is new HTML_Loader ("gnat_rm.html");
   --  Menu Help->GNAT Reference Manual

   package On_ARM95_Help is new HTML_Loader ("arm95.html");
   --  Menu Help->Ada 95 Reference Manual

   package On_GDB_Help is new HTML_Loader ("gdb.html");
   --  Menu Help->Using the GNU Debugger

   package On_GCC_Help is new HTML_Loader ("gcc.html");
   --  Menu Help->Using GCC

   procedure On_About
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Menu Help->About...

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      File   : String) return Boolean
   is
      Buffer   : String_Access;
      Stream   : Csc_HTML_Stream;
      Success  : Boolean;

   begin
      if not Is_Regular_File (File) then
         Insert (Kernel, File & (-": File not found"), Mode => Error);
         return False;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      Buffer := Read_File (File);

      if Buffer /= null then
         Trace (Me, "loading file: " & File);
         Free (Html.Current_Help_File);
         Html.Current_Help_File := new String'(File);
         Stream := HTML_Begin (Html.Csc);
         HTML_Write (Html.Csc, Stream, Buffer.all);
         HTML_End (Html.Csc, Stream, Stream_OK);
         Free (Buffer);
         Success := True;
      else
         Trace (Me, "link not found: " & File);
         Success := False;
      end if;

      Pop_State (Kernel_Handle (Kernel));

      return Success;
   end Load_File;

   -------------------
   -- Url_Requested --
   -------------------

   procedure Url_Requested
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Html   : constant Help_Browser := Help_Browser (Object);
      Url    : constant String := Get_String (Nth (Params, 1));
      Stream : constant Csc_HTML_Stream :=
        Csc_HTML_Stream (Get_Proxy (Nth (Params, 2)));
      Buffer : String_Access;

   begin
      Trace (Me, "url requested: " & Url);

      if Is_Absolute_Path (Url) then
         Trace (Me, "Absolute path");
         Buffer := Read_File (Url);
      else
         declare
            Base_Dir : constant String :=
              Dir_Name (Html.Current_Help_File.all);
         begin
            Buffer := Read_File (Base_Dir & Url);
            Trace (Me, "url normalized: " & Base_Dir & Url);
         end;
      end if;

      if Buffer /= null then
         Stream_Write (Stream, Buffer.all);
         Free (Buffer);
      else
         Insert (Html.Kernel, Url & (-": File not found"), Mode => Error);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Url_Requested;

   ------------
   -- On_Url --
   ------------

   procedure On_Url
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Object);
      --  Html     : Help_Browser := Help_Browser (Object);
      Url : constant String := Get_String (Nth (Params, 1));
   begin
      Trace (Me, "On_Url: " & Url);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Url;

   -----------------
   -- Display_Url --
   -----------------

   procedure Display_Url
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      Url    : String)
   is
      Anchor   : Natural := Index (Url, "#");
      Result   : Boolean := True;
   begin
      if Anchor = 0 then
         Anchor := Url'Last + 1;
      end if;

      if Is_Absolute_Path (Url) then
         if Url /= Html.Current_Help_File.all then
            Result := Load_File
              (Kernel, Html, Url (Url'First .. Anchor - 1));
         end if;
      else
         declare
            Base_Dir : constant String :=
              Dir_Name (Html.Current_Help_File.all);
            Basename : constant String :=
              Base_Name (Html.Current_Help_File.all);

         begin
            if Url (Url'First .. Anchor - 1) /= Basename then
               Result := Load_File
                 (Kernel, Html,
                  Base_Dir & Url (Url'First .. Anchor - 1));
            end if;
         end;
      end if;

      if Result and then Anchor < Url'Last then
         Push_State (Kernel_Handle (Kernel), Busy);
         Trace (Me, "jumping to anchor " & Url (Anchor .. Url'Last));
         Result := Jump_To_Anchor
           (Html.Csc, Url (Anchor + 1 .. Url'Last));
         Pop_State (Kernel_Handle (Kernel));
      end if;

   end Display_Url;

   ------------------
   -- Link_Clicked --
   ------------------

   procedure Link_Clicked
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
      Url : constant String := Get_String (Nth (Params, 1));

   begin
      Open_Html (Kernel, Url);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Link_Clicked;

   -------------------
   -- Title_Changed --
   -------------------

   procedure Title_Changed
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
      Title : constant String := Get_String (Nth (Params, 1));
      MDI   : constant MDI_Window := Get_MDI (Kernel);
      Child : MDI_Child;

   begin
      Child := Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);

      if Child /= null then
         Set_Title (Child, (-"Help: ") & Title);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Title_Changed;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Html  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      H   : constant Help_Browser := Help_Browser (Html);
      Adj : Gtk_Adjustment;
   begin
      case Get_Key_Val (Event) is
         when GDK_Up =>
            Adj := Get_Vadjustment (H);
            Set_Value
              (Adj,
               Gdouble'Max
                 (Get_Lower (Adj),
                  Get_Value (Adj) - Get_Step_Increment (Adj)));

         when GDK_Down =>
            Adj := Get_Vadjustment (H);
            Set_Value
              (Adj,
               Gdouble'Min
                 (Get_Value (Adj) + Get_Step_Increment (Adj),
                  Get_Upper (Adj) - Get_Page_Size (Adj)));

         when GDK_Page_Up =>
            Adj := Get_Vadjustment (H);
            Set_Value
              (Adj,
               Gdouble'Max
                 (Get_Lower (Adj),
                  Get_Value (Adj) - Get_Page_Size (Adj)));

         when GDK_Page_Down =>
            Adj := Get_Vadjustment (H);
            Set_Value
              (Adj,
               Gdouble'Min
                 (Get_Value (Adj) + Get_Page_Size (Adj),
                  Get_Upper (Adj) - Get_Page_Size (Adj)));

         when others =>
            return False;
      end case;

      return True;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return True;
   end Key_Press;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Html : access Gtk_Widget_Record'Class) is
   begin
      Free (Help_Browser (Html).Current_Help_File);
   end On_Destroy;

   ----------------
   -- On_Zoom_In --
   ----------------

   Font_Adjust : Integer;
   pragma Import (C, Font_Adjust, "_gdk_font_adjust");

   procedure On_Zoom_In
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      MDI   : constant MDI_Window := Get_MDI (Kernel);
      Help  : Help_Browser;
      Child : constant MDI_Child :=
        Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);

   begin
      Font_Adjust := Font_Adjust + 2;

      if Child = null then
         return;
      end if;

      Help := Help_Browser (Get_Widget (Child));

      declare
         File : constant String := Help.Current_Help_File.all;
      begin
         Destroy (Help);
         Display_Help (Kernel, File);
      end;
   end On_Zoom_In;

   -----------------
   -- On_Zoom_Out --
   -----------------

   procedure On_Zoom_Out
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      MDI   : constant MDI_Window := Get_MDI (Kernel);
      Help  : Help_Browser;
      Child : constant MDI_Child :=
        Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);

   begin
      if Font_Adjust < -6 then
         return;
      end if;

      Font_Adjust := Font_Adjust - 2;

      if Child = null then
         return;
      end if;

      Help := Help_Browser (Get_Widget (Child));

      declare
         File : constant String := Help.Current_Help_File.all;
      begin
         Destroy (Help);
         Display_Help (Kernel, File);
      end;
   end On_Zoom_Out;

   ------------------------
   -- Create_Html_Editor --
   ------------------------

   function Create_Html_Editor
     (Kernel : access Kernel_Handle_Record'Class; File : String)
      return Help_Browser
   is
      Html   : Help_Browser;
      Result : Boolean;
      pragma Unreferenced (Result);

   begin
      Html := new Help_Browser_Record;
      Html.Kernel := Kernel_Handle (Kernel);
      Gtk.Scrolled_Window.Initialize (Html);
      Set_Policy (Html, Policy_Automatic, Policy_Always);
      Gtk_New (Html.Csc);
      Add (Html, Html.Csc);
      Set_Size_Request
        (Html,
         Get_Pref (Kernel, Default_Widget_Width),
         Get_Pref (Kernel, Default_Widget_Height));

      Widget_Callback.Object_Connect
        (Html.Csc, "url_requested", Url_Requested'Access, Slot_Object => Html);
      Kernel_Callback.Object_Connect
        (Html.Csc, "link_clicked",
         Link_Clicked'Access, User_Data => Kernel_Handle (Kernel),
         Slot_Object => Html);
      Widget_Callback.Object_Connect
        (Html.Csc, "on_url", On_Url'Access, Slot_Object => Html);
      Kernel_Callback.Object_Connect
        (Html.Csc, "title_changed",
         Title_Changed'Access, User_Data => Kernel_Handle (Kernel),
         Slot_Object => Html);
      Return_Callback.Object_Connect
        (Html.Csc, "key_press_event",
         Return_Callback.To_Marshaller (Key_Press'Access), Html);
      Widget_Callback.Connect
        (Html, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));

      Result := Load_File (Kernel, Html, File);

      return Html;
   end Create_Html_Editor;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : String)
   is
      MDI      : constant MDI_Window := Get_MDI (Kernel);
      Scrolled : Help_Browser;
      Child    : MDI_Child;
      Result   : Boolean;
      pragma Unreferenced (Result);

   begin
      if not Is_Regular_File (Help_File) then
         Insert (Kernel, Help_File & (-": File not found"), Mode => Error);
         return;
      end if;

      Child := Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);

      if Child = null then
         Scrolled := Create_Html_Editor (Kernel, Help_File);
         Child := Put (MDI, Scrolled);
         Set_Title (Child, -"Help");
         Show_All (Scrolled);
         Raise_Child (Child);

      else
         Scrolled := Help_Browser (Get_Widget (Child));

         if Scrolled.Current_Help_File /= null
           and then Help_File /= Scrolled.Current_Help_File.all
         then
            Result := Load_File (Kernel, Scrolled, Help_File);
         end if;

         Raise_Child (Child);
      end if;
   end Display_Help;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget
   is
      Editor : Help_Browser;
      File   : Glib.String_Ptr;
   begin
      if Node.Tag.all = "Help_Browser" then
         File := Get_Field (Node, "File");
         if File /= null then
            Editor := Create_Html_Editor (User, File.all);
         end if;

         return Gtk_Widget (Editor);
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
      N, Child : Node_Ptr;
   begin
      if Widget.all in Help_Browser_Record'Class then
         N := new Node;
         N.Tag := new String'("Help_Browser");

         Child := new Node;
         Child.Tag := new String'("File");

         if Help_Browser (Widget).Current_Help_File = null then
            Child.Value := new String'("");
         else
            Child.Value := new String'
              (Help_Browser (Widget).Current_Help_File.all);
         end if;

         Add_Child (N, Child);
         return N;
      end if;

      return null;
   end Save_Desktop;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      pragma Unreferenced (Mode);
   begin
      if Mime_Type = Mime_Html_File then
         declare
            File  : constant String := Get_String (Data (Data'First));
            MDI   : constant MDI_Window := Get_MDI (Kernel);
            Child : constant MDI_Child := Find_MDI_Child_By_Tag
              (MDI, Help_Browser_Record'Tag);
            Html  : Help_Browser;
         begin
            if Child = null then
               Display_Help (Kernel, File);

               return True;
            end if;

            Html := Help_Browser (Get_Widget (Child));
            Display_Url (Kernel, Html, File);
            Raise_Child (Child);
         end;

         return True;
      end if;

      return False;
   end Mime_Action;

   --------------
   -- On_About --
   --------------

   procedure On_About
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Kernel, Button);
   begin
      Button := Message_Dialog
        ("GPS " & GVD.Version & " (" & GVD.Source_Date &
         (-") hosted on ") & GVD.Target & ASCII.LF & ASCII.LF &
         (-"the GNAT Programming System") & ASCII.LF & ASCII.LF &
         "(c) 2001-2002 ACT-Europe",
         Buttons => Button_OK,
         Title   => -"About...");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_About;

   ------------------
   -- On_Open_HTML --
   ------------------

   procedure On_Open_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Filename : constant String :=
           Select_File
             (-"Open HTML File",
              File_Pattern      => "*.htm*",
              Pattern_Name      => -"HTML files",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              History           => Get_History (Kernel));

      begin
         if Filename /= "" then
            Open_Html (Kernel, Filename);
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_HTML;

   ------------
   -- Search --
   ------------

   function Search
     (Context         : access Help_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      MDI      : constant MDI_Window := Get_MDI (Kernel);
      Child    : constant MDI_Child := Find_MDI_Child_By_Tag
        (MDI, Help_Browser_Record'Tag);
      Scrolled : Help_Browser;

   begin
      if Child = null then
         return False;
      end if;

      Scrolled := Help_Browser (Get_Widget (Child));

      if Context.First_Search then
         Context.First_Search := False;
         return Search
           (Get_Engine (Scrolled.Csc),
            Text           => Context_As_String (Context),
            Case_Sensitive => Get_Options (Context).Case_Sensitive,
            Forward        => not Search_Backward,
            Regular        => Get_Options (Context).Regexp);
      else
         return Search_Next (Get_Engine (Scrolled.Csc));
      end if;
   end Search;

   ------------------
   -- Help_Factory --
   ------------------

   function Help_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget) return Search_Context_Access
   is
      pragma Unreferenced (Kernel, Extra_Information, All_Occurences);

      Context : Help_Context_Access;
   begin
      Context := new Help_Context;
      Context.First_Search := True;
      return Search_Context_Access (Context);
   end Help_Factory;

   -------------------
   -- Show_Tutorial --
   -------------------

   procedure Show_Tutorial
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      On_GPS_Tutorial.On_Load_Html
        (Get_Main_Window (Kernel), Kernel_Handle (Kernel));
   end Show_Tutorial;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Help  : constant String := "/_" & (-"Help") & '/';
      Name  : constant String := -"Help";
      Mitem : Gtk_Menu_Item;

   begin
      Register_Module
        (Module                  => Help_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Help_Module_Name,
         Priority                => Default_Priority - 20,
         Default_Context_Factory => null,
         MDI_Child_Tag           => Help_Browser_Record'Tag,
         Mime_Handler            => Mime_Action'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Search_Function
        (Kernel => Kernel,
         Data   => (Length            => Name'Length,
                    Label             => Name,
                    Factory           => Help_Factory'Access,
                    Extra_Information => null,
                    Id                => Help_Module_ID,
                    Mask              => All_Options and not Supports_Replace
                      and not Search_Backward
                      and not Whole_Word and not All_Occurrences));

      --  Add help menus

      Register_Menu (Kernel, Help, -"_Zoom in", "", On_Zoom_In'Access);
      Register_Menu (Kernel, Help, -"Zoom _out", "", On_Zoom_Out'Access);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Help, Mitem);

      Register_Menu
        (Kernel, Help, -"_Open HTML File...", "", On_Open_HTML'Access);
      On_Welcome.Register_Menu (Kernel, -"_Welcome");
      On_GPS_Tutorial.Register_Menu
        (Kernel, -"GNAT Programming System _Tutorial");
      On_GPS_Help.Register_Menu
        (Kernel, -"Using the GNAT _Programming System");
      On_GVD_Help.Register_Menu (Kernel, -"Using the GNU _Visual Debugger");
      On_GNAT_UG_Help.Register_Menu (Kernel, -"GNAT _User's Guide");
      On_GNAT_RM_Help.Register_Menu (Kernel, -"GNAT _Reference Manual");
      On_ARM95_Help.Register_Menu (Kernel, -"_Ada 95 Reference Manual");
      On_GDB_Help.Register_Menu (Kernel, -"Using the GNU _Debugger");
      On_GCC_Help.Register_Menu (Kernel, -"Using _GCC");
      Register_Menu (Kernel, Help, -"A_bout...", "", On_About'Access);
   end Register_Module;

end Help_Module;
