-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Widget;                   use Gtk.Widget;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Glide_Intl;                   use Glide_Intl;
with Traces;                       use Traces;
with OS_Utils;                     use OS_Utils;
with Ada.Exceptions;               use Ada.Exceptions;
with Find_Utils;                   use Find_Utils;
with String_Utils;                 use String_Utils;
with Gtk.Clipboard;                use Gtk.Clipboard;
with Generic_List;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Help_Module is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Help");

   Template_Index : constant String := "help_index.html";

   Font_Adjust : Integer;
   pragma Import (C, Font_Adjust, "_gdk_font_adjust");

   type Help_File_Record is record
      File  : GNAT.OS_Lib.String_Access;
      Descr : GNAT.OS_Lib.String_Access;
   end record;

   procedure Free (Data : in out Help_File_Record);
   package Help_File_List is new Generic_List (Help_File_Record, Free);
   use Help_File_List;

   type Help_Category_Record is record
      Name  : GNAT.OS_Lib.String_Access;
      Files : Help_File_List.List;
   end record;
   type Help_Category_Access is access Help_Category_Record;

   procedure Free (Data : in out Help_Category_Access);
   package Help_Category_List is new Generic_List (Help_Category_Access, Free);
   use Help_Category_List;

   type Help_Browser_Record is new Gtk_Scrolled_Window_Record with record
      Kernel : Kernel_Handle;
      Current_Help_File : GNAT.OS_Lib.String_Access;
      --  The current help file displayed. Used to find relative (hyper) links.

      Csc : Csc_HTML;
   end record;
   type Help_Browser is access all Help_Browser_Record'Class;

   type Help_Module_ID_Record is new Glide_Kernel.Module_ID_Record with record
      Categories : Help_Category_List.List;
      --  The registered help files
   end record;
   type Help_Module_ID_Access is access all Help_Module_ID_Record'Class;

   Help_Module_ID   : Help_Module_ID_Access;
   Help_Module_Name : constant String := "Help_Viewer";

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

   procedure Link_Clicked
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the link_clicked signal

   procedure Title_Changed
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle);
   --  Handler for the title_changed signal

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
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
     (Kernel : access Kernel_Handle_Record'Class;
      File : String) return Help_Browser;
   --  Create a new html editor that edits File.

   function Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : String) return Help_Browser;
   --  Display HTML Help file.

   function Key_Press
     (Html : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Handles the scrolling through the keyboard keys

   procedure On_Destroy (Html : access Gtk_Widget_Record'Class);
   --  Called when an html browser is destroyed.

   procedure On_Load_Done (Html : access Gtk_Widget_Record'Class);
   --  Called when a file has been loaded.

   procedure On_Copy (Html : access Glib.Object.GObject_Record'Class);
   --  Callback for the "Copy" contextual menu item.

   procedure On_Zoom_In
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Zoom in

   procedure On_Zoom_Out
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Zoom in

   procedure On_Load_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Load HMTL_File in the HTML/Help widget

   procedure Register_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      HTML_File : String;
      Descr     : String;
      Category  : String;
      Menu_Path : String);
   --  Register the menu in the GPS menubar

   procedure On_Open_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Open HTML...
   --  Display a file selection dialog, and then open the HTML file in the
   --  help widget.

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Generate a context corresponding to the currently viewed location.

   function Help_Contextual
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  The contextual menu for HTML viewers.

   procedure On_About
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Menu Help->About...

   type String_Menu_Item_Record (Length : Natural) is new Gtk_Menu_Item_Record
     with record
        File : String (1 .. Length);
     end record;
   type String_Menu_Item is access all String_Menu_Item_Record'Class;

   procedure Parse_Index_Files (Kernel : access Kernel_Handle_Record'Class);
   --  Parse all the index files in the path

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Help_File_Record) is
   begin
      Free (Data.File);
      Free (Data.Descr);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Help_Category_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Help_Category_Record, Help_Category_Access);
   begin
      Free (Data.Name);
      Free (Data.Files);
      Unchecked_Free (Data);
   end Free;

   ---------------------
   -- Help_Contextual --
   ---------------------

   function Help_Contextual
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event);
      Mitem : Gtk_Menu_Item;
   begin
      Gtk_New (Mitem, -"Copy");
      Add (Menu, Mitem);

      Object_Callback.Object_Connect
        (Mitem, "activate",
         Object_Callback.To_Marshaller (On_Copy'Access),
         Slot_Object => Object,
         After => True);

      return Default_Factory (Kernel, Gtk_Widget (Event_Widget));
   end Help_Contextual;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Child);
      Context : URL_Context_Access;
   begin
      Context := new URL_Context;

      Set_Context_Information
        (Context => Context,
         Kernel  => Kernel,
         Creator => Module_ID (Help_Module_ID));

      return Selection_Context_Access (Context);
   end Default_Factory;

   ------------------
   -- On_Load_HTML --
   ------------------

   procedure On_Load_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Item : constant String_Menu_Item := String_Menu_Item (Widget);
   begin
      Open_Html (Kernel, Item.File);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Load_HTML;

   -------------------
   -- Register_Help --
   -------------------

   procedure Register_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      HTML_File : String;
      Descr     : String;
      Category  : String;
      Menu_Path : String)
   is
      File : constant String := Locate_Html_File (Kernel, HTML_File);
      Item : String_Menu_Item;
      Node : Help_Category_List.List_Node;
      Cat  : Help_Category_Access;
   begin
      if Menu_Path /= "" then
         Item := new String_Menu_Item_Record (HTML_File'Length);
         Gtk.Menu_Item.Initialize_With_Mnemonic (Item, Base_Name (Menu_Path));
         Item.File := HTML_File;

         Register_Menu
           (Kernel      => Kernel,
            Parent_Path => Dir_Name (Menu_Path),
            Item        => Gtk_Menu_Item (Item));
         Set_Sensitive (Item, File /= "");

         Kernel_Callback.Connect
           (Item, "activate",
            Kernel_Callback.To_Marshaller (On_Load_HTML'Access),
            Kernel_Handle (Kernel));
      end if;

      Node := First (Help_Module_ID.Categories);
      while Node /= Help_Category_List.Null_Node loop
         Cat := Data (Node);
         exit when Cat.Name.all = Category;
         Node := Next (Node);
      end loop;

      if Node = Help_Category_List.Null_Node then
         Cat := new Help_Category_Record'
           (Name  => new String'(Category), Files => Help_File_List.Null_List);
         Append (Help_Module_ID.Categories, Cat);
      end if;

      Append (Cat.Files,
              (File  => new String'(HTML_File),
               Descr => new String'(Descr)));
   end Register_Help;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      File   : String) return Boolean
   is
      Buffer   : GNAT.OS_Lib.String_Access;
      Stream   : Csc_HTML_Stream;
      Success  : Boolean;
      Index    : Natural;
      Str      : Unbounded_String;
      Cat      : Help_Category_List.List_Node;
      F        : Help_File_List.List_Node;
      Contents_Marker : constant String := ASCII.LF & "@@CONTENTS@@";

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

         if Base_Name (File) /= Template_Index then
            HTML_Write (Html.Csc, Stream, Buffer.all);

         else
            Index := Buffer'First;
            while Index + Contents_Marker'Length - 1 <= Buffer'Last
              and then Buffer (Index .. Index + Contents_Marker'Length - 1) /=
              Contents_Marker
            loop
               Index := Index + 1;
            end loop;

            Str := To_Unbounded_String (Buffer (Buffer'First .. Index - 1));

            Cat := First (Help_Module_ID.Categories);
            Append (Str, "<table cellspacing=""0"" width=""100%"" border=""2"""
                    & "cellpadding=""6"">");

            while Cat /= Help_Category_List.Null_Node loop
               Append (Str,
                       "<tr><td bgcolor=""#006db6"">"
                       & "<font face=""tahoma"" size=""+2"" color=""#FFFFFF"">"
                       & Data (Cat).Name.all
                       & "</font></td> </tr>" & ASCII.LF);

               F := First (Data (Cat).Files);
               while F /= Help_File_List.Null_Node loop
                  Append (Str,
                          "<tr><td><a href=""" & Data (F).File.all
                          & """>" & Data (F).Descr.all
                          & "</a></td></tr>");
                  F := Next (F);
               end loop;

               Cat := Next (Cat);
            end loop;

            Append (Str, "</table>");
            Append
              (Str, Buffer (Index + Contents_Marker'Length .. Buffer'Last));

            HTML_Write (Html.Csc, Stream, To_String (Str));
         end if;

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
      Buffer : GNAT.OS_Lib.String_Access;

   begin
      Trace (Me, "url requested: " & Url);

      if Is_Absolute_Path (Url) then
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
      Trace (Me, "Link_Clicked: " & Url);
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
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Destroy;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy (Html : access GObject_Record'Class) is
      Browser : constant Help_Browser := Help_Browser (Html);
   begin
      Set_Text (Get, Get_Selection (Browser.Csc));
   end On_Copy;

   ------------------
   -- On_Load_Done --
   ------------------

   procedure On_Load_Done (Html : access Gtk_Widget_Record'Class) is
      Browser : constant Help_Browser := Help_Browser (Html);
   begin
      --  This is a dirty tweak to force the adjustment of the scrolled
      --  window after the load is done.
      Queue_Resize (Browser);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Load_Done;

   ----------------
   -- On_Zoom_In --
   ----------------

   procedure On_Zoom_In
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      MDI   : constant MDI_Window := Get_MDI (Kernel);
      Help  : Help_Browser;
      Child : constant MDI_Child :=
        Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);
      Success : Boolean;
      pragma Unreferenced (Success);

   begin
      Font_Adjust := Font_Adjust + 2;
      Set_Pref (Kernel, Help_Font_Adjust, Gint (Font_Adjust));

      if Child = null then
         return;
      end if;

      --  Force a reload of the file to show the new font.
      Help := Help_Browser (Get_Widget (Child));
      Success := Load_File (Kernel, Help, Help.Current_Help_File.all);
   end On_Zoom_In;

   -----------------
   -- On_Zoom_Out --
   -----------------

   procedure On_Zoom_Out
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      MDI   : constant MDI_Window := Get_MDI (Kernel);
      Help  : Help_Browser;
      Child : constant MDI_Child :=
        Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);
      Success : Boolean;
      pragma Unreferenced (Success);

   begin
      if Font_Adjust < -6 then
         return;
      end if;

      Font_Adjust := Font_Adjust - 2;
      Set_Pref (Kernel, Help_Font_Adjust, Gint (Font_Adjust));

      if Child = null then
         return;
      end if;

      --  Force a reload of the file to show the new font.
      Help := Help_Browser (Get_Widget (Child));
      Success := Load_File (Kernel, Help, Help.Current_Help_File.all);
   end On_Zoom_Out;

   ------------------------
   -- Create_Html_Editor --
   ------------------------

   function Create_Html_Editor
     (Kernel : access Kernel_Handle_Record'Class;
      File : String) return Help_Browser
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

      Widget_Callback.Object_Connect
        (Html.Csc, "url_requested", Url_Requested'Access, Slot_Object => Html);
      Kernel_Callback.Object_Connect
        (Html.Csc, "link_clicked",
         Link_Clicked'Access, User_Data => Kernel_Handle (Kernel),
         Slot_Object => Html);
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
      Widget_Callback.Object_Connect
        (Html.Csc, "load_done",
         Widget_Callback.To_Marshaller (On_Load_Done'Access), Html);

      Font_Adjust := Integer (Get_Pref (Kernel, Help_Font_Adjust));

      Result := Load_File (Kernel, Html, File);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Html.Csc,
         Object          => Html,
         ID              => Module_ID (Help_Module_ID),
         Context_Func    => Help_Contextual'Access);

      return Html;
   end Create_Html_Editor;

   ------------------
   -- Display_Help --
   ------------------

   function Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : String) return Help_Browser
   is
      MDI      : constant MDI_Window := Get_MDI (Kernel);
      Scrolled : Help_Browser;
      Child    : MDI_Child;
      Result   : Boolean;
      pragma Unreferenced (Result);

   begin
      if not Is_Regular_File (Help_File) then
         Insert (Kernel, Help_File & (-": File not found"), Mode => Error);
         return null;
      end if;

      Child := Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);

      if Child = null then
         Scrolled := Create_Html_Editor (Kernel, Help_File);
         Child := Put
           (MDI, Scrolled,
            Focus_Widget => Gtk_Widget (Scrolled.Csc),
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height));
         Set_Focus_Child (Child);
         Set_Title (Child, -"Help");
         Show_All (Scrolled);
         Raise_Child (Child);

      else
         Scrolled := Help_Browser (Get_Widget (Child));

         if Scrolled.Current_Help_File = null
           or else Help_File /= Scrolled.Current_Help_File.all
         then
            Result := Load_File (Kernel, Scrolled, Help_File);
         end if;

         Raise_Child (Child);
      end if;

      return Scrolled;
   end Display_Help;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      Editor : Help_Browser;
      File   : Glib.String_Ptr;
   begin
      if Node.Tag.all = "Help_Browser" then
         File := Get_Field (Node, "File");
         if File /= null then
            Editor := Create_Html_Editor (User, File.all);

            return Put
              (MDI, Gtk_Widget (Editor),
               Default_Width  => Get_Pref (User, Default_Widget_Width),
               Default_Height => Get_Pref (User, Default_Widget_Height));
         else
            return null;
         end if;
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
            File   : constant String := Get_String (Data (Data'First));
            Anchor : constant String := Get_String (Data (Data'First + 2));
            Html  : Help_Browser;
            Result : Boolean;
            pragma Unreferenced (Result);
         begin
            Html := Display_Help (Kernel, File);

            if Html /= null and then Anchor /= "" then
               Result := Jump_To_Anchor (Html.Csc, Anchor);
            end if;
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
      use ASCII;

      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      Button := Message_Dialog
        ("GPS " & GVD.Version & " (" & GVD.Source_Date &
           (-") hosted on ") & GVD.Target & LF &
         (-"GNAT ") & GNAT_Version (Kernel) & LF & LF &
         (-"the GNAT Programming System") & LF & LF &
         "(c) 2001-2003 ACT-Europe",
         Buttons => Button_OK,
         Title   => -"About...",
         Parent  => Get_Main_Window (Kernel));

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
              Parent            => Get_Main_Window (Kernel),
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
      Open_Html (Kernel, "gps-tutorial.html");
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Show_Tutorial;

   -----------------------
   -- Parse_Index_Files --
   -----------------------

   procedure Parse_Index_Files (Kernel : access Kernel_Handle_Record'Class) is
      Index_File : constant String := "gps_index.xml";
      Path : GNAT.OS_Lib.String_Access := Getenv ("GPS_DOC_PATH");
      Iter : Path_Iterator;
      Node, Tmp, Field : Node_Ptr;
   begin
      if Path /= null then
         Iter := Start (Path.all);
         loop
            declare
               Dir : constant String := Current (Path.all, Iter);
               Full : constant String := Name_As_Directory (Dir) & Index_File;
               Name, Descr, Menu, Cat : String_Ptr;
               Empty : aliased String := "";
            begin
               exit when Dir = "";

               if Is_Regular_File (Full) then
                  Trace (Me, "Parsing index " & Full);
                  Node := Parse (Full);

                  Tmp := Node.Child;
                  while Tmp /= null loop
                     if Tmp.Tag.all = "file" then
                        Name  := Empty'Unrestricted_Access;
                        Descr := Empty'Unrestricted_Access;
                        Menu  := Empty'Unrestricted_Access;
                        Cat   := Empty'Unrestricted_Access;

                        Field := Tmp.Child;
                        while Field /= null loop
                           if Field.Tag.all = "name" then
                              Name := Field.Value;

                           elsif Field.Tag.all = "descr" then
                              Descr := Field.Value;

                           elsif Field.Tag.all = "menu" then
                              Menu := Field.Value;

                           elsif Field.Tag.all = "category" then
                              Cat := Field.Value;

                           else
                              Insert
                                (Kernel,
                                 -"Invalid field in documentation index file "
                                 & Full);
                           end if;

                           Field := Field.Next;
                        end loop;

                        Trace (Me, "Adding " & Name.all & ' ' & Menu.all);
                        Register_Help
                          (Kernel,
                           HTML_File => Name.all,
                           Descr     => Descr.all,
                           Category  => Cat.all,
                           Menu_Path => Menu.all);
                     end if;

                     Tmp := Tmp.Next;
                  end loop;

                  Free (Node);
               end if;
            end;

            Iter := Next (Path.all, Iter);
         end loop;

         Free (Path);
      end if;
   end Parse_Index_Files;

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
      Help_Module_ID := new Help_Module_ID_Record;
      Register_Module
        (Module                  => Module_ID (Help_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Help_Module_Name,
         Priority                => Default_Priority - 20,
         Contextual_Menu_Handler => null,
         Default_Context_Factory => Default_Factory'Access,
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
                    Id                => Module_ID (Help_Module_ID),
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

      declare
         Item : String_Menu_Item;
      begin
         Item := new String_Menu_Item_Record (Template_Index'Length);
         Gtk.Menu_Item.Initialize_With_Mnemonic (Item, -"_Contents");
         Item.File := Template_Index;
         Register_Menu
           (Kernel      => Kernel,
            Parent_Path => Help,
            Item        => Gtk_Menu_Item (Item));
         Kernel_Callback.Connect
           (Item, "activate",
            Kernel_Callback.To_Marshaller (On_Load_HTML'Access),
            Kernel_Handle (Kernel));
      end;

      Parse_Index_Files (Kernel);

      Register_Menu
        (Kernel, Help, -"A_bout", "", On_About'Access);
   end Register_Module;

end Help_Module;
