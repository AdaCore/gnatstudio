-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
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
with Glib.Convert;                 use Glib.Convert;
with Glib.Object;                  use Glib.Object;
with Glib.Xml_Int;                 use Glib.Xml_Int;
with XML_Parsers;
with Glib.Values;                  use Glib.Values;
with GVD;
with Csc_HTML_Widget;              use Csc_HTML_Widget;
with GNAT.Directory_Operations;    use GNAT.Directory_Operations;
with GNAT.OS_Lib;                  use GNAT.OS_Lib;
with Glide_Kernel;                 use Glide_Kernel;
with Glide_Kernel.Console;         use Glide_Kernel.Console;
with Glide_Kernel.Hooks;           use Glide_Kernel.Hooks;
with Glide_Kernel.Modules;         use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;     use Glide_Kernel.Preferences;
with Glide_Kernel.Standard_Hooks;  use Glide_Kernel.Standard_Hooks;
with Glide_Main_Window;            use Glide_Main_Window;
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
with File_Utils;                   use File_Utils;
with String_Utils;                 use String_Utils;
with Gtk.Clipboard;                use Gtk.Clipboard;
with Generic_List;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Histories;                    use Histories;
with Glide_Kernel.Scripts;         use Glide_Kernel.Scripts;
with VFS;                          use VFS;
with System;                       use System;

package body Help_Module is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Help");

   Template_Index : constant String := "help_index.html";
   Index_File     : constant String := "gps_index.xml";

   Help_History_Key : constant History_Key := "help-recent-files";

   Font_Adjust : Integer;
   pragma Import (C, Font_Adjust, "_gdk_font_adjust");

   Url_Cst    : aliased constant String := "URL";
   Anchor_Cst : aliased constant String := "anchor";
   Dir_Cst    : aliased constant String := "directory";
   Name_Cst   : aliased constant String := "name";
   Help_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Url_Cst'Access, 2 => Anchor_Cst'Access);
   Add_Doc_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Dir_Cst'Access);
   Getdoc_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);

   type Help_File_Record is record
      File       : VFS.Virtual_File;
      Shell_Cmd  : GNAT.OS_Lib.String_Access;
      Shell_Lang : GNAT.OS_Lib.String_Access;
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
      Current_Help_File : VFS.Virtual_File;
      --  The current help file displayed. Used to find relative (hyper) links.

      Csc : Csc_HTML;
   end record;
   type Help_Browser is access all Help_Browser_Record'Class;

   type Help_Module_ID_Record is new Glide_Kernel.Module_ID_Record with record
      Categories : Help_Category_List.List;
      --  The registered help files

      Doc_Path : GNAT.OS_Lib.String_Access;

      Html_Class : Class_Type;
      Help_Class : Class_Type;
   end record;
   type Help_Module_ID_Access is access all Help_Module_ID_Record'Class;

   procedure Destroy (Module : in out Help_Module_ID_Record);
   --  Destroy the memory associated with Module

   Help_Module_ID   : Help_Module_ID_Access;
   Help_Module_Name : constant String := "Help_Viewer";

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      File   : VFS.Virtual_File) return Boolean;
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

   function Open_Help_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : Hooks_Data'Class) return Boolean;
   --  Process, if possible, the data sent by the kernel

   function Create_Html_Editor
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return MDI_Child;
   --  Create a new html editor that edits File.

   function Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : VFS.Virtual_File) return Help_Browser;
   --  Display HTML Help file.

   function Key_Press
     (Html : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Handles the scrolling through the keyboard keys

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
     (Kernel     : access Kernel_Handle_Record'Class;
      HTML_File  : VFS.Virtual_File := VFS.No_File;
      Shell_Cmd  : String := "";
      Shell_Lang : String := "";
      Descr      : String;
      Category   : String;
      Menu_Path  : String);
   --  Register the menu in the GPS menubar.
   --  The name of the HTML file is either hard-coded in HTML_File or
   --  read from the result of a shell_cmd

   procedure On_Open_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Help->Open HTML...
   --  Display a file selection dialog, and then open the HTML file in the
   --  help widget.

   procedure Open_HTML_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Anchor : String := "");
   --  Open an HTML file.

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Handles customization strings for this module

   procedure Command_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handler for HTML commands.

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

   type String_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      File       : VFS.Virtual_File;
      Shell      : GNAT.OS_Lib.String_Access;
      Shell_Lang : GNAT.OS_Lib.String_Access;
   end record;
   type String_Menu_Item is access all String_Menu_Item_Record'Class;

   procedure Parse_Index_Files (Kernel : access Kernel_Handle_Record'Class);
   --  Parse all the index files in the path

   type On_Recent is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   procedure Activate (Callback : access On_Recent; Item : String);

   function Create_Html
     (Name   : Glib.UTF8_String;
      Kernel : access Kernel_Handle_Record'Class) return VFS.Virtual_File;
   --  Filename can be a full name or a base name, and can include ancors (e.g
   --  "foo.html#anchor").

   function Get_Shell_Documentation
     (XML_Doc_File : Glib.Xml_Int.Node_Ptr;
      Language, Full_Name : String) return String;
   --  Return the documentation for Entity, as read in the XML file

   function Initialize_XML_Doc
     (Kernel : access Kernel_Handle_Record'Class) return Glib.Xml_Int.Node_Ptr;
   --  Parse and return the XML documentation file

   function Get_Data is new Ada.Unchecked_Conversion
     (System.Address, Glib.Xml_Int.Node_Ptr);
   function Set_Data is new Ada.Unchecked_Conversion
     (Glib.Xml_Int.Node_Ptr, System.Address);
   --  The data stored in an instance of the Help class (this is the XML tree
   --  corresponding to the XML documentation file

   procedure On_Destroy_Html_Class (Value : System.Address);
   pragma Convention (C, On_Destroy_Html_Class);
   --  Called when an instance of the HTML class is destroyed

   -----------------
   -- Create_Html --
   -----------------

   function Create_Html
     (Name   : Glib.UTF8_String;
      Kernel : access Kernel_Handle_Record'Class) return VFS.Virtual_File
   is
      --  We still pass Kernel as a parameter so that we can easily one day
      --  query the module from the kernel instead of keeping a global
      --  variable to store it.
      pragma Unreferenced (Kernel);
      Full : GNAT.OS_Lib.String_Access;
      Anchor : Natural := Index (Name, "#");
   begin
      if Is_Absolute_Path_Or_URL (Name) then
         return Create (Full_Filename => Name);
      end if;

      if Anchor = 0 then
         Anchor := Name'Last + 1;
      end if;

      Full := Locate_Regular_File
        (Locale_From_UTF8 (Name (Name'First .. Anchor - 1)),
         Help_Module_ID.Doc_Path.all);

      if Full = null then
         return VFS.No_File;
      else
         declare
            F : constant String := Locale_To_UTF8 (Full.all);
         begin
            Free (Full);
            if Anchor <= Name'Last then
               return Create (Full_Filename => F & Name (Anchor .. Name'Last));
            else
               return Create (Full_Filename => F);
            end if;
         end;
      end if;
   end Create_Html;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Help_Module_ID_Record) is
   begin
      Free (Module.Categories);
      Free (Module.Doc_Path);
   end Destroy;

   -----------------------------
   -- Get_Shell_Documentation --
   -----------------------------

   function Get_Shell_Documentation
     (XML_Doc_File : Glib.Xml_Int.Node_Ptr;
      Language, Full_Name : String) return String
   is
      Tmp     : Node_Ptr := XML_Doc_File.Child;
      Descr, Params, Returns, See_Also, Example : Unbounded_String;
      Child   : Node_Ptr;
   begin
      if XML_Doc_File = null then
         Trace (Me, "MANU: XML file hasn't been parsed");
      end if;

      Tmp := XML_Doc_File.Child;

      while Tmp /= null loop
         if Tmp.Tag.all = "shell_doc"
           and then Get_Attribute (Tmp, "name", "") = Full_Name
         then
            Child := Tmp.Child;
            while Child /= null loop
               if Child.Tag.all = "description" then
                  Descr := Descr & ASCII.LF & Child.Value.all;

               elsif Child.Tag.all = "param" then
                  if Params /= Null_Unbounded_String then
                     Params := Params & ASCII.LF;
                  end if;

                  Params := Params
                    & Get_Attribute (Child, "name") & ':'
                    & ASCII.HT;

                  declare
                     Default : constant String := Get_Attribute
                       (Child, "default", "@@");
                  begin
                     if Default /= "@@" then
                        Params := Params & "(default=""" & Default & """) ";
                     end if;
                  end;
                  Params := Params & Child.Value.all;

               elsif Child.Tag.all = "return" then
                  Returns := To_Unbounded_String
                    ("returns" & ASCII.HT & Child.Value.all);

               elsif Child.Tag.all = "see_also" then
                  if See_Also /= Null_Unbounded_String then
                     See_Also := See_Also & ASCII.LF;
                  end if;
                  See_Also := See_Also & ASCII.LF
                    & "See also: " & Get_Attribute (Child, "name", "");

               elsif Child.Tag.all = "example" then
                  if Case_Insensitive_Equal
                    (Get_Attribute (Child, "lang", GPS_Shell_Name),
                     Language)
                  then
                     Example := Example & ASCII.LF & ASCII.LF
                       & "Example:"
                       & Child.Value.all;
                  end if;
               end if;

               Child := Child.Next;
            end loop;

            if Params /= Null_Unbounded_String
              and then Returns /= Null_Unbounded_String
            then
               Params := Params & ASCII.LF;
            end if;

            return To_String
              (Params & Returns & ASCII.LF & Descr & See_Also & Example);
         end if;

         Tmp := Tmp.Next;
      end loop;

      return "";
   end Get_Shell_Documentation;

   ------------------------
   -- Initialize_XML_Doc --
   ------------------------

   function Initialize_XML_Doc
     (Kernel : access Kernel_Handle_Record'Class) return Glib.Xml_Int.Node_Ptr
   is
      Error : GNAT.OS_Lib.String_Access;
      Tmp   : Glib.Xml_Int.Node_Ptr;
   begin
      Trace (Me, "Parsing XML file "
             & Get_System_Dir (Kernel)
             & "share/gps/shell_commands.xml");
      XML_Parsers.Parse
        (File  =>
           Get_System_Dir (Kernel) & "share/gps/shell_commands.xml",
         Tree  => Tmp,
         Error => Error);
      if Error /= null then
         Insert (Kernel, Error.all, Mode => Glide_Kernel.Console.Error);
         Free (Error);
      end if;
      return Tmp;
   end Initialize_XML_Doc;

   ---------------------------
   -- On_Destroy_Html_Class --
   ---------------------------

   procedure On_Destroy_Html_Class (Value : System.Address) is
      XML : Node_Ptr := Get_Data (Value);
   begin
      if XML /= null then
         Trace (Me, "Freeing XML file");
         Glib.Xml_Int.Free (XML);
      end if;
   end On_Destroy_Html_Class;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Inst     : Class_Instance;
      XML      : Node_Ptr;
   begin
      if Command = Constructor_Method then
         Inst := Nth_Arg (Data, 1, Help_Module_ID.Help_Class);
         XML  := Initialize_XML_Doc (Kernel);
         Set_Data (Inst, Set_Data (XML), On_Destroy_Html_Class'Access);
         Free (Inst);

      elsif Command = "getdoc" then
         Name_Parameters (Data, Getdoc_Parameters);
         Inst := Nth_Arg (Data, 1, Help_Module_ID.Help_Class);

         declare
            Doc : constant String := Get_Shell_Documentation
              (Get_Data (Get_Data (Inst)),
               Get_Name (Get_Script (Data)), Nth_Arg (Data, 2));
         begin
            if Doc /= "" then
               Set_Return_Value (Data, Doc);
            else
               Set_Error_Msg (Data, "");
            end if;
         end;

      elsif Command = "reset" then
         Inst := Nth_Arg (Data, 1, Help_Module_ID.Help_Class);
         Set_Data (Inst, System.Null_Address);

      elsif Command = "browse" then
         Name_Parameters (Data, Help_Cmd_Parameters);
         Open_HTML_File
           (Get_Kernel (Data),
            File   => Create_Html (Nth_Arg (Data, 1), Get_Kernel (Data)),
            Anchor => Nth_Arg (Data, 2, Default => ""));

      elsif Command = "add_doc_directory" then
         declare
            Old : GNAT.OS_Lib.String_Access := Help_Module_ID.Doc_Path;
         begin
            Help_Module_ID.Doc_Path := new String'
              (Nth_Arg (Data, 1) & Path_Separator & Old.all);
            Free (Old);
         end;

      end if;
   end Command_Handler;

   --------------
   -- Activate --
   --------------

   procedure Activate (Callback : access On_Recent; Item : String) is
   begin
      Open_Html (Callback.Kernel, Create_Html (Item, Callback.Kernel));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Activate;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Help_File_Record) is
   begin
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
      if Item.File /= VFS.No_File then
         Open_Html (Kernel, Create_Html (Full_Name (Item.File).all, Kernel));
      elsif Item.Shell /= null then
         declare
            Errors : aliased Boolean := False;
            File   : constant String := Execute_Command
              (Script      =>
                 Lookup_Scripting_Language (Kernel, Item.Shell_Lang.all),
               Command     => Item.Shell.all,
               Console     => null,
               Hide_Output => False,
               Errors      => Errors'Unchecked_Access);
         begin
            if Errors then
               Insert
                 (Kernel,
                  -"Couldn't generate the HTML file through the shell command "
                  & Item.Shell.all,
                  Mode => Error);
            else
               Open_Html (Kernel, Create_Html (File, Kernel));
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Load_HTML;

   -------------------
   -- Register_Help --
   -------------------

   procedure Register_Help
     (Kernel     : access Kernel_Handle_Record'Class;
      HTML_File  : VFS.Virtual_File := VFS.No_File;
      Shell_Cmd  : String := "";
      Shell_Lang : String := "";
      Descr      : String;
      Category   : String;
      Menu_Path  : String)
   is
      Item : String_Menu_Item;
      Node : Help_Category_List.List_Node;
      Cat  : Help_Category_Access;
   begin
      if Menu_Path /= "" then
         Item := new String_Menu_Item_Record;
         Gtk.Menu_Item.Initialize_With_Mnemonic (Item, Base_Name (Menu_Path));
         Item.File       := HTML_File;
         Item.Shell      := new String'(Shell_Cmd);

         if Shell_Lang = "" then
            Item.Shell_Lang := new String'(GPS_Shell_Name);
         else
            Item.Shell_Lang := new String'(Shell_Lang);
         end if;

         Register_Menu
           (Kernel      => Kernel,
            Parent_Path => Dir_Name (Menu_Path),
            Item        => Gtk_Menu_Item (Item));

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
           (Name  => new String'(Category),
            Files => Help_File_List.Null_List);
         Append (Help_Module_ID.Categories, Cat);
      end if;

      Append (Cat.Files,
              (File       => HTML_File,
               Shell_Cmd  => new String'(Shell_Cmd),
               Shell_Lang => new String'(Shell_Lang),
               Descr      => new String'(Descr)));
   end Register_Help;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Kernel : access Kernel_Handle_Record'Class;
      Html   : access Help_Browser_Record'Class;
      File   : VFS.Virtual_File) return Boolean
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
         Insert (Kernel, Full_Name (File).all & (-": File not found"),
                 Mode => Error);
         return False;
      end if;

      Add_To_History (Kernel, Help_History_Key, Full_Name (File).all);

      Push_State (Kernel_Handle (Kernel), Busy);
      Buffer := Read_File (File);

      if Buffer /= null then
         Trace (Me, "loading file: " & Full_Name (File).all);
         Html.Current_Help_File := File;
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
                  Append (Str, "<tr><td><a href=""");
                  if Data (F).File = VFS.No_File then
                     Append (Str, "%" & Data (F).Shell_Lang.all & ":"
                             & Glib.Xml_Int.Protect (Data (F).Shell_Cmd.all));
                  else
                     Append (Str, Full_Name (Data (F).File).all);
                  end if;

                  Append (Str, """>" & Data (F).Descr.all
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
         Trace (Me, "link not found: " & Full_Name (File).all);
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
      Url_File : Virtual_File;

   begin
      Trace (Me, "url requested: " & Url);

      if Is_Absolute_Path_Or_URL (Url) then
         Url_File := Create (Full_Filename => Url);
      else
         declare
            Base_Dir : constant String :=
              Dir_Name (Html.Current_Help_File).all;
         begin
            Url_File := Create (Full_Filename => Base_Dir & Url);
         end;
      end if;

      Buffer := Read_File (Url_File);
      Trace (Me, "url normalized: " & Full_Name (Url_File).all);

      if Buffer /= null then
         Stream_Write (Stream, Buffer.all);
         Free (Buffer);
      else
         Insert (Html.Kernel, Url & (-": File not found"), Mode => Error);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Url_Requested;

   ------------------
   -- Link_Clicked --
   ------------------

   procedure Link_Clicked
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Kernel : Kernel_Handle)
   is
      Html : constant Help_Browser := Help_Browser (Object);
      Url  : constant String := Get_String (Nth (Params, 1));
      First, Last : Integer;

   begin
      Trace (Me, "Link_Clicked: " & Url);

      if Is_Absolute_Path_Or_URL (Url) then
         Open_Html (Kernel, Create_Html (Url, Kernel));

      elsif Url (Url'First) = '%' then
         First := Url'First + 1;
         Last := First;
         while Last <= Url'Last and then Url (Last) /= ':' loop
            Last := Last + 1;
         end loop;

         declare
            Errors : aliased Boolean := False;
            File   : constant String := Execute_Command
              (Script      => Lookup_Scripting_Language
                 (Kernel, Url (First .. Last - 1)),
               Command     => Url (Last + 1 .. Url'Last),
               Console     => null,
               Hide_Output => True,
               Errors      => Errors'Unchecked_Access);
         begin
            if not Errors then
               Open_Html (Kernel, Create_Html (File, Kernel));
            end if;
         end;

      elsif Url (Url'First) = '#' then
         Open_Html
           (Kernel,
            Create_Html
              (Full_Name (Html.Current_Help_File).all & Url, Kernel));

      else
         Open_Html
           (Kernel,
            Create_Html
              (Dir_Name (Html.Current_Help_File).all & Url, Kernel));
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return True;
   end Key_Press;

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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Success := Load_File (Kernel, Help, Help.Current_Help_File);
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
      Success := Load_File (Kernel, Help, Help.Current_Help_File);
   end On_Zoom_Out;

   ------------------------
   -- Create_Html_Editor --
   ------------------------

   function Create_Html_Editor
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return MDI_Child
   is
      Html   : Help_Browser;
      Child  : MDI_Child;
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

      Child := Put
        (Kernel, Html,
         Focus_Widget => Gtk_Widget (Html.Csc),
         Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
         Default_Height => Get_Pref (Kernel, Default_Widget_Height),
         Module => Help_Module_ID,
         Desktop_Independent => True);
      Set_Title (Child, -"Help");

      return Child;
   end Create_Html_Editor;

   ------------------
   -- Display_Help --
   ------------------

   function Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : VFS.Virtual_File) return Help_Browser
   is
      MDI      : constant MDI_Window := Get_MDI (Kernel);
      Scrolled : Help_Browser;
      Child    : MDI_Child;
      Result   : Boolean;
      pragma Unreferenced (Result);

   begin
      if not Is_Regular_File (Help_File) then
         Insert (Kernel,
                 Full_Name (Help_File).all & (-": File not found"),
                 Mode => Error);
         return null;
      end if;

      Child := Find_MDI_Child_By_Tag (MDI, Help_Browser_Record'Tag);

      if Child = null then
         Child := Create_Html_Editor (Kernel, Help_File);
      end if;

      Scrolled := Help_Browser (Get_Widget (Child));

      if Scrolled.Current_Help_File = VFS.No_File
        or else Help_File /= Scrolled.Current_Help_File
      then
         Result := Load_File (Kernel, Scrolled, Help_File);
      end if;

      Set_Focus_Child (Child);
      Raise_Child (Child);

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
      pragma Unreferenced (MDI);
      File   : Glib.String_Ptr;
   begin
      if Node.Tag.all = "Help_Browser" then
         File := Get_Field (Node, "File");
         if File /= null then
            return Create_Html_Editor (User, Create_Html (File.all, User));
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

         Child.Value := new String'
           (Full_Name (Help_Browser (Widget).Current_Help_File).all);

         Add_Child (N, Child);
         return N;
      end if;

      return null;
   end Save_Desktop;

   --------------------
   -- Open_HTML_File --
   --------------------

   procedure Open_HTML_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Anchor : String := "")
   is
      Html   : Help_Browser;
      Result : Boolean;
      Vadj   : Gtk_Adjustment;
      pragma Unreferenced (Result);
   begin
      Html := Display_Help (Kernel, File);

      if Html /= null then
         if Anchor /= "" then
            Result := Jump_To_Anchor (Html.Csc, Anchor);
         else
            Vadj := Get_Vadjustment (Html);
            Set_Value (Vadj, Get_Lower (Vadj));
            Set_Vadjustment (Html, Vadj);
         end if;
      end if;
   end Open_HTML_File;

   --------------------
   -- Open_Help_Hook --
   --------------------

   function Open_Help_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : Hooks_Data'Class) return Boolean
   is
      D    : constant Html_Hooks_Args := Html_Hooks_Args (Data);
      Args : Argument_List (1 .. 3);
      Html : Virtual_File := Create_Html (Full_Name (D.File).all, Kernel);
   begin
      if Html = VFS.No_File then
         return True;
      else
         Open_HTML_File (Kernel, Html, D.Anchor);
      end if;

      if D.Enable_Navigation then
         Args (1) := new String'("HTML.browse");
         Args (2) := new String'(Full_Name (D.File).all);
         Args (3) := new String'(D.Anchor);

         Execute_GPS_Shell_Command (Kernel, "add_location_command", Args);

         for J in Args'Range loop
            Free (Args (J));
         end loop;
      end if;

      return True;
   end Open_Help_Hook;

   --------------
   -- On_About --
   --------------

   procedure On_About
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      use ASCII;

      Button     : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

      Top        : constant Glide_Window := Glide_Window
        (Get_Main_Window (Kernel));
      About_File : constant String :=
        Format_Pathname (Top.Prefix_Directory.all & "/share/gps/about.txt");
      Contents   : GNAT.OS_Lib.String_Access;

   begin
      Contents := Read_File (About_File);

      if Contents = null then
         Contents := new String'("");
      end if;

      Button := Message_Dialog
        (GPS_Name (Top) & " " & GVD.Version & " (" & GVD.Source_Date &
           (-") hosted on ") & GVD.Target & LF &
         (-"GNAT ") & GNAT_Version (Kernel) & LF & LF &
         (-"the GNAT Programming System") & LF & Contents.all & LF &
         "(c) 2001-2004 ACT-Europe",
         Buttons => Button_OK,
         Title   => -"About...",
         Parent  => Get_Current_Window (Kernel));
      Free (Contents);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Filename : constant Virtual_File :=
           Select_File
             (-"Open HTML File",
              File_Pattern      => "*.htm*",
              Pattern_Name      => -"HTML files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              History           => Get_History (Kernel));

      begin
         if Filename /= VFS.No_File then
            Open_Html (Kernel, Filename);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Open_Html (Kernel, Create_Html ("gps-tutorial.html", Kernel));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Show_Tutorial;

   -------------------------
   -- Set_URL_Information --
   -------------------------

   procedure Set_URL_Information
     (Context : access URL_Context;
      URL     : String := "") is
   begin
      Free (Context.URL);
      Context.URL := new String'(URL);
   end Set_URL_Information;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out URL_Context) is
   begin
      Free (Context.URL);
   end Destroy;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);
      Name, Descr, Menu, Cat : Node_Ptr;
      Shell, Shell_Lang : GNAT.OS_Lib.String_Access;
      N     : Node_Ptr := Node;
      Field : Node_Ptr;
   begin
      while N /= null loop
         if N.Tag.all = "documentation_file" then
            Name  := null;
            Descr := null;
            Menu  := null;
            Cat   := null;
            Shell := null;
            Shell_Lang := null;

            Field := N.Child;
            while Field /= null loop
               if Field.Tag.all = "name" then
                  Name := Field;
               elsif Field.Tag.all = "descr" then
                  Descr := Field;
               elsif Field.Tag.all = "menu" then
                  Menu := Field;
               elsif Field.Tag.all = "category" then
                  Cat := Field;
               elsif Field.Tag.all = "shell" then
                  Shell := new String'(Field.Value.all);
                  Shell_Lang := new String'
                    (Get_Attribute (Field, "lang", "shell"));
               else
                  Insert
                    (Kernel,
                     -"Invalid node in customization file "
                     & Full_Name (File).all & ": " & Field.Tag.all);
               end if;

               Field := Field.Next;
            end loop;

            if Name /= null then
               Trace (Me, "Adding " & Name.Value.all & ' ' & Menu.Value.all);
               Register_Help
                 (Kernel,
                  HTML_File  => Create_Html (Name.Value.all, Kernel),
                  Descr      => Descr.Value.all,
                  Category   => Cat.Value.all,
                  Menu_Path  => Menu.Value.all);
            else
               if Shell = null then
                  Insert
                    (Kernel,
                     -("<documentation_file> customization must specify either"
                       & " a <name> or a <shell> node"),
                     Mode => Error);
               else
                  Register_Help
                    (Kernel,
                     HTML_File  => VFS.No_File,
                     Shell_Cmd  => Shell.all,
                     Shell_Lang => Shell_Lang.all,
                     Descr      => Descr.Value.all,
                     Category   => Cat.Value.all,
                     Menu_Path  => Menu.Value.all);
               end if;
            end if;

            Free (Shell);
            Free (Shell_Lang);
         end if;

         N := N.Next;
      end loop;
   end Customize;

   -----------------------
   -- Parse_Index_Files --
   -----------------------

   procedure Parse_Index_Files (Kernel : access Kernel_Handle_Record'Class) is
      Iter : Path_Iterator;
      Err : GNAT.OS_Lib.String_Access;
   begin
      Iter := Start (Help_Module_ID.Doc_Path.all);
      while not At_End (Help_Module_ID.Doc_Path.all, Iter) loop
         declare
            Dir : constant String :=
              Current (Help_Module_ID.Doc_Path.all, Iter);
            Full : constant String := Name_As_Directory (Dir) & Index_File;
            Node : Node_Ptr;
         begin
            exit when Dir = "";

            if Is_Regular_File (Full) then
               Trace (Me, "Parsing index " & Full);

               XML_Parsers.Parse (Full, Node, Err);

               if Node = null then
                  Insert (Kernel, Err.all, Mode => Error);
                  Free (Err);
               else
                  Customize (Kernel, Create (Full), Node.Child, System_Wide);
                  Free (Node);
               end if;
            end if;
         end;

         Iter := Next (Help_Module_ID.Doc_Path.all, Iter);
      end loop;
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
      Recent_Menu_Item : Gtk_Menu_Item;
      Path_From_Env : GNAT.OS_Lib.String_Access := Getenv ("GPS_DOC_PATH");

   begin
      Help_Module_ID := new Help_Module_ID_Record;
      Register_Module
        (Module                  => Module_ID (Help_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Help_Module_Name,
         Priority                => Glide_Kernel.Default_Priority - 20,
         Contextual_Menu_Handler => null,
         Customization_Handler   => Customize'Access,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
      Add_Hook (Kernel, Html_Action_Hook, Open_Help_Hook'Access);

      if Path_From_Env = null or else Path_From_Env.all = "" then
         Help_Module_ID.Doc_Path := new String'
           (Get_System_Dir (Kernel) & "/doc/gps/html/");
      else
         Help_Module_ID.Doc_Path := new String'
           (Path_From_Env.all & Path_Separator
            & Get_System_Dir (Kernel) & "/doc/gps/html/");
      end if;

      Free (Path_From_Env);

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

      Recent_Menu_Item :=
        Register_Menu (Kernel, Help, -"_Recent", "", null,
                       Ref_Item   => -"Open HTML File...",
                       Add_Before => False);
      Associate (Get_History (Kernel).all,
                 Help_History_Key,
                 Recent_Menu_Item,
                 new On_Recent'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));

      --  Register commands

      Help_Module_ID.Html_Class := New_Class (Kernel, "HTML");
      Help_Module_ID.Help_Class := New_Class (Kernel, "Help");

      Register_Command
        (Kernel,
         Command      => Constructor_Method,
         Class        => Help_Module_ID.Help_Class,
         Handler      => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "getdoc",
         Class         => Help_Module_ID.Help_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "reset",
         Class         => Help_Module_ID.Help_Class,
         Handler       => Command_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "browse",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Help_Module_ID.Html_Class,
         Static_Method => True,
         Handler      => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "add_doc_directory",
         Minimum_Args => Add_Doc_Cmd_Parameters'Length,
         Maximum_Args => Add_Doc_Cmd_Parameters'Length,
         Class        => Help_Module_ID.Html_Class,
         Static_Method => True,
         Handler      => Command_Handler'Access);

      declare
         Item : String_Menu_Item;
      begin
         Item := new String_Menu_Item_Record;
         Gtk.Menu_Item.Initialize_With_Mnemonic (Item, -"_Contents");
         Item.File := Create (Template_Index);
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
