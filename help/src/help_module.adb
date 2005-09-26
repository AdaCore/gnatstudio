-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Object;                use Glib.Object;
with Glib.Xml_Int;               use Glib.Xml_Int;
with XML_Parsers;
with Config;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Main_Window;            use GPS.Main_Window;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.File_Selector;       use Gtkada.File_Selector;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Widget;                 use Gtk.Widget;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Custom;          use GPS.Kernel.Custom;
with Traces;                     use Traces;
with OS_Utils;                   use OS_Utils;
with Ada.Exceptions;             use Ada.Exceptions;
with File_Utils;                 use File_Utils;
with String_Utils;               use String_Utils;
with Generic_List;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Histories;                  use Histories;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with VFS;                        use VFS;
with System;                     use System;
with Welcome_Page;               use Welcome_Page;

package body Help_Module is

   Me : constant Debug_Handle := Create ("GPS.Kernel.Help");

   Template_Index   : constant String := "help_index.html";
   Index_File       : constant String := "gps_index.xml";

   Help_History_Key : constant History_Key := "help-recent-files";

   Url_Cst          : aliased constant String := "URL";
   Anchor_Cst       : aliased constant String := "anchor";
   Dir_Cst          : aliased constant String := "directory";
   Name_Cst         : aliased constant String := "name";
   Navigation_Cst   : aliased constant String := "navigation";

   Browse_Cmd_Parameters  : constant Cst_Argument_List :=
                              (1 => Url_Cst'Access,
                               2 => Anchor_Cst'Access,
                               3 => Navigation_Cst'Access);
   Add_Doc_Cmd_Parameters : constant Cst_Argument_List :=
                              (1 => Dir_Cst'Access);
   Getdoc_Parameters      : constant Cst_Argument_List :=
                              (1 => Name_Cst'Access);

   type Help_File_Record is record
      File       : VFS.Virtual_File;
      Shell_Cmd  : GNAT.OS_Lib.String_Access;
      Shell_Lang : GNAT.OS_Lib.String_Access;
      Descr      : GNAT.OS_Lib.String_Access;
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

   type Help_Module_ID_Record is new Module_ID_Record with record
      Categories : Help_Category_List.List;
      --  The registered help files

      Doc_Path   : GNAT.OS_Lib.String_Access;

      Html_Class : Class_Type;
      Help_Class : Class_Type;
   end record;
   type Help_Module_ID_Access is access all Help_Module_ID_Record'Class;

   procedure Destroy (Module : in out Help_Module_ID_Record);
   procedure Customize
     (Module : access Help_Module_ID_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   function Default_Context_Factory
     (Module : access Help_Module_ID_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  See inherited documentation

   procedure Add_Doc_Directory
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String);
   --  Add a new directory to the documentation path

   Help_Module_ID   : Help_Module_ID_Access;
   Help_Module_Name : constant String := "Help_Viewer";

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Support functions for the MDI

   function Open_Help_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : access Hooks_Data'Class) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : VFS.Virtual_File);
   --  Display HTML Help file.

   procedure Display_Help
     (Kernel    : access Kernel_Handle_Record'Class;
      Help_File : VFS.Virtual_File) is separate;

   procedure On_Load_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Load HMTL_File in the HTML/Help widget

   procedure On_Load_Index
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Create and load the index of all help contents

   procedure Register_Help
     (Kernel     : access Kernel_Handle_Record'Class;
      HTML_File  : VFS.Virtual_File := VFS.No_File;
      Shell_Cmd  : String := "";
      Shell_Lang : String := "";
      Descr      : String;
      Category   : String;
      Menu_Path  : String;
      Menu_Before : String := "";
      Menu_After  : String := "");
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

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for HTML commands.

   procedure On_About
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Menu Help->About...

   procedure On_Welcome
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Menu Help->Welcome...

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

   procedure Parse_Index_File
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String);
   --  Parse the index file for one specific directory

   function Get_Shell_Documentation
     (XML_Doc_File : Glib.Xml_Int.Node_Ptr;
      Language, Full_Name : String;
      HTML_Format : Boolean) return String;
   --  Return the documentation for Entity, as read in the XML file.
   --  The documentation is formated in HTML if HTML_Format is True

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
      Full   : GNAT.OS_Lib.String_Access;
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
      Language, Full_Name : String;
      HTML_Format : Boolean) return String
   is
      Tmp     : Node_Ptr := XML_Doc_File.Child;
      Descr, Params, Returns, See_Also, Example : Unbounded_String;
      Child   : Node_Ptr;
      Obsolescent : Unbounded_String;
   begin
      Tmp := XML_Doc_File.Child;

      while Tmp /= null loop
         if Tmp.Tag.all = "shell_doc"
           and then (Get_Attribute (Tmp, "name", "") = Full_Name
                     or else Get_Attribute (Tmp, "real_name", "") = Full_Name)
         then
            Child       := Tmp.Child;
            Obsolescent := Null_Unbounded_String;

            while Child /= null loop
               if Child.Tag.all = "description" then
                  if HTML_Format then
                     Descr := Descr & "<tr><td colspan='3'>"
                       & Child.Value.all & "</td></tr>";
                  else
                     Descr := Descr & Child.Value.all;
                  end if;

               elsif Child.Tag.all = "obsolescent" then
                  if HTML_Format then
                     Obsolescent := To_Unbounded_String
                       ("<tr><td colspan='3' class='obsolescent'>"
                        & "This is obsolescent</td></tr>");
                  end if;

               elsif Child.Tag.all = "param" then
                  if HTML_Format then
                     Params := Params
                       & "<tr><td class=""name"">"
                       & Get_Attribute (Child, "name") & "</td>";
                  else
                     Params := Params & ASCII.LF
                        & Get_Attribute (Child, "name") & ASCII.HT;
                  end if;

                  declare
                     Default : constant String := Get_Attribute
                       (Child, "default", "@@");
                  begin
                     if Default /= "@@" then
                        if HTML_Format then
                           Params := Params
                             & "<td class='default'>(default="""
                             & Default & """)</td>";
                        else
                           Params := Params & Default & ASCII.HT;
                        end if;
                     elsif HTML_Format then
                        Params :=
                          Params & "<td class='default'>Mandatory</td>";
                     end if;
                  end;

                  if HTML_Format then
                     Params :=
                       Params & "<td>" & Child.Value.all & "</td></tr>";
                  else
                     Params := Params & Child.Value.all;
                  end if;

               elsif Child.Tag.all = "return" then
                  if HTML_Format then
                     Returns := To_Unbounded_String
                       ("</tr><td class=""return"">Returns</td>"
                        & "<td colspan='2' class=""descr"">"
                        & Child.Value.all
                        & "</td></tr>");
                  else
                     Returns :=
                       To_Unbounded_String ("Returns " & Child.Value.all);
                  end if;

               elsif Child.Tag.all = "see_also" then
                  if HTML_Format then
                     See_Also := See_Also
                       & "<tr><td class='header'>See also</td>"
                       & "<td class='seeAlso' colspan='2'>"
                       & Get_Attribute (Child, "name", "")
                       & "</td></tr>";
                  end if;

               elsif Child.Tag.all = "example" then
                  if Equal
                    (Get_Attribute (Child, "lang", GPS_Shell_Name),
                     Language, False)
                  then
                     if HTML_Format then
                        Descr := Descr
                          & "<tr><td colspan='3' class='example'>"
                          & Child.Value.all & "</td></tr>";
                     else
                        Example := Example & ASCII.LF & Child.Value.all;
                     end if;
                  end if;
               end if;

               Child := Child.Next;
            end loop;

            if HTML_Format then
               return To_String
                 (Obsolescent & Params & Returns & Descr & Example & See_Also);
            else
               return To_String
                 (Params & Returns & ASCII.LF & Descr & ASCII.LF & Example);
            end if;
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
         Insert (Kernel, Error.all, Mode => GPS.Kernel.Console.Error);
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
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Inst   : Class_Instance;
      XML    : Node_Ptr;
   begin
      if Command = Constructor_Method then
         Inst := Nth_Arg (Data, 1, Help_Module_ID.Help_Class);
         XML  := Initialize_XML_Doc (Kernel);
         Set_Data (Inst, Help_Module_ID.Help_Class,
                   Set_Data (XML), On_Destroy_Html_Class'Access);
         Free (Inst);

      elsif Command = "getdoc" then
         Name_Parameters (Data, Getdoc_Parameters);
         Inst := Nth_Arg (Data, 1, Help_Module_ID.Help_Class);

         declare
            Doc : constant String := Get_Shell_Documentation
              (Get_Data (Get_Data (Inst, Help_Module_ID.Help_Class)),
               Get_Name (Get_Script (Data)), Nth_Arg (Data, 2),
               HTML_Format => Nth_Arg (Data, 3, False));
         begin
            if Doc /= "" then
               Set_Return_Value (Data, Doc);
            else
               Set_Error_Msg
                 (Data, "No documentation for " & Nth_Arg (Data, 2));
            end if;
         end;

         Free (Inst);

      elsif Command = "file" then
         Set_Return_Value
           (Data, Get_System_Dir (Kernel)
            & "share/gps/shell_commands.xml");

      elsif Command = "reset" then
         Inst := Nth_Arg (Data, 1, Help_Module_ID.Help_Class);
         Set_Data (Inst, Help_Module_ID.Help_Class, System.Null_Address);
         Free (Inst);

      elsif Command = "browse" then
         Name_Parameters (Data, Browse_Cmd_Parameters);
         declare
            File : constant Virtual_File :=
              Create_Html (Nth_Arg (Data, 1), Get_Kernel (Data));
            Anchor : constant String := Nth_Arg (Data, 2, Default => "");
         begin
            Open_HTML_File
              (Get_Kernel (Data),
               File   => File,
               Anchor => Anchor);
         end;

      elsif Command = "add_doc_directory" then
         Add_Doc_Directory (Get_Kernel (Data), Nth_Arg (Data, 1));
      end if;
   end Command_Handler;

   -----------------------
   -- Add_Doc_Directory --
   -----------------------

   procedure Add_Doc_Directory
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String)
   is
      Old : GNAT.OS_Lib.String_Access := Help_Module_ID.Doc_Path;
      Dir : constant String := Normalize_Pathname
        (Directory, Get_System_Dir (Kernel));
      Iter : Path_Iterator := Start (Old.all);
   begin
      while not At_End (Old.all, Iter) loop
         if Current (Old.all, Iter) = Dir then
            return;
         end if;
         Iter := Next (Old.all, Iter);
      end loop;

      Trace (Me, "Adding " & Dir & " to GPS_DOC_PATH");
      Help_Module_ID.Doc_Path := new String'(Dir & Path_Separator & Old.all);
      Free (Old);

      Parse_Index_File (Kernel, Directory => Dir);
   end Add_Doc_Directory;

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

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   function Default_Context_Factory
     (Module : access Help_Module_ID_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Child);
      Context : URL_Context_Access;
   begin
      Context := new URL_Context;

      Set_Context_Information
        (Context => Context,
         Kernel  => Get_Kernel (Module.all),
         Creator => Abstract_Module_ID (Module));

      return Selection_Context_Access (Context);
   end Default_Context_Factory;

   ------------------
   -- On_Load_HTML --
   ------------------

   procedure On_Load_HTML
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Item      : constant String_Menu_Item := String_Menu_Item (Widget);
      HTML_File : VFS.Virtual_File := VFS.No_File;
   begin
      if Item.File /= VFS.No_File then
         HTML_File := Create_Html (Full_Name (Item.File).all, Kernel);
         Trace (Me, "Loading HTML file " & Full_Name (Item.File).all
                & " => " & Full_Name (HTML_File).all);

      elsif Item.Shell /= null then
         Trace (Me, "On_Load_HTML: No file specified, executing shell cmd");
         declare
            Errors : aliased Boolean := False;
            File   : constant String := GPS.Kernel.Scripts.Execute_Command
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
               HTML_File := Create_Html (File, Kernel);
            end if;
         end;
      end if;

      if HTML_File /= VFS.No_File then
         Open_Html (Kernel, HTML_File);
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
     (Kernel      : access Kernel_Handle_Record'Class;
      HTML_File   : VFS.Virtual_File := VFS.No_File;
      Shell_Cmd   : String := "";
      Shell_Lang  : String := "";
      Descr       : String;
      Category    : String;
      Menu_Path   : String;
      Menu_Before : String := "";
      Menu_After  : String := "")
   is
      Item : String_Menu_Item;
      Node : Help_Category_List.List_Node;
      Cat  : Help_Category_Access;
   begin
      if Menu_Path /= "" then
         Item := new String_Menu_Item_Record;
         Gtk.Menu_Item.Initialize_With_Mnemonic (Item, Base_Name (Menu_Path));
         Item.File       := HTML_File;

         if Shell_Cmd /= "" then
            Item.Shell      := new String'(Shell_Cmd);
         end if;

         if Shell_Lang = "" then
            Item.Shell_Lang := new String'(GPS_Shell_Name);
         else
            Item.Shell_Lang := new String'(Shell_Lang);
         end if;

         if Menu_Before /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path => Dir_Name (Menu_Path),
               Item        => Gtk_Menu_Item (Item),
               Ref_Item    => Menu_Before,
               Add_Before  => True);
         elsif Menu_After /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path => Dir_Name (Menu_Path),
               Item        => Gtk_Menu_Item (Item),
               Ref_Item    => Menu_After,
               Add_Before  => False);
         else
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path => Dir_Name (Menu_Path),
               Item        => Gtk_Menu_Item (Item));
         end if;

         Kernel_Callback.Connect
           (Item, "activate", On_Load_HTML'Access, Kernel_Handle (Kernel));
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
      if Node.Tag.all = "Welcome_Page" then
         return Create_Welcome_Page (User);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Welcome_Page_Record'Class then
         N := new Node;
         N.Tag := new String'("Welcome_Page");

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
      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      Trace (Me, "Open_HTML_File " & Full_Name (File).all & " #" & Anchor);
      Display_Help (Kernel, File);

      --  ??? should we pass URLs (file://) to the web browser instead of
      --  files ?
   end Open_HTML_File;

   --------------------
   -- Open_Help_Hook --
   --------------------

   function Open_Help_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D    : constant Html_Hooks_Args := Html_Hooks_Args (Data.all);
      Html : constant Virtual_File :=
        Create_Html (Full_Name (D.File).all, Kernel);
   begin
      if Html = VFS.No_File then
         return True;
      else
         Open_HTML_File (Kernel, Html, D.Anchor);
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

      Top        : constant GPS_Window := GPS_Window
        (Get_Main_Window (Kernel));
      About_File : constant String :=
        Format_Pathname (Get_System_Dir (Kernel) & "/share/gps/about.txt");
      Contents   : GNAT.OS_Lib.String_Access;

   begin
      Contents := Read_File (About_File);

      if Contents = null then
         Contents := new String'("");
      end if;

      Button := Message_Dialog
        (GPS_Name (Top) & " " & Config.Version & " (" & Config.Source_Date &
           (-") hosted on ") & Config.Target & LF &
         (-"GNAT ") & GNAT_Version (Kernel) & LF & LF &
         (-"the GNAT Programming Studio") & LF & Contents.all & LF &
         "(c) 2001-2005 AdaCore",
         Buttons => Button_OK,
         Title   => -"About...",
         Parent  => Get_Current_Window (Kernel));
      Free (Contents);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_About;

   ----------------
   -- On_Welcome --
   ----------------

   procedure On_Welcome
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Display_Welcome_Page (Kernel);
   end On_Welcome;

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
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
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
     (Module : access Help_Module_ID_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);
      Kernel : constant Kernel_Handle := Get_Kernel (Module.all);
      Name, Descr, Menu, Cat : Node_Ptr;
      Shell, Shell_Lang      : GNAT.OS_Lib.String_Access;
      Field                  : Node_Ptr;
      HTML_File              : Virtual_File;
   begin
      if Node.Tag.all = "doc_path" then
         Add_Doc_Directory (Kernel, Node.Value.all);

      elsif Node.Tag.all = "documentation_file" then
         Name  := null;
         Descr := null;
         Menu  := null;
         Cat   := null;
         Shell := null;
         Shell_Lang := null;

         Field := Node.Child;
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

         if Menu = null then
            Insert (Kernel,
                    -"<documentation_file> must have a <menu> child",
                    Mode => Error);
         elsif Name /= null then
            HTML_File := Create_Html (Name.Value.all, Kernel);
            if HTML_File = VFS.No_File then
               Trace (Me, "Not adding " & Name.Value.all
                      & " since file not found");
            else
               Trace (Me, "Adding " & Name.Value.all & ' ' & Menu.Value.all);
               Register_Help
                 (Kernel,
                  HTML_File   => Create_Html (Name.Value.all, Kernel),
                  Descr       => Descr.Value.all,
                  Category    => Cat.Value.all,
                  Menu_Before => Get_Attribute (Menu, "before", ""),
                  Menu_After  => Get_Attribute (Menu, "after", ""),
                  Menu_Path   => Menu.Value.all);
            end if;
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
                  HTML_File   => VFS.No_File,
                  Shell_Cmd   => Shell.all,
                  Shell_Lang  => Shell_Lang.all,
                  Descr       => Descr.Value.all,
                  Category    => Cat.Value.all,
                  Menu_Before => Get_Attribute (Menu, "before", ""),
                  Menu_After  => Get_Attribute (Menu, "after", ""),
                  Menu_Path   => Menu.Value.all);
            end if;
         end if;

         Free (Shell);
         Free (Shell_Lang);
      end if;
   end Customize;

   ----------------------
   -- Parse_Index_File --
   ----------------------

   procedure Parse_Index_File
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : String)
   is
      Full    : constant String := Name_As_Directory (Directory) & Index_File;
      Node, N : Node_Ptr;
      Err     : GNAT.OS_Lib.String_Access;
   begin
      if Is_Regular_File (Full) then
         Trace (Me, "Parsing index " & Full);

         XML_Parsers.Parse (Full, Node, Err);

         if Node = null then
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);
         else
            N := Node.Child;
            while N /= null loop
               Customize (Help_Module_ID, Create (Full), N, System_Wide);
               N := N.Next;
            end loop;
            Free (Node);
         end if;
      end if;
   end Parse_Index_File;

   -----------------------
   -- Parse_Index_Files --
   -----------------------

   procedure Parse_Index_Files (Kernel : access Kernel_Handle_Record'Class) is
      Iter : Path_Iterator;
   begin
      Iter := Start (Help_Module_ID.Doc_Path.all);
      while not At_End (Help_Module_ID.Doc_Path.all, Iter) loop
         Parse_Index_File
           (Kernel    => Kernel,
            Directory => Current (Help_Module_ID.Doc_Path.all, Iter));
         Iter := Next (Help_Module_ID.Doc_Path.all, Iter);
      end loop;
   end Parse_Index_Files;

   -------------------
   -- On_Load_Index --
   -------------------

   procedure On_Load_Index
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      File     : constant Virtual_File := Create_Html (Template_Index, Kernel);
      Buffer   : GNAT.OS_Lib.String_Access := Read_File (File);
      Index    : Natural;
      Str      : Unbounded_String;
      In_Category : Unbounded_String;
      Cat      : Help_Category_List.List_Node;
      F        : Help_File_List.List_Node;
      Contents_Marker : constant String := ASCII.LF & "@@CONTENTS@@";
      Output   : constant Virtual_File :=
        Create (Get_Home_Dir (Kernel) & "help_index.html");
      Output_Write : Writable_File;

   begin
      Trace (Me, "loading file: " & Full_Name (File).all);
      if Buffer /= null then
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
            In_Category := Null_Unbounded_String;

            F := First (Data (Cat).Files);
            while F /= Help_File_List.Null_Node loop
               if Data (F).File /= VFS.No_File then
                  Append (In_Category, "<tr><td><a href=""");
                  Append (In_Category, Full_Name (Data (F).File).all);
                  Append (In_Category, """>" & Data (F).Descr.all
                          & "</a></td></tr>");
               end if;
               F := Next (F);
            end loop;

            if In_Category /= Null_Unbounded_String then
               Append (Str,
                       "<tr><td bgcolor=""#006db6"">"
                       & "<font face=""tahoma"" size=""+2"" color=""#FFFFFF"">"
                       & Data (Cat).Name.all
                       & "</font></td> </tr>" & ASCII.LF);
               Append (Str, In_Category);
            end if;

            Cat := Next (Cat);
         end loop;

         Append (Str, "</table>");
         Append
           (Str, Buffer (Index + Contents_Marker'Length .. Buffer'Last));

         Output_Write := Write_File (Output);
         Write (Output_Write, To_String (Str), As_UTF8 => False);
         Close (Output_Write);

         Free (Buffer);

         Open_Html (Kernel, Output);
      end if;
   end On_Load_Index;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Help             : constant String := "/_" & (-"Help") & '/';
      Recent_Menu_Item : Gtk_Menu_Item;
      Path_From_Env    : GNAT.OS_Lib.String_Access := Getenv ("GPS_DOC_PATH");

   begin
      Help_Module_ID := new Help_Module_ID_Record;
      Register_Module
        (Module       => Module_ID (Help_Module_ID),
         Kernel       => Kernel,
         Module_Name  => Help_Module_Name,
         Priority     => GPS.Kernel.Modules.Default_Priority - 20);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
      Add_Hook (Kernel, Html_Action_Hook, Open_Help_Hook'Access);

      if Path_From_Env.all = "" then
         Help_Module_ID.Doc_Path := new String'
           (Get_System_Dir (Kernel) & "doc/gps/html/");
      else
         Help_Module_ID.Doc_Path := new String'
           (Path_From_Env.all & Path_Separator &
            Get_System_Dir (Kernel) & "doc/gps/html/");
      end if;

      --  Add help menus

      Register_Menu
        (Kernel, Help, -"_Welcome", "", On_Welcome'Access);

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
         Command => Constructor_Method,
         Class   => Help_Module_ID.Help_Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "getdoc",
         Class        => Help_Module_ID.Help_Class,
         Minimum_Args => 1,
         Maximum_Args => 2,
         Handler      => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command => "reset",
         Class   => Help_Module_ID.Help_Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command => "file",
         Class   => Help_Module_ID.Help_Class,
         Handler => Command_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "browse",
         Minimum_Args  => 1,
         Maximum_Args  => 3,
         Class         => Help_Module_ID.Html_Class,
         Static_Method => True,
         Handler       => Command_Handler'Access);
      Register_Command
        (Kernel,
         Command       => "add_doc_directory",
         Minimum_Args  => Add_Doc_Cmd_Parameters'Length,
         Maximum_Args  => Add_Doc_Cmd_Parameters'Length,
         Class         => Help_Module_ID.Html_Class,
         Static_Method => True,
         Handler       => Command_Handler'Access);

      Register_Menu
        (Kernel,
         Parent_Path => Help,
         Text        => -"_Contents",
         Callback    => On_Load_Index'Access);

      Parse_Index_Files (Kernel);

      Register_Menu
        (Kernel, Help, -"A_bout", "", On_About'Access);

      if Path_From_Env.all = "" then
         --  We add the custom path here to make sure that the node parsed by
         --  the custom module will be able to find the documentation. We have
         --  not added this path before as we don't wan't to parse the
         --  documentation node twice.

         Free (Help_Module_ID.Doc_Path);

         Help_Module_ID.Doc_Path := new String'
           (Get_Custom_Path & Path_Separator &
            Get_System_Dir (Kernel) & "doc/gps/html/");
      end if;

      Free (Path_From_Env);
   end Register_Module;

end Help_Module;
