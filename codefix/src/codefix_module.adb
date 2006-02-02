-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
--                              AdaCore                              --
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

--  This package defines the module for code fixing.

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;               use GNAT.Regpat;
with System;

with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Glib.Object;               use Glib.Object;

with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Types;               use Basic_Types;
with Codefix.Errors_Parser;     use Codefix.Errors_Parser;
with Codefix.GPS_Io;            use Codefix.GPS_Io;
with Codefix.Graphics;          use Codefix.Graphics;
with Codefix.Text_Manager;      use Codefix.Text_Manager;
with Commands.Codefix;          use Commands.Codefix;
with Commands;                  use Commands;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Projects.Registry;         use Projects.Registry;
with Projects;                  use Projects;
with Traces;                    use Traces;
with VFS;                       use VFS;
with Glib;                      use Glib;

package body Codefix_Module is

   use Codefix.Formal_Errors.Command_List;

   Codefix_Answer_Xpm : aliased Pixmap_Array;
   pragma Import (C, Codefix_Answer_Xpm, "codefix_answer_xpm");

   Codefix_Ambiguous_Xpm : aliased Pixmap_Array;
   pragma Import (C, Codefix_Ambiguous_Xpm, "codefix_ambiguous_xpm");

   Me          : constant Debug_Handle := Create ("Codefix_Module");

   Codefix_GUI : constant Debug_Handle := Create ("Codefix_GUI", Off);
   --  ??? Disabled by default, as the UI is not ready yet.

   Location_Button_Name : constant String := "Codefix";

   Output_Cst        : aliased constant String := "output";
   Category_Cst      : aliased constant String := "category";
   Regexp_Cst        : aliased constant String := "regexp";
   File_Index_Cst    : aliased constant String := "file_index";
   Line_Index_Cst    : aliased constant String := "line_index";
   Col_Index_Cst     : aliased constant String := "column_index";
   Style_Index_Cst   : aliased constant String := "style_index";
   Warning_Index_Cst : aliased constant String := "warning_index";
   Msg_Index_Cst     : aliased constant String := "msg_index";
   File_Cst          : aliased constant String := "file_location";
   Message_Cst       : aliased constant String := "message";
   Codefix_Cst       : aliased constant String := "codefix";
   Choice_Cst        : aliased constant String := "choice";
   Parse_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Category_Cst'Access,
      2 => Output_Cst'Access,
      3 => Regexp_Cst'Access,
      4 => File_Index_Cst'Access,
      5 => Line_Index_Cst'Access,
      6 => Col_Index_Cst'Access,
      7 => Msg_Index_Cst'Access,
      8 => Style_Index_Cst'Access,
      9 => Warning_Index_Cst'Access);
   Codefix_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Category_Cst'Access);
   Error_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Codefix_Cst'Access,
      2 => File_Cst'Access,
      3 => Message_Cst'Access);
   Fix_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Choice_Cst'Access);

   type Codefix_Sessions_Array is array (Natural range <>) of Codefix_Session;
   type Codefix_Sessions is access Codefix_Sessions_Array;

   type Codefix_Module_ID_Record is new Module_ID_Record with record
      Sessions      : Codefix_Sessions;
      Codefix_Class : Class_Type;
      Codefix_Error_Class : Class_Type;
   end record;
   type Codefix_Module_ID_Access is access all Codefix_Module_ID_Record'Class;

   Codefix_Module_ID   : Codefix_Module_ID_Access;
   Codefix_Module_Name : constant String := "Code_Fixing";

   procedure Destroy (Id : in out Codefix_Module_ID_Record);

   type Codefix_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Fix_Command  : Ptr_Command;
      Error        : Error_Id;
      Kernel       : Kernel_Handle;
      Session      : Codefix_Session;
   end record;
   type Codefix_Menu_Item is access all Codefix_Menu_Item_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Codefix_Sessions_Array, Codefix_Sessions);

   procedure Destroy (Session : in out Codefix_Session);
   --  Destroy the contents of the session

   procedure Gtk_New (This : out Codefix_Menu_Item; Label : String := "");
   procedure Initialize
     (Menu_Item : access Codefix_Menu_Item_Record'Class;
      Label     : String);
   --  Create a new codefix menu item

   procedure Execute_Corrupted_Cb
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Error_Message : String);
   --  Handles error messages when an error can no longer be corrected.

   procedure Codefix_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  ??? Will be removed soon

   procedure On_Fix
     (Kernel  : access Kernel_Handle_Record'Class;
      Session : Codefix_Session;
      Error   : Error_Id;
      Command : Text_Command'Class);
   procedure On_Fix (Widget : access Gtk_Widget_Record'Class);
   --  Fixes the error that is proposed on a Menu_Item of Codefix.

   procedure Codefix_Handler
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Creates and shows the Codefix window.

   procedure Compilation_Finished_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Initializes the fix list of Codefix.

   type GPS_Navigator is new Text_Navigator_Abstr with null record;

   function Get_Body_Or_Spec
     (Text : GPS_Navigator; File_Name : Virtual_File) return Virtual_File;
   --  See inherited documentation

   function New_Text_Interface (This : GPS_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class);
   --  Set the value of the Text_Interface's kernel

   procedure Activate_Codefix
     (Kernel               : Kernel_Handle;
      Output               : String;
      Category             : String;
      File_Location_Regexp : String := "";
      File_Index           : Integer := -1;
      Line_Index           : Integer := -1;
      Col_Index            : Integer := -1;
      Msg_Index            : Integer := -1;
      Style_Index          : Integer := -1;
      Warning_Index        : Integer := -1);
   --  Activate postfix for a given compilation output.
   --  The regular expression and the indexes are used to extract relevant
   --  information from the error messages. The default is to use the GPS's
   --  predefined regexps that matches GNAT and gcc error messages.

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for this module

   procedure Error_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for CodefixError class

   function Get_Session_By_Name (Category : String) return Codefix_Session;
   --  Find the codefix session associated with the given category, or null
   --  if there is none.

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Session  : Codefix_Session);
   function Get_Data
     (Instance : access Class_Instance_Record'Class) return Codefix_Session;
   --  Set or retrieve the session from an instance of Codefix

   type Codefix_Error_Data is record
      Error   : Error_Id;
      Session : Codefix_Session;
   end record;
   type Codefix_Error_Data_Access is access Codefix_Error_Data;

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Error    : Codefix_Error_Data);
   function Get_Data
     (Instance : access Class_Instance_Record'Class) return Codefix_Error_Data;
   --  Set or retrieve the error from an instance of CodefixError

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Codefix_Error_Data_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Codefix_Error_Data, Codefix_Error_Data_Access);

   procedure On_Destroy_Error (Value : System.Address);
   pragma Convention (C, On_Destroy_Error);
   --  Destroy a Codefix_Error_Data_Access

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Session : in out Codefix_Session) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Codefix_Session_Record, Codefix_Session);
   begin
      Free (Session.Current_Text);
      Free (Session.Corrector);
      Free (Session.Category);
      Unchecked_Free (Session);
   end Destroy;

   ----------------------
   -- Get_Body_Or_Spec --
   ----------------------

   function Get_Body_Or_Spec
     (Text : GPS_Navigator; File_Name : Virtual_File) return Virtual_File
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Text);
   begin
      return Create
        (Name           => Other_File_Base_Name
           (Get_Project_From_File
              (Project_Registry (Get_Registry (Kernel).all), File_Name),
            File_Name),
         Kernel         => Kernel,
         Use_Object_Path => False);
   end Get_Body_Or_Spec;

   ------------
   -- On_Fix --
   ------------

   procedure On_Fix
     (Kernel  : access Kernel_Handle_Record'Class;
      Session : Codefix_Session;
      Error   : Error_Id;
      Command : Text_Command'Class)
   is
      Err : constant Error_Message := Get_Error_Message (Error);
   begin
      Validate_And_Commit
        (Session.Corrector.all, Session.Current_Text.all, Error, Command);

      Remove_Location_Action
        (Kernel        => Kernel,
         Identifier    => Location_Button_Name,
         Category      => Session.Category.all,
         File          => Get_File (Err),
         Line          => Get_Line (Err),
         Column        => Get_Column (Err),
         Message       => Get_Message (Err));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Fix;

   ------------
   -- On_Fix --
   ------------

   procedure On_Fix (Widget : access Gtk_Widget_Record'Class) is
      Mitem : constant Codefix_Menu_Item := Codefix_Menu_Item (Widget);
   begin
      On_Fix (Mitem.Kernel, Mitem.Session, Mitem.Error, Mitem.Fix_Command.all);
   end On_Fix;

   ----------------------
   -- Activate_Codefix --
   ----------------------

   procedure Activate_Codefix
     (Kernel               : Kernel_Handle;
      Output               : String;
      Category             : String;
      File_Location_Regexp : String := "";
      File_Index           : Integer := -1;
      Line_Index           : Integer := -1;
      Col_Index            : Integer := -1;
      Msg_Index            : Integer := -1;
      Style_Index          : Integer := -1;
      Warning_Index        : Integer := -1)
   is
      Errors_Found : Compilation_Output;
      Session      : Codefix_Session;
      Fi, Li, Ci, Mi, Si, Wi : Integer;

      Command : Command_Access;
   begin
      if Codefix_Module_ID.Sessions /= null then
         for S in Codefix_Module_ID.Sessions'Range loop
            if Codefix_Module_ID.Sessions (S).Category.all = Category then
               Session := Codefix_Module_ID.Sessions (S);
               exit;
            end if;
         end loop;
      end if;

      if Session = null then
         declare
            Tmp : Codefix_Sessions := Codefix_Module_ID.Sessions;
         begin
            Session := new Codefix_Session_Record;
            Session.Category := new String'(Category);

            if Tmp = null then
               Codefix_Module_ID.Sessions := new Codefix_Sessions_Array'
                 (1 .. 1 => Session);
            else
               Codefix_Module_ID.Sessions := new Codefix_Sessions_Array'
                 (Tmp.all & Session);
            end if;

            Unchecked_Free (Tmp);
         end;
      end if;

      Free (Session.Corrector);
      Free (Session.Current_Text);

      Session.Corrector    := new Correction_Manager;
      Session.Current_Text := new GPS_Navigator;

      if File_Index = -1 then
         Fi := Integer (Get_Pref (File_Pattern_Index));
      else
         Fi := File_Index;
      end if;

      if Line_Index = -1 then
         Li := Integer (Get_Pref (Line_Pattern_Index));
      else
         Li := Line_Index;
      end if;

      if Col_Index = -1 then
         Ci := Integer (Get_Pref (Column_Pattern_Index));
      else
         Ci := Col_Index;
      end if;

      if Msg_Index = -1 then
         Mi := Integer (Get_Pref (Message_Pattern_Index));
      else
         Mi := Msg_Index;
      end if;

      if Style_Index = -1 then
         Si := Integer (Get_Pref (Style_Pattern_Index));
      else
         Si := Style_Index;
      end if;

      if Warning_Index = -1 then
         Wi := Integer (Get_Pref (Warning_Pattern_Index));
      else
         Wi := Warning_Index;
      end if;

      if File_Location_Regexp = "" then
         Set_Regexp
           (Errors_Found,
            File_Location_Regexp => Compile (Get_Pref (File_Pattern)),
            File_Index_In_Regexp    => Fi,
            Line_Index_In_Regexp    => Li,
            Col_Index_In_Regexp     => Ci,
            Msg_Index_In_Regexp     => Mi,
            Style_Index_In_Regexp   => Si,
            Warning_Index_In_Regexp => Wi);
      else
         Set_Regexp
           (Errors_Found,
            File_Location_Regexp => Compile (File_Location_Regexp),
            File_Index_In_Regexp    => Fi,
            Line_Index_In_Regexp    => Li,
            Col_Index_In_Regexp     => Ci,
            Msg_Index_In_Regexp     => Mi,
            Style_Index_In_Regexp   => Si,
            Warning_Index_In_Regexp => Wi);
      end if;

      Set_Kernel      (Session.Current_Text.all, Kernel);
      Set_Error_Cb    (Session.Corrector.all, Execute_Corrupted_Cb'Access);
      Set_Last_Output (Errors_Found, Kernel, Output);
      Analyze
        (Session.Corrector.all, Session.Current_Text.all,
         Errors_Found, null);

      --  Update the location window to show which errors can be fixed

      Command := new Codefix_Add_Command;
      Codefix_Add_Command (Command.all).Kernel := Kernel;
      Codefix_Add_Command (Command.all).Session := Session;
      Codefix_Add_Command (Command.all).Current_Error :=
        Get_First_Error (Session.Corrector.all);
      Codefix_Add_Command (Command.all).Errors_Num :=
        Get_Number_Of_Errors (Session.Corrector.all);

      Launch_Background_Command
        (Kernel,
         Command,
         Active   => True,
         Show_Bar => True,
         Queue_Id => Codefix_Module_Name);

      Free (Errors_Found);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Activate_Codefix;

   -----------------------------
   -- Compilation_Finished_Cb --
   -----------------------------

   procedure Compilation_Finished_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Compilation_Data : constant Compilation_Hooks_Args :=
        Compilation_Hooks_Args (Data.all);

   begin
      Activate_Codefix
        (Kernel_Handle (Kernel),
         Execute_GPS_Shell_Command (Kernel, "get_build_output"),
         Compilation_Data.Category);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Compilation_Finished_Cb;

   ---------------------
   -- Codefix_Handler --
   ---------------------

   procedure Codefix_Handler
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Graphic_Codefix : Graphic_Codefix_Access;
      Child           : GPS_MDI_Child;
      Session         : Codefix_Session;
   begin
      if Codefix_Module_ID.Sessions = null then
         return;
      end if;

      --  ??? Should select session interactively

      Session := Codefix_Module_ID.Sessions
        (Codefix_Module_ID.Sessions'First);

      Update_All (Session.Current_Text.all);

      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Graphic_Codefix_Record'Tag));
      if Child = null then
         Gtk_New
           (Graphic_Codefix,
            Kernel,
            Session,
            Remove_Pixmap'Access,
            Create_Pixmap_And_Category'Access);
         Gtk_New (Child, Graphic_Codefix,
                  Module => Codefix_Module_ID);
         Set_Title (Child, -"Code fixing", -"Codefix");
         Put (Get_MDI (Kernel), Child);
      else
         Graphic_Codefix := Graphic_Codefix_Access (Get_Widget (Child));
         Load_Next_Error (Graphic_Codefix, True);
         Raise_Child (Child);
      end if;

      Set_Focus_Child (Child);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Codefix_Handler;

   -------------------------
   -- Get_Session_By_Name --
   -------------------------

   function Get_Session_By_Name (Category : String) return Codefix_Session is
   begin
      if Codefix_Module_ID.Sessions /= null then
         for S in Codefix_Module_ID.Sessions'Range loop
            if Codefix_Module_ID.Sessions (S).Category.all = Category then
               return Codefix_Module_ID.Sessions (S);
            end if;
         end loop;
      end if;
      return null;
   end Get_Session_By_Name;

   -------------------
   -- Remove_Pixmap --
   -------------------

   procedure Remove_Pixmap
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id)
   is
      Err : constant Error_Message := Get_Error_Message (Error);
   begin
      Remove_Location_Action
        (Kernel     => Kernel,
         Identifier => Location_Button_Name,
         Category   => Session.Category.all,
         File       => Get_File (Err),
         Line       => Get_Line (Err),
         Column     => Get_Column (Err),
         Message    => Get_Message (Err));
   end Remove_Pixmap;

   --------------------------------
   -- Create_Pixmap_And_Category --
   --------------------------------

   procedure Create_Pixmap_And_Category
     (Kernel       : access Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id)
   is
      New_Action    : Action_Item;
      Err : constant Error_Message := Get_Error_Message (Error);
   begin
      New_Action := new Line_Information_Record;
      New_Action.Text := new String'(-"Fix error");

      if Get_Number_Of_Fixes (Error) = 1 then
         New_Action.Image := Gdk_New_From_Xpm_Data (Codefix_Answer_Xpm);
      else
         New_Action.Image := Gdk_New_From_Xpm_Data (Codefix_Ambiguous_Xpm);
      end if;

      New_Action.Associated_Command := new Codefix_Command;
      Codefix_Command (New_Action.Associated_Command.all).Error   := Error;
      Codefix_Command (New_Action.Associated_Command.all).Session :=
        Codefix_Session (Session);
      Codefix_Command (New_Action.Associated_Command.all).Kernel :=
        Kernel_Handle (Kernel);

      Add_Location_Action
        (Kernel        => Kernel,
         Identifier    => Location_Button_Name,
         Category      => Session.Category.all,
         File          => Get_File (Err),
         Line          => Get_Line (Err),
         Column        => Get_Column (Err),
         Message       => Get_Message (Err),
         Action        => New_Action);
   end Create_Pixmap_And_Category;

   -----------------------------
   -- Codefix_Contextual_Menu --
   -----------------------------

   procedure Codefix_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Location      : Message_Context_Access;
      Session       : Codefix_Session;
      Error         : Error_Id;

   begin
      if Context.all in Message_Context'Class then
         Location := Message_Context_Access (Context);

         if not Has_Category_Information (Location)
           or else Codefix_Module_ID.Sessions = null
         then
            return;
         end if;

         Session := Get_Session_By_Name (Category_Information (Location));

         if Session = null then
            return;
         end if;

         if not Has_Message_Information (Location)
           or else not Has_File_Information (Location)
         then
            return;
         end if;

         Error := Search_Error
           (Session.Corrector.all,
            File    => File_Information (Location),
            Line    => Line_Information (Location),
            Column  => Column_Information (Location),
            Message => Message_Information (Location));

         if Error /= Null_Error_Id and then not Is_Fixed (Error) then
            Create_Submenu (Get_Kernel (Context), Menu, Session, Error);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Codefix_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Codefix_Module_ID := new Codefix_Module_ID_Record;
      Register_Module
        (Module                  => Module_ID (Codefix_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Codefix_Module_Name,
         Priority                => Default_Priority);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Code Fixing",
         Submenu => Codefix_Contextual_Menu'Access);

      if Active (Codefix_GUI) then
         Register_Menu
           (Kernel      => Kernel,
            Parent_Path => "/" & (-"Tools"),
            Text        => -"_Code Fixing",
            Callback    => Codefix_Handler'Access);
      end if;

      Add_Hook
        (Kernel, Compilation_Finished_Hook,
         Wrapper (Compilation_Finished_Cb'Access),
         Name => "codefix.compilation_finished");

      Codefix_Module_ID.Codefix_Class := New_Class (Kernel, "Codefix");
      Codefix_Module_ID.Codefix_Error_Class := New_Class
        (Kernel, "CodefixError");

      Register_Command
        (Kernel, "parse",
         Minimum_Args  => 2,
         Maximum_Args  => Parse_Cmd_Parameters'Length,
         Class         => Codefix_Module_ID.Codefix_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => Codefix_Cmd_Parameters'Length,
         Maximum_Args => Codefix_Cmd_Parameters'Length,
         Class        => Codefix_Module_ID.Codefix_Class,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => Error_Cmd_Parameters'Length - 1,
         Maximum_Args => Error_Cmd_Parameters'Length,
         Class        => Codefix_Module_ID.Codefix_Error_Class,
         Handler      => Error_Command_Handler'Access);
      Register_Command
        (Kernel, "errors",
         Class        => Codefix_Module_ID.Codefix_Class,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "sessions",
         Class         => Codefix_Module_ID.Codefix_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "possible_fixes",
         Class        => Codefix_Module_ID.Codefix_Error_Class,
         Handler      => Error_Command_Handler'Access);
      Register_Command
        (Kernel, "fix",
         Minimum_Args => Fix_Cmd_Parameters'Length - 1,
         Maximum_Args => Fix_Cmd_Parameters'Length,
         Class        => Codefix_Module_ID.Codefix_Error_Class,
         Handler      => Error_Command_Handler'Access);
      Register_Command
        (Kernel, "message",
         Class        => Codefix_Module_ID.Codefix_Error_Class,
         Handler      => Error_Command_Handler'Access);
      Register_Command
        (Kernel, "location",
         Class        => Codefix_Module_ID.Codefix_Error_Class,
         Handler      => Error_Command_Handler'Access);

      Register_Preferences (Kernel);

      Add_Parser (new Agregate_Misspelling);
      Add_Parser (new Double_Misspelling);
      Add_Parser (new Light_Misspelling);
      Add_Parser (new Goto_Misspelling);
      Add_Parser (new Library_Misspelling);
      Add_Parser (new Sth_Should_Be_Sth);
      Add_Parser (new Should_Be_Semicolon);
      Add_Parser (new And_Meant);
      Add_Parser (new Or_Meant);
      Add_Parser (new Bad_End_Block);
      Add_Parser (new Unqualified_Expression);
      Add_Parser (new Goes_Before);
      Add_Parser (new Sth_Expected_3);
      Add_Parser (new Sth_Expected_2);
      Add_Parser (new Sth_Expected);
      Add_Parser (new Missing_Kw);
      Add_Parser (new Missing_Sep);
      Add_Parser (new Missing_All);
      Add_Parser (new Statement_Missing);
      Add_Parser (new Space_Missing);
      Add_Parser (new Two_Spaces_Missing);
      Add_Parser (new Name_Missing);
      Add_Parser (new Double_Keyword);
      Add_Parser (new Extra_Paren);
      Add_Parser (new Redundant_Keyword);
      Add_Parser (new Unexpected_Sep);
      Add_Parser (new Unexpected_Word);
      Add_Parser (new Kw_Not_Allowed);
      Add_Parser (new Sep_Not_Allowed);
      Add_Parser (new Already_Use_Visible);
      Add_Parser (new Should_Be_In);
      Add_Parser (new Bad_Column);
      Add_Parser (new Main_With_Missing);
      Add_Parser (new Bad_Casing_Standard);
      Add_Parser (new Bad_Casing_Declared);
      Add_Parser (new Bad_Casing_Keyword);
      Add_Parser (new Object_Not_Referenced);
      Add_Parser (new Pkg_Not_Referenced);
      Add_Parser (new Never_Read);
      Add_Parser (new Never_Assigned);
      Add_Parser (new Pragma_Missplaced);
      Add_Parser (new Constant_Expected);
      Add_Parser (new Possible_Interpretation);
      Add_Parser (new Hidden_Declaration);
      Add_Parser (new Redundant_Conversion);
      Add_Parser (new Missplaced_With);
      Add_Parser (new Not_Fully_Conformant);
      Add_Parser (new Generic_Use_Unallowed);
      Add_Parser (new Non_Visible_Declaration);
      Initialize_Parsers;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Register_Module;

   ---------------------------
   -- Error_Command_Handler --
   ---------------------------

   procedure Error_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Instance : Class_Instance;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Error_Cmd_Parameters);
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Error_Class);

         declare
            Codefix : constant Class_Instance :=
              Nth_Arg (Data, 2, Codefix_Module_ID.Codefix_Class);
            Message : constant String  := Nth_Arg (Data, 4, "");
            Session : constant Codefix_Session := Get_Data (Codefix);
            Location : constant File_Location_Info := Get_Data (Data, 3);
            File    : constant Class_Instance := Get_File (Location);
            Error   : constant Error_Id := Search_Error
              (Session.Corrector.all,
               Get_File (Get_Data (File)),
               Get_Line (Location),
               Get_Column (Location), Message);
         begin
            if Error = Null_Error_Id then
               Set_Error_Msg (Data, -"No fixable error at that location");
            else
               Set_Data (Instance, Codefix_Error_Data'(Error, Session));
            end if;
         end;

      elsif Command = "possible_fixes" then
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Error_Class);

         declare
            Error : constant Codefix_Error_Data := Get_Data (Instance);
            Solution_Node : Command_List.List_Node :=
              First (Get_Solutions (Error.Error));
         begin
            Set_Return_Value_As_List (Data);

            while Solution_Node /= Command_List.Null_Node loop
               Set_Return_Value
                 (Data, Get_Caption (Command_List.Data (Solution_Node)));
               Solution_Node := Next (Solution_Node);
            end loop;
         end;

      elsif Command = "fix" then
         Name_Parameters (Data, Fix_Cmd_Parameters);
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Error_Class);

         declare
            Error : constant Codefix_Error_Data := Get_Data (Instance);
            Choice : Integer := Nth_Arg (Data, 2, 0);
            Solution_Node : Command_List.List_Node :=
              First (Get_Solutions (Error.Error));
         begin
            while Choice /= 0
              and then Solution_Node /= Command_List.Null_Node
            loop
               Solution_Node := Next (Solution_Node);
               Choice := Choice - 1;
            end loop;

            if Solution_Node /= Command_List.Null_Node then
               On_Fix (Get_Kernel (Data), Error.Session,
                       Error.Error, Command_List.Data (Solution_Node));
            end if;
         end;

      elsif Command = "message" then
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Error_Class);
         declare
            Error : constant Codefix_Error_Data := Get_Data (Instance);
            Msg   : constant Error_Message := Get_Error_Message (Error.Error);
         begin
            Set_Return_Value (Data, Get_Message (Msg));
         end;

      elsif Command = "location" then
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Error_Class);
         declare
            Error : constant Codefix_Error_Data := Get_Data (Instance);
            Msg   : constant Error_Message := Get_Error_Message (Error.Error);
         begin
            Set_Return_Value
              (Data,
               Create_File_Location
                 (Get_Script (Data),
                  File   => Create_File (Get_Script (Data), Get_File (Msg)),
                  Line   => Get_Line (Msg),
                  Column => Get_Column (Msg)));
         end;

      end if;
   end Error_Command_Handler;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Instance : Class_Instance;
      Session  : Codefix_Session;
   begin
      if Command = "parse" then
         Name_Parameters (Data, Parse_Cmd_Parameters);
         Activate_Codefix
           (Get_Kernel (Data),
            Output               => Nth_Arg (Data, 2),
            Category             => Nth_Arg (Data, 1),
            File_Location_Regexp => Nth_Arg (Data, 3, ""),
            File_Index           => Nth_Arg (Data, 4, -1),
            Line_Index           => Nth_Arg (Data, 5, -1),
            Col_Index            => Nth_Arg (Data, 6, -1),
            Msg_Index            => Nth_Arg (Data, 7, -1),
            Style_Index          => Nth_Arg (Data, 8, -1),
            Warning_Index        => Nth_Arg (Data, 9, -1));

      elsif Command = Constructor_Method then
         Name_Parameters (Data, Codefix_Cmd_Parameters);
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Class);
         Session := Get_Session_By_Name (Nth_Arg (Data, 2));

         if Session = null then
            Set_Error_Msg (Data, -"No codefix session for that category");
         else
            Set_Data (Instance, Session);
         end if;

      elsif Command = "sessions" then
         Set_Return_Value_As_List (Data);

         if Codefix_Module_ID.Sessions /= null then
            for S in Codefix_Module_ID.Sessions'Range loop
               Set_Return_Value
                 (Data, Codefix_Module_ID.Sessions (S).Category.all);
            end loop;
         end if;

      elsif Command = "errors" then
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Class);
         Session := Get_Data (Instance);

         declare
            Error : Error_Id := Get_First_Error (Session.Corrector.all);
            Err : Class_Instance;
         begin
            Set_Return_Value_As_List (Data);
            while Error /= Null_Error_Id loop
               Err := New_Instance
                 (Get_Script (Data), Codefix_Module_ID.Codefix_Error_Class);
               Set_Data (Err, Codefix_Error_Data'(Error, Session));
               Set_Return_Value (Data, Err);
               Error := Next (Error);
            end loop;
         end;

      end if;
   end Default_Command_Handler;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Session  : Codefix_Session)
   is
   begin
      if not Is_Subclass
        (Get_Script (Instance),
         Instance,
         Codefix_Module_ID.Codefix_Class)
      then
         raise Invalid_Data;
      end if;

      Set_Data (Instance,
                Class => Codefix_Module_ID.Codefix_Class,
                Value => Session.all'Address);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Class_Instance_Record'Class) return Codefix_Session
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Codefix_Session);
      pragma Warnings (On);

   begin
      if not Is_Subclass
        (Get_Script (Instance),
         Instance,
         Codefix_Module_ID.Codefix_Class)
      then
         raise Invalid_Data;
      end if;

      return Convert
        (Get_Data (Instance, Codefix_Module_ID.Codefix_Class));
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Error    : Codefix_Error_Data)
   is
      Err : Codefix_Error_Data_Access;
   begin
      if not Is_Subclass
        (Get_Script (Instance),
         Instance,
         Codefix_Module_ID.Codefix_Error_Class)
      then
         raise Invalid_Data;
      end if;

      Err := new Codefix_Error_Data'(Error);
      Set_Data
        (Instance,
         Class      => Codefix_Module_ID.Codefix_Error_Class,
         Value      => Err.all'Address,
         On_Destroy => On_Destroy_Error'Access);
   end Set_Data;

   ----------------------
   -- On_Destroy_Error --
   ----------------------

   procedure On_Destroy_Error (Value : System.Address) is
      Err : Codefix_Error_Data_Access := Convert (Value);
   begin
      Unchecked_Free (Err);
   end On_Destroy_Error;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Class_Instance_Record'Class)
      return Codefix_Error_Data is
   begin
      if not Is_Subclass
        (Get_Script (Instance),
         Instance,
         Codefix_Module_ID.Codefix_Error_Class)
      then
         raise Invalid_Data;
      end if;

      return Convert
        (Get_Data (Instance, Codefix_Module_ID.Codefix_Error_Class)).all;
   end Get_Data;

   ------------------------
   -- New_Text_Interface --
   ------------------------

   function New_Text_Interface (This : GPS_Navigator) return Ptr_Text is
      pragma Unreferenced (This);
   begin
      return new Console_Interface;
   end New_Text_Interface;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class) is
   begin
      Set_Kernel (Console_Interface (File), Get_Kernel (This));
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (This : out Codefix_Menu_Item; Label : String := "") is
   begin
      This := new Codefix_Menu_Item_Record;
      Codefix_Module.Initialize (This, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Menu_Item : access Codefix_Menu_Item_Record'Class;
      Label     : String) is
   begin
      Gtk.Menu_Item.Initialize (Menu_Item, Label);
   end Initialize;

   --------------------
   -- Create_Submenu --
   --------------------

   procedure Create_Submenu
     (Kernel       : access Kernel_Handle_Record'Class;
      Menu         : access Gtk.Menu.Gtk_Menu_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id)
   is
      Solution_Node : Command_List.List_Node;
      Mitem         : Codefix_Menu_Item;
   begin
      Solution_Node := First (Get_Solutions (Error));

      while Solution_Node /= Command_List.Null_Node loop
         Gtk_New (Mitem, Get_Caption (Data (Solution_Node)));

         --  ??? Where is this freed
         Mitem.Fix_Command  := new Text_Command'Class'(Data (Solution_Node));
         Mitem.Error        := Error;
         Mitem.Kernel       := Kernel_Handle (Kernel);
         Mitem.Session      := Codefix_Session (Session);
         Widget_Callback.Connect (Mitem, "activate", On_Fix'Access);
         Append (Menu, Mitem);

         Solution_Node := Next (Solution_Node);
      end loop;
   end Create_Submenu;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Codefix_Module_ID_Record) is
   begin
      if Id.Sessions /= null then
         for S in Id.Sessions'Range loop
            Destroy (Id.Sessions (S));
         end loop;
         Unchecked_Free (Id.Sessions);
      end if;

      Free_Parsers;
      Destroy (Module_ID_Record (Id));
   end Destroy;

   --------------------------
   -- Execute_Corrupted_Cb --
   --------------------------

   procedure Execute_Corrupted_Cb
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Error_Message : String) is
   begin
      Trace (Me, "Fix of current error is no longer pertinent");
      Trace (Me, "Exception got: " & Error_Message);
      Insert
        (Kernel, -"Fix of current error is no longer relevant");
   end Execute_Corrupted_Cb;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Remove_Policy := Glib.Properties.Creation.Param_Spec_Enum
        (Codefix_Remove_Policy_Properties.Gnew_Enum
           (Name  => "Remove-Policy-When-Fixing",
            Nick  => -"Remove policy when fixing code",
            Blurb => -("Prefered way to fix code when part have to be " &
              "removed."),
            Default => Always_Remove));
      Register_Property
        (Kernel, Param_Spec (Remove_Policy), -"General");
   end Register_Preferences;

   --------------------------
   -- Policy_To_Operations --
   --------------------------

   function Policy_To_Operations
     (Policy : Codefix_Remove_Policy) return Useless_Entity_Operations is
   begin
      case Policy is
         when Always_Remove =>
            return Remove_Entity;
         when Always_Comment =>
            return Comment_Entity;
         when Propose_Both_Choices =>
            return Comment_Entity or Remove_Entity;
      end case;
   end Policy_To_Operations;

end Codefix_Module;
