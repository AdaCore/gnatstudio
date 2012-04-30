------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

--  This package defines the module for code fixing.

with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;               use GNAT.Regpat;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;

with Glib.Object;               use Glib.Object;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;

with String_Utils;              use String_Utils;
with Basic_Types;               use Basic_Types;
with UTF8_Utils;                use UTF8_Utils;
with Codefix.Errors_Parser;     use Codefix.Errors_Parser;
with Codefix.Error_Lists;       use Codefix.Error_Lists;
with Codefix.GPS_Io;            use Codefix.GPS_Io;
with Codefix.Text_Manager;      use Codefix.Text_Manager;
with Codefix.GNAT_Parser;
with Codefix.SPARK_Parser;
with Commands.Codefix;          use Commands.Codefix;
with Commands;                  use Commands;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Legacy;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Traces;                    use Traces;
with GNATCOLL.Arg_Lists;    use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Glib;                      use Glib;
with GPS.Editors;               use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body Codefix_Module is

   Me          : constant Debug_Handle := Create ("Codefix_Module");

   Codefix_Class_Name       : constant String := "Codefix";
   Codefix_Error_Class_Name : constant String := "CodefixError";

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
      Sessions            : Codefix_Sessions;
      Codefix_Class       : Class_Type;
      Codefix_Error_Class : Class_Type;
      Codefix_Processor   : Fix_Processor;
   end record;
   type Codefix_Module_ID_Access is access all Codefix_Module_ID_Record'Class;

   Codefix_Module_ID   : Codefix_Module_ID_Access;
   Codefix_Module_Name : constant String := "Code_Fixing";

   overriding procedure Destroy (Id : in out Codefix_Module_ID_Record);

   type Codefix_Properties is new Instance_Property_Record with record
      Session : Codefix_Session;
   end record;
   type Codefix_Properties_Access is access all Codefix_Properties'Class;

   type Fix_Mode_Type is (Specific, Simple, Style_And_Warnings, Similar);

   type Codefix_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Kernel          : Kernel_Handle;
      Session         : Codefix_Session;
      Fix_Mode        : Fix_Mode_Type;
      Fix_Command     : Ptr_Command;
      Error           : Error_Id;
      Matching_Parser : Error_Parser_Access;
      Solution_Index  : Positive;
      Total_Solutions : Positive;
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

   type GPS_Execute_Corrupted_Record is new Execute_Corrupted_Record
   with record
      Kernel : Kernel_Handle;
   end record;

   overriding procedure Panic
     (Corruption    : access GPS_Execute_Corrupted_Record;
      Error_Message : String);
   --  Handles error messages when an error can not be corrected.

   overriding procedure Obsolescent
     (Corruption    : access GPS_Execute_Corrupted_Record;
      Error_Message : String);
   --  Handles error messages when an error can no longer be corrected because
   --  of changes in the buffer.

   type Codefix_Contextual_Menu is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Codefix_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Adds multiple messages fixes menus

   procedure On_Fix
     (Kernel  : access Kernel_Handle_Record'Class;
      Session : Codefix_Session;
      Error   : Error_Id;
      Command : Text_Command'Class);
   procedure On_Fix (Widget : access Gtk_Widget_Record'Class);
   --  Fixes the error that is proposed on a Menu_Item of Codefix.

   procedure Compilation_Finished_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Initializes the fix list of Codefix.

   type GPS_Navigator is new Text_Navigator_Abstr with record
      Kernel : Kernel_Handle;
   end record;

   overriding function New_Text_Interface
     (This : GPS_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   overriding procedure Initialize
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

   procedure Set_Data (Instance : Class_Instance; Session  : Codefix_Session);
   function Get_Data (Instance : Class_Instance) return Codefix_Session;
   --  Set or retrieve the session from an instance of Codefix

   type Codefix_Error_Data is record
      Error   : Error_Id;
      Session : Codefix_Session;
   end record;

   procedure Set_Data (Instance : Class_Instance; Error : Codefix_Error_Data);
   function Get_Data (Instance : Class_Instance) return Codefix_Error_Data;
   --  Set or retrieve the error from an instance of CodefixError

   type Codefix_Error_Property is new Instance_Property_Record with record
      Error : Codefix_Error_Data;
   end record;
   type Codefix_Error_Property_Access
     is access all Codefix_Error_Property'Class;

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
      if Command.Is_Writable then
         Validate_And_Commit
           (Session.Corrector.all, Session.Current_Text.all, Error, Command);

         GPS.Kernel.Messages.Legacy.Set_Action_Item
           (Kernel,
            Session.Category.all,
            Get_File (Err),
            Get_Line (Err),
            Natural (Get_Column (Err)),
            Get_Message (Err),
            null);
      else
         GPS.Kernel.Console.Insert
           (Kernel,
            -"cannot fix readonly file",
            Mode => GPS.Kernel.Console.Error);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Fix;

   procedure On_Fix (Widget : access Gtk_Widget_Record'Class) is
      Mitem : constant Codefix_Menu_Item := Codefix_Menu_Item (Widget);
   begin
      case Mitem.Fix_Mode is
         when Specific =>
            On_Fix
              (Mitem.Kernel,
               Mitem.Session,
               Mitem.Error,
               Mitem.Fix_Command.all);

         when others =>
            declare
               Error                 : Error_Id :=
                 Get_First_Error (Mitem.Session.Corrector.all);
               Solution_Node         : Solution_List_Iterator;
               Solution_Index        : Positive;
               Command               : Ptr_Command;
               Unique_Simple_Command : Ptr_Command;
            begin
               --  Loop over all the errors stored in the session and fix
               --  all the ones with one simple fix and matching the fix mode.

               while Error /= Null_Error_Id loop
                  if not Is_Fixed (Error) then
                     Unique_Simple_Command := null;
                     Solution_Index := 1;

                     Solution_Node := First (Get_Solutions (Error));

                     while not At_End (Solution_Node) loop
                        Command := Get_Command (Solution_Node);

                        if Command.Complexity = Simple then
                           if Mitem.Fix_Mode = Simple
                             or else
                               (Mitem.Fix_Mode = Style_And_Warnings
                                and then Is_Style_Or_Warning
                                  (Get_Error_Message (Error)))
                             or else
                               (Mitem.Fix_Mode = Similar
                                and then Command.Get_Parser.all'Tag
                                  = Mitem.Matching_Parser.all'Tag
                                and then Solution_Index
                                  = Mitem.Solution_Index
                                and then Length (Get_Solutions (Error))
                                  = Mitem.Total_Solutions)
                           then
                              if Unique_Simple_Command = null then
                                 Unique_Simple_Command := Command;
                              else
                                 Unique_Simple_Command := null;
                                 exit;
                              end if;
                           end if;

                           Solution_Index := Solution_Index + 1;
                        end if;

                        Solution_Node := Next (Solution_Node);
                     end loop;

                     if Unique_Simple_Command /= null then
                        --  If this is an error set up to be fixed, fix it

                        On_Fix
                          (Mitem.Kernel,
                           Mitem.Session,
                           Error,
                           Unique_Simple_Command.all);
                     end if;
                  end if;

                  Error := Next (Error);
               end loop;
            end;
      end case;

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
      Errors_Found : Error_Message_List;
      Session      : Codefix_Session;
      Fi, Li, Ci, Mi, Si, Wi : Integer;

      Command : Command_Access;
      Options : Fix_Options;
   begin
      Initialize (Errors_Found);

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
      GPS_Navigator (Session.Current_Text.all).Kernel := Kernel;

      Session.Timestamp := Session.Timestamp + 1;

      if File_Index = -1 then
         Fi := File_Pattern_Index.Get_Pref;
      else
         Fi := File_Index;
      end if;

      if Line_Index = -1 then
         Li := Line_Pattern_Index.Get_Pref;
      else
         Li := Line_Index;
      end if;

      if Col_Index = -1 then
         Ci := Column_Pattern_Index.Get_Pref;
      else
         Ci := Col_Index;
      end if;

      if Msg_Index = -1 then
         Mi := Message_Pattern_Index.Get_Pref;
      else
         Mi := Msg_Index;
      end if;

      if Style_Index = -1 then
         Si := Style_Pattern_Index.Get_Pref;
      else
         Si := Style_Index;
      end if;

      if Warning_Index = -1 then
         Wi := Warning_Pattern_Index.Get_Pref;
      else
         Wi := Warning_Index;
      end if;

      if File_Location_Regexp = "" then
         Set_Regexp
           (Errors_Found,
            File_Location_Regexp => Compile (File_Pattern.Get_Pref),
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

      Set_Registry (Session.Current_Text.all, Get_Registry (Kernel));
      Set_Construct_Database
        (Session.Current_Text.all, Get_Construct_Database (Kernel));
      Set_Error_Cb
        (Session.Corrector.all,
         new GPS_Execute_Corrupted_Record'
           (Execute_Corrupted_Record with Kernel));
      Set_Context (Session.Current_Text.all, Refactoring_Context (Kernel));
      Add_Errors_From (Errors_Found, Get_Registry (Kernel), Output);

      Options.Remove_Policy := Policy_To_Operations
        (Remove_Policy.Get_Pref);

      Analyze
        (Session.Corrector.all, Codefix_Module_ID.Codefix_Processor,
         Session.Current_Text.all, Errors_Found, Options, null);

      --  Update the location window to show which errors can be fixed

      Command := new Codefix_Add_Command;
      Codefix_Add_Command (Command.all).Session_Timestamp := Session.Timestamp;
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
      when E : others => Trace (Exception_Handle, E);
   end Activate_Codefix;

   -----------------------------
   -- Compilation_Finished_Cb --
   -----------------------------

   procedure Compilation_Finished_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Compilation_Data : constant Compilation_Finished_Hooks_Args :=
        Compilation_Finished_Hooks_Args (Data.all);
      Cmd : Arg_List;

   begin
      Cmd := Create ("get_build_output");
      Append_Argument (Cmd, Compilation_Data.Target_Name, One_Arg);
      Append_Argument (Cmd, Compilation_Data.Shadow'Img, One_Arg);
      Append_Argument (Cmd, Compilation_Data.Background'Img, One_Arg);
      Append_Argument (Cmd, "True", One_Arg);  --  Get the output "as_string"

      if Compilation_Data.Background then
         Activate_Codefix
           (Kernel_Handle (Kernel),
            Execute_GPS_Shell_Command (Kernel, Cmd),
            Compilation_Data.Category);
      else
         Activate_Codefix
           (Kernel_Handle (Kernel),
            Execute_GPS_Shell_Command (Kernel, Cmd),
            -"Builder results");
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Compilation_Finished_Cb;

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
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Session : access Codefix_Session_Record;
      Error   : Error_Id)
   is
      Err : constant Error_Message := Get_Error_Message (Error);
   begin
      GPS.Kernel.Messages.Legacy.Set_Action_Item
        (Kernel,
         Session.Category.all,
         Get_File (Err),
         Get_Line (Err),
         Natural (Get_Column (Err)),
         Get_Message (Err),
         null);
   end Remove_Pixmap;

   --------------------------------
   -- Create_Pixmap_And_Category --
   --------------------------------

   procedure Create_Pixmap_And_Category
     (Kernel  : access Kernel_Handle_Record'Class;
      Session : access Codefix_Session_Record;
      Error   : Error_Id)
   is
      New_Action : Action_Item;
      Err        : constant Error_Message := Get_Error_Message (Error);
   begin
      New_Action := new Line_Information_Record;
      New_Action.Tooltip_Text := new String'
        (-"<b>Fix: </b>" & Get_Message (Err));

      if Get_Number_Of_Fixes (Error) = 1 then
         New_Action.Image := Render_Icon
           (Widget   => Gtk_Widget (Get_Main_Window (Kernel)),
            Stock_Id => "gps-wrench",
            Size     => Icon_Size_Menu);
      else
         New_Action.Image := Render_Icon
           (Widget   => Gtk_Widget (Get_Main_Window (Kernel)),
            Stock_Id => "gps-wrench-multi",
            Size     => Icon_Size_Menu);
      end if;

      New_Action.Associated_Command := new Codefix_Command;
      Codefix_Command (New_Action.Associated_Command.all).Session_Timestamp :=
        Session.Timestamp;
      Codefix_Command (New_Action.Associated_Command.all).Error   := Error;
      Codefix_Command (New_Action.Associated_Command.all).Session :=
        Codefix_Session (Session);
      Codefix_Command (New_Action.Associated_Command.all).Kernel :=
        Kernel_Handle (Kernel);

      GPS.Kernel.Messages.Legacy.Set_Action_Item
        (Kernel,
         Session.Category.all,
         Get_File (Err),
         Get_Line (Err),
         Natural (Get_Column (Err)),
         Get_Message (Err),
         New_Action);
   end Create_Pixmap_And_Category;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Codefix_Contextual_Menu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
      Session : Codefix_Session;
      Error   : Error_Id;
   begin
      if Has_Message_Information (Context) then
         if not Has_Category_Information (Context)
           or else Codefix_Module_ID.Sessions = null
         then
            return;
         end if;

         Session := Get_Session_By_Name (Category_Information (Context));

         if Session = null then
            return;
         end if;

         if not Has_Message_Information (Context)
           or else not Has_File_Information (Context)
         then
            return;
         end if;

         Error := Search_Error
           (Session.Corrector.all,
            File    => File_Information (Context),
            Line    => Contexts.Line_Information (Context),
            Column  => Column_Information (Context),
            Message => Remove_Markup (Message_Information (Context)));

         if Error /= Null_Error_Id and then not Is_Fixed (Error) then
            Create_Submenu (Get_Kernel (Context), Menu, Session, Error);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Append_To_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Codefix_Module_ID := new Codefix_Module_ID_Record;

      Register_Module
        (Module      => Module_ID (Codefix_Module_ID),
         Kernel      => Kernel,
         Module_Name => Codefix_Module_Name,
         Priority    => Default_Priority);

      Register_Contextual_Submenu
        (Kernel,
         Name    => "Auto Fix",
         Submenu => new Codefix_Contextual_Menu);

      Add_Hook
        (Kernel, Compilation_Finished_Hook,
         Wrapper (Compilation_Finished_Cb'Access),
         Name => "codefix.compilation_finished");

      Codefix_Module_ID.Codefix_Class :=
        New_Class (Kernel, Codefix_Class_Name);
      Codefix_Module_ID.Codefix_Error_Class := New_Class
        (Kernel, Codefix_Error_Class_Name);

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
        (Kernel, "error_at",
         Class         => Codefix_Module_ID.Codefix_Class,
         Handler       => Default_Command_Handler'Access,
         Minimum_Args  => 3,
         Maximum_Args  => 4);
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

      Codefix.GNAT_Parser.Register_Parsers
        (Codefix_Module_ID.Codefix_Processor);

      Codefix.SPARK_Parser.Register_Parsers
        (Codefix_Module_ID.Codefix_Processor);

      Initialize_Parsers (Codefix_Module_ID.Codefix_Processor);
   exception
      when E : others => Trace (Exception_Handle, E);
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
            Codefix  : constant Class_Instance :=
                         Nth_Arg (Data, 2, Codefix_Module_ID.Codefix_Class);
            Message  : constant String  := Nth_Arg (Data, 4, "");
            Session  : constant Codefix_Session := Get_Data (Codefix);
            Location : constant File_Location_Info := Get_Data (Data, 3);
            File     : constant Class_Instance := Get_File (Location);
            Error    : constant Error_Id := Search_Error
              (Session.Corrector.all,
               Get_Data (File),
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
            Error         : constant Codefix_Error_Data := Get_Data (Instance);
            Solution_Node : Solution_List_Iterator :=
                              First (Get_Solutions (Error.Error));
         begin
            Set_Return_Value_As_List (Data);

            while not At_End (Solution_Node) loop
               Set_Return_Value
                 (Data, Get_Caption (Get_Command (Solution_Node).all));
               Solution_Node := Next (Solution_Node);
            end loop;
         end;

      elsif Command = "fix" then
         Name_Parameters (Data, Fix_Cmd_Parameters);
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Error_Class);

         declare
            Error         : constant Codefix_Error_Data := Get_Data (Instance);
            Choice        : Integer := Nth_Arg (Data, 2, 0);
            Solution_Node : Solution_List_Iterator :=
                              First (Get_Solutions (Error.Error));
         begin
            while Choice /= 0
              and then not At_End (Solution_Node)
            loop
               Solution_Node := Next (Solution_Node);
               Choice := Choice - 1;
            end loop;

            if not At_End (Solution_Node) then
               On_Fix (Get_Kernel (Data), Error.Session,
                       Error.Error, Get_Command (Solution_Node).all);
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
            --  ??? Is the conversion to Visible_Column_Type correct ?
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
      Valid    : Boolean;
      S        : String_Access;
      Output   : Unchecked_String_Access;
      Len      : Natural;

   begin
      if Command = "parse" then
         Name_Parameters (Data, Parse_Cmd_Parameters);

         S := new String'(Nth_Arg (Data, 2));
         Unknown_To_UTF8 (S.all, Output, Len, Valid);

         if not Valid then
            Set_Error_Msg (Data, -"Could not convert input to UTF8");
         else
            if Output = null then
               Activate_Codefix
                 (Get_Kernel (Data),
                  Output               => S.all,
                  Category             => Nth_Arg (Data, 1),
                  File_Location_Regexp => Nth_Arg (Data, 3, ""),
                  File_Index           => Nth_Arg (Data, 4, -1),
                  Line_Index           => Nth_Arg (Data, 5, -1),
                  Col_Index            => Nth_Arg (Data, 6, -1),
                  Msg_Index            => Nth_Arg (Data, 7, -1),
                  Style_Index          => Nth_Arg (Data, 8, -1),
                  Warning_Index        => Nth_Arg (Data, 9, -1));
            else
               Activate_Codefix
                 (Get_Kernel (Data),
                  Output               => Output (1 .. Len),
                  Category             => Nth_Arg (Data, 1),
                  File_Location_Regexp => Nth_Arg (Data, 3, ""),
                  File_Index           => Nth_Arg (Data, 4, -1),
                  Line_Index           => Nth_Arg (Data, 5, -1),
                  Col_Index            => Nth_Arg (Data, 6, -1),
                  Msg_Index            => Nth_Arg (Data, 7, -1),
                  Style_Index          => Nth_Arg (Data, 8, -1),
                  Warning_Index        => Nth_Arg (Data, 9, -1));
               Free (Output);
            end if;
         end if;

         Free (S);

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
            Err   : Class_Instance;
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
      elsif Command = "error_at" then
         Instance := Nth_Arg (Data, 1, Codefix_Module_ID.Codefix_Class);
         Session := Get_Data (Instance);

         declare
            File    : constant Virtual_File :=
              Get_Data (Nth_Arg (Data, 2));
            Line    : constant Integer := Nth_Arg (Data, 3);
            Column  : constant Integer := Nth_Arg (Data, 4);
            Message : constant String := Nth_Arg (Data, 5, "");

            Error : constant Error_Id := Search_Error
              (This    => Session.Corrector.all,
               File    => File,
               Line    => Line,
               Column  => Visible_Column_Type (Column),
               Message => Message);

            Err : Class_Instance;
         begin
            if Error /= Null_Error_Id then
               Err := New_Instance
                 (Get_Script (Data), Codefix_Module_ID.Codefix_Error_Class);
               Set_Data (Err, Codefix_Error_Data'(Error, Session));
               Set_Return_Value (Data, Err);
            else
               Set_Return_Value (Data, No_Class_Instance);
            end if;
         end;

      end if;
   end Default_Command_Handler;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Session : Codefix_Session) is
   begin
      if not Is_Subclass (Instance, Codefix_Module_ID.Codefix_Class) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, Codefix_Class_Name,
         Codefix_Properties'(Session => Session));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : Class_Instance) return Codefix_Session is
   begin
      if not Is_Subclass (Instance, Codefix_Module_ID.Codefix_Class) then
         raise Invalid_Data;
      end if;

      return Codefix_Properties_Access
        (Instance_Property'(Get_Data (Instance, Codefix_Class_Name)))
        .Session;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Error : Codefix_Error_Data) is
   begin
      if not Is_Subclass (Instance, Codefix_Module_ID.Codefix_Error_Class) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance,
         Codefix_Error_Class_Name,
         Codefix_Error_Property'(Error => Error));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : Class_Instance) return Codefix_Error_Data is
   begin
      if not Is_Subclass (Instance, Codefix_Module_ID.Codefix_Error_Class) then
         raise Invalid_Data;
      end if;

      return Codefix_Error_Property_Access
        (Instance_Property'(Get_Data (Instance, Codefix_Error_Class_Name)))
        .Error;
   end Get_Data;

   ------------------------
   -- New_Text_Interface --
   ------------------------

   overriding function New_Text_Interface
     (This : GPS_Navigator) return Ptr_Text
   is
      pragma Unreferenced (This);
   begin
      return new Console_Interface;
   end New_Text_Interface;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class) is
   begin
      Initialize (Text_Navigator_Abstr (This), File);
      Set_Kernel (Console_Interface (File), This.Kernel);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (This : out Codefix_Menu_Item; Label : String := "") is
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
     (Kernel  : access Kernel_Handle_Record'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class;
      Session : access Codefix_Session_Record;
      Error   : Error_Id)
   is
      Fix_Command    : Ptr_Command;
      Mitem          : Codefix_Menu_Item;
      Sub_Menu       : Gtk.Menu.Gtk_Menu;
      Menu_Item      : Gtk.Menu_Item.Gtk_Menu_Item;
      Solutions      : constant Solution_List := Get_Solutions (Error);
      Solution_Node  : Solution_List_Iterator;
      Simple_Number  : Integer := 0;
   begin
      Solution_Node := First (Solutions);

      while not At_End (Solution_Node) loop
         Fix_Command := Get_Command (Solution_Node);

         if Fix_Command.Complexity = Simple then
            Simple_Number := Simple_Number + 1;

            Gtk.Menu.Gtk_New (Sub_Menu);
            Gtk.Menu_Item.Gtk_New
              (Menu_Item, Get_Caption (Get_Command (Solution_Node).all));

            Menu_Item.Set_Submenu (Sub_Menu);

            Gtk_New (Mitem, "Apply to this occurence");
            Mitem.Fix_Mode     := Specific;
            Mitem.Fix_Command  := Fix_Command;
            Mitem.Error        := Error;
            Mitem.Kernel       := Kernel_Handle (Kernel);
            Mitem.Session      := Codefix_Session (Session);
            Widget_Callback.Connect (Mitem, Signal_Activate, On_Fix'Access);
            Append (Sub_Menu, Mitem);

            Gtk_New (Mitem, "Apply to all similar errors");
            Mitem.Fix_Mode        := Similar;
            Mitem.Matching_Parser := Fix_Command.Get_Parser;
            Mitem.Solution_Index  := Simple_Number;
            Mitem.Total_Solutions := Length (Solutions);
            Mitem.Kernel          := Kernel_Handle (Kernel);
            Mitem.Session         := Codefix_Session (Session);
            Widget_Callback.Connect (Mitem, Signal_Activate, On_Fix'Access);
            Append (Sub_Menu, Mitem);

            Append (Menu, Menu_Item);
         else
            Gtk_New (Mitem, Get_Caption (Get_Command (Solution_Node).all));

            Mitem.Fix_Mode     := Specific;
            Mitem.Fix_Command  := Fix_Command;
            Mitem.Error        := Error;
            Mitem.Kernel       := Kernel_Handle (Kernel);
            Mitem.Session      := Codefix_Session (Session);
            Widget_Callback.Connect (Mitem, Signal_Activate, On_Fix'Access);
            Append (Menu, Mitem);
         end if;

         Solution_Node := Next (Solution_Node);
      end loop;

      if Simple_Number > 0 then
         if Is_Style_Or_Warning (Get_Error_Message (Error)) then
            Gtk_New (Mitem, "Fix all simple style errors and warnings");

            Mitem.Fix_Mode := Style_And_Warnings;
            Mitem.Kernel   := Kernel_Handle (Kernel);
            Mitem.Session  := Codefix_Session (Session);
            Widget_Callback.Connect (Mitem, Signal_Activate, On_Fix'Access);
            Append (Menu, Mitem);
         end if;

         Gtk_New (Mitem, "Fix all simple errors");

         Mitem.Fix_Mode := Simple;
         Mitem.Kernel   := Kernel_Handle (Kernel);
         Mitem.Session  := Codefix_Session (Session);
         Widget_Callback.Connect (Mitem, Signal_Activate, On_Fix'Access);
         Append (Menu, Mitem);
      end if;
   end Create_Submenu;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Codefix_Module_ID_Record) is
   begin
      if Id.Sessions /= null then
         for S in Id.Sessions'Range loop
            Destroy (Id.Sessions (S));
         end loop;
         Unchecked_Free (Id.Sessions);
      end if;

      Free (Codefix_Module_ID.Codefix_Processor);
      Destroy (Module_ID_Record (Id));
   end Destroy;

   --------------------------
   -- Execute_Corrupted_Cb --
   --------------------------

   overriding procedure Panic
     (Corruption    : access GPS_Execute_Corrupted_Record;
      Error_Message : String) is
   begin
      Trace (Me, "Fix of current error is no longer pertinent");
      Trace (Me, "Exception got: " & Error_Message);

      Insert
        (Corruption.Kernel,
         -"Fix of current error is no longer relevant");
   end Panic;

   overriding procedure Obsolescent
     (Corruption    : access GPS_Execute_Corrupted_Record;
      Error_Message : String)
   is
      pragma Unreferenced (Error_Message);
   begin
      Insert
        (Corruption.Kernel,
         -"Fix of current error is no longer relevant");
   end Obsolescent;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Remove_Policy := Codefix_Remove_Policy_Preferences.Create
        (Get_Preferences (Kernel),
         Name  => "Remove-Policy-When-Fixing",
         Label => -"Remove policy when fixing code",
         Doc   => -("Prefered way to fix code when part have to be " &
           "removed."),
         Page    => -"General",
         Default => Always_Remove);
   end Register_Preferences;

end Codefix_Module;
