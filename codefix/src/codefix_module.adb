-----------------------------------------------------------------------
--                               G P S                               --
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

--  This package defines the module for code fixing.

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;            use GNAT.OS_Lib;

with Gtk.Menu;               use Gtk.Menu;
with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.MDI;             use Gtkada.MDI;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gdk.Pixbuf;             use Gdk.Pixbuf;

with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;
with Glide_Kernel;           use Glide_Kernel;
with Glide_Kernel.Modules;   use Glide_Kernel.Modules;
with Glide_Kernel.Console;   use Glide_Kernel.Console;
with Glide_Kernel.Scripts;   use Glide_Kernel.Scripts;
with Glide_Intl;             use Glide_Intl;

with Traces;                 use Traces;
with Basic_Types;            use Basic_Types;

with Codefix;                use Codefix;
with Codefix.Graphics;       use Codefix.Graphics;
with Codefix.GPS_Io;         use Codefix.GPS_Io;
with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Errors_Parser;  use Codefix.Errors_Parser;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;
with VFS;                    use VFS;

with Commands.Codefix;       use Commands.Codefix;

package body Codefix_Module is

   Codefix_Answer_Xpm : aliased Pixmap_Array;
   pragma Import (C, Codefix_Answer_Xpm, "codefix_answer_xpm");

   Codefix_Ambiguous_Xpm : aliased Pixmap_Array;
   pragma Import (C, Codefix_Ambiguous_Xpm, "codefix_ambiguous_xpm");

   Me : constant Debug_Handle := Create ("Codefix_Module");

   Output_Cst : aliased String := "output";
   Category_Cst : aliased String := "category";
   Parse_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Output_Cst'Access, 2 => Category_Cst'Access);

   type Codefix_Sessions_Array is array (Natural range <>) of Codefix_Session;
   type Codefix_Sessions is access Codefix_Sessions_Array;

   type Codefix_Module_ID_Record is new Glide_Kernel.Module_ID_Record with
      record
         Sessions : Codefix_Sessions;
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
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Error_Message : String);
   --  Handles error messages when an error can no longer be corrected.

   procedure On_Fix (Widget : access Gtk_Widget_Record'Class);
   --  Fixes the error that is proposed on a Menu_Item of Codefix.

   procedure Codefix_Handler
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Creates and shows the Codefix window.

   procedure Codefix_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Check is the current location is a fixable error, and propose the fix
   --  if possible.

   procedure Compilation_Finished_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
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
     (Kernel   : Kernel_Handle;
      Output   : String;
      Category : String);
   --  Activate postfix for a given compilation output

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for this module

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
     (Text : GPS_Navigator; File_Name : Virtual_File) return Virtual_File is
   begin
      return Other_File_Name (Get_Kernel (Text), File_Name);
   end Get_Body_Or_Spec;

   ------------
   -- On_Fix --
   ------------

   procedure On_Fix (Widget : access Gtk_Widget_Record'Class) is
      Mitem : constant Codefix_Menu_Item := Codefix_Menu_Item (Widget);
      Error : constant Error_Message := Get_Error_Message (Mitem.Error);
   begin
      Validate_And_Commit
        (Mitem.Session.Corrector.all,
         Mitem.Session.Current_Text.all,
         Mitem.Error,
         Mitem.Fix_Command.all);

      Remove_Location_Action
        (Kernel        => Mitem.Kernel,
         Identifier    => Location_Button_Name,
         Category      => Mitem.Session.Category.all,
         File          => Get_File (Error),
         Line          => Get_Line (Error),
         Column        => Get_Column (Error),
         Message       => Cut_Message (Get_Message (Error)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Fix;

   ----------------------
   -- Activate_Codefix --
   ----------------------

   procedure Activate_Codefix
     (Kernel   : Kernel_Handle;
      Output   : String;
      Category : String)
   is
      Current_Error : Error_Id;
      Errors_Found : Ptr_Errors_Interface   := new Compilation_Output;
      Session      : Codefix_Session;
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

      Set_Kernel   (Session.Current_Text.all, Kernel);
      Set_Error_Cb (Session.Corrector.all, Execute_Corrupted_Cb'Access);
      Set_Last_Output
        (Compilation_Output (Errors_Found.all), Kernel, Output);
      Analyze
        (Session.Corrector.all, Session.Current_Text.all,
         Errors_Found.all, null);

      --  Update the location window to show which errors can be fixed

      Current_Error := Get_First_Error (Session.Corrector.all);
      while Current_Error /= Null_Error_Id loop
         Create_Pixmap_And_Category (Kernel, Session, Current_Error);
         Current_Error := Next (Current_Error);
      end loop;

      Free (Errors_Found);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Activate_Codefix;

   -----------------------------
   -- Compilation_Finished_Cb --
   -----------------------------

   procedure Compilation_Finished_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Args);
      Compilation_Category : constant String := -"Builder results";
      --  ??? This is a duplicate from commands-builder.ads

   begin
      Activate_Codefix
        (Kernel, Execute_GPS_Shell_Command (Kernel, "get_build_output"),
         Compilation_Category);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Compilation_Finished_Cb;

   ---------------------
   -- Codefix_Handler --
   ---------------------

   procedure Codefix_Handler
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Graphic_Codefix : Graphic_Codefix_Access;
      Child           : MDI_Child;
      Session         : Codefix_Session;
   begin
      if Codefix_Module_ID.Sessions = null then
         return;
      end if;

      --  ??? Should select session interactively

      Session := Codefix_Module_ID.Sessions
        (Codefix_Module_ID.Sessions'First);

      Update_All (Session.Current_Text.all);

      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Graphic_Codefix_Record'Tag);
      if Child = null then
         Gtk_New
           (Graphic_Codefix,
            Kernel,
            Session,
            Remove_Pixmap'Access,
            Create_Pixmap_And_Category'Access);
         Child := Put (Kernel, Graphic_Codefix, Module => Codefix_Module_ID);
         Set_Title (Child, -"Code fixing", -"Codefix");
      else
         Graphic_Codefix := Graphic_Codefix_Access (Get_Widget (Child));
         Load_Next_Error (Graphic_Codefix, True);
         Raise_Child (Child);
      end if;

      Set_Focus_Child (Child);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Codefix_Handler;

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
      Menu_Item     : Gtk_Menu_Item;
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

         --  Get the session given the name of the category

         for S in Codefix_Module_ID.Sessions'Range loop
            if Codefix_Module_ID.Sessions (S).Category.all =
              Category_Information (Location)
            then
               Session := Codefix_Module_ID.Sessions (S);
               exit;
            end if;
         end loop;

         if Session = null then
            return;
         end if;

         if not Has_Message_Information (Location)
           or else not Has_File_Information (Location)
         then
            return;
         end if;

         Gtk_New (Menu_Item, -"Code fixing");

         Error := Search_Error
           (Session.Corrector.all,
            File    => File_Information (Location),
            Line    => Line_Information (Location),
            Column  => Column_Information (Location),
            Message => Message_Information (Location));

         if Error /= Null_Error_Id and then not Is_Fixed (Error) then
            Set_Submenu
              (Menu_Item,
               Create_Submenu (Get_Kernel (Context), Session, Error));
            Append (Menu, Menu_Item);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Codefix_Contextual_Menu;

   -------------------
   -- Remove_Pixmap --
   -------------------

   procedure Remove_Pixmap
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
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
         Message    => Cut_Message (Get_Message (Err)));
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
         Message       => Cut_Message (Get_Message (Err)),
         Action        => New_Action);
   end Create_Pixmap_And_Category;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Codefix_Module_ID := new Codefix_Module_ID_Record;
      Register_Module
        (Module                  => Module_ID (Codefix_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Codefix_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Codefix_Contextual_Menu'Access);

--      --  ??? Disabled for now, as the UI is not quite ready yet.

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => "/" & (-"Tools"),
         Text        => -"_Code Fixing",
         Callback    => Codefix_Handler'Access,
         Sensitive   => True);

      Kernel_Callback.Connect
        (Kernel,
         Compilation_Finished_Signal,
         Compilation_Finished_Cb'Access,
         Kernel_Handle (Kernel));

      Register_Command
        (Kernel,
         Command      => "codefix_parse",
         Params       => Parameter_Names_To_Usage (Parse_Cmd_Parameters),
         Description  =>
           -("Parse the output of a tool, and suggests auto-fix possibilities"
             & " whenever possible. This adds small icons in the location"
             & " window, so that the user can click on it to fix compilation"
             & " errors. You should call locations_parse with the same output"
             & " prior to calling this command."),
         Minimum_Args => Parse_Cmd_Parameters'Length,
         Maximum_Args => Parse_Cmd_Parameters'Length,
         Handler      => Default_Command_Handler'Access);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Register_Module;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "codefix_parse" then
         Name_Parameters (Data, Parse_Cmd_Parameters);
         Activate_Codefix (Get_Kernel (Data), Nth_Arg (Data, 1),
                           Category => Nth_Arg (Data, 2));
      end if;
   end Default_Command_Handler;

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

   function Create_Submenu
     (Kernel       : access Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id) return Gtk_Menu
   is
      Menu          : Gtk_Menu;
      Solution_Node : Command_List.List_Node;
      Mitem         : Codefix_Menu_Item;
   begin
      Gtk_New (Menu);
      Solution_Node := First (Get_Solutions (Error));

      while Solution_Node /= Command_List.Null_Node loop
         Gtk_New (Mitem, Get_Caption (Data (Solution_Node)));

         --  ??? Where is this freed
         Mitem.Fix_Command  := new Text_Command'Class'(Data (Solution_Node));
         Mitem.Error        := Error;
         Mitem.Kernel       := Kernel_Handle (Kernel);
         Mitem.Session      := Codefix_Session (Session);
         Widget_Callback.Connect
           (Mitem, "activate", Widget_Callback.To_Marshaller (On_Fix'Access));
         Append (Menu, Mitem);

         Solution_Node := Next (Solution_Node);
      end loop;

      return Menu;
   end Create_Submenu;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Codefix_Module_ID_Record) is
   begin
      for S in Id.Sessions'Range loop
         Destroy (Id.Sessions (S));
      end loop;
      Unchecked_Free (Id.Sessions);

      Free_Parsers;
      Destroy (Module_ID_Record (Id));
   end Destroy;

   --------------------------
   -- Execute_Corrupted_Cb --
   --------------------------

   procedure Execute_Corrupted_Cb
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Error_Message : String) is
   begin
      Trace (Me, "Fix of current error is no longer pertinent");
      Trace (Me, "Exception got: " & Error_Message);
      Insert
        (Kernel, -"Fix of current error is no longer relevant");
   end Execute_Corrupted_Cb;

end Codefix_Module;
