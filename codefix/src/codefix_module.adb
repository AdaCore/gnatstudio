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
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Gtk.Menu;               use Gtk.Menu;
with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtkada.MDI;             use Gtkada.MDI;
with Gdk.Pixbuf;             use Gdk.Pixbuf;

with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;
with Glide_Kernel;           use Glide_Kernel;
with Glide_Kernel.Modules;   use Glide_Kernel.Modules;
with Glide_Kernel.Console;   use Glide_Kernel.Console;
with Glide_Kernel.Project;   use Glide_Kernel.Project;
with Glide_Intl;             use Glide_Intl;
with Projects.Registry;      use Projects, Projects.Registry;

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

with Commands.Codefix;       use Commands.Codefix;

package body Codefix_Module is

   Codefix_Answer_Xpm : aliased Pixmap_Array;
   pragma Import (C, Codefix_Answer_Xpm, "codefix_answer_xpm");

   Codefix_Ambiguous_Xpm : aliased Pixmap_Array;
   pragma Import (C, Codefix_Ambiguous_Xpm, "codefix_ambiguous_xpm");

   Me : constant Debug_Handle := Create ("Codefix_Module");

   procedure On_Fix
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
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

   type GPS_Navigator is new Text_Navigator_Abstr with record
      Kernel : Kernel_Handle;
   end record;

   function Get_Body_Or_Spec
     (Text : GPS_Navigator; File_Name : String) return String;
   --  See inherited documentation

   function New_Text_Interface (This : GPS_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class);
   --  Set the value of the Text_Interface's kernel

   ----------------------
   -- Get_Body_Or_Spec --
   ----------------------

   function Get_Body_Or_Spec
     (Text : GPS_Navigator; File_Name : String) return String
   is
      F       : constant String := Base_Name (File_Name);
      Project : constant Project_Type :=
        Get_Project_From_File (Get_Registry (Text.Kernel), F);
   begin
      return Get_Full_Path_From_File
        (Registry        => Get_Registry (Text.Kernel),
         Filename        => Other_File_Name (Project, F),
         Use_Source_Path => True,
         Use_Object_Path => False);
   end Get_Body_Or_Spec;

   ------------
   -- On_Fix --
   ------------

   procedure On_Fix
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Mitem : constant Codefix_Menu_Item := Codefix_Menu_Item (Widget);
      pragma Unreferenced (Context);
   begin
      Validate_And_Commit
        (Codefix_Module_ID.Corrector.all,
         Codefix_Module_ID.Current_Text.all,
         Mitem.Error,
         Mitem.Fix_Command.all);

      Remove_Location_Action
        (Kernel        => Codefix_Module_ID.Kernel,
         Identifier    => Location_Button_Name,
         Category      => Compilation_Category,
         File          => Get_Error_Message (Mitem.Error).File_Name.all,
         Line          => Get_Error_Message (Mitem.Error).Line,
         Column        => Get_Error_Message (Mitem.Error).Col,
         Message       =>
           Cut_Message (Get_Message (Get_Error_Message (Mitem.Error))));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Fix;

   -----------------------------
   -- Compilation_Finished_Cb --
   -----------------------------

   procedure Compilation_Finished_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Args);

      Current_Error : Error_Id;

   begin
      Free (Codefix_Module_ID.Errors_Found);
      Free (Codefix_Module_ID.Corrector);
      Free (Codefix_Module_ID.Current_Text);

      Codefix_Module_ID.Errors_Found := new Compilation_Output;
      Codefix_Module_ID.Corrector := new Correction_Manager;
      Codefix_Module_ID.Current_Text := new GPS_Navigator;

      GPS_Navigator (Codefix_Module_ID.Current_Text.all).Kernel := Kernel;
      Set_Error_Cb
        (Codefix_Module_ID.Corrector.all, Execute_Corrupted_Cb'Access);

      Get_Last_Output
        (Compilation_Output
           (Codefix_Module_ID.Errors_Found.all), Kernel);

      Analyze
        (Codefix_Module_ID.Corrector.all,
         Codefix_Module_ID.Current_Text.all,
         Codefix_Module_ID.Errors_Found.all,
         null);

      Current_Error := Get_First_Error (Codefix_Module_ID.Corrector.all);

      while Current_Error /= Null_Error_Id loop
         Create_Pixmap (Current_Error);
         Current_Error := Next (Current_Error);
      end loop;

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
   begin
      Update_All (Codefix_Module_ID.Current_Text.all);

      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Graphic_Codefix_Record'Tag);
      if Child = null then
         Gtk_New
           (Graphic_Codefix,
            Kernel,
            Codefix_Module_ID.Current_Text,
            Codefix_Module_ID.Corrector,
            Remove_Pixmap'Access,
            Create_Pixmap'Access);

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
      Error         : Error_Id;
      Error_Caption : GNAT.OS_Lib.String_Access;
      Menu_Item     : Gtk_Menu_Item;

   begin
      if Context.all in Message_Context'Class then
         Location := Message_Context_Access (Context);

         if not Has_Category_Information (Location)
           or else Category_Information (Location) /= Compilation_Category
         then
            return;
         end if;

         if Has_Message_Information (Location)
           and then Has_File_Information (Location)
         then
            Assign (Error_Caption,
                    File_Information (Location) &
                    ":" & Message_Information (Location));
         else
            return;
         end if;

         Gtk_New (Menu_Item, -"Code fixing");

         Error := Search_Error
           (Codefix_Module_ID.Corrector.all, Error_Caption.all);

         if Error /= Null_Error_Id and then not Is_Fixed (Error) then
            Set_Submenu
              (Menu_Item,
               Create_Submenu (Error));
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

   procedure Remove_Pixmap (Error : Error_Id) is
   begin
      Remove_Location_Action
        (Kernel        => Codefix_Module_ID.Kernel,
         Identifier    => Location_Button_Name,
         Category      => Compilation_Category,
         File          => Get_Error_Message (Error).File_Name.all,
         Line          => Get_Error_Message (Error).Line,
         Column        => Get_Error_Message (Error).Col,
         Message       =>
           Cut_Message (Get_Message (Get_Error_Message (Error))));
   end Remove_Pixmap;

   -------------------
   -- Create_Pixmap --
   -------------------

   procedure Create_Pixmap (Error : Error_Id) is
      New_Action    : Action_Item;
   begin
      New_Action := new Line_Information_Record;
      New_Action.Text := new String'(-"Fix error");

      if Get_Number_Of_Fixes (Error) = 1 then
         New_Action.Image := Gdk_New_From_Xpm_Data (Codefix_Answer_Xpm);
      else
         New_Action.Image := Gdk_New_From_Xpm_Data (Codefix_Ambiguous_Xpm);
      end if;

      New_Action.Associated_Command := new Codefix_Command;
      Codefix_Command (New_Action.Associated_Command.all).Error :=
        Error;
      Codefix_Command (New_Action.Associated_Command.all).Current_Text :=
        Codefix_Module_ID.Current_Text;
      Codefix_Command (New_Action.Associated_Command.all).Corrector :=
        Codefix_Module_ID.Corrector;
      Codefix_Command (New_Action.Associated_Command.all).Kernel :=
        Codefix_Module_ID.Kernel;

      Add_Location_Action
        (Kernel        => Codefix_Module_ID.Kernel,
         Identifier    => Location_Button_Name,
         Category      => "Builder Results",
         File          => Get_Error_Message (Error).File_Name.all,
         Line          => Get_Error_Message (Error).Line,
         Column        => Get_Error_Message (Error).Col,
         Message       =>
           Cut_Message (Get_Message (Get_Error_Message (Error))),
         Action        => New_Action);
   end Create_Pixmap;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Codefix_Module_ID := new Codefix_Module_ID_Record;

      Codefix_Module_ID.Current_Text := new GPS_Navigator;
      Codefix_Module_ID.Errors_Found := new Compilation_Output;
      Codefix_Module_ID.Corrector := new Correction_Manager;
      Codefix_Module_ID.Kernel := Kernel_Handle (Kernel);

      GPS_Navigator (Codefix_Module_ID.Current_Text.all).Kernel :=
        Kernel_Handle (Kernel);

      Register_Module
        (Module                  => Module_ID (Codefix_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Codefix_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Codefix_Contextual_Menu'Access);

      --  ??? Disabled for now, as the UI is not quite ready yet.

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => "/" & (-"Tools"),
         Text        => -"_Code Fixing",
         Callback    => Codefix_Handler'Access,
         Sensitive   => False);

      Kernel_Callback.Connect
        (Kernel,
         Compilation_Finished_Signal,
         Compilation_Finished_Cb'Access,
         Kernel_Handle (Kernel));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Register_Module;

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
      Set_Kernel (Console_Interface (File), This.Kernel);
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
     (Menu_Item : access Codefix_Menu_Item_Record;
      Label     : String) is
   begin
      Gtk.Menu_Item.Initialize (Menu_Item, Label);
   end Initialize;

   --------------------
   -- Create_Submenu --
   --------------------

   function Create_Submenu (Error : Error_Id) return Gtk_Menu is
      Menu          : Gtk_Menu;
      Solution_Node : Command_List.List_Node;
      Context       : Selection_Context_Access;
      --  ??? Where is this context freed ?
   begin
      Gtk_New (Menu);
      Context := new Selection_Context;
      Glide_Kernel.Set_Context_Information
        (Context,
         Codefix_Module_ID.Kernel,
         Module_ID (Codefix_Module_ID));

      Solution_Node := First (Get_Solutions (Error));

      while Solution_Node /= Command_List.Null_Node loop
         declare
            Mitem : Codefix_Menu_Item;
            Str   : GNAT.OS_Lib.String_Access;
         begin
            Gtk_New (Mitem, Get_Caption (Data (Solution_Node)));
            Assign (Str, Get_Caption (Data (Solution_Node)));
            Mitem.Fix_Command := new Text_Command'Class'
              (Data (Solution_Node));
            Free (Str);
            Mitem.Error := Error;
            Context_Callback.Connect
              (Mitem,
               "activate",
               Context_Callback.To_Marshaller (On_Fix'Access),
               Context);
            Append (Menu, Mitem);
         end;

         Solution_Node := Next (Solution_Node);
      end loop;

      return Menu;
   end Create_Submenu;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Codefix_Module_ID_Record) is
   begin
      Free (Id.Current_Text);
      Free (Id.Corrector);
      Free (Id.Errors_Found);
      Free_Parsers;
      Destroy (Module_ID_Record (Id));
   end Destroy;

   --------------------------
   -- Execute_Corrupted_Cb --
   --------------------------

   procedure Execute_Corrupted_Cb (Error_Message : String) is
   begin
      Trace
        (Me, "Fix of current error is no longer pertinent");
      Trace (Me, "Exception got: " & Error_Message);
      Insert
        (Codefix_Module_ID.Kernel,
         -"Fix of current error is no longer relevant");
   end Execute_Corrupted_Cb;

end Codefix_Module;
