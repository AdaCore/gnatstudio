-----------------------------------------------------------------------
--                               G P S                               --
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

--  This package defines the module for code fixing.

with Ada.Exceptions;         use Ada.Exceptions;

with Gtk.Menu;               use Gtk.Menu;
with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtkada.MDI;             use Gtkada.MDI;
with Gdk.Pixbuf;             use Gdk.Pixbuf;

with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;
with Glide_Kernel;           use Glide_Kernel;
with Glide_Kernel.Modules;   use Glide_Kernel.Modules;
with Glide_Kernel.Console;   use Glide_Kernel.Console;
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

   function New_Text_Interface (This : GPS_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   function Get_Body_Or_Spec
     (This : GPS_Navigator; File_Name : String) return String;
   --  Return the spec name if File_Name is a body, or the body name if
   --  File_Name is a spec

   procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class);
   --  Set the value of the Text_Interface's kernel

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
      Window          : MDI_Window;
      Child           : MDI_Child;
   begin
      Update_All (Codefix_Module_ID.Current_Text.all);

      Gtk_New
        (Graphic_Codefix,
         Kernel,
         Codefix_Module_ID.Current_Text,
         Codefix_Module_ID.Corrector,
         Remove_Pixmap'Access,
         Create_Pixmap'Access);

      Window := Get_MDI (Kernel);
      Child := Put (Window, Graphic_Codefix);
      Set_Title (Child, -"Code fixing", -"Codefix");

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

      Location      : File_Location_Context_Access;
      Error         : Error_Id;
      Error_Caption : Dynamic_String := null;
      Menu_Item     : Gtk_Menu_Item;

   begin
      if Context.all in File_Location_Context'Class then
         Location := File_Location_Context_Access (Context);

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
         Identifier    => "--  ???",
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

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => "/" & (-"Tools"),
         Text        => -"_Code Fixing",
         Callback    => Codefix_Handler'Access);


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

   ----------------------
   -- Get_Body_Or_Spec --
   ----------------------

   function Get_Body_Or_Spec
     (This : GPS_Navigator; File_Name : String) return String
   is
      pragma Unreferenced (This);
   begin
      --  ??? Should ask the project for the body file instead
      case File_Name (File_Name'Last) is
         when 'b' =>
            return File_Name (File_Name'First .. File_Name'Last - 1) & 's';
         when 's' =>
            return File_Name (File_Name'First .. File_Name'Last - 1) & 'b';
         when others =>
            raise Codefix_Panic;
      end case;
   end Get_Body_Or_Spec;

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
      --  ??? Where this context is freed ?
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
            Str : Dynamic_String;
         begin
            Gtk_New (Mitem, Get_Caption (Data (Solution_Node)));
            Assign (Str, Get_Caption (Data (Solution_Node)));
            Mitem.Fix_Command := new Text_Command'Class'
              (Data (Solution_Node));
            Codefix.Free (Str);
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
         -"Fix of current error is no longer pertinent");
   end Execute_Corrupted_Cb;

end Codefix_Module;
