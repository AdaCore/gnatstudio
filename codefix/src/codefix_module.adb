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
with Gtk.Widget;             use Gtk.Widget;

with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;
with Glide_Kernel;           use Glide_Kernel;
with Glide_Kernel.Modules;   use Glide_Kernel.Modules;

with Codefix;                use Codefix;
with Codefix.Graphics;       use Codefix.Graphics;
with Codefix.GPS_Io;         use Codefix.GPS_Io;
with Glide_Intl;             use Glide_Intl;
with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;

with Traces;                 use Traces;

package body Codefix_Module is

   Me : constant Debug_Handle := Create ("Codefix_Module");

   procedure On_Fix
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  ???

   procedure Codefix_Handler
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  ???

   procedure Codefix_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  ???

   procedure Compilation_Finished_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  ???

   type GPS_Navigator is new Text_Navigator_Abstr with record
      Kernel : Kernel_Handle;
   end record;

   function New_Text_Interface (This : GPS_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   function Get_Body_Or_Spec
     (This : GPS_Navigator; File_Name : String) return String;
   --  ???

   procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class);
   --  ???

   function Get_Current_Cursor
     (Current_Text : GPS_Navigator;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class;
   --  ???

   ------------
   -- On_Fix --
   ------------

   procedure On_Fix
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Mitem : constant Codefix_Menu_Item := Codefix_Menu_Item (Widget);
      pragma Unreferenced (Context);

      Result : Extract;
   begin
      Execute
        (Mitem.Fix_Command.all,
         Codefix_Module_ID.Current_Text.all,
         Result);

      Validate_And_Commit
        (Codefix_Module_ID.Corrector,
         Codefix_Module_ID.Current_Text.all,
         Mitem.Error,
         Mitem.Fix_Command.all);

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
   begin
      Free (Codefix_Module_ID.Errors_Found.all);
      Free (Codefix_Module_ID.Corrector);

      Get_Last_Output
        (Compilation_Output
           (Codefix_Module_ID.Errors_Found.all), Kernel);

      Analyze
        (Codefix_Module_ID.Corrector,
         Codefix_Module_ID.Current_Text.all,
         Codefix_Module_ID.Errors_Found.all,
         null);

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
      Gtk_New
        (Graphic_Codefix,
         Kernel,
         Codefix_Module_ID.Current_Text);

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
      Solution_Node : Command_List.List_Node;
      Menu_Item     : Gtk_Menu_Item;
      Submenu       : Gtk_Menu;

   begin
      if Context.all in File_Location_Context'Class then
         Location := File_Location_Context_Access (Context);

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
         Gtk_New (Submenu);

         Error := Search_Error
           (Codefix_Module_ID.Corrector, Error_Caption.all);
         if Error /= Null_Error_Id then
            Solution_Node := First (Get_Solutions (Error));

            while Solution_Node /= Command_List.Null_Node loop
               declare
                  Mitem : Codefix_Menu_Item;
               begin
                  Gtk_New (Mitem, Get_Caption (Data (Solution_Node)));
                  Mitem.Fix_Command := new Text_Command'Class'
                    (Data (Solution_Node));
                  Mitem.Error := Error;
                  Context_Callback.Connect
                    (Mitem,
                     "activate",
                     Context_Callback.To_Marshaller (On_Fix'Access),
                     Selection_Context_Access (Context));
                  Append (Submenu, Mitem);
               end;

               Solution_Node := Next (Solution_Node);
            end loop;

            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
            Append (Menu, Menu_Item);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Codefix_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Codefix_Module_ID := new Codefix_Module_ID_Record;

      Codefix_Module_ID.Current_Text := new GPS_Navigator;
      Codefix_Module_ID.Errors_Found := new Compilation_Output;
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

   ------------------------
   -- Get_Current_Cursor --
   ------------------------

   function Get_Current_Cursor
     (Current_Text : GPS_Navigator;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class
   is
      New_Cursor : File_Cursor;
   begin
      Assign
        (New_Cursor.File_Name,
         Interpret_Command
           (Current_Text.Kernel,
            "get_file " & Get_Id (GPS_Mark (Mark))));

      New_Cursor.Col := Natural'Value
        (Interpret_Command
           (Current_Text.Kernel,
            "get_column " & Get_Id (GPS_Mark (Mark))));

      New_Cursor.Line := Natural'Value
        (Interpret_Command
           (Current_Text.Kernel,
            "get_line " & Get_Id (GPS_Mark (Mark))));

      return New_Cursor;
   end Get_Current_Cursor;

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

end Codefix_Module;
