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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package defines the module for code fixing.

with Ada.Exceptions;         use Ada.Exceptions;

with Gtkada.MDI;             use Gtkada.MDI;

with Glib.Object;            use Glib.Object;
with Glide_Kernel;           use Glide_Kernel;
with Glide_Kernel.Modules;   use Glide_Kernel.Modules;

with Codefix;                use Codefix;
with Codefix.Graphics;       use Codefix.Graphics;
with Codefix.GPS_Io;         use Codefix.GPS_Io;
with Codefix_Interface_Intl; use Codefix_Interface_Intl;
with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;

with Traces;                 use Traces;

package body Codefix_Module is

   Me : constant Debug_Handle := Create ("Codefix_Module");

   procedure Codefix_Handler
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);

   type GPS_Navigator is new Text_Navigator_Abstr with record
      Kernel : Kernel_Handle;
   end record;

   function New_Text_Interface (This : GPS_Navigator) return Ptr_Text;
   --  Create and initialise a new Text_Interface used by the text navigator.

   function Get_Body_Or_Spec (This : GPS_Navigator; File_Name : String)
     return String;

   procedure Initialize
     (This : GPS_Navigator;
      File : in out Text_Interface'Class);

   function Get_Current_Cursor
     (Current_Text : GPS_Navigator;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class;

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
      Navigator       : constant Ptr_Text_Navigator := new GPS_Navigator;
      Errors_List     : constant Ptr_Errors_Interface :=
        new Compilation_Output;
   begin

      Get_Last_Output (Compilation_Output (Errors_List.all), Kernel);
      GPS_Navigator (Navigator.all).Kernel := Kernel;

      Gtk_New
        (Graphic_Codefix,
         Kernel,
         Navigator,
         Errors_List);

      Window := Get_MDI (Kernel);
      Child := Put (Window, Graphic_Codefix);
      Set_Title (Child, "Code fixing", "Codefix");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Codefix_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Register_Module
        (Module                  => Codefix_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Codefix_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => "/" & (-"Tools"),
         Text        => -"_Code Fixing",
         Callback    => Codefix_Handler'Access);
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

   function Get_Body_Or_Spec (This : GPS_Navigator; File_Name : String)
     return String is
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

end Codefix_Module;
