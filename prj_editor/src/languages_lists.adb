-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Basic_Types;      use Basic_Types;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Glide_Intl;       use Glide_Intl;
with Glide_Kernel;     use Glide_Kernel;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Glib;             use Glib;
with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Object;       use Gtk.Object;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Language_Handlers; use Language_Handlers;
with Projects;         use Projects;
with String_Utils;     use String_Utils;

package body Languages_Lists is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);

   procedure Destroyed (List : access Gtk_Widget_Record'Class);
   --  Called when the languages editor is destroyed

   procedure Changed (List : access Gtk_Widget_Record'Class);
   --  Emits the "changed" signal

   List_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   List_Signals : constant chars_ptr_array := (1 => New_String ("changed"));

   type Language_Check_Button_Record (Length : Natural)
      is new Gtk_Check_Button_Record
   with record
      Language : String (1 .. Length);
   end record;
   type Language_Check_Button is access all Language_Check_Button_Record'Class;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (List : access Gtk_Widget_Record'Class) is
   begin
      Unchecked_Free (Languages_List (List).Languages);
   end Destroyed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (List : out Languages_List;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project : Projects.Project_Type)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));
      Check : Language_Check_Button;
      Box  : Gtk_Box;
   begin
      List := new Languages_List_Record;
      Initialize (List, -"Languages");
      Set_Border_Width (List, 5);

      Gtk.Object.Initialize_Class_Record
        (List,
         Signals      => List_Signals,
         Class_Record => List_Class_Record,
         Type_Name    => "LanguagesList",
         Parameters   => Signal_Parameters);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (List, Box);

      List.Kernel    := Kernel_Handle (Kernel);
      List.Languages := new Widget_Array (Languages'Range);

      for L in Languages'Range loop
         declare
            S : String := Languages (L).all;
         begin
            Mixed_Case (S);
            Check := new Language_Check_Button_Record (S'Length);
            Gtk.Check_Button.Initialize (Check, S);
            Check.Language := Languages (L).all;
            Set_Active (Check, S = "Ada");
         end;
         Pack_Start (Box, Check);

         List.Languages (L) := Gtk_Widget (Check);

         Widget_Callback.Object_Connect
           (Check, "toggled",
            Widget_Callback.To_Marshaller (Changed'Access),
            List);
      end loop;

      if Project /= No_Project then
         declare
            Project_Languages : Argument_List :=  Get_Languages (Project);
         begin
            for L in Languages'Range loop
               Set_Active
                 (Gtk_Check_Button (List.Languages (L)), Contains
                  (Project_Languages, Languages (L).all,
                   Case_Sensitive => False));
            end loop;

            Free (Project_Languages);
         end;
      end if;

      Widget_Callback.Connect
        (List, "destroy", Widget_Callback.To_Marshaller (Destroyed'Access));

      Free (Languages);
   end Gtk_New;

   -------------
   -- Changed --
   -------------

   procedure Changed (List : access Gtk_Widget_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (List, "changed");
   end Changed;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages
     (List : access Languages_List_Record) return GNAT.OS_Lib.Argument_List
   is
      New_Languages : Argument_List (List.Languages'Range);
      Num_Languages : Natural := New_Languages'First;
      Check         : Language_Check_Button;

   begin
      for J in List.Languages'Range loop
         Check := Language_Check_Button (List.Languages (J));

         if Get_Active (Check) then
            New_Languages (Num_Languages) := new String'(Check.Language);
            Num_Languages := Num_Languages + 1;
         end if;
      end loop;

      return New_Languages (New_Languages'First .. Num_Languages - 1);
   end Get_Languages;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (List : access Languages_List_Record; Language : String)
      return Boolean
   is
      Check : constant Gtk_Check_Button := Get_Check_Button
        (List, Language);
   begin
      return Check  /= null and then Get_Active (Check);
   end Is_Selected;

   ----------------------
   -- Get_Check_Button --
   ----------------------

   function Get_Check_Button
     (List : access Languages_List_Record; Language : String)
      return Gtk.Check_Button.Gtk_Check_Button
   is
      Check : Language_Check_Button;
   begin
      for L in List.Languages'Range loop
         Check := Language_Check_Button (List.Languages (L));
         if Check.Language = Language then
            return Gtk_Check_Button (Check);
         end if;
      end loop;
      return null;
   end Get_Check_Button;

end Languages_Lists;
