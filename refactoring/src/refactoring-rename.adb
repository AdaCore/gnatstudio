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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;        use Ada.Exceptions;

with Glide_Kernel;          use Glide_Kernel;
with Glide_Kernel.Contexts; use Glide_Kernel.Contexts;
with Glide_Kernel.Scripts;  use Glide_Kernel.Scripts;
with Glide_Intl;            use Glide_Intl;
with Src_Info;              use Src_Info;
with Src_Info.Queries;      use Src_Info.Queries;
with Traces;                use Traces;
with VFS;                   use VFS;
with Refactoring.Performers; use Refactoring.Performers;

with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Gtk.Box;               use Gtk.Box;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Label;             use Gtk.Label;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Widget;            use Gtk.Widget;

package body Refactoring.Rename is

   Me : constant Debug_Handle := Create ("Refactor");

   type Renaming_Performer_Record (New_Name_Length : Natural)
     is new Refactor_Performer_Record with record
       Auto_Save : Boolean;
       New_Name  : String (1 .. New_Name_Length);
     end record;
   type Renaming_Performer is access all Renaming_Performer_Record'Class;
   procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information;
      Refs          : Location_Array;
      No_LI_List    : File_Array;
      Stale_LI_List : File_Array);
   --  Implements the "Renaming entity" refactoring.

   type Entity_Renaming_Dialog_Record is new Gtk_Dialog_Record with record
      New_Name          : Gtk_GEntry;
      Auto_Save         : Gtk_Check_Button;
      Rename_Primitives : Gtk_Check_Button;
   end record;
   type Entity_Renaming_Dialog is access all
     Entity_Renaming_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog : out Entity_Renaming_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information);
   --  Create a new dialog for renaming entities

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog : out Entity_Renaming_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);
   begin
      Dialog := new Entity_Renaming_Dialog_Record;
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => -"Renaming entity",
         Parent => Get_Main_Window (Kernel),
         Flags  => Destroy_With_Parent);

      Gtk_New (Label, -"Renaming "
               & Get_Full_Name (Entity    => Entity,
                                Decl_File => Locate_From_Source_And_Complete
                                  (Kernel, Get_Declaration_File_Of (Entity))));
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

      Gtk_New (Label, -"New name: ");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Dialog.New_Name);
      Set_Text (Dialog.New_Name, Get_Name (Entity));
      Select_Region (Dialog.New_Name, 0, -1);
      Set_Activates_Default (Dialog.New_Name, True);
      Pack_Start (Box, Dialog.New_Name);

      Gtk_New (Dialog.Auto_Save, -"Automatically save modified files");
      Set_Active (Dialog.Auto_Save, False);
      Pack_Start (Get_Vbox (Dialog), Dialog.Auto_Save, Expand => False);

      Gtk_New (Dialog.Rename_Primitives,
               -"Rename overriding and overridden entities (not implemented)");
      Set_Sensitive (Dialog.Rename_Primitives, False);
      Pack_Start (Get_Vbox (Dialog), Dialog.Rename_Primitives);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information;
      Refs          : Location_Array;
      No_LI_List    : File_Array;
      Stale_LI_List : File_Array)
   is
      pragma Unreferenced (No_LI_List, Stale_LI_List);
      Name : constant String := Get_Name (Entity);
   begin
      --  Replace first the last occurrences since we are about to modify
      --  the file, and the locations would become invalid
      for L in reverse Refs'Range loop
         Execute_GPS_Shell_Command
           (Kernel  => Kernel,
            Command => "Editor.replace_text "
              & Full_Name (Refs (L).File).all
              & Integer'Image (Refs (L).Line)
              & Integer'Image (Refs (L).Column)
              & ' ' & Factory.New_Name
              & " 0 " & Integer'Image (Name'Length));

         if Factory.Auto_Save
           and then (L = Refs'First or else Refs (L).File /= Refs (L - 1).File)
         then
            Execute_GPS_Shell_Command
              (Kernel, "Editor.save_buffer """
               & Full_Name (Refs (L).File).all & '"');
         end if;
      end loop;
   end Execute;

   ----------------------
   -- On_Rename_Entity --
   ----------------------

   procedure On_Rename_Entity
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Ent     : Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Dialog  : Entity_Renaming_Dialog;
      Entity  : constant Entity_Information := Get_Entity (Ent);
   begin
      Ref (Selection_Context_Access (Ent));

      if Entity /= No_Entity_Information then
         Gtk_New (Dialog, Get_Kernel (Context), Entity);
         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK
           and then Get_Name (Entity) /= Get_Text (Dialog.New_Name)
         then
            declare
               New_Name : constant String := Get_Text (Dialog.New_Name);
               Refactor : constant Renaming_Performer :=
                 new Renaming_Performer_Record'
                   (Refactor_Performer_Record with
                    New_Name_Length => New_Name'Length,
                    New_Name        => New_Name,
                    Auto_Save       => Get_Active (Dialog.Auto_Save));
            begin
               Get_All_Locations (Get_Kernel (Context), Entity, Refactor);
            end;
         end if;

         Destroy (Dialog);
      end if;

      Unref (Selection_Context_Access (Ent));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Unref (Selection_Context_Access (Ent));
         Destroy (Dialog);
   end On_Rename_Entity;

end Refactoring.Rename;
