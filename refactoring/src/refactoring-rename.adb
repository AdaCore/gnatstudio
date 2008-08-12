-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007, AdaCore              --
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

with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Scripts;     use GPS.Kernel.Scripts;
with GPS.Intl;               use GPS.Intl;
with Entities;               use Entities;
with Entities.Queries;       use Entities.Queries;
with Traces;                 use Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with Refactoring.Performers; use Refactoring.Performers;
with Histories;              use Histories;
with Commands.Interactive;   use Commands, Commands.Interactive;

with Glib;                   use Glib;
with Gtk.Box;                use Gtk.Box;
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Label;              use Gtk.Label;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Tooltips;           use Gtk.Tooltips;
with Gtk.Widget;             use Gtk.Widget;

package body Refactoring.Rename is

   use File_Arrays;
   use Location_Arrays;

   Auto_Save_Hist         : constant History_Key := "refactor_auto_save";
   Auto_Compile_Hist      : constant History_Key := "refactor_auto_compile";
   Rename_Primitives_Hist : constant History_Key := "refactor_primitives";

   Name_Cst               : aliased constant String := "name";
   Include_Overriding_Cst : aliased constant String := "include_overriding";

   type Rename_Entity_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Rename_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Rename Entity" menu

   type Renaming_Performer_Record (Old_Name_Length, New_Name_Length : Natural)
     is new Refactor_Performer_Record with record
       Auto_Save   : Boolean;
       Old_Name    : String (1 .. Old_Name_Length);
       New_Name    : String (1 .. New_Name_Length);
     end record;
   type Renaming_Performer is access all Renaming_Performer_Record'Class;
   procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information;
      Refs          : Location_Arrays.Instance;
      No_LI_List    : File_Arrays.Instance;
      Stale_LI_List : File_Arrays.Instance);
   --  Implements the "Renaming entity" refactoring.

   type Entity_Renaming_Dialog_Record is new Gtk_Dialog_Record with record
      New_Name          : Gtk_GEntry;
      Auto_Save         : Gtk_Check_Button;
      Auto_Compile      : Gtk_Check_Button;
      Rename_Primitives : Gtk_Check_Button;
   end record;
   type Entity_Renaming_Dialog is access all
     Entity_Renaming_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog : out Entity_Renaming_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information);
   --  Create a new dialog for renaming entities

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handling of shell commands

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
      if not Save_MDI_Children
        (Kernel, Force => False)
      then
         return;
      end if;

      Dialog := new Entity_Renaming_Dialog_Record;
      Gtk.Dialog.Initialize
        (Dialog,
         Title  => -"Renaming entity",
         Parent => Get_Current_Window (Kernel),
         Flags  => Destroy_With_Parent);

      Gtk_New (Label, -"Renaming " & Get_Full_Name (Entity => Entity));
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Vbox (Dialog), Box, Expand => False);

      Gtk_New (Label, -"New name: ");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Dialog.New_Name);
      Set_Text (Dialog.New_Name, Get_Name (Entity).all);
      Select_Region (Dialog.New_Name, 0, -1);
      Set_Activates_Default (Dialog.New_Name, True);
      Pack_Start (Box, Dialog.New_Name);

      Gtk_New (Dialog.Auto_Save, -"Automatically save modified files");
      Associate (Get_History (Kernel).all, Auto_Save_Hist, Dialog.Auto_Save);
      Pack_Start (Get_Vbox (Dialog), Dialog.Auto_Save, Expand => False);

      Gtk_New
        (Dialog.Auto_Compile,
         -"Automatically recompile files (not implemented)");
      Set_Sensitive (Dialog.Auto_Compile, False);
      Associate (Get_History (Kernel).all,
                 Auto_Compile_Hist,
                 Dialog.Auto_Compile);
      Pack_Start (Get_Vbox (Dialog), Dialog.Auto_Compile, Expand => False);

      Gtk_New (Dialog.Rename_Primitives,
               -"Rename overriding and overridden entities");
      Set_Tip
        (Get_Tooltips (Kernel),
         Dialog.Rename_Primitives,
         -("If the entity is a subprogram, also rename subprograms that"
           & " override or are overridden by it." & ASCII.LF
           & "If the entity is a subprogram parameter, also rename parameters"
           & " with the same name in overriding or overridden subprograms."));
      Create_New_Boolean_Key_If_Necessary
        (Hist          => Get_History (Kernel).all,
         Key           => Rename_Primitives_Hist,
         Default_Value => True);
      Associate (Get_History (Kernel).all,
                 Rename_Primitives_Hist,
                 Dialog.Rename_Primitives);
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
      Refs          : Location_Arrays.Instance;
      No_LI_List    : File_Arrays.Instance;
      Stale_LI_List : File_Arrays.Instance)
   is
      pragma Unreferenced (No_LI_List, Stale_LI_List);
      Name : constant String := Get_Name (Entity).all;
      Errors : File_Arrays.Instance := File_Arrays.Empty_Instance;

      procedure Terminate_File (File : Virtual_File);
      --  Finish the processing for a given file

      procedure Terminate_File (File : Virtual_File) is
      begin
         Finish_Undo_Group (Kernel, File);
         if Factory.Auto_Save then
            Execute_GPS_Shell_Command
              (Kernel, "Editor.save_buffer """ & Full_Name (File).all & '"');
         end if;
      end Terminate_File;

   begin
      --  Replace first the last occurrences since we are about to modify
      --  the file, and the locations would become invalid
      for L in reverse Location_Arrays.First .. Last (Refs) loop
         if L = Last (Refs)
           or else Refs.Table (L).File /= Refs.Table (L + 1).File
         then
            if L /= Last (Refs) then
               Terminate_File (Get_Filename (Refs.Table (L + 1).File));
            end if;

            Start_Undo_Group (Kernel, Get_Filename (Refs.Table (L).File));
         end if;

         if not Insert_Text
           (Kernel,
            Get_Filename (Refs.Table (L).File),
            Refs.Table (L).Line,
            Refs.Table (L).Column,
            Factory.New_Name,
            Indent          => False,
            Replaced_Length => Name'Length,
            Only_If_Replacing => Factory.Old_Name)
         then
            if Length (Errors) = 0
              or else Refs.Table (L).File /= Errors.Table (Last (Errors))
            then
               Append (Errors, Refs.Table (L).File);
            end if;
         end if;
      end loop;

      if Length (Refs) > 0 then
         Terminate_File
           (Get_Filename (Refs.Table (Location_Arrays.First).File));
      end if;

      if Length (Errors) > 0 then
         if not Dialog
           (Kernel,
            Title => -"References not replaced",
            Msg   =>
            -("Some references could not be replaced because one or more files"
              & " were already modified"),
            Files => Errors,
            Execute_Label => Gtk.Stock.Stock_Ok,
            Cancel_Label  => Gtk.Stock.Stock_Undo)
         then
            for L in Location_Arrays.First .. Last (Refs) loop
               if L = Location_Arrays.First
                 or else Refs.Table (L).File /= Refs.Table (L - 1).File
               then
                  Execute_GPS_Shell_Command
                    (Kernel, "Editor.undo """
                     & Full_Name (Get_Filename (Refs.Table (L).File)).all
                     & '"');
               end if;
            end loop;
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Rename_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Dialog  : Entity_Renaming_Dialog;
      Entity  : constant Entity_Information :=
        Get_Entity (Context.Context, Ask_If_Overloaded => True);
   begin
      if Entity /= null then
         Gtk_New (Dialog, Get_Kernel (Context.Context), Entity);
         if Dialog = null then
            return Failure;
         end if;

         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK
           and then Get_Name (Entity).all /= Get_Text (Dialog.New_Name)
         then
            declare
               New_Name : constant String := Get_Text (Dialog.New_Name);
               Refactor : constant Renaming_Performer :=
                 new Renaming_Performer_Record'
                   (Refactor_Performer_Record with
                    Old_Name_Length => Get_Name (Entity)'Length,
                    Old_Name        => Get_Name (Entity).all,
                    New_Name_Length => New_Name'Length,
                    New_Name        => New_Name,
                    Auto_Save       => Get_Active (Dialog.Auto_Save));
            begin
               Get_All_Locations
                 (Kernel        => Get_Kernel (Context.Context),
                  Entity        => Entity,
                  On_Completion => Refactor,
                  Overridden    => Get_Active (Dialog.Rename_Primitives),
                  Auto_Compile  => Get_Active (Dialog.Auto_Compile));
            end;
         end if;

         Destroy (Dialog);
      end if;
      return Success;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Destroy (Dialog);
         return Failure;
   end Execute;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
   begin
      if Command = "rename" then
         Name_Parameters (Data, (1 => Name_Cst'Access,
                                 2 => Include_Overriding_Cst'Access));
         declare
            Entity   : constant Entity_Information := Get_Data (Data, 1);
            New_Name : constant String := Nth_Arg (Data, 2);
            Include_Overridding : constant Boolean := Nth_Arg (Data, 3, True);
            Refactor : constant Renaming_Performer :=
              new Renaming_Performer_Record'
                (Refactor_Performer_Record with
                 New_Name_Length => New_Name'Length,
                 New_Name        => New_Name,
                 Old_Name_Length => Get_Name (Entity)'Length,
                 Old_Name        => Get_Name (Entity).all,
                 Auto_Save       => False);
         begin
            Get_All_Locations
              (Get_Kernel (Data),
               Entity,
               Refactor,
               Auto_Compile    => False,
               Overridden      => Include_Overridding,
               Background_Mode => False);
         end;
      end if;
   end Entity_Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access Kernel_Handle_Record'Class)
   is
      C : constant Interactive_Command_Access := new Rename_Entity_Command;
   begin
      Register_Contextual_Menu
        (Kernel,
         Name  => "Rename entity",
         Label => "Refactoring/Rename %e",
         Filter => Lookup_Filter (Kernel, "Entity"),
         Action => C);
      Register_Command
        (Kernel, "rename", 1, 2, Entity_Command_Handler'Access,
         Get_Entity_Class (Kernel));
   end Register_Refactoring;

end Refactoring.Rename;
