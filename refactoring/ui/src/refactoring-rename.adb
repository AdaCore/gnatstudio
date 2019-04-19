------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib;                       use Glib;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Widget;                 use Gtk.Widget;

with Commands.Interactive;       use Commands, Commands.Interactive;
with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel;                 use GPS.Kernel;
with Histories;                  use Histories;
with Refactoring.UI;             use Refactoring.UI;
with Refactoring.Performers;     use Refactoring.Performers;
with Refactoring.Services;       use Refactoring.Services;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GPS.Dialogs;                use GPS.Dialogs;
with Xref;                       use Xref;

package body Refactoring.Rename is
   Me : constant Trace_Handle := Create ("GPS.REFACTORING.RENAME");

   use Location_Arrays;

   Auto_Save_Hist         : constant History_Key := "refactor_auto_save";
   Auto_Compile_Hist      : constant History_Key := "refactor_auto_compile";
   Rename_Primitives_Hist : constant History_Key := "refactor_primitives";
   Make_Writable_Hist     : constant History_Key := "refactor_make_writable";

   Name_Cst               : aliased constant String := "name";
   Include_Overriding_Cst : aliased constant String := "include_overriding";
   Make_Writable_Cst      : aliased constant String := "make_writable";
   Auto_Save_Cst          : aliased constant String := "auto_save";

   type Rename_Entity_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Rename_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Rename Entity" menu

   type Renaming_Performer_Record is new Refactor_Performer_Record with record
       Auto_Save   : Boolean;
       Old_Name    : Unbounded_String;
       New_Name    : Unbounded_String;
   end record;
   type Renaming_Performer is access all Renaming_Performer_Record'Class;
   overriding procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Refs          : Location_Arrays.List;
      No_LI_List    : Source_File_Set;
      Stale_LI_List : Source_File_Set);
   --  Implements the "Renaming entity" refactoring

   type Entity_Renaming_Dialog_Record is new GPS_Dialog_Record with record
      New_Name          : Gtk_GEntry;
      Auto_Save         : Gtk_Check_Button;
      Auto_Compile      : Gtk_Check_Button;
      Rename_Primitives : Gtk_Check_Button;
      Make_Writable     : Gtk_Check_Button;
   end record;
   type Entity_Renaming_Dialog is access all
     Entity_Renaming_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog : out Entity_Renaming_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class);
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
      Entity : Root_Entity'Class)
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
      GPS.Dialogs.Initialize
        (Dialog,
         Title  => -"Renaming entity",
         Kernel => Kernel);

      Gtk_New (Label, -"Renaming " & Qualified_Name (Entity));
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Get_Content_Area (Dialog), Label, Expand => False);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

      Gtk_New (Label, -"New name: ");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Dialog.New_Name);
      Dialog.New_Name.Set_Name ("new_name");
      Set_Text (Dialog.New_Name, Get_Name (Entity));
      Select_Region (Dialog.New_Name, 0, -1);
      Set_Activates_Default (Dialog.New_Name, True);
      Pack_Start (Box, Dialog.New_Name);

      Gtk_New (Dialog.Auto_Save, -"Automatically save modified files");
      Associate (Get_History (Kernel).all, Auto_Save_Hist, Dialog.Auto_Save);
      Pack_Start
        (Get_Content_Area (Dialog), Dialog.Auto_Save, Expand => False);

      Gtk_New
        (Dialog.Auto_Compile,
         -"Automatically recompile files (not implemented)");
      Set_Sensitive (Dialog.Auto_Compile, False);
      Associate (Get_History (Kernel).all,
                 Auto_Compile_Hist,
                 Dialog.Auto_Compile);
      Pack_Start
        (Get_Content_Area (Dialog), Dialog.Auto_Compile, Expand => False);

      Gtk_New (Dialog.Rename_Primitives,
               -"Rename overriding and overridden entities");
      Set_Tooltip_Text
        (Dialog.Rename_Primitives,
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
      Pack_Start (Get_Content_Area (Dialog), Dialog.Rename_Primitives);

      Gtk_New (Dialog.Make_Writable, -"Make files writable");
      Set_Tooltip_Text
        (Dialog.Make_Writable,
         -("If a read-only file contains references to the entity, this"
           & " switch will make the file writable so that changes can be made."
           & " If the switch is off, then the file will not be edited, but the"
           & " renaming will only be partial."));
      Create_New_Boolean_Key_If_Necessary
        (Hist          => Get_History (Kernel).all,
         Key           => Make_Writable_Hist,
         Default_Value => True);
      Associate (Get_History (Kernel).all,
                 Make_Writable_Hist,
                 Dialog.Make_Writable);
      Pack_Start (Get_Content_Area (Dialog), Dialog.Make_Writable);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Factory       : access Renaming_Performer_Record;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Refs          : Location_Arrays.List;
      No_LI_List    : Source_File_Set;
      Stale_LI_List : Source_File_Set)
   is
      pragma Unreferenced (No_LI_List, Stale_LI_List);
      Buffer_Factory : constant Editor_Buffer_Factory_Access :=
        Get_Buffer_Factory (Kernel);
      Name           : constant String := Get_Name (Entity);
      Errors         : Source_File_Set;

      procedure Process_Locations
        (File : Virtual_File;
         Locs : Location_Arrays.List);
      --  Process a list of locations that are in the same file

      -----------------------
      -- Process_Locations --
      -----------------------

      procedure Process_Locations
        (File : Virtual_File;
         Locs : Location_Arrays.List)
      is
         Was_Open : constant Boolean := Buffer_Factory.Get
           (File  => File,
            Force => False, Open_View => False) /= Nil_Editor_Buffer;
         Buffer   : constant Editor_Buffer'Class := Buffer_Factory.Get (File);
         G        : constant Group_Block := Buffer.New_Undo_Group;
      begin
         for Loc of Locs loop
            if not Insert_Text
              (Kernel.Refactoring_Context,
               Loc.File,
               Loc.Line,
               Loc.Column,
               To_String (Factory.New_Name),
               Indent            => False,
               Replaced_Length   => Name'Length,
               Only_If_Replacing => To_String (Factory.Old_Name))
            then
               Create_Simple_Message
                 (Get_Messages_Container (Kernel),
                  (-"Refactoring - rename ") & To_String (Factory.Old_Name)
                  & (-" to ") & To_String (Factory.New_Name),
                  Loc.File,
                  Loc.Line,
                  Loc.Column,
                  -"error, failed to rename entity",
                  Unspecified,
                  Side_And_Locations);

               Errors.Include (Loc.File);

            else
               --  Renaming done, insert entry into locations view

               Create_Simple_Message
                 (Get_Messages_Container (Kernel),
                  (-"Refactoring - rename ") & To_String (Factory.Old_Name)
                  & (-" to ") & To_String (Factory.New_Name),
                  Loc.File,
                  Loc.Line,
                  Loc.Column,
                  -"entity renamed",
                  Unspecified,
                  Side_And_Locations);
            end if;
         end loop;

         if Factory.Auto_Save then
            Buffer.Save (Interactive => False);
            if not Was_Open then
               Buffer.Close;
            end if;
         end if;
      end Process_Locations;

      C : Location_Arrays.Cursor;
      Current_File : Virtual_File := No_File;
      Current_Loc  : General_Location;
      Locations_In_Current_File : Location_Arrays.List;

   begin
      --  We process the list of locations:
      --     - file by file, so that we create one undo/redo group per file
      --     - in reverse order, so that we do not invalidate locations
      --       during the renaming process
      --  The loop below takes care of the grouping

      C := Refs.Last;
      while Has_Element (C) loop
         declare
            Loc : constant General_Location := Element (C);
         begin
            if Loc = Current_Loc then
               --  Skip the same location. This could happen for subprogram
               --  without separate declaration, when the location is
               --  reported twice. Find_Next_Location promises that
               --  duplicates come one after the other.
               null;
            else
               --  Do we have a new file?
               if Current_File = No_File
                 or else Current_File /= Loc.File
               then
                  if Current_File /= No_File then
                     --  We had a file before: process all locations on this
                     --  previous current file
                     Process_Locations
                       (Current_File, Locations_In_Current_File);
                     Locations_In_Current_File.Clear;
                  end if;

                  --  Bump the current file
                  Current_File := Loc.File;
               end if;

               --  Add the location to the list of locs for the current file
               Locations_In_Current_File.Append (Loc);
               Current_Loc := Loc;
            end if;
         end;
         Previous (C);
      end loop;

      --  We need to catch the leftovers from the loop above

      if Current_File /= No_File then
         Process_Locations (Current_File, Locations_In_Current_File);
      end if;

      --  The calls to Process_Locations above might have generated entries
      --  in the Errors list. Process this now.

      if not Errors.Is_Empty then
         if not Dialog
           (Kernel,
            Title         => -"References not replaced",
            Msg           =>
            -("Some references could not be replaced because one or more files"
              & " were already modified"),
            Files         => Errors,
            Execute_Label => Gtk.Stock.Stock_Ok,
            Cancel_Label  => Gtk.Stock.Stock_Undo)
         then
            declare
               Filenames   : File_Array (1 .. Integer (Refs.Length));
               First_Empty : Positive := 1;
               Found       : Boolean;
               The_File    : Virtual_File := No_File;
            begin
               for Loc of Refs loop
                  if The_File = No_File
                     or else The_File /= Loc.File
                  then
                     The_File := Loc.File;

                     --  We do not want to undo with No_File, since the
                     --  call to Get below would return the current buffer

                     if The_File /= No_File then

                        --  Check whether this file is already in the array

                        Found := False;
                        for F in 1 .. First_Empty - 1 loop
                           if Filenames (F) = The_File then
                              Found := True;
                              exit;
                           end if;
                        end loop;

                        --  The file is not found: add it

                        if not Found then
                           Filenames (First_Empty) := The_File;
                           First_Empty := First_Empty + 1;
                        end if;
                     end if;
                  end if;

                  --  Undo once for every buffer we have

                  for F in 1 .. First_Empty - 1 loop
                     Buffer_Factory.Get (Filenames (F)).Undo;
                  end loop;
               end loop;
            end;
         end if;
      end if;
   exception
      when E : others =>
         Trace (Me, E);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Rename_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Dialog  : Entity_Renaming_Dialog;
      Entity  : constant Root_Entity'Class := Get_Entity (Context.Context);
   begin
      if Entity /= No_Root_Entity then
         if Get_Kernel (Context.Context).Databases.Is_Up_To_Date
           (File_Information (Context.Context))
         then
            Gtk_New (Dialog, Get_Kernel (Context.Context), Entity);
            if Dialog = null then
               return Failure;
            end if;

            Show_All (Dialog);

            if Run (Dialog) = Gtk_Response_OK then
               declare
                  Refactor : constant Renaming_Performer :=
                    new Renaming_Performer_Record'
                      (Refactor_Performer_Record with
                       Old_Name        => To_Unbounded_String
                         (Get_Name (Entity)),
                       New_Name        => To_Unbounded_String
                         (Get_Text (Dialog.New_Name)),
                       Auto_Save       =>
                         Get_Active (Dialog.Auto_Save));
               begin
                  Get_All_Locations
                    (Kernel        => Get_Kernel (Context.Context),
                     Entity        => Entity,
                     On_Completion => Refactor,
                     Overridden    => Get_Active (Dialog.Rename_Primitives),
                     Make_Writable => Get_Active (Dialog.Make_Writable),
                     Auto_Compile  => Get_Active (Dialog.Auto_Compile));
               end;
            end if;

            Destroy (Dialog);
         else
            Create_Simple_Message
              (Get_Messages_Container (Get_Kernel (Context.Context)),
               (-"Refactoring - rename ") & Get_Name (Entity) & " failed",
               File_Information (Context.Context),
               0,
               0,
               -"The navigation information for this file is not up-to-date"
               & " (recompile to regenerate it)",
               Medium,
               Side_And_Locations);
         end if;
      end if;

      return Success;

   exception
      when E : others =>
         Trace (Me, E);
         Destroy (Dialog);
         return Failure;
   end Execute;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "rename" then
         Name_Parameters (Data, (1 => Name_Cst'Access,
                                 2 => Include_Overriding_Cst'Access,
                                 3 => Make_Writable_Cst'Access,
                                 4 => Auto_Save_Cst'Access));
         declare
            Entity         : constant Root_Entity'Class := Get_Data (Data, 1);
            Include_Overridding : constant Boolean := Nth_Arg (Data, 3, True);
            Make_Writable       : constant Boolean := Nth_Arg (Data, 4, False);
            Auto_Save           : constant Boolean := Nth_Arg (Data, 5, False);
            Refactor            : constant Renaming_Performer :=
              new Renaming_Performer_Record'
                (Refactor_Performer_Record with
                 New_Name        => To_Unbounded_String
                   (String'(Nth_Arg (Data, 2))),
                 Old_Name        => To_Unbounded_String
                   (Get_Name (Entity)),
                 Auto_Save       => Auto_Save);
         begin
            Get_All_Locations
              (Get_Kernel (Data),
               Entity,
               Refactor,
               Auto_Compile    => False,
               Overridden      => Include_Overridding,
               Make_Writable   => Make_Writable,
               Background_Mode => False);
         end;
      end if;
   end Entity_Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Register_Action
        (Kernel, "rename entity",
         Command      => new Rename_Entity_Command,
         Description  => -"Rename an entity, including its references",
         Category     => -"Refactoring",
         Filter       => Lookup_Filter (Kernel, "Entity"),
         For_Learning => True);
      Register_Contextual_Menu
        (Kernel,
         Label  => "Refactoring/Rename %s",
         Action => "rename entity",
         Group  => Editing_Contextual_Group);

      Kernel.Scripts.Register_Command
        ("rename", 1, 4, Entity_Command_Handler'Access,
         Get_Entity_Class (Kernel));
   end Register_Refactoring;

end Refactoring.Rename;
