------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Tribooleans;       use GNATCOLL.Tribooleans;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Gdk.Event;                  use Gdk.Event;
with Gdk.Window;                 use Gdk.Window;
with Gdk;                        use Gdk;
with Glib;                       use Glib;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk;                        use Gtk;

with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;

with Projects;                   use Projects;
with Src_Editor_Box;             use Src_Editor_Box;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Editor_View;            use Src_Editor_View;
with Std_Dialogs;                use Std_Dialogs;
with Xref;                       use Xref;

package body Src_Editor_Module.Commands is

   Me : constant Trace_Handle := Create ("Source_Editor_Module.Commands");

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Body_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Body (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Dispatching_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Count  : Integer := 0;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Db     : constant General_Xref_Database := Kernel.Databases;

      function On_Callee
        (Callee, Primitive_Of : General_Entity) return Boolean;

      ---------------
      -- On_Callee --
      ---------------

      function On_Callee
        (Callee, Primitive_Of : General_Entity) return Boolean
      is
         pragma Unreferenced (Callee, Primitive_Of);
      begin
         --  Consider dispatching calls only if we find more than one
         --  potential target, to avoid creating submenu with only one entry
         Count := Count + 1;
         return Count <= 1;
      end On_Callee;

   begin
      --  Assertion commented out, since does not always hold, e.g. at start
      --  up when the xref DB is loaded, if the 'Load Xref info' pref is set.
      --  pragma Assert (Frozen (Get_Database (Kernel)) = Create_And_Update);

      if Is_Dispatching_Call (Context) = Indeterminate then
         Push_State (Kernel, Busy);

         Xref.For_Each_Dispatching_Call
           (Dbase     => Db,
            Entity    => Get_Entity (Context),
            Ref       => Get_Closest_Ref (Context),
            On_Callee => On_Callee'Access,
            Policy    => Submenu_For_Dispatching_Calls.Get_Pref);

         Pop_State (Get_Kernel (Context));

         --  See comment above to see why this code is commented out pragma
         --  Assert (Frozen (Get_Database (Kernel)) = Create_And_Update);

         Set_Is_Dispatching_Call (Context, Count > 1);
      end if;

      return Is_Dispatching_Call (Context) = To_Boolean (True);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Get_Entity_Type_Of (Context) /= No_General_Entity;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Parent_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Parent_Types (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Access_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : constant General_Entity := Get_Entity (Context);
      Kernel : constant Kernel_Handle  := Get_Kernel (Context);

   begin
      return Entity /= No_General_Entity
        and then Kernel.Databases.Is_Access (Entity)
        and then Kernel.Databases.Is_Type (Entity);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_File_Information (Context) then
         declare
            File       : constant Virtual_File := File_Information (Context);
            Other_File : constant Virtual_File :=
              Get_Registry (Kernel).Tree.Other_File (File);
         begin
            return Other_File /= No_File
              and then Other_File /= File;
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Line_Numbers_Area_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Event  : constant Gdk_Event := Get_Current_Event;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Editor : constant Source_Editor_Box  :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      return Event /= null
        and then Editor /= null
        and then Get_Window (Event) =
          Get_Window (Editor.Get_View, Text_Window_Left);
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context.Context);
      File       : constant Virtual_File := File_Information (Context.Context);
      Other_File : constant Virtual_File :=
        Get_Registry (Kernel).Tree.Other_File (File);
   begin
      Trace (Me, "Goto_Other_File_Command File="
             & Display_Full_Name (File)
             & " Other_File=" & Display_Full_Name (Other_File));

      if Other_File /= GNATCOLL.VFS.No_File then
         Open_File_Editor (Kernel, Other_File, Line => 0);
         return Standard.Commands.Success;
      else
         return Standard.Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Line_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Box : constant Source_Editor_Box :=
              Get_Source_Box_From_MDI (Find_Current_Editor (Command.Kernel));
   begin
      On_Goto_Line (Box, Command.Kernel);
      return Standard.Commands.Success;
   end Execute;

   ------------------
   -- On_Goto_Line --
   ------------------

   procedure On_Goto_Line
     (Widget : access GObject_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Box : constant Source_Editor_Box := Source_Editor_Box (Widget);
   begin
      declare
         Str : constant String := Simple_Entry_Dialog
           (Get_Current_Window (Kernel),
            -"Goto Line...", -"Enter line number:",
            Win_Pos_Mouse, Get_History (Kernel), "Goto_Line");

      begin
         if Str = "" or else Str (Str'First) = ASCII.NUL then
            return;
         end if;

         Push_Current_Editor_Location_In_History (Kernel);
         Set_Cursor_Location
           (Box, Editable_Line_Type'Value (Str), 1, Centering => With_Margin);
         Add_Navigation_Location (Box);

      exception
         when Constraint_Error =>
            Kernel.Insert
              (-"Invalid line number: " & Str, Mode => Error);
      end;
   end On_Goto_Line;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Declaration_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Box    : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Box,
         Context => Context.Context);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Next_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Box    : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => True,
         Editor  => Box,
         Context => Context.Context);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Type_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Db      : constant General_Xref_Database := Kernel.Databases;
      Entity  : constant General_Entity := Get_Entity (Context.Context);

   begin
      if Entity = No_General_Entity then
         --  Probably means that we either could not locate the ALI file,
         --  or it could also be that we failed to parse it. Either way,
         --  a message should have already been printed. So, just abort.

         Kernel.Insert
           (-"No cross-reference information found for "
            & Entity_Name_Information (Context.Context) & ASCII.LF,
            Mode => Error);
         return Standard.Commands.Failure;

      else
         declare
            Entity_Type : General_Entity;
            Location    : General_Location;

         begin
            Entity_Type := Get_Type_Of (Db, Entity);

            if Is_Predefined_Entity (Db, Entity_Type) then
               Kernel.Insert
                 (Get_Name (Db, Entity)
                  & " is of predefined type "
                  & Get_Name (Db, Entity_Type));
               return Standard.Commands.Failure;

            else
               Location := Db.Get_Declaration (Entity_Type).Loc;
               Go_To_Closest_Match
                 (Kernel,
                  Filename => Location.File,
                  Line     => Editable_Line_Type (Location.Line),
                  Column   => Location.Column,
                  Entity   => Entity_Type);

               return Standard.Commands.Success;
            end if;
         end;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Type_Hierarchy_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Db     : constant General_Xref_Database := Kernel.Databases;

      procedure Insert (Name : String; Entity : General_Entity);
      --  Add entry for Entity into the location view

      function Get_Type_Or_Ref
        (Entity : General_Entity) return General_Entity;
      pragma Inline (Get_Type_Or_Ref);
      --  Retruns the type of Entity (handle case where entity is an access
      --  type, in this case we returned the pointed entity).

      ---------------------
      -- Get_Type_Or_Ref --
      ---------------------

      function Get_Type_Or_Ref
        (Entity : General_Entity) return General_Entity is
      begin
         if Db.Is_Access (Entity) then
            return Db.Pointed_Type (Entity);
         elsif Db.Is_Type (Entity) then
            declare
               Parents : constant Entity_Array :=
                 Db.Parent_Types (Entity, Recursive => False);
            begin
               if Parents'Length /= 0 then
                  return Parents (Parents'First);
               else
                  return No_General_Entity;
               end if;
            end;
         else
            return Get_Type_Of (Db, Entity);
         end if;
      end Get_Type_Or_Ref;

      ------------
      -- Insert --
      ------------

      procedure Insert (Name : String; Entity : General_Entity) is
         Kind : constant String := Db.Get_Display_Kind (Entity);
         Loc  : constant General_Location :=
           Db.Get_Declaration (Entity).Loc;
      begin
         Create_Simple_Message
           (Get_Messages_Container (Kernel),
            -"Type Hierarchy for " & Name,
            Loc.File,
            Loc.Line,
            Loc.Column,
            Get_Name (Db, Entity) & " (" & Kind & ')',
            0,
            (Editor_Side => True,
             Locations   => True));
      end Insert;

      Entity      : constant General_Entity := Get_Entity (Context.Context);
      Entity_Type : General_Entity;

   begin
      if Entity = No_General_Entity then
         --  Probably means that we either could not locate the ALI file,
         --  or it could also be that we failed to parse it. Either way,
         --  a message should have already been printed. So, just abort.

         Kernel.Insert_UTF8
           (-"No cross-reference information found for "
            & Entity_Name_Information (Context.Context) & ASCII.LF,
            Mode => Error);
         return Standard.Commands.Failure;

      else
         declare
            Name : constant String := Db.Get_Name (Entity);
         begin
            if Db.Is_Type (Entity) then
               Insert (Name, Entity);
               Entity_Type := Get_Type_Or_Ref (Entity);
            else
               Entity_Type := Db.Get_Type_Of (Entity);
            end if;

            if Is_Predefined_Entity (Db, Entity_Type) then
               Kernel.Insert
                 (Name & (-" is of predefined type ")
                  & Get_Name (Db, Entity_Type));
               return Standard.Commands.Failure;
            end if;

            loop
               exit when Entity_Type = No_General_Entity
                 or else Db.Is_Predefined_Entity (Entity_Type);

               Insert (Name, Entity_Type);
               Entity_Type := Get_Type_Or_Ref (Entity_Type);
            end loop;
         end;

         return Standard.Commands.Success;
      end if;
   end Execute;

end Src_Editor_Module.Commands;
