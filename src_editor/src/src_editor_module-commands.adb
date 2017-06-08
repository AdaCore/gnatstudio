------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Tribooleans;       use GNATCOLL.Tribooleans;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Charsets;        use GPS.Kernel.Charsets;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Main_Window;            use GPS.Main_Window;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Window;                 use Gdk.Window;
with Gdk;                        use Gdk;
with Glib;                       use Glib;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Size_Group;             use Gtk.Size_Group;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk;                        use Gtk;
with Gtk.Stock;                  use Gtk.Stock;
with Gtkada.File_Selector;       use Gtkada.File_Selector;
with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;
with Language_Handlers.GUI;      use Language_Handlers.GUI;
with Projects;                   use Projects;
with Src_Editor_Box;             use Src_Editor_Box;
with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Module.Markers;       use Src_Editor_Module.Markers;
with Src_Editor_Buffer.Text_Handling; use Src_Editor_Buffer.Text_Handling;
with Src_Editor_Module;               use Src_Editor_Module;
with Src_Editor_View;                 use Src_Editor_View;
with Src_Printing.Fabric;
with GPS.Dialogs;                     use GPS.Dialogs;
with UTF8_Utils;                      use UTF8_Utils;
with Xref;                            use Xref;

package body Src_Editor_Module.Commands is

   Me : constant Trace_Handle := Create ("Source_Editor_Module.Commands");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Entity'Class,
      Root_Entity_Access);

   procedure Comment_Uncomment
     (Kernel  : Kernel_Handle;
      Comment : Boolean;
      Context : GPS.Kernel.Selection_Context);
   --  Comment or uncomment the current selection, if any.
   --  Auxiliary procedure for On_Comment_Lines and On_Uncomment_Lines.

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Specification_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Specification (Context);
   end Filter_Matches_Primitive;

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
      Count : Integer := 0;

      function On_Callee (Callee : Root_Entity'Class) return Boolean;

      ---------------
      -- On_Callee --
      ---------------

      function On_Callee (Callee : Root_Entity'Class) return Boolean is
         pragma Unreferenced (Callee);
      begin
         --  Consider dispatching calls only if we find more than one
         --  potential target, to avoid creating submenu with only one entry
         Count := Count + 1;
         return Count <= 1;
      end On_Callee;

      --  Ensure Xref has been computed for the context
      Entity : Root_Entity'Class := Get_Entity (Context);
      pragma Unreferenced (Entity);
   begin

      --  Assertion commented out, since does not always hold, e.g. at start
      --  up when the xref DB is loaded, if the 'Load Xref info' pref is set.
      --  pragma Assert (Frozen (Get_Database (Kernel)) = Create_And_Update);

      if Is_Dispatching_Call (Context) = Indeterminate then
         Xref.For_Each_Dispatching_Call
           (Ref       => Get_Closest_Ref (Context),
            On_Callee => On_Callee'Access);

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
      return Get_Entity_Type_Of (Context) /= No_Root_Entity;
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
      Entity : constant Root_Entity'Class := Get_Entity (Context);

   begin
      return Entity /= No_Root_Entity
        and then Is_Access (Entity)
        and then Is_Type (Entity);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Line_Numbers_Area_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Event  : constant Gdk_Event         := Get_Current_Event;
      Kernel : constant Kernel_Handle     := Get_Kernel (Context);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      return Event /= null
        and then Editor /= null
        and then
          Get_Window (Event) =
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
      Other_File : constant Virtual_File  :=
        Get_Registry (Kernel).Tree.Other_File (File);
   begin
      Trace
        (Me,
         "Goto_Other_File_Command File=" &
         Display_Full_Name (File) &
         " Other_File=" &
         Display_Full_Name (Other_File));

      if Other_File /= GNATCOLL.VFS.No_File then
         Open_File_Action_Hook.Run
         (Kernel, Other_File,
          Project => Project_Information (Context.Context),
          Line    => 0);
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
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Box    : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      On_Goto_Line (Box, Kernel);
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
         Str : constant String :=
           Display_Text_Input_Dialog
             (Kernel  => Kernel,
              Title   => -"Goto Line...",
              Message => -"Enter line number:",
              Key     => "Goto_Line");
      begin
         if Str = "" or else Str (Str'First) = ASCII.NUL then
            return;
         end if;

         Push_Current_Editor_Location_In_History (Kernel);
         Set_Cursor_Location
           (Box,
            Editable_Line_Type'Value (Str),
            1,
            Centering => With_Margin);
         Add_Navigation_Location (Box);

      exception
         when Constraint_Error =>
            Kernel.Insert (-"Invalid line number: " & Str, Mode => Error);
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
      Kernel : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Box    : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Box /= null then
         Goto_Declaration_Or_Body
           (Kernel,
            To_Body => False,
            Editor  => Box,
            Context => Context.Context);
      end if;
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
      Kernel : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Entity : constant Root_Entity'Class := Get_Entity (Context.Context);

   begin
      if Entity = No_Root_Entity then
         --  Probably means that we either could not locate the ALI file,
         --  or it could also be that we failed to parse it. Either way,
         --  a message should have already been printed. So, just abort.

         Kernel.Insert
            (-"No cross-reference information found for " &
             Entity_Name_Information (Context.Context) & ASCII.LF,
             Mode => Error);
         return Standard.Commands.Failure;

      else
         declare
            Entity_Type : constant Root_Entity'Class := Get_Type_Of (Entity);
            Location    : General_Location;

         begin
            if Is_Predefined_Entity (Entity_Type) then
               Kernel.Insert
               (Get_Name (Entity) &
                  (-" is of predefined type """) &
                  Get_Name (Entity_Type) & """");
               return Standard.Commands.Failure;

            else
               Location := Get_Declaration (Entity_Type).Loc;
               Go_To_Closest_Match
                 (Kernel,
                  Filename => Location.File,
                  Project  => Location.Project,
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

      procedure Insert (Name : String; Entity : Root_Entity'Class);
      --  Add entry for Entity into the location view

      function Get_Type_Or_Ref
        (Entity : Root_Entity'Class) return Root_Entity'Class;
      pragma Inline (Get_Type_Or_Ref);
      --  Retruns the type of Entity (handle case where entity is an access
      --  type, in this case we returned the pointed entity).

      ---------------------
      -- Get_Type_Or_Ref --
      ---------------------

      function Get_Type_Or_Ref
        (Entity : Root_Entity'Class) return Root_Entity'Class
      is
      begin
         if Is_Access (Entity) then
            return Pointed_Type (Entity);
         elsif Is_Type (Entity) then
            declare
               Parents : Entity_Array :=
                 Parent_Types (Entity, Recursive => False);
            begin
               if Parents'Length /= 0 then
                  declare
                     Res : constant Root_Entity'Class :=
                       Parents (Parents'First).all;
                  begin
                     Free (Parents);
                     return Res;
                  end;
               else
                  Free (Parents);
                  return No_Root_Entity;
               end if;
            end;
         else
            return Get_Type_Of (Entity);
         end if;
      end Get_Type_Or_Ref;

      ------------
      -- Insert --
      ------------

      procedure Insert (Name : String; Entity : Root_Entity'Class) is
         Kind : constant String           := Get_Display_Kind (Entity);
         Loc  : constant General_Location := Get_Declaration (Entity).Loc;
      begin
         Create_Simple_Message
           (Get_Messages_Container (Kernel),
            -"Type Hierarchy for " & Name,
            Loc.File,
            Loc.Line,
            Loc.Column,
            Get_Name (Entity) & " (" & Kind & ')',
            0,
            Side_And_Locations);
      end Insert;

      Entity      : constant Root_Entity'Class := Get_Entity (Context.Context);
      Entity_Type : Root_Entity_Access;

   begin
      if Entity = No_Root_Entity then
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
            Name : constant String := Get_Name (Entity);
         begin
            if Is_Type (Entity) then
               Insert (Name, Entity);
               Entity_Type := new Root_Entity'Class'(Get_Type_Or_Ref (Entity));
            else
               Entity_Type := new Root_Entity'Class'(Get_Type_Of (Entity));
            end if;

            if Is_Predefined_Entity (Entity_Type.all) then
               Kernel.Insert
                  (Name & (-" is of predefined type """) &
                   Get_Name (Entity_Type.all) & """");
               Unchecked_Free (Entity_Type);
               return Standard.Commands.Failure;
            end if;

            loop
               exit when Entity_Type.all = No_Root_Entity
                 or else Is_Predefined_Entity (Entity_Type.all);

               Insert (Name, Entity_Type.all);

               declare
                  N : constant Root_Entity'Class :=
                    Get_Type_Or_Ref (Entity_Type.all);
               begin
                  Unchecked_Free (Entity_Type);
                  Entity_Type := new Root_Entity'Class'(N);
               end;
            end loop;
         end;

         Unchecked_Free (Entity_Type);
         return Standard.Commands.Success;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle    := Get_Kernel (Context.Context);
      Dir    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
   begin
      if Has_Directory_Information (Context.Context) then
         Dir := Directory_Information (Context.Context);
      end if;

      declare
         Filename : constant Virtual_File :=
           Select_File
             (Title             => -"Open File",
              Base_Directory    => Dir,
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
              Kind              => Open_File,
              File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
              Pattern_Name      => -"All files;Ada files;C/C++ files",
              History           => Get_History (Kernel));
      begin
         if Filename /= GNATCOLL.VFS.No_File then
            --  Open with the first possible project, the user cannot choose
            --  which specific project to use (in the case of aggregates)
            Open_File_Action_Hook.Run
               (Kernel, Filename, Project => No_Project);
         end if;
      end;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_Remote_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      declare
         Filename : constant Virtual_File :=
           Select_File
             (Title             => -"Open Remote File",
              Parent            => Get_Current_Window (Kernel),
              Remote_Browsing   => True,
              Use_Native_Dialog => False,
              Kind              => Open_File,
              File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
              Pattern_Name      => -"All files;Ada files;C/C++ files",
              History           => Get_History (Kernel));

      begin
         if Filename /= GNATCOLL.VFS.No_File then
            Open_File_Action_Hook.Run
               (Kernel, Filename, Project => No_Project);
         end if;
      end;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access New_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ignore : Source_Editor_Box;
      pragma Unreferenced (Command, Ignore);
      Dir : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;

   begin
      if Has_Directory_Information (Context.Context) then
         Dir := Directory_Information (Context.Context);
      end if;

      Ignore :=
        Open_File
          (Kernel,
           File        => GNATCOLL.VFS.No_File,
           Project     => GNATCOLL.Projects.No_Project,
           Line        => 1,
           Column      => 1,
           Column_End  => 1,
           Initial_Dir => Dir);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Save_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Success : Boolean;
      pragma Unreferenced (Command);
   begin
      Save_To_File (Kernel, Success => Success);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Save_As_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Success : Boolean;
      Source  : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Source /= null then
         declare
            Old_Name : constant Virtual_File := Get_Filename (Source);
            New_Name : constant Virtual_File :=
              Select_File
                (Title             => -"Save File As",
                 Parent            => Get_Current_Window (Kernel),
                 Base_Directory    => Dir (Old_Name),
                 Default_Name      => Base_Name (Old_Name),
                 Remote_Browsing   => not Is_Local (Old_Name),
                 Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                 Kind              => Save_File,
                 File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                 Pattern_Name      => -"All files;Ada files;C/C++ files",
                 History           => Get_History (Kernel));

         begin
            if New_Name /= GNATCOLL.VFS.No_File then
               Save_To_File (Kernel, New_Name, Success);
            end if;
         end;
      end if;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      MDI    : MDI_Window;
      Child  : MDI_Child;
   begin
      case Command.Mode is
         when Close_All =>
            if Save_MDI_Children (Kernel) then
               Close_All_Children (Kernel);
            end if;

         when Close_One =>
            MDI   := Get_MDI (Kernel);
            Child := Get_Focus_Child (MDI);

            if Child /= null then
               Close (MDI, Get_Widget (Child));
            end if;
         when Close_All_Except_Current =>
            declare
               Buffer : constant Editor_Buffer'Class :=
                 GPS.Editors.Get
                   (This        => Get_Buffer_Factory (Kernel).all,
                    File        => No_File,
                    Force       => False,
                    Open_Buffer => False,
                    Open_View   => False);
               List : constant Buffer_Lists.List :=
                 Buffers (Get_Buffer_Factory (Kernel).all);

            begin
               for Editor of List loop
                  if Editor /= Buffer then
                     Editor.Close;
                  end if;
               end loop;
            end;
      end case;

      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Print_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Child  : constant MDI_Child     := Find_Current_Editor (Kernel);
      Source : Source_Editor_Box;
   begin
      if Get_Focus_Child (Get_MDI (Kernel)) /= Child then
         Kernel.Insert ("No source file selected", Mode => Error);
         return Standard.Commands.Failure;
      end if;

      Source := Get_Source_Box_From_MDI (Child);

      if Source = null then
         return Standard.Commands.Failure;
      end if;

      Src_Printing.Fabric.Create.Print (Source);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Print_Selection_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Child : constant MDI_Child     := Find_Current_Editor (Kernel);
      Source               : Source_Editor_Box;
      Start_Line, End_Line : Editable_Line_Type;
   begin
      if Has_Area_Information (Context.Context) then
         Get_Area (Context.Context, Natural (Start_Line), Natural (End_Line));
      else
         Kernel.Insert ("No selection", Mode => Error);
         return Standard.Commands.Failure;
      end if;

      if Get_Focus_Child (Get_MDI (Kernel)) /= Child then
         Kernel.Insert ("No source file selected", Mode => Error);
         return Standard.Commands.Failure;
      end if;

      Source := Get_Source_Box_From_MDI (Child);

      if Source = null then
         return Standard.Commands.Failure;
      end if;

      Src_Printing.Fabric.Create.Print (Source, Start_Line, End_Line);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access New_View_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      New_View (Kernel);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Editor : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Editor = null then
         return Standard.Commands.Failure;
      end if;

      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => True,
         Editor  => Editor,
         Context => Context.Context);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Comment_Lines_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Comment_Uncomment (Kernel, Comment => True, Context => Context.Context);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Fold_All_Blocks_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Current /= null then
         Src_Editor_Buffer.Line_Information.Fold_All (Get_Buffer (Current));
      end if;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Unfold_All_Blocks_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      if Current /= null then
         Src_Editor_Buffer.Line_Information.Unfold_All (Get_Buffer (Current));
      end if;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Uncomment_Lines_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Comment_Uncomment (Kernel, Comment => False, Context => Context.Context);
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Refill_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle     := Get_Kernel (Context.Context);
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Ignore : Boolean;
      pragma Unreferenced (Command, Ignore);
   begin
      if Current /= null then
         Ignore := Do_Refill (Get_Buffer (Current));
      end if;
      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Line : Natural;
   begin
      Trace
        (Me,
         "On_Edit_File: " & (+Full_Name (File_Information (Context.Context))));

      if Has_Line_Information (Context.Context) then
         Line := Contexts.Line_Information (Context.Context);
      else
         Line := 1;
      end if;

      Open_File_Action_Hook.Run
         (Get_Kernel (Context.Context),
          File    => File_Information (Context.Context),
          Project => Project_Information (Context.Context),
          Line    => Line,
          Column  => Column_Information (Context.Context));
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Editor_Properties_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      File : constant GNATCOLL.VFS.Virtual_File :=
        File_Information (Context.Context);
      Kernel      : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dialog      : Gtk_Dialog;
      Label       : Gtk_Label;
      Lang        : Gtk_Combo_Box_Text;
      Charset     : Gtk_Combo_Box_Text;
      Strip       : Gtk_Check_Button;
      Strip_Lines : Gtk_Check_Button;
      Box         : Gtk_Box;
      Size        : Gtk_Size_Group;
      Buffer      : Source_Buffer;
      Ignore      : Gtk_Widget;
      pragma Unreferenced (Ignore);

   begin
      Buffer :=
        Get_Buffer
          (Get_Source_Box_From_MDI (Get_Focus_Child (Get_MDI (Kernel))));
      if Buffer = null then
         return Failure;
      end if;

      Gtk_New
        (Dialog,
         Title  => -"Properties for " & Display_Full_Name (File),
         Parent => Get_Main_Window (Kernel),
         Flags  => Destroy_With_Parent);
      Set_Default_Size_From_History (Dialog, "editor-props", Kernel, 400, 200);

      Gtk_New (Size);

      --  Base name

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => True);
      Gtk_New (Label, -"File:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Label, Display_Base_Name (File));
      Label.Set_Selectable (True);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);

      --  Directory

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => True);
      Gtk_New (Label, -"Directory:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);
      Gtk_New (Label, Unknown_To_UTF8 (+Dir_Name (File)));
      Label.Set_Selectable (True);
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);

      --  Language

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Language: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Lang :=
        Create_Language_Combo
          (Get_Language_Handler (Kernel),
           File,
           Default => Get_Name (Get_Language (Buffer)));
      Pack_Start (Box, Lang, Expand => True, Fill => True);

      --  Charset

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Character set: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Charset := Create_Charset_Combo (File, Default => Get_Charset (Buffer));
      Pack_Start (Box, Charset, Expand => True, Fill => True);

      --  Trailing spaces

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Strip blanks: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Strip, "");
      Strip.Set_Active (Get_Strip_Trailing_Blanks (Buffer));
      Pack_Start (Box, Strip, Expand => True, Fill => True);

      --  Trailing blank lines

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => True);

      Gtk_New (Label, -"Strip lines: ");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Size, Label);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Strip_Lines, "");
      Strip_Lines.Set_Active (Get_Strip_Trailing_Lines (Buffer));
      Pack_Start (Box, Strip_Lines, Expand => True, Fill => True);

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Grab_Default (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Text   : constant String := Get_Active_Text (Lang);
            Header : constant String := -"(From project) ";
            Index  : Natural         := Text'First;
         begin
            if Text'Length >= Header'Length
              and then Text (Index .. Index + Header'Length - 1) = Header
            then
               Index := Index + Header'Length;
            end if;

            Set_Language
              (Buffer,
               Get_Language_By_Name
                 (Get_Language_Handler (Kernel),
                  Text (Index .. Text'Last)));
            Set_Charset (Buffer, Selected_Charset (Charset));

            Set_Strip_Trailing_Blanks (Buffer, Strip.Get_Active);
            Set_Strip_Trailing_Lines (Buffer, Strip_Lines.Get_Active);
         end;
      end if;

      Destroy (Dialog);
      return Success;
   end Execute;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Kernel  :     access Kernel_Handle_Record'Class;
      Name    :     GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success : out Boolean)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Source : Source_Editor_Box;
   begin
      if Child = null then
         Success := False;
         return;
      end if;

      Source := Source_Editor_Box (Get_Widget (Child));
      Save_To_File (Source, Name, Success);
   end Save_To_File;

   --------------
   -- New_View --
   --------------

   procedure New_View (Kernel : access Kernel_Handle_Record'Class) is
      Current : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Ignore : Source_Editor_Box;
      pragma Unreferenced (Ignore);

   begin
      if Current /= null then
         Ignore := New_View (Kernel, Current, Get_Project (Current));
      end if;
   end New_View;

   -----------------------
   -- Comment_Uncomment --
   -----------------------

   procedure Comment_Uncomment
     (Kernel  : Kernel_Handle;
      Comment : Boolean;
      Context : GPS.Kernel.Selection_Context)
   is
      pragma Unreferenced (Context);
      Editor : constant MDI_Child := Find_Current_Editor (Kernel);
   begin
      if Editor /= null then
         declare
            Start_Line : Editable_Line_Type;
            Start_Col  : Character_Offset_Type;
            End_Line   : Editable_Line_Type;
            End_Col    : Character_Offset_Type;
            Buffer     : Source_Buffer;
            Lang       : Language_Access;
            Found      : Boolean;
            Block      : Unbounded_String := Null_Unbounded_String;
         begin
            Buffer := Get_Buffer (Get_Source_Box_From_MDI (Editor));

            --  Return immediately if the editor is not writable

            if not Get_Writable (Buffer) then
               return;
            end if;

            --  Get the selection bounds of the current editor

            Buffer.Get_Selection_Bounds
              (Start_Line   => Start_Line,
               Start_Column => Start_Col,
               End_Line     => End_Line,
               End_Column   => End_Col,
               Found        => Found);

            --  Comment the selected text if any or the current line if there
            --  is no selection. Don't comment the ending line of the selection
            --  if no character is selected on this line
            --  (i.e: when End_Col = 1).

            if not Found then
               Buffer.Get_Cursor_Position
                 (Line   => Start_Line,
                  Column => Start_Col);
               End_Line := Start_Line;
            elsif End_Col = 1 and then End_Line > Start_Line then
               End_Line := End_Line - 1;
            end if;

            for J in Start_Line .. End_Line loop
               Append
                 (Block,
                  Get_Chars (Buffer => Buffer, Line => J, Column => 1));
            end loop;

            Lang := Buffer.Get_Language;

            Replace_Slice
              (Buffer,
               Text   => Comment_Block (Lang, To_String (Block), Comment),
               Line   => Start_Line,
               Column => 1,
               Before => 0,
               After  => UTF8_Utils.UTF8_Length (To_String (Block)));
         end;
      end if;
   end Comment_Uncomment;

end Src_Editor_Module.Commands;
