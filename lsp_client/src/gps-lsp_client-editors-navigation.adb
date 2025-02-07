------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2025, AdaCore                  --
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

with Ada.Containers.Vectors;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;
with GNATCOLL.Xref;

with Gdk.Device;                use Gdk.Device;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Main;                  use Gdk.Main;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Screen;                use Gdk.Screen;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Window;                use Gdk.Window;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib;                      use Glib;
with Glib_Values_Utils;         use Glib_Values_Utils;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.MDI;                use Gtkada.MDI;

with VSS.Strings.Conversions;

with GPS.Editors;               use GPS.Editors;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;

with GPS.LSP_Client.Editors.Tooltips;
with GPS.LSP_Client.Requests.Simple_Editor_Requests;
use GPS.LSP_Client.Requests.Simple_Editor_Requests;
with GPS.LSP_Client.Requests;   use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Utilities;  use GPS.LSP_Client.Utilities;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;

with Default_Preferences;       use Default_Preferences;
with Default_Preferences.Enums; use Default_Preferences.Enums;
with GUI_Utils;                 use GUI_Utils;
with Language;                  use Language;
with LSP.Types;                 use LSP.Types;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Module;         use Src_Editor_Module;
with Xref;                      use Xref;

package body GPS.LSP_Client.Editors.Navigation is

   Me : constant Trace_Handle := Create
     ("GPS.LSP.NAVIGATION", GNATCOLL.Traces.On);

   Me_Advanced : constant Trace_Handle := Create
     ("GPS.LSP.NAVIGATION.ADVANCED", GNATCOLL.Traces.Off);
   Proposals_Menu_Notes_Width  : constant := 500;
   Proposals_Menu_Notes_Height : constant := 150;
   --  The size of the entities proposals menu notes.

   -----------------
   -- Preferences --
   -----------------

   package Display_Ancestry_On_Navigation_Prefs is
     new Default_Preferences.Enums.Generics
       (Enumeration =>
           LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy);

   Display_Ancestry_On_Navigation_Pref :
      Display_Ancestry_On_Navigation_Prefs.Preference;

   --  Implementation of simple text editor requests

   type GPS_LSP_Simple_Request is new Abstract_Simple_Request with record
      Entity_Name : Unbounded_String;
      Column      : Visible_Column_Type;
   end record;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Simple_Request;
      Result : LSP.Messages.Location_Or_Link_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Simple_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding function Auto_Cancel
     (Self : in out GPS_LSP_Simple_Request) return Boolean is (True);

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Simple_Request; Reason : Reject_Reason);

   --------------------------------------------------------------------
   -- Goto Declaration/Body/Type Declaration Commands and Hyper Mode --
   --------------------------------------------------------------------

   type Goto_Command_Type is new Interactive_Command with record
      Action_Kind : Command_Kind;
   end record;
   overriding function Execute
     (Command : access Goto_Command_Type;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Goto actions command type.

   type Entity_Info_Type is record
      Label        : Unbounded_String;
      Project_Path : Virtual_File;
      File         : Virtual_File;
      Line         : Editable_Line_Type;
      Column       : Visible_Column_Type;
   end record;
   --  Type used to represent an entity.

   procedure Cancel_Activity_Bar
     (Kernel : Kernel_Handle; File : Virtual_File);
   --  Remove the activity bar for the editor for the given file, if any

   package Entity_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Entity_Info_Type,
      "="          => "=");

   type Entity_Proposals_Menu_Record is new Gtk_Window_Record with record
      Kernel       : Kernel_Handle;
      Tree_View    : Gtk_Tree_View;
      Notes_Window : Gtk_Scrolled_Window;
   end record;
   type Entity_Proposals_Menu is access all Entity_Proposals_Menu_Record'Class;
   --  Type representing a menu that displays several entity proposals.
   --  This kind of menus is displayed when the language server returns several
   --  locations in response to editor requests.

   Col_Label   : constant := 0;
   Col_Project : constant := 1;
   Col_File    : constant := 2;
   Col_Line    : constant := 3;
   Col_Column  : constant := 4;
   --  The column numbers used for entity proposals

   Column_Types : constant Glib.GType_Array (0 .. 4) :=
     (Col_Label   => GType_String,
      Col_Project => Get_Virtual_File_Type,
      Col_File    => Get_Virtual_File_Type,
      Col_Line    => GType_Int,
      Col_Column  => GType_Int);
   --  The column types used for entity proposals

   function Get_Primitives_Hierarchy_On_Dispatching
     (Context     : Selection_Context;
      Action_Kind : Command_Kind)
      return Entity_Info_Vectors.Vector;
   --  When the user's cursor is on a dispatching call, return all the
   --  declarations/bodies in the hierarchy for this primitive.

   procedure Display_Menu_For_Entities_Proposals
     (Kernel   : not null Kernel_Handle;
      Entities : Entity_Info_Vectors.Vector;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type);
   --  Display a menu with all the listed entity proposals.
   --  When specified, Root_X and Root_Y are used to position the menu: the
   --  current pointer's position is used otherwise.

   type On_MDI_Child_Selected is new Mdi_Child_Hooks_Function with record
      Proposals_Menu : Entity_Proposals_Menu;
   end record;
   overriding procedure Execute
     (Self   : On_MDI_Child_Selected;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : Gtkada.MDI.MDI_Child);
   --  Called when the focused MDI child changes. Used to close the
   --  entities proposals menu.

   function On_Entity_Proposals_Menu_Key_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Called when the users presses a key in the entity proposals menu.
   --  Close the menu if the ESC key was pressed.

   procedure On_Entity_Proposals_Menu_Show
     (Self : access Gtk_Widget_Record'Class);
   --  Called when the entity proposals menu is shown.
   --  Used to grab the keyboard focus.

   procedure On_Entity_Proposals_Menu_Destroy
     (Self : access Gtk_Widget_Record'Class);
   --  Called when the entity proposals menu is destroyed.
   --  Used to make sure we ungrab the keyboard focus.

   procedure On_Entity_Item_Clicked
     (Self   : access GObject_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : not null access Gtk_Tree_View_Column_Record'Class);
   --  Called when clicking on an entity proposal menu item.
   --  Jump to the clicked entity.

   procedure On_Entity_Item_Selected
     (Self : access GObject_Record'Class);
   --  Called when selecting an entity proposal menu item.
   --  Display the associated hover text in the menu's notes part.

   procedure Search_Entity_From_Comment
     (Buffer            : GPS.Editors.Editor_Buffer'Class;
      Src_Buffer        : Source_Buffer;
      Entity_Name       : String;
      Line              : in out Editable_Line_Type;
      Column            : in out Visible_Column_Type);
   --  Search the first occurrence of Entity_Name in Buffer that is not within
   --  a comment. The location of the occurrence is returned in Line and Column
   --  if the search succeed, otherwise Line and Column will be left
   --  Unmodified.

   procedure LSP_Hyper_Mode_Click_Callback
     (Kernel      : not null Kernel_Handle;
      Buffer      : GPS.Editors.Editor_Buffer'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Line        : Editable_Line_Type;
      Column      : Visible_Column_Type;
      Entity_Name : String;
      Alternate   : Boolean);
   --  The hyper mode click callback based on the LSP.
   --  When the LSP is disabled for the buffer's language, defaults to the
   --  default behavior based on the old xref engine.

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Command_Type;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use GNATCOLL.Xref;

      Kernel        : constant Kernel_Handle := Get_Kernel (Context.Context);
      File          : constant Virtual_File :=
        File_Information (Context.Context);
      Project       : constant Project_Type := Get_Project_For_File
        (Kernel.Get_Project_Tree, File => File);
      Buffer        : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File, Open_View => False, Open_Buffer => False);
      Editor        : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI
          (Find_Editor
             (Kernel,
              File          => File,
              Project       => Project,
              Unlocked_Only => False));
      Lang          : constant Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_By_Name
          (Get_File_Language (Context.Context));
      Line          : constant Editable_Line_Type := Editable_Line_Type
        (Line_Information (Context.Context));
      Column        : constant Visible_Column_Type := Column_Information
        (Context.Context);
      Entity_Name   : constant String := Entity_Name_Information
        (Context.Context);
      Src_Buffer    : constant Source_Buffer :=
        (if Editor /= null then Editor.Get_Buffer else null);
      Actual_Line   : Editable_Line_Type := Line;
      Actual_Column : Visible_Column_Type := Column;
      Request       : Request_Access;
   begin
      if Editor = null then
         return Commands.Failure;
      end if;

      --  Check if we are tring to exeucte the navigation action from a
      --  comment: if it's the case, search for an occurrence of the mentioned
      --  entity in the current file and try to execute the request from this
      --  occurrence.

      if Is_In_Comment (Src_Buffer, Line, Column) then
         Search_Entity_From_Comment
           (Buffer      => Buffer,
            Src_Buffer  => Src_Buffer,
            Entity_Name => Entity_Name,
            Line        => Actual_Line,
            Column      => Actual_Column);
      end if;

      declare
         Location : constant GPS.Editors.Editor_Location'Class :=
           Buffer.New_Location (Integer (Actual_Line), Actual_Column);
      begin
         Request := new GPS_LSP_Simple_Request'
           (GPS.LSP_Client.Requests.LSP_Request with
            Kernel      => Get_Kernel (Context.Context),
            Command     => Command.Action_Kind,
            File        => File_Information (Context.Context),
            Position    => Location_To_LSP_Position (Location),
            Entity_Name => To_Unbounded_String
              (Entity_Name_Information (Context.Context)),
            Display_Ancestry_On_Navigation =>
              Display_Ancestry_On_Navigation_Pref.Get_Pref,
            Column      => Location.Column);
      end;

      Editor.Set_Activity_Progress_Bar_Visibility (True);

      if GPS.LSP_Client.Requests.Execute
        (Language => Lang,
         Request  => Request_Access (Request))
      then
         return Commands.Success;
      end if;

      --  The request is not sent, use "old" implementation

      if Is_Dispatching (Context.Context) then
         Display_Menu_For_Entities_Proposals
           (Kernel   => Kernel,
            Entities => Get_Primitives_Hierarchy_On_Dispatching
              (Context     => Context.Context,
               Action_Kind => Command.Action_Kind),
            Line     => Actual_Line,
            Column   => Actual_Column);
      else
         declare
            Entity   : constant Root_Entity'Class :=
                         Get_Entity (Context.Context);
            Location : General_Location;
            Current  : General_Location;
         begin
            if Entity = No_Root_Entity then
               --  Probably means that we either could not locate the ALI file,
               --  or it could also be that we failed to parse it. Either way,
               --  a message should have already been printed. So, just abort.

               Kernel.Insert
                 ("No cross-reference information found for "
                  & Entity_Name_Information (Context.Context) & ASCII.LF,
                  Mode => Error);
               return Commands.Failure;
            end if;

            --  Get the declaration/body

            case Command.Action_Kind is
               when Goto_Body =>
                  Current :=
                    (File         => File,
                     Line         => Line_Information (Context.Context),
                     Project_Path => Project_Information
                       (Context.Context).Project_Path,
                     Column       => Entity_Column_Information
                       (Context.Context));
                  Location := Get_Body (Entity, After => Current);
               when Goto_Spec | Goto_Spec_Or_Body =>
                  Get_Entity_Spec_Locations (Context.Context, Location);
               when Goto_Type_Decl =>
                  Location := Get_Declaration (Get_Type_Of (Entity)).Loc;
            end case;

            if Location /= No_Location then
               Go_To_Closest_Match
                 (Kernel                      => Kernel,
                  Filename                    => Location.File,
                  Project                     => Get_Project (Location),
                  Line                        => Editable_Line_Type
                    (Location.Line),
                  Column                      => Location.Column,
                  Entity_Name                 => Get_Name (Entity),
                  Display_Msg_On_Non_Accurate => False);
            end if;
         end;
      end if;

      return Commands.Success;
   end Execute;

   --------------------------------
   -- Search_Entity_From_Comment --
   --------------------------------

   procedure Search_Entity_From_Comment
     (Buffer            : GPS.Editors.Editor_Buffer'Class;
      Src_Buffer        : Source_Buffer;
      Entity_Name       : String;
      Line              : in out Editable_Line_Type;
      Column            : in out Visible_Column_Type)
   is
      Occurrence_Start_Loc : Editor_Location'Class :=
        Buffer.New_Location
          (Line   => 1,
           Column => 1);
      Occurrence_End_Loc   : Editor_Location'Class := Occurrence_Start_Loc;
      Continue_Search      : Boolean := True;
      Success              : Boolean := False;
   begin
      --  Search the first occurrence in the buffer that is not within a
      --  comment

      while Continue_Search loop
         Occurrence_End_Loc.Search
           (Pattern           => Entity_Name,
            Whole_Word        => True,
            Dialog_On_Failure => False,
            Starts            => Occurrence_Start_Loc,
            Ends              => Occurrence_End_Loc,
            Success           => Success);

         Continue_Search := Success
           and then Is_In_Comment
             (Src_Buffer,
              Editable_Line_Type (Occurrence_Start_Loc.Line),
              Occurrence_Start_Loc.Column);
      end loop;

      if Success then
         Line := Editable_Line_Type (Occurrence_Start_Loc.Line);
         Column := Occurrence_Start_Loc.Column;
      end if;
   end Search_Entity_From_Comment;

   -----------------------------------
   -- LSP_Hyper_Mode_Click_Callback --
   -----------------------------------

   procedure LSP_Hyper_Mode_Click_Callback
     (Kernel      : not null Kernel_Handle;
      Buffer      : GPS.Editors.Editor_Buffer'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Line        : Editable_Line_Type;
      Column      : Visible_Column_Type;
      Entity_Name : String;
      Alternate   : Boolean)
   is
      Request       : Request_Access;
      File          : constant Virtual_File := Buffer.File;
      Editor        : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI
          (Find_Editor
             (Kernel,
              File          => File,
              Project       => Project,
              Unlocked_Only => False));
      Src_Buffer    : constant Source_Buffer := Editor.Get_Buffer;
      Actual_Line   : Editable_Line_Type := Line;
      Actual_Column : Visible_Column_Type := Column;
   begin
      if Me.Is_Active then

         --  Check if we are ctrl-clicking on a comment: if it's the case,
         --  search for an occurrence of the clicked enity in the current file
         --  and try to go to its declaration.

         if Is_In_Comment (Src_Buffer, Line, Column) then
            Search_Entity_From_Comment
              (Buffer      => Buffer,
               Src_Buffer  => Src_Buffer,
               Entity_Name => Entity_Name,
               Line        => Actual_Line,
               Column      => Actual_Column);

            declare
               Location : constant GPS.Editors.Editor_Location'Class :=
                 Buffer.New_Location
                   (Integer (Actual_Line), Actual_Column);
            begin
               Request := new GPS_LSP_Simple_Request'
                 (GPS.LSP_Client.Requests.LSP_Request with
                  Kernel                         => Kernel,
                  Command                        => Goto_Spec,
                  File                           => File,
                  Position                       =>
                    Location_To_LSP_Position (Location),
                  Entity_Name                    =>
                    To_Unbounded_String (Entity_Name),
                  Display_Ancestry_On_Navigation =>
                    Display_Ancestry_On_Navigation_Pref.Get_Pref,
                  Column                         => Location.Column);
            end;

         else
            declare
               Location : constant GPS.Editors.Editor_Location'Class :=
                 Buffer.New_Location
                   (Integer (Actual_Line), Actual_Column);
            begin
               Request := new GPS_LSP_Simple_Request'
                 (GPS.LSP_Client.Requests.LSP_Request with
                  Kernel                         => Kernel,
                  Command                        =>
                    (if Alternate then Goto_Body else Goto_Spec_Or_Body),
                  File                           => File,
                  Position                       =>
                    Location_To_LSP_Position (Location),
                  Entity_Name                    =>
                    To_Unbounded_String (Entity_Name),
                  Display_Ancestry_On_Navigation =>
                    Display_Ancestry_On_Navigation_Pref.Get_Pref,
                  Column                         => Location.Column);
            end;
         end if;

         Editor.Set_Activity_Progress_Bar_Visibility (True);
      end if;

      if not GPS.LSP_Client.Requests.Execute
        (Language => Buffer.Get_Language,
         Request  => Request_Access (Request))
      then
         --  Use old implementation

         Src_Editor_Module.Default_Hyper_Mode_Click_Callback
           (Kernel      => Kernel,
            Buffer      => Buffer,
            Project     => Project,
            Line        => Actual_Line,
            Column      => Actual_Column,
            Entity_Name => Entity_Name,
            Alternate   => Alternate);
      end if;
   end LSP_Hyper_Mode_Click_Callback;

   -------------------------
   -- Cancel_Activity_Bar --
   -------------------------

   procedure Cancel_Activity_Bar
     (Kernel : Kernel_Handle; File : Virtual_File)
   is
      Project : constant Project_Type := Get_Project_For_File
        (Kernel.Get_Project_Tree, File =>  File);
      Editor  : constant Source_Editor_Box :=
                  Get_Source_Box_From_MDI
                    (Find_Editor
                       (Kernel,
                        File    => File,
                        Project => Project,
                        Unlocked_Only => False));
   begin
      if Editor /= null then
         Editor.Set_Activity_Progress_Bar_Visibility (False);
      end if;
   end Cancel_Activity_Bar;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Simple_Request;
      Result : LSP.Messages.Location_Or_Link_Vector)
   is
      use type Ada.Containers.Count_Type;

      function Kinds_Label
        (Kind : LSP.Messages.AlsReferenceKind_Set) return String;
      --  Return a label for displaying the kinds in the menus

      -----------------
      -- Kinds_Label --
      -----------------

      function Kinds_Label
        (Kind : LSP.Messages.AlsReferenceKind_Set) return String
      is
         use type LSP.Messages.AlsReferenceKind_Set;

         Has_Content : Boolean := False;
         Result      : Unbounded_String;
      begin
         if Kind = LSP.Messages.Empty_Set then
            return "";
         end if;

         for Str of Kind.As_Strings loop
            if Has_Content then
               Append (Result, ", ");
            end if;
            Append
              (Result, VSS.Strings.Conversions.To_UTF_8_String (Str));
            Has_Content := True;
         end loop;

         if Has_Content then
            return "[" & To_String (Result) & "] ";
         else
            return "";
         end if;
      end Kinds_Label;

      use type LSP.Messages.Location_Or_Link_Kind;
   begin
      Trace (Me_Advanced, "Result received");

      Cancel_Activity_Bar (Self.Kernel, Self.File);

      if Result.Kind = LSP.Messages.Empty_Vector_Kind then
         Trace (Me_Advanced, "No locations found");
         return;
      elsif Result.Kind = LSP.Messages.LocationLink_Vector_Kind then
         Trace (Me_Advanced, "Unexpected result kind");
         return;
      end if;

      --  If we have only one location in the result, go to it.
      --  Otherwise, display all the location proposals in a contextual
      --  menu. This can happen when ctrl-clicking on dispatching calls for
      --  instance.

      if Result.Locations.Length = 1 then
         declare
            Loc     : constant LSP.Messages.Location :=
              Result.Locations.First_Element;
            File    : constant Virtual_File := To_Virtual_File (Loc.uri);
            Infos   : constant File_Info_Set := Get_Registry
              (Self.Kernel).Tree.Info_Set (File);
            Project : constant Project_Type :=
              File_Info'Class (Infos.First_Element).Project (True);
            --  Don't forget to add 1 to both line and column numbers since
            --  LSP lines/columns are zero-based.
            Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
              Self.Kernel.Get_Buffer_Factory.Get_Holder (File => File);
            Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Loc.span.first);
         begin
            --  Go the closest match of the returned location.

            case Self.Command is
               when Goto_Spec | Goto_Body | Goto_Spec_Or_Body =>
                  --  In the case of Goto_Spec or Goto_Body, we can use
                  --  Go_To_Closest_Match on the result, attempting to find
                  --  Entity_Name.
                  Go_To_Closest_Match
                    (Kernel      => Self.Kernel,
                     Filename    => File,
                     Project     => Project,
                     Line        => Editable_Line_Type (Location.Line),
                     Column      => Location.Column,
                     Entity_Name => To_String (Self.Entity_Name),
                     Display_Msg_On_Non_Accurate => False);

               when Goto_Type_Decl =>
                  --  In the case of Goto_Type_Decl, the Entity_Name is not
                  --  what we're looking for: don't try Go_To_Closest_Match.
                  declare
                     To : constant GPS.Editors.Editor_Location'Class :=
                       GPS.LSP_Client.Utilities.LSP_Position_To_Location
                         (Holder.Editor, Loc.span.last);
                  begin
                     Open_File_Action_Hook.Run
                       (Kernel     => Self.Kernel,
                        File       => File,
                        Project    => Project,
                        Line       => Location.Line,
                        Column     => Location.Column,
                        Column_End => To.Column);
                  end;
            end case;
         end;
      else
         declare
            Entities : Entity_Info_Vectors.Vector;
         begin
            for Location of Result.Locations loop
               declare
                  File    : constant Virtual_File := To_Virtual_File
                    (Location.uri);
                  Infos   : constant File_Info_Set := Get_Registry
                    (Self.Kernel).Tree.Info_Set (File);
                  Project : constant Project_Type :=
                    File_Info'Class (Infos.First_Element).Project
                    (True);
                  Holder  : constant GPS.Editors.
                    Controlled_Editor_Buffer_Holder :=
                      Self.Kernel.Get_Buffer_Factory.Get_Holder (File => File);
                  From    : constant GPS.Editors.Editor_Location'Class :=
                    GPS.LSP_Client.Utilities.LSP_Position_To_Location
                      (Holder.Editor, Location.span.first);
               begin
                  Entities.Append
                    (Entity_Info_Type'
                       (Label        => To_Unbounded_String
                            (Kinds_Label (Location.alsKind)
                             & "<b>" & Escape_Text
                               (To_String (Self.Entity_Name))
                             & "</b>"
                             & " in <b>"
                             & Escape_Text (File.Display_Base_Name)
                             & "</b>"),
                        Project_Path => Project.Project_Path,
                        File         => File,
                        Line         => Editable_Line_Type (From.Line),
                        Column       => From.Column));
               end;
            end loop;

            Display_Menu_For_Entities_Proposals
              (Kernel   => Self.Kernel,
               Entities => Entities,
               Line     => Editable_Line_Type (Self.Position.line + 1),
               Column   => Self.Column);
         end;
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Simple_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.File);
      Trace
        (Me,
         "Error received after sending "
         & VSS.Strings.Conversions.To_UTF_8_String (Self.Method));
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Simple_Request; Reason : Reject_Reason)
   is
      pragma Unreferenced (Reason);
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.File);
      Trace
        (Me_Advanced,
         VSS.Strings.Conversions.To_UTF_8_String (Self.Method)
         & " has been rejected");
   end On_Rejected;

   -----------------------------------------
   -- Display_Menu_For_Entities_Proposals --
   -----------------------------------------

   procedure Display_Menu_For_Entities_Proposals
     (Kernel   : not null Kernel_Handle;
      Entities : Entity_Info_Vectors.Vector;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type)
   is
      Proposals_Menu : Entity_Proposals_Menu;
      Hbox           : Gtk_Hbox;
      Scrolled       : Gtk_Scrolled_Window;
      Sep            : Gtk_Hseparator;
      Model          : Gtk_Tree_Store;
      Iter           : Gtk_Tree_Iter;
      Menu_Width     : Gint;
      Total_Width    : Gint;
      Total_Height   : Gint;
      Dummy          : Gint;

      procedure Get_Size;
      --  Retrieve the size of the entities proposals menu widgets, in order
      --  to place it correctly.

      procedure Set_Position;
      --  Set the position of the entities proposals menu

      procedure Get_Size is
         Sep_Width : Gint;
      begin
         --  Realize and show the widgets before trying to get their size,
         --  otherwise it won't work.

         Scrolled.Realize;
         Scrolled.Show_All;
         Sep.Realize;
         Sep.Show_All;

         --  Get the menu's size

         Proposals_Menu.Tree_View.Get_Preferred_Width
           (Dummy, Menu_Width);
         Sep.Get_Preferred_Width (Dummy, Sep_Width);
         Menu_Width := Menu_Width + Sep_Width;

         --  Set the window's total size

         Total_Width := Menu_Width + Proposals_Menu_Notes_Width;
         Total_Height := Proposals_Menu_Notes_Height;
      end Get_Size;

      procedure Set_Position is
         Editor            : constant Source_Editor_Box :=
           Get_Source_Box_From_MDI
             (Find_Current_Editor (Kernel, Only_If_Focused => True));
         Toplevel          : constant Gtk_Window :=
           Gtk_Window (Editor.Get_Toplevel);
         Screen            : constant Gdk_Screen := Toplevel.Get_Screen;
         Geom              : Gdk_Rectangle;
         Monitor           : Gint;
         Root_X            : Gint;
         Root_Y            : Gint;
         Screen_Width      : Gint;
         Screen_Height     : Gint;
      begin
         if Editor = null then
            return;
         end if;

         --  Get the root coordinates for the request's original position:
         --  we'll place the menu right under by default.

         Editor.Get_View.Get_Root_Coords_For_Location
           (Line   => Line + 1, Column => Column,
            Root_X => Root_X, Root_Y => Root_Y);

         --  Get the screen size

         Monitor := Screen.Get_Monitor_At_Point (Root_X, Root_Y);
         Screen.Get_Monitor_Geometry (Monitor, Geom);

         Screen_Width  := Geom.Width;
         Screen_Height := Geom.Height;

         --  Set Root_X, Root_Y into the physical monitor area, this is needed
         --  to check if the tooltip actually fit into the current monitor or
         --  not.

         Root_X := Root_X - Geom.X;
         Root_Y := Root_Y - Geom.Y;

         --  If the whole window (menu and notes) goes outside of the screen on
         --  the x-axis, two cases:
         --
         --    . The menu in itself (without the notes) does not fit on the
         --      screen : in that case, shift the menu's position by the total
         --      window's size and place the notes on the left, before the
         --      menu.
         --
         --    . The menu in itself (the left part) fits on the screen: in that
         --      case, just place the notes before the menu.

         if Root_X + Total_Width > Screen_Width then

            if Root_X + Menu_Width > Screen_Width then
               Root_X := Screen_Width - Total_Width;
            else
               Root_X := Root_X - Proposals_Menu_Notes_Width;
            end if;

            Hbox.Reorder_Child
              (Proposals_Menu.Notes_Window, 0);
         end if;

         --  If the window goes outside of the screen on the y-axis, place it
         --  above the request's original position, instead of under.

         if Root_Y + Total_Height > Screen_Height then
            Editor.Get_View.Get_Root_Coords_For_Location
              (Line   => Line, Column => Column,
               Root_X => Dummy, Root_Y => Root_Y);
            Root_Y := Root_Y - Total_Height;
         end if;

         --  Don't forget to add back the active screen's X, Y when placing the
         --  menu.
         Proposals_Menu.Move (Root_X + Geom.X, Root_Y + Geom.Y);
      end Set_Position;

   begin
      --  Create the menu's window.
      --  We can't use Gtk_Menu widgets here because they are modal, and we
      --  want the user to be able to scroll in the side notes window to
      --  see the hover text corresponding to a given proposal (a kind of
      --  preview).

      Proposals_Menu := new Entity_Proposals_Menu_Record;
      Proposals_Menu.Kernel := Kernel;

      Gtk.Window.Initialize (Proposals_Menu,  Window_Popup);
      Proposals_Menu.Set_Type_Hint (Window_Type_Hint_Menu);
      Proposals_Menu.Set_Decorated (False);
      Proposals_Menu.Set_Resizable (False);
      Proposals_Menu.Set_Skip_Taskbar_Hint (True);
      Proposals_Menu.Set_Skip_Pager_Hint (True);
      Proposals_Menu.Set_Name ("entity-proposals-menu");
      Get_Style_Context (Proposals_Menu).Add_Class ("menu");

      Proposals_Menu.On_Key_Press_Event
        (On_Entity_Proposals_Menu_Key_Press'Access);
      Proposals_Menu.On_Show
        (On_Entity_Proposals_Menu_Show'Access);
      Proposals_Menu.On_Destroy
        (On_Entity_Proposals_Menu_Destroy'Access);

      --  Create the menu's hbox

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Proposals_Menu.Add (Hbox);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Hbox.Pack_Start (Scrolled, Expand => False);

      --  Create the menu's tree view

      Proposals_Menu.Tree_View := Create_Tree_View
        (Column_Types => Column_Types,
         Column_Names       => (1 => new String'("Label")),
         Show_Column_Titles => False);
      Model := -(Proposals_Menu.Tree_View.Get_Model);

      for Entity of Entities loop
         Model.Append (Iter, Null_Iter);

         Set_And_Clear
           (Model,
            Iter,
            (Col_Label, Col_Project, Col_File, Col_Line, Col_Column),
            (1 => As_String (To_String (Entity.Label)),
             2 => As_File (Entity.Project_Path),
             3 => As_File (Entity.File),
             4 => As_Int (Gint (Entity.Line)),
             5 => As_Int (Gint (Entity.Column))));
      end loop;

      Scrolled.Add (Proposals_Menu.Tree_View);
      Proposals_Menu.Tree_View.Set_Activate_On_Single_Click (True);
      Proposals_Menu.Tree_View.Set_Hover_Selection (True);

      Proposals_Menu.Tree_View.On_Row_Activated
        (On_Entity_Item_Clicked'Access,
         Slot => Proposals_Menu);
      Proposals_Menu.Tree_View.Get_Selection.On_Changed
        (On_Entity_Item_Selected'Access,
         Slot => Proposals_Menu);

      --  Add a separator between the tree view that displays the proposals
      --  and the right part that displays the associated notes.

      Gtk_New_Vseparator (Sep);
      Hbox.Pack_Start (Sep, Expand => False);

      --  Create the notes scrolled window.

      Gtk_New (Proposals_Menu.Notes_Window);
      Proposals_Menu.Notes_Window.Set_Name ("entity-proposals-menu-notes");
      Get_Style_Context (Proposals_Menu.Notes_Window).Add_Class
        ("notes");
      Hbox.Pack_Start (Proposals_Menu.Notes_Window, Expand => False);
      Proposals_Menu.Notes_Window.Set_Policy
        (Policy_Automatic,
         Policy_Automatic);
      Proposals_Menu.Notes_Window.Set_Size_Request
        (Proposals_Menu_Notes_Width, Proposals_Menu_Notes_Height);

      --  Display the menu's window

      Get_Size;
      Set_Position;
      Proposals_Menu.Show_All;

      --  React on MDI focus changes, to close the entities proposals menu
      --  if the user clicks somewhere else.
      Mdi_Child_Selected_Hook.Add
        (new On_MDI_Child_Selected'
           (Mdi_Child_Hooks_Function with Proposals_Menu => Proposals_Menu),
         Watch => Proposals_Menu);
   end Display_Menu_For_Entities_Proposals;

   ---------------------------------------------
   -- Get_Primitives_Hierarchy_On_Dispatching --
   ---------------------------------------------

   function Get_Primitives_Hierarchy_On_Dispatching
     (Context     : Selection_Context;
      Action_Kind : Command_Kind)
      return Entity_Info_Vectors.Vector
   is
      Entities : Entity_Info_Vectors.Vector;

      function Append_Entity (Callee : Root_Entity'Class) return Boolean;

      function Reference_Is_Body_Filter
        (Ref : Root_Entity_Reference'Class) return Boolean
      is
        (Ref.Reference_Is_Body);

      -------------------
      -- Append_Entity --
      -------------------

      function Append_Entity (Callee : Root_Entity'Class) return Boolean
      is
         Target_Location : constant General_Location :=
                             (case Action_Kind is
                                 when Goto_Body => Get_Body (Callee),
                                 when Goto_Spec | Goto_Spec_Or_Body =>
                                   Get_Declaration (Callee).Loc,
                                 when Goto_Type_Decl =>
                                   Get_Declaration (Get_Type_Of (Callee)).Loc);
         Primitive_Of    : Entity_Array := Is_Primitive_Of (Callee);
         Type_Entity     : constant Root_Entity'Class :=
                             Primitive_Of (Primitive_Of'First).all;
      begin
         Entities.Append
           (Entity_Info_Type'
              (Label         => To_Unbounded_String
                   ("<b>" & Escape_Text (Type_Entity.Get_Name) & "."
                    & Escape_Text (Callee.Get_Name) & "</b>"
                    & " in <b>"
                    & Escape_Text (Target_Location.File.Display_Base_Name)
                    & "</b>"),
               Project_Path => Target_Location.Project_Path,
               File         => Target_Location.File,
               Line         => Editable_Line_Type (Target_Location.Line),
               Column       => Target_Location.Column));
         Free (Primitive_Of);

         return True;
      end Append_Entity;

   begin
      Xref.For_Each_Dispatching_Call
        (Ref       => Get_Closest_Ref (Context),
         On_Callee => Append_Entity'Access,
         Filter    =>
           (case Action_Kind is
               when Goto_Body => Reference_Is_Body_Filter'Unrestricted_Access,
               when Goto_Spec | Goto_Spec_Or_Body | Goto_Type_Decl => null));

      return Entities;
   end Get_Primitives_Hierarchy_On_Dispatching;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_MDI_Child_Selected;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : Gtkada.MDI.MDI_Child) is
   begin
      Self.Proposals_Menu.Notes_Window.Destroy;
      Self.Proposals_Menu.Destroy;
   end Execute;

   ----------------------------------------
   -- On_Entity_Proposals_Menu_Key_Press --
   ----------------------------------------

   function On_Entity_Proposals_Menu_Key_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      use Gdk.Types;

      Menu : constant Entity_Proposals_Menu := Entity_Proposals_Menu (Self);
   begin
      if Event.Keyval = GDK_Escape then
         Keyboard_Ungrab;
         Menu.Notes_Window.Destroy;
         Menu.Destroy;

         return True;
      end if;

      return False;
   end On_Entity_Proposals_Menu_Key_Press;

   -----------------------------------
   -- On_Entity_Proposals_Menu_Show --
   -----------------------------------

   procedure On_Entity_Proposals_Menu_Show
     (Self : access Gtk_Widget_Record'Class)
   is
      Menu : constant Entity_Proposals_Menu :=
        Entity_Proposals_Menu (Self);
      Dummy : Gdk_Grab_Status;
   begin
      Grab_Toplevel_Focus
        (Get_MDI (Menu.Kernel), Menu.Tree_View);

      Dummy := Keyboard_Grab (Menu.Get_Window, False);
   end On_Entity_Proposals_Menu_Show;

   --------------------------------------
   -- On_Entity_Proposals_Menu_Destroy --
   --------------------------------------

   procedure On_Entity_Proposals_Menu_Destroy
     (Self : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Keyboard_Ungrab;
   end On_Entity_Proposals_Menu_Destroy;

   ----------------------------
   -- On_Entity_Item_Clicked --
   ----------------------------

   procedure On_Entity_Item_Clicked
     (Self   : access GObject_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : not null access Gtk_Tree_View_Column_Record'Class)
   is
      pragma Unreferenced (Column);

      Menu         : constant Entity_Proposals_Menu := Entity_Proposals_Menu
        (Self);
      Model        : constant Gtk_Tree_Model := Menu.Tree_View.Get_Model;
      Iter         : constant Gtk_Tree_Iter := Get_Iter
        (Menu.Tree_View.Get_Model, Path);
      Label        : constant String := Get_String (Model, Iter, Col_Label);
      Project_Path : constant Virtual_File := Get_File
        (Model, Iter, Col_Project);
      File         : constant Virtual_File := Get_File
        (Model, Iter, Col_File);
      Line         :  constant Gint := Get_Int (Model, Iter, Col_Line);
      Col          :  constant Gint := Get_Int (Model, Iter, Col_Column);
   begin
      Keyboard_Ungrab;
      Menu.Notes_Window.Destroy;
      Menu.Destroy;

      Go_To_Closest_Match
        (Kernel                      => Menu.Kernel,
         Filename                    => File,
         Project                     => Get_Registry
           (Menu.Kernel).Tree.Project_From_Path (Project_Path),
         Line                        => Editable_Line_Type (Line),
         Column                      => Visible_Column_Type (Col),
         Entity_Name                 => Label,
         Display_Msg_On_Non_Accurate => False);

   exception
      when E : others =>
         Trace (Me_Advanced, Exception_Message (E));
         Trace (Me_Advanced, "Exception caught while clicking on entity:");
         Increase_Indent (Me);
         Trace (Me_Advanced, "file:" & File.Display_Base_Name);
         Trace (Me_Advanced, "label:" & Label);
         Trace (Me_Advanced, "line:" & Line'Img);
         Trace (Me_Advanced, "column:" & Line'Img);
         Decrease_Indent (Me);
   end On_Entity_Item_Clicked;

   -----------------------------
   -- On_Entity_Item_Selected --
   -----------------------------

   procedure On_Entity_Item_Selected
     (Self  : access GObject_Record'Class)
   is
      Menu         : constant Entity_Proposals_Menu :=
                       Entity_Proposals_Menu (Self);
      Notes_Window : Gtk_Scrolled_Window renames Menu.Notes_Window;
      Model        : Gtk_Tree_Model;
      Iter         : Gtk_Tree_Iter;
   begin
      Get_Selected (Menu.Tree_View.Get_Selection, Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         File         : constant Virtual_File := Get_File
           (Model, Iter, Col_File);
         Line         :  constant Gint := Get_Int (Model, Iter, Col_Line);
         Col          :  constant Gint := Get_Int (Model, Iter, Col_Column);
      begin
         Remove_All_Children (Notes_Window);

         Notes_Window.Add
           (GPS.LSP_Client.Editors.Tooltips.Query_Tooltip_For_Entity
              (Kernel              => Menu.Kernel,
               File                => File,
               Line                => Integer (Line),
               Column              => Visible_Column_Type (Col),
               For_Global_Tooltips => False));

         Notes_Window.Show_All;
      end;

   exception
      when E : others =>
         Trace (Me_Advanced, Exception_Message (E));
   end On_Entity_Item_Selected;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle)
   is
      Has_Entity_Name_Filter : constant Action_Filter :=
                                 Lookup_Filter (Kernel, "Has entity name");
   begin
      --  Register the navigation actions and preferences based on the LSP

      Register_Action
        (Kernel, "goto declaration",
         Command      => new Goto_Command_Type'
           (Root_Command with Action_Kind => Goto_Spec),
         Description  => "Jump to the declaration of the current entity",
         Category     => "Editor",
         For_Learning => False,
         Filter       => Has_Entity_Name_Filter);

      Register_Action
        (Kernel, "goto body",
         Command      => new Goto_Command_Type'
           (Root_Command with Action_Kind => Goto_Body),
         Description  =>
           "Jump to the implementation/body of the current entity",
         Category     => "Editor",
         For_Learning => False,
         Filter       => Has_Entity_Name_Filter);

      Register_Action
        (Kernel, "goto declaration or body",
         Command      => new Goto_Command_Type'
           (Root_Command with Action_Kind => Goto_Spec_Or_Body),
         Description  => "Jump to the declaration or to the body of the "
         & "current entity depending on the context",
         Category     => "Editor",
         For_Learning => False,
         Filter       => Has_Entity_Name_Filter);

      Register_Action
        (Kernel, "goto type of entity",
         Command      => new Goto_Command_Type'
           (Root_Command with Action_Kind => Goto_Type_Decl),
         Description  => "Jump to the declaration for the type of the entity",
         Category     => "Editor",
         For_Learning => False,
         Filter       => Has_Entity_Name_Filter);

      Display_Ancestry_On_Navigation_Pref :=
        Display_Ancestry_On_Navigation_Prefs.Create
          (Manager  => Kernel.Get_Preferences,
           Path     => "Editor/Ada:Navigation",
           Name     => "display-ancestry-on-navigation",
           Label    => "Display ancestry on navigation",
           Doc      => "Controls the policy regarding the listing of the "
           & "subprogram ancestry when executing navigation requests on "
           & "subprograms (e.g : when ctrl-clicking on a subprogram "
           & "declaration).",
           Default  => LSP.Messages.Usage_And_Abstract_Only);

      --  Register the hyper mode click callback based on the LSP

      Src_Editor_Module.Set_Hyper_Mode_Click_Callback
        (LSP_Hyper_Mode_Click_Callback'Access);
   end Register_Module;

end GPS.LSP_Client.Editors.Navigation;
