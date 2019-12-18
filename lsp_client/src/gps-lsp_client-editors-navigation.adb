------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2020, AdaCore                  --
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
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;      use GNATCOLL.VFS.GtkAda;
with GNATCOLL.Xref;

with Gdk.Device;               use Gdk.Device;
with Gdk.Event;                use Gdk.Event;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Window;               use Gdk.Window;
with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib;                     use Glib;
with Glib_Values_Utils;        use Glib_Values_Utils;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Kernel.Xref;          use GPS.Kernel.Xref;

with GPS.LSP_Client.Editors.Tooltips;
with GPS.LSP_Client.Requests.Definition;
use GPS.LSP_Client.Requests.Definition;
with GPS.LSP_Client.Requests;  use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Utilities; use GPS.LSP_Client.Utilities;
with GPS.LSP_Module;           use GPS.LSP_Module;

with Basic_Types;              use Basic_Types;
with Commands.Interactive;     use Commands.Interactive;
with Commands;                 use Commands;
with GUI_Utils;                use GUI_Utils;
with Language;                 use Language;
with LSP.Types;                use LSP.Types;
with Src_Editor_Box;           use Src_Editor_Box;
with Src_Editor_Module;        use Src_Editor_Module;
with Xref;                     use Xref;

package body GPS.LSP_Client.Editors.Navigation is

   Me : constant Trace_Handle := Create
     ("GPS.LSP.NAVIGATION", GNATCOLL.Traces.On);

   --------------------------------------------------------
   -- LSP textDocument/definition Request Implementation --
   --------------------------------------------------------

   type GPS_LSP_Definition_Request is
     new Abstract_Definition_Request with record
      Kernel      : Kernel_Handle;
      Entity_Name : Unbounded_String;
      Root_X      : Gint := -1;
      Root_Y      : Gint := -1;
   end record;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Definition_Request;
      Result : LSP.Messages.Location_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Definition_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Definition_Request);

   ------------------------------------------------------------
   -- LSP textDocument/typeDefinition Request Implementation --
   ------------------------------------------------------------

   type GPS_LSP_Type_Definition_Request is
     new Abstract_Type_Definition_Request with record
      Kernel : Kernel_Handle;
   end record;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Type_Definition_Request;
      Result : LSP.Messages.Location_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Type_Definition_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Type_Definition_Request);

   ------------------------------------------------------------
   -- LSP textDocument/implementation Request Implementation --
   ------------------------------------------------------------

   type GPS_LSP_Implementation_Request is
     new Abstract_Implementation_Request with record
      Kernel : Kernel_Handle;
      Entity_Name : Unbounded_String;
      Root_X      : Gint := -1;
      Root_Y      : Gint := -1;
   end record;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Implementation_Request;
      Result : LSP.Messages.Location_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Implementation_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Implementation_Request);

   --------------------------------------------------------------------
   -- Goto Declaration/Body/Type Declaration Commands and Hyper Mode --
   --------------------------------------------------------------------

   type Goto_Action_Command_Kind is (Goto_Body, Goto_Spec, Goto_Type_Decl);
   type Goto_Command_Type is new Interactive_Command with record
      Action_Kind : Goto_Action_Command_Kind;
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
   --  locations in response to textDocument/definition.

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
      Action_Kind : Goto_Action_Command_Kind)
      return Entity_Info_Vectors.Vector;
   --  When the user's cursor is on a dispatching call, return all the
   --  declarations/bodies in the hierarchy for this primitive.

   procedure Display_Menu_For_Entities_Proposals
     (Kernel   : not null Kernel_Handle;
      Entities : Entity_Info_Vectors.Vector;
      Root_X   : Gint := -1;
      Root_Y   : Gint := -1);
   --  Display a menu with all the listed entity proposals.
   --  When specified, Root_X and Root_Y are used to position the menu: the
   --  current pointer's position is used otherwise.

   function On_Entity_Proposals_Menu_Focus_Out
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean;
   --  Called when the focus leaves the entity proposals menu.
   --  Close the menu in this case.

   function On_Entity_Proposals_Menu_Key_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Called when the users presses a key in the entity proposals menu.
   --  Close the menu if the ESC key was pressed.

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

   procedure LSP_Hyper_Mode_Click_Callback
     (Kernel      : not null Kernel_Handle;
      Buffer      : GPS.Editors.Editor_Buffer'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Line        : Editable_Line_Type;
      Column      : Visible_Column_Type;
      Entity_Name : String;
      Alternate   : Boolean;
      Root_X      : Gint;
      Root_Y      : Gint);
   --  The hyper mode click callback based on the LSP.
   --  When the LSP is disabled for the buffer's language, defaults to the
   --  default behavior based on the old xref engine.

   procedure On_Result
     (Kernel      : not null Kernel_Handle;
      Result      : LSP.Messages.Location_Vector;
      Entity_Name : String;
      Root_X      : Gint := -1;
      Root_Y      : Gint := -1);
   --  Called when receiving a textDocument/definition result.
   --  Go to the match if the result contains only one location and display
   --  a menu at (Root_X, Root_Y) when the result contains several locations.

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Goto_Command_Type;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use GNATCOLL.Xref;

      Kernel    : constant Kernel_Handle := Get_Kernel (Context.Context);
      File      : constant Virtual_File := File_Information (Context.Context);
      Project   : constant Project_Type := Get_Project_For_File
        (Kernel.Get_Project_Tree, File => File);
      Editor    : constant Source_Editor_Box :=
                    Get_Source_Box_From_MDI
                      (Find_Editor
                         (Kernel,
                          File    => File,
                          Project => Project));
      Lang      : constant Language_Access :=
                    Kernel.Get_Language_Handler.Get_Language_By_Name
                      (Get_File_Language (Context.Context));
      Line      : constant Integer := Line_Information (Context.Context);
      Column    : constant Visible_Column_Type := Column_Information
        (Context.Context);
      Request   : Request_Access;
      Root_X    : Gint;
      Root_Y    : Gint;
   begin
      if LSP_Is_Enabled (Lang) then
         case Command.Action_Kind is
            when Goto_Spec | Goto_Body =>
               --  Get the root coordinates of the current editor location.
               --  This is used to display a popup menu at this position if
               --  textDocument/definition returns multiple proposals.
               --  We add +1 to the current line to display the menu under
               --  the entity, instead of above it.

               Editor.Get_View.Get_Root_Coords_For_Location
                 (Line   => Editable_Line_Type (Line) + 1,
                  Column => Column,
                  Root_X => Root_X,
                  Root_Y => Root_Y);

               if Command.Action_Kind = Goto_Spec then
                  Trace (Me, "Executing the textDocument/definition request");
                  Request := new GPS_LSP_Definition_Request'
                    (LSP_Request with
                     Text_Document   => File_Information (Context.Context),
                     Line            => Line_Information (Context.Context),
                     Column          => Column_Information (Context.Context),
                     Kernel          => Get_Kernel (Context.Context),
                     Entity_Name     => To_Unbounded_String
                       (Entity_Name_Information (Context.Context)),
                     Root_X          => Root_X,
                     Root_Y          => Root_Y);
               else
                  Trace (Me,
                         "Executing the textDocument/implementation request");
                  Request := new GPS_LSP_Implementation_Request'
                    (LSP_Request with
                     Text_Document   => File_Information (Context.Context),
                     Line            => Line_Information (Context.Context),
                     Column          => Column_Information (Context.Context),
                     Kernel          => Get_Kernel (Context.Context),
                     Entity_Name     => To_Unbounded_String
                       (Entity_Name_Information (Context.Context)),
                     Root_X          => Root_X,
                     Root_Y          => Root_Y);
               end if;

            when Goto_Type_Decl =>
               Trace (Me, "Executing the textDocument/typeDefinition request");

               Request := new GPS_LSP_Type_Definition_Request'
                 (LSP_Request with
                  Text_Document   => File_Information (Context.Context),
                  Line            => Line_Information (Context.Context),
                  Column          => Column_Information (Context.Context),
                  Kernel          => Get_Kernel (Context.Context));
         end case;

         Editor.Set_Activity_Progress_Bar_Visibility (True);
         GPS.LSP_Client.Requests.Execute
           (Language => Lang,
            Request  => Request_Access (Request));

      elsif Is_Dispatching (Context.Context) then
         Display_Menu_For_Entities_Proposals
           (Kernel   => Kernel,
            Entities => Get_Primitives_Hierarchy_On_Dispatching
              (Context     => Context.Context,
               Action_Kind => Command.Action_Kind));
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
               when Goto_Spec =>
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
      Alternate   : Boolean;
      Root_X      : Gint;
      Root_Y      : Gint)
   is
      Request : Request_Access;
   begin
      if LSP_Is_Enabled (Buffer.Get_Language) then
         declare
            File   : constant Virtual_File := Buffer.File;
            Editor : constant Source_Editor_Box :=
                       Get_Source_Box_From_MDI
                         (Find_Editor
                            (Kernel,
                             File    => File,
                             Project => Project));
         begin
            if Alternate then
               Trace (Me, "Executing the textDocument/implementation request");
               Request := new GPS_LSP_Implementation_Request'
                 (LSP_Request with
                  Text_Document   => File,
                  Line            => Positive (Line),
                  Column          => Column,
                  Kernel          => Kernel,
                  Entity_Name     => To_Unbounded_String (Entity_Name),
                  Root_X          => Root_X,
                  Root_Y          => Root_Y);
            else
               Trace (Me, "Executing the textDocument/definition request");
               Request := new GPS_LSP_Definition_Request'
                 (LSP_Request with
                  Text_Document   => File,
                  Line            => Positive (Line),
                  Column          => Column,
                  Kernel          => Kernel,
                  Entity_Name     => To_Unbounded_String (Entity_Name),
                  Root_X          => Root_X,
                  Root_Y          => Root_Y);
            end if;

            Editor.Set_Activity_Progress_Bar_Visibility (True);

            GPS.LSP_Client.Requests.Execute
              (Language => Buffer.Get_Language,
               Request  => Request_Access (Request));
         end;
      else
         Src_Editor_Module.Default_Hyper_Mode_Click_Callback
           (Kernel      => Kernel,
            Buffer      => Buffer,
            Project     => Project,
            Line        => Line,
            Column      => Column,
            Entity_Name => Entity_Name,
            Alternate   => Alternate,
            Root_X      => Root_X,
            Root_Y      => Root_Y);
      end if;
   end LSP_Hyper_Mode_Click_Callback;

   ---------------
   -- On_Result --
   ---------------

   procedure On_Result
     (Kernel      : not null Kernel_Handle;
      Result      : LSP.Messages.Location_Vector;
      Entity_Name : String;
      Root_X      : Gint := -1;
      Root_Y      : Gint := -1)
   is
      use type Ada.Containers.Count_Type;
   begin
      Trace (Me, "Result received");

      if Result.Is_Empty then
         Trace (Me, "No locations found");
         return;
      end if;

      --  If we have only one location in the result, go to it.
      --  Otherwise, display all the location proposals in a contextual
      --  menu. This can happen when ctrl-clicking on dispatching calls for
      --  instance.

      if Result.Length = 1 then
         declare
            Location     : constant LSP.Messages.Location :=
                             Result.First_Element;
            File         : constant Virtual_File := To_Virtual_File
              (Location.uri);
            Infos        : constant File_Info_Set := Get_Registry
              (Kernel).Tree.Info_Set (File);
            Project      : constant Project_Type :=
                             File_Info'Class (Infos.First_Element).Project
                             (True);
         begin
            --  Go the closest match of the returned location.
            --  Don't forget to add 1 to both line and column numbers since LSP
            --  lines/columns are zero-based.

            Go_To_Closest_Match
              (Kernel                      => Kernel,
               Filename                    => File,
               Project                     => Project,
               Line                        => Editable_Line_Type
                 (Location.span.first.line + 1),
               Column                      =>
                 GPS.LSP_Client.Utilities.UTF_16_Offset_To_Visible_Column
                   (Location.span.first.character),
               Entity_Name                 => Entity_Name,
               Display_Msg_On_Non_Accurate => False);
         end;
      else
         declare
            Entities : Entity_Info_Vectors.Vector;
         begin
            for Location of Result loop
               declare
                  File    : constant Virtual_File := To_Virtual_File
                    (Location.uri);
                  Infos   : constant File_Info_Set := Get_Registry
                    (Kernel).Tree.Info_Set (File);
                  Project : constant Project_Type :=
                              File_Info'Class (Infos.First_Element).Project
                              (True);
               begin
                  Entities.Append
                    (Entity_Info_Type'
                       (Label        => To_Unbounded_String
                            ("<b>" & Entity_Name & "</b>"
                             & " in <b>"
                             & Escape_Text (File.Display_Base_Name)
                             & "</b>"),
                        Project_Path => Project.Project_Path,
                        File         => File,
                        Line         => Editable_Line_Type
                          (Location.span.first.line + 1),
                        Column       => UTF_16_Offset_To_Visible_Column
                          (Location.span.first.character)));

               end;
            end loop;

            Display_Menu_For_Entities_Proposals
              (Kernel   => Kernel,
               Entities => Entities,
               Root_X   => Root_X,
               Root_Y   => Root_Y);
         end;
      end if;
   end On_Result;

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
                        Project => Project));
   begin
      if Editor /= null then
         Editor.Set_Activity_Progress_Bar_Visibility (False);
      end if;
   end Cancel_Activity_Bar;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Definition_Request;
      Result : LSP.Messages.Location_Vector) is
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.Text_Document);

      On_Result
        (Kernel      => Self.Kernel,
         Result      => Result,
         Entity_Name => To_String (Self.Entity_Name),
         Root_X      => Self.Root_X,
         Root_Y      => Self.Root_Y);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Definition_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.Text_Document);
      Trace
        (Me, "Error received after sending textDocument/definition request");
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Definition_Request) is
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.Text_Document);
      Trace (Me, "textDocument/definition request has been rejected");
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Implementation_Request;
      Result : LSP.Messages.Location_Vector) is
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.Text_Document);

      On_Result
        (Kernel      => Self.Kernel,
         Result      => Result,
         Entity_Name => To_String (Self.Entity_Name),
         Root_X      => Self.Root_X,
         Root_Y      => Self.Root_Y);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Implementation_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Code, Message, Data);
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.Text_Document);
      Trace
        (Me,
         "Error received after sending textDocument/implementation request");
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Implementation_Request) is
   begin
      Cancel_Activity_Bar (Self.Kernel, Self.Text_Document);

      Trace (Me, "textDocument/implementation request has been rejected");
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Type_Definition_Request;
      Result : LSP.Messages.Location_Vector) is
   begin
      On_Result
        (Kernel      => Self.Kernel,
         Result      => Result,
         Entity_Name => "");
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Type_Definition_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Self, Code, Message, Data);
   begin
      Trace
        (Me,
         "Error received after sending textDocument/typeDefinition request");
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Type_Definition_Request)
   is
      pragma Unreferenced (Self);
   begin
      Trace (Me, "textDocument/typeDefinition request has been rejected");
   end On_Rejected;

   -----------------------------------------
   -- Display_Menu_For_Entities_Proposals --
   -----------------------------------------

   procedure Display_Menu_For_Entities_Proposals
     (Kernel   : not null Kernel_Handle;
      Entities : Entity_Info_Vectors.Vector;
      Root_X        : Gint := -1;
      Root_Y        : Gint := -1)
   is
      Proposals_Menu : Entity_Proposals_Menu;
      Hbox           : Gtk_Hbox;
      Scrolled       : Gtk_Scrolled_Window;
      Sep            : Gtk_Hseparator;
      Model          : Gtk_Tree_Store;
      Iter           : Gtk_Tree_Iter;
   begin
      --  Create the menu's window.
      --  We can't use Gtk_Menu widgets here because they are modal, and we
      --  want the user to be able to scroll in the side notes window to
      --  see the hover text corresponding to a given proposal (a kind of
      --  preview).

      Proposals_Menu := new Entity_Proposals_Menu_Record;
      Proposals_Menu.Kernel := Kernel;

      Gtk.Window.Initialize (Proposals_Menu,  Window_Toplevel);
      Proposals_Menu.Set_Type_Hint (Window_Type_Hint_Combo);
      Proposals_Menu.Set_Resizable (False);
      Proposals_Menu.Set_Skip_Taskbar_Hint (True);
      Proposals_Menu.Set_Skip_Pager_Hint (True);
      Proposals_Menu.Set_Name ("entity-proposals-menu");
      Get_Style_Context (Proposals_Menu).Add_Class ("menu");

      Proposals_Menu.On_Focus_Out_Event
        (On_Entity_Proposals_Menu_Focus_Out'Access);
      Proposals_Menu.On_Key_Press_Event
        (On_Entity_Proposals_Menu_Key_Press'Access);

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

      --  Display the menu's window

      Proposals_Menu.Notes_Window.Set_Size_Request (500, 150);
      Proposals_Menu.Move (Root_X, Root_Y);
      Proposals_Menu.Show_All;

      --  Let the tree view grab the focus
      Grab_Toplevel_Focus (Get_MDI (Kernel), Proposals_Menu.Tree_View);
   end Display_Menu_For_Entities_Proposals;

   ---------------------------------------------
   -- Get_Primitives_Hierarchy_On_Dispatching --
   ---------------------------------------------

   function Get_Primitives_Hierarchy_On_Dispatching
     (Context     : Selection_Context;
      Action_Kind : Goto_Action_Command_Kind)
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
                                 when Goto_Spec =>
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
               when Goto_Spec | Goto_Type_Decl => null));

      return Entities;
   end Get_Primitives_Hierarchy_On_Dispatching;

   ----------------------------------------
   -- On_Entity_Proposals_Menu_Focus_Out --
   ----------------------------------------

   function On_Entity_Proposals_Menu_Focus_Out
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      Menu : constant Entity_Proposals_Menu := Entity_Proposals_Menu (Self);
   begin
      Menu.Notes_Window.Destroy;
      Menu.Destroy;

      return True;
   end On_Entity_Proposals_Menu_Focus_Out;

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
         Menu.Notes_Window.Destroy;
         Menu.Destroy;

         return True;
      end if;

      return False;
   end On_Entity_Proposals_Menu_Key_Press;

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
         Trace (Me, Exception_Message (E));
         Trace (Me, "Exception caught while clicking on entity:");
         Increase_Indent (Me);
         Trace (Me, "file:" & File.Display_Base_Name);
         Trace (Me, "label:" & Label);
         Trace (Me, "line:" & Line'Img);
         Trace (Me, "column:" & Line'Img);
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
         Trace (Me, Exception_Message (E));
   end On_Entity_Item_Selected;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle)
   is
      Has_Entity_Name_Filter : constant Action_Filter :=
                                 Lookup_Filter (Kernel, "Has entity name");
   begin
      --  Register the navigation actions based on the LSP

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
        (Kernel, "goto type of entity",
         Command      => new Goto_Command_Type'
           (Root_Command with Action_Kind => Goto_Type_Decl),
         Description  => "Jump to the declaration for the type of the entity",
         Category     => "Editor",
         For_Learning => False,
         Filter       => Has_Entity_Name_Filter);

      --  Register the hyper mode click callback based on the LSP

      Src_Editor_Module.Set_Hyper_Mode_Click_Callback
        (LSP_Hyper_Mode_Click_Callback'Access);
   end Register_Module;

end GPS.LSP_Client.Editors.Navigation;
