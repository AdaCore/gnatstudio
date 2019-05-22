------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Xref;

with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib;                     use Glib;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Widget;               use Gtk.Widget;

with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Kernel.Xref;          use GPS.Kernel.Xref;
with GPS.LSP_Module;           use GPS.LSP_Module;
with GPS.LSP_Client.Requests;  use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Definition;
use GPS.LSP_Client.Requests.Definition;
with GPS.LSP_Client.Utilities; use GPS.LSP_Client.Utilities;

with Basic_Types;              use Basic_Types;
with Commands.Interactive;     use Commands.Interactive;
with Commands;                 use Commands;
with Language;                 use Language;
with LSP.Types;                use LSP.Types;
with Src_Editor_Box;           use Src_Editor_Box;
with Src_Editor_Module;
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
   end record;
   type GPS_LSP_Definition_Request_Access is
     access all GPS_LSP_Definition_Request'Class;

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

   ---------------------------------------------------
   -- Goto Declaration/Body Commands and Hyper Mode --
   ---------------------------------------------------

   type Goto_Action_Command_Kind is (Goto_Body, Goto_Spec);
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

   package Entity_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Entity_Info_Type,
      "="          => "=");

   type Entity_Info_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Kernel : Kernel_Handle;
      Entity : Entity_Info_Type;
   end record;
   type Entity_Info_Menu_Item is access all Entity_Info_Menu_Item_Record'Class;

   function Get_Primitives_Hierarchy_On_Dispatching
     (Context     : Selection_Context;
      Action_Kind : Goto_Action_Command_Kind)
      return Entity_Info_Vectors.Vector;
   --  When the user's cursor is on a dispatching call, return all the
   --  declarations/bodies in the hierarchy for this primitive.

   procedure Display_Menu_For_Entities_Proposals
     (Kernel   : not null Kernel_Handle;
      Entities : Entity_Info_Vectors.Vector);
   --  Display a contextual menu with all the listed entity proposals.

   procedure On_Entity_Item_Clicked
     (Self : access Gtk_Menu_Item_Record'Class);
   --  Called when clicking on an entity poposal menu item.

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

      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      File    : constant Virtual_File := File_Information (Context.Context);
      Lang    : constant Language_Access :=
                  Kernel.Get_Language_Handler.Get_Language_By_Name
                    (Get_File_Language (Context.Context));
      Request : GPS_LSP_Definition_Request_Access;
   begin
      if LSP_Is_Enabled (Lang) then

         Request := new GPS_LSP_Definition_Request'
           (LSP_Request with
            Text_Document   => File_Information (Context.Context),
            Line            => Line_Information (Context.Context),
            Column          => Column_Information (Context.Context),
            Kernel          => Get_Kernel (Context.Context),
            Entity_Name     => To_Unbounded_String
              (Entity_Name_Information (Context.Context)));

         Trace (Me, "Executing the textDocument/definition request");
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
            end case;

            if Location /= No_Location then
               Go_To_Closest_Match
                 (Kernel      => Kernel,
                  Filename    => Location.File,
                  Project     => Get_Project (Location),
                  Line        => Editable_Line_Type (Location.Line),
                  Column      => Location.Column,
                  Entity_Name => Get_Name (Entity));
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
      Alternate   : Boolean)
   is
      Request : GPS_LSP_Definition_Request_Access;
   begin
      if LSP_Is_Enabled (Buffer.Get_Language) then
         Trace (Me, "Executing the textDocument/definition request");

         Request := new GPS_LSP_Definition_Request'
           (LSP_Request with
            Text_Document   => Buffer.File,
            Line            => Positive (Line),
            Column          => Column,
            Kernel          => Kernel,
            Entity_Name     => To_Unbounded_String (Entity_Name));

         GPS.LSP_Client.Requests.Execute
           (Language => Buffer.Get_Language,
            Request  => Request_Access (Request));
      else
         Src_Editor_Module.Default_Hyper_Mode_Click_Callback
           (Kernel      => Kernel,
            Buffer      => Buffer,
            Project     => Project,
            Line        => Line,
            Column      => Column,
            Entity_Name => Entity_Name,
            Alternate   => Alternate);
      end if;
   end LSP_Hyper_Mode_Click_Callback;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Definition_Request;
      Result : LSP.Messages.Location_Vector) is
   begin
      Trace (Me, "Result received");

      if Result.Is_Empty then
         Trace (Me, "No locations found");
         return;
      end if;

      --  TODO: handle the case where multiple locations are returned by
      --  displaying a contextual menu with all the proposals

      declare
         Loc     : constant LSP.Messages.Location := Result.First_Element;
         File    : constant Virtual_File := To_Virtual_File (Loc.uri);
         Infos   : constant File_Info_Set := Get_Registry
           (Self.Kernel).Tree.Info_Set (File);
         Project : constant Project_Type :=
                     File_Info'Class (Infos.First_Element).Project (True);
      begin
         --  Go the closest match of the returned location.
         --  Don't forget to add 1 to both line and column numbers since LSP
         --  lines/columns are zero-based.

         Go_To_Closest_Match
           (Kernel      => Self.Kernel,
            Filename    => File,
            Project     => Project,
            Line        => Editable_Line_Type (Loc.span.first.line + 1),
            Column      => Visible_Column_Type (Loc.span.first.character + 1),
            Entity_Name => To_String (Self.Entity_Name));
      end;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Definition_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Self, Code, Message, Data);
   begin
      Trace
        (Me, "Error received after sending textDocument/definition request");
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Definition_Request)
   is
      pragma Unreferenced (Self);
   begin
      Trace (Me, "textDocument/definition request has been rejected");
   end On_Rejected;

   -----------------------------------------
   -- Display_Menu_For_Entities_Proposals --
   -----------------------------------------

   procedure Display_Menu_For_Entities_Proposals
     (Kernel   : not null Kernel_Handle;
      Entities : Entity_Info_Vectors.Vector)
   is
      Menu : Gtk_Menu;
      Item : Entity_Info_Menu_Item;
   begin
      Gtk_New (Menu);
      Menu.Set_Name ("dispatching proposals menu");

      for Entity of Entities loop
         Item := new Entity_Info_Menu_Item_Record'
           (GObject_Record with
            Kernel => Kernel,
            Entity => Entity);
         Initialize (Item, To_String (Entity.Label));
         Gtk_Label (Item.Get_Child).Set_Use_Markup (True);
         Item.On_Activate (On_Entity_Item_Clicked'Access);
         Menu.Append (Item);
      end loop;

      Menu.Show_All;
      Popup_Custom_Contextual_Menu
        (Menu   => Menu,
         Kernel => Kernel);

      Menu.Select_First (True);
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
                                   Get_Declaration (Callee).Loc);
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
               when Goto_Spec => null));

      return Entities;
   end Get_Primitives_Hierarchy_On_Dispatching;

   ----------------------------
   -- On_Entity_Item_Clicked --
   ----------------------------

   procedure On_Entity_Item_Clicked
     (Self : access Gtk_Menu_Item_Record'Class)
   is
      Item   : constant Entity_Info_Menu_Item := Entity_Info_Menu_Item (Self);
      Entity : constant Entity_Info_Type := Item.Entity;
   begin
      Go_To_Closest_Match
        (Kernel      => Item.Kernel,
         Filename    => Entity.File,
         Project     => Get_Registry (Item.Kernel).Tree.Project_From_Path
         (Path => Entity.Project_Path),
         Line        => Entity.Line,
         Column      => Entity.Column,
         Entity_Name => To_String (Entity.Label));
   end On_Entity_Item_Clicked;

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

      --  Register the hyper mode click callback based on the LSP

      Src_Editor_Module.Set_Hyper_Mode_Click_Callback
        (LSP_Hyper_Mode_Click_Callback'Access);
   end Register_Module;

end GPS.LSP_Client.Editors.Navigation;
