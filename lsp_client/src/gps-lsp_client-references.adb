------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

--  Here are items which are needed from Language Server:
--    - Entity name
--    - Reference_Kind
--    - Caller
--    - List of real reference kinds for an entity
--    - all entities in project/file

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;               use GNAT.Strings;

with GNATCOLL.Scripts;
with GNATCOLL.Utils;
with GNATCOLL.VFS;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Radio_Button;           use Gtk.Radio_Button;
with Gtk.Stock;
with Gtk.Vbutton_Box;            use Gtk.Vbutton_Box;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Handlers;            use Gtkada.Handlers;

with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Entities;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;
with GPS.Location_View;
with GPS.LSP_Module;
with GPS.LSP_Client.Utilities;
with GPS.Scripts.Commands;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;
with Histories;
with Language.Ada;
with Src_Editor_Module.Shell;

with Basic_Types;
with LSP.Messages;
with LSP.Types;
with GPS.LSP_Client.Requests.References;

package body GPS.LSP_Client.References is

   type Find_Refs_Command (Locals_Only : Boolean; Specific : Boolean) is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Find references command which calls LSP or old implementation.

   type Has_Entity_Name_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Entity_Name_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity is an access type.

   type Result_Filter is record
      Ref_Kinds : GNAT.Strings.String_List_Access;
      --  The reference kinds' name that should be displayed, or none for all.
      --  Any null value is ignored in this array.

--  will be used when we have references kinds
      --  Filter    : Reference_Kind_Filter;
      --  One of the predefined filters
   end record;
   --  Will be used for filtering results

   -- References_Command --

   type References_Command is
     new Abstract_References_Command with record
      Locations : LSP.Messages.Location_Vector;
   end record;
   type Ref_Command_Access is access all References_Command'Class;
   --  Used to transfer references lists via python API

   overriding function Execute
     (Command : access References_Command)
      return Command_Return_Type is (Failure);

   overriding procedure Get_Result
     (Self : not null access References_Command;
      Data : in out GNATCOLL.Scripts.Callback_Data'Class);

   -- References_Request --

   type References_Request is
     new GPS.LSP_Client.Requests.References.Abstract_References_Request with
      record
         Kernel      : Kernel_Handle;
         Title       : Unbounded_String;
         Name        : Unbounded_String;
         From_File   : GNATCOLL.VFS.Virtual_File;
         Show_Caller : Boolean := False;
         Filter      : Result_Filter;
         Command     : Ref_Command_Access;
      end record;
   type References_Request_Access is access all References_Request;
   --  Used for communicate with LSP

   overriding procedure Finalize (Self : in out References_Request);

   overriding procedure On_Result_Message
     (Self   : in out References_Request;
      Result : LSP.Messages.Location_Vector);

   -- Others --

   function All_Refs_Category
     (Entity             : String;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean)
      return String;
   --  Return a title for the Locations view

   type Filters_Buttons is array (Natural range <>) of Gtk_Check_Button;
   type Filters_Buttons_Access is access Filters_Buttons;
   type References_Filter_Dialog_Record is new Gtk_Dialog_Record with record
      Filters : Filters_Buttons_Access;
   end record;
   type References_Filter_Dialog is access all
     References_Filter_Dialog_Record'Class;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Filters_Buttons, Filters_Buttons_Access);

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   --  Select or unselect all filters in "Find references..."

   procedure Find_All_Refs
     (Kernel   : Kernel_Handle;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Integer;
      Column   : Basic_Types.Visible_Column_Type;
      Name     : String;
      Implicit : Boolean;
      In_File  : GNATCOLL.VFS.Virtual_File;
      Data     : GNATCOLL.Scripts.Callback_Data_Access);
   --  Implements GPS.EditorBuffer.find_all_refs and
   --  GPS.EditorBuffer.references python API

   Message_Flag : constant Message_Flags :=
     (Editor_Side => True,
      Editor_Line => False,
      Locations   => True);

   -----------------------
   -- All_Refs_Category --
   -----------------------

   function All_Refs_Category
     (Entity             : String;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean)
      return String is
   begin
      if All_From_Same_File then
         return "Entities imported into " &
           GNATCOLL.VFS."+"(Local_File.Base_Name);

      elsif Local_Only then
         return "Local references for " & Entity & " in " &
           GNATCOLL.VFS."+"(Local_File.Base_Name);

      else
         return "References for " & Entity;
      end if;
   end All_Refs_Category;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Lang   : Standard.Language.Language_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Title  : Unbounded_String;
   begin
      File := File_Information (Context.Context);
      Lang := Kernel.Get_Language_Handler.Get_Language_From_File (File);

      if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
         Title := To_Unbounded_String
           (All_Refs_Category
              (Entity_Name_Information (Context.Context),
               Command.Locals_Only,
               File,
               Command.Specific));

         if Command.Specific then
            declare
               Dialog             : References_Filter_Dialog;
               Box                : Gtk_Box;
               Col                : array (1 .. 2) of Gtk_Box;
               Filter_Box         : Gtk_Vbutton_Box;
               Index              : Integer := Col'First;
               Project_And_Recursive,
               File_Only          : Gtk_Radio_Button;
               Show_Caller        : Gtk_Check_Button;
--  will be used when we have all entities from the file
--                 From_Same_File     : Gtk_Radio_Button;
               Include_Overriding : Gtk_Check_Button;
               Frame              : Gtk_Frame;
               Ignore             : Gtk_Widget;
               Button             : Gtk_Button;

--  will be used when we have references kinds
               --  All_Refs : GNAT.Strings.String_List :=
               --    Kernel.Databases.All_Real_Reference_Kinds;
               All_Refs : GNAT.Strings.String_List (1 .. 6) :=
                 (new String'("Read"),
                  new String'("Write"),
                  new String'("Read_Or_Write"),
                  new String'("Implicit reference"),
                  new String'("Dispatching_Call"),
                  new String'("Reference"));

            begin
               Dialog := new References_Filter_Dialog_Record;
               Dialog.Filters := new Filters_Buttons (All_Refs'Range);

               Initialize
                 (Dialog,
                  Title  => "Find References Options",
                  Parent => Kernel.Get_Main_Window,
                  Flags  => Modal
                  or Use_Header_Bar_From_Settings (Kernel.Get_Main_Window));

               --  Context choice

               Gtk_New (Frame, "Context");
               Pack_Start (Get_Content_Area (Dialog), Frame);
               Gtk_New_Vbox (Box, Homogeneous => True);
               Add (Frame, Box);

               Gtk_New (Project_And_Recursive, Widget_SList.Null_List,
                        "In all projects");
               Pack_Start (Box, Project_And_Recursive);
               Histories.Create_New_Boolean_Key_If_Necessary
                 (Get_History (Kernel).all,
                  "Find_Prefs_Project_Recursive",
                  True);
               Histories.Associate
                 (Get_History (Kernel).all,
                  "Find_Prefs_Project_Recursive",
                  Project_And_Recursive);

               Gtk_New (File_Only, Get_Group (Project_And_Recursive),
                        "In current file");
               Pack_Start (Box, File_Only);
               Histories.Create_New_Boolean_Key_If_Necessary
                 (Get_History (Kernel).all, "Find_Prefs_File_Only", False);
               Histories.Associate
                 (Get_History (Kernel).all, "Find_Prefs_File_Only", File_Only);

--  will be used when we all entities from the file
--                 Gtk_New
--                   (From_Same_File, Get_Group (Project_And_Recursive),
--                    "All entities imported from same file");
--                 Pack_Start (Box, From_Same_File);
--                 Histories.Create_New_Boolean_Key_If_Necessary
--                   (Get_History (Kernel).all,
--                    "Find_Prefs_From_Same_File",
--                    False);
--                 Histories.Associate
--                   (Get_History (Kernel).all, "Find_Prefs_From_Same_File",
--                    From_Same_File);

               --  Filter choice

               Gtk_New (Frame, "Filter");
               Pack_Start (Get_Content_Area (Dialog), Frame);
               Gtk_New_Hbox (Box, Homogeneous => False);
               Add (Frame, Box);

               for C in Col'Range loop
                  Gtk_New_Vbox (Col (C), Homogeneous => True);
                  Pack_Start (Box, Col (C), Expand => True);
               end loop;

               for F in Dialog.Filters'Range loop
                  Gtk_New (Dialog.Filters (F), All_Refs (F).all);
                  Pack_Start (Col (Index), Dialog.Filters (F));
                  Histories.Create_New_Boolean_Key_If_Necessary
                    (Get_History (Kernel).all,
                     Histories.History_Key
                       ("Find_Prefs_Filter_" & F'Img), True);
                  Histories.Associate
                    (Get_History (Kernel).all,
                     Histories.History_Key ("Find_Prefs_Filter_" & F'Img),
                     Dialog.Filters (F));
                  Index := Index + 1;
                  if Index > Col'Last then
                     Index := Col'First;
                  end if;
               end loop;

               Gtk_New (Filter_Box);
               Set_Layout (Filter_Box, Buttonbox_Spread);
               Pack_Start (Box, Filter_Box, Padding => 5);

               Gtk_New (Button, "Select all");
               Pack_Start (Filter_Box, Button);
               Widget_Callback.Object_Connect
                 (Button, Signal_Clicked, Select_All_Filters'Access, Dialog);

               Gtk_New (Button, "Unselect all");
               Pack_Start (Filter_Box, Button);
               Widget_Callback.Object_Connect
                 (Button, Signal_Clicked, Unselect_All_Filters'Access, Dialog);

               --  Extra info choice

               Gtk_New (Frame, "Advanced Search");
               Pack_Start (Get_Content_Area (Dialog), Frame);
               Gtk_New_Vbox (Box, Homogeneous => True);
               Add (Frame, Box);

               Gtk_New (Show_Caller, "Show context");
               Pack_Start (Box, Show_Caller);
               Histories.Create_New_Boolean_Key_If_Necessary
                 (Get_History (Kernel).all, "Find_Prefs_Show_Caller", False);
               Histories.Associate
                 (Get_History (Kernel).all,
                  "Find_Prefs_Show_Caller",
                  Show_Caller);

               Gtk_New
                 (Include_Overriding,
                  "Include overriding and overridden operations");
               Pack_Start (Box, Include_Overriding);
               Histories.Create_New_Boolean_Key_If_Necessary
                 (Get_History (Kernel).all,
                  "Find_Prefs_Include_Overriding",
                  False);
               Histories.Associate
                 (Get_History (Kernel).all,
                  "Find_Prefs_Include_Overriding",
                  Include_Overriding);

               Ignore := Add_Button
                 (Dialog, Gtk.Stock.Stock_Ok, Gtk_Response_OK);
               Ignore := Add_Button
                 (Dialog, Gtk.Stock.Stock_Cancel, Gtk_Response_Cancel);

               Show_All (Dialog);

               if Run (Dialog) = Gtk_Response_OK then
                  for F in Dialog.Filters'Range loop
                     if not Get_Active (Dialog.Filters (F)) then
                        Free (All_Refs (F));
                     end if;
                  end loop;

                  Kernel.Get_Messages_Container.Remove_Category
                    (To_String (Title), Message_Flag);

--  will be used when we have references kinds
--   if Get_Active (From_Same_File) then
--   get file(1) where entity is declared / get declaration request
--   get all entities declared in this file(1)
--   send separate requests for each entity

                  declare
                     Filter : constant Result_Filter :=
                       (Ref_Kinds => new GNAT.Strings.String_List'(All_Refs));

                     From_File : constant GNATCOLL.VFS.Virtual_File :=
                       (if Get_Active (File_Only)
                        then File
                        else GNATCOLL.VFS.No_File);

                     Request : References_Request_Access :=
                       new References_Request;
                  begin
                     Request.Kernel              := Kernel;
                     Request.Title               := Title;
                     Request.Name                := To_Unbounded_String
                       (Entity_Name_Information (Context.Context));
                     Request.Text_Document       := File;
                     Request.Line                :=
                       Line_Information (Context.Context);
                     Request.Column              :=
                       Column_Information (Context.Context);
                     Request.Include_Declaration :=
                       Get_Active (Include_Overriding);
                     Request.Show_Caller         := Get_Active (Show_Caller);
                     Request.Filter              := Filter;
                     Request.From_File           := From_File;

                     GPS.LSP_Client.Requests.Execute
                       (Lang,
                        GPS.LSP_Client.Requests.Request_Access (Request));
                  end;

                  Unchecked_Free (Dialog.Filters);
                  Destroy (Dialog);

                  return Commands.Success;
               else
                  Unchecked_Free (Dialog.Filters);
                  GNATCOLL.Utils.Free (All_Refs);
                  Destroy (Dialog);
                  return Commands.Failure;
               end if;
            end;

         else
            Kernel.Get_Messages_Container.Remove_Category
              (To_String (Title), Message_Flag);

            --  Open the Locations view if needed and put in foreground.
            --  Display an activity progress bar on since references can take
            --  some time to compute.

            GPS.Location_View.Raise_Locations_Window
              (Self             => Kernel,
               Give_Focus       => False,
               Create_If_Needed => True);
            GPS.Location_View.Set_Activity_Progress_Bar_Visibility
              (GPS.Location_View.Get_Or_Create_Location_View (Kernel),
               Visible => True);

            declare
               use type Language.Language_Access;

               Request : References_Request_Access :=
                           new References_Request;
            begin
               Request.Kernel              := Kernel;
               Request.Title               := Title;
               Request.Name                := To_Unbounded_String
                 (Entity_Name_Information (Context.Context));
               Request.Text_Document       := File;
               Request.Line                := Line_Information
                 (Context.Context);
               Request.Column              :=
                 Column_Information (Context.Context);
               Request.Include_Declaration := True;
               Request.From_File           :=
                 (if Command.Locals_Only
                  then File
                  else GNATCOLL.VFS.No_File);
               Request.Show_Caller         := Kernel.Get_Language_Handler.
                 Get_Language_From_File (File) = Language.Ada.Ada_Lang;

               GPS.LSP_Client.Requests.Execute
                 (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
            end;

            return Commands.Success;
         end if;

      else
         --  Old implementation with XRef
         if Command.Specific then
            declare
               C : aliased GPS.Kernel.Entities.Find_Specific_Refs_Command;
            begin
               return C.Execute (Context);
            end;

         else
            declare
               C : aliased GPS.Kernel.Entities.Find_All_Refs_Command;
            begin
               C.Locals_Only := Command.Locals_Only;
               return C.Execute (Context);
            end;
         end if;
      end if;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Entity_Name_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Entity_Name_Information (Context);
   end Filter_Matches_Primitive;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out References_Request;
      Result : LSP.Messages.Location_Vector)
   is
      use GNATCOLL.VFS;
      use LSP.Types;
      use LSP.Messages;

      Cursor  : Location_Vectors.Cursor := Result.First;
      File    : Virtual_File;
      Loc     : Location;
      Message : GPS.Kernel.Messages.Markup.Markup_Message_Access
        with Unreferenced;

   begin
      GPS.Location_View.Set_Activity_Progress_Bar_Visibility
        (GPS.Location_View.Get_Or_Create_Location_View (Self.Kernel),
         Visible => False);

      while Location_Vectors.Has_Element (Cursor) loop
         Loc  := Location_Vectors.Element (Cursor);
         File := GPS.LSP_Client.Utilities.To_Virtual_File (Loc.uri);

         if Self.From_File = No_File
           or else Self.From_File = File
                  --  and then Is_Valid_Filter
         then
            if Self.Command = null then
               Message :=
                 GPS.Kernel.Messages.Markup.Create_Markup_Message
                   (Self.Kernel.Get_Messages_Container,
                    To_String (Self.Title),
                    File,
                    (if Loc.span.first.line <= 0
                     then 1
                     else Integer (Loc.span.first.line) + 1),
                    GPS.LSP_Client.Utilities.UTF_16_Offset_To_Visible_Column
                      (Loc.span.first.character),
                    To_String (Self.Name),

--  will be used when we have references kinds
--  & Reference_Kind
--  & if Self.Show_Caller and then Get_Caller (Ref) /= No_Root_Entity then
--       Add "called by" information to the responce

                    Unspecified,
                    Message_Flag);

            else
               --  fill command list to return as a result via python API
               Self.Command.Locations.Append (Loc);
            end if;
         end if;

         Location_Vectors.Next (Cursor);
      end loop;
   end On_Result_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out References_Request) is
   begin
      GPS.Location_View.Set_Activity_Progress_Bar_Visibility
        (GPS.Location_View.Get_Or_Create_Location_View (Self.Kernel),
         Visible => False);

      if Self.Filter.Ref_Kinds /= null then
         GNATCOLL.Utils.Free (Self.Filter.Ref_Kinds.all);
         Free (Self.Filter.Ref_Kinds);
      end if;

      GPS.LSP_Client.Requests.References.Finalize
        (GPS.LSP_Client.Requests.References.Abstract_References_Request
           (Self));
   end Finalize;

   ------------------------
   -- Select_All_Filters --
   ------------------------

   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         Set_Active (D.Filters (F), True);
      end loop;
   end Select_All_Filters;

   --------------------------
   -- Unselect_All_Filters --
   --------------------------

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         Set_Active (D.Filters (F), False);
      end loop;
   end Unselect_All_Filters;

   -------------------
   -- Find_All_Refs --
   -------------------

   procedure Find_All_Refs
     (Kernel   : Kernel_Handle;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Integer;
      Column   : Basic_Types.Visible_Column_Type;
      Name     : String;
      Implicit : Boolean;
      In_File  : GNATCOLL.VFS.Virtual_File;
      Data     : GNATCOLL.Scripts.Callback_Data_Access)
   is
      Lang  : Standard.Language.Language_Access;
      Title : Unbounded_String;

   begin
      Lang := Kernel.Get_Language_Handler.Get_Language_From_File (File);

      if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
         --  Implicit is used for Is_Read_Or_Write_Or_Implicit_Reference
         Title := To_Unbounded_String
           (All_Refs_Category (Name, False, File, False));

         Kernel.Get_Messages_Container.Remove_Category
           (To_String (Title), Message_Flag);

         declare
            use GNATCOLL.Scripts;
            use type Language.Language_Access;

            Command : constant Ref_Command_Access :=
              (if Data = null
               then null
               else new References_Command);

            Request : References_Request_Access :=
              new References_Request;
         begin
            Request.Kernel              := Kernel;
            Request.Title               := Title;
            Request.Name                := To_Unbounded_String (Name);
            Request.Text_Document       := File;
            Request.Line                := Line;
            Request.Column              := Column;
            Request.Include_Declaration := True;
            Request.From_File           := GNATCOLL.VFS.No_File;
            Request.Show_Caller         := Kernel.Get_Language_Handler.
              Get_Language_From_File (File) = Language.Ada.Ada_Lang;
            Request.Command             := Command;

            GPS.LSP_Client.Requests.Execute
              (Lang, GPS.LSP_Client.Requests.Request_Access (Request));

            if Data /= null then
               Data.Set_Return_Value
                 (GPS.Scripts.Commands.Get_Instance
                    (GPS.Scripts.Commands.Create_Wrapper (Command),
                     Data.Get_Script,
                     Class_To_Create => References_Command_Class_Name));
            end if;
         end;

      else
         --  Use old implementation Src_Editor_Module -> Entity -> Xref

         Src_Editor_Module.Shell.Find_All_Refs
           (Kernel, File, Line, Column, Name, Implicit, In_File, Data);
      end if;
   end Find_All_Refs;

   ----------------
   -- Get_Result --
   ----------------

   overriding procedure Get_Result
     (Self : not null access References_Command;
      Data : in out GNATCOLL.Scripts.Callback_Data'Class)
   is
      use GNATCOLL.Scripts;
      use GPS.Kernel.Scripts;

      Inst : Class_Instance;
   begin
      Set_Return_Value_As_List (Data);

      for Loc of Self.Locations loop
         Inst := Create_File_Location
           (Script => Get_Script (Data),
            File   => Create_File
              (Script => Get_Script (Data),
               File   => GPS.LSP_Client.Utilities.To_Virtual_File (Loc.uri)),
            Line   => Integer (Loc.span.first.line) + 1,
            Column => GPS.LSP_Client.Utilities.UTF_16_Offset_To_Visible_Column
              (Loc.span.first.character));

         Set_Return_Value (Data, Inst);
      end loop;
   end Get_Result;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is
      Has_Entity_Name : constant Action_Filter := new Has_Entity_Name_Filter;
   begin
      Src_Editor_Module.Shell.Find_All_Refs_Handler := Find_All_Refs'Access;

      Register_Action
        (Kernel, "find all references",
         Command     => new Find_Refs_Command (False, False),
         Description =>
           "List all references to the selected entity"
             & " in the Locations window",
         Filter => Has_Entity_Name);

      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel,
         Name   => "Find All References",
         Action => "find all references",
         Group  => GPS.Kernel.Modules.UI.Navigation_Contextual_Group);

      Register_Action
        (Kernel, "find all local references",
         Command     => new Find_Refs_Command (True, False),
         Description =>
           "List all references in the selected file to the selected entity"
           & " in the Locations window",
         Filter => Has_Entity_Name);

      Register_Action
        (Kernel, "find references...",
         Command     => new Find_Refs_Command (False, True),
         Description =>
           "List all references to the selected entity"
           & " in the Locations window, with extra filters",
         Filter => Has_Entity_Name);
   end Register;

end GPS.LSP_Client.References;
