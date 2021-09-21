------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2021, AdaCore                   --
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

with GNATCOLL.JSON;
with GNATCOLL.Scripts;
with GNATCOLL.VFS;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.Xref;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Radio_Button;           use Gtk.Radio_Button;
with Gtk.Stock;
with Gtk.Widget;                 use Gtk.Widget;
with Glib.Convert;               use Glib.Convert;

with VSS.JSON.Pull_Readers.Simple;
with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with GPS.Default_Styles;         use GPS.Default_Styles;
with GPS.Editors;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Entities;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;
with GPS.Location_View;
with GPS.LSP_Module;
with GPS.LSP_Client.Requests.References;
with GPS.LSP_Client.Utilities;   use GPS.LSP_Client.Utilities;
with GPS.Main_Window;            use GPS.Main_Window;
with GPS.Scripts.Commands;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;
with Dialog_Utils;               use Dialog_Utils;
with Histories;
with Language;
with Src_Editor_Module.Shell;

with Basic_Types;                use Basic_Types;
with LSP.JSON_Streams;
with LSP.Messages;
with String_Utils;               use String_Utils;
with UTF8_Utils;

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

   type Result_Filter (Is_Set : Boolean := False) is record
      Ref_Kinds : VSS.String_Vectors.Virtual_String_Vector;
      --  The reference kinds' name that should be displayed.
   end record;
   --  Will be used for filtering results.
   --  When Is_Set is False, the filter is not active and Ref_Kinds should
   --  not be taken into account.

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
         Title     : Unbounded_String;
         Name      : Unbounded_String;
         Filter    : Result_Filter;
         Command   : Ref_Command_Access;
         File_Only : Boolean;
         Column    : Visible_Column_Type;
      end record;
   --  Used for communicate with LSP

   overriding procedure Finalize (Self : in out References_Request);

   overriding function Get_Task_Label
     (Self : References_Request) return String
   is
     ("querying references");

   overriding procedure On_Result_Message
     (Self   : in out References_Request;
      Result : LSP.Messages.Location_Vector);

   overriding procedure On_Error_Message
     (Self    : in out References_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);
   -- Others --

   function All_Refs_Category
     (Entity             : String;
      Line               : Integer;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File)
      return String;
   --  Return a suitable category for references action messages.

   type Filters_Buttons is array (Natural range <>) of Gtk_Check_Button;
   type Filters_Buttons_Access is access Filters_Buttons;
   type References_Filter_Dialog_Record is new Gtk_Dialog_Record with record
      Filters      : Filters_Buttons_Access;
      Include_Decl : Gtk_Check_Button;
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

   function All_Reference_Kinds
     return VSS.String_Vectors.Virtual_String_Vector;
   --  Returns list of all supported reference kinds.

   Message_Flag : constant Message_Flags :=
     (Editor_Side => True,
      Editor_Line => False,
      Locations   => True);

   -----------------------
   -- All_Refs_Category --
   -----------------------

   function All_Refs_Category
     (Entity             : String;
      Line               : Integer;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File)
      return String is
   begin
      if Local_Only then
         return "Local references for "
           & Entity
           & " ("  & Local_File.Display_Base_Name
           & ":" & String_Utils.Image (Line) & ") " & "in "
           & (Local_File.Display_Base_Name);

      else
         return "References for " & Entity
           & " (" & Local_File.Display_Base_Name
           & ":" & String_Utils.Image (Line) & ")";
      end if;
   end All_Refs_Category;

   -------------------------
   -- All_Reference_Kinds --
   -------------------------

   function All_Reference_Kinds
     return VSS.String_Vectors.Virtual_String_Vector
   is
      Interesting_Kinds : constant LSP.Messages.AlsReferenceKind_Set :=
        (Is_Server_Side => True, As_Flags =>
           (LSP.Messages.Parent => False,
            others => True));
      Interesting_Strs  : LSP.Messages.AlsReferenceKind_Set;

      JS     : aliased LSP.JSON_Streams.JSON_Stream;
      Output : aliased
        VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;

   begin
      JS.Set_Stream (Output'Unchecked_Access);
      LSP.Messages.AlsReferenceKind_Set'Write (JS'Access, Interesting_Kinds);
      JS.End_Document;

      declare
         Memory : aliased
           VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
         Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         Input  : aliased LSP.JSON_Streams.JSON_Stream
           (False, Reader'Unchecked_Access);
      begin
         Memory.Set_Data (Output.Buffer);
         Reader.Set_Stream (Memory'Unchecked_Access);
         Reader.Read_Next;
         pragma Assert (Reader.Is_Start_Document);
         Reader.Read_Next;
         LSP.Messages.AlsReferenceKind_Set'Read
           (Input'Access, Interesting_Strs);

         return Interesting_Strs.As_Strings;
      end;
   end All_Reference_Kinds;

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
      Line   : Integer;
      Column : Visible_Column_Type;
   begin
      File := File_Information (Context.Context);
      Lang := Kernel.Get_Language_Handler.Get_Language_From_File (File);

      if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
         Line :=
           (if Has_Entity_Line_Information (Context.Context) then
               Integer (Entity_Line_Information (Context.Context))
            else
               Line_Information (Context.Context));
         Column :=
           (if Has_Entity_Column_Information (Context.Context) then
               Entity_Column_Information (Context.Context)
            else
               Column_Information (Context.Context));

         Title := To_Unbounded_String
           (All_Refs_Category
              (Entity     => Entity_Name_Information (Context.Context),
               Line       => Line,
               Local_Only => Command.Locals_Only,
               Local_File => File));

         if Command.Specific then
            declare
               All_Refs              : constant
                 VSS.String_Vectors.Virtual_String_Vector :=
                   All_Reference_Kinds;
               Dialog                : References_Filter_Dialog;
               Main_View             : Dialog_View;
               Filters_View          : Dialog_View_With_Button_Box;
               Group_Widget          : Dialog_Group_Widget;
               Button                : Gtk_Button;
               Project_And_Recursive : Gtk_Radio_Button;
               File_Only             : Gtk_Radio_Button;
               Ignore                : Gtk_Widget;

            begin
               Dialog := new References_Filter_Dialog_Record;
               Dialog.Filters := new Filters_Buttons (1 .. All_Refs.Length);

               Initialize
                 (Dialog,
                  Title  => "Find References Options",
                  Parent => Kernel.Get_Main_Window,
                  Flags  => Modal
                  or Use_Header_Bar_From_Settings (Kernel.Get_Main_Window));
               Set_Default_Size_From_History
                 (Win    => Dialog,
                  Name   => "Find References Options",
                  Kernel => Kernel,
                  Width  => 450,
                  Height => 280);

               Main_View := new Dialog_View_Record;
               Dialog_Utils.Initialize (Main_View);
               Dialog.Get_Content_Area.Pack_Start (Main_View);

               --  Context choice

               Group_Widget := new Dialog_Group_Widget_Record;
               Dialog_Utils.Initialize
                 (Self                => Group_Widget,
                  Parent_View         => Main_View,
                  Group_Name          => "Context",
                  Allow_Multi_Columns => True);

               Gtk_New (Project_And_Recursive, Widget_SList.Null_List,
                        "In all projects");
               Group_Widget.Create_Child
                 (Widget => Project_And_Recursive,
                  Doc    => "Perform the search in whole project hierarchy.");

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
               Group_Widget.Create_Child
                 (Widget => File_Only,
                  Doc    => "Perform the search in the current file only.");

               Histories.Create_New_Boolean_Key_If_Necessary
                 (Get_History (Kernel).all, "Find_Prefs_File_Only", False);
               Histories.Associate
                 (Get_History (Kernel).all, "Find_Prefs_File_Only", File_Only);

               --  Filter choice

               Group_Widget := new Dialog_Group_Widget_Record;
               Group_Widget.Initialize
                 (Parent_View         => Main_View,
                  Group_Name          => "Filters",
                  Allow_Multi_Columns => False);

               Filters_View := new Dialog_View_With_Button_Box_Record;
               Dialog_Utils.Initialize
                 (Self     => Filters_View,
                  Position => Pos_Right);

               Group_Widget.Append_Child
                 (Filters_View,
                  Expand => True,
                  Fill   => True);

               Group_Widget := new Dialog_Group_Widget_Record;
               Group_Widget.Initialize
                 (Parent_View => Filters_View);

               --  Add the LSP 'includeDeclaration' predefined filter
               Gtk_New (Dialog.Include_Decl, Label => "include declaration");
               Group_Widget.Create_Child (Dialog.Include_Decl);

               Histories.Create_New_Boolean_Key_If_Necessary
                 (Get_History (Kernel).all,
                  Histories.History_Key'
                    ("Find_Prefs_Filter_include_decl"),
                  False);

                  Histories.Associate
                    (Get_History (Kernel).all,
                     Histories.History_Key'
                       ("Find_Prefs_Filter_include_decl"),
                     Dialog.Include_Decl);

               --  Add the server specific filters
               for F in Dialog.Filters'Range loop
                  Gtk_New
                    (Dialog.Filters (F),
                     VSS.Strings.Conversions.To_UTF_8_String (All_Refs (F)));
                  Group_Widget.Create_Child (Dialog.Filters (F));

                  Histories.Create_New_Boolean_Key_If_Necessary
                    (Get_History (Kernel).all,
                     Histories.History_Key
                       ("Find_Prefs_Filter_" & F'Img), True);
                  Histories.Associate
                    (Get_History (Kernel).all,
                     Histories.History_Key ("Find_Prefs_Filter_" & F'Img),
                     Dialog.Filters (F));
               end loop;

               Gtk_New (Button, "Select all");
               Filters_View.Append_Button (Button);
               Widget_Callback.Object_Connect
                 (Button, Signal_Clicked,
               Select_All_Filters'Access, Dialog);

               Gtk_New (Button, "Unselect all");
               Filters_View.Append_Button (Button);
               Widget_Callback.Object_Connect
                 (Button, Signal_Clicked, Unselect_All_Filters'Access, Dialog);

               --  Extra info choice

               Ignore := Add_Button
                 (Dialog, Gtk.Stock.Stock_Ok, Gtk_Response_OK);
               Set_Default_Response (Dialog, Gtk_Response_OK);
               Ignore := Add_Button
                 (Dialog, Gtk.Stock.Stock_Cancel, Gtk_Response_Cancel);

               Show_All (Dialog);

               if Run (Dialog) = Gtk_Response_OK then
                  Kernel.Get_Messages_Container.Remove_Category
                    (To_String (Title), Message_Flag);

                  declare
                     Filter  : Result_Filter (Is_Set => True);
                     Request : GPS.LSP_Client.Requests.Request_Access;

                  begin
                     for F in Dialog.Filters'Range loop
                        if Dialog.Filters (F).Get_Active then
                           Filter.Ref_Kinds.Append (All_Refs (F));
                        end if;
                     end loop;

                     declare
                        Holder   : constant GPS.Editors.
                          Controlled_Editor_Buffer_Holder :=
                            Kernel.Get_Buffer_Factory.Get_Holder (File);
                        Location : constant
                          GPS.Editors.Editor_Location'Class :=
                            Holder.Editor.New_Location (Line, Column);
                     begin
                        Request := new References_Request'
                          (GPS.LSP_Client.Requests.LSP_Request with
                           Kernel              => Kernel,
                           File                => File,
                           Title               => Title,
                           Name                =>
                             To_Unbounded_String
                               (Entity_Name_Information (Context.Context)),
                           Position            =>
                             Location_To_LSP_Position (Location),
                           Include_Declaration =>
                             Dialog.Include_Decl.Get_Active,
                           Filter              => Filter,
                           File_Only           => File_Only.Get_Active,
                           Command             => null,
                           Column              => Location.Column);
                     end;

                     GPS.Location_View.Set_Activity_Progress_Bar_Visibility
                       (GPS.Location_View.Get_Or_Create_Location_View (Kernel),
                        Visible => True);

                     GPS.LSP_Client.Requests.Execute (Lang, Request);
                  end;

                  Unchecked_Free (Dialog.Filters);
                  Destroy (Dialog);

                  return Commands.Success;
               else
                  Unchecked_Free (Dialog.Filters);
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
               Holder   : constant GPS.Editors.
                 Controlled_Editor_Buffer_Holder :=
                   Kernel.Get_Buffer_Factory.Get_Holder (File);
               Location : constant GPS.Editors.Editor_Location'Class :=
                 Holder.Editor.New_Location (Line, Column);

               Request : GPS.LSP_Client.Requests.Request_Access :=
                 new References_Request'
                   (GPS.LSP_Client.Requests.LSP_Request with
                    Kernel              => Kernel,
                    Title               => Title,
                    Name                => To_Unbounded_String
                      (Entity_Name_Information (Context.Context)),
                    Position            => Location_To_LSP_Position (Location),
                    Include_Declaration => True,
                    File                => File,
                    File_Only           => Command.Locals_Only,
                    Filter              =>
                       Result_Filter'(Is_Set    => False,
                                      Ref_Kinds => All_Reference_Kinds),
                    Command             => null,
                    Column              => Location.Column);

            begin
               GPS.LSP_Client.Requests.Execute (Lang, Request);
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
      use Basic_Types;
      use GNATCOLL.VFS;
      use GNATCOLL.Xref;
      use GPS.Editors;
      use LSP.Messages;

      function Match (Item : LSP.Messages.Location) return Boolean;
      --  Return True when one of reference kinds of the given location match
      --  selected filter criteria.

      -----------
      -- Match --
      -----------

      function Match (Item : LSP.Messages.Location) return Boolean is
         use type VSS.Strings.Virtual_String;

      begin
         --  Return True if there is no filter or if the reference has not
         --  any associated kind.
         if not Self.Filter.Is_Set
           or else Item.alsKind.As_Strings.Is_Empty
         then
            return True;
         end if;

         --  Try to match the filter otherwise
         for K of Item.alsKind.As_Strings loop
            for F of Self.Filter.Ref_Kinds loop
               if K = F then
                  return True;
               end if;
            end loop;
         end loop;

         return False;
      end Match;

      File                 : Virtual_File;
      Message              : GPS.Kernel.Messages.Markup.Markup_Message_Access;
      Kinds                : Ada.Strings.Unbounded.Unbounded_String;
      Aux                  : VSS.String_Vectors.Virtual_String_Vector;
      Buffers_To_Close     : Editor_Buffer_Lists.List;
      Locations            : constant GPS.Location_View.Location_View_Access :=
        GPS.Location_View.Get_Or_Create_Location_View
          (Self.Kernel);
      References_Displayed : Boolean := False;
   begin
      if Locations /= null then
         GPS.Location_View.Set_Activity_Progress_Bar_Visibility
           (Locations,
            Visible => False);
      end if;

      for Loc of Result loop
         File := GPS.LSP_Client.Utilities.To_Virtual_File (Loc.uri);

         if (not Self.File_Only
             or else Self.File = File)
           and then Match (Loc)
         then
            if Self.Command = null then
               --  Construct list of reference kinds in form "[kind, kind]"
               --  if any.

               Kinds := Null_Unbounded_String;

               if Loc.alsKind /= Empty_Set
                 and then not Loc.alsKind.As_Strings.Is_Empty
               then
                  Aux := Loc.alsKind.As_Strings;

                  for S of Aux loop
                     if Kinds = "" then
                        Append (Kinds, '[');

                     else
                        Append (Kinds, ", ");
                     end if;

                     Append
                       (Kinds, VSS.Strings.Conversions.To_UTF_8_String (S));
                  end loop;

                  Append (Kinds, "] ");
               end if;

               declare
                  Buffer       : Editor_Buffer_Holders.Holder :=
                                          Editor_Buffer_Holders.To_Holder
                                            (Self.Kernel.Get_Buffer_Factory.Get
                                               (File            => File,
                                                Force           => False,
                                                Open_Buffer     => False,
                                                Open_View       => False,
                                                Focus           => False,
                                                Only_If_Focused => False));
               begin

                  --  If no buffer was opened for the given file, open a new
                  --  one.
                  --  Append it to the list of buffers that we should close
                  --  when exiting the functions.

                  if Buffer.Element = Nil_Editor_Buffer then
                     Buffer := Editor_Buffer_Holders.To_Holder
                       (Self.Kernel.Get_Buffer_Factory.Get
                          (File            => File,
                           Force           => False,
                           Open_Buffer     => True,
                           Open_View       => False,
                           Focus           => False,
                           Only_If_Focused => False));
                     Buffers_To_Close.Append (Buffer);
                  end if;

                  declare
                     From : constant GPS.Editors.Editor_Location'Class :=
                       GPS.LSP_Client.Utilities.LSP_Position_To_Location
                         (Buffer.Element, Loc.span.first);
                     To   : constant GPS.Editors.Editor_Location'Class :=
                       GPS.LSP_Client.Utilities.LSP_Position_To_Location
                         (Buffer.Element, Loc.span.last);

                     Start_Loc  : constant GPS.Editors.Editor_Location'Class :=
                                    Buffer.Element.New_Location_At_Line
                                      (From.Line);
                     End_Loc    : constant GPS.Editors.Editor_Location'Class :=
                                       Start_Loc.End_Of_Line;
                     Whole_Line : constant String := Buffer.Element.Get_Chars
                       (From => Start_Loc,
                        To   => End_Loc);
                     Start      : Natural := Whole_Line'First;
                     Last       : Natural := Whole_Line'Last;
                     Before_Idx : Natural := 0;
                     After_Idx  : Natural := 0;
                  begin

                     --  We got the whole line containing the reference: strip
                     --  the blankspaces at the beginning/end of the line.

                     Skip_Blanks (Whole_Line, Index => Start);
                     Skip_Blanks_Backward (Whole_Line, Index => Last);

                     --  Get the text after and before the reference and
                     --  concatenate it with the reference itself surrounded by
                     --  bold markup.

                     Before_Idx := (Whole_Line'First - 1)
                       + UTF8_Utils.Column_To_Index
                       (Whole_Line,
                        Character_Offset_Type (Loc.span.first.character));

                     After_Idx := (Whole_Line'First - 1)
                       + UTF8_Utils.Column_To_Index
                       (Whole_Line,
                        Character_Offset_Type (Loc.span.last.character) + 1);

                     --  Ensure that Before_Idx and After_Idx are within
                     --  the range: these indexes may be outside of the range
                     --  when the reference name is placed at the start/end
                     --  of the line.

                     if Before_Idx not in Whole_Line'Range then
                        Before_Idx := Whole_Line'First;
                     end if;

                     if After_Idx not in Whole_Line'Range then
                        After_Idx := Whole_Line'Last;
                     end if;

                     declare
                        Before_Text : constant String :=
                          Whole_Line
                            (Start .. Before_Idx);
                        After_Text  : constant String :=
                          Whole_Line
                            (After_Idx .. Last);
                        Name_Text   : constant String :=
                          Whole_Line
                            (Before_Idx + 1 .. After_Idx - 1);
                        Msg_Text    : constant String :=
                          Escape_Text (Before_Text)
                          & "<b>"
                          & Escape_Text (Name_Text)
                          & "</b>"
                          & Escape_Text (After_Text);
                     begin
                        References_Displayed := True;

                        Message :=
                          GPS.Kernel.Messages.Markup.Create_Markup_Message
                            (Container  =>
                               Self.Kernel.Get_Messages_Container,
                             Category   => To_String (Self.Title),
                             File       => File,
                             Line       => From.Line,
                             Column     => From.Column,
                             Text       => To_String (Kinds) & Msg_Text,

                                 --  will be used when we have references
                             --  kinds & if Self.Show_Caller and then
                             --  Get_Caller (Ref) /= No_Root_Entity then
                             --  Add "called by" information to the response

                             Importance => Unspecified,
                             Flags      => Message_Flag);

                        GPS.Kernel.Messages.Set_Highlighting
                          (Self   => Message,
                           Style  => Search_Results_Style,
                           --  The number of characters to highlight is the
                           --  number of decoded UTF-8 characters
                           Length => Highlight_Length
                             (To.Column - From.Column));
                     end;
                  end;
               end;

            else
               --  fill command list to return as a result via python API
               Self.Command.Locations.Append (Loc);
            end if;
         end if;
      end loop;

      --  Close all the buffers that were not opened at the beginning.
      --  This allows to save memory.

      for Buffer of Buffers_To_Close loop
         Buffer.Element.Close;
      end loop;

      --  If no references have been found or if none of them matched the
      --  filters, display a "No references found" message.

      if not References_Displayed then
         declare
            Filter_List : Unbounded_String;

         begin
            if Self.Filter.Is_Set then
               for Name of Self.Filter.Ref_Kinds loop
                  if Filter_List /= Null_Unbounded_String then
                     Append
                       (Filter_List,
                        " | "
                        & VSS.Strings.Conversions.To_UTF_8_String (Name));
                  else
                     Append
                       (Filter_List,
                        VSS.Strings.Conversions.To_UTF_8_String (Name));
                  end if;
               end loop;
               if Filter_List /= Null_Unbounded_String then
                  Append (Filter_List, "]");
                  Filter_List := " for filters [" & Filter_List;
               end if;
            end if;

            Message :=
              GPS.Kernel.Messages.Markup.Create_Markup_Message
                (Container  => Self.Kernel.Get_Messages_Container,
                 Category   => To_String (Self.Title),
                 File       => Self.File,
                 Line       => Integer (Self.Position.line) + 1,
                 Column     => Self.Column,
                 Text       =>
                   "No references found for "
                 & To_String (Self.Name)
                 & To_String (Filter_List),
                 Importance => Unspecified,
                 Flags      => Message_Flag);
         end;
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out References_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      Locations : constant GPS.Location_View.Location_View_Access :=
                           GPS.Location_View.Get_Or_Create_Location_View
                             (Self.Kernel);
   begin
      if Locations /= null then
         GPS.Location_View.Set_Activity_Progress_Bar_Visibility
           (Locations,
            Visible => False);
      end if;
   end On_Error_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out References_Request) is
      Locations_View : constant GPS.Location_View.Location_View_Access :=
                         GPS.Location_View.Get_Or_Create_Location_View
                           (Self.Kernel,
                            Allow_Creation => False);
   begin
      if Locations_View /= null then
         GPS.Location_View.Set_Activity_Progress_Bar_Visibility
           (Locations_View,
            Visible => False);
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
      Set_Active (D.Include_Decl, True);
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
      Set_Active (D.Include_Decl, False);
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
      Lang   : Standard.Language.Language_Access;
      Title  : Unbounded_String;
      Result : Boolean := False;

   begin
      Lang := Kernel.Get_Language_Handler.Get_Language_From_File (File);

      if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
         --  Implicit is used for Is_Read_Or_Write_Or_Implicit_Reference
            Title := To_Unbounded_String
              (All_Refs_Category
                   (Entity     => Name,
                    Line       => Line,
                    Local_Only => False,
                    Local_File => File));

         Kernel.Get_Messages_Container.Remove_Category
           (To_String (Title), Message_Flag);

         declare
            use GNATCOLL.Scripts;

            Command : constant Ref_Command_Access :=
              (if Data = null
               then null
               else new References_Command);

            Holder   : constant GPS.Editors.
              Controlled_Editor_Buffer_Holder :=
                Kernel.Get_Buffer_Factory.Get_Holder (File);
            Location : constant GPS.Editors.Editor_Location'Class :=
              Holder.Editor.New_Location (Line, Column);

            Request : GPS.LSP_Client.Requests.Request_Access :=
              new References_Request'
                (GPS.LSP_Client.Requests.LSP_Request with
                 Kernel              => Kernel,
                 Title               => Title,
                 Name                => To_Unbounded_String (Name),
                 Position            => Location_To_LSP_Position (Location),
                 Include_Declaration => True,
                 File                => File,
                 File_Only           => False,
                 Filter              => Result_Filter'
                   (Is_Set    => False,
                    Ref_Kinds => <>),
                 Command             => Command,
                 Column              => Location.Column);

         begin
            Result := GPS.LSP_Client.Requests.Execute (Lang, Request);

            if Result
              and then Data /= null
            then
               Data.Set_Return_Value
                 (GPS.Scripts.Commands.Get_Instance
                    (GPS.Scripts.Commands.Create_Wrapper (Command),
                     Data.Get_Script,
                     Class_To_Create => References_Command_Class_Name));
            end if;
         end;
      end if;

      if not Result then
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
         declare
            File : constant GNATCOLL.VFS.Virtual_File :=
              GPS.LSP_Client.Utilities.To_Virtual_File (Loc.uri);
            Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
              Get_Kernel (Data).Get_Buffer_Factory.Get_Holder (File => File);
            Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Loc.span.first);
         begin
            Inst := Create_File_Location
              (Script => Get_Script (Data),
               File   => Create_File
                 (Script => Get_Script (Data),
                  File   => File),
               Line   => Location.Line,
               Column => Location.Column);
         end;
         Set_Return_Value (Data, Inst);
      end loop;
   end Get_Result;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is
      Has_Entity_Name : constant Action_Filter := new Has_Entity_Name_Filter;
      Find_All        : constant String := "Find All References";
      Find_Dialog     : constant String := "Find References...";
   begin
      Src_Editor_Module.Shell.Find_All_Refs_Handler := Find_All_Refs'Access;

      Register_Action
        (Kernel, Find_All,
         Command     => new Find_Refs_Command (False, False),
         Description =>
           "List all references to the selected entity"
             & " in the Locations window",
         Filter => Has_Entity_Name);

      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel,
         Name   => Find_All,
         Action => Find_All,
         Group  => GPS.Kernel.Modules.UI.Navigation_Contextual_Group);

      Register_Action
        (Kernel, "find all local references",
         Command     => new Find_Refs_Command (True, False),
         Description =>
           "List all references in the selected file to the selected entity"
           & " in the Locations window",
         Filter => Has_Entity_Name);

      Register_Action
        (Kernel, Find_Dialog,
         Command     => new Find_Refs_Command (False, True),
         Description =>
           "List all references to the selected entity"
           & " in the Locations window, with extra filters",
         Filter => Has_Entity_Name);

      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel,
         Name   => Find_Dialog,
         Action => Find_Dialog,
         Group  => GPS.Kernel.Modules.UI.Navigation_Contextual_Group);
   end Register;

end GPS.LSP_Client.References;
