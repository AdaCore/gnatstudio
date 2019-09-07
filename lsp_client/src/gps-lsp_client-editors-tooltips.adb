------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.Traces;               use GNATCOLL.Traces;

with Glib;                          use Glib;
with Glib.Convert;                  use Glib.Convert;
with Glib.Object;
with Glib.Values;

with Gtkada.Style;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Handlers;                  use Gtk.Handlers;
with Gtk.Label;                     use Gtk.Label;
with Gtk.Separator;                 use Gtk.Separator;

with Entities_Tooltips;             use Entities_Tooltips;
with GUI_Utils;                     use GUI_Utils;
with GPS.LSP_Client.Requests;       use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Hover; use GPS.LSP_Client.Requests.Hover;
with GPS.LSP_Module;                use GPS.LSP_Module;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;        use GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;      use GPS.Kernel.Style_Manager;
with Language;                      use Language;
with LAL.Module;
with LAL.Core_Module;
with LAL.Highlighters;
with Libadalang.Analysis;
with Libadalang.Common;
with Langkit_Support.Text;
with Outline_View;                  use Outline_View;
with Tooltips;                      use Tooltips;
with Xref;                          use Xref;

package body GPS.LSP_Client.Editors.Tooltips is

   Me : constant Trace_Handle := Create
     ("GPS.LSP.TOOLTIPS", GNATCOLL.Traces.On);

   Show_Tooltip_After_Query : Boolean := True;
   --  Flag used to know whether we want to display the tooltip immediately
   --  after a tooltip query.
   --  This is True by default, unless we are hovering on an entity: in that
   --  case, an LSP request will be sent and we'll only be able to show the
   --  tooltip after receiving the result from the underlying language server.

   type LSP_Client_Editor_Tooltip_Handler is new Editor_Tooltip_Handler with
     null record;
   --  Type representing LSP-based tooltip handlers.

   type GPS_LSP_Hover_Request is new Abstract_Hover_Request with record
      Kernel                       : Kernel_Handle;

      Tooltip_Hbox                 : Gtk_Hbox;
      --  The box containing the tooltip text blocks

      Tooltip_Destroyed_Handler_ID : Handler_Id;
      --  The handler on signal-destroy used to detect a tooltip destruction
      --  while waiting for the hover request result.
   end record;
   type GPS_LSP_Hover_Request_Access is access all GPS_LSP_Hover_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Hover_Request;
      Result : LSP.Messages.Hover);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Hover_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected (Self : in out GPS_LSP_Hover_Request);

   overriding function Get_Tooltip_Widget_For_Entity
     (Tooltip : not null access LSP_Client_Editor_Tooltip_Handler;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget;

   overriding function Show_Tooltip_On_Create_Contents
     (Tooltip : not null access LSP_Client_Editor_Tooltip_Handler)
      return Boolean
   is
     (Show_Tooltip_After_Query);

   package Tooltip_Destroyed_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type  => Gtk_Widget_Record,
      User_Type    => GPS_LSP_Hover_Request_Access);

   procedure On_Tooltip_Destroyed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : GPS_LSP_Hover_Request_Access);
   --  Called when the tooltip to be displayed gets detroyed while waiting for
   --  the hover request result.

   type Highlightable_Tooltip_Label_Type is
     new Gtk_Label_Record and LAL.Highlighters.Highlightable_Interface with
      record
         Kernel      : Kernel_Handle;
         Markup_Text : Unbounded_String;
      end record;
   --  A type of label that implements the LAL highlightable interface to
   --  highlight the declarations displayed in tooltips.

   type Highlightable_Tooltip_Label_Type_Access is
     access all Highlightable_Tooltip_Label_Type'Class;

   overriding procedure Highlight_Token
     (Self  : in out Highlightable_Tooltip_Label_Type;
      Token : Libadalang.Common.Token_Reference;
      Style : String);

   overriding procedure Remove_Highlighting
     (Self  : in out Highlightable_Tooltip_Label_Type;
      Style : String;
      From  : Integer;
      To    : Integer);

   function Query_Tooltip_For_Entity
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer;
      Column : Visible_Column) return Gtk_Widget;
   --  Query a tooltip for the given entity by sending the LSP
   --  textDocument/hover request.

   --------------------------
   -- On_Tooltip_Destroyed --
   --------------------------

   procedure On_Tooltip_Destroyed
     (Widget    : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues;
      User_Data : GPS_LSP_Hover_Request_Access)
   is
      pragma Unreferenced (Widget, Params);
   begin
      User_Data.Tooltip_Hbox := null;
   end On_Tooltip_Destroyed;

   ------------------------------
   -- Query_Tooltip_For_Entity --
   ------------------------------

   function Query_Tooltip_For_Entity
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer;
      Column : Visible_Column) return Gtk_Widget
   is
      Request      : GPS_LSP_Hover_Request_Access;
      Tooltip_Hbox : Gtk_Hbox;
      Buffer       : constant GPS.Editors.Editor_Buffer'Class :=
                       Kernel.Get_Buffer_Factory.Get
                         (File        => File,
                          Open_Buffer => False,
                          Open_View   => False,
                          Force       => False);
   begin
      Show_Tooltip_After_Query := False;

      Gtk_New_Hbox (Tooltip_Hbox, Homogeneous => False);

      Request := new GPS_LSP_Hover_Request'
        (LSP_Request with
         Text_Document                => File,
         Line                         => Line,
         Column                       => Column,
         Kernel                       => Kernel_Handle (Kernel),
         Tooltip_Hbox                 => Tooltip_Hbox,
         Tooltip_Destroyed_Handler_ID => <>);

      Request.Tooltip_Destroyed_Handler_ID :=
        Tooltip_Destroyed_Callback.Object_Connect
          (Tooltip_Hbox, Signal_Destroy,
           On_Tooltip_Destroyed'Access,
           Slot_Object => Tooltip_Hbox,
           User_Data   => Request);

      Trace
        (Me, "Tooltip about to be displayed: sending the hover request");
      GPS.LSP_Client.Requests.Execute
        (Buffer.Get_Language, Request_Access (Request));

      return Gtk_Widget (Tooltip_Hbox);
   end Query_Tooltip_For_Entity;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Hover_Request;
      Result : LSP.Messages.Hover)
   is
      use LSP.Types;

      Tooltip_Block_Label : Highlightable_Tooltip_Label_Type_Access;
      Vbox                : Gtk_Vbox;
      Hsep                : Gtk_Hseparator;
   begin
      --  If the tooltip has been destroyed before the response, return
      --  directly.
      if Self.Tooltip_Hbox = null then
         return;
      end if;

      --  Disconnect the callback on the tooltip's destruction now that we
      --  received the response.
      Disconnect
        (Object => Self.Tooltip_Hbox,
         Id     => Self.Tooltip_Destroyed_Handler_ID);

      --  Append the contents to the tooltip or "No data available" when empty

      Remove_All_Children (Self.Tooltip_Hbox);

      if not Result.contents.Is_Empty then
         Trace (Me, "Non-empty response received on hover request");

         Gtk_New_Vbox (Vbox, Homogeneous => False);
         Self.Tooltip_Hbox.Pack_Start (Vbox);

         for Tooltip_Block of Result.contents loop
            if Tooltip_Block_Label /= null then
               Gtk_New_Hseparator (Hsep);
               Vbox.Pack_Start (Hsep);
            end if;

            Tooltip_Block_Label := new Highlightable_Tooltip_Label_Type'
              (Glib.Object.GObject_Record with
               Kernel      => Self.Kernel,
               Markup_Text => <>);
            Gtk.Label.Initialize (Tooltip_Block_Label);
            Tooltip_Block_Label.Set_Alignment (0.0, 0.5);

            Set_Font_And_Colors
              (Widget     => Tooltip_Block_Label,
               Fixed_Font => True);
            Vbox.Pack_Start (Tooltip_Block_Label, Expand => False);

            --  If the tooltip block is a simple string, display it as it is.
            --  Otherwise, if a language is specified, try to highlight the
            --  tooltip block (this only works for Ada currently).

            if Tooltip_Block.Is_String then
               Tooltip_Block_Label.Set_Use_Markup (False);
               Tooltip_Block_Label.Set_Text
                 (To_UTF_8_String (Tooltip_Block.value));
            elsif To_UTF_8_String (Tooltip_Block.language) = "ada" then
               declare
                  use Libadalang.Analysis;
                  use Libadalang.Common;
                  use LAL.Core_Module;

                  LAL_Module : constant LAL.Core_Module.LAL_Module_Id :=
                                 LAL.Module.Get_LAL_Core_Module;
                  Unit       : constant Analysis_Unit :=
                           Get_From_Buffer
                             (Context  =>
                                 LAL_Module.Get_Current_Analysis_Context,
                              Filename => "",
                              Charset  => "UTF-8",
                              Buffer   => To_UTF_8_String
                                (Tooltip_Block.value),
                              Rule     => Basic_Decl_Rule);
                  Success    : Boolean;
               begin
                  Success := Tooltip_Block_Label.Highlight_Using_Tree
                    (Unit => Unit);

                  --  If we failed to highlight the given Ada code, display it
                  --  wihout any highlighting instead.

                  if Success then
                     Tooltip_Block_Label.Set_Markup
                       (To_String (Tooltip_Block_Label.Markup_Text));
                  else
                     Tooltip_Block_Label.Set_Use_Markup (False);
                     Tooltip_Block_Label.Set_Text
                       (To_UTF_8_String (Tooltip_Block.value));
                  end if;
               end;
            end if;
         end loop;
      else
         Trace (Me, "Empty reponse received on hover request");
      end if;

      Show_Finalized_Tooltip;
      Show_Tooltip_After_Query := True;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Hover_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Code, Self);
   begin
      Trace (Me, "Error received on hover request: " & Message);
      Trace (Me, "Data: " & GNATCOLL.JSON.Write (Data));
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out GPS_LSP_Hover_Request) is
      pragma Unreferenced (Self);
   begin
      Trace (Me, "The hover request has been rejected");
   end On_Rejected;

   ----------------------------------------------
   -- Create_LSP_Client_Editor_Tooltip_Handler --
   ----------------------------------------------

   function Create_LSP_Client_Editor_Tooltip_Handler
     (Box : not null access Source_Editor_Box_Record'Class)
      return Editor_Tooltip_Handler_Access
   is
      Tooltip : constant Editor_Tooltip_Handler_Access :=
                  new LSP_Client_Editor_Tooltip_Handler;
   begin
      Tooltip.Set_Source_Editor_Box (Box);

      return Tooltip;
   end Create_LSP_Client_Editor_Tooltip_Handler;

   -----------------------------------
   -- Get_Tooltip_Widget_For_Entity --
   -----------------------------------

   overriding function Get_Tooltip_Widget_For_Entity
     (Tooltip : not null access LSP_Client_Editor_Tooltip_Handler;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      File   : constant GNATCOLL.VFS.Virtual_File :=
                       File_Information (Context);
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Kernel.Get_Buffer_Factory.Get
                   (File        => File,
                    Open_Buffer => False,
                    Open_View   => False,
                    Force       => False);
      Lang   : constant Language_Access := Buffer.Get_Language;
   begin

      --  Send the LSP textDocument/hover request only if the LSP is enabled
      --  for the current buffer. Fallback on the old entities tooltip handler
      --  based on xrefs ontherwise.

      if LSP_Is_Enabled (Lang) then
         return Query_Tooltip_For_Entity
           (Kernel => Kernel,
            File   => File,
            Line   => Integer (Entity_Line_Information (Context)),
            Column => Entity_Column_Information (Context));
      else
         return Editor_Tooltip_Handler
           (Tooltip.all).Get_Tooltip_Widget_For_Entity (Context);
      end if;
   end Get_Tooltip_Widget_For_Entity;

   ---------------------
   -- Highlight_Token --
   ---------------------

   overriding procedure Highlight_Token
     (Self  : in out Highlightable_Tooltip_Label_Type;
      Token : Libadalang.Common.Token_Reference;
      Style : String)
   is
      use Libadalang.Common;
      use Langkit_Support.Text;

      Highlight_Style : constant Style_Access :=
                          Get_Style_Manager (Self.Kernel).Get
                          (Key        => Style,
                           Allow_Null => True);
   begin
      if Highlight_Style = null then
         Self.Markup_Text := Self.Markup_Text
           & Escape_Text (To_UTF8 (Text (Token)));
      else
         Self.Markup_Text := Self.Markup_Text & "<span foreground="""
           & Gtkada.Style.To_Hex (Get_Foreground (Highlight_Style))
           & """>"
           & Escape_Text (To_UTF8 (Text (Token)))
           & "</span>";
      end if;
   end Highlight_Token;

   -------------------------
   -- Remove_Highlighting --
   -------------------------

   overriding procedure Remove_Highlighting
     (Self  : in out Highlightable_Tooltip_Label_Type;
      Style : String;
      From  : Integer;
      To    : Integer) is null;

   ---------------------------------
   -- LSP_Outline_Tooltip_Factory --
   ---------------------------------

   function LSP_Outline_Tooltip_Factory
     (Kernel      : not null access Kernel_Handle_Record'Class;
      File        : GNATCOLL.VFS.Virtual_File;
      Entity_Name : String;
      Line        : Integer;
      Column      : Visible_Column) return Gtk_Widget
   is
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Kernel.Get_Buffer_Factory.Get
                   (File        => File,
                    Open_Buffer => False,
                    Open_View   => False,
                    Force       => False);
      Lang   : constant Language_Access := Buffer.Get_Language;
   begin
      if LSP_Is_Enabled (Lang) then
         Set_Outline_Tooltips_Synchronous (False);

         return Query_Tooltip_For_Entity
           (Kernel => Kernel,
            File   => File,
            Line   => Line,
            Column => Column);
      else
         declare
            Ref    : Root_Entity_Reference_Ref;
            Entity : constant Root_Entity'Class :=
                       Kernel.Databases.Get_Entity
                          (Name        => Entity_Name,
                           Closest_Ref => Ref,
                           Loc         =>
                            (File   => File,
                             Line   => Line,
                             Column => Column,
                             others => <>));
         begin
            Set_Outline_Tooltips_Synchronous (True);

            if Entity /= No_Root_Entity then
               return Entities_Tooltips.Draw_Tooltip
                 (Kernel      => Kernel,
                  Draw_Border => True,
                  Ref         => Ref.Element,
                  Entity      => Entity);
            else
               return null;
            end if;
         end;
      end if;
   end LSP_Outline_Tooltip_Factory;

end GPS.LSP_Client.Editors.Tooltips;
