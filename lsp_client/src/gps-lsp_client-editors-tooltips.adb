------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2026, AdaCore                  --
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

with Ada.Strings.Unbounded;

with GNATCOLL.JSON;
with GNATCOLL.Traces;               use GNATCOLL.Traces;

with GPS.LSP_Client.Editors.Semantic_Tokens;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;

with Glib;                          use Glib;
with Glib.Convert.VSS_Utils;        use Glib.Convert.VSS_Utils;
with Glib.Object;                   use Glib.Object;
with Glib.Values;

with Gdk.Event;                     use Gdk.Event;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Event_Box;                 use Gtk.Event_Box;
with Gtk.Handlers;                  use Gtk.Handlers;
with Gtk.Label;                     use Gtk.Label;
with Gtk.Separator;                 use Gtk.Separator;
with Gtkada.Style;

with VSS.Strings.Conversions;

with Entities_Tooltips;             use Entities_Tooltips;
with GUI_Utils;                     use GUI_Utils;
with GPS.LSP_Client.Utilities;
with GPS.LSP_Client.Requests;       use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Hover; use GPS.LSP_Client.Requests.Hover;
with GPS.LSP_Module;                use GPS.LSP_Module;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;        use GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;      use GPS.Kernel.Style_Manager;
with Language;                      use Language;
with Language_Handlers;             use Language_Handlers;
with LAL.Module;
with LAL.Core_Module;
with LAL.Highlighters;
with Libadalang.Analysis;
with Libadalang.Common;
with Outline_View;                  use Outline_View;
with String_Utils;
with Tooltips;                      use Tooltips;
with Xref;                          use Xref;

package body GPS.LSP_Client.Editors.Tooltips is

   Me : constant Trace_Handle := Create
     ("GPS.LSP.TOOLTIPS", GNATCOLL.Traces.On);

   Max_Highlighting_Chars : constant := 10_000;
   --  Do not try to use LAL to highlight tooltips that exceed 10_000
   --  characters, since it can be slow.

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
      Tooltip_Vbox                 : Gtk_Vbox;
      --  The box containing the tooltip text blocks

      Tooltip_Destroyed_Handler_ID : Handler_Id;
      --  The handler on signal-destroy used to detect a tooltip destruction
      --  while waiting for the hover request result.

      For_Global_Tooltips          : Boolean;
      --  True if this request is being made for global tooltips, False
      --  otherwise.

      --  Settings for representation adjusting
      Xalign                       : Glib.Gfloat;
      Yalign                       : Glib.Gfloat;
      Font                         : Pango.Font.Pango_Font_Description;
      Separator_Expand             : Boolean;
      Separator_Padding            : Guint;
   end record;
   type GPS_LSP_Hover_Request_Access is access all GPS_LSP_Hover_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Hover_Request;
      Result : LSP.Messages.Optional_Hover);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Hover_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Hover_Request; Reason : Reject_Reason);

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

   function On_Tooltip_Label_Clicked
     (Self  : access GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;

   procedure On_Tooltip_Label_Hidden
     (Self : access Gtk_Widget_Record'Class);

   type Highlightable_Tooltip_Label_Type is
     new Gtk_Label_Record and LAL.Highlighters.Highlightable_Interface with
      record
         Kernel      : Kernel_Handle;
         Markup_Text : VSS.Strings.Virtual_String;
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
      if User_Data /= null then
         User_Data.Tooltip_Vbox := null;
      end if;
   end On_Tooltip_Destroyed;

   ------------------------------
   -- On_Tooltip_Label_Clicked --
   ------------------------------

   function On_Tooltip_Label_Clicked
     (Self  : access GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Event);
      Tooltip_Label : constant Highlightable_Tooltip_Label_Type_Access :=
        Highlightable_Tooltip_Label_Type_Access (Self);
   begin
      Tooltip_Label.Set_Selectable (True);
      Set_Tooltip_Highlighted (True);
      Set_Tooltip_Clipboard_Widget (Gtk_Widget (Tooltip_Label));

      return False;
   end On_Tooltip_Label_Clicked;

   -----------------------------
   -- On_Tooltip_Label_Hidden --
   -----------------------------

   procedure On_Tooltip_Label_Hidden
     (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Set_Tooltip_Highlighted (False);
   end On_Tooltip_Label_Hidden;

   ------------------------------
   -- Query_Tooltip_For_Entity --
   ------------------------------

   function Query_Tooltip_For_Entity
     (Kernel              : not null access Kernel_Handle_Record'Class;
      File                : GNATCOLL.VFS.Virtual_File;
      Line                : Integer;
      Column              : Visible_Column_Type;
      For_Global_Tooltips : Boolean := True;
      Xalign              : Glib.Gfloat := 0.0;
      Yalign              : Glib.Gfloat := 0.5;
      Font                : Pango.Font.Pango_Font_Description := null;
      Separator_Expand    : Boolean := False;
      Separator_Padding   : Guint := 0)
      return Gtk_Widget
   is
      Request            : GPS_LSP_Hover_Request_Access;
      Tooltip_Vbox       : Gtk_Vbox;
      Lang               : constant Language.Language_Access :=
        Get_Language_From_File
          (Kernel.Get_Language_Handler,
           File);
      Holder   : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Kernel.Get_Buffer_Factory.Get_Holder (File => File);
      Location : constant GPS.Editors.Editor_Location'Class :=
        Holder.Editor.New_Location (Line, Column);

   begin
      if Line = 0 then
         --  This is a special line => do nothing
         return null;
      end if;

      Show_Tooltip_After_Query := False;

      Gtk_New_Vbox (Tooltip_Vbox, Homogeneous => False);

      Request := new GPS_LSP_Hover_Request'
        (LSP_Request with
           Kernel                       => Kernel_Handle (Kernel),
         File                         => File,
         Position                     =>
           GPS.LSP_Client.Utilities.Location_To_LSP_Position (Location),
         Tooltip_Vbox                 => Tooltip_Vbox,
         Tooltip_Destroyed_Handler_ID => <>,
         For_Global_Tooltips          => For_Global_Tooltips,
         Xalign                       => Xalign,
         Yalign                       => Yalign,
         Font                         => Font,
         Separator_Expand             => Separator_Expand,
         Separator_Padding            => Separator_Padding);

      Request.Tooltip_Destroyed_Handler_ID :=
        Tooltip_Destroyed_Callback.Object_Connect
          (Tooltip_Vbox, Signal_Destroy,
           On_Tooltip_Destroyed'Access,
           Slot_Object => Tooltip_Vbox,
           User_Data   => Request);

      Trace
        (Me, "Tooltip about to be displayed: sending the hover request");

      if GPS.LSP_Client.Requests.Execute
        (Lang, Request_Access (Request))
      then
         --  Hover request is being sent: create the tooltip area
         --  for the semantic token information if the preference
         --  is enabled.
         GPS.LSP_Client.Editors.Semantic_Tokens.Create_Semantic_Token_Tooltip
           (Tooltip_Hbox => Tooltip_Vbox,
            File         => File,
            Line         => Line,
            Column       => Column);

         return Gtk_Widget (Tooltip_Vbox);
      else
         return null;
      end if;
   end Query_Tooltip_For_Entity;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Hover_Request;
      Result : LSP.Messages.Optional_Hover)
   is
      use LSP.Messages;
      use type Pango.Font.Pango_Font_Description;
      use type VSS.Strings.Virtual_String;

      Tooltip_Block_Label : Highlightable_Tooltip_Label_Type_Access;
      Vbox                : Gtk_Vbox;
      Hsep                : Gtk_Hseparator;
      Max_Width_Chars     : constant := 80;

      procedure New_Tooltip_Block_Label;
      --  Creates new Tooltip_Block_Label

      procedure New_Tooltip_Block_Label is
         Event_Box : Gtk_Event_Box;
      begin
         if Tooltip_Block_Label /= null then
            Gtk_New_Hseparator (Hsep);
            Vbox.Pack_Start
              (Hsep,
               Expand  => Self.Separator_Expand,
               Padding => Self.Separator_Padding);
         end if;

         Tooltip_Block_Label := new Highlightable_Tooltip_Label_Type'
           (Glib.Object.GObject_Record with
              Kernel      => Self.Kernel,
            Markup_Text => <>);
         Gtk.Label.Initialize (Tooltip_Block_Label);

         Tooltip_Block_Label.Set_Alignment (Self.Xalign, Self.Yalign);
         if Self.Font = null then
            Set_Font_And_Colors
              (Widget     => Tooltip_Block_Label,
               Fixed_Font => True);
         else
            Tooltip_Block_Label.Modify_Font (Self.Font);
         end if;
         Gtk_New (Event_Box);
         Event_Box.Add_Events (Button_Press_Mask);
         Event_Box.On_Button_Press_Event
           (On_Tooltip_Label_Clicked'Access, Tooltip_Block_Label);
         Tooltip_Block_Label.On_Destroy (On_Tooltip_Label_Hidden'Access);
         Event_Box.Add (Tooltip_Block_Label);
         Vbox.Pack_Start (Event_Box, Expand => False);
      end New_Tooltip_Block_Label;

   begin
      --  If the tooltip has been destroyed before the response, return
      --  directly.
      if Self.Tooltip_Vbox = null then
         return;
      end if;

      --  Disconnect the callback on the tooltip's destruction now that we
      --  received the response.
      Disconnect
        (Object => Self.Tooltip_Vbox,
         Id     => Self.Tooltip_Destroyed_Handler_ID);

      --  Append the contents to the tooltip or "No data available" when empty

      if Result.Is_Set and then Result.Value.contents.Is_MarkupContent then
         if Result.Value.contents.MarkupContent.kind = plaintext then
            Gtk_New_Vbox (Vbox, Homogeneous => False);
            Self.Tooltip_Vbox.Pack_Start (Vbox);

            New_Tooltip_Block_Label;

            Tooltip_Block_Label.Set_Use_Markup (False);
            Tooltip_Block_Label.Set_Text
              (Ada.Strings.Unbounded.To_String
                 (String_Utils.Wrap_At_Words
                      (S     => VSS.Strings.Conversions.To_UTF_8_String
                           (Result.Value.contents.MarkupContent.value),
                       Limit => Max_Width_Chars)));

         else
            Trace
              (Me, "MarkupContent.markdown in hover reponse not supported");
         end if;

      elsif Result.Is_Set and then not Result.Value.contents.Vector.Is_Empty
      then
         Trace (Me, "Non-empty response received on hover request");

         Gtk_New_Vbox (Vbox, Homogeneous => False);
         Self.Tooltip_Vbox.Pack_Start (Vbox);

         for Tooltip_Block of Result.Value.contents.Vector loop
            New_Tooltip_Block_Label;

            --  If language is specified for the tooltip block and it is "ada",
            --  try to highlight this block. Otherwise process tooltip block
            --  as plaintext.
            if not Tooltip_Block.Is_String
              and then Tooltip_Block.language = "ada"
              and then Integer
                (Tooltip_Block.value.Character_Length) < Max_Highlighting_Chars
            then
               declare
                  use Libadalang.Analysis;
                  use Libadalang.Common;
                  use LAL.Core_Module;

                  LAL_Module   : constant LAL.Core_Module.LAL_Module_Id :=
                    LAL.Module.Get_LAL_Core_Module;
                  Tooltip_Text : constant String :=
                    VSS.Strings.Conversions.To_UTF_8_String
                      (Tooltip_Block.value);
                  Unit         : constant Analysis_Unit :=
                    Get_From_Buffer
                      (Context  =>
                         LAL_Module.Get_Current_Analysis_Context,
                       Filename => "",
                       Charset  => "UTF-8",
                       Buffer   =>
                         Ada.Strings.Unbounded.To_String
                           (String_Utils.Wrap_At_Words
                              (S     => Tooltip_Text,
                               Limit => Max_Width_Chars)),
                       Rule     => Basic_Decl_Rule);
                  Success    : Boolean;
               begin
                  Success := Tooltip_Block_Label.Highlight_Using_Tree
                    (Unit => Unit);

                  --  If we failed to highlight the given Ada code, display it
                  --  wihout any highlighting instead.

                  if Success then
                     Tooltip_Block_Label.Set_Markup
                       (VSS.Strings.Conversions.To_UTF_8_String
                          (Tooltip_Block_Label.Markup_Text));
                  else
                     Tooltip_Block_Label.Set_Use_Markup (False);
                     Tooltip_Block_Label.Set_Text
                       (Ada.Strings.Unbounded.To_String
                          (String_Utils.Wrap_At_Words
                               (S     => Tooltip_Text,
                                Limit => Max_Width_Chars)));
                  end if;
               end;

            else
               Tooltip_Block_Label.Set_Use_Markup (False);
               Tooltip_Block_Label.Set_Text
                 (Ada.Strings.Unbounded.To_String
                    (String_Utils.Wrap_At_Words
                         (S     => VSS.Strings.Conversions.To_UTF_8_String
                              (Tooltip_Block.value),
                          Limit => Max_Width_Chars)));
            end if;
         end loop;
      else
         Trace (Me, "Empty response received on hover request");
         Show_Tooltip_After_Query := True;
         return;
      end if;

      if Self.For_Global_Tooltips then
         Show_Finalized_Tooltip;
      else
         if Self.Tooltip_Vbox /= null then
            Self.Tooltip_Vbox.Show_All;
         end if;
      end if;

      Show_Tooltip_After_Query := True;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Hover_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      --  Disconnect the callback on the tooltip's destruction now that we
      --  received an error response.
      if Self.Tooltip_Vbox /= null then
         Disconnect
           (Object => Self.Tooltip_Vbox,
            Id     => Self.Tooltip_Destroyed_Handler_ID);
      end if;
      Trace
        (Me,
         "Error received on hover request: "
         & VSS.Strings.Conversions.To_UTF_8_String (Message));
      Trace (Me, "Data: " & GNATCOLL.JSON.Write (Data));
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Hover_Request; Reason : Reject_Reason) is
   begin
      --  Disconnect the callback on the tooltip's destruction now that we
      --  got rejected
      if Self.Tooltip_Vbox /= null then
         Disconnect
           (Object => Self.Tooltip_Vbox,
            Id     => Self.Tooltip_Destroyed_Handler_ID);
      end if;

      Trace
        (Me,
         "The hover request has been rejected with reason: "
         & Reject_Reason'Image (Reason));
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

      function Is_LSP_Tooltips_Enabled return Boolean;
      function Is_LSP_Tooltips_Enabled return Boolean
      is
         Capabilities : LSP.Messages.ServerCapabilities;
      begin
         if LSP_Is_Enabled (Lang) then
            Capabilities := GPS.LSP_Module.Get_Language_Server
              (Lang).Get_Client.Capabilities;

            return Capabilities.hoverProvider.Is_Set;

         else
            return False;
         end if;
      end Is_LSP_Tooltips_Enabled;

   begin

      --  Send the LSP textDocument/hover request only if the LSP is enabled
      --  for the current buffer. Fallback on the old entities tooltip handler
      --  based on xrefs ontherwise.

      if Is_LSP_Tooltips_Enabled then
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
      use type VSS.Strings.Virtual_String;

      Template        : constant
        VSS.Strings.Templates.Virtual_String_Template :=
          "<span foreground=""{}"">{}</span>";
      Highlight_Style : constant Style_Access :=
        Get_Style_Manager (Self.Kernel).Get
        (Key        => Style,
         Allow_Null => True);
   begin
      if Highlight_Style = null then
         Self.Markup_Text := Self.Markup_Text & Escape_Text (Text (Token));
      else
         Self.Markup_Text :=
           Self.Markup_Text
           & Template.Format
           (VSS.Strings.Formatters.Strings.Image
              (VSS.Strings.Conversions.To_Virtual_String
                   (Gtkada.Style.To_Hex (Get_Foreground (Highlight_Style)))),
           VSS.Strings.Formatters.Strings.Image
              (Escape_Text (Text (Token))));
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
      Column      : Visible_Column_Type) return Gtk_Widget
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
