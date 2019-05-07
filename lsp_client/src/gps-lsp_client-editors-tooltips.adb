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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.Traces;               use GNATCOLL.Traces;

with Glib;                          use Glib;
with Glib.Object;
with Glib.Values;

with Gtkada.Style;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Handlers;                  use Gtk.Handlers;
with Gtk.Label;                     use Gtk.Label;
with Gtk.Widget;                    use Gtk.Widget;

with GUI_Utils;                     use GUI_Utils;
with GPS.LSP_Client.Requests;       use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Hover; use GPS.LSP_Client.Requests.Hover;
with GPS.Kernel;                    use GPS.Kernel;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;        use GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;      use GPS.Kernel.Style_Manager;
with LAL.Module;
with LAL.Core_Module;
with LAL.Highlighters;
with Libadalang.Analysis;
with Libadalang.Common;
with Langkit_Support.Text;

package body GPS.LSP_Client.Editors.Tooltips is

   Me : constant Trace_Handle := Create
     ("GPS.LSP.TOOLTIPS", GNATCOLL.Traces.On);

   No_Data_Label : constant String := "No data available";

   type LSP_Client_Editor_Tooltip_Handler is new Editor_Tooltip_Handler with
     null record;
   --  Type representing LSP-based tooltip handlers.

   type GPS_LSP_Hover_Request is new Abstract_Hover_Request with record
      Kernel                       : Kernel_Handle;

      Tooltip_Vbox                 : Gtk_Vbox;
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

   overriding procedure Highlight_Token
     (Self  : in out Highlightable_Tooltip_Label_Type;
      Token : Libadalang.Common.Token_Reference;
      Style : String);

   overriding procedure Remove_Highlighting
     (Self  : in out Highlightable_Tooltip_Label_Type;
      Style : String;
      From  : Integer;
      To    : Integer);

   procedure On_Erroneous_Reponse (Self : GPS_LSP_Hover_Request'Class);
   --  Display a 'No data available' label on the tooltip when receiving
   --  erroneous reponses (e.g: errors or empty responses).

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
      User_Data.Tooltip_Vbox := null;
   end On_Tooltip_Destroyed;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Hover_Request;
      Result : LSP.Messages.Hover)
   is
      use LSP.Types;

      Tooltip_Block_Label : access Highlightable_Tooltip_Label_Type;
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

      Remove_All_Children (Self.Tooltip_Vbox);

      if not Result.contents.Is_Empty then
         Trace (Me, "Non-empty responce received on hover request");

         for Tooltip_Block of Result.contents loop
            Tooltip_Block_Label := new Highlightable_Tooltip_Label_Type'
              (Glib.Object.GObject_Record with
               Kernel      => Self.Kernel,
               Markup_Text => <>);
            Gtk.Label.Initialize (Tooltip_Block_Label);

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
                              Buffer   => To_UTF_8_String
                                (Tooltip_Block.value),
                              Rule     => Basic_Decl_Rule);
               begin
                  Tooltip_Block_Label.Highlight_Using_Tree (Unit => Unit);
                  Tooltip_Block_Label.Set_Markup
                    (To_String (Tooltip_Block_Label.Markup_Text));
               end;
            end if;

            Set_Font_And_Colors
              (Widget     => Tooltip_Block_Label,
               Fixed_Font => True);
            Self.Tooltip_Vbox.Pack_Start (Tooltip_Block_Label);
         end loop;
      else
         Trace (Me, "Empty reponse received on hover request");
         Self.On_Erroneous_Reponse;
      end if;

      Self.Tooltip_Vbox.Show_All;
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
      pragma Unreferenced (Code);
   begin
      Trace (Me, "Error received on hover request: " & Message);
      Trace (Me, "Data: " & GNATCOLL.JSON.Write (Data));
      Self.On_Erroneous_Reponse;
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out GPS_LSP_Hover_Request) is
   begin
      Trace (Me, "The hover request has been rejected");
      Self.On_Erroneous_Reponse;
   end On_Rejected;

   ----------------------------------------------
   -- Create_LSP_Client_Editor_Tooltip_Handler --
   ----------------------------------------------

   function Create_LSP_Client_Editor_Tooltip_Handler
     (Box : not null access Source_Editor_Box_Record'Class)
      return Editor_Tooltip_Handler_Access
   is
      Tooltip : constant access LSP_Client_Editor_Tooltip_Handler :=
                  new LSP_Client_Editor_Tooltip_Handler;
   begin
      Tooltip.Set_Source_Editor_Box (Box);

      return Editor_Tooltip_Handler_Access (Tooltip);
   end Create_LSP_Client_Editor_Tooltip_Handler;

   -----------------------------------
   -- Get_Tooltip_Widget_For_Entity --
   -----------------------------------

   overriding function Get_Tooltip_Widget_For_Entity
     (Tooltip : not null access LSP_Client_Editor_Tooltip_Handler;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Tooltip);
      Kernel       : constant Kernel_Handle := Get_Kernel (Context);
      File         : constant GNATCOLL.VFS.Virtual_File :=
                       File_Information (Context);
      Buffer       : constant GPS.Editors.Editor_Buffer'Class :=
                       Kernel.Get_Buffer_Factory.Get
                         (File        => File,
                          Open_Buffer => False,
                          Open_View   => False,
                          Force       => False);
      Request      : GPS_LSP_Hover_Request_Access;
      Tooltip_Vbox : Gtk_Vbox;
      Label        : Gtk_Label;
   begin
      Gtk_New_Vbox (Tooltip_Vbox);
      Gtk_New (Label, "Loading...");
      Tooltip_Vbox.Pack_Start (Label, Expand => False);

      Request := new GPS_LSP_Hover_Request'
        (LSP_Request with
           Text_Document                => File,
         Line                         => Line_Information (Context),
         Column                       => Column_Information (Context),
         Kernel                       => Kernel,
         Tooltip_Vbox                 => Tooltip_Vbox,
         Tooltip_Destroyed_Handler_ID => <>);

      Request.Tooltip_Destroyed_Handler_ID :=
        Tooltip_Destroyed_Callback.Object_Connect
          (Tooltip_Vbox, Signal_Destroy,
           On_Tooltip_Destroyed'Access,
           Slot_Object => Tooltip_Vbox,
           User_Data   => Request);

      Trace (Me, "Tooltip about to be displayed: sending the hover request");
      GPS.LSP_Client.Requests.Execute
        (Buffer.Get_Language, Request_Access (Request));

      return Gtk_Widget (Tooltip_Vbox);
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
         Self.Markup_Text := Self.Markup_Text & To_UTF8 (Text (Token));
      else
         Self.Markup_Text := Self.Markup_Text & "<span foreground="""
           & Gtkada.Style.To_Hex (Get_Foreground (Highlight_Style))
           & """>"
           & To_UTF8 (Text (Token))
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

   ------------------------------------
   -- Create_Erroneous_Reponse_Label --
   ------------------------------------

   procedure On_Erroneous_Reponse (Self : GPS_LSP_Hover_Request'Class)
   is
      Erroneous_Label : access Highlightable_Tooltip_Label_Type;
   begin
      Remove_All_Children (Self.Tooltip_Vbox);

      Erroneous_Label := new Highlightable_Tooltip_Label_Type'
        (Glib.Object.GObject_Record with
           Kernel      => Self.Kernel,
         Markup_Text => <>);
      Gtk.Label.Initialize
        (Erroneous_Label,
         Str => No_Data_Label);

      Set_Font_And_Colors
        (Widget     => Erroneous_Label,
         Fixed_Font => True);
      Self.Tooltip_Vbox.Pack_Start (Erroneous_Label);

      Self.Tooltip_Vbox.Show_All;
   end On_Erroneous_Reponse;

end GPS.LSP_Client.Editors.Tooltips;
