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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Glib.Convert;
with Glib.Object;                     use Glib.Object;
with Glib.Values;
with Glib;                            use Glib;
with GNAT.Strings;                    use GNAT.Strings;
with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Symbols;                use GNATCOLL.Symbols;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GPS.Intl;                        use GPS.Intl;
with GPS.Kernel.Actions;              use GPS.Kernel.Actions;
with GPS.Kernel.MDI;                  use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;                use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;          use GPS.Kernel.Preferences;
with GPS.Kernel;                      use GPS.Kernel;
with GPS.Stock_Icons;                 use GPS.Stock_Icons;
with GPS.VCS;                         use GPS.VCS;
with Gtk.Arguments;                   use Gtk.Arguments;
with Gtk.Enums;                       use Gtk.Enums;
with Gtk.Handlers;                    use Gtk.Handlers;
with Gtk.Label;                       use Gtk.Label;
with Gtk.Style_Context;               use Gtk.Style_Context;
with Gtk.Text_Iter;                   use Gtk.Text_Iter;
with Gtkada.Handlers;
with Gtkada.MDI;                      use Gtkada.MDI;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Language;                        use Language;
with Pango.Layout;                    use Pango.Layout;
with Src_Editor_Box;                  use Src_Editor_Box;
with Src_Editor_Module.Commands;      use Src_Editor_Module.Commands;
with Src_Editor_Module.Markers;       use Src_Editor_Module.Markers;
with Src_Editor_Module;               use Src_Editor_Module;
with String_Utils;

package body Src_Editor_Status_Bar is

   Extra_Info_Pos : constant := 1;
   --  Position in the toolbar of the extra items that are inserted for VCS
   --  status.

   procedure Setup (Data : Source_Editor_Status_Bar; Id : Handler_Id);
   package Bar_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Source_Editor_Status_Bar,
      Setup       => Setup);

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar);
   --  This handler is merely a proxy to Show_Cursor_Position. It just
   --  extracts the necessary values from Params, and pass them on to
   --  Show_Cursor_Position.

   procedure Show_Cursor_Position
     (Bar    : access Source_Editor_Status_Bar_Record'Class;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type);
   --  Redraw the cursor position in the Line/Column areas of the status bar

   procedure On_Read_Only_Pressed (Ob : access GObject_Record'Class);
   --  Toggle read-only/writable state of a given box

   function On_Subprogram_Link
     (Ob  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args : Gtk_Args) return Boolean;
   --  Called when the user clicks on one of the links in the subprogram box in
   --  the status bar.

   procedure On_Goto_Line_Func (Ob : access GObject_Record'Class);
   --  Callback when clicking on the line number in the status bar

   procedure Buffer_Information_Handler
     (Ob     : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar);
   --  Reflect the change in buffer information

   procedure Destroy_Info_Frames
     (Bar : access Source_Editor_Status_Bar_Record'Class);
   --  Destroy Box.Buffer_Info_Frames

   type On_VCS_Status_Changed is new Vcs_File_Status_Hooks_Function with record
      Bar : Source_Editor_Status_Bar;
   end record;
   overriding procedure Execute
     (Self          : On_VCS_Status_Changed;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Vcs           : not null access Abstract_VCS_Engine'Class;
      Files         : File_Sets.Set;
      Props         : VCS_File_Properties);

   procedure On_VCS_Status_Clicked (Bar : access GObject_Record'Class);
   --  Called when the user clicks on the VCS status icon

   -------------------------
   -- Destroy_Info_Frames --
   -------------------------

   procedure Destroy_Info_Frames
     (Bar : access Source_Editor_Status_Bar_Record'Class)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Frames_Array, Frames_Array_Access);
   begin
      if Bar.Buffer_Info_Frames /= null then
         for J of Bar.Buffer_Info_Frames.all loop
            Bar.Toolbar.Remove (J);
         end loop;

         Unchecked_Free (Bar.Buffer_Info_Frames);
      end if;
   end Destroy_Info_Frames;

   --------------------------------
   -- Buffer_Information_Handler --
   --------------------------------

   procedure Buffer_Information_Handler
     (Ob     : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar)
   is
      pragma Unreferenced (Ob, Params);
      Info  : constant Extra_Information_Array_Access :=
        Get_Extra_Information (Bar.Buffer);
   begin
      Destroy_Info_Frames (Bar);

      if Info = null then
         return;
      end if;

      Bar.Buffer_Info_Frames := new Frames_Array (Info'Range);

      for J in Bar.Buffer_Info_Frames'Range loop
         Gtk_New (Bar.Buffer_Info_Frames (J));

         if Info (J).Icon.all /= "" then
            Bar.Buffer_Info_Frames (J).Set_Icon_Name (Info (J).Icon.all);
         end if;

         if Info (J).Info.Text /= Null_Unbounded_String then
            Bar.Buffer_Info_Frames (J).Set_Label
              (To_String (Info (J).Info.Text));
         end if;

         if Info (J).Tooltip /= null
           and then Info (J).Tooltip.all /= ""
         then
            Bar.Buffer_Info_Frames (J).Set_Tooltip_Markup
              (Info (J).Tooltip.all);
         end if;

         Bar.Toolbar.Insert (Bar.Buffer_Info_Frames (J), Extra_Info_Pos);
      end loop;

      Bar.Show_All;
   end Buffer_Information_Handler;

   ----------------------------
   -- Update_Subprogram_Name --
   ----------------------------

   procedure Update_Subprogram_Name
     (Bar : not null access Source_Editor_Status_Bar_Record'Class;
      Update_Tree : Boolean := False)
   is
      Block : Block_Record;
      Node  : Sem_Node_Holders.Holder;
      Val   : Unbounded_String;
   begin
      if Display_Subprogram_Names.Get_Pref then
         Block := Get_Subprogram_Block
           (Bar.Buffer, Bar.Current_Line, Update_Tree);
         if Block.Block_Type /= Cat_Unknown
           and then Block.Name /= No_Symbol
         then
            --  We cannot control the underline from the theme unfortunately.
            Val := To_Unbounded_String
              ("<span underline='none'><a href='" & Block.First_Line'Img & "'>"
               & Glib.Convert.Escape_Text (Get (Block.Name).all)
               & "</a></span>");

            Node := Sem_Node_Holders.To_Holder
              (Block.Tree_Node.Element.Parent);
            while Node.Element /= No_Semantic_Node loop
               Val :=
                 "<span underline='none'><a href='"
                 & Node.Element.Sloc_Start.Line'Img
                 & "'>" & Get (Node.Element.Name).all & "</a></span>."
                 & Val;
               Node.Replace_Element (Node.Element.Parent);
            end loop;

            Bar.Function_Label.Set_Markup (To_String (Val));
            return;
         end if;
      end if;

      Bar.Function_Label.Set_Text ("");
   end Update_Subprogram_Name;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status
     (Bar : not null access Source_Editor_Status_Bar_Record'Class)
   is
      Child : constant MDI_Child := Find_Child
        (Get_Kernel (Bar.Buffer), Source_Editor_Box (Bar.Box));
   begin
      if Child /= null then
         case Get_Status (Bar.Buffer) is
            when Unmodified | Readonly | Saved =>
               Child.Set_Icon_Name (File_Pixbuf);
               Bar.Modified_Status.Set_Icon_Name (File_Pixbuf);

            when Unsaved =>
               Child.Set_Icon_Name (File_Unsaved_Pixbuf);
               Bar.Modified_Status.Set_Icon_Name (File_Unsaved_Pixbuf);

            when Modified =>
               Child.Set_Icon_Name (File_Modified_Pixbuf);
               Bar.Modified_Status.Set_Icon_Name (File_Modified_Pixbuf);
         end case;
      end if;

      if Get_Writable (Bar.Buffer) then
         Bar.Read_Only.Set_Icon_Name ("gps-unlock-symbolic");
         Bar.Read_Only.Set_Tooltip_Text (-"Writable");
         Get_Style_Context (Bar.View).Remove_Class ("read-only");
      else
         Bar.Read_Only.Set_Icon_Name ("gps-lock-symbolic");
         Bar.Read_Only.Set_Tooltip_Text (-"Read Only");
         Get_Style_Context (Bar.View).Add_Class ("read-only");
      end if;
   end Update_Status;

   -----------------------
   -- On_Goto_Line_Func --
   -----------------------

   procedure On_Goto_Line_Func (Ob : access GObject_Record'Class) is
      Bar : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Ob);
   begin
      On_Goto_Line (Bar.Box, Get_Kernel (Bar.Buffer));
   end On_Goto_Line_Func;

   ------------------------
   -- On_Subprogram_Link --
   ------------------------

   function On_Subprogram_Link
     (Ob  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args : Gtk_Args) return Boolean
   is
      Bar : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Ob);
      URI : constant String := To_String (Args, 1);
      Line : constant Editable_Line_Type := Editable_Line_Type'Value (URI);
      Box  : constant Source_Editor_Box := Source_Editor_Box (Bar.Box);
   begin
      Push_Current_Editor_Location_In_History (Get_Kernel (Bar.Buffer));
      Set_Cursor_Location (Box, Line, 1, Centering => With_Margin);
      Add_Navigation_Location (Box);
      return True;
   end On_Subprogram_Link;

   --------------------------
   -- On_Read_Only_Pressed --
   --------------------------

   procedure On_Read_Only_Pressed (Ob : access GObject_Record'Class) is
      Bar : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Ob);
   begin
      Source_Editor_Box (Bar.Box).Set_Writable
        (not Get_Writable (Bar.Buffer), Explicit => True);
   end On_Read_Only_Pressed;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Source_Editor_Status_Bar; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   --------------------------
   -- Show_Cursor_Position --
   --------------------------

   procedure Show_Cursor_Position
     (Bar    : access Source_Editor_Status_Bar_Record'Class;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type)
   is
      Pos : constant String :=
         String_Utils.Image (Integer (Line))
         & ':' & String_Utils.Image (Integer (Column));
      Start, The_End : Gtk_Text_Iter;
      Result : Boolean;
      Lines, Offset : Gint;
   begin
      Bar.Buffer.Get_Selection_Bounds (Start, The_End, Result);
      if Result then
         Lines := Get_Line (The_End) - Get_Line (Start) + 1;
         Offset := Get_Offset (The_End) - Get_Offset (Start);
         Bar.Cursor_Loc.Set_Label
            ("("
             & Image (Integer (Lines), Min_Width => 1) & " lines,"
             & Offset'Img & " chars) " & Pos);
      else
         Bar.Cursor_Loc.Set_Label (Pos);
      end if;
   end Show_Cursor_Position;

   -------------------------------------
   -- Cursor_Position_Changed_Handler --
   -------------------------------------

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Bar    : Source_Editor_Status_Bar)
   is
      pragma Unreferenced (Buffer);
      Child : MDI_Child;
      File  : GNATCOLL.VFS.Virtual_File := Get_Filename (Bar.Buffer);
   begin
      if File = GNATCOLL.VFS.No_File then
         File := Get_File_Identifier (Bar.Buffer);
      end if;

      Bar.Current_Line :=
        Editable_Line_Type (Values.Get_Int (Values.Nth (Params, 1)));

      --  In case there are multiple views, we only want to change the one that
      --  last had the focus. Otherwise, they would all end up with the same
      --  line number, which is inaccurate.
      --  The box might not have the focus currently: if we are for instance
      --  changing the current line from the "Go to line" dialog, the latter
      --  still has the focus at this point.

      Child := Find_Editor
        (Get_Kernel (Bar.Buffer),
         File,
         Get_Project (Bar.View));

      if Child /= null and then Get_Widget (Child) = Gtk_Widget (Bar.Box) then
         Show_Cursor_Position
           (Bar,
            Line   => Bar.Current_Line,
            Column => Character_Offset_Type
              (Values.Get_Int (Values.Nth (Params, 2))));
      end if;
   end Cursor_Position_Changed_Handler;

   ---------------------------
   -- On_VCS_Status_Clicked --
   ---------------------------

   procedure On_VCS_Status_Clicked (Bar : access GObject_Record'Class) is
      B : constant Source_Editor_Status_Bar := Source_Editor_Status_Bar (Bar);
      Kernel : constant Kernel_Handle := Get_Kernel (B.Buffer);
      Dummy : Boolean;
   begin
      if Perspective_Exists (Kernel, "VCS") then
         Load_Perspective (Kernel, "VCS");
      else
         Insert
           (Kernel,
            -"The VCS perspective is not defined in your environment."
            & ASCII.LF
            & (-"Consider using the menu /File/Reset all perspectives"));

         Dummy := Execute_Action (Kernel, "open Commits");
      end if;
   end On_VCS_Status_Clicked;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self          : On_VCS_Status_Changed;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Vcs           : not null access Abstract_VCS_Engine'Class;
      Files         : File_Sets.Set;
      Props         : VCS_File_Properties)
   is
      pragma Unreferenced (Kernel);
      Bar : constant Source_Editor_Status_Bar := Self.Bar;
   begin
      if Files.Contains (Bar.Buffer.Get_Filename) then
         declare
            D : constant Status_Display := Vcs.Get_Display (Props.Status);
         begin
            if Bar.VCS_Status = null then
               Gtk_New (Bar.VCS_Status);
               Bar.VCS_Status.On_Clicked (On_VCS_Status_Clicked'Access, Bar);
               Bar.Toolbar.Insert (Bar.VCS_Status, Extra_Info_Pos);
               Bar.VCS_Status.Show_All;
            end if;

            Bar.VCS_Status.Set_Icon_Name (To_String (D.Icon_Name));
            Bar.VCS_Status.Set_Tooltip_Markup
               (Vcs.Get_Tooltip_For_File (Bar.Buffer.Get_Filename));
         end;
      end if;
   end Execute;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Bar    : out Source_Editor_Status_Bar;
      Box    : Gtk_Event_Box;
      View   : Source_View;
      Buffer : Source_Buffer)
   is
      H      : access On_VCS_Status_Changed;
      Kernel : constant Kernel_Handle := Get_Kernel (Buffer);
      P      : constant Project_Type := Get_Project (View);
      VCS    : Abstract_VCS_Engine_Access;
   begin
      Bar := new Source_Editor_Status_Bar_Record;
      Initialize_Hbox (Bar, Homogeneous => False);

      Bar.View := View;
      Bar.Buffer := Buffer;
      Bar.Box := Box;

      --  Avoid resizing the main window whenever a label is changed
      Bar.Set_Resize_Mode (Resize_Queue);

      Gtk_New (Bar.Function_Label);
      Bar.Function_Label.Set_Ellipsize (Ellipsize_Start);
      Bar.Function_Label.Set_Alignment (0.0, 0.5);
      Bar.Pack_Start (Bar.Function_Label, Expand => True, Fill => True);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Bar.Function_Label,
         Gtk.Label.Signal_Activate_Link, On_Subprogram_Link'Access, Bar);

      Gtk_New (Bar.Toolbar);
      Bar.Toolbar.Set_Icon_Size (Icon_Size_Local_Toolbar);
      Bar.Pack_End (Bar.Toolbar, Expand => False);

      Gtk_New (Bar.Cursor_Loc, Label => "1:1");
      Bar.Cursor_Loc.Set_Homogeneous (False);
      Bar.Toolbar.Insert (Bar.Cursor_Loc);
      Bar.Cursor_Loc.On_Clicked (On_Goto_Line_Func'Access, Bar);

      Gtk_New (Bar.Modified_Status);
      Bar.Modified_Status.Set_Homogeneous (False);
      Bar.Toolbar.Insert (Bar.Modified_Status);

      Gtk_New (Bar.Read_Only);
      Bar.Read_Only.Set_Homogeneous (False);
      Bar.Read_Only.On_Clicked (On_Read_Only_Pressed'Access, Bar);
      Bar.Toolbar.Insert (Bar.Read_Only);

      Bar_Callback.Connect
        (Bar.Buffer,
         Signal_Cursor_Position_Changed,
         Cursor_Position_Changed_Handler'Access,
         User_Data => Bar,
         After     => True);

      Bar_Callback.Connect
        (Bar.Buffer,
         Signal_Buffer_Information_Changed,
         Buffer_Information_Handler'Access,
         User_Data => Bar,
         After     => True);

      --  Monitor changes to VCS status (and get the initial status for the
      --  file)
      H := new On_VCS_Status_Changed'
        (Vcs_File_Status_Hooks_Function with Bar => Bar);
      Vcs_File_Status_Changed_Hook.Add (H, Watch => Bar);  --  will update

      VCS := Kernel.VCS.Get_VCS (P);

      declare
         Set : File_Sets.Set;
      begin
         VCS.Ensure_Status_For_Files ((1 => Buffer.Get_Filename));
         Set.Include (Buffer.Get_Filename);
         H.Execute        --  display initial value
           (Kernel,
            VCS,
            Set,
            VCS.File_Properties_From_Cache (Buffer.Get_Filename));
      end;

      Show_Cursor_Position (Bar, Line => 1, Column => 1);
      Get_Style_Context (Bar).Add_Class ("gps-editor-status-bar");
   end Gtk_New;

end Src_Editor_Status_Bar;
