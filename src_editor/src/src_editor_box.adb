-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with GNAT.OS_Lib;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;
with Glide_Kernel;               use Glide_Kernel;
with Glide_Kernel.Console;       use Glide_Kernel.Console;
with Glide_Kernel.Modules;       use Glide_Kernel.Modules;
with Glide_Kernel.Project;       use Glide_Kernel.Project;
with Glide_Kernel.Preferences;   use Glide_Kernel.Preferences;
with Glide_Kernel.Scripts;       use Glide_Kernel.Scripts;
with Gdk;                        use Gdk;
with Gdk.Color;                  use Gdk.Color;
with Gdk.Event;                  use Gdk.Event;
with Gdk.GC;                     use Gdk.GC;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;                 use Gdk.Window;
with Pango.Layout;               use Pango.Layout;
with Pango.Font;                 use Pango.Font;

with Gtk;                        use Gtk;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Clipboard;              use Gtk.Clipboard;
with Gtk.Container;              use Gtk.Container;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Event_Box;              use Gtk.Event_Box;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Text_Mark;              use Gtk.Text_Mark;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.File_Selector;       use Gtkada.File_Selector;
with Gtkada.MDI;                 use Gtkada.MDI;
with GUI_Utils;                  use GUI_Utils;
with Glide_Intl;                 use Glide_Intl;

with Basic_Types;
with Language;                   use Language;
with Language.Ada;               use Language.Ada;
with Language_Handlers;          use Language_Handlers;
with String_Utils;               use String_Utils;
with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_View;            use Src_Editor_View;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Info;                   use Src_Info;
with Src_Info.Prj_Utils;         use Src_Info.Prj_Utils;
with Src_Info.Queries;           use Src_Info.Queries;
with Traces;                     use Traces;
with VFS;                        use VFS;
with GVD.Dialogs;                use GVD.Dialogs;
--  ??? Used for Simple_Entry_Dialog. Should move this procedure in GUI_Utils

with Commands;                   use Commands;
with Commands.Editor;            use Commands.Editor;
with Find_Utils;                 use Find_Utils;

with Gtkada.Types;              use Gtkada.Types;
with Gdk.Pixbuf;                use Gdk.Pixbuf;

package body Src_Editor_Box is

   Me : constant Debug_Handle := Create ("Source_Editor");

   editor_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   editor_modified_xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, editor_xpm, "mini_page_xpm");
   pragma Import (C, editor_modified_xpm, "modified_page_xpm");

   procedure Setup (Data : Source_Editor_Box; Id : Handler_Id);
   package Box_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Source_Editor_Box,
      Setup       => Setup);

   --------------------------
   -- Forward declarations --
   --------------------------

   procedure Get_Declaration_Info
     (Editor      : access Source_Editor_Box_Record;
      Context     : access Entity_Selection_Context'Class;
      Entity      : out Entity_Information);
   --  Perform a cross-reference to the declaration of the entity located at
   --  (Line, Column) in Editor. Fail silently when no declaration or no
   --  entity can be located, and set File_Decl to null.
   --  Entity is set to the entity that was found, or No_Entity_Information if
   --  not found. It must be destroyed by the caller.

   function Get_Contextual_Menu
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Same as the public Get_Contextual_Menu, Event_Widget is ignored.

   function To_Box_Line (Line : Gint) return Natural;
   pragma Inline (To_Box_Line);
   --  Convert a line number in the Source Buffer to a line number in the
   --  Source Box. This conversion is necessary because line numbers start
   --  from 1 in the Source Box (this is the natural numbering for humans),
   --  whereas it starts from 0 in the Source Box.

   function To_Box_Column (Col : Gint) return Natural;
   pragma Inline (To_Box_Column);
   --  Convert a column number in the Source Buffer to a column number
   --  in the Source Box. Same rationale as in To_Box_Line.

   function To_Buffer_Line (Line : Natural) return Gint;
   pragma Inline (To_Buffer_Line);
   --  Convert a line number in the Source Box to a line number in the
   --  Source Buffer. Same rationale as in To_Box_Line.

   function To_Buffer_Column (Col : Natural) return Gint;
   pragma Inline (To_Buffer_Column);
   --  Convert to a column number in the Source Box to a column number
   --  in the Source Buffer. Same rationale as in To_Box_Line.

   procedure Show_Cursor_Position
     (Box    : Source_Editor_Box;
      Line   : Gint;
      Column : Gint);
   --  Redraw the cursor position in the Line/Column areas of the status bar.

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  This handler is merely a proxy to Show_Cursor_Position. It just
   --  extracts the necessary values from Params, and pass them on to
   --  Show_Cursor_Position.

   procedure Buffer_Information_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Reflect the change in buffer information.

   procedure Status_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Reflect the change in buffer status.

   procedure On_Box_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Callback for the "destroy" signal.

   procedure On_Toggle_Overwrite
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Callback to be called when the view receives the "toggle_overwrite"
   --  signal.

   procedure Initialize_Box
     (Box    : access Source_Editor_Box_Record;
      Kernel : Glide_Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access);
   --  Perform the initialization of the given editor box. If Source_Buffer
   --  is null, then a new buffer will automatically be created. Otherwise,
   --  the editor creates a new editor for the same Source_Buffer.

   function Focus_In (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_in event. This checks whether the physical file
   --  on the disk is more recent than the one that was read for the editor.

   function Focus_Out (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_out event.

   function Key_Press (Box : access GObject_Record'Class) return Boolean;
   --  Check whether the file has been modified on disk

   function On_Read_Only_Pressed
     (Box : access GObject_Record'Class) return Boolean;
   --  Toggle read-only/writable state of a given box.

   function Check_Timestamp_Idle (Box : GObject) return Boolean;
   --  Idle callback to check that the timestamp of a file hasn't changed.

   ----------------------------------
   -- The contextual menu handling --
   ----------------------------------

   procedure On_Goto_Line
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto Line" contextual menu

   function On_Goto_Line_Func
     (Editor : access GObject_Record'Class) return Boolean;
   --  Callback when clicking on the line number in the status bar

   procedure On_Goto_Declaration
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto Declaration" contextual menu

   procedure On_Goto_Next_Body
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto Body" contextual menu

   procedure On_Goto_Other_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "Goto spec <-> body" contextual menu

   procedure Find_Closest_Match
     (Source : access Source_Editor_Box_Record'Class;
      Line   : in out Natural;
      Column : in out Natural;
      Entity : String);
   --  Find the reference to Entity in Source which is closest to (Line,
   --  Column), and sets Line and Column to this new location

   procedure Add_Navigation_Location
     (Source : access Source_Editor_Box_Record'Class);
   --  Add a navigation command to mark the given location in the source
   --  editor. Used to remember the location before Xref navigation.

   -----------------------------
   -- Add_Navigation_Location --
   -----------------------------

   procedure Add_Navigation_Location
     (Source : access Source_Editor_Box_Record'Class)
   is
      Args   : GNAT.OS_Lib.Argument_List (1 .. 5);
      Line   : Editable_Line_Type;
      Column : Positive;
   begin
      Get_Cursor_Position (Source.Source_Buffer, Line, Column);

      Args (1) := new String'("edit");
      Args (2) := new String'
        (Full_Name (Get_Filename (Source.Source_Buffer)).all);
      Args (3) := new String'(Editable_Line_Type'Image (Line));
      Args (4) := new String'(Positive'Image (Column));
      Args (5) := new String'("0");

      Execute_GPS_Shell_Command (Source.Kernel, "add_location_command", Args);

      for J in Args'Range loop
         GNAT.OS_Lib.Free (Args (J));
      end loop;
   end Add_Navigation_Location;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Source_Editor_Box; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   ------------------------
   -- Find_Closest_Match --
   ------------------------

   procedure Find_Closest_Match
     (Source : access Source_Editor_Box_Record'Class;
      Line   : in out Natural;
      Column : in out Natural;
      Entity : String)
   is
      Best_Line     : Integer := 0;
      Best_Column   : Integer := 0;

      function Callback (Match : Match_Result) return Boolean;
      --  Called every time a reference to the entity is found

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
         Line_Diff : constant Integer :=
           abs (Match.Line - Line) - abs (Best_Line - Line);
         Col_Diff : constant Integer :=
           abs (Match.Column - Column) - abs (Best_Column - Column);
      begin
         if Line_Diff < 0
           or else (Line_Diff = 0 and then Col_Diff < 0)
         then
            Best_Line := Match.Line;
            Best_Column := Match.Column;
         end if;

         return True;
      end Callback;


      Context : aliased Root_Search_Context;
      L, C : Integer := 1;

      Buffer : constant String := Get_Slice (Source, 1, 1);
      Index  : Integer := Buffer'First;
      Was_Partial : Boolean;

   begin
      Set_Context
        (Context'Access,
         Look_For => Entity,
         Options  => (Case_Sensitive =>
                        Get_Language_Context
                          (Get_Language (Source)).Case_Sensitive,
                      Whole_Word     => True,
                      Regexp         => False));

      Scan_Buffer_No_Scope
        (Context     => Context'Access,
         Buffer      => Buffer,
         Start_Index => Buffer'First,
         End_Index   => Buffer'Last,
         Callback    => Callback'Unrestricted_Access,
         Ref_Index   => Index,
         Ref_Line    => L,
         Ref_Column  => C,
         Was_Partial => Was_Partial);

      Line   := Best_Line;
      Column := Best_Column;
   end Find_Closest_Match;

   ------------------------------
   -- Goto_Declaration_Or_Body --
   ------------------------------

   procedure Goto_Declaration_Or_Body
     (Kernel  : access Kernel_Handle_Record'Class;
      To_Body : Boolean;
      Editor  : access Source_Editor_Box_Record'Class;
      Context : access Entity_Selection_Context'Class)
   is
      Source          : Source_Editor_Box;
      Source_Info     : LI_File_Ptr;
      Status          : Src_Info.Queries.Find_Decl_Or_Body_Query_Status;
      Entity          : Entity_Information;
      Location        : File_Location;
      L, C            : Natural;
      Length          : Natural;
      File_Up_To_Date : Boolean;
      Filename        : Virtual_File;

   begin
      if Get_Filename (Editor) = VFS.No_File then
         Console.Insert
           (Kernel, -"Cross-references not possible on unamed files",
            Mode => Error);
         return;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      Source_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Filename (Editor));

      --  Abort if we could not locate the associated Source_Info.
      --  Probably means that we either could not locate the ALI file,
      --  or it could also be that we failed to parse it. Either way,
      --  a message should have already been printed. So, just abort.

      if Source_Info = No_LI_File then
         Console.Insert
           (Kernel,
            -"No cross-reference information found for "
            & Full_Name (Get_Filename (Editor)).all & ASCII.LF
            & (-("Recompile your file or select Build->Recompute Xref"
                 & " Information, depending on the language")),
            Mode           => Error);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      if To_Body then
         Find_Next_Body
           (Kernel             => Kernel,
            Lib_Info           => Source_Info,
            File_Name          => Get_Filename (Editor),
            Entity_Name        => Entity_Name_Information (Context),
            Line               => Modules.Line_Information (Context),
            Column             => Entity_Column_Information (Context),
            Location           => Location,
            Status             => Status);

      else
         Find_Declaration_Or_Overloaded
           (Kernel             => Kernel,
            Lib_Info           => Source_Info,
            File_Name          => Get_Filename (Editor),
            Entity_Name        => Entity_Name_Information (Context),
            Line               => Modules.Line_Information (Context),
            Column             => Entity_Column_Information (Context),
            Entity             => Entity,
            Status             => Status);
      end if;

      case Status is
         when Entity_Not_Found | Overloaded_Entity_Found =>
            Console.Insert
              (Kernel, -"Cross-reference failed for "
                 & Entity_Name_Information (Context),
               Mode           => Error);
            Pop_State (Kernel_Handle (Kernel));
            return;

         when Internal_Error =>
            Console.Insert
              (Kernel, -"Cross-reference internal error detected",
               Mode => Error);
            Pop_State (Kernel_Handle (Kernel));
            return;

         when No_Body_Entity_Found =>
            if To_Body then
               Console.Insert
                 (Kernel,
                  -"This entity does not have an associated body",
                  Mode => Error);
            else
               Console.Insert
                 (Kernel,
                  -"This entity does not have an associated declaration",
                  Mode => Error);
            end if;

            Pop_State (Kernel_Handle (Kernel));
            return;

         when Fuzzy_Match =>
            Console.Insert
              (Kernel, -("The cross-reference information isn't up-to-date."
                         & " The result is the closest match, but might not be"
                         & " accurate"));

         when Success =>
            null; --  No error message to print
      end case;

      Length := Entity_Name_Information (Context)'Length;

      if To_Body then
         --  Open the file, and reset Source to the new editor in order to
         --  highlight the region returned by the Xref query.

         L := Get_Line (Location);
         C := Get_Column (Location);

         Trace (Me, "Goto_Declaration_Or_Body: Opening file "
                & Full_Name (Get_File (Location)).all);
         Filename := Get_File (Location);
         if Dir_Name (Filename).all = "" then
            Insert (Kernel, -"File not found: "
                    & Base_Name (Filename), Mode => Error);
         end if;

      else
         --  Open the file, and reset Source to the new editor in order to
         --  highlight the region returned by the Xref query.

         L := Get_Declaration_Line_Of (Entity);
         C := Get_Declaration_Column_Of (Entity);
         Filename := Get_Declaration_File_Of (Entity);

         if Dir_Name (Filename).all = "" then
            Insert (Kernel, -"File not found: "
                    & Base_Name (Filename), Mode => Error);
         end if;
      end if;

      if Dir_Name (Filename).all /= "" then
         Add_Navigation_Location (Editor);
         Open_File_Editor
           (Kernel, Filename, L, C, C + Length,
            Enable_Navigation => True);

      else
         Destroy (Entity);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      --  Find the correct location for the entity, in case it is in fact
      --  different from what was found in the LI file (ie for instance the LI
      --  was older than the destination file).

      Source := Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

      if Source /= null then

         --  Find the closest match of the entity, in case the LI file wasn't
         --  up-to-date.

         File_Up_To_Date := Is_Valid_Location (Source, L, C)
           and then Is_Valid_Location (Source, L, C + Length)
           and then Get_Slice
             (Source.Source_Buffer,
              To_Buffer_Line (L), To_Buffer_Column (C),
              To_Buffer_Line (L), To_Buffer_Column (C + Length)) =
                Entity_Name_Information (Context);

         --  Search for the closest reference to the entity if
         --  necessary. Otherwise, there's nothing to be done, since the region
         --  was already selected when opening the editor
         if not File_Up_To_Date then
            Console.Insert
              (Kernel,
               -("The cross-reference information and the destination file do"
                 & " not match, the cursor was set at the closest reference"
                 & " to ") & Entity_Name_Information (Context));

            --  Search for the closest reference to entity, and highlight the
            --  appropriate region in the editor.

            Find_Closest_Match
              (Source, L, C, Entity_Name_Information (Context));
            Open_File_Editor
              (Kernel, Filename, L, C, C + Length, False);
         end if;
      end if;

      Destroy (Entity);
      Pop_State (Kernel_Handle (Kernel));

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Goto_Declaration_Or_Body;

   --------------------------
   -- Get_Declaration_Info --
   --------------------------

   procedure Get_Declaration_Info
     (Editor      : access Source_Editor_Box_Record;
      Context     : access Entity_Selection_Context'Class;
      Entity      : out Entity_Information)
   is
      Source_Info    : LI_File_Ptr;
      Status         : Src_Info.Queries.Find_Decl_Or_Body_Query_Status;
      Filename       : constant Virtual_File := Get_Filename (Editor);

   begin
      if Filename = VFS.No_File then
         Entity := No_Entity_Information;
         return;
      end if;

      Push_State (Editor.Kernel, Busy);
      Source_Info := Locate_From_Source_And_Complete (Editor.Kernel, Filename);

      --  Exit if we could not locate the associated Source_Info.

      if Source_Info /= No_LI_File then
         --  Don't use Find_Declaration_Or_Overloaded, since we don't want to
         --  ask the user interactively for the tooltips.
         Find_Declaration
           (Lib_Info    => Source_Info,
            File_Name   => Filename,
            Entity_Name => Entity_Name_Information (Context),
            Line        => Modules.Line_Information (Context),
            Column      => Entity_Column_Information (Context),
            Entity      => Entity,
            Status      => Status);

         if Status /= Success and then Status /= Fuzzy_Match then
            Destroy (Entity);
         end if;
      else
         Entity := No_Entity_Information;
      end if;

      Pop_State (Editor.Kernel);

   exception
      when E : others =>
         Pop_State (Editor.Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Entity := No_Entity_Information;
   end Get_Declaration_Info;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Editor : access Source_Editor_Box_Record) is
   begin
      Grab_Focus (Editor.Source_View);
   end Grab_Focus;

   ------------------
   -- Draw_Tooltip --
   ------------------

   procedure Draw_Tooltip
     (Widget        : access Source_View_Record'Class;
      Data          : in out Editor_Tooltip_Data;
      Pixmap        : out Gdk.Gdk_Pixmap;
      Width, Height : out Glib.Gint;
      Area          : out Gdk_Rectangle)
   is
      Line, Col, Cursor_Col : Gint;
      Mouse_X, Mouse_Y : Gint;
      Win_X, Win_Y     : Gint;
      Start_Iter       : Gtk_Text_Iter;
      End_Iter         : Gtk_Text_Iter;
      Mask             : Gdk.Types.Gdk_Modifier_Type;
      Win              : Gdk.Gdk_Window;
      Context          : aliased Entity_Selection_Context;
      Location         : Gdk_Rectangle;
      Filename         : constant Virtual_File := Get_Filename (Data.Box);
      Out_Of_Bounds    : Boolean;
      Window           : Gdk.Gdk_Window;
      Window_Width     : Gint;
      Window_Height    : Gint;
      Window_Depth     : Gint;


   begin
      Width  := 0;
      Height := 0;
      Pixmap := null;
      Area   := (0, 0, 0, 0);

      if not Get_Pref (Data.Box.Kernel, Display_Tooltip) then
         return;
      end if;

      Window := Get_Window (Widget, Text_Window_Text);

      Get_Geometry
        (Window, Win_X, Win_Y, Window_Width, Window_Height, Window_Depth);
      Get_Pointer
        (Window, Mouse_X, Mouse_Y, Mask, Win);

      if Mouse_X < Win_X
        or else Mouse_Y < Win_Y
        or else Win_X + Window_Width < Mouse_X
        or else Win_Y + Window_Height < Mouse_Y
      then
         --  Invalid position: the cursor is outside the text, do not
         --  display a tooltip.

         return;
      end if;

      Window_To_Buffer_Coords
        (Widget, Mouse_X, Mouse_Y, Line, Col, Out_Of_Bounds);

      if Out_Of_Bounds then
         --  Do not display a tooltip in an invalid location,
         --  for example after the end of a line.

         return;
      end if;

      Cursor_Col := Col;
      Get_Iter_At_Line_Offset
        (Data.Box.Source_Buffer, Start_Iter, Line, Col);
      Search_Entity_Bounds (Start_Iter, End_Iter);
      Get_Screen_Position (Data.Box.Source_Buffer, Start_Iter, Line, Col);

      --  Compute the area surrounding the entity, relative to the pointer
      --  coordinates

      Get_Iter_Location (Widget, Start_Iter, Location);
      Buffer_To_Window_Coords
        (Widget, Text_Window_Text, Location.X, Location.Y, Win_X, Win_Y);
      Area.X := Win_X - Mouse_X;
      Area.Y := Win_Y - Mouse_Y;
      Get_Iter_Location (Widget, End_Iter, Location);
      Buffer_To_Window_Coords
        (Widget, Text_Window_Text, Location.X, Location.Y, Win_X, Win_Y);
      Area.Width  := Win_X - Mouse_X - Area.X + Location.Width;
      Area.Height := Win_Y - Mouse_Y - Area.Y + Location.Height;

      declare
         Entity_Name : constant String := Get_Text (Start_Iter, End_Iter);
         Entity : Entity_Information;

      begin
         if Entity_Name = "" then
            return;
         end if;

         Trace (Me, "Tooltip on " & Entity_Name);
         Set_Context_Information
           (Context'Unchecked_Access, Data.Box.Kernel, Src_Editor_Module_Id);
         Set_File_Information
           (Context      => Context'Unchecked_Access,
            File         => Filename,
            Line         => To_Box_Line (Line),
            Column       => To_Box_Column (Cursor_Col));
         Set_Entity_Information
           (Context       => Context'Unchecked_Access,
            Entity_Name   => Entity_Name,
            Entity_Column => To_Box_Column (Col));
         Glide_Kernel.Modules.Compute_Tooltip
           (Data.Box.Kernel, Context'Unchecked_Access, Pixmap, Width, Height);

         if Pixmap /= null then
            Destroy (Context);
            return;
         end if;

         --  No module wants to handle this tooltip. Default to built-in
         --  tooltip, based on cross references.

         Get_Declaration_Info (Data.Box, Context'Unchecked_Access, Entity);

         Destroy (Context);

         if Entity = No_Entity_Information then
            return;
         end if;

         declare
            Str : constant String :=
              Scope_To_String (Get_Scope (Entity)) & ' ' &
              (-Kind_To_String (Get_Kind (Entity))) & ' ' &
              Get_Full_Name
                (Entity,
                 Locate_From_Source_And_Complete
                   (Data.Box.Kernel, Get_Declaration_File_Of (Entity)),
                 ".")
              & ASCII.LF
              & (-"declared at ")
              & Base_Name (Get_Declaration_File_Of (Entity)) & ':'
              & Image (Get_Declaration_Line_Of (Entity));

         begin
            Create_Pixmap_From_Text
              (Text     => Str,
               Font     => Get_Pref (Data.Box.Kernel, Default_Font),
               Bg_Color => White (Get_Default_Colormap),
               Widget   => Widget,
               Pixmap   => Pixmap,
               Width    => Width,
               Height   => Height);
         end;

         Destroy (Entity);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Draw_Tooltip;

   -----------------
   -- To_Box_Line --
   -----------------

   function To_Box_Line (Line : Gint) return Natural is
   begin
      return Natural (Line + 1);
   end To_Box_Line;

   -------------------
   -- To_Box_Column --
   -------------------

   function To_Box_Column (Col : Gint) return Natural is
   begin
      return Natural (Col + 1);
   end To_Box_Column;

   --------------------
   -- To_Buffer_Line --
   --------------------

   function To_Buffer_Line (Line : Natural) return Gint is
   begin
      return Gint (Line) - 1;
   end To_Buffer_Line;

   ----------------------
   -- To_Buffer_Column --
   ----------------------

   function To_Buffer_Column (Col : Natural) return Gint is
   begin
      return Gint (Col) - 1;
   end To_Buffer_Column;

   --------------------------
   -- Show_Cursor_Position --
   --------------------------

   procedure Show_Cursor_Position
     (Box    : Source_Editor_Box;
      Line   : Gint;
      Column : Gint) is
   begin
      Set_Text
        (Box.Cursor_Loc_Label,
         Image (Integer (Line)) & ':' & Image (Integer (Column)));
   end Show_Cursor_Position;

   ----------------------------
   -- Status_Changed_Handler --
   ----------------------------

   procedure Status_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer, Params);
      Child : constant MDI_Child := Find_Child (Box.Kernel, Box);
   begin
      case Get_Status (Box.Source_Buffer) is
         when Unmodified =>
            Set_Text (Box.Modified_Label, -"Unmodified");
            Set_Icon (Child, Gdk_New_From_Xpm_Data (editor_xpm));

         when Saved =>
            Set_Text (Box.Modified_Label, -"Saved");
            Set_Icon (Child, Gdk_New_From_Xpm_Data (editor_xpm));

         when Modified =>
            Set_Text (Box.Modified_Label, -"Modified");
            Set_Icon (Child, Gdk_New_From_Xpm_Data (editor_modified_xpm));
      end case;
   end Status_Changed_Handler;

   --------------------------------
   -- Buffer_Information_Handler --
   --------------------------------

   procedure Buffer_Information_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer, Params);

      Info : constant Extra_Information_Array_Access
        := Get_Extra_Information (Box.Source_Buffer);
      Label : Gtk_Label;

      use type Basic_Types.String_Access;
   begin
      if Box.Buffer_Info_Frames /= null then
         for J in Box.Buffer_Info_Frames'Range loop
            Remove (Box.Label_Box, Box.Buffer_Info_Frames (J));
         end loop;

         Unchecked_Free (Box.Buffer_Info_Frames);
      end if;

      if Info = null then
         return;
      end if;

      Box.Buffer_Info_Frames := new Frames_Array (Info'Range);

      for J in Box.Buffer_Info_Frames'Range loop
         if Info (J).Info.Text /= null then
            Gtk_New (Label, Info (J).Info.Text.all);
         else
            Gtk_New (Label);
         end if;

         Gtk_New (Box.Buffer_Info_Frames (J));
         Set_Shadow_Type (Box.Buffer_Info_Frames (J), Shadow_In);

         Add (Box.Buffer_Info_Frames (J), Label);

         Pack_End
           (Box.Label_Box,
            Box.Buffer_Info_Frames (J),
            Expand  => False,
            Fill    => True,
            Padding => 0);
      end loop;

      Show_All (Box.Label_Box);
   end Buffer_Information_Handler;

   -------------------------------------
   -- Cursor_Position_Changed_Handler --
   -------------------------------------

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer);

   begin
      if Has_Focus_Is_Set (Box.Source_View) then
         Show_Cursor_Position
           (Box,
            Line   => Values.Get_Int (Values.Nth (Params, 1)),
            Column => Values.Get_Int (Values.Nth (Params, 2)));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Cursor_Position_Changed_Handler;

   --------------------
   -- On_Box_Destroy --
   --------------------

   procedure On_Box_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Object, Params);
      Child_Box : Source_Editor_Box;
   begin
      --  If the editor was primary, look for other editors with this buffer,
      --  and give the Primary attribute to one of them.

      if Box.Primary then
         Child_Box := Find_Other_Editor
           (Box.Kernel,
            Gtk_Text_View (Box.Source_View),
            Gtk_Text_Buffer (Box.Source_Buffer));

         if Child_Box /= null then
            Child_Box.Primary := True;
         end if;
      end if;

      Disconnect (Box.Source_Buffer, Box.Cursor_Handler);
      Disconnect (Box.Source_Buffer, Box.Status_Handler);
      Disconnect (Box.Source_Buffer, Box.Buffer_Info_Handler);
      Editor_Tooltips.Destroy_Tooltip (Box.Tooltip);

      if Box.Default_GC /= null then
         Unref (Box.Bg_GC);
         Unref (Box.Default_GC);
      end if;

      Delete (Box.Source_View);
      Unref (Box.Source_Buffer);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Box_Destroy;

   -------------------------
   -- On_Toggle_Overwrite --
   -------------------------

   procedure On_Toggle_Overwrite
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Object, Params);
   begin
      Box.Overwrite := not Box.Overwrite;

      if Box.Overwrite then
         Set_Text (Box.Overwrite_Label, -"Overwrite");
      else
         Set_Text (Box.Overwrite_Label, -"Insert");
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Toggle_Overwrite;

   --------------------
   -- Initialize_Box --
   --------------------

   procedure Initialize_Box
     (Box    : access Source_Editor_Box_Record;
      Kernel : Glide_Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access)
   is
      Frame          : Gtk_Frame;
      Event_Box      : Gtk_Event_Box;
      Scrolling_Area : Gtk_Scrolled_Window;
      Data           : Editor_Tooltip_Data;

   begin
      Glib.Object.Initialize (Box);

      Box.Kernel := Kernel;

      Gtk_New_Vbox (Box.Root_Container, Homogeneous => False);

      Gtk_New (Scrolling_Area);
      Set_Policy
        (Scrolling_Area,
         H_Scrollbar_Policy => Policy_Automatic,
         V_Scrollbar_Policy => Policy_Automatic);
      Pack_Start (Box.Root_Container, Scrolling_Area,
                  Expand => True, Fill => True);

      if Source = null then
         Gtk_New (Box.Source_Buffer, Kernel, Lang => Lang);
         Box.Primary := True;
      else
         Box.Source_Buffer := Source;
      end if;

      Ref (Box.Source_Buffer);
      Gtk_New (Box.Source_View, Box.Source_Buffer, Kernel);
      Add (Scrolling_Area, Box.Source_View);

      Data.Box := Source_Editor_Box (Box);
      Editor_Tooltips.New_Tooltip (Box.Source_View, Data, Box.Tooltip);

      --  The status bar, at the bottom of the window...

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Box.Root_Container, Frame, Expand => False, Fill => False);

      Gtk_New_Hbox (Box.Label_Box, Homogeneous => False, Spacing => 2);
      Add (Frame, Box.Label_Box);

      --  Line:Column number area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Event_Box);
      Add (Frame, Event_Box);
      Gtk_New (Box.Cursor_Loc_Label, "1:1");
      Add (Event_Box, Box.Cursor_Loc_Label);

      --  Setup a minimal size for the line:column area, to avoid too much
      --  resizing.

      declare
         Layout : constant Pango_Layout :=
           Create_Pango_Layout (Box.Cursor_Loc_Label, "99999:999");
         Width, Height : Gint;
      begin
         Get_Pixel_Size (Layout, Width, Height);
         Set_Size_Request (Frame, Width, Height);
         Unref (Layout);
      end;

      Object_Return_Callback.Object_Connect
        (Event_Box, "button_press_event",
         Object_Return_Callback.To_Marshaller (On_Goto_Line_Func'Access),
         Box);

      --  Modified file area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Modified_Label);
      Add (Frame, Box.Modified_Label);

      --  Read only file area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Event_Box);
      Add (Frame, Event_Box);
      Gtk_New (Box.Read_Only_Label);
      Add (Event_Box, Box.Read_Only_Label);
      Object_Return_Callback.Object_Connect
        (Event_Box, "button_press_event",
         Object_Return_Callback.To_Marshaller (On_Read_Only_Pressed'Access),
         Box);

      --  Insert/Overwrite label
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Event_Box);
      Add (Frame, Event_Box);
      Gtk_New (Box.Overwrite_Label, -"Insert");
      Add (Event_Box, Box.Overwrite_Label);

      Box_Callback.Connect
        (Box.Source_View,
         "toggle_overwrite",
         On_Toggle_Overwrite'Access,
         User_Data => Source_Editor_Box (Box));

      --  Filename area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Box.Label_Box, Frame, Expand => True, Fill => True);
      --  Gtk_New (Box.Filename_Label);
      --  ??? Commented out as not used for the moment.

      --  Connect to source buffer signals.

      Box.Cursor_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         "cursor_position_changed",
         Cursor_Position_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box.Status_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         "status_changed",
         Status_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box.Buffer_Info_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         "buffer_information_changed",
         Buffer_Information_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box_Callback.Connect
        (Box.Source_View,
         "destroy",
         On_Box_Destroy'Access,
         User_Data => Source_Editor_Box (Box));

      Show_Cursor_Position (Source_Editor_Box (Box), Line => 1, Column => 1);

      Add_Events (Box.Source_View, Focus_Change_Mask);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, "focus_in_event",
         Object_Return_Callback.To_Marshaller (Focus_In'Access), Box, False);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, "focus_out_event",
         Object_Return_Callback.To_Marshaller (Focus_Out'Access), Box, False);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, "key_press_event",
         Object_Return_Callback.To_Marshaller (Key_Press'Access), Box);

      --  The Contextual Menu handling
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Box.Source_View,
         Object          => Box,
         ID              => Src_Editor_Module_Id,
         Context_Func    => Get_Contextual_Menu'Access);
   end Initialize_Box;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press (Box : access GObject_Record'Class) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      if B.Timestamp_Mode = Check_At_Modify then
         if not Check_Timestamp (B.Source_Buffer, Ask_User => True) then
            --  Do not propagate the key press event
            return True;
         end if;

         B.Timestamp_Mode := Check_At_Focus;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         return False;
   end Key_Press;

   --------------------------
   -- On_Read_Only_Pressed --
   --------------------------

   function On_Read_Only_Pressed
     (Box : access GObject_Record'Class) return Boolean
   is
      Editor : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      Editor.Writable := not Editor.Writable;
      Set_Editable (Editor.Source_View, Editor.Writable);

      if Editor.Writable then
         Set_Text (Editor.Read_Only_Label, -"Writable");
         Add_Controls (Editor.Source_Buffer);
      else
         Set_Text (Editor.Read_Only_Label, -"Read Only");
         Remove_Controls (Editor.Source_Buffer);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         return False;
   end On_Read_Only_Pressed;

   --------------------------
   -- Check_Timestamp_Idle --
   --------------------------

   function Check_Timestamp_Idle (Box : GObject) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      if B.Timestamp_Mode = Check_At_Focus then
         B.Timestamp_Mode := Checking;

         if not Check_Timestamp (B.Source_Buffer, Ask_User => True) then
            --  We'll ask again next time the user wants to modify the file.
            B.Timestamp_Mode := Check_At_Modify;
            return False;
         end if;

         B.Timestamp_Mode := Check_At_Focus;
      end if;

      return False;
   end Check_Timestamp_Idle;

   -------------------------
   -- Get_Total_Ref_Count --
   -------------------------

   function Get_Total_Ref_Count
     (Editor : access Source_Editor_Box_Record)
      return Integer is
   begin
      return Get_Total_Ref_Count (Editor.Source_Buffer);
   end Get_Total_Ref_Count;

   -------------------
   -- Get_Ref_Count --
   -------------------

   function Get_Ref_Count
     (Editor : access Source_Editor_Box_Record)
      return Integer is
   begin
      return Get_Ref_Count (Editor.Source_Buffer);
   end Get_Ref_Count;

   ---------------------
   -- Check_Timestamp --
   ---------------------

   procedure Check_Timestamp
     (Editor : access Source_Editor_Box_Record;
      Force  : Boolean := False)
   is
      B : Boolean;
      pragma Unreferenced (B);

   begin
      Editor.Timestamp_Mode := Check_At_Modify;

      if Get_Status (Editor.Source_Buffer) /= Modified then
         B := Check_Timestamp
           (Editor.Source_Buffer, Ask_User => False, Force => Force);
      else
         B := Check_Timestamp (Editor.Source_Buffer, Ask_User => True,
                               Force => Force);
      end if;
   end Check_Timestamp;

   --------------
   -- Focus_In --
   --------------

   function Focus_In (Box : access GObject_Record'Class) return Boolean is
      Id        : Idle_Handler_Id;
      pragma Unreferenced (Id);

      B         : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      --  We must do the check in an idle callback: otherwise, when the dialog
      --  is pop up on the screen, and since it is modal, then button release
      --  event is never sent to the editor, and there is a drag selection
      --  taking place.

      Id := Object_Idle.Add (Check_Timestamp_Idle'Access, GObject (Box));

      --  Connect the Undo/Redo buttons to the buffer.
      if B.Writable then
         Add_Controls (B.Source_Buffer);
      end if;

      return False;
   end Focus_In;

   ---------------
   -- Focus_Out --
   ---------------

   function Focus_Out (Box : access GObject_Record'Class) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      --  Disconnect the Undo/Redo buttons from the buffer.

      Remove_Controls (B.Source_Buffer);
      return False;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Focus_Out;

   -------------------------
   -- Get_Contextual_Menu --
   -------------------------

   function Get_Contextual_Menu
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event_Widget);
   begin
      return Get_Contextual_Menu (Kernel, Object, Event, Menu);
   end Get_Contextual_Menu;

   -------------------------
   -- Get_Contextual_Menu --
   -------------------------

   function Get_Contextual_Menu
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access
   is
      Editor     : constant Source_Editor_Box := Source_Editor_Box (Object);
      V          : constant Source_View := Editor.Source_View;
      Line       : Gint := 0;
      Column     : Gint := 0;
      Entity_Column : Gint;
      X, Y       : Gint;
      Item       : Gtk_Menu_Item;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Context    : Entity_Selection_Context_Access;
      Filename   : constant VFS.Virtual_File :=
        Get_Filename (Editor.Source_Buffer);
      Result     : Boolean;
      Out_Of_Bounds : Boolean := False;

   begin
      Context := new Entity_Selection_Context;
      Set_Context_Information
        (Context => Context,
         Kernel  => Kernel,
         Creator => Src_Editor_Module_Id);

      --  Click in the line numbers area ?

      if Event /= null
        and then Get_Window (Event) = Get_Window (V, Text_Window_Left)
      then
         Window_To_Buffer_Coords
           (Editor.Source_View, Text_Window_Left,
            Gint (Get_X (Event)), Gint (Get_Y (Event)),
            X, Y);
         Get_Iter_At_Location (Editor.Source_View, Start_Iter, X, Y);
         Line := Get_Line (Start_Iter);
         Place_Cursor (Editor.Source_Buffer, Start_Iter);

         if Menu /= null then
            Gtk_New (Item, -"Goto line...");
            Add (Menu, Item);
            Context_Callback.Object_Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Goto_Line'Access),
               User_Data   => Selection_Context_Access (Context),
               Slot_Object => Editor);

            Gtk_New (Item, -"Goto file spec/body");
            Add (Menu, Item);
            Context_Callback.Object_Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Goto_Other_File'Access),
               User_Data   => Selection_Context_Access (Context),
               Slot_Object => Editor,
               After       => True);

            Gtk_New (Item, -"Goto parent unit");
            Add (Menu, Item);
            Set_Sensitive (Item, False);
         end if;

      --  Else click in the text area

      else
         if Event = null
           or else Get_Event_Type (Event) not in Button_Press .. Button_Release
         then
            Get_Iter_At_Mark (Editor.Source_Buffer, Start_Iter,
                              Get_Insert (Editor.Source_Buffer));
            Line   := Get_Line (Start_Iter);
            Column := Get_Line_Offset (Start_Iter);

         else
            Event_To_Buffer_Coords
              (Editor.Source_View, Event, Line, Column, Out_Of_Bounds);
         end if;

         if Out_Of_Bounds then
            --  Invalid position: the cursor is outside the text.

            Get_Iter_At_Line_Offset
              (Editor.Source_Buffer, Start_Iter, Line, Column);
            Set_File_Information
              (Context,
               File      => Filename,
               Line      => Integer (To_Box_Line (Line)),
               Column    => Integer (To_Box_Column (Column)));

         else
            --  Check whether there is a selection

            Get_Selection_Bounds
              (Editor.Source_Buffer, Start_Iter, End_Iter, Result);

            if Result
              and then Get_Line (Start_Iter) /= Get_Line (End_Iter)
            then
               --  Multiple-line selection: return an area context.

               Unref (Selection_Context_Access (Context));

               declare
                  Area       : File_Area_Context_Access;
                  Start_Line : Integer;
                  End_Line   : Integer;
               begin
                  Area := new File_Area_Context;

                  Set_Context_Information
                    (Context => Area,
                     Kernel  => Kernel,
                     Creator => Src_Editor_Module_Id);
                  Set_File_Information
                    (Area,
                     File => Filename);

                  Start_Line := To_Box_Line (Get_Line (Start_Iter));
                  End_Line   := To_Box_Line (Get_Line (End_Iter));

                  --  Do not consider the last line selected if only the first
                  --  character is selected.

                  if Get_Line_Offset (End_Iter) = 0 then
                     End_Line := End_Line - 1;
                  end if;

                  --  Do not consider the first line selected if only the last
                  --  character is selected.

                  if Ends_Line (Start_Iter) then
                     Start_Line := Start_Line + 1;
                  end if;

                  Set_Area_Information (Area, Start_Line, End_Line);

                  return Selection_Context_Access (Area);
               end;

            elsif not Result
              or else Line /= Get_Line (Start_Iter)
              or else Column < Get_Line_Offset (Start_Iter) - 1
              or else Column > Get_Line_Offset (End_Iter) + 1
            then
               --  No selection, get the entity under the cursor.

               Get_Iter_At_Line_Offset
                 (Editor.Source_Buffer, Start_Iter, Line, Column);
               Search_Entity_Bounds (Start_Iter, End_Iter);
            end if;

            --  Expand the tabs
            Get_Screen_Position
              (Editor.Source_Buffer, Start_Iter, Line, Entity_Column);

            Set_Entity_Information
              (Context,
               Get_Text (Start_Iter, End_Iter),
               To_Box_Column (Entity_Column));
         end if;

         Set_File_Information
           (Context,
            File      => Filename,
            Line      => Integer (To_Box_Line (Line)),
            Column    => Integer (To_Box_Column (Column)));

         if Menu /= null then
            --  Move the cursor at the correct location. The cursor is grabbed
            --  automatically by the kernel when displaying the menu, and this
            --  would result in unwanted scrolling otherwise.

            Place_Cursor (Editor.Source_Buffer, Start_Iter);

            if Has_Entity_Name_Information (Context) then
               Push_State (Get_Kernel (Context), Busy);

               declare
                  Name   : constant String :=
                    Krunch (Entity_Name_Information (Context));
                  Entity : constant Src_Info.Queries.Entity_Information :=
                    Get_Entity (Context);

               begin
                  if Entity /= No_Entity_Information then
                     Gtk_New (Item, -"Goto declaration of " & Name);
                     Add (Menu, Item);
                     Context_Callback.Object_Connect
                       (Item, "activate",
                        Context_Callback.To_Marshaller
                          (On_Goto_Declaration'Access),
                        User_Data   => Selection_Context_Access (Context),
                        Slot_Object => Editor,
                        After       => True);

                     if Is_Subprogram (Entity) then
                        Gtk_New (Item, -"Goto body of " & Name);
                     else
                        Gtk_New (Item, -"Goto full declaration of " & Name);
                     end if;

                     Add (Menu, Item);
                     Context_Callback.Object_Connect
                       (Item, "activate",
                        Context_Callback.To_Marshaller
                          (On_Goto_Next_Body'Access),
                        User_Data   => Selection_Context_Access (Context),
                        Slot_Object => Editor,
                        After       => True);
                  end if;
               end;

               Pop_State (Get_Kernel (Context));
            end if;

            Gtk_New (Item, -"Goto file spec/body");
            Add (Menu, Item);
            Context_Callback.Object_Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Goto_Other_File'Access),
               User_Data   => Selection_Context_Access (Context),
               Slot_Object => Editor,
               After       => True);

            Gtk_New (Item, -"Goto parent unit");
            Add (Menu, Item);
            Set_Sensitive (Item, False);
         end if;
      end if;

      return Selection_Context_Access (Context);
   end Get_Contextual_Menu;

   ------------------------
   -- On_Goto_Other_File --
   ------------------------

   procedure On_Goto_Other_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);

   begin
      Push_State (Kernel, Busy);

      if Has_File_Information (C) then
         declare
            Other_File : constant Virtual_File := Other_File_Name
              (Kernel, File_Information (C));
         begin
            if Other_File /= VFS.No_File then
               Open_File_Editor (Kernel, Other_File, Line => 0);
            end if;
         end;
      end if;

      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Other_File;

   ------------------
   -- On_Goto_Line --
   ------------------

   procedure On_Goto_Line
     (Editor : access GObject_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle)
   is
      Box : constant Source_Editor_Box := Source_Editor_Box (Editor);
   begin
      declare
         Str : constant String := Simple_Entry_Dialog
           (Get_Main_Window (Kernel), -"Goto Line...", -"Enter line number:",
            Win_Pos_Mouse, Get_History (Kernel), "Goto_Line");

      begin
         if Str = "" or else Str (Str'First) = ASCII.NUL then
            return;
         end if;

         Set_Cursor_Location (Box, Positive'Value (Str));

      exception
         when Constraint_Error =>
            Console.Insert
              (Kernel, -"Invalid line number: " & Str, Mode => Error);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Line;

   ------------------
   -- On_Goto_Line --
   ------------------

   procedure On_Goto_Line
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      On_Goto_Line
        (Editor => Source_Editor_Box (Widget), Kernel => Get_Kernel (Context));
   end On_Goto_Line;

   -----------------------
   -- On_Goto_Line_Func --
   -----------------------

   function On_Goto_Line_Func
     (Editor : access GObject_Record'Class) return Boolean is
   begin
      On_Goto_Line (Editor, Source_Editor_Box (Editor).Kernel);
      return True;
   end On_Goto_Line_Func;

   -------------------------
   -- On_Goto_Declaration --
   -------------------------

   procedure On_Goto_Declaration
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Source_Editor_Box (Widget),
         Context => C);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Declaration;

   -----------------------
   -- On_Goto_Next_Body --
   -----------------------

   procedure On_Goto_Next_Body
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => True,
         Editor  => Source_Editor_Box (Widget),
         Context => C);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Next_Body;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box    : out Source_Editor_Box;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize (Box, Kernel, Lang);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Editor_Box_Record;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access) is
   begin
      Initialize_Box (Box, Kernel, Lang => Lang);
   end Initialize;

   ---------------------
   -- Create_New_View --
   ---------------------

   procedure Create_New_View
     (Box    : out Source_Editor_Box;
      Kernel : access Kernel_Handle_Record'Class;
      Source : access Source_Editor_Box_Record) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize_Box
        (Box, Kernel_Handle (Kernel), Source.Source_Buffer,
         Get_Language (Source));
      Box.Writable := Source.Writable;

      --  ??? Is this really useful ?
      Set_Filename (Box, Get_Filename (Source));

      Set_Text (Box.Modified_Label, Get_Text (Source.Modified_Label));

      if Box.Writable then
         Set_Text (Box.Read_Only_Label, -"Writable");
      else
         Set_Text (Box.Read_Only_Label, -"Read Only");
         Set_Editable (Box.Source_View, False);
      end if;
   end Create_New_View;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Box : in out Source_Editor_Box) is
   begin
      Editor_Tooltips.Destroy_Tooltip (Box.Tooltip);
      Unref (Box.Root_Container);
      Box := null;
   end Destroy;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Box    : access Source_Editor_Box_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class) is
   begin
      Add (Parent, Box.Root_Container);

      --  When detaching the Root_Container, the Root_Container is Ref'ed to
      --  avoid its automatic destruction (see procedure Detach below). This
      --  implies that we need to Unref it each time we attach it, except for
      --  the first time (or we might end up destroying the Root_Container, as
      --  it has never been Ref'ed).

      if Box.Never_Attached then
         Box.Never_Attached := False;
      else
         Unref (Box.Root_Container);
      end if;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Box    : access Source_Editor_Box_Record) is
      Parent : constant Gtk_Container :=
        Gtk_Container (Get_Parent (Box.Root_Container));
   begin
      --  Increment the reference counter before detaching the Root_Container
      --  from the parent widget, to make sure it is not automatically
      --  destroyed by gtk.

      Ref (Box.Root_Container);
      Remove (Parent, Box.Root_Container);
   end Detach;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Box : access Source_Editor_Box_Record)
     return Glide_Kernel.Kernel_Handle is
   begin
      return Box.Kernel;
   end Get_Kernel;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   function Needs_To_Be_Saved
     (Editor : access Source_Editor_Box_Record)
      return Boolean is
   begin
      return Editor.Primary and then Needs_To_Be_Saved (Editor.Source_Buffer);
   end Needs_To_Be_Saved;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Editor   : access Source_Editor_Box_Record;
      Filename : VFS.Virtual_File) is
   begin
      Set_Filename (Editor.Source_Buffer, Filename);
   end Set_Filename;

   -------------------------
   -- Set_File_Identifier --
   -------------------------

   procedure Set_File_Identifier
     (Editor   : access Source_Editor_Box_Record;
      Filename : VFS.Virtual_File) is
   begin
      Set_File_Identifier (Editor.Source_Buffer, Filename);
   end Set_File_Identifier;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Editor : access Source_Editor_Box_Record) return Virtual_File is
   begin
      return Get_Filename (Editor.Source_Buffer);
   end Get_Filename;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Force_Focus     : Boolean := True;
      Success         : out Boolean) is
   begin
      Load_File (Editor.Source_Buffer, Filename, Lang_Autodetect, Success);
      Set_Cursor_Location (Editor, 1, 1, Force_Focus);

      if Success then
         Set_Filename (Editor.Source_Buffer, Filename);
         Set_Text (Editor.Modified_Label, -"Unmodified");

         Editor.Writable := Is_Writable (Filename);

         if Editor.Writable then
            Set_Text (Editor.Read_Only_Label, -"Writable");
         else
            Set_Text (Editor.Read_Only_Label, -"Read Only");
            Set_Editable (Editor.Source_View, False);
         end if;
      end if;
   end Load_File;

   ---------------------
   -- Load_Empty_File --
   ---------------------

   procedure Load_Empty_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : VFS.Virtual_File;
      Lang_Handler    : Language_Handlers.Language_Handler;
      Lang_Autodetect : Boolean := True) is
   begin
      if Lang_Autodetect then
         Set_Language
           (Editor.Source_Buffer,
            Get_Language_From_File (Lang_Handler, Filename));
      end if;

      Set_Cursor_Location (Editor, 1, 1);
      Set_Filename (Editor.Source_Buffer, Filename);
      Set_Text (Editor.Modified_Label, -"Unmodified");

      Editor.Writable := True;
      Set_Text (Editor.Read_Only_Label, -"Writable");
   end Load_Empty_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : VFS.Virtual_File := VFS.No_File;
      Success  : out Boolean)
   is
      File       : constant VFS.Virtual_File :=
        Get_Filename (Editor.Source_Buffer);
      Constructs : Construct_List;
      Info       : Construct_Access;
      New_Base_Name : GNAT.OS_Lib.String_Access;

      use type Basic_Types.String_Access;
   begin
      --  Do not authorize saving a read-only file, unless we save it to
      --  another disk file.

      if not Editor.Writable
        and then Filename = VFS.No_File
      then
         Success := False;
         return;
      end if;

      --  ??? Should we reset the read-only status to False: either the
      --  file already had that status, or we just saved a read-only file to
      --  a different disk file, and thus this is now writable

      Success := True;

      if Filename = VFS.No_File then
         if File = VFS.No_File then
            --  ??? This is Ada specific
            --  Figure out what the name of the file should be, based on the
            --  unit <-> file name mapping

            Parse_Constructs
              (Ada_Lang, Get_Slice (Editor.Source_Buffer, 0, 0), Constructs);
            Info := Constructs.Last;

            if Info = null
              or else
                (Info.Category /= Cat_Procedure
                 and then Info.Category /= Cat_Function
                 and then Info.Category /= Cat_Package)
              or else Info.Name = null
            then
               --  No unit name found
               New_Base_Name := new String'("");
            else
               --  Info.Name is a valid Ada unit name

               if Info.Is_Declaration then
                  New_Base_Name := new String'
                    (Get_Source_Filename
                       (To_Lower (Info.Name.all) & "%s",
                        Get_Project (Editor.Kernel)));
               else
                  New_Base_Name := new String'
                    (Get_Source_Filename
                       (To_Lower (Info.Name.all) & "%b",
                        Get_Project (Editor.Kernel)));
               end if;
            end if;

            Free (Constructs);

            declare
               Name : constant Virtual_File :=
                 Select_File
                   (Title             => -"Save File As",
                    Parent            => Get_Main_Window (Editor.Kernel),
                    Default_Name      => New_Base_Name.all,
                    Use_Native_Dialog =>
                      Get_Pref (Editor.Kernel, Use_Native_Dialogs),
                    Kind              => Save_File,
                    History           => Get_History (Editor.Kernel));

            begin
               GNAT.OS_Lib.Free (New_Base_Name);

               if Name = VFS.No_File then
                  Success := False;
                  return;
               end if;

               if Is_Regular_File (Name) then
                  if Message_Dialog
                    (Msg => Base_Name (Name)
                       & (-" already exists. Do you want to overwrite ?"),
                     Dialog_Type => Confirmation,
                     Buttons => Button_OK or Button_Cancel,
                     Title => "Confirm overwriting",
                     Parent => Get_Main_Window (Editor.Kernel)) /= Button_OK
                  then
                     Success := False;
                  end if;
               end if;

               if Success then
                  Save_To_File (Editor.Source_Buffer, Name, Success);
               end if;
            end;
         else
            if not Check_Timestamp (Editor.Source_Buffer, Ask_User => False)
              and then Message_Dialog
                (Msg => Base_Name (File)
                        & (-" changed on disk. Do you want to overwrite ?"),
                 Dialog_Type => Confirmation,
                 Buttons => Button_OK or Button_Cancel,
                 Title => -"File changed on disk",
                 Parent => Get_Main_Window (Editor.Kernel)) /= Button_OK
            then
               Success := False;
            else
               Save_To_File (Editor.Source_Buffer, File, Success);
            end if;
         end if;

      else
         if Is_Regular_File (Filename) then
            if Message_Dialog
              (Msg => Base_Name (Filename)
               & (-" already exists. Do you want to overwrite ?"),
               Dialog_Type => Confirmation,
               Buttons => Button_OK or Button_Cancel,
               Title => "Confirm overwriting",
               Parent => Get_Main_Window (Editor.Kernel)) /= Button_OK
            then
               Success := False;
            end if;
         end if;

         if Success then
            Save_To_File (Editor.Source_Buffer, Filename, Success);
         end if;
      end if;

      if Success then
         Set_Text (Editor.Modified_Label, -"Saved");
      end if;
   end Save_To_File;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Editor : access Source_Editor_Box_Record;
      Lang   : Language.Language_Access := null) is
   begin
      Set_Language (Editor.Source_Buffer, Lang);
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Editor : access Source_Editor_Box_Record)
      return Language.Language_Access is
   begin
      return Get_Language (Editor.Source_Buffer);
   end Get_Language;

   -----------------------
   -- Is_Valid_Location --
   -----------------------

   function Is_Valid_Location
     (Editor : access Source_Editor_Box_Record;
      Line   : Positive;
      Column : Positive := 1) return Boolean
   is
      Buffer_Line : constant Gint := To_Buffer_Line (Line);
      Buffer_Col  : constant Gint := To_Buffer_Column (Column);
   begin
      return Is_Valid_Position (Editor.Source_Buffer, Buffer_Line, Buffer_Col);
   end Is_Valid_Location;

   -------------------------
   -- Set_Screen_Location --
   -------------------------

   procedure Set_Screen_Location
     (Editor      : access Source_Editor_Box_Record;
      Line        : Positive;
      Column      : Positive := 1;
      Force_Focus : Boolean  := True)
   is
      Buffer_Line : constant Gint := To_Buffer_Line (Line);
      Buffer_Col  : constant Gint := To_Buffer_Column (Column);
   begin
      if Force_Focus then
         Grab_Focus (Editor.Source_View);
      end if;

      if Is_Valid_Position (Editor.Source_Buffer, Buffer_Line, 0) then
         Set_Screen_Position (Editor.Source_Buffer, Buffer_Line, Buffer_Col);
         Scroll_To_Cursor_Location (Editor.Source_View, True);

      else
         Console.Insert
           (Editor.Kernel, -"Invalid source location: " & Image (Line) &
              ':' & Image (Column), Mode => Error);
      end if;
   end Set_Screen_Location;

   -------------------------
   -- Set_Cursor_Location --
   -------------------------

   procedure Set_Cursor_Location
     (Editor      : access Source_Editor_Box_Record;
      Line        : Positive;
      Column      : Positive := 1;
      Force_Focus : Boolean  := True)
   is
      Editable_Line : constant Editable_Line_Type := Editable_Line_Type (Line);

   begin
      if Is_Valid_Position (Editor.Source_Buffer, Editable_Line, Column) then
         if Force_Focus then
            Grab_Focus (Editor.Source_View);
         end if;

         Set_Cursor_Position (Editor.Source_Buffer, Editable_Line, Column);
         Scroll_To_Cursor_Location (Editor.Source_View, True);

      else
         if Column = 1 then
            Console.Insert
              (Editor.Kernel, -"Invalid line number: " & Image (Line),
               Mode => Error);
         else
            Console.Insert
              (Editor.Kernel, -"Invalid source location: " & Image (Line) &
               ':' & Image (Column), Mode => Error);
         end if;
      end if;
   end Set_Cursor_Location;

   -------------------------
   -- Get_Cursor_Location --
   -------------------------

   procedure Get_Cursor_Location
     (Editor : access Source_Editor_Box_Record;
      Line   : out Positive;
      Column : out Positive)
   is
      Buffer_Line : Gint;
      Buffer_Col  : Gint;
   begin
      Get_Cursor_Position (Editor.Source_Buffer, Buffer_Line, Buffer_Col);
      Line   := To_Box_Line (Buffer_Line);
      Column := To_Box_Column (Buffer_Col);
   end Get_Cursor_Location;

   -------------------------------
   -- Scroll_To_Cursor_Location --
   -------------------------------

   procedure Scroll_To_Cursor_Location
     (Editor : access Source_Editor_Box_Record) is
   begin
      Scroll_To_Cursor_Location (Editor.Source_View);
   end Scroll_To_Cursor_Location;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : out Positive;
      Start_Column : out Positive;
      End_Line     : out Positive;
      End_Column   : out Positive;
      Found        : out Boolean)
   is
      Start_L : Gint;
      Start_C : Gint;
      End_L   : Gint;
      End_C   : Gint;
   begin
      Get_Selection_Bounds
        (Editor.Source_Buffer, Start_L, Start_C, End_L, End_C, Found);
      Start_Line   := To_Box_Line (Start_L);
      Start_Column := To_Box_Column (Start_C);
      End_Line     := To_Box_Line (End_L);
      End_Column   := To_Box_Column (End_C);
   end Get_Selection_Bounds;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Natural := 0;
      End_Column   : Natural := 0) return String is
   begin
      return Get_Slice
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column));
   end Get_Slice;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;
      Text         : String)
   is
      C : Editor_Replace_Slice;
   begin
      Create
        (C,
         Editor.Source_Buffer,
         Editable_Line_Type (Start_Line),
         Natural (Start_Column),
         Editable_Line_Type (End_Line),
         Natural (End_Column),
         Text);

      External_End_Action (Editor.Source_Buffer);
      Enqueue (Editor.Source_Buffer, Command_Access (C));
   end Replace_Slice;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Editor : access Source_Editor_Box_Record) is
   begin
      Select_All (Editor.Source_Buffer);
   end Select_All;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;
      Expand_Tabs  : Boolean := True) is
   begin
      Select_Region
        (Editor.Source_Buffer,
         To_Buffer_Line (Start_Line), To_Buffer_Column (Start_Column),
         To_Buffer_Line (End_Line), To_Buffer_Column (End_Column),
         Expand_Tabs);
   end Select_Region;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard (Editor : access Source_Editor_Box_Record) is
   begin
      Cut_Clipboard
        (Editor.Source_Buffer, Gtk.Clipboard.Get,
         Default_Editable => Editor.Writable);
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard (Editor : access Source_Editor_Box_Record) is
   begin
      Copy_Clipboard (Editor.Source_Buffer, Gtk.Clipboard.Get);
   end Copy_Clipboard;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard (Editor : access Source_Editor_Box_Record) is
      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      --  Delete the selected region if it exists.
      --  ??? This works around a bug which it seems is in gtk+, to be
      --  investigated.
      --  Scenario to reproduce the gtk bug : do a "select_region" and then
      --  a "paste_clipboard", twice. (See C703-005)

      if Selection_Exists (Editor.Source_Buffer) then
         Result := Delete_Selection (Editor.Source_Buffer, False, False);
      end if;

      Paste_Clipboard
        (Editor.Source_Buffer, Gtk.Clipboard.Get,
         Default_Editable => Editor.Writable);
   end Paste_Clipboard;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Info       : Glide_Kernel.Modules.Line_Information_Data) is
   begin
      Add_File_Information
        (Editor.Source_Buffer,
         Identifier,
         Gtk_Widget (Editor.Source_View),
         Info);
   end Add_File_Information;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Editor         : access Source_Editor_Box_Record;
      Identifier     : String;
      Stick_To_Data  : Boolean;
      Every_Line     : Boolean) is
   begin
      Create_Line_Information_Column
        (Editor.Source_Buffer, Identifier, Stick_To_Data, Every_Line);
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Editor         : access Source_Editor_Box_Record;
      Identifier     : String) is
   begin
      Remove_Line_Information_Column (Editor.Source_Buffer, Identifier);
   end Remove_Line_Information_Column;

   ---------------------
   -- Add_Blank_Lines --
   ---------------------

   function Add_Blank_Lines
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      GC     : Gdk.GC.Gdk_GC;
      Text   : String;
      Number : Positive) return Gtk.Text_Mark.Gtk_Text_Mark is
   begin
      return Add_Blank_Lines (Editor.Source_Buffer, Line, GC, Text, Number);
   end Add_Blank_Lines;

   ------------------
   --  Create_Mark --
   ------------------

   function Create_Mark
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      Column : Positive) return Gtk.Text_Mark.Gtk_Text_Mark is
   begin
      return Create_Mark (Editor.Source_Buffer, Line, Column);
   end Create_Mark;

   ---------------------------
   -- Add_Line_Highlighting --
   ---------------------------

   procedure Add_Line_Highlighting
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      Id     : String) is
   begin
      Add_Line_Highlighting (Editor.Source_Buffer, Line, Id);
   end Add_Line_Highlighting;

   ------------------------------
   -- Remove_Line_Highlighting --
   ------------------------------

   procedure Remove_Line_Highlighting
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      Id     : String) is
   begin
      Remove_Line_Highlighting (Editor.Source_Buffer, Line, Id);
   end Remove_Line_Highlighting;

   --------------------
   -- Scroll_To_Mark --
   --------------------

   procedure Scroll_To_Mark
     (Editor : access Source_Editor_Box_Record;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Length : Natural := 0)
   is
      Iter           : Gtk_Text_Iter;
      Mark_Iter      : Gtk_Text_Iter;
      Success        : Boolean;

   begin
      Get_Iter_At_Mark (Editor.Source_Buffer, Iter, Mark);

      Success := Scroll_To_Iter
        (Editor.Source_View, Iter, 0.0, True, 0.5, 0.5);
      Place_Cursor (Editor.Source_Buffer, Iter);

      if Length /= 0 then
         Get_Iter_At_Mark (Editor.Source_Buffer, Mark_Iter, Mark);
         Copy (Mark_Iter, Iter);

         Forward_Chars (Iter, Gint (Length), Success);

         --  If there is only one character to highlight and that character
         --  is an ASCII.LF, we do not highlight it, since nothing at all
         --  would be visible, due to the way the GtkTextView highlights
         --  line ends.

         if Success
           and then not
             (Length = 1
              and then Get_Char (Mark_Iter) = ASCII.LF)
         then
            Select_Region
              (Editor.Source_Buffer,
               Get_Line (Mark_Iter),
               Get_Line_Offset (Mark_Iter),
               Get_Line (Iter),
               Get_Line_Offset (Iter),
               Expand_Tabs => False);
         end if;
      end if;
   end Scroll_To_Mark;

   -------------------
   -- Get_Last_Line --
   -------------------

   function Get_Last_Line
     (Editor   : access Source_Editor_Box_Record) return Positive
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_End_Iter (Editor.Source_Buffer, Iter);
      return To_Box_Line (Get_Line (Iter));
   end Get_Last_Line;

   ---------------------
   -- Get_Block_Start --
   ---------------------

   function Get_Block_Start
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Natural
   is
      B_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Editor.Source_Buffer, Line);
      Block : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);
   begin
      return Natural (Block.First_Line);
   end Get_Block_Start;

   -------------------
   -- Get_Block_End --
   -------------------

   function Get_Block_End
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Natural
   is
      B_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Editor.Source_Buffer, Line);
      Block : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);
   begin
      return Natural (Block.Last_Line);
   end Get_Block_End;

   --------------------
   -- Get_Block_Type --
   --------------------

   function Get_Block_Type
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String
   is
      B_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Editor.Source_Buffer, Line);
      Block : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);
   begin
      return Language_Category'Image (Block.Block_Type);
   end Get_Block_Type;

   ---------------------
   -- Get_Block_Level --
   ---------------------

   function Get_Block_Level
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return Natural
   is
      B_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Editor.Source_Buffer, Line);
      Block : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);
   begin
      return Natural (Block.Indentation_Level);
   end Get_Block_Level;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Editor : access Source_Editor_Box_Record)
      return String
   is
      Begin_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Bounds (Editor.Source_Buffer, Begin_Iter, End_Iter);
      return Get_Text (Begin_Iter, End_Iter);
   end Get_Buffer;

   ----------
   -- Undo --
   ----------

   procedure Undo (Editor : access Source_Editor_Box_Record) is
   begin
      if Editor.Writable then
         Undo (Editor.Source_Buffer);
      end if;
   end Undo;

   ----------
   -- Redo --
   ----------

   procedure Redo (Editor : access Source_Editor_Box_Record) is
   begin
      if Editor.Writable then
         Redo (Editor.Source_Buffer);
      end if;
   end Redo;

   --------------
   -- Get_View --
   --------------

   function Get_View (Editor : access Source_Editor_Box_Record)
      return Src_Editor_View.Source_View is
   begin
      return Editor.Source_View;
   end Get_View;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer (Editor : access Source_Editor_Box_Record)
      return Src_Editor_Buffer.Source_Buffer is
   begin
      return Editor.Source_Buffer;
   end Get_Buffer;

   ------------------
   -- Get_Writable --
   ------------------

   function Get_Writable
     (Editor : access Source_Editor_Box_Record) return Boolean is
   begin
      return Editor.Writable;
   end Get_Writable;

end Src_Editor_Box;
