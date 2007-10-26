-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2007, AdaCore                --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;

with Gdk;                       use Gdk;
with Gdk.Event;                 use Gdk.Event;
with Gdk.GC;                    use Gdk.GC;
with Gdk.Main;                  use Gdk.Main;
with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib;                      use Glib;

with Gtk;                       use Gtk;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Drawing_Area;          use Gtk.Drawing_Area;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Editor;           use Commands.Editor;
with Entities.Queries;          use Entities.Queries;
with Entities;                  use Entities;
with Find_Utils;                use Find_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Charsets;       use GPS.Kernel.Charsets;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GUI_Utils;                 use GUI_Utils;
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with Language_Handlers;          use Language_Handlers;
with Pango.Layout;              use Pango.Layout;
with Projects.Registry;         use Projects.Registry;
with Projects;                  use Projects;
with Src_Editor_Box.Tooltips;   use Src_Editor_Box.Tooltips;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Module.Markers; use Src_Editor_Module.Markers;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with Tooltips;                  use Tooltips;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body Src_Editor_Box is

   Me : constant Debug_Handle := Create ("Source_Editor");

   procedure Setup (Data : Source_Editor_Box; Id : Handler_Id);
   package Box_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Source_Editor_Box,
      Setup       => Setup);

   --------------------------
   -- Forward declarations --
   --------------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" signal.

   procedure Get_Contextual_Menu
     (Context      : in out Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Same as the public Get_Contextual_Menu, Event_Widget is ignored.

   procedure Show_Cursor_Position
     (Box    : access Source_Editor_Box_Record'Class;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type);
   --  Redraw the cursor position in the Line/Column areas of the status bar

   procedure Show_Which_Function
     (Box  : Source_Editor_Box;
      Name : String);
   pragma Inline (Show_Which_Function);
   --  Name if not null is the name of the enclosing subprogram, it is
   --  displayed on the left of the status bar.

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
   --  Reflect the change in buffer information

   procedure Status_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Reflect the change in buffer status

   procedure Filename_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Reflect the change in buffer filename

   procedure On_Box_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Callback for the "destroy" signal

   procedure On_Goto_Declaration_Of
     (Kernel : access GObject_Record'Class;
      Entity : Entity_Information);
   --  Jump to the declaration of the given entity

   procedure On_Goto_Body_Of
     (Kernel : access GObject_Record'Class;
      Entity : Entity_Information);
   --  Jump to the body of the given entity

   procedure On_Toggle_Overwrite
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Callback to be called when the view receives the "toggle_overwrite"
   --  signal.

   function Focus_In (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_in event. This checks whether the physical file
   --  on the disk is more recent than the one that was read for the editor.

   function Focus_Out (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_out event

   function Key_Press (Box : access GObject_Record'Class) return Boolean;
   --  Check whether the file has been modified on disk

   function On_Read_Only_Pressed
     (Box : access GObject_Record'Class) return Boolean;
   --  Toggle read-only/writable state of a given box

   function Check_Timestamp_Idle (Box : GObject) return Boolean;
   --  Idle callback to check that the timestamp of a file hasn't changed

   procedure Go_To_Closest_Match
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : Virtual_File;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type;
      Entity   : Entity_Information);
   --  Open an editor for Filename. Go to Line, Column, or the nearest
   --  occurrence of Entity close by.

   procedure Set_Writable (Views : Views_Array; Writable : Boolean);
   --  Make the views editable and update their read-only label according to
   --  Writable.
   --  The purpose of this procedure is to share some code between the public
   --  subprograms Set_Writable and Check_Writable.

   ----------------------------------
   -- The contextual menu handling --
   ----------------------------------

   procedure On_Goto_Line
     (Widget  : access GObject_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Callback for the "Goto Line" contextual menu

   function On_Goto_Line_Func
     (Editor : access GObject_Record'Class) return Boolean;
   --  Callback when clicking on the line number in the status bar

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
      Line   : Editable_Line_Type;
      Dummy  : Character_Offset_Type;
      File   : VFS.Virtual_File := Get_Filename (Source.Source_Buffer);
   begin
      if File = VFS.No_File then
         File := Get_File_Identifier (Source.Source_Buffer);
      end if;

      Get_Cursor_Position (Source.Source_Buffer, Line, Dummy);
      Push_Marker_In_History
        (Kernel => Source.Kernel,
         Marker => Create_File_Marker
           (Kernel => Source.Kernel,
            File   => File,
            Line   => Line,
            Column => 1));
      --  ??? Any reason to throw away the column and putting 1 here?
   end Add_Navigation_Location;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Source_Editor_Box; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data);
   end Setup;

   --------------------------
   -- Read_Only_By_Default --
   --------------------------

   Read_Only_Set : Boolean := False;

   procedure Read_Only_By_Default (State : Boolean := True) is
   begin
      Read_Only_Set := State;
   end Read_Only_By_Default;

   ------------------------------
   -- Goto_Declaration_Or_Body --
   ------------------------------

   procedure Goto_Declaration_Or_Body
     (Kernel  : access Kernel_Handle_Record'Class;
      To_Body : Boolean;
      Editor  : access Source_Editor_Box_Record'Class;
      Context : Selection_Context)
   is
      Entity   : Entity_Information;
      Location : File_Location;
      L        : Editable_Line_Type;
      C        : Visible_Column_Type;
      Filename : Virtual_File;

   begin
      if Get_Filename (Editor) = VFS.No_File then
         Console.Insert
           (Kernel, -"Cross-references not possible on unamed files",
            Mode => Error);
         return;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      Entity := Get_Entity (Context, Ask_If_Overloaded => True);

      if Entity = null then
         --  Probably means that we either could not locate the ALI file,
         --  or it could also be that we failed to parse it. Either way,
         --  a message should have already been printed. So, just abort.

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

      Ref (Entity);

      if To_Body then
         Find_Next_Body
           (Entity           => Entity,
            Current_Location =>
              (File   => Get_Or_Create
                 (Db   => Get_Database (Get_Kernel (Context)),
                  File => File_Information (Context)),
               Line   => GPS.Kernel.Contexts.Line_Information (Context),
               Column =>
                 GPS.Kernel.Contexts.Entity_Column_Information (Context)),
            Location         => Location);

         if Location = Entities.No_File_Location then
            Location := Get_Declaration_Of (Entity);
         end if;

         --  Open the file, and reset Source to the new editor in order to
         --  highlight the region returned by the Xref query.

         L := Convert (Get_Line (Location));
         C := Get_Column (Location);

         Filename := Get_Filename (Get_File (Location));
         Trace (Me, "Goto_Declaration_Or_Body: Opening file "
                & Full_Name (Filename).all);
      else
         --  Open the file, and reset Source to the new editor in order to
         --  highlight the region returned by the Xref query.

         L := Convert (Get_Line (Get_Declaration_Of (Entity)));
         C := Get_Column (Get_Declaration_Of (Entity));
         Filename := Get_Filename (Get_File (Get_Declaration_Of (Entity)));
      end if;

      Go_To_Closest_Match (Kernel, Filename, L, C, Entity);

      Unref (Entity);
      Pop_State (Kernel_Handle (Kernel));

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Kernel_Handle (Kernel));
   end Goto_Declaration_Or_Body;

   -------------------------
   -- Go_To_Closest_Match --
   -------------------------

   procedure Go_To_Closest_Match
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : Virtual_File;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type;
      Entity   : Entity_Information)
   is
      Length            : constant Natural := Get_Name (Entity).all'Length;
      Source            : Source_Editor_Box;
      File_Up_To_Date   : Boolean;
      L                 : Natural;
      Is_Case_Sensitive : Boolean;
      Arg               : GNAT.Strings.String_Access;

      Char_Column       : Character_Offset_Type;
   begin
      if Dir_Name (Filename).all = "" then
         Insert (Kernel, -"File not found: "
                 & Base_Name (Filename), Mode => Error);
         return;
      end if;

      Source := Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      if Source /= null then
         Add_Navigation_Location (Source);
      end if;

      Open_File_Editor
        (Kernel, Filename, Natural (Line), Column,
         Column + Visible_Column_Type (Length),
         Enable_Navigation => True);

      --  Find the correct location for the entity, in case it is in fact
      --  different from what was found in the LI file (ie for instance the LI
      --  was older than the destination file).

      Source := Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

      if Source /= null then

         Char_Column := Collapse_Tabs (Source.Source_Buffer, Line, Column);

         --  Find the closest match of the entity, in case the LI file wasn't
         --  up-to-date.

         File_Up_To_Date := Is_Valid_Position
             (Source.Source_Buffer, Line, Char_Column)
           and then Is_Valid_Position
             (Source.Source_Buffer, Line,
              Char_Column + Character_Offset_Type (Length));

         Is_Case_Sensitive := Get_Language_Context
           (Get_Language (Source.Source_Buffer)).Case_Sensitive;

         File_Up_To_Date := File_Up_To_Date
           and then Equal
             (Get_Text
                  (Source.Source_Buffer,
                   Line, Char_Column,
                   Line,
                   Char_Column + Character_Offset_Type (Length)),
              Get_Name (Entity).all,
              Case_Sensitive => Is_Case_Sensitive);

         --  Search for the closest reference to the entity if
         --  necessary. Otherwise, there's nothing to be done, since the region
         --  was already selected when opening the editor
         if not File_Up_To_Date then
            Console.Insert
              (Kernel,
               -("The cross-reference information and the destination file do"
                 & " not match, the cursor was set at the closest reference"
                 & " to ") & Get_Name (Entity).all);

            --  Search for the closest reference to entity, and highlight the
            --  appropriate region in the editor.

            declare
               Buffer : GNAT.Strings.String_Access;
               Col    : Visible_Column_Type;
            begin
               L := Convert (Line);
               Buffer := Get_String (Source.Source_Buffer);
               Find_Closest_Match
                 (Buffer.all, L, Char_Column, Get_Name (Entity).all,
                  Case_Sensitive => Get_Language_Context
                    (Get_Language (Source.Source_Buffer)).Case_Sensitive);
               Free (Buffer);

               Col := Expand_Tabs
                 (Source.Source_Buffer, Line, Char_Column);
               Open_File_Editor
                 (Kernel, Filename, L, Col,
                  Col + Visible_Column_Type (Length), False);
               --  ??? Calcluation for the end column is wrong if there is
               --  an ASCII.HT within Length distance of Col.
            end;
         end if;
      end if;

      Arg := new String'(Full_Name (Filename).all);
      Execute_GPS_Shell_Command (Kernel, "Editor.cursor_center", (1 => Arg));
      Free (Arg);
   end Go_To_Closest_Match;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Editor : access Source_Editor_Box_Record) is
   begin
      Grab_Focus (Editor.Source_View);
   end Grab_Focus;

   -----------------
   -- To_Box_Line --
   -----------------

   function To_Box_Line
     (B    : Source_Buffer;
      Line : Gint) return Natural
   is
      The_Line : Natural;
   begin
      The_Line := Natural (Get_Editable_Line (B, Buffer_Line_Type (Line + 1)));

      --  If the line is not an editable line, return 1.

      if The_Line = 0 then
         return 1;
      else
         return The_Line;
      end if;
   end To_Box_Line;

   -------------------
   -- To_Box_Column --
   -------------------

   function To_Box_Column (Col : Gint) return Character_Offset_Type is
   begin
      return Character_Offset_Type (Col + 1);
   end To_Box_Column;

   --------------------------
   -- Show_Cursor_Position --
   --------------------------

   procedure Show_Cursor_Position
     (Box    : access Source_Editor_Box_Record'Class;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type) is
   begin
      Set_Text
        (Box.Cursor_Loc_Label,
         Image (Integer (Line)) & ':' & Image (Integer (Column)));
   end Show_Cursor_Position;

   -------------------------
   -- Show_Which_Function --
   -------------------------

   procedure Show_Which_Function
     (Box  : Source_Editor_Box;
      Name : String) is
   begin
      Set_Text (Box.Function_Label, Name);
   end Show_Which_Function;

   ---------------------------
   -- Clear_Subprogram_Name --
   ---------------------------

   procedure Clear_Subprogram_Name
     (Editor : access Source_Editor_Box_Record) is
   begin
      Set_Text (Editor.Function_Label, "");
   end Clear_Subprogram_Name;

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
            if Child /= null then
               Set_Icon (Child, File_Pixbuf);
               Ref (File_Pixbuf);
            end if;

         when Unsaved =>
            Set_Text (Box.Modified_Label, -"Unsaved");
            if Child /= null then
               Set_Icon (Child, File_Unsaved_Pixbuf);
               Ref (File_Unsaved_Pixbuf);
            end if;

         when Saved =>
            Set_Text (Box.Modified_Label, -"Saved");
            if Child /= null then
               Set_Icon (Child, File_Pixbuf);
               Ref (File_Pixbuf);
            end if;

         when Modified =>
            Set_Text (Box.Modified_Label, -"Modified");
            if Child /= null then
               Set_Icon (Child, File_Modified_Pixbuf);
               Ref (File_Modified_Pixbuf);
            end if;
      end case;

      File_Status_Changed
        (Get_Kernel (Box),
         Get_Filename (Box.Source_Buffer),
         Get_Status (Box.Source_Buffer));
   exception
      when E : others => Trace (Exception_Handle, E);
   end Status_Changed_Handler;

   ------------------------------
   -- Filename_Changed_Handler --
   ------------------------------

   procedure Filename_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer, Params);
      Child : constant MDI_Child := Find_Child (Box.Kernel, Box);
   begin
      --  Update the title
      Set_Title
        (Child,
         Box.Source_Buffer.Get_Filename.Full_Name.all,
         Box.Source_Buffer.Get_Filename.Base_Name);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Filename_Changed_Handler;

   --------------------------------
   -- Buffer_Information_Handler --
   --------------------------------

   procedure Buffer_Information_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer, Params);

      Info : constant Extra_Information_Array_Access :=
               Get_Extra_Information (Box.Source_Buffer);
      Label : Gtk_Label;

   begin
      if Box.Buffer_Info_Frames /= null then
         for J in Box.Buffer_Info_Frames'Range loop
            Remove (Box.Label_Box, Box.Buffer_Info_Frames (J).Frame);
            Remove (Box.Label_Box, Box.Buffer_Info_Frames (J).Separator);
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

         Gtk_New (Box.Buffer_Info_Frames (J).Frame);
         Set_Shadow_Type (Box.Buffer_Info_Frames (J).Frame, Shadow_None);

         Gtk_New_Vseparator (Box.Buffer_Info_Frames (J).Separator);
         Pack_End
           (Box.Label_Box,
            Box.Buffer_Info_Frames (J).Separator,
            Expand => False,
            Fill => False);

         Add (Box.Buffer_Info_Frames (J).Frame, Label);

         Pack_End
           (Box.Label_Box,
            Box.Buffer_Info_Frames (J).Frame,
            Expand  => False,
            Fill    => True,
            Padding => 0);
      end loop;

      Show_All (Box.Label_Box);
   end Buffer_Information_Handler;

   --------------------------
   -- Show_Subprogram_Name --
   --------------------------

   procedure Show_Subprogram_Name
     (Box             : Source_Editor_Box;
      Subprogram_Name : String) is
   begin
      Show_Which_Function (Box, Subprogram_Name);
   end Show_Subprogram_Name;

   -------------------------------------
   -- Cursor_Position_Changed_Handler --
   -------------------------------------

   procedure Cursor_Position_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer);
      Child : MDI_Child;
      File  : VFS.Virtual_File := Get_Filename (Box.Source_Buffer);
   begin
      if File = VFS.No_File then
         File := Get_File_Identifier (Box.Source_Buffer);
      end if;

      Box.Current_Line :=
        Editable_Line_Type (Values.Get_Int (Values.Nth (Params, 1)));

      --  In case there are multiple views, we only want to change the one that
      --  last had the focus. Otherwise, they would all end up with the same
      --  line number, which is inaccurate.
      --  The box might not have the focus currently: if we are for instance
      --  changing the current line from the "Go to line" dialog, the latter
      --  still has the focus at this point.

      Child := Find_Editor (Box.Kernel, File);

      if Child /= null and then Get_Widget (Child) = Gtk_Widget (Box) then
         Show_Cursor_Position
           (Box,
            Line   => Box.Current_Line,
            Column => Character_Offset_Type
              (Values.Get_Int (Values.Nth (Params, 2))));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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
   begin
      Disconnect (Box.Source_Buffer, Box.Cursor_Handler);
      Disconnect (Box.Source_Buffer, Box.Status_Handler);
      Disconnect (Box.Source_Buffer, Box.Buffer_Info_Handler);

      if Box.Default_GC /= null then
         Unref (Box.Bg_GC);
         Unref (Box.Default_GC);
      end if;

      Delete (Box.Source_View);

      --  Remove the idle handler if it was registered
      if Box.Check_Timestamp_Registered then
         Idle_Remove (Box.Check_Timestamp_Id);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Toggle_Overwrite;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Editor_Box_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access)
   is
      Frame          : Gtk_Frame;
      Event_Box      : Gtk_Event_Box;
      Scrolling_Area : Gtk_Scrolled_Window;
      Drawing_Area   : Gtk_Drawing_Area;
      Hbox           : Gtk_Hbox;
      Separator      : Gtk_Vseparator;

   begin
      Initialize_Vbox (Box, Homogeneous => False);

      Box.Kernel := Kernel;

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => True, Fill => True);

      Gtk_New (Drawing_Area);
      Set_Size_Request (Drawing_Area, 1, -1);

      Pack_Start (Hbox, Drawing_Area, Expand => False, Fill => False);

      Gtk_New (Scrolling_Area);
      Set_Policy
        (Scrolling_Area,
         H_Scrollbar_Policy => Policy_Automatic,
         V_Scrollbar_Policy => Policy_Automatic);
      Pack_End (Hbox, Scrolling_Area, Expand => True, Fill => True);

      if Source = null then
         Gtk_New (Box.Source_Buffer, Kernel, Lang => Lang);
      else
         Box.Source_Buffer := Source;
      end if;

      Gtk_New (Box.Source_View,
               Scrolling_Area,
               Drawing_Area,
               Box.Source_Buffer, Kernel);

      Add (Scrolling_Area, Box.Source_View);

      if Source = null then
         --  The newly created buffer is now under the responsability of the
         --  view.
         Unref (Box.Source_Buffer);
      end if;

      Set_Tooltip
        (Tooltip   => Create_Tooltips (Box),
         On_Widget => Box.Source_View,
         Timeout   => Guint32 (Get_Pref (Tooltip_Timeout)));

      --  The status bar, at the bottom of the window...

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_Etched_In);
      Pack_Start (Box, Frame, Expand => False, Fill => False);

      Gtk_New_Hbox (Box.Label_Box, Homogeneous => False, Spacing => 2);
      Add (Frame, Box.Label_Box);

      --  Avoid resizing the main window whenever a label is changed.
      Set_Resize_Mode (Box.Label_Box, Resize_Queue);

      --  Line:Column number area...
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_None);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => False);
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
        (Event_Box, Signal_Button_Press_Event, On_Goto_Line_Func'Access, Box);

      Gtkada.Handlers.Return_Callback.Connect
        (Box,
         Gtk.Widget.Signal_Delete_Event,
         Delete_Callback'Access,
         After => False);

      --  Modified file area...
      Gtk_New_Vseparator (Separator);
      Pack_End (Box.Label_Box, Separator, Expand => False, Fill => False);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_None);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Modified_Label);
      Add (Frame, Box.Modified_Label);

      --  Read only file area...
      Gtk_New_Vseparator (Separator);
      Pack_End (Box.Label_Box, Separator, Expand => False, Fill => False);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_None);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Event_Box);
      Add (Frame, Event_Box);
      Gtk_New (Box.Read_Only_Label);
      Add (Event_Box, Box.Read_Only_Label);
      Object_Return_Callback.Object_Connect
        (Event_Box, Signal_Button_Press_Event,
         On_Read_Only_Pressed'Access, Box);

      --  Insert/Overwrite label
      Gtk_New_Vseparator (Separator);
      Pack_End (Box.Label_Box, Separator, Expand => False, Fill => False);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_None);
      Pack_End (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Overwrite_Label, -"Insert");

      --  ??? Using an Event_Box should not be necessary, but it avoids
      --  some overlaps when resizing the editor window

      Gtk_New (Event_Box);
      Add (Frame, Event_Box);
      Add (Event_Box, Box.Overwrite_Label);
      Box_Callback.Connect
        (Box.Source_View,
         Signal_Toggle_Overwrite,
         On_Toggle_Overwrite'Access,
         User_Data => Source_Editor_Box (Box));

      --  Function location area
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_None);
      Pack_Start (Box.Label_Box, Frame, Expand => False, Fill => True);
      Gtk_New (Box.Function_Label);

      --  Using an Event_Box to avoid some overlaps when resizing the editor
      --  window. See also ??? comment above.

      Gtk_New (Event_Box);
      Add (Frame, Event_Box);
      Add (Event_Box, Box.Function_Label);

      --  Connect to source buffer signals

      Box.Cursor_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         Signal_Cursor_Position_Changed,
         Cursor_Position_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box.Status_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         Signal_Status_Changed,
         Status_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box.Status_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         Signal_Filename_Changed,
         Filename_Changed_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box.Buffer_Info_Handler := Box_Callback.Connect
        (Box.Source_Buffer,
         Signal_Buffer_Information_Changed,
         Buffer_Information_Handler'Access,
         User_Data => Source_Editor_Box (Box),
         After     => True);

      Box_Callback.Connect
        (Box.Source_View,
         Signal_Destroy,
         On_Box_Destroy'Access,
         User_Data => Source_Editor_Box (Box));

      Show_Cursor_Position
        (Source_Editor_Box (Box), Line => 1, Column => 1);

      Add_Events (Box.Source_View, Focus_Change_Mask);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, Signal_Focus_In_Event, Focus_In'Access, Box, False);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, Signal_Focus_Out_Event,
         Focus_Out'Access, Box, False);
      Object_Return_Callback.Object_Connect
        (Box.Source_View, Signal_Key_Press_Event, Key_Press'Access, Box);

      --  The Contextual Menu handling
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Box.Source_View,
         Object          => Box,
         ID              => Src_Editor_Module_Id,
         Context_Func    => Get_Contextual_Menu'Access);
   end Initialize;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press (Box : access GObject_Record'Class) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      if B.Timestamp_Mode = Check_At_Modify then
         Check_Timestamp_And_Reload
           (B, Interactive => True, Always_Reload => False);

         B.Timestamp_Mode := Check_At_Focus;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);

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
      Set_Writable
        (Editor, not Get_Writable (Editor.Source_Buffer), Explicit => True);

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);

         return False;
   end On_Read_Only_Pressed;

   --------------------------
   -- Check_Timestamp_Idle --
   --------------------------

   function Check_Timestamp_Idle (Box : GObject) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      --  If we are currently holding down the mouse button, then we do not
      --  want to offer to reload the file, since the pop-up dialog will
      --  not be able to get the mouse input. Therefore we do nothing in
      --  this idle callback, but keep it registered so that the timestamp
      --  is checked after the button release.

      if Pointer_Is_Grabbed then
         --  No need to try again if the file is up-to-date. Otherwise, we'll
         --  need to try again. One issue here is that while displaying a
         --  contextual menu for an editor that didn't have the focus before,
         --  and the file on disk has been modified, the idle callback is
         --  called in a loop, and that takes 100% of CPU (G227-035).
         B.Check_Timestamp_Registered :=
           not Check_Timestamp (B.Source_Buffer, Update => False);
         return B.Check_Timestamp_Registered;
      end if;

      B.Check_Timestamp_Registered := False;
      if B.Timestamp_Mode = Check_At_Focus then
         B.Timestamp_Mode := Checking;

         Check_Timestamp_And_Reload
           (B, Interactive => True, Always_Reload => False);

         Check_Writable (B);

         B.Timestamp_Mode := Check_At_Focus;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);

         return False;
   end Check_Timestamp_Idle;

   --------------
   -- Focus_In --
   --------------

   function Focus_In (Box : access GObject_Record'Class) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      --  We must do the check in an idle callback: otherwise, when the dialog
      --  is popped up on the screen, and since it is modal, the button
      --  release event is never sent to the editor, and there is a drag
      --  selection taking place.

      if not B.Check_Timestamp_Registered then
         B.Check_Timestamp_Registered := True;
         B.Check_Timestamp_Id :=
           Object_Idle.Add (Check_Timestamp_Idle'Access, GObject (Box));
      end if;

      --  Connect the Undo/Redo buttons to the buffer
      if Get_Writable (B.Source_Buffer) then
         Add_Controls (B.Source_Buffer);
      else
         Remove_Controls (B.Source_Buffer);
      end if;

      return False;
   end Focus_In;

   ---------------
   -- Focus_Out --
   ---------------

   function Focus_Out (Box : access GObject_Record'Class) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
   begin
      --  Disconnect the Undo/Redo buttons from the buffer

      Remove_Controls (B.Source_Buffer);
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Focus_Out;

   -------------------------
   -- Get_Contextual_Menu --
   -------------------------

   procedure Get_Contextual_Menu
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget);
   begin
      Get_Contextual_Menu (Context, Kernel, Object, Event, Menu);
   end Get_Contextual_Menu;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access In_Line_Numbers_Area_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Event  : constant Gdk_Event := Get_Current_Event;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Editor : constant Source_Editor_Box  :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));

   begin
      return Event /= null
        and then Editor /= null
        and then Get_Window (Event) =
          Get_Window (Editor.Source_View, Text_Window_Left);
   end Filter_Matches_Primitive;

   -------------------------
   -- Get_Contextual_Menu --
   -------------------------

   procedure Get_Contextual_Menu
     (Context : in out GPS.Kernel.Selection_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object  : access Glib.Object.GObject_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      Editor        : constant Source_Editor_Box := Source_Editor_Box (Object);
      V             : constant Source_View := Editor.Source_View;
      Filename      : constant VFS.Virtual_File :=
                        Get_Filename (Editor.Source_Buffer);
      Line          : Gint := 0;
      Column        : Gint := 0;
      X, Y          : Gint;
      Start_Iter    : Gtk_Text_Iter;
      End_Iter      : Gtk_Text_Iter;
      Entity_Start  : Gtk_Text_Iter;
      Entity_End    : Gtk_Text_Iter;
      Has_Selection : Boolean;
      Out_Of_Bounds : Boolean := False;
      Start_Line, End_Line : Integer;
      Selection_Is_Single_Entity : Boolean;
      Click_In_Selection         : Boolean;

      function In_Selection
        (L, C : Gint; First, Last : Gtk_Text_Iter) return Boolean;
      --  Return True if (L,C) is between (First, Last)

      ------------------
      -- In_Selection --
      ------------------

      function In_Selection
        (L, C : Gint; First, Last : Gtk_Text_Iter) return Boolean
      is
         L_Start : constant Gint := Get_Line (First);
         C_Start : constant Gint := Get_Line_Offset (First);
         L_End   : constant Gint := Get_Line (Last);
         C_End   : constant Gint := Get_Line_Offset (Last);
      begin
         --  ??? We should use Gtk.Text_Iter.In_Range, but that requires an
         --  iter in parameter, which we do not have currently.
         return (L > L_Start
                 or else (L = L_Start and then C >= C_Start))
           and then
             (L < L_End
              or else (L = L_End and then C <= C_End));
      end In_Selection;

   begin
      Stop_Selection_Drag (V);

      --  Click in the line numbers area ?

      if Event /= null
        and then Get_Window (Event) = Get_Window (V, Text_Window_Left)
      then
         Window_To_Buffer_Coords
           (Editor.Source_View, Text_Window_Left,
            Gint (Get_X (Event)), Gint (Get_Y (Event)), X, Y);
         Get_Iter_At_Location (Editor.Source_View, Start_Iter, X, Y);
         Line := Get_Line (Start_Iter);
         Place_Cursor (Editor.Source_Buffer, Start_Iter);

      --  Else click in the text area

      else
         if Event = null
           or else Get_Event_Type (Event) not in Button_Press .. Button_Release
         then
            Get_Iter_At_Mark
              (Editor.Source_Buffer, Start_Iter,
               Get_Insert (Editor.Source_Buffer));
            Line   := Get_Line (Start_Iter);
            Column := Get_Line_Offset (Start_Iter);

         else
            Event_To_Buffer_Coords
              (Editor.Source_View, Event, Line, Column, Out_Of_Bounds);
         end if;

         Get_Selection_Bounds
           (Editor.Source_Buffer, Start_Iter, End_Iter, Has_Selection);

         if Out_Of_Bounds and then not Has_Selection then
            --  Invalid position: the cursor is outside the text
            Get_Iter_At_Line_Offset
              (Editor.Source_Buffer, Start_Iter, Line, Column);

            Acquire_Focus (Editor.Source_View);
            Place_Cursor (Editor.Source_Buffer, Start_Iter);

         else
            Get_Iter_At_Line_Offset
              (Editor.Source_Buffer, Entity_Start, Line, Column);

            Click_In_Selection :=
              Has_Selection
              and then In_Range (Entity_Start, Start_Iter, End_Iter);

            --  Get the current entity bounds

            Search_Entity_Bounds (Entity_Start, Entity_End);
            Selection_Is_Single_Entity :=
              Has_Selection
              and then Equal (Entity_Start, Start_Iter)
              and then Equal (Entity_End, End_Iter);

            --  If we click in the current selection, use this as the context.
            --  However, if the selection is a single entity, we should create
            --  a context such that cross-references menus also appear

            if Has_Selection
              and then not Selection_Is_Single_Entity
              and then In_Selection (Line, Column, Start_Iter, End_Iter)
            then
               Start_Line := To_Box_Line
                 (Editor.Source_Buffer, Get_Line (Start_Iter));
               End_Line   := To_Box_Line
                 (Editor.Source_Buffer, Get_Line (End_Iter));

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

               --  Set the column to the start of the selection

               Column := Get_Line_Offset (Start_Iter);

               Set_Area_Information
                 (Context,
                  Get_Text (Start_Iter, End_Iter),
                  Start_Line, End_Line);

            else
               --  Expand the tabs

               declare
                  The_Line   : Editable_Line_Type;
                  The_Column : Character_Offset_Type;
               begin
                  Get_Iter_Position
                    (Editor.Source_Buffer, Entity_Start,
                     The_Line, The_Column);

                  Set_Entity_Information
                    (Context,
                     Get_Text (Entity_Start, Entity_End),
                     Expand_Tabs
                       (Editor.Source_Buffer, The_Line, The_Column));
               end;

               if Menu /= null
                 and then not Click_In_Selection
               then
                  --  Move the cursor at the correct location. The cursor is
                  --  grabbed automatically by the kernel when displaying the
                  --  menu, and this would result in unwanted scrolling
                  --  otherwise..
                  --  Do not move the cursor if we have clicked in the
                  --  selection, since otherwise that cancels the selection
                  --
                  --  Force the focus on the MDI window right away, instead of
                  --  waiting for the editor to gain the focus later on.
                  --  Otherwise, if the editor doesn't have the focus at this
                  --  point, it will move back to its Saved_Cursor_Mark when
                  --  it does, instead of where we have used Place_Cursor. Note
                  --  that explicitly using Save_Cursor_Position doesn't work
                  --  either, since it needs to be called after Place_Cursor,
                  --  which does the scrolling to Saved_Cursor_Mark.

                  Acquire_Focus (Editor.Source_View);
                  Place_Cursor (Editor.Source_Buffer, Entity_Start);
               end if;
            end if;
         end if;

         Set_File_Information
           (Context,
            File   => Filename,
            Line   => Integer (To_Box_Line (Editor.Source_Buffer, Line)),
            Column => Expand_Tabs
              (Get_Buffer (Editor),
               Editable_Line_Type (To_Box_Line (Editor.Source_Buffer, Line)),
               To_Box_Column (Column)));
         Set_Context_Information
           (Context => Context,
            Kernel  => Kernel,
            Creator => Abstract_Module_ID (Src_Editor_Module_Id));
      end if;
   end Get_Contextual_Menu;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Creator : access Goto_Body_Menu_Label;
      Context : Selection_Context) return String
   is
      pragma Unreferenced (Creator);
      Entity : Entity_Information;
   begin
      Entity := Get_Entity (Context);
      if Entity /= null then
         if Is_Container (Get_Kind (Entity).Kind) then
            return -"Goto body of "
              & Emphasize (Entity_Name_Information (Context));
         else
            return -"Goto full declaration of "
              & Emphasize (Entity_Name_Information (Context));
         end if;
      end if;

      return "";
   end Get_Label;

   ----------------------------
   -- On_Goto_Declaration_Of --
   ----------------------------

   procedure On_Goto_Declaration_Of
     (Kernel : access GObject_Record'Class;
      Entity : Entity_Information)
   is
      Location : constant File_Location := Get_Declaration_Of (Entity);
   begin
      Go_To_Closest_Match
        (Kernel_Handle (Kernel),
         Filename => Get_Filename (Location.File),
         Line     => Convert (Location.Line),
         Column   => Location.Column,
         Entity   => Entity);
   end On_Goto_Declaration_Of;

   ---------------------
   -- On_Goto_Body_Of --
   ---------------------

   procedure On_Goto_Body_Of
     (Kernel : access GObject_Record'Class;
      Entity : Entity_Information)
   is
      Location : File_Location;
   begin
      Find_Next_Body
        (Entity               => Entity,
         Location             => Location);
      Go_To_Closest_Match
        (Kernel_Handle (Kernel),
         Filename => Get_Filename (Location.File),
         Line     => Convert (Location.Line),
         Column   => Location.Column,
         Entity   => Entity);
   end On_Goto_Body_Of;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Declaration_Submenu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
      Iter : Entity_Reference_Iterator;
      Item : Gtk_Menu_Item;
      E, E2 : Entity_Information;
      Label : Gtk_Label;
      Pref : constant Dispatching_Menu_Policy := Dispatching_Menu_Policy'Val
        (Get_Pref (Submenu_For_Dispatching_Calls));

   begin
      Trace (Me, "Computing Dispatch_Declaration_Submenu");
      Push_State (Get_Kernel (Context), Busy);

      if Pref = From_Memory then
         Freeze (Get_Database (Get_Kernel (Context)));
         Gtk_New (Label, -"<i>Partial information only</i>");
         Set_Use_Markup (Label, True);
         Set_Alignment (Label, 0.0, 0.5);
         Gtk_New (Item);
         Add (Item, Label);
         Set_Sensitive (Item, False);
         Add (Menu, Item);
      end if;

      Find_All_References
        (Iter                  => Iter,
         Entity                => Get_Entity (Context),
         File_Has_No_LI_Report => null,
         In_File               => null,
         Filter                => (Declaration => True, others => False),
         Include_Overriding    => True,
         Include_Overridden    => False);
      while not At_End (Iter) loop
         E := Get_Entity (Iter);
         if E /= null then
            E2 := Is_Primitive_Operation_Of (E);
            if E2 /= null then
               Gtk_New
                 (Label, "Primitive of: " & Emphasize (Get_Name (E2).all));
               Set_Use_Markup (Label, True);
               Set_Alignment (Label, 0.0, 0.5);
               Gtk_New (Item);
               Add (Item, Label);

               GPS.Kernel.Entity_Callback.Object_Connect
                 (Item, Gtk.Menu_Item.Signal_Activate,
                  On_Goto_Declaration_Of'Access, Get_Kernel (Context), E);
               Add (Menu, Item);
            end if;
         end if;

         Next (Iter);
      end loop;
      Destroy (Iter);

      if Pref = From_Memory then
         Thaw (Get_Database (Get_Kernel (Context)));
      end if;

      Pop_State (Get_Kernel (Context));
      Trace (Me, "Done computing Dispatch_Declaration_Submenu");

   exception
      when E : others =>
         Trace (Me, E);
         Thaw (Get_Database (Get_Kernel (Context)));
         Pop_State (Get_Kernel (Context));
   end Append_To_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Body_Submenu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
      Iter  : Entity_Reference_Iterator;
      Item  : Gtk_Menu_Item;
      E, E2 : Entity_Information;
      Label : Gtk_Label;
      Pref : constant Dispatching_Menu_Policy := Dispatching_Menu_Policy'Val
        (Get_Pref (Submenu_For_Dispatching_Calls));
   begin
      --  The declaration_dispatch menu already made sure we have
      --  correctly loaded all relevant .ALI files. So in the loop below we
      --  freeze the xref database to make sure we do not waste time in useless
      --  system calls

      if Pref = From_Memory then
         Gtk_New (Label, -"<i>Partial information only</i>");
         Set_Use_Markup (Label, True);
         Set_Alignment (Label, 0.0, 0.5);
         Gtk_New (Item);
         Add (Item, Label);
         Set_Sensitive (Item, False);
         Add (Menu, Item);
      end if;

      Freeze (Get_Database (Get_Kernel (Context)));
      Push_State (Get_Kernel (Context), Busy);
      Trace (Me, "Computing Dispatch_Body_Submenu");

      Find_All_References
        (Iter                  => Iter,
         Entity                => Get_Entity (Context),
         File_Has_No_LI_Report => null,
         In_File               => null,
         Filter                => (Body_Entity => True, others => False),
         Include_Overriding    => True,
         Include_Overridden    => False);
      while not At_End (Iter) loop
         E := Get_Entity (Iter);
         if E /= null then
            E2 := Is_Primitive_Operation_Of (E);
            if E2 /= null then
               Gtk_New
                 (Label, "Primitive of: " & Emphasize (Get_Name (E2).all));
               Set_Use_Markup (Label, True);
               Set_Alignment (Label, 0.0, 0.5);
               Gtk_New (Item);
               Add (Item, Label);

               GPS.Kernel.Entity_Callback.Object_Connect
                 (Item, Gtk.Menu_Item.Signal_Activate,
                  On_Goto_Body_Of'Access, Get_Kernel (Context), E);
               Add (Menu, Item);
            end if;
         end if;

         Next (Iter);
      end loop;
      Destroy (Iter);

      Thaw (Get_Database (Get_Kernel (Context)));
      Pop_State (Get_Kernel (Context));
      Trace (Me, "Done computing Dispatch_Body_Submenu");

   exception
      when E : others =>
         Trace (Me, E);
         Thaw (Get_Database (Get_Kernel (Context)));
         Pop_State (Get_Kernel (Context));
   end Append_To_Menu;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Has_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      Entity := Get_Entity (Context);
      return Entity /= null and then Get_Type_Of (Entity) /= null;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_File_Information (Context) then
         declare
            File    : constant VFS.Virtual_File := File_Information (Context);
            Project    : constant Project_Type := Get_Project_From_File
              (Get_Registry (Kernel).all, File);
            Other_File : constant String := Other_File_Base_Name
              (Project, File);
         begin
            if Other_File /= "" and then Other_File /= Base_Name (File) then
               return True;
            end if;
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Has_Body_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity           : Entity_Information;
      Location         : Entities.File_Location;
      Current_Location : Entities.File_Location;
   begin
      Entity := Get_Entity (Context);

      if Entity /= null then
         Current_Location :=
           (File   => Get_Or_Create
              (Db   => Get_Database (Get_Kernel (Context)),
               File => File_Information (Context)),
            Line   => Contexts.Line_Information (Context),
            Column => Entity_Column_Information (Context));

         Find_Next_Body
           (Entity   => Entity,
            Current_Location => Current_Location,
            Location => Location);

         return Location /= Entities.No_File_Location
           and then Location /= Current_Location;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Is_Dispatching_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Ref : constant Entity_Reference := Get_Closest_Ref (Context);
      Pref : constant Dispatching_Menu_Policy := Dispatching_Menu_Policy'Val
        (Get_Pref (Submenu_For_Dispatching_Calls));
   begin
      if Pref = Never then
         return False;
      else
         return Get_Kind (Ref) = Dispatching_Call;
      end if;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Goto_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context.Context);
      File  : constant VFS.Virtual_File := File_Information (Context.Context);
      Project    : constant Project_Type := Get_Project_From_File
        (Get_Registry (Kernel).all, File);
      Other_File : constant Virtual_File := Create
        (Other_File_Base_Name (Project, File), Project);
   begin
      Trace (Me, "Goto_Other_File_Command File="
             & Full_Name (File).all
             & " Project=" & Project_Name (Project)
             & " Other_File=" & Full_Name (Other_File).all);
      if Other_File /= VFS.No_File then
         Open_File_Editor (Kernel, Other_File, Line => 0);
         return Commands.Success;
      else
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Goto_Line_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Box    : constant Source_Editor_Box :=
        Get_Source_Box_From_MDI (Find_Current_Editor (Command.Kernel));
   begin
      On_Goto_Line (Box, Command.Kernel);
      return Commands.Success;
   end Execute;

   ------------------
   -- On_Goto_Line --
   ------------------

   procedure On_Goto_Line
     (Widget : access GObject_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Box : constant Source_Editor_Box := Source_Editor_Box (Widget);
   begin
      declare
         Str : constant String := Simple_Entry_Dialog
           (Get_Current_Window (Kernel),
            -"Goto Line...", -"Enter line number:",
            Win_Pos_Mouse, Get_History (Kernel), "Goto_Line");

      begin
         if Str = "" or else Str (Str'First) = ASCII.NUL then
            return;
         end if;

         Push_Current_Editor_Location_In_History (Kernel);
         Set_Cursor_Location (Box, Editable_Line_Type'Value (Str), 1);
         Add_Navigation_Location (Box);

      exception
         when Constraint_Error =>
            Console.Insert
              (Kernel, -"Invalid line number: " & Str, Mode => Error);
      end;
   end On_Goto_Line;

   -----------------------
   -- On_Goto_Line_Func --
   -----------------------

   function On_Goto_Line_Func
     (Editor : access GObject_Record'Class) return Boolean is
   begin
      --  ??? Not nice to get a context here
      On_Goto_Line (Source_Editor_Box (Editor),
                    Source_Editor_Box (Editor).Kernel);
      return True;
   end On_Goto_Line_Func;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Goto_Declaration_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Box    : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => False,
         Editor  => Box,
         Context => Context.Context);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Goto_Next_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Box    : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
   begin
      Goto_Declaration_Or_Body
        (Kernel,
         To_Body => True,
         Editor  => Box,
         Context => Context.Context);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Goto_Type_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel      : constant Kernel_Handle := Get_Kernel (Context.Context);
      Editor      : constant Source_Editor_Box :=
                      Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
      Entity      : constant Entity_Information :=
                      Get_Entity (Context.Context, Ask_If_Overloaded => True);
      Entity_Type : Entity_Information;
   begin
      if Entity = null then
         --  Probably means that we either could not locate the ALI file,
         --  or it could also be that we failed to parse it. Either way,
         --  a message should have already been printed. So, just abort.

         Console.Insert
           (Kernel,
            -"No cross-reference information found for "
            & Full_Name (Get_Filename (Editor)).all & ASCII.LF
            & (-("Recompile your file or select Build->Recompute Xref"
                 & " Information, depending on the language")),
            Mode           => Error);
         return Commands.Failure;

      else
         Entity_Type := Get_Type_Of (Entity);

         if Is_Predefined_Entity (Entity_Type) then
            Console.Insert
              (Kernel,
               -Get_Name (Entity).all & " is of predefined type "
               & Get_Name (Entity_Type).all);
            return Commands.Failure;

         else
            Go_To_Closest_Match
              (Kernel,
               Filename => Get_Filename
                 (Get_File (Get_Declaration_Of (Entity_Type))),
               Line     => Convert
                 (Get_Line (Get_Declaration_Of (Entity_Type))),
               Column   => Get_Column (Get_Declaration_Of (Entity_Type)),
               Entity   => Entity_Type);
            return Commands.Success;
         end if;
      end if;
   end Execute;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);
      Kernel : constant Kernel_Handle :=
                 Get_Kernel (Source_Editor_Box (Widget));
   begin
      --  We cannot delete the last remaining view if the buffer has to be
      --  saved and the user cancelled the action.
      --  The call to Needs_To_Be_Saved will return False if the box is not the
      --  last view, so that we always authorize closing the other views

      return Needs_To_Be_Saved (Get_Buffer (Source_Editor_Box (Widget)))
        and then not Save_MDI_Children
          (Kernel,
           Children => (1 => Find_MDI_Child (Get_MDI (Kernel), Widget)),
           Force => False);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Delete_Callback;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box    : out Source_Editor_Box;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize (Box, Kernel, null, Lang);
   end Gtk_New;

   ---------------------
   -- Create_New_View --
   ---------------------

   procedure Create_New_View
     (Box    : out Source_Editor_Box;
      Kernel : access Kernel_Handle_Record'Class;
      Source : access Source_Editor_Box_Record) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize
        (Box, Kernel_Handle (Kernel), Source.Source_Buffer,
         Get_Language (Source.Source_Buffer));
      Set_Text (Box.Modified_Label, Get_Text (Source.Modified_Label));

      if Get_Writable (Box.Source_Buffer) then
         Set_Text (Box.Read_Only_Label, -"Writable");
      else
         Set_Text (Box.Read_Only_Label, -"Read Only");
         Set_Editable (Box.Source_View, False);
      end if;
   end Create_New_View;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Box : access Source_Editor_Box_Record)
     return GPS.Kernel.Kernel_Handle is
   begin
      return Box.Kernel;
   end Get_Kernel;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Editor : access Source_Editor_Box_Record) return Virtual_File is
   begin
      return Get_Filename (Editor.Source_Buffer);
   end Get_Filename;

   --------------------
   -- Check_Writable --
   --------------------

   procedure Check_Writable (Editor : access Source_Editor_Box_Record) is
      Views    : constant Views_Array := Get_Views (Editor.Source_Buffer);
      Writable : Boolean;
   begin
      if Get_Explicit_Writable_Set (Editor.Source_Buffer) then
         return;
      end if;

      if Read_Only_Set then
         Writable := False;
      else
         Writable := Get_Filename (Editor.Source_Buffer) = VFS.No_File
           or else VFS.Is_Writable (Get_Filename (Editor.Source_Buffer))
         or else (not Is_Regular_File (Get_Filename (Editor.Source_Buffer))
                    and then Get_Writable (Editor.Source_Buffer));
      end if;

      Set_Writable (Editor.Source_Buffer, Writable, Explicit => False);
      Set_Writable (Views, Get_Writable (Editor.Source_Buffer));
   end Check_Writable;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (Views : Views_Array; Writable : Boolean) is
   begin
      for V in Views'Range loop
         Set_Editable (Views (V).Source_View, Writable);

         if Writable then
            Set_Text (Views (V).Read_Only_Label, -"Writable");
         else
            Set_Text (Views (V).Read_Only_Label, -"Read Only");
         end if;
      end loop;
   end Set_Writable;

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

      if Success then
         Set_Cursor_Location (Editor, 1, 1, Force_Focus);
         Set_Filename (Editor.Source_Buffer, Filename);
         Editor.Source_Buffer.Status_Changed;
         Check_Writable (Editor);
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
      Set_Charset (Editor.Source_Buffer, Get_File_Charset (Filename));
      Set_Text (Editor.Modified_Label, -"Unmodified");
      Set_Writable (Editor.Source_Buffer, Writable => True, Explicit => False);
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
      File          : constant VFS.Virtual_File :=
                        Get_Filename (Editor.Source_Buffer);
      Constructs    : Construct_List;
      Info          : Construct_Access;
      New_Base_Name : GNAT.Strings.String_Access;
      Part          : Projects.Unit_Part;

      Buffer        : GNAT.Strings.String_Access;

   begin
      --  Do not authorize saving a read-only file, unless we save it to
      --  another disk file.

      if not Get_Writable (Editor.Source_Buffer)
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

            Buffer := Get_String (Editor.Source_Buffer);
            Parse_Constructs (Ada_Lang, Buffer.all, Constructs);
            Free (Buffer);

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
                  Part := Projects.Unit_Spec;
               else
                  Part := Projects.Unit_Body;
               end if;

               New_Base_Name := new String'
                 (Projects.Get_Filename_From_Unit
                    (Project         => Get_Project (Editor.Kernel),
                     Unit_Name       => To_Lower (Info.Name.all),
                     Part            => Part,
                     File_Must_Exist => False,
                     Language        => Ada_String));
            end if;

            Free (Constructs);

            declare
               Name : constant Virtual_File :=
                 Select_File
                   (Title             => -"Save File As",
                    Parent            => Get_Current_Window (Editor.Kernel),
                    Default_Name      => New_Base_Name.all,
                    Use_Native_Dialog =>
                      Get_Pref (Use_Native_Dialogs),
                    Kind              => Save_File,
                    File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                    Pattern_Name      => -"All files;Ada files;C/C++ files",
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
                     Parent => Get_Current_Window (Editor.Kernel)) /= Button_OK
                  then
                     Success := False;
                  end if;
               end if;

               if Success then
                  Save_To_File (Editor.Source_Buffer, Name, Success);
               end if;
            end;
         else
            if not Check_Timestamp (Editor.Source_Buffer)
              and then Message_Dialog
                (Msg => Base_Name (File)
                        & (-" changed on disk. Do you want to overwrite ?"),
                 Dialog_Type => Confirmation,
                 Buttons => Button_OK or Button_Cancel,
                 Title => -"File changed on disk",
                 Parent => Get_Current_Window (Editor.Kernel)) /= Button_OK
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
               Parent => Get_Current_Window (Editor.Kernel)) /= Button_OK
            then
               Success := False;
            end if;
         end if;

         if Success then
            Save_To_File (Editor.Source_Buffer, Filename, Success);
         end if;
      end if;
   end Save_To_File;

   --------------------------------
   -- Check_Timestamp_And_Reload --
   --------------------------------

   procedure Check_Timestamp_And_Reload
     (Editor        : access Source_Editor_Box_Record;
      Interactive   : Boolean;
      Always_Reload : Boolean)
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);

      Response : Gtk_Response_Type;
      Success  : Boolean;
      Line     : Editable_Line_Type;
      Column   : Character_Offset_Type;
      Data     : aliased File_Hooks_Args;
   begin
      if Get_Filename (Editor.Source_Buffer) /= VFS.No_File then
         if Always_Reload
           or else not Check_Timestamp (Editor.Source_Buffer, Update => True)
         then
            if Always_Reload or else not Interactive then
               Response := Gtk_Response_No;
            else
               Data.File := Get_Filename (Editor.Source_Buffer);

               if not Run_Hook_Until_Success
                 (Editor.Kernel, File_Changed_Detected_Hook,
                  Data'Unchecked_Access)
               then

                  Dialog := Create_Gtk_Dialog
                    (Base_Name (Get_Filename (Editor.Source_Buffer))
                     & (-" changed on disk.")
                     & ASCII.LF & ASCII.LF
                     & (-"Click on Ignore to keep this editing session.")
                     & ASCII.LF
                     & (-"Click on Reload to reload the file from disk")
                     & ASCII.LF
                     & (-"and discard your current changes."),
                     Dialog_Type   => Confirmation,
                     Title         => -"File changed on disk",
                     Justification => Justify_Left,
                     Parent        => Get_Current_Window (Editor.Kernel));

                  Button := Add_Button (Dialog, -"Ignore", Gtk_Response_Yes);
                  Button := Add_Button (Dialog, -"Reload", Gtk_Response_No);

                  --  Ungrab the pointer if needed, to avoid freezing the
                  --  interface if the user is e.g. moving the GPS window

                  Pointer_Ungrab;
                  Show_All (Dialog);
                  Response := Run (Dialog);
                  Destroy (Dialog);
               end if;
            end if;

            case Response is
               when Gtk_Response_Yes =>
                  null;

               when Gtk_Response_No =>
                  Get_Cursor_Position (Get_Buffer (Editor), Line, Column);
                  Load_File
                    (Editor.Source_Buffer,
                     Filename        => Get_Filename (Editor.Source_Buffer),
                     Lang_Autodetect => True,
                     Success         => Success);
                  Set_Cursor_Location (Editor, Line, Column, False);

               when others =>
                  null;
            end case;
         end if;
      end if;
   end Check_Timestamp_And_Reload;

   -------------------------
   -- Set_Cursor_Location --
   -------------------------

   procedure Set_Cursor_Location
     (Editor      : access Source_Editor_Box_Record;
      Line        : Editable_Line_Type;
      Column      : Character_Offset_Type := 1;
      Force_Focus : Boolean := True;
      Centering   : Centering_Type := Minimal)
   is
      Editable_Line : Editable_Line_Type renames Line;

   begin
      if Is_Valid_Position (Editor.Source_Buffer, Editable_Line, Column) then
         if Force_Focus then
            Grab_Focus (Editor.Source_View);
         end if;

         Set_Cursor_Position
           (Editor.Source_Buffer, Editable_Line, Column,
            Centering => Centering,
            Internal  => False);
         Save_Cursor_Position (Editor.Source_View);
         Scroll_To_Cursor_Location (Editor.Source_View, Centering);

      elsif Is_Valid_Position (Editor.Source_Buffer, Editable_Line) then
         --  We used to generate an error message (Invalid column number),
         --  but this was too intrusive: in the case of e.g. loading the
         --  desktop, if it often the case that the files have been modified
         --  and the column is no longer valid, so we silently ignore this.
         --  ??? Consider going to the last column instead of the first

         if Force_Focus then
            Grab_Focus (Editor.Source_View);
         end if;

         Set_Cursor_Position
           (Editor.Source_Buffer, Editable_Line, 1, Centering, False);
         Save_Cursor_Position (Editor.Source_View);
         Scroll_To_Cursor_Location (Editor.Source_View, Centering);

      else
         if Column = 1 then
            Console.Insert
              (Editor.Kernel,
               -"Invalid line number: " & Image (Integer (Line)),
               Mode => Error);
         else
            Console.Insert
              (Editor.Kernel,
               -"Invalid source location: " &
               Image (Integer (Line)) &
               ':' & Image (Natural (Column)), Mode => Error);
         end if;
      end if;
   end Set_Cursor_Location;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Sliced
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
         Character_Offset_Type (Start_Column),
         Editable_Line_Type (End_Line),
         Character_Offset_Type (End_Column),
         Text);

      External_End_Action (Editor.Source_Buffer);
      Enqueue (Editor.Source_Buffer, Command_Access (C));
   end Replace_Sliced;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Info       : Standard_Hooks.Line_Information_Data) is
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
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Every_Line : Boolean) is
   begin
      Create_Line_Information_Column
        (Editor.Source_Buffer, Identifier, Every_Line);
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String) is
   begin
      Remove_Line_Information_Column (Editor.Source_Buffer, Identifier);
   end Remove_Line_Information_Column;

   --------------------
   -- Scroll_To_Mark --
   --------------------

   procedure Scroll_To_Mark
     (Editor : access Source_Editor_Box_Record;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Length : Natural := 0)
   is
      Iter      : Gtk_Text_Iter;
      Mark_Iter : Gtk_Text_Iter;
      Success   : Boolean;

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
               Get_Line_Offset (Iter));
         end if;
      end if;
   end Scroll_To_Mark;

   -------------------
   -- Get_Last_Line --
   -------------------

   function Get_Last_Line
     (Editor : access Source_Editor_Box_Record) return Positive
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_End_Iter (Editor.Source_Buffer, Iter);
      return To_Box_Line (Editor.Source_Buffer, Get_Line (Iter));
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
      Block  : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);
   begin
      return Natural (Block.Last_Line);
   end Get_Block_End;

   --------------------
   -- Get_Block_Name --
   --------------------

   function Get_Block_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String
   is
      B_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Editor.Source_Buffer, Line);
      Block  : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);

   begin
      if Block.Name = null then
         return "";
      else
         return Block.Name.all;
      end if;
   end Get_Block_Name;

   --------------------
   -- Get_Block_Type --
   --------------------

   function Get_Block_Type
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String
   is
      B_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Editor.Source_Buffer, Line);
      Block  : constant Block_Record :=
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
      Block  : constant Block_Record :=
        Get_Block (Editor.Source_Buffer, B_Line);
   begin
      return Natural (Block.Indentation_Level);
   end Get_Block_Level;

   --------------------
   -- Get_Subprogram --
   --------------------

   function Get_Subprogram
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type :=
        Src_Editor_Buffer.Editable_Line_Type'Last) return Entity_Information
   is
      Block           : Block_Record;
      Entity          : Entity_Information;
      Status          : Find_Decl_Or_Body_Query_Status;
      L               : Buffer_Line_Type;
      Normalized_Line : Editable_Line_Type := Line;
      Ref             : Entity_Reference;
   begin
      if Normalized_Line = Editable_Line_Type'Last then
         Normalized_Line := Editor.Current_Line;
      end if;

      Block := Get_Subprogram_Block (Editor.Source_Buffer, Normalized_Line);

      L := Get_Buffer_Line (Editor.Source_Buffer, Normalized_Line);

      if Block.Name /= null then
         Find_Declaration_Or_Overloaded
           (Kernel      => Editor.Kernel,
            File        => Get_Or_Create
              (Db   => Get_Database (Editor.Kernel),
               File => Get_Filename (Editor.Source_Buffer)),
            Entity_Name => Block.Name.all,
            Line        => Integer
              (Get_Editable_Line (Editor.Source_Buffer, L) - 1),
            Ask_If_Overloaded => False,
            Column      => 1,
            Entity      => Entity,
            Closest_Ref => Ref,
            Status      => Status);
         return Entity;
      else
         return null;
      end if;
   end Get_Subprogram;

   -------------------------
   -- Get_Subprogram_Name --
   -------------------------

   function Get_Subprogram_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type :=
        Src_Editor_Buffer.Editable_Line_Type'Last) return String
   is
      Normalized_Line : Editable_Line_Type := Line;
      Block           : Block_Record;
   begin
      if Normalized_Line = Editable_Line_Type'Last then
         Normalized_Line := Editor.Current_Line;
      end if;

      Block := Get_Subprogram_Block (Editor.Source_Buffer, Normalized_Line);

      if Block.Name /= null then
         return Block.Name.all;
      else
         return "";
      end if;
   end Get_Subprogram_Name;

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
      if Get_Writable (Editor.Source_Buffer) then
         Undo (Editor.Source_Buffer);
      end if;
   end Undo;

   ----------
   -- Redo --
   ----------

   procedure Redo (Editor : access Source_Editor_Box_Record) is
   begin
      if Get_Writable (Editor.Source_Buffer) then
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
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Editor   : access Source_Editor_Box_Record;
      Writable : Boolean;
      Explicit : Boolean := False)
   is
      Views       : constant Views_Array := Get_Views (Editor.Source_Buffer);
      Is_Writable : Boolean;
   begin
      Set_Writable (Editor.Source_Buffer, Writable, Explicit);

      Is_Writable := Get_Writable (Editor.Source_Buffer);

      if Is_Writable then
         Add_Controls (Editor.Source_Buffer);
      else
         Remove_Controls (Editor.Source_Buffer);
      end if;

      Set_Writable (Views, Is_Writable);
   end Set_Writable;

   ---------------
   -- Get_Views --
   ---------------

   function Get_Views (Buffer : Source_Buffer) return Views_Array is
      Iter  : Child_Iterator := First_Child (Get_MDI (Get_Kernel (Buffer)));
      Box   : Source_Editor_Box;
      Count : Natural := 0;
   begin
      while Get (Iter) /= null loop
         Count := Count + 1;
         Next (Iter);
      end loop;

      declare
         Result : Views_Array (1 .. Count);
      begin
         Count := 1;
         Iter  := First_Child (Get_MDI (Get_Kernel (Buffer)));
         while Get (Iter) /= null loop
            if Get_Widget (Get (Iter)).all in
              Source_Editor_Box_Record'Class
            then
               Box := Source_Editor_Box (Get_Widget (Get (Iter)));
               if Get_Filename (Box) = Get_Filename (Buffer) then
                  Result (Count) := Box;
                  Count := Count + 1;
               end if;
            end if;
            Next (Iter);
         end loop;
         return Result (Result'First .. Count - 1);
      end;
   end Get_Views;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   function Needs_To_Be_Saved
     (Box    : not null access Source_Editor_Box_Record;
      Single : Boolean) return Boolean is
   begin
      if not Needs_To_Be_Saved (Box.Source_Buffer) then
         return False;
      else
         declare
            Views : constant Views_Array := Get_Views (Box.Source_Buffer);
         begin
            return Views'Length = 1
              or else (not Single
                       and then Box = Views (Views'First));
         end;
      end if;
   end  Needs_To_Be_Saved;

end Src_Editor_Box;
