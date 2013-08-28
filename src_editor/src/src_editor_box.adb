------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets; use Ada.Containers;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Symbols;           use GNATCOLL.Symbols;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Xref;

with Gdk;                        use Gdk;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Main;                   use Gdk.Main;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Window;                 use Gdk.Window;
with Gdk.Display;                use Gdk.Display;
with Gdk.Screen;                 use Gdk.Screen;

with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;
with Glib;                       use Glib;

with Gtk;                        use Gtk;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Drawing_Area;           use Gtk.Drawing_Area;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Text_Mark;              use Gtk.Text_Mark;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.File_Selector;       use Gtkada.File_Selector;
with Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;

with Find_Utils;                 use Find_Utils;
with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Charsets;        use GPS.Kernel.Charsets;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Xref;            use GPS.Kernel.Xref;

with GUI_Utils;                  use GUI_Utils;
with Language;                   use Language;
with Language.Ada;               use Language.Ada;
with Language_Handlers;          use Language_Handlers;
with Src_Editor_Box.Scrolled_Window; use Src_Editor_Box.Scrolled_Window;
with Src_Editor_Box.Tooltips;    use Src_Editor_Box.Tooltips;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;
with Src_Editor_Module;          use Src_Editor_Module;
with Src_Editor_View;            use Src_Editor_View;
with String_Utils;               use String_Utils;
with Tooltips;                   use Tooltips;
with Traces;

with Xref; use Xref;

package body Src_Editor_Box is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Trace_Handle := Create ("Source_Editor");

   procedure Setup (Data : Source_Editor_Box; Id : Handler_Id);
   package Box_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Glib.Object.GObject_Record,
      User_Type   => Source_Editor_Box,
      Setup       => Setup);

   type Entity_And_Kernel is record
      Kernel : Kernel_Handle;
      Entity : General_Entity;
   end record;

   package Entity_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Entity_And_Kernel);

   --------------------------
   -- Forward declarations --
   --------------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" signal

   procedure Get_Contextual_Menu
     (Context      : in out Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Same as the public Get_Contextual_Menu, Event_Widget is ignored

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
     (Object : access GObject_Record'Class;
      Data   : Entity_And_Kernel);
   --  Jump to the declaration of the given entity

   procedure On_Goto_Body_Of
     (Object : access GObject_Record'Class;
      Data   : Entity_And_Kernel);
   --  Jump to the body of the given entity

   function Focus_In (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_in event. This checks whether the physical file
   --  on the disk is more recent than the one that was read for the editor.

   function Focus_Out (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_out event

   function Key_Press (Box : access GObject_Record'Class) return Boolean;
   --  Check whether the file has been modified on disk

   function Check_Timestamp_Idle (Box : GObject) return Boolean;
   --  Idle callback to check that the timestamp of a file hasn't changed

   procedure Set_Writable (Views : Views_Array; Writable : Boolean);
   --  Make the views editable and update their read-only label according to
   --  Writable.
   --  The purpose of this procedure is to share some code between the public
   --  subprograms Set_Writable and Check_Writable.

   procedure Append_To_Dispatching_Menu
     (Menu          : access Gtk.Menu.Gtk_Menu_Record'Class;
      Context       : GPS.Kernel.Selection_Context;
      Default_Title : String;
      Filter        : Reference_Kind_Filter;
      Show_Default  : Boolean;
      Callback      : Entity_Callback.Simple_Handler);
   --  Create the submenus for dispatching calls

   ----------------------------------
   -- The contextual menu handling --
   ----------------------------------

   -----------------------------
   -- Add_Navigation_Location --
   -----------------------------

   procedure Add_Navigation_Location
     (Source : access Source_Editor_Box_Record'Class)
   is
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      File   : Virtual_File := Get_Filename (Source.Source_Buffer);
   begin
      if File = GNATCOLL.VFS.No_File then
         File := Get_File_Identifier (Source.Source_Buffer);
      end if;

      Get_Cursor_Position (Source.Source_Buffer, Line, Column);
      Push_Marker_In_History
        (Kernel => Source.Kernel,
         Marker => Create_File_Marker
           (Kernel => Source.Kernel,
            File   => File,
            Line   => Line,
            Column => Column));
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
      Entity   : General_Entity;
      Location : General_Location;
      Current  : General_Location;

   begin
      if Get_Filename (Editor) = GNATCOLL.VFS.No_File then
         Kernel.Insert
           (-"Cross-references not possible on unamed files",
            Mode => Error);
         return;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      --  Before getting its entity we check if the LI handler has unresolved
      --  imported references to force updating its references

      Ensure_Context_Up_To_Date (Context);

      --  Query the entity

      Entity := Get_Entity (Context);

      if Entity = No_General_Entity then
         --  Probably means that we either could not locate the ALI file,
         --  or it could also be that we failed to parse it. Either way,
         --  a message should have already been printed. So, just abort.

         Kernel.Insert
           (-"No cross-reference information found for "
            & Entity_Name_Information (Context) & ASCII.LF,
            Mode => Error);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      Ref (Entity);

      --  Get the declaration/body

      if To_Body then
         Current := (File => File_Information (Context),
                     Line => GPS.Kernel.Contexts.Line_Information (Context),
                     Column => Entity_Column_Information (Context));
         Location := Get_Body (Kernel.Databases, Entity, After => Current);
      else
         Location := Kernel.Databases.Get_Declaration (Entity).Loc;
      end if;

      --  Open a file editor at the location found

      if Location /= No_Location then
         Go_To_Closest_Match
           (Kernel      => Kernel,
            Filename    => Location.File,
            Line        => Convert (Location.Line),
            Column      => Location.Column,
            Entity_Name => Kernel.Databases.Get_Name (Entity));
      end if;

      Unref (Entity);
      Pop_State (Kernel_Handle (Kernel));

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
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
      Entity   : General_Entity)
   is
      Db : constant General_Xref_Database := Kernel.Databases;
   begin
      Go_To_Closest_Match
        (Kernel, Filename, Line, Column, Get_Name (Db, Entity));
   end Go_To_Closest_Match;

   -------------------------
   -- Go_To_Closest_Match --
   -------------------------

   procedure Go_To_Closest_Match
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename    : GNATCOLL.VFS.Virtual_File;
      Line        : Editable_Line_Type;
      Column      : Visible_Column_Type;
      Entity_Name : String)
   is
      Length            : constant Natural := Entity_Name'Length;
      Source            : Source_Editor_Box;
      File_Up_To_Date   : Boolean;
      L                 : Natural;
      Is_Case_Sensitive : Boolean;
      Iter              : Gtk_Text_Iter;

      Char_Column       : Character_Offset_Type;
   begin
      if Dir (Filename) = No_File then
         Insert (Kernel, -"File not found: "
                 & Display_Base_Name (Filename), Mode => Error);
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
              Entity_Name,
              Case_Sensitive => Is_Case_Sensitive);

         --  Search for the closest reference to the entity if
         --  necessary. Otherwise, there's nothing to be done, since the region
         --  was already selected when opening the editor
         if not File_Up_To_Date then
            --  Remove selection
            Get_Iter_At_Mark
              (Source.Source_Buffer, Iter,
               Get_Mark (Source.Source_Buffer, "selection_bound"));
            Place_Cursor (Source.Source_Buffer, Iter);

            if Get_Language_Context
              (Get_Language (Source.Source_Buffer)).Accurate_Xref
            then
               Kernel.Insert
                 (-("xref info mismatch, cursor was set at closest ref to ")
                    & Entity_Name);
            end if;

            --  Search for the closest reference to entity, and highlight the
            --  appropriate region in the editor.

            declare
               Buffer       : GNAT.Strings.String_Access;
               Col, Col_End : Visible_Column_Type;
               Found        : Boolean;
            begin
               L := Convert (Line);
               Buffer := Get_String (Source.Source_Buffer);
               Find_Closest_Match
                 (Buffer.all, L, Char_Column, Found,
                  Entity_Name,
                  Case_Sensitive => Get_Language_Context
                    (Get_Language (Source.Source_Buffer)).Case_Sensitive);
               Free (Buffer);

               Col := Expand_Tabs
                 (Source.Source_Buffer, Line, Char_Column);

               if Found then
                  Col_End := Col + Visible_Column_Type (Length);
                  --  ??? Computation for the end column is wrong if there is
                  --  an ASCII.HT within Length distance of Col.
               else
                  Col_End := 0;
               end if;

               Open_File_Editor
                 (Kernel, Filename, L, Col, Col_End, False);
            end;
         end if;
      end if;

      Get_Buffer_Factory (Kernel).Get (Filename).Current_View.Center;
   end Go_To_Closest_Match;

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

      --  If the line is not an editable line, return 1

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

   ----------------------------
   -- Status_Changed_Handler --
   ----------------------------

   procedure Status_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Buffer, Params);
   begin
      Update_Status (Box.Status_Bar);
      File_Status_Changed
        (Get_Kernel (Box),
         Get_Filename (Box.Source_Buffer),
         Get_Status (Box.Source_Buffer));
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
         Box.Source_Buffer.Get_Filename.Display_Full_Name,
         Box.Source_Buffer.Get_Filename.Display_Base_Name);
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end Filename_Changed_Handler;

   ----------------------------
   -- Update_Subprogram_Name --
   ----------------------------

   procedure Update_Subprogram_Name
     (Box : not null access Source_Editor_Box_Record'Class) is
   begin
      Update_Subprogram_Name (Box.Status_Bar);
   end Update_Subprogram_Name;

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

      Disconnect (Box.Source_Buffer, Box.Status_Handler);

      Delete (Box.Source_View);

      --  Remove the idle handler if it was registered
      if Box.Check_Timestamp_Registered then
         Glib.Main.Remove (Box.Check_Timestamp_Id);
      end if;
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
   end On_Box_Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box    : access Source_Editor_Box_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Source : Source_Buffer := null;
      Lang   : Language.Language_Access)
   is
      Scrolling_Area : Tooltip_Scrolled_Window;
      Drawing_Area   : Gtk_Drawing_Area;
      Hbox           : Gtk_Hbox;
      Frame          : Gtk_Frame;

   begin
      Initialize_Vbox (Box, Homogeneous => False);

      Box.Kernel := Kernel;

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => True, Fill => True);

      Gtk_New (Drawing_Area);
      Hbox.Pack_Start (Drawing_Area, Expand => False, Fill => False);
      Drawing_Area.Set_Size_Request (1, -1);

      Gtk_New (Scrolling_Area);
      Hbox.Pack_End (Scrolling_Area, Expand => True, Fill => True);

      if Source = null then
         Gtk_New (Box.Source_Buffer, Kernel, Lang => Lang);
      else
         Box.Source_Buffer := Source;
      end if;

      Gtk_New (Box.Source_View,
               Scrolling_Area,
               Drawing_Area,
               Box.Source_Buffer, Kernel);
      Scrolling_Area.Add (Box.Source_View);

      if Source = null then
         --  The newly created buffer is now under the responsability of the
         --  view.
         Unref (Box.Source_Buffer);
      end if;

      Create_Tooltips (Box).Set_Tooltip (Box.Source_View);

      --  The status bar, at the bottom of the window...

      Gtk_New (Box.Status_Bar, Box, Box.Source_View, Box.Source_Buffer);

      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_None);
      Pack_Start (Box, Frame, Expand => False, Fill => False);

      Frame.Add (Box.Status_Bar);

      Gtkada.Handlers.Return_Callback.Connect
        (Box,
         Gtk.Widget.Signal_Delete_Event,
         Delete_Callback'Access,
         After => False);

      --  Connect to source buffer signals

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

      Box_Callback.Connect
        (Box.Source_View,
         Signal_Destroy,
         On_Box_Destroy'Access,
         User_Data => Source_Editor_Box (Box));

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
         Trace (Traces.Exception_Handle, E);
         return False;
   end Key_Press;

   --------------------------
   -- Check_Timestamp_Idle --
   --------------------------

   function Check_Timestamp_Idle (Box : GObject) return Boolean is
      B : constant Source_Editor_Box := Source_Editor_Box (Box);
      Is_Grabbed : Boolean := False;
   begin
      Check_Writable (B);

      --  If we are currently holding down the mouse button, then we do not
      --  want to offer to reload the file, since the pop-up dialog will
      --  not be able to get the mouse input. Therefore we do nothing in
      --  this idle callback, but keep it registered so that the timestamp
      --  is checked after the button release.

      if Pointer_Is_Grabbed then
         Is_Grabbed := True;
      else
         --  If the pointer is not grabbed, we might still have a button down,
         --  for instance when dragging the window around. In this case,
         --  do nothing until the button is released.
         declare
            State     : Gdk.Types.Gdk_Modifier_Type;
            Ignored_X, Ignored_Y : Gint;
            Display   : constant Gdk_Display := Get_Default;
            Screen    : Gdk.Screen.Gdk_Screen;
         begin
            Get_Pointer (Display => Display,
                         Screen  => Screen,
                         X       => Ignored_X,
                         Y       => Ignored_Y,
                         Mask    => State);
            if (State and Button1_Mask) /= 0 then
               Is_Grabbed := True;
            end if;
         end;
      end if;

      if Is_Grabbed then
         --  No need to try again if the file is up-to-date. Otherwise, we'll
         --  need to try again. One issue here is that while displaying a
         --  contextual menu for an editor that didn't have the focus before,
         --  and the file on disk has been modified, the idle callback is
         --  called in a loop, and that takes 100% of CPU (G227-035).
         B.Check_Timestamp_Registered :=
           not Check_Timestamp_And_Diff (B.Source_Buffer, Update => False)
           or else
             (not Is_Regular_File (Get_Filename (B.Source_Buffer))
              and then B.Source_Buffer.Has_Been_Saved
              and then B.Source_Buffer.Get_Writable);
         --  The above part is to detect a writable file that has been saved at
         --  some point (so was existing on disk) but removed somehow.
         return B.Check_Timestamp_Registered;
      end if;

      B.Check_Timestamp_Registered := False;
      if B.Timestamp_Mode = Check_At_Focus then
         B.Timestamp_Mode := Checking;

         Check_Timestamp_And_Reload
           (B, Interactive => True, Always_Reload => False);

         B.Timestamp_Mode := Check_At_Focus;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
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
           Object_Idle.Idle_Add (Check_Timestamp_Idle'Access, GObject (Box));
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
         Trace (Traces.Exception_Handle, E);
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
      Get_Contextual_Menu
        (Context, Kernel, Object,
         Location => Location_Event,
         Event    => Event,
         Menu     => Menu);
   end Get_Contextual_Menu;

   -------------------------
   -- Get_Contextual_Menu --
   -------------------------

   procedure Get_Contextual_Menu
     (Context  : in out GPS.Kernel.Selection_Context;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object   : access Glib.Object.GObject_Record'Class;
      Location : Location_Type := Location_Cursor;
      Event    : Gdk.Event.Gdk_Event := null;
      Menu     : Gtk.Menu.Gtk_Menu := null)
   is
      Editor                     : constant Source_Editor_Box :=
                                     Source_Editor_Box (Object);
      V                          : constant Source_View := Editor.Source_View;
      Filename                   : constant GNATCOLL.VFS.Virtual_File :=
                                     Get_Filename (Editor.Source_Buffer);
      Loc                        : Location_Type := Location;
      Line                       : Gint := 0;
      Column                     : Gint := 0;
      X, Y                       : Gint;
      Start_Iter                 : Gtk_Text_Iter;
      End_Iter                   : Gtk_Text_Iter;
      Entity_Start               : Gtk_Text_Iter;
      Entity_End                 : Gtk_Text_Iter;
      Has_Selection              : Boolean;
      Out_Of_Bounds              : Boolean := False;
      Start_Line, End_Line       : Integer;
      Selection_Is_Single_Entity : Boolean;
      Click_In_Selection         : Boolean;
      Mouse_X, Mouse_Y           : Gint;
      Mask                       : Gdk.Types.Gdk_Modifier_Type;
      Win                        : Gdk.Gdk_Window;

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
         return Has_Selection
           and then
             (L > L_Start or else (L = L_Start and then C >= C_Start))
           and then
             (L < L_End or else (L = L_End and then C <= C_End));
      end In_Selection;

      Cursor_Location : Gtk_Text_Iter;
      Xevent, Yevent : Gdouble;
   begin
      if Location = Location_Event
        and then
        (Event = null
         or else Get_Event_Type (Event) not in Button_Press .. Button_Release)
      then
         Loc := Location_Cursor;
      end if;

      --  If there is indeed a menu, cancel the selection drag.
      --  Otherwise, we are calling this function only to create a context (for
      --  instance when displaying a tooltip) and we should not cancel the
      --  selection drag.
      if Menu /= null then
         Stop_Selection_Drag (V);
      end if;

      Set_Context_Information
        (Context => Context,
         Kernel  => Kernel,
         Creator => Abstract_Module_ID (Src_Editor_Module_Id));

      --  Compute the location for which we should compute the context.
      --  Output of this block is (Line, Column) within the editor.

      case Loc is
         when Location_Event =>
            if Get_Window (Event) = Get_Window (V, Text_Window_Left) then
               --  Click in the line numbers area
               Get_Coords (Event, Xevent, Yevent);
               Window_To_Buffer_Coords
                 (Editor.Source_View, Text_Window_Left,
                  Gint (Xevent), Gint (Yevent), X, Y);
               Get_Iter_At_Location (Editor.Source_View, Start_Iter, X, Y);
               Line := Get_Line (Start_Iter);
               Place_Cursor (Editor.Source_Buffer, Start_Iter);
               --  ??? We are not computing the context

               Set_File_Information
                 (Context,
                  Files  => (1 => Filename),
                  Line   => Integer (To_Box_Line (Editor.Source_Buffer, Line)),
                  Column => 0);
               return;

            else
               Event_To_Buffer_Coords
                 (Editor.Source_View, Event, Line, Column, Out_Of_Bounds);
            end if;

         when Location_Cursor =>
            Get_Iter_At_Mark
              (Editor.Source_Buffer, Start_Iter,
               Get_Insert (Editor.Source_Buffer));
            Line   := Get_Line (Start_Iter);
            Column := Get_Line_Offset (Start_Iter);

         when Location_Mouse =>
            Get_Pointer (Get_Window (V, Text_Window_Text),
                         Mouse_X, Mouse_Y, Mask, Win);
            Window_To_Buffer_Coords
              (V, Mouse_X, Mouse_Y, Line, Column, Out_Of_Bounds);

            if Out_Of_Bounds then
               Set_File_Information (Context, Files  => (1 => Filename));
               return;
            end if;
      end case;

      --  If we have a current selection, use it as the context if the user
      --  clicked inside it (ie consider the selection as an opaque block and
      --  don't look inside)

      Get_Selection_Bounds
        (Editor.Source_Buffer, Start_Iter, End_Iter, Has_Selection);

      if Out_Of_Bounds and then not Has_Selection then
         Trace (Me, "Get_Context_Menu: no current selection");

         --  The position we are looking at is outside the text, and there is
         --  no current selection. We move the cursor to a valid location, but
         --  there is no additional info to set in the context.
         Get_Iter_At_Line_Offset
           (Editor.Source_Buffer, Start_Iter, Line, Column);
         Acquire_Focus (Editor.Source_View);
         Place_Cursor (Editor.Source_Buffer, Start_Iter);

         --  Set basic context information about current file

         Set_File_Information
           (Context,
            Files  => (1 => Filename),
            Line   => Integer (To_Box_Line (Editor.Source_Buffer, Line)),
            Column => Expand_Tabs
              (Get_Buffer (Editor),
               Editable_Line_Type (To_Box_Line (Editor.Source_Buffer, Line)),
               To_Box_Column (Column)));

      else
         Get_Iter_At_Line_Offset
           (Editor.Source_Buffer, Entity_Start, Line, Column);
         Copy (Source => Entity_Start, Dest => Cursor_Location);

         Click_In_Selection :=
           Has_Selection
           and then In_Range (Entity_Start, Start_Iter, End_Iter);

         if Active (Me) then
            Trace (Me,
                   "Get_Context_Menu: line="
                   & To_Box_Line (Editor.Source_Buffer, Line)'Img
                   & " click in selection? " & Click_In_Selection'Img);
         end if;

         declare
            Str        : Src_String :=
              Get_String
                (Get_Buffer (Editor),
                 Get_Editable_Line
                   (Editor.Source_Buffer,
                    Buffer_Line_Type (Get_Line (Entity_Start)) + 1));
            The_Line   : Editable_Line_Type;
            The_Column : Character_Offset_Type;

         begin
            Search_Entity_Bounds
              (Entity_Start, Entity_End,
               Maybe_File => Str.Contents /= null
                 and then Has_Include_Directive
                   (Str.Contents (1 .. Str.Length)));
            Selection_Is_Single_Entity :=
              Has_Selection
              and then Equal (Entity_Start, Start_Iter)
              and then Equal (Entity_End, End_Iter);

            --  If the location is in the current selection, use this as the
            --  context. However, if the selection is a single entity, we
            --  should create a context such that cross-references menus also
            --  appear.

            if not Selection_Is_Single_Entity
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

               Set_File_Information
                 (Context,
                  Files  => (1 => Filename),
                  Line   => Integer (To_Box_Line (Editor.Source_Buffer, Line)),
                  Column => Expand_Tabs
                    (Get_Buffer (Editor),
                     Editable_Line_Type
                       (To_Box_Line (Editor.Source_Buffer, Line)),
                     To_Box_Column (Column)));

               Trace (Me, "Set_Area_Information");
               Set_Area_Information
                 (Context,
                  Get_Text (Start_Iter, End_Iter),
                  Start_Line, End_Line);

            else
               --  Set basic context information about current file. Must be
               --  done before we set the entity information.

               Set_File_Information
                 (Context,
                  Files  => (1 => Filename),
                  Line   => Integer (To_Box_Line (Editor.Source_Buffer, Line)),
                  Column => Expand_Tabs
                    (Get_Buffer (Editor),
                     Editable_Line_Type
                       (To_Box_Line (Editor.Source_Buffer, Line)),
                     To_Box_Column (Column)));

               --  Expand the tabs

               Get_Iter_Position
                 (Editor.Source_Buffer, Entity_Start, The_Line, The_Column);

               if Str.Contents /= null then
                  if Click_In_Selection then
                     --  If there was a selection and we clicked in it, we only
                     --  should take the selection into account. For instance,
                     --  this is a way for the user to force a specific name to
                     --  be sent to the debugger instead of the whole
                     --  expression

                     Set_Entity_Information
                       (Context,
                        Entity_Name => Get_Text (Entity_Start, Entity_End),
                        Entity_Column => Expand_Tabs
                          (Editor.Source_Buffer, The_Line, The_Column));

                  else
                     Set_Entity_Information
                       (Context,
                        Entity_Name   => Get_Text (Entity_Start, Entity_End),
                        Entity_Column => Expand_Tabs
                          (Editor.Source_Buffer, The_Line, The_Column),
                        From_Expression =>
                          Parse_Reference_Backwards
                            (Get_Language (Get_Buffer (Editor)),
                             Buffer          => Str.Contents (1 .. Str.Length),
                             Start_Offset      =>
                               String_Index_Type
                                 (Get_Line_Index (Entity_End))));
                  end if;
               else
                  Set_Entity_Information
                    (Context,
                     Entity_Name   => Get_Text (Entity_Start, Entity_End),
                     Entity_Column => Expand_Tabs
                       (Editor.Source_Buffer, The_Line, The_Column));
               end if;

               Free (Str);

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
                  Place_Cursor (Editor.Source_Buffer, Cursor_Location);
               end if;
            end if;
         end;
      end if;
   end Get_Contextual_Menu;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Creator : access Goto_Body_Menu_Label;
      Context : Selection_Context) return String
   is
      pragma Unreferenced (Creator);

      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Entity : constant General_Entity := Get_Entity (Context);
      Decl   : General_Entity_Declaration;

   begin
      if Entity /= No_General_Entity then
         Decl := Kernel.Databases.Get_Declaration (Entity);

         if Decl.Body_Is_Full_Declaration then
            return Substitute_Label
              (-"Goto full declaration of %ef", Context);
         else
            return Substitute_Label (-"Goto body of %ef", Context);
         end if;
      end if;

      return "";
   end Get_Label;

   ----------------------------
   -- On_Goto_Declaration_Of --
   ----------------------------

   procedure On_Goto_Declaration_Of
     (Object : access GObject_Record'Class;
      Data   : Entity_And_Kernel)
   is
      pragma Unreferenced (Object);
      K        : constant Kernel_Handle := Data.Kernel;
      Db       : constant General_Xref_Database := K.Databases;
      Location : constant General_Location :=
        Get_Declaration (Db, Data.Entity).Loc;
   begin
      Go_To_Closest_Match
        (Kernel   => K,
         Filename => Location.File,
         Line     => Convert (Location.Line),
         Column   => Location.Column,
         Entity   => Data.Entity);
   end On_Goto_Declaration_Of;

   ---------------------
   -- On_Goto_Body_Of --
   ---------------------

   procedure On_Goto_Body_Of
     (Object : access GObject_Record'Class;
      Data   : Entity_And_Kernel)
   is
      pragma Unreferenced (Object);
      K   : constant Kernel_Handle := Data.Kernel;
      Db  : constant General_Xref_Database := K.Databases;
      Loc : General_Location;

   begin
      Loc := Db.Get_Body (Data.Entity);
      Go_To_Closest_Match
        (Kernel   => K,
         Filename => Loc.File,
         Line     => Convert (Loc.Line),
         Column   => Loc.Column,
         Entity   => Data.Entity);
   end On_Goto_Body_Of;

   --------------------------------
   -- Append_To_Dispatching_Menu --
   --------------------------------

   procedure Append_To_Dispatching_Menu
     (Menu          : access Gtk.Menu.Gtk_Menu_Record'Class;
      Context       : GPS.Kernel.Selection_Context;
      Default_Title : String;
      Filter        : Reference_Kind_Filter;
      Show_Default  : Boolean;
      Callback      : Entity_Callback.Simple_Handler)
   is
      Item   : Gtk_Menu_Item;
      Label  : Gtk_Label;
      Count  : Natural := 0;
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Xref_Db : constant General_Xref_Database := Kernel.Databases;

      function On_Callee
        (Callee, Primitive_Of : General_Entity) return Boolean;

      --  CP record is used to sort the menu entries by means of an ordered set

      type CP is record
         Callee, Primitive_Of : General_Entity;
      end record;

      function "<" (Left, Right : CP) return Boolean;
      overriding function "=" (Left, Right : CP) return Boolean;

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : CP) return Boolean is
      begin
         return Get_Name (Xref_Db, Left.Primitive_Of)
           < Get_Name (Xref_Db, Right.Primitive_Of);
      end "<";

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : CP) return Boolean is
      begin
         return Get_Name (Xref_Db, Left.Primitive_Of)
           = Get_Name (Xref_Db, Right.Primitive_Of);
      end "=";

      package CP_Set is new Ordered_Sets (CP);

      procedure Fill_Menu (Position : CP_Set.Cursor);
      --  Fill the menu with the entities pointed to by Position

      ---------------
      -- Fill_Menu --
      ---------------

      procedure Fill_Menu (Position : CP_Set.Cursor) is
         E : constant CP := CP_Set.Element (Position);
      begin
         Gtk_New (Label,
                  "Primitive of: "
                  & Emphasize (Get_Name (Xref_Db, E.Primitive_Of)));
         Set_Use_Markup (Label, True);
         Set_Alignment (Label, 0.0, 0.5);
         Gtk_New (Item);
         Add (Item, Label);
         Entity_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Callback, (Kernel => Get_Kernel (Context), Entity => E.Callee));
         Add (Menu, Item);
         Count := Count + 1;
      end Fill_Menu;

      E_Set : CP_Set.Set;

      ---------------
      -- On_Callee --
      ---------------

      function On_Callee
        (Callee, Primitive_Of : General_Entity) return Boolean
      is
         New_Elem : constant CP := (Callee, Primitive_Of);
      begin
         E_Set.Include (New_Elem);
         return True;
      end On_Callee;

      --  Start of processing for Append_To_Dispatching_Menu

   begin
      Increase_Indent (Me, "Computing Dispatch_Submenu: " & Default_Title);
      Push_State (Kernel, Busy);

      --  Before getting its entity we check if the LI handler has unresolved
      --  imported references to force updating its references
      --  ??? Seems unneeded

      Ensure_Context_Up_To_Date (Context);

      Xref.For_Each_Dispatching_Call
        (Dbase     => Xref_Db,
         Entity    => Get_Entity (Context),
         Ref       => Get_Closest_Ref (Context),
         On_Callee => On_Callee'Access,
         Filter    => Filter);

      E_Set.Iterate (Fill_Menu'Access);

      --  If we have not found any possible call, we must be missing some
      --  .ALI file (for instance the one containing the declaration of the
      --  tagged type). In that case we show the same info as we would have for
      --  a non-dispatching call to be as helpful as possible

      if Count = 0 and then Show_Default then
         Gtk_New
           (Label,
            Default_Title
              & Emphasize (Entity_Name_Information (Context)));
         Set_Use_Markup (Label, True);
         Set_Alignment (Label, 0.0, 0.5);
         Gtk_New (Item);
         Add (Item, Label);
         Entity_Callback.Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Callback, (Kernel => Kernel, Entity => Get_Entity (Context)));
         Add (Menu, Item);
      end if;

      Pop_State (Kernel);
      Decrease_Indent (Me, "Done computing Dispatch_Declaration_Submenu");

   exception
      when E : others =>
         Trace (Me, E);
         Pop_State (Kernel);
   end Append_To_Dispatching_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Declaration_Submenu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
   begin
      Append_To_Dispatching_Menu
        (Menu          => Menu,
         Context       => Context,
         Default_Title => -"Declaration of ",
         Filter        => null,
         Show_Default  => True,
         Callback      => On_Goto_Declaration_Of'Access);
   end Append_To_Menu;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Goto_Dispatch_Body_Submenu;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory, Object);
   begin
      Append_To_Dispatching_Menu
        (Menu          => Menu,
         Context       => Context,
         Default_Title => -"Body of ",
         Filter        => Reference_Is_Body'Access,
         Show_Default  => Has_Body (Context),
         Callback      => On_Goto_Body_Of'Access);
   end Append_To_Menu;

   --------------
   -- Has_Body --
   --------------

   function Has_Body (Context : GPS.Kernel.Selection_Context) return Boolean is
      Kernel : constant Kernel_Handle  := Get_Kernel (Context);
      Entity : constant General_Entity := Get_Entity (Context);
      Location         : General_Location;
      Spec_Location : General_Location;
   begin
      if Entity = No_General_Entity then
         return False;
      end if;

      Spec_Location := Kernel.Databases.Get_Declaration (Entity).Loc;

      Location := Kernel.Databases.Get_Body (Entity);

      return Location /= No_Location
        and then Location /= Spec_Location;
   end Has_Body;

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
         Trace (Traces.Exception_Handle, E);
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
      Source : access Source_Editor_Box_Record)
   is
      Line : Editable_Line_Type;
      Col  : Character_Offset_Type;
   begin
      Box := new Source_Editor_Box_Record;
      Initialize
        (Box, Kernel_Handle (Kernel), Source.Source_Buffer,
         Get_Language (Source.Source_Buffer));

      if not Get_Writable (Box.Source_Buffer) then
         Set_Editable (Box.Source_View, False);
      end if;

      --  Preserve the current location
      Get_Cursor_Position (Box.Source_Buffer, Line, Col);
      Box.Set_Cursor_Location
        (Line   => Line,
         Column => Col);
      Box.Source_View.Set_Position_Set_Explicitely;

      Update_Status (Box.Status_Bar);
   end Create_New_View;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Box : access Source_Editor_Box_Record) return GPS.Kernel.Kernel_Handle is
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
      if not Get_Explicit_Writable_Set (Editor.Source_Buffer) then
         if Read_Only_Set then
            Writable := False;

         else
            Writable :=
              Get_Filename (Editor.Source_Buffer) = GNATCOLL.VFS.No_File
              or else Is_Writable (Get_Filename (Editor.Source_Buffer))
              or else
                (not Is_Regular_File (Get_Filename (Editor.Source_Buffer))
                 and then Get_Writable (Editor.Source_Buffer));
         end if;

         Set_Writable (Editor.Source_Buffer, Writable, Explicit => False);
      end if;

      Set_Writable (Views, Get_Writable (Editor.Source_Buffer));
   end Check_Writable;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (Views : Views_Array; Writable : Boolean) is
   begin
      for V in Views'Range loop
         Set_Editable (Views (V).Source_View, Writable);
         Update_Status (Views (V).Status_Bar);

         --  Changing the class does not take into account the CSS
         --  background-color, for some reason, although it does take other
         --  attributes like "color" into account.
         Views (V).Source_View.Set_Background_Color;
      end loop;
   end Set_Writable;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Force_Focus     : Boolean := True;
      Success         : out Boolean) is
   begin
      Load_File (Editor.Source_Buffer, Filename, Lang_Autodetect, Success);

      if Success then
         Set_Cursor_Location (Editor, 1, 1, Force_Focus);
         Set_Filename (Editor.Source_Buffer, Filename);
         Editor.Source_Buffer.Status_Changed;
      end if;
   end Load_File;

   ---------------------
   -- Load_Empty_File --
   ---------------------

   procedure Load_Empty_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
      Initial_Dir     : GNATCOLL.VFS.Virtual_File;
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
      Set_Initial_Dir (Editor.Source_Buffer, Initial_Dir);
      Set_Charset (Editor.Source_Buffer, Get_File_Charset (Filename));

      Set_Writable (Editor.Source_Buffer, Writable => True, Explicit => False);
      Load_Empty_File (Editor.Source_Buffer);

      Update_Status (Editor.Status_Bar);
   end Load_Empty_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success  : out Boolean;
      Force    : Boolean := False)
   is
      File          : constant GNATCOLL.VFS.Virtual_File :=
                        Get_Filename (Editor.Source_Buffer);
      Constructs    : Construct_List;
      Info          : Construct_Access;
      New_Base_Name : Filesystem_String_Access;
      Part          : Unit_Parts;

      Buffer        : GNAT.Strings.String_Access;

   begin
      --  Do not authorize saving a read-only file, unless we save it to
      --  another disk file.

      if not Get_Writable (Editor.Source_Buffer)
        and then Filename = GNATCOLL.VFS.No_File
      then
         Success := False;
         return;
      end if;

      --  ??? Should we reset the read-only status to False: either the
      --  file already had that status, or we just saved a read-only file to
      --  a different disk file, and thus this is now writable

      Success := True;

      if Filename = GNATCOLL.VFS.No_File or else Filename.Is_Directory then
         if File = GNATCOLL.VFS.No_File or else File.Is_Directory then
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
              or else Info.Name = No_Symbol
            then
               --  No unit name found
               New_Base_Name := new Filesystem_String'("");
            else
               --  Info.Name is a valid Ada unit name

               if Info.Is_Declaration then
                  Part := Unit_Spec;
               else
                  Part := Unit_Body;
               end if;

               New_Base_Name := new Filesystem_String'
                 (Get_Project (Editor.Kernel).File_From_Unit
                    (Unit_Name       => To_Lower (Get (Info.Name).all),
                     Part            => Part,
                     File_Must_Exist => False,
                     Language        => "ada"));
            end if;

            Free (Constructs);

            declare
               Name : constant Virtual_File := Select_File
                 (Title             => -"Save File As",
                  Base_Directory    => Editor.Source_Buffer.Get_Initial_Dir,
                  Parent            => Get_Current_Window (Editor.Kernel),
                  Default_Name      => New_Base_Name.all,
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Save_File,
                  File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                  Pattern_Name      => -"All files;Ada files;C/C++ files",
                  History           => Get_History (Editor.Kernel));

            begin
               Free (New_Base_Name);

               if Name = GNATCOLL.VFS.No_File then
                  Success := False;
                  return;
               end if;

               if not Force
                 and then Is_Regular_File (Name)
                 and then Message_Dialog
                   (Msg => Display_Base_Name (Name)
                    & (-" already exists. Do you want to overwrite ?"),
                    Dialog_Type => Confirmation,
                    Buttons     => Button_OK or Button_Cancel,
                    Title       => "Confirm overwriting",
                    Parent      =>
                      Get_Current_Window (Editor.Kernel)) /= Button_OK
               then
                  Success := False;
               end if;

               if Success then
                  Save_To_File
                    (Editor.Source_Buffer, Name, Success, Force => Force);
               end if;
            end;

         else
            if not Force
              and then not Check_Timestamp_And_Diff (Editor.Source_Buffer)
              and then Message_Dialog
                (Msg => Display_Base_Name (File)
                        & (-" changed on disk. Do you want to overwrite ?"),
                 Dialog_Type => Confirmation,
                 Buttons     => Button_OK or Button_Cancel,
                 Title       => -"File changed on disk",
                 Parent      =>
                   Get_Current_Window (Editor.Kernel)) /= Button_OK
            then
               Success := False;
            else
               Save_To_File
                 (Editor.Source_Buffer, File, Success, Force => Force);
            end if;
         end if;

      else
         if not Force
           and then Is_Regular_File (Filename)
           and then Message_Dialog
             (Msg => Display_Base_Name (Filename)
              & (-" already exists. Do you want to overwrite ?"),
              Dialog_Type => Confirmation,
              Buttons     => Button_OK or Button_Cancel,
              Title       => "Confirm overwriting",
              Parent      => Get_Current_Window (Editor.Kernel)) /= Button_OK
         then
            Success := False;
         end if;

         if Success then
            Save_To_File
              (Editor.Source_Buffer, Filename, Success, Force => Force);
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
      Ignore : Gtk_Widget;
      pragma Unreferenced (Ignore);

      Exists   : constant Boolean :=
                   Is_Regular_File
                     (Get_Filename (Editor.Source_Buffer));
      Response : Gtk_Response_Type;
      Success  : Boolean;
      Line     : Editable_Line_Type;
      Column   : Character_Offset_Type;
      Data     : aliased File_Hooks_Args;
   begin
      if Get_Filename (Editor.Source_Buffer) /= GNATCOLL.VFS.No_File then
         if Always_Reload
           or else not Check_Timestamp_And_Diff
             (Editor.Source_Buffer, Update => True)
           or else (not Exists
                    and then Editor.Source_Buffer.Has_Been_Saved
                    and then Editor.Source_Buffer.Get_Writable)
         --  The above part is to detect a writable file that has been saved at
         --  some point (so was existing on disk) but removed somehow.
         then
            if Always_Reload
              or else not Interactive

              --  No dialog in the testsuite
              or else Active (Traces.Testsuite_Handle)
            then
               Response := Gtk_Response_No;

            else
               Data.File := Get_Filename (Editor.Source_Buffer);

               if not Run_Hook_Until_Success
                 (Editor.Kernel, File_Changed_Detected_Hook,
                  Data'Unchecked_Access)
               then
                  if Exists then
                     Dialog := Create_Gtk_Dialog
                       (Display_Base_Name (Get_Filename (Editor.Source_Buffer))
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
                     Ignore := Add_Button (Dialog, -"Reload", Gtk_Response_No);

                  else
                     Dialog := Create_Gtk_Dialog
                       (Display_Base_Name (Get_Filename (Editor.Source_Buffer))
                        & (-" removed from disk.")
                        & ASCII.LF & ASCII.LF
                        & (-"Click on Ignore to keep this editing session.")
                        & ASCII.LF
                        & (-"Click on Close to remove the file buffer."),
                        Dialog_Type   => Confirmation,
                        Title         => -"File removed from disk",
                        Justification => Justify_Left,
                        Parent        => Get_Current_Window (Editor.Kernel));
                     Ignore := Add_Button (Dialog, -"Close", Gtk_Response_No);
                  end if;

                  Ignore := Add_Button (Dialog, -"Ignore", Gtk_Response_Yes);

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
                  if not Exists then
                     --  Re-create the file from current buffer content
                     declare
                        F : Virtual_File;
                        W : Writable_File;
                        C : GNAT.Strings.String_Access :=
                              Get_String (Editor.Source_Buffer);
                     begin
                        F := Create_From_UTF8
                          (String
                             (Full_Name
                                (Get_Filename (Editor.Source_Buffer)).all));
                        W := Write_File (F);
                        Write (W, C.all);
                        Close (W);
                        Free (C);
                     end;
                  end if;

               when Gtk_Response_No =>
                  if not Exists then
                     Close_File_Editors
                       (Editor.Kernel,
                        Get_Filename (Editor.Source_Buffer));

                  else
                     Get_Cursor_Position (Get_Buffer (Editor), Line, Column);
                     Load_File
                       (Editor.Source_Buffer,
                        Filename        => Get_Filename (Editor.Source_Buffer),
                        Lang_Autodetect => True,
                        Success         => Success);

                     if Is_Valid_Position (Editor.Source_Buffer, Line) then
                        Set_Cursor_Location (Editor, Line, Column, False);
                     end if;
                  end if;

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
      Centering   : Centering_Type := Minimal;
      Extend_Selection : Boolean := False)
   is
      Editable_Line : Editable_Line_Type renames Line;

   begin
      End_Action (Editor.Source_Buffer);

      if Is_Valid_Position (Editor.Source_Buffer, Editable_Line, Column) then
         if Force_Focus then
            Grab_Toplevel_Focus (Get_MDI (Editor.Kernel), Editor);
         end if;

         Set_Cursor_Position
           (Editor.Source_Buffer, Editable_Line, Column,
            Internal  => False,
            Extend_Selection => Extend_Selection);

         if Centering /= Minimal then
            Editor.Source_View.Set_Position_Set_Explicitely;
         end if;

         Save_Cursor_Position (Editor.Source_View);
         Scroll_To_Cursor_Location (Editor.Source_View, Centering);

      elsif Is_Valid_Position (Editor.Source_Buffer, Editable_Line) then
         --  We used to generate an error message (Invalid column number),
         --  but this was too intrusive: in the case of e.g. loading the
         --  desktop, if it often the case that the files have been modified
         --  and the column is no longer valid, so we silently ignore this.
         --  ??? Consider going to the last column instead of the first

         if Force_Focus then
            Grab_Toplevel_Focus (Get_MDI (Editor.Kernel), Editor);
         end if;

         Set_Cursor_Position
           (Editor.Source_Buffer, Editable_Line, 1, False,
            Extend_Selection => Extend_Selection);

         if Centering /= Minimal then
            Editor.Source_View.Set_Position_Set_Explicitely;
         end if;

         Save_Cursor_Position (Editor.Source_View);
         Scroll_To_Cursor_Location (Editor.Source_View, Centering);

      else
         if Column = 1 then
            Editor.Kernel.Insert
              (-"Invalid line number: " & String_Utils.Image (Integer (Line)),
               Mode => Error);
         else
            Editor.Kernel.Insert
              (-"Invalid source location: " &
               String_Utils.Image (Integer (Line)) &
               ':' & String_Utils.Image (Natural (Column)), Mode => Error);
         end if;
      end if;
   end Set_Cursor_Location;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Messages   : Message_Array) is
   begin
      Add_File_Information (Editor.Source_Buffer, Identifier, Messages);
   end Add_File_Information;

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
      Ignore    : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Get_Iter_At_Mark (Editor.Source_Buffer, Iter, Mark);
      End_Action (Editor.Source_Buffer);
      Ignore := Scroll_To_Iter
        (Editor.Source_View, Iter, 0.0, True, 0.5, 0.5);

      --  Ignore value of Success. We want to keep doing the code below
      --  even in case of failure, see e.g. bookmarks.[12] tests

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
           and then not (Length = 1 and then Get_Char (Mark_Iter) = ASCII.LF)
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
      Block : constant Block_Record := Get_Block
        (Editor.Source_Buffer, Line, True);
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
      Block : constant Block_Record := Get_Block
        (Editor.Source_Buffer, Line, True);
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
      Block : constant Block_Record := Get_Block
        (Editor.Source_Buffer, Line, True);
   begin
      return Get (Block.Name).all;
   end Get_Block_Name;

   --------------------
   -- Get_Block_Type --
   --------------------

   function Get_Block_Type
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String
   is
      Block : constant Block_Record := Get_Block
        (Editor.Source_Buffer, Line, True);
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
      Block : constant Block_Record := Get_Block
        (Editor.Source_Buffer, Line, True);
   begin
      return Natural (Block.Indentation_Level);
   end Get_Block_Level;

   -------------------------
   -- Get_Subprogram_Name --
   -------------------------

   function Get_Subprogram_Name
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type) return String
   is
      Block       : Block_Record;
   begin
      Block := Get_Subprogram_Block (Editor.Source_Buffer, Line);

      if Block.Name /= No_Symbol then
         return Get (Block.Name).all;
      else
         return "";
      end if;
   end Get_Subprogram_Name;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Editor : access Source_Editor_Box_Record) return String
   is
      Begin_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Bounds (Editor.Source_Buffer, Begin_Iter, End_Iter);
      return Get_Text (Begin_Iter, End_Iter);
   end Get_Buffer;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Editor   : access Source_Editor_Box_Record;
      Writable : Boolean;
      Explicit : Boolean := False)
   is
      Views : constant Views_Array := Get_Views (Editor.Source_Buffer);
   begin
      Set_Writable (Editor.Source_Buffer, Writable, Explicit);

      if Writable then
         Add_Controls (Editor.Source_Buffer);
      else
         Remove_Controls (Editor.Source_Buffer);
      end if;

      Set_Writable (Views, Writable);
   end Set_Writable;

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
