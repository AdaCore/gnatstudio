------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                    use GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.Symbols;               use GNATCOLL.Symbols;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;
with GNATCOLL.VFS;                   use GNATCOLL.VFS;

with Gdk;                            use Gdk;
with Gdk.Event;                      use Gdk.Event;

with Glib.Object;                    use Glib.Object;
with Glib.Values;                    use Glib.Values;
with Glib;                           use Glib;

with Gtk;                            use Gtk;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Dialog;                     use Gtk.Dialog;
with Gtk.Drawing_Area;               use Gtk.Drawing_Area;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Event_Box;                  use Gtk.Event_Box;
with Gtk.Frame;                      use Gtk.Frame;
with Gtk.Handlers;                   use Gtk.Handlers;
with Gtk.Text_Iter;                  use Gtk.Text_Iter;
with Gtk.Text_Mark;                  use Gtk.Text_Mark;
with Gtk.Widget;                     use Gtk.Widget;

with Gtkada.Dialogs;                 use Gtkada.Dialogs;
with Gtkada.File_Selector;           use Gtkada.File_Selector;
with Gtkada.Handlers;
with Gtkada.MDI;                     use Gtkada.MDI;

with Find_Utils;                     use Find_Utils;
with GPS.Dialogs;                    use GPS.Dialogs;
with GPS.Editors.Line_Information;   use GPS.Editors.Line_Information;
with GPS.Intl;                       use GPS.Intl;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Charsets;            use GPS.Kernel.Charsets;
with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                 use GPS.Kernel.MDI;
with GPS.Kernel.Modules;             use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;          use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Project;             use GPS.Kernel.Project;

with GUI_Utils;                      use GUI_Utils;
with Language;                       use Language;
with Language.Ada;                   use Language.Ada;
with Language_Handlers;              use Language_Handlers;
with Src_Editor_Box.Scrolled_Window; use Src_Editor_Box.Scrolled_Window;
with Src_Editor_Box.Tooltips;        use Src_Editor_Box.Tooltips;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Src_Editor_Module.Markers;      use Src_Editor_Module.Markers;
with Src_Editor_Module;              use Src_Editor_Module;
with Src_Editor_View;                use Src_Editor_View;
with String_Utils;
with Tooltips;                       use Tooltips;
with Xref;                           use Xref;

package body Src_Editor_Box is

   use type Basic_Types.Visible_Column_Type;

   Me : constant Trace_Handle := Create ("GPS.SOURCE_EDITOR.EDITOR_BOX");

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
   --  Callback for the "delete_event" signal

   procedure Status_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Reflect the change in buffer status

   procedure Filename_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Reflect the change in buffer filename or project information.

   procedure On_Box_Destroy
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box);
   --  Callback for the "destroy" signal

   function Focus_In (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_in event. This checks whether the physical file
   --  on the disk is more recent than the one that was read for the editor.

   function Focus_Out (Box : access GObject_Record'Class) return Boolean;
   --  Callback for the focus_out event

   procedure Initialize
     (Box         : access Source_Editor_Box_Record'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Filename    : GNATCOLL.VFS.Virtual_File;
      Source      : access Source_Buffer_Record'Class);
   --  Internal version of Initialize, which can create new views

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
           (Kernel  => Source.Kernel,
            File    => File,
            Project => Get_Project (Source),
            Line    => Line,
            Column  => Column));
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
      Location : General_Location;
      Current  : General_Location;

   begin
      if Get_Filename (Editor) = GNATCOLL.VFS.No_File then
         Kernel.Insert
           (-"Cross-references not possible on unamed files",
            Mode => Error);
         return;
      end if;

      --  Query the entity
      declare
         Entity : constant Root_Entity'Class := Get_Entity (Context);
      begin

         if Entity = No_Root_Entity then
            --  Probably means that we either could not locate the ALI file,
            --  or it could also be that we failed to parse it. Either way,
            --  a message should have already been printed. So, just abort.

            Kernel.Insert
              (-"No cross-reference information found for "
               & Entity_Name_Information (Context) & ASCII.LF,
               Mode => Error);
            return;
         end if;

         --  Get the declaration/body

         if To_Body then
            Current := (File => File_Information (Context),
                        Line => GPS.Kernel.Contexts.Line_Information (Context),
                        Project_Path => Project_Information
                          (Context).Project_Path,
                        Column => Entity_Column_Information (Context));
            Location := Get_Body (Entity, After => Current);
         else
            Get_Entity_Spec_Locations (Context, Location);
         end if;

         if Location /= No_Location then
            Go_To_Closest_Match
              (Kernel      => Kernel,
               Filename    => Location.File,
               Project     => Get_Project (Location),
               Line        => Convert (Location.Line),
               Column      => Location.Column,
               Entity_Name => Get_Name (Entity));
         end if;
      end;
   end Goto_Declaration_Or_Body;

   -------------------------
   -- Go_To_Closest_Match --
   -------------------------

   procedure Go_To_Closest_Match
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : Virtual_File;
      Project  : GNATCOLL.Projects.Project_Type;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type;
      Entity   : Root_Entity'Class) is
   begin
      Go_To_Closest_Match
        (Kernel, Filename, Project, Line, Column, Get_Name (Entity));
   end Go_To_Closest_Match;

   -------------------------
   -- Go_To_Closest_Match --
   -------------------------

   procedure Go_To_Closest_Match
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename    : GNATCOLL.VFS.Virtual_File;
      Project     : GNATCOLL.Projects.Project_Type;
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

      Open_File_Action_Hook.Run
         (Kernel, File => Filename, Project => Project,
          Line => Natural (Line), Column => Column,
          Column_End => Column + Visible_Column_Type (Length),
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
             (To_String (Get_Text
                (Source.Source_Buffer,
                 Line, Char_Column,
                 Line, Char_Column + Character_Offset_Type (Length))),
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
               Buffer := Get_Text (Source.Source_Buffer);
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

               Open_File_Action_Hook.Run
                 (Kernel, Filename, Project => Project,
                  Line => L, Column => Col, Column_End => Col_End,
                  Enable_Navigation => False);
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
      File_Status_Changed_Hook.Run
        (Get_Kernel (Box),
         File   => Get_Filename (Box.Source_Buffer),
         Status => Get_Status (Box.Source_Buffer));
   end Status_Changed_Handler;

   ------------------------------
   -- Filename_Changed_Handler --
   ------------------------------

   procedure Filename_Changed_Handler
     (Buffer : access Glib.Object.GObject_Record'Class;
      Params : Glib.Values.GValues;
      Box    : Source_Editor_Box)
   is
      pragma Unreferenced (Params);
      Buf    : constant Source_Buffer := Source_Buffer (Buffer);
      Child  : constant MDI_Child := Find_Child (Box.Kernel, Box);
      File   : constant Virtual_File := Buf.Get_Filename;
      P      : constant Project_Type := Get_Project (Box);
      Show_Project : constant Boolean :=
        P /= No_Project and then
        Get_Registry (Box.Kernel).Tree.Root_Project.Is_Aggregate_Project;
      P_Name : constant String :=
        (if Show_Project
         then " (" & P.Project_Path.Display_Base_Name & ')'
         else "");
      P_Full_Name   : constant String :=
        (if Show_Project
         then " - Project : " & P.Project_Path.Display_Full_Name
         else "");
   begin
      if Buf.Get_Title /= "" then
         Child.Set_Title
           (Buf.Get_Title & ASCII.LF & File.Display_Full_Name & P_Full_Name,
            Buf.Get_Title);
      elsif Is_Local (File) then
         Child.Set_Title
           (File.Display_Full_Name & P_Full_Name,
            File.Display_Base_Name & P_Name);
      else
         Child.Set_Title
           (File.Get_Host & ":|" & File.Display_Full_Name & P_Full_Name,
            File.Display_Base_Name & P_Name);
      end if;
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
   end On_Box_Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box         : access Source_Editor_Box_Record'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Filename    : GNATCOLL.VFS.Virtual_File;
      Source      : access Source_Buffer_Record'Class)
   is
      Lang_Autodetect : constant Boolean := True;

      Scrolling_Area : Tooltip_Scrolled_Window;
      Drawing_Area   : Gtk_Drawing_Area;
      Hbox           : Gtk_Hbox;
      Frame          : Gtk_Frame;
      Success        : Boolean;

   begin
      Event_Box.Initialize (Box);
      Gtk_New_Vbox (Box.Box, Homogeneous => False);
      Add (Box, Box.Box);

      Box.Kernel := Kernel;

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box.Box, Hbox, Expand => True, Fill => True);

      Gtk_New (Drawing_Area);
      Hbox.Pack_Start (Drawing_Area, Expand => False, Fill => False);
      Drawing_Area.Set_Size_Request (1, -1);

      Gtk_New (Scrolling_Area);
      Hbox.Pack_End (Scrolling_Area, Expand => True, Fill => True);

      --  load the buffer first, so that we can find out the relevant project
      --  automatically for the view.

      if Source /= null then
         Box.Source_Buffer := Source_Buffer (Source);
      else
         Gtk_New (Box.Source_Buffer, Kernel, Lang => null);

         if Filename.Is_Regular_File then
            Load_File (Box.Source_Buffer, Filename, Lang_Autodetect, Success);

            if Success then
               Box.Source_Buffer.Status_Changed;
            end if;

         else
            if Lang_Autodetect then
               Set_Language
                 (Box.Source_Buffer,
                  Get_Language_From_File
                    (Get_Language_Handler (Kernel), Filename));
            end if;

            Set_Initial_Dir (Box.Source_Buffer, Create (Filename.Dir_Name));
            Set_Charset (Box.Source_Buffer, Get_File_Charset (Filename));

            Box.Source_Buffer.Mark_Buffer_Writable (Writable => True);
            Load_Empty_File (Box.Source_Buffer);
         end if;

         Set_Filename (Box.Source_Buffer, Filename);
      end if;

      Gtk_New
        (Box.Source_View,
         Project,
         Scrolling_Area,
         Drawing_Area,
         Box.Source_Buffer, Kernel);
      Scrolling_Area.Add (Box.Source_View);

      --  The newly created buffer is now under the responsability of the
      --  view.
      if Source = null then
         Unref (Box.Source_Buffer);
      end if;

      --  Associate a tooltip handler to the newly created view

      declare
         Tooltip_Factory : constant Editor_Tooltip_Handler_Factory_Access :=
                  Src_Editor_Module.Get_Editor_Tooltip_Handler_Factory;
      begin
         Tooltip_Factory (Box).Associate_To_Widget (Box);
      end;

      --  The status bar, at the bottom of the window...

      Gtk_New (Box.Status_Bar, Gtk_Event_Box (Box),
               Box.Source_View, Box.Source_Buffer);

      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_None);
      Pack_Start (Box.Box, Frame, Expand => False, Fill => False);

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

      --  The Contextual Menu handling
      Setup_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Box.Source_View);

      --  We do not want to send a context_changed even here. It will be done
      --  later anyway, with more accurate information.
      Update_Status (Box.Status_Bar);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box         : access Source_Editor_Box_Record'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Filename    : GNATCOLL.VFS.Virtual_File) is
   begin
      Initialize (Box, Project, Kernel, Filename, Source => null);
   end Initialize;

   --------------
   -- Focus_In --
   --------------

   function Focus_In (Box : access GObject_Record'Class) return Boolean is
      B          : constant Source_Editor_Box := Source_Editor_Box (Box);
      Old_Status : constant Boolean := B.Source_Buffer.Get_Writable;

   begin
      Check_Writable (B);

      --  Connect the Undo/Redo buttons to the buffer
      if B.Source_Buffer.Get_Writable then
         Add_Controls (B.Source_Buffer);
      else
         Remove_Controls (B.Source_Buffer);
      end if;

      if Old_Status /= B.Source_Buffer.Get_Writable then
         Get_Kernel (B).Refresh_Context;
         --  Refresh context to update state of Undo/Redo actions when file
         --  permissions has been changed.
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
         Trace (Me, E);
         return False;
   end Focus_Out;

   -----------------------
   -- Has_Specification --
   -----------------------

   function Has_Specification
     (Context : GPS.Kernel.Selection_Context) return Boolean
   is
      Entity        : constant Root_Entity'Class := Get_Entity (Context);
      Spec_Location : General_Location;
      Body_Location : General_Location;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Integer := 0;
   begin
      if Entity = No_Root_Entity then
         return False;
      end if;

      if Has_File_Information (Context)
        and then Has_Line_Information (Context)
      then
         File := File_Information (Context);
         Line := GPS.Kernel.Contexts.Line_Information (Context);
         if Ada.Characters.Handling.To_Lower
           (Get_File_Language (Context)) /= "ada"
         then
            return True;
         end if;
      end if;

      Get_Entity_Locations (Context, Spec_Location, Body_Location);

      if Spec_Location = No_Location then
         return False;
      end if;

      if Spec_Location.File = File
        and then Spec_Location.Line = Line
      then
         --  we are on the same line so don't add "goto to ..."
         return False;
      end if;

      if Is_Subprogram (Entity) then
         return Spec_Location.File /= Body_Location.File
           or else Spec_Location.Line /= Body_Location.Line
           or else Spec_Location.Column /= Body_Location.Column;
      else
         return True;
      end if;
   end Has_Specification;

   --------------
   -- Has_Body --
   --------------

   function Has_Body (Context : GPS.Kernel.Selection_Context) return Boolean is
      Entity        : constant Root_Entity'Class := Get_Entity (Context);
      Spec_Location : General_Location;
      Body_Location : General_Location;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Integer := 0;
   begin
      if Entity = No_Root_Entity then
         return False;
      end if;

      if Has_File_Information (Context)
        and then Has_Line_Information (Context)
      then
         File := File_Information (Context);
         Line := GPS.Kernel.Contexts.Line_Information (Context);
         if Ada.Characters.Handling.To_Lower
           (Get_File_Language (Context)) /= "ada"
         then
            return True;
         end if;
      end if;

      Get_Entity_Locations (Context, Spec_Location, Body_Location);

      if Body_Location = No_Location then
         return False;
      end if;

      if Body_Location.File = File
        and then Body_Location.Line = Line
      then
         --  we are on the same line so don't add "goto to ..."
         return False;
      end if;

      if Is_Subprogram (Entity) then
         return True;
      else
         return Body_Location /= Spec_Location;
      end if;
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
         Trace (Me, E);
         return False;
   end Delete_Callback;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box         : out Source_Editor_Box;
      Project     : GNATCOLL.Projects.Project_Type;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Filename    : GNATCOLL.VFS.Virtual_File) is
   begin
      Box := new Source_Editor_Box_Record;
      Initialize (Box, Project, Kernel, Filename);
   end Gtk_New;

   ---------------------
   -- Create_New_View --
   ---------------------

   procedure Create_New_View
     (Box     : out Source_Editor_Box;
      Project : GNATCOLL.Projects.Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Source  : access Source_Editor_Box_Record)
   is
      Line : Editable_Line_Type;
      Col  : Character_Offset_Type;
   begin
      --  Capture the current position before we create a new view for the
      --  buffer, or we lost that information.
      Get_Cursor_Position (Source.Source_Buffer, Line, Col);

      Box := new Source_Editor_Box_Record;
      Initialize
        (Box, Project, Kernel_Handle (Kernel),
         Filename    => No_File,
         Source      => Source.Source_Buffer);

      if not Get_Writable (Box.Source_Buffer) then
         Set_Editable (Box.Source_View, False);
      end if;

      --  Preserve the current location
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

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Editor : access Source_Editor_Box_Record)
      return GNATCOLL.Projects.Project_Type is
   begin
      return Get_Project (Editor.Source_View);
   end Get_Project;

   --------------------
   -- Check_Writable --
   --------------------

   procedure Check_Writable (Editor : access Source_Editor_Box_Record) is
      Writable : Boolean;
   begin
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

      Editor.Source_Buffer.Mark_Buffer_Writable (Writable);
   end Check_Writable;

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

      Dialog        : GPS_Dialog;

   begin
      --  Do not authorize saving a read-only file, unless we save it to
      --  another disk file. We check on the disk, not the status of the file,
      --  to prevent hijacking files intentionally made read-only on the disk
      --  for instance by Clearcase.

      if File /= GNATCOLL.VFS.No_File
        and then not File.Is_Writable
        and then Filename = GNATCOLL.VFS.No_File
        and then not Force
      then
         --  No modal dialog in the testsuite
         if Active (Testsuite_Handle) then
            Editor.Kernel.Insert
              ("File is read-only on disk " & File.Display_Full_Name,
               Mode => Error);
            Success := False;
            return;
         end if;

         Gtk_New (Dialog,
                  Title => -"Overwrite read-only file ?",
                  Kernel => Editor.Kernel);
         Dialog.Add_Label
           (-"File is read-only on disk: " & ASCII.LF
            & File.Display_Full_Name & ASCII.LF
            & "Overwrite anyway ?");
         Dialog.Add_Button ("Overwrite", Gtk_Response_Yes);
         Dialog.Add_Button ("Do not save", Gtk_Response_No);

         Dialog.Show_All;
         if Dialog.Run = Gtk_Response_No then
            Dialog.Destroy;
            Success := False;
            return;
         end if;

         Dialog.Destroy;
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

            Buffer := Get_Text (Editor.Source_Buffer);
            Parse_Constructs (Ada_Lang,
                              Editor.Get_Filename, Buffer.all, Constructs);
            Free (Buffer);

            Info := Constructs.Last;

            if Info = null
              or else
                (Info.Info.Category /= Cat_Procedure
                 and then Info.Info.Category /= Cat_Function
                 and then Info.Info.Category /= Cat_Package)
              or else Info.Info.Name = No_Symbol
            then
               --  No unit name found
               New_Base_Name := new Filesystem_String'("");
            else
               --  Info.Name is a valid Ada unit name

               if Info.Info.Is_Declaration then
                  Part := Unit_Spec;
               else
                  Part := Unit_Body;
               end if;

               New_Base_Name := new Filesystem_String'
                 (Get_Project (Editor.Kernel).File_From_Unit
                    (Unit_Name       => To_Lower (Get (Info.Info.Name).all),
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
                 and then GPS_Message_Dialog
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

                  if Success
                    and then Editor.Source_View.Get_Project = No_Project
                  then
                     --  Project view was recomputed, we should now update the
                     --  project information for this editor.

                     Editor.Source_View.Set_Project;
                  end if;
               end if;
            end;

         else
            if not Force
              and then Check_Monitored_Files
                (Editor.Kernel, Interactive => True, Only_On_File => File)
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
           and then GPS_Message_Dialog
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

   -------------------------
   -- Set_Cursor_Location --
   -------------------------

   procedure Set_Cursor_Location
     (Editor                : access Source_Editor_Box_Record;
      Line                  : Editable_Line_Type;
      Column                : Character_Offset_Type := 1;
      Force_Focus           : Boolean := True;
      Raise_Child           : Boolean := False;
      Centering             : Centering_Type := Minimal;
      Extend_Selection      : Boolean := False;
      Synchronous_Scrolling : Boolean := True)
   is
      Editable_Line : Editable_Line_Type renames Line;

      procedure Raise_And_Focus;
      --  Raise and/or focus the editor, or do nothing, as requested by the
      --  Force_Focus and Raise_Child parameters.

      ---------------------
      -- Raise_And_Focus --
      ---------------------

      procedure Raise_And_Focus is
         C : constant MDI_Child := Find_MDI_Child_From_Widget (Editor);
      begin
         if C /= null then
            if Force_Focus then
               Set_Focus_Child (Get_MDI (Editor.Kernel), C);
               Grab_Toplevel_Focus (Get_MDI (Editor.Kernel), Editor,
                                    Present => True);
            end if;

            if Force_Focus or Raise_Child then
               Gtkada.MDI.Raise_Child (C);
            end if;
         end if;
      end Raise_And_Focus;

   begin
      End_Action (Editor.Source_Buffer);

      if Is_Valid_Position (Editor.Source_Buffer, Editable_Line, Column) then
         Raise_And_Focus;

         Set_Cursor_Position
           (Editor.Source_Buffer, Editable_Line, Column,
            Internal  => False,
            Extend_Selection => Extend_Selection);

         if Centering /= Minimal then
            Editor.Source_View.Set_Position_Set_Explicitely;
         end if;

         Save_Cursor_Position (Editor.Source_View);
         Scroll_To_Cursor_Location
           (View        => Editor.Source_View,
            Centering   => Centering,
            Synchronous => Synchronous_Scrolling);

      elsif Is_Valid_Position (Editor.Source_Buffer, Editable_Line) then
         --  We used to generate an error message (Invalid column number),
         --  but this was too intrusive: in the case of e.g. loading the
         --  desktop, if it often the case that the files have been modified
         --  and the column is no longer valid, so we silently ignore this.
         --  ??? Consider going to the last column instead of the first

         Raise_And_Focus;

         Set_Cursor_Position
           (Editor.Source_Buffer, Editable_Line, 1, False,
            Extend_Selection => Extend_Selection);

         if Centering /= Minimal then
            Editor.Source_View.Set_Position_Set_Explicitely;
         end if;

         Save_Cursor_Position (Editor.Source_View);
         Scroll_To_Cursor_Location
           (View        => Editor.Source_View,
            Centering   => Centering,
            Synchronous => Synchronous_Scrolling);

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
      Line   : Editable_Line_Type) return Natural
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
      Line   : Editable_Line_Type) return Natural
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
      Line   : Editable_Line_Type) return String
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
      Line   : Editable_Line_Type) return String
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
      Line   : Editable_Line_Type) return Natural
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
      Line   : Editable_Line_Type) return String
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

   ---------------------
   -- Get_Source_View --
   ---------------------

   function Get_Source_View
     (Editor : access Source_Editor_Box_Record)
      return Src_Editor_View.Source_View is
   begin
      return Editor.Source_View;
   end Get_Source_View;

   --------------------
   -- Get_Status_Bar --
   --------------------

   function Get_Status_Bar
     (Editor : access Source_Editor_Box_Record)
      return Source_Editor_Status_Bar is
   begin
      return Editor.Status_Bar;
   end Get_Status_Bar;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Editor   : access Source_Editor_Box_Record;
      Writable : Boolean)
   is
      File : constant GNATCOLL.VFS.Virtual_File :=
        Editor.Source_Buffer.Get_Filename;
   begin
      --  Check if the editor is associated to a file on the disk. If yes,
      --  change permissions on the disk as well.
      if File /= GNATCOLL.VFS.No_File then
         Editor.Kernel.Make_File_Writable (File, Writable);
      end if;

      Editor.Source_Buffer.Mark_Buffer_Writable (Writable);

      --  Let the world know that the status has changed (in particular that
      --  the undo/redo queue has changed).
      Editor.Kernel.Refresh_Context;
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
