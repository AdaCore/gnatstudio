-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;

with Gdk;
with Gdk.Color;  use Gdk.Color;
with Gdk.Event;  use Gdk.Event;
with Gtk.Enums;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gdk.Types;  use Gdk.Types;
with Gdk.Window; use Gdk.Window;

with Gtk;                       use Gtk;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;

with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;

with VCS;

with VCS_View_Pixmaps;          use VCS_View_Pixmaps;
with VCS_View_API;              use VCS_View_API;
with VCS_Module;                use VCS_Module;

with Log_Utils;                 use Log_Utils;

with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                use GPS.Intl;
with VFS;                       use VFS;
with Tooltips;

with Basic_Types;               use Basic_Types;
with Traces;                    use Traces;

with GUI_Utils;                 use GUI_Utils;
with File_Utils;                use File_Utils;
with Histories;                 use Histories;

package body VCS_View_Pkg is

   Me : constant Debug_Handle := Create ("VCS_INTERFACE");

   type VCS_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Page : VCS_Page_Access;
   end record;
   type VCS_Tooltips_Access is access all VCS_Tooltips'Class;
   procedure Draw
     (Tooltip : access VCS_Tooltips;
      Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
      Area   : out Gdk.Rectangle.Gdk_Rectangle);
   --  See inherited documentation

   --------------------
   -- Local packages --
   --------------------

   package Explorer_Selection_Foreach is
     new Selection_Foreach (VCS_View_Record);

   package Page_Status_Callback is new Gtk.Handlers.User_Callback
     (GObject_Record, Natural);

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.
   --  Base_Name_Column is the first column, because we want the built-in
   --  interactive search to be performed on the base name of the files.

   Base_Name_Column          : constant := 0;
   Name_Column               : constant := 1;
   Local_Rev_Column          : constant := 2;
   Rep_Rev_Column            : constant := 3;
   Status_Description_Column : constant := 4;
   Status_Pixbuf_Column      : constant := 5;
   Has_Log_Column            : constant := 6;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Base_Name_Column          => GType_String,
         Name_Column               => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type,
         Has_Log_Column            => GType_Boolean);
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Refresh (Explorer : VCS_View_Access);
   --  Redraw the files in the VCS Explorer.

   procedure Create_Model (VCS_View : access VCS_Page_Record'Class);
   --  Creates the underlying tree model for VCS_View.

   procedure Set_Column_Types (Explorer : access VCS_Page_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Fill_Info
     (Explorer      : access VCS_Page_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Line_Info     : Line_Record;
      Success       : out Boolean);
   --  Fills the tree info at the given Iter with values from
   --  Status_Record.
   --  Success tells whether the information has been filled or not.

   function Get_Iter_From_Name
     (Explorer : access VCS_Page_Record'Class;
      Name     : VFS.Virtual_File) return Gtk_Tree_Iter;
   --  Return the Iter associated with the given name.
   --  Name is an absolute file name.
   --  Return Null_Iter if no such iter was found.

   function Get_Page_For_Identifier
     (Explorer   : access VCS_View_Record'Class;
      Identifier : VCS_Access) return VCS_Page_Access;
   --  Return the page relative to Identifier. Create it if necessary.

   function Copy_Context
     (Context : Selection_Context_Access)
      return Selection_Context_Access;
   --  Copy the information in Context that are relevant to the explorer,
   --  and create a new context containing them.

   function Copy (X : Line_Record) return Line_Record;
   --  Return a deep copy of X.

   procedure On_Selected (Explorer : access Gtk_Widget_Record'Class);
   --  Give the focus to the current page tree.

   procedure Toggle_Show_Status
     (Explorer : access GObject_Record'Class;
      Index    : Natural);

   ---------------
   -- Callbacks --
   ---------------

   procedure Show_All_Status
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for activation of "Show all status"

   procedure Hide_All_Status
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for activation of "Show all status"

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event)
      return Boolean;
   --  Callback for the "button_press" event.

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event)
     return Boolean;
   --  Callback for the "delete_event" signal

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal, connected before.

   type File_Hook_Record is new Hook_Args_Record with record
      Explorer : VCS_View_Access;
   end record;
   type File_Hook is access all File_Hook_Record'Class;
   procedure Execute
     (Hook   : File_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      File_Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal.

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Default context factory.

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path;
   --  Return the path at which Event has occured.
   --  User must free memory associated to the returned path.

   function Get_Cached_Data
     (Page  : VCS_Page_Access;
      Index : VFS.Virtual_File) return Line_Record;
   --  Get the cached data.

   procedure Set_Cached_Data
     (Page  : VCS_Page_Access;
      Index : VFS.Virtual_File;
      Data  : Line_Record);
   --  Set the cached data.

   function To_History_Key (S : in String) return History_Key;
   --  Return history key corresponding to S.

   --------------------
   -- To_History_Key --
   --------------------

   function To_History_Key (S : in String) return History_Key is
      Result : History_Key (S'First .. S'Last);
   begin
      for J in S'Range loop
         if not Is_Alphanumeric (S (J)) then
            Result (J) := '_';
         else
            Result (J) := S (J);
         end if;
      end loop;

      return Result;
   end To_History_Key;

   ---------------------
   -- Get_Cached_Data --
   ---------------------

   function Get_Cached_Data
     (Page  : VCS_Page_Access;
      Index : VFS.Virtual_File) return Line_Record is
   begin
      return Copy (Status_Hash.Get (Page.Cached_Status, Index).Line);
   end Get_Cached_Data;

   ---------------------
   -- Set_Cached_Data --
   ---------------------

   procedure Set_Cached_Data
     (Page  : VCS_Page_Access;
      Index : VFS.Virtual_File;
      Data  : Line_Record) is
   begin
      Status_Hash.Set (Page.Cached_Status, Index, (Line => Copy (Data)));
   end Set_Cached_Data;

   -----------------
   -- On_Selected --
   -----------------

   procedure On_Selected (Explorer : access Gtk_Widget_Record'Class) is
      View : constant VCS_View_Access := VCS_View_Access (Explorer);
      Page : VCS_Page_Access;
   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (View.Notebook, Get_Current_Page (View.Notebook)));
      Grab_Focus (Page.Tree);
   end On_Selected;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access VCS_Tooltips;
      Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
      Area   : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Window : Gdk.Window.Gdk_Window;
      New_Window : Gdk_Window;
      Mask : Gdk_Modifier_Type;
      Width  : Glib.Gint := 0;
      Height : Glib.Gint := 0;

      X, Y      : Gint;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y    : Gint;
      Row_Found : Boolean;
      Iter      : Gtk_Tree_Iter;

      Text      : String_Access;
   begin
      Window := Get_Bin_Window (Tooltip.Page.Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);

      Get_Path_At_Pos
        (Tooltip.Page.Tree, X, Y, Path, Column, Cell_X, Cell_Y, Row_Found);

      if not Row_Found then
         return;
      end if;

      Get_Cell_Area (Tooltip.Page.Tree, Path, Column, Area);
      Iter := Get_Iter (Tooltip.Page.Model, Path);
      Path_Free (Path);

      if Column = Tooltip.Page.Status_Column then
         Text := new String'
           (-"Status: "
            & Get_String
              (Tooltip.Page.Model, Iter, Status_Description_Column));

      elsif Column = Tooltip.Page.Log_Column then
         if Get_Boolean (Tooltip.Page.Model, Iter, Has_Log_Column) then
            Text := new String'(-"A revision log exists for this file");
         else
            Text := new String'(-"No revision log exists for this file");
         end if;

      elsif Column = Tooltip.Page.File_Column then
         Text := new String'
           (Get_String (Tooltip.Page.Model, Iter, Name_Column));
      end if;

      if Text /= null then
         Create_Pixmap_From_Text
           (Text.all,
            Get_Pref (Tooltip.Page.Kernel, Default_Font),
            White (Get_Default_Colormap),
            Tooltip.Page.Tree,
            Pixmap,
            Width,
            Height);
         Free (Text);
      end if;
   end Draw;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Record) is
   begin
      Free (X.Status);
   end Free;

   ----------
   -- Copy --
   ----------

   function Copy (X : Line_Record) return Line_Record is
   begin
      return (Copy_File_Status (X.Status), X.Log);
   end Copy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : VCS_View_Access) is
      Page   : VCS_Page_Access;
   begin
      if Explorer /= null then
         for J in 1 .. Explorer.Number_Of_Pages loop
            Page := VCS_Page_Access
              (Get_Nth_Page (Explorer.Notebook, Gint (J - 1)));

            if Page.Shown then
               Scroll_To_Point (Page.Tree, 0, 0);
            end if;

            Clear (Page.Model);
            Status_Hash.Reset (Page.Stored_Status);
         end loop;
      end if;
   end Clear;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event, View);
   begin
      Hide_VCS_Explorer;
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Delete;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View     : access Gtk_Widget_Record'Class) is
      The_View : constant VCS_View_Access := VCS_View_Access (View);
      Page     : VCS_Page_Access;
   begin
      for J in 1 .. The_View.Number_Of_Pages loop
         Page := VCS_Page_Access
           (Get_Nth_Page (The_View.Notebook, Gint (J - 1)));

         Status_Hash.Reset (Page.Stored_Status);
         Status_Hash.Reset (Page.Cached_Status);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : VCS_View_Access) is
      use Status_Hash;

      L        : Iterator;
      Iter     : Gtk_Tree_Iter;
      Success  : Boolean;
      Page     : VCS_Page_Access;
      Sort_Col : Gint;
   begin
      Push_State (Explorer.Kernel, Busy);
      Page := VCS_Page_Access
        (Get_Nth_Page (Explorer.Notebook,
                       Get_Current_Page (Explorer.Notebook)));

      Scroll_To_Point (Page.Tree, 0, 0);

      Clear (Page.Model);
      Sort_Col := Freeze_Sort (Page.Model);

      Get_First (Page.Stored_Status, L);

      while Get_Element (L) /= No_Element loop
         for J in Page.Status'Range loop
            if Page.Status (J).Status =
              Get_Element (L).Line.Status.Status
            then
               if Page.Status (J).Display then
                  Append (Page.Model, Iter, Null_Iter);
                  Fill_Info (Page, Iter, Get_Element (L).Line, Success);

                  if not Success then
                     Remove (Page.Model, Iter);
                  end if;
               end if;

               exit;
            end if;
         end loop;

         Get_Next (Page.Stored_Status, L);
      end loop;

      Thaw_Sort (Page.Model, Sort_Col);
      Columns_Autosize (Page.Tree);
      Pop_State (Explorer.Kernel);
   end Refresh;

   -----------------
   -- Refresh_Log --
   -----------------

   procedure Refresh_Log
     (Explorer : access VCS_View_Record;
      File     : VFS.Virtual_File)
   is
      Page          : VCS_Page_Access;
      Log           : Boolean;
      Dummy         : Boolean;
      Iter          : Gtk_Tree_Iter;
      Line          : Line_Record;
   begin
      if Get_Log_From_File (Explorer.Kernel, File, False) = VFS.No_File then
         Log := False;
      else
         Log := True;
      end if;

      for J in 1 .. Explorer.Number_Of_Pages loop
         Page := VCS_Page_Access
           (Get_Nth_Page (Explorer.Notebook, Gint (J - 1)));

         --  Refresh the information in the cache.

         Line := Get_Cached_Data (Page, File);

         if Line /= No_Data then
            Set_Cached_Data (Page, File, (Line.Status, Log));
            Iter := Get_Iter_From_Name (Page, File);

            if Iter /= Null_Iter then
               Fill_Info (Page, Iter, (Line.Status, Log), Dummy);
            end if;
         end if;

         Free (Line);
      end loop;
   end Refresh_Log;

   -------------------------
   -- Display_File_Status --
   -------------------------

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True)
   is
      Explorer      : constant VCS_View_Access :=
                        Get_Explorer (Kernel, False, False);
      Status_Temp   : File_Status_List.List_Node
        := File_Status_List.First (Status);
      Found         : Boolean := False;
      Page          : VCS_Page_Access;
      Log           : Boolean;
      Line          : Line_Record;
      Sort_Id       : Gint;
      Cache_Empty   : Boolean;
      Iter          : Status_Hash.Iterator;

      use Status_Hash;

      Up_To_Date_Status : File_Status;
      Registered_Status : constant Status_Array :=
                            Get_Registered_Status (VCS_Identifier);

      use type File_Status_List.List_Node;
   begin
      if Registered_Status'Length >= 2 then
         Up_To_Date_Status := Registered_Status (Registered_Status'First + 1);
      end if;

      --  Free the logs associated to the files that are up-to-date, and
      --  update the vcs label in the editors.

      while Status_Temp /= File_Status_List.Null_Node loop
         declare
            S      : constant File_Status_Record :=
                       File_Status_List.Data (Status_Temp);
            File   : constant Virtual_File := S.File;
         begin
            --  Clear the logs

            if Clear_Logs
              and then S.Status = Up_To_Date_Status
            then
               declare
                  Log   : constant Virtual_File :=
                            Get_Log_From_File (Kernel, File, False);
               begin
                  if Log /= VFS.No_File
                    and then Is_Regular_File (Log)
                  then
                     Delete (Log);
                     Close_File_Editors (Kernel, Log);
                  end if;

                  Remove_File_From_Mapping (Kernel, File);
               end;
            end if;

            --  Display the editor status

            if Is_Open (Kernel, File) then
               Display_Editor_Status (Kernel, VCS_Identifier, S);
            end if;

         exception
            when E : others =>
               Trace (Exception_Handle, "Unexpected exception: "
                      & Exception_Information (E));
         end;

         Status_Temp := File_Status_List.Next (Status_Temp);
      end loop;

      Status_Temp := File_Status_List.First (Status);

      if Explorer = null then
         return;
      end if;

      Page := Get_Page_For_Identifier (Explorer, VCS_Identifier);
      Status_Hash.Get_First (Page.Cached_Status, Iter);
      Cache_Empty := Get_Element (Iter) = No_Element;

      Set_Current_Page (Explorer.Notebook,
                        Page_Num (Explorer.Notebook, Page));

      Push_State (Kernel, Busy);
      Sort_Id := Freeze_Sort (Page.Model);

      while Status_Temp /= File_Status_List.Null_Node loop
         declare
            File : constant Virtual_File :=
                     File_Status_List.Data (Status_Temp).File;
         begin
            Line := Get_Cached_Data (Page, File);

            if Line = No_Data
              or else Override_Cache
            then
               Log := Get_Log_From_File (Kernel, File, False) /= VFS.No_File;

               Line :=
                 (Copy_File_Status (File_Status_List.Data (Status_Temp)), Log);
               Set_Cached_Data (Page, File, Line);
            end if;
         end;

         --  The info that we want to display is now in Line,
         --  if it already exists in Page.Stored_Status, we simply modify
         --  the element, otherwise we add it to the list.

         if Display
           or else Cache_Empty
         then
            declare
               New_Status         : constant Line_Record := Copy (Line);
               New_File           : constant Virtual_File :=
                                      New_Status.Status.File;
               Temp_Stored_Status : Iterator;
               E                  : Element;
               Iter               : Gtk_Tree_Iter := Null_Iter;
               Success            : Boolean;
            begin
               Get_First (Page.Stored_Status, Temp_Stored_Status);
               Found := False;

               if Get_Element (Temp_Stored_Status) = No_Element then
                  Set (Page.Stored_Status, New_File, (Line => New_Status));
               else
                  E := Get (Page.Stored_Status, New_File);

                  if E /= No_Element then
                     Found := True;
                     Set (Page.Stored_Status, New_File, (Line => New_Status));
                     Iter := Get_Iter_From_Name
                       (Page, New_Status.Status.File);
                  end if;

                  if not Found
                    and then Force_Display
                  then
                     Set (Page.Stored_Status, New_File, (Line => New_Status));
                  end if;
               end if;

               --  Append the iter only if the status is currently shown.

               for J in Page.Status'Range loop
                  if Page.Status (J).Status = New_Status.Status.Status then
                     --  Do not append the file if it is up-to-date but the
                     --  file does not exist.

                     if Page.Status (J).Display
                       and then not
                         (Page.Status (J).Status = Up_To_Date_Status
                          and then not Is_Regular_File
                            (New_Status.Status.File))
                     then
                        if Iter = Null_Iter
                          and then Force_Display
                        then
                           Append (Page.Model, Iter, Null_Iter);
                        end if;

                        if Iter /= Null_Iter then
                           Fill_Info (Page, Iter, New_Status, Success);
                        end if;
                     else
                        if Iter /= Null_Iter then
                           Remove (Page.Model, Iter);
                        end if;
                     end if;

                     exit;
                  end if;
               end loop;
            end;
         end if;

         Free (Line);

         Status_Temp := File_Status_List.Next (Status_Temp);
      end loop;

      Thaw_Sort (Page.Model, Sort_Id);
      Pop_State (Kernel);
   end Display_File_Status;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer      : access VCS_Page_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Line_Info     : Line_Record;
      Success       : out Boolean)
   is
      Pixbuf : Gdk_Pixbuf;
   begin
      Success := True;

      if Line_Info.Status.File = No_File
        or else VFS.Is_Directory (Line_Info.Status.File)
      then
         Success := False;
         return;
      end if;

      Set (Explorer.Model, Iter, Has_Log_Column, Line_Info.Log);
      Set (Explorer.Model, Iter, Name_Column,
           Full_Name (Line_Info.Status.File, True).all);

      Set (Explorer.Model, Iter, Base_Name_Column,
           Base_Name (Line_Info.Status.File));

      if not String_List.Is_Empty (Line_Info.Status.Working_Revision) then
         Set (Explorer.Model, Iter, Local_Rev_Column,
              String_List.Head (Line_Info.Status.Working_Revision));
      else
         Set (Explorer.Model, Iter, Local_Rev_Column, -"n/a");
      end if;

      if not String_List.Is_Empty (Line_Info.Status.Repository_Revision) then
         Set (Explorer.Model, Iter, Rep_Rev_Column,
              String_List.Head (Line_Info.Status.Repository_Revision));
      else
         Set (Explorer.Model, Iter, Rep_Rev_Column, -"n/a");
      end if;

      if Line_Info.Status.Status.Stock_Id /= null then
         Pixbuf := Render_Icon
           (Explorer,
            Line_Info.Status.Status.Stock_Id.all,
            Gtk.Enums.Icon_Size_Small_Toolbar);
      end if;

      if Pixbuf /= null then
         Set (Explorer.Model, Iter, Status_Pixbuf_Column, C_Proxy (Pixbuf));
      else
         Pixbuf := Render_Icon
           (Explorer,
            VCS.Unknown.Stock_Id.all,
            Gtk.Enums.Icon_Size_Menu);

         Set (Explorer.Model, Iter, Status_Pixbuf_Column, C_Proxy (Pixbuf));
      end if;

      if Line_Info.Status.Status.Label /= null then
         Set (Explorer.Model, Iter, Status_Description_Column,
              Line_Info.Status.Status.Label.all);
      else
         Set (Explorer.Model, Iter, Status_Description_Column,
              VCS.Unknown.Label.all);
      end if;
   end Fill_Info;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access VCS_Page_Record'Class;
      Name     : VFS.Virtual_File) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Get_Iter_First (Explorer.Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Explorer.Model, Iter, Name_Column) =
           Full_Name (Name, True).all
         then
            return Iter;
         end if;

         Next (Explorer.Model, Iter);
      end loop;

      return Null_Iter;
   end Get_Iter_From_Name;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : VCS_View_Access) return String_List.List
   is
      Result : String_List.List;
      Page   : VCS_Page_Access;

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access);
      --  Add an item to Result.

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access)
      is
         pragma Unreferenced (Model, Path, Data);
      begin
         String_List.Append
           (Result, Get_String (Page.Model, Iter, Name_Column));
      end Add_Selected_Item;

   begin
      if Explorer = null then
         return Result;
      end if;

      Page := VCS_Page_Access (Get_Nth_Page (Explorer.Notebook,
                               Get_Current_Page (Explorer.Notebook)));

      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Page.Tree),
         Add_Selected_Item'Unrestricted_Access,
         Explorer_Selection_Foreach.Data_Type_Access (Explorer));
      return Result;
   end Get_Selected_Files;

   ------------------
   -- Push_Message --
   ------------------

   procedure Push_Message
     (Kernel   : Kernel_Handle;
      M_Type   : Message_Type;
      Message  : String) is
   begin
      Console.Insert (Kernel, Message, Mode => M_Type);
   end Push_Message;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_Page_Record'Class) is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Toggle_Rend : Gtk_Cell_Renderer_Toggle;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);
      Gtk_New (Toggle_Rend);

      Set_Rules_Hint (Explorer.Tree, True);

      Gtk_New (Explorer.Status_Column);
      Set_Title (Explorer.Status_Column, -"Status");
      Pack_Start (Explorer.Status_Column, Pixbuf_Rend, False);
      Add_Attribute
        (Explorer.Status_Column, Pixbuf_Rend, "pixbuf", Status_Pixbuf_Column);
      Set_Clickable (Explorer.Status_Column, True);
      Set_Sort_Column_Id (Explorer.Status_Column, Status_Description_Column);
      Dummy := Append_Column (Explorer.Tree, Explorer.Status_Column);

      Gtk_New (Explorer.Log_Column);
      Set_Title (Explorer.Log_Column, -"Log");
      Pack_Start (Explorer.Log_Column, Toggle_Rend, False);
      Add_Attribute
        (Explorer.Log_Column, Toggle_Rend, "active", Has_Log_Column);
      Set_Clickable (Explorer.Log_Column, False);
      Dummy := Append_Column (Explorer.Tree, Explorer.Log_Column);

      Gtk_New (Explorer.File_Column);
      Set_Title (Explorer.File_Column, -"File name");
      Pack_Start (Explorer.File_Column, Text_Rend, True);
      Add_Attribute
        (Explorer.File_Column, Text_Rend, "text", Base_Name_Column);
      Set_Clickable (Explorer.File_Column, True);
      Set_Sort_Column_Id (Explorer.File_Column, Base_Name_Column);
      Set_Resizable (Explorer.File_Column, True);
      Dummy := Append_Column (Explorer.Tree, Explorer.File_Column);

      Gtk_New (Col);
      Set_Title (Col, -"Working rev.");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Local_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Head rev.");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Rep_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);
   end Set_Column_Types;

   -------------------
   --  Create_Model --
   -------------------

   procedure Create_Model (VCS_View : access VCS_Page_Record'Class) is
   begin
      Gtk_New (VCS_View.Model, Columns_Types);
   end Create_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (VCS_View : out VCS_View_Access;
      Kernel   : Kernel_Handle := null) is
   begin
      VCS_View := new VCS_View_Record;
      VCS_View_Pkg.Initialize (VCS_View, Kernel);
   end Gtk_New;

   ---------------------
   -- Show_All_Status --
   ---------------------

   procedure Show_All_Status
     (Explorer : access Gtk_Widget_Record'Class)
   is
      E    : constant VCS_View_Access := VCS_View_Access (Explorer);
      Page : VCS_Page_Access;

   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (E.Notebook, Get_Current_Page (E.Notebook)));

      for J in Page.Status'Range loop
         Page.Status (J).Display := True;
         Set_History
           (Get_History (E.Kernel).all,
            To_History_Key (Page.Status (J).Status.Label.all),
            True);
      end loop;

      Refresh (E);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Show_All_Status;

   ---------------------
   -- Hide_All_Status --
   ---------------------

   procedure Hide_All_Status
     (Explorer : access Gtk_Widget_Record'Class)
   is
      E    : constant VCS_View_Access := VCS_View_Access (Explorer);
      Page : VCS_Page_Access;
   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (E.Notebook, Get_Current_Page (E.Notebook)));

      for J in Page.Status'Range loop
         Page.Status (J).Display := False;
         Set_History
           (Get_History (E.Kernel).all,
            To_History_Key (Page.Status (J).Status.Label.all),
            False);
      end loop;

      Refresh (E);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Hide_All_Status;

   ------------------------
   -- Toggle_Show_Status --
   ------------------------

   procedure Toggle_Show_Status
     (Explorer : access GObject_Record'Class;
      Index    : Natural)
   is
      E    : constant VCS_View_Access := VCS_View_Access (Explorer);
      Page : VCS_Page_Access;
   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (E.Notebook, Get_Current_Page (E.Notebook)));

      if Index in Page.Status'Range then
         Page.Status (Index).Display := not Page.Status (Index).Display;
      end if;

      Refresh (E);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Toggle_Show_Status;

   -----------------------
   -- Get_Path_At_Event --
   -----------------------

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path
   is
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;

   begin
      Path := Gtk_New;
      Get_Path_At_Pos
        (Tree,
         Gint (X),
         Gint (Y),
         Path,
         Column,
         Buffer_X,
         Buffer_Y,
         Row_Found);

      return Path;
   end Get_Path_At_Event;

   ------------------
   -- Context_Func --
   ------------------

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access

   is
      pragma Unreferenced (Event_Widget);

      Check    : Gtk_Check_Menu_Item;
      Mitem    : Gtk_Menu_Item;
      Context  : File_Selection_Context_Access;

      Submenu  : Gtk_Menu;

      Files    : String_List.List;
      Explorer : constant VCS_View_Access := VCS_View_Access (Object);
      Page     : VCS_Page_Access;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (Explorer.Notebook,
                       Get_Current_Page (Explorer.Notebook)));

      --  If there is no selection, select the item under the cursor.

      Path := Get_Path_At_Event (Page.Tree, Event);

      if Path /= null
        and then not Path_Is_Selected (Get_Selection (Page.Tree), Path)
      then
         Unselect_All (Get_Selection (Page.Tree));
         Select_Path (Get_Selection (Page.Tree), Path);

         Iter := Get_Iter (Page.Model, Path);
         Path_Free (Path);

         String_List.Append
           (Files, Get_String (Page.Model, Iter, Name_Column));
      else
         Files := Get_Selected_Files (Explorer);
      end if;

      --  Create the context

      if not String_List.Is_Empty (Files) then
         declare
            First_File : constant Virtual_File := Create
              (Full_Filename => String_List.Head (Files));
         begin
            Context := new File_Selection_Context;

            Set_Context_Information
              (Context,
               Kernel,
               VCS_Module_ID);
            Set_File_Information (Context, File => First_File);
            Set_Current_Context (Explorer, Selection_Context_Access (Context));
            VCS_Contextual_Menu
              (Kernel_Handle (Kernel), Explorer.Context, Menu, False);

            Gtk_New (Mitem);
            Append (Menu, Mitem);
         end;
      end if;

      String_List.Free (Files);

      if Context = null then
         Submenu := Menu;
      else
         Gtk_New (Mitem, -"Filters");
         Append (Menu, Mitem);

         Gtk_New (Submenu);
         Set_Submenu (Mitem, Submenu);
      end if;

      Gtk_New (Mitem, -"Show all status");
      Append (Submenu, Mitem);
      Widget_Callback.Object_Connect
         (Mitem, "activate", Show_All_Status'Access, Explorer);

      Gtk_New (Mitem, -"Hide all status");
      Append (Submenu, Mitem);
      Widget_Callback.Object_Connect
         (Mitem, "activate", Hide_All_Status'Access, Explorer);

      Gtk_New (Mitem);
      Append (Submenu, Mitem);

      for J in Page.Status'Range loop
         Gtk_New (Check, Label => -"Show " & Page.Status (J).Status.Label.all);
         Set_Active (Check, Page.Status (J).Display);
         Append (Submenu, Check);
         Page_Status_Callback.Object_Connect
           (Check, "activate", Toggle_Show_Status'Access, Explorer, J);
         Associate
           (Get_History (Kernel).all,
            To_History_Key (Page.Status (J).Status.Label.all),
            Check);
      end loop;

      if Context /= null then
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;

      return Selection_Context_Access (Context);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return null;
   end Context_Func;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Page     : VCS_Page_Access;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (Explorer.Notebook,
                       Get_Current_Page (Explorer.Notebook)));

      if Get_Event_Type (Event) = Gdk_2button_Press then
         Path := Get_Path_At_Event (Page.Tree, Event);

         if Path /= null then
            Iter := Get_Iter (Page.Model, Path);
            Open_File_Editor
              (Kernel,
               Create
                 (Full_Filename =>
                    Get_String (Page.Model, Iter, Name_Column)));
            Emit_Stop_By_Name (Page.Tree, "button_press_event");
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook      : File_Hook_Record;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Data : access Hooks_Data'Class)
   is
      D        : constant File_Hooks_Args := File_Hooks_Args (File_Data.all);
      Log_Name : constant String := Full_Name (D.File).all;
      Line     : Line_Record;
   begin
      if Log_Name'Length > 4
        and then Log_Name (Log_Name'Last - 3 .. Log_Name'Last) = "$log"
      then
         declare
            File        : constant Virtual_File :=
              Get_File_From_Log (Kernel, D.File);
            Page        : VCS_Page_Access;
            use Status_Hash;
            E           : Element;
         begin
            Browse_Files :
            for J in 1 .. Hook.Explorer.Number_Of_Pages loop
               Page := VCS_Page_Access
                 (Get_Nth_Page (Hook.Explorer.Notebook, Gint (J - 1)));

               Line := Get_Cached_Data (Page, File);

               if Line /= No_Data then
                  Set_Cached_Data (Page, File, (Line.Status, True));
               end if;

               E := Get (Page.Stored_Status, File);

               if E /= No_Element then
                  Set (Page.Stored_Status, File,
                       (Line => (Copy_File_Status (Line.Status),
                                 True)));
                  Refresh (Hook.Explorer);
                  return;
               end if;
            end loop Browse_Files;
         end;
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (VCS_View : access VCS_View_Record'Class;
      Kernel   : Kernel_Handle)
   is
      Vbox1 : Gtk_Vbox;
      Hook  : File_Hook;
      Page  : VCS_Page_Access;
      pragma Unreferenced (Page);

   begin
      Init_Graphics;
      Initialize_Hbox (VCS_View);

      VCS_View.Kernel := Kernel;
      Gtk_New_Vbox (Vbox1, False, 0);
      Pack_Start (VCS_View, Vbox1);

      Gtk_New (VCS_View.Notebook);
      Set_Show_Tabs (VCS_View.Notebook, False);
      Set_Show_Border (VCS_View.Notebook, False);
      Pack_Start (Vbox1, VCS_View.Notebook);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (VCS_View, "delete_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Delete'Access),
         VCS_View, After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (VCS_View, "destroy", On_Destroy'Access, VCS_View);

      declare
         VCS_List : constant GNAT.OS_Lib.Argument_List :=
           Get_VCS_List (VCS_Module_ID);
      begin
         for J in VCS_List'Range loop
            Page := Get_Page_For_Identifier
              (VCS_View, Get_VCS_From_Id (VCS_List (J).all));
         end loop;
      end;

      Hook := new File_Hook_Record'
        (Hook_Args_Record with Explorer => VCS_View_Access (VCS_View));
      Add_Hook (Kernel, File_Edited_Hook, Hook, Watch => GObject (VCS_View));

      --  Can't do this through the Focus_Widget parameter to Gtkada.MDI.Put,
      --  since the focus child is dynamic.
      Widget_Callback.Connect
        (VCS_View, "grab_focus", On_Selected'Access, After => True);
   end Initialize;

   -----------------------------
   -- Get_Page_For_Identifier --
   -----------------------------

   function Get_Page_For_Identifier
     (Explorer   : access VCS_View_Record'Class;
      Identifier : VCS_Access) return VCS_Page_Access
   is
      Page            : VCS_Page_Access;
      Selection       : Gtk_Tree_Selection;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Label           : Gtk_Label;
      Tooltip         : VCS_Tooltips_Access;

   begin
      for J in 1 .. Explorer.Number_Of_Pages loop
         Page := VCS_Page_Access
           (Get_Nth_Page (Explorer.Notebook, Gint (J - 1)));

         if Page.Reference = Identifier then
            return Page;
         end if;
      end loop;

      --  If this point is reached, that means that no page containing
      --  Identifier could be found, therefore we create it.

      Explorer.Number_Of_Pages := Explorer.Number_Of_Pages + 1;

      Page := new VCS_Page_Record;
      Initialize_Hbox (Page);

      Page.Kernel    := Explorer.Kernel;
      Page.Reference := Identifier;

      Create_Model (Page);

      Gtk_New (Page.Tree, Page.Model);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Automatic);
      Pack_Start (Page, Scrolledwindow1, True, True, 0);

      Selection := Get_Selection (Page.Tree);
      Set_Mode (Selection, Gtk.Enums.Selection_Multiple);
      Add (Scrolledwindow1, Page.Tree);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Page.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Explorer,
         After => False);

      Register_Contextual_Menu
        (Explorer.Kernel,
         Page.Tree,
         Explorer,
         VCS_Module_ID,
         Context_Func'Access);

      Set_Column_Types (Page);

      Gtk_New (Label, Name (Identifier));

      Append_Page (Explorer.Notebook, Page, Label);

      Tooltip := new VCS_Tooltips;
      Tooltip.Page := Page;
      Set_Tooltip (Tooltip, Page.Tree);

      declare
         Status : constant Status_Array := Get_Registered_Status (Identifier);
      begin
         Page.Status := new Page_Status_Array (Status'Range);

         for J in Page.Status'Range loop
            Page.Status (J).Status  := Status (J);

            declare
               Key : constant History_Key :=
                       To_History_Key (Page.Status (J).Status.Label.all);
            begin
               Create_New_Boolean_Key_If_Necessary
                 (Get_History (Explorer.Kernel).all, Key, True);
               Page.Status (J).Display := Get_History
                 (Get_History (Explorer.Kernel).all, Key);
            end;
         end loop;
      end;
      --  Emit a "clicked" signal on the file column to sort it.

      Clicked (Page.File_Column);

      return Page;
   end Get_Page_For_Identifier;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Explorer : access VCS_View_Record) return VCS_Access
   is
      Page : VCS_Page_Access;
   begin
      Page := VCS_Page_Access
        (Get_Nth_Page
          (Explorer.Notebook, Get_Current_Page (Explorer.Notebook)));

      if Page /= null then
         return Page.Reference;
      else
         return Get_VCS_From_Id ("");
      end if;
   end Get_Current_Ref;

   ------------------
   -- Copy_Context --
   ------------------

   function Copy_Context
     (Context : Selection_Context_Access)
      return Selection_Context_Access
   is
      Result : Selection_Context_Access;
      File   : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         Result := new File_Selection_Context;
         File := File_Selection_Context_Access (Context);

         Set_Context_Information
           (Result,
            Get_Kernel (Context),
            Get_Creator (Context));

         Set_File_Information
           (File_Selection_Context_Access (Result),
            File_Information (File),
            Project_Information (File));

         return Result;

      else
         return null;
      end if;
   end Copy_Context;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Explorer : access VCS_View_Record)
      return Selection_Context_Access is
   begin
      if Explorer.Context = null then
         declare
            Context : File_Selection_Context_Access;
            Files   : String_List.List;
            First   : VFS.Virtual_File;
         begin
            Files := Get_Selected_Files (VCS_View_Access (Explorer));

            if not String_List.Is_Empty (Files) then
               Context := new File_Selection_Context;
               First := Create (String_List.Head (Files));

               Set_Context_Information
                 (Context, Explorer.Kernel, VCS_Module_ID);
               Set_File_Information (Context, File => First);

               Set_Current_Context
                 (Explorer, Selection_Context_Access (Context));

               String_List.Free (Files);
               return Selection_Context_Access (Context);
            else
               return null;
            end if;
         end;
      else
         return Copy_Context (Explorer.Context);
      end if;
   end Get_Current_Context;

   -------------------------
   -- Set_Current_Context --
   -------------------------

   procedure Set_Current_Context
     (Explorer : access VCS_View_Record;
      Context  : Selection_Context_Access) is
   begin
      if Explorer.Context /= null then
         Unref (Explorer.Context);
         Explorer.Context := null;
      end if;

      Explorer.Context := Copy_Context (Context);
   end Set_Current_Context;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List
   is
      Result         : String_List.List;
      Focused_Child  : constant MDI_Child :=
        Get_Focus_Child (Get_MDI (Kernel));
      Explorer_Child : constant MDI_Child :=
        Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);

   begin
      if Explorer_Child = Focused_Child
        and then Explorer_Child /= null
      then
         Result := Get_Selected_Files
           (VCS_View_Access (Get_Widget (Explorer_Child)));
      else
         declare
            File : constant VFS.Virtual_File :=
              Get_Current_File (Get_Current_Context (Kernel));
         begin
            if File /= VFS.No_File then
               String_List.Append (Result, Full_Name (File).all);
            end if;
         end;
      end if;

      return Result;
   end Get_Selected_Files;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Context : Selection_Context_Access) return String
   is
      File : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File) then
            Trace (Me, "Directory= " & Directory_Information (File));
            return Directory_Information (File);
         else
            Trace (Me, "No directory");
         end if;

      elsif Context = null then
         Trace (Me, "null context");
      else
         Trace (Me, "Invalid context");
      end if;

      return Get_Current_Dir;
   end Get_Current_Dir;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Context : Selection_Context_Access) return Virtual_File
   is
      File : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         return File_Information (File);
      end if;

      return VFS.No_File;
   end Get_Current_File;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (VCS_View : access VCS_View_Record) return GPS.Kernel.Kernel_Handle is
   begin
      return VCS_View.Kernel;
   end Get_Kernel;

   -----------------------
   -- Get_Cached_Status --
   -----------------------

   function Get_Cached_Status
     (Explorer : access VCS_View_Record;
      File     : VFS.Virtual_File;
      Ref      : VCS_Access) return File_Status_Record
   is
      Page : constant VCS_Page_Access :=
               Get_Page_For_Identifier (Explorer, Ref);
   begin
      return Get_Cached_Data (Page, File).Status;
   end Get_Cached_Status;


   ----------
   -- Hash --
   ----------

   function Hash is new HTables.Hash (Header_Num);

   function Hash (F : Virtual_File) return Header_Num is
   begin
      if Filenames_Are_Case_Sensitive then
         return Hash (Full_Name (F).all);
      else
         return Hash (To_Lower (Full_Name (F).all));
      end if;
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Virtual_File) return Boolean is
   begin
      return F1 = F2;
   end Equal;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Element) is
   begin
      Free (X.Line);
   end Free;

end VCS_View_Pkg;
