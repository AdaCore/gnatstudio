-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;

with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;

with Gtkada.Handlers;           use Gtkada.Handlers;

with Basic_Types;               use Basic_Types;
with Filesystems;               use Filesystems;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GUI_Utils;                 use GUI_Utils;
with Log_Utils;                 use Log_Utils;
with VCS;                       use VCS;
with VCS_Module;                use VCS_Module;
with VCS_View_Pixmaps;          use VCS_View_Pixmaps;
with Traces;                    use Traces;
with GNAT.Strings;

package body VCS_View is

   use type GNAT.Strings.String_Access;

   package Explorer_Selection_Foreach is
     new Selection_Foreach (VCS_View_Access);

   function Copy_Context
     (Context : Selection_Context) return Selection_Context;
   --  Copy the information in Context that are relevant to the explorer,
   --  and create a new context containing them.

   --------------
   -- File_Key --
   --------------

   function File_Key (File : GNATCOLL.VFS.Virtual_File) return String is
   begin
      if Is_Directory (File) then
         --  In case of a directory we remove the last directory separator.
         --  What we want here is to be able to have the path to the directory
         --  to handle.
         declare
            Dir : constant String := Full_Name (File, True).all;
         begin
            return Dir (Dir'First .. Dir'Last - 1);
         end;
      else
         return Full_Name (File, True).all;
      end if;
   end File_Key;

   ---------------------
   -- For_Every_Nodes --
   ---------------------

   procedure For_Every_Nodes
     (Explorer  : access VCS_View_Record'Class;
      Root_Only : Boolean)
   is
      procedure Iterate (Iter : Gtk_Tree_Iter; Root : Boolean);
      --  Parse recursively the tree model

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Iter : Gtk_Tree_Iter; Root : Boolean) is
         Quit : Boolean := False;
         I, K : Gtk_Tree_Iter;
      begin
         Iter_Copy (Iter, I);

         while not Quit and then I /= Null_Iter loop
            if Has_Child (Explorer.Model, I) and then not Root_Only then
               Iterate (Children (Explorer.Model, I), Root => False);
            end if;

            Iter_Copy (I, K);
            Next (Explorer.Model, I);
            --  We copy and compute the next iterator now to support Action
            --  that deletes nodes.

            Action (K, Root, Quit);
         end loop;
      end Iterate;

   begin
      Iterate (Get_Iter_First (Explorer.Model), Root => True);
   end For_Every_Nodes;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access VCS_View_Record'Class;
      Kernel   : Kernel_Handle)
   is
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Tooltip         : VCS_Tooltips_Access;
      Selection       : Gtk_Tree_Selection;

   begin
      Init_Graphics;
      Initialize_Hbox (Explorer);

      Explorer.Kernel := Kernel;

      Create_Model (Explorer);

      Gtk_New (Explorer.Tree, Explorer.Model);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Automatic);
      Pack_Start (Explorer, Scrolledwindow1, True, True, 0);

      Selection := Get_Selection (Explorer.Tree);
      Set_Mode (Selection, Gtk.Enums.Selection_Multiple);
      Add (Scrolledwindow1, Explorer.Tree);

      Do_Initialize (Explorer, Kernel);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Explorer,
         After => False);

      Tooltip := new VCS_Tooltips;
      Tooltip.Explorer := VCS_View.VCS_View_Access (Explorer);
      Set_Tooltip (Tooltip, Explorer.Tree);

      --  Emit a "clicked" signal on the file column to sort it

      Clicked (Explorer.File_Column);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer, Signal_Delete_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Delete'Access),
         Explorer, After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Explorer, Signal_Destroy, On_Destroy'Access, Explorer);

      --  Can't do this through the Focus_Widget parameter to Gtkada.MDI.Put,
      --  since the focus child is dynamic.
      Widget_Callback.Connect
        (Explorer, Signal_Grab_Focus, On_Selected'Access, After => True);
   end Initialize;

   -------------------
   --  Create_Model --
   -------------------

   procedure Create_Model
     (Explorer : access VCS_View_Record'Class) is
   begin
      Gtk_New (Explorer.Model, Columns_Types (Explorer));
   end Create_Model;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Iter := Find_Iter_For_Event
           (Explorer.Tree, Get_Model (Explorer.Tree), Event);

         if Iter /= Null_Iter then
            Path := Get_Path (Get_Model (Explorer.Tree), Iter);

            if Get_Depth (Path) = 2 then
               Iter := Get_Iter (Explorer.Model, Path);
               Open_File_Editor
                 (Kernel,
                  Create
                    (Full_Filename =>
                       Get_String (Explorer.Model, Iter, Name_Column)),
                  Line   => 0,
                  Column => 0);
               Emit_Stop_By_Name (Explorer.Tree, "button_press_event");
            end if;

            Path_Free (Path);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Tooltip : access VCS_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Window     : Gdk.Window.Gdk_Window;
      New_Window : Gdk_Window;
      Mask       : Gdk_Modifier_Type;

      X, Y       : Gint;
      Path       : Gtk_Tree_Path;
      Column     : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y     : Gint;
      Row_Found  : Boolean;
      Iter       : Gtk_Tree_Iter;

      Text       : GNAT.Strings.String_Access;

   begin
      if Tooltip.Explorer.Tree = null then
         return;
      end if;

      Window := Get_Bin_Window (Tooltip.Explorer.Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);

      Get_Path_At_Pos
        (Tooltip.Explorer.Tree, X, Y, Path, Column, Cell_X, Cell_Y, Row_Found);

      if not Row_Found then
         return;
      end if;

      Get_Cell_Area (Tooltip.Explorer.Tree, Path, Column, Area);
      Iter := Get_Iter (Tooltip.Explorer.Model, Path);
      Path_Free (Path);

      if Column = Tooltip.Explorer.Status_Column then
         Text := new String'
           (-"Status: "
            & Get_String
              (Tooltip.Explorer.Model, Iter, Status_Description_Column));

      elsif Column = Tooltip.Explorer.Log_Column then
         --  ??? check if we are on an activity line or a file line
         if Get_Boolean (Tooltip.Explorer.Model, Iter, Has_Log_Column) then
            Text := new String'(-"A revision log exists for this file");
         else
            Text := new String'(-"No revision log exists for this file");
         end if;

      elsif Column = Tooltip.Explorer.File_Column then
         Text := new String'
           (Get_String (Tooltip.Explorer.Model, Iter, Name_Column));
      end if;

      if Text /= null then
         Create_Pixmap_From_Text
           (Text.all,
            Default_Font.Get_Pref,
            Tooltip_Color.Get_Pref,
            Tooltip.Explorer.Tree,
            Pixmap);
         GNAT.Strings.Free (Text);
      end if;
   end Draw;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer  : access VCS_View_Record'Class;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean)
   is
      Pixbuf : Gdk_Pixbuf;
   begin
      Success := True;

      if Line_Info.Status.File = No_File then
         Success := False;
         return;
      end if;

      Do_Fill_Info (Explorer.all, Iter, Line_Info, Success);

      if not Success then
         return;
      end if;

      Set (Explorer.Model, Iter, Has_Log_Column, Line_Info.Log);
      Set (Explorer.Model, Iter, Name_Column,
           Full_Name (Line_Info.Status.File).all);
      Set (Explorer.Model, Iter, Key_Column,
           File_Key (Line_Info.Status.File));

      if Is_Directory (Line_Info.Status.File) then
         Set (Explorer.Model, Iter, Base_Name_Column,
              '[' & Base_Dir_Name (Line_Info.Status.File) & ']');
      else
         Set (Explorer.Model, Iter, Base_Name_Column,
              Base_Name (Line_Info.Status.File));
      end if;

      if Line_Info.Status.Working_Revision = null then
         Set (Explorer.Model, Iter, Local_Rev_Column, -"n/a");
      else
         Set (Explorer.Model, Iter, Local_Rev_Column,
              Line_Info.Status.Working_Revision.all);
      end if;

      if Line_Info.Status.Repository_Revision = null then
         Set (Explorer.Model, Iter, Rep_Rev_Column, -"n/a");
      else
         Set (Explorer.Model, Iter, Rep_Rev_Column,
              Line_Info.Status.Repository_Revision.all);
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
   -- Get_Iter_From_File --
   ------------------------

   function Get_Iter_From_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File) return Gtk_Tree_Iter
   is
      Full_Name : constant String := File_Key (File);

      Result    : Gtk_Tree_Iter := Null_Iter;

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean);
      --  Check if Iter correspond to File

      -------------
      -- Look_In --
      -------------

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean) is
      begin
         if not Root
           and then Get_String (Explorer.Model, Iter, Key_Column) = Full_Name
         then
            Result := Iter;
            Quit   := True;
         end if;
      end Check_Iter;

      procedure Check_Files is new For_Every_Nodes (Check_Iter);

   begin
      Check_Files (Explorer, Root_Only => False);
      return Result;
   end Get_Iter_From_File;

   function Get_Iter_From_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Parent   : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      Full_Name : constant String := File_Key (File);
      Iter      : Gtk_Tree_Iter := Children (Explorer.Model, Parent);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Explorer.Model, Iter, Key_Column) = Full_Name then
            return Iter;
         end if;
         Next (Explorer.Model, Iter);
      end loop;
      return Null_Iter;
   end Get_Iter_From_File;

   ----------------------------
   -- Get_Iter_For_Root_Node --
   ----------------------------

   function Get_Iter_For_Root_Node
     (Explorer : access VCS_View_Record'Class;
      Column   : Gint;
      Value    : String) return Gtk_Tree_Iter
   is
      Result : Gtk_Tree_Iter := Null_Iter;

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean);
      --  Check if Iter corresponds to File

      -------------
      -- Look_In --
      -------------

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean)
      is
         pragma Unreferenced (Root);
      begin
         if Get_String (Explorer.Model, Iter, Column) = Value then
            Result := Iter;
            Quit   := True;
         end if;
      end Check_Iter;

      procedure Check_Roots is new For_Every_Nodes (Check_Iter);

   begin
      Check_Roots (Explorer, Root_Only => True);
      return Result;
   end Get_Iter_For_Root_Node;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Explorer : access VCS_View_Record) return Kernel_Handle is
   begin
      return Explorer.Kernel;
   end Get_Kernel;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : VCS_View_Access) return String_List.List
   is
      Result : String_List.List;

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access);
      --  Add an item to Result

      -----------------------
      -- Add_Selected_Item --
      -----------------------

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access)
      is
         pragma Unreferenced (Model, Path, Data);
      begin
         if not (Parent (Explorer.Model, Iter) = Null_Iter) then
            --  Do not take root nodes, those are the activity name or the VCS
            --  project name not a file.

            String_List.Append
              (Result, Get_String (Explorer.Model, Iter, Name_Column));
         end if;
      end Add_Selected_Item;

      E  : aliased VCS_View_Access := Explorer;
      EA : constant Explorer_Selection_Foreach.Data_Type_Access :=
             E'Unchecked_Access;

   begin
      if Explorer = null then
         return Result;
      end if;

      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Explorer.Tree),
         Add_Selected_Item'Unrestricted_Access, EA);
      return Result;
   end Get_Selected_Files;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
   begin
      Save_Cache (Explorer.Kernel, Get_Status_Cache);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Destroy;

   -----------------
   -- On_Selected --
   -----------------

   procedure On_Selected (View : access Gtk_Widget_Record'Class) is
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
   begin
      Grab_Focus (Explorer.Tree);
   end On_Selected;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
   begin
      Do_Delete (Explorer.all);
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Delete;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Explorer : access VCS_View_Record) return Selection_Context
   is
      Context : Selection_Context;
   begin
      if Explorer.Context = No_Context then
         declare
            Files : String_List.List :=
                      Get_Selected_Files (VCS_View_Access (Explorer));
         begin
            Context := New_Context;

            Set_Context_Information
              (Context, Explorer.Kernel,
               Abstract_Module_ID (VCS_Module_ID));

            if not String_List.Is_Empty (Files) then
               Set_File_Information (Context, Files => Create (Files));
               String_List.Free (Files);
            end if;

            Set_Current_Context (Explorer, Context);
            return Context;
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
      Context  : Selection_Context) is
   begin
      Explorer.Context := Context;
   end Set_Current_Context;

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : access VCS_View_Record'Class) is
   begin
      if Realized_Is_Set (Explorer.Tree) then
         Scroll_To_Point (Explorer.Tree, 0, 0);
      end if;

      Clear (Explorer.Model);
   end Clear;

   ------------------
   -- Copy_Context --
   ------------------

   function Copy_Context
     (Context : Selection_Context) return Selection_Context
   is
      Result : Selection_Context;
   begin
      if Context /= No_Context then
         Result := New_Context;

         if Has_Activity_Information (Context) then
            Set_Activity_Information (Result, Activity_Information (Context));
         end if;

         if Has_File_Information (Context) then
            Set_File_Information
              (Result,
               File_Information (Context),
               Project_Information (Context));
         end if;

         Set_Context_Information
           (Result, Get_Kernel (Context), Get_Creator (Context));
      end if;

      return Result;
   end Copy_Context;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : access VCS_View_Record'Class) is
      Sort_Col : Gint;
   begin
      Push_State (Explorer.Kernel, Busy);

      Sort_Col := Freeze_Sort (Explorer.Model);

      Do_Refresh (Explorer);

      Thaw_Sort (Explorer.Model, Sort_Col);
      Columns_Autosize (Explorer.Tree);
      Pop_State (Explorer.Kernel);
   end Refresh;

   -----------------
   -- Refresh_Log --
   -----------------

   procedure Refresh_Log
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Log   : Boolean;
      Dummy : Boolean;
      Iter  : Gtk_Tree_Iter;
      Line  : Line_Record;
   begin
      if Get_Log_From_File (Explorer.Kernel, File, False) = No_File then
         Log := False;
      else
         Log := True;
      end if;

      --  Refresh the information in the cache

      Line := Get_Cache (Get_Status_Cache, File);

      if Line /= No_Data then
         Line.Log := Log;
         Set_Cache (Get_Status_Cache, File, Line);
         Iter := Get_Iter_From_File (Explorer, File);

         if Iter /= Null_Iter then
            Fill_Info (Explorer, Iter, Line, Dummy);
         end if;
      end if;
   end Refresh_Log;

   ------------------
   -- Collapse_All --
   ------------------

   procedure Collapse_All (Explorer : access VCS_View_Record'Class) is
   begin
      Collapse_All (Explorer.Tree);
   end Collapse_All;

   ----------------
   -- Expand_All --
   ----------------

   procedure Expand_All (Explorer : access VCS_View_Record'Class) is
   begin
      Expand_All (Explorer.Tree);
   end Expand_All;

   ------------------------------
   -- Select_Files_Same_Status --
   ------------------------------

   procedure Select_Files_Same_Status
     (Explorer : access VCS_View_Record'Class)
   is
      Files : String_List.List;
      Iter  : Gtk_Tree_Iter;

   begin
      --  Get the current selection, look for the status

      Files := Get_Selected_Files (VCS_View_Access (Explorer));

      if String_List.Length (Files) = 1 then
         --  We have a single file selected

         Iter := Get_Iter_From_File
           (Explorer,
            Create (String_List.Data (String_List.First (Files))));

         declare
            procedure Select_Same_Status
              (Iter : in out Gtk_Tree_Iter;
               Root : Boolean;
               Quit : in out Boolean);

            Status    : constant String :=
                          Get_String
                            (Explorer.Model, Iter, Status_Description_Column);
            Selection : constant Gtk_Tree_Selection :=
                          Get_Selection (Explorer.Tree);

            ------------------------
            -- Select_Same_Status --
            ------------------------

            procedure Select_Same_Status
              (Iter : in out Gtk_Tree_Iter;
               Root : Boolean;
               Quit : in out Boolean)
            is
               pragma Unreferenced (Root, Quit);
               S : constant String :=
                     Get_String
                       (Explorer.Model, Iter, Status_Description_Column);
            begin
               if Status = S then
                  Select_Iter (Selection, Iter);
               end if;
            end Select_Same_Status;

            ----------------
            -- Select_All --
            ----------------

            procedure Select_All is new For_Every_Nodes (Select_Same_Status);

         begin
            --  Walk through the tree and select all nodes having the same
            --  status.
            Select_All (Explorer, False);
         end;
      end if;
   end Select_Files_Same_Status;

   ------------------------
   -- On_Menu_Expand_All --
   ------------------------

   procedure On_Menu_Expand_All
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_Activity_Information (Context) then
         Expand_All (Get_Activities_Explorer (Kernel));
      else
         Expand_All (Get_Explorer (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Expand_All;

   --------------------------
   -- On_Menu_Collaspe_All --
   --------------------------

   procedure On_Menu_Collapse_All
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_Activity_Information (Context) then
         Collapse_All (Get_Activities_Explorer (Kernel));
      else
         Collapse_All (Get_Explorer (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Collapse_All;

   --------------------------------------
   -- On_Menu_Select_Files_Same_Status --
   --------------------------------------

   procedure On_Menu_Select_Files_Same_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_Activity_Information (Context) then
         Select_Files_Same_Status (Get_Activities_Explorer (Kernel));
      else
         Select_Files_Same_Status (Get_Explorer (Kernel));
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Select_Files_Same_Status;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Iter : Gtk_Tree_Iter := Get_Iter_From_File (Explorer, File);
   begin
      if Iter /= Null_Iter then
         Remove (Explorer.Model, Iter);
      end if;
   end Remove_File;

end VCS_View;
