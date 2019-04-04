------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GUI_Utils;                 use GUI_Utils;
with Gdk.Rectangle;
with Gdk;
with Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;
with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Log_Utils;                 use Log_Utils;
with VCS;                       use VCS;
with VCS_Module;                use VCS_Module;

package body VCS_View is

   use type GNAT.Strings.String_Access;

   procedure Selection_Changed (View  : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed on the explorer

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal, connected before

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
            Dir : constant String := String (Full_Name (File, True).all);
         begin
            return Dir (Dir'First .. Dir'Last - 1);
         end;
      else
         return String (Full_Name (File, True).all);
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
         I := Iter;

         while not Quit and then I /= Null_Iter loop
            if Has_Child (Explorer.Model, I) and then not Root_Only then
               Iterate (Children (Explorer.Model, I), Root => False);
            end if;

            K := I;
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
      Kernel   : access Kernel_Handle_Record'Class)
   is
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Tooltip         : VCS_Tooltip_Handler_Access;
      Selection       : Gtk_Tree_Selection;

   begin
      Initialize_Hbox (Explorer);

      Explorer.Kernel := Kernel_Handle (Kernel);

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

      Do_Initialize (Explorer, Explorer.Kernel);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Explorer,
         After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Explorer.Tree.Get_Selection,
         Gtk.Tree_Selection.Signal_Changed,
         Gtkada.Handlers.Widget_Callback.To_Marshaller
           (Selection_Changed'Access),
         Explorer,
         After => False);

      Tooltip := new VCS_Tooltip_Handler;
      Tooltip.Explorer := VCS_View.VCS_View_Access (Explorer);
      Tooltip.Associate_To_Widget (Explorer.Tree);

      --  Emit a "clicked" signal on the file column to sort it

      Clicked (Explorer.File_Column);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer, Signal_Delete_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Delete'Access),
         Explorer, After => False);

      Explorer.On_Destroy (On_Destroy'Access);

      --  Can't do this through the Focus_Widget parameter to Gtkada.MDI.Put,
      --  since the focus child is dynamic.
      Widget_Callback.Connect
        (Explorer, Signal_Grab_Focus, On_Selected'Access, After => True);
   end Initialize;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model
     (Explorer : access VCS_View_Record'Class) is
   begin
      Gtk_New (Explorer.Model, Columns_Types (Explorer));
   end Create_Model;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (View  : access Gtk_Widget_Record'Class) is
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
   begin
      --  Reset the context
      Explorer.Kernel.Context_Changed (Explorer.Build_View_Context (null));
   end Selection_Changed;

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
         Iter := Find_Iter_For_Event (Explorer.Tree, Event);

         if Iter /= Null_Iter then
            Path := Get_Path (Get_Model (Explorer.Tree), Iter);

            if Get_Depth (Path) = 2 then
               Iter := Get_Iter (Explorer.Model, Path);
               Open_File_Action_Hook.Run
                 (Kernel,
                  File    => Get_File (Explorer.Model, Iter, File_Column),
                  Project => No_Project,  --  any project will do
                  Line   => 0,
                  Column => 0);
               Emit_Stop_By_Name (Explorer.Tree, "button_press_event");
            end if;

            Path_Free (Path);
         end if;
      end if;

      return False;
   end Button_Press;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip : access VCS_Tooltip_Handler;
      Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y    : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);  --  a Gtk_Tree_View, not the explorer

      Label      : Gtk_Label;

      Path       : Gtk_Tree_Path;
      Column     : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y     : Gint;
      Row_Found  : Boolean;
      Iter       : Gtk_Tree_Iter;
      Area       : Gdk.Rectangle.Gdk_Rectangle;

   begin
      if Tooltip.Explorer.Tree = null then
         return null;
      end if;
--        Window := Get_Bin_Window (Tooltip.Explorer.Tree);
--        Get_Pointer (Window, X1, Y1, Mask, New_Window);

      Get_Path_At_Pos
        (Tooltip.Explorer.Tree, X, Y, Path, Column, Cell_X, Cell_Y, Row_Found);

      if not Row_Found then
         return null;
      end if;

      Get_Cell_Area (Tooltip.Explorer.Tree, Path, Column, Area);
      Iter := Get_Iter (Tooltip.Explorer.Model, Path);
      Path_Free (Path);

      Tooltip.Set_Tip_Area (Area);

      if Column = Tooltip.Explorer.Status_Column then
         Gtk_New
           (Label,
            -"<b>Status</b>: "
            & Get_String
              (Tooltip.Explorer.Model, Iter, Status_Description_Column));
         Label.Set_Use_Markup (True);

      elsif Column = Tooltip.Explorer.Log_Column then
         --  ??? check if we are on an activity line or a file line
         if Get_Boolean (Tooltip.Explorer.Model, Iter, Has_Log_Column) then
            Gtk_New (Label, -"A revision log exists for this file");
         else
            Gtk_New (Label, -"No revision log exists for this file");
         end if;

      elsif Column = Tooltip.Explorer.File_Column then
         Gtk_New
           (Label,
            Get_File
              (Tooltip.Explorer.Model, Iter, File_Column).Display_Full_Name);
      end if;

      return Gtk_Widget (Label);
   end Create_Contents;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer  : access VCS_View_Record'Class;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean)
   is
      Values  : Glib.Values.GValue_Array (1 .. 9);
      Columns : constant Columns_Array (Values'Range) :=
        (Has_Log_Column, Local_Rev_Column, Rep_Rev_Column,
         Status_Icon_Name_Column, Status_Description_Column,
         File_Column, Name_Column, Key_Column, Base_Name_Column);
      Last    : Gint := 5;

   begin
      if Line_Info.Status.File = No_File then
         Success := False;
         return;
      end if;

      Do_Fill_Info (Explorer.all, Iter, Line_Info, Success);

      if not Success then
         return;
      end if;

      Values (1 .. 5) :=
        (1 => As_Boolean (Line_Info.Log),
         2 => As_String
           (if Line_Info.Status.Working_Revision = null
            then "n/a"
            else Line_Info.Status.Working_Revision.all),
         3 => As_String
           (if Line_Info.Status.Repository_Revision = null
            then"n/a"
            else Line_Info.Status.Repository_Revision.all),
         4 => As_String
           (if Line_Info.Status.Status.Icon_Name = null
            then VCS.Unknown.Icon_Name.all
            else Line_Info.Status.Status.Icon_Name.all),
         5 => As_String
           (if Line_Info.Status.Status.Label = null
            then VCS.Unknown.Label.all
            else Line_Info.Status.Status.Label.all));

      if Get_File (Explorer.Model, Iter, File_Column) /=
        Line_Info.Status.File
      then
         Last := 9;

         Values (6 .. 9) :=
           (6 => As_File (Line_Info.Status.File),
            7 => As_String (Line_Info.Status.File.Display_Full_Name),
            8 => As_String (File_Key (Line_Info.Status.File)),
            9 => As_String
              (if Is_Directory (Line_Info.Status.File)
               then'[' & (+Base_Dir_Name (Line_Info.Status.File)) & ']'
               else Display_Base_Name (Line_Info.Status.File)));
      end if;

      Set_And_Clear
        (Explorer.Model, Iter, Columns (1 .. Last), Values (1 .. Last));
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

      ----------------
      -- Check_Iter --
      ----------------

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

      ----------------
      -- Check_Iter --
      ----------------

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
     (Explorer : VCS_View_Access) return File_Array_Access
   is
      Result : File_Array_Access := new File_Array'(1 .. 0 => <>);

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
      --  Add an item to Result

      -----------------------
      -- Add_Selected_Item --
      -----------------------

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      is
         pragma Unreferenced (Model, Path);
      begin
         if not (Parent (Explorer.Model, Iter) = Null_Iter) then
            --  Do not take root nodes, those are the activity name or the VCS
            --  project name not a file.

            GNATCOLL.VFS.Append
              (Result,
               Get_File (Explorer.Model, Iter, File_Column));
         end if;
      end Add_Selected_Item;

   begin
      if Explorer = null then
         return Result;
      end if;

      Explorer.Tree.Get_Selection.Selected_Foreach
        (Add_Selected_Item'Unrestricted_Access);

      return Result;
   end Get_Selected_Files;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
   begin
      Save_Cache (Explorer.Kernel, Get_Status_Cache);
      On_Destroy (Explorer.all);
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
   end On_Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : access VCS_View_Record'Class) is
   begin
      if Explorer.Tree.Get_Realized then
         Scroll_To_Point (Explorer.Tree, 0, 0);
      end if;

      Clear (Explorer.Model);
   end Clear;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : access VCS_View_Record'Class) is
      Sort_Col : Gint;
   begin
      Sort_Col := Freeze_Sort (Explorer.Model);

      Do_Refresh (Explorer);

      Thaw_Sort (Explorer.Model, Sort_Col);
      Columns_Autosize (Explorer.Tree);
   end Refresh;

   ------------------
   -- Refresh_File --
   ------------------

   procedure Refresh_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Log      : Boolean := False)
   is
      Dummy : Boolean;
      Iter  : Gtk_Tree_Iter;
      Line  : Line_Record;
   begin
      --  Refresh the information in the cache

      Line := Get_Cache (Get_Status_Cache, File);

      if Log then
         if Get_Log_From_File (Explorer.Kernel, File, False) = No_File then
            Line.Log := False;
         else
            Line.Log := True;
         end if;
      end if;

      if Line /= No_Data then
         Set_Cache (Get_Status_Cache, File, Line);
         Iter := Get_Iter_From_File (Explorer, File);

         if Iter /= Null_Iter then
            Fill_Info (Explorer, Iter, Line, Dummy);
         end if;
      end if;
   end Refresh_File;

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
      Files : File_Array_Access;
      Iter  : Gtk_Tree_Iter;

   begin
      --  Get the current selection, look for the status

      Files := Get_Selected_Files (VCS_View_Access (Explorer));

      if Files'Length = 1 then
         --  We have a single file selected

         Iter := Get_Iter_From_File (Explorer, Files (Files'First));

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

      Unchecked_Free (Files);
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
   end On_Menu_Expand_All;

   --------------------------
   -- On_Menu_Collapse_All --
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
