------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers;

with Gdk;

with Glib_String_Utils;         use Glib_String_Utils;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Gtk;                       use Gtk;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Tree_Sortable;         use Gtk.Tree_Sortable;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Log_Utils;                 use Log_Utils;
with Projects;                  use Projects;
with VCS_Activities;            use VCS_Activities;
with VCS_Module;                use VCS_Module;
with VCS_Module.Actions;        use VCS_Module.Actions;
with VCS_Utils;                 use VCS_Utils;
with VCS_View_API;              use VCS_View_API;
with Ignore_Db;                 use Ignore_Db;

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;

package body VCS_View.Explorer is

   --------------------
   -- Local packages --
   --------------------

   package Page_Status_Callback is new Gtk.Handlers.User_Callback
     (GObject_Record, Natural);

   function Status_Sort
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Used to sort on status in the explorer, this special sort routine is
   --  needed to keep the order also sorted on names.

   -------------------
   -- Columns_Types --
   -------------------

   overriding function Columns_Types
     (Explorer : access VCS_Explorer_View_Record) return GType_Array
   is
      pragma Unreferenced (Explorer);
   begin
      return GType_Array'
        (Base_Name_Column          => GType_String,
         Name_Column               => GType_String,
         File_Column               => Get_Virtual_File_Type,
         Key_Column                => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Icon_Name_Column   => GType_String,
         Has_Log_Column            => GType_Boolean,
         Activity_Column           => GType_String,
         Control_Column            => GType_Boolean);
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (Explorer : access VCS_Explorer_View_Record);
   --  Sets the types of columns to be displayed in the tree_view

   ---------------
   -- Callbacks --
   ---------------

   procedure Show_All_Status
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for activation of "Show all status"

   procedure Hide_All_Status
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for activation of "Show all status"

   procedure Toggle_Show_Status
     (Explorer : access GObject_Record'Class;
      Index    : Natural);
   --  Callback for activation of each filter

   type On_File_Edited is new File_Hooks_Function with record
      Explorer : VCS_Explorer_View_Access;
   end record;
   overriding procedure Execute
     (Self      : On_File_Edited;
      Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File);
   --  Callback for the "file_edited" signal

   procedure Contextual_Menu_Factory
     (Context : Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Default context factory

   function To_History_Key (S : String) return History_Key;
   --  Return history key corresponding to S

   function Match_Filter
     (Explorer : VCS_Explorer_View_Record;
      Label    : String) return Boolean;
   --  Returns True if Status can be displayed in the explorer

   procedure Display_File_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      File_Status    : File_Status_Record;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Display        : Boolean := True;
      Displayed      : out Boolean);

   procedure Remove_Empty_Root
     (Explorer : access VCS_Explorer_View_Record'Class);
   --  Removes root node with no children

   --------------------
   -- To_History_Key --
   --------------------

   function To_History_Key (S : String) return History_Key is
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

   ---------------
   -- Do_Delete --
   ---------------

   overriding procedure Do_Delete
     (Explorer : VCS_Explorer_View_Record)
   is
      pragma Unreferenced (Explorer);
   begin
      Hide_VCS_Explorer;
   end Do_Delete;

   ----------------
   -- Do_Refresh --
   ----------------

   overriding procedure Do_Refresh
     (Explorer : access VCS_Explorer_View_Record)
   is
      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean);
      --  Check if Iter match the current filter, remove node if not

      ----------------
      -- Check_Iter --
      ----------------

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean)
      is
         pragma Unreferenced (Quit);
         C_Stat : constant String :=
                    Get_String
                      (Explorer.Model, Iter, Status_Description_Column);
      begin
         if not Root then
            if not Match_Filter (Explorer.all, C_Stat) then
               --  Current status does not match or the status has changed
               Set (Explorer.Hidden,
                    Get_String (Explorer.Model, Iter, Key_Column), 0);
               Remove (Explorer.Model, Iter);

            else
               declare
                  File    : constant Virtual_File :=
                              Get_File (Explorer.Model, Iter, File_Column);
                  Line    : constant Line_Record :=
                              Get_Cache (Get_Status_Cache, File);
                  Success : Boolean;
               begin
                  Fill_Info (Explorer, Iter, Line, Success);
               end;
            end if;
         end if;
      end Check_Iter;

      ----------------------
      -- Check_File_Nodes --
      ----------------------

      procedure Check_File_Nodes is new For_Every_Nodes (Check_Iter);

   begin
      --  First update the current nodes

      Check_File_Nodes (Explorer, Root_Only => False);

      --  Now go through all files that are not displayed due to filter

      declare
         Status    : File_Status_Record;
         Displayed : Boolean;
         Iter      : File_Hash.String_Hash_Table.Cursor;
      begin
         Get_First (Explorer.Hidden, Iter);

         while Get_Element (Iter) /= 1 loop
            Status.File := Create (+Get_Key (Iter));
            Display_File_Status
              (Kernel         => Explorer.Kernel,
               File_Status    => Status,
               VCS_Identifier => Explorer.VCS,
               Override_Cache => False,
               Force_Display  => True,
               Displayed      => Displayed);

            if Displayed then
               Remove (Explorer.Hidden, Get_Key (Iter));
            end if;

            Get_Next (Explorer.Hidden, Iter);
         end loop;
      end;

      Remove_Empty_Root (Explorer);
   end Do_Refresh;

   -----------------------
   -- Remove_Empty_Root --
   -----------------------

   procedure Remove_Empty_Root
     (Explorer : access VCS_Explorer_View_Record'Class)
   is

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean);
      --  Check if Iter is an empty node, remove it in this case

      ----------------
      -- Check_Iter --
      ----------------

      procedure Check_Iter
        (Iter : in out Gtk_Tree_Iter;
         Root : Boolean;
         Quit : in out Boolean)
      is
         pragma Unreferenced (Root, Quit);
      begin
         if not Has_Child (Explorer.Model, Iter) then
            Remove (Explorer.Model, Iter);
         end if;
      end Check_Iter;

      ----------------------
      -- Check_Root_Nodes --
      ----------------------

      procedure Check_Root_Nodes is new For_Every_Nodes (Check_Iter);

   begin
      Check_Root_Nodes (Explorer, Root_Only => True);
   end Remove_Empty_Root;

   ------------------
   -- Match_Filter --
   ------------------

   function Match_Filter
     (Explorer : VCS_Explorer_View_Record;
      Label    : String) return Boolean is
   begin
      declare
         Filter_Status : constant Status_Array_Access :=
                           Get (Explorer.Status, Name (Explorer.VCS));
      begin
         for K in Filter_Status'Range loop
            if Filter_Status (K).Status.Label.all = Label then
               return Filter_Status (K).Display;
            end if;
         end loop;
      end;

      --  No corresponding status, ok to display
      return True;
   end Match_Filter;

   -----------------------
   -- On_Remove_Project --
   -----------------------

   procedure On_Remove_Project
     (Explorer : access VCS_Explorer_View_Record;
      Project  : String)
   is
      Iter : Gtk_Tree_Iter :=
               Get_Iter_For_Root_Node (Explorer, Name_Column, Project);
   begin
      if Iter /= Null_Iter then
         Remove (Explorer.Model, Iter);
         Refresh (Explorer);
      end if;
   end On_Remove_Project;

   -------------------------
   -- Display_File_Status --
   -------------------------

   procedure Display_File_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      File_Status    : File_Status_Record;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Display        : Boolean := True;
      Displayed      : out Boolean)
   is
      Explorer : constant VCS_Explorer_View_Access :=
                   Get_Explorer (Kernel, False, False);

      File     : Virtual_File renames File_Status.File;

      New_Root : Boolean := False;
      --  Set to true if we have created a project node

      function Get_Or_Create_Project_Iter
        (File : Virtual_File) return Gtk_Tree_Iter;
      --  This routine creates a project from the explorer if needed. It
      --  returns the tree iterator for the newly created project or Null_Iter
      --  otherwise.

      --------------------------------
      -- Get_Or_Create_Project_Iter --
      --------------------------------

      function Get_Or_Create_Project_Iter
        (File : Virtual_File) return Gtk_Tree_Iter
      is
         function Get_Or_Create_Name
           (Name : String; File : Virtual_File; Display_VCS : Boolean)
            return Gtk_Tree_Iter;
         --  Get or create node named Name

         ------------------------
         -- Get_Or_Create_Name --
         ------------------------

         function Get_Or_Create_Name
           (Name : String; File : Virtual_File; Display_VCS : Boolean)
            return Gtk_Tree_Iter
         is
            R_Iter : Gtk_Tree_Iter;

         begin
            R_Iter :=
              Get_Iter_For_Root_Node (Explorer, Name_Column, Name);

            if R_Iter = Null_Iter then
               Append (Explorer.Model, R_Iter, Null_Iter);

               Set_And_Clear
                 (Explorer.Model, R_Iter,
                  (Name_Column, File_Column, Base_Name_Column, Control_Column),
                  (1 => As_String (Name),
                   2 => As_File   (File),
                   3 => As_String
                     (if Display_VCS
                      then (Name & " (" & VCS.Name (VCS_Identifier) & ')')
                      else Name),
                   4 => As_Boolean (False)));

               New_Root := True;
            end if;

            return R_Iter;
         end Get_Or_Create_Name;

         --  Take the first possible project, since for a given physical
         --  file the VCS will be the same
         Sets : constant File_Info_Set :=
           Get_Registry (Kernel).Tree.Info_Set (File);
         Project : constant Project_Type :=
           File_Info'Class (Sets.First_Element).Project;

      begin
         if Project = No_Project then
            return Get_Or_Create_Name
              ("No project", GNATCOLL.VFS.No_File, False);
         else
            return Get_Or_Create_Name
              (Project.Name, Project.Project_Path, True);
         end if;
      end Get_Or_Create_Project_Iter;

      P_Iter : Gtk_Tree_Iter := Null_Iter;
      Line   : Line_Record;
      Log    : Boolean;

   begin
      Displayed := False;

      if not GNATCOLL.VFS.Is_Directory (File)
        and then not Ignore_File (VCS_Identifier, File)
      then
         Line := Get_Cache (Get_Status_Cache, File);

         if Line = No_Data
           or else Override_Cache
           or else Line.Status.Status = Unknown
         then
            Log :=
              Get_Log_From_File (Kernel, File, False) /= GNATCOLL.VFS.No_File;

            --  Set_Cache already does a copy of the File_Status, no need to
            --  redo it here.
            Line := (File_Status, Log);
            Set_Cache (Get_Status_Cache, File, Line);
         end if;

         --  The info that we want to display is now in Line,
         --  Check wether the current file matches the filer.

         if Display then
            declare
               Iter    : Gtk_Tree_Iter := Null_Iter;
               Success : Boolean;
            begin
               Iter := Get_Iter_From_File (Explorer, Line.Status.File);

               if Match_Filter
                 (VCS_Explorer_View_Record (Explorer.all),
                  Line.Status.Status.Label.all)
               then
                  if Iter = Null_Iter and then Force_Display then
                     P_Iter := Get_Or_Create_Project_Iter (File);
                     Append (Explorer.Model, Iter, P_Iter);
                     Displayed := True;
                  end if;

                  if Iter /= Null_Iter then
                     Fill_Info (Explorer, Iter, Line, Success);
                  end if;

               else
                  --  Register this file as part of the explorer

                  if Get (Explorer.Hidden, File_Key (File)) = 1 then
                     Set (Explorer.Hidden, File_Key (File), 0);
                  end if;

                  --  And if present on the explorer, remove it

                  if Iter /= Null_Iter then
                     Remove (Explorer.Model, Iter);
                  end if;
               end if;
            end;
         end if;
      end if;

      if New_Root then
         --  Expand this new node
         declare
            Path : Gtk_Tree_Path;
            Res  : Boolean;
            pragma Unreferenced (Res);
         begin
            Path := Get_Path (Explorer.Model, P_Iter);
            Res := Expand_Row (Explorer.Tree, Path, True);
            Path_Free (Path);
         end;
      end if;

      Remove_Empty_Root (Explorer);
   end Display_File_Status;

   -------------------------
   -- Display_File_Status --
   -------------------------

   procedure Display_File_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Status         : File_Status_List.Vector;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True)
   is
      use type Ada.Containers.Count_Type;

      Explorer          : constant VCS_Explorer_View_Access :=
                            Get_Explorer (Kernel, False, False);
      Registered_Status : constant VCS.Status_Array :=
                            Get_Registered_Status (VCS_Identifier);

      Sort_Id           : Gint := 0;

      Up_To_Date_Status : VCS_File_Status;
      Filter_Status     : Status_Array_Access :=
                            Get (Explorer.Status, Name (VCS_Identifier));

   begin
      if Registered_Status'Length >= 2 then
         Up_To_Date_Status := Registered_Status (Registered_Status'First + 1);
      end if;

      Explorer.VCS := VCS_Identifier;

      if Filter_Status = null then
         --  No status registered for this VCS
         Filter_Status := new Status_Array (Registered_Status'Range);

         for J in Registered_Status'Range loop
            Filter_Status (J).Status  := Registered_Status (J);
            declare
               Key : constant History_Key :=
                       To_History_Key (Filter_Status (J).Status.Label.all);
            begin
               Create_New_Boolean_Key_If_Necessary
                 (Get_History (Explorer.Kernel).all, Key, True);
               Filter_Status (J).Display := Get_History
                 (Get_History (Explorer.Kernel).all, Key);
            end;
         end loop;

         Set (Explorer.Status, Name (VCS_Identifier), Filter_Status);
      end if;

      --  Free the logs associated to the files that are up-to-date, and
      --  update the vcs label in the editors.

      Update_Files_Status
        (Kernel, Status, VCS_Identifier, Clear_Logs, Up_To_Date_Status);

      if Explorer = null then
         return;
      end if;

      if Status.Length > 1 then
         Sort_Id := Freeze_Sort (Explorer.Model);
      end if;

      --  Iterate over each file

      for Item of Status loop
         declare
            Displayed : Boolean;
            pragma Unreferenced (Displayed);
         begin
            Display_File_Status
              (Kernel, Item, Explorer.VCS, Override_Cache, Force_Display,
               Display, Displayed);
         end;
      end loop;

      Remove_Empty_Root (Explorer);

      if Status.Length > 1 then
         Thaw_Sort (Explorer.Model, Sort_Id);
      end if;
   end Display_File_Status;

   --------------------
   -- No_VCS_Message --
   --------------------

   procedure No_VCS_Message (Explorer : VCS_Explorer_View_Access) is
      R_Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      Clear (Explorer.Model);
      Append (Explorer.Model, R_Iter, Null_Iter);
      Explorer.Model.Set
        (R_Iter, Base_Name_Column, -"No VCS for this project");
   end No_VCS_Message;

   ---------------
   -- Fill_Info --
   ---------------

   overriding procedure Do_Fill_Info
     (Explorer  : VCS_Explorer_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean) is
   begin
      Set_And_Clear
        (Explorer.Model, Iter,
         (Activity_Column, Control_Column),
         (As_String (Get_Name (Get_File_Activity (Line_Info.Status.File))),
          As_Boolean (True)));

      Success := True;
   end Do_Fill_Info;

   -----------------
   -- Status_Sort --
   -----------------

   function Status_Sort
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      A_Status : constant String :=
                   Get_String (Model, A, Status_Description_Column);
      B_Status : constant String :=
                   Get_String (Model, B, Status_Description_Column);
   begin
      if A_Status = B_Status then
         return Compare
           (Get_String (Model, A, Base_Name_Column),
            Get_String (Model, B, Base_Name_Column));
      else
         return Compare (A_Status, B_Status);
      end if;
   end Status_Sort;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_Explorer_View_Record) is
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

      Gtk_New (Explorer.File_Column);
      Set_Title (Explorer.File_Column, -"Project / File");
      Pack_Start (Explorer.File_Column, Text_Rend, True);
      Add_Attribute
        (Explorer.File_Column, Text_Rend, "text", Base_Name_Column);
      Set_Clickable (Explorer.File_Column, True);
      Set_Sort_Column_Id (Explorer.File_Column, Base_Name_Column);
      Set_Resizable (Explorer.File_Column, True);
      Dummy := Append_Column (Explorer.Tree, Explorer.File_Column);

      Gtk_New (Explorer.Status_Column);
      Set_Title (Explorer.Status_Column, -"Status");
      Pack_Start (Explorer.Status_Column, Pixbuf_Rend, False);
      Add_Attribute
        (Explorer.Status_Column, Pixbuf_Rend, "icon-name",
         Status_Icon_Name_Column);
      Set_Clickable (Explorer.Status_Column, True);
      Set_Sort_Column_Id (Explorer.Status_Column, Status_Description_Column);

      Set_Sort_Func
        (+Explorer.Model,
         Status_Description_Column,
         Sort_Func => Status_Sort'Access);
      Set_Sort_Column_Id
        (+Explorer.Model, Status_Description_Column, Sort_Ascending);
      Dummy := Append_Column (Explorer.Tree, Explorer.Status_Column);

      Gtk_New (Explorer.Log_Column);
      Set_Title (Explorer.Log_Column, -"Log");
      Pack_Start (Explorer.Log_Column, Toggle_Rend, False);
      Add_Attribute
        (Explorer.Log_Column, Toggle_Rend, "active", Has_Log_Column);
      Add_Attribute
        (Explorer.Log_Column, Toggle_Rend, "visible", Control_Column);
      Set_Clickable (Explorer.Log_Column, False);
      Dummy := Append_Column (Explorer.Tree, Explorer.Log_Column);

      Gtk_New (Explorer.Activity_Column);
      Set_Title (Explorer.Activity_Column, -"Activity");
      Pack_Start (Explorer.Activity_Column, Text_Rend, True);
      Add_Attribute
        (Explorer.Activity_Column, Text_Rend, "text", Activity_Column);
      Dummy := Append_Column (Explorer.Tree, Explorer.Activity_Column);

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out VCS_Explorer_View_Access;
      Kernel   : access Kernel_Handle_Record'Class := null) is
   begin
      Explorer := new VCS_Explorer_View_Record;
      Initialize (Explorer, Kernel);
      Set_Name (Explorer.Tree, "VCS Explorer Tree");
   end Gtk_New;

   ---------------------
   -- Show_All_Status --
   ---------------------

   procedure Show_All_Status
     (Explorer : access Gtk_Widget_Record'Class)
   is
      E : constant VCS_Explorer_View_Access :=
            VCS_Explorer_View_Access (Explorer);
      S : Status_Array_Access;
   begin
      S := Get (E.Status, Name (E.VCS));

      for J in S'Range loop
         S (J).Display := True;

         Set_History
           (Get_History (E.Kernel).all,
            To_History_Key (S (J).Status.Label.all),
            True);
      end loop;

      Refresh (E);
   end Show_All_Status;

   ---------------------
   -- Hide_All_Status --
   ---------------------

   procedure Hide_All_Status
     (Explorer : access Gtk_Widget_Record'Class)
   is
      E : constant VCS_Explorer_View_Access :=
            VCS_Explorer_View_Access (Explorer);
      S : constant Status_Array_Access :=
            Get (E.Status, Name (E.VCS));
   begin
      for J in S'Range loop
         S (J).Display := False;
         Set_History
           (Get_History (E.Kernel).all,
            To_History_Key (S (J).Status.Label.all),
            False);
      end loop;

      Refresh (E);
   end Hide_All_Status;

   ------------------------
   -- Toggle_Show_Status --
   ------------------------

   procedure Toggle_Show_Status
     (Explorer : access GObject_Record'Class;
      Index    : Natural)
   is
      E : constant VCS_Explorer_View_Access :=
            VCS_Explorer_View_Access (Explorer);
      S : constant Status_Array_Access :=
            Get (E.Status, Name (E.VCS));
   begin
      if Index in S'Range then
         S (Index).Display := not S (Index).Display;
      end if;

      Refresh (E);
   end Toggle_Show_Status;

   ------------------------
   -- Build_View_Context --
   ------------------------

   overriding function Build_View_Context
     (Explorer : not null access VCS_Explorer_View_Record;
      Event    : Gdk.Event.Gdk_Event)
      return Selection_Context
   is
      Child : constant MDI_Child := Find_MDI_Child_From_Widget (Explorer);
      Context  : Selection_Context :=
        GPS_MDI_Child_Record (Child.all).Build_Context (Event);
      Kernel    : constant Kernel_Handle := Explorer.Kernel;
      Files     : File_Array_Access;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      Project   : Project_Type := No_Project;
   begin
      --  If there is no selection, select the item under the cursor

      Iter := Find_Iter_For_Event (Explorer.Tree, Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Get_Model (Explorer.Tree), Iter);

         if Event /= null
           and then not Path_Is_Selected (Get_Selection (Explorer.Tree), Path)
         then
            --  Right click over a line which is not the current selection,
            --  this line becomes the new selection.
            Unselect_All (Get_Selection (Explorer.Tree));
            Select_Path (Get_Selection (Explorer.Tree), Path);
         end if;

         if Get_Depth (Path) = 1 then
            Iter := Get_Iter (Explorer.Model, Path);
            Project := Get_Registry (Kernel).Tree.Project_From_Name
              (Get_String (Explorer.Model, Iter, Name_Column));
            Set_File_Information
              (Context,
               Files   => GNATCOLL.VFS.Empty_File_Array,
               Project => Project);

         elsif Get_Depth (Path) > 1 then
            Files := Get_Selected_Files (VCS_View_Access (Explorer));

            Iter := Parent (Explorer.Model, Get_Iter (Explorer.Model, Path));

            Project := Get_Registry (Kernel).Tree.Project_From_Name
              (Get_String (Explorer.Model, Iter, Name_Column));

            Set_File_Information
              (Context,
               Files   => Files.all,
               Project => Project);

            Unchecked_Free (Files);
         end if;

         Path_Free (Path);
      end if;

      if Project /= No_Project then
         Explorer.VCS := Get_Current_Ref (Kernel, Project);
      end if;

      return Context;
   end Build_View_Context;

   -----------------------------
   -- Contextual_Menu_Factory --
   -----------------------------

   procedure Contextual_Menu_Factory
     (Context : Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      Project   : constant Project_Type := Project_Information (Context);
      Root_Node : constant Boolean :=
        not Has_File_Information (Context);
      Check     : Gtk_Check_Menu_Item;
      Mitem     : Gtk_Menu_Item;
      Sep       : Gtk_Separator_Menu_Item;
      Submenu   : Gtk_Menu;
      Explorer : constant VCS_Explorer_View_Access :=
        VCS_Explorer_View_Access
          (GPS_MDI_Child
             (Get_MDI (Get_Kernel (Context)).Get_Focus_Child)
             .Get_Actual_Widget);

   begin
      if Root_Node then
         if Project = No_Project then
            Gtk_New (Mitem, Label => -"Remove node");
         else
            Gtk_New (Mitem, Label => -"Remove project");
         end if;

         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, Signal_Activate, On_Menu_Remove_Project'Access, Context);
         Set_Sensitive (Mitem, True);
      end if;

      VCS_Explorer_Contextual_Menu (Context, Menu, Show_Everything => False);

      Gtk_New (Mitem, -"Filters");
      Append (Menu, Mitem);

      Gtk_New (Submenu);
      Set_Submenu (Mitem, Submenu);

      Gtk_New (Mitem, -"Show all status");
      Append (Submenu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Signal_Activate, Show_All_Status'Access, Explorer);

      Gtk_New (Mitem, -"Hide all status");
      Append (Submenu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, Signal_Activate, Hide_All_Status'Access, Explorer);

      Gtk_New (Sep);
      Append (Submenu, Sep);

      declare
         S : constant Status_Array_Access :=
               Get (Explorer.Status, Name (Explorer.VCS));
      begin
         for J in S'Range loop
            Gtk_New (Check, Label => -"Show " & S (J).Status.Label.all);
            Set_Active (Check, S (J).Display);
            Append (Submenu, Check);
            Page_Status_Callback.Object_Connect
              (Check, Signal_Activate, Toggle_Show_Status'Access, Explorer, J);
            Associate
              (Get_History (Get_Kernel (Context)).all,
               To_History_Key (S (J).Status.Label.all),
               Check);
         end loop;
      end;
   end Contextual_Menu_Factory;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : On_File_Edited;
      Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File)
   is
      Line     : Line_Record;
   begin
      if Is_A_Log (File) then
         declare
            F : constant Virtual_File := Get_File_From_Log (Kernel, File);
         begin
            Line := Get_Cache (Get_Status_Cache, F);

            if Line /= No_Data then
               Line.Log := True;
               Set_Cache (Get_Status_Cache, F, Line);
            end if;

            Refresh (Self.Explorer);
         end;
      end if;
   end Execute;

   -------------------
   -- Do_Initialize --
   -------------------

   overriding procedure Do_Initialize
     (Explorer : access VCS_Explorer_View_Record;
      Kernel   : Kernel_Handle) is
   begin
      Setup_Contextual_Menu
        (Kernel,
         Explorer.Tree,
         Context_Func => Contextual_Menu_Factory'Access);

      Set_Column_Types (Explorer);

      File_Edited_Hook.Add
         (new On_File_Edited'
             (File_Hooks_Function with
              Explorer => VCS_Explorer_View_Access (Explorer)),
          Watch => Explorer);
   end Do_Initialize;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return File_Array_Access is
   begin
      return Get_Selected_Files
        (VCS_View_Access (Get_Explorer (Kernel, False, False)));
   end Get_Selected_Files;

   ----------------
   -- On_Destroy --
   ----------------

   overriding procedure On_Destroy (Self : in out VCS_Explorer_View_Record) is
   begin
      Status_Hash.String_Hash_Table.Reset (Self.Status);
   end On_Destroy;

end VCS_View.Explorer;
