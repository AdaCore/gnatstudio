-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;        use Glib;

with Gdk.Event;  use Gdk.Event;
with Gdk.Pixbuf; use Gdk.Pixbuf;

with Gtk;                       use Gtk;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
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

with VCS;

with VCS_View_Pixmaps;          use VCS_View_Pixmaps;
with VCS_View_API;              use VCS_View_API;
with VCS_Module;                use VCS_Module;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Intl;                use Glide_Intl;

with Basic_Types;               use Basic_Types;

with Prj;                       use Prj;
with Prj_API;                   use Prj_API;
with Prj.Tree;                  use Prj.Tree;

with Traces; use Traces;

package body VCS_View_Pkg is

   Me : Debug_Handle := Create ("VCS_INTERFACE");

   --------------------
   -- Local packages --
   --------------------

   package Explorer_Selection_Foreach is
     new Selection_Foreach (VCS_View_Record);

   package Check_VCS_View_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, VCS_View_Access);

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

   Name_Column               : constant := 0;
   Base_Name_Column          : constant := 1;
   Local_Rev_Column          : constant := 2;
   Rep_Rev_Column            : constant := 3;
   Status_Description_Column : constant := 4;
   Status_Pixbuf_Column      : constant := 5;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Name_Column               => GType_String,
         Base_Name_Column          => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type);
      --  The Log_Editor_Column should contain a procedure which returns the
      --  log string given a filename.
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   function String_Array_To_String_List
     (S : String_Id_Array) return String_List.List;
   --  Convenience function to make a string_list out of a String_Id_Array.

   procedure Refresh (Explorer : VCS_View_Access);
   --  ???

   procedure Create_Model (VCS_View : access VCS_View_Record'Class);
   --  Creates the underlying tree model for VCS_View.

   procedure Set_Column_Types (Explorer : access VCS_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Fill_Info
     (Explorer      : access VCS_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Status_Record : File_Status_Record;
      Success       : out Boolean);
   --  Fills the tree info at the given Iter with values from
   --  Status_Record.
   --  Success tells whether the information has been filled or not.

   function Get_Iter_From_Name
     (Explorer : access VCS_View_Record'Class;
      Name     : String) return Gtk_Tree_Iter;
   --  Return the Iter associated with the given name.
   --  Name is a base file name.
   --  Return Null_Iter if no such iter was found.

   ---------------
   -- Callbacks --
   ---------------

   procedure Change_Hide_Up_To_Date
     (Item     : access Gtk_Check_Menu_Item_Record'Class;
      Explorer : VCS_View_Access);
   --  Callback for toggling of "Hide up-to-date files".

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
       Event   : Gdk_Event)
      return Boolean;
   --  Callback for the "button_press" event.

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : VCS_View_Access) is
   begin
      if Explorer /= null then
         Clear (Explorer.Model);
         File_Status_List.Free (Explorer.Stored_Status);
      end if;
   end Clear;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : VCS_View_Access) is
      use File_Status_List;

      L        : List_Node := First (Explorer.Stored_Status);
      Iter     : Gtk_Tree_Iter;
      Success  : Boolean;

   begin
      Clear (Explorer.Model);

      while L /= Null_Node loop
         if not Explorer.Hide_Up_To_Date
           or else (Data (L).Status /= Up_To_Date
                    and then Data (L).Status /= Unknown)
         then
            Append (Explorer.Model, Iter, Null_Iter);
            Fill_Info (Explorer, Iter, Data (L), Success);

            if not Success then
               Remove (Explorer.Model, Iter);
            end if;
         end if;

         L := Next (L);
      end loop;

      Columns_Autosize (Explorer.Tree);
   end Refresh;

   -------------------------
   -- Display_File_Status --
   -------------------------

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False)
   is
      use File_Status_List;

      Child         : MDI_Child
        := Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);
      Explorer      : VCS_View_Access;
      Cache_Temp    : List_Node;
      Status_Temp   : List_Node := First (Status);
      Found         : Boolean := False;

   begin
      if Child = null then
         return;
      else
         Explorer := VCS_View_Access (Get_Widget (Child));
      end if;

      Push_State (Kernel, Busy);

      while Status_Temp /= Null_Node loop
         Cache_Temp := First (Explorer.Cached_Status);
         Found      := False;

         while not Found
           and then Cache_Temp /= Null_Node
         loop
            if String_List.Head (Data (Status_Temp).File_Name)
              = String_List.Head (Data (Cache_Temp).File_Name)
            then
               --  We have found an entry in the cache with the corresponding
               --  information.

               Found := True;

               if Override_Cache then
                  --  Enter the new file information into the cache.

                  Set_Data (Cache_Temp, Copy_File_Status (Data (Status_Temp)));
               end if;

               exit;
            end if;

            Cache_Temp := Next (Cache_Temp);
         end loop;

         --  If the status for this file was not found in the cache,
         --  add the information to the cache.

         if not Found then
            Prepend (Explorer.Cached_Status,
                     Copy_File_Status (Data (Status_Temp)));
            Cache_Temp := First (Explorer.Cached_Status);
         end if;

         --  The info that we want to display is now in Data (Cache_Temp),
         --  if it already exists in Explorer.Stored_Status, we simply modify
         --  the element, otherwise we add it to the list.

         declare
            New_Status         : File_Status_Record :=
              Copy_File_Status (Data (Cache_Temp));
            New_File_Name      : String :=
              String_List.Head (New_Status.File_Name);
            Temp_Stored_Status : File_Status_List.List_Node :=
              File_Status_List.First (Explorer.Stored_Status);
            Iter               : Gtk_Tree_Iter := Null_Iter;
            Success            : Boolean;

         begin
            Found := False;

            while not Found
              and then Temp_Stored_Status /= Null_Node
            loop
               if New_File_Name =
                 String_List.Head (Data (Temp_Stored_Status).File_Name)
               then
                  Found := True;
                  Set_Data (Temp_Stored_Status, New_Status);
                  Iter := Get_Iter_From_Name
                    (Explorer, String_List.Head (New_Status.File_Name));
               end if;

               Temp_Stored_Status := Next (Temp_Stored_Status);
            end loop;

            if not Found
              and then Force_Display
            then
               Prepend (Explorer.Stored_Status, New_Status);
            end if;

            if not (Explorer.Hide_Up_To_Date
                    and then (New_Status.Status = Up_To_Date
                              or else New_Status.Status = Unknown))
            then
               if Iter = Null_Iter
                 and then Force_Display
               then
                  Append (Explorer.Model, Iter, Null_Iter);
               end if;

               if Iter /= Null_Iter then
                  Fill_Info (Explorer, Iter, New_Status, Success);
               end if;
            else
               if Iter /= Null_Iter then
                  Remove (Explorer.Model, Iter);
               end if;
            end if;
         end;

         Status_Temp := Next (Status_Temp);
      end loop;

      Pop_State (Kernel);
   end Display_File_Status;

   -------------------------
   -- Display_String_List --
   -------------------------

   procedure Display_String_List
     (Kernel   : Kernel_Handle;
      List     : String_List.List;
      M_Type   : Message_Type := Verbose)
   is
      use String_List;

      Temp_List : List_Node := First (List);
   begin
      while Temp_List /= Null_Node loop
         Push_Message
           (Kernel, M_Type, "   "  & Data (Temp_List));
         Temp_List := Next (Temp_List);
      end loop;
   end Display_String_List;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer      : access VCS_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Status_Record : File_Status_Record;
      Success       : out Boolean) is
   begin
      Success := True;

      if String_List.Is_Empty (Status_Record.File_Name)
        or else GNAT.OS_Lib.Is_Directory
                 (String_List.Head (Status_Record.File_Name))
      then
         Success := False;
         return;
      end if;

      Set (Explorer.Model, Iter, Name_Column,
           String_List.Head (Status_Record.File_Name));

      Set (Explorer.Model, Iter, Base_Name_Column,
           Base_Name (String_List.Head (Status_Record.File_Name)));

      if not String_List.Is_Empty (Status_Record.Working_Revision) then
         Set (Explorer.Model, Iter, Local_Rev_Column,
              String_List.Head (Status_Record.Working_Revision));
      else
         Set (Explorer.Model, Iter, Local_Rev_Column, -"n/a");
      end if;

      if not String_List.Is_Empty (Status_Record.Repository_Revision) then
         Set (Explorer.Model, Iter, Rep_Rev_Column,
              String_List.Head (Status_Record.Repository_Revision));
      else
         Set (Explorer.Model, Iter, Rep_Rev_Column, -"n/a");
      end if;

      case Status_Record.Status is
         when Unknown =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Unknown_Pixbuf));
         when Not_Registered =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Not_Registered_Pixbuf));
         when Up_To_Date =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Up_To_Date_Pixbuf));
         when Removed =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Removed_Pixbuf));
         when Modified =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Modified_Pixbuf));
         when Needs_Merge =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Needs_Merge_Pixbuf));
         when Needs_Update =>
            Set (Explorer.Model, Iter, Status_Pixbuf_Column,
                 C_Proxy (Status_Needs_Update_Pixbuf));
      end case;

      Set (Explorer.Model, Iter, Status_Description_Column,
           File_Status'Image (Status_Record.Status));
   end Fill_Info;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access VCS_View_Record'Class;
      Name     : String) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Get_Iter_First (Explorer.Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Explorer.Model, Iter, Name_Column) = Name then
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
         pragma Unreferenced (Model, Path);
      begin
         String_List.Append
           (Result, Get_String (Data.Model, Iter, Name_Column));
      end Add_Selected_Item;

   begin
      if Explorer = null then
         return Result;
      end if;

      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Explorer.Tree),
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
      Console.Insert
        (Kernel, Message, Highlight_Sloc => False, Mode => M_Type);
   end Push_Message;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_View_Record'Class) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Dummy         : Gint;

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Explorer.Tree, True);

      Gtk_New (Col);
      Set_Title (Col, -"Status");
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Status_Pixbuf_Column);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, Status_Description_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"File name");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, Base_Name_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Local rev.");
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

   procedure Create_Model (VCS_View : access VCS_View_Record'Class) is
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

   ----------------------------
   -- Change_Hide_Up_To_Date --
   ----------------------------

   procedure Change_Hide_Up_To_Date
     (Item     : access Gtk_Check_Menu_Item_Record'Class;
      Explorer : VCS_View_Access)
   is
      pragma Unreferenced (Item);
   begin
      Explorer.Hide_Up_To_Date := not Explorer.Hide_Up_To_Date;
      Refresh (Explorer);
   end Change_Hide_Up_To_Date;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event)
     return Boolean
   is
      Menu     : Gtk_Menu;
      Check    : Gtk_Check_Menu_Item;
      Mitem    : Gtk_Menu_Item;
      Context  : File_Selection_Context_Access :=
        new File_Selection_Context;

      Files    : String_List.List;
      Explorer : VCS_View_Access := VCS_View_Access (View);
      Kernel   : Kernel_Handle := Explorer.Kernel;

   begin
      if Get_Button (Event) = 1 then
         return False;
      end if;

      Gtk_New (Menu);

      Files := Get_Selected_Files (Explorer);

      --  If there is no selection, select the item under the cursor.
      if String_List.Is_Empty (Files) then
         declare
            X         : Gdouble := Get_X (Event);
            Y         : Gdouble := Get_Y (Event);
            Buffer_X  : Gint;
            Buffer_Y  : Gint;
            Row_Found : Boolean;
            Path      : Gtk_Tree_Path;
            Column    : Gtk_Tree_View_Column := null;
            Iter      : Gtk_Tree_Iter;
         begin
            Path := Gtk_New;
            Get_Path_At_Pos
              (Explorer.Tree,
               Gint (X),
               Gint (Y),
               Path,
               Column,
               Buffer_X,
               Buffer_Y,
               Row_Found);

            if Path /= null then
               Select_Path (Get_Selection (Explorer.Tree), Path);
               Iter := Get_Iter (Explorer.Model, Path);
               Path_Free (Path);

               String_List.Append
                 (Files, Get_String (Explorer.Model, Iter, Name_Column));
            end if;
         end;
      end if;

      if not String_List.Is_Empty (Files) then
         declare
            First_File : String := String_List.Head (Files);
         begin
            Set_Context_Information
              (Context,
               Kernel,
               VCS_Module_ID);
            Set_File_Information
              (Context,
               Directory => Dir_Name (First_File),
               File_Name => Base_Name (First_File));
            VCS_Contextual_Menu (Explorer, Context, Menu);

            Gtk_New (Mitem);
            Append (Menu, Mitem);
         end;
      end if;

      String_List.Free (Files);

      Gtk_New (Check, Label => -"Hide up-to-date files");
      Set_Active (Check, Explorer.Hide_Up_To_Date);
      Append (Menu, Check);

      Check_VCS_View_Handler.Connect
        (Check, "activate",
         Check_VCS_View_Handler.To_Marshaller (Change_Hide_Up_To_Date'Access),
         Explorer);
      Grab_Focus (Explorer);
      Show_All (Menu);
      Popup (Menu);

      return True;
   end Button_Press;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (VCS_View : access VCS_View_Record'Class;
      Kernel   : Kernel_Handle)
   is
      Vbox1           : Gtk_Vbox;
      Hbox1           : Gtk_Hbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Selection       : Gtk_Tree_Selection;

   begin
      Init_Graphics;
      Initialize_Hbox (VCS_View);

      VCS_View.Kernel := Kernel;

      Gtk_New_Vbox (Vbox1, False, 0);
      Pack_Start (VCS_View, Vbox1);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Vbox1, Hbox1, True, True, 3);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Automatic);
      Pack_Start (Hbox1, Scrolledwindow1, True, True, 3);

      Create_Model (VCS_View);

      Gtk_New (VCS_View.Tree, VCS_View.Model);
      Selection := Get_Selection (VCS_View.Tree);
      Set_Mode (Selection, Gtk.Enums.Selection_Multiple);
      Add (Scrolledwindow1, VCS_View.Tree);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (VCS_View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press'Access),
         VCS_View,
         After => False);

      Set_Column_Types (VCS_View);
   end Initialize;

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer (Kernel : Kernel_Handle) return VCS_View_Access is
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);

      if Child = null then
         return null;
      else
         return VCS_View_Access (Get_Widget (Child));
      end if;
   end Get_Explorer;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Kernel : access Kernel_Handle_Record'Class)
     return VCS_Access
   is
      pragma Unreferenced (Kernel);
   begin
      return Get_VCS_From_Id ("CVS");
      --  ??? should get this information from the project !!
   end Get_Current_Ref;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle)
     return String_List.List
   is
      Result         : String_List.List;
      Focused_Child  : MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
      Explorer_Child : MDI_Child
        := Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);
   begin
      if Explorer_Child = Focused_Child
        and then Explorer_Child /= null
      then
         Result := Get_Selected_Files
           (VCS_View_Access (Get_Widget (Explorer_Child)));
      elsif Get_Current_File (Kernel) /= "" then
         String_List.Append (Result, Get_Current_File (Kernel));
      end if;

      return Result;
   end Get_Selected_Files;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Kernel : access Kernel_Handle_Record'Class)
     return String
   is
      Context : Selection_Context_Access := Get_Current_Context (Kernel);
      File    : File_Selection_Context_Access := null;
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

   function Get_Current_File (Kernel : Kernel_Handle) return String is
      Context : Selection_Context_Access := Get_Current_Context (Kernel);
      File    : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);
         if Has_File_Information (File) then
            return Directory_Information (File) & File_Information (File);
         end if;
      end if;

      return "";
   end Get_Current_File;

   -------------------------
   -- Get_Dirs_In_Project --
   -------------------------

   function Get_Dirs_In_Project
     (Kernel : Kernel_Handle) return String_List.List
   is
      Result   : String_List.List;
      Project  : Project_Node_Id;
   begin
      Project := Get_Project (Kernel);

      declare
         Iterator : Imported_Project_Iterator := Start (Project, True);
      begin
         while Current (Iterator) /= Empty_Node loop
            String_List.Concat (Result,
                                String_Array_To_String_List
                                (Source_Dirs (Current (Iterator))));
            Next (Iterator);
         end loop;
      end;

      return Result;
   end Get_Dirs_In_Project;

   --------------------------
   -- Get_Files_In_Project --
   --------------------------

   function Get_Files_In_Project
     (Project : Project_Node_Id) return String_List.List
   is
      Result  : String_List.List;
      Files   : String_Array_Access;
   begin
      Files   := Get_Source_Files (Project, True);

      for J in reverse Files.all'Range loop
         String_List.Prepend (Result, Files.all (J).all);
      end loop;

      Free (Files);

      return Result;
   end Get_Files_In_Project;

   ---------------------------------
   -- String_Array_To_String_List --
   ---------------------------------

   function String_Array_To_String_List
     (S : String_Id_Array) return String_List.List
   is
      Result : String_List.List;
   begin
      for J in reverse S'Range loop
         String_List.Prepend (Result, Get_String (S (J)));
      end loop;

      return Result;
   end String_Array_To_String_List;

end VCS_View_Pkg;
