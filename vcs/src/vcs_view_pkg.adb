-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Glib.Values; use Glib.Values;
with Glib.Object; use Glib.Object;

with Gdk.Event;  use Gdk.Event;
with Gdk.Pixbuf; use Gdk.Pixbuf;

with Gtk;                       use Gtk;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                 use Gtk.Enums;
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

with Log_Editor_Window_Pkg;     use Log_Editor_Window_Pkg;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with VCS;

with VCS_View_Pixmaps;          use VCS_View_Pixmaps;
with VCS_View_API;              use VCS_View_API;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Intl;                use Glide_Intl;

with Basic_Types;               use Basic_Types;

with Prj;                       use Prj;
with Prj_API;                   use Prj_API;
with Prj.Tree;                  use Prj.Tree;

package body VCS_View_Pkg is

   ------------------
   --  Local types --
   ------------------

   type Log_Parameter is record
      Explorer   : VCS_View_Access;
      Kernel     : Kernel_Handle;
      Log_Editor : Log_Editor_Window_Access;
      VCS_Ref    : VCS_Access;
   end record;
   --  This type is a convenience type used to pass parameters to some
   --  callbacks.

   package Explorer_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Log_Parameter);

   type Iter_Action is access
     procedure (Explorer : access VCS_View_Record'Class;
                Iter     : Gtk_Tree_Iter);
   --  Any action that occurs on one row in the tree view.

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
   Log_Column                : constant := 6;
   Log_Editor_Column         : constant := 7;
   Log_Editable_Column       : constant := 8;

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
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type,
         Log_Column                => GType_String,
         Log_Editor_Column         => GType_Object,
         Log_Editable_Column       => GType_Boolean);
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

   procedure Foreach_Selected_File
     (Explorer : access VCS_View_Record'Class;
      Action   : Iter_Action);
   --  Run the Action for each of the selected file.

   function Get_Iter_From_Name
     (Explorer : access VCS_View_Record'Class;
      Name     : String) return Gtk_Tree_Iter;
   --  Return the Iter associated with the given name.
   --  Name is a base file name.
   --  Return Null_Iter if no such iter was found.

   ---------------
   -- Callbacks --
   ---------------

   procedure Log_Editor_Text_Changed
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Log_Parameter);
   --  ???

   procedure Log_Editor_Ok_Clicked
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Log_Parameter);
   --  ???

   procedure Log_Editor_Close
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Log_Parameter);
   --  ???

   procedure Edited_Callback
     (Object      : access Gtk_Widget_Record'Class;
      Params      : Glib.Values.GValues);
   --  ???

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
      L        : File_Status_List.List := Explorer.Stored_Status;
      Iter     : Gtk_Tree_Iter;
      Success  : Boolean;
   begin
      Clear (Explorer.Model);

      while not Is_Empty (L) loop
         if not Explorer.Hide_Up_To_Date
           or else (Head (L).Status /= Up_To_Date
                    and then Head (L).Status /= Unknown)
         then
            Append (Explorer.Model, Iter, Null_Iter);
            Fill_Info (Explorer, Iter, Head (L), Success);

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
      Override_Cache : Boolean)
   is
      use File_Status_List;

      Child         : MDI_Child
        := Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);
      Explorer      : VCS_View_Access;
      Cache_Temp    : File_Status_List.List;
      Status_Temp   : File_Status_List.List := Status;
      Found         : Boolean := False;

      function Copy_File_Status
        (F : in File_Status_Record) return File_Status_Record;

      function Copy_String_List
        (S : in String_List.List) return String_List.List;

      function Copy_String_List
        (S : in String_List.List) return String_List.List
      is
         use String_List;
         Result : String_List.List;
         Temp   : String_List.List := S;
      begin
         while not Is_Empty (Temp) loop
            Append (Result, Head (Temp));
            Temp := Next (Temp);
         end loop;

         return Result;
      end Copy_String_List;

      function Copy_File_Status
        (F : in File_Status_Record) return File_Status_Record
      is
         Result : File_Status_Record;
      begin
         Result.File_Name := Copy_String_List (F.File_Name);
         Result.Working_Revision := Copy_String_List (F.Working_Revision);
         Result.Repository_Revision
           := Copy_String_List (F.Repository_Revision);
         Result.Tags := Copy_String_List (F.Tags);
         Result.Users := Copy_String_List (F.Users);
         Result.Status := F.Status;
         return Result;
      end Copy_File_Status;

   begin
      if Child = null then
         return;
      else
         Explorer := VCS_View_Access (Get_Widget (Child));
      end if;

      while not Is_Empty (Status_Temp) loop
         Cache_Temp := Explorer.Cached_Status;
         Found      := False;

         while not Found
           and then not Is_Empty (Cache_Temp)
         loop
            if String_List.Head (Head (Status_Temp).File_Name)
              = String_List.Head (Head (Cache_Temp).File_Name)
            then
               --  We have found an entry in the cache with the corresponding
               --  information.

               Found := True;

               if Override_Cache then
                  --  Enter the new file information into the cache.
                  Replace_Head (Cache_Temp,
                                Copy_File_Status (Head (Status_Temp)));
               end if;

               exit;
            end if;

            Cache_Temp := Next (Cache_Temp);
         end loop;

         --  If the status for this file was not found in the cache,
         --  add the information to the cache.

         if not Found then
            Prepend (Explorer.Cached_Status,
                     Copy_File_Status (Head (Status_Temp)));
            Cache_Temp := Explorer.Cached_Status;
         end if;

         --  The info that we want to display is now in Head (Cache_Temp),
         --  if it already exists in Explorer.Stored_Status, we simply modify
         --  the element, otherwise we add it to the list.

         declare
            New_Status         : File_Status_Record
              := Copy_File_Status (Head (Cache_Temp));
            New_File_Name      : String
              := String_List.Head (New_Status.File_Name);
            Temp_Stored_Status : File_Status_List.List
              := Explorer.Stored_Status;
            Iter               : Gtk_Tree_Iter := Null_Iter;
            Success            : Boolean;
         begin
            Found := False;

            while not Found
              and then not Is_Empty (Temp_Stored_Status)
            loop
               if New_File_Name =
                 String_List.Head (Head (Temp_Stored_Status).File_Name)
               then
                  Found := True;
                  Replace_Head (Temp_Stored_Status, New_Status);
                  Iter := Get_Iter_From_Name
                    (Explorer, String_List.Head (New_Status.File_Name));
               end if;

               Temp_Stored_Status := Next (Temp_Stored_Status);
            end loop;

            if not Found then
               Prepend (Explorer.Stored_Status, New_Status);
            end if;

            if not (Explorer.Hide_Up_To_Date
                    and then (New_Status.Status = Up_To_Date
                              or else New_Status.Status = Unknown))
            then
               if Iter = Null_Iter then
                  Append (Explorer.Model, Iter, Null_Iter);
               end if;

               Fill_Info (Explorer, Iter, New_Status, Success);
            else
               if Iter /= Null_Iter then
                  Remove (Explorer.Model, Iter);
               end if;
            end if;
         end;

         Status_Temp := Next (Status_Temp);
      end loop;
   end Display_File_Status;

   -------------------------
   -- Display_String_List --
   -------------------------

   procedure Display_String_List
     (Kernel   : Kernel_Handle;
      List     : String_List.List;
      M_Type   : Message_Type := Verbose)
   is
      Temp_List : String_List.List := List;
   begin
      while not String_List.Is_Empty (Temp_List) loop
         Push_Message
           (Kernel, M_Type, "   "  & String_List.Head (Temp_List));
         Temp_List := String_List.Next (Temp_List);
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
        or else Is_Directory (String_List.Head (Status_Record.File_Name))
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
      Set (Explorer.Model, Iter, Log_Column, "");
      Set (Explorer.Model, Iter, Log_Editor_Column, GObject' (null));
      Set (Explorer.Model, Iter, Log_Editable_Column, True);
   end Fill_Info;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access VCS_View_Record'Class;
      Name     : String) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Get_Iter_Root (Explorer.Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Explorer.Model, Iter, Name_Column) = Name then
            return Iter;
         end if;

         Next (Explorer.Model, Iter);
      end loop;

      return Null_Iter;
   end Get_Iter_From_Name;

   ---------------------------
   -- Foreach_Selected_File --
   ---------------------------

   procedure Foreach_Selected_File
     (Explorer : access VCS_View_Record'Class;
      Action   : Iter_Action)
   is
      procedure On_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access);
      --  Launch the Action for one item.

      procedure On_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access)
      is
         pragma Unreferenced (Model, Path);
      begin
         Action (Data, Iter);
      end On_Selected_Item;

   begin
      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Explorer.Tree),
         On_Selected_Item'Unrestricted_Access,
         Explorer_Selection_Foreach.Data_Type_Access (Explorer));
   end Foreach_Selected_File;

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

   -----------------------------
   -- Log_Editor_Text_Changed --
   -----------------------------

   procedure Log_Editor_Text_Changed
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Log_Parameter)
   is
      pragma Unreferenced (Object);
      Temp_Path : String_List.List := Parameter.Log_Editor.Files;
      Iter      : Gtk_Tree_Iter;

   begin
      while not String_List.Is_Empty (Temp_Path) loop
         Iter := Get_Iter_From_Name
           (Parameter.Explorer,
            String_List.Head (Temp_Path));

         if Iter /= Null_Iter then
            Set (Parameter.Explorer.Model, Iter, Log_Column,
                 Get_Text (Parameter.Log_Editor));
         end if;

         Temp_Path := String_List.Next (Temp_Path);
      end loop;
   end Log_Editor_Text_Changed;

   ---------------------------
   -- Log_Editor_Ok_Clicked --
   ---------------------------

   procedure Log_Editor_Ok_Clicked
     (Object    : access Gtk_Widget_Record'Class;
      Parameter : Log_Parameter)
   is
      pragma Unreferenced (Object);

      Logs       : String_List.List;
      Files_Copy : String_List.List;
      Files_Temp : String_List.List := Parameter.Log_Editor.Files;
   begin
      while not String_List.Is_Empty (Files_Temp) loop
         String_List.Append (Logs, Get_Text (Parameter.Log_Editor));
         String_List.Append (Files_Copy, String_List.Head (Files_Temp));
         Files_Temp := String_List.Next (Files_Temp);
      end loop;

      Commit (Parameter.VCS_Ref,
              Files_Copy,
              Logs);
      Get_Status (Parameter.VCS_Ref,
                  Files_Copy);

      Close (Parameter.Log_Editor);
   end Log_Editor_Ok_Clicked;

   ----------------------
   -- Log_Editor_Close --
   ----------------------

   procedure Log_Editor_Close
     (Object      : access Gtk_Widget_Record'Class;
      Parameter   : Log_Parameter)
   is
      pragma Unreferenced (Object);
      Value      : GValue;
      Iter       : Gtk_Tree_Iter;
      Temp_Paths : String_List.List := Parameter.Log_Editor.Files;

   begin
      if Parameter.Explorer /= null then
         Init (Value, GType_String);

         while not String_List.Is_Empty (Temp_Paths) loop
            Iter := Get_Iter_From_Name
              (Parameter.Explorer,
               String_List.Head (Temp_Paths));

            if Iter /= Null_Iter then
               Set (Parameter.Explorer.Model,
                    Iter, Log_Editor_Column, GObject' (null));
            end if;

            Temp_Paths := String_List.Next (Temp_Paths);
         end loop;
      end if;
   end Log_Editor_Close;

   --------------
   -- Edit_Log --
   --------------

   procedure Edit_Log
     (Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;
      Files    : String_List.List;
      Ref      : VCS_Access)
   is
      Log_Editor       : Log_Editor_Window_Access;
      Parameter_Object : Log_Parameter;

      procedure Create_And_Launch_Log_Editor
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter);
      --  Creates a log editor for the given row,
      --  displays it, and connects the necessary callbacks.

      procedure Create_And_Launch_Log_Editor
        (Explorer : access VCS_View_Record'Class;
         Iter     : Gtk_Tree_Iter)
      is
         Stored_Object : GObject;
         Child         : MDI_Child;
         File_Name     : String
           := Get_String (Explorer.Model, Iter, Name_Column);
      begin
         Stored_Object := Get_Object (Explorer.Model, Iter, Log_Editor_Column);

         if Stored_Object = null then
            Gtk_New (Log_Editor);
            Parameter_Object.Log_Editor := Log_Editor;

            Set_Title (Log_Editor,
                       -"Log editor : "
                       & Base_Name (File_Name));

            Add_File_Name (Log_Editor, File_Name);

            Set (Explorer.Model,
                 Iter, Log_Editor_Column,
                 GObject (Log_Editor));

            Set_Text (Log_Editor,
                      Get_String (Explorer.Model, Iter, Log_Column));

            Explorer_Callback.Connect
              (Log_Editor.Ok_Button,
               "clicked",
               Explorer_Callback.To_Marshaller (Log_Editor_Ok_Clicked'Access),
               Parameter_Object);

            Explorer_Callback.Connect
              (Log_Editor.Log_Text,
               "insert_text",
               Explorer_Callback.To_Marshaller
                (Log_Editor_Text_Changed'Access),
               Parameter_Object,
               After => True);

            Explorer_Callback.Connect
              (Log_Editor.Log_Text,
               "destroy",
               Explorer_Callback.To_Marshaller (Log_Editor_Close'Access),
               Parameter_Object);

            if Explorer.Kernel = null then
               Show_All (Log_Editor);
            else
               Child := Put (Get_MDI (Explorer.Kernel), Log_Editor);
            end if;
         end if;
      end Create_And_Launch_Log_Editor;

      Temp_Files : String_List.List := Files;
      Child      : MDI_Child;

   begin
      pragma Assert (Ref /= null);

      Parameter_Object.Explorer := VCS_View_Access (Explorer);
      Parameter_Object.Kernel := Kernel;
      Parameter_Object.VCS_Ref := Ref;

      if Explorer /= null then
         Foreach_Selected_File
           (Explorer, Create_And_Launch_Log_Editor'Unrestricted_Access);
      else
         while not String_List.Is_Empty (Temp_Files) loop
            Gtk_New (Log_Editor);
            Parameter_Object.Log_Editor := Log_Editor;

            Set_Title
              (Log_Editor, -"Log editor : "
               & Base_Name (String_List.Head (Temp_Files)));
            Add_File_Name (Log_Editor, String_List.Head (Temp_Files));
            Set_Text (Log_Editor, "");

            Explorer_Callback.Connect
              (Log_Editor.Ok_Button,
               "clicked",
               Explorer_Callback.To_Marshaller (Log_Editor_Ok_Clicked'Access),
               Parameter_Object);

            if Kernel = null then
               Show_All (Log_Editor);
            else
               Child := Put (Get_MDI (Kernel), Log_Editor);
            end if;

            Temp_Files := String_List.Next (Temp_Files);
         end loop;
      end if;
   end Edit_Log;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Explorer      : constant VCS_View_Access := VCS_View_Access (Object);
      Iter          : Gtk_Tree_Iter;
      Path_String   : String := Get_String (Nth (Params, 1));
      Text_String   : String := Get_String (Nth (Params, 2));
      Text_Value    : GValue := Nth (Params, 2);
      Log_Editor    : Log_Editor_Window_Access;
      Stored_Object : GObject;

   begin
      Iter := Get_Iter_From_String (Explorer.Model, Path_String);

      if Iter = Null_Iter then
         return;
      end if;

      Set_Value (Explorer.Model, Iter, Log_Column, Text_Value);

      Stored_Object := Get_Object (Explorer.Model, Iter, Log_Editor_Column);

      if Stored_Object /= null then
         Log_Editor := Log_Editor_Window_Access (Stored_Object);
         Set_Text (Log_Editor, Text_String);
      end if;
   end Edited_Callback;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_View_Record'Class) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Editable_Rend : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Dummy         : Gint;

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Editable_Rend);
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
      Set_Title (Col, -"Local file name");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, Base_Name_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Local revision");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Local_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Repository revision");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Rep_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Log");
      Pack_Start (Col, Editable_Rend, True);
      Add_Attribute (Col, Editable_Rend, "text", Log_Column);
      Add_Attribute (Col, Editable_Rend, "editable", Log_Editable_Column);

      Dummy := Append_Column (Explorer.Tree, Col);

      Widget_Callback.Object_Connect
        (Editable_Rend,
         "edited",
         Edited_Callback'Access,
         Gtk_Widget (Explorer));
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
      Context  : File_Selection_Context_Access := new File_Selection_Context;

      Files    : String_List.List;
      Explorer : VCS_View_Access := VCS_View_Access (View);
      Kernel   : Kernel_Handle := Explorer.Kernel;

   begin
      if Get_Button (Event) = 1 then
         return False;
      end if;

      Gtk_New (Menu);

      Files := Get_Selected_Files (Kernel);

      if not String_List.Is_Empty (Files) then
         declare
            First_File : String := String_List.Head (Files);
         begin
            Set_Context_Information
              (Context,
               Kernel,
               Get_Creator (Get_Current_Explorer_Context (Kernel)));
            Set_File_Name_Information
              (Context,
               Dir_Name (First_File),
               First_File);
            Set_File_Information
              (Context,
               Get_Project_From_File (Get_Project_View (Kernel), First_File));
            VCS_Contextual_Menu (Explorer, Context, Menu);

            Gtk_New (Mitem);
            Append (Menu, Mitem);
         end;
      end if;

      Destroy (Context.all);
      String_List.Free (Files);

      Gtk_New (Check, Label => -"Hide up-to-date files");
      Set_Active (Check, Explorer.Hide_Up_To_Date);
      Append (Menu, Check);

      Check_VCS_View_Handler.Connect
        (Check, "activate",
         Check_VCS_View_Handler.To_Marshaller (Change_Hide_Up_To_Date'Access),
         Explorer);
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
      Set_Policy (Scrolledwindow1, Policy_Automatic, Policy_Automatic);
      Pack_Start (Hbox1, Scrolledwindow1, True, True, 3);

      Create_Model (VCS_View);

      Gtk_New (VCS_View.Tree, VCS_View.Model);
      Selection := Get_Selection (VCS_View.Tree);
      Set_Mode (Selection, Selection_Multiple);
      Add (Scrolledwindow1, VCS_View.Tree);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (VCS_View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         VCS_View);

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
      Context : Selection_Context_Access :=
        Get_Current_Explorer_Context (Kernel);
      File    : File_Selection_Context_Access := null;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File) then
            return Directory_Information (File);
         end if;
      end if;

      return Get_Current_Dir;
   end Get_Current_Dir;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File (Kernel : Kernel_Handle) return String is
      Context : Selection_Context_Access :=
        Get_Current_Explorer_Context (Kernel);
      File    : File_Selection_Context_Access := null;
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
