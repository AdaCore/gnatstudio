-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

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

with VCS;

with VCS_View_Pixmaps;          use VCS_View_Pixmaps;
with VCS_View_API;              use VCS_View_API;
with VCS_Module;                use VCS_Module;

with Log_Utils;                 use Log_Utils;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Intl;                use Glide_Intl;

with Basic_Types;               use Basic_Types;
with Traces;                    use Traces;

package body VCS_View_Pkg is

   Me : constant Debug_Handle := Create ("VCS_INTERFACE");

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
         Log_Column                => Gdk.Pixbuf.Get_Type);
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
      Name     : String) return Gtk_Tree_Iter;
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

   ---------------
   -- Callbacks --
   ---------------

   procedure Change_Hide_Up_To_Date
     (Item     : access Gtk_Check_Menu_Item_Record'Class;
      Explorer : VCS_View_Access);
   --  Callback for toggling of "Hide up-to-date files".

   procedure Change_Hide_Not_Registered
     (Item     : access Gtk_Check_Menu_Item_Record'Class;
      Explorer : VCS_View_Access);
   --  Callback for toggling of "Hide not registered files".

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event)
      return Boolean;
   --  Callback for the "button_press" event.

   function On_Delete
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event)
     return Boolean;
   --  Callback for the "delete_event" signal

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Record)
   is
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
            Free (Page.Stored_Status);
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
      pragma Unreferenced (Event);
      The_View : constant VCS_View_Access := VCS_View_Access (View);
      Page     : VCS_Page_Access;
   begin
      for J in 1 .. The_View.Number_Of_Pages loop
         Page := VCS_Page_Access
           (Get_Nth_Page (The_View.Notebook, Gint (J - 1)));

         Free (Page.Stored_Status);
         Free (Page.Cached_Status);
      end loop;

      return False;
   end On_Delete;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : VCS_View_Access) is
      L        : List_Node;
      Iter     : Gtk_Tree_Iter;
      Success  : Boolean;
      Page     : VCS_Page_Access;

   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (Explorer.Notebook,
                       Get_Current_Page (Explorer.Notebook)));

      Scroll_To_Point (Page.Tree, 0, 0);

      Clear (Page.Model);

      L := First (Page.Stored_Status);

      while L /= Null_Node loop
         if not (Explorer.Hide_Up_To_Date
                   and then (Data (L).Status.Status = Up_To_Date
                               or else Data (L).Status.Status = Unknown))
           and then not (Explorer.Hide_Not_Registered
                           and then Data (L).Status.Status = Not_Registered)
         then
            Append (Page.Model, Iter, Null_Iter);
            Fill_Info (Page, Iter, Data (L), Success);

            if not Success then
               Remove (Page.Model, Iter);
            end if;
         end if;

         L := Next (L);
      end loop;

      Columns_Autosize (Page.Tree);
   end Refresh;

   -----------------
   -- Refresh_Log --
   -----------------

   procedure Refresh_Log
     (Explorer : access VCS_View_Record;
      File     : String)
   is
      Page          : VCS_Page_Access;
      List_Temp     : List_Node;
      Log           : Boolean;
      Found         : Boolean;
      Iter          : Gtk_Tree_Iter;
   begin
      if Get_Log_From_File (Explorer.Kernel, File, False) = "" then
         Log := False;
      else
         Log := True;
      end if;

      for J in 1 .. Explorer.Number_Of_Pages loop
         Page := VCS_Page_Access
           (Get_Nth_Page (Explorer.Notebook, Gint (J - 1)));

         --  Refresh the information in the cache.

         List_Temp := First (Page.Cached_Status);
         Found := False;

         while List_Temp /= Null_Node and then not Found loop
            if File = String_List.Head
              (Data (List_Temp).Status.File_Name)
            then
               --  The data was found in the list, override it.

               Set_Data
                 (List_Temp,
                    (Copy_File_Status
                       (Data (List_Temp).Status), Log));

               Found := True;
            end if;

            List_Temp := Next (List_Temp);
         end loop;

         if Found then
            List_Temp := First (Page.Stored_Status);

            while List_Temp /= Null_Node loop
               if File = String_List.Head
                 (Data (List_Temp).Status.File_Name)
               then
                  --  The data was found in the list, override it.
                  --  and refresh the model if needed.

                  Set_Data
                    (List_Temp,
                       (Copy_File_Status
                          (Data (List_Temp).Status), Log));

                  Iter := Get_Iter_From_Name (Page, File);

                  if Iter /= Null_Iter then
                     Fill_Info (Page, Iter, Data (List_Temp), Found);
                  end if;
               end if;

               List_Temp := Next (List_Temp);
            end loop;
         end if;

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
      Clear_Logs     : Boolean := False)
   is
      Child         : constant MDI_Child :=
        Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);
      Explorer      : VCS_View_Access;
      Cache_Temp    : List_Node;
      Status_Temp   : File_Status_List.List_Node
        := File_Status_List.First (Status);
      Found         : Boolean := False;
      Page          : VCS_Page_Access;
      Log           : Boolean;

      use type File_Status_List.List_Node;
   begin
      --  Free the logs associated to the files that are up-to-date.

      if Clear_Logs then
         while Status_Temp /= File_Status_List.Null_Node loop
            declare
               S      : constant File_Status_Record :=
                 File_Status_List.Data (Status_Temp);
               File   : constant String := String_List.Head (S.File_Name);
            begin
               if S.Status = Up_To_Date then
                  declare
                     Log   : constant String
                       := Get_Log_From_File (Kernel, File, False);
                     Dummy : Boolean;
                  begin
                     if Log /= ""
                       and then GNAT.OS_Lib.Is_Regular_File (Log)
                     then
                        GNAT.OS_Lib.Delete_File (Log, Dummy);
                        Close_File_Editors (Kernel, Log);
                     end if;

                     Remove_File_From_Mapping (Kernel, File);
                  end;
               end if;

            exception
               when E : others =>
                  Trace (Me, "Unexpected exception: "
                           & Exception_Information (E));
            end;

            Status_Temp := File_Status_List.Next (Status_Temp);
         end loop;

         Status_Temp := File_Status_List.First (Status);
      end if;

      if Child = null then
         return;
      else
         Explorer := VCS_View_Access (Get_Widget (Child));
      end if;

      Page := Get_Page_For_Identifier (Explorer, VCS_Identifier);
      Set_Current_Page (Explorer.Notebook,
                        Page_Num (Explorer.Notebook, Page));

      Push_State (Kernel, Busy);

      while Status_Temp /= File_Status_List.Null_Node loop
         Cache_Temp := First (Page.Cached_Status);
         Found      := False;

         declare
            File : constant String := String_List.Head
              (File_Status_List.Data (Status_Temp).File_Name);
         begin
            while not Found
              and then Cache_Temp /= Null_Node
            loop
               if File
                 = String_List.Head (Data (Cache_Temp).Status.File_Name)
               then
                  --  We have found an entry in the cache with the
                  --  corresponding information.

                  Found := True;

                  if Override_Cache then
                     --  Enter the new file information into the cache.

                     if Get_Log_From_File (Kernel, File, False) = "" then
                        Log := False;
                     else
                        Log := True;
                     end if;

                     Set_Data
                       (Cache_Temp,
                          (Copy_File_Status
                             (File_Status_List.Data (Status_Temp)), Log));
                  end if;

                  exit;
               end if;

               Cache_Temp := Next (Cache_Temp);
            end loop;

            --  If the status for this file was not found in the cache,
            --  add the information to the cache.

            if not Found then
               if Get_Log_From_File (Kernel, File, False) = "" then
                  Log := False;
               else
                  Log := True;
               end if;

               Prepend (Page.Cached_Status,
                          (Copy_File_Status
                             (File_Status_List.Data (Status_Temp)), Log));
               Cache_Temp := First (Page.Cached_Status);
            end if;
         end;

         --  The info that we want to display is now in Data (Cache_Temp),
         --  if it already exists in Page.Stored_Status, we simply modify
         --  the element, otherwise we add it to the list.

         declare
            New_Status         : constant Line_Record :=
              Copy (Data (Cache_Temp));
            New_File_Name      : constant String :=
              String_List.Head (New_Status.Status.File_Name);
            Temp_Stored_Status : List_Node := First (Page.Stored_Status);
            Iter               : Gtk_Tree_Iter := Null_Iter;
            Success            : Boolean;
         begin
            Found := False;

            while not Found
              and then Temp_Stored_Status /= Null_Node
            loop
               if New_File_Name =
                 String_List.Head (Data (Temp_Stored_Status).Status.File_Name)
               then
                  Found := True;
                  Set_Data (Temp_Stored_Status, New_Status);
                  Iter := Get_Iter_From_Name
                    (Page, String_List.Head (New_Status.Status.File_Name));
               end if;

               Temp_Stored_Status := Next (Temp_Stored_Status);
            end loop;

            if not Found
              and then Force_Display
            then
               Prepend (Page.Stored_Status, New_Status);
            end if;

            if not (Explorer.Hide_Up_To_Date
                    and then (New_Status.Status.Status = Up_To_Date
                              or else New_Status.Status.Status = Unknown))
              and then not (Explorer.Hide_Not_Registered
                            and then New_Status.Status.Status
                              = Not_Registered)
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
         end;

         Status_Temp := File_Status_List.Next (Status_Temp);
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

      Temp_List : String_List.List_Node := String_List.First (List);
   begin
      while Temp_List /= String_List.Null_Node loop
         Push_Message
           (Kernel, M_Type, "   "  & String_List.Data (Temp_List));
         Temp_List := String_List.Next (Temp_List);
      end loop;
   end Display_String_List;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer      : access VCS_Page_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Line_Info     : Line_Record;
      Success       : out Boolean) is
   begin
      Success := True;

      if String_List.Is_Empty (Line_Info.Status.File_Name)
        or else GNAT.OS_Lib.Is_Directory
        (String_List.Head (Line_Info.Status.File_Name))
      then
         Success := False;
         return;
      end if;

      if Line_Info.Log then
         Set (Explorer.Model, Iter, Log_Column,
              C_Proxy (Status_Up_To_Date_Pixbuf));
      else
         Set (Explorer.Model, Iter, Log_Column,
              C_Proxy (Status_Not_Registered_Pixbuf));
      end if;

      Set (Explorer.Model, Iter, Name_Column,
           String_List.Head (Line_Info.Status.File_Name));

      Set (Explorer.Model, Iter, Base_Name_Column,
           Base_Name (String_List.Head (Line_Info.Status.File_Name)));

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

      case Line_Info.Status.Status is
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
           File_Status'Image (Line_Info.Status.Status));
   end Fill_Info;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access VCS_Page_Record'Class;
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
      Console.Insert
        (Kernel, Message, Highlight_Sloc => False, Mode => M_Type);
   end Push_Message;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Explorer : access VCS_Page_Record'Class) is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;

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
      Set_Title (Col, -"Log");
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Log_Column);
      Set_Clickable (Col, True);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"File name");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, Base_Name_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

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

   --------------------------------
   -- Change_Hide_Not_Registered --
   --------------------------------

   procedure Change_Hide_Not_Registered
     (Item     : access Gtk_Check_Menu_Item_Record'Class;
      Explorer : VCS_View_Access)
   is
      pragma Unreferenced (Item);
   begin
      Explorer.Hide_Not_Registered := not Explorer.Hide_Not_Registered;
      Refresh (Explorer);
   end Change_Hide_Not_Registered;

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
      Explorer : constant VCS_View_Access := VCS_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Page     : VCS_Page_Access;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

      function Get_Path_At_Event return Gtk_Tree_Path;
      --  Return the path at which Event has occured.
      --  User must free memory associated to the returned path.

      function Get_Path_At_Event return Gtk_Tree_Path is
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
           (Page.Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         return Path;
      end Get_Path_At_Event;

   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (Explorer.Notebook,
                       Get_Current_Page (Explorer.Notebook)));

      if Get_Event_Type (Event) = Gdk_2button_Press then
         Path := Get_Path_At_Event;

         if Path /= null then
            Iter := Get_Iter (Page.Model, Path);
            Open_File_Editor
              (Kernel,
               Get_String (Page.Model, Iter, Name_Column));
         end if;

         return True;
      end if;

      if Get_Button (Event) = 1 then
         Free (Selection_Context_Access (Context));
         return False;
      end if;

      Gtk_New (Menu);

      --  If there is no selection, select the item under the cursor.
      Path := Get_Path_At_Event;

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

      if not String_List.Is_Empty (Files) then
         declare
            First_File : constant String := String_List.Head (Files);
         begin
            Set_Context_Information
              (Context,
               Kernel,
               VCS_Module_ID);
            Set_File_Information
              (Context,
               Directory => Dir_Name (First_File),
               File_Name => Base_Name (First_File));

            Set_Current_Context (Explorer, Selection_Context_Access (Context));
            VCS_Contextual_Menu (Explorer, Explorer.Context, Menu);

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

      Gtk_New (Check, Label => -"Hide non registered files");
      Set_Active (Check, Explorer.Hide_Not_Registered);
      Append (Menu, Check);
      Check_VCS_View_Handler.Connect
        (Check, "activate",
         Check_VCS_View_Handler.To_Marshaller
         (Change_Hide_Not_Registered'Access),
         Explorer);

      Grab_Focus (Explorer);
      Show_All (Menu);
      Popup (Menu);

      Free (Selection_Context_Access (Context));
      return True;
   end Button_Press;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_edited" signal.

   procedure File_Edited_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      Explorer : constant VCS_View_Access := VCS_View_Access (Widget);
      Log      : constant String := Get_String (Nth (Args, 1));

   begin
      if Log'Length > 4
        and then Log (Log'Last - 3 .. Log'Last) = "$log"
      then
         declare
            File        : constant String := Get_File_From_Log (Kernel, Log);
            Page        : VCS_Page_Access;
            Cache_Node  : List_Node;
            Stored_Node : List_Node;
         begin
            Browse_Files :
            for J in 1 .. Explorer.Number_Of_Pages loop
               Page := VCS_Page_Access
                 (Get_Nth_Page (Explorer.Notebook, Gint (J - 1)));

               Cache_Node := First (Page.Cached_Status);

               while Cache_Node /= Null_Node loop
                  if String_List.Head
                    (Data (Cache_Node).Status.File_Name) = File
                  then
                     --  The file was found in the cache, update it.
                     Set_Data (Cache_Node,
                               (Copy_File_Status (Data (Cache_Node).Status),
                                True));

                     Stored_Node := First (Page.Stored_Status);

                     while Stored_Node /= Null_Node loop
                        if String_List.Head
                          (Data (Stored_Node).Status.File_Name) = File
                        then
                           Set_Data (Stored_Node,
                                     (Copy_File_Status
                                        (Data (Stored_Node).Status),
                                      True));
                           Refresh (Explorer);
                           return;
                        end if;

                        Stored_Node := Next (Stored_Node);
                     end loop;

                     return;
                  end if;

                  Cache_Node := Next (Cache_Node);
               end loop;

            end loop Browse_Files;
         end;
      end if;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (VCS_View : access VCS_View_Record'Class;
      Kernel   : Kernel_Handle)
   is
      Vbox1           : Gtk_Vbox;
      Dummy_Page      : VCS_Page_Access;

   begin
      Init_Graphics;
      Initialize_Hbox (VCS_View);

      VCS_View.Kernel := Kernel;

      Gtk_New_Vbox (Vbox1, False, 0);
      Pack_Start (VCS_View, Vbox1);

      Gtk_New (VCS_View.Notebook);
      Set_Show_Tabs (VCS_View.Notebook, False);
      Pack_Start (Vbox1, VCS_View.Notebook);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (VCS_View,
         "delete_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (On_Delete'Access),
         VCS_View,
         After => False);

      declare
         VCS_List : constant GNAT.OS_Lib.Argument_List :=
           Get_VCS_List (VCS_Module_ID);
      begin
         for J in VCS_List'Range loop
            Dummy_Page := Get_Page_For_Identifier
              (VCS_View, Get_VCS_From_Id (VCS_List (J).all));
         end loop;
      end;

      Kernel_Callback.Object_Connect
        (Kernel,
         File_Edited_Signal,
         File_Edited_Cb'Access,
         VCS_View,
         Kernel);
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

      Page.Reference := Identifier;

      Create_Model (Page);

      Gtk_New (Page.Tree, Page.Model);


      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Automatic);
      Pack_Start (Page, Scrolledwindow1, True, True, 3);

      Selection := Get_Selection (Page.Tree);
      Set_Mode (Selection, Gtk.Enums.Selection_Multiple);
      Add (Scrolledwindow1, Page.Tree);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Page.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press'Access),
         Explorer,
         After => False);

      Set_Column_Types (Page);

      Gtk_New (Label, Name (Identifier));

      Append_Page (Explorer.Notebook, Page, Label);

      return Page;
   end Get_Page_For_Identifier;

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
     (Explorer : access VCS_View_Record)
     return VCS_Access
   is
      Page     : VCS_Page_Access;
   begin
      Page := VCS_Page_Access
        (Get_Nth_Page (Explorer.Notebook,
                       Get_Current_Page (Explorer.Notebook)));

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

      File_Info      : String_Access;
      Directory_Info : String_Access;
   begin
      if Context.all in File_Selection_Context'Class then
         Result := new File_Selection_Context;
         File := File_Selection_Context_Access (Context);

         Set_Context_Information
           (Result,
            Get_Kernel (Context),
            Get_Creator (Context));

         if Has_File_Information (File) then
            File_Info := new String'(File_Information (File));
         else
            File_Info := new String'("");
         end if;

         if Has_Directory_Information (File) then
            Directory_Info := new String'(Directory_Information (File));
         else
            Directory_Info := new String'("");
         end if;

         Set_File_Information
           (File_Selection_Context_Access (Result),
            Directory_Info.all,
            File_Info.all,
            Project_Information (File));

         Free (File_Info);
         Free (Directory_Info);
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
      return Copy_Context (Explorer.Context);
   end Get_Current_Context;

   -------------------------
   -- Set_Current_Context --
   -------------------------

   procedure Set_Current_Context
     (Explorer : access VCS_View_Record;
      Context  : Selection_Context_Access)
   is
   begin
      if Explorer.Context /= null then
         Free (Explorer.Context);
         Explorer.Context := null;
      end if;

      Explorer.Context := Copy_Context (Context);
   end Set_Current_Context;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle)
     return String_List.List
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
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
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
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
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

end VCS_View_Pkg;
