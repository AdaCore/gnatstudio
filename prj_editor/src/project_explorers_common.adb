-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Calendar;              use Ada.Calendar;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Strings;              use GNAT.Strings;

with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Glib.Convert;              use Glib.Convert;

with Basic_Types;               use Basic_Types;
with Entities;                  use Entities;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with HTables;
with Language.Unknown;          use Language.Unknown;
with Language.Icons;            use Language.Icons;
with Language_Handlers;         use Language_Handlers;
with Projects.Registry;         use Projects, Projects.Registry;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with Namet;                     use Namet;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Project_Explorers_Common is

   Me : constant Debug_Handle := Create ("Project_Explorers_Common");

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column          => Gdk.Pixbuf.Get_Type,
         Absolute_Name_Column => GType_String,
         Base_Name_Column     => GType_String,
         Node_Type_Column     => GType_Int,
         User_Data_Column     => GType_Pointer,
         Line_Column          => GType_Int,
         Column_Column        => GType_Int,
         Project_Column       => GType_Int,
         Category_Column      => GType_Int,
         Up_To_Date_Column    => GType_Boolean,
         Entity_Base_Column   => GType_String,
         Timestamp_Column     => GType_Int);
   end Columns_Types;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics (Widget : Gtk_Widget) is

      function R (Id : String) return Gdk_Pixbuf;
      --  Convenience function: create the Gdk_Pixbuf from stock Id

      -------
      -- R --
      -------

      function R (Id : String) return Gdk_Pixbuf is
      begin
         return Render_Icon (Widget, Id, Icon_Size_Menu);
      end R;

   begin
      --  If initialization has already been done, exit
      if Open_Pixbufs (Project_Node) /= null then
         return;
      end if;

      Language.Icons.Init_Graphics (Widget);

      Open_Pixbufs (Project_Node)  := R ("gps-project-open");
      Close_Pixbufs (Project_Node) := R ("gps-project-closed");

      Open_Pixbufs (Modified_Project_Node)  := R ("gps-project-modified-open");
      Close_Pixbufs (Modified_Project_Node) :=
        R ("gps-project-modified-closed");

      --  ??? Would be nice to have different pixbufs for these
      Open_Pixbufs (Extends_Project_Node)  := R ("gps-project-open");
      Close_Pixbufs (Extends_Project_Node) := R ("gps-project-closed");

      Open_Pixbufs (Directory_Node)  := R ("gps-folder-open");
      Close_Pixbufs (Directory_Node) := R ("gps-folder-closed");
      Open_Pixbufs (Obj_Directory_Node)  := R ("gps-folder-obj-open");
      Close_Pixbufs (Obj_Directory_Node) := R ("gps-folder-obj-closed");

      Open_Pixbufs (Exec_Directory_Node)  := R ("gps-folder-exec-open");
      Close_Pixbufs (Exec_Directory_Node) := R ("gps-folder-exec-closed");
      Open_Pixbufs (File_Node)  := R ("gps-file");
      Close_Pixbufs (File_Node) := R ("gps-file");

      Open_Pixbufs (Category_Node)  := R ("gps-box");
      Close_Pixbufs (Category_Node) := R ("gps-box");
   end Init_Graphics;

   -----------------
   -- Append_File --
   -----------------

   procedure Append_File
     (Kernel : Kernel_Handle;
      Model  : Gtk_Tree_Store;
      Base   : Gtk_Tree_Iter;
      File   : GNATCOLL.VFS.Virtual_File;
      Sorted : Boolean := False)
   is
      Iter  : Gtk_Tree_Iter;
      Iter2 : Gtk_Tree_Iter;
      Lang  : Language_Access;
      Done  : Boolean;
   begin
      if Sorted then
         Iter := Children (Model, Base);
         Done := False;

         while Iter /= Null_Iter loop
            Iter2 := Iter;

            if Get_Node_Type (Model, Iter) = File_Node then
               declare
                  Name : constant String := Get_Base_Name (Model, Iter);
               begin
                  if Name > File.Base_Name then
                     Insert_Before (Model, Iter2, Base, Iter);
                     Iter := Iter2;
                     Done := True;

                     exit;
                  end if;
               end;
            end if;

            Next (Model, Iter);
         end loop;

         if not Done then
            Append (Model, Iter, Base);
         end if;
      else

         Append (Model, Iter, Base);
      end if;

      Set (Model, Iter, Absolute_Name_Column, Full_Name (File).all);
      Set (Model, Iter, Base_Name_Column, Base_Name (File));
      Set (Model, Iter, Icon_Column, C_Proxy (Close_Pixbufs (File_Node)));
      Set (Model, Iter, Node_Type_Column, Gint (Node_Types'Pos (File_Node)));
      Set (Model, Iter, Up_To_Date_Column, False);

      Lang := Get_Language_From_File
        (Language_Handler (Get_Language_Handler (Kernel)), File);

      if Lang /= Unknown_Lang then
         Append_Dummy_Iter (Model, Iter);
      end if;
   end Append_File;

   -----------------------
   -- Append_Dummy_Iter --
   -----------------------

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter)
   is
      Iter : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Base);
      Set_Node_Type (Model, Iter, Entity_Node, Expanded => False);
   end Append_Dummy_Iter;

   --------------------------
   -- Append_Category_Node --
   --------------------------

   function Append_Category_Node
     (Model         : Gtk_Tree_Store;
      File          : GNATCOLL.VFS.Virtual_File;
      Category      : Language_Category;
      Category_Name : Strings.String_Access;
      Parent_Iter   : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      Name    : constant String :=
                  Language.Category_Name (Category, Category_Name);
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
   begin
      Sibling := Children (Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Model, Sibling, Base_Name_Column) <= Name
         loop
            Next (Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Model, N, Parent_Iter);
         else
            Insert_Before (Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Model, N, Absolute_Name_Column, Display_Full_Name (File));
      Set (Model, N, Base_Name_Column, Locale_To_UTF8 (Name));
      Set (Model, N, Icon_Column,
           C_Proxy (Entity_Icons (False, Visibility_Public) (Category)));
      Set (Model, N, Node_Type_Column, Gint (Node_Types'Pos (Category_Node)));
      Set (Model, N, Up_To_Date_Column, True);
      Set (Model, N, Category_Column, Language_Category'Pos (Category));

      return N;
   end Append_Category_Node;

   --------------------
   -- Entity_Name_Of --
   --------------------

   function Entity_Name_Of
     (Construct          : Construct_Information;
      Show_Profiles      : Boolean;
      Max_Profile_Length : Positive := Positive'Last) return String
   is

      function Escape return String;
      pragma Inline (Escape);
      --  Escape Construct.Name.all as a pango markup string.
      --  The characters which need to be escaped in pango markup language are
      --  '&', '<', '>', '\', and '"'.
      --  The code here assumes that Entity names, in any language, can only
      --  contain '&', '<', '>', or '"'  and that if it does, one of these
      --  characters is necessarily in the first position, for the overloading
      --  of operators such as '<' or '&&', or for a quoted name.

      ------------
      -- Escape --
      ------------

      function Escape return String is
         C : Character;
      begin
         if Construct.Name.all = "" then
            return "";
         end if;

         C := Construct.Name (Construct.Name'First);

         if C = '"' or else C = '&' or else C = '<' or else C = '>' then
            return Escape_Text (Construct.Name.all);
         else
            return Construct.Name.all;
         end if;
      end Escape;

      Name : constant String := Reduce (Escape);

   begin
      if Show_Profiles and then Construct.Profile /= null then
         return Name & " <span foreground=""#555555"">"
           & Reduce (Construct.Profile.all, Max_Profile_Length) & "</span>";
      else
         return Name;
      end if;
   end Entity_Name_Of;

   --------------------
   -- Entity_Icon_Of --
   --------------------

   function Entity_Icon_Of
     (Construct : Construct_Information) return Gdk_Pixbuf is
   begin
      return Entity_Icons
        (Construct.Is_Declaration, Construct.Visibility) (Construct.Category);
   end Entity_Icon_Of;

   ------------------------
   -- Append_Entity_Node --
   ------------------------

   function Append_Entity_Node
     (Model       : Gtk_Tree_Store;
      File        : GNATCOLL.VFS.Virtual_File;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
   begin
      Sibling := Children (Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Model, Sibling, Base_Name_Column)
           <= Construct.Name.all
         loop
            Next (Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Model, N, Parent_Iter);
         else
            Insert_Before (Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Model, N, Absolute_Name_Column, Display_Full_Name (File));
      Set (Model, N, Base_Name_Column, Entity_Name_Of (Construct, True));
      Set (Model, N, Entity_Base_Column, Reduce (Construct.Name.all));
      Set (Model, N, Icon_Column, C_Proxy (Entity_Icon_Of (Construct)));
      Set (Model, N, Node_Type_Column, Gint (Node_Types'Pos (Entity_Node)));
      Set (Model, N, Line_Column, Gint (Construct.Sloc_Entity.Line));
      Set (Model, N, Column_Column, Gint (Construct.Sloc_Entity.Column));
      Set (Model, N, Up_To_Date_Column, True);
      return N;
   end Append_Entity_Node;

   ----------------------
   -- Append_File_Info --
   ----------------------

   procedure Append_File_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter;
      File_Name : GNATCOLL.VFS.Virtual_File)
   is
      Languages  : constant Language_Handler :=
                     Language_Handler (Get_Language_Handler (Kernel));

      N, N2      : Gtk_Tree_Iter;
      Iter       : Gtk_Tree_Iter;

      Lang       : Language_Access;
      Constructs : Construct_List;
      Category   : Language_Category;

      package Iter_Map is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Gtk_Tree_Iter,
         Hash            => HTables.String_Hash,
         Equivalent_Keys => "=");
      use Iter_Map;

      Categories : Iter_Map.Map;
      Handler    : LI_Handler;

      Node_Appended : Boolean := False;

   begin
      --  Mark the file information as up-to-date

      Set (Model, Node, Timestamp_Column,
           Gint (File_Time_Stamp (File_Name) - GNATCOLL.Utils.No_Time));

      --  Remove any previous information for this file

      N := Children (Model, Node);

      while N /= Null_Iter loop
         N2 := N;
         Next (Model, N);
         Remove (Model, N2);
      end loop;

      Handler := Get_LI_Handler_From_File (Languages, File_Name);

      if Handler = null then
         return;
      end if;

      Push_State (Kernel, Busy);

      Lang := Get_Language_From_File (Languages, File_Name);

      if Lang /= null then
         Parse_File_Constructs
           (Handler, Languages, File_Name, Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Category := Filter_Category (Constructs.Current.Category);

               if Category /= Cat_Unknown
                 and then Category /= Cat_Parameter
                 and then Category /= Cat_Field
               then
                  declare
                     Name     : constant String :=
                                  Category_Name (Category,
                                                 Constructs.Current.
                                                   Category_Name);
                     Cursor   : Iter_Map.Cursor;
                     New_Iter : Gtk_Tree_Iter;

                  begin
                     Cursor := Iter_Map.Find (Categories, Name);

                     if Cursor = No_Element then
                        New_Iter :=
                           Append_Category_Node
                             (Model,
                              File_Name,
                              Category      => Category,
                              Category_Name =>
                                Constructs.Current.Category_Name,
                              Parent_Iter   => Node);
                        Insert (Categories, Name, New_Iter);

                     else
                        New_Iter := Element (Cursor);
                     end if;

                     N := Append_Entity_Node
                       (Model, File_Name, Constructs.Current.all, New_Iter);
                  end;

                  Node_Appended := True;
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         --  If no node was appended, add a "no entity" node

         if not Node_Appended then
            Append (Model, Iter, Node);
            Set (Model, Iter, Base_Name_Column,
                 "<span foreground=""#555555"">"
                 & (-"(no entity)")
                 & "</span>");
            Set (Model, Iter, Node_Type_Column,
                 Gint (Node_Types'Pos (Category_Node)));
         end if;

         Free (Constructs);
      else
         Trace (Me, "No known language for " & Full_Name (File_Name).all);
      end if;

      Pop_State (Kernel);
   end Append_File_Info;

   ---------------------
   -- Filter_Category --
   ---------------------

   function Filter_Category
     (Category : Language_Category) return Language_Category is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      if Category in Dependency_Category
        or else Category in Construct_Category
        or else Category = Cat_Representation_Clause
        or else Category = Cat_Local_Variable
      then
         return Cat_Unknown;

         --  All subprograms are grouped together

      elsif Category in Subprogram_Explorer_Category then
         return Cat_Procedure;

      elsif Category in Type_Category then
         return Cat_Type;

      end if;

      return Category;
   end Filter_Category;

   --------------
   -- Dnd_Data --
   --------------

   function Dnd_Data
     (Child : access MDI_Explorer_Child_Record; Copy : Boolean)
      return Gtkada.MDI.MDI_Child
   is
      C : MDI_Child;
   begin
      if Child.Dnd_From_File = GNATCOLL.VFS.No_File then
         --  So that we can move the explorer itself
         return MDI_Child (Child);

      else
         if Copy then
            C := Find_MDI_Child_By_Name
              (Get_MDI (Child.Kernel), Full_Name (Child.Dnd_From_File).all);
         end if;

         if Copy and then C /= null then
            return Dnd_Data (C, Copy => True);
         else
            Open_File_Editor
              (Child.Kernel, Child.Dnd_From_File, Line => 0, Column => 0);
         end if;

         return Get_Focus_Child (Get_MDI (Child.Kernel));
      end if;
   end Dnd_Data;

   -------------------------
   -- Child_Drag_Finished --
   -------------------------

   procedure Child_Drag_Finished (Child : access MDI_Explorer_Child_Record) is
   begin
      --  So that we can also move the explorer itself
      Child.Dnd_From_File := GNATCOLL.VFS.No_File;
   end Child_Drag_Finished;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Kernel    : Kernel_Handle;
      Child     : access MDI_Explorer_Child_Record'Class;
      Tree      : access Gtk_Tree_View_Record'Class;
      Model     : Gtk_Tree_Store;
      Event     : Gdk_Event;
      Add_Dummy : Boolean) return Boolean
   is
      Iter         : Gtk_Tree_Iter;
      Path         : Gtk_Tree_Path;
      Line, Column : Gint;
   begin
      if Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (Tree, Model, Event);

         if Iter /= Null_Iter then
            Path := Get_Path (Model, Iter);
            Set_Cursor (Tree, Path, null, False);
            Path_Free (Path);

            case Node_Types'Val
              (Integer (Get_Int (Model, Iter, Node_Type_Column)))
            is

               when Directory_Node | Project_Node | Category_Node =>
                  Cancel_Child_Drag (Child);

                  if Get_Event_Type (Event) = Gdk_2button_Press then
                     declare
                        Path    : Gtk_Tree_Path;
                        Success : Boolean;
                        pragma Unreferenced (Success);
                     begin
                        Path := Get_Path (Model, Iter);

                        if Row_Expanded (Tree, Path) then
                           Success := Collapse_Row (Tree, Path);
                        else
                           if Add_Dummy then
                              Append_Dummy_Iter (Model, Iter);
                           end if;

                           Success := Expand_Row (Tree, Path, False);
                        end if;

                        Path_Free (Path);
                     end;
                  end if;

                  return False;

               when File_Node =>
                  if Get_Event_Type (Event) = Gdk_2button_Press
                    or else Get_Event_Type (Event) = Gdk_3button_Press
                  then
                     Cancel_Child_Drag (Child);
                     Open_File_Editor
                       (Kernel,
                        Create
                          (Full_Filename =>
                             Get_String (Model, Iter, Absolute_Name_Column)),
                        Line   => 0,
                        Column => 0);
                     return True;

                  elsif Get_Event_Type (Event) = Button_Press then
                     --  Drag-and-drop does not work on floating MDI children

                     if Get_State (Child) /= Floating then
                        Child.Kernel        := Kernel;
                        Child.Dnd_From_File := Create
                          (Full_Filename =>
                             Get_String (Model, Iter, Absolute_Name_Column));

                        Child_Drag_Begin (Child, Event);
                     end if;
                     return False;

                  else
                     Cancel_Child_Drag (Child);
                  end if;

               when Entity_Node =>
                  Cancel_Child_Drag (Child);

                  if Get_Event_Type (Event) = Button_Release then
                     Line := Get_Int (Model, Iter, Line_Column);
                     Column := Get_Int (Model, Iter, Column_Column);

                     Open_File_Editor
                       (Kernel,
                        Create
                          (Full_Filename =>
                             Get_String (Model, Iter, Absolute_Name_Column)),
                        Line   => Natural (Line),
                        Column => Visible_Column_Type (Column));
                  end if;
                  return False;

               when others =>
                  Cancel_Child_Drag (Child);
                  return False;
            end case;

         end if;
      end if;

      return False;
   end On_Button_Press;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Kernel : Kernel_Handle;
      Tree   : access Gtk_Tree_View_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      use type Gdk.Types.Gdk_Key_Type;

      Iter         : Gtk_Tree_Iter;
      Line, Column : Gint;
      Model        : Gtk_Tree_Model;

   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);

      if Iter = Null_Iter then
         return False;
      end if;

      if Get_Key_Val (Event) = GDK_Return then
         case Node_Types'Val
           (Integer (Get_Int (Model, Iter, Node_Type_Column))) is

         when File_Node =>
            Open_File_Editor
              (Kernel,
               Create
                 (Full_Filename =>
                  Get_String (Model, Iter, Absolute_Name_Column)),
               Line   => 0,
               Column => 0);

         when Entity_Node =>
            Line := Get_Int (Model, Iter, Line_Column);
            Column := Get_Int (Model, Iter, Column_Column);

            Open_File_Editor
              (Kernel,
               Create
                 (Full_Filename =>
                  Get_String (Model, Iter, Absolute_Name_Column)),
               Line   => Natural (Line),
               Column => Visible_Column_Type (Column));

         when others =>
            null;
         end case;
      end if;

      return False;
   end On_Key_Press;

   -------------------
   -- Get_Node_Type --
   -------------------

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types is
   begin
      return
        Node_Types'Val
          (Integer (Get_Int (Model, Node, Node_Type_Column)));
   end Get_Node_Type;

   -------------------
   -- Set_Node_Type --
   -------------------

   procedure Set_Node_Type
     (Model    : Gtk_Tree_Store;
      Node     : Gtk_Tree_Iter;
      N_Type   : Node_Types;
      Expanded : Boolean) is
   begin
      Set (Model, Node, Node_Type_Column, Gint (Node_Types'Pos (N_Type)));

      if N_Type not in Category_Node .. Entity_Node then
         if Expanded then
            Set (Model, Node, Icon_Column, C_Proxy (Open_Pixbufs (N_Type)));
         else
            Set (Model, Node, Icon_Column, C_Proxy (Close_Pixbufs (N_Type)));
         end if;
      end if;
   end Set_Node_Type;

   -----------------------
   -- Get_Category_Type --
   -----------------------

   function Get_Category_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Language_Category is
   begin
      return
        Language_Category'Val
          (Integer (Get_Int (Model, Node, Category_Column)));
   end Get_Category_Type;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Boolean is
   begin
      case Get_Node_Type (Model, Node) is
         when File_Node =>
            declare
               --  ??? Virtual_File should be stored directly in the tree
               File : constant Virtual_File := Create
                 (Full_Filename =>
                    Get_String (Model, Node, Absolute_Name_Column));
            begin
               return Duration (Get_Int (Model, Node, Timestamp_Column)) +
                 GNATCOLL.Utils.No_Time =
                   File_Time_Stamp (File);
            end;

         when others =>
            return Get_Boolean (Model, Node, Up_To_Date_Column);
      end case;
   end Is_Up_To_Date;

   --------------------
   -- Set_Up_To_Date --
   --------------------

   procedure Set_Up_To_Date
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter;
      State : Boolean) is
   begin
      Set (Model, Node, Up_To_Date_Column, State);
   end Set_Up_To_Date;

   -------------------
   -- Get_Base_Name --
   -------------------

   function Get_Base_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return String is
   begin
      return Get_String (Model, Node, Base_Name_Column);
   end Get_Base_Name;

   -----------------------
   -- Get_Absolute_Name --
   -----------------------

   function Get_Absolute_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return String is
   begin
      return Get_String (Model, Node, Absolute_Name_Column);
   end Get_Absolute_Name;

   ------------------------
   -- Get_File_From_Node --
   ------------------------

   function Get_File_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File
   is
      Absolute : constant String := Get_Absolute_Name (Model, Node);
   begin
      if Absolute = "" then
         return GNATCOLL.VFS.No_File;
      else
         return Create (Full_Filename => Absolute);
      end if;
   end Get_File_From_Node;

   -----------------------------
   -- Get_Directory_From_Node --
   -----------------------------

   function Get_Directory_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return String
   is
      S : constant String := Get_Absolute_Name (Model, Node);
   begin
      if S = "" then
         return "";
      else
         if Get_Node_Type (Model, Node) = Directory_Node then
            return S;
         else
            return Dir_Name (S);
         end if;
      end if;
   end Get_Directory_From_Node;

   ---------------------------
   -- Get_Project_From_Node --
   ---------------------------

   function Get_Project_From_Node
     (Model     : Gtk_Tree_Store;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Node      : Gtk_Tree_Iter;
      Importing : Boolean) return Project_Type
   is
      Parent_Iter : Gtk_Tree_Iter;
      Node_Type   : Node_Types;
      Project     : Project_Type;
      N           : Name_Id;
   begin
      if Importing then
         Parent_Iter := Parent (Model, Node);

         if Parent_Iter = Null_Iter then
            return Get_Project (Kernel);
         end if;

      else
         Parent_Iter := Node;
      end if;

      while Parent_Iter /= Null_Iter loop
         Node_Type := Get_Node_Type (Model, Parent_Iter);

         exit when Node_Type = Project_Node
           or else Node_Type = Extends_Project_Node
           or else Node_Type = Modified_Project_Node;

         Parent_Iter := Parent (Model, Parent_Iter);
      end loop;

      if Parent_Iter /= Null_Iter then
         N := Name_Id (Get_Int (Model, Parent_Iter, Project_Column));
         Assert (Me, N /= No_Name,
                 "Get_Project_From_Node: no project found");
         Project := Get_Project_From_Name (Get_Registry (Kernel).all, N);

      else
         --  Should we fall back on Get_Project_From_File ?
         Project := No_Project;
      end if;

      return Project;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return No_Project;
   end Get_Project_From_Node;

   ---------------------
   -- Context_Factory --
   ---------------------

   procedure Context_Factory
     (Context : in out Selection_Context;
      Kernel  : Kernel_Handle;
      Tree    : access Gtk_Tree_View_Record'Class;
      Model   : Gtk_Tree_Store;
      Event   : Gdk_Event;
      Menu    : Gtk_Menu)
   is
      pragma Unreferenced (Menu);

      function Entity_Base (Name : String) return String;
      --  Return the "basename" for the entity, ie convert "parent.name" to
      --  "name", in the case of Ada parent packages.
      --  ??? Should this be done by the parser itself

      -----------------
      -- Entity_Base --
      -----------------

      function Entity_Base (Name : String) return String is
      begin
         --  ??? Should use standard UTF8 subprogams
         for C in reverse Name'Range loop
            if Name (C) = '.' then
               return Name (C + 1 .. Name'Last);
            end if;
         end loop;
         return Name;
      end Entity_Base;

      Iter      : constant Gtk_Tree_Iter :=
                    Find_Iter_For_Event (Tree, Model, Event);
      Node_Type : Node_Types;
      L         : Integer := 0;

   begin
      if Iter /= Null_Iter then
         Node_Type := Get_Node_Type (Model, Iter);
      else
         return;
      end if;

      if Node_Type = Entity_Node then
         Set_Entity_Information
           (Context       => Context,
            Entity_Name   => Entity_Base
              (Get_String (Model, Iter, Entity_Base_Column)),
            Entity_Column => Visible_Column_Type
              (Get_Int (Model, Iter, Column_Column)));
         L := Integer (Get_Int (Model, Iter, Line_Column));
      end if;

      if Node_Type = Project_Node
        or else Node_Type = Extends_Project_Node
        or else Node_Type = Modified_Project_Node
      then
         Set_File_Information
           (Context           => Context,
            Project           =>
              Get_Project_From_Node (Model, Kernel, Iter, False),
            Importing_Project =>
              Get_Project_From_Node (Model, Kernel, Iter, True));

      else
         Set_File_Information
           (Context      => Context,
            Files        => (1 => Get_File_From_Node (Model, Iter)),
            Project      =>
              Get_Project_From_Node (Model, Kernel, Iter, False),
            Importing_Project =>
              Get_Project_From_Node (Model, Kernel, Iter, True),
            Line         => L);
      end if;
   end Context_Factory;

end Project_Explorers_Common;
