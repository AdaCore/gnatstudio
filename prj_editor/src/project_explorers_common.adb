-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Basic_Types;               use Basic_Types;
with Pixmaps_IDE;               use Pixmaps_IDE;
with Pixmaps_Prj;               use Pixmaps_Prj;
with Glib.Convert;              use Glib.Convert;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Language.Unknown;          use Language.Unknown;
with Language;                  use Language;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with String_Utils;              use String_Utils;
with Src_Info;                  use Src_Info;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GUI_Utils;                 use GUI_Utils;
with Traces;                    use Traces;
with Projects.Registry;         use Projects, Projects.Registry;
with Ada.Exceptions;            use Ada.Exceptions;
with Types;                     use Types;
with VFS;                       use VFS;

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

   procedure Init_Graphics is
   begin
      --  If initialization has already been done, exit.
      if Open_Pixbufs (Project_Node) /= null then
         return;
      end if;

      Open_Pixbufs (Project_Node)  :=
        Gdk_New_From_Xpm_Data (project_xpm);
      Close_Pixbufs (Project_Node) :=
        Gdk_New_From_Xpm_Data (project_closed_xpm);
      Open_Pixbufs (Modified_Project_Node)  :=
        Gdk_New_From_Xpm_Data (project_modified_xpm);
      Close_Pixbufs (Modified_Project_Node) :=
        Gdk_New_From_Xpm_Data (project_modified_closed_xpm);
      Open_Pixbufs (Extends_Project_Node)  :=
        Gdk_New_From_Xpm_Data (project_ext_xpm);
      Close_Pixbufs (Extends_Project_Node) :=
        Gdk_New_From_Xpm_Data (project_ext_closed_xpm);
      Open_Pixbufs (Directory_Node)  :=
        Gdk_New_From_Xpm_Data (mini_ofolder_xpm);
      Close_Pixbufs (Directory_Node) :=
        Gdk_New_From_Xpm_Data (mini_folder_xpm);
      Open_Pixbufs (Obj_Directory_Node)  :=
        Gdk_New_From_Xpm_Data (mini_folder_object_xpm);
      Close_Pixbufs (Obj_Directory_Node) :=
        Gdk_New_From_Xpm_Data (mini_folder_object_xpm);
      Open_Pixbufs (Exec_Directory_Node)  :=
        Gdk_New_From_Xpm_Data (mini_folder_exec_xpm);
      Close_Pixbufs (Exec_Directory_Node) :=
        Gdk_New_From_Xpm_Data (mini_folder_exec_xpm);
      Open_Pixbufs (File_Node)  :=
        Gdk_New_From_Xpm_Data (mini_page_xpm);
      Close_Pixbufs (File_Node) :=
        Gdk_New_From_Xpm_Data (mini_page_xpm);
      Open_Pixbufs (Category_Node)  :=
        Gdk_New_From_Xpm_Data (var_xpm);
      Close_Pixbufs (Category_Node) :=
        Gdk_New_From_Xpm_Data (var_xpm);

   end Init_Graphics;

   -----------------
   -- Append_File --
   -----------------

   procedure Append_File
     (Kernel : Kernel_Handle;
      Model  : Gtk_Tree_Store;
      Base   : Gtk_Tree_Iter;
      File   : VFS.Virtual_File)
   is
      Iter : Gtk_Tree_Iter;
      Lang : Language_Access;
   begin
      Append (Model, Iter, Base);

      Set (Model, Iter, Absolute_Name_Column, Full_Name (File).all);
      Set (Model, Iter, Base_Name_Column, Base_Name (File).all);
      Set (Model, Iter, Icon_Column, C_Proxy (Close_Pixbufs (File_Node)));
      Set (Model, Iter, Node_Type_Column, Gint (Node_Types'Pos (File_Node)));
      Set (Model, Iter, Up_To_Date_Column, False);

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Kernel)), File);

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
     (Model       : Gtk_Tree_Store;
      File        : VFS.Virtual_File;
      Category    : Language_Category;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
      Name    : constant String := Category_Name (Category);
   begin
      Sibling := Children (Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Model, Sibling, Base_Name_Column)
           <= Name
         loop
            Next (Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Model, N, Parent_Iter);
         else
            Insert_Before (Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Model, N, Absolute_Name_Column, Locale_Full_Name (File));
      Set (Model, N, Base_Name_Column, Locale_To_UTF8 (Name));
      Set (Model, N, Icon_Column,
           C_Proxy (Close_Pixbufs (Category_Node)));
      Set (Model, N, Node_Type_Column,
           Gint (Node_Types'Pos (Category_Node)));
      Set (Model, N, Up_To_Date_Column, True);
      Set (Model, N, Category_Column, Language_Category'Pos (Category));

      return N;
   end Append_Category_Node;

   ------------------------
   -- Append_Entity_Node --
   ------------------------

   function Append_Entity_Node
     (Model       : Gtk_Tree_Store;
      File        : VFS.Virtual_File;
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

      Set (Model, N, Absolute_Name_Column, Locale_Full_Name (File));

      if Construct.Is_Declaration then
         if Construct.Profile /= null then
            Set (Model, N, Base_Name_Column,
                 Locale_To_UTF8 (Reduce (Construct.Name.all) & " (spec) " &
                                 Reduce (Construct.Profile.all)));
         else
            Set (Model, N, Base_Name_Column,
                 Locale_To_UTF8 (Reduce (Construct.Name.all) & " (spec)"));

         end if;

      elsif Construct.Profile /= null then
         Set (Model, N, Base_Name_Column,
              Locale_To_UTF8 (Reduce (Construct.Name.all & " " &
                                      Construct.Profile.all)));
      else
         Set (Model, N, Base_Name_Column,
              Locale_To_UTF8 (Reduce (Construct.Name.all)));
      end if;

      Set (Model, N, Entity_Base_Column,
           Locale_To_UTF8 (Reduce (Construct.Name.all)));

      Set (Model, N, Icon_Column,
           C_Proxy (Close_Pixbufs (Entity_Node)));
      Set (Model, N, Node_Type_Column,
           Gint (Node_Types'Pos (Entity_Node)));

      Set (Model, N,
           Line_Column, Gint (Construct.Sloc_Entity.Line));
      Set (Model, N,
           Column_Column, Gint (Construct.Sloc_Entity.Column));
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
      File_Name : VFS.Virtual_File)
   is
      use Src_Info;

      N, N2      : Gtk_Tree_Iter;

      Lang       : Language_Access;
      Constructs : Construct_List;
      Category   : Language_Category;

      type Gtk_Tree_Iter_Array is array (Language_Category'Range)
        of Gtk_Tree_Iter;
      Categories : Gtk_Tree_Iter_Array := (others => Null_Iter);
      Languages  : constant Glide_Language_Handler :=
        Glide_Language_Handler (Get_Language_Handler (Kernel));
      Handler    : Src_Info.LI_Handler;

   begin
      --  Mark the file information as up-to-date

      Set (Model, Node, Timestamp_Column,
           Gint (To_Timestamp (File_Time_Stamp (File_Name))));

      --  Remove any previous information for this file.

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

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Kernel)), File_Name);

      if Lang /= null then
         Parse_File_Constructs
           (Handler, Get_Project (Kernel),
            Languages, File_Name, Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Category := Filter_Category (Constructs.Current.Category);
               if Category /= Cat_Unknown then
                  if Categories (Category) = Null_Iter then
                     Categories (Category) := Append_Category_Node
                       (Model,
                        File_Name,
                        Category    => Category,
                        Parent_Iter => Node);
                  end if;

                  N := Append_Entity_Node
                    (Model, File_Name,
                     Constructs.Current.all, Categories (Category));
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

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

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Kernel    : Kernel_Handle;
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
                 (Integer (Get_Int (Model, Iter, Node_Type_Column))) is

               when Directory_Node | Project_Node | Category_Node =>
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

                           Success := Expand_Row
                             (Tree, Path, False);
                        end if;

                        Path_Free (Path);
                     end;
                  end if;

                  return False;

               when File_Node =>
                  if (Get_Event_Type (Event) = Gdk_2button_Press
                      or else Get_Event_Type (Event) = Gdk_3button_Press)
                  then
                     Open_File_Editor
                       (Kernel,
                        Create
                          (Full_Filename =>
                             Get_String (Model, Iter, Absolute_Name_Column)));
                     return True;
                  end if;

               when Entity_Node =>
                  if Get_Event_Type (Event) = Button_Release then
                     Line := Get_Int (Model, Iter, Line_Column);
                     Column := Get_Int (Model, Iter, Column_Column);

                     Open_File_Editor
                       (Kernel,
                        Create
                          (Full_Filename =>
                             Get_String (Model, Iter, Absolute_Name_Column)),
                        Line   => Natural (Line),
                        Column => Natural (Column));
                  end if;
                  return False;

               when others =>
                  return False;
            end case;

         end if;
      end if;

      return False;
   end On_Button_Press;

   -------------------
   -- Get_Node_Type --
   -------------------

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types is
   begin
      return
        Node_Types'Val
          (Integer
               (Get_Int (Model, Node, Node_Type_Column)));
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

      if Expanded then
         Set (Model, Node, Icon_Column,
              C_Proxy (Open_Pixbufs (N_Type)));
      else
         Set (Model, Node, Icon_Column,
              C_Proxy (Close_Pixbufs (N_Type)));
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
          (Integer
               (Get_Int (Model, Node, Category_Column)));
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
            return Timestamp (Get_Int (Model, Node, Timestamp_Column))
              =  To_Timestamp
                (File_Time_Stamp
                     (Get_String (Model, Node, Absolute_Name_Column)));

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
     (Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter)
      return VFS.Virtual_File
   is
      Absolute : constant String := Get_Absolute_Name (Model, Node);
   begin
      if Absolute = "" then
         return VFS.No_File;
      else
         return Create (Full_Filename => Absolute);
      end if;
   end Get_File_From_Node;

   -----------------------------
   -- Get_Directory_From_Node --
   -----------------------------

   function Get_Directory_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter)
      return String
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
      Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
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
         Project := Get_Project_From_Name (Get_Registry (Kernel), N);

      else
         --  Should we fall back on Get_Project_From_File ?
         Project := No_Project;
      end if;

      return Project;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return No_Project;
   end Get_Project_From_Node;

   ---------------------
   -- Context_Factory --
   ---------------------

   function Context_Factory
     (Kernel     : Kernel_Handle;
      Tree       : access Gtk_Tree_View_Record'Class;
      Model      : Gtk_Tree_Store;
      Event      : Gdk_Event;
      Menu       : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Menu);

      function Entity_Base (Name : String) return String;
      --  Return the "basename" for the entity, ie convert "parent.name" to
      --  "name", in the case of Ada parent packages.
      --  ??? Should this be done by the parser itself

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

      Iter        : constant Gtk_Tree_Iter := Find_Iter_For_Event
        (Tree, Model, Event);
      Context     : Selection_Context_Access;
      Node_Type   : Node_Types;
      L           : Integer := 0;

   begin
      if Iter /= Null_Iter then
         Node_Type := Get_Node_Type (Model, Iter);

      else
         return Context;
      end if;

      if Node_Type = Entity_Node then
         Context := new Entity_Selection_Context;
      else
         Context := new File_Selection_Context;
      end if;

      if Node_Type = Entity_Node then
         Set_Entity_Information
           (Context     => Entity_Selection_Context_Access (Context),
            Entity_Name => Entity_Base
              (Get_String (Model, Iter, Entity_Base_Column)),
            Entity_Column => Integer
              (Get_Int (Model, Iter, Column_Column)));
         L := Integer (Get_Int (Model, Iter, Line_Column));
      end if;

      Set_File_Information
        (Context      => File_Selection_Context_Access (Context),
         File         => Get_File_From_Node (Model, Iter),
         Project      => Get_Project_From_Node (Model, Kernel, Iter, False),
         Importing_Project =>
           Get_Project_From_Node (Model, Kernel, Iter, True),
         Line         => L);

      return Context;

   end Context_Factory;

end Project_Explorers_Common;
