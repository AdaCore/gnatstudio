-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gtk.Handlers;              use Gtk.Handlers;
with Interfaces.C;              use Interfaces.C;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Prj.Proc;                  use Prj.Proc;
with Prj.Tree;                  use Prj.Tree;
with Prj.PP;                    use Prj.PP;
with Namet;                     use Namet;
with Stringt;                   use Stringt;
with Snames;                    use Snames;
with Types;                     use Types;

with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Src_Info.ALI;

package body Glide_Kernel is

   Project_Changed_Signal      : constant String := "project_changed";
   Project_View_Changed_Signal : constant String := "project_view_changed";

   Signals : constant chars_ptr_array :=
     (1 => New_String (Project_Changed_Signal),
      2 => New_String (Project_View_Changed_Signal));
   --  The list of signals defined for this object

   Kernel_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this object

   package Object_Callback is new Gtk.Handlers.Callback
     (Glib.Object.GObject_Record);

   procedure Create_Default_Project
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create a default project file.
   --  ??? This should actually be read from an external file when we have a
   --  ??? full installation procedure for Glide

   ----------------------------
   -- Create_Default_Project --
   ----------------------------

   procedure Create_Default_Project
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Prj_Decl, Decl, Item, L, Term, Expr, Str2 : Project_Node_Id;
      Current_Dir : constant String := Get_Current_Dir;
      Current_Dir_Name : Name_Id;
   begin
      Kernel.Project := Default_Project_Node (N_Project);
      Kernel.Project_Is_Default := True;

      --  Adding the name of the project
      Name_Len := 7;
      Name_Buffer (1 .. Name_Len) := "default";
      Set_Name_Of (Kernel.Project, Name_Find);

      --  Adding the project path
      Name_Len := Current_Dir'Length;
      Name_Buffer (1 .. Name_Len) := Current_Dir;
      Current_Dir_Name := Name_Find;
      Set_Path_Name_Of (Kernel.Project, Current_Dir_Name);
      Set_Directory_Of (Kernel.Project, Current_Dir_Name);

      --  The project declaration
      Prj_Decl := Default_Project_Node (N_Project_Declaration);
      Set_Project_Declaration_Of (Kernel.Project, Prj_Decl);

      --  Source dirs => only the current directory
      Decl := Default_Project_Node (N_Declarative_Item);
      Set_First_Declarative_Item_Of (Prj_Decl, Decl);
      Item := Default_Project_Node (N_Attribute_Declaration, Prj.List);
      Set_Current_Item_Node (Decl, Item);
      Set_Name_Of (Item, Name_Source_Dirs);

      Expr := Default_Project_Node (N_Expression, Prj.List);
      Term := Default_Project_Node (N_Term, Prj.List);
      Set_First_Term (Expr, Term);
      L := Default_Project_Node (N_Literal_String_List, Prj.List);
      Set_Current_Term (Term, L);

      Str2 := Default_Project_Node (N_Expression, Prj.Single);
      Set_First_Expression_In_List (L, Str2);
      L := Default_Project_Node (N_Term, Prj.Single);
      Set_First_Term (Str2, L);

      Start_String;
      Store_String_Chars (".");
      Str2 := Default_Project_Node (N_Literal_String, Prj.Single);
      Set_String_Value_Of (Str2, End_String);
      Set_Current_Term (L, Str2);

      Set_Expression_Of (Item, Expr);

      --  Obj dirs => the current directory
      Set_Next_Declarative_Item
        (Decl, Default_Project_Node (N_Declarative_Item));
      Decl := Next_Declarative_Item (Decl);
      Item := Default_Project_Node (N_Attribute_Declaration, Prj.Single);
      Set_Current_Item_Node (Decl, Item);
      Set_Name_Of (Item, Name_Object_Dir);

      Expr := Default_Project_Node (N_Expression, Prj.Single);
      Term := Default_Project_Node (N_Term, Prj.Single);
      Set_First_Term (Expr, Term);
      L := Default_Project_Node (N_Literal_String, Prj.Single);
      Set_Current_Term (Term, L);
      Start_String;
      Store_String_Chars (".");
      Set_String_Value_Of (L, End_String);

      Set_Expression_Of (Item, Expr);

      --  Register the name of the project so that we can retrieve it from one
      --  of its views

      Prj.Tree.Tree_Private_Part.Projects_Htable.Set
        (Prj.Tree.Name_Of (Kernel.Project),
         Prj.Tree.Tree_Private_Part.Project_Name_And_Node'
         (Name => Prj.Tree.Name_Of (Kernel.Project),
          Node => Kernel.Project,
          Modified => False));

      Recompute_View (Kernel);
   end Create_Default_Project;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Handle      : out Kernel_Handle;
      Main_Window : Gtk.Window.Gtk_Window)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None), 2 => (1 => GType_None));

   begin
      Handle := new Kernel_Handle_Record;
      Handle.Main_Window := Main_Window;
      Glib.Object.Initialize (Handle);
      Initialize_Class_Record
        (Handle, Signals, Kernel_Class, "GlideKernel", Signal_Parameters);

      Create_Default_Project (Handle);
      Reset_Source_Info_List (Handle);
   end Gtk_New;

   ---------------------
   -- Set_Source_Path --
   ---------------------

   procedure Set_Source_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String) is
   begin
      GNAT.OS_Lib.Free (Handle.Source_Path);
      Handle.Source_Path := new String'(Path);
   end Set_Source_Path;

   ---------------------
   -- Get_Source_Path --
   ---------------------

   function Get_Source_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Source_Path = null then
         return "";
      end if;
      return Handle.Source_Path.all;
   end Get_Source_Path;

   ---------------------
   -- Set_Object_Path --
   ---------------------

   procedure Set_Object_Path
     (Handle : access Kernel_Handle_Record;
      Path   : String) is
   begin
      GNAT.OS_Lib.Free (Handle.Object_Path);
      Handle.Object_Path := new String'(Path);
   end Set_Object_Path;

   ---------------------
   -- Get_Object_Path --
   ---------------------

   function Get_Object_Path
     (Handle : access Kernel_Handle_Record) return String is
   begin
      if Handle.Object_Path = null then
         return "";
      end if;
      return Handle.Object_Path.all;
   end Get_Object_Path;

   --------------------
   -- Parse_ALI_File --
   --------------------

   procedure Parse_ALI_File
     (Handle       : access Kernel_Handle_Record;
      ALI_Filename : String;
      Unit         : out Src_Info.LI_File_Ptr;
      Success      : out Boolean) is
   begin
      Src_Info.ALI.Parse_ALI_File
        (ALI_Filename => ALI_Filename,
         Project      => Get_Project_View (Handle),
         Source_Path  => Get_Source_Path (Handle),
         List         => Handle.Source_Info_List,
         Unit         => Unit,
         Success      => Success);
   end Parse_ALI_File;

   ----------------------------
   -- Reset_Source_Info_List --
   ----------------------------

   procedure Reset_Source_Info_List
     (Handle : access Kernel_Handle_Record) is
   begin
      Src_Info.Reset (Handle.Source_Info_List);
   end Reset_Source_Info_List;

   --------------------------
   -- Get_Source_Info_List --
   --------------------------

   function Get_Source_Info_List
     (Handle : access Kernel_Handle_Record) return Src_Info.LI_File_List is
   begin
      return Handle.Source_Info_List;
   end Get_Source_Info_List;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Project_Changed_Signal);
   end Project_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed (Handle : access Kernel_Handle_Record) is
   begin
      Object_Callback.Emit_By_Name (Handle, Project_View_Changed_Signal);
   end Project_View_Changed;

end Glide_Kernel;
