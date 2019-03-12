------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with Gtkada.MDI;

with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with Projects.Views;
with Src_Editor_Module;

package body CodePeer.Module.Editors is

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been closed

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been opened

   type On_MDI_Child_Selected
     (Module : not null access Module_Id_Record'Class) is
     new Mdi_Child_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_MDI_Child_Selected;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : Gtkada.MDI.MDI_Child);

   procedure Show_Annotations
     (Module : in out Module_Id_Record'Class;
      Buffer : GPS.Editors.Line_Information.GPS_Editor_Buffer'Class;
      File   : Code_Analysis.File_Access);
   --  Show annotations for the specified file in the specified buffer

   ----------------------
   -- Hide_Annotations --
   ----------------------

   procedure Hide_Annotations
     (Self : in out Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      Kernel : constant GPS.Kernel.Kernel_Handle := Self.Get_Kernel;
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File.Name, False, False, False);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor) is
         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : CodePeer.Subprogram_Data'Class
         renames CodePeer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.CodePeer_Data.all);

      begin
         if not Data.Mark.Is_Empty
           and then Data.Mark.Element.Is_Present
         then
            GPS_Editor_Buffer'Class (Buffer).Remove_Special_Lines
              (Data.Mark.Element, Data.Special_Lines);
         end if;

         Data.Mark.Clear;
         Data.Special_Lines := 0;
      end Process;

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer
        and then Buffer in GPS_Editor_Buffer'Class
      then
         File.Subprograms.Iterate (Process'Access);
      end if;
   end Hide_Annotations;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      use type Code_Analysis.Code_Analysis_Tree;

      Project_Node : Code_Analysis.Project_Access;
      Project_View : Projects.Views.Project_View_Reference;

   begin
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);

      begin
         Project_View :=
           Projects.Views.Create_Project_View_Reference
             (Kernel, F_Info.Project);
      end;

      if Module.Tree /= null
        and then Module.Tree.Contains (Project_View)
      then
         Project_Node := Module.Tree.Element (Project_View);

         if Project_Node.Files.Contains (File) then
            Hide_Annotations (Module.all, Project_Node.Files.Element (File));
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      use type Code_Analysis.Code_Analysis_Tree;

      Project_Node : Code_Analysis.Project_Access;
      Project_View : Projects.Views.Project_View_Reference;

   begin
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);

      begin
         Project_View :=
           Projects.Views.Create_Project_View_Reference
             (Kernel, F_Info.Project);
      end;

      if Module.Tree /= null
        and then Module.Tree.Contains (Project_View)
      then
         Project_Node := Module.Tree.Element (Project_View);

         if Project_Node.Files.Contains (File) then
            Show_Annotations (Module.all, Project_Node.Files.Element (File));
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_MDI_Child_Selected;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : Gtkada.MDI.MDI_Child)
   is
      File_Name       : GNATCOLL.VFS.Virtual_File;
      Project         : Projects.Views.Project_View_Reference;
      Project_Node    : Code_Analysis.Project_Access;
      File_Node       : Code_Analysis.File_Access;
      Subprogram_Data : CodePeer.Subprogram_Data_Access;

   begin
      if not Src_Editor_Module.Is_Source_Box (Child)
        or else Self.Module.Tree = null
      then
         --  Selected child is not a source editor or there is no analysis
         --  information.

         return;
      end if;

      File_Name :=
        Src_Editor_Module.Get_Source_Box_From_MDI (Child).Get_Filename;

      if File_Name = No_File then
         --  Source buffer doesn't have file name.

         return;
      end if;

      Project :=
        Projects.Views.Create_Project_View_Reference
          (Kernel,
           GNATCOLL.Projects.File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set
              (File_Name).First_Element).Project);

      if not Self.Module.Tree.Contains (Project) then
         --  There is no analysis information available for given project.

         return;
      end if;

      Project_Node :=
        Code_Analysis.Get_Or_Create (Self.Module.Tree, Project);

      if not Project_Node.Files.Contains (File_Name) then
         --  There is no analysis information available for given file.

         return;
      end if;

      File_Node := Code_Analysis.Get_Or_Create (Project_Node, File_Name);

      declare
         File_Data : CodePeer.File_Data'Class
           renames CodePeer.File_Data'Class
             (File_Node.Analysis_Data.CodePeer_Data.all);

      begin
         if File_Node.Subprograms.Is_Empty
           or else File_Data.Annotations_File = No_File
           or else File_Data.Annotations_Loaded
         then
            --  There is no subprograms in given file or there is no
            --  annotation information available for given file or
            --  annotation information was loaded previously (last means that
            --  it was displayed and may be hidden by the user, thus it should
            --  not be redisplayed).

            return;
         end if;
      end;

      Subprogram_Data :=
        CodePeer.Subprogram_Data_Access
          (File_Node.Subprograms.First_Element.Analysis_Data.CodePeer_Data);

      if Subprogram_Data.Mark.Is_Empty then
         Show_Annotations (Self.Module.all, File_Node);
      end if;
   end Execute;

   ---------------------------------
   -- Register_Editor_Integration --
   ---------------------------------

   procedure Register_Editor_Integration
     (Module : not null CodePeer_Module_Id)
   is
   begin
      File_Closed_Hook.Add (new On_File_Closed);
      File_Edited_Hook.Add (new On_File_Edited);
      Mdi_Child_Selected_Hook.Add (new On_MDI_Child_Selected (Module));
   end Register_Editor_Integration;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations
     (Module : in out Module_Id_Record'Class;
      Buffer : GPS.Editors.Line_Information.GPS_Editor_Buffer'Class;
      File   : Code_Analysis.File_Access)
   is

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor);

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor)
      is

         procedure Process_Annotation
           (Position : CodePeer.Annotation_Vectors.Cursor);

         procedure Process_Annotations
           (Position : CodePeer.Annotation_Maps.Cursor);

         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : CodePeer.Subprogram_Data'Class
           renames CodePeer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.CodePeer_Data.all);
         Indent          : constant String :=
                             Ada.Strings.Fixed."*"
                               (Subprogram_Node.Column - 1, ' ');

         ------------------------
         -- Process_Annotation --
         ------------------------

         procedure Process_Annotation
           (Position : CodePeer.Annotation_Vectors.Cursor)
         is
            Annotation : constant CodePeer.Annotation_Access :=
                           CodePeer.Annotation_Vectors.Element (Position);

         begin
            if Annotation.Lifeage in Added .. Unchanged then
               Buffer.Add_Special_Line
                 (Start_Line => Subprogram_Node.Line,
                  Text       =>
                    Indent & "--    " & To_String (Annotation.Text),
                  Style      => Module.Annotations_Style);
               Data.Special_Lines := Data.Special_Lines + 1;
            end if;
         end Process_Annotation;

         -------------------------
         -- Process_Annotations --
         -------------------------

         procedure Process_Annotations
           (Position : CodePeer.Annotation_Maps.Cursor)
         is
            Key     : constant CodePeer.Annotation_Category_Access :=
                        CodePeer.Annotation_Maps.Key (Position);
            Element : constant CodePeer.Annotation_Vector_Access :=
                        CodePeer.Annotation_Maps.Element (Position);

         begin
            Buffer.Add_Special_Line
              (Start_Line => Subprogram_Node.Line,
               Text       => Indent & "--  " & To_String (Key.Text) & ":",
               Style      => Module.Annotations_Style);
            Data.Special_Lines := Data.Special_Lines + 1;

            Element.Iterate (Process_Annotation'Access);

            Buffer.Add_Special_Line
              (Start_Line => Subprogram_Node.Line,
               Text       => Indent & "--",
               Style      => Module.Annotations_Style);
            Data.Special_Lines := Data.Special_Lines + 1;
         end Process_Annotations;

      begin
         if Data.Lifeage in Added .. Unchanged then
            Data.Mark.Replace_Element
              (Buffer.Add_Special_Line
                 (Start_Line => Subprogram_Node.Line,
                  Text       => Indent & "--",
                  Style      => Module.Annotations_Style));
            Data.Special_Lines := Data.Special_Lines + 1;

            Buffer.Add_Special_Line
              (Start_Line => Subprogram_Node.Line,
               Text       => Indent & "--  Subprogram: "
               & Subprogram_Node.Name.all,
               Style      => Module.Annotations_Style);
            Data.Special_Lines := Data.Special_Lines + 1;

            Buffer.Add_Special_Line
              (Start_Line => Subprogram_Node.Line,
               Text       => Indent & "--",
               Style      => Module.Annotations_Style);
            Data.Special_Lines := Data.Special_Lines + 1;

            Data.Annotations.Iterate (Process_Annotations'Access);
         end if;
      end Process_Subprogram;

      Data : CodePeer.File_Data'Class renames CodePeer.File_Data'Class
        (File.Analysis_Data.CodePeer_Data.all);

   begin
      --  Load annotations data.

      if Data.Annotations_File /= No_File
        and then not Data.Annotations_Loaded
      then
         Module.Load_Annotations (File.all);
      end if;

      File.Subprograms.Iterate (Process_Subprogram'Access);
   end Show_Annotations;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations
     (Module : in out Module_Id_Record'Class;
      File   : Code_Analysis.File_Access)
   is
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Module.Get_Kernel.Get_Buffer_Factory.Get (File.Name);

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer
        and then Buffer in GPS_Editor_Buffer'Class
      then
         Show_Annotations (Module, GPS_Editor_Buffer'Class (Buffer), File);
      end if;
   end Show_Annotations;

end CodePeer.Module.Editors;
