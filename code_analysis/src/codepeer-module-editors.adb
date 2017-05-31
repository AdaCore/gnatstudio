------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;

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

   procedure Show_Annotations
     (Module : in out Module_Id_Record'Class;
      Buffer : GPS.Editors.Line_Information.GPS_Editor_Buffer'Class;
      File   : Code_Analysis.File_Access);
   --  Show annotations for the specified file in the specified buffer

   ----------------------
   -- Hide_Annotations --
   ----------------------

   procedure Hide_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      Kernel : constant GPS.Kernel.Kernel_Handle := Self.Get_Kernel;
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        (Kernel.Get_Buffer_Factory.Get (File.Name, False, False, False));

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
      Project      : Project_Type;
   begin
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      begin
         Project := F_Info.Project;
      end;

      if Module.Tree /= null
        and then Module.Tree.Contains (Project)
      then
         Project_Node := Module.Tree.Element (Project);

         if Project_Node.Files.Contains (File) then
            Hide_Annotations (Module, Project_Node.Files.Element (File));
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
      Project      : Project_Type;
   begin
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      begin
         Project := F_Info.Project;
      end;

      if Module.Tree /= null
        and then Module.Tree.Contains (Project)
      then
         Project_Node := Module.Tree.Element (Project);

         if Project_Node.Files.Contains (File) then
            Show_Annotations (Module.all, Project_Node.Files.Element (File));
         end if;
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      File_Closed_Hook.Add (new On_File_Closed);
      File_Edited_Hook.Add (new On_File_Edited);
   end Register_Module;

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

   ----------------------------------------
   -- Show_Annotations_In_Opened_Editors --
   ----------------------------------------

   procedure Show_Annotations_In_Opened_Editors
     (Module : in out Module_Id_Record'Class)
   is
      use Code_Analysis.Project_Maps;
      use Code_Analysis.File_Maps;
      use GPS.Editors.Buffer_Lists;

      Buffers          : GPS.Editors.Buffer_Lists.List :=
        Module.Kernel.Get_Buffer_Factory.Buffers;
      Project_Position : Code_Analysis.Project_Maps.Cursor :=
        Module.Tree.First;
      File_Position    : Code_Analysis.File_Maps.Cursor;
      File             : GNATCOLL.VFS.Virtual_File;
      Buffer_Position  : GPS.Editors.Buffer_Lists.Cursor;

   begin
      Projects :
      while Has_Element (Project_Position) loop
         File_Position := Element (Project_Position).Files.First;

         while Has_Element (File_Position) loop
            File := Element (File_Position).Name;
            Buffer_Position := Buffers.First;

            while Has_Element (Buffer_Position) loop
               if Element (Buffer_Position).File = File then
                  if Element (Buffer_Position) in GPS_Editor_Buffer'Class then
                     Show_Annotations
                       (Module,
                        GPS_Editor_Buffer'Class (Element (Buffer_Position)),
                        Element (File_Position));
                  end if;

                  Delete (Buffers, Buffer_Position);

                  exit Projects when Buffers.Is_Empty;
                  exit;
               end if;

               Next (Buffer_Position);
            end loop;

            Next (File_Position);
         end loop;

         Next (Project_Position);
      end loop Projects;
   end Show_Annotations_In_Opened_Editors;

end CodePeer.Module.Editors;
