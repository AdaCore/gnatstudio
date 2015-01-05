------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

with GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Project;

with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body CodePeer.Module.Editors is

   use GPS.Editors;
   use GPS.Kernel.Project;
   use GPS.Kernel.Standard_Hooks;

   procedure On_File_Closed_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Called when a file has been closed

   procedure On_File_Edited_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Called when a file has been opened

   procedure Show_Annotations
     (Buffer : GPS.Editors.Line_Information.GPS_Editor_Buffer'Class;
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

         procedure Free is
           new Ada.Unchecked_Deallocation
             (GPS.Editors.Editor_Mark'Class, Editor_Mark_Access);

         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : CodePeer.Subprogram_Data'Class
         renames CodePeer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.CodePeer_Data.all);

      begin
         if Data.Mark /= null
           and then Data.Mark.Is_Present
         then
            GPS_Editor_Buffer'Class (Buffer).Remove_Special_Lines
              (Data.Mark.all, Data.Special_Lines);
         end if;

         Free (Data.Mark);
         Data.Special_Lines := 0;
      end Process;

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer
        and then Buffer in GPS_Editor_Buffer'Class
      then
         File.Subprograms.Iterate (Process'Access);
      end if;
   end Hide_Annotations;

   -------------------------
   -- On_File_Closed_Hook --
   -------------------------

   procedure On_File_Closed_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      use type Code_Analysis.Code_Analysis_Tree;

      D            : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Project_Node : Code_Analysis.Project_Access;
      Project      : Project_Type;

   begin
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (D.File).First_Element);
      begin
         Project := F_Info.Project;
      end;

      if Module.Tree /= null
        and then Module.Tree.Contains (Project)
      then
         Project_Node := Module.Tree.Element (Project);

         if Project_Node.Files.Contains (D.File) then
            Hide_Annotations (Module, Project_Node.Files.Element (D.File));
         end if;
      end if;
   end On_File_Closed_Hook;

   -------------------------
   -- On_File_Edited_Hook --
   -------------------------

   procedure On_File_Edited_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      use type Code_Analysis.Code_Analysis_Tree;

      D            : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Project_Node : Code_Analysis.Project_Access;
      Project      : Project_Type;

   begin
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry (Kernel).Tree.Info_Set (D.File).First_Element);
      begin
         Project := F_Info.Project;
      end;

      if Module.Tree /= null
        and then Module.Tree.Contains (Project)
      then
         Project_Node := Module.Tree.Element (Project);

         if Project_Node.Files.Contains (D.File) then
            Show_Annotations (Module, Project_Node.Files.Element (D.File));
         end if;
      end if;
   end On_File_Edited_Hook;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.File_Closed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_File_Closed_Hook'Access),
         Name  => "codepeer.file_closed");
      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.File_Edited_Hook,
         GPS.Kernel.Hooks.Wrapper (On_File_Edited_Hook'Access),
         Name  => "codepeer.file_edited");
   end Register_Module;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations
     (Buffer : GPS.Editors.Line_Information.GPS_Editor_Buffer'Class;
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
                 (Subprogram_Node.Line,
                  Indent & "--    " & Annotation.Text.all,
                  Annotation_Style_Name);
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
              (Subprogram_Node.Line,
               Indent & "--  " & Key.Text.all & ":",
               Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;

            Element.Iterate (Process_Annotation'Access);

            Buffer.Add_Special_Line
              (Subprogram_Node.Line, Indent & "--", Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;
         end Process_Annotations;

      begin
         if Data.Lifeage in Added .. Unchanged then
            Data.Mark :=
              new GPS.Editors.Editor_Mark'Class'
                (Buffer.Add_Special_Line
                     (Subprogram_Node.Line,
                      Indent & "--",
                      Annotation_Style_Name));
            Data.Special_Lines := Data.Special_Lines + 1;

            Buffer.Add_Special_Line
              (Subprogram_Node.Line,
               Indent & "--  Subprogram: " & Subprogram_Node.Name.all,
               Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;

            Buffer.Add_Special_Line
              (Subprogram_Node.Line, Indent & "--", Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;

            Data.Annotations.Iterate (Process_Annotations'Access);
         end if;
      end Process_Subprogram;

   begin
      File.Subprograms.Iterate (Process_Subprogram'Access);
   end Show_Annotations;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Self.Get_Kernel.Get_Buffer_Factory.Get (File.Name);

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer
        and then Buffer in GPS_Editor_Buffer'Class
      then
         Show_Annotations (GPS_Editor_Buffer'Class (Buffer), File);
      end if;
   end Show_Annotations;

   ----------------------------------------
   -- Show_Annotations_In_Opened_Editors --
   ----------------------------------------

   procedure Show_Annotations_In_Opened_Editors
     (Self : access Module_Id_Record'Class)
   is
      use Code_Analysis.Project_Maps;
      use Code_Analysis.File_Maps;
      use GPS.Editors.Buffer_Lists;

      Buffers          : GPS.Editors.Buffer_Lists.List :=
        Self.Kernel.Get_Buffer_Factory.Buffers;
      Project_Position : Code_Analysis.Project_Maps.Cursor :=
        Self.Tree.First;
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
                       (GPS_Editor_Buffer'Class (Element (Buffer_Position)),
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
