------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;

with CodePeer.Module.Editors;

package body CodePeer.Module.Commands is

   function Is_Show_Hide_Allowed
     (Module  : CodePeer.Module.CodePeer_Module_Id;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Returns True when show/hide annotations is allowed.

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Hide_Annotations_Command;
      Context : Standard.Commands.Interactive.Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type is
   begin
      if Is_Show_Hide_Allowed (Self.Module, Context.Context) then
         declare
            Project_Node    : constant Code_Analysis.Project_Access :=
              Code_Analysis.Get_Or_Create
                (Self.Module.Tree,
                 GPS.Kernel.Contexts.Project_Information (Context.Context));
            File_Node       : constant Code_Analysis.File_Access :=
              Code_Analysis.Get_Or_Create
                (Project_Node,
                 GPS.Kernel.Contexts.File_Information (Context.Context));

         begin
            CodePeer.Module.Editors.Hide_Annotations
              (Self.Module.all, File_Node);
         end;
      end if;

      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Review_Message_Command)
      return Standard.Commands.Command_Return_Type
   is
      Context  : constant GPS.Kernel.Selection_Context :=
        Self.Module.Kernel.Get_Current_Context;
      Messages : Standard.CodePeer.Message_Vectors.Vector;

   begin
      for Message of Messages_Information (Context) loop
         if Message.all in Standard.CodePeer.Message'Class then
            Messages.Append (Standard.CodePeer.Message_Access (Message));
         end if;
      end loop;

      if not Messages.Is_Empty then
         Self.Module.Review_Messages (Messages);
      end if;

      return Standard.Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Show_Annotations_Command;
      Context : Standard.Commands.Interactive.Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type is
   begin
      if Is_Show_Hide_Allowed (Self.Module, Context.Context) then
         declare
            Project_Node    : constant Code_Analysis.Project_Access :=
              Code_Analysis.Get_Or_Create
                (Self.Module.Tree,
                 GPS.Kernel.Contexts.Project_Information (Context.Context));
            File_Node       : constant Code_Analysis.File_Access :=
              Code_Analysis.Get_Or_Create
                (Project_Node,
                 GPS.Kernel.Contexts.File_Information (Context.Context));

         begin
            CodePeer.Module.Editors.Show_Annotations
              (Self.Module.all, File_Node);
         end;
      end if;

      return Standard.Commands.Success;
   end Execute;

   --------------------------
   -- Is_Show_Hide_Allowed --
   --------------------------

   function Is_Show_Hide_Allowed
     (Module  : CodePeer.Module.CodePeer_Module_Id;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      use type Code_Analysis.Code_Analysis_Tree;

   begin
      return
        Module.Tree /= null
          and then GPS.Kernel.Contexts.Has_File_Information (Context);
   end Is_Show_Hide_Allowed;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Hide_Annotations_Filter;
      Context : Selection_Context) return Boolean is
   begin
      if not Is_Show_Hide_Allowed (Filter.Module, Context) then
         return False;
      end if;

      declare
         use type GPS.Editors.Editor_Buffer'Class;

         Project_Node    : constant Code_Analysis.Project_Access :=
           Code_Analysis.Get_Or_Create
             (Filter.Module.Tree,
              GPS.Kernel.Contexts.Project_Information (Context));
         File_Node       : constant Code_Analysis.File_Access :=
           Code_Analysis.Get_Or_Create
             (Project_Node,
              GPS.Kernel.Contexts.File_Information (Context));
         Subprogram_Node : Code_Analysis.Subprogram_Access;
         Subprogram_Data : CodePeer.Subprogram_Data_Access;
         Kernel          : constant GPS.Kernel.Kernel_Handle :=
           GPS.Kernel.Get_Kernel (Context);
         Buffer          : constant GPS.Editors.Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get
             (File_Node.Name, False, False, False);

      begin
         if not File_Node.Subprograms.Is_Empty then
            Subprogram_Node :=
              Code_Analysis.Subprogram_Maps.Element
                (File_Node.Subprograms.First);
            Subprogram_Data :=
              CodePeer.Subprogram_Data_Access
                (Subprogram_Node.Analysis_Data.CodePeer_Data);

            if Buffer /= GPS.Editors.Nil_Editor_Buffer then
               if not Subprogram_Data.Mark.Is_Empty then
                  return True;
               end if;
            end if;
         end if;
      end;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Show_Annotations_Filter;
      Context : Selection_Context) return Boolean is
   begin
      if not Is_Show_Hide_Allowed (Filter.Module, Context) then
         return False;
      end if;

      declare
         use type GPS.Editors.Editor_Buffer'Class;

         Project_Node    : constant Code_Analysis.Project_Access :=
           Code_Analysis.Get_Or_Create
             (Filter.Module.Tree,
              GPS.Kernel.Contexts.Project_Information (Context));
         File_Node       : constant Code_Analysis.File_Access :=
           Code_Analysis.Get_Or_Create
             (Project_Node,
              GPS.Kernel.Contexts.File_Information (Context));
         Subprogram_Node : Code_Analysis.Subprogram_Access;
         Subprogram_Data : CodePeer.Subprogram_Data_Access;
         Kernel          : constant GPS.Kernel.Kernel_Handle :=
           GPS.Kernel.Get_Kernel (Context);
         Buffer          : constant GPS.Editors.Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get
             (File_Node.Name, False, False, False);

      begin
         if not File_Node.Subprograms.Is_Empty then
            Subprogram_Node :=
              Code_Analysis.Subprogram_Maps.Element
                (File_Node.Subprograms.First);
            Subprogram_Data :=
              CodePeer.Subprogram_Data_Access
                (Subprogram_Node.Analysis_Data.CodePeer_Data);

            if Buffer /= GPS.Editors.Nil_Editor_Buffer then
               if Subprogram_Data.Mark.Is_Empty then
                  return True;
               end if;
            end if;
         end if;
      end;

      return False;
   end Filter_Matches_Primitive;

end CodePeer.Module.Commands;
