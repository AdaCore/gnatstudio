------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with Ada.Calendar;      use Ada.Calendar;

with GNATCOLL.Symbols;  use GNATCOLL.Symbols;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPS.Kernel.Hooks;  use GPS.Kernel.Hooks;

with Basic_Types;
with Language;          use Language;
with Language.Abstract_Language_Tree;
use Language.Abstract_Language_Tree;
with Outline_View;      use Outline_View;

package body Outline_View_Provider_Semantic_Trees is

   Me : constant Trace_Handle := Create ("OUTLINE_PROVIDER.SEMANTIC_TREE");

   type Semantic_Provider is new Outline_Provider with record
      Kernel : Kernel_Handle;
   end record;
   type Semantic_Provider_Access is access all Semantic_Provider;

   overriding procedure Start_Fill
     (Self : access Semantic_Provider; File : Virtual_File);

   overriding procedure Stop_Fill (Self : access Semantic_Provider) is null;

   overriding function Support_Language
     (Self : access Semantic_Provider;
      Lang : Language_Access)
      return Boolean;

   type On_Semantic_Tree_Updated is new File_Hooks_Function with record
      Provider : Semantic_Provider_Access;
   end record;
   overriding procedure Execute
     (Self   : On_Semantic_Tree_Updated;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);

   ----------------
   -- Start_Fill --
   ----------------

   overriding procedure Start_Fill
     (Self : access Semantic_Provider; File : Virtual_File) is
   begin
      if File /= No_File then
         declare
            Tree : Semantic_Tree'Class :=
              Self.Kernel.Get_Abstract_Tree_For_File ("OUTLINE", File);
         begin
            if Tree.File = No_File then
               --  Handle the case where the language couldn't be found
               Outline_View.Finished_Computing (Self.Kernel);
               return;
            end if;

            Tree.Update_Async;
            --  will be completed after triggering Semantic_Tree_Updated_Hook
         end;
      else
         --  Empty file => Stop here
         Outline_View.Finished_Computing (Self.Kernel);
      end if;
   end Start_Fill;

   ----------------------
   -- Support_Language --
   ----------------------

   overriding function Support_Language
     (Self : access Semantic_Provider;
      Lang : Language_Access)
      return Boolean
   is
      pragma Unreferenced (Self, Lang);
   begin
      return True;
   end Support_Language;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Semantic_Tree_Updated;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      procedure Add_Recursive
        (Model : Outline_View.Outline_Model_Access;
         Nodes : Semantic_Node_Array'Class);
      --  Add Nodes in the model and recursively add their children

      -------------------
      -- Add_Recursive --
      -------------------

      procedure Add_Recursive
        (Model : Outline_View.Outline_Model_Access;
         Nodes : Semantic_Node_Array'Class)
      is
         use type Basic_Types.Visible_Column_Type;
      begin
         for I in 1 .. Nodes.Length loop
            declare
               Node    : constant Semantic_Node'Class := Nodes.Get (I);
               Def     : constant Language.Sloc_T := Node.Sloc_Def;
               Visible : Boolean;
            begin
               if Node.Is_Valid then
                  Outline_View.Add_Row
                    (Self           => Model,
                     Name           => Get (Node.Name).all,
                     Profile        => Get (Node.Profile).all,
                     Category       => Node.Category,
                     Is_Declaration => Node.Is_Declaration,
                     Visibility     => Node.Visibility,
                     Def_Line       => Def.Line,
                     Def_Col        => Def.Column,
                     Def_End_Line   => -1,
                     Def_End_Col    => -1,
                     End_Line       => Node.Sloc_End.Line,
                     Id             => Get (Node.Unique_Id).all,
                     Visible        => Visible);
                  if Visible then
                     Add_Recursive (Model, Node.Children);
                     Outline_View.Move_Cursor (Model, Outline_View.Up);
                  end if;
               end if;
            end;
         end loop;
      end Add_Recursive;
   begin
      declare
         Model : Outline_View.Outline_Model_Access :=
           Outline_View.Get_Outline_Model
             (Kernel_Handle (Kernel), File, Default => True);
         Now   : constant Time := Clock;
      begin
         if Model = null then
            return;
         end if;

         declare
            Tree : constant Semantic_Tree'Class :=
              Kernel.Get_Abstract_Tree_For_File ("OUTLINE", File);
         begin
            if Tree = No_Semantic_Tree then
               return;
            end if;

            if Tree.Is_Ready then
               Outline_View.Clear_Outline_Model (Model);
               Add_Recursive (Model, Tree.Root_Nodes);
            end if;
         end;

         Outline_View.Free (Model);
         Trace
           (Me,
            "Time elapsed to compute outline:" & Duration'Image (Clock - Now));
      end;

      Outline_View.Finished_Computing (Kernel_Handle (Kernel));
   exception
      when Outline_View.Outline_Error =>
         Trace (Me, "Can't detach the model");
         Outline_View.Finished_Computing (Kernel_Handle (Kernel));
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle)
   is
      Provider : constant Semantic_Provider_Access :=
        new Semantic_Provider'(Kernel => Kernel);
   begin
      Semantic_Tree_Updated_Hook.Add
        (new On_Semantic_Tree_Updated'(
         File_Hooks_Function with Provider => Provider));
      Outline_View.Set_Default_Provider (Outline_Provider_Access (Provider));
   end Register_Module;

end Outline_View_Provider_Semantic_Trees;
