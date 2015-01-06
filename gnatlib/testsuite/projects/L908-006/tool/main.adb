--
--  Copyright (C) 2015, AdaCore
--

with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.VFS_Utils;          use GNATCOLL.VFS_Utils;

with Ada.Text_IO;

with GNAT.Strings;            use GNAT.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure Main is
   Env                 : Project_Environment_Access;
   Source_Project_Tree : GNATCOLL.Projects.Project_Tree;

   P : Project_Type;
begin
   Initialize (Env);
   Env.Register_Default_Language_Extension ("C", ".h", ".c");
   Source_Project_Tree.Load (GNATCOLL.VFS.Create (+"aaa.gpr"), Env);

   if Source_Project_Tree.Root_Project = No_Project then
      Ada.Text_IO.Put_Line ("project not loaded");
   end if;

   declare
      SL : constant String_List := (1 => new String'("src\src_in"));
      S  : constant String      := "obj\obj_in";
   begin
      Set_Attribute
        (Self      => Source_Project_Tree.Root_Project,
         Attribute => Source_Dirs_Attribute,
         Values    => SL);
      Set_Attribute
        (Self      => Source_Project_Tree.Root_Project,
         Attribute => Obj_Dir_Attribute,
         Value     => S);
   end;

   declare
      Save_Success : Boolean;
   begin
      Save_Success := Save (Source_Project_Tree.Root_Project);
      if not Save_Success then
         Ada.Text_IO.Put_Line ("cannot save project");
      end if;
   end;

   Source_Project_Tree.Unload;

   --  Reloading project.

   Source_Project_Tree.Load (GNATCOLL.VFS.Create (+"aaa.gpr"), Env);

   if Source_Project_Tree.Root_Project = No_Project then
      Ada.Text_IO.Put_Line ("project not loaded 2");
   end if;

   P := Source_Project_Tree.Root_Project;

   declare
      Vals : constant String_List :=
        Attribute_Value (P, Source_Dirs_Attribute).all;
   begin
      if Vals (Vals'First).all /= "src/src_in" then
         Ada.Text_IO.Put_Line ("backslash not converted in list");
      end if;
      if Attribute_Value (P, Obj_Dir_Attribute) /= "obj/obj_in" then
         Ada.Text_IO.Put_Line ("backslash not converted in string");
      end if;
   end;

   Source_Project_Tree.Unload;
   Free (Env);
end Main;
