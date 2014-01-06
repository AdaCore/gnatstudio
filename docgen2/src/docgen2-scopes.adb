------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

package body Docgen2.Scopes is
   Empty_Stack : Context_Stack.Vector renames Context_Stack.Empty_Vector;

   procedure Pop (Context : in out Analysis_Context);
   --  Remove an element from the stack

   procedure Push
     (Context : in out Analysis_Context;
      Elem    : Context_Element);
   --  Add an element to the stack

   function Top (Context : Analysis_Context) return Context_Element;
   --  Returns the element in the top of the stack

   function Is_Empty (Context : Analysis_Context) return Boolean;
   --  Returns True if the stack is empty

   ---------
   -- Pop --
   ---------

   procedure Pop (Context : in out Analysis_Context) is
   begin
      pragma Assert (not Is_Empty (Context));
      Context.Stack.Delete_Last (Count => 1);
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (Context : in out Analysis_Context;
      Elem    : Context_Element) is
   begin
      Context.Stack.Append (Elem);
   end Push;

   ---------
   -- Top --
   ---------

   function Top
     (Context : Analysis_Context) return Context_Element is
   begin
      pragma Assert (not Is_Empty (Context));
      return Context.Stack.Last_Element;
   end Top;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope
     (Context : Analysis_Context) return Context_Element is
   begin
      return Top (Context);
   end Current_Scope;

   -----------------
   -- Enter_Scope --
   -----------------

   procedure Enter_Scope
     (Context : in out Analysis_Context;
      Parent  : Entity_Info;
      Entity  : Entity_Info)
   is
      New_Scope : constant Context_Element :=
        (Parent_Entity => Parent,
         Pkg_Entity    => Entity,
         Parent_Iter   => Context.Iter);
   begin
      Push (Context, New_Scope);
      Context.Iter :=
        Next (Context.Tree, Context.Iter, Jump_Into);
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope
     (Context : in out Analysis_Context) is
   begin
      Context.Iter :=
        Next (Context.Tree, Top (Context).Parent_Iter, Jump_Over);
      Pop (Context);
   end Exit_Scope;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Analysis_Context) is
   begin
      Free (Context.Tree);
      Free (Context.File_Buffer);
      Free (Context.Comments);
   end Free;

   ------------------
   -- Get_Comments --
   ------------------

   function Get_Comments
     (Context : Analysis_Context) return Comments_List is
   begin
      return Context.Comments;
   end Get_Comments;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Context : Analysis_Context) return access Simple_Construct_Information is
   begin
      return Get_Construct (Context.Iter);
   end Get_Construct;

   --------------
   -- Get_File --
   --------------

   function Get_File (Context : Analysis_Context) return Virtual_File is
   begin
      return Context.File;
   end Get_File;

   ---------------------
   -- Get_File_Buffer --
   ---------------------

   function Get_File_Buffer
     (Context : Analysis_Context) return GNAT.Strings.String_Access is
   begin
      return Context.File_Buffer;
   end Get_File_Buffer;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Context : Analysis_Context) return Language_Handler
   is
   begin
      return Context.Language;
   end Get_Language;

   -------------------------------
   -- Get_Parent_Scope_Iterator --
   -------------------------------

   function Get_Parent_Scope_Iterator
     (Context : Analysis_Context) return Construct_Tree_Iterator is
   begin
      return Get_Parent_Scope (Context.Tree, Context.Iter);
   end Get_Parent_Scope_Iterator;

   --------------
   -- Has_Next --
   --------------

   function Has_Next_Construct
     (Context : Analysis_Context) return Boolean is
   begin
      return Context.Iter /= Null_Construct_Tree_Iterator;
   end Has_Next_Construct;

   --------------------
   -- Get_New_Pkg_Id --
   --------------------

   function Get_New_Pkg_Id
     (Context : in out Analysis_Context) return Natural is
   begin
      Context.Pkg_Nb := Context.Pkg_Nb + 1;
      return Context.Pkg_Nb;
   end Get_New_Pkg_Id;

   -------------
   -- In_Body --
   -------------

   function In_Body (Context : Analysis_Context) return Boolean is
   begin
      return Context.In_Body;
   end In_Body;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Context : Analysis_Context) return Boolean is
   begin
      return Context.Stack.Is_Empty;
   end Is_Empty;

   ----------
   -- Next --
   ----------

   procedure Next_Construct
     (Context : in out Analysis_Context) is
   begin
      Context.Iter :=
        Next (Context.Tree, Context.Iter, Jump_Over);
   end Next_Construct;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (File_EInfo  : Entity_Info;
      Tree        : Construct_Tree;
      File_Buffer : GNAT.Strings.String_Access;
      Language    : Language_Handler;
      Comments    : Comments_List) return Analysis_Context
   is
      Ctxt_Elem   : Context_Element;
      New_Context : Analysis_Context;

   begin
      New_Context :=
        (Stack          => Empty_Stack,
         Iter           => First (Tree),
         Tree           => Tree,
         File_Buffer    => File_Buffer,
         Language       => Language,
         File           => File_EInfo.Location.Spec_Loc.File,
         In_Body        => File_EInfo.Is_Body,
         Comments       => Comments,
         Pkg_Nb         => 0);

      Ctxt_Elem :=
        (Parent_Entity => File_EInfo,
         Pkg_Entity    => null,
         Parent_Iter   => Null_Construct_Tree_Iterator);

      Push (New_Context, Ctxt_Elem);

      return New_Context;
   end New_Context;

end Docgen2.Scopes;
