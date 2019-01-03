------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2019, AdaCore                     --
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

with GPS.Editors; use GPS.Editors;
with GPS.Kernel; use GPS.Kernel;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GNATCOLL.Any_Types; use GNATCOLL.Any_Types;
with String_Utils; use String_Utils;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Completion.Python is

   function Create_Simple_Proposal
     (Resolver : access Completion_Python;
      Category : Language_Category;
      Name, Label, Documentation, Action_Name, Icon_Name : String)
      return Simple_Python_Completion_Proposal;
   --  Creates a simple completion proposal

   function Current_Location
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Editor_Location'Class;
   --  Return the current location

   function Object_To_Proposal
     (Resolver : access Completion_Python;
      Object   : Class_Instance)
      return Simple_Python_Completion_Proposal;
   --  Return a completion proposal, assuming Object is a CompletionProposal

   ----------------------
   -- Lazy computation --
   ----------------------

   --  The following implements a virtual list binding to a python object
   --  which is capable of computing lazily a list of completion proposals.

   type Python_Component is
     new Completion_List_Pckg.Virtual_List_Component
   with record
      Resolver : access Completion_Python;
      Object   : Class_Instance;
   end record;

   type Python_Iterator is
     new Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      Resolver : access Completion_Python;
      Object   : Class_Instance;
      Proposal : Simple_Python_Completion_Proposal := No_Proposal;
   end record;

   overriding function First (List : Python_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   overriding function At_End (It : Python_Iterator) return Boolean;
   overriding procedure Next (It : in out Python_Iterator);
   overriding function Get
     (It : in out Python_Iterator) return Completion_Proposal'Class;

   -----------
   -- First --
   -----------

   overriding function First
     (List : Python_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      Sub    : constant Subprogram_Type := Get_Method
        (List.Object, "_ada_first");
      Script : constant Scripting_Language  := Get_Script (Sub.all);
      Args   : constant Callback_Data'Class := Create (Script, 0);

      Iterator : Python_Iterator;
   begin
      Iterator.Resolver := List.Resolver;
      Iterator.Object := Execute (Sub, Args);

      return Iterator;
   end First;

   ------------
   -- At_End --
   ------------

   overriding function At_End (It : Python_Iterator) return Boolean is
      Sub    : constant Subprogram_Type := Get_Method
        (It.Object, "_ada_at_end");
      Script : constant Scripting_Language  := Get_Script (Sub.all);
      Args   : constant Callback_Data'Class := Create (Script, 0);
   begin
      return Execute (Sub, Args);
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Python_Iterator) is
      Sub     : constant Subprogram_Type := Get_Method
        (It.Object, "_ada_next");
      Script  : constant Scripting_Language  := Get_Script (Sub.all);
      Args    : constant Callback_Data'Class := Create (Script, 0);
      Ignored : Any_Type := Execute (Sub, Args);
   begin
      null;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (It : in out Python_Iterator) return Completion_Proposal'Class
   is
      Sub    : constant Subprogram_Type := Get_Method
        (It.Object, "_ada_get");
      Script : constant Scripting_Language  := Get_Script (Sub.all);
      Args   : constant Callback_Data'Class := Create (Script, 0);
      Object : Class_Instance;

   begin
      --  The code below can be used to implement a cache
      --  ??? and probably should

--        if It.Proposal = No_Proposal then
--           Object := Execute (Sub, Args);
--           It.Proposal := Object_To_Proposal (It.Resolver, Object);
--        end if;
--
--        return It.Proposal;

      Object := Execute (Sub, Args);
      return Object_To_Proposal (It.Resolver, Object);
   end Get;

   ----------------------
   -- Current_Location --
   ----------------------

   function Current_Location
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Editor_Location'Class
   is
      Buf : constant Editor_Buffer'Class :=
        GPS.Editors.Get (This        => Kernel.Get_Buffer_Factory.all,
                         File        => File,
                         Force       => False,
                         Open_Buffer => False,
                         Open_View   => False);
   begin
      return Buf.Current_View.Cursor;
   end Current_Location;

   ----------------------------
   -- Create_Simple_Proposal --
   ----------------------------

   function Create_Simple_Proposal
     (Resolver : access Completion_Python;
      Category : Language_Category;
      Name, Label, Documentation, Action_Name, Icon_Name : String)
      return Simple_Python_Completion_Proposal
   is
      Proposal : Simple_Python_Completion_Proposal;
   begin
      Proposal := (Resolver => Resolver,
                   Category => Category,
                   Name     => new String'(Name),
                   Label    => To_Unbounded_String (Label),
                   Documentation => To_Unbounded_String (Documentation),
                   Action_Name => To_Unbounded_String (Action_Name),
                   Icon_Name => To_Unbounded_String (Icon_Name));

      return Proposal;
   end Create_Simple_Proposal;

   ------------------------
   -- Object_To_Proposal --
   ------------------------

   function Object_To_Proposal
     (Resolver : access Completion_Python;
      Object   : Class_Instance)
      return Simple_Python_Completion_Proposal
   is
      Sub      : Subprogram_Type := Get_Method (Object, "get_data_as_list");
      Script   : constant Scripting_Language  := Get_Script (Sub.all);
      Proposal : Simple_Python_Completion_Proposal;
      Args     : constant Callback_Data'Class := Create (Script, 0);
      Fields   : constant List_Instance       := Execute (Sub, Args);
   begin
      Proposal := Create_Simple_Proposal
        (Resolver,
         Language_Category'Val (Nth_Arg (Fields, 6) - 1),
         Nth_Arg (Fields, 1),
         Nth_Arg (Fields, 2),
         Nth_Arg (Fields, 3),
         Nth_Arg (Fields, 4),
         Nth_Arg (Fields, 5));
      Free (Sub);
      return Proposal;
   end Object_To_Proposal;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver   : access Completion_Python;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List)
   is
      pragma Unreferenced (Offset);
   begin
      if Resolver.Lang_Name = ""
        or else To_Lower (+Resolver.Lang_Name)
        = To_Lower (Context.Lang.Get_Name)
      then
         declare
            Sub      : Subprogram_Type :=
              Get_Method (Resolver.Object, "get_completion_prefix");
            Script   : constant Scripting_Language  := Get_Script (Sub.all);
            Args     : Callback_Data'Class := Create (Script, 2);
            Loc      : constant Editor_Location'Class :=
              Current_Location (Get_Kernel (Script), Context.File);
            Loc_Inst : constant Class_Instance :=
              Create_Instance (Loc, Script);

            Component : Python_Component;
         begin
            --  First get the completion prefix
            Set_Nth_Arg (Args, 1, Resolver.Object);
            Set_Nth_Arg (Args, 2, Loc_Inst);
            Result.Searched_Identifier := new String'(Execute (Sub, Args));
            Free (Sub);

            --  And get the list of completions
            Sub := Get_Method (Resolver.Object, "_ada_get_completions");
            Set_Nth_Arg (Args, 1, Resolver.Object);
            Set_Nth_Arg (Args, 2, Loc_Inst);

            Component := (Resolver, Execute (Sub, Args));
            Free (Sub);

            Completion_List_Pckg.Append (Result.List, Component);
         end;
      end if;
   end Get_Completion_Root;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Resolver : Completion_Python)
      return String is
   begin
      return "python" & Resolver.Id'Img;
   end Get_Id;

   ---------------------
   -- Get_Action_Name --
   ---------------------

   overriding function Get_Action_Name
     (Proposal : Simple_Python_Completion_Proposal) return String is
   begin
      return To_String (Proposal.Action_Name);
   end Get_Action_Name;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : Simple_Python_Completion_Proposal) return String is
   begin
      return To_String (Proposal.Documentation);
   end Get_Documentation;

   --------------------------
   -- Get_Custom_Icon_Name --
   --------------------------

   overriding function Get_Custom_Icon_Name
     (Proposal : Simple_Python_Completion_Proposal) return String is
   begin
      return To_String (Proposal.Icon_Name);
   end Get_Custom_Icon_Name;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Proposal : Simple_Python_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return String
   is
      pragma Unreferenced (Db);
   begin
      return To_String (Proposal.Label);
   end Get_Label;

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : Simple_Python_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id
   is
      pragma Unreferenced (Db);
   begin
      return (Proposal.Name'Length,
              "PYTHON  ",
              Proposal.Name.all,
              GNATCOLL.VFS.No_File, 0, 0);
   end To_Completion_Id;

   ------------
   -- Create --
   ------------

   Counter : Positive := 1;
   --  A counter to implement unicity of the identifiers for the registered
   --  resolvers. Positive overflow? unlikely.

   function Create
     (Class : Class_Instance;
      Lang_Name : String) return Completion_Python_Access
   is
   begin
      Counter := Counter + 1;

      return new Completion_Python'
        (Lang_Name => +Lang_Name,
         Object    => Class,
         Id        => Counter,
         Manager   => <>);
   end Create;

   ---------------
   -- Deep_Copy --
   ---------------

   overriding function Deep_Copy
     (Proposal : Simple_Python_Completion_Proposal)
      return Completion_Proposal'Class is
   begin
      return Simple_Python_Completion_Proposal'
        (Resolver      => Proposal.Resolver,
         Name          => new String'(Proposal.Name.all),
         Category      => Proposal.Category,
         Label         => Proposal.Label,
         Documentation => Proposal.Documentation,
         Icon_Name     => Proposal.Icon_Name,
         Action_Name   => Proposal.Action_Name);
   end Deep_Copy;

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   overriding function Get_Initial_Completion_List
     (Manager : access Generic_Completion_Manager;
      Context : Completion_Context)
      return Completion_List
   is
      It       : Completion_Resolver_List_Pckg.Cursor;
      Result   : Completion_List;

      New_Context : constant Completion_Context :=
        new Completion_Context_Record;
   begin
      New_Context.all := Context.all;
      Append (Manager.Contexts, New_Context);

      It := First (Manager.Ordered_Resolvers);
      while It /= Completion_Resolver_List_Pckg.No_Element loop
         Completion.Get_Completion_Root
           (Resolver => Element (It),
            Offset   => 0,
            Context  => New_Context,
            Result   => Result);

         It := Next (It);
      end loop;

      return Result;
   end Get_Initial_Completion_List;

end Completion.Python;
