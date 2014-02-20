------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

      List      : Completion_List_Extensive_Pckg.Extensive_List_Pckg.List;
      Proposal  : Simple_Python_Completion_Proposal;
      Sub       : Subprogram_Type := Get_Method
        (Resolver.Class, "get_completions");
      Script    : constant Scripting_Language  := Get_Script (Sub.all);
      Args      : Callback_Data'Class := Create (Script, 2);
      P         : Class_Instance;

      Loc : constant Editor_Location'Class := Current_Location
        (Get_Kernel (Script), Context.File);

   begin
      Set_Nth_Arg (Args, 1, Resolver.Class);
      Set_Nth_Arg (Args, 2, Create_Instance (Loc, Script));

      declare
         Proposals : constant List_Instance := Execute (Sub, Args);
      begin
         for J in 1 .. Proposals.Number_Of_Arguments loop
            P   := Nth_Arg (Proposals, J);

            Sub := Get_Method (P, "get_data_as_list");
            declare
               Args   : constant Callback_Data'Class := Create (Script, 0);
               Fields : constant List_Instance       := Execute (Sub, Args);
            begin
               Proposal := Create_Simple_Proposal
                 (Resolver,
                  Cat_Custom,
                  Nth_Arg (Fields, 1),
                  Nth_Arg (Fields, 2),
                  Nth_Arg (Fields, 3),
                  Nth_Arg (Fields, 4),
                  Nth_Arg (Fields, 5));
            end;

            Completion_List_Extensive_Pckg.Extensive_List_Pckg.Append
              (List, Proposal);
         end loop;
      end;

      Completion_List_Pckg.Append
        (Result.List, Completion_List_Extensive_Pckg.To_Extensive_List (List));
   end Get_Completion_Root;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Resolver : Completion_Python)
      return String
   is
      pragma Unreferenced (Resolver);
   begin
      return "python";
      --  ??? implement a system to make this unique per resolver
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

   function Create (Class : Class_Instance) return Completion_Python_Access is
      R : Completion_Python_Access;
   begin
      R := new Completion_Python;
      R.Class := Class;
      return R;
   end Create;

end Completion.Python;
