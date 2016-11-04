------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.VCS_Engines;         use GPS.VCS_Engines;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with VCS2.Scripts;            use VCS2.Scripts;

package body VCS2.Module is
   Me : constant Trace_Handle := Create ("VCS2");

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed.
   --  This looks for what VCS engine to use for each project. It tries to
   --  reuse existing engines when possible, to benefit from their caches.

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      Dummy : constant Block_Trace_Handle :=
        Create (Me, "Computing VCS repositories for each project");

      pragma Unreferenced (Self);

      function Repo_From_Project
        (F : not null access VCS_Engine_Factory'class;
         P : Project_Type) return String;
      --  Guess the repo for a given project.

      function Engine_From_Repo
        (F    : not null access VCS_Engine_Factory'class;
         Repo : String) return not null VCS_Engine_Access;
      --  Return the engine to use for a iven repository

      ----------------------
      -- Engine_From_Repo --
      ----------------------

      function Engine_From_Repo
        (F    : not null access VCS_Engine_Factory'class;
         Repo : String) return not null VCS_Engine_Access
      is
         Engine : VCS_Engine_Access;
         R      : Virtual_File;
      begin
         if Repo = "" then
            return No_VCS_Engine (Kernel);
         else
            R := Create (+Repo);
            Engine := Get_VCS (Kernel, R);
            if Engine.all in Dummy_VCS_Engine'Class then
               Trace (Me, "  New engine " & Repo);
               Engine := F.Create_Engine (Repo);
               Set_VCS (Kernel, R, Engine);

               --  if Repo is of the form 'root/.git' or 'root/CVS',... we also
               --  want to register 'root' itself for this VCS even if it does
               --  not contain project sources. This is needed for
               --  Guess_VCS_For_Directory

               if R.Is_Directory then
                  Set_VCS (Kernel, R.Get_Parent, Engine);
               end if;
            else
               Trace (Me, "  Shared engine " & Repo);
            end if;
            return Engine;
         end if;
      end Engine_From_Repo;

      -----------------------
      -- Repo_From_Project --
      -----------------------

      function Repo_From_Project
        (F : not null access VCS_Engine_Factory'class;
         P : Project_Type) return String
      is
         S : File_Array_Access := P.Source_Files (Recursive => False);
      begin
         if S'Length = 0 then
            Unchecked_Free (S);
            return "";
         else
            return R : constant String := F.Find_Repo (S (S'First)) do
               Unchecked_Free (S);
            end return;
         end if;
      end Repo_From_Project;

      Iter   : Project_Iterator;
      P      : Project_Type;
      Engine : VCS_Engine_Access;

   begin
      Iter := Get_Project (Kernel).Start (Recursive => True);
      loop
         P := Current (Iter);
         exit when P = No_Project;

         Engine := No_VCS_Engine (Kernel);

         declare
            Kind : constant String := To_Lower
              (P.Attribute_Value
                 (VCS_Kind_Attribute,
                  Default      => "auto",
                  Use_Extended => True));
            Repo : constant String := P.Attribute_Value
              (VCS_Repository_Root, Use_Extended => True);
            F    : VCS_Engine_Factory_Access;
            All_Factories : constant access Name_To_Factory.Map :=
              All_VCS_Factories (Kernel);

         begin
            if Kind /= "auto" then
               Trace (Me, "Using VCS attribute for " & P.Name
                      & " => " & Kind & " " & Repo);
               F := Get_VCS_Factory (Kernel, Kind);
               if F = null then
                  Insert (Kernel, P.Project_Path.Display_Full_Name
                          & ": unknown VCS: " & Kind);
               else
                  Engine := Engine_From_Repo
                    (F,
                     (if Repo /= "" then Repo else Repo_From_Project (F, P)));
               end if;

            else
               --  Need to find the closest repo (if for instance we have a
               --  CVS working dir nested in a git working dir, then CVS
               --  should be used). We use the longuest path for this, even if
               --  that won't work for systems using environment variables.

               Trace (Me, "Guessing engine for " & P.Name);
               declare
                  Longuest : VCS_Engine_Factory_Access;
                  Longuest_R : String_Access;
               begin
                  for F of All_Factories.all loop
                     declare
                        R : constant String := Repo_From_Project (F, P);
                     begin
                        if R /= ""
                          and then (Longuest_R = null
                                   or else R'Length > Longuest_R'Length)
                        then
                           Free (Longuest_R);
                           Longuest_R := new String'(R);
                           Longuest := F;
                        end if;
                     end;
                  end loop;

                  if Longuest /= null then
                     Engine := Engine_From_Repo (Longuest, Longuest_R.all);
                     Free (Longuest_R);
                  end if;
               end;
            end if;
         end;

         Set_VCS (Kernel, P.Project_Path, Engine);
         Next (Iter);
      end loop;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      VCS2.Scripts.Register_Scripts (Kernel);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
   end Register_Module;

end VCS2.Module;
