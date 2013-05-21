------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Search;                use GPS.Search;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body GPS.Kernel.Search.Filenames is

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Filenames_Search_Provider) is
   begin
      Unchecked_Free (Self.Files);
   end Free;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Filenames_Search_Provider;
      Pattern : not null access Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      if Self.Files = null then
         --  ??? Should include runtime files
         --  Get_Registry (Kernel).Environmnet.Predefined_Source_Files
         Self.Files :=
           Get_Project (Self.Kernel).Source_Files (Recursive => True);
      end if;

      Self.Index := Self.Files'First - 1;
      Self.Pattern := Pattern;
   end Set_Pattern;

   ----------------------------
   -- Build_Filenames_Result --
   ----------------------------

   function Build_Filenames_Result
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File;
       Score  : Natural := 100)
      return GPS.Search.Search_Result_Access is
   begin
      return new Filenames_Search_Result'
        (Kernel => Kernel_Handle (Kernel),
         Score  => Score,
         Short  => new String'(+File.Base_Name),
         Long   => new String'(File.Display_Full_Name),
         File   => File);
   end Build_Filenames_Result;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Filenames_Search_Provider;
      Result   : out Search_Result_Access;
      Has_Next : out Boolean)
   is
      F : Virtual_File;
      C : Search_Context;
   begin
      while Self.Index < Self.Files'Last loop
         Self.Index := Self.Index + 1;
         F := Self.Files (Self.Index);

         C := Self.Pattern.Start (+F.Base_Name);
         if C /= No_Match then
            Result := Build_Filenames_Result
               (Self.Kernel, F, C.Score);
            Has_Next := Self.Index < Self.Files'Last;
            return;
         end if;
      end loop;

      Has_Next := False;
      Result := null;
   end Next;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Filenames_Search_Result;
      Give_Focus : Boolean) is
   begin
      Open_File_Editor
        (Self.Kernel, Self.File,
         Enable_Navigation => True,
         New_File          => False,
         Focus             => Give_Focus,
         Line              => 0,
         Column            => 0);
   end Execute;

end GPS.Kernel.Search.Filenames;
