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

--  This package contains code of integration with source editors

private package CodePeer.Module.Editors is

   procedure Hide_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access);
   --  Hide annotations for the specified file

   procedure Show_Annotations
     (Module : in out Module_Id_Record'Class;
      File   : Code_Analysis.File_Access);
   --  Show annotations for the specified file

   procedure Show_Annotations_In_Opened_Editors
     (Module : in out Module_Id_Record'Class);
   --  Update annotations in opened source editors

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register editor's specific features

end CodePeer.Module.Editors;
