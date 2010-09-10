-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2010, AdaCore                  --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Object;   use Glib.Object;

with Traces;          use Traces;

with GPS.Kernel.Console;    use GPS.Kernel.Console;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;    use GPS.Kernel.Project;

with GPS.Intl;              use GPS.Intl;

with Project_Templates;     use Project_Templates;
with Project_Templates.GUI; use Project_Templates.GUI;

package body Project_Templates.GPS is

   package Virtual_File_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Virtual_File);

   type Project_Templates_Module is record
      Dirs : Virtual_File_List.List;
   end record;

   Module_Id : Project_Templates_Module;

   procedure On_New_From_Template
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback when the menu is selected

   procedure Register_Menus (Kernel : access Kernel_Handle_Record'Class);
   --  Register the menus

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the commands

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
   begin
      --  ??? Add shell commands here.
      null;
   end Register_Commands;

   --------------------------
   -- On_New_From_Template --
   --------------------------

   procedure On_New_From_Template
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      use Virtual_File_List;
      C : Cursor;
      E : Unbounded_String;

      Project   : Virtual_File;
      Dir       : Virtual_File;
      Installed : Boolean;

      Templates : Project_Templates_List.List;
   begin
      --  Read all available templates
      C := First (Module_Id.Dirs);

      while Has_Element (C) loop
         Read_Templates_Dir (Element (C), E, Templates);

         if E /= Null_Unbounded_String then
            Insert (Kernel, To_String (E), Mode => Error);
         end if;

         Next (C);
      end loop;

      if Templates.Is_Empty then
         Insert
           (Kernel,
            -"Could not load any project templates.",
            Mode => Error);
         return;
      end if;

      --  Launch the GUI

      Install_Template (Templates, Installed, Dir, Project, E);

      if E /= Null_Unbounded_String then
         Insert (Kernel, To_String (E), Mode => Error);

      elsif Installed then
         --  There has been no error: we can proceed with the loading of the
         --  project.

         --  First change directory

         Change_Dir (Dir);

         --  Then load the project

         Load_Project (Kernel, Project);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_New_From_Template;

   --------------------
   -- Register_Menus --
   --------------------

   procedure Register_Menus (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Register_Menu (Kernel      => Kernel,
                     Parent_Path => -"/_Project",
                     Text        => -"New from _Template...",
                     Callback    => On_New_From_Template'Access,
                     Stock_Image => "gps-project-closed",
                     Ref_Item    => "_New...",
                     Add_Before  => False);
   end Register_Menus;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class) is
   begin
      --  Register the default template dir.
      Module_Id.Dirs.Append
        (Create_From_Dir (Kernel.Get_System_Dir, "share/gps/templates"));

      Register_Commands (Kernel);
      Register_Menus (Kernel);
   end Register_Module;

end Project_Templates.GPS;
