------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Gtk.Enums;             use Gtk.Enums;
with Glib.Object;           use Glib.Object;

with GNATCOLL.Scripts;      use GNATCOLL.Scripts;
with Commands.Interactive;  use Commands, Commands.Interactive;
with GPS.Kernel.Actions;    use GPS.Kernel.Actions;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Project;    use GPS.Kernel.Project;
with GPS.Kernel.Scripts;    use GPS.Kernel.Scripts;
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

   type Project_From_Template_Command is new Interactive_Command
      with null record;
   overriding function Execute
     (Command : access Project_From_Template_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback when the menu is selected

   procedure Template_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for shell commands

   ------------------------------
   -- Template_Command_Handler --
   ------------------------------

   procedure Template_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
   begin
      if Command = "add_templates_dir" then
         declare
            File : constant Virtual_File := Nth_Arg (Data, 1);
         begin
            if File.Is_Directory then
               Module_Id.Dirs.Append (File);
            else
               Set_Error_Msg (Data, -"Parameter should be a directory.");
            end if;
         end;
      end if;
   end Template_Command_Handler;

   -----------------------------------------
   -- Display_Project_Templates_Assistant --
   -----------------------------------------

   function Display_Project_Templates_Assistant
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean
   is
      use Virtual_File_List;
      C : Cursor;
      E : Unbounded_String;

      Project   : Virtual_File;
      Dir       : Virtual_File;
      Installed : Boolean;

      Chosen    : Project_Template;
      Templates : Project_Templates_List.List;
      Success   : Boolean := False;
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
         return False;
      end if;

      --  Launch the GUI

      Install_Template
        (Templates     => Templates,
         Parent        => Parent,
         Chosen        => Chosen,
         Installed     => Installed,
         Dir           => Dir,
         Project       => Project,
         Errors        => E,
         Default_Label => "Simple Project");

      if Installed then
         Success := True;

         --  First change directory

         Change_Dir (Dir);

         --  Then load the project

         if Project = No_File then
            Insert
              (Kernel,
               -"Project template deployed, no project found.",
               Mode => Info);

         elsif not Project.Is_Regular_File then
            Insert
              (Kernel,
               -"Template deployed, but project is not a regular file.",
               Mode => Error);
         else
            Load_Project (Kernel, Project);
         end if;

         --  Execute the post-hook

         if Chosen.Post_Hook /= No_File then
            declare
               Python : constant Scripting_Language :=
                 Kernel.Scripts.Lookup_Scripting_Language ("python");
               Errors : Boolean;
            begin
               Execute_File
                 (Python, +Chosen.Post_Hook.Full_Name, Errors => Errors);

               if Errors then
                  Insert
                    (Kernel,
                     -"Errors occurred when running the post-deployment hook.",
                     Mode => Error);
               end if;
            end;
         end if;
      end if;

      if E /= Null_Unbounded_String then
         Insert
           (Kernel,
            -"The following occurred when deploying the Project from template:"
            & ASCII.LF & To_String (E), Mode => Error);
      end if;

      return Success;
   end Display_Project_Templates_Assistant;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Project_From_Template_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Success : Boolean with Unreferenced;
   begin
      Success := Display_Project_Templates_Assistant
        (Kernel,
         Parent => Get_Current_Window (Kernel));
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class) is
      Project_Template_Class : constant Class_Type :=
        New_Class (Kernel, "ProjectTemplate");
   begin
      --  Register the default template dir.
      Module_Id.Dirs.Append
        (Create_From_Dir (Kernel.Get_System_Dir, "share/gps/templates"));

      Register_Command
        (Kernel,
         "add_templates_dir",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Template_Command_Handler'Access,
         Class         => Project_Template_Class,
         Static_Method => True);

      Register_Action
        (Kernel, "create project from template",
         new Project_From_Template_Command,
         -"Open a dialog to create a new project from an existing template");
   end Register_Module;

end Project_Templates.GPS;
