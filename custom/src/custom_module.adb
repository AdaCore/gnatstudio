-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Glib;                    use Glib;
with Glib.Xml_Int;            use Glib.Xml_Int;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Ada.Exceptions;          use Ada.Exceptions;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Intl;              use Glide_Intl;

with Traces;                  use Traces;
with Commands;                use Commands;
with Commands.Custom;         use Commands.Custom;
with String_Utils;            use String_Utils;

package body Custom_Module is

   Me : constant Debug_Handle := Create ("Custom_Module");

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      File      : constant String :=
        String_Utils.Name_As_Directory (Get_Home_Dir (Kernel)) & "custom";
      Node      : Node_Ptr;
      Child     : Node_Ptr;
      Current_Path : String_Access;

      procedure Add_Child
        (Parent_Path  : String;
         Current_Node : Node_Ptr);
      --  Add a menuitem or submenu to the Parent_Path, according to
      --  what Current_Node contains.

      procedure Add_Child
        (Parent_Path  : String;
         Current_Node : Node_Ptr)
      is
         Current_Child : Node_Ptr;
         Current_Child_Child : Node_Ptr;
         Command       : Command_Access := null;
         Current_Title : String_Access;
      begin
         if Current_Node = null
           or else Current_Node.Tag = null
         then
            return;
         end if;

         if Current_Node.Tag.all = "Submenu" then
            Current_Child := Current_Node.Child;

            if Current_Child.Tag.all = "Title" then
               Current_Title := new String'
                 (Parent_Path & "/" & Current_Child.Value.all);

               Current_Child_Child := Current_Child.Next;

               while Current_Child_Child /= null loop
                  Add_Child (Current_Title.all,
                             Current_Child_Child);
                  Current_Child_Child := Current_Child_Child.Next;
               end loop;
            end if;

         elsif Current_Node.Tag.all = "Menuitem" then
            Current_Child := Current_Node.Child;

            while Current_Child /= null loop
               if Current_Child.Tag.all = "Title" then
                  if Current_Title /= null then
                     Free (Current_Title);
                  end if;

                  Current_Title := new String' (Current_Child.Value.all);
               end if;

               if Current_Child.Tag.all = "Action" then
                  declare
                     Args : Argument_List_Access
                       := Argument_String_To_List (Current_Child.Value.all);
                     A : Argument_List_Access;
                     C : Custom_Command_Access;
                  begin
                     if Args'Length > 1 then
                        A := new Argument_List'
                          (Args (Args'First + 1 .. Args'Last));
                     end if;

                     Create (C,
                             Kernel_Handle (Kernel),
                             Args (Args'First).all,
                             A);
                     if Command = null then
                        Command := Command_Access (C);
                     else
                        Add_Consequence_Action (Command, Command_Access (C));
                     end if;
                  end;
               end if;

               Current_Child := Current_Child.Next;
            end loop;

            Register_Menu (Kernel,
                           Parent_Path,
                           Current_Title.all,
                           "",
                           null,
                           Command);
         end if;

         Free (Current_Title);
      end Add_Child;

   begin
      if not Is_Regular_File (File) then
         return;
      end if;

      Node := Parse (File).Child;

      Register_Module
        (Module                  => Custom_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Custom_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

      while Node /= null loop
         if Node.Tag.all = "Menu" then
            Child := Node.Child;

            if Child.Tag.all = "Title" then
               Current_Path := new String' (Child.Value.all);

               while Child /= null loop
                  Add_Child ("/" & Current_Path.all, Child);
                  Child := Child.Next;
               end loop;
            end if;
         end if;

         Node := Node.Next;
      end loop;

      Free (Current_Path);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Register_Module;

end Custom_Module;
