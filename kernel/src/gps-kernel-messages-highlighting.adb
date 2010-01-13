-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;
with Traces;

package body GPS.Kernel.Messages.Highlighting is

   use GPS.Kernel.Styles;
   use Traces;

   type On_File_Edited_Hook_Record
     (Manager : not null access Highlighting_Manager'Class) is
     new GPS.Kernel.Hooks.Function_With_Args with null record;

   type On_File_Edited_Hook is access On_File_Edited_Hook_Record'Class;

   overriding procedure Execute
     (Hook   : On_File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "file_edited" hook. Redirects call to highlighting
   --  manager.

   procedure Highlight
     (Self    : not null access Highlighting_Manager'Class;
      Message : not null access Abstract_Message'Class);
   --  Highlights location of the message in the source editor.

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : On_File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

   begin
      Hook.Manager.File_Opened
        (GPS.Kernel.Standard_Hooks.File_Hooks_Args (Data.all).File);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Execute;

   -----------------
   -- File_Opened --
   -----------------

   procedure File_Opened
     (Self : not null access Highlighting_Manager;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Controller : constant Messages_Container_Access :=
        Get_Messages_Container (Self.Kernel);
      Categories : constant Unbounded_String_Array :=
        Controller.Get_Categories;

   begin
      for J in Categories'Range loop
         declare
            Messages : constant Message_Array :=
              Controller.Get_Messages (Categories (J), File);

         begin
            for J in Messages'Range loop
               Self.Highlight (Messages (J));
            end loop;
         end;
      end loop;
   end File_Opened;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight
     (Self    : not null access Highlighting_Manager'Class;
      Message : not null access Abstract_Message'Class) is
   begin
      if Message.Get_Parent = null
        and then Message.Get_Highlighting_Style /= null
      then
         if Message.Get_Highlighting_Length /= 0 then
            Get_Buffer_Factory (Self.Kernel).Get
              (Message.Get_File, Open_View => False).Apply_Style
              (Message.Get_Highlighting_Style,
               Message.Get_Editor_Mark.Line,
               Message.Get_Editor_Mark.Column,
               Message.Get_Editor_Mark.Column
               + Message.Get_Highlighting_Length);

         else
            Get_Buffer_Factory (Self.Kernel).Get
              (Message.Get_File, Open_View => False).Apply_Style
              (Message.Get_Highlighting_Style, Message.Get_Editor_Mark.Line);
         end if;
      end if;
   end Highlight;

   ------------------------------
   -- Message_Property_Changed --
   ------------------------------

   overriding procedure Message_Property_Changed
     (Self     : not null access Highlighting_Manager;
      Message  : not null access Abstract_Message'Class;
      Property : String)
   is
   begin
      if Property = "highlighting" then
         Self.Highlight (Message);
      end if;
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class) is
   begin
      if Message.Get_Highlighting_Style /= null then
         Get_Buffer_Factory (Self.Kernel).Get
           (Message.Get_File, Open_View => False).Remove_Style
           (Message.Get_Highlighting_Style,
            Message.Get_Editor_Mark.Line,
            Message.Get_Editor_Mark.Column,
            Message.Get_Editor_Mark.Column + Message.Get_Highlighting_Length);
      end if;
   end Message_Removed;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class) is

      function To_Address is
        new Ada.Unchecked_Conversion
          (Highlighting_Manager_Access, System.Address);

      Manager : constant Highlighting_Manager_Access :=
        new Highlighting_Manager (Kernel);
      Hook    : constant On_File_Edited_Hook :=
        new On_File_Edited_Hook_Record (Manager);

   begin
      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (Manager));
      Kernel.Highlighting_Manager := To_Address (Manager);
      GPS.Kernel.Hooks.Add_Hook
        (Kernel,
         GPS.Kernel.File_Edited_Hook,
         Hook,
         "location_view.file_edited");
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      function To_Highlighting_Manager is
        new Ada.Unchecked_Conversion
          (System.Address, Highlighting_Manager_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Highlighting_Manager'Class, Highlighting_Manager_Access);

      Manager : Highlighting_Manager_Access :=
                  To_Highlighting_Manager (Kernel.Highlighting_Manager);

   begin
      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (Manager));
      Free (Manager);
   end Unregister;

end GPS.Kernel.Messages.Highlighting;
