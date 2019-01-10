------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Strings.Hash;
with GPS.Editors;                    use GPS.Editors;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.References;
with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;
with Src_Editor_Box;                 use Src_Editor_Box;
with Src_Editor_Buffer;              use Src_Editor_Buffer;

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

package body Src_Editor_Module.Messages is
   use Style_Maps;
   use Style_Sets;

   type On_File_Edited is new File_Hooks_Function with record
      Manager : not null access Highlighting_Manager'Class;
   end record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_edited" hook. Redirects call to highlighting
   --  manager.

   function Get
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Source_Buffer;
   --  Return the editor for File

   ---------
   -- Get --
   ---------

   function Get
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Source_Buffer
   is
      Child : MDI_Child;
      Box   : Source_Editor_Box;
   begin
      if File /= GNATCOLL.VFS.No_File then
         Child := Find_Editor
           (Kernel, File,
            GNATCOLL.Projects.No_Project);  --  ??? any project
      end if;

      if Child /= null then
         Box := Get_Source_Box_From_MDI (Child);
         return Get_Buffer (Box);
      end if;

      return null;
   end Get;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Kernel);
   begin
      Self.Manager.File_Opened (File);
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

      B : Source_Buffer;
   begin
      for J in Categories'Range loop
         declare
            Messages : Message_Array :=
              Controller.Get_Messages (Categories (J), File);
            Last     : Natural := 0;
            pragma Assert (Messages'First = 1);

         begin
            --  Lookup B only once
            if B = null then
               B := Get (Self.Kernel, File);

               --  If we could not find an editor at this stage, return.
               if B = null then
                  return;
               end if;
            end if;

            --  Take message flags into account

            for J in Messages'Range loop
               if Messages (J).Get_Flags /= Empty_Message_Flags then
                  Last := Last + 1;

                  if Last /= J then
                     Messages (Last) := Messages (J);
                  end if;
               end if;
            end loop;

            if Last > 0 then
               Add_File_Information
                 (B, To_String (Categories (J)), Messages (1 .. Last));
            end if;
         end;
      end loop;
   end File_Opened;

   ------------------
   -- File_Removed --
   ------------------

   overriding procedure File_Removed
     (Self     : not null access Highlighting_Manager;
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      procedure Free is
        new Unchecked_Deallocation (Style_Sets.Set, Style_Set_Access);

      Map_Position : Style_Maps.Cursor := Self.Map.Find ((Category, File));
      Styles       : Style_Set_Access;
      Set_Position : Style_Sets.Cursor;

   begin
      if Has_Element (Map_Position) then
         Styles := Style_Maps.Element (Map_Position);
         Self.Map.Delete (Map_Position);

         Set_Position := Styles.First;

         while Has_Element (Set_Position) loop
            Get_Buffer_Factory (Self.Kernel).Get
              (File, Open_View => False).Remove_Style
              (Get_Name (Style_Sets.Element (Set_Position)),
               0,
               0,
               0);

            Next (Set_Position);
         end loop;

         Free (Styles);
      end if;
   end File_Removed;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Style_Access) return Containers.Hash_Type is
   begin
      return Strings.Hash (Get_Name (Item));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Key) return Containers.Hash_Type is
   begin
      return
        Strings.Hash
          (To_String (Item.Category) & String (Item.File.Full_Name.all));
   end Hash;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class)
   is
      B : Source_Buffer;

   begin
      B := Get (Self.Kernel, Message.Get_File);

      if B /= null then
         Add_File_Information
           (B, Message.Get_Category, (1 => Message_Access (Message)));
      end if;
   end Message_Added;

   ------------------------------
   -- Message_Property_Changed --
   ------------------------------

   overriding procedure Message_Property_Changed
     (Self     : not null access Highlighting_Manager;
      Message  : not null access Abstract_Message'Class;
      Property : String)
 is
      B : Source_Buffer;
   begin
      if Property = "highlighting" then
         B := Get (Self.Kernel, Message.Get_File);
         if B = null then
            --  This can happen, for instance when loading GPS. In which case
            --  we can return safely, since the messages will be added as part
            --  of On_File_Opened.
            return;
         end if;

         Highlight_Message (Buffer        => B,
                            Editable_Line => 0,
                            Buffer_Line   => 0,
                            Message       => Message_Access (Message));

         --  ??? We should refresh the message when property is "action"
      end if;
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class)
   is
      B      : Source_Buffer;

   begin
      B := Get (Self.Kernel, Message.Get_File);

      if B /= null then
         Remove_Message
           (B, GPS.Kernel.Messages.References.Create
              (Message_Access (Message)));
      else
         --  It is possible that B = null, for instance if a container decides
         --  to remove the messages in reaction to a "file_closed" event.
         --  (This is the case for the vdiff view, for instance).
         --  In this case, all we have to do is remove the note contained in
         --  the message

         Free_Note (Message_Access (Message));
      end if;
   end Message_Removed;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class) is

      function To_Address is
        new Unchecked_Conversion (Highlighting_Manager_Access, System.Address);

      Id      : constant Source_Editor_Module :=
                  Source_Editor_Module (Src_Editor_Module_Id);
      Manager : constant Highlighting_Manager_Access :=
                  new Highlighting_Manager (Kernel);

   begin
      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (Manager),
         (Editor_Line => True, Editor_Side => True, Locations => False));
      Id.Highlighting_Manager := To_Address (Manager);
      File_Edited_Hook.Add
         (new On_File_Edited'(File_Hooks_Function with Manager => Manager),
          Last => True);
      --  Register this hook with Last => True, so that it is called after the
      --  one (registered in Src_Editor_Module.Register_Module that reacts to
      --  file_edited and updates marks.
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      function To_Highlighting_Manager is
        new Unchecked_Conversion (System.Address, Highlighting_Manager_Access);

      procedure Free is
        new Unchecked_Deallocation
          (Highlighting_Manager'Class, Highlighting_Manager_Access);

      Id      : constant Source_Editor_Module :=
                  Source_Editor_Module (Src_Editor_Module_Id);
      Manager : Highlighting_Manager_Access :=
                  To_Highlighting_Manager (Id.Highlighting_Manager);

   begin
      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (Manager));
      Free (Manager);
   end Unregister;

end Src_Editor_Module.Messages;
