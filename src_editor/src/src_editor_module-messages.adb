-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2010, AdaCore                      --
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
with Ada.Strings.Hash;

with GPS.Editors; use GPS.Editors;

with GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;
with Traces;

with Src_Editor_Box; use Src_Editor_Box;

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

package body Src_Editor_Module.Messages is

   use Ada.Strings.Unbounded;
   use Style_Maps;
   use Style_Sets;
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

   procedure Set_Action
     (Self    : not null access Highlighting_Manager'Class;
      Message : not null access Abstract_Message'Class);
   --  Adds an action to the message in the source editor

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
         Child := Find_Editor (Kernel, File);
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

   ------------------
   -- File_Removed --
   ------------------

   overriding procedure File_Removed
     (Self     : not null access Highlighting_Manager;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Style_Sets.Set, Style_Set_Access);

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
              (Style_Sets.Element (Set_Position),
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
     (Item : GPS.Styles.Style_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Get_Name (Item));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Key) return Ada.Containers.Hash_Type is
   begin
      return
        Ada.Strings.Hash
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
           (B,
            Message.Get_Category,
            (1 => Message_Access (Message)));
      end if;
   end Message_Added;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action
     (Self    : not null access Highlighting_Manager'Class;
      Message : not null access Abstract_Message'Class)
   is
      B : Source_Buffer;

   begin
      B := Get (Self.Kernel, Message.Get_File);

      if B /= null then
         --  ??? Here we re-add all messages, but we could simply recompute the
         --  width of the column info
         Add_File_Information
           (B,
            Message.Get_Category,
            (1 => Message_Access (Message)));
      end if;
   end Set_Action;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight
     (Self    : not null access Highlighting_Manager'Class;
      Message : not null access Abstract_Message'Class) is
   begin
      --  ??? Possible optimization: we should be able to highlight without
      --  going through the buffer API here.
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
              (Message.Get_Highlighting_Style,
               Message.Get_Editor_Mark.Line);
         end if;

         declare
            K        : constant Key :=
              (Message.Get_Category, Message.Get_File);
            Position : constant Style_Maps.Cursor := Self.Map.Find (K);
            Styles   : Style_Set_Access;

         begin
            if Has_Element (Position) then
               Styles := Style_Maps.Element (Position);

            else
               Styles := new Style_Sets.Set;
               Self.Map.Insert (K, Styles);
            end if;

            if not Styles.Contains (Message.Get_Highlighting_Style) then
               Styles.Insert (Message.Get_Highlighting_Style);
            end if;
         end;
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

      elsif Property = "action" then
         Self.Set_Action (Message);
      end if;
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class)
   is
      Buffer : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Self.Kernel).Get
        (Message.Get_File, Open_View => False);

      B : Source_Buffer;

   begin
      B := Get (Self.Kernel, Message.Get_File);

      if Message.Get_Highlighting_Style /= null then
         Buffer.Remove_Style
           (Message.Get_Highlighting_Style,
            Message.Get_Editor_Mark.Line,
            Message.Get_Editor_Mark.Column,
            Message.Get_Editor_Mark.Column + Message.Get_Highlighting_Length);
         null;
      end if;

      if B /= null then
         Remove_Messages (B, (1 => Message_Access (Message)));
      end if;
   end Message_Removed;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class) is
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      function To_Address is
        new Ada.Unchecked_Conversion
          (Highlighting_Manager_Access, System.Address);

      Manager : constant Highlighting_Manager_Access :=
        new Highlighting_Manager (Kernel);
      Hook    : constant On_File_Edited_Hook :=
        new On_File_Edited_Hook_Record (Manager);

   begin
      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (Manager),
         (Editor_Side => True, Locations => False));
      Id.Highlighting_Manager := To_Address (Manager);
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
      Id : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);

      function To_Highlighting_Manager is
        new Ada.Unchecked_Conversion
          (System.Address, Highlighting_Manager_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Highlighting_Manager'Class, Highlighting_Manager_Access);

      Manager : Highlighting_Manager_Access :=
        To_Highlighting_Manager (Id.Highlighting_Manager);

   begin
      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (Manager));
      Free (Manager);
   end Unregister;

end Src_Editor_Module.Messages;
