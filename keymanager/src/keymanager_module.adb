-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
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

with Glide_Kernel; use Glide_Kernel;
with Gdk.Event;    use Gdk.Event;
with Gdk.Types;    use Gdk.Types;
with Glib.Xml_Int; use Glib.Xml_Int;
with Commands;     use Commands;
with HTables;      use HTables;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GUI_Utils;    use GUI_Utils;

package body KeyManager_Module is

   type Keys_Header_Num is range 0 .. 1000;
   type Key_Binding is record
      Key      : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type;
   end record;

   type Key_Description is record
      Name    : String_Access;
      Tooltip : String_Access;
      Command : Command_Access;
      On_Key_Press, On_Key_Release : Boolean;
   end record;
   No_Key : constant Key_Description := (null, null, null, False, False);

   function Hash (Key : Key_Binding) return Keys_Header_Num;
   procedure Free (Element : in out Key_Description);
   --  Support functions for creating the htable

   package Key_Htable is new Simple_HTable
     (Header_Num   => Keys_Header_Num,
      Element      => Key_Description,
      Free_Element => Free,
      No_Element   => No_Key,
      Key          => Key_Binding,
      Hash         => Hash,
      Equal        => "=");
   use Key_Htable;

   type Key_Manager_Record is new Glide_Kernel.Key_Handler_Record with record
      Kernel : Kernel_Handle;
      Table  : Key_Htable.HTable;
   end record;
   type Key_Manager_Access is access all Key_Manager_Record'Class;

   procedure Register_Key
     (Handler        : access Key_Manager_Record;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : access Commands.Root_Command'Class;
      Tooltip        : String := "";
      On_Key_Press   : Boolean := True;
      On_Key_Release : Boolean := False);
   function Process_Event
     (Handler  : access Key_Manager_Record;
      Event    : Event_Data) return Boolean;
   procedure Free (Handler : in out Key_Manager_Record);
   --  See documentation for imported subprograms

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record'Class);
   --  Load the customized key bindings

   ----------
   -- Hash --
   ----------

   function Hash (Key : Key_Binding) return Keys_Header_Num is
   begin
      return Keys_Header_Num
      ((Integer (Key.Key) + Integer (Key.Modifier) * 16#FFFF#)
         mod Integer (Keys_Header_Num'Last + 1));
   end Hash;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out Key_Description) is
   begin
      Free (Element.Name);
      Free (Element.Tooltip);
      Destroy (Element.Command);
   end Free;

   ------------------
   -- Register_Key --
   ------------------

   procedure Register_Key
     (Handler        : access Key_Manager_Record;
      Name           : String;
      Default_Key    : Gdk.Types.Gdk_Key_Type;
      Default_Mod    : Gdk.Types.Gdk_Modifier_Type;
      Command        : access Commands.Root_Command'Class;
      Tooltip        : String := "";
      On_Key_Press   : Boolean := True;
      On_Key_Release : Boolean := False)
   is
      Iter    : Key_Htable.Iterator;
      Binding, Binding2 : Key_Description;
   begin
      Binding2 := (Name           => new String'(Name),
                   Tooltip        => new String'(Tooltip),
                   Command        => Command_Access (Command),
                   On_Key_Press   => On_Key_Press,
                   On_Key_Release => On_Key_Release);

      --  Chech whether command is already associated with a key binding
      --  We do not use the most efficient method, since we simply
      --  traverse a list, but there aren't hundreds of keybindings...

      Get_First (Handler.Table, Iter);
      loop
         Binding := Get_Element (Iter);
         exit when Binding = No_Key;

         if Binding.Name.all = Name then
            --  Keep the current key binding, since it was probably customized
            --  by the user
            Set (Handler.Table, Get_Key (Iter), Binding2);
            return;
         end if;

         Get_Next (Handler.Table, Iter);
      end loop;

      Set (Handler.Table, Key_Binding'(Default_Key, Default_Mod), Binding2);
   end Register_Key;

   -------------------
   -- Process_Event --
   -------------------

   function Process_Event
     (Handler  : access Key_Manager_Record;
      Event    : Event_Data) return Boolean
   is
      Key   : constant Gdk_Key_Type      := Get_Key_Val (Get_Event (Event));
      Modif : constant Gdk_Modifier_Type := Get_State (Get_Event (Event));
      Binding : constant Key_Description :=
        Get (Handler.Table, (Key, Modif));
   begin
      if Binding /= No_Key
        and then Binding.Command /= null
        and then
          ((Get_Event_Type (Get_Event (Event)) = Key_Press
            and then Binding.On_Key_Press)
           or else (Get_Event_Type (Get_Event (Event)) = Key_Release
                    and then Binding.On_Key_Release))
        and then Execute (Binding.Command) = Success
      then
         return True;
      end if;

      return False;
   end Process_Event;

   ----------
   -- Free --
   ----------

   procedure Free (Handler : in out Key_Manager_Record) is
      Filename : constant String := Get_Home_Dir (Handler.Kernel) & "keys.xml";
      File, Child : Node_Ptr;
      Iter : Key_Htable.Iterator;
      Binding : Key_Description;
   begin
      File     := new Node;
      File.Tag := new String'("Keys");

      Get_First (Handler.Table, Iter);
      loop
         Binding := Get_Element (Iter);
         exit when Binding = No_Key;

         Child := new Node;
         Child.Tag := new String'("Key");
         Set_Attribute (Child, "name", Binding.Name.all);
         Child.Value := new String'
           (Image (Get_Key (Iter).Key, Get_Key (Iter).Modifier));

         Add_Child (File, Child);

         Get_Next (Handler.Table, Iter);
      end loop;

      Print (File, Filename);
      Free (File);

      Reset (Handler.Table);
   end Free;

   ----------------------
   -- Load_Custom_Keys --
   ----------------------

   procedure Load_Custom_Keys
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : access Key_Manager_Record'Class)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "keys.xml";
      File, Child : Node_Ptr;
      Key : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type;
      Binding : Key_Description;
   begin
      File := Parse (Filename);
      Child := File.Child;

      while Child /= null loop
         Value (Child.Value.all, Key, Modif);
         Binding := (Name    => new String'(Get_Attribute (Child, "name")),
                     Tooltip => null,
                     Command => null,
                     On_Key_Press   => False,
                     On_Key_Release => False);
         Set (Manager.Table, (Key, Modif), Binding);
         Child := Child.Next;
      end loop;

      Free (File);
   end Load_Custom_Keys;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Manager : constant Key_Manager_Access := new Key_Manager_Record;
   begin
      Manager.Kernel := Kernel_Handle (Kernel);
      Load_Custom_Keys (Kernel, Manager);
      Set_Key_Handler (Kernel, Manager);
   end Register_Module;

end KeyManager_Module;
