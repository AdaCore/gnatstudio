-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gtk.Handlers;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;

with Build_Configurations;        use Build_Configurations;
with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with Traces;                    use Traces;

with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Build_Command_Manager;     use Build_Command_Manager;

package body Builder_Facility_Module is

   Me : constant Debug_Handle := Create ("Builder_Facility_Module");

   package String_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Tool_Button_Record, String);

   package Buttons_Map is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Gtk_Tool_Button);

   package Target_XML_List is new Ada.Containers.Doubly_Linked_Lists
     (Node_Ptr);

   use Target_XML_List;

   package Target_Map is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Target_XML_List.List);
   --  The key in this map is a model name, and the element is a set of XML
   --  describing targets associated with this model.

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Registry : Build_Config_Registry_Access;

      Buttons  : Buttons_Map.Map;
      --  The set of toolbar buttons

      Unregistered_Targets : Target_Map.Map;
      --  This is a set of targets that could not be registered because their
      --  model was not registered at the time the module parsed this target.
      --  Every time a new model is registered, the module will look at this
      --  list of unregistered targets, and register all targets corresponding
      --  to the new model.
      --  Doing this means that targets can be passed to the module *before*
      --  their corresponding models. This way, there is no load order to
      --  maintain, and scripts can be executed in any order.
   end record;

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record'Class;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Log (M : String; Mode : Message_Mode);
   --  Logger for the registry.

   function Get_Kernel return Kernel_Handle;
   --  Utility function to get the kernel

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Launch the build manager

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   procedure Clear_Toolbar_Buttons;
   --  Remove the build-related buttons from the main toolbar

   procedure Install_Toolbar_Buttons;
   --  Install the build-related buttons into the main toolbar

   procedure Install_Button_For_Target (Target : Target_Access);
   --  Install one button in the toolbar

   function Get_Targets_File return GNATCOLL.VFS.Virtual_File;
   --  Return the file where user targets are stored.

   procedure Attempt_Target_Register (XML : Node_Ptr; Allow_Update : Boolean);
   --  Attempt to register target described in XML. If the model is not
   --  registered yet, add this target to the list of unregistered targets.
   --  Allow_Update indicates whether the data in the target can overwrite
   --  data in previously registered targets of the same type.

   procedure On_Button_Click
     (Widget : access Gtk_Tool_Button_Record'Class;
      Name   : String);
   --  Called when a user clicks on a toolbar button.
   --  Name is the name of the target corresponding to that button.

   procedure Save_Targets;
   procedure Load_Targets;
   --  Save/Load the targets in the user-defined XML

   ----------------------
   -- Get_Targets_File --
   ----------------------

   function Get_Targets_File return GNATCOLL.VFS.Virtual_File is
      Filename : constant String := Get_Home_Dir (Get_Kernel) & "targets.xml";
   begin
      return Create (Filename);
   end Get_Targets_File;

   ------------------
   -- Save_Targets --
   ------------------

   procedure Save_Targets is
      N       : Node_Ptr;
      Success : Boolean;
   begin
      N := Save_All_Targets_To_XML (Builder_Module_ID.Registry);
      Print (N, Full_Name (Get_Targets_File).all, Success);

      if not Success then
         Trace (Me, "Error when saving targets file");
      end if;
   end Save_Targets;

   ------------------
   -- Load_Targets --
   ------------------

   procedure Load_Targets is
      N : Node_Ptr;
      C : Node_Ptr;
   begin
      N := Parse (Full_Name (Get_Targets_File).all);

      if N = null then
         Trace (Me, "Error when loading targets file");
      else
         C := N.Child;

         while C /= null loop
            if C.Tag.all /= "target" then
               Trace (Me, "Error in targets file");
               return;
            end if;

            Attempt_Target_Register (C, True);

            C := C.Next;
         end loop;
      end if;
   end Load_Targets;

   -----------------------------
   -- Attempt_Target_Register --
   -----------------------------

   procedure Attempt_Target_Register
     (XML          : Node_Ptr;
      Allow_Update : Boolean)
   is
      Model_Name : Unbounded_String;
      T          : Target_Access;
      List       : Target_XML_List.List;
      use Target_Map;

   begin
      Model_Name := To_Unbounded_String (Get_Attribute (XML, "model", ""));

      --  Sanity check
      if Model_Name = "" then
         Log (-"Error: target XML description has an empty model", Error);
         return;
      end if;

      --  Check whether the model is registered
      if Is_Registered_Model (Builder_Module_ID.Registry, Model_Name) then
         --  The model is registered: add the target immediately

         T := Load_Target_From_XML
           (Builder_Module_ID.Registry, XML, Allow_Update);

         if T /= null
           and then Get_Properties (T).Icon_In_Toolbar
         then
            Install_Button_For_Target (T);
         end if;

      else
         --  The model is not registered: add XML to the list of unregistered
         --  targets

         --  Look for a list for the same model name

         if Builder_Module_ID.Unregistered_Targets.Contains (Model_Name) then
            List := Builder_Module_ID.Unregistered_Targets.Element
              (Model_Name);

            if Allow_Update then
               List.Append (Deep_Copy (XML));
            else
               List.Prepend (Deep_Copy (XML));
            end if;

            Builder_Module_ID.Unregistered_Targets.Replace (Model_Name, List);
         else
            List.Append (Deep_Copy (XML));
            Builder_Module_ID.Unregistered_Targets.Insert (Model_Name, List);
         end if;
      end if;
   end Attempt_Target_Register;

   ---------
   -- Log --
   ---------

   procedure Log (M : String; Mode : Message_Mode) is
      pragma Unreferenced (Mode);
      Kernel : constant Kernel_Handle := Get_Kernel (Builder_Module_ID.all);
   begin
      Insert (Kernel, (-"Build facility: ") & M);
   end Log;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel return Kernel_Handle is
   begin
      return Get_Kernel (Builder_Module_ID.all);
   end Get_Kernel;

   ---------------------------
   -- Clear_Toolbar_Buttons --
   ---------------------------

   procedure Clear_Toolbar_Buttons is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Get_Kernel);
      use Buttons_Map;

      C : Buttons_Map.Cursor;
   begin
      --  Browse through the registry and remove already added buttons

      C := Builder_Module_ID.Buttons.First;

      while Has_Element (C) loop
         Remove (Toolbar, Element (C));
         Next (C);
      end loop;

      --  Remove all buttons from registry

      Builder_Module_ID.Buttons.Clear;

   end Clear_Toolbar_Buttons;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Widget : access Gtk_Tool_Button_Record'Class;
      Name   : String)
   is
      pragma Unreferenced (Widget);
   begin
      Launch_Target (Get_Kernel, Builder_Module_ID.Registry, Name);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Button_Click;

   -------------------------------
   -- Install_Button_For_Target --
   -------------------------------

   procedure Install_Button_For_Target (Target : Target_Access) is
      Toolbar : constant Gtk_Toolbar   := Get_Toolbar (Get_Kernel);
      Button : Gtk.Tool_Button.Gtk_Tool_Button;
   begin
      Gtk_New_From_Stock (Button, Get_Icon (Target));
      Set_Label (Button, Get_Name (Target));
      Builder_Module_ID.Buttons.Insert
        (To_Unbounded_String (Get_Name (Target)), Button);
      Insert (Toolbar => Toolbar, Item    => Button);
      Show_All (Button);

      String_Callback.Connect
        (Button, "clicked",
         On_Button_Click'Access,
         Get_Name (Target));
   end Install_Button_For_Target;

   -----------------------------
   -- Install_Toolbar_Buttons --
   -----------------------------

   procedure Install_Toolbar_Buttons is
      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;

   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         if Get_Properties (T).Icon_In_Toolbar then
            Install_Button_For_Target (T);
         end if;

         Next (C);
      end loop;
   end Install_Toolbar_Buttons;

   ----------------------
   -- On_Build_Manager --
   ----------------------

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Changes_Made : Boolean;
   begin
      Configuration_Dialog
        (Builder_Module_ID.Registry,
         Get_Main_Window (Kernel),
         Get_Tooltips (Kernel),
         Changes_Made);

      if Changes_Made then
         Clear_Toolbar_Buttons;
         Install_Toolbar_Buttons;

         --  Save the user-defined targets
         Save_Targets;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Build_Manager;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Module, File);
      T            : Target_Access;
      Allow_Update : Boolean;
      Model_Name   : Unbounded_String;
      L            : Target_XML_List.List;
      C            : Target_XML_List.Cursor;

   begin
      if Node.Tag.all = "target" then
         --  If the Customization_Level we are parsing is user-specific, this
         --  means that we are looking at a target saved by the user. In this
         --  case we allow the XML to update the values of a previously
         --  registered target.

         Allow_Update := (Level = User_Specific);
         Attempt_Target_Register (Node, Allow_Update);

      elsif Node.Tag.all = "target-model" then
         Create_Model_From_XML (Builder_Module_ID.Registry, Node);

         --  We have just added a model: register any target pending on this
         --  model.

         Model_Name := To_Unbounded_String (Get_Attribute (Node, "name", ""));

         if Builder_Module_ID.Unregistered_Targets.Contains (Model_Name) then
            L := Builder_Module_ID.Unregistered_Targets.Element (Model_Name);

            if not L.Is_Empty then
               C := L.First;

               while Has_Element (C) loop
                  T := Load_Target_From_XML
                    (Builder_Module_ID.Registry, Element (C), True);

                  if T /= null
                    and then Get_Properties (T).Icon_In_Toolbar
                  then
                     Install_Button_For_Target (T);
                  end if;

                  --  Free memory
                  declare
                     Node_To_Free : Node_Ptr := Element (C);
                  begin
                     Free (Node_To_Free);
                  end;

                  Next (C);
               end loop;

               Builder_Module_ID.Unregistered_Targets.Delete (Model_Name);
            end if;
         end if;
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Build : constant String := '/' & (-"New builder") & '/';
   begin
      Builder_Module_ID := new Builder_Module_ID_Record;

      --  Initialise the registy
      Builder_Module_ID.Registry := Create (Log'Unrestricted_Access);

      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder Facility");

      Register_Menu (Kernel, "/_" & (-"New builder"), Ref_Item => -"Tools");
      Register_Menu (Kernel, Build, -"Build Manager", "",
                     On_Build_Manager'Access);

      --  Load the user-defined targets.
      Load_Targets;
   end Register_Module;

end Builder_Facility_Module;
