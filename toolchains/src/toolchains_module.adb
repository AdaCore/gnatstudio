-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2008, AdaCore                  --
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

with System.OS_Lib;             use System.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gtk.Dialog;                use Gtk.Dialog;
with Traces;                    use Traces;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Toolchains;                use Toolchains;
with Toolchains_Dialog;         use Toolchains_Dialog;
with Projects;
with Projects.Registry;
with Builder_Facility_Module;

package body Toolchains_Module is

   type Toolchains_Property is new GPS.Kernel.Properties.Property_Record
   with record
      Active           : Boolean;
      Tools_Path       : String_Access;
      Use_Xrefs_Subdir : Boolean;
      Compiler_Path    : String_Access;
   end record;

   overriding procedure Save
     (Property : access Toolchains_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr);
   overriding procedure Load
     (Property : in out Toolchains_Property; From : Glib.Xml_Int.Node_Ptr);
   overriding procedure Destroy (Property : in out Toolchains_Property);
   --  See inherited doc.

   pragma Warnings (Off); --  Yes, it's not dispatching and it's expected.
   procedure Apply
     (Property : Toolchains_Property;
      Kernel   : GPS.Kernel.Kernel_Handle);
   pragma Warnings (On);
   --  Applies the property.

   procedure On_Menu
     (Widget : access GObject_Record'Class; Kernel : GPS.Kernel.Kernel_Handle);
   --  Config menu

   pragma Warnings (Off); --  Yes, it's not dispatching and it's expected.
   function Get_Property return Toolchains_Property;
   pragma Warnings (On);
   --  Retrieve the global property

   procedure On_GPS_Started
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when GPS is starting

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Toolchains_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr)
   is
      Child : Glib.Xml_Int.Node_Ptr;
   begin
      if Property.Active then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("active");
         Add_Child (Node, Child);
      end if;

      if Property.Use_Xrefs_Subdir then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("use_xrefs_subdir");
         Add_Child (Node, Child);
      end if;

      if Property.Tools_Path /= null then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("tools_path");
         Child.Value := new String'(Property.Tools_Path.all);
         Add_Child (Node, Child);
      end if;

      if Property.Compiler_Path /= null then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("compiler_path");
         Child.Value := new String'(Property.Compiler_Path.all);
         Add_Child (Node, Child);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Toolchains_Property; From : Glib.Xml_Int.Node_Ptr)
   is
      Child : Glib.Xml_Int.Node_Ptr;
   begin
      Child := Find_Tag (From.Child, "active");
      Property.Active := Child /= null;

      --  Set Use_Xrefs_Subdir on by default when the module is not active.
      --  This has no impact on its actual xrefs_subdir state (Property.Active
      --  state is always checked first), but this will check the corresponding
      --  button in the dialog by default, which is a desirable thing.
      Child := Find_Tag (From.Child, "use_xrefs_subdir");
      Property.Use_Xrefs_Subdir := Child /= null or else not Property.Active;

      Child := Find_Tag (From.Child, "tools_path");
      if Child /= null then
         Property.Tools_Path := new String'(Child.Value.all);
      else
         Property.Tools_Path := new String'("");
      end if;

      Child := Find_Tag (From.Child, "compiler_path");
      if Child /= null then
         Property.Compiler_Path := new String'(Child.Value.all);
      else
         Property.Compiler_Path := new String'("");
      end if;
   end Load;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out Toolchains_Property) is
   begin
      Free (Property.Tools_Path);
      Free (Property.Compiler_Path);
   end Destroy;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Property : Toolchains_Property;
      Kernel   : GPS.Kernel.Kernel_Handle) is
   begin
      if Property.Tools_Path /= null
        and then Property.Compiler_Path /= null
      then
         Toolchains.Set_Toolchains_Properties
           (Active               => Property.Active,
            Tool_Search_Path     => Property.Tools_Path.all,
            Compiler_Search_Path => Property.Compiler_Path.all);
      else
         Toolchains.Set_Toolchains_Properties
           (Active               => Property.Active,
            Tool_Search_Path     => "",
            Compiler_Search_Path => "");
      end if;

      if Property.Active
        and then Property.Use_Xrefs_Subdir
      then
         --  ??? .xrefs and mode "xref" should not be string literals, but
         --  stored somewhere instead.
         if Projects.Registry.Get_Xrefs_Subdir
           (Get_Registry (Kernel).all) /= ".xrefs"
         then
            Projects.Registry.Set_Xrefs_Subdir
              (Get_Registry (Kernel).all, ".xrefs");
            GPS.Kernel.Project.Recompute_View (Kernel);
         end if;

         Builder_Facility_Module.Set_Subdir ("xref", ".xrefs");
         Builder_Facility_Module.Activate_Mode ("xref", True);

      else
         if Projects.Registry.Get_Xrefs_Subdir
           (Get_Registry (Kernel).all) /= ""
         then
            Projects.Registry.Set_Xrefs_Subdir
              (Get_Registry (Kernel).all, "");
            GPS.Kernel.Project.Recompute_View (Kernel);
         end if;

         Builder_Facility_Module.Set_Subdir ("xref", ".");
         Builder_Facility_Module.Activate_Mode ("xref", False);
      end if;
   end Apply;

   --------------------
   -- On_GPS_Started --
   --------------------

   procedure On_GPS_Started
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Property : constant Toolchains_Property := Get_Property;
   begin
      Apply (Property, GPS.Kernel.Kernel_Handle (Kernel));
   end On_GPS_Started;

   -------------
   -- On_Menu --
   -------------

   procedure On_Menu
     (Widget : access GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Property      : Toolchains_Property := Get_Property;
      Prop_Access   : Property_Access;
      Dialog        : Toolchains_Dialog.Dialog;
      Resp          : Gtk_Response_Type;
      Compiler      : constant String :=
                        Projects.Get_Attribute_Value
                          (GPS.Kernel.Project.Get_Project (Kernel),
                           Projects.Compiler_Command_Attribute,
                           Default => "gnatmake",
                           Index   => "Ada");
      Default_Path  : String_Access;
      Tools_Path    : String_Access;
      Compiler_Path : String_Access;

   begin
      if Property.Tools_Path = null
        or else Property.Tools_Path.all = ""
        or else Property.Compiler_Path = null
        or else Property.Compiler_Path.all = ""
      then
         declare
            Path : String_Access := Locate_Exec_On_Path (Compiler);
         begin
            if Path /= null then
               Default_Path := new String'(Dir_Name (Path.all));
               Free (Path);
            else
               Default_Path := new String'("");
            end if;
         end;
      end if;

      if Property.Tools_Path = null then
         Tools_Path := Default_Path;
      elsif Property.Tools_Path.all = "" then
         Tools_Path := Default_Path;
      else
         Tools_Path := Property.Tools_Path;
      end if;

      if Property.Compiler_Path = null then
         Compiler_Path := Default_Path;
      elsif Property.Compiler_Path.all = "" then
         Compiler_Path := Default_Path;
      else
         Compiler_Path := Property.Compiler_Path;
      end if;

      Gtk_New
        (Dialog, Kernel, Property.Active,
         Tools_Path.all, Property.Use_Xrefs_Subdir,
         Compiler_Path.all);

      if Default_Path /= null then
         Free (Default_Path);
      end if;

      Resp := Dialog.Run;

      if Resp = Gtk_Response_OK then
         Property.Active := Get_Active (Dialog);
         Property.Tools_Path := new String'(Get_Tools_Path (Dialog));
         Property.Compiler_Path := new String'(Get_Compiler_Path (Dialog));
         Property.Use_Xrefs_Subdir := Get_Use_Xrefs_Subdir (Dialog);

         Prop_Access := new Toolchains_Property'(Property);
         Set_Property
           (Kernel,
            Index_Name  => "toolchains_property",
            Index_Value => "toolchains_property",
            Name        => "property",
            Property    => Prop_Access,
            Persistent  => True);

         Apply (Property, Kernel);
      end if;

      Destroy (Dialog);
   end On_Menu;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property return Toolchains_Property is
      Property : Toolchains_Property;
      Success  : Boolean;
   begin
      Get_Property
        (Property,
         Index_Name  => "toolchains_property",
         Index_Value => "toolchains_property",
         Name        => "property",
         Found       => Success);

      if not Success then
         return Toolchains_Property'
           (Active           => False,
            Tools_Path       => null,
            Use_Xrefs_Subdir => True,
            Compiler_Path    => null);
      else
         return Property;
      end if;
   end Get_Property;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools    : constant String := '/' & (-"Build/Settings") & '/';
   begin
      Register_Menu (Kernel, Tools, -"T_oolchains", "", On_Menu'Access);
      --  Load the property after all modules and plug-ins are loaded.
      Add_Hook (Kernel, GPS_Started_Hook,
                Wrapper (On_GPS_Started'Access),
                Name  => "toolchains_module.gps_started");
   end Register_Module;

end Toolchains_Module;
