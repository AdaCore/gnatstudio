------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Assertions;         use System.Assertions;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GUI_Utils;                 use GUI_Utils;
with XML_Utils;                 use XML_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with XML_Parsers;

package body GPS.Kernel.Custom is

   Me            : constant Trace_Handle := Create ("GPS.KERNEL.CUSTOM");
   XML_Extension : constant Filesystem_String := ".xml";

   use Scripts_Hash.String_Hash_Table;

   type Scripts_Htable_Record is new Root_Table with record
      Table : Scripts_Hash.String_Hash_Table.Instance;
   end record;
   type Scripts_Htable_Access is access all Scripts_Htable_Record'Class;

   overriding procedure Reset (Table : access Scripts_Htable_Record);
   --  Reset the table

   procedure Parse_Custom_Dir
     (Kernel           : access Kernel_Handle_Record'Class;
      Directory        : Virtual_File;
      Level            : Customization_Level;
      Default_Autoload : Boolean;
      Force_Load       : Boolean);
   --  Parse and process all the XML files in the directory. Only those files
   --  that should be automatically loaded according to
   --  $HOME/.gnatstudio/startup.xml and Default_Autoload are loaded.

   -------------------------
   -- Autoload_System_Dir --
   -------------------------

   function Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir (Get_System_Dir (Kernel), "share/gps/plug-ins");
   end Autoload_System_Dir;

   ----------------------------
   -- No_Autoload_System_Dir --
   ----------------------------

   function No_Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir (Get_System_Dir (Kernel), "share/gps/library");
   end No_Autoload_System_Dir;

   -----------------------
   -- Autoload_User_Dir --
   -----------------------

   function Autoload_User_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File
   is
      Dir : constant Virtual_File :=
              Create_From_Dir (Get_Home_Dir (Kernel), "plug-ins");
   begin
      if not Is_Directory (Dir) then
         Make_Dir (Dir);
      end if;

      return Dir;
   end Autoload_User_Dir;

   ----------------------
   -- Support_Core_Dir --
   ----------------------

   function Support_Core_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir
         (Get_System_Dir (Kernel), "share/gps/support/core");
   end Support_Core_Dir;

   --------------------
   -- Support_UI_Dir --
   --------------------

   function Support_UI_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir
         (Get_System_Dir (Kernel), "share/gps/support/ui");
   end Support_UI_Dir;

   ---------------------------
   -- Support_Languages_Dir --
   ---------------------------

   function Support_Languages_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir
         (Get_System_Dir (Kernel), "share/gps/support/languages");
   end Support_Languages_Dir;

   -----------------------------
   -- Support_No_Autoload_Dir --
   -----------------------------

   function Support_No_Autoload_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File is
   begin
      return Create_From_Dir
         (Get_System_Dir (Kernel), "share/gps/support/noload");
   end Support_No_Autoload_Dir;

   ---------------------
   -- Get_Custom_Path --
   ---------------------

   function Get_Custom_Path return File_Array is
      Env : constant String := Getenv_With_Fallback
        ("GNATSTUDIO_CUSTOM_PATH", "GPS_CUSTOM_PATH");
   begin
      return From_Path (+Env);
   end Get_Custom_Path;

   ----------------------
   -- Parse_Custom_Dir --
   ----------------------

   procedure Parse_Custom_Dir
     (Kernel    : access Kernel_Handle_Record'Class;
      Directory : Virtual_File;
      Level     : Customization_Level;
      Default_Autoload : Boolean;
      Force_Load       : Boolean)
   is
      Files : File_Array_Access;
      File_Node : Node_Ptr;
   begin
      if Is_Directory (Directory) then
         Files := Read_Dir (Directory, Files_Only);

         for J in Files'Range loop
            declare
               F     : Virtual_File renames Files (J);
               Error : GNAT.Strings.String_Access;
            begin
               if File_Extension (F) = XML_Extension
                 and then Is_Regular_File (F)
               then
                  if Force_Load
                    or else Load_File_At_Startup
                      (Kernel, F,
                       Default        => Default_Autoload)
                  then
                     Trace (Me, "Loading " & Display_Full_Name (F));

                     XML_Parsers.Parse (F, File_Node, Error);

                     if File_Node = null then
                        Trace (Me, "Could not parse XML file: "
                               & Display_Full_Name (F));
                        Kernel.Insert (Error.all, Mode => GPS.Kernel.Error);
                        Free (Error);
                     else
                        Execute_Customization_String
                          (Kernel, F, File_Node.Child, Level);
                        Free (File_Node);
                     end if;
                  end if;
               end if;

            exception
               when Assert_Failure =>
                  Kernel.Insert
                    (-"Could not parse custom file "
                     & Display_Full_Name (F), Mode => GPS.Kernel.Error);
            end;
         end loop;

         Unchecked_Free (Files);
      end if;

   exception
      when VFS_Directory_Error =>
         null;
   end Parse_Custom_Dir;

   ------------------------------
   -- Load_System_Custom_Files --
   ------------------------------

   procedure Load_System_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Env_Path : constant File_Array := Get_Custom_Path;
      N        : Node_Ptr;

   begin
      Kernel.Custom_Files_Loaded := System_Level;

      --  Load the hard-coded customization strings first, so that anyone
      --  can override them.

      if Kernel.Customization_Strings /= null then
         Trace (Me, "Executing customization strings previously registered");
         Execute_Customization_String
           (Kernel,
            No_File,
            Kernel.Customization_Strings,
            Hard_Coded);

         --  Can't call Free itself, since it doesn't free the siblings
         while Kernel.Customization_Strings /= null loop
            N := Kernel.Customization_Strings;
            Kernel.Customization_Strings := Kernel.Customization_Strings.Next;
            Free (N);
         end loop;
      end if;

      --  Load the system plugins directory first, so that its contents can
      --  be overridden locally by the user
      Parse_Custom_Dir
        (Kernel, Autoload_System_Dir (Kernel), System_Wide,
         Default_Autoload => True, Force_Load => False);
      Parse_Custom_Dir
        (Kernel, No_Autoload_System_Dir (Kernel), System_Wide,
         Default_Autoload => False, Force_Load => False);

      for J in Env_Path'Range loop
         if Env_Path (J) /= No_File then
            Trace (Me, "Loading XML file from "
                & Env_Path (J).Display_Full_Name);
            Parse_Custom_Dir
              (Kernel, Env_Path (J), Project_Wide,
               Default_Autoload => True, Force_Load => False);
         end if;
      end loop;
   end Load_System_Custom_Files;

   ----------------------------
   -- Load_User_Custom_Files --
   ----------------------------

   procedure Load_User_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      N : Node_Ptr;
   begin
      Kernel.Custom_Files_Loaded := User_Level;

      --  Load the hard-coded customization strings first, so that anyone
      --  can override them.

      if Kernel.Customization_Strings /= null then
         Trace (Me, "Executing customization strings previously registered");
         Execute_Customization_String
           (Kernel,
            No_File,
            Kernel.Customization_Strings,
            Hard_Coded);

         --  Can't call Free itself, since it doesn't free the siblings
         while Kernel.Customization_Strings /= null loop
            N := Kernel.Customization_Strings;
            Kernel.Customization_Strings := Kernel.Customization_Strings.Next;
            Free (N);
         end loop;
      end if;

      Parse_Custom_Dir
        (Kernel, Autoload_User_Dir (Kernel), User_Specific,
         Default_Autoload => True, Force_Load => False);
   end Load_User_Custom_Files;

   -----------------------------------
   -- Load_No_Autoload_Custom_Files --
   -----------------------------------

   procedure Load_No_Autoload_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Parse_Custom_Dir
        (Kernel, No_Autoload_System_Dir (Kernel), System_Wide,
         Default_Autoload => False, Force_Load => False);
   end Load_No_Autoload_Custom_Files;

   ------------------------------
   -- Add_Customization_String --
   ------------------------------

   function Add_Customization_String
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Customization : XML_Utils.UTF8_String;
      From_File     : Filesystem_String;
      Start_Line    : Positive := 1) return String
   is
      --  Add a valid prefix and toplevel node, since the string won't
      --  contain any.

      Node : Node_Ptr;
      N    : Node_Ptr;
      Err  : GNAT.Strings.String_Access;

   begin
      --  Don't do this at declaration time, since we want to catch exceptions

      --  If the string appears to be a complete file, accept it as is
      if Customization'Length > 5
        and then Customization (Customization'First .. Customization'First + 4)
          = "<?xml"
      then
         XML_Parsers.Parse_Buffer
           (Buffer     => Customization,
            From_File  => From_File,
            Start_Line => Start_Line,
            Tree       => Node,
            Error      => Err);

      else
         --  else enclose it
         XML_Parsers.Parse_Buffer
           ("<?xml version=""1.0""?><GPS>" & Customization & "</GPS>",
            From_File  => From_File,
            Start_Line => Start_Line,
            Tree       => Node,
            Error      => Err);
      end if;

      --  If the custom files have already been loaded, this means that all
      --  modules have been registered and are ready to listen to events. We
      --  can then inform them all. Otherwise, we need to append them to the
      --  list which will be executed later when all modules have been
      --  registered.

      if Node /= null then
         Free (Err);

         if Kernel.Custom_Files_Loaded = User_Level then
            begin
               Execute_Customization_String
                 (Kernel, Create (From_File), Node.Child, Hard_Coded);
               Free (Node);
            exception
               when E : others =>
                  Trace (Me, E);
                  return "Error while executing parse_xml()";
            end;

         else
            N := Kernel.Customization_Strings;
            if N = null then
               Kernel.Customization_Strings := Node.Child;

            else
               while N.Next /= null loop
                  N := N.Next;
               end loop;

               N.Next := Node.Child;
            end if;

            N := Node.Child;
            while N /= null loop
               N.Parent := null;
               N := N.Next;
            end loop;

            Node.Child := null;
            Free (Node);
         end if;
         return "";

      else
         declare
            E : constant String := Err.all;
         begin
            Free (Err);
            return E;
         end;
      end if;

   exception
      when E : others =>
         --  This is purely internal error for programmers, no need for console

         Trace (Me, "Could not parse custom string " & Customization
                & ' ' & Exception_Message (E));
         Free (Err);
         Free (Node);
         return "Internal error";
   end Add_Customization_String;

   --------------------------------
   -- Parse_Startup_Scripts_List --
   --------------------------------

   procedure Parse_Startup_Scripts_List
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Startup : constant Virtual_File :=
                  Create_From_Dir (Get_Home_Dir (Kernel), "startup.xml");
      Err     : GNAT.Strings.String_Access;
      Node, N : Node_Ptr;
      Script  : Script_Description_Access;
      Mode    : Load_Mode;
   begin
      Kernel.Startup_Scripts := new Scripts_Htable_Record;

      if Is_Regular_File (Startup) then
         XML_Parsers.Parse
           (File  => Startup,
            Tree  => Node,
            Error => Err);

         if Node = null then
            Trace (Me, "Error while loading startup.xml: " & Err.all);
            Kernel.Insert
              ("Could not parse startup.xml: " & Err.all,
               Mode => Error);
            Free (Err);

         else
            N := Node.Child;
            while N /= null loop
               if N.Tag.all = "startup" then
                  begin
                     if Boolean'Value (Get_Attribute (N, "load")) then
                        Mode := Explicit_On;
                     else
                        Mode := Explicit_Off;
                     end if;
                  exception
                     when Constraint_Error =>
                        Mode := Explicit_On;
                  end;

                  Script := new Script_Description'
                    (Mode           => Mode,
                     Loaded         => False,
                     File           => GNATCOLL.VFS.No_File);

                  Set
                    (Scripts_Htable_Access
                       (Kernel.Startup_Scripts).Table,
                     K => Get_Attribute (N, "file"),
                     E => Script);
               end if;
               N := N.Next;
            end loop;
            Free (Node);
         end if;
      else
         Trace (Me, "File not found: " & Startup.Display_Full_Name);
      end if;
   end Parse_Startup_Scripts_List;

   -------------------------------
   -- Save_Startup_Scripts_List --
   -------------------------------

   procedure Save_Startup_Scripts_List
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Startup : constant Virtual_File :=
                  Create_From_Dir (Get_Home_Dir (Kernel), "startup.xml");
      File, Child : Node_Ptr;
      Iter        : Scripts_Hash.String_Hash_Table.Cursor;
      Script      : Script_Description_Access;
      Success     : Boolean;

   begin
      File     := new Node;
      File.Tag := new String'("GPS");

      Get_First (Scripts_Htable_Access (Kernel.Startup_Scripts).Table, Iter);
      loop
         Script := Get_Element (Iter);
         exit when Script = null;

         case Script.Mode is
            when Automatic =>
               null;

            when Explicit_On | Explicit_Off =>
               Child := new Node;
               Child.Tag := new String'("startup");

               Set_Attribute
                 (Child, "load", Boolean'Image (Script.Mode = Explicit_On));
               Set_Attribute (Child, "file", Get_Key (Iter));

               Add_Child (File, Child);
         end case;

         Get_Next (Scripts_Htable_Access (Kernel.Startup_Scripts).Table, Iter);
      end loop;

      Trace (Me, "Saving " & Startup.Display_Full_Name);
      Print (File, Startup, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Startup);
      end if;
   end Save_Startup_Scripts_List;

   -----------------------------
   -- For_All_Startup_Scripts --
   -----------------------------

   procedure For_All_Startup_Scripts
     (Kernel : access Kernel_Handle_Record'Class;
      Callback : not null access procedure
        (Name     : String;
         File     : GNATCOLL.VFS.Virtual_File;
         Loaded   : Boolean;
         Explicit : Boolean))
   is
      Iter : Scripts_Hash.String_Hash_Table.Cursor;
      S    : Script_Description_Access;
   begin
      Get_First (Scripts_Htable_Access (Kernel.Startup_Scripts).Table, Iter);

      loop
         S := Get_Element (Iter);
         exit when S = null;

         --  If we have a source file

         if S.File /= GNATCOLL.VFS.No_File then
            Callback
              (Name     => Get_Key (Iter),
               File     => S.File,
               Loaded   => S.Loaded,
               Explicit => S.Mode /= Automatic);
         end if;

         Get_Next (Scripts_Htable_Access (Kernel.Startup_Scripts).Table, Iter);
      end loop;
   end For_All_Startup_Scripts;

   -----------------------------
   -- Override_Startup_Script --
   -----------------------------

   procedure Override_Startup_Script
     (Kernel         : access Kernel_Handle_Record'Class;
      Base_Name      : String;
      Load           : Boolean)
   is
      Startup : Script_Description_Access :=
                  Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                       K => Base_Name);
      Mode : constant Load_Mode :=
        (if Load then Explicit_On else Explicit_Off);
   begin
      if Startup = null then
         Startup := new Script_Description'
           (Mode           => Mode,
            Loaded         => False,
            File           => GNATCOLL.VFS.No_File);
         Set (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
              K => Base_Name,
              E => Startup);
      else
         Startup.Mode := Mode;
      end if;
   end Override_Startup_Script;

   --------------------------
   -- Load_File_At_Startup --
   --------------------------

   function Load_File_At_Startup
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : GNATCOLL.VFS.Virtual_File;
      Default        : Boolean) return Boolean
   is
      Startup : Script_Description_Access;
   begin
      --  The base name would be "" for python module (ie subdirectories).

      if File.Base_Name = "" then
         Startup := Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                         K => +File.Base_Dir_Name);
      else
         Startup := Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                         K => +File.Base_Name);
      end if;

      if Startup = null then
         Startup := new Script_Description'
           (Mode             => Automatic,
            Loaded           => Default,
            File             => File);

         if File.Base_Name = "" then
            Set (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                 K => +File.Base_Dir_Name,
                 E => Startup);
         else
            Set (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                 K => +File.Base_Name,
                 E => Startup);
         end if;

      elsif Startup.File /= File
        and then Startup.File /= GNATCOLL.VFS.No_File
      then
         Insert (Kernel,
                 -"There are several startup scripts with the same name: "
                 & Startup.File.Display_Full_Name
                 & ASCII.LF
                 & (-"Not loading: ") & File.Display_Full_Name,
                 Mode => Error);
         return False;

      else
         Startup.File   := File;
         Startup.Loaded :=
           Startup.Mode = Explicit_On
           or else (Startup.Mode /= Explicit_Off and then Default);
      end if;

      return Startup.Loaded;
   end Load_File_At_Startup;

   -------------------------------
   -- Get_Script_From_Base_Name --
   -------------------------------

   function Get_Script_From_Base_Name
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Base_Name : String) return Script_Description_Access
   is
      Script : constant Script_Description_Access :=
                 Get (Scripts_Htable_Access (Kernel.Startup_Scripts).Table,
                      K => Base_Name);
   begin
      return Script;
   end Get_Script_From_Base_Name;

   ----------
   -- Free --
   ----------

   procedure Free (File : in out Script_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Script_Description, Script_Description_Access);
   begin
      Unchecked_Free (File);
   end Free;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Table : access Scripts_Htable_Record) is
   begin
      Reset (Table.Table);
   end Reset;

end GPS.Kernel.Custom;
