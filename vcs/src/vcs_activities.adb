-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
--                              AdaCore                              --
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

with Ada.Calendar;          use Ada.Calendar;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;           use GNAT;
with GNAT.Dynamic_Tables;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;

with GPS.Kernel.Project;    use GPS.Kernel.Project;
with Projects;              use Projects;
with Projects.Registry;     use Projects.Registry;
with Traces;                use Traces;
with Glib.Xml_Int;          use Glib.Xml_Int;
with XML_Parsers;

package body VCS_Activities is

   Me : constant Debug_Handle := Create ("VCS_Activities");

   Activities_Filename : constant String := "activities.xml";

   type Activity_Record is record
      Project      : String_Access;
      Name         : String_Access;
      Id           : String_Access;
      VCS          : VCS_Access;
      Group_Commit : Boolean := False;
      Files        : String_List.List;
   end record;

   package Activity_Table is new GNAT.Dynamic_Tables
     (Activity_Record, Natural, 1, 5, 20);
   use Activity_Table;

   Set : Activity_Table.Instance;

   -----------
   -- Image --
   -----------

   function Image (Activity : Activity_Id) return String is
      Id : constant String := Activity_Id'Image (Activity);
   begin
      return Id (Id'First + 1 .. Id'Last);
   end Image;

   -----------
   -- Value --
   -----------

   function Value  (Str : String) return Activity_Id is
   begin
      return Activity_Id'Value (Str);
   end Value;

   ---------------------
   -- Load_Activities --
   ---------------------

   procedure Load_Activities (Kernel : access Kernel_Handle_Record'Class) is

      Filename : constant String :=
                   Get_Home_Dir (Kernel) & Activities_Filename;

      procedure Parse_Activity (Node : Node_Ptr);
      --  Parse an activity node

      --------------------
      -- Parse_Activity --
      --------------------

      procedure Parse_Activity (Node : Node_Ptr) is
         Id           : constant String := Get_Attribute (Node, "id");
         Project      : constant String := Get_Attribute (Node, "project");
         Name         : constant String := Get_Attribute (Node, "name");
         Group_Commit : constant Boolean :=
                          Boolean'Value
                            (Get_Attribute (Node, "group_commit", "false"));
         Child        : Node_Ptr := Node.Child;
         Item         : Activity_Record;
      begin
         Item := (new String'(Project), new String'(Name), new String'(Id),
                  null, Group_Commit, String_List.Null_List);

         while Child /= null loop
            if Child.Tag.all = "file" then
               String_List.Append (Item.Files, Child.Value.all);
               --  Note that here we can't use Add_File. At this point the
               --  project is not yet loaded and we can't compute the VCS for
               --  each activities.
            end if;
            Child := Child.Next;
         end loop;

         Append (Set, Item);
      end Parse_Activity;

      File, Child : Node_Ptr;
      Err         : OS_Lib.String_Access;

   begin
      if OS_Lib.Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);

         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Trace (Me, Err.all);
            OS_Lib.Free (Err);

         else
            --  Get node custom_section

            Child := File.Child;

            --  Get node activities

            Child := Child.Child;

            while Child /= null loop
               if Child.Tag.all = "activity" then
                  Parse_Activity (Child);
               else
                  Trace (Exception_Handle,
                         "Unknown activity node " & Child.Tag.all);
               end if;
               Child := Child.Next;
            end loop;

            Free (File);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Load_Activities;

   ---------------------
   -- Save_Activities --
   ---------------------

   procedure Save_Activities (Kernel : access Kernel_Handle_Record'Class) is

      Filename : constant String :=
                   Get_Home_Dir (Kernel) & Activities_Filename;

      File, Ada_Child : Node_Ptr;
      Child, F_Child  : Node_Ptr;
      F_Iter          : String_List.List_Node;
   begin
      File     := new Node;
      File.Tag := new String'("custom_section");

      Ada_Child     := new Node;
      Ada_Child.Tag := new String'("activities");
      Add_Child (File, Ada_Child);

      for K in 1 .. Last (Set) loop
         declare
            Item : constant Activity_Record := Set.Table (K);
         begin
            Child     := new Node;
            Child.Tag := new String'("activity");

            Set_Attribute (Child, "name", Item.Name.all);
            Set_Attribute (Child, "project", Item.Project.all);
            Set_Attribute (Child, "id", Item.Id.all);

            Add_Child (Ada_Child, Child);

            if not String_List.Is_Empty (Item.Files) then
               --  Append all files to this child

               F_Iter := String_List.First (Item.Files);

               for K in 1 .. String_List.Length (Item.Files) loop
                  F_Child       := new Node;
                  F_Child.Tag   := new String'("file");
                  F_Child.Value := new String'(String_List.Data (F_Iter));

                  Add_Child (Child, F_Child);

                  F_Iter := String_List.Next (F_Iter);
               end loop;
            end if;
         end;
      end loop;

      Trace (Me, "Saving " & Filename);
      Print (File, Filename);
      Free (File);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Save_Activities;

   ------------------
   -- New_Activity --
   ------------------

   function New_Activity
     (Kernel : access Kernel_Handle_Record'Class) return Activity_Id is
   begin
      New_Id : loop
         declare
            UID   : constant String :=
                      Image (Clock, Picture_String'("%Y%m%d%H%M%S%i"));
            Found : Boolean := False;
         begin
            for K in 1 .. Last (Set) loop
               if Set.Table (K).Id.all = UID then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               --  Retreive the current root project name
               Append
                 (Set,
                  (new String'(Project_Path
                                 (Get_Root_Project
                                    (Get_Registry (Kernel).all))),
                   new String'("New Activity"),
                   new String'(UID),
                   null,
                   False,
                   String_List.Null_List));

               Save_Activities (Kernel);

               return Activity_Id (Last (Set));
            end if;
         end;
      end loop New_Id;
   end New_Activity;

   --------------------------
   -- Get_VCS_For_Activity --
   --------------------------

   function Get_VCS_For_Activity
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id) return VCS_Access
   is
      VCS   : VCS_Access := Set.Table (Natural (Activity)).VCS;
      Files : String_List.List;
   begin
      if VCS = null then
         --  It is possible that the VCS is not yet known. This happen just
         --  after loading the activities XML registry. Compute it now, we know
         --  that all current files are using the same VCS otherwise they won't
         --  have been append.

         Files := Set.Table (Natural (Activity)).Files;

         if not String_List.Is_Empty (Files)then
            declare
               File    : constant Virtual_File :=
                           Create (String_List.Head (Files));
               Project : constant Project_Type :=
                           Get_Project_From_File
                             (Get_Registry (Kernel).all, File);

            begin
               VCS := Get_VCS_From_Id
                 (Get_Attribute_Value (Project, Vcs_Kind_Attribute));
               Set.Table (Natural (Activity)).VCS := VCS;
            end;
         end if;
      end if;

      return VCS;
   end Get_VCS_For_Activity;

   ----------------------
   -- Get_Project_Path --
   ----------------------

   function Get_Project_Path (Activity : Activity_Id) return String is
   begin
      return Set.Table (Natural (Activity)).Project.all;
   end Get_Project_Path;

   ---------------------
   -- Delete_Activity --
   ---------------------

   procedure Delete_Activity
     (Kernel : access Kernel_Handle_Record'Class; Activity : Activity_Id)
   is
      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      File_Name : constant String :=
                    Logs_Dir & OS_Lib.Directory_Separator &
                    Set.Table (Natural (Activity)).Id.all & "$log";
      K         : constant Natural := Natural (Activity);
      Success   : Boolean;
   begin
      declare
         Item : Activity_Record := Set.Table (K);
      begin
         String_List.Free (Item.Files);
         Free (Item.Name);
         Free (Item.Id);
      end;

      for I in K .. Last (Set) - 1 loop
         Set.Table (K) := Set.Table (K + 1);
      end loop;

      Set_Last (Set, Last (Set) - 1);

      Save_Activities (Kernel);

      OS_Lib.Delete_File (File_Name, Success);
   end Delete_Activity;

   ----------------------------
   -- Get_Activity_From_Name --
   ----------------------------

   function Get_Activity_From_Name (Name : String) return Activity_Id is
   begin
      for K in 1 .. Last (Set) loop
         if Set.Table (K).Name.all = Name
           or else Set.Table (K).Id.all = Name
         then
            return Activity_Id (K);
         end if;
      end loop;
      return No_Activity;
   end Get_Activity_From_Name;

   -------------
   -- Has_Log --
   -------------

   function Has_Log
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id) return Boolean
   is
      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      File_Name : constant String :=
                    Logs_Dir & OS_Lib.Directory_Separator &
                    Set.Table (Natural (Activity)).Id.all & "$log";
      Log_File : constant Virtual_File :=
                    Create (Full_Filename => File_Name);
   begin
      return Is_Regular_File (Log_File);
   end Has_Log;

   ------------------
   -- Get_Log_File --
   ------------------

   function Get_Log_File
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id) return Virtual_File
   is
      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      File_Name : constant String :=
                    Logs_Dir & OS_Lib.Directory_Separator &
                    Set.Table (Natural (Activity)).Id.all & "$log";
      File      : constant Virtual_File := Create (File_Name);
      F         : OS_Lib.File_Descriptor;
   begin
      if not Is_Regular_File (File) then
         F := OS_Lib.Create_New_File (File_Name, OS_Lib.Text);
         OS_Lib.Close (F);
         return File;

      else
         return File;
      end if;
   end Get_Log_File;

   -------------
   -- Get_Log --
   -------------

   function Get_Log
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id) return String
   is
      use type OS_Lib.String_Access;
      File : constant Virtual_File := Get_Log_File (Kernel, Activity);
      R    : OS_Lib.String_Access;
   begin
      R := Read_File (File);

      if R = null then
         return "";

      else
         declare
            S : constant String := R.all;
         begin
            OS_Lib.Free (R);
            return S;
         end;
      end if;
   end Get_Log;

   -----------
   -- First --
   -----------

   function First return Activity_Id is
   begin
      if Last (Set) = 0 then
         return No_Activity;
      else
         return 1;
      end if;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Activity : Activity_Id) return Activity_Id is
   begin
      if Natural (Activity) = Last (Set) then
         return No_Activity;
      else
         return Activity + 1;
      end if;
   end Next;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Activity : Activity_Id) return String is
   begin
      if Activity = No_Activity then
         return "";
      else
         return Set.Table (Natural (Activity)).Name.all;
      end if;
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Activity : Activity_Id; Name : String) is
   begin
      Free (Set.Table (Natural (Activity)).Name);
      Set.Table (Natural (Activity)).Name := new String'(Name);
   end Set_Name;

   -----------------------
   -- Get_File_Activity --
   -----------------------

   function Get_File_Activity (File : VFS.Virtual_File) return Activity_Id is
   begin
      for K in 1 .. Last (Set) loop
         declare
            Item : constant Activity_Record := Set.Table (K);
         begin
            if Is_In_List (Item.Files, Full_Name (File, True).all) then
               return Activity_Id (K);
            end if;
         end;
      end loop;

      return No_Activity;
   end Get_File_Activity;

   ---------------------------
   -- Get_Files_In_Activity --
   ---------------------------

   function Get_Files_In_Activity
     (Activity : Activity_Id) return String_List.List is
   begin
      return Set.Table (Natural (Activity)).Files;
   end Get_Files_In_Activity;

   --------------
   -- Add_File --
   --------------

   procedure Add_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      File     : Virtual_File)
   is
      Project : constant Project_Type :=
                  Get_Project_From_File (Get_Registry (Kernel).all, File);
      VCS     : constant VCS_Access :=
                  Get_VCS_From_Id
                    (Get_Attribute_Value (Project, Vcs_Kind_Attribute));
      Item    : constant Activity_Record := Set.Table (Natural (Activity));
   begin
      --  ??? check that File is not yet present

      if (Item.VCS /= null and then VCS /= Item.VCS)
        or else Get_File_Activity (File) /= No_Activity
      then
         --  ??? dialog saying that it is not possible (2 diff VCS)
         --  ??? or file already part of an activity.
         return;
      end if;

      Set.Table (Natural (Activity)).VCS := VCS;

      String_List.Append
        (Set.Table (Natural (Activity)).Files, Full_Name (File, True).all);

      Save_Activities (Kernel);
   end Add_File;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
      File     : Virtual_File) is
   begin
      Remove_From_List
        (Set.Table (Natural (Activity)).Files, Full_Name (File, True).all);
      Save_Activities (Kernel);
   end Remove_File;

   ----------------------
   -- Get_Group_Commit --
   ----------------------

   function Get_Group_Commit (Activity : Activity_Id) return Boolean is
   begin
      return Set.Table (Natural (Activity)).Group_Commit;
   end Get_Group_Commit;

   -------------------------
   -- Toggle_Group_Commit --
   -------------------------

   procedure Toggle_Group_Commit (Activity : Activity_Id) is
   begin
      Set.Table (Natural (Activity)).Group_Commit :=
        not Set.Table (Natural (Activity)).Group_Commit;
   end Toggle_Group_Commit;

begin
   Init (Set);
end VCS_Activities;
