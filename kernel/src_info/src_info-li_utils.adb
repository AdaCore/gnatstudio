with Text_IO; use Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
package body Src_Info.LI_Utils is

   procedure Insert_Declaration_Internal
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : in LI_File_Ptr;
      List                    : in LI_File_List;
      Symbol_Name             : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point);
   --  Inserts declaration into specified E_Declaration_Info_List


   function Find_Declaration_Internal
     (Declaration_Info_Ptr    : in E_Declaration_Info_List;
      Symbol_Name             : in String := "";
      Class_Name              : in String := "";
      Kind                    : in E_Kind := No_Kind;
      Location                : in Point := Invalid_Point)
   return E_Declaration_Info_List;
   --  Finds declaration with given attributes in
   --  specified E_Declaration_Info_List

   procedure Create_LI_File
     (File                    : out LI_File_Ptr;
      Handler                 : in LI_Handler;
      Source_Filename         : in String;
      Parsed                  : in Boolean);
   --  Creates an empty LI_File structure

   procedure Create_File_Info
     (FI_Ptr   : in out File_Info_Ptr;
      Source_Filename : in String;
      Directory_Name  : in String);
   --  Creates an empty File_Info (without declarations)


   function Belongs_To_Class
     (Declaration_Info_Ptr           : in E_Declaration_Info_List;
      Class_Name      : in String;
      Position        : in Point) return Boolean;
   --  Checks if given position belongs to class body (found in the given
   --  list of declarations)

   -------------------------------------------------------------------------

   --------------------------
   --  Insert_declaration  --
   --------------------------

   procedure Insert_Declaration
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
      List                    : in out LI_File_List;
      Symbol_Name             : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point;
      Declaration_Info        : out E_Declaration_Info_List)
   is
      D_Ptr : E_Declaration_Info_List;
   begin
      if File = No_LI_File then
         Create_LI_File
           (Handler           => Handler,
            File              => File,
            Source_Filename   => Source_Filename,
            Parsed            => True);
      else
         pragma Assert (File.LI.LI_Filename.all = Source_Filename,
                     "Invalid Source Filename");
         pragma Assert (LI_Handler (Handler) = File.LI.Handler,
                     "Invalid Handler");
         null;
      end if;
      if File.LI.Body_Info = null then
         File.LI.Body_Info := new File_Info'
               (Unit_Name => null,
                Source_Filename => new String'(Source_Filename),
                Directory_Name => null,
         --  ??? we should extract Directory_Name from Source_Filename
                File_Timestamp => To_Timestamp
                                    (File_Time_Stamp (Source_Filename)),
         --  ??? File_Time_Stamp returns rubbish if filename is incorrect
                Original_Filename => null,
                Original_Line => 1,
                Declarations => null);
      end if;
      if File.LI.Body_Info.Declarations = null then
         File.LI.Body_Info.Declarations := new E_Declaration_Info_Node;
         File.LI.Body_Info.Declarations.Next := null;
         D_Ptr := File.LI.Body_Info.Declarations;
      else
         D_Ptr := File.LI.Body_Info.Declarations;
         loop
            if D_Ptr.Value.Declaration.Location.Line = Location.Line
              and then D_Ptr.Value.Declaration.Location.Column =
                                                      Location.Column
            then
               D_Ptr.Value.Declaration := No_Declaration;
               exit;
            end if;
            if D_Ptr.Next = null then
               D_Ptr.Next := new E_Declaration_Info_Node;
               D_Ptr.Next.Next := null;
               D_Ptr := D_Ptr.Next;
               exit;
            end if;
            D_Ptr := D_Ptr.Next;
         end loop;
      end if;
      Insert_Declaration_Internal (D_Ptr, File, List, Symbol_Name,
            Source_Filename,
            Location, Parent_Filename, Parent_Location, Kind, Scope,
            End_Of_Scope_Location, Rename_Filename, Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Declaration;

   -------------------------
   --  Insert_Dependency  --
   -------------------------

   procedure Insert_Dependency
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
      List                    : in out LI_File_List;
      Source_Filename         : in String;
      Referred_Filename       : in String)
   is
      Dep_Ptr : Dependency_File_Info_List;
      Tmp_LI_File : LI_File;
      Tmp_LI_File_Ptr : LI_File_Ptr;
      Success : Boolean;
   begin
      --  checking existance of given LI_File and create new one if necessary
      if File = No_LI_File then
         Create_LI_File
           (File              => File,
            Handler           => Handler,
            Source_Filename   => Source_Filename,
            Parsed            => True);
      else
         pragma Assert (LI_Handler (Handler) = File.LI.Handler,
                     "Invalid Handler");
         if File.LI.Parsed = False then
            Tmp_LI_File := File.LI;
            File.LI := (Parsed    => True,
                       Handler   => Tmp_LI_File.Handler,
                       LI_Filename  => Tmp_LI_File.LI_Filename,
                       Spec_Info    => Tmp_LI_File.Spec_Info,
                       Body_Info    => Tmp_LI_File.Body_Info,
                       Separate_Info     => Tmp_LI_File.Separate_Info,
                       LI_Timestamp      =>
                         To_Timestamp (File_Time_Stamp (Source_Filename)),
            --  ??? if file is not found?
                       Compilation_Errors_Found => False,
                       Dependencies_Info => null);
         end if;
      end if;
      --  Now we are searching through common list of LI_Files and
      --  trying to locate file with given name. If not found we are
      --  inserting new dependency
      Tmp_LI_File_Ptr := Get (List.Table, Referred_Filename);
      if Tmp_LI_File_Ptr = No_LI_File then
         Create_LI_File
           (File              => Tmp_LI_File_Ptr,
            Handler           => Handler,
            Source_Filename   => Referred_Filename,
            Parsed            => True);
         Create_File_Info
           (FI_Ptr            => Tmp_LI_File_Ptr.LI.Body_Info,
            Source_Filename   => Referred_Filename,
            Directory_Name    => "");
         Add (List.Table, Tmp_LI_File_Ptr, Success);
         pragma Assert (Success,
               "unable to insert new LI_File into the common LI_File_List");
         --  Tmp_LI_File_Ptr := No_LI_File;
      end if;
      --  Is this is a first dependencies info in this file?
      if File.LI.Dependencies_Info = null then
         --  creating new Dependency_File_Info_Node object
         File.LI.Dependencies_Info := new Dependency_File_Info_Node;
         File.LI.Dependencies_Info.Value.File.LI := Tmp_LI_File_Ptr;
         File.LI.Dependencies_Info.Value.File.Part := Unit_Body;
         File.LI.Dependencies_Info.Value.File.Source_Filename
                                             := new String'(Referred_Filename);
         File.LI.Dependencies_Info.Value.Dep_Info :=
                                          (Depends_From_Spec => False,
                                           Depends_From_Body => True);
         File.LI.Dependencies_Info.Value.Declarations := null;
         File.LI.Dependencies_Info.Next := null;
         Dep_Ptr := File.LI.Dependencies_Info;
      else
         Dep_Ptr := File.LI.Dependencies_Info;
         --  trying to locate Dependency_File_Info with given Source_Filename
         loop
            exit when
               Dep_Ptr.Value.File.Source_Filename.all = Referred_Filename;
            if Dep_Ptr.Next = null then
               --  Unable to find suitable Dependency_File_Info.
               --  Creating a new one.
               Dep_Ptr.Next := new Dependency_File_Info_Node;
               Dep_Ptr.Next.Next := null;
               Dep_Ptr := Dep_Ptr.Next;
               Dep_Ptr.Value :=
                 (File => (LI   => Tmp_LI_File_Ptr,
                           Part => Unit_Body,
                           Source_Filename =>
                              new String'(Referred_Filename)),
                  Dep_Info => (Depends_From_Spec => False,
                                      Depends_From_Body => True),
                  Declarations => null);
               exit;
            end if;
            Dep_Ptr := Dep_Ptr.Next;
         end loop;
      end if;
      --  Dep_Ptr now points to the right Dependency_File_Info
      --  Inserting null stub declaration
      --  if Dep_Ptr.Value.Declarations = null then
      --     Dep_Ptr.Value.Declarations := new E_Declaration_Info_Node'
      --       (Value => (Declaration => No_Declaration,
      --                  References => new E_Reference_Node'
      --                    (Value => No_Reference,
      --                     Next  => null)),
      --        Next  => null);
      --  end if;
   end Insert_Dependency;


   -------------------------------------
   --  Insert_Dependency_Declaration  --
   -------------------------------------

   procedure Insert_Dependency_Declaration
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
      List                    : in out LI_File_List;
      Symbol_Name             : in String;
      Referred_Filename       : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point;
      Declaration_Info        : out E_Declaration_Info_List)
   is
      D_Ptr, Tmp_Ptr : E_Declaration_Info_List;
      Dep_Ptr : Dependency_File_Info_List;
      Tmp_LI_File : LI_File;
      Tmp_LI_File_Ptr : LI_File_Ptr;
      Success : Boolean;
   begin
      --  checking existance of given LI_File and create new one if necessary
      if File = No_LI_File then
         Create_LI_File
           (File => File,
            Handler => Handler,
            Source_Filename => Source_Filename,
            Parsed => True);
      else
         pragma Assert (LI_Handler (Handler) = File.LI.Handler,
                     "Invalid Handler");
         if File.LI.Parsed = False then
            Tmp_LI_File := File.LI;
            File.LI := (Parsed    => True,
                       Handler   => Tmp_LI_File.Handler,
                       LI_Filename  => Tmp_LI_File.LI_Filename,
                       Spec_Info    => Tmp_LI_File.Spec_Info,
                       Body_Info    => Tmp_LI_File.Body_Info,
                       Separate_Info     => Tmp_LI_File.Separate_Info,
                       LI_Timestamp      => To_Timestamp
                         (File_Time_Stamp (Source_Filename)),
            --  ??? if file is not found?
                       Compilation_Errors_Found => False,
                       Dependencies_Info => null);
         end if;
      end if;
      --  Now we are searching through common list of LI_Files and
      --  trying to locate file with given name. If not found or if there
      --  are no such symbol declared in the found file then
      --  we are inserting new declaration
      Tmp_LI_File_Ptr := Get (List.Table, Referred_Filename);
      if Tmp_LI_File_Ptr = No_LI_File then
         Insert_Declaration
           (Handler            => Handler,
            File               => Tmp_LI_File_Ptr,
            List               => List,
            Symbol_Name        => Symbol_Name,
            Source_Filename    => Referred_Filename,
            Location           => Location,
            Parent_Filename    => Parent_Filename,
            Parent_Location    => Parent_Location,
            Kind               => Kind,
            Scope              => Scope,
            End_Of_Scope_Location => End_Of_Scope_Location,
            Rename_Filename    => Rename_Filename,
            Declaration_Info   => Tmp_Ptr);
         Add (List.Table, Tmp_LI_File_Ptr, Success);
         pragma Assert (Success,
               "unable to insert new LI_File into the common LI_File_List");
      else
         begin
            D_Ptr := Find_Declaration
                       (File              => Tmp_LI_File_Ptr,
                        Symbol_Name       => Symbol_Name,
                        Location          => Location);
         exception
            when Declaration_Not_Found =>
               Insert_Declaration
                 (Handler            => Handler,
                  File               => Tmp_LI_File_Ptr,
                  List               => List,
                  Symbol_Name        => Symbol_Name,
                  Source_Filename    => Referred_Filename,
                  Location           => Location,
                  Parent_Filename    => Parent_Filename,
                  Parent_Location    => Parent_Location,
                  Kind               => Kind,
                  Scope              => Scope,
                  End_Of_Scope_Location => End_Of_Scope_Location,
                  Rename_Filename    => Rename_Filename,
                  Declaration_Info   => Tmp_Ptr);
         end;
      end if;
      --  Is this is a first dependencies info in this file?
      if File.LI.Dependencies_Info = null then
         --  creating new Dependency_File_Info_Node object
         File.LI.Dependencies_Info := new Dependency_File_Info_Node;
         File.LI.Dependencies_Info.Value.File.LI := Tmp_LI_File_Ptr;
         File.LI.Dependencies_Info.Value.File.Part := Unit_Body;
         File.LI.Dependencies_Info.Value.File.Source_Filename
                                             := new String'(Referred_Filename);
         File.LI.Dependencies_Info.Value.Dep_Info :=
                                          (Depends_From_Spec => False,
                                           Depends_From_Body => True);
         File.LI.Dependencies_Info.Value.Declarations := null;
         File.LI.Dependencies_Info.Next := null;
         Dep_Ptr := File.LI.Dependencies_Info;
      else
         Dep_Ptr := File.LI.Dependencies_Info;
         --  trying to locate Dependency_File_Info with given Source_Filename
         loop
            exit when
               Dep_Ptr.Value.File.Source_Filename.all = Referred_Filename;
            if Dep_Ptr.Next = null then
               --  Unable to find suitable Dependency_File_Info.
               --  Creating a new one.
               Dep_Ptr.Next := new Dependency_File_Info_Node;
               Dep_Ptr.Next.Next := null;
               Dep_Ptr := Dep_Ptr.Next;
               Dep_Ptr.Value.File.LI   := Tmp_LI_File_Ptr;
               Dep_Ptr.Value.File.Part := Unit_Body;
               Dep_Ptr.Value.File.Source_Filename
                                       := new String'(Referred_Filename);
               exit;
            end if;
            Dep_Ptr := Dep_Ptr.Next;
         end loop;
      end if;
      --  Now Dep_Ptr points to valid Dependency_File_Info_Node object
      --  Inserting new declaration
      if Dep_Ptr.Value.Declarations = null then
         --  this is a first declaration for this Dependency_File_Info
         Dep_Ptr.Value.Declarations := new E_Declaration_Info_Node;
         Dep_Ptr.Value.Declarations.Next := null;
         D_Ptr := Dep_Ptr.Value.Declarations;
      else
         --  Inserting to the end of the declaration's list
         D_Ptr := Dep_Ptr.Value.Declarations;
         loop
            if D_Ptr.Value.Declaration.Location.Line = Location.Line
              and then D_Ptr.Value.Declaration.Location.Line = Location.Line
            then
               D_Ptr.Value.Declaration := No_Declaration;
               exit;
            end if;
            if D_Ptr.Next = null then
               D_Ptr.Next := new E_Declaration_Info_Node;
               D_Ptr.Next.Next := null;
               D_Ptr := D_Ptr.Next;
               exit;
            end if;
            D_Ptr := D_Ptr.Next;
         end loop;
      end if;
      Insert_Declaration_Internal (D_Ptr, File, List, Symbol_Name,
            Referred_Filename,
            Location, Parent_Filename, Parent_Location, Kind, Scope,
            End_Of_Scope_Location, Rename_Filename, Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Dependency_Declaration;


   ------------------
   --  Add_Parent  --
   ------------------

   procedure Add_Parent
     (Declaration_Info        : in out E_Declaration_Info_List;
      List                    : in LI_File_List;
      Parent_Filename         : in String;
      Parent_Location         : in Point)
   is
      FL_Ptr : File_Location_List;
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      if Declaration_Info = null then
         raise Declaration_Not_Found;
      end if;
      if Declaration_Info.Value.Declaration.Parent_Location = null then
         Declaration_Info.Value.Declaration.Parent_Location
               := new File_Location_Node;
         FL_Ptr := Declaration_Info.Value.Declaration.Parent_Location;
      else
         FL_Ptr := Declaration_Info.Value.Declaration.Parent_Location;
         loop
            if FL_Ptr.Value.Line = Parent_Location.Line
              and then FL_Ptr.Value.Column = Parent_Location.Column
              and then FL_Ptr.Value.File.Source_Filename.all = Parent_Filename
            then
               return;
            end if;
            if FL_Ptr.Next = null then
               FL_Ptr.Next := new File_Location_Node;
               FL_Ptr := FL_Ptr.Next;
               exit;
            end if;
            FL_Ptr := FL_Ptr.Next;
         end loop;
      end if;
      Tmp_LI_File_Ptr := Get (List.Table, Parent_Filename);
      if Tmp_LI_File_Ptr = No_LI_File then
         if Parent_Filename = Declaration_Info.Value.Declaration
                                 .Location.File.Source_Filename.all
         then
            Tmp_LI_File_Ptr := Declaration_Info.Value.Declaration
                                 .Location.File.LI;
         else
            raise Parent_Not_Available;
         end if;
      end if;
      FL_Ptr.Value :=
        (File        => (LI => Tmp_LI_File_Ptr,
                         Part => Unit_Body,
                         Source_Filename =>
                               new String'(Parent_Filename)),
         Line        => Parent_Location.Line,
         Column      => Parent_Location.Column);
      FL_Ptr.Next := null;
   end Add_Parent;

   ------------------------
   --  Set_End_Of_Scope  --
   ------------------------

   procedure Set_End_Of_Scope
     (Declaration_Info        : in out E_Declaration_Info_List;
      Location                : in Point;
      Kind                    : in Reference_Kind := End_Of_Body) is
   begin
      Declaration_Info.Value.Declaration.End_Of_Scope.Location.Line
                                                         := Location.Line;
      Declaration_Info.Value.Declaration.End_Of_Scope.Location.Column
                                                         := Location.Column;
      Declaration_Info.Value.Declaration.End_Of_Scope.Location.File.LI
                      := Declaration_Info.Value.Declaration.Location.File.LI;
      Declaration_Info.Value.Declaration.End_Of_Scope.Location.File.Part
                      := Unit_Body;
      Declaration_Info.Value.Declaration
        .End_Of_Scope.Location.File.Source_Filename :=
          new String'
            (Declaration_Info.Value.Declaration
               .Location.File.Source_Filename.all);
      Declaration_Info.Value.Declaration.End_Of_Scope.Kind := Kind;
   end Set_End_Of_Scope;

   ------------------------
   --  Insert_Reference  --
   ------------------------

   procedure Insert_Reference
     (Declaration_Info        : in out E_Declaration_Info_List;
      File                    : in LI_File_Ptr;
      Source_Filename         : in String;
      Location                : in Point;
      Kind                    : Reference_Kind)
   is
      R_Ptr : E_Reference_List;
   begin
      if Declaration_Info = null then
         --  ??? what should we do if Declaration_Info is null?
         return;
      elsif Declaration_Info.Value.References = null then
         Declaration_Info.Value.References := new E_Reference_Node;
         Declaration_Info.Value.References.Next := null;
         R_Ptr := Declaration_Info.Value.References;
      else
         R_Ptr := Declaration_Info.Value.References;
         loop
            exit when R_Ptr.Next = null;
            R_Ptr := R_Ptr.Next;
         end loop;
         R_Ptr.Next := new E_Reference_Node;
         R_Ptr.Next.Next := null;
         R_Ptr := R_Ptr.Next;
         --  ??? Shouldn't we try to locate existent reference with
         --  given attributes
      end if;
      R_Ptr.Value :=
         (Location => (File => (LI => File,
                                Part => Unit_Body,
                                Source_Filename =>
                                              new String'(Source_Filename)),
                       Line => Location.Line,
                       Column => Location.Column),
          Kind => Kind);
      --  ??? We have R_Ptr.Value.Location.File.LI set to File because
      --  we always insert references only from the current file.
   end Insert_Reference;

   ------------------------
   --  Find_Declaration  --
   ------------------------

   function Find_Declaration
     (File                    : in LI_File_Ptr;
      Symbol_Name             : in String := "";
      Class_Name              : in String := "";
      Kind                    : in E_Kind := No_Kind;
      Location                : in Point := Invalid_Point)
   return E_Declaration_Info_List is
   begin
      if (File = No_LI_File)
            or else (File.LI.Body_Info = null)
            or else (File.LI.Body_Info.Declarations = null) then
         raise Declaration_Not_Found;
      end if;
      return Find_Declaration_Internal
               (Declaration_Info_Ptr => File.LI.Body_Info.Declarations,
                Symbol_Name          => Symbol_Name,
                Class_Name           => Class_Name,
                Kind                 => Kind,
                Location             => Location);
   end Find_Declaration;

   -----------------------------------
   --  Find_Dependency_Declaration  --
   -----------------------------------

   function Find_Dependency_Declaration
     (File                    : in LI_File_Ptr;
      Symbol_Name             : in String := "";
      Class_Name              : in String := "";
      Filename                : in String := "";
      Kind                    : in E_Kind := No_Kind;
      Location                : in Point := Invalid_Point)
   return E_Declaration_Info_List is
      Dep_Ptr : Dependency_File_Info_List;
   begin
      if (File = No_LI_File)
            or else (File.LI.Dependencies_Info = null) then
         raise Declaration_Not_Found;
      end if;
      Dep_Ptr := File.LI.Dependencies_Info;
      loop
         if Dep_Ptr = null then
            raise Declaration_Not_Found;
         end if;

         if Dep_Ptr.Value.File = No_Source_File then
            Put_Line ("No_Source_File encountered in the Dep_Info list for "
               & Get_LI_Filename (File));
         end if;

         exit when Dep_Ptr.Value.File.Source_Filename.all = Filename;
         Dep_Ptr := Dep_Ptr.Next;
      end loop;
      if Dep_Ptr.Value.Declarations = null then
         raise Declaration_Not_Found;
      end if;
      return Find_Declaration_Internal
               (Declaration_Info_Ptr => Dep_Ptr.Value.Declarations,
                Symbol_Name          => Symbol_Name,
                Class_Name           => Class_Name,
      --  ??? Should the dependency declaration of class
      --  necessary be located together with method dependency
      --  declaration?
                Kind                 => Kind,
                Location             => Location);
   end Find_Dependency_Declaration;
   --  ??? Class_Name parameter should be used properly

   -------------------------------------------------------------------------

   -----------------------------------
   --  Insert_Declaration_Internal  --
   -----------------------------------

   procedure Insert_Declaration_Internal
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : in LI_File_Ptr;
      List                    : in LI_File_List;
      Symbol_Name             : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point)
   is
      Tmp_LI_File_Ptr : LI_File_Ptr;
   begin
      if D_Ptr = null then
         return;
      end if;
      D_Ptr.Value.Declaration.Name := new String'(Symbol_Name);
      D_Ptr.Value.Declaration.Location :=
                   (File => (LI => File,
                             Part => Unit_Body,
                             Source_Filename => new String'(Source_Filename)),
                    Line => Location.Line,
                    Column => Location.Column);
      D_Ptr.Value.Declaration.Kind := Kind;
      if Parent_Location = Invalid_Point then
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node;
         D_Ptr.Value.Declaration.Parent_Location.Value := Null_File_Location;
         D_Ptr.Value.Declaration.Parent_Location.Next := null;
      elsif Parent_Location = Predefined_Point then
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node;
         D_Ptr.Value.Declaration.Parent_Location.Value :=
                                                Predefined_Entity_Location;
         D_Ptr.Value.Declaration.Parent_Location.Next := null;
      else
         --  Processing parent information
         if Source_Filename = Parent_Filename then
            Tmp_LI_File_Ptr := File;
         else
            Tmp_LI_File_Ptr := Get (List.Table, Parent_Filename);
            if (Tmp_LI_File_Ptr = No_LI_File) then
               --  ??? What should we do if LI_File for parent does not exist?
               raise Parent_Not_Available;
            end if;
         end if;
         D_Ptr.Value.Declaration.Parent_Location := new File_Location_Node;
         D_Ptr.Value.Declaration.Parent_Location.Value :=
               (File => (LI => Tmp_LI_File_Ptr,
                         Part => Unit_Body,
                         Source_Filename => new String'(Parent_Filename)),
                Line => Parent_Location.Line,
                Column => Parent_Location.Column);
         D_Ptr.Value.Declaration.Parent_Location.Next := null;
         --  ??? How does the procedure looks like to support multiply
         --  inheritance?
      end if;
      D_Ptr.Value.Declaration.Scope := Scope;
      if End_Of_Scope_Location = Invalid_Point then
         D_Ptr.Value.Declaration.End_Of_Scope := No_Reference;
      else
         D_Ptr.Value.Declaration.End_Of_Scope.Location :=
                   (File => (LI => File,
                             Part => Unit_Body,
                             Source_Filename => new String'(Source_Filename)),
                    Line => End_Of_Scope_Location.Line,
                    Column => End_Of_Scope_Location.Column);
         D_Ptr.Value.Declaration.End_Of_Scope.Kind := End_Of_Body;
      end if;
      if Rename_Location = Invalid_Point then
         D_Ptr.Value.Declaration.Rename := Null_File_Location;
      else
         D_Ptr.Value.Declaration.Rename :=
                   (File => (LI => No_LI_File,
                             Part => Unit_Body,
                             Source_Filename => new String'(Rename_Filename)),
                    Line => Rename_Location.Line,
                    Column => Rename_Location.Column);
         --  ??? we need to search for appropriate File in which
         --  renamed entity is really declared
      end if;
   end Insert_Declaration_Internal;

   -----------------------------------
   --  Find_Declaration_Internal  --
   -----------------------------------

   function Find_Declaration_Internal
     (Declaration_Info_Ptr    : in E_Declaration_Info_List;
      Symbol_Name             : in String := "";
      Class_Name              : in String := "";
      Kind                    : in E_Kind := No_Kind;
      Location                : in Point := Invalid_Point)
   return E_Declaration_Info_List is
      D_Ptr : E_Declaration_Info_List := Declaration_Info_Ptr;
   begin
      loop
         exit when D_Ptr = null;
         if ((Symbol_Name'Length > 0
                and then (D_Ptr.Value.Declaration.Name.all = Symbol_Name))
            or else (Symbol_Name'Length = 0))
           and then (
             (Location /= Invalid_Point
                and then D_Ptr.Value.Declaration.Location.Line
                                                   = Location.Line
                and then D_Ptr.Value.Declaration.Location.Column
                                                   = Location.Column)
             or else (Location = Invalid_Point)
           ) and then (
             (Class_Name /= ""
                and then Belongs_To_Class
                          (Declaration_Info_Ptr => Declaration_Info_Ptr,
                           Class_Name           => Class_Name,
                           Position             => Location))
             or else (Class_Name = "")
           ) and then (
             (Kind /= No_Kind
                and then D_Ptr.Value.Declaration.Kind = Kind)
             or else Kind = No_Kind
           )
         then
            return D_Ptr;
         end if;
         D_Ptr := D_Ptr.Next;
      end loop;
      raise Declaration_Not_Found;
   end Find_Declaration_Internal;

   ----------------------
   --  Create_LI_File  --
   ----------------------

   procedure Create_LI_File
     (File                    : out LI_File_Ptr;
      Handler                 : in LI_Handler;
      Source_Filename         : in String;
      Parsed                  : in Boolean) is
   begin
      if (Parsed) then
         File := new LI_File_Constrained'
                   (LI =>  (Parsed => True,
                           Handler => LI_Handler (Handler),
                           LI_Filename => new String' (Source_Filename),
                           Body_Info => null,
                           Spec_Info => null,
                           Dependencies_Info => null,
                           Compilation_Errors_Found => False,
                           Separate_Info => null,
                           LI_Timestamp => To_Timestamp
                             (File_Time_Stamp (Source_Filename))));
         --  ??? If source_filename is incorrect?

      else
         File := new LI_File_Constrained'
                  (LI =>  (Parsed => False,
                           Handler => LI_Handler (Handler),
                           LI_Filename => new String' (Source_Filename),
                           Body_Info => null,
                           Spec_Info => null,
                           Separate_Info => null,
                           LI_Timestamp => To_Timestamp
                             (File_Time_Stamp (Source_Filename))));
         --  ??? If source_filename is incorrect?
      end if;
   end Create_LI_File;

   ------------------------
   --  Create_File_Info  --
   ------------------------

   procedure Create_File_Info
     (FI_Ptr   : in out File_Info_Ptr;
      Source_Filename : in String;
      Directory_Name  : in String) is
   begin
      FI_Ptr := new File_Info'
        (Unit_Name         => null,
         Source_Filename   => new String'(Source_Filename),
         Directory_Name    => new String'(Directory_Name),
         File_Timestamp    => To_Timestamp
                                (File_Time_Stamp (Source_Filename)),
         Original_Filename => null,
         Original_Line     => 1,
         Declarations      => null);
   end Create_File_Info;


   ------------------------
   --  Belongs_To_Class  --
   ------------------------

   function Belongs_To_Class
     (Declaration_Info_Ptr : in E_Declaration_Info_List;
      Class_Name           : in String;
      Position             : in Point) return Boolean
   is
      D_Ptr : E_Declaration_Info_List := Declaration_Info_Ptr;
   begin
      loop
         exit when D_Ptr = null;
         if D_Ptr.Value.Declaration.Kind = Record_Type
           and then D_Ptr.Value.Declaration.Name.all = Class_Name
           and then (
             D_Ptr.Value.Declaration.Location.Line < Position.Line
               or else (D_Ptr.Value.Declaration.Location.Line =
                                                   Position.Line and
                 D_Ptr.Value.Declaration.Location.Column < Position.Column))
           and then (
             D_Ptr.Value.Declaration.End_Of_Scope.Location.Line > Position.Line
               or else (D_Ptr.Value.Declaration.End_Of_Scope.Location.Line =
                                                   Position.Line and
                 D_Ptr.Value.Declaration.End_Of_Scope.Location.Column >
                                                   Position.Column))
         then
            return True;
         end if;
         D_Ptr := D_Ptr.Next;
      end loop;
      return False;
   end Belongs_To_Class;

end Src_Info.LI_Utils;
