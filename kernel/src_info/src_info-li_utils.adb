package body Src_Info.LI_Utils is

   procedure Insert_Declaration_Internal
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : in LI_File_Ptr;
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
      Symbol_Name             : in String;
      Location                : in Point) return E_Declaration_Info_List;
   --  Finds declaration with given attributes in
   --  specified E_Declaration_Info_List

   function eq (str1 : String; str2 : String) return Boolean;
   --  Checks two strings for equality

   -------------------------------------------------------------------------

   --------------------------
   --  Insert_declaration  --
   --------------------------

   procedure Insert_Declaration
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
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
         File := new LI_File_Constrained'
               (LI =>  (Parsed => True,
                        Handler => LI_Handler (Handler),
                        LI_Filename => new String'(Source_Filename),
                        Body_Info => new File_Info,
                        Spec_Info => null,
                        Separate_Info => null,
                        Dependencies_Info => null,
                        Compilation_Errors_Found => False,
                        LI_Timestamp => 0));
         File.LI.LI_Filename := new String'(Source_Filename);
         File.LI.Body_Info := new File_Info'
               (Unit_Name => null,
                Source_Filename => new String'(Source_Filename),
                Directory_Name => null,
                File_Timestamp => 0,
                Original_Filename => null,
                Original_Line => 1,
                Declarations => null);
         File.LI.Spec_Info := null;
         File.LI.Separate_Info := null;
      else
         pragma Assert (File.LI.LI_Filename.all = Source_Filename,
                     "Invalid Source Filename");
         pragma Assert (LI_Handler (Handler) = File.LI.Handler,
                     "Invalid Handler");
         null;
      end if;
      if File.LI.Body_Info.Declarations = null then
         File.LI.Body_Info.Declarations := new E_Declaration_Info_Node;
         File.LI.Body_Info.Declarations.Next := null;
         D_Ptr := File.LI.Body_Info.Declarations;
      else
         D_Ptr := File.LI.Body_Info.Declarations;
         loop
            exit when D_Ptr.Next = null;
            D_Ptr := D_Ptr.Next;
         end loop;
         D_Ptr.Next := new E_Declaration_Info_Node;
         D_Ptr.Next.Next := null;
         D_Ptr := D_Ptr.Next;
      end if;
      Insert_Declaration_Internal (D_Ptr, File, Symbol_Name, Source_Filename,
            Location, Parent_Filename, Parent_Location, Kind, Scope,
            End_Of_Scope_Location, Rename_Filename, Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Declaration;

   -------------------------------------
   --  Insert_Dependency_Declaration  --
   -------------------------------------

   procedure Insert_Dependency_Declaration
     (Handler                 : in LI_Handler;
      File                    : in out LI_File_Ptr;
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
      Dep_Ptr : Dependency_File_Info_List;
      Tmp_LI_File : LI_File;
   begin
      if File = No_LI_File then
         File := new LI_File_Constrained'
            (LI => (Parsed => True,
                    Handler => LI_Handler (Handler),
                    LI_Filename => new String'(Source_Filename),
                    Body_Info => null,
                    Spec_Info => null,
                    Separate_Info => null,
                    LI_Timestamp => 0,
                    Compilation_Errors_Found => False,
                    Dependencies_Info => null));
      else
         pragma Assert (File.LI.LI_Filename.all /= Source_Filename,
                     "Invalid Source Filename");
         pragma Assert (LI_Handler (Handler) /= File.LI.Handler,
                     "Invalid Handler");
         if File.LI.Parsed = False then
            Tmp_LI_File := File.LI;
            File.LI := (Parsed => True,
                        Handler => LI_Handler (Handler),
                        LI_Filename => new String'(Source_Filename),
                        Body_Info => Tmp_LI_File.Body_Info,
                        Spec_Info => null,
                        Separate_Info => null,
                        LI_Timestamp => 0,
                        Compilation_Errors_Found => False,
                        Dependencies_Info => null);
         end if;
      end if;
      if File.LI.Dependencies_Info = null then
         File.LI.Dependencies_Info := new Dependency_File_Info_Node;
         File.LI.Dependencies_Info.Value.File := No_Source_File;
         --  FIX ME
         File.LI.Dependencies_Info.Value.Dep_Info :=
                                          (Depends_From_Spec => False,
                                           Depends_From_Body => True);
         File.LI.Dependencies_Info.Value.Declarations := null;
         File.LI.Dependencies_Info.Next := null;
         Dep_Ptr := File.LI.Dependencies_Info;
      else
         Dep_Ptr := File.LI.Dependencies_Info;
         loop
            exit when Dep_Ptr.Next = null;
            Dep_Ptr := Dep_Ptr.Next;
         end loop;
         Dep_Ptr.Next := new Dependency_File_Info_Node;
         Dep_Ptr.Next.Next := null;
         Dep_Ptr := Dep_Ptr.Next;
      end if;
      if Dep_Ptr.Value.Declarations = null then
         Dep_Ptr.Value.Declarations := new E_Declaration_Info_Node;
         Dep_Ptr.Value.Declarations.Next := null;
         D_Ptr := Dep_Ptr.Value.Declarations;
      else
         D_Ptr := Dep_Ptr.Value.Declarations;
         loop
            exit when D_Ptr.Next = null;
            D_Ptr := D_Ptr.Next;
         end loop;
         D_Ptr.Next := new E_Declaration_Info_Node;
         D_Ptr.Next.Next := null;
         D_Ptr := D_Ptr.Next;
      end if;
      Insert_Declaration_Internal (D_Ptr, File, Symbol_Name, Source_Filename,
            Location, Parent_Filename, Parent_Location, Kind, Scope,
            End_Of_Scope_Location, Rename_Filename, Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Dependency_Declaration;

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
      if R_Ptr = null then
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
      end if;
      R_Ptr.Value :=
         (Location => (File => (LI => File,
                                Part => Unit_Body,
                                Source_Filename =>
                                              new String'(Source_Filename)),
                       Line => Location.Line,
                       Column => Location.Column),
          Kind => Kind);
   end Insert_Reference;

   function Find_Declaration
     (File                    : in LI_File_Ptr;
      Symbol_Name             : in String;
      Location                : in Point) return E_Declaration_Info_List is
   begin
      if (File = No_LI_File)
            or else (File.LI.Body_Info = null)
            or else (File.LI.Body_Info.Declarations = null) then
         raise Declaration_Not_Found;
      end if;
      return Find_Declaration_Internal (File.LI.Body_Info.Declarations,
                                        Symbol_Name,
                                        Location);
   end Find_Declaration;

   -----------------------------------
   --  Find_Dependency_Declaration  --
   -----------------------------------

   function Find_Dependency_Declaration
     (File                    : in LI_File_Ptr;
      Symbol_Name             : in String;
      Filename                : in String;
      Location                : in Point) return E_Declaration_Info_List
   is
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
         exit when eq (Dep_Ptr.Value.File.Source_Filename.all, Filename);
         Dep_Ptr := Dep_Ptr.Next;
      end loop;
      if Dep_Ptr.Value.Declarations = null then
         raise Declaration_Not_Found;
      end if;
      return Find_Declaration_Internal (Dep_Ptr.Value.Declarations,
                                        Symbol_Name,
                                        Location);
   end Find_Dependency_Declaration;

   -------------------------------------------------------------------------

   -----------------------------------
   --  Insert_Declaration_Internal  --
   -----------------------------------

   procedure Insert_Declaration_Internal
     (D_Ptr                   : in out E_Declaration_Info_List;
      File                    : in LI_File_Ptr;
      Symbol_Name             : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point) is
   begin
      if D_Ptr = null then
         return;
      end if;
      D_Ptr.Value.References := null;
      D_Ptr.Value.Declaration.Name := new String'(Symbol_Name);
      D_Ptr.Value.Declaration.Location :=
                   (File => (LI => File,
                             Part => Unit_Body,
                             Source_Filename => new String'(Source_Filename)),
                    Line => Location.Line,
                    Column => Location.Column);
      D_Ptr.Value.Declaration.Kind := Kind;
      if Parent_Location = Invalid_Point then
         D_Ptr.Value.Declaration.Parent_Location := Null_File_Location;
      else
         D_Ptr.Value.Declaration.Parent_Location :=
               (File => (LI => File,
                         Part => Unit_Body,
                         Source_Filename => new String'(Parent_Filename)),
                Line => Parent_Location.Line,
                Column => Parent_Location.Column);
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
      end if;
      if Rename_Location = Invalid_Point then
         D_Ptr.Value.Declaration.Rename := Null_File_Location;
      else
         D_Ptr.Value.Declaration.Rename :=
                   (File => (LI => File,
                             Part => Unit_Body,
                             Source_Filename => new String'(Rename_Filename)),
                    Line => Rename_Location.Line,
                    Column => Rename_Location.Column);
      end if;
   end Insert_Declaration_Internal;

   -----------------------------------
   --  Find_Declaration_Internal  --
   -----------------------------------

   function Find_Declaration_Internal
     (Declaration_Info_Ptr    : in E_Declaration_Info_List;
      Symbol_Name             : in String;
      Location                : in Point) return E_Declaration_Info_List
   is
      D_Ptr : E_Declaration_Info_List;
   begin
      D_Ptr := Declaration_Info_Ptr;
      loop
         exit when D_Ptr = null;
         if eq (D_Ptr.Value.Declaration.Name.all, Symbol_Name)
           and then (D_Ptr.Value.Declaration.Location.Line = Location.Line)
           and then (D_Ptr.Value.Declaration.Location.Column = Location.Column)
         then
            return D_Ptr;
         end if;
         D_Ptr := D_Ptr.Next;
      end loop;
      raise Declaration_Not_Found;
   end Find_Declaration_Internal;

   ----------
   --  eq  --
   ----------

   function eq (str1 : String; str2 : String) return Boolean is
   begin
      if str1'Length /= str2'Length then
         return False;
      else
         return str1 = str2;
      end if;
   end eq;

   -------------------------------------------------------------------------

end Src_Info.LI_Utils;
