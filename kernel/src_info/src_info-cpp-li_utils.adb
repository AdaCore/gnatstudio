package body Src_Info.CPP.LI_Utils is

   procedure Insert_Declaration_Internal (
      D_Ptr                   : in out E_Declaration_Info_List;
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

   -------------------------------------------------------------------------

   procedure Insert_Declaration (
      Handler                 : in LI_Handler;
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
      Declaration_Info        : in out E_Declaration_Info_List := null
   ) is
      D_Ptr : E_Declaration_Info_List;
   begin
      if File = No_LI_File
      then
         File := new LI_File_Constrained'
            (LI => (Parsed := False,
                    Handler => LI_Handler (Handler),
                    LI_Filename => new String'(Source_Filename),
                    Body_Info => new File_Info,
                    Spec_Info => null,
                    Separate_Info => null,
                    LI_Timestamp => 0));
         File := new LI_File_Constrained;
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
         if File.LI.Body_Info.Source_Filename.all /= Source_Filename then
            raise Invalid_Source_Filename;
         end if;
      end if;
      if File.LI.Body_Info.Declarations = null
      then
         File.LI.Body_Info.Declarations := new E_Declaration_Info_Node;
         File.LI.Body_Info.Declarations.next := null;
         D_Ptr := File.LI.Body_Info.Declarations;
      else
         D_Ptr := File.LI.Body_Info.Declarations;
         loop
            exit when D_Ptr.next = null;
            D_Ptr := D_Ptr.next;
         end loop;
         D_Ptr.next := new E_Declaration_Info_Node;
         D_Ptr.next.next := null;
         D_Ptr := D_Ptr.next;
      end if;
      Insert_Declaration_Internal (D_Ptr, File, Symbol_Name, Source_Filename,
            Location, Parent_Filename, Parent_Location, Kind, Scope,
            End_Of_Scope_Location, Rename_Filename, Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Declaration;

   procedure Insert_Dependency_Declaration (
      Handler                 : in LI_Handler
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Source_Filename         : in String;
      Location                : in Point;
      Parent_Filename         : in String := "";
      Parent_Location         : in Point := Invalid_Point;
      Kind                    : in E_Kind;
      Scope                   : in E_Scope;
      End_Of_Scope_Location   : in Point := Invalid_Point;
      Rename_Filename         : in String := "";
      Rename_Location         : in Point := Invalid_Point;
      Declaration_Info        : in out E_Declaration_Info_List := null) is
   begin
      D_Ptr : E_Declaration_Info_List;
      Tmp_LI_File : LI_File;
   begin
      if File = No_LI_File then
         File := new LI_File_Constrained'
            (LI => (Parsed := True,
                    Handler => LI_Handler (Handler),
                    LI_Filename => new String'(Source_Filename),
                    Body_Info => null,
                    Spec_Info => null,
                    Separate_Info => null,
                    LI_Timestamp => 0,
                    Compilation_Errors_Found => False,
                    Dependencies_Info => null));
      elsif File.LI.Parsed = False then
         Tmp_LI_File := File.LI;
         File := new LI_File_Constrained'
            (LI => (Parsed := True,
                    Handler => LI_Handler (Handler),
                    LI_Filename => new String'(Source_Filename),
                    Body_Info => Tmp_LI_File.Body_Info,
                    Spec_Info => null,
                    Separate_Info => null,
                    LI_Timestamp => 0,
                    Compilation_Errors_Found => False,
                    Dependencies_Info => null));
      end if;
      if File.LI.Dependencies_Info = null
      then
         File.LI.Dependencies_Info := new Dependency_File_Info_Node;
         File.LI.Dependencies_Info.File := No_Source_File;
         --  FIX ME
         File.LI.Dependencies_Info.Dep_Info := (Depends_From_Spec => False,
                                                Depends_From_Body => True);
         File.LI.Dependencies_Info.Declarations := null;
      end if;                                 
      if File.LI.Dependencies_Info.Declarations = null
      then
         File.LI.Dependencies_Info.Declarations := new E_Declaration_Info_Node;
         File.LI.Dependencies_Info.Declarations.next := null;
         D_Ptr := File.LI.Dependencies_Info.Declarations;
      else
         D_Ptr := File.LI.Dependencies_Info.Declarations;
         loop
            exit when D_Ptr.next = null;
            D_Ptr := D_Ptr.next;
         end loop;
         D_Ptr.next := new E_Declaration_Info_Node;
         D_Ptr.next.next := null;
         D_Ptr := D_Ptr.next;
      end if;
      Insert_Declaration_Internal (D_Ptr, File, Symbol_Name, Source_Filename,
            Location, Parent_Filename, Parent_Location, Kind, Scope,
            End_Of_Scope_Location, Rename_Filename, Rename_Location);
      Declaration_Info := D_Ptr;
   end Insert_Dependency_Declaration;

   procedure Insert_Reference (
      Declaration_Info        : in out E_Declaration_Info_List;
      Source_Filename         : in String;
      Location                : in Point;
      Kind                    : Reference_Kind) is
   begin
      null;
   end Insert_Reference;

   function Find_Declaration (
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Location                : in Point) return E_Declaration_Info_List is
   begin
      null;
   end Find_Declaration;

   function Find_Dependency_Declaration (
      File                    : in out LI_File_Ptr;
      Name                    : in String;
      Location                : in Point) return E_Declaration_Info_List is
   begin
      null;
   end Find_Dependency_Declaration;

   -------------------------------------------------------------------------
   procedure Insert_Declaration_Internal (
      D_Ptr                   : in out E_Declaration_Info_List;
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
      D_Ptr.value.References := null;
      D_Ptr.value.Declaration.Name := new String'(Symbol_Name);
      D_Ptr.value.Declaration.Location.File.LI := File;
      D_Ptr.value.Declaration.Location.File.Part := Unit_Body;
      D_Ptr.value.Declaration.Location.File.Source_Filename
            := new String'(Source_Filename);
      D_Ptr.value.Declaration.Location.Line := Location.Line;
      D_Ptr.value.Declaration.Location.Column := Location.Column;
      D_Ptr.value.Declaration.Kind := Kind;
      if Parent_Location = Invalid_Point then
         D_Ptr.value.Declaration.Parent_Location := Null_File_Location;
      else
         D_Ptr.value.Declaration.Parent_Location.File.LI := File;
         D_Ptr.value.Declaration.Parent_Location.File.Part := Unit_Body;
         D_Ptr.value.Declaration.Parent_Location.File.Source_Filename
               := new String'(Parent_Filename);
         D_Ptr.value.Declaration.Parent_Location.Line := Parent_Location.Line;
         D_Ptr.value.Declaration.Parent_Location.Column
               := Parent_Location.Column;
      end if;
      if End_Of_Scope_Location = Invalid_Point then
         D_Ptr.value.Declaration.End_Of_Scope := No_Reference;
      else
         D_Ptr.value.Declaration.End_Of_Scope.Location.File.LI := File;
         D_Ptr.value.Declaration.End_Of_Scope.Location.File.Part := Unit_Body;
         D_Ptr.value.Declaration.End_Of_Scope.Location.File.Source_Filename
               := new String'(Source_Filename);
         D_Ptr.value.Declaration.End_Of_Scope.Location.Line
               := End_Of_Scope_Location.Line;
         D_Ptr.value.Declaration.End_Of_Scope.Location.Column
               := End_Of_Scope_Location.Column;
      end if;
      if Rename_Location = Invalid_Point then
         D_Ptr.value.Declaration.Rename := Null_File_Location;
      else
         D_Ptr.value.Declaration.Rename.File.LI := File;
         D_Ptr.value.Declaration.Rename.File.Part := Unit_Body;
         D_Ptr.value.Declaration.Rename.File.Source_Filename :=
               new String'(Rename_Filename);
         D_Ptr.value.Declaration.Rename.Line := Rename_Location.Line;
         D_Ptr.value.Declaration.Rename.Column := Rename_Location.Column;
      end if;
   end Insert_Declaration_Internal;
   -------------------------------------------------------------------------

end Src_Info.CPP.LI_Utils;
