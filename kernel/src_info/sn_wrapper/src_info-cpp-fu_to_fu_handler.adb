separate (Src_Info.CPP)


------------------------
--  Fu_To_Fu_Handler  --
------------------------

procedure Fu_To_Fu_Handler (Ref : TO_Table) is
   P              : Pair_Ptr;
   Fn             : FU_Table;
   Decl_Info      : E_Declaration_Info_List;
   Overloaded     : Boolean := False;
   Init           : Boolean := True;
   No_Body        : Boolean := True;
   Kind           : E_Kind;
   FDecl          : FD_Table;
   FDecl_Tmp      : FD_Table;
   Ref_Id         : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Filename_Buf   : SN.String_Access;
   Filename       : Segment;
   Start_Position : Point;

   function Find_Function (Fn : FU_Table; FD_Tab : FD_Table)
      return E_Declaration_Info_List;
   --  searches for forward declaration. if no fwd decl found, searches for
   --  implementation. If nothing found throws Declaration_Not_Found

   function Find_Function (Fn : FU_Table; FD_Tab : FD_Table)
      return E_Declaration_Info_List is
      Decl_Info    : E_Declaration_Info_List;
   begin
      Decl_Info := Find_First_Forward_Declaration
        (FD_Tab.Buffer,
         FD_Tab.Name,
         FD_Tab.File_Name,
         FD_Tab.Return_Type,
         FD_Tab.Arg_Types);
      if Decl_Info = null then
         raise Declaration_Not_Found;
      end if;
      return Decl_Info;
   exception
      when Declaration_Not_Found =>
         return Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
            Location    => Fn.Start_Position);
   end Find_Function;

begin

   Info ("Fu_To_Fu_Handler: " & Ref_Id);

   Set_Cursor
     (SN_Table (FD),
      By_Key,
      Ref_Id,
      False);

   loop
      P := Get_Pair (SN_Table (FD), Next_By_Key);
      exit when P = null;
      FDecl_Tmp := Parse_Pair (P.all);
      Free (P);
      if Init then
         FDecl := FDecl_Tmp;
         Init  := False;
      else
         Overloaded := not Cmp_Arg_Types -- skip multiple fwd decls
            (FDecl.Buffer,
             FDecl_Tmp.Buffer,
             FDecl.Arg_Types,
             FDecl_Tmp.Arg_Types);
         Free (FDecl_Tmp);
         exit when Overloaded;
      end if;
   end loop;

   if Init then -- referred function not found
      Fail ("declaration for referred function " & Ref_Id & " not found");
      return;
   end if;

   if not Overloaded then
      Set_Cursor
        (SN_Table (FU),
         By_Key,
         Ref_Id,
         False);

      loop
         P := Get_Pair (SN_Table (FU), Next_By_Key);
         exit when P = null;
         Fn := Parse_Pair (P.all);
         Free (P);
         No_Body := False;
         exit when Cmp_Arg_Types
            (FDecl.Buffer,
             Fn.Buffer,
             FDecl.Arg_Types,
             Fn.Arg_Types);
         Free (FDecl);
         No_Body := True;
      end loop;

      if No_Body then
         Filename_Buf   := FDecl.Buffer;
         Filename       := FDecl.File_Name;
         Start_Position := FDecl.Start_Position;
      else
         Filename_Buf   := Fn.Buffer;
         Filename       := Fn.File_Name;
         Start_Position := Fn.Start_Position;
      end if;

      --  If procedure
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      if FDecl.Buffer (FDecl.Return_Type.First .. FDecl.Return_Type.Last)
            = "void" then
         Kind := Non_Generic_Function_Or_Operator;
      else
         Kind := Non_Generic_Procedure;
      end if;
      if Filename_Buf (Filename.First .. Filename.Last)
            = Get_LI_Filename (Global_LI_File) then
         begin
            --  this is a function defined in the current file
            --  it may be either forward declared or implemented
            --  right away
            if No_Body then
               Decl_Info := Find_First_Forward_Declaration
                 (FDecl.Buffer,
                  FDecl.Name,
                  FDecl.File_Name,
                  FDecl.Return_Type,
                  FDecl.Arg_Types);
               if Decl_Info = null then
                  raise Declaration_Not_Found;
               end if;
            else
               Decl_Info := Find_Function (Fn, FDecl);
            end if;
         exception
            when Declaration_Not_Found =>
               --  function is in the current file, but used before
               --  declaration. Create forward declaration
               Insert_Declaration
                 (Handler            => LI_Handler (Global_CPP_Handler),
                  File               => Global_LI_File,
                  List               => Global_LI_File_List,
                  Symbol_Name        => Ref_Id,
                  Source_Filename    => Ref.Buffer
                     (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location           => Ref.Position,
                  Kind               => Kind,
                  Scope              => Global_Scope,
                  Declaration_Info   => Decl_Info);
         end;
      else
         begin
            --  this function is defined somewhere else...
            Decl_Info := Find_Dependency_Declaration
              (File                 => Global_LI_File,
               Symbol_Name          => Ref_Id,
               Filename             => Filename_Buf
                 (Filename.First .. Filename.Last),
               Location             => Start_Position);
         exception
            when Declaration_Not_Found => -- insert dep decl
               Insert_Dependency_Declaration
                 (Handler            => LI_Handler (Global_CPP_Handler),
                  File               => Global_LI_File,
                  List               => Global_LI_File_List,
                  Symbol_Name        => Ref_Id,
                  Source_Filename    =>
                     Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location           => Start_Position,
                  Kind               => Kind,
                  Scope              => Global_Scope,
                  Referred_Filename  => Filename_Buf
                    (Filename.First .. Filename.Last),
                  Declaration_Info   => Decl_Info);
         end;
      end if;

      if not No_Body then
         Free (Fn);
      end if;
   else -- overloaded entity
      --  have we already declared it?
      begin
         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Ref_Id,
            Kind        => Overloaded_Entity);
      exception
         when Declaration_Not_Found =>
            Decl_Info := new E_Declaration_Info_Node'
              (Value =>
                 (Declaration => No_Declaration,
                  References => null),
               Next => Global_LI_File.LI.Body_Info.Declarations);
            Decl_Info.Value.Declaration.Name := new String'(Ref_Id);
            Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
            Global_LI_File.LI.Body_Info.Declarations := Decl_Info;
      end;
   end if;
   Free (FDecl);

   Insert_Reference
     (Decl_Info,
      Global_LI_File,
      Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Ref.Position,
      Reference);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("unable to find function " & Ref_Id);
   return;
end Fu_To_Fu_Handler;

