separate (Src_Info.CPP)


------------------------
--  Fu_To_Fu_Handler  --
------------------------

procedure Fu_To_Fu_Handler (Ref : TO_Table) is
   P              : Pair_Ptr;
   Fn             : FU_Table;
   Fn_Tmp         : FU_Table;
   Decl_Info      : E_Declaration_Info_List;
   Overloaded     : Boolean := False;
   Forward_Declared : Boolean := False;
   No_Body        : Boolean := True;
   Kind           : E_Kind;
   FDecl          : FD_Table;
   FDecl_Tmp      : FD_Table;
   Ref_Id         : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Buffer   : SN.String_Access;
   Filename       : Segment;
   Return_Type    : Segment;
   Start_Position : Point;
begin

   Info ("Fu_To_Fu_Handler: " & Ref_Id);

   if Is_Open (SN_Table (FD)) then
      Set_Cursor (SN_Table (FD), By_Key, Ref_Id, False);

      loop
         P := Get_Pair (SN_Table (FD), Next_By_Key);
         exit when P = null;
         FDecl_Tmp := Parse_Pair (P.all);
         Free (P);
         if not Forward_Declared then
            FDecl := FDecl_Tmp;
            Forward_Declared := True;
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
   end if;

   if not Overloaded then
      --  Forward declarations may be overloaded by inline implementations
      --  this is what we check here. If no forward declaration was found
      --  above we search for a suitable function body
      Set_Cursor (SN_Table (FU), By_Key, Ref_Id, False);

      loop
         P := Get_Pair (SN_Table (FU), Next_By_Key);
         exit when P = null;
         Fn_Tmp := Parse_Pair (P.all);
         Free (P);
         if not Forward_Declared and No_Body then
            --  No forward decls, but we found the first function
            --  with the same name
            Fn      := Fn_Tmp;
            No_Body := False;
         elsif not Forward_Declared and not No_Body then
            --  No forward decls and we found one more function body
            --  with the same name
            Overloaded := True;
            Free (Fn_Tmp);
         elsif Forward_Declared and No_Body then
            --  We have found some forward declaration, but no body
            --  is yet found. Do we have overloading here?
            Overloaded := not Cmp_Arg_Types
               (Fn_Tmp.Buffer,
                FDecl.Buffer,
                Fn_Tmp.Arg_Types,
                FDecl.Arg_Types);
            if not Overloaded then -- we found the body!
               No_Body := False;
               Fn      := Fn_Tmp;
            else
               Free (Fn_Tmp); -- it's not our body, but it's overloading
            end if;
         else -- Forward_Declared and not No_Body
            --  We have found forward declaration and corresponding body
            --  all other bodies should be overloading functions
            Overloaded := True;
            Free (Fn_Tmp);
         end if;
         exit when Overloaded;
      end loop;
   end if;

   if not Forward_Declared and No_Body then
      Fail ("Can't find either forward declaration or body for " & Ref_Id);
      return;
   end if;

   if not Overloaded then
      pragma Assert (Forward_Declared or not No_Body, "Hey, what's up?");
      if No_Body then
         Buffer   := FDecl.Buffer;
         Filename       := FDecl.File_Name;
         Start_Position := FDecl.Start_Position;
         Return_Type    := FDecl.Return_Type;
      else
         Buffer   := Fn.Buffer;
         Filename       := Fn.File_Name;
         Start_Position := Fn.Start_Position;
         Return_Type    := Fn.Return_Type;
      end if;

      if Buffer (Return_Type.First .. Return_Type.Last) = "void" then
         Kind := Non_Generic_Function_Or_Operator;
      else
         Kind := Non_Generic_Procedure;
      end if;
      --  If procedure
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      if Buffer (Filename.First .. Filename.Last)
            = Get_LI_Filename (Global_LI_File) then
         begin
            --  this is a function defined in the current file
            --  it may be either forward declared or implemented
            --  right away
            if Forward_Declared then
               Decl_Info := Find_First_Forward_Declaration
                 (FDecl.Buffer,
                  FDecl.Name,
                  FDecl.File_Name,
                  FDecl.Return_Type,
                  FDecl.Arg_Types);
               if Decl_Info = null then
                  raise Declaration_Not_Found;
               end if;
            else -- when only body is available
               Decl_Info := Find_Declaration
                 (File        => Global_LI_File,
                  Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
                  Location    => Fn.Start_Position);
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
               Filename             => Buffer
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
                  Referred_Filename  => Buffer
                    (Filename.First .. Filename.Last),
                  Declaration_Info   => Decl_Info);
         end;
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

   if Forward_Declared then
      Free (FDecl);
   end if;

   if not No_Body then
      Free (Fn);
   end if;

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

