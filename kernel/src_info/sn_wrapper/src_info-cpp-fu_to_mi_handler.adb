separate (Src_Info.CPP)


------------------------
--  Fu_To_Mi_Handler  --
------------------------

procedure Fu_To_Mi_Handler (Ref : TO_Table) is
   P             : Pair_Ptr;
   Fn            : MI_Table;
   MDecl         : MD_Table;
   MDecl_Tmp     : MD_Table;
   Decl_Info     : E_Declaration_Info_List;
   Overloaded    : Boolean := False;
   Init          : Boolean := True;
   Pure_Virtual  : Boolean := False;
   Kind          : E_Kind;
   IsTemplate    : Boolean := False;
   Ref_Id        : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Ref_Class     : constant String := Ref.Buffer
     (Ref.Referred_Class.First .. Ref.Referred_Class.Last);
   Attributes    : SN_Attributes;
   Filename_Buf  : SN.String_Access;
   Filename      : Segment;
   Start_Position : Point;

   function Find_Method (Fn : MI_Table; MD_Tab : MD_Table)
      return E_Declaration_Info_List;
   --  searches for forward declaration. if no fwd decl found, searches for
   --  implementation. If nothing found throws Declaration_Not_Found
   --  TODO multiple forward declarations

   function Find_Method (Fn : MI_Table; MD_Tab : MD_Table)
      return E_Declaration_Info_List is
      Decl_Info    : E_Declaration_Info_List;
   begin
      Decl_Info := Find_First_Forward_Declaration
        (MD_Tab.Buffer,
         MD_Tab.Class,
         MD_Tab.Name,
         MD_Tab.File_Name,
         MD_Tab.Return_Type,
         MD_Tab.Arg_Types);
      if Decl_Info = null then
         raise Declaration_Not_Found;
      end if;
      return Decl_Info;
   exception
      when Declaration_Not_Found =>
         return Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
            Class_Name  => Fn.Buffer (Fn.Class.First .. Fn.Class.Last),
            Location    => Fn.Start_Position);
   end Find_Method;

begin

   Info ("Fu_To_Mi_Handler: " & Ref_Id);

   Set_Cursor
     (SN_Table (MD),
      By_Key,
      Ref_Class & Field_Sep & Ref_Id & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (MD), Next_By_Key);
      exit when P = null;
      MDecl_Tmp := Parse_Pair (P.all);
      Free (P);
      if Init then
         Init  := False;
         MDecl := MDecl_Tmp;
      else
         Overloaded := not Cmp_Arg_Types -- skip multiple fws decls
           (MDecl_Tmp.Buffer,
            MDecl.Buffer,
            MDecl_Tmp.Arg_Types,
            MDecl.Arg_Types);
         Free (MDecl_Tmp);
         exit when Overloaded;
      end if;
   end loop;

   if Init then -- declaration for the referred method not found
      --  ??? We should handle this situation in a special way:
      Fail ("unable to find method " & Ref_Class & "::" & Ref_Id);
      return;
   end if;

   --  Once we have found the declaration(s) we may try to look up
   --  implementation as well
   if not Overloaded then
      Set_Cursor
        (SN_Table (MI),
         By_Key,
         Ref_Class & Field_Sep & Ref_Id & Field_Sep,
         False);

      Init := True;
      loop
         P := Get_Pair (SN_Table (MI), Next_By_Key);
         exit when P = null;
         Fn := Parse_Pair (P.all);
         Free (P);
         Init := False;
         exit when Cmp_Arg_Types
           (MDecl.Buffer,
            Fn.Buffer,
            MDecl.Arg_Types,
            Fn.Arg_Types);
         Init := True;
         Free (Fn);
      end loop;

      if Init then -- implementation for the referred method not found
         --  this must be a pure virtual method
         Attributes := SN_Attributes (MDecl.Attributes);
         if (Attributes and SN_PUREVIRTUAL) /= SN_PUREVIRTUAL then
            Fail ("failed to locate method implementation, but it is not"
               & " an abstract one: " & Ref_Class & "::" & Ref_Id);
            Free (MDecl);
            return;
         end if;
         Pure_Virtual := True;
      end if;

      --  If method
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      declare
         Class_Def    : CL_Table;
      begin -- check if this class is template
         Class_Def := Find (SN_Table (CL), Ref_Class);
         IsTemplate := Class_Def.Template_Parameters.First
            < Class_Def.Template_Parameters.Last;
         Free (Class_Def);
      exception
         when DB_Error | Not_Found =>
            null;
      end;

      if Pure_Virtual then
         Filename_Buf   := MDecl.Buffer;
         Filename       := MDecl.File_Name;
         Start_Position := MDecl.Start_Position;
      else
         Filename_Buf   := Fn.Buffer;
         Filename       := Fn.File_Name;
         Start_Position := Fn.Start_Position;
      end if;

      if MDecl.Buffer (MDecl.Return_Type.First .. MDecl.Return_Type.Last)
            = "void" then
         if IsTemplate then
            Kind := Generic_Function_Or_Operator;
         else
            Kind := Non_Generic_Function_Or_Operator;
         end if;
      else
         if IsTemplate then
            Kind := Generic_Procedure;
         else
            Kind := Non_Generic_Procedure;
         end if;
      end if;
      if Filename_Buf (Filename.First .. Filename.Last)
            = Get_LI_Filename (Global_LI_File) then
         begin
            --  this is a method defined in the current file
            --  it may be either forward declared or implemented
            --  right away
            if Pure_Virtual then
               Decl_Info := Find_First_Forward_Declaration
                 (MDecl.Buffer,
                  MDecl.Class,
                  MDecl.Name,
                  MDecl.File_Name,
                  MDecl.Return_Type,
                  MDecl.Arg_Types);
               if Decl_Info = null then
                  raise Declaration_Not_Found;
               end if;
            else
               Decl_Info := Find_Method (Fn, MDecl);
            end if;
         exception
            when Declaration_Not_Found =>
               --  method is in the current file, but used before
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
            --  this method is defined somewhere else...
            Decl_Info := Find_Dependency_Declaration
              (File                 => Global_LI_File,
               Symbol_Name          => Ref_Id,
               Class_Name           => Ref_Class,
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
                  Source_Filename    => Ref.Buffer
                     (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location           => Start_Position,
                  Kind               => Kind,
                  Scope              => Global_Scope,
                  Referred_Filename  => Filename_Buf
                     (Filename.First .. Filename.Last),
                  Declaration_Info   => Decl_Info);
         end;
      end if;

      if not Pure_Virtual then
         Free (Fn);
      end if;
   else -- overloaded entity
      --  have we already declared it?
      declare
         Class_Def : CL_Table;
      begin
         Class_Def := Find (SN_Table (CL), Ref_Class);
         --  ??? what to do when several classes with one name are available
         --  what about unions?

         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Ref_Id,
            Class_Name  => Ref_Class,
            Kind        => Overloaded_Entity,
            Location    => Class_Def.Start_Position);
         Free (Class_Def);
      exception
         when DB_Error | Not_Found =>
            Fail ("Failed to lookup class " & Ref_Class
               & " for method " & Ref_Id);
            Free (MDecl);
            return;
         when Declaration_Not_Found =>
            Decl_Info := new E_Declaration_Info_Node'
              (Value =>
                 (Declaration => No_Declaration,
                  References => null),
               Next => Global_LI_File.LI.Body_Info.Declarations);
            Decl_Info.Value.Declaration.Name := new String'(Ref_Id);
            Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
            Decl_Info.Value.Declaration.Location.Line :=
               Class_Def.Start_Position.Line;
            Decl_Info.Value.Declaration.Location.File :=
               (LI              => Global_LI_File,
                Part            => Unit_Body,
                Source_Filename =>
                   new String'(Get_LI_Filename (Global_LI_File)));
            Decl_Info.Value.Declaration.Location.Column :=
               Class_Def.Start_Position.Column;
            Global_LI_File.LI.Body_Info.Declarations := Decl_Info;
            Free (Class_Def);
      end;
   end if;
   Free (MDecl);

   Insert_Reference
     (Decl_Info,
      Global_LI_File,
      Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Ref.Position,
      Reference);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("unable to find method " & Ref_Class & "::" & Ref_Id);
      return;
end Fu_To_Mi_Handler;

