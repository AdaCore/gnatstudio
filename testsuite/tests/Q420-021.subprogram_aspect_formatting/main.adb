procedure Main is

   procedure Enter_Root_Mode
     with
       Global  => (Input  => CPU_Global.CPU_ID, In_Out => X86_64.State),Depends => (X86_64.State =>+ CPU_Global.CPU_ID);

   procedure Enter_Root_Mode with
     Global  => (Input  => CPU_Global.CPU_ID, In_Out => X86_64.State),Depends => (X86_64.State =>+ CPU_Global.CPU_ID);

begin
   null;
end Main;
