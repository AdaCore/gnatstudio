with Glide_Kernel; use Glide_Kernel;
with Commands;     use Commands;
with Diff_Utils;   use Diff_Utils;


package Vdiff2_Command is

   type Handler_Action is access procedure
     (Kernel : Kernel_Handle;
      Diff : in out Diff_Head_Access);

   type Diff_Command is new Root_Command with record
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Occurrence_List_Access;
      Action    : Handler_Action;
   end record;
   type Diff_Command_Access is access all Diff_Command;

   procedure Create
     (Item      : out Diff_Command_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Occurrence_List_Access;
      Action    : Handler_Action);

   function Execute (Command : access Diff_Command)
                     return Command_Return_Type;

   procedure Next_Difference (Kernel : Kernel_Handle;
                             Diff   : in out  Diff_Head_Access);

   procedure Prev_Difference (Kernel : Kernel_Handle;
                             Diff   : in out Diff_Head_Access);

   procedure First_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access);

   procedure Last_Difference (Kernel : Kernel_Handle;
                             Diff   : in out Diff_Head_Access);
end Vdiff2_Command;
