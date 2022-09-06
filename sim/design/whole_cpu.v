
module whole_cpu
  (input clk, reset,
   output [7:0] ir, pc, areg, breg, xreg, qreg, dbus,
   output `Control controlBits);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus,aluOut;
   wire `Control controlBits;
   wire aIsZero,flagCarry;

   wire loadIR,storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,doSubtract,doJump;

   wire _;
   assign {loadIR,_,_,_,_,_,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,_,doSubtract,doJump
           } = controlBits;

   assign dbus = assertE ? aluOut : 'z;
   assign dbus = assertA ? areg : 'z;
   assign dbus = assertX ? xreg : 'z;

   rom prog (    (assertM &&  immediate),         pc,dbus);
   ram data (clk,(assertM && !immediate),storeMem,xreg,dbus);

   programCounterNET p (reset,clk,doJump,immediate,dbus,pc);

   fetch_unit f (clk,reset,loadIR,dbus,ir);

   control c (ir,aIsZero,flagCarry,controlBits);

   registers r (reset,clk,controlBits,dbus,
                areg,breg,xreg,qreg);

   alu a (clk,reset,doSubtract,assertE,areg,breg,
          aluOut,aIsZero,flagCarry);

endmodule
