
module whole_cpu
  (input clk, reset,
   output [7:0] ir, pc, areg, breg, xreg, qreg, dbus,
   output `Control controlBits);

   wire [7:0] ir,pc,areg,breg,xreg,qreg,dbus,aluOut;
   wire flagCarry;
   wire `Control controlBits;
   wire carry, aIsZero;

   wire storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,doSubtract,doJump;

   wire _;
   assign {_,_,_,_,_,_,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,_,doSubtract,doJump
           } = controlBits;

   assign dbus = assertE ? aluOut : 'z;
   assign dbus = assertA ? areg : 'z;
   assign dbus = assertX ? xreg : 'z;

   rom prog (    (assertM &&  immediate),         pc,dbus);
   ram data (clk,(assertM && !immediate),storeMem,xreg,dbus);

   programCounterNET p (reset,clk,doJump,immediate,dbus,pc);

   registers r (reset,clk,controlBits,carry,dbus,
                ir,areg,breg,xreg,qreg,flagCarry);

   control c (ir,aIsZero,flagCarry,controlBits);

   alu a (areg,breg,doSubtract,aluOut,carry,aIsZero);

endmodule

