
`define Control [1:14]

module main;

   reg reset, clk;
   initial reset = 1;
   initial #2 clk = 0;
   initial #3 reset = 0;
   always #5 clk <= ~clk;

   wire [7:0] ir,pc,areg,breg,xreg,qreg,dbus,aluOut;
   wire flagCarry;
   wire `Control controlBits;
   wire storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;
   wire carry, aIsZero;

   assign dbus = assertE ? aluOut : 'z;
   assign dbus = assertA ? areg : 'z;
   assign dbus = assertX ? xreg : 'z;

   assign {storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract} = controlBits[7:14];

   rom prog (    (assertM &&  immediate),         pc,dbus);
   ram data (clk,(assertM && !immediate),storeMem,xreg,dbus);

   monitor m (clk,ir,pc,areg,breg,xreg,qreg,controlBits,dbus);

   registers r (reset,clk,controlBits,carry,dbus,
                ir,pc,areg,breg,xreg,qreg,flagCarry);

   control c (ir,aIsZero,flagCarry,controlBits);

   alu a (areg,breg,doSubtract,aluOut,carry,aIsZero);

endmodule
