
`define Control [1:14]

module main;

   reg reset, clk;
   initial reset = 1;
   initial #2 clk = 0;
   initial #2 reset = 0;
   always #5 clk <= ~clk;

   wire [7:0] ir,pc,areg,breg,xreg,qreg,abus,dbus,aluOut;
   wire flagCarry;
   wire `Control controlBits;
   wire storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;
   wire carry, aIsZero;

   assign abus = immediate?pc:xreg;

   assign dbus = assertE ? aluOut : 'z;
   assign dbus = assertA ? areg : 'z;
   assign dbus = assertX ? xreg : 'z;

   assign {storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract} = controlBits[7:14];

   memory mem (clk,assertM,storeMem,abus,dbus);

   monitor m (clk,ir,pc,areg,breg,xreg,qreg,controlBits,abus,dbus);

   registers r (reset,clk,controlBits,carry,dbus,abus,
                ir,pc,areg,breg,xreg,qreg,flagCarry);

   control c (ir,aIsZero,flagCarry,controlBits);

   alu a (areg,breg,doSubtract,aluOut,carry,aIsZero);

endmodule
