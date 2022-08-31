
`define Control [1:14]

module main;

   reg clk = 0;
   always #5 clk = ~clk;

   wire [7:0] ir,pc,areg,breg,xreg,qreg;
   wire flagCarry;

   wire [7:0] aluOut = doSubtract ? areg - breg : areg + breg;
   wire       carry = doSubtract ? !(breg > areg) : (areg + breg >= 256);
   wire       aIsZero = (areg == 0);

   wire [7:0] abus = immediate?pc:xreg;
   wire [7:0] dbus;

   assign dbus = assertE ? aluOut : 'z;
   assign dbus = assertA ? areg : 'z;
   assign dbus = assertX ? xreg : 'z;

   wire `Control controlBits;

   wire storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;

   assign {storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract} = controlBits[7:14];

   memory mem (clk,assertM,storeMem,abus,dbus);

   monitor m (clk,ir,pc,areg,breg,xreg,qreg,controlBits,abus,dbus);

   registers r (clk,controlBits,carry,dbus,abus,
                ir,pc,areg,breg,xreg,qreg,flagCarry);

   control c (ir,aIsZero,flagCarry,controlBits);

endmodule
