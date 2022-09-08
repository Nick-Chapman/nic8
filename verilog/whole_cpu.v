
`define suff _NET

module whole_cpu (input clk, reset);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus;
   wire `Control controlBits;
   wire aIsZero,flagCarry;

   wire loadBarIR,storeMemBar;
   wire assertBarM,assertBarE,assertBarA,assertBarX;
   wire immediate,doSubtract,doJump;

   wire clkBar = ~clk;
   wire resetBar = ~reset;

   wire _;
   assign {loadBarIR,storeMemBar,
           _,_,_,_,
           assertBarM,assertBarE,_,_,
           immediate,doSubtract,doJump
           } = controlBits;

   rom prog (    (~assertBarM &&  immediate),            pc,  dbus);
   ram data (clk,(~assertBarM && !immediate),storeMemBar,xreg,dbus);

   programCounter`suff p (resetBar,clk,~doJump,immediate,dbus,pc);

   fetch`suff f (clk,resetBar,loadBarIR,dbus,ir);

   control`suff c (ir,clk,aIsZero,flagCarry,controlBits);

   registers`suff registers
     (clkBar,resetBar,controlBits,dbus,areg,breg,xreg,qreg);

   alu`suff a
     (clk,reset,doSubtract,assertBarE,areg,breg,dbus,aIsZero,flagCarry);

endmodule
