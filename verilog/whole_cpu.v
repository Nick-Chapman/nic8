
`define suff _NET

module whole_cpu (input clk, reset);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus;
   wire `Control controlBits;
   wire aIsZero,flagCarry,flagShift;

   wire loadBarIR,storeMemBar;
   wire assertBarRom,assertBarRam;
   wire assertBarM,assertBarE,assertBarS,assertBarA,assertBarX;
   wire doSubtract,doJump;

   wire clkBar = ~clk;
   wire resetBar = ~reset;

   wire _;
   assign {loadBarIR,storeMemBar,
           _,_,_,_,
           assertBarRom,assertBarRam,
           assertBarE,assertBarS,_,_,
           doSubtract,doJump
           } = controlBits;

   rom prog (    ~assertBarRom,            pc,  dbus);
   ram data (clk,~assertBarRam,storeMemBar,xreg,dbus);

   programCounter`suff p (resetBar,clk,~doJump,!assertBarRom,dbus,pc);

   fetch`suff f (clk,resetBar,loadBarIR,dbus,ir);

   control`suff c (ir,clk,aIsZero,flagCarry,controlBits);

   registers`suff registers
     (clkBar,resetBar,controlBits,dbus,areg,breg,xreg,qreg);

   alu`suff a
     (clk,reset,doSubtract,assertBarE,assertBarS,areg,breg,dbus,aIsZero,flagCarry,flagShift);

endmodule
