
`define suff _NET

module whole_cpu (input clk, reset);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus;
   wire aIsZero,flagCarry,flagShift;
   wire loadBarIR,storeMemBar, triggerA,triggerB,triggerX,triggerQ, assertBarRom,assertBarRam, assertBarE,assertBarS,assertBarA,assertBarX, doSubtract,doJump;

   wire clkBar = ~clk;
   wire resetBar = ~reset;
   wire assertRom = ~assertBarRom;
   wire assertRam = ~assertBarRam;
   wire doJumpBar = ~doJump;

   rom prog (    assertRom,            pc,  dbus);
   ram data (clk,assertRam,storeMemBar,xreg,dbus);

   programCounter`suff p (resetBar,clk,doJumpBar,assertRom,dbus,pc);

   fetch`suff f (clk,resetBar,loadBarIR,dbus,ir);

   control`suff c
     (.ir(ir),
      .clk(clk),
      .aIsZero(aIsZero),
      .flagCarry(flagCarry),
      .loadBarIR(loadBarIR),
      .storeMemBar(storeMemBar),
      .triggerA(triggerA),
      .triggerB(triggerB),
      .triggerX(triggerX),
      .triggerQ(triggerQ),
      .assertBarRom(assertBarRom),
      .assertBarRam(assertBarRam),
      .assertBarE(assertBarE),
      .assertBarS(assertBarS),
      .assertBarA(assertBarA),
      .assertBarX(assertBarX),
      .doSubtract(doSubtract),
      .doJump(doJump));

   registers`suff registers
     (clkBar,resetBar,
      triggerA,triggerB,triggerX,triggerQ,assertBarA,assertBarX
      ,dbus,areg,breg,xreg,qreg
      );

   alu`suff a
     (clk,reset,doSubtract,assertBarE,assertBarS,areg,breg,
      dbus,aIsZero,flagCarry,flagShift
      );

endmodule
