
`define suff _NET

module whole_cpu (input clk, reset);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus;
   wire aIsZero,flagCarry,flagShift;
   wire loadBarIR,storeMemBar, triggerA,triggerB,triggerX,triggerQ,triggerC,triggerS, assertRom,assertRam, assertBarE,assertBarS,assertBarA,assertBarX, doSubtract,doJumpBar;

   wire resetBar = ~reset;

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
      .triggerC(triggerC),
      .triggerS(triggerS),
      .assertRom(assertRom),
      .assertRam(assertRam),
      .assertBarE(assertBarE),
      .assertBarS(assertBarS),
      .assertBarA(assertBarA),
      .assertBarX(assertBarX),
      .doSubtract(doSubtract),
      .doJumpBar(doJumpBar));

   registers`suff registers
     (resetBar,
      triggerA,triggerB,triggerX,triggerQ,assertBarA,assertBarX
      ,dbus,areg,breg,xreg,qreg
      );

   alu`suff a
     (clk,reset,doSubtract,assertBarE,assertBarS,triggerC,triggerS,areg,breg,
      dbus,aIsZero,flagCarry,flagShift
      );

endmodule
