
`define suff _NET

module whole_cpu (input clk, resetBar);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus,ibus;
   wire aIsZero,flagCarry,flagShift;
   wire loadBarIR,storeMemBar, triggerA,triggerB,triggerX,triggerQ,triggerC,triggerS, assertRom,assertRam,assertRomBar, assertBarE,assertBarS,assertBarA,assertBarB,assertBarX, doSubtract,doCarryIn,doShiftIn,doJumpBar,doJump;

   rom`suff prog (    assertRomBar,         pc,  dbus, ibus);
   ram`suff data (clk,assertRam,storeMemBar,xreg,dbus);

   wire doInc;
   assign doInc = 1'b1;

   programCounter`suff p (resetBar,clk,doJumpBar,doInc,dbus,pc);

   wire fetchDenied;
   assign fetchDenied = assertRom | doJump; //TODO: gate!

   fetch`suff f (clk,resetBar,fetchDenied,ibus,ir);

   control`suff c
     (.ir(ir),
      .clk(clk),
      .aIsZero(aIsZero),
      .flagCarry(flagCarry),
      .flagShift(flagShift),
      .loadBarIR(loadBarIR),
      .storeMemBar(storeMemBar),
      .triggerA(triggerA),
      .triggerB(triggerB),
      .triggerX(triggerX),
      .triggerQ(triggerQ),
      .triggerC(triggerC),
      .triggerS(triggerS),
      .assertRom(assertRom),
      .assertRomBar(assertRomBar),
      .assertRam(assertRam),
      .assertBarE(assertBarE),
      .assertBarS(assertBarS),
      .assertBarA(assertBarA),
      .assertBarB(assertBarB),
      .assertBarX(assertBarX),
      .doSubtract(doSubtract),
      .doCarryIn(doCarryIn),
      .doShiftIn(doShiftIn),
      .doJumpBar(doJumpBar),
      .doJump(doJump));

   registers`suff registers
     (resetBar,
      triggerA,triggerB,triggerX,triggerQ,assertBarA,assertBarB,assertBarX
      ,dbus,areg,breg,xreg,qreg
      );

   alu`suff a
     (clk,resetBar,doSubtract,doCarryIn,doShiftIn,assertBarE,assertBarS,triggerC,triggerS,areg,breg,
      dbus,aIsZero,flagCarry,flagShift
      );

endmodule
