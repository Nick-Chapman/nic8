
`define suff _NET

module whole_cpu (input clk, resetBar);

   wire [7:0] pc,ir,areg,breg,xreg,qreg,dbus,ibus;
   wire aIsZero,flagCarry,flagShift;
   wire storeMemBar, triggerA,triggerB,triggerX,triggerQ,triggerC,triggerS, assertZeroBar,assertRam,assertRomBar, assertBarE,assertBarS,assertBarA,assertBarB,assertBarX, doSubtract,doCarryIn,doShiftIn,doJumpBar,denyFetch;

   assign dbus = assertZeroBar ? 8'bz : 8'b0; // register pulldown

   rom`suff prog (    assertRomBar,         pc,  dbus, ibus);
   ram`suff data (clk,assertRam,storeMemBar,xreg,dbus);

   wire doInc;
   assign doInc = 1'b1;

   programCounter`suff p (resetBar,clk,doJumpBar,doInc,dbus,pc);

   fetch`suff f (clk,resetBar,denyFetch,ibus,ir);

   control`suff c
     (.ir(ir),
      .clk(clk),
      .aIsZero(aIsZero),
      .flagCarry(flagCarry),
      .flagShift(flagShift),
      .storeMemBar(storeMemBar),
      .triggerA(triggerA),
      .triggerB(triggerB),
      .triggerX(triggerX),
      .triggerQ(triggerQ),
      .triggerC(triggerC),
      .triggerS(triggerS),
      .assertZeroBar(assertZeroBar),
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
      .denyFetch(denyFetch));

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
