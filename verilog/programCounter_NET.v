
module programCounter_NET
  (input resetBar, clk, doJumpBar, doInc, input [7:0] dbus,
   output wire [7:0] pc);

   wire carryLo2Hi;

   LS161 lo
     (.CLRB(resetBar),
      .CLK(clk),
      .A(dbus[0]),
      .B(dbus[1]),
      .C(dbus[2]),
      .D(dbus[3]),
      .ENP(doInc),
      .LOADB(doJumpBar),
      .ENT(1'b1),
      .QD(pc[3]),
      .QC(pc[2]),
      .QB(pc[1]),
      .QA(pc[0]),
      .CO(carryLo2Hi));

   LS161 hi
     (.CLRB(resetBar),
      .CLK(clk),
      .A(dbus[4]),
      .B(dbus[5]),
      .C(dbus[6]),
      .D(dbus[7]),
      .ENP(doInc),
      .LOADB(doJumpBar),
      .ENT(carryLo2Hi),
      .QD(pc[7]),
      .QC(pc[6]),
      .QB(pc[5]),
      .QA(pc[4]),
      .CO());

endmodule
