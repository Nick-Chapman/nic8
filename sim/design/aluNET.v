
module aluNET(input clk, reset, doSubtract, assertE,
           input [7:0] areg, breg,
           output [7:0] dbus,
           output aIsZero,
           output reg [0:0] flagCarry);

   wire coutLO,coutHI;
   wire [7:0] aluOut;

   assign aIsZero = (areg == 0); // TODO: 8-wide nor

   wire [7:0] bin = doSubtract ? ~breg : breg; //TODO: invertor+mux ?

   // TODO: replace with latch chip:
   always #1 if (reset) flagCarry = 0;
   always @(posedge(clk || ~assertE)) flagCarry = coutHI;

   LS245 u0 (.ENB(~assertE), .DIR(1'b1), .A(aluOut), .B(dbus));

   LS283 lo
     (.A1(areg[0] ),
      .A2(areg[1]),
      .A3(areg[2]),
      .A4(areg[3]),
      .B1(bin[0]),
      .B2(bin[1]),
      .B3(bin[2]),
      .B4(bin[3]),
      .E1(aluOut[0]),
      .E2(aluOut[1]),
      .E3(aluOut[2]),
      .E4(aluOut[3]),
      .CIN(doSubtract),
      .COUT(coutLO));

   LS283 hi
     (.A1(areg[4]),
      .A2(areg[5]),
      .A3(areg[6]),
      .A4(areg[7]),
      .B1(bin[4]),
      .B2(bin[5]),
      .B3(bin[6]),
      .B4(bin[7]),
      .E1(aluOut[4]),
      .E2(aluOut[5]),
      .E3(aluOut[6]),
      .E4(aluOut[7]),
      .CIN(coutLO),
      .COUT(coutHI));

endmodule
