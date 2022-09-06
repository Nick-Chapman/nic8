
module registersNET
  (input reset, clk, input `Control controlBits, input carry, input [7:0] dbus,
   output [7:0] ir, areg, breg, xreg, qreg, output [0:0] flagCarry
   );

   reg [7:0] ir;
   reg [0:0] flagCarry;

   wire loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract,doJump;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract,doJump
           } = controlBits;

   always #1 if (reset) begin
      flagCarry = 0;
   end

   LS273 u0 (.MRB(!reset), .CP(clk), .D(loadIR ? dbus : 8'b0), .Q(ir));

   always @(posedge(clk || ~assertE)) flagCarry = carry;

   LS273 u1 (.MRB(!reset), .CP(clk || ~loadA), .D(dbus), .Q(areg));
   LS273 u2 (.MRB(!reset), .CP(clk || ~loadB), .D(dbus), .Q(breg));
   LS273 u3 (.MRB(!reset), .CP(clk || ~loadX), .D(dbus), .Q(xreg));
   LS273 u4 (.MRB(!reset), .CP(clk || ~doOut), .D(dbus), .Q(qreg));

endmodule
