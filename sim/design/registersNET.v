
module registersNET
  (input reset, clk, input `Control controlBits, input carry, input [7:0] dbus,
   output [7:0] ir, pc, areg, breg, xreg, qreg, output [0:0] flagCarry
   );

   reg [7:0] pc,ir;
   reg [0:0] flagCarry;

   wire loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract
           } = controlBits;

   always #1 if (reset) begin
      pc = 0;
      flagCarry = 0;
   end

   LS273 u0 (.MRB(!reset), .CP(clk), .D(loadIR ? dbus : 8'b0), .Q(ir));

   wire doJump = loadPC && jumpControl;

   always @(posedge(clk || ~doJump),
            posedge(clk || ~immediate))
     pc <=  doJump ? dbus : pc + 1;

   always @(posedge(clk || ~assertE)) flagCarry = carry;

   LS273 u1 (.MRB(!reset), .CP(clk || ~loadA), .D(dbus), .Q(areg));
   LS273 u2 (.MRB(!reset), .CP(clk || ~loadB), .D(dbus), .Q(breg));
   LS273 u3 (.MRB(!reset), .CP(clk || ~loadX), .D(dbus), .Q(xreg));
   LS273 u4 (.MRB(!reset), .CP(clk || ~doOut), .D(dbus), .Q(qreg));

endmodule
