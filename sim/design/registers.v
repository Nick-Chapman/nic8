
module registers
  (input reset, clk, input `Control controlBits, input carry, input [7:0] dbus,
   output reg [7:0] ir, pc, areg, breg, xreg, qreg, output reg [0:0] flagCarry
   );

   wire loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract
           } = controlBits;

   always #1 if (reset) begin
      pc = 0;
      ir = 0;
      areg = 0;
      breg = 0;
      xreg = 0;
      qreg = 0;
      flagCarry = 0;
   end

   always @(posedge clk) ir <= loadIR ? dbus : 0;
   always @(posedge(clk || ~(loadPC && jumpControl))) pc <= xreg;
   always @(posedge(clk || ~immediate)) pc <= pc + 1;
   always @(posedge(clk || ~loadA)) areg <= dbus;
   always @(posedge(clk || ~loadB)) breg <= dbus;
   always @(posedge(clk || ~loadX)) xreg <= dbus;
   always @(posedge(clk || ~doOut)) qreg <= dbus;
   always @(posedge(clk || ~assertE)) flagCarry = carry;

endmodule
