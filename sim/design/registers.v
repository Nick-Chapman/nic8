
module registers
  (input clk, input `Control controlBits, input carry, input [7:0] dbus,abus,
   output reg [7:0] ir, pc, areg, breg, xreg, qreg, output reg [0:0] flagCarry
   );

   wire loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract
           } = controlBits;

   initial ir = 0;
   initial pc = 0;
   initial areg = 0;
   initial breg = 0;
   initial xreg = 0;
   initial flagCarry = 0;

   always @(posedge clk) ir <= loadIR ? dbus : 0;
   always @(posedge clk) if (loadPC && jumpControl) pc <= abus;
   always @(posedge clk) if (immediate) pc <= pc + 1;
   always @(posedge clk) if (loadA) areg <= dbus;
   always @(posedge clk) if (loadB) breg <= dbus;
   always @(posedge clk) if (loadX) xreg <= dbus;
   always @(posedge clk) if (doOut) qreg <= dbus;
   always @(posedge clk) if (assertE) flagCarry = carry;

endmodule
