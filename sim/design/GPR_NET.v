
module GPR_NET(input resetBar, assertBar, trigger,
               inout [7:0] bus,
               output [7:0] contents //for display on LEDs
               );

   LS273 flipFlop (.MRB(resetBar), .CP(trigger), .D(bus), .Q(contents));
   LS245 lineDriver (.ENB(assertBar), .DIR(1'b1), .A(contents), .B(bus));

endmodule
