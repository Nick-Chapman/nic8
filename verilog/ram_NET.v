
module ram_NET (input clk, outputEnable, writeEnableBar,
                input [7:0] addr,
                inout [7:0] data);

   //wire wb = writeEnableBar; //nope
   //wire wb = ~(~writeEnableBar); //yes! wat?

   //wire wb = ~(clk & ~writeEnableBar); //better?
   //wire wb = ~clk | writeEnableBar;
   wire wb = clk | writeEnableBar; // Also works

   MB8416A ram
     (.WB(wb),
      .GB(!outputEnable),
      .EB(1'b0),
      .A({3'b0,addr}),
      .DQ(data));

endmodule
