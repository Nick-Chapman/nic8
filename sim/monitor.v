
module monitor
  (input clk,
   input [7:0] ir, pc, areg, breg, xreg, qreg,
   input `Control controlBits,
   input [7:0] dbus
   );

   reg verbose;
   initial verbose = $test$plusargs("verbose");

   initial if (verbose) $display("*nic8 simulation*");
   initial if (verbose) printBar;

   //always @(posedge clk) #1 printStatus;
   always @(clk) if (verbose) begin printStatus; #1 printStatus; end

   always @(qreg) if (!verbose) #1 $display("%03d",qreg);

   int steps;
   initial begin
     if (! $value$plusargs("steps=%d", steps)) begin
        $display("ERROR: please specify +steps=<value>.");
        $finish;
     end
   end

   int ticks = 0;
   always @(posedge clk) ticks++;
   always #1 if (verbose) if ($time > (10*steps)) $finish();

   wire loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem;
   wire assertM,assertE,assertA,assertX;
   wire immediate,jumpControl,doSubtract;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,jumpControl,doSubtract
           } = controlBits;

   task printBar;
      $display("-------------------------------------------------------");
      $display("ticks(^)   PC AR BR XR IR  MEAX IPAXBMQ  i j  OUT  dbus");
      $display("-------------------------------------------------------");
   endtask

   task printStatus;
      $display("%4d(%s)  %2h %2h %2h %2h %2h |%b%b%b%b|%b%b%b%b%b%b%b| %b %b {%03d}  %2h"
               ,ticks,(clk?"pos":"neg")
               ,pc,areg,breg,xreg,ir
               ,assertM,assertE,assertA,assertX
               ,loadIR,loadPC,loadA,loadX,loadB,storeMem,doOut
               ,immediate,jumpControl
               ,qreg
               ,dbus
               );
   endtask

endmodule

