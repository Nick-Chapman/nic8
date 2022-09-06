
module monitor
  (input clk,
   input [7:0] pc, ir, areg, breg, xreg, qreg,
   input `Control controlBits,
   input [7:0] dbus
   );

   reg verbose;
   initial verbose = $test$plusargs("verbose");

   reg showNoChange; //show no-change info
   initial showNoChange = $test$plusargs("change");

   initial if (verbose) $display("*nic8 simulation*");
   initial if (verbose) printBar;

   always @(posedge clk) if (verbose) #1 printStatus;
   //always @(clk) if (verbose) begin printStatus; #1 printStatus; end
   //always @(pc,ir,areg,breg,xreg,qreg) if (verbose) #1 printStatus;

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
   wire immediate,jumpControl,doSubtract,doJump;

   assign {loadIR,loadPC,loadA,loadB,loadX,doOut,storeMem,
           assertM,assertE,assertA,assertX,
           immediate,doSubtract,doJump
           } = controlBits;

   task printBar;
      $display("-------------------------------------------------------");
      $display("ticks(^)   PC AR BR XR IR  MEAX IPAXBMQ  i j  OUT  dbus");
      $display("-------------------------------------------------------");
   endtask

   wire [1:8] same = " ";
   wire [1:8] star = " ";

   task printStatus;
      $display("%4d(%s)  %s %s %s %s %s |%b%b%b%b|%b%b%b%b%b%b%b| %b %b {%03d}  %s"
               ,ticks,(clk?"pos":"neg")

               ,show(pc,pc1)
               ,show(areg,areg1)
               ,show(breg,breg1)
               ,show(xreg,xreg1)
               ,show(ir,ir1)

               ,assertM,assertE,assertA,assertX
               ,loadIR,loadPC,loadA,loadX,loadB,storeMem,doOut
               ,immediate,doJump
               ,qreg
               ,show(dbus,dbus1)
               );
      snap;
   endtask

   function [1:16] show(input [1:8] w, w1);
      if (showNoChange && (w == w1))
        show="~~";
      else
        show = {hex(w[1:4]),hex(w[5:8])};
   endfunction

   function [1:8] hex(input [1:4] w);
      case (w)
        0: hex="0";
        1: hex="1";
        2: hex="2";
        3: hex="3";
        4: hex="4";
        5: hex="5";
        6: hex="6";
        7: hex="7";
        8: hex="8";
        9: hex="9";
        10: hex="a";
        11: hex="b";
        12: hex="c";
        13: hex="d";
        14: hex="e";
        15: hex="f";
      endcase
   endfunction

   reg [7:0] ir1, pc1, areg1, breg1, xreg1, dbus1;
   task snap;
      ir1 = ir;
      pc1 = pc;
      areg1 = areg;
      breg1 = breg;
      xreg1 = xreg;
      dbus1 = dbus;
   endtask

endmodule

