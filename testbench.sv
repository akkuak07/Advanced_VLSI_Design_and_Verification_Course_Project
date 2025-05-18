////////////////////////////////////////////////////////////////////////////////////////////// transaction class //////////////////////////////////////////////////////////////////////////////
class transaction;
  
   rand bit [31:0] paddr;
   rand bit [7:0] pwdata;
   rand bit psel;
   rand bit penable;
   randc bit pwrite;
   bit [7:0] prdata;
   bit pready;
   bit pslverr;
  
   constraint addr_c { paddr >= 0; paddr <= 15; } // Added constraint to restrit address within the small range as our memory defined earlier has depth of 16
   constraint data_c { pwdata >= 0; pwdata <= 255; } // Added constraint to restrit data within the small range
   
   function void display(input string tag);	// Added a user defined method display to display the value of all important data that we require to debug an entire verification enviorment
	$display("[%0s] :  paddr:%0d  pwdata:%0d pwrite:%0b  prdata:%0d pslverr:%0b @ %0t",tag,paddr,pwdata, pwrite, prdata, pslverr,$time);
   endfunction

 endclass
  
////////////////////////////////////////////////////////////////////////////////////////////// generator class //////////////////////////////////////////////////////////////////////////////
class generator;
  
    transaction tr;	//transaction object
    mailbox #(transaction) mbx;	// mailbox to send the data to a driver
    int count = 0;	// count is to define the no. of random stimuli that we plan to apply  to a DUT
  
    event nextdrv; ///driver completed task of triggering interface
    event nextsco; ///scoreboard completed its objective
    event done;	// As soon as we send the required no. of stimuli requested by user, we will be triggering done event to stop our verification enviorment execution
    
   function new(mailbox #(transaction) mbx);	// In custom constructor we will be taking an argument of mailbox working between generator & driver
	this.mbx = mbx;
    tr=new();
   endfunction;
  
    task run();
      repeat(count)	// started the main task of generator , we call the repeat block with specified count
        begin    
            assert(tr.randomize()) else $error("Randomization failed");  // called the randomize method, it will generate the random value for all the input ports
            mbx.put(tr);	// used put() method of mailbox to send this data to the driver	
            tr.display("GEN");	// called the display() method with 'GEN' tag, so it will display values of all data members along with tag 'GEN'
            @(nextdrv);	// we wait for driver to complete its operation i.e. application of a stimuli to a DUT
            @(nextsco);	//we also wait till scoreboard completes the process of comparing the response with an expected data
          end  
      ->done;	// As soon as we send the required no. of stimuli requested by user we trigger a done event which leads to termination of our simulation 
    endtask
   
endclass

///////////////////////////////////////////////////////////////////////////////////////////////// driver class //////////////////////////////////////////////////////////////////////////////  
class driver;
  
    virtual abp_if vif;	// driver requires a virtual interface to get an access of a signal to which we will be applying a random stimuli that we recieve from generator  
    mailbox #(transaction) mbx;	// driver requires mailbox to recieve the data from generator
    transaction datac; // we also require transaction object to save the data recieve from a generator
  
    event nextdrv;
  
    function new(mailbox #(transaction) mbx);
		this.mbx = mbx;
    endfunction;
  
  
   task reset();	// reset task will apply reset to our DUT
     vif.presetn <= 1'b0;
     vif.psel    <= 1'b0;
     vif.penable <= 1'b0;
     vif.pwdata  <= 0;
     vif.paddr   <= 0;
     vif.pwrite  <= 1'b0;
     repeat(5) @(posedge vif.pclk);	// we keep our system reset for 5 edges of a pclk before we remove reset
     vif.presetn <= 1'b1;
     $display("[DRV] : RESET DONE");
     $display("----------------------------------------------------------------------------");
   endtask
    
   task run();
     forever begin
      
       mbx.get(datac);	// we called the get() method to recieve the data from the generator
       @(posedge vif.pclk);	// we wait for arrival of a positive edge of a clk
       if(datac.pwrite == 1) // we will check if it is the write operation
		begin
			vif.psel    <= 1'b1;
			vif.penable <= 1'b0;
			vif.pwdata  <= datac.pwdata;
			vif.paddr   <= datac.paddr;
			vif.pwrite  <= 1'b1;	// this is how we specify the first cycle of an APB transfer
            @(posedge vif.pclk);	// then we wait for positive edge of pclk & then we make penable as '1'
            vif.penable <= 1'b1;	// this marks second cycle of APB transfer
            @(posedge vif.pclk);
            vif.psel <= 1'b0;
            vif.penable <= 1'b0;
            vif.pwrite <= 1'b0;	// So, after completion of an APB transfer we keep psel, penable & pwrite to a default value of '0'
            datac.display("DRV");	//display the data that we sent to a DUT, datac is the container where we stor the data that we recieve from a generator
            ->nextdrv;          
         end
       else if (datac.pwrite == 0) //  we will check if it is the read operation
         begin
			vif.psel <= 1'b1;
			vif.penable <= 1'b0;
            vif.pwdata <= 0;
            vif.paddr <= datac.paddr;
            vif.pwrite <= 1'b0;	// this is how we specify the first cycle of an APB transfer
            @(posedge vif.pclk);	// then we wait for positive edge of pclk & then we make penable as '1'
            vif.penable <= 1'b1;	// this marks second cycle of APB transfer
            @(posedge vif.pclk);
            vif.psel <= 1'b0;
            vif.penable <= 1'b0;
            vif.pwrite <= 1'b0;	// So, after completion of an APB transfer we keep psel, penable & pwrite to a default value of '0'
            datac.display("DRV");	//display the data that we sent to a DUT, datac is the container where we stor the data that we recieve from a generator
            ->nextdrv;
         end
      
     end
   endtask
  
endclass

///////////////////////////////////////////////////////////////////////////////////////////////// monitor class //////////////////////////////////////////////////////////////////////////////   
class monitor;
  
    virtual abp_if vif;	// In monitor we need to capture the response of DUT, hence we required a virtual interface
    mailbox #(transaction) mbx;	// mailbox to send the data to a scoreboard for comparision
    transaction tr;
  
    function new(mailbox #(transaction) mbx);
		this.mbx = mbx;    
    endfunction;
  
   task run();
	tr = new();	// we create an object of transaction
    forever 
		begin
			@(posedge vif.pclk);	// we wait for the positive edge of the clock and wait till completion of our transfer
            if(vif.pready)	// so, APB transfer completion is mark when pready becomes high
				begin
					tr.pwdata  = vif.pwdata;
					tr.paddr   = vif.paddr;
					tr.pwrite  = vif.pwrite;
					tr.prdata  = vif.prdata;
					tr.pslverr = vif.pslverr; // After APB transfer completion, we capture all of the data and all of the data is required in a scoreboard to compare with golden response
					@(posedge vif.pclk);
					tr.display("MON");	// then we trigger the display() method  for transaction 'tr' with the tag as 'MON' and send this data to a scoreboard
					mbx.put(tr);
				end
        end
    endtask
  
endclass

////////////////////////////////////////////////////////////////////////////////////////////// scoreboard class //////////////////////////////////////////////////////////////////////////////
class scoreboard;
  
    mailbox #(transaction) mbx;
    transaction tr;
    event nextsco;
	
	bit [7:0] pwdata[16] = '{default:0};	// Here since we are working on a memory s we have declared an array which is capable of carrying 8-bit data and the depth is 16
    bit [7:0] rdata;
    int err = 0;	// Added this variable to store an error count
  
    function new(mailbox #(transaction) mbx);
		this.mbx = mbx;    
    endfunction;
  
   task run();
	forever
		begin
			mbx.get(tr); // Here firstly we are recieving the data from monitor
			tr.display("SCO");
			
			if( (tr.pwrite == 1'b1) && (tr.pslverr == 1'b0))  ///write access
				begin
					pwdata[tr.paddr] = tr.pwdata;
					$display("[SCO] : DATA STORED DATA : %0d ADDR: %0d",tr.pwdata, tr.paddr);
				end
			else if((tr.pwrite == 1'b0) && (tr.pslverr == 1'b0))  ///read access
				begin
					rdata = pwdata[tr.paddr];    
					if( tr.prdata == rdata)
						$display("[SCO] : Data Matched");          
					else	// else inditates neither write nor read therefore data is mismatched & therefore error variable is also incremented in this case.
						begin
							err++;
							$display("[SCO] : Data Mismatched");
						end
				end
			else if(tr.pslverr == 1'b1)	// if we have an pslverr, then we simply mention the message that SLV ERROR DETECTED
				begin
					$display("[SCO] : SLV ERROR DETECTED");
				end  
			$display("---------------------------------------------------------------------------------------------------");
			->nextsco;
		end
	endtask
  
endclass

////////////////////////////////////////////////////////////////////////////////////////////// environment class ////////////////////////////////////////////////////////////////////////////// 
class environment;
	
	generator gen;
    driver drv;
    monitor mon;
    scoreboard sco;
	
	event nextgd; ///gen -> drv
    event nextgs;  /// gen -> sco
	
	mailbox #(transaction) gdmbx; ///gen - drv
    mailbox #(transaction) msmbx;  /// mon - sco
	
	virtual abp_if vif;
	
	function new(virtual abp_if vif);// As we need to create instance of all of the components in the enviornment	
		gdmbx = new();
		gen = new(gdmbx);
		drv = new(gdmbx);
		
		msmbx = new();
		mon = new(msmbx);
		sco = new(msmbx);
    
		this.vif = vif;
		drv.vif = this.vif;
		mon.vif = this.vif;
    
		gen.nextsco = nextgs;
		sco.nextsco = nextgs;
    
		gen.nextdrv = nextgd;
		drv.nextdrv = nextgd;
	endfunction
	
	task pre_test();	// In pre_test(), we apply reset to our DUT
		drv.reset();
	endtask
	
	task test();	// In main test(), we will call the main task of generator, driver, monitor & scoreboard
		fork
			gen.run();
			drv.run();
			mon.run();
			sco.run();
		join_any
   endtask
   
   task post_test(); // In post_test(), we will wait till event 'done' is triggered & event 'done' of generator will rigger when we send the requested no. of stimulus (as it depends on count)
	wait(gen.done.triggered);  
    $display("----Total number of Mismatch : %0d------",sco.err);	// As 'done' triggers we will be displaying the total no. of mismatch i.e. printing the value of error count in scoreboard
    $finish();
   endtask
   
   task run();
	pre_test();
    test();
    post_test();  
   endtask
  
 endclass
  
////////////////////////////////////////////////////////////////////////////////////////////// testbench module ////////////////////////////////////////////////////////////////////////////// 
module tb;
    
    abp_if vif();	// We have added an interface, forms the connection of an interface signal to a DUT 
	
	apb_s dut ( vif.pclk, vif.presetn, vif.paddr, vif.psel, vif.penable, vif.pwdata, vif.pwrite, vif.prdata, vif.pready, vif.pslverr );
	
	initial	// Generate the clock
		begin
			vif.pclk <= 0;
		end
		
	always #10 vif.pclk <= ~vif.pclk;
		
	environment env;
	
	initial
		begin
			env = new(vif);
			env.gen.count = 20;	// Specified the count of stimuli to be 20 
			env.run();	// Then we just called the main task of an enviornment
		end
	
	initial
		begin
			$dumpfile("dump.vcd");
			$dumpvars;
		end
    
endmodule  