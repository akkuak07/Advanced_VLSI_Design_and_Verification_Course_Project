module apb_s
 (
     input  pclk,
     input  presetn,
     input [31:0] paddr,	// Address bus of size 32-bit
     input psel,
     input penable,
     input [7:0] pwdata,	// Write date of size 8-bit
     input pwrite,
    
     output reg [7:0] prdata,	// Read date of size 8-bit
     output reg pready,
     output     pslverr
 );

  localparam [1:0] idle = 0, write = 1, read = 2;	// As slave FSM has 3-states
   reg [7:0] mem[16];	// Declared a memory of 8-bit with depth 16 since write and read data busses are of size 8-bit 
   reg [1:0] state, nstate;	//declared two state variables, one to hold the reset decoding logic & other to hold the value written by next state decoder
    bit  addr_err , addv_err, data_err; // addr_range - should be less than 16
                                       // addr_val - be greater than or equal to 0 (Presence of x or z will be considered as an invalid value)
                                       // data_val - be greater than or equal to 0 (Presence of x or z will be considered as an invalid value)  
 
 ////////////////////////////////////////////////////////////////////////// reset decoder //////////////////////////////////////////////////////////////////////////////////////////
   always@(posedge pclk, negedge presetn)	//to include asynchronours reset, we have both plck & presetn in the sesitivity list of always block
     begin
       if(presetn == 1'b0)
           state <= idle;	// if presetn is low, we will be staying in the idle state.
       else
           state <= nstate;	// else we will be simply following value provided by next state decoder
     end
    
 /////////////////////////////////////////////////////////////////// next state , output decoder ///////////////////////////////////////////////////////////////////////////////////
   always@(*)	// this always block, predicts next state and it will also give out value for each output port
     begin	//so we have 3 ouput port present in our slave i.e. prdata, pready & pslverr. so here we will be writing logic for prdata & pready in this FSM.
		case(state)
			idle:
				begin
					prdata    = 8'h00; // in idle state prdata & pready both will have a default value of 0
					pready    = 1'b0;
        
						if(psel == 1'b1 && pwrite == 1'b1)  //we check whether user have started any valid transaction if psel=1
							nstate = write;	//then based on the value of pwrite i.e if pwrite=1 then we will write the data to the slave
						else if (psel == 1'b1 && pwrite == 1'b0)
							nstate = read;	// or if pwrite=0 then we will retun the data requested by user.
						else
							nstate = idle;	// if any of the above is not true, we will be staying in the idle state.
				end  
			write: // Since we need to update the memory in write state, we check first that we have a valid second cycle of APB transfer
				begin
					if(psel == 1'b1 && penable == 1'b1) // In the second cycle of APB transfer both psel=1 & penable=1
						begin
							if(!addr_err && !addv_err && !data_err ) //here we checked that we donot have presence of any error in transaction
								begin
									pready = 1'b1;	// here we complete an apb transfer by making pready high
									mem[paddr]  = pwdata; // we also update the memory with the data provided by user on pwdata bus
									nstate      = idle;	// And then we jump to idle state for next transaction
								end
							else	// else indicates we have a presence of error in the data provided by a user
								begin
									nstate = idle;
									pready = 1'b1;	// here we still mark completion of transfer but at the same instance we will also be raising pslverr
								end      
						end
				end
			read:
				begin
					if(psel == 1'b1 && penable == 1'b1 )	// here in read state we again check whether we have a valid second cycle of APB transfer
						begin
							if(!addr_err && !addv_err && !data_err ) // //here we checked that we donot have presence of any error in transaction
								begin
									pready = 1'b1;	// here we complete an apb transfer by making pready high
									prdata = mem[paddr]; //we return the data requested by user
									nstate      = idle;	// And then we jump to idle state for next transaction
								end
							else	// else indicates we have a presence of error
								begin
									pready = 1'b1;	// here we still mark completion of transfer but at the same instance we will also be raising pslverr
									prdata = 8'h00;	//prdata we are forcing to '0', its not mandatory as per ABP transaction, so we can provide either '0' or 'X'
									nstate      = idle;
								end
						end
				end 
			default :
				begin
					nstate = idle;	//default value for a state is idle
					prdata    = 8'h00;	//default value for prdata is '0'
					pready    = 1'b0;	//default value for pready is '0'
				end
		endcase
     end
  
 ////////////////////////////////////////////////////////////////// checking valid values of address ///////////////////////////////////////////////////////////////////////////////
 reg av_t = 0;
	always@()
		begin
			if(paddr >= 0)	//to detect the presence of an unknown values, we just need to see if paddr>=0, as it will be true in most of the valid values 
				av_t = 1'b0;
			else	//but if it has presence of 'x' or 'z', the logic will trigger making value av_t=1 indicating invalid address & if this is the case, we need to trigger addv_err
				av_t = 1'b1;
		end
  
 ///////////////////////////////////////////////////////////////// checking valid values of address ////////////////////////////////////////////////////////////////////////////////
 reg dv_t = 0;
	always@()
		begin
			if(pwdata >= 0)	//to detect the presence of an unknown values, we just need to see if pwdata>=0, as it will be true in most of the valid values 
				dv_t = 1'b0;
			else	//but if it has presence of 'x' or 'z', the logic will trigger making value dv_t=1 indicating invalid address & if this is the case, we need to trigger data_err
				dv_t = 1'b1;
		end
  
 assign addr_err = ((nstate == write || read) && (paddr > 15)) ? 1'b1 : 1'b0; // we wait till we reach the second stage of APB transfer and if paddr>15, then addr_err triggers
 assign addv_err = (nstate == write || read) ? av_t : 1'b0;	// we wait till we reach the second stage of APB transfer, in this case we will simply follow value of av_t else '0'
 assign data_err = (nstate == write || read) ? dv_t : 1'b0;	// we wait till we reach the second stage of APB transfer, in this case we will simply follow value of dv_t else '0'
 assign pslverr  = (psel == 1'b1 && penable == 1'b1) ? ( addv_err || addr_err || data_err) : 1'b0;
 endmodule