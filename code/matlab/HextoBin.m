%Method - HextoBin
%Description - Convert Hexadecimal String to Binary

%Parameter - PTHex
%   Desc -Hexadecimal String
%Return - PTbin
%   Desc - Binary string (equivalence) of PTHex

function [PTbin] = HextoBin(PTHex)

PTbin = [];

for incr1 = 1:1:length(PTHex)
   %PTbin= [PTbin dec2bin(hex2dec(PTHex(incr1)),4)]; 
   switch PTHex(incr1)
       case '0' 
          PTbin = [PTbin '0000']; 
       case '1'
          PTbin = [PTbin '0001'];
       case '2'
          PTbin = [PTbin '0010']; 
       case '3'
          PTbin = [PTbin '0011'];  
       case '4'
          PTbin = [PTbin '0100'];  
       case '5'
          PTbin = [PTbin '0101'];  
       case '6'
          PTbin = [PTbin '0110'];  
       case '7'
          PTbin = [PTbin '0111'];  
       case '8'
          PTbin = [PTbin '1000'];  
       case '9'
          PTbin = [PTbin '1001'];  
       case {'a','A'}
          PTbin = [PTbin '1010'];  
       case {'b','B'}
          PTbin = [PTbin '1011'];  
       case {'c','C'}
          PTbin = [PTbin '1100'];  
       case {'d','D'}
          PTbin = [PTbin '1101'];  
       case {'e','E'}           
          PTbin = [PTbin '1110']; 
       case {'f','F'} 
          PTbin = [PTbin '1111'];  
   end            
end