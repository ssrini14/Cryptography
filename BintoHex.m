%Method - BintoHex
%Description - Convert Binary String to Hexadecimal

%Parameter - PTBin
%   Desc -Binary string
%Return - PTHex
%   Desc - Hexadecimal string (equivalence) of PTBin

function [PTHex] = BintoHex(PTBin)

PTHex = [];
for incr1 = 1:4:length(PTBin)
   switch PTBin(incr1:incr1+3)
       case '0000' 
          PTHex = [PTHex '0']; 
       case '0001'
          PTHex = [PTHex '1'];
       case '0010'
          PTHex = [PTHex '2']; 
       case '0011'
          PTHex = [PTHex '3'];  
       case '0100'
          PTHex = [PTHex '4'];  
       case '0101'
          PTHex = [PTHex '5'];  
       case '0110'
          PTHex = [PTHex '6'];  
       case '0111'
          PTHex = [PTHex '7'];  
       case '1000'
          PTHex = [PTHex '8'];  
       case '1001'
          PTHex = [PTHex '9'];  
       case '1010'
          PTHex = [PTHex 'A'];  
       case '1011'
          PTHex = [PTHex 'B'];  
       case '1100'
          PTHex = [PTHex 'C'];  
       case '1101'
          PTHex = [PTHex 'D'];  
       case '1110'           
          PTHex = [PTHex 'E']; 
       case '1111' 
          PTHex = [PTHex 'F'];  
   end   
end