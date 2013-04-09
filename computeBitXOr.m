%Method - computeBitXOr
%Description - Compute Bitwise XOR

%Parameter - inpBits1
%   Desc -Binary string from Input 1
%Parameter - inpBits2
%   Desc -Binary string from Input 2
%Return - outputBit
%   Desc - Bitwise Xored output Bits from Input 1 and Input 2

function [outputBit] = computeBitXOr(inpBits1,inpBits2)
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here
lenBT1 = length(inpBits1);
lenBT2 = length(inpBits2);

if(lenBT1 > lenBT2)
   inpBits2 = [dec2bin(0,lenBT1-lenBT2) inpBits2];
elseif(lenBT2 > lenBT1)
   inpBits1 = [dec2bin(0,lenBT2-lenBT1) inpBits1];   
end

outputBit = sprintf('%d',abs(inpBits1-inpBits2));

%outputBit(isspace(outputBit)) = [];

outputBit(strfind(outputBit,' ')) = [];

% for incr1 = 1:1:lenBT1
%    if(strcmpi(inpBits1(incr1),inpBits2(incr1)))
%        outputBit(incr1) = '0';
%    else
%        outputBit(incr1) = '1';
%    end    
% end    


