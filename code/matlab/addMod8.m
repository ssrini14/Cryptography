function [SumOut] = addMod8(X1,X2)

SumOut = dec2bin(mod(bin2dec(X1) + bin2dec(X2),256),8);

% lenBT1 = length(inpBits1);
% lenBT2 = length(inpBits2);
% 
% if(lenBT1 > lenBT2)
%    inpBits2 = [dec2bin(0,lenBT1-lenBT2) inpBits2];
% elseif(lenBT2 > lenBT1)
%    inpBits1 = [dec2bin(0,lenBT2-lenBT1) inpBits1];   
% end
% 
% outputBit = inpBits2;
% Carry = 0;
% for incr1 = lenBT1:-1:1
%    if(strcmpi(inpBits1(incr1),inpBits2(incr1)) && strcmpi(inpBits1(incr1),'1'))
%        if(Carry == 1)
%         outputBit(incr1) = '1';
%         Carry = 1;
%        else
%         outputBit(incr1) = '0';  
%         Carry = 1;
%        end
%    elseif(strcmpi(inpBits1(incr1),inpBits2(incr1)) && strcmpi(inpBits1(incr1),'0'))
%        if(Carry == 1)
%         outputBit(incr1) = '1';
%         Carry = 0;
%        else
%         outputBit(incr1) = '0';
%         Carry = 0;   
%        end 
%    else
%        if(Carry == 1)
%         outputBit(incr1) = '0';
%         Carry = 1;
%        else
%         outputBit(incr1) = '1';
%         Carry = 0;   
%        end 
%    end    
% end  
% outputBit = [sprintf('%d',Carry) outputBit];
% outputBit = outputBit(end-7:end);

