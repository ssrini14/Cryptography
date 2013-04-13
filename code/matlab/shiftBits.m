%Method - shiftBits
%Description - Rotate the bits to left or right

%Parameter - BitVal
%   Desc -Input Bit stream to be rotated
%Parameter - shiftVal
%   Desc - Integer indicating the rotation, positive is right shift and
%   negative is left shift
%Return - BitShiftVal
%   Desc - Rotated Bits from the original bit stream

function [BitShiftVal] = shiftBits(BitVal,shiftVal)

Idx = 1:1:length(BitVal);
SizeBit = length(BitVal);

% shiftIdx = circshift(Idx',shiftVal)';
% BitShiftVal = BitVal(shiftIdx);


if(shiftVal < 0)
  shiftVal = -1 * shiftVal;
  shiftIdx  = Idx + shiftVal;
  shiftIdx(shiftIdx > SizeBit) = shiftIdx(shiftIdx > SizeBit) -SizeBit;
  BitShiftVal = BitVal(shiftIdx);
else
  shiftIdx  = Idx - shiftVal;
  shiftIdx(shiftIdx <= 0) = shiftIdx(shiftIdx <= 0) + SizeBit;
  BitShiftVal = BitVal(shiftIdx);
end